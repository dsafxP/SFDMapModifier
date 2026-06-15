#!/usr/bin/env -S dotnet fsi --

open System
open System.IO
open System.Text

let appendName = "_modified"
let version = "v1.1-dev"

type SFDBinaryReader(stream: Stream) =
    inherit BinaryReader(stream)

    member val AutoCloseStream = true with get, set

    member this.ReadStringNonNull() =
        let str = this.ReadString()
        if String.IsNullOrEmpty(str) then "" else str

    member this.ReadStringNullDelimiter() =
        let rec readBytes (bytes: ResizeArray<byte>) =
            let b = this.ReadByte()
            if b <> 0uy then
                bytes.Add(b)
                readBytes bytes
            else
                bytes.ToArray()
        Encoding.UTF8.GetString(readBytes (ResizeArray()))

    override this.Dispose(disposing: bool) =
        if this.AutoCloseStream then base.Dispose(disposing)

type SFDBinaryWriter(stream: Stream) =
    inherit BinaryWriter(stream)

    member val AutoCloseStream = true with get, set

    member this.WriteStringNullDelimiter(text: string) =
        let sanitizedText = if String.IsNullOrEmpty(text) then "" else text.Replace("\0", "")
        let bytes = Encoding.UTF8.GetBytes(sanitizedText)
        this.Write(bytes)
        this.Write(byte 0)

    override this.Dispose(disposing: bool) =
        if this.AutoCloseStream then base.Dispose(disposing)

/// Parse a contiguous hex string (e.g. "31C2BD...") into raw bytes.
let hexStringToBytes (hex: string) : byte[] =
    [| for i in 0 .. 2 .. hex.Length - 2 -> Convert.ToByte(hex.[i..i+1], 16) |]

let modifyMtHeader (filePath: string, replacementBytes: byte[]) =
    let headerName = "h_mt"
    let terminatorByte = byte 0x04

    try
        use fs = new FileStream(filePath, FileMode.Open, FileAccess.ReadWrite)
        use reader = new SFDBinaryReader(fs)

        let fileBytes = reader.BaseStream |> fun s -> Array.init (int s.Length) (fun _ -> reader.ReadByte())
        let headerBytes = Encoding.ASCII.GetBytes(headerName)

        let offsets =
            fileBytes
            |> Array.windowed headerBytes.Length
            |> Array.mapi (fun idx window -> if window = headerBytes then Some idx else None)
            |> Array.choose id

        match offsets |> Array.tryItem 1 with
        | Some secondOffset ->
            let valueStart = secondOffset + headerBytes.Length
            let terminatorIndex =
                fileBytes.[valueStart..]
                |> Array.tryFindIndex ((=) terminatorByte)

            match terminatorIndex with
            | Some index ->
                let terminatorOffset = valueStart + index
                let beforeHeader = fileBytes.[..secondOffset - 1]
                let afterTerminator = fileBytes.[terminatorOffset..]
                let newFileBytes = Array.concat [beforeHeader; headerBytes; replacementBytes; afterTerminator]

                let newFilePath = Path.Combine(Path.GetDirectoryName(filePath), Path.GetFileNameWithoutExtension(filePath) + appendName + Path.GetExtension(filePath))
                use writer = new SFDBinaryWriter(new FileStream(newFilePath, FileMode.Create, FileAccess.Write))
                writer.Write(newFileBytes)

                printfn "Successfully modified and saved the file: %s" newFilePath
            | None -> printfn "Error: Terminator byte not found after the header."
        | None -> printfn "Error: Second occurrence of header not found."
    with
    | :? IOException as ex -> printfn "I/O error: %s" ex.Message
    | ex -> printfn "An error occurred: %s" ex.Message

let modifyPublishIdHeader(filePath: string, publishId: string) =
    let publishIdWithPrefix = Array.append [| byte 0x0A |] (Encoding.UTF8.GetBytes(publishId))
    let peiHeader = Encoding.ASCII.GetBytes("h_pei")
    let mHeader = [| byte 0x01; byte 0x4D; byte 0x01 |]
    let terminatorByte = byte 0x08

    try
        use fs = new FileStream(filePath, FileMode.Open, FileAccess.ReadWrite)
        use reader = new SFDBinaryReader(fs)

        let fileBytes = reader.BaseStream |> fun s -> Array.init (int s.Length) (fun _ -> reader.ReadByte())

        // Step 1: Find the "h_pei" header and replace the null byte with the publish ID
        let peiOffset =
            fileBytes
            |> Array.windowed peiHeader.Length
            |> Array.tryFindIndex (fun window -> window = peiHeader)

        match peiOffset with
        | Some(offset) ->
            // Remove the null byte after "h_pei" and insert the publish ID
            let beforePei = fileBytes.[..offset + peiHeader.Length - 1]
            let afterPei = fileBytes.[(offset + peiHeader.Length)..]
            let newFileBytes1 = Array.concat [beforePei; publishIdWithPrefix; afterPei]

            // Remove the extra null byte after the publish ID in the h_pei section
            let removePeiExtraByte =
                newFileBytes1
                |> Array.mapi (fun idx byte ->
                    if idx = (offset + peiHeader.Length + publishIdWithPrefix.Length) then None
                    else Some byte)
                |> Array.choose id

            // Step 2: Find the "M" header in the modified bytes (after replacing the "h_pei" header)
            let mHeaderOffset =
                removePeiExtraByte
                |> Array.windowed mHeader.Length
                |> Array.tryFindIndex (fun window -> window = mHeader)

            match mHeaderOffset with
            | Some(mOffset) ->
                // **Only remove one byte before the publish ID in the M header** (no other removal)
                let beforeM = removePeiExtraByte.[..mOffset + mHeader.Length - 1]
                let afterM = removePeiExtraByte.[(mOffset + mHeader.Length)..]

                // Remove just one byte before the publish ID and then insert it
                let newFileBytes2 =
                    if mOffset + mHeader.Length < removePeiExtraByte.Length then
                        Array.concat [beforeM; afterM.[1..]; publishIdWithPrefix; afterM.[1..]]
                    else
                        Array.concat [beforeM; publishIdWithPrefix; afterM]

                // Step 3: Look for the terminator byte
                let terminatorIndex = Array.tryFindIndex (fun byte -> byte = terminatorByte) newFileBytes2.[mOffset..]
                match terminatorIndex with
                | Some(tIndex) ->
                    let terminatorOffset = mOffset + tIndex
                    let beforeTerminator = newFileBytes2.[..terminatorOffset - 1]
                    let afterTerminator = newFileBytes2.[terminatorOffset ..]
                    let finalFileBytes = Array.concat [beforeTerminator; publishIdWithPrefix; afterTerminator]

                    // Write the modified bytes back to a new file
                    let newFilePath = Path.Combine(Path.GetDirectoryName(filePath), Path.GetFileNameWithoutExtension(filePath) + appendName + Path.GetExtension(filePath))
                    use writer = new SFDBinaryWriter(new FileStream(newFilePath, FileMode.Create, FileAccess.Write))
                    writer.Write(finalFileBytes)

                    printfn "Successfully modified and saved the file: %s" newFilePath
                | None -> printfn "Error: Terminator byte not found."
            | None -> printfn "Error: 'M' header not found."
        | None -> printfn "Error: 'h_pei' header not found."
    with
    | :? IOException as ex -> printfn "I/O error: %s" ex.Message
    | ex -> printfn "An error occurred: %s" ex.Message

let choosePublishIdHeader (filePath: string) =
    let isValidPublishId (publishId: string) =
        // Check if publish ID is digits only and at least 10 characters long
        publishId |> String.forall Char.IsDigit && publishId.Length >= 10

    printf "Enter the new Publish ID: "
    let publishId = Console.ReadLine().Trim()

    if isValidPublishId publishId then
        modifyPublishIdHeader(filePath, publishId)
    else
        printfn "Error: Publish ID must be at least 10 digits long and contain only numeric characters."

let modifyVersion(filePath: string, versionCode: string) =
    let startByte = byte 0x08
    let terminatorByte = byte 0x04

    try
        use fs = new FileStream(filePath, FileMode.Open, FileAccess.ReadWrite)
        use reader = new SFDBinaryReader(fs)

        // Read the entire file into a byte array
        let fileBytes = reader.BaseStream |> fun s -> Array.init (int s.Length) (fun _ -> reader.ReadByte())
        let replacementBytes = Encoding.ASCII.GetBytes(versionCode)

        // Find the first occurrence of the startByte
        let startIndex =
            fileBytes |> Array.tryFindIndex ((=) startByte)

        match startIndex with
        | Some sIdx ->
            // Find the first occurrence of the terminatorByte after the startByte
            let terminatorIndex =
                fileBytes.[sIdx + 1..] |> Array.tryFindIndex ((=) terminatorByte)

            match terminatorIndex with
            | Some tIdx ->
                let tIdxGlobal = sIdx + 1 + tIdx

                // Split the file into three parts: before the startByte, the replacement, and after the terminatorByte
                let beforeStart = fileBytes.[..sIdx]
                let afterTerminator = fileBytes.[tIdxGlobal..]
                let newFileBytes = Array.concat [beforeStart; replacementBytes; afterTerminator]

                // Save the modified file
                let newFilePath = Path.Combine(
                                    Path.GetDirectoryName(filePath),
                                    Path.GetFileNameWithoutExtension(filePath) + appendName + Path.GetExtension(filePath))
                use writer = new SFDBinaryWriter(new FileStream(newFilePath, FileMode.Create, FileAccess.Write))
                writer.Write(newFileBytes)

                printfn "Successfully modified and saved the file: %s" newFilePath
            | None ->
                printfn "Error: Terminator byte not found after start byte."
        | None ->
            printfn "Error: Start byte not found in the file."
    with
    | :? IOException as ex -> printfn "I/O error: %s" ex.Message
    | ex -> printfn "An error occurred: %s" ex.Message

let chooseModifyVersion(filePath: string) =
    let isValidVersion(versionCode: string) =
        versionCode.StartsWith("v.1.") && versionCode.Length = 8

    printf "Enter the version code (Example: v.1.0.0a): "
    let versionCode = Console.ReadLine().Trim()

    if (isValidVersion(versionCode)) then
        modifyVersion(filePath, versionCode)
    else
        printfn "Error: Invalid version code."

let modifyAuthorLock (filePath: string, lockValue: bool) =
    let headerName = "h_el"

    try
        use fs = new FileStream(filePath, FileMode.Open, FileAccess.ReadWrite)
        use reader = new SFDBinaryReader(fs)

        let fileBytes = reader.BaseStream |> fun s -> Array.init (int s.Length) (fun _ -> reader.ReadByte())
        let headerBytes = Encoding.ASCII.GetBytes(headerName)

        let offset =
            fileBytes
            |> Array.windowed headerBytes.Length
            |> Array.tryFindIndex (fun window -> window = headerBytes)

        match offset with
        | Some idx ->
            let boolOffset = idx + headerBytes.Length
            let newByte = if lockValue then byte 0x01 else byte 0x00
            let newFileBytes = Array.copy fileBytes
            newFileBytes.[boolOffset] <- newByte

            let newFilePath = Path.Combine(Path.GetDirectoryName(filePath), Path.GetFileNameWithoutExtension(filePath) + appendName + Path.GetExtension(filePath))
            use writer = new SFDBinaryWriter(new FileStream(newFilePath, FileMode.Create, FileAccess.Write))
            writer.Write(newFileBytes)

            let action = if lockValue then "locked" else "unlocked"
            printfn "Successfully %s author and saved the file: %s" action newFilePath
        | None -> printfn "Error: 'h_el' header not found."
    with
    | :? IOException as ex -> printfn "I/O error: %s" ex.Message
    | ex -> printfn "An error occurred: %s" ex.Message


let rec showMenuAndExecute filePath =
    printfn "Choose an option:"
    printfn "1. Official unlock"
    printfn "2. Author lock"
    printfn "3. Author unlock"
    printfn "4. Set Publish Id"
    printfn "5. Set Version"
    printf "Enter your choice: "
    match Console.ReadLine() with
    | "1" -> modifyMtHeader (filePath, Encoding.ASCII.GetBytes("SFDMAPEDIT"))
    | "2" -> modifyAuthorLock (filePath, true)
    | "3" -> modifyAuthorLock (filePath, false)
    | "4" -> choosePublishIdHeader filePath
    | "5" -> chooseModifyVersion filePath
    | _ ->
        printfn "Invalid choice. Please try again."
        showMenuAndExecute filePath


let printHelp () =
    printfn "Usage: Program.fsx -f <file> [options]"
    printfn ""
    printfn "Options:"
    printfn "  -f, --file <path>       Path to the file (required)"
    printfn "  -i, --interactive       Launch interactive menu (default if no mode given)"
    printfn "  -m, --mode <number>     Run a specific mode non-interactively:"
    printfn "                            1 = Official unlock"
    printfn "                            2 = Author lock"
    printfn "                            3 = Author unlock"
    printfn "                            4 = Set Publish ID  (requires -a <publishId>)"
    printfn "                            5 = Set Version     (requires -a <versionCode>)"
    printfn "  -a, --arg <value>       Argument for the selected mode"
    printfn "  -h, --help              Show this help message"
    printfn "  -V, --version           Print version and exit"

let printVersion () =
    printfn "%s" version

let runMode (filePath: string) (mode: string) (modeArg: string option) =
    match mode with
    | "1" ->
        modifyMtHeader (filePath, Encoding.ASCII.GetBytes("SFDMAPEDIT"))
    | "2" ->
        modifyAuthorLock (filePath, true)
    | "3" ->
        modifyAuthorLock (filePath, false)
    | "4" ->
        match modeArg with
        | Some arg ->
            let isValidPublishId (id: string) = id |> String.forall Char.IsDigit && id.Length >= 10
            if isValidPublishId arg then modifyPublishIdHeader (filePath, arg)
            else printfn "Error: Publish ID must be at least 10 digits long and contain only numeric characters."
        | None -> printfn "Error: Mode 4 (Set Publish ID) requires -a <publishId>."
    | "5" ->
        match modeArg with
        | Some arg ->
            let isValidVersion (v: string) = v.StartsWith("v.1.") && v.Length = 8
            if isValidVersion arg then modifyVersion (filePath, arg)
            else printfn "Error: Invalid version code."
        | None -> printfn "Error: Mode 5 (Set Version) requires -a <versionCode>."
    | other ->
        printfn "Error: Unknown mode '%s'. Valid modes are 1–5." other


type CliArgs = {
    File: string option
    Mode: string option
    ModeArg: string option
    Interactive: bool
    ShowHelp: bool
    ShowVersion: bool
}

let defaultArgs = { File = None; Mode = None; ModeArg = None; Interactive = false; ShowHelp = false; ShowVersion = false }

let rec parseArgs (tokens: string list) (acc: CliArgs) : CliArgs =
    match tokens with
    | [] -> acc
    | ("-h" | "--help") :: rest ->
        parseArgs rest { acc with ShowHelp = true }
    | ("-V" | "--version") :: rest ->
        parseArgs rest { acc with ShowVersion = true }
    | ("-i" | "--interactive") :: rest ->
        parseArgs rest { acc with Interactive = true }
    | ("-f" | "--file") :: value :: rest ->
        parseArgs rest { acc with File = Some (value.Trim('"')) }
    | ("-m" | "--mode") :: value :: rest ->
        parseArgs rest { acc with Mode = Some value }
    | ("-a" | "--arg") :: value :: rest ->
        parseArgs rest { acc with ModeArg = Some value }
    | unknown :: rest ->
        printfn "Warning: Unknown argument '%s' (ignored)." unknown
        parseArgs rest acc


let rawArgs = Environment.GetCommandLineArgs() |> Array.toList
let cli = parseArgs rawArgs defaultArgs

if cli.ShowHelp then
    printHelp ()
elif cli.ShowVersion then
    printVersion ()
else
    let filePath =
        match cli.File with
        | Some fp -> fp
        | None ->
            printfn "No file specified. Please drag and drop a file into the console or use -f <path>."
            Console.ReadLine().Trim('"')

    if File.Exists(filePath) then
        match cli.Mode with
        | Some m -> runMode filePath m cli.ModeArg
        | None   -> showMenuAndExecute filePath   // -i flag or no mode = interactive
    else
        printfn "Error: File not found: %s" filePath
