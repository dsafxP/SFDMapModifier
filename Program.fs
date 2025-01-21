open System
open System.IO
open System.Text

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

let appendName = "_modified"

let modifyMtHeader (filePath: string, replacementValue: string) =
    let headerName = "h_mt"
    let terminatorByte = byte 0x04

    try
        use fs = new FileStream(filePath, FileMode.Open, FileAccess.ReadWrite)
        use reader = new SFDBinaryReader(fs)

        let fileBytes = reader.BaseStream |> fun s -> Array.init (int s.Length) (fun _ -> reader.ReadByte())
        let headerBytes = Encoding.ASCII.GetBytes(headerName)
        let replacementBytes = Encoding.ASCII.GetBytes(replacementValue)

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
    let nullByte = byte 0x00

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
    printf "Enter the new Publish ID: "
    let publishId = Console.ReadLine()

    modifyPublishIdHeader(filePath, publishId)

[<EntryPoint>]
let main argv =
    let fileExtension = ".sfdm"

    let rec showMenuAndExecute filePath =
        printfn "Choose an option:"
        printfn "1. Unlock Official map"
        printfn "2. Set Publish Id"
        printf "Enter your choice (1 or 2): "
        match Console.ReadLine() with
        | "1" -> modifyMtHeader (filePath, "SFDMAPEDIT")
        | "2" -> choosePublishIdHeader filePath
        | _ ->
            printfn "Invalid choice. Please try again."
            showMenuAndExecute filePath

    if argv.Length = 1 then
        let filePath = argv.[0].Trim('"')  // Strip quotes from filepath
        if File.Exists(filePath) then
            if Path.GetExtension(filePath).ToLower() = fileExtension then
                showMenuAndExecute filePath
            else
                printfn "Error: File must have a %s extension." fileExtension
        else
            printfn "Error: File not found."
    else
        printfn "Please drag and drop a %s file into the console." fileExtension
        let filePath = Console.ReadLine().Trim('"')  // Strip quotes from filepath
        if File.Exists(filePath) then
            if Path.GetExtension(filePath).ToLower() = fileExtension then
                showMenuAndExecute filePath
            else
                printfn "Error: File must have a %s extension." fileExtension
        else
            printfn "Error: File not found."

    printfn "Press any key to exit..."
    Console.ReadKey() |> ignore
    0
