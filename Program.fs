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

let modifyMapHeader (filePath: string, replacementValue: string) =
    let headerName = "h_mt"
    let appendName = "_modified"
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
            | None -> printfn "Error: Terminator byte not found after the header."
        | None -> printfn "Error: Second occurrence of header not found."
    with
    | :? IOException as ex -> printfn "I/O error: %s" ex.Message
    | ex -> printfn "An error occurred: %s" ex.Message

[<EntryPoint>]
let main argv =
    let fileExtension = ".sfdm"

    if argv.Length = 1 then
        let filePath = argv.[0]
        if File.Exists(filePath) then
            if Path.GetExtension(filePath).ToLower() = fileExtension then
                modifyMapHeader (filePath, "SFDMAPEDIT")
            else
                printfn "Error: File must have a %s extension." fileExtension
        else
            printfn "Error: File not found."
    else
        printfn "Please drag and drop a %s file into the console." fileExtension
        let filePath = Console.ReadLine()
        if File.Exists(filePath) then
            if Path.GetExtension(filePath).ToLower() = fileExtension then
                modifyMapHeader (filePath, "SFDMAPEDIT")
            else
                printfn "Error: File must have a %s extension." fileExtension
        else
            printfn "Error: File not found."

    printfn "Press any key to exit..."
    Console.ReadKey() |> ignore
    0
