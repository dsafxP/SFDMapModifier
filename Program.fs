open System
open System.IO
open System.Text

let fileExtension = ".sfdm"

let modifyHeader filePath =
    try
        // Open the file in binary mode for both reading and writing
        use fs = new FileStream(filePath, FileMode.Open, FileAccess.ReadWrite)
        use reader = new BinaryReader(fs)

        // Define the header name and replacement value
        let headerName = "h_mt"
        let replacementValue = "SFDMAPEDIT"

        let appendName = "_modified"

        let replacementBytes = Encoding.ASCII.GetBytes(replacementValue)

        // Read the entire file into memory
        let fileBytes = reader.ReadBytes(int fs.Length)

        // Find all the offsets of the header
        let headerBytes = Encoding.ASCII.GetBytes(headerName)
        let headerOffsets =
            fileBytes
            |> Array.windowed headerBytes.Length
            |> Array.mapi (fun idx window -> (idx, window))
            |> Array.filter (fun (_, window) -> window = headerBytes)
            |> Array.map fst

        match headerOffsets |> Array.tryItem 1 with
        | Some secondHeaderOffset ->
            // Determine where the header value starts
            let valueStart = secondHeaderOffset + headerBytes.Length

            // Find the index of the 0x04 byte (the terminating byte to stop removing bytes)
            let terminatorByte = byte 0x04
            let terminatorIndex = Array.tryFindIndex (fun byte -> byte = terminatorByte) fileBytes.[valueStart..]

            match terminatorIndex with
            | Some index ->
                // Calculate the end of the section to replace
                let terminatorOffset = valueStart + index

                // Create a new file content by concatenating before the header, replacement bytes, and after the terminator
                let beforeHeader = fileBytes.[..secondHeaderOffset-1] // Everything before the second "h_mt"
                let afterTerminator = fileBytes.[terminatorOffset..] // Everything after the 0x04 byte

                // Create the new file content
                let newFileBytes = Array.concat [beforeHeader; headerBytes; replacementBytes; afterTerminator]

                // Generate the new file path by appending "_modified" before the extension
                let newFilePath = Path.Combine(Path.GetDirectoryName(filePath), Path.GetFileNameWithoutExtension(filePath) + appendName + Path.GetExtension(filePath))

                // Write the modified file content to the new file
                use writer = new FileStream(newFilePath, FileMode.Create, FileAccess.Write)
                writer.Write(newFileBytes, 0, newFileBytes.Length)
            | None ->
                printfn "Error: Terminator byte not found after the header."
        | None ->
            printfn "Error: Second occurrence of header not found in the file."

    with
    | :? IOException as ex ->
        printfn "I/O error: %s" ex.Message
    | ex ->
        printfn "An error occurred: %s" ex.Message

[<EntryPoint>]
let main argv =
    if argv.Length = 1 then
        // Process the file passed as a command-line argument
        let filePath = argv.[0]
        if File.Exists(filePath) then
            if Path.GetExtension(filePath).ToLower() = fileExtension then
                modifyHeader filePath
            else
                printfn "Error: File must have a %s extension." fileExtension
        else
            printfn "Error: File not found."
    else
        // If no file argument is passed, prompt for a drag-and-drop file
        printfn "Please drag and drop a %s file into the console." fileExtension
        let filePath = Console.ReadLine() // Read the file path input
        if File.Exists(filePath) then
            if Path.GetExtension(filePath).ToLower() = fileExtension then
                modifyHeader filePath
            else
                printfn "Error: File must have a %s extension." fileExtension
        else
            printfn "Error: File not found."
    
    printfn "Press any key to exit..."
    Console.ReadKey() |> ignore
    0
