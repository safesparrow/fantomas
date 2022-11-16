open System
open System.IO
open Fantomas.Core
open Fantomas
open Fantomas.Daemon
open Argu
open System.Text
open Fantomas.Format
open Microsoft.FSharp.Collections

let extensions = set [| ".fs"; ".fsx"; ".fsi"; ".ml"; ".mli" |]

type Arguments =
    | [<Unique; AltCommandLine("-r")>] Recurse
    | [<Unique>] Force
    | [<Unique>] Profile
    | [<Unique>] Out of string
    | [<Unique>] Check
    | [<Unique>] Daemon
    | [<Unique>] Parallel
    | [<Unique; AltCommandLine("-v")>] Version
    | [<MainCommand>] Input of string list

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Recurse -> "Process the input folder recursively."
            | Force -> "Print the output even if it is not valid F# code. For debugging purposes only."
            | Out _ ->
                "Give a valid path for files/folders. Files should have .fs, .fsx, .fsi, .ml or .mli extension only. Multiple files/folders are not supported."
            | Profile -> "Print performance profiling information."
            | Check ->
                "Don't format files, just check if they have changed. Exits with 0 if it's formatted correctly, with 1 if some files need formatting and 99 if there was an internal error"
            | Daemon -> "Daemon mode, launches an LSP-like server to can be used by editor tooling."
            | Version -> "Displays the version of Fantomas"
            | Parallel -> "Process files in parallel"
            | Input _ ->
                sprintf
                    "Input paths: can be multiple folders or files with %s extension."
                    (Seq.map (fun s -> "*" + s) extensions |> String.concat ",")

let time f =
    let sw = Diagnostics.Stopwatch.StartNew()
    let res = f ()
    sw.Stop()
    printfn "Time taken: %O s" sw.Elapsed
    res

[<RequireQualifiedAccess>]
type InputPath =
    | File of string
    | Folder of string
    | Multiple of files: string list * folder: string list
    | NoFSharpFile of string
    | NotFound of string
    | Unspecified

[<RequireQualifiedAccess>]
type OutputPath =
    | IO of string
    | NotKnown

let isInExcludedDir (fullPath: string) =
    set [| "obj"; ".fable"; "fable_modules"; "node_modules" |]
    |> Set.map (fun dir -> sprintf "%c%s%c" Path.DirectorySeparatorChar dir Path.DirectorySeparatorChar)
    |> Set.exists fullPath.Contains

let isFSharpFile (s: string) =
    Set.contains (Path.GetExtension s) extensions

/// Get all appropriate files, either recursively or non-recursively
let rec allFiles isRec path =
    let searchOption =
        (if isRec then
             SearchOption.AllDirectories
         else
             SearchOption.TopDirectoryOnly)

    Directory.GetFiles(path, "*.*", searchOption)
    // |> Seq.filter (fun f ->
    //     isFSharpFile f
    //     && not (isInExcludedDir f)
    //     && not (IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) f))
    |> Seq.toArray
    |> Seq.toArray
    |> Array.chunkBySize 50
    |> Array.map (fun files ->
        async {
            return
                files
                |> Array.filter (fun f ->
                    isFSharpFile f
                    && not (isInExcludedDir f)
                    && not (IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) f))
        })
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.concat

/// Fantomas assumes the input files are UTF-8
/// As is stated in F# language spec: https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf#page=25
let private hasByteOrderMark file =
    if File.Exists(file) then
        let preamble = Encoding.UTF8.GetPreamble()

        use file = new FileStream(file, FileMode.Open, FileAccess.Read)

        let mutable bom = Array.zeroCreate 3
        file.Read(bom, 0, 3) |> ignore
        bom = preamble
    else
        false

/// Format a source string using given config and write to a text writer
let processSourceString (force: bool) s (fileName: string) config =
    let writeResult (formatted: string) = ()
    // if hasByteOrderMark fileName then
    //     File.WriteAllText(fileName, formatted, Encoding.UTF8)
    // else
    //     File.WriteAllText(fileName, formatted)
    //
    // printfn $"%s{fileName} has been written."

    async {
        let! formatted = s |> Format.formatContentAsync config fileName

        match formatted with
        | Format.FormatResult.Formatted(_, formattedContent) -> formattedContent |> writeResult
        | Format.InvalidCode(file, formattedContent) when force ->
            printfn $"%s{file} was not valid after formatting."
            formattedContent |> writeResult
        | Format.FormatResult.Unchanged file -> printfn $"'%s{file}' was unchanged"
        | Format.IgnoredFile file -> printfn $"'%s{file}' was ignored"
        | Format.FormatResult.Error(_, ex) -> raise ex
        | Format.InvalidCode(file, _) -> raise (exn $"Formatting {file} lead to invalid F# code")
    }

/// Format inFile and write to text writer
let processSourceFile (force: bool) inFile (tw: TextWriter) =
    async {
        let! formatted = Format.formatFileAsync inFile

        match formatted with
        | Format.FormatResult.Formatted(_, formattedContent) -> tw.Write(formattedContent)
        | Format.InvalidCode(file, formattedContent) when force ->
            printfn $"%s{file} was not valid after formatting."
            tw.Write(formattedContent)
        | Format.FormatResult.Unchanged _ -> inFile |> File.ReadAllText |> tw.Write
        | Format.IgnoredFile file -> printfn $"'%s{file}' was ignored"
        | Format.FormatResult.Error(_, ex) -> raise ex
        | Format.InvalidCode(file, _) -> raise (exn $"Formatting {file} lead to invalid F# code")
    }
    |> Async.RunSynchronously

let private writeInColor consoleColor (content: string) =
    let currentColor = Console.ForegroundColor
    Console.ForegroundColor <- consoleColor
    Console.WriteLine(content)
    Console.ForegroundColor <- currentColor

[<Literal>]
let StdInLineLimit = 2000

/// Read input from stdin, with a given line limit until EOF occurs.
///
/// Returns **None** if no lines were read or the stdin was not redirected through a pipe
let readFromStdin (lineLimit: int) =
    // The original functionality of the stdin flag, only accepted redirected input
    if not <| Console.IsInputRedirected then
        None
    else
        let isNotEof line = not <| isNull line
        let appendWithNewline acc next = acc + "\n" + next

        let input =
            Seq.initInfinite (fun _ -> Console.ReadLine())
            |> Seq.truncate lineLimit
            |> Seq.takeWhile isNotEof
            |> Seq.reduce appendWithNewline

        if String.IsNullOrWhiteSpace input then
            None
        else
            Some(input)

let private reportCheckResults (output: TextWriter) (checkResult: Format.CheckResult) =
    checkResult.Errors
    |> List.map (fun (filename, exn) -> sprintf "error: Failed to format %s: %s" filename (exn.ToString()))
    |> Seq.iter output.WriteLine

    checkResult.Formatted
    |> List.map (sprintf "%s needs formatting")
    |> Seq.iter output.WriteLine

let runCheckCommand (recurse: bool) (inputPath: InputPath) : int =
    let check files =
        Async.RunSynchronously(Format.checkCode files)

    let processCheckResult (checkResult: Format.CheckResult) =
        if checkResult.IsValid then
            stdout.WriteLine "No changes required."
            0
        else
            reportCheckResults stdout checkResult
            if checkResult.HasErrors then 1 else 99

    match inputPath with
    | InputPath.NoFSharpFile s ->
        eprintfn "Input path '%s' is unsupported file type" s
        1
    | InputPath.NotFound s ->
        eprintfn "Input path '%s' not found" s
        1
    | InputPath.Unspecified _ ->
        eprintfn "No input path provided. Call with --help for usage information."
        1
    | InputPath.File f when (IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) f) ->
        printfn "'%s' was ignored" f
        0
    | InputPath.File path -> path |> Seq.singleton |> check |> processCheckResult
    | InputPath.Folder path -> path |> allFiles recurse |> check |> processCheckResult
    | InputPath.Multiple(files, folders) ->
        let allFilesToCheck =
            seq {
                yield! files
                yield! (Seq.collect (allFiles recurse) folders)
            }

        allFilesToCheck |> check |> processCheckResult

type Force = bool

type Op =
    | File of Force * string * string
    | IgnoredFile of string
    | CreateDirectory of string

[<EntryPoint>]
let main argv =
    Environment.CurrentDirectory <- "c:/projekty/fsharp/fsharp_main"

    let errorHandler =
        ProcessExiter(
            colorizer =
                function
                | ErrorCode.HelpText -> None
                | _ -> Some ConsoleColor.Red
        )

    let parser =
        ArgumentParser.Create<Arguments>(programName = "dotnet fantomas", errorHandler = errorHandler)

    let results = parser.ParseCommandLine argv

    let outputPath =
        match results.TryGetResult <@ Arguments.Out @> with
        | Some output -> OutputPath.IO output
        | None -> OutputPath.NotKnown

    let inputPath =
        let maybeInput = results.TryGetResult <@ Arguments.Input @>

        match maybeInput with
        | Some [ input ] ->
            if Directory.Exists(input) then
                InputPath.Folder input
            elif File.Exists input && isFSharpFile input then
                InputPath.File input
            elif File.Exists input then
                InputPath.NoFSharpFile input
            else
                InputPath.NotFound input
        | Some inputs ->
            let isFolder (path: string) = Path.GetExtension(path) = ""

            let rec loop
                (files: string list)
                (finalContinuation: string list * string list -> string list * string list)
                =
                match files with
                | [] -> finalContinuation ([], [])
                | h :: rest ->
                    loop rest (fun (files, folders) ->
                        if isFolder h then
                            files, (h :: folders)
                        else
                            (h :: files), folders
                        |> finalContinuation)

            let filesAndFolders = loop inputs id
            InputPath.Multiple filesAndFolders
        | None -> InputPath.Unspecified

    let force = results.Contains <@ Arguments.Force @>
    let profile = results.Contains <@ Arguments.Profile @>
    let recurse = results.Contains <@ Arguments.Recurse @>

    let version = results.TryGetResult <@ Arguments.Version @>

    let fileToFile (force: bool) (inFile: string) (outFile: string) =
        try
            printfn $"Processing %s{inFile}"
            let hasByteOrderMark = hasByteOrderMark inFile

            use buffer =
                if hasByteOrderMark then
                    new StreamWriter(
                        new FileStream(outFile, FileMode.OpenOrCreate, FileAccess.ReadWrite),
                        Encoding.UTF8
                    )
                else
                    new StreamWriter(outFile)

            if profile then
                File.ReadLines(inFile) |> Seq.length |> printfn "Line count: %i"

                time (fun () -> processSourceFile force inFile buffer)
            else
                processSourceFile force inFile buffer

            buffer.Flush()
            printfn "%s has been written." outFile
        with exn ->
            reraise ()

    let stringToFile (force: bool) (s: string) (outFile: string) config =
        processSourceString force s outFile config

    let collectFile force inputFile outputFile = Op.File(force, inputFile, outputFile)

    let collectFolder force inputFolder outputFolder =
        seq {
            if not <| Directory.Exists(outputFolder) then
                yield Op.CreateDirectory outputFolder

            yield!
                allFiles recurse inputFolder
                |> Seq.map (fun i ->
                    // s supposes to have form s1/suffix
                    let suffix = i.Substring(inputFolder.Length + 1)

                    let o =
                        if inputFolder <> outputFolder then
                            Path.Combine(outputFolder, suffix)
                        else
                            i

                    Op.File(force, i, o))
        }
        |> Seq.toList

    let collectFilesAndFolders force (files: string list) (folders: string list) : Op list =
        let files =
            files
            |> List.map (fun file ->
                if (IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) file) then
                    Op.IgnoredFile file
                else
                    Op.File(force, file, file))

        let folders =
            folders |> List.collect (fun folder -> collectFolder force folder folder)

        files @ folders

    let processFile force inputFile outputFile =
        async {
            printfn "[%d] Processing %s" (System.Threading.Thread.CurrentThread.ManagedThreadId) inputFile
            let content = File.ReadAllText inputFile
            let config = EditorConfig.readConfiguration inputFile
            return! stringToFile force content inputFile config
        }

    let check = results.Contains <@ Arguments.Check @>
    let isDaemon = results.Contains <@ Arguments.Daemon @>
    let processInParallel = results.Contains <@ Arguments.Parallel @>

    let go (processInParallel: bool) (ops: Op list) : unit =
        let _lock = obj ()

        ops
        |> List.choose (function
            | Op.CreateDirectory dir -> Some dir
            | _ -> None)
        |> List.iter (fun outputFolder -> Directory.CreateDirectory(outputFolder) |> ignore)

        ops
        |> List.toArray
        // Order files by Length Descending, so that biggest files are processed first.
        |> Array.Parallel.map (fun op ->
            match op with
            | Op.File(b, s, s1) -> FileInfo(s).Length, op
            | _ -> 1000000000, op)
        |> Array.sortByDescending fst
        |> Array.map snd
        |> Array.map (fun op ->
            match op with
            | Op.File(b, s, s1) -> processFile b s s1
            | Op.IgnoredFile s -> async { printfn $"'%s{s}' was ignored" }
            | Op.CreateDirectory dir -> failwith $"Unexpected operation CreateDirectory at this stage")
        |> fun tasks ->
            if processInParallel then
                tasks |> Async.Parallel |> Async.RunSynchronously
            else
                tasks |> Array.map Async.RunSynchronously
        |> ignore

    if Option.isSome version then
        let version = CodeFormatter.GetVersion()
        printfn $"Fantomas v%s{version}"
    elif isDaemon then
        let daemon =
            new FantomasDaemon(Console.OpenStandardOutput(), Console.OpenStandardInput())

        AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> (daemon :> IDisposable).Dispose())

        daemon.WaitForClose.GetAwaiter().GetResult()
        exit 0
    elif check then
        inputPath |> runCheckCommand recurse |> exit
    else
        let go = go processInParallel

        try
            match inputPath, outputPath with
            | InputPath.NoFSharpFile s, _ ->
                eprintfn "Input path '%s' is unsupported file type." s
                exit 1
            | InputPath.NotFound s, _ ->
                eprintfn "Input path '%s' not found." s
                exit 1
            | InputPath.Unspecified, _ ->
                eprintfn "Input path is missing. Call with --help for usage information."
                exit 1
            | InputPath.File f, _ when (IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) f) ->
                printfn "'%s' was ignored" f
            | InputPath.File p1, OutputPath.NotKnown -> processFile force p1 p1 |> Async.RunSynchronously
            | InputPath.File p1, OutputPath.IO p2 -> processFile force p1 p2 |> Async.RunSynchronously
            | InputPath.Folder p1, OutputPath.NotKnown -> collectFolder force p1 p1 |> go
            | InputPath.Folder p1, OutputPath.IO p2 -> [ Op.File(force, p1, p2) ] |> go
            | InputPath.Multiple(files, folders), OutputPath.NotKnown ->
                collectFilesAndFolders force files folders |> go
            | InputPath.Multiple _, OutputPath.IO _ ->
                eprintfn "Multiple input files are not supported with the --out flag."
                exit 1
        with exn ->
            printfn "%s" exn.Message
            exit 1

    0
