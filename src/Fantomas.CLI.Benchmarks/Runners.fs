module Fantomas.CLI.Benchmarks.Runners

open System.IO
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Engines
open Fantomas.Core

let config = FormatConfig.FormatConfig.Default

type BenchmarksBase() =
    let projectDir = "c:/projekty/fsharp/fsharp_main"
    
    let test (processInParallel: bool) =
        let args =
            Array.append
                (if processInParallel then [|"-parallel"|] else [||])
                [|"-r"; projectDir|]
        Program.main  args |> ignore
    
    [<Benchmark>]
    [<GcServer(true)>]
    member _.CheckSequentialServerGc() =
        test false
        
    [<Benchmark>]
    [<GcServer(false)>]
    member _.CheckSequentialWorkstationGc() =
        test false

    [<Benchmark>]
    [<GcServer(true)>]
    member _.CheckInParallelServerGc() =
        test true

    [<Benchmark>]
    [<GcServer(false)>]
    member _.CheckInParallelWorkstationGc() =
        test true

[<MemoryDiagnoser>]
[<RankColumn>]
[<SimpleJob(runStrategy=RunStrategy.ColdStart)>]
type ColdStart() =
    inherit BenchmarksBase()

[<MemoryDiagnoser>]
[<RankColumn>]
[<SimpleJob(runStrategy=RunStrategy.Monitoring)>]
type Regular() =
    inherit BenchmarksBase()
