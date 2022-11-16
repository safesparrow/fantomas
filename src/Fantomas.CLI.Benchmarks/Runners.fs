module Fantomas.CLI.Benchmarks.Runners

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Engines
open Fantomas.Core

let config = FormatConfig.FormatConfig.Default

[<MemoryDiagnoser>]
[<RankColumn>]
[<SimpleJob(runStrategy = RunStrategy.ColdStart, targetCount = 1)>]
type ColdStart() =
    let projectDir = "c:/projekty/fsharp/fsharp_main"

    let test (processInParallel: bool) =
        let args =
            Array.append (if processInParallel then [| "--parallel" |] else [||]) [| "-r"; projectDir |]

        Program.main args |> ignore
    //Thread.Sleep(100)

    [<Benchmark>]
    [<GcServer(true)>]
    member _.CheckSequentialServerGc() = test false

    // [<Benchmark>]
    // [<GcServer(false)>]
    // member _.CheckSequentialWorkstationGc() = test false

    [<Benchmark>]
    [<GcServer(true)>]
    member _.CheckInParallelServerGc() = test true
//
// [<Benchmark>]
// [<GcServer(false)>]
// member _.CheckInParallelWorkstationGc() = test true
