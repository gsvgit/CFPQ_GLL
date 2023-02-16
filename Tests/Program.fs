
open CFPQ_GLL
open Tests
open Logging
open CFPQ_GLL.RsmBuilder

let debug src input name =
    ChangeTargetLevel GLL Trace
    let query = src ()
    let graph = LinearGraphReader.mkLinearGraph id input
    graph.ToDot $"{name}_graph.dot"
    (query :> RSM.RSM).ToDot $"{name}_rsm.dot"
    ErrorRecoveringTest.run graph query $"{name}_sppf.dot" |> ignore
    ChangeTargetLevel GLL Info

let runTests () =
    ErrorRecoveringTest.initTestCases ()
    Expecto.Tests.runTestsWithCLIArgs [] [||] (ErrorRecoveringTest.tests ())

[<EntryPoint>]
let main _ =
    runTests ()



