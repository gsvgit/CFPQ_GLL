
open CFPQ_GLL
open Tests
open Expecto
open Logging
open CFPQ_GLL.RsmBuilder

let debug src input name =
    ChangeTargetLevel GLL Trace
    let query = src ()
    let graph = LinearGraphReader.mkLinearGraph id input
    graph.ToDot $"{name}_graph.dot"
    (query :> RSM.RSM).ToDot $"{name}_rsm.dot"
    ErrorRecoveringTest.run graph query $"{name}_sppf.dot"

let runTests () =
    ErrorRecoveringTest.initTestCases ()
    runTestsWithCLIArgs [] [||] (ErrorRecoveringTest.tests ())

[<EntryPoint>]
let main _ =

    ErrorRecoveringTest.generateTests ErrorRecoveringTest.SimpleGolang [
        """1+ ; r1 ; """
        ""
        "1+" // Вот здесь странная деревяшка (2.dot)
    ]
    runTests ()



