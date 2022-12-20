
open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.Common
open CFPQ_GLL.GLL
open CFPQ_GLL.InputGraph
open Tests
open Tests.InputGraph
open CFPQ_GLL.SPPF
open Expecto
open Logging
open CFPQ_GLL.RsmBuilder
open Tests.LinearGraphReader


let config = {FsCheckConfig.defaultConfig with maxTest = 10000}

let defaultLoggerConfig =
    [SPPF; RSM; GSS; GLL; RSMBuilder; DescriptorStack]
    |> List.map (fun x -> x, CFPQ_GLL.Logging.Info)

[<EntryPoint>]
let main _ =

    AddTargets defaultLoggerConfig
    ChangeTargetLevel GLL CFPQ_GLL.Logging.Trace
    Logging.ConfigureWriter <| new System.IO.StreamWriter("log.txt")

    GolangRSM.golangRSM.ToDot ("golangRSM.dot", ErrorRecoveringTest.reverseDict GolangRSM.terminalMapping, ErrorRecoveringTest.reverseDict GolangRSM.nonTerminalMapping)
//    RSMCalculator.calculatorRSM () |> fst |> fun x -> x.ToDot "calculatorRSM.dot"

    let program ="""
func cycles(x int, y int) int {
    if ( x == 0 ) {
        return y;
    }
}
"""
    let g = LinearGraphReader.mkLinearGraph id GolangRSM.terminalMapping program
    ErrorRecoveringTest.run g GolangRSM.golangRSM GolangRSM.terminalMapping GolangRSM.nonTerminalMapping
    0
//    let tests =
//        testList "All tests" [
//            //LinearGraphReader.``Linear graph creating tests``
//            ErrorRecoveringTest.``Error recovering tests``
//        ] |> testSequenced
//
//    runTestsWithCLIArgs [] [||] tests




