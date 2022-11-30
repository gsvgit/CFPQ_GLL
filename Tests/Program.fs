
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
    ChangeTargetLevel GLL CFPQ_GLL.Logging.Info

    GolangRSM.golangRSM.ToDot "golangRSM.dot"
    0
//    let tests =
//        testList "All tests" [
//            //LinearGraphReader.``Linear graph creating tests``
//            ErrorRecoveringTest.``Error recovering tests``
//        ] |> testSequenced
//
//    runTestsWithCLIArgs [] [||] tests




