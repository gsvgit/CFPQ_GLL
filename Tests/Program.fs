
open CFPQ_GLL
open Tests
open Expecto
open Logging
open CFPQ_GLL.RsmBuilder

let config = {FsCheckConfig.defaultConfig with maxTest = 10000}

let defaultLoggerConfig =
    [SPPF; RSM; GSS; GLL; RSMBuilder; DescriptorStack]
    |> List.map (fun x -> x, CFPQ_GLL.Logging.Info)

[<EntryPoint>]
let main _ =

    AddTargets defaultLoggerConfig
    ChangeTargetLevel GLL CFPQ_GLL.Logging.Trace
    //Logging.ConfigureWriter <| new System.IO.StreamWriter("log.txt")

    let rsm = GolangRSM.golangSrc ()
    rsm.ToDot "golangRSM.dot"
//    RSMCalculator.calculatorRSM () |> fst |> fun x -> x.ToDot "calculatorRSM.dot"

    let mkAlt x = List.map t x |> List.reduce ( +|+ )
    let spaces = [' '; '\t'; '\n'; '\r'] |> mkAlt

    // let src () =
    //         let S = nt "S"
    //         let B = nt "B"
    //         let E = nt "E"
    //         let C = nt "C"
    //         [
    //             S => many B +|+ E
    //             B => t '{' ** many S ** t '}'
    //             E => C ** t ';'
    //             C => literal "aa" ** t 'b' +|+ protect (t 'c' ** t ' ' ** t 'd')
    //         ] |> build [' ']

    // let rsm, tm, ntm =
    //     let S = nt "S"
    //     let T = nt "T"
    //     [ S =>  t 'a' +|+ T
    //       T => t 'b' +|+ S ] |> build [' '; '\t'; '\n'; '\r']


//     let mutable hugeTest ="""
// func e() int {
// var ex int = ;
// return x - y;
// }
// """

    let mutable hugeTest ="""1+ ; r1 ; """
    let src () =
        let IntExpr = nt "IntExpr"

        let Statement = nt "Statement"
        let Block = nt "Block"
        let Program = nt "Program"
        // [
        //     Program   =>  Block
        //     IntExpr   =>  t '1' ** t '+' ** t '1'
        //                   +|+ t '1'
        //     Block     =>  many Statement
        //     Statement => IntExpr ** t ';'
        //                  +|+ t 'r' ** IntExpr ** t ';'
        // ] |> build [' ']
        [
            Program   =>  Block
            IntExpr   =>  t '1' ** t '+' ** t '1'
                          +|+ t '1'
            Block     =>  many Statement
            Statement => IntExpr ** t ';'
                         +|+ t 'r' ** IntExpr ** t ';'
        ] |> build [' ']

    ErrorRecoveringTest.mkTestList src [
        hugeTest
        // ""
        // "{{}{{}}}"
        // "{{}{}"
        // "{{} aa b; {}}"
        // "{{} aa b {}}"
        // "{{} aa b aa b ; {}}"
        // "{{} aa b {;} aa b {}}"
        // "{{} aa b {;} aa b ; {} } "
        // "{c d;}"
        // "{c d}"
        // "{aa b; c d}"
        // "{c d c d;}"
        // "{c d c d;"
        // "c d c d;}"
        // "c d c d;"
        // "c d {} c d;"
    ]
    let src () =
        let S = nt "S"
        [
            S => many (t '(' ** S ** t ')' +|+  t '{' ** S ** t '}' +|+ t '[' ** S ** t ']')
        ] |> build [' ']

    let rsm = src ()

    // let x = LinearGraphReader.mkLinearGraph id tm hugeTest
    // x.ToDot ((), "test.dot")

    rsm.ToDot "rsm.dot"

    ErrorRecoveringTest.mkTestList src [
        "[{[]]"
    ]

//     let program ="""
// func expression(x int, y int) int {
//     var expr int = (x + y) ^ z * (x - y ^ z);
//     expr = expr; f(x, y);
//     var expr bool = x + y + z + x + y + z;
//     if (x < y) {
//         return x + y;
//     }
//     return x - y;
// }
// """

    // let g = mkLinearGraph id GolangRSM.terminalMapping program
    // ErrorRecoveringTest.run g GolangRSM.golangRSM GolangRSM.terminalMapping GolangRSM.nonTerminalMapping

    let tests =
        testList "All tests" [
            //LinearGraphReader.``Linear graph creating tests``
            ErrorRecoveringTest.``Error recovering tests``
        ] |> testSequenced

    //runTestsWithCLIArgs [] [||] tests
    0




