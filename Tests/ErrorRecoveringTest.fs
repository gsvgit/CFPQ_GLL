module Tests.ErrorRecoveringTest


open System
open System.Collections.Generic
open System.Text
open CFPQ_GLL.Common
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF
open CFPQ_GLL
open Expecto
open FSharpx.Collections
open GLLTests
open Tests.InputGraph
open Tests.LinearGraphReader
open CFPQ_GLL.RsmBuilder

let reverseDict (dict: Dictionary<'a, 'b>) =
    let x = dict |> Dictionary.KeyCollection |> Seq.map (fun k -> dict[k], k)
    let result = Dictionary()
    for k, v in x do
        result.Add(k, v)
    result

let run
    (graph: InputGraph)
    (q: RSM)
    (terminalMapping: Dictionary<char, int<terminalSymbol>>)
    dotFileName
    =
    let startV = 0<inputGraphVertex>
    let finalV = LanguagePrimitives.Int32WithMeasure<inputGraphVertex>(graph.NumberOfVertices() - 1)

    let reversedTerminalMapping =
        let result = reverseDict terminalMapping
        result.Add(-1<terminalSymbol>, 'ε')
        result

    //graph.ToDot (0, "graph.dot")
    //printfn "Graph is saved to graph.dot"

    let startVertices,mapping = graph.ToCfpqCoreGraph startV
    let finalVertices = mapping[finalV]

    let start = DateTime.Now
    let result = GLL.errorRecoveringEval finalVertices startVertices q GLL.AllPaths
    //printfn $"Execution time: {(DateTime.Now - start).TotalMilliseconds}"

    match result with
    | GLL.QueryResult.MatchedRanges _ ->
        let sppf = q.OriginalStartState.NonTerminalNodes.ToArray()
        let root = sppf |> Array.filter (fun n -> finalVertices = n.RightPosition && startVertices = n.LeftPosition) |> Array.minBy(fun n -> n.Distance)
        let distances = sppf |> Array.filter (fun n -> finalVertices = n.RightPosition && startVertices = n.LeftPosition)  |> Array.map (fun n -> n.Distance) |> Array.sort |> Array.toList
        //printfn $"Distances: {distances}"


        let actual = TriplesStoredSPPF([|root|], Dictionary())
        actual.ToDot (reversedTerminalMapping, dotFileName)
        //printfn $"SPPF is saved to {validDotFileName}"

        let output =
            match calculateActual root with
            | a, b, c, d, e, f -> $"({a}, {b}, {c}, {d}, {e}, {f}<distance>)"

        printfn $"{output}"

    | _ -> failwith "Unexpected result."

let gllTestCase
    lazySource
    (input: string)
    expected =
    let query, tm = lazySource()
    let graphMaker = mkLinearGraph id tm
    let graph = graphMaker input
    let startV = 0<inputGraphVertex>
    let finalV = LanguagePrimitives.Int32WithMeasure<inputGraphVertex>(graph.NumberOfVertices() - 1)
    testCase input <| fun () -> runErrorRecoveringGLLAndCheckResult input graph startV finalV query expected

let mkTestList f (inputs: string seq) =

    inputs
    |> Seq.iteri (fun i s ->
        let query, tm = f()
        let graphMaker = mkLinearGraph id tm
        printf $"""mkTest "{s}" """
        run (graphMaker s) query tm $"{i}.dot"
    )


let ``Error recovering tests`` =

    let ``On [*x rsm`` =

        let src () =
            let Lst = nt "List"
            let Elem = nt "Elem"
            [
                Lst => t '[' ** Elem
                Elem => t 'x' +|+ Lst
            ] |> build [' ']

        let mkTest = gllTestCase src

        testList "Latest bug" [
            mkTest "[ [" (0, 4, 4, 10, 3, 1<distance>)
            mkTest "[ [x" (0, 4, 4, 10, 3, 0<distance>)
            mkTest "[" (0, 2, 2, 4, 1, 1<distance>)
            mkTest " x" (0, 3, 2, 6, 2, 1<distance>)
            mkTest "" (0, 2, 2, 4, 1, 2<distance>)
            mkTest "[x[" (0, 3, 2, 6, 2, 1<distance>)
        ] |> testSequenced


    let ``On ca*b* rsm`` =

        let src () =
            let S = nt "S"
            [
                S => t 'c' ** many (t 'a') ** many (t 'b')
            ] |> build [' ']

        let mkTest = gllTestCase src

        testList "ca*b*" [
            mkTest "" (0, 1, 1, 1, 0, 1<distance>)
            mkTest "cab" (0, 3, 1, 5, 2, 0<distance>)
            mkTest "caabb" (0, 5, 1, 9, 4, 0<distance>)
            mkTest "caaaba" (0, 6, 1, 11, 5, 1<distance>)
            mkTest "ab" (0, 3, 1, 5, 2, 1<distance>)
            mkTest "ccab" (0, 4, 1, 7, 3, 1<distance>)
        ] |> testSequenced


    let ``On ab rsm`` =

        let src () =
            let S = nt "S"
            [
                S => t 'a' ** t 'b'
            ] |> build [' ']

        let mkTest = gllTestCase src

        testList "ab" [
            mkTest "" (0, 2, 1, 3, 1, 2<distance>)
            mkTest "ab" (0, 2, 1, 3, 1, 0<distance>)
            mkTest "abbbb" (0, 9, 1, 15, 8, 3<distance>)
            mkTest "ba" (0, 6, 1, 9, 4, 2<distance>)
            mkTest "a" (0, 2, 1, 3, 1, 1<distance>)
            mkTest "b" (0, 2, 1, 3, 1, 1<distance>)
            mkTest "a b " (0, 4, 1, 7, 3, 0<distance>)
            mkTest "a b b b b " (0, 16, 1, 29, 15, 3<distance>)
            mkTest "b   a " (0, 7, 1, 13, 6, 2<distance>)
        ] |> testSequenced


    let ``On golang rsm`` =

        let mkTest = gllTestCase GolangRSM.golangSrc

        testList "On golang rsm" [
            mkTest GolangRSM.functionSample (0, 129, 63, 319, 128, 0<distance>)
            mkTest GolangRSM.expressionSample (0, 183, 163, 527, 182, 0<distance>)
            mkTest GolangRSM.cycleSample (0, 199, 165, 561, 198, 0<distance>)
            mkTest "func f() int {}" (2, 15, 6, 38, 16, 0<distance>)
            //mkTest "fnc f() int {}" (2, 15, 6, 38, 16, 1<distance>)
            // mkTest "fnc f() int {" (2, 15, 6, 38, 16, 2<distance>)
            // mkTest "fnc f() {}" (2, 14, 6, 36, 15, 4<distance>)
            // mkTest "unc f() int {}" (2, 15, 6, 38, 16, 1<distance>)
            mkTest "" (1, 0, 1, 1, 0, 0<distance>)
            // mkTest "x" (0, 1, 1, 1, 0, 1<distance>)
            // mkTest "func f() int { return 1 }" (1, 28, 19, 75, 29, 1<distance>)
            // mkTest "fu\nnc f() int { return 1; } "  (1, 28, 13, 69, 28, 1<distance>)
        ] |> testSequenced

    let ``On Dyck lang`` =

        let src () =
            let S = nt "S"
            [
                S => Epsilon +|+ t '(' ** S ** t ')' ** S
            ] |> build [' ']

        let mkTest = gllTestCase src

        testList "On Dyck lang" [
            mkTest "" (1, 0, 1, 1, 0, 0<distance>)
            mkTest "()" (2, 2, 3, 9, 3, 0<distance>)
            mkTest "()()" (3, 4, 5, 17, 6, 0<distance>)
            mkTest "()(())" (4, 6, 7, 25, 9, 0<distance>)
            mkTest "(()())" (4, 6, 7, 25, 9, 0<distance>)
            mkTest "(" (1, 3, 2, 9, 3, 1<distance>)
            mkTest ")" (0, 1, 1, 1, 0, 1<distance>)
            mkTest "(()" (3, 5, 5, 20, 9, 1<distance>)
            mkTest "(()()" (5, 7, 8, 30, 12, 1<distance>)
        ]

    let ``On multi Dyck lang`` =

        let src () =
            let S = nt "S"
            let S1 = nt "S1"
            let S2 = nt "S2"
            let S3 = nt "S3"
            [
                S => Epsilon +|+ S1 +|+ S2 +|+ S3
                S1 => t '(' ** S ** t ')' ** S
                S2 => t '{' ** S ** t '}' ** S
                S3 => t '[' ** S ** t ']' ** S
            ] |> build [' ']

        let mkTest = gllTestCase src

        testList "On multi Dyck lang" [
            mkTest "{{[[]]}}()" (6, 10, 16, 46, 15, 0<distance>)
            mkTest "{[]}{(())()}" (7, 12, 19, 55, 18, 0<distance>)
            // mkTest "{]" (2, 7, 5, 23, 11, 2<distance>)
            // mkTest "[(}" (3, 26, 14, 80, 51, 3<distance>)
            // mkTest "[(])" (4, 14, 11, 47, 21, 2<distance>)
        ]

    let ``On Dyck lang  by regexp`` =

        let src () =
            let S = nt "S"
            [
                S => many(t '(' ** S ** t ')')
            ] |> build [' ']

        let mkTest = gllTestCase src

        testList "On Dyck lang by regexp" [
            mkTest "" (1, 0, 1, 1, 0, 0<distance>)
            mkTest "()" (1, 2, 2, 6, 2, 0<distance>)
            mkTest "()()" (2, 4, 3, 13, 5, 0<distance>)
            mkTest "()(())" (2, 6, 4, 18, 7, 0<distance>)
            mkTest "(()())" (2, 6, 4, 18, 7, 0<distance>)
            mkTest "(" (1, 3, 2, 7, 2, 1<distance>)
            mkTest ")" (0, 1, 1, 1, 0, 1<distance>)
            mkTest "(()" (2, 5, 4, 16, 7, 1<distance>)
            mkTest "(()()" (3, 9, 6, 30, 16, 1<distance>)
        ]

    let ``On multi Dyck lang by regexp`` =

        let src () =
            let S = nt "S"
            [
                S => many (t '(' ** S ** t ')' +|+  t '{' ** S ** t '}' +|+ t '[' ** S ** t ']')
            ] |> build [' ']

        let mkTest = gllTestCase src

        testList "On multi Dyck lang by regexp" [
            mkTest "" (1, 0, 1, 1, 0, 0<distance>)
            mkTest "()" (1, 2, 2, 6, 2, 0<distance>)
            mkTest "()()" (2, 4, 3, 13, 5, 0<distance>)
            mkTest "()(())" (2, 6, 4, 18, 7, 0<distance>)
            mkTest "(()())" (2, 6, 4, 18, 7, 0<distance>)
            mkTest "(" (1, 3, 2, 7, 2, 1<distance>)
            mkTest ")" (0, 1, 1, 1, 0, 1<distance>)
            mkTest "(()" (2, 5, 4, 16, 7, 1<distance>)
            mkTest "(()()" (3, 9, 6, 30, 16, 1<distance>)
            mkTest "{{[[]]}}()" (2, 10, 6, 28, 11, 0<distance>)
            mkTest "{[]}{(())()}" (3, 12, 7, 35, 14, 0<distance>)
            // mkTest "{]" (3, 11, 6, 27, 13, 2<distance>)
            // mkTest "[(}" (3, 25, 7, 63, 44, 3<distance>)
            // mkTest "[(])" (3, 17, 8, 43, 24, 2<distance>)
            mkTest "[ { [ ] ] " (3, 16, 7, 45, 23, 1<distance>)
        ]

    let ``Ambiguous`` =

        let src () =
            let S = nt "S"
            [
                S => t 'a'
                     +|+ S
                     +|+ S ** S
                     +|+ S ** S ** S
            ] |> build [' ']

        let mkTest = gllTestCase src

        testList "Ambiguous" [
            mkTest "" (0, 1, 1, 1, 0, 1<distance>)
            mkTest "a" (0, 1, 1, 1, 0, 0<distance>)
            mkTest "a " (0, 2, 1, 3, 1, 0<distance>)
            mkTest "aa" (0, 2, 3, 5, 1, 0<distance>)
            mkTest "a a" (0, 3, 3, 7, 2, 0<distance>)
            mkTest "aaa" (0, 3, 5, 9, 2, 0<distance>)
        ]

    testList "Error recovering" [
        ``On [*x rsm``
        ``On ca*b* rsm``
        ``On ab rsm``
        ``On golang rsm``
        ``On Dyck lang``
        ``On multi Dyck lang``
        ``On Dyck lang  by regexp``
        ``On multi Dyck lang by regexp``
        ``Ambiguous``
    ] |> testSequenced

