module Tests.ErrorRecoveringTest


open System
open System.Collections.Generic
open CFPQ_GLL.Common
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
    dotFileName
    =
    let startV = 0<inputGraphVertex>
    let finalV = LanguagePrimitives.Int32WithMeasure<inputGraphVertex>(graph.NumberOfVertices() - 1)

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
        let root = sppf |> Array.filter (fun n -> finalVertices = n.RightPosition && startVertices = n.LeftPosition) |> Array.minBy(fun n -> n.Weight)
        let weights = sppf |> Array.filter (fun n -> finalVertices = n.RightPosition && startVertices = n.LeftPosition)  |> Array.map (fun n -> n.Weight) |> Array.sort |> Array.toList
        //printfn $"Distances: {distances}"


        let actual = TriplesStoredSPPF([|root|], Dictionary())
        actual.ToDot dotFileName
        //printfn $"SPPF is saved to {validDotFileName}"

        let output =
            match calculateActual root with
            | a, b, c, d, e, f -> $"({a}, {b}, {c}, {d}, {e}, {f}<weight>)"

        printfn $"{output}"

    | _ -> failwith "Unexpected result."

let gllTestCase
    lazySource
    (input: string)
    expected =
    let query = lazySource()
    let graphMaker = mkLinearGraph id
    let graph = graphMaker input
    let startV = 0<inputGraphVertex>
    let finalV = LanguagePrimitives.Int32WithMeasure<inputGraphVertex>(graph.NumberOfVertices() - 1)
    testCase input <| fun () -> runErrorRecoveringGLLAndCheckResult input graph startV finalV query expected

let mkTestList f (inputs: string seq) =

    inputs
    |> Seq.iteri (fun i s ->
        let query = f()
        let graphMaker = mkLinearGraph id
        printf $"""mkTest "{s}" """
        run (graphMaker s) query $"{i}.dot"
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
            mkTest "[ [" (0, 4, 4, 10, 3, 1<weight>)
            mkTest "[ [x" (0, 4, 4, 10, 3, 0<weight>)
            mkTest "[" (0, 2, 2, 4, 1, 1<weight>)
            mkTest " x" (0, 3, 2, 6, 2, 1<weight>)
            mkTest "" (0, 2, 2, 4, 1, 2<weight>)
            mkTest "[x[" (0, 3, 2, 6, 2, 1<weight>)
        ] |> testSequenced


    let ``On ca*b* rsm`` =

        let src () =
            let S = nt "S"
            [
                S => t 'c' ** many (t 'a') ** many (t 'b')
            ] |> build [' ']

        let mkTest = gllTestCase src

        testList "ca*b*" [
            mkTest "" (0, 1, 1, 1, 0, 1<weight>)
            mkTest "cab" (0, 3, 1, 5, 2, 0<weight>)
            mkTest "caabb" (0, 5, 1, 9, 4, 0<weight>)
            mkTest "caaaba" (0, 6, 1, 11, 5, 1<weight>)
            mkTest "ab" (0, 3, 1, 5, 2, 1<weight>)
            mkTest "ccab" (0, 4, 1, 7, 3, 1<weight>)
        ] |> testSequenced


    let ``On ab rsm`` =

        let src () =
            let S = nt "S"
            [
                S => t 'a' ** t 'b'
            ] |> build [' ']

        let mkTest = gllTestCase src

        testList "ab" [
            mkTest "" (0, 2, 1, 3, 1, 2<weight>)
            mkTest "ab" (0, 2, 1, 3, 1, 0<weight>)
            mkTest "abbbb" (0, 9, 1, 15, 8, 3<weight>)
            mkTest "ba" (0, 6, 1, 9, 4, 2<weight>)
            mkTest "a" (0, 2, 1, 3, 1, 1<weight>)
            mkTest "b" (0, 2, 1, 3, 1, 1<weight>)
            mkTest "a b " (0, 4, 1, 7, 3, 0<weight>)
            mkTest "a b b b b " (0, 16, 1, 29, 15, 3<weight>)
            mkTest "b   a " (0, 7, 1, 13, 6, 2<weight>)
        ] |> testSequenced


    let ``On golang rsm`` =

        let mkTest = gllTestCase GolangRSM.golangSrc

        testList "On golang rsm" [
            mkTest GolangRSM.functionSample (0, 129, 63, 319, 128, 0<weight>)
            mkTest GolangRSM.expressionSample (0, 183, 163, 527, 182, 0<weight>)
            mkTest GolangRSM.cycleSample (0, 199, 165, 561, 198, 0<weight>)
            mkTest "func f() int {}" (2, 15, 6, 38, 16, 0<weight>)
            //mkTest "fnc f() int {}" (2, 15, 6, 38, 16, 1<weight>)
            // mkTest "fnc f() int {" (2, 15, 6, 38, 16, 2<weight>)
            // mkTest "fnc f() {}" (2, 14, 6, 36, 15, 4<weight>)
            // mkTest "unc f() int {}" (2, 15, 6, 38, 16, 1<weight>)
            mkTest "" (1, 0, 1, 1, 0, 0<weight>)
            // mkTest "x" (0, 1, 1, 1, 0, 1<weight>)
            // mkTest "func f() int { return 1 }" (1, 28, 19, 75, 29, 1<weight>)
            // mkTest "fu\nnc f() int { return 1; } "  (1, 28, 13, 69, 28, 1<weight>)
        ] |> testSequenced

    let ``On Dyck lang`` =

        let src () =
            let S = nt "S"
            [
                S => Epsilon +|+ t '(' ** S ** t ')' ** S
            ] |> build [' ']

        let mkTest = gllTestCase src

        testList "On Dyck lang" [
            mkTest "" (1, 0, 1, 1, 0, 0<weight>)
            mkTest "()" (2, 2, 3, 9, 3, 0<weight>)
            mkTest "()()" (3, 4, 5, 17, 6, 0<weight>)
            mkTest "()(())" (4, 6, 7, 25, 9, 0<weight>)
            mkTest "(()())" (4, 6, 7, 25, 9, 0<weight>)
            mkTest "(" (1, 3, 2, 9, 3, 1<weight>)
            mkTest ")" (0, 1, 1, 1, 0, 1<weight>)
            mkTest "(()" (3, 5, 5, 20, 9, 1<weight>)
            mkTest "(()()" (5, 7, 8, 30, 12, 1<weight>)
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
            mkTest "{{[[]]}}()" (6, 10, 16, 46, 15, 0<weight>)
            mkTest "{[]}{(())()}" (7, 12, 19, 55, 18, 0<weight>)
            // mkTest "{]" (2, 7, 5, 23, 11, 2<weight>)
            // mkTest "[(}" (3, 26, 14, 80, 51, 3<weight>)
            // mkTest "[(])" (4, 14, 11, 47, 21, 2<weight>)
        ]

    let ``On Dyck lang  by regexp`` =

        let src () =
            let S = nt "S"
            [
                S => many(t '(' ** S ** t ')')
            ] |> build [' ']

        let mkTest = gllTestCase src

        testList "On Dyck lang by regexp" [
            mkTest "" (1, 0, 1, 1, 0, 0<weight>)
            mkTest "()" (1, 2, 2, 6, 2, 0<weight>)
            mkTest "()()" (2, 4, 3, 13, 5, 0<weight>)
            mkTest "()(())" (2, 6, 4, 18, 7, 0<weight>)
            mkTest "(()())" (2, 6, 4, 18, 7, 0<weight>)
            mkTest "(" (1, 3, 2, 7, 2, 1<weight>)
            mkTest ")" (0, 1, 1, 1, 0, 1<weight>)
            mkTest "(()" (2, 5, 4, 16, 7, 1<weight>)
            mkTest "(()()" (3, 9, 6, 30, 16, 1<weight>)
        ]

    let ``On multi Dyck lang by regexp`` =

        let src () =
            let S = nt "S"
            [
                S => many (t '(' ** S ** t ')' +|+  t '{' ** S ** t '}' +|+ t '[' ** S ** t ']')
            ] |> build [' ']

        let mkTest = gllTestCase src

        testList "On multi Dyck lang by regexp" [
            mkTest "" (1, 0, 1, 1, 0, 0<weight>)
            mkTest "()" (1, 2, 2, 6, 2, 0<weight>)
            mkTest "()()" (2, 4, 3, 13, 5, 0<weight>)
            mkTest "()(())" (2, 6, 4, 18, 7, 0<weight>)
            mkTest "(()())" (2, 6, 4, 18, 7, 0<weight>)
            mkTest "(" (1, 3, 2, 7, 2, 1<weight>)
            mkTest ")" (0, 1, 1, 1, 0, 1<weight>)
            mkTest "(()" (2, 5, 4, 16, 7, 1<weight>)
            mkTest "(()()" (3, 9, 6, 30, 16, 1<weight>)
            mkTest "{{[[]]}}()" (2, 10, 6, 28, 11, 0<weight>)
            mkTest "{[]}{(())()}" (3, 12, 7, 35, 14, 0<weight>)
            // mkTest "{]" (3, 11, 6, 27, 13, 2<weight>)
            // mkTest "[(}" (3, 25, 7, 63, 44, 3<weight>)
            // mkTest "[(])" (3, 17, 8, 43, 24, 2<weight>)
            mkTest "[ { [ ] ] " (3, 16, 7, 45, 23, 1<weight>)
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
            mkTest "" (0, 1, 1, 1, 0, 1<weight>)
            mkTest "a" (0, 1, 1, 1, 0, 0<weight>)
            mkTest "a " (0, 2, 1, 3, 1, 0<weight>)
            mkTest "aa" (0, 2, 3, 5, 1, 0<weight>)
            mkTest "a a" (0, 3, 3, 7, 2, 0<weight>)
            mkTest "aaa" (0, 3, 5, 9, 2, 0<weight>)
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

