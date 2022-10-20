module Tests.LinearGraphReader

open System.IO
open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.InputGraph
open Expecto
open CFPQ_GLL.Common
open FSharpx.Collections
open Tests.InputGraph


type Config =
    | LinearGraph
    | LinearGraphWithDeletions
    | LinearGraphWithInsertions
    | LinearGraphWithDeletionsAndInsertions

let mkLinearGraph
    (onText: string -> string)
    (terminalsMapping: Dictionary<char, int<terminalSymbol>>)
    (config: Config)
    (inputString: string) =

    let getTerminal symbol =
        if terminalsMapping.ContainsKey(symbol) |> not then failwith $"Unexpected symbol[{symbol}] in input file"
        terminalsMapping[symbol]

    let terminals = inputString |> onText |> Seq.toArray |> Array.map getTerminal
    let vertices = Array.init (terminals.Length + 1) id
    let allLanguageTerminals = terminalsMapping.Values |> HashSet
    //allLanguageTerminals.RemoveWhere (fun x -> x = terminalsMapping.[' '] || x = terminalsMapping.['\r']|| x = terminalsMapping.['\n'])

    let mkVertex i = LanguagePrimitives.Int32WithMeasure<inputGraphVertex> i
    let mkEdge v1 t v2 = TerminalEdge(mkVertex v1, t, mkVertex v2)
    let mkErrorEdge v1 t v2 = ErrorTerminalEdge(mkVertex v1, t, mkVertex v2)
    let mkLinearEdge v t = mkEdge v t (v + 1)
    let mkLinearEpsilonEdge v = ErrorEpsilonEdge(mkVertex v, mkVertex (v + 1))

    let deletionsEdges = Array.mapi (fun i _ -> mkLinearEpsilonEdge i) terminals
    let insertionEdges =
        let createInsertionsEdgesForVertex i = Seq.map (fun t -> mkErrorEdge i t i) allLanguageTerminals
        Array.map createInsertionsEdgesForVertex vertices |> Seq.concat |> Seq.toArray

    let linearGraphEdges = Array.mapi mkLinearEdge terminals
    let linearGraphWithDeletionsEdges = Array.append linearGraphEdges deletionsEdges
    let linearGraphWithInsertionsEdges = Array.append linearGraphEdges insertionEdges
    let linearGraphWithDeletionsAndInsertions = Array.append linearGraphWithDeletionsEdges insertionEdges

    match config with
    | LinearGraph -> InputGraph(linearGraphEdges, false)
    | LinearGraphWithDeletions -> InputGraph(linearGraphWithDeletionsEdges, true)
    | LinearGraphWithInsertions -> InputGraph(linearGraphWithInsertionsEdges, true)
    | LinearGraphWithDeletionsAndInsertions -> InputGraph(linearGraphWithDeletionsAndInsertions, true)


let readLinearGraph
    (onText: string -> string)
    (terminalsMapping: Dictionary<char, int<terminalSymbol>>)
    (config: Config)
    (filePath: string) =

    File.ReadAllText(filePath) |> mkLinearGraph onText terminalsMapping config

let ``Linear graph creating tests`` =

    let aTerminal = 0<terminalSymbol>
    let bTerminal = 1<terminalSymbol>

    let terminalsMapping = Dictionary<char,int<terminalSymbol>>()
    terminalsMapping.Add('a', aTerminal)
    terminalsMapping.Add('b', bTerminal)

    let assertGraphEqual (actual: InputGraph) (expected: InputGraph) =
        actual.ToDot (1,"actual.dot")
        expected.ToDot(1, "expected.dot")
        let actualVertices = actual.AllVertices()
        let expectedVertices = expected.AllVertices()
        Expect.sequenceEqual actualVertices expectedVertices "Vertices should be equal"
        let sort (arr: ResizeArray<InputGraphEdge>) =
            ResizeArray.sortBy (fun (x: InputGraphEdge) -> x.TargetVertex.GetHashCode() * 13 + x.TerminalSymbol.GetHashCode()) arr
        for v in actualVertices do
            let actualTerminalEdges = actual.OutgoingTerminalEdges v
            let expectedTerminalEdges = expected.OutgoingTerminalEdges v
            sort actualTerminalEdges
            sort expectedTerminalEdges
            Expect.sequenceEqual actualTerminalEdges expectedTerminalEdges "Edges should be equal"
        for v in expectedVertices do
            let actualTerminalEdges = actual.OutgoingTerminalEdges v
            let expectedTerminalEdges = expected.OutgoingTerminalEdges v
            sort actualTerminalEdges
            sort expectedTerminalEdges
            Expect.sequenceEqual actualTerminalEdges expectedTerminalEdges "Edges should be equal"

    let aInputString = "a"
    let abInputString = "ab"
    let abaInputString = "aba"
    let abcInputString = "abc"

    let mkGraph = mkLinearGraph id terminalsMapping

    let mkTest config edges input () =
        let enableErrorRecovering =
            match config with
            | LinearGraph -> false
            | _ -> true
        let expected = InputGraph(edges, enableErrorRecovering)

        let actual = mkGraph config input

        assertGraphEqual actual expected


    let ``Fail on incorrect string`` =
        testList "Fail on incorrect string" [
            testCase "abc" (fun () ->
                Expect.throws (fun () ->
                    mkGraph LinearGraph abcInputString |> ignore
                ) "Fail on incorrect string"
            )
        ] |> testSequenced

    let ``Simple linear graph tests`` =
        let mkLinearGraphTest = mkTest LinearGraph

        let aTest =
            mkLinearGraphTest
            <| [|TerminalEdge(0<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)|]
            <| aInputString |> testCase "a"

        let abTest =
            mkLinearGraphTest
            <| [|
                TerminalEdge(0<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)
                TerminalEdge(1<inputGraphVertex>, bTerminal, 2<inputGraphVertex>)
            |]
            <| abInputString |> testCase "ab"

        let abaTest =
            mkLinearGraphTest
            <| [|
                TerminalEdge(0<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)
                TerminalEdge(1<inputGraphVertex>, bTerminal, 2<inputGraphVertex>)
                TerminalEdge(2<inputGraphVertex>, aTerminal, 3<inputGraphVertex>)
            |]
            <| abaInputString |> testCase "aba"

        testList "Simple linear graph tests" [
            aTest
            abTest
            abaTest
        ] |> testSequenced

    let ``Linear graph with deletions tests`` =
        let mkLinearGraphTest = mkTest LinearGraphWithDeletions

        let aTest =
            mkLinearGraphTest
            <| [|
                TerminalEdge(0<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)

                ErrorEpsilonEdge(0<inputGraphVertex>, 1<inputGraphVertex>)
            |]
            <| aInputString |> testCase "a"

        let abTest =
            mkLinearGraphTest
            <| [|
                TerminalEdge(0<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)
                TerminalEdge(1<inputGraphVertex>, bTerminal, 2<inputGraphVertex>)

                ErrorEpsilonEdge(0<inputGraphVertex>, 1<inputGraphVertex>)
                ErrorEpsilonEdge(1<inputGraphVertex>, 2<inputGraphVertex>)
            |]
            <| abInputString |> testCase "ab"

        let abaTest =
            mkLinearGraphTest
            <| [|
                TerminalEdge(0<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)
                TerminalEdge(1<inputGraphVertex>, bTerminal, 2<inputGraphVertex>)
                TerminalEdge(2<inputGraphVertex>, aTerminal, 3<inputGraphVertex>)

                ErrorEpsilonEdge(0<inputGraphVertex>, 1<inputGraphVertex>)
                ErrorEpsilonEdge(1<inputGraphVertex>, 2<inputGraphVertex>)
                ErrorEpsilonEdge(2<inputGraphVertex>, 3<inputGraphVertex>)
            |]
            <| abaInputString |> testCase "aba"

        testList "Linear graph with deletions tests" [
            aTest
            abTest
            abaTest
        ] |> testSequenced

    let ``Linear graph with insertions tests`` =
        let mkLinearGraphTest = mkTest LinearGraphWithInsertions

        let aTest =
            mkLinearGraphTest
            <| [|
                TerminalEdge(0<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)

                ErrorTerminalEdge(0<inputGraphVertex>, aTerminal, 0<inputGraphVertex>)
                ErrorTerminalEdge(0<inputGraphVertex>, bTerminal, 0<inputGraphVertex>)
                ErrorTerminalEdge(1<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)
                ErrorTerminalEdge(1<inputGraphVertex>, bTerminal, 1<inputGraphVertex>)
            |]
            <| aInputString |> testCase "a"

        let abTest =
            mkLinearGraphTest
            <| [|
                TerminalEdge(0<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)
                TerminalEdge(1<inputGraphVertex>, bTerminal, 2<inputGraphVertex>)

                ErrorTerminalEdge(0<inputGraphVertex>, aTerminal, 0<inputGraphVertex>)
                ErrorTerminalEdge(0<inputGraphVertex>, bTerminal, 0<inputGraphVertex>)
                ErrorTerminalEdge(1<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)
                ErrorTerminalEdge(1<inputGraphVertex>, bTerminal, 1<inputGraphVertex>)
                ErrorTerminalEdge(2<inputGraphVertex>, aTerminal, 2<inputGraphVertex>)
                ErrorTerminalEdge(2<inputGraphVertex>, bTerminal, 2<inputGraphVertex>)
            |]
            <| abInputString |> testCase "ab"

        let abaTest =
            mkLinearGraphTest
            <| [|
                TerminalEdge(0<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)
                TerminalEdge(1<inputGraphVertex>, bTerminal, 2<inputGraphVertex>)
                TerminalEdge(2<inputGraphVertex>, aTerminal, 3<inputGraphVertex>)

                ErrorTerminalEdge(0<inputGraphVertex>, aTerminal, 0<inputGraphVertex>)
                ErrorTerminalEdge(0<inputGraphVertex>, bTerminal, 0<inputGraphVertex>)
                ErrorTerminalEdge(1<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)
                ErrorTerminalEdge(1<inputGraphVertex>, bTerminal, 1<inputGraphVertex>)
                ErrorTerminalEdge(2<inputGraphVertex>, aTerminal, 2<inputGraphVertex>)
                ErrorTerminalEdge(2<inputGraphVertex>, bTerminal, 2<inputGraphVertex>)
                ErrorTerminalEdge(3<inputGraphVertex>, aTerminal, 3<inputGraphVertex>)
                ErrorTerminalEdge(3<inputGraphVertex>, bTerminal, 3<inputGraphVertex>)
            |]
            <| abaInputString |> testCase "aba"

        testList "Linear graph with insertions tests" [
            aTest
            abTest
            abaTest
        ] |> testSequenced

    let ``Linear graph with deletions and insertions tests`` =
        let mkLinearGraphTest = mkTest LinearGraphWithDeletionsAndInsertions

        let aTest =
            mkLinearGraphTest
            <| [|
                TerminalEdge(0<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)

                ErrorTerminalEdge(0<inputGraphVertex>, aTerminal, 0<inputGraphVertex>)
                ErrorTerminalEdge(0<inputGraphVertex>, bTerminal, 0<inputGraphVertex>)
                ErrorTerminalEdge(1<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)
                ErrorTerminalEdge(1<inputGraphVertex>, bTerminal, 1<inputGraphVertex>)

                ErrorEpsilonEdge(0<inputGraphVertex>, 1<inputGraphVertex>)
            |]
            <| aInputString |> testCase "a"

        let abTest =
            mkLinearGraphTest
            <| [|
                TerminalEdge(0<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)
                TerminalEdge(1<inputGraphVertex>, bTerminal, 2<inputGraphVertex>)

                ErrorTerminalEdge(0<inputGraphVertex>, aTerminal, 0<inputGraphVertex>)
                ErrorTerminalEdge(0<inputGraphVertex>, bTerminal, 0<inputGraphVertex>)
                ErrorTerminalEdge(1<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)
                ErrorTerminalEdge(1<inputGraphVertex>, bTerminal, 1<inputGraphVertex>)
                ErrorTerminalEdge(2<inputGraphVertex>, aTerminal, 2<inputGraphVertex>)
                ErrorTerminalEdge(2<inputGraphVertex>, bTerminal, 2<inputGraphVertex>)

                ErrorEpsilonEdge(0<inputGraphVertex>, 1<inputGraphVertex>)
                ErrorEpsilonEdge(1<inputGraphVertex>, 2<inputGraphVertex>)
            |]
            <| abInputString |> testCase "ab"

        let abaTest =
            mkLinearGraphTest
            <| [|
                TerminalEdge(0<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)
                TerminalEdge(1<inputGraphVertex>, bTerminal, 2<inputGraphVertex>)
                TerminalEdge(2<inputGraphVertex>, aTerminal, 3<inputGraphVertex>)

                ErrorTerminalEdge(0<inputGraphVertex>, aTerminal, 0<inputGraphVertex>)
                ErrorTerminalEdge(0<inputGraphVertex>, bTerminal, 0<inputGraphVertex>)
                ErrorTerminalEdge(1<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)
                ErrorTerminalEdge(1<inputGraphVertex>, bTerminal, 1<inputGraphVertex>)
                ErrorTerminalEdge(2<inputGraphVertex>, aTerminal, 2<inputGraphVertex>)
                ErrorTerminalEdge(2<inputGraphVertex>, bTerminal, 2<inputGraphVertex>)
                ErrorTerminalEdge(3<inputGraphVertex>, aTerminal, 3<inputGraphVertex>)
                ErrorTerminalEdge(3<inputGraphVertex>, bTerminal, 3<inputGraphVertex>)

                ErrorEpsilonEdge(0<inputGraphVertex>, 1<inputGraphVertex>)
                ErrorEpsilonEdge(1<inputGraphVertex>, 2<inputGraphVertex>)
                ErrorEpsilonEdge(2<inputGraphVertex>, 3<inputGraphVertex>)
            |]
            <| abaInputString |> testCase "aba"

        testList "Linear graph with deletions and insertions tests" [
            aTest
            abTest
            abaTest
        ] |> testSequenced

    testList "Linear graph creating tests" [
        ``Fail on incorrect string``
        ``Simple linear graph tests``
        ``Linear graph with deletions tests``
        ``Linear graph with insertions tests``
        ``Linear graph with deletions and insertions tests``
    ] |> testSequenced
