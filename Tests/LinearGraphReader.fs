module Tests.LinearGraphReader

open System.IO
open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.InputGraph
open Expecto
open CFPQ_GLL.Common
open FSharpx.Collections
open Tests.InputGraph






let mkLinearGraph
    (onText: string -> string)
    (terminalsMapping: Dictionary<char, int<terminalSymbol>>)
    (inputString: string) =

    let getTerminal symbol =
        if terminalsMapping.ContainsKey(symbol) |> not then failwith $"Unexpected symbol[{symbol}] in input file"
        terminalsMapping[symbol]

    let terminals = inputString |> onText |> Seq.toArray |> Array.map getTerminal
    let mkVertex i = LanguagePrimitives.Int32WithMeasure<inputGraphVertex> i
    let mkEdge v1 t v2 = DefaultTerminalEdge(mkVertex v1, t, mkVertex v2)
    let mkLinearEdge v t = mkEdge v t (v + 1)
    let linearGraphEdgesWithoutFinal = Array.mapi mkLinearEdge terminals
    let finalVertex = mkVertex linearGraphEdgesWithoutFinal.Length
    let linearGraphEdges = Array.append linearGraphEdgesWithoutFinal [|DefaultTerminalEdge(finalVertex, EOF, finalVertex)|]

    InputGraph(linearGraphEdges, true)



let readLinearGraph
    (onText: string -> string)
    (terminalsMapping: Dictionary<char, int<terminalSymbol>>)
    (filePath: string) =

    File.ReadAllText(filePath) |> mkLinearGraph onText terminalsMapping

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

    let mkTest edges input () =

        let expected = InputGraph(edges, true)
        let actual = mkGraph input
        assertGraphEqual actual expected


    let ``Fail on incorrect string`` =
        testList "Fail on incorrect string" [
            testCase "abc" (fun () ->
                Expect.throws (fun () ->
                    mkGraph abcInputString |> ignore
                ) "Fail on incorrect string"
            )
        ] |> testSequenced

    let ``Simple linear graph tests`` = // TODO: add EOF
        let mkLinearGraphTest = mkTest

        let aTest =
            mkLinearGraphTest
            <| [|DefaultTerminalEdge(0<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)|]
            <| aInputString |> testCase "a"

        let abTest =
            mkLinearGraphTest
            <| [|
                DefaultTerminalEdge(0<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)
                DefaultTerminalEdge(1<inputGraphVertex>, bTerminal, 2<inputGraphVertex>)
            |]
            <| abInputString |> testCase "ab"

        let abaTest =
            mkLinearGraphTest
            <| [|
                DefaultTerminalEdge(0<inputGraphVertex>, aTerminal, 1<inputGraphVertex>)
                DefaultTerminalEdge(1<inputGraphVertex>, bTerminal, 2<inputGraphVertex>)
                DefaultTerminalEdge(2<inputGraphVertex>, aTerminal, 3<inputGraphVertex>)
            |]
            <| abaInputString |> testCase "aba"

        testList "Simple linear graph tests" [
            aTest
            abTest
            abaTest
        ] |> testSequenced


    testList "Linear graph creating tests" [
        ``Fail on incorrect string``
        ``Simple linear graph tests``
    ] |> testSequenced
