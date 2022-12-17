module Tests.ErrorRecoveringTest


open System
open System.Collections.Generic
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

let run
    (graph: InputGraph)
    (q: RSM)
    (terminalMapping: Dictionary<char, int<terminalSymbol>>)
    (nonTerminalMapping: Dictionary<string, IRsmState>)
    =
    let startV = 0<inputGraphVertex>
    let finalV = LanguagePrimitives.Int32WithMeasure<inputGraphVertex>(graph.NumberOfVertices() - 1)

    let reverseDict (dict: Dictionary<'a, 'b>) =
        let x = dict |> Dictionary.KeyCollection |> Seq.map (fun k -> dict[k], k)
        let result = Dictionary()
        for k, v in x do
            result.Add(k, v)
        result

    let reversedTerminalMapping =
        let result = reverseDict terminalMapping
        result.Add(-1<terminalSymbol>, 'ε')
        result

    let reversedNonTerminalMapping = reverseDict nonTerminalMapping

    graph.ToDot (0, "graph.dot")
    printfn "Graph is saved to graph.dot"

    let startVertices,mapping = graph.ToCfpqCoreGraph startV
    let finalVertices = mapping[finalV]

    let start = DateTime.Now
    let result = GLL.errorRecoveringEval finalVertices startVertices q GLL.AllPaths
    printfn $"Execution time: {(DateTime.Now - start).TotalMilliseconds}"

    match result with
    | GLL.QueryResult.MatchedRanges _ ->
        let sppf = q.OriginalStartState.NonTerminalNodes.ToArray()
        let root = sppf |> Array.filter (fun n -> finalVertices = n.RightPosition && startVertices = n.LeftPosition) |> Array.minBy(fun n -> n.Distance)
        let distances = sppf |> Array.map (fun n -> n.Distance) |> Array.sort |> Array.toList
        printfn $"Distances: {distances}"

        let actual = TriplesStoredSPPF([|root|], Dictionary())
        actual.ToDot (reversedTerminalMapping, reversedNonTerminalMapping,  "sppf.dot")
        printfn "SPPF is saved to sppf.dot"

        dumpResultToConsole actual
    | _ -> failwith "Unexpected result."

let gllTestCase
    (query: RSM)
    (graphMaker: string -> InputGraph)
    (input: string)
    (expected: Dictionary<int,TriplesStoredSPPFNode> * ResizeArray<int * int> * int<distance>[]) =
    let graph = graphMaker input
    let startV = 0<inputGraphVertex>
    let finalV = LanguagePrimitives.Int32WithMeasure<inputGraphVertex>(graph.NumberOfVertices() - 1)
    testCase input <| fun () -> runErrorRecoveringGLLAndCheckResult input graph startV finalV query expected



let gllTestCaseFromGrammar (grammar: seq<Rule>) =
    let query, mapping, _ = build [] grammar
    let graphMaker = mkLinearGraph id mapping
    gllTestCase query graphMaker

let ``Error recovering tests`` =

//    let `` On mini ML RSM`` =
//        let rsm () =
//            let rsm, terminalMapping =
//                let Num = nt "Num"
//                let Arithmetic = nt "Arithmetic"
//                let Var = nt "Var"
//                let Atom = nt "Atom"
//                let Prod = nt "Prod"
//                let Program = nt "Program"
//                let Expr = nt "Expr"
//                let Compare = nt "Compare"
//                let space = t " " +|+ t "\n" +|+ t "\r" +|+ t "\t"
//                let ws x = many space ** x
//
//                [
//                    Program => Expr // many Expr ** many space
//
//                    Num  => ws(([|1..9|] |> Array.map (string >> t) |> Array.reduce ( +|+ ))
//                             ** many ([|0..9|] |> Array.map (string >> t) |> Array.reduce ( +|+ )))
//                    Var  => ws(([|'a'..'z'|] |> Array.map (string >> t) |> Array.reduce ( +|+ ))
//                            ** many (    ([|'a'..'z'|] |> Array.map (string >> t) |> Array.reduce ( +|+ ))
//                                     +|+ ([|0..9|] |> Array.map (string >> t) |> Array.reduce ( +|+ ))))
//                    Atom => (* Num
//                            +|+ *) Var //(Var ** many (space ** Atom))
//                            //+|+ (ws (t "(") ** Expr ** ws (t ")"))
//                            //+|+ (Var ** t "[" ** Expr ** t"]")
//                    //Prod => nonemptyList Atom (ws (t "*" +|+ t "/"))
//                    //Arithmetic => nonemptyList Prod (ws(t "+" +|+ t "-"))
//                    //Compare => nonemptyList Arithmetic (ws ((opt (t"!") ** t "=") +|+ ((t ">" +|+ t "<") ** opt (t "="))))
//                    Expr => Atom +|+
//                        //Compare
//                        (**|* *) (ws(literal "let") (* ** opt(ws(literal "rec"))*) ** space ** Var ** many (space ** Var) ** ws(t "=") ** Expr ** ws(literal "in") ** Expr)
//                        //+|+ (ws(literal "if") ** Expr ** ws(literal "then") ** Expr ** opt (ws(literal "else") ** Expr))
//                        //+|+ (ws(literal "while") ** Expr ** ws(literal "do") ** Expr)
//                ]
//                |> build []
//            rsm, terminalMapping
//
//        let incorrectInsertions =
//            let testName = "Incorrect insertion"
//            testCase testName <| fun () ->
//                let q,terminalMapping = rsm ()
//                let graph = mkLinearGraph id terminalMapping "lt x = y in x"
//                let startV = 0<inputGraphVertex>
//                let finishV = 13<inputGraphVertex>
//                let expected =
//                    let nodes = Dictionary<_,_>()
//                    nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
//                    nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
//                    nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
//                    nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
//                    nodes.Add(4, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,1<terminalSymbol>,2<inputGraphVertex>))
//                    nodes.Add(5, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
//                    nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,2<terminalSymbol>,1<inputGraphVertex>))
//
//                    let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (2,5); (5,6); |])
//                    let distances = [|0<distance>|]
//                    (nodes,edges,distances)
//
//                runErrorRecoveringGLLAndCheckResult testName graph startV finishV q expected
//
//        testList "Mini ML" [
//            incorrectInsertions
//        ] |> testSequenced

    let ``On ab RSM`` =

        let abRSM () =
            // S -> ab
            let startState = RsmState(true, false) :> IRsmState
            let state = RsmState(false, false) :> IRsmState
            let finalState = RsmState(false, true) :> IRsmState
            let box = RSMBox()
            box.AddState startState
            box.AddState state
            box.AddState finalState
            startState.AddTerminalEdge(1<terminalSymbol>, state)
            state.AddTerminalEdge(2<terminalSymbol>, finalState)
            RSM([|box|], box)

        let graphMaker = mkLinearGraph id (
            Dictionary([KeyValuePair('a',1<terminalSymbol>); KeyValuePair('b', 2<terminalSymbol>)])
        )

        let abRsmTestCase = gllTestCase (abRSM()) graphMaker

        let ab =
            let expected =
                let nodes = Dictionary<_,_>()
                nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(4, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,1<terminalSymbol>,2<inputGraphVertex>))
                nodes.Add(5, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,2<terminalSymbol>,1<inputGraphVertex>))

                let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (2,5); (5,6);|])
                let distances = [|0<distance>|]
                (nodes,edges,distances)

            abRsmTestCase "ab" expected

        let abb =
            let expected =
                let nodes = Dictionary<_,_>()
                nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,1<rsmState>))
                nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,2<rsmState>))
                nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,1<terminalSymbol>,3<inputGraphVertex>))
                nodes.Add(7, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,2<rsmState>,1<rsmState>))
                nodes.Add(8, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,2<terminalSymbol>,2<inputGraphVertex>))
                nodes.Add(9, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,1<rsmState>,1<rsmState>))
                nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,-1<terminalSymbol>,1<inputGraphVertex>))

                let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (4,7); (7,8); (2,9); (9,10); |])
                let distances = [|1<distance>|]
                (nodes,edges,distances)

            abRsmTestCase "abb" expected


        let bb =
            let expected =
                let nodes = Dictionary<_,_>()
                nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,1<rsmState>))
                nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,2<rsmState>))
                nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,1<terminalSymbol>,0<inputGraphVertex>))
                nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,2<rsmState>,1<rsmState>))
                nodes.Add(8, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,2<terminalSymbol>,2<inputGraphVertex>))
                nodes.Add(9, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,1<rsmState>,1<rsmState>))
                nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,-1<terminalSymbol>,1<inputGraphVertex>))


                let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (4,7); (7,8); (2,9)
                                             (9,10);|])
                let distances = [|2<distance>|]
                (nodes,edges,distances)

            abRsmTestCase "bb" expected

        testList "On ab RSM" [
            ab
            abb
            bb
        ] |> testSequenced

    let ``On ca*b* RSM`` =

        let caStarBStarRSM () =
            // S -> ca*b*
            let startState = RsmState(true, false) :> IRsmState
            let aState = RsmState(false, true) :> IRsmState
            let bState = RsmState(false, true) :> IRsmState
            let box = RSMBox()
            box.AddState startState
            box.AddState aState
            box.AddState bState
            startState.AddTerminalEdge(3<terminalSymbol>, aState)
            aState.AddTerminalEdge(1<terminalSymbol>, aState)
            aState.AddTerminalEdge(2<terminalSymbol>, bState)
            bState.AddTerminalEdge(2<terminalSymbol>, bState)
            RSM([|box|], box)

        let graphMaker = mkLinearGraph id (
            Dictionary([KeyValuePair('a',1<terminalSymbol>); KeyValuePair('b', 2<terminalSymbol>); KeyValuePair('c', 3<terminalSymbol>)])
        )

        let caStarBStarRSMTestCase = gllTestCase (caStarBStarRSM ()) graphMaker

        let cb =
            let expected =
                let nodes = Dictionary<_,_>()
                nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(4, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,3<terminalSymbol>,2<inputGraphVertex>))
                nodes.Add(5, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,2<terminalSymbol>,1<inputGraphVertex>))

                let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (2,5); (5,6);|])
                let distances = [|0<distance>|]
                (nodes,edges,distances)

            caStarBStarRSMTestCase "cb" expected


        let ca =
            let expected =
                let nodes = Dictionary<_,_>()
                nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,1<rsmState>))
                nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(4, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,3<terminalSymbol>,2<inputGraphVertex>))
                nodes.Add(5, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,1<rsmState>,1<rsmState>))
                nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,1<terminalSymbol>,1<inputGraphVertex>))

                let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (2,5); (5,6);|])
                let distances = [|0<distance>|]
                (nodes,edges,distances)

            caStarBStarRSMTestCase "ca" expected

        let caaabbb =
            let expected =
                let nodes = Dictionary<_,_>()
                nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,1<rsmState>))
                nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,1<rsmState>))
                nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(6, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,2<rsmState>))
                nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (5<inputGraphVertex>,2<rsmState>))
                nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,5<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(10, TriplesStoredSPPFNode.IntermediateNode (6<inputGraphVertex>,2<rsmState>))
                nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,6<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(12, TriplesStoredSPPFNode.IntermediateNode (7<inputGraphVertex>,2<rsmState>))
                nodes.Add(13, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,7<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(14, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,3<terminalSymbol>,7<inputGraphVertex>))
                nodes.Add(15, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,6<inputGraphVertex>,2<rsmState>,2<rsmState>))
                nodes.Add(16, TriplesStoredSPPFNode.TerminalNode (7<inputGraphVertex>,1<terminalSymbol>,6<inputGraphVertex>))
                nodes.Add(17, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,5<inputGraphVertex>,2<rsmState>,2<rsmState>))
                nodes.Add(18, TriplesStoredSPPFNode.TerminalNode (6<inputGraphVertex>,1<terminalSymbol>,5<inputGraphVertex>))
                nodes.Add(19, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,4<inputGraphVertex>,2<rsmState>,2<rsmState>))
                nodes.Add(20, TriplesStoredSPPFNode.TerminalNode (5<inputGraphVertex>,1<terminalSymbol>,4<inputGraphVertex>))
                nodes.Add(21, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,2<rsmState>,1<rsmState>))
                nodes.Add(22, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,2<terminalSymbol>,3<inputGraphVertex>))
                nodes.Add(23, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,1<rsmState>,1<rsmState>))
                nodes.Add(24, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,2<terminalSymbol>,2<inputGraphVertex>))
                nodes.Add(25, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,1<rsmState>,1<rsmState>))
                nodes.Add(26, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,2<terminalSymbol>,1<inputGraphVertex>))


                let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (6,7); (7,8); (8,9); (9,10)
                                             (10,11); (11,12); (12,13); (13,14); (12,15); (15,16); (10,17); (17,18)
                                             (8,19); (19,20); (6,21); (21,22); (4,23); (23,24); (2,25); (25,26); |])
                let distances = [|0<distance>|]
                (nodes,edges,distances)

            caStarBStarRSMTestCase "caaabbb" expected

        let caaacbbb =
            let expected =
                let nodes = Dictionary<_,_>()
                nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,1<rsmState>))
                nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,1<rsmState>))
                nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(6, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,2<rsmState>))
                nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (5<inputGraphVertex>,2<rsmState>))
                nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,5<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(10, TriplesStoredSPPFNode.IntermediateNode (6<inputGraphVertex>,2<rsmState>))
                nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,6<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(12, TriplesStoredSPPFNode.IntermediateNode (7<inputGraphVertex>,2<rsmState>))
                nodes.Add(13, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,7<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(14, TriplesStoredSPPFNode.IntermediateNode (8<inputGraphVertex>,2<rsmState>))
                nodes.Add(15, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,8<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(16, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,3<terminalSymbol>,8<inputGraphVertex>))
                nodes.Add(17, TriplesStoredSPPFNode.RangeNode (8<inputGraphVertex>,7<inputGraphVertex>,2<rsmState>,2<rsmState>))
                nodes.Add(18, TriplesStoredSPPFNode.TerminalNode (8<inputGraphVertex>,1<terminalSymbol>,7<inputGraphVertex>))
                nodes.Add(19, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,6<inputGraphVertex>,2<rsmState>,2<rsmState>))
                nodes.Add(20, TriplesStoredSPPFNode.TerminalNode (7<inputGraphVertex>,1<terminalSymbol>,6<inputGraphVertex>))
                nodes.Add(21, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,5<inputGraphVertex>,2<rsmState>,2<rsmState>))
                nodes.Add(22, TriplesStoredSPPFNode.TerminalNode (6<inputGraphVertex>,1<terminalSymbol>,5<inputGraphVertex>))
                nodes.Add(23, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,4<inputGraphVertex>,2<rsmState>,2<rsmState>))
                nodes.Add(24, TriplesStoredSPPFNode.TerminalNode (5<inputGraphVertex>,-1<terminalSymbol>,4<inputGraphVertex>))
                nodes.Add(25, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,2<rsmState>,1<rsmState>))
                nodes.Add(26, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,2<terminalSymbol>,3<inputGraphVertex>))
                nodes.Add(27, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,1<rsmState>,1<rsmState>))
                nodes.Add(28, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,2<terminalSymbol>,2<inputGraphVertex>))
                nodes.Add(29, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,1<rsmState>,1<rsmState>))
                nodes.Add(30, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,2<terminalSymbol>,1<inputGraphVertex>))


                let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (6,7); (7,8); (8,9); (9,10)
                                             (10,11); (11,12); (12,13); (13,14); (14,15); (15,16); (14,17); (17,18)
                                             (12,19); (19,20); (10,21); (21,22); (8,23); (23,24); (6,25); (25,26)
                                             (4,27); (27,28); (2,29); (29,30); |])
                let distances = [|1<distance>|]
                (nodes,edges,distances)

            caStarBStarRSMTestCase "caaacbbb" expected

        let caaabbcb =
            let expected =
                let nodes = Dictionary<_,_>()
                nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,1<rsmState>))
                nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,1<rsmState>))
                nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(6, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,1<rsmState>))
                nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (5<inputGraphVertex>,2<rsmState>))
                nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,5<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(10, TriplesStoredSPPFNode.IntermediateNode (6<inputGraphVertex>,2<rsmState>))
                nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,6<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(12, TriplesStoredSPPFNode.IntermediateNode (7<inputGraphVertex>,2<rsmState>))
                nodes.Add(13, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,7<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(14, TriplesStoredSPPFNode.IntermediateNode (8<inputGraphVertex>,2<rsmState>))
                nodes.Add(15, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,8<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(16, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,3<terminalSymbol>,8<inputGraphVertex>))
                nodes.Add(17, TriplesStoredSPPFNode.RangeNode (8<inputGraphVertex>,7<inputGraphVertex>,2<rsmState>,2<rsmState>))
                nodes.Add(18, TriplesStoredSPPFNode.TerminalNode (8<inputGraphVertex>,1<terminalSymbol>,7<inputGraphVertex>))
                nodes.Add(19, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,6<inputGraphVertex>,2<rsmState>,2<rsmState>))
                nodes.Add(20, TriplesStoredSPPFNode.TerminalNode (7<inputGraphVertex>,1<terminalSymbol>,6<inputGraphVertex>))
                nodes.Add(21, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,5<inputGraphVertex>,2<rsmState>,2<rsmState>))
                nodes.Add(22, TriplesStoredSPPFNode.TerminalNode (6<inputGraphVertex>,1<terminalSymbol>,5<inputGraphVertex>))
                nodes.Add(23, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,4<inputGraphVertex>,2<rsmState>,1<rsmState>))
                nodes.Add(24, TriplesStoredSPPFNode.TerminalNode (5<inputGraphVertex>,2<terminalSymbol>,4<inputGraphVertex>))
                nodes.Add(25, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,1<rsmState>,1<rsmState>))
                nodes.Add(26, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,2<terminalSymbol>,3<inputGraphVertex>))
                nodes.Add(27, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,1<rsmState>,1<rsmState>))
                nodes.Add(28, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                nodes.Add(29, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,1<rsmState>,1<rsmState>))
                nodes.Add(30, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,2<terminalSymbol>,1<inputGraphVertex>))


                let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (6,7); (7,8); (8,9); (9,10)
                                             (10,11); (11,12); (12,13); (13,14); (14,15); (15,16); (14,17); (17,18)
                                             (12,19); (19,20); (10,21); (21,22); (8,23); (23,24); (6,25); (25,26)
                                             (4,27); (27,28); (2,29); (29,30); |])
                let distances = [|1<distance>|]
                (nodes,edges,distances)

            caStarBStarRSMTestCase "caaabbcb" expected

        let ab =
            let expected =
                let nodes = Dictionary<_,_>()
                nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (0<inputGraphVertex>,2<rsmState>))
                nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,3<terminalSymbol>,0<inputGraphVertex>))
                nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,2<rsmState>,2<rsmState>))
                nodes.Add(8, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,1<terminalSymbol>,2<inputGraphVertex>))
                nodes.Add(9, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                nodes.Add(10, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,2<terminalSymbol>,1<inputGraphVertex>))


                let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (4,7); (7,8); (2,9); (9,10);|])
                let distances = [|1<distance>|]
                (nodes,edges,distances)

            caStarBStarRSMTestCase "ab" expected

        let ccc =
            let expected =
                let nodes = Dictionary<_,_>()
                nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,1<rsmState>))
                nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,1<rsmState>))
                nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,3<terminalSymbol>,3<inputGraphVertex>))
                nodes.Add(7, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,1<rsmState>,1<rsmState>))
                nodes.Add(8, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                nodes.Add(9, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,0<rsmState>))
                nodes.Add(10, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,0<rsmState>))
                nodes.Add(11, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,-1<terminalSymbol>,3<inputGraphVertex>))
                nodes.Add(12, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(13, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,3<terminalSymbol>,2<inputGraphVertex>))
                nodes.Add(14, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,1<rsmState>,1<rsmState>))
                nodes.Add(15, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,-1<terminalSymbol>,1<inputGraphVertex>))
                nodes.Add(16, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,0<rsmState>))
                nodes.Add(17, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
                nodes.Add(18, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,0<rsmState>))
                nodes.Add(19, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,0<rsmState>))
                nodes.Add(20, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                nodes.Add(21, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(22, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,3<terminalSymbol>,1<inputGraphVertex>))



                let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (4,7); (7,8); (3,9); (9,10)
                                             (10,11); (9,12); (12,13); (2,14); (14,15); (1,16); (16,17); (17,18)
                                             (18,10); (18,19); (19,20); (16,21); (21,22);|])
                let distances = [|2<distance>|]
                (nodes,edges,distances)

            caStarBStarRSMTestCase "ccc" expected

        testList "On ca*b* RSM" [
            cb
            ca
            caaabbb
            caaacbbb
            caaabbcb
            ab
            ccc
        ] |> testSequenced

    let ``On calculator RSM`` =

        let calculatorRSM () = RSMCalculator.calculatorRSM () |> fun (rsm,_,_)  -> rsm
        let terminalMapping = RSMCalculator.calculatorRSM () |> fun (_,m,_) -> m
        let graphMaker = mkLinearGraph id terminalMapping
        let calculatorRSMTestCase = gllTestCase (calculatorRSM ()) graphMaker

        let ``x=3`` =
            let expected =
                let nodes = Dictionary<_,_>()
                nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,2<rsmState>))
                nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(4, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,3<rsmState>,1<inputGraphVertex>))
                nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,3<rsmState>,4<rsmState>))
                nodes.Add(6, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,5<rsmState>))
                nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,3<rsmState>,5<rsmState>))
                nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,6<rsmState>))
                nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,3<rsmState>,6<rsmState>))
                nodes.Add(10, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,7<rsmState>,3<inputGraphVertex>))
                nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,7<rsmState>,8<rsmState>))
                nodes.Add(12, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,33<terminalSymbol>,3<inputGraphVertex>))
                nodes.Add(13, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,6<rsmState>,5<rsmState>))
                nodes.Add(14, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,42<terminalSymbol>,2<inputGraphVertex>))
                nodes.Add(15, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,5<rsmState>,4<rsmState>))
                nodes.Add(16, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,9<rsmState>,1<inputGraphVertex>))
                nodes.Add(17, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,9<rsmState>,10<rsmState>))
                nodes.Add(18, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,11<rsmState>,1<inputGraphVertex>))
                nodes.Add(19, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,11<rsmState>,12<rsmState>))
                nodes.Add(20, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,13<rsmState>,1<inputGraphVertex>))
                nodes.Add(21, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,13<rsmState>,14<rsmState>))
                nodes.Add(22, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,15<rsmState>,1<inputGraphVertex>))
                nodes.Add(23, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,15<rsmState>,16<rsmState>))
                nodes.Add(24, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,17<rsmState>,1<inputGraphVertex>))
                nodes.Add(25, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,17<rsmState>,18<rsmState>))
                nodes.Add(26, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,3<terminalSymbol>,1<inputGraphVertex>))
                nodes.Add(27, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                nodes.Add(28, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))




                let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (6,7); (7,8); (8,9); (9,10)
                                             (10,11); (11,12); (8,13); (13,14); (6,15); (15,16); (16,17); (17,18)
                                             (18,19); (19,20); (20,21); (21,22); (22,23); (23,24); (24,25); (25,26)
                                             (2,27); (27,28);|])
                let distances = [|1<distance>|]
                (nodes,edges,distances)

            calculatorRSMTestCase "x=3" expected

        let ``x=3;`` =
            let expected =
                let nodes = Dictionary<_,_>()
                nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(4, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,3<rsmState>,2<inputGraphVertex>))
                nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,3<rsmState>,4<rsmState>))
                nodes.Add(6, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,5<rsmState>))
                nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,3<rsmState>,5<rsmState>))
                nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,6<rsmState>))
                nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,3<rsmState>,6<rsmState>))
                nodes.Add(10, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,7<rsmState>,4<inputGraphVertex>))
                nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,7<rsmState>,8<rsmState>))
                nodes.Add(12, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,33<terminalSymbol>,4<inputGraphVertex>))
                nodes.Add(13, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,6<rsmState>,5<rsmState>))
                nodes.Add(14, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,42<terminalSymbol>,3<inputGraphVertex>))
                nodes.Add(15, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,5<rsmState>,4<rsmState>))
                nodes.Add(16, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,9<rsmState>,2<inputGraphVertex>))
                nodes.Add(17, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,9<rsmState>,10<rsmState>))
                nodes.Add(18, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,11<rsmState>,2<inputGraphVertex>))
                nodes.Add(19, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,11<rsmState>,12<rsmState>))
                nodes.Add(20, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,13<rsmState>,2<inputGraphVertex>))
                nodes.Add(21, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,13<rsmState>,14<rsmState>))
                nodes.Add(22, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,15<rsmState>,2<inputGraphVertex>))
                nodes.Add(23, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,15<rsmState>,16<rsmState>))
                nodes.Add(24, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,17<rsmState>,2<inputGraphVertex>))
                nodes.Add(25, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,17<rsmState>,18<rsmState>))
                nodes.Add(26, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,3<terminalSymbol>,2<inputGraphVertex>))
                nodes.Add(27, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                nodes.Add(28, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))




                let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (6,7); (7,8); (8,9); (9,10)
                                             (10,11); (11,12); (8,13); (13,14); (6,15); (15,16); (16,17); (17,18)
                                             (18,19); (19,20); (20,21); (21,22); (22,23); (23,24); (24,25); (25,26)
                                             (2,27); (27,28);|])
                let distances = [|0<distance>|]
                (nodes,edges,distances)

            calculatorRSMTestCase "x=3;" expected

        let ``kek=42;`` =
            let expected =
                let nodes = Dictionary<_,_>()
                nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(4, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,3<rsmState>,2<inputGraphVertex>))
                nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,3<rsmState>,4<rsmState>))
                nodes.Add(6, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,5<rsmState>))
                nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,3<rsmState>,5<rsmState>))
                nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,6<rsmState>))
                nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,3<rsmState>,6<rsmState>))
                nodes.Add(10, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,7<rsmState>,4<inputGraphVertex>))
                nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,7<rsmState>,8<rsmState>))
                nodes.Add(12, TriplesStoredSPPFNode.IntermediateNode (5<inputGraphVertex>,8<rsmState>))
                nodes.Add(13, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,5<inputGraphVertex>,7<rsmState>,8<rsmState>))
                nodes.Add(14, TriplesStoredSPPFNode.IntermediateNode (6<inputGraphVertex>,8<rsmState>))
                nodes.Add(15, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,6<inputGraphVertex>,7<rsmState>,8<rsmState>))
                nodes.Add(16, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,20<terminalSymbol>,6<inputGraphVertex>))
                nodes.Add(17, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,5<inputGraphVertex>,8<rsmState>,8<rsmState>))
                nodes.Add(18, TriplesStoredSPPFNode.TerminalNode (6<inputGraphVertex>,14<terminalSymbol>,5<inputGraphVertex>))
                nodes.Add(19, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,4<inputGraphVertex>,8<rsmState>,8<rsmState>))
                nodes.Add(20, TriplesStoredSPPFNode.TerminalNode (5<inputGraphVertex>,20<terminalSymbol>,4<inputGraphVertex>))
                nodes.Add(21, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,6<rsmState>,5<rsmState>))
                nodes.Add(22, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,42<terminalSymbol>,3<inputGraphVertex>))
                nodes.Add(23, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,5<rsmState>,4<rsmState>))
                nodes.Add(24, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,9<rsmState>,2<inputGraphVertex>))
                nodes.Add(25, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,9<rsmState>,10<rsmState>))
                nodes.Add(26, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,11<rsmState>,2<inputGraphVertex>))
                nodes.Add(27, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,11<rsmState>,12<rsmState>))
                nodes.Add(28, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,13<rsmState>,2<inputGraphVertex>))
                nodes.Add(29, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,13<rsmState>,14<rsmState>))
                nodes.Add(30, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,15<rsmState>,2<inputGraphVertex>))
                nodes.Add(31, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,15<rsmState>,16<rsmState>))
                nodes.Add(32, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,17<rsmState>,2<inputGraphVertex>))
                nodes.Add(33, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,17<rsmState>,18<rsmState>))
                nodes.Add(34, TriplesStoredSPPFNode.IntermediateNode (7<inputGraphVertex>,18<rsmState>))
                nodes.Add(35, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,7<inputGraphVertex>,17<rsmState>,18<rsmState>))
                nodes.Add(36, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,4<terminalSymbol>,7<inputGraphVertex>))
                nodes.Add(37, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,2<inputGraphVertex>,18<rsmState>,18<rsmState>))
                nodes.Add(38, TriplesStoredSPPFNode.TerminalNode (7<inputGraphVertex>,2<terminalSymbol>,2<inputGraphVertex>))
                nodes.Add(39, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                nodes.Add(40, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))

                let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (6,7); (7,8); (8,9); (9,10)
                                             (10,11); (11,12); (12,13); (13,14); (14,15); (15,16); (14,17); (17,18)
                                             (12,19); (19,20); (8,21); (21,22); (6,23); (23,24); (24,25); (25,26)
                                             (26,27); (27,28); (28,29); (29,30); (30,31); (31,32); (32,33); (33,34)
                                             (34,35); (35,36); (34,37); (37,38); (2,39); (39,40); |])
                let distances = [|0<distance>|]
                (nodes,edges,distances)

            calculatorRSMTestCase "kek=42;" expected

        let ``x=2;y=4;`` =
            let expected =
                let nodes = Dictionary<_,_>()
                nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,3<rsmState>))
                nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,3<rsmState>))
                nodes.Add(6, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,4<rsmState>,3<inputGraphVertex>))
                nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,4<rsmState>,5<rsmState>))
                nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,6<rsmState>))
                nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,4<rsmState>,6<rsmState>))
                nodes.Add(10, TriplesStoredSPPFNode.IntermediateNode (5<inputGraphVertex>,7<rsmState>))
                nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,5<inputGraphVertex>,4<rsmState>,7<rsmState>))
                nodes.Add(12, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,8<rsmState>,5<inputGraphVertex>))
                nodes.Add(13, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,5<inputGraphVertex>,8<rsmState>,9<rsmState>))
                nodes.Add(14, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,33<terminalSymbol>,5<inputGraphVertex>))
                nodes.Add(15, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,4<inputGraphVertex>,7<rsmState>,6<rsmState>))
                nodes.Add(16, TriplesStoredSPPFNode.TerminalNode (5<inputGraphVertex>,42<terminalSymbol>,4<inputGraphVertex>))
                nodes.Add(17, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,6<rsmState>,5<rsmState>))
                nodes.Add(18, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,10<rsmState>,3<inputGraphVertex>))
                nodes.Add(19, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,10<rsmState>,11<rsmState>))
                nodes.Add(20, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,12<rsmState>,3<inputGraphVertex>))
                nodes.Add(21, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,12<rsmState>,13<rsmState>))
                nodes.Add(22, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,14<rsmState>,3<inputGraphVertex>))
                nodes.Add(23, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,14<rsmState>,15<rsmState>))
                nodes.Add(24, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,16<rsmState>,3<inputGraphVertex>))
                nodes.Add(25, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,16<rsmState>,17<rsmState>))
                nodes.Add(26, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,18<rsmState>,3<inputGraphVertex>))
                nodes.Add(27, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,18<rsmState>,19<rsmState>))
                nodes.Add(28, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,2<terminalSymbol>,3<inputGraphVertex>))
                nodes.Add(29, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,3<rsmState>,2<rsmState>))
                nodes.Add(30, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
                nodes.Add(31, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                nodes.Add(32, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                nodes.Add(33, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(34, TriplesStoredSPPFNode.IntermediateNode (6<inputGraphVertex>,3<rsmState>))
                nodes.Add(35, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,6<inputGraphVertex>,0<rsmState>,3<rsmState>))
                nodes.Add(36, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,4<rsmState>,6<inputGraphVertex>))
                nodes.Add(37, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,6<inputGraphVertex>,4<rsmState>,5<rsmState>))
                nodes.Add(38, TriplesStoredSPPFNode.IntermediateNode (7<inputGraphVertex>,6<rsmState>))
                nodes.Add(39, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,7<inputGraphVertex>,4<rsmState>,6<rsmState>))
                nodes.Add(40, TriplesStoredSPPFNode.IntermediateNode (8<inputGraphVertex>,7<rsmState>))
                nodes.Add(41, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,8<inputGraphVertex>,4<rsmState>,7<rsmState>))
                nodes.Add(42, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,8<rsmState>,8<inputGraphVertex>))
                nodes.Add(43, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,8<inputGraphVertex>,8<rsmState>,9<rsmState>))
                nodes.Add(44, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,34<terminalSymbol>,8<inputGraphVertex>))
                nodes.Add(45, TriplesStoredSPPFNode.RangeNode (8<inputGraphVertex>,7<inputGraphVertex>,7<rsmState>,6<rsmState>))
                nodes.Add(46, TriplesStoredSPPFNode.TerminalNode (8<inputGraphVertex>,42<terminalSymbol>,7<inputGraphVertex>))
                nodes.Add(47, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,6<inputGraphVertex>,6<rsmState>,5<rsmState>))
                nodes.Add(48, TriplesStoredSPPFNode.NonTerminalNode (7<inputGraphVertex>,10<rsmState>,6<inputGraphVertex>))
                nodes.Add(49, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,6<inputGraphVertex>,10<rsmState>,11<rsmState>))
                nodes.Add(50, TriplesStoredSPPFNode.NonTerminalNode (7<inputGraphVertex>,12<rsmState>,6<inputGraphVertex>))
                nodes.Add(51, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,6<inputGraphVertex>,12<rsmState>,13<rsmState>))
                nodes.Add(52, TriplesStoredSPPFNode.NonTerminalNode (7<inputGraphVertex>,14<rsmState>,6<inputGraphVertex>))
                nodes.Add(53, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,6<inputGraphVertex>,14<rsmState>,15<rsmState>))
                nodes.Add(54, TriplesStoredSPPFNode.NonTerminalNode (7<inputGraphVertex>,16<rsmState>,6<inputGraphVertex>))
                nodes.Add(55, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,6<inputGraphVertex>,16<rsmState>,17<rsmState>))
                nodes.Add(56, TriplesStoredSPPFNode.NonTerminalNode (7<inputGraphVertex>,18<rsmState>,6<inputGraphVertex>))
                nodes.Add(57, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,6<inputGraphVertex>,18<rsmState>,19<rsmState>))
                nodes.Add(58, TriplesStoredSPPFNode.TerminalNode (7<inputGraphVertex>,4<terminalSymbol>,6<inputGraphVertex>))
                nodes.Add(59, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,1<inputGraphVertex>,3<rsmState>,2<rsmState>))
                nodes.Add(60, TriplesStoredSPPFNode.TerminalNode (6<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))

                let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (6,7); (7,8); (8,9); (9,10)
                                             (10,11); (11,12); (12,13); (13,14); (10,15); (15,16); (8,17); (17,18)
                                             (18,19); (19,20); (20,21); (21,22); (22,23); (23,24); (24,25); (25,26)
                                             (26,27); (27,28); (4,29); (29,30); (2,31); (31,32); (32,33); (33,34)
                                             (34,35); (35,36); (36,37); (37,38); (38,39); (39,40); (40,41); (41,42)
                                             (42,43); (43,44); (40,45); (45,46); (38,47); (47,48); (48,49); (49,50)
                                             (50,51); (51,52); (52,53); (53,54); (54,55); (55,56); (56,57); (57,58)
                                             (34,59); (59,60);|])
                let distances = [|0<distance>|]
                (nodes,edges,distances)

            calculatorRSMTestCase "x=2;y=4;" expected

        let ``x=2y=4;`` =
            let expected =
                let nodes = Dictionary<_,_>()
                nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(4, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,3<rsmState>))
                nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,3<rsmState>))
                nodes.Add(6, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,4<rsmState>,2<inputGraphVertex>))
                nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,4<rsmState>,5<rsmState>))
                nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,6<rsmState>))
                nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,4<rsmState>,6<rsmState>))
                nodes.Add(10, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,7<rsmState>))
                nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,4<rsmState>,7<rsmState>))
                nodes.Add(12, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,8<rsmState>,4<inputGraphVertex>))
                nodes.Add(13, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,8<rsmState>,9<rsmState>))
                nodes.Add(14, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,33<terminalSymbol>,4<inputGraphVertex>))
                nodes.Add(15, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,7<rsmState>,6<rsmState>))
                nodes.Add(16, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,42<terminalSymbol>,3<inputGraphVertex>))
                nodes.Add(17, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,6<rsmState>,5<rsmState>))
                nodes.Add(18, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,10<rsmState>,2<inputGraphVertex>))
                nodes.Add(19, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,10<rsmState>,11<rsmState>))
                nodes.Add(20, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,12<rsmState>,2<inputGraphVertex>))
                nodes.Add(21, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,12<rsmState>,13<rsmState>))
                nodes.Add(22, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,14<rsmState>,2<inputGraphVertex>))
                nodes.Add(23, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,14<rsmState>,15<rsmState>))
                nodes.Add(24, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,16<rsmState>,2<inputGraphVertex>))
                nodes.Add(25, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,16<rsmState>,17<rsmState>))
                nodes.Add(26, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,18<rsmState>,2<inputGraphVertex>))
                nodes.Add(27, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,18<rsmState>,19<rsmState>))
                nodes.Add(28, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,2<terminalSymbol>,2<inputGraphVertex>))
                nodes.Add(29, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,2<inputGraphVertex>,3<rsmState>,2<rsmState>))
                nodes.Add(30, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,0<terminalSymbol>,2<inputGraphVertex>))
                nodes.Add(31, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                nodes.Add(32, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                nodes.Add(33, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(34, TriplesStoredSPPFNode.IntermediateNode (5<inputGraphVertex>,3<rsmState>))
                nodes.Add(35, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,5<inputGraphVertex>,0<rsmState>,3<rsmState>))
                nodes.Add(36, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,4<rsmState>,5<inputGraphVertex>))
                nodes.Add(37, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,5<inputGraphVertex>,4<rsmState>,5<rsmState>))
                nodes.Add(38, TriplesStoredSPPFNode.IntermediateNode (6<inputGraphVertex>,6<rsmState>))
                nodes.Add(39, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,6<inputGraphVertex>,4<rsmState>,6<rsmState>))
                nodes.Add(40, TriplesStoredSPPFNode.IntermediateNode (7<inputGraphVertex>,7<rsmState>))
                nodes.Add(41, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,7<inputGraphVertex>,4<rsmState>,7<rsmState>))
                nodes.Add(42, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,8<rsmState>,7<inputGraphVertex>))
                nodes.Add(43, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,7<inputGraphVertex>,8<rsmState>,9<rsmState>))
                nodes.Add(44, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,34<terminalSymbol>,7<inputGraphVertex>))
                nodes.Add(45, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,6<inputGraphVertex>,7<rsmState>,6<rsmState>))
                nodes.Add(46, TriplesStoredSPPFNode.TerminalNode (7<inputGraphVertex>,42<terminalSymbol>,6<inputGraphVertex>))
                nodes.Add(47, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,5<inputGraphVertex>,6<rsmState>,5<rsmState>))
                nodes.Add(48, TriplesStoredSPPFNode.NonTerminalNode (6<inputGraphVertex>,10<rsmState>,5<inputGraphVertex>))
                nodes.Add(49, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,5<inputGraphVertex>,10<rsmState>,11<rsmState>))
                nodes.Add(50, TriplesStoredSPPFNode.NonTerminalNode (6<inputGraphVertex>,12<rsmState>,5<inputGraphVertex>))
                nodes.Add(51, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,5<inputGraphVertex>,12<rsmState>,13<rsmState>))
                nodes.Add(52, TriplesStoredSPPFNode.NonTerminalNode (6<inputGraphVertex>,14<rsmState>,5<inputGraphVertex>))
                nodes.Add(53, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,5<inputGraphVertex>,14<rsmState>,15<rsmState>))
                nodes.Add(54, TriplesStoredSPPFNode.NonTerminalNode (6<inputGraphVertex>,16<rsmState>,5<inputGraphVertex>))
                nodes.Add(55, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,5<inputGraphVertex>,16<rsmState>,17<rsmState>))
                nodes.Add(56, TriplesStoredSPPFNode.NonTerminalNode (6<inputGraphVertex>,18<rsmState>,5<inputGraphVertex>))
                nodes.Add(57, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,5<inputGraphVertex>,18<rsmState>,19<rsmState>))
                nodes.Add(58, TriplesStoredSPPFNode.TerminalNode (6<inputGraphVertex>,4<terminalSymbol>,5<inputGraphVertex>))
                nodes.Add(59, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,1<inputGraphVertex>,3<rsmState>,2<rsmState>))
                nodes.Add(60, TriplesStoredSPPFNode.TerminalNode (5<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))

                let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (6,7); (7,8); (8,9); (9,10)
                                             (10,11); (11,12); (12,13); (13,14); (10,15); (15,16); (8,17); (17,18)
                                             (18,19); (19,20); (20,21); (21,22); (22,23); (23,24); (24,25); (25,26)
                                             (26,27); (27,28); (4,29); (29,30); (2,31); (31,32); (32,33); (33,34)
                                             (34,35); (35,36); (36,37); (37,38); (38,39); (39,40); (40,41); (41,42)
                                             (42,43); (43,44); (40,45); (45,46); (38,47); (47,48); (48,49); (49,50)
                                             (50,51); (51,52); (52,53); (53,54); (54,55); (55,56); (56,57); (57,58)
                                             (34,59); (59,60); |])
                let distances = [|1<distance>|]
                (nodes,edges,distances)

            calculatorRSMTestCase "x=2y=4;" expected

        let ``x=2+4*3`` =
            let expected =
                let nodes = Dictionary<_,_>()
                nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,2<rsmState>))
                nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,2<rsmState>))
                nodes.Add(4, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,3<rsmState>,1<inputGraphVertex>))
                nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,3<rsmState>,4<rsmState>))
                nodes.Add(6, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,5<rsmState>))
                nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,3<rsmState>,5<rsmState>))
                nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,6<rsmState>))
                nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,3<rsmState>,6<rsmState>))
                nodes.Add(10, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,7<rsmState>,3<inputGraphVertex>))
                nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,7<rsmState>,8<rsmState>))
                nodes.Add(12, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,33<terminalSymbol>,3<inputGraphVertex>))
                nodes.Add(13, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,6<rsmState>,5<rsmState>))
                nodes.Add(14, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,42<terminalSymbol>,2<inputGraphVertex>))
                nodes.Add(15, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,5<rsmState>,4<rsmState>))
                nodes.Add(16, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,9<rsmState>,1<inputGraphVertex>))
                nodes.Add(17, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,9<rsmState>,10<rsmState>))
                nodes.Add(18, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,11<rsmState>))
                nodes.Add(19, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,4<inputGraphVertex>,9<rsmState>,11<rsmState>))
                nodes.Add(20, TriplesStoredSPPFNode.IntermediateNode (5<inputGraphVertex>,12<rsmState>))
                nodes.Add(21, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,5<inputGraphVertex>,9<rsmState>,12<rsmState>))
                nodes.Add(22, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,9<rsmState>,5<inputGraphVertex>))
                nodes.Add(23, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,5<inputGraphVertex>,9<rsmState>,10<rsmState>))
                nodes.Add(24, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,13<rsmState>,5<inputGraphVertex>))
                nodes.Add(25, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,5<inputGraphVertex>,13<rsmState>,14<rsmState>))
                nodes.Add(26, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,15<rsmState>,5<inputGraphVertex>))
                nodes.Add(27, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,5<inputGraphVertex>,15<rsmState>,16<rsmState>))
                nodes.Add(28, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,17<rsmState>,5<inputGraphVertex>))
                nodes.Add(29, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,5<inputGraphVertex>,17<rsmState>,18<rsmState>))
                nodes.Add(30, TriplesStoredSPPFNode.NonTerminalNode (2<inputGraphVertex>,19<rsmState>,5<inputGraphVertex>))
                nodes.Add(31, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,5<inputGraphVertex>,19<rsmState>,20<rsmState>))
                nodes.Add(32, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,2<terminalSymbol>,5<inputGraphVertex>))
                nodes.Add(33, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,4<inputGraphVertex>,12<rsmState>,11<rsmState>))
                nodes.Add(34, TriplesStoredSPPFNode.TerminalNode (5<inputGraphVertex>,36<terminalSymbol>,4<inputGraphVertex>))
                nodes.Add(35, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,1<inputGraphVertex>,11<rsmState>,10<rsmState>))
                nodes.Add(36, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,13<rsmState>,1<inputGraphVertex>))
                nodes.Add(37, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,1<inputGraphVertex>,13<rsmState>,14<rsmState>))
                nodes.Add(38, TriplesStoredSPPFNode.IntermediateNode (6<inputGraphVertex>,21<rsmState>))
                nodes.Add(39, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,6<inputGraphVertex>,13<rsmState>,21<rsmState>))
                nodes.Add(40, TriplesStoredSPPFNode.IntermediateNode (7<inputGraphVertex>,22<rsmState>))
                nodes.Add(41, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,7<inputGraphVertex>,13<rsmState>,22<rsmState>))
                nodes.Add(42, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,13<rsmState>,7<inputGraphVertex>))
                nodes.Add(43, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,7<inputGraphVertex>,13<rsmState>,14<rsmState>))
                nodes.Add(44, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,15<rsmState>,7<inputGraphVertex>))
                nodes.Add(45, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,7<inputGraphVertex>,15<rsmState>,16<rsmState>))
                nodes.Add(46, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,17<rsmState>,7<inputGraphVertex>))
                nodes.Add(47, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,7<inputGraphVertex>,17<rsmState>,18<rsmState>))
                nodes.Add(48, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,19<rsmState>,7<inputGraphVertex>))
                nodes.Add(49, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,7<inputGraphVertex>,19<rsmState>,20<rsmState>))
                nodes.Add(50, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,4<terminalSymbol>,7<inputGraphVertex>))
                nodes.Add(51, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,6<inputGraphVertex>,22<rsmState>,21<rsmState>))
                nodes.Add(52, TriplesStoredSPPFNode.TerminalNode (7<inputGraphVertex>,38<terminalSymbol>,6<inputGraphVertex>))
                nodes.Add(53, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,1<inputGraphVertex>,21<rsmState>,14<rsmState>))
                nodes.Add(54, TriplesStoredSPPFNode.NonTerminalNode (6<inputGraphVertex>,15<rsmState>,1<inputGraphVertex>))
                nodes.Add(55, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,1<inputGraphVertex>,15<rsmState>,16<rsmState>))
                nodes.Add(56, TriplesStoredSPPFNode.NonTerminalNode (6<inputGraphVertex>,17<rsmState>,1<inputGraphVertex>))
                nodes.Add(57, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,1<inputGraphVertex>,17<rsmState>,18<rsmState>))
                nodes.Add(58, TriplesStoredSPPFNode.NonTerminalNode (6<inputGraphVertex>,19<rsmState>,1<inputGraphVertex>))
                nodes.Add(59, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,1<inputGraphVertex>,19<rsmState>,20<rsmState>))
                nodes.Add(60, TriplesStoredSPPFNode.TerminalNode (6<inputGraphVertex>,3<terminalSymbol>,1<inputGraphVertex>))
                nodes.Add(61, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                nodes.Add(62, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,0<terminalSymbol>,1<inputGraphVertex>))

                let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (6,7); (7,8); (8,9); (9,10)
                                             (10,11); (11,12); (8,13); (13,14); (6,15); (15,16); (16,17); (17,18)
                                             (18,19); (19,20); (20,21); (21,22); (22,23); (23,24); (24,25); (25,26)
                                             (26,27); (27,28); (28,29); (29,30); (30,31); (31,32); (20,33); (33,34)
                                             (18,35); (35,36); (36,37); (37,38); (38,39); (39,40); (40,41); (41,42)
                                             (42,43); (43,44); (44,45); (45,46); (46,47); (47,48); (48,49); (49,50)
                                             (40,51); (51,52); (38,53); (53,54); (54,55); (55,56); (56,57); (57,58)
                                             (58,59); (59,60); (2,61); (61,62); |])
                let distances = [|1<distance>|]
                (nodes,edges,distances)

            calculatorRSMTestCase "x=2+4*3" expected

//        let ``x`` =
//            let expected =
//                let nodes = Dictionary<_,_>()
//                let edges = ResizeArray<_>([||])
//                let distances = [|0<distance>|]
//                (nodes,edges,distances)
//
//            //calculatorRSMTestCase "kek=42" expected
//            gllTestCaseWithPrettySPPF (calculatorRSM ()) graphMaker terminalMapping "x=2+4*3" expected

        testList "On calculator RSM" [
            ``x=3;``
            ``x=3``
            ``kek=42;``
            ``x=2;y=4;``
            ``x=2y=4;``
            ``x=2+4*3``
        ] |> testSequenced

    testList "Error recovering tests" [
//        ``On ab RSM``
//        ``On ca*b* RSM``
        ``On calculator RSM``
    ] |> testSequenced

