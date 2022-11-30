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

let run (graph: InputGraph) (q: RSM) =
    let startV = 0<inputGraphVertex>
    let finalV = LanguagePrimitives.Int32WithMeasure<inputGraphVertex>(graph.NumberOfVertices() - 1)

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
        actual.ToDot "sppf.dot"
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
    let query, mapping = build [] grammar
    let graphMaker = mkLinearGraph id mapping
    gllTestCase query graphMaker

let ``Error recovering tests`` =

    let `` On mini ML RSM`` =
        let rsm () =
            let rsm, terminalMapping =
                let Num = nt "Num"
                let Arithmetic = nt "Arithmetic"
                let Var = nt "Var"
                let Atom = nt "Atom"
                let Prod = nt "Prod"
                let Program = nt "Program"
                let Expr = nt "Expr"
                let Compare = nt "Compare"
                let space = t " " +|+ t "\n" +|+ t "\r" +|+ t "\t"
                let ws x = many space ** x

                [
                    Program => Expr // many Expr ** many space

                    Num  => ws(([|1..9|] |> Array.map (string >> t) |> Array.reduce ( +|+ ))
                             ** many ([|0..9|] |> Array.map (string >> t) |> Array.reduce ( +|+ )))
                    Var  => ws(([|'a'..'z'|] |> Array.map (string >> t) |> Array.reduce ( +|+ ))
                            ** many (    ([|'a'..'z'|] |> Array.map (string >> t) |> Array.reduce ( +|+ ))
                                     +|+ ([|0..9|] |> Array.map (string >> t) |> Array.reduce ( +|+ ))))
                    Atom => (* Num
                            +|+ *) Var //(Var ** many (space ** Atom))
                            //+|+ (ws (t "(") ** Expr ** ws (t ")"))
                            //+|+ (Var ** t "[" ** Expr ** t"]")
                    //Prod => nonemptyList Atom (ws (t "*" +|+ t "/"))
                    //Arithmetic => nonemptyList Prod (ws(t "+" +|+ t "-"))
                    //Compare => nonemptyList Arithmetic (ws ((opt (t"!") ** t "=") +|+ ((t ">" +|+ t "<") ** opt (t "="))))
                    Expr => Atom +|+
                        //Compare
                        (**|* *) (ws(literal "let") (* ** opt(ws(literal "rec"))*) ** space ** Var ** many (space ** Var) ** ws(t "=") ** Expr ** ws(literal "in") ** Expr)
                        //+|+ (ws(literal "if") ** Expr ** ws(literal "then") ** Expr ** opt (ws(literal "else") ** Expr))
                        //+|+ (ws(literal "while") ** Expr ** ws(literal "do") ** Expr)
                ]
                |> build []
            rsm, terminalMapping

        let incorrectInsertions =
            let testName = "Incorrect insertion"
            testCase testName <| fun () ->
                let q,terminalMapping = rsm ()
                let graph = mkLinearGraph id terminalMapping "lt x = y in x"
                let startV = 0<inputGraphVertex>
                let finishV = 13<inputGraphVertex>
                let expected =
                    let nodes = Dictionary<_,_>()
                    nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                    nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                    nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                    nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(4, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,1<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(5, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                    nodes.Add(6, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,2<terminalSymbol>,1<inputGraphVertex>))

                    let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (2,5); (5,6); |])
                    let distances = [|0<distance>|]
                    (nodes,edges,distances)

                runErrorRecoveringGLLAndCheckResult testName graph startV finishV q expected

        testList "Mini ML" [
            incorrectInsertions
        ] |> testSequenced

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

    testList "Error recovering tests" [
        ``On ab RSM``
        ``On ca*b* RSM``
    ] |> testSequenced

