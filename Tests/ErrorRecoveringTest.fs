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

let test2 (graph: InputGraph) q =

    let startV = 0<inputGraphVertex>
    let finalV = LanguagePrimitives.Int32WithMeasure<inputGraphVertex>(graph.NumberOfVertices() - 1)
    graph.ToDot (0, "graph.dot")
    let startVertices,mapping = graph.ToCfpqCoreGraph startV
    let finalVertices = mapping[finalV]
    let start = DateTime.Now
    let result = GLL.errorRecoveringEval finalVertices startVertices q GLL.AllPaths
    printfn $"{(DateTime.Now - start).TotalMilliseconds}"

    match result with
    | GLL.QueryResult.MatchedRanges ranges ->
        let sppf = q.OriginalStartState.NonTerminalNodes.ToArray()
        let root = sppf |> Array.filter (fun n -> finalVertices = n.RightPosition && startVertices = n.LeftPosition) |> Array.minBy(fun n -> n.Distance)
        let distances = sppf |> Array.map (fun n -> n.Distance) |> Array.sort
        //printfn $"D for %s{validDotFileName}: %A{distances}"
        let actual = TriplesStoredSPPF([|root|], Dictionary())
        ()
        //actual.ToDot "sppf.dot"

        //Tests.GLLTests.dumpResultToConsole actual
    | _ -> failwith "Unexpected result."
    0

let testFromString (maker: string -> InputGraph) text q =
    test2 (maker text) q

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
                let space = t " " *|* t "\n" *|* t "\r" *|* t "\t"
                let ws x = many space ++ x

                [
                    Program => Expr // many Expr ++ many space

                    Num  => ws(([|1..9|] |> Array.map (string >> t) |> Array.reduce ( *|* ))
                             ++ many ([|0..9|] |> Array.map (string >> t) |> Array.reduce ( *|* )))
                    Var  => ws(([|'a'..'z'|] |> Array.map (string >> t) |> Array.reduce ( *|* ))
                            ++ many (    ([|'a'..'z'|] |> Array.map (string >> t) |> Array.reduce ( *|* ))
                                     *|* ([|0..9|] |> Array.map (string >> t) |> Array.reduce ( *|* ))))
                    Atom => (* Num
                            *|* *) Var //(Var ++ many (space ++ Atom))
                            //*|* (ws (t "(") ++ Expr ++ ws (t ")"))
                            //*|* (Var ++ t "[" ++ Expr ++ t"]")
                    //Prod => nonemptyList Atom (ws (t "*" *|* t "/"))
                    //Arithmetic => nonemptyList Prod (ws(t "+" *|* t "-"))
                    //Compare => nonemptyList Arithmetic (ws ((opt (t"!") ++ t "=") *|* ((t ">" *|* t "<") ++ opt (t "="))))
                    Expr => Atom *|*
                        //Compare
                        (**|* *) (ws(literal "let") (* ++ opt(ws(literal "rec"))*) ++ space ++ Var ++ many (space ++ Var) ++ ws(t "=") ++ Expr ++ ws(literal "in") ++ Expr)
                        //*|* (ws(literal "if") ++ Expr ++ ws(literal "then") ++ Expr ++ opt (ws(literal "else") ++ Expr))
                        //*|* (ws(literal "while") ++ Expr ++ ws(literal "do") ++ Expr)
                ]
                |> build
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

        let ab =
            let testName = "ab"
            testCase testName <| fun () ->
                let graph = graphMaker "ab"
                let startV = 0<inputGraphVertex>
                let finishV = 2<inputGraphVertex>
                let q = abRSM ()
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

                runErrorRecoveringGLLAndCheckResult testName graph startV finishV q expected

        let abb =
            let testName = "abb"
            testCase testName <| fun () ->
                let graph = graphMaker "abb"
                let startV = 0<inputGraphVertex>
                let finishV = 3<inputGraphVertex>
                let q = abRSM ()
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

                runErrorRecoveringGLLAndCheckResult testName graph startV finishV q expected

        let bb =
            let testName = "bb"
            testCase testName <| fun () ->
                let graph = graphMaker "bb"
                let startV = 0<inputGraphVertex>
                let finishV = 2<inputGraphVertex>
                let q = abRSM ()
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

                runErrorRecoveringGLLAndCheckResult testName graph startV finishV q expected



        testList "On ab RSM" [
            ab
            abb
            bb
        ] |> testSequenced

    let ``On ca*b* RSM`` =

        let CAStarBStarRSM () =
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


        let cb =
            let testName = "cb"
            testCase testName <| fun () ->
                let str ="caaabbbbbbbbbbbc"
                let graph = graphMaker str
                let startV = 0<inputGraphVertex>
                let finishV = str.Length * 1<inputGraphVertex>
                let q = CAStarBStarRSM ()
                q.ToDot "rsm"
                let expected =
                    let nodes = Dictionary<_,_>()


                    let edges = ResizeArray<_>([||])
                    let distances = [||]
                    (nodes,edges,distances)

                runErrorRecoveringGLLAndCheckResult testName graph startV finishV q expected



        testList "On ca*b* RSM" [
            cb
        ] |> testSequenced

    testList "Error recovering tests" [
        ``On ab RSM``
        //``On ca*b* RSM``
    ] |> testSequenced

