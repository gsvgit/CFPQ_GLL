﻿open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.BTree
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF

let runExample (graph,startV,q) =
    let reachable,matched = GLL.eval graph startV q
    let sppf = matched.ToSPPF(startV,q)
    let x = TriplesStoredSPPF(sppf)
    x.Edges |> Seq.iter (fun (x,y) -> printf $"(%i{x},%i{y}); ")
    x.Nodes
    |> Seq.iter (fun kvp ->
        match kvp.Value with
        | TriplesStoredSPPFNode.EpsilonNode (_pos,_rsm) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.EpsilonNode (%i{_pos}<graphVertex>,%i{_rsm}<rsmState>))"
        | TriplesStoredSPPFNode.TerminalNode (_from,_terminal,_to) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.TerminalNode (%i{_from}<graphVertex>,%i{_terminal}<terminalSymbol>,%i{_to}<graphVertex>))" 
        | TriplesStoredSPPFNode.NonTerminalNode (_from,_nonTerminal,_to) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.NonTerminalNode (%i{_from}<graphVertex>,%i{_nonTerminal}<rsmState>,%i{_to}<graphVertex>))"
        | TriplesStoredSPPFNode.RangeNode (_posFrom, _posTo, _rsmFrom, _rsmTo) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.RangeNode (%i{_posFrom}<graphVertex>,%i{_posTo}<graphVertex>,%i{_rsmFrom}<rsmState>,%i{_rsmTo}<rsmState>))"
        | TriplesStoredSPPFNode.IntermediateNode (_pos, _rsm) -> printfn $"nodes.Add(%i{kvp.Key}, TriplesStoredSPPFNode.IntermediateNode (%i{_pos}<graphVertex>,%i{_rsm}<rsmState>))"
        )
    
    x.ToDot "1.dot"
    printfn $"SPPF: %A{sppf}"
    printfn $"Reachable: %A{reachable}"

let example1 =
    let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)
                             InputGraph.TerminalEdge(1<graphVertex>,0<terminalSymbol>,4<graphVertex>)
                             InputGraph.TerminalEdge(2<graphVertex>,0<terminalSymbol>,1<graphVertex>)
                             InputGraph.TerminalEdge(2<graphVertex>,0<terminalSymbol>,3<graphVertex>)
                             InputGraph.TerminalEdge(5<graphVertex>,0<terminalSymbol>,2<graphVertex>)
                             InputGraph.TerminalEdge(4<graphVertex>,1<terminalSymbol>,6<graphVertex>)
                             InputGraph.TerminalEdge(10<graphVertex>,2<terminalSymbol>,5<graphVertex>)
                             
                             InputGraph.TerminalEdge(6<graphVertex>,0<terminalSymbol>,7<graphVertex>)
                             InputGraph.TerminalEdge(7<graphVertex>,0<terminalSymbol>,9<graphVertex>)
                             InputGraph.TerminalEdge(8<graphVertex>,0<terminalSymbol>,9<graphVertex>)
                             InputGraph.TerminalEdge(9<graphVertex>,0<terminalSymbol>,10<graphVertex>)
                             InputGraph.TerminalEdge(7<graphVertex>,3<terminalSymbol>,6<graphVertex>)
                             InputGraph.TerminalEdge(10<graphVertex>,4<terminalSymbol>,8<graphVertex>)
                             
                             InputGraph.TerminalEdge(11<graphVertex>,0<terminalSymbol>,12<graphVertex>)
                             InputGraph.TerminalEdge(12<graphVertex>,0<terminalSymbol>,13<graphVertex>)
                             InputGraph.TerminalEdge(14<graphVertex>,0<terminalSymbol>,12<graphVertex>)
                             InputGraph.TerminalEdge(14<graphVertex>,0<terminalSymbol>,15<graphVertex>)
                             InputGraph.TerminalEdge(13<graphVertex>,5<terminalSymbol>,6<graphVertex>)
                             InputGraph.TerminalEdge(10<graphVertex>,6<terminalSymbol>,14<graphVertex>)
                             
                             |])
    
    let box = RSMBox(0<rsmState>, HashSet([0<rsmState>]),
                [|TerminalEdge(0<rsmState>,0<terminalSymbol>,0<rsmState>)
                  TerminalEdge(0<rsmState>,1<terminalSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
                  TerminalEdge(2<rsmState>,2<terminalSymbol>,0<rsmState>)
                  
                  TerminalEdge(0<rsmState>,3<terminalSymbol>,3<rsmState>)
                  NonTerminalEdge(3<rsmState>,0<rsmState>,4<rsmState>)
                  TerminalEdge(4<rsmState>,4<terminalSymbol>,0<rsmState>)
                  
                  TerminalEdge(0<rsmState>,5<terminalSymbol>,5<rsmState>)
                  NonTerminalEdge(5<rsmState>,0<rsmState>,6<rsmState>)
                  TerminalEdge(6<rsmState>,6<terminalSymbol>,0<rsmState>)
                  
                  TerminalEdge(0<rsmState>,1<terminalSymbol>,0<rsmState>)
                  TerminalEdge(0<rsmState>,3<terminalSymbol>,0<rsmState>)
                  TerminalEdge(0<rsmState>,5<terminalSymbol>,0<rsmState>)
                  
                  |])
    
    let q = RSM([|box|],box)
    let startV = [|0<graphVertex>|]
    graph,startV,q
   
   

(*

let example9 n startV =
    let callEdges = [|for i in 0 .. n -> InputGraph.TerminalEdge(LanguagePrimitives.Int32WithMeasure i,0<terminalSymbol>,LanguagePrimitives.Int32WithMeasure (i+1)) |]
    let returnEdges = [|for i in n + 2 .. 2 * n + 2 -> InputGraph.TerminalEdge(LanguagePrimitives.Int32WithMeasure i,1<terminalSymbol>,LanguagePrimitives.Int32WithMeasure (i+1)) |]
    let graph = InputGraph(Array.concat [
                                         callEdges; returnEdges
                                         [|InputGraph.TerminalEdge(LanguagePrimitives.Int32WithMeasure (n+1),0<terminalSymbol>,LanguagePrimitives.Int32WithMeasure 0)|]
                                         [|
                                           InputGraph.TerminalEdge(LanguagePrimitives.Int32WithMeasure 0,1<terminalSymbol>,LanguagePrimitives.Int32WithMeasure(n + 2))
                                           InputGraph.TerminalEdge(LanguagePrimitives.Int32WithMeasure (2*n+3),1<terminalSymbol>,LanguagePrimitives.Int32WithMeasure 0)
                                           |]
                                         
                                         ])
    let q = RSM(HashSet<_>([0<rsmState>]), HashSet([0<rsmState>]),
                [|CFGEdge(0<rsmState>,0<rsmState>)
                  TerminalEdge(0<rsmState>,0<terminalSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
                  TerminalEdge(2<rsmState>,1<terminalSymbol>,0<rsmState>)|])
    let reachable = GLL.eval graph startV q [|0<rsmState>|]
    printfn $"Reachable: %A{reachable}" 
    *)
let loadGraphFromCSV file (callLabelsMappings:Dictionary<_,_>) =
    let edges = ResizeArray<_>()
    System.IO.File.ReadLines file
    |> Seq.map (fun s -> s.Split " ")
    |> Seq.iter (fun a ->
        if callLabelsMappings.ContainsKey a.[2]
        then
            edges.Add (InputGraph.TerminalEdge(a.[0] |> int |> LanguagePrimitives.Int32WithMeasure
                                               , callLabelsMappings.[a.[2]] |> fst |> LanguagePrimitives.Int32WithMeasure
                                               , a.[1] |> int |> LanguagePrimitives.Int32WithMeasure))
            edges.Add (InputGraph.TerminalEdge(a.[1] |> int |> LanguagePrimitives.Int32WithMeasure
                                               , callLabelsMappings.[a.[2]] |> snd |> LanguagePrimitives.Int32WithMeasure
                                               , a.[0] |> int |> LanguagePrimitives.Int32WithMeasure))
        else edges.Add (InputGraph.TerminalEdge (a.[0] |> int |> LanguagePrimitives.Int32WithMeasure,
                                                 4<terminalSymbol>,
                                                 a.[1] |> int |> LanguagePrimitives.Int32WithMeasure))
            )
    InputGraph <| edges.ToArray()

let loadNodesFormCSV file =
    System.IO.File.ReadLines file
    |> Seq.map (int >> LanguagePrimitives.Int32WithMeasure)
    |> Array.ofSeq

let defaultMap =
    let res = Dictionary<_,_>()
    res.Add("subClassOf",(0,1))
    res.Add("type",(2,3))
    res
let g1 =
    let box = RSMBox(0<rsmState>, HashSet([3<rsmState>]),
                [|TerminalEdge(0<rsmState>,1<terminalSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
                  TerminalEdge(1<rsmState>,0<terminalSymbol>,3<rsmState>)
                  TerminalEdge(2<rsmState>,0<terminalSymbol>,3<rsmState>)
                  
                  TerminalEdge(0<rsmState>,3<terminalSymbol>,4<rsmState>)
                  NonTerminalEdge(4<rsmState>,0<rsmState>,5<rsmState>)
                  TerminalEdge(4<rsmState>,2<terminalSymbol>,3<rsmState>)
                  TerminalEdge(5<rsmState>,2<terminalSymbol>,3<rsmState>)|])
    RSM([|box|], box)

let example10_go_hierarchy () =
    let graph = loadGraphFromCSV "/home/gsv/Downloads/go_hierarchy.csv" defaultMap
    let nodes = loadNodesFormCSV "/home/gsv/Downloads/go_hierarchy_nodes.csv"
    nodes
    |> Array.iter (fun n ->
        let reachable = GLL.eval graph [|n|] g1
        printfn $"Reachable: %A{reachable}")

let example11_go_allPairs () =
    let graph = loadGraphFromCSV "/home/gsv/Downloads/go.csv" defaultMap
    let reachable,matched = GLL.eval graph (graph.AllVertices()) g1
    matched.Statistics () |> printfn "%A"
    printfn $"Reachable: %A{reachable.Count}"

let example11_go_singleSourceForAll () =
    let graph = loadGraphFromCSV "/home/gsv/Downloads/go.csv" defaultMap
    for n in graph.AllVertices() do
        let reachable,matched = GLL.eval graph [|n|] g1 
        printfn $"Reachable: %A{reachable.Count}"
        
let example12_go_hierarchy_singleSourceForAll () =
    let graph = loadGraphFromCSV "/home/gsv/Downloads/go_hierarchy.csv" defaultMap
    for n in graph.AllVertices() do
        let reachable,matched = GLL.eval graph [|n|] g1 
        printfn $"Reachable: %A{reachable.Count}"
    
let example12_go_hierarchy_allPairs () =
    let graph = loadGraphFromCSV "/home/gsv/Downloads/go_hierarchy.csv" defaultMap
    let reachable,matched = GLL.eval graph (graph.AllVertices()) g1
    matched.Statistics ()
    |> printfn "%A"
    printfn $"Reachable: %A{reachable.Count}"

let randomFillDictionary n =
    let dict = Dictionary<_,_>()
    let random = System.Random()
    for i in 0..n do
        let key = int64 <| random.Next()
        let value = HashSet<int> (2000)
        let flg, v = dict.TryGetValue key
        if not flg
        then dict.Add(key,value)
    dict


let randomFillBTree n =
    let bTree = BTree()
    let random = System.Random()
    for i in 0..n do
        let key = int64 <| random.Next()
        let value = HashSet<int> (2000)
        let v = bTree.TryGetValue key
        match v with
        | Some _ -> ()
        | None -> bTree.Add(key,value)
    bTree
    
[<EntryPoint>]
let main argv =   
  
    //runExample example1
    //example10_go_hierarchy()
    example11_go_allPairs ()
    //example11_go_singleSourceForAll ()
    //example12_go_hierarchy_allPairs ()
    //example12_go_hierarchy_singleSourceForAll ()
    
    (*
    let tree= BTree()
    tree.Add(1L,1.0)
    tree.Add(20L,2.0)
    tree.Add(60L,3.0)
    tree.Add(5L,4.0)
    tree.Add(4L,5.0)
    tree.Add(70L,6.0)
    tree.Add(8L,7.0)
    tree.Add(3L,8.0)
    tree.Add(90L,9.0)
    tree.Add(10L,10.0)
    tree.Add(11L,1.0)
    tree.Add(2L,2.0)
    tree.Add(6L,3.0)
    tree.Add(50L,4.0)
    tree.Add(40L,5.0)
    tree.Add(7L,6.0)
    tree.Add(80L,7.0)
    tree.Add(30L,8.0)
    tree.Add(9L,9.0)
    tree.Add(100L,10.0)
    printfn "%A" tree
    
    let c = 10000000
    
    let start = System.DateTime.Now 
    let bTree = randomFillBTree c
    printfn $"BTree: %A{(System.DateTime.Now - start).TotalMilliseconds} milliseconds"
    
    let r = System.Random()
    let start = System.DateTime.Now
    for i in 0..c do
        bTree.TryGetValue(int64 <| r.Next())
    printfn $"BTree find: %A{(System.DateTime.Now - start).TotalMilliseconds} milliseconds"
    
    let start = System.DateTime.Now 
    let dict = randomFillDictionary c
    printfn $"Dictionary: %A{(System.DateTime.Now - start).TotalMilliseconds} milliseconds"
    
    let start = System.DateTime.Now
    for i in 0..c do
        dict.TryGetValue(int64 <| r.Next())
    printfn $"Dict find: %A{(System.DateTime.Now - start).TotalMilliseconds} milliseconds"
    *)
      
    0 // return an integer exit code