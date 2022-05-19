// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF

let example1 () =
    let graph = InputGraph([|InputGraph.CFGEdge(0<graphVertex>,1<graphVertex>)|])
    let startV = [|0<graphVertex>|]
    let q = RSM(HashSet<_>([|0<rsmState>|]), HashSet([1<rsmState>]),[|CFGEdge(0<rsmState>,1<rsmState>)|])
    let reachable,matched = GLL.eval graph startV q [|0<rsmState>|]
    let sppf = matched.ToSPPF(q)
    printfn $"SPPF: %A{sppf}"
    printfn $"Reachable: %A{reachable}"
    
let example2 () =
    let graph = InputGraph([|InputGraph.CFGEdge(0<graphVertex>,1<graphVertex>)|])
    let startV = [|0<graphVertex>|]
    let q = RSM(HashSet<_>([0<rsmState>]), HashSet([0<rsmState>]),[|CFGEdge(0<rsmState>,0<rsmState>)|])
    let reachable = GLL.eval graph startV q [|0<rsmState>|]
    printfn $"Reachable: %A{reachable}"

let example3 () =
    let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,0<graphVertex>)|])
    let startV = [|0<graphVertex>|]
    let q = RSM(HashSet<_>([0<rsmState>]), HashSet([0<rsmState>]),[|TerminalEdge(0<rsmState>,0<terminalSymbol>,0<rsmState>)|])
    let reachable,matched = GLL.eval graph startV q [|0<rsmState>|]
    let sppf = matched.ToSPPF(q)
    printfn $"SPPF: %A{sppf}"
    printfn $"Reachable: %A{reachable}"

let example3_5 () =
    let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)
                             InputGraph.TerminalEdge(1<graphVertex>,0<terminalSymbol>,2<graphVertex>) |])
    let startV = [|0<graphVertex>|]
    let q = RSM(HashSet<_>([0<rsmState>]), HashSet([2<rsmState>]),
                [|
                  TerminalEdge(0<rsmState>,0<terminalSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
                  TerminalEdge(0<rsmState>,0<terminalSymbol>,2<rsmState>)|])
    let reachable,matched = GLL.eval graph startV q [|0<rsmState>|]
    let sppf = matched.ToSPPF(q)
    printfn $"SPPF: %A{sppf}"
    printfn $"Reachable: %A{reachable}"    

let example3_5_1 () =
    let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)
                             InputGraph.TerminalEdge(1<graphVertex>,0<terminalSymbol>,2<graphVertex>) |])
    let startV = [|0<graphVertex>|]
    let q = RSM(HashSet<_>([0<rsmState>]), HashSet([0<rsmState>; 2<rsmState>]),
                [|
                  TerminalEdge(0<rsmState>,0<terminalSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
                |])
    let reachable,matched = GLL.eval graph startV q [|0<rsmState>|]
    let sppf = matched.ToSPPF(q)
    printfn $"SPPF: %A{sppf}"
    printfn $"Reachable: %A{reachable}" 

let example3_5_2 () =
    let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)
                           |])
    let startV = [|0<graphVertex>|]
    let q = RSM(HashSet<_>([0<rsmState>]), HashSet([0<rsmState>; 2<rsmState>]),
                [|
                  TerminalEdge(0<rsmState>,0<terminalSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
                |])
    let reachable,matched = GLL.eval graph startV q [|0<rsmState>|]
    let sppf = matched.ToSPPF(q)
    printfn $"SPPF: %A{sppf}"
    printfn $"Reachable: %A{reachable}" 
    
let example4 () =
    let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)
                             InputGraph.TerminalEdge(1<graphVertex>,1<terminalSymbol>,2<graphVertex>) |])
    let startV = [|0<graphVertex>|]
    let q = RSM(HashSet<_>([0<rsmState>]), HashSet([0<rsmState>]),
                [|CFGEdge(0<rsmState>,0<rsmState>)
                  TerminalEdge(0<rsmState>,0<terminalSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
                  TerminalEdge(2<rsmState>,1<terminalSymbol>,0<rsmState>)|])
    let reachable,matched = GLL.eval graph startV q [|0<rsmState>|]
    let sppf = matched.ToSPPF(q)
    printfn $"SPPF: %A{sppf}"
    printfn $"Reachable: %A{reachable}"    

let example5 startV =
    let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)
                             InputGraph.TerminalEdge(1<graphVertex>,0<terminalSymbol>,2<graphVertex>)
                             InputGraph.TerminalEdge(2<graphVertex>,0<terminalSymbol>,0<graphVertex>)
                             
                             InputGraph.TerminalEdge(0<graphVertex>,1<terminalSymbol>,3<graphVertex>)
                             InputGraph.TerminalEdge(3<graphVertex>,1<terminalSymbol>,0<graphVertex>) |])
    let q = RSM(HashSet<_>([0<rsmState>]), HashSet([0<rsmState>]),
                [|CFGEdge(0<rsmState>,0<rsmState>)
                  TerminalEdge(0<rsmState>,0<terminalSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
                  TerminalEdge(2<rsmState>,1<terminalSymbol>,0<rsmState>)|])
    let reachable = GLL.eval graph startV q [|0<rsmState>|]
    printfn $"Reachable: %A{reachable}"    

let example6 () =
    let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,1<graphVertex>)
                             InputGraph.TerminalEdge(1<graphVertex>,0<terminalSymbol>,2<graphVertex>)
                             
                             InputGraph.TerminalEdge(2<graphVertex>,1<terminalSymbol>,3<graphVertex>)
                             InputGraph.TerminalEdge(3<graphVertex>,1<terminalSymbol>,4<graphVertex>) |])
    let startV = [|0<graphVertex>|]
    let q = RSM(HashSet<_>([0<rsmState>]), HashSet([0<rsmState>]),
                [|CFGEdge(0<rsmState>,0<rsmState>)
                  TerminalEdge(0<rsmState>,0<terminalSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
                  TerminalEdge(2<rsmState>,1<terminalSymbol>,0<rsmState>)|])
    let reachable = GLL.eval graph startV q [|0<rsmState>|]
    printfn $"Reachable: %A{reachable}"    

let example7 () =
    let graph = InputGraph([|InputGraph.TerminalEdge(0<graphVertex>,0<terminalSymbol>,0<graphVertex>)
                             
                             InputGraph.TerminalEdge(0<graphVertex>,1<terminalSymbol>,1<graphVertex>)
                             InputGraph.TerminalEdge(1<graphVertex>,1<terminalSymbol>,0<graphVertex>) |])
    let startV = [|0<graphVertex>|]
    let q = RSM(HashSet<_>([0<rsmState>]), HashSet([0<rsmState>]),
                [|CFGEdge(0<rsmState>,0<rsmState>)
                  TerminalEdge(0<rsmState>,0<terminalSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
                  TerminalEdge(2<rsmState>,1<terminalSymbol>,0<rsmState>)|])
    let reachable = GLL.eval graph startV q [|0<rsmState>|]
    printfn $"Reachable: %A{reachable}"    


let example8 startV =
    let graph = InputGraph([|InputGraph.CFGEdge(0<graphVertex>,1<graphVertex>)
                             InputGraph.CFGEdge(1<graphVertex>,4<graphVertex>)
                             InputGraph.CFGEdge(2<graphVertex>,1<graphVertex>)
                             InputGraph.CFGEdge(2<graphVertex>,3<graphVertex>)
                             InputGraph.CFGEdge(5<graphVertex>,2<graphVertex>)
                             InputGraph.TerminalEdge(4<graphVertex>,0<terminalSymbol>,6<graphVertex>)
                             InputGraph.TerminalEdge(10<graphVertex>,1<terminalSymbol>,5<graphVertex>)
                             
                             InputGraph.CFGEdge(6<graphVertex>,7<graphVertex>)
                             InputGraph.CFGEdge(7<graphVertex>,9<graphVertex>)
                             InputGraph.CFGEdge(8<graphVertex>,9<graphVertex>)
                             InputGraph.CFGEdge(9<graphVertex>,10<graphVertex>)
                             InputGraph.TerminalEdge(7<graphVertex>,2<terminalSymbol>,6<graphVertex>)
                             InputGraph.TerminalEdge(10<graphVertex>,3<terminalSymbol>,8<graphVertex>)
                             
                             InputGraph.CFGEdge(11<graphVertex>,12<graphVertex>)
                             InputGraph.CFGEdge(12<graphVertex>,13<graphVertex>)
                             InputGraph.CFGEdge(14<graphVertex>,12<graphVertex>)
                             InputGraph.CFGEdge(14<graphVertex>,15<graphVertex>)
                             InputGraph.TerminalEdge(13<graphVertex>,4<terminalSymbol>,6<graphVertex>)
                             InputGraph.TerminalEdge(10<graphVertex>,5<terminalSymbol>,14<graphVertex>)
                             
                             |])
    
    let q = RSM(HashSet<_>([0<rsmState>]), HashSet([0<rsmState>]),
                [|CFGEdge(0<rsmState>,0<rsmState>)
                  TerminalEdge(0<rsmState>,0<terminalSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
                  TerminalEdge(2<rsmState>,1<terminalSymbol>,0<rsmState>)
                  
                  TerminalEdge(0<rsmState>,2<terminalSymbol>,3<rsmState>)
                  NonTerminalEdge(3<rsmState>,0<rsmState>,4<rsmState>)
                  TerminalEdge(4<rsmState>,3<terminalSymbol>,0<rsmState>)
                  
                  TerminalEdge(0<rsmState>,4<terminalSymbol>,5<rsmState>)
                  NonTerminalEdge(5<rsmState>,0<rsmState>,6<rsmState>)
                  TerminalEdge(6<rsmState>,5<terminalSymbol>,0<rsmState>)
                  
                  TerminalEdge(0<rsmState>,0<terminalSymbol>,0<rsmState>)
                  TerminalEdge(0<rsmState>,2<terminalSymbol>,0<rsmState>)
                  TerminalEdge(0<rsmState>,4<terminalSymbol>,0<rsmState>)
                  |])
    let reachable,matched = GLL.eval graph startV q [|0<rsmState>|]
    printfn "Reachable:"
    reachable |> Seq.iter (printf "%A, ")    

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
        else edges.Add (InputGraph.CFGEdge (a.[0] |> int |> LanguagePrimitives.Int32WithMeasure,
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
    RSM(HashSet<_>([0<rsmState>]), HashSet([3<rsmState>]),
                [|TerminalEdge(0<rsmState>,1<terminalSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
                  TerminalEdge(1<rsmState>,0<terminalSymbol>,3<rsmState>)
                  TerminalEdge(2<rsmState>,0<terminalSymbol>,3<rsmState>)
                  
                  TerminalEdge(0<rsmState>,3<terminalSymbol>,4<rsmState>)
                  NonTerminalEdge(4<rsmState>,0<rsmState>,5<rsmState>)
                  TerminalEdge(4<rsmState>,2<terminalSymbol>,3<rsmState>)
                  TerminalEdge(5<rsmState>,2<terminalSymbol>,3<rsmState>)|])
let example10_go_hierarchy () =
    let graph = loadGraphFromCSV "/home/gsv/Downloads/go_hierarchy.csv" defaultMap
    let nodes = loadNodesFormCSV "/home/gsv/Downloads/go_hierarchy_nodes.csv"
    nodes
    |> Array.iter (fun n ->
        let reachable = GLL.eval graph [|n|] g1 [|0<rsmState>|]
        printfn $"Reachable: %A{reachable}")

let example11_go_allPairs () =
    let graph = loadGraphFromCSV "/home/gsv/Downloads/go.csv" defaultMap
    let reachable,matched = GLL.eval graph (graph.AllVertices()) g1 [|0<rsmState>|]
    printfn $"Reachable: %A{reachable.Count}"

let example11_go_singleSourceForAll () =
    let graph = loadGraphFromCSV "/home/gsv/Downloads/go.csv" defaultMap
    for n in graph.AllVertices() do
        let reachable,matched = GLL.eval graph [|n|] g1 [|0<rsmState>|]
        printfn $"Reachable: %A{reachable.Count}"
        
let example12_go_hierarchy_singleSourceForAll () =
    let graph = loadGraphFromCSV "/home/gsv/Downloads/go_hierarchy.csv" defaultMap
    for n in graph.AllVertices() do
        let reachable,matched = GLL.eval graph [|n|] g1 [|0<rsmState>|]
        printfn $"Reachable: %A{reachable.Count}"
    
let example12_go_hierarchy_allPairs () =
    let graph = loadGraphFromCSV "/home/gsv/Downloads/go_hierarchy.csv" defaultMap
    let reachable,matched = GLL.eval graph (graph.AllVertices()) g1 [|0<rsmState>|]
    printfn $"Reachable: %A{reachable.Count}"
    
[<EntryPoint>]
let main argv =   
    //example1 ()
    //example2 ()
    //example3 ()
    //example3_5 ()
    //example3_5_2 ()
    example3_5_1 ()
    //example4 ()
    //example5 [|1<graphVertex>|]
    (*example6 ()
    example7 ()    
    example8 [|0<graphVertex>; 11<graphVertex>; 6<graphVertex>|]
    *)//example9 3000 [|1<graphVertex>|]
    //example10_go_hierarchy ()
    (*example11_go_allPairs ()
    *)//example12_go_hierarchy_singleSourceForAll ()
    //example11_go_singleSourceForAll ()
    //example12_go_hierarchy_allPairs()
    0 // return an integer exit code