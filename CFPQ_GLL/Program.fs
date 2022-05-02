// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM

// Define a function to construct a message to print
let from whom =
    $"from %s{whom}"

let example1 () =
    let graph = InputGraph([|InputGraph.CFGEdge(0<graphVertex>,1<graphVertex>)|])
    let startV = [|0<graphVertex>|]
    let q = RSM(0<rsmState>, System.Collections.Generic.HashSet([1<rsmState>]),[|CFGEdge(0<rsmState>,1<rsmState>)|])
    let reachable = GLL.eval graph startV q
    printfn $"Reachable: %A{reachable}"
    
let example2 () =
    let graph = InputGraph([|InputGraph.CFGEdge(0<graphVertex>,1<graphVertex>)|])
    let startV = [|0<graphVertex>|]
    let q = RSM(0<rsmState>, System.Collections.Generic.HashSet([0<rsmState>]),[|CFGEdge(0<rsmState>,0<rsmState>)|])
    let reachable = GLL.eval graph startV q
    printfn $"Reachable: %A{reachable}"

let example3 () =
    let graph = InputGraph([|InputGraph.CFGEdge(0<graphVertex>,0<graphVertex>)|])
    let startV = [|0<graphVertex>|]
    let q = RSM(0<rsmState>, System.Collections.Generic.HashSet([0<rsmState>]),[|CFGEdge(0<rsmState>,0<rsmState>)|])
    let reachable = GLL.eval graph startV q
    printfn $"Reachable: %A{reachable}"
    
let example4 () =
    let graph = InputGraph([|InputGraph.CallEdge(0<graphVertex>,0<callSymbol>,1<graphVertex>)
                             InputGraph.ReturnEdge(1<graphVertex>,0<returnSymbol>,2<graphVertex>) |])
    let startV = [|0<graphVertex>|]
    let q = RSM(0<rsmState>, System.Collections.Generic.HashSet([0<rsmState>]),
                [|CFGEdge(0<rsmState>,0<rsmState>)
                  CallEdge(0<rsmState>,0<callSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,2<rsmState>)
                  ReturnEdge(2<rsmState>,0<returnSymbol>,0<rsmState>)|])
    let reachable = GLL.eval graph startV q
    printfn $"Reachable: %A{reachable}"    

let example5 startV =
    let graph = InputGraph([|InputGraph.CallEdge(0<graphVertex>,0<callSymbol>,1<graphVertex>)
                             InputGraph.CallEdge(1<graphVertex>,0<callSymbol>,2<graphVertex>)
                             InputGraph.CallEdge(2<graphVertex>,0<callSymbol>,0<graphVertex>)
                             InputGraph.ReturnEdge(0<graphVertex>,0<returnSymbol>,3<graphVertex>)
                             InputGraph.ReturnEdge(3<graphVertex>,0<returnSymbol>,0<graphVertex>) |])
    let q = RSM(0<rsmState>, System.Collections.Generic.HashSet([0<rsmState>]),
                [|CFGEdge(0<rsmState>,0<rsmState>)
                  CallEdge(0<rsmState>,0<callSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,2<rsmState>)
                  ReturnEdge(2<rsmState>,0<returnSymbol>,0<rsmState>)|])
    let reachable = GLL.eval graph startV q
    printfn $"Reachable: %A{reachable}"    

let example6 () =
    let graph = InputGraph([|InputGraph.CallEdge(0<graphVertex>,0<callSymbol>,1<graphVertex>)
                             InputGraph.CallEdge(1<graphVertex>,0<callSymbol>,2<graphVertex>)
                             InputGraph.ReturnEdge(2<graphVertex>,0<returnSymbol>,3<graphVertex>)
                             InputGraph.ReturnEdge(3<graphVertex>,0<returnSymbol>,4<graphVertex>) |])
    let startV = [|0<graphVertex>|]
    let q = RSM(0<rsmState>, System.Collections.Generic.HashSet([0<rsmState>]),
                [|CFGEdge(0<rsmState>,0<rsmState>)
                  CallEdge(0<rsmState>,0<callSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,2<rsmState>)
                  ReturnEdge(2<rsmState>,0<returnSymbol>,0<rsmState>)|])
    let reachable = GLL.eval graph startV q
    printfn $"Reachable: %A{reachable}"    

let example7 () =
    let graph = InputGraph([|InputGraph.CallEdge(0<graphVertex>,0<callSymbol>,0<graphVertex>)
                             InputGraph.ReturnEdge(0<graphVertex>,0<returnSymbol>,1<graphVertex>)
                             InputGraph.ReturnEdge(1<graphVertex>,0<returnSymbol>,0<graphVertex>) |])
    let startV = [|0<graphVertex>|]
    let q = RSM(0<rsmState>, System.Collections.Generic.HashSet([0<rsmState>]),
                [|CFGEdge(0<rsmState>,0<rsmState>)
                  CallEdge(0<rsmState>,0<callSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,2<rsmState>)
                  ReturnEdge(2<rsmState>,0<returnSymbol>,0<rsmState>)|])
    let reachable = GLL.eval graph startV q
    printfn $"Reachable: %A{reachable}"    


let example8 startV =
    let graph = InputGraph([|InputGraph.CFGEdge(0<graphVertex>,1<graphVertex>)
                             InputGraph.CFGEdge(1<graphVertex>,4<graphVertex>)
                             InputGraph.CFGEdge(2<graphVertex>,1<graphVertex>)
                             InputGraph.CFGEdge(2<graphVertex>,3<graphVertex>)
                             InputGraph.CFGEdge(5<graphVertex>,2<graphVertex>)
                             InputGraph.CallEdge(4<graphVertex>,0<callSymbol>,6<graphVertex>)
                             InputGraph.ReturnEdge(10<graphVertex>,0<returnSymbol>,5<graphVertex>)
                             
                             InputGraph.CFGEdge(6<graphVertex>,7<graphVertex>)
                             InputGraph.CFGEdge(7<graphVertex>,9<graphVertex>)
                             InputGraph.CFGEdge(8<graphVertex>,9<graphVertex>)
                             InputGraph.CFGEdge(9<graphVertex>,10<graphVertex>)
                             InputGraph.CallEdge(7<graphVertex>,1<callSymbol>,6<graphVertex>)
                             InputGraph.ReturnEdge(10<graphVertex>,1<returnSymbol>,8<graphVertex>)
                             
                             InputGraph.CFGEdge(11<graphVertex>,12<graphVertex>)
                             InputGraph.CFGEdge(12<graphVertex>,13<graphVertex>)
                             InputGraph.CFGEdge(14<graphVertex>,12<graphVertex>)
                             InputGraph.CFGEdge(14<graphVertex>,15<graphVertex>)
                             InputGraph.CallEdge(13<graphVertex>,2<callSymbol>,6<graphVertex>)
                             InputGraph.ReturnEdge(10<graphVertex>,2<returnSymbol>,14<graphVertex>)
                             
                             |])
    
    let q = RSM(0<rsmState>, System.Collections.Generic.HashSet([0<rsmState>]),
                [|CFGEdge(0<rsmState>,0<rsmState>)
                  CallEdge(0<rsmState>,0<callSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,2<rsmState>)
                  ReturnEdge(2<rsmState>,0<returnSymbol>,0<rsmState>)
                  
                  CallEdge(0<rsmState>,1<callSymbol>,3<rsmState>)
                  NonTerminalEdge(3<rsmState>,4<rsmState>)
                  ReturnEdge(4<rsmState>,1<returnSymbol>,0<rsmState>)
                  
                  CallEdge(0<rsmState>,2<callSymbol>,5<rsmState>)
                  NonTerminalEdge(5<rsmState>,6<rsmState>)
                  ReturnEdge(6<rsmState>,2<returnSymbol>,0<rsmState>)
                  
                  CallEdge(0<rsmState>,0<callSymbol>,0<rsmState>)
                  CallEdge(0<rsmState>,1<callSymbol>,0<rsmState>)
                  CallEdge(0<rsmState>,2<callSymbol>,0<rsmState>)
                  |])
    let reachable = GLL.eval graph startV q
    printfn "Reachable:"
    reachable |> Seq.iter (printf "%A, ")    

let example9 n startV =
    let callEdges = [|for i in 0 .. n -> InputGraph.CallEdge(LanguagePrimitives.Int32WithMeasure i,0<callSymbol>,LanguagePrimitives.Int32WithMeasure (i+1)) |]
    let returnEdges = [|for i in n + 2 .. 2 * n + 2 -> InputGraph.ReturnEdge(LanguagePrimitives.Int32WithMeasure i,0<returnSymbol>,LanguagePrimitives.Int32WithMeasure (i+1)) |]
    let graph = InputGraph(Array.concat [
                                         callEdges; returnEdges
                                         [|InputGraph.CallEdge(LanguagePrimitives.Int32WithMeasure (n+1),0<callSymbol>,LanguagePrimitives.Int32WithMeasure 0)|]
                                         [|
                                           InputGraph.ReturnEdge(LanguagePrimitives.Int32WithMeasure 0,0<returnSymbol>,LanguagePrimitives.Int32WithMeasure(n + 2))
                                           InputGraph.ReturnEdge(LanguagePrimitives.Int32WithMeasure (2*n+3),0<returnSymbol>,LanguagePrimitives.Int32WithMeasure 0)
                                           |]
                                         
                                         ])
    let q = RSM(0<rsmState>, HashSet([0<rsmState>]),
                [|CFGEdge(0<rsmState>,0<rsmState>)
                  CallEdge(0<rsmState>,0<callSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,2<rsmState>)
                  ReturnEdge(2<rsmState>,0<returnSymbol>,0<rsmState>)|])
    let reachable = GLL.eval graph startV q
    printfn $"Reachable: %A{reachable}" 
    
let loadGraphFromCSV file (callLabelsMappings:Dictionary<_,_>) =
    let edges = ResizeArray<_>()
    System.IO.File.ReadLines file
    |> Seq.map (fun s -> s.Split " ")
    |> Seq.iter (fun a ->
        if callLabelsMappings.ContainsKey a.[2]
        then
            edges.Add (InputGraph.CallEdge(a.[0] |> int |> LanguagePrimitives.Int32WithMeasure
                                               , callLabelsMappings.[a.[2]] |> LanguagePrimitives.Int32WithMeasure
                                               , a.[1] |> int |> LanguagePrimitives.Int32WithMeasure))
            edges.Add (InputGraph.ReturnEdge(a.[1] |> int |> LanguagePrimitives.Int32WithMeasure
                                               , callLabelsMappings.[a.[2]] |> LanguagePrimitives.Int32WithMeasure
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
    res.Add("subClassOf",0)
    res.Add("type",1)
    res
let g1 =
    RSM(0<rsmState>, HashSet([3<rsmState>]),
                [|ReturnEdge(0<rsmState>,0<returnSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,2<rsmState>)
                  CallEdge(1<rsmState>,0<callSymbol>,3<rsmState>)
                  CallEdge(2<rsmState>,0<callSymbol>,3<rsmState>)
                  
                  ReturnEdge(0<rsmState>,1<returnSymbol>,4<rsmState>)
                  NonTerminalEdge(4<rsmState>,5<rsmState>)
                  CallEdge(4<rsmState>,1<callSymbol>,3<rsmState>)
                  CallEdge(5<rsmState>,1<callSymbol>,3<rsmState>)|])
let example10_go_hierarchy () =
    let graph = loadGraphFromCSV "/home/gsv/Downloads/go_hierarchy.csv" defaultMap
    let nodes = loadNodesFormCSV "/home/gsv/Downloads/go_hierarchy_nodes.csv"
    nodes
    |> Array.iter (fun n ->
        let reachable = GLL.eval graph [|n|] g1 
        printfn $"Reachable: %A{reachable}")

let example11_go_allPairs () =
    let graph = loadGraphFromCSV "/home/gsv/Downloads/go.csv" defaultMap
    let reachable = GLL.eval graph (graph.AllVertices()) g1 
    printfn $"Reachable: %A{reachable.Count}"

let example11_go_singleSourceForAll () =
    let graph = loadGraphFromCSV "/home/gsv/Downloads/go.csv" defaultMap
    for n in graph.AllVertices() do
        let reachable = GLL.eval graph [|n|] g1 
        printfn $"Reachable: %A{reachable.Count}"
    
    
[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    example1 ()
    example2 ()
    example3 ()
    example4 ()
    example5 [|1<graphVertex>|]
    example6 ()
    example7 ()    
    example8 [|0<graphVertex>; 11<graphVertex>; 6<graphVertex>|]
    //example9 3000 [|1<graphVertex>|]
    //example10_go_hierarchy ()
    example11_go_allPairs ()
    //example11_go_singleSourceForAll ()
    0 // return an integer exit code