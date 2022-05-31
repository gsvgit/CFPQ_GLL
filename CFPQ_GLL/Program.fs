open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.BTree
open CFPQ_GLL.GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF

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

let allPairs blockSize filePath mode =
    let start = System.DateTime.Now 
    let graph = loadGraphFromCSV filePath defaultMap
    let res = evalParallel blockSize graph (graph.AllVertices()) g1 mode
    //matched.Statistics () |> printfn "%A"
    printfn $"Total processing time: %A{(System.DateTime.Now - start).TotalMilliseconds} milliseconds"
    //printfn $"Reachable: %A{res}"
    
let singleSourceForAll filePath mode =
    let graph = loadGraphFromCSV filePath defaultMap
    for n in graph.AllVertices() do
        let result = GLL.eval graph [|n|] g1 mode
        printfn $"Reachable: %A{result}"    
    
let example10_go_hierarchy () =
    let graph = loadGraphFromCSV "/home/gsv/Downloads/go_hierarchy.csv" defaultMap
    let nodes = loadNodesFormCSV "/home/gsv/Downloads/go_hierarchy_nodes.csv"
    nodes
    |> Array.iter (fun n ->
        let reachable = GLL.eval graph [|n|] g1
        printfn $"Reachable: %A{reachable}")

    
[<EntryPoint>]
let main argv =
    let mode =
         if argv.[1] = "r"
         then Mode.ReachabilityOnly
         else Mode.AllPaths    
    if argv.[0] = "ss"
    then singleSourceForAll argv.[2] mode
    elif argv.[0] = "ap"
    then allPairs (int argv.[2]) argv.[3] mode
    else
        printfn "Unexpected parameters."
        printfn "Usage: [ss|ap] [r|p] blocksize? filepath"
    //runExample example1
    //example10_go_hierarchy()
    //example11_go_allPairs ()
    //example11_go_singleSourceForAll ()
    //example12_go_hierarchy_allPairs ()
    //example12_go_hierarchy_singleSourceForAll ()
    
      
    0 // return an integer exit code