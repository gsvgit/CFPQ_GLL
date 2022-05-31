open System.Collections.Generic
open Argu
open Argu.ArguAttributes
open CFPQ_GLL
open CFPQ_GLL.GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM

type ArgQueryMode =
    | All_Paths = 0
    | Reachability_Only = 1

type Arguments =
    | [<Mandatory>] Graph of string
    | Parallel of int
    | Mode of ArgQueryMode
    
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Graph _ -> "File with graph"
            | Parallel _ -> "Run naive parallel version with block of specified size."
            | Mode _ -> "Mode of query."
 
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

let loadJavaGraphFromCSV file =
    let mutable terminalSymbolCount = 0
    let terminalSymbolsMapping = SortedDictionary<_,_>()
    let edges = ResizeArray<_>()
    System.IO.File.ReadLines file
    |> Seq.map (fun s -> s.Split " ")
    |> Seq.iter (fun a ->
        let terminal, reversedTerminal =
            if terminalSymbolsMapping.ContainsKey a.[1]
            then terminalSymbolsMapping.[a.[1]]
            else
                let terminalId = terminalSymbolCount * 1<terminalSymbol>
                terminalSymbolsMapping.Add(a.[1],(terminalId, terminalId + 1<terminalSymbol>))
                terminalSymbolCount <- terminalSymbolCount + 2
                terminalId , (terminalId + 1<terminalSymbol>) 
            
            
        edges.Add (InputGraph.TerminalEdge(a.[0] |> int |> LanguagePrimitives.Int32WithMeasure
                                           , terminal 
                                           , a.[2] |> int |> LanguagePrimitives.Int32WithMeasure))
        edges.Add (InputGraph.TerminalEdge(a.[2] |> int |> LanguagePrimitives.Int32WithMeasure
                                           , reversedTerminal
                                           , a.[0] |> int |> LanguagePrimitives.Int32WithMeasure))
        
        )
    InputGraph <| edges.ToArray()
    , terminalSymbolsMapping


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

type LoadStorePairsInfo =
    val LoadTerminalId : int<terminalSymbol>
    val LoadTerminalReversedId : int<terminalSymbol>
    val StoreTerminalId : int<terminalSymbol>
    val StoreTerminalReversedId : int<terminalSymbol>
    new (loadTerminalInfo, storeTerminalInfo) =
        {
            LoadTerminalId = fst loadTerminalInfo
            LoadTerminalReversedId = snd loadTerminalInfo
            StoreTerminalId = fst storeTerminalInfo
            StoreTerminalReversedId = snd storeTerminalInfo
        }
let javaRsm (terminalSymbolsMapping:SortedDictionary<string,_>) =
    
    let alloc = fst terminalSymbolsMapping.["alloc"]
    let alloc_r = snd terminalSymbolsMapping.["alloc"]
    let assign = fst terminalSymbolsMapping.["assign"]
    let assign_r = snd terminalSymbolsMapping.["assign"]
    
    let loadStorePairs =
        [|
          for kvp in terminalSymbolsMapping do
            if kvp.Key.StartsWith "load_"
            then
                let store = "store_" + (kvp.Key.Split '_').[1]
                if terminalSymbolsMapping.ContainsKey(store)
                then yield LoadStorePairsInfo(kvp.Value, terminalSymbolsMapping.[store])
        |]
               
    let mutable freeStateId = 7<rsmState>
     
    let pointsTo =
        RSMBox(
           0<rsmState>,
           HashSet([1<rsmState>]),
           [|
              TerminalEdge(0<rsmState>, alloc, 1<rsmState>)
              TerminalEdge(0<rsmState>, assign, 0<rsmState>)               
              for loadStorePair in loadStorePairs do
                  yield! [|
                      TerminalEdge(0<rsmState>, loadStorePair.LoadTerminalId, freeStateId)
                      NonTerminalEdge(freeStateId, 4<rsmState>, freeStateId + 1<rsmState>)
                      TerminalEdge(freeStateId + 1<rsmState>, loadStorePair.StoreTerminalId, 0<rsmState>)
                  |]
                  freeStateId <- freeStateId + 2<rsmState>
           |]           
        )
    let flowsTo =
        RSMBox(
           2<rsmState>,
           HashSet([3<rsmState>]),
           [|
              TerminalEdge(2<rsmState>, alloc_r, 3<rsmState>)
              TerminalEdge(3<rsmState>, assign_r, 3<rsmState>)
              for loadStorePair in loadStorePairs do
                  yield! [|
                      TerminalEdge(3<rsmState>, loadStorePair.StoreTerminalReversedId, freeStateId)
                      NonTerminalEdge(freeStateId, 4<rsmState>, freeStateId + 1<rsmState>)
                      TerminalEdge(freeStateId + 1<rsmState>, loadStorePair.LoadTerminalReversedId, 3<rsmState>)
                  |]
                  freeStateId <- freeStateId + 2<rsmState>
           |]
        )
    let alias =
       RSMBox(
              4<rsmState>,
              HashSet([6<rsmState>]),
              [|
                  NonTerminalEdge(4<rsmState>, 0<rsmState>, 5<rsmState>)
                  NonTerminalEdge(5<rsmState>, 2<rsmState>, 6<rsmState>)              
              |]
              )
       
    RSM([|alias; pointsTo; flowsTo|], alias)
    
let runJava evaluator filePath =
    let graph,mapping = loadJavaGraphFromCSV filePath 
    let q = javaRsm mapping
    printfn "Data loaded."
    printfn $"Vertices: %A{graph.NumberOfVertices()}"
    let start = System.DateTime.Now
    let res = evaluator graph (graph.AllVertices()) q
    printfn $"Total processing time: %A{(System.DateTime.Now - start).TotalMilliseconds} milliseconds"


[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<_>(programName = "Benchmarks.exe")
    let args = parser.Parse argv
    let graph = args.GetResult Graph
    let mode =
        match args.GetResult Mode with
        | ArgQueryMode.All_Paths -> Mode.AllPaths
        | ArgQueryMode.Reachability_Only -> Mode.ReachabilityOnly
        | x -> failwithf $"Unexpected query mode: %A{x}."
        
    if args.Contains Parallel
    then
        let blockSize = args.GetResult Parallel
        runJava (fun graph vertices q -> evalParallel blockSize graph vertices q mode) graph
    else
        runJava (fun graph vertices q -> eval graph vertices q mode) graph
    (*let mode =
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
    *)
    
    //let graph,mapping = loadJavaGraphFromCSV "/media/gsv/PSI 2015/graphs/tradebeans/data.csv"
    //let graph,mapping = loadJavaGraphFromCSV "/media/gsv/PSI 2015/graphs/jython/data.csv"
    //"/media/gsv/PSI 2015/graphs/pmd/data.csv"
        
    (*for n in graph.AllVertices() do
        printfn $"%i{n}"
        eval graph [|n|] q AllPaths*)
        //printfn $"%A{res}"
    //runExample example1
    //example10_go_hierarchy()
    //example11_go_allPairs ()
    //example11_go_singleSourceForAll ()
    //example12_go_hierarchy_allPairs ()
    //example12_go_hierarchy_singleSourceForAll ()
    
      
    0 // return an integer exit code