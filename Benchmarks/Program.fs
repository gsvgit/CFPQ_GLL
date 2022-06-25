open System.Collections.Generic
open Argu
open CFPQ_GLL
open CFPQ_GLL.GLL
open CFPQ_GLL.GSS
open Tests.InputGraph
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF
open CFPQ_GLL.InputGraph

type ArgQueryMode =
    | All_Paths = 0
    | Reachability_Only = 1
    
type ArgTaskType =
    | All_Pairs = 0
    | Single_Source = 1
    | Single_Source_Continuously = 2
    
type ArgQuery =
    | G1 = 0
    | G2 = 1
    | Geo = 2
    | Andersen = 3
    | Java = 4

type Arguments =
    | [<Mandatory>] Graph of string
    | Parallel of int
    | [<Mandatory>] Mode of ArgQueryMode
    | [<Mandatory>] TaskType of ArgTaskType
    | [<Mandatory>] Query of ArgQuery
    
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Graph _ -> "File with graph"
            | Parallel _ -> "Run naive parallel version with block of specified size."
            | Mode _ -> "Mode of query."
            | TaskType _ -> "Task type."
            | Query _ -> "Query to evaluate: one of predefined queries."
 
let loadGraphFromCSV file (callLabelsMappings:Dictionary<_,_>) =
    let edges = ResizeArray<_>()
    System.IO.File.ReadLines file
    |> Seq.map (fun s -> s.Split " ")
    |> Seq.iter (fun a ->
        if callLabelsMappings.ContainsKey a.[2]
        then
            edges.Add (Tests.InputGraph.TerminalEdge (a.[0] |> int |> LanguagePrimitives.Int32WithMeasure
                                               , callLabelsMappings.[a.[2]] |> fst |> LanguagePrimitives.Int32WithMeasure
                                               , a.[1] |> int |> LanguagePrimitives.Int32WithMeasure))
            edges.Add (Tests.InputGraph.TerminalEdge(a.[1] |> int |> LanguagePrimitives.Int32WithMeasure
                                               , callLabelsMappings.[a.[2]] |> snd |> LanguagePrimitives.Int32WithMeasure
                                               , a.[0] |> int |> LanguagePrimitives.Int32WithMeasure))
        else edges.Add (Tests.InputGraph.TerminalEdge (a.[0] |> int |> LanguagePrimitives.Int32WithMeasure,
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
            
            
        edges.Add (Tests.InputGraph.TerminalEdge(a.[0] |> int |> LanguagePrimitives.Int32WithMeasure
                                           , terminal 
                                           , a.[2] |> int |> LanguagePrimitives.Int32WithMeasure))
        edges.Add (Tests.InputGraph.TerminalEdge(a.[2] |> int |> LanguagePrimitives.Int32WithMeasure
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
    
let runAllPairs parallelBlocks (graph:InputGraph) q mode =     
    let start = System.DateTime.Now
    match parallelBlocks with
    | None -> eval graph (graph.AllVertices()) q mode
    | Some blockSize -> evalParallel blockSize graph (graph.AllVertices()) q mode
    |> ignore
    printfn $"Total processing time: %A{(System.DateTime.Now - start).TotalMilliseconds} milliseconds"

  
let singleSourceForAllContinuously (graph:InputGraph) q mode =    
    let mutable gss = GSS()
    let mutable matchedRanges = MatchedRanges(q)
    let vertices =
        //[|116292<graphVertex>; 116291<graphVertex>; 116293<graphVertex>; 116294<graphVertex>; 116295<graphVertex>; 116296<graphVertex>; 116297<graphVertex>|]
        graph.AllVertices()
    for n in vertices do
        printfn $"V: %i{n}"
        let startVertices = [|n|]
        let reachableVertices =
            let d = Dictionary<_,_>(startVertices.Length)
            startVertices
            |> Array.iter (fun v -> d.Add(v, HashSet<_>()))
            d
        let res, newGss = evalFromState reachableVertices gss matchedRanges graph startVertices q mode
        gss <-newGss 
        match res with
        | QueryResult.MatchedRanges ranges -> matchedRanges <- ranges
        | _ -> ()
        
let singleSourceForAll (graph:InputGraph) q mode =        
    for n in graph.AllVertices() do
        let startVertices = [|n|]        
        eval graph startVertices q mode |> ignore            

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<_>(programName = "dotnet Benchmarks.dll")
    let args = parser.Parse argv
       
    let graph, query =
        match args.GetResult Query with
        | ArgQuery.G1 ->
            let graph = loadGraphFromCSV (args.GetResult Graph) defaultMap
            graph, g1
        | ArgQuery.Java ->
            let graph,mapping = loadJavaGraphFromCSV (args.GetResult Graph)
            graph, javaRsm mapping
        | x -> failwithf $"Unexpected query: %A{x}."
    
    printfn "Data loaded."
    printfn $"Vertices: %A{graph.NumberOfVertices()}"
    
    let mode =
        match args.GetResult Mode with
        | ArgQueryMode.All_Paths -> Mode.AllPaths
        | ArgQueryMode.Reachability_Only -> Mode.ReachabilityOnly
        | x -> failwithf $"Unexpected query mode: %A{x}."
        
    match args.GetResult TaskType with
    | ArgTaskType.All_Pairs ->        
        let parallelBlockSize =
            if args.Contains Parallel
            then Some <| args.GetResult Parallel
            else None
        runAllPairs parallelBlockSize graph query mode
    | ArgTaskType.Single_Source ->        
        singleSourceForAll graph query mode
    | ArgTaskType.Single_Source_Continuously ->        
        singleSourceForAllContinuously graph query mode
    | x -> failwithf $"Unexpected task type: %A{x}."
      
    0 // return an integer exit code