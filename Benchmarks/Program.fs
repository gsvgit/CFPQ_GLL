open Benchmarks.ErrorRecoveringBenchmark

[<EntryPoint>]
let main _ =
    // let summary = BenchmarkRunner.Run<ErrorRecoveringBenchmark.ErrorRecoveringBenchmark>()
    // summary.ResultsDirectoryPath |> printfn "Results directory: %s"
    benchmarkData |> Array.iter (fun d ->
        printfn $"[{System.DateTime.Now}] Run {d.Name}"
        runGLL d
        printfn $"[{System.DateTime.Now}] Completed {d.Name}"
    )
    //BenchmarkDataGeneration.generateBenchmarkData [| 100; 1000; |] [| 0; 1 |]
    0

//type ArgQueryMode =
//    | All_Paths = 0
//    | Reachability_Only = 1
//
//type ArgTaskType =
//    | All_Pairs = 0
//    | Single_Source = 1
//    | Single_Source_Continuously = 2
//
//type ArgQuery =
//    | G1 = 0
//    | G2 = 1
//    | Geo = 2
//    | Andersen = 3
//    | Java = 4
//    | VSharp = 5
//
//type Arguments =
//    | [<Mandatory>] Graph of string
//    | [<Mandatory>] Mode of ArgQueryMode
//    | [<Mandatory>] TaskType of ArgTaskType
//    | [<Mandatory>] Query of ArgQuery
//
//    interface IArgParserTemplate with
//        member this.Usage =
//            match this with
//            | Graph _ -> "File with graph."
//            | Mode _ -> "Mode of query."
//            | TaskType _ -> "Task type."
//            | Query _ -> "Query to evaluate: one of predefined queries."
//
//let loadGraphFromCSV file (callLabelsMappings:Dictionary<_,_>) =
//    let edges = ResizeArray<_>()
//    System.IO.File.ReadLines file
//    |> Seq.map (fun s -> s.Split " ")
//    |> Seq.iter (fun a ->
//        if callLabelsMappings.ContainsKey a.[2]
//        then
//            edges.Add (Tests.InputGraph.TerminalEdge (a.[0] |> int |> LanguagePrimitives.Int32WithMeasure
//                                               , callLabelsMappings.[a.[2]] |> fst |> LanguagePrimitives.Int32WithMeasure
//                                               , a.[1] |> int |> LanguagePrimitives.Int32WithMeasure))
//            edges.Add (Tests.InputGraph.TerminalEdge(a.[1] |> int |> LanguagePrimitives.Int32WithMeasure
//                                               , callLabelsMappings.[a.[2]] |> snd |> LanguagePrimitives.Int32WithMeasure
//                                               , a.[0] |> int |> LanguagePrimitives.Int32WithMeasure))
//        else edges.Add (Tests.InputGraph.TerminalEdge (a.[0] |> int |> LanguagePrimitives.Int32WithMeasure,
//                                                 4<terminalSymbol>,
//                                                 a.[1] |> int |> LanguagePrimitives.Int32WithMeasure))
//            )
//    InputGraph <| edges.ToArray()
//
//let loadJavaGraphFromCSV file =
//    let mutable terminalSymbolCount = 0
//    let terminalSymbolsMapping = SortedDictionary<_,_>()
//    let edges = ResizeArray<_>()
//    System.IO.File.ReadLines file
//    |> Seq.map (fun s -> s.Split " ")
//    |> Seq.iter (fun a ->
//        let terminal, reversedTerminal =
//            if terminalSymbolsMapping.ContainsKey a.[1]
//            then terminalSymbolsMapping.[a.[1]]
//            else
//                let terminalId = terminalSymbolCount * 1<terminalSymbol>
//                terminalSymbolsMapping.Add(a.[1],(terminalId, terminalId + 1<terminalSymbol>))
//                terminalSymbolCount <- terminalSymbolCount + 2
//                terminalId , (terminalId + 1<terminalSymbol>)
//
//
//        edges.Add (Tests.InputGraph.TerminalEdge(a.[0] |> int |> LanguagePrimitives.Int32WithMeasure
//                                           , terminal
//                                           , a.[2] |> int |> LanguagePrimitives.Int32WithMeasure))
//        edges.Add (Tests.InputGraph.TerminalEdge(a.[2] |> int |> LanguagePrimitives.Int32WithMeasure
//                                           , reversedTerminal
//                                           , a.[0] |> int |> LanguagePrimitives.Int32WithMeasure))
//
//        )
//    InputGraph <| edges.ToArray()
//    , terminalSymbolsMapping
//
//
//let loadNodesFormCSV file =
//    System.IO.File.ReadLines file
//    |> Seq.map (int >> LanguagePrimitives.Int32WithMeasure)
//    |> Array.ofSeq
//
//let defaultMap =
//    let res = Dictionary<_,_>()
//    res.Add("subClassOf",(0,1))
//    res.Add("type",(2,3))
//    res
//let g1 =
//    let box,_ = GLLTests.makeRsmBox(Dictionary(), 0<rsmState>, HashSet([3<rsmState>]),
//                [|TerminalEdge(0<rsmState>,1<terminalSymbol>,1<rsmState>)
//                  NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
//                  TerminalEdge(1<rsmState>,0<terminalSymbol>,3<rsmState>)
//                  TerminalEdge(2<rsmState>,0<terminalSymbol>,3<rsmState>)
//
//                  TerminalEdge(0<rsmState>,3<terminalSymbol>,4<rsmState>)
//                  NonTerminalEdge(4<rsmState>,0<rsmState>,5<rsmState>)
//                  TerminalEdge(4<rsmState>,2<terminalSymbol>,3<rsmState>)
//                  TerminalEdge(5<rsmState>,2<terminalSymbol>,3<rsmState>)|])
//    RSM([|box|], box)
//
//let example10_go_hierarchy () =
//    let graph = loadGraphFromCSV "/home/gsv/Downloads/go_hierarchy.csv" defaultMap
//    let nodes = loadNodesFormCSV "/home/gsv/Downloads/go_hierarchy_nodes.csv"
//    nodes
//    |> Array.iter (fun n ->
//        let startVertices,_ = graph.ToCfpqCoreGraph (HashSet [|n|])
//        let reachable = GLL.defaultEval startVertices g1
//        printfn $"Reachable: %A{reachable}")
//
//type LoadStorePairsInfo =
//    val LoadTerminalId : int<terminalSymbol>
//    val LoadTerminalReversedId : int<terminalSymbol>
//    val StoreTerminalId : int<terminalSymbol>
//    val StoreTerminalReversedId : int<terminalSymbol>
//    new (loadTerminalInfo, storeTerminalInfo) =
//        {
//            LoadTerminalId = fst loadTerminalInfo
//            LoadTerminalReversedId = snd loadTerminalInfo
//            StoreTerminalId = fst storeTerminalInfo
//            StoreTerminalReversedId = snd storeTerminalInfo
//        }
//
//let vSharRSM maxCallSymbol =
//    let firstFreeCallTerminalId = maxCallSymbol + 1<terminalSymbol>
//    let terminalForCFGEdge = 0<terminalSymbol>
//    let balancedBracketsBox,m =
//      let mutable firstFreeRsmState = 3<rsmState>
//      GLLTests.makeRsmBox(
//          Dictionary(),
//          2<rsmState>,
//          HashSet [|2<rsmState>|],
//          [|
//              yield RSMEdges.TerminalEdge (2<rsmState>, terminalForCFGEdge, 2<rsmState>)
//              for callSymbol in 1<terminalSymbol> .. 2<terminalSymbol> .. firstFreeCallTerminalId - 1<terminalSymbol> do
//                  yield RSMEdges.TerminalEdge(2<rsmState>, callSymbol, firstFreeRsmState)
//                  yield RSMEdges.NonTerminalEdge(firstFreeRsmState, 2<rsmState>, firstFreeRsmState + 1<rsmState>)
//                  yield RSMEdges.TerminalEdge(firstFreeRsmState + 1<rsmState>, callSymbol + 1<terminalSymbol>, 2<rsmState>)
//                  firstFreeRsmState <- firstFreeRsmState + 2<rsmState>
//          |])
//    let startBox,_ =
//            GLLTests.makeRsmBox(
//                m,
//                0<rsmState>,
//                HashSet [|0<rsmState>; 1<rsmState>|],
//                [|
//                    yield RSMEdges.NonTerminalEdge(0<rsmState>, 2<rsmState>, 1<rsmState>)
//                    for callSymbol in 2<terminalSymbol> .. 2<terminalSymbol> .. firstFreeCallTerminalId - 1<terminalSymbol> do
//                      yield RSMEdges.TerminalEdge(1<rsmState>, callSymbol, 0<rsmState>)
//                |]
//                )
//
//    (*let startBox =
//            RSMBox(
//                0<rsmState>,
//                HashSet [|0<rsmState>|],
//                [|
//                    yield RSMEdges.TerminalEdge(0<rsmState>, terminalForCFGEdge, 0<rsmState>)
//                    yield RSMEdges.NonTerminalEdge(0<rsmState>, 1<rsmState>, 0<rsmState>)
//                    //for callSymbol in 1<terminalSymbol> .. 2<terminalSymbol> .. firstFreeCallTerminalId - 1<terminalSymbol> do
//                    for callSymbol in 2<terminalSymbol> .. 2<terminalSymbol> .. firstFreeCallTerminalId - 1<terminalSymbol> do
//                      yield RSMEdges.TerminalEdge(0<rsmState>, callSymbol, 0<rsmState>)
//                |]
//                )
//    let balancedBracketsBox =
//      let mutable firstFreeRsmState = 3<rsmState>
//      RSMBox(
//          1<rsmState>,
//          HashSet [|1<rsmState>; 2<rsmState>|],
//          [|
//              for callSymbol in 1<terminalSymbol> .. 2<terminalSymbol> .. firstFreeCallTerminalId - 1<terminalSymbol> do
//                  yield RSMEdges.TerminalEdge(1<rsmState>, callSymbol, firstFreeRsmState)
//                  yield RSMEdges.NonTerminalEdge(firstFreeRsmState, 0<rsmState>, firstFreeRsmState + 1<rsmState>)
//                  yield RSMEdges.TerminalEdge(firstFreeRsmState + 1<rsmState>, callSymbol + 1<terminalSymbol>, 2<rsmState>)
//                  yield RSMEdges.TerminalEdge(firstFreeRsmState, terminalForCFGEdge, firstFreeRsmState)
//                  yield RSMEdges.TerminalEdge(firstFreeRsmState + 1<rsmState>, terminalForCFGEdge, firstFreeRsmState + 1<rsmState>)
//                  firstFreeRsmState <- firstFreeRsmState + 2<rsmState>
//          |])
//          *)
//    RSM([|startBox; balancedBracketsBox|], startBox)
//
//let loadVSharpGraphFromCSV filePath =
//    let mutable maxTerminal = 0<terminalSymbol>
//    let edges = ResizeArray<_>()
//    System.IO.File.ReadLines filePath
//    |> Seq.map (fun s -> s.Split " ")
//    |> Seq.iter (fun a ->
//        let terminal = int a.[2] * 1<terminalSymbol>
//
//        if terminal > maxTerminal then maxTerminal <- terminal
//
//        edges.Add (Tests.InputGraph.TerminalEdge(a.[0] |> int |> LanguagePrimitives.Int32WithMeasure
//                                           , terminal
//                                           , a.[1] |> int |> LanguagePrimitives.Int32WithMeasure))
//
//        )
//    InputGraph <| edges.ToArray()
//    , maxTerminal
//
//let javaRsm (terminalSymbolsMapping:SortedDictionary<string,_>) =
//
//    let alloc = fst terminalSymbolsMapping.["alloc"]
//    let alloc_r = snd terminalSymbolsMapping.["alloc"]
//    let assign = fst terminalSymbolsMapping.["assign"]
//    let assign_r = snd terminalSymbolsMapping.["assign"]
//
//    let loadStorePairs =
//        [|
//          for kvp in terminalSymbolsMapping do
//            if kvp.Key.StartsWith "load_"
//            then
//                let store = "store_" + (kvp.Key.Split '_').[1]
//                if terminalSymbolsMapping.ContainsKey(store)
//                then yield LoadStorePairsInfo(kvp.Value, terminalSymbolsMapping.[store])
//        |]
//
//    let naiveRsm =
//        let mutable freeStateId = 7<rsmState>
//        let mapping = Dictionary()
//
//        let pointsTo = RSMBox()
//        let pointsToStartState = RsmState(true,false)
//        pointsTo.AddState pointsToStartState
//        mapping.Add(0<rsmState>, pointsToStartState :> IRsmState)
//
//        let flowsTo = RSMBox()
//        let flowsToStartState = RsmState(true,false)
//        flowsTo.AddState flowsToStartState
//        mapping.Add(2<rsmState>, flowsToStartState)
//
//        let alias = RSMBox()
//        let aliasStartState = RsmState(true,false)
//        alias.AddState aliasStartState
//        mapping.Add(4<rsmState>, aliasStartState)
//
//        let mapping =
//            GLLTests.fillRsmBox(
//               pointsTo,
//               mapping,
//               0<rsmState>,
//               HashSet([1<rsmState>]),
//               [|
//                  TerminalEdge(0<rsmState>, alloc, 1<rsmState>)
//                  TerminalEdge(0<rsmState>, assign, 0<rsmState>)
//                  for loadStorePair in loadStorePairs do
//                      yield! [|
//                          TerminalEdge(0<rsmState>, loadStorePair.LoadTerminalId, freeStateId)
//                          NonTerminalEdge(freeStateId, 4<rsmState>, freeStateId + 1<rsmState>)
//                          TerminalEdge(freeStateId + 1<rsmState>, loadStorePair.StoreTerminalId, 0<rsmState>)
//                      |]
//                      freeStateId <- freeStateId + 2<rsmState>
//               |]
//            )
//        let mapping =
//            GLLTests.fillRsmBox(
//               flowsTo,
//               mapping,
//               2<rsmState>,
//               HashSet([3<rsmState>]),
//               [|
//                  TerminalEdge(2<rsmState>, alloc_r, 3<rsmState>)
//                  TerminalEdge(3<rsmState>, assign_r, 3<rsmState>)
//                  for loadStorePair in loadStorePairs do
//                      yield! [|
//                          TerminalEdge(3<rsmState>, loadStorePair.StoreTerminalReversedId, freeStateId)
//                          NonTerminalEdge(freeStateId, 4<rsmState>, freeStateId + 1<rsmState>)
//                          TerminalEdge(freeStateId + 1<rsmState>, loadStorePair.LoadTerminalReversedId, 3<rsmState>)
//                      |]
//                      freeStateId <- freeStateId + 2<rsmState>
//               |]
//            )
//        let mapping =
//           GLLTests.fillRsmBox(
//                  alias,
//                  mapping,
//                  4<rsmState>,
//                  HashSet([6<rsmState>]),
//                  [|
//                      NonTerminalEdge(4<rsmState>, 0<rsmState>, 5<rsmState>)
//                      NonTerminalEdge(5<rsmState>, 2<rsmState>, 6<rsmState>)
//                  |]
//                  )
//
//        RSM([|alias; pointsTo; flowsTo|], pointsTo)
//
//    let optimizedRsm =
//        let mutable freeStateId = 4<rsmState>
//        let mapping = Dictionary()
//
//        let pointsTo = RSMBox()
//        let pointsToStartState = RsmState(true,false)
//        pointsTo.AddState pointsToStartState
//        mapping.Add(0<rsmState>, pointsToStartState :> IRsmState)
//
//        let flowsTo = RSMBox()
//        let flowsToStartState = RsmState(true,false)
//        flowsTo.AddState flowsToStartState
//        mapping.Add(2<rsmState>, flowsToStartState)
//
//        let mapping =
//            GLLTests.fillRsmBox(
//               pointsTo,
//               mapping,
//               0<rsmState>,
//               HashSet([1<rsmState>]),
//               [|
//                  TerminalEdge(0<rsmState>, alloc, 1<rsmState>)
//                  TerminalEdge(0<rsmState>, assign, 0<rsmState>)
//                  for loadStorePair in loadStorePairs do
//                      yield! [|
//                          TerminalEdge(0<rsmState>, loadStorePair.LoadTerminalId, freeStateId)
//                          NonTerminalEdge(freeStateId, 0<rsmState>, freeStateId + 1<rsmState>)
//                          NonTerminalEdge(freeStateId + 1<rsmState>, 2<rsmState>, freeStateId + 2<rsmState>)
//                          TerminalEdge(freeStateId + 2<rsmState>, loadStorePair.StoreTerminalId, 0<rsmState>)
//                      |]
//                      freeStateId <- freeStateId + 3<rsmState>
//               |]
//            )
//        let mapping =
//            GLLTests.fillRsmBox(
//               flowsTo,
//               mapping,
//               2<rsmState>,
//               HashSet([3<rsmState>]),
//               [|
//                  TerminalEdge(2<rsmState>, alloc_r, 3<rsmState>)
//                  TerminalEdge(3<rsmState>, assign_r, 3<rsmState>)
//                  for loadStorePair in loadStorePairs do
//                      yield! [|
//                          TerminalEdge(3<rsmState>, loadStorePair.StoreTerminalReversedId, freeStateId)
//                          NonTerminalEdge(freeStateId, 0<rsmState>, freeStateId + 1<rsmState>)
//                          NonTerminalEdge(freeStateId + 1<rsmState>, 2<rsmState>, freeStateId + 2<rsmState>)
//                          TerminalEdge(freeStateId + 2<rsmState>, loadStorePair.LoadTerminalReversedId, 3<rsmState>)
//                      |]
//                      freeStateId <- freeStateId + 3<rsmState>
//               |]
//            )
//
//        RSM([|pointsTo; flowsTo|], pointsTo)
//
//    optimizedRsm
//
//
//let runAllPairs (graph:InputGraph) q mode =
//    let start = System.DateTime.Now
//    let startVertices,_ = graph.ToCfpqCoreGraph (HashSet (graph.AllVertices()))
//    let res = defaultEval startVertices q mode
//    let reachable =
//        match res with
//        | QueryResult.MatchedRanges _ -> q.OriginalStartState.NonTerminalNodes.ToArray().Length
//        | QueryResult.ReachabilityFacts x -> x |> Seq.fold (fun x v -> x + v.Value.Count) 0
//    printfn $"Reachable: %A{reachable}."
//    printfn $"Total processing time: %A{(System.DateTime.Now - start).TotalMilliseconds} milliseconds"
//
//
//let singleSourceForAllContinuously (graph:InputGraph) q mode =
//    let mutable gss = GSS()
//    let mutable matchedRanges = MatchedRanges()
//    let vertices =
//        graph.AllVertices()
//    let startVertices, mapping = graph.ToCfpqCoreGraph (HashSet vertices)
//    for n in vertices do
//        printfn $"V: %i{n}"
//        //let startVertices = [|n|]
//        let startVertices = HashSet[|mapping.[n]|]
//        let reachableVertices =
//            let d = Dictionary<_,_>(startVertices.Count)
//            startVertices
//            |> Seq.iter (fun v -> d.Add(v, HashSet<_>()))
//            d
//        let res = defaultEvalFromState reachableVertices gss matchedRanges startVertices (HashSet())q mode
//        match res with
//        | QueryResult.MatchedRanges ranges -> matchedRanges <- ranges
//        | _ -> ()
//
//let singleSourceForAll (graph:InputGraph) q mode =
//    let startVertices,_ =  graph.ToCfpqCoreGraph (HashSet (graph.AllVertices()))
//    for n in startVertices do
//        let startVertices =  HashSet [|n|]
//        let res = defaultEval startVertices q mode |> ignore
//        printfn $"%A{res}"
//
//[<EntryPoint>]
//let main argv =
//    let parser = ArgumentParser.Create<_>(programName = "dotnet Benchmarks.dll")
//    let args = parser.Parse argv
//
//    let graph, query =
//        match args.GetResult Query with
//        | ArgQuery.G1 ->
//            let graph = loadGraphFromCSV (args.GetResult Graph) defaultMap
//            graph, g1
//        | ArgQuery.Java ->
//            let graph,mapping = loadJavaGraphFromCSV (args.GetResult Graph)
//            graph, javaRsm mapping
//        | ArgQuery.VSharp ->
//            let graph,maxTerminal = loadVSharpGraphFromCSV (args.GetResult Graph)
//            graph, vSharRSM maxTerminal
//        | x -> failwithf $"Unexpected query: %A{x}."
//
//    printfn "Data loaded."
//    printfn $"Vertices: %A{graph.NumberOfVertices()}"
//
//    let mode =
//        match args.GetResult Mode with
//        | ArgQueryMode.All_Paths -> Mode.AllPaths
//        | ArgQueryMode.Reachability_Only -> Mode.ReachabilityOnly
//        | x -> failwithf $"Unexpected query mode: %A{x}."
//
//    match args.GetResult TaskType with
//    | ArgTaskType.All_Pairs ->
//        runAllPairs graph query mode
//    | ArgTaskType.Single_Source ->
//        singleSourceForAll graph query mode
//    | ArgTaskType.Single_Source_Continuously ->
//        singleSourceForAllContinuously graph query mode
//    | x -> failwithf $"Unexpected task type: %A{x}."
//
//    0
