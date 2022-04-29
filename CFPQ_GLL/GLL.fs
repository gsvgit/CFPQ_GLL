module CFPQ_GLL.GLL
open CFPQ_GLL.RSM
open CFPQ_GLL.GSS
open CFPQ_GLL.InputGraph
open FSharpx.Collections

[<Measure>] type descriptor

let MASK_FOR_INPUT_POSITION = int64 (System.UInt64.MaxValue >>> BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE <<< BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE)
let MASK_FOR_GSS_VERTEX = int64 (System.UInt64.MaxValue >>> BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE <<< BITS_FOR_RSM_STATE)
let MASK_FOR_RSM_STATE = int64 (System.UInt64.MaxValue >>> 2 * BITS_FOR_GRAPH_VERTICES)

let packDescriptor (inputPos:int<graphVertex>) (gssVertex:int<gssVertex>) (rsmState:int<rsmState>) : int64<descriptor>=
    let _inputPos = (int64 inputPos) <<< (BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE)
    let _gssVertex = (int64 gssVertex) <<< BITS_FOR_RSM_STATE
    let _rsmState = int64 rsmState
    (_inputPos ||| _gssVertex ||| _rsmState) |> LanguagePrimitives.Int64WithMeasure 

let unpackDescriptor (descriptor:int64<descriptor>) : int<graphVertex> * int<gssVertex> * int<rsmState> =
    let descriptor = int64 descriptor
    let inputPos = int32 (descriptor &&& MASK_FOR_INPUT_POSITION >>> BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure
    let gssVertex = int32 (descriptor &&& MASK_FOR_GSS_VERTEX >>> BITS_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure
    let rsmState = int32 (descriptor &&& MASK_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure
    inputPos, gssVertex, rsmState
    
let eval (graph:InputGraph) startVertices (query:RSM) =
    let handledDescriptors = System.Collections.Generic.HashSet<_>()
    let reachableVertices = ResizeArray<_>()
    let descriptorToProcess = System.Collections.Generic.Stack<_>()
    
    let addDescriptor descriptor =
        if not <| handledDescriptors.Contains descriptor
        then descriptorToProcess.Push descriptor 
    
    let gss = GSS()
    startVertices
    |> Array.iter (fun v ->
        let gssVertex = gss.AddNewVertex v
        packDescriptor v gssVertex query.StartState
        |> descriptorToProcess.Push
        )
    
    let handleDescriptor descriptor =
        
        handledDescriptors.Add descriptor |> ignore
        
        let inputPos, gssVertex, rsmState = unpackDescriptor descriptor
        
        if query.IsFinalState rsmState                        
        then
            let startPosition = gss.GetRespectiveInputPosition gssVertex
            if Array.contains startPosition startVertices
            then reachableVertices.Add (startPosition,inputPos)
            
            gss.Pop gssVertex inputPos
            |> ResizeArray.iter (fun (newGSSVertex, newRSMState) -> packDescriptor inputPos newGSSVertex newRSMState |> addDescriptor)
            
        let outgoingCallEdgesInGraph = graph.OutgoingCallEdges inputPos
        let outgoingReturnEdgesInGraph = graph.OutgoingReturnEdges inputPos
        let outgoingCFGEdgesInGraph = graph.OutgoingCFGEdges inputPos
            
        let outgoingNonTerminalEdgeInRSM = query.OutgoingNonTerminalEdge rsmState
        let outgoingCallEdgesInRSM = query.OutgoingCallEdges rsmState
        let outgoingReturnEdgesInRSM = query.OutgoingReturnEdges rsmState
        let outgoingCFGEdgesInRSM = query.OutgoingCFGEdges rsmState
        
        match outgoingNonTerminalEdgeInRSM with
        | Some nextRSMState ->
               let newGSSVertex, positionsForPops = gss.AddEdge(gssVertex, nextRSMState, inputPos)
               packDescriptor inputPos newGSSVertex query.StartState
               |> addDescriptor
               positionsForPops
               |> ResizeArray.iter (fun pos -> packDescriptor pos gssVertex nextRSMState |> addDescriptor)
        | None -> ()
        
        outgoingCallEdgesInRSM
        |> Array.iter (fun e1 ->
            outgoingCallEdgesInGraph
            |> Array.iter (fun e2 ->
                let nextPosition, inputSymbol = unpackInputGraphCallEdge e2
                let nextState, symbol = unpackRSMCallEdge e1
                if inputSymbol = symbol
                then packDescriptor nextPosition gssVertex nextState |> addDescriptor))
            
        outgoingReturnEdgesInRSM
        |> Array.iter (fun e1 ->
            outgoingReturnEdgesInGraph
            |> Array.iter (fun e2 ->
                let nextPosition, inputSymbol = unpackInputGraphReturnEdge e2
                let nextState, symbol = unpackRSMReturnEdge e1
                if inputSymbol = symbol
                then packDescriptor nextPosition gssVertex nextState |> addDescriptor))
        
        outgoingCFGEdgesInRSM
        |> Array.iter (fun e1 ->
            outgoingCFGEdgesInGraph
            |> Array.iter (fun e2 ->
                let nextPosition = unpackInputGraphCFGEdge e2
                let nextState = unpackRSMCFGEdge e1
                packDescriptor nextPosition gssVertex nextState |> addDescriptor))
    
    let startTime = System.DateTime.Now    
    
    while descriptorToProcess.Count > 0 do
        descriptorToProcess.Pop()
        |> handleDescriptor
    
    printfn $"Average throughput: %A{float handledDescriptors.Count / (System.DateTime.Now - startTime).TotalSeconds} descriptors per second."
        
    reachableVertices
