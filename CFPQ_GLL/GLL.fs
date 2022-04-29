module CFPQ_GLL.GLL
open CFPQ_GLL.RSM
open CFPQ_GLL.GSS
open CFPQ_GLL.InputGraph
open FSharpx.Collections

[<Measure>] type descriptor

[<Struct>]
type Descriptor =
    val InputPosition: int<graphVertex>
    val GssVertex: int<gssVertex>
    val RsmState: int<rsmState>
    new(inputPosition, gssVertex, rsmState) = {InputPosition = inputPosition; GssVertex = gssVertex; RsmState = rsmState}
        
let MASK_FOR_INPUT_POSITION = int64 (System.UInt64.MaxValue >>> BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE <<< BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE)
let MASK_FOR_GSS_VERTEX = int64 (System.UInt64.MaxValue >>> BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE <<< BITS_FOR_RSM_STATE)
let MASK_FOR_RSM_STATE = int64 (System.UInt64.MaxValue >>> 2 * BITS_FOR_GRAPH_VERTICES)

let packDescriptor (inputPos:int<graphVertex>) (gssVertex:int<gssVertex>) (rsmState:int<rsmState>) : int64<descriptor>=
    let _inputPos = (int64 inputPos) <<< (BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE)
    let _gssVertex = (int64 gssVertex) <<< BITS_FOR_RSM_STATE
    let _rsmState = int64 rsmState
    (_inputPos ||| _gssVertex ||| _rsmState) |> LanguagePrimitives.Int64WithMeasure 

let unpackDescriptor (descriptor:int64<descriptor>) =
    let descriptor = int64 descriptor
    let inputPos = int32 (descriptor &&& MASK_FOR_INPUT_POSITION >>> BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure
    let gssVertex = int32 (descriptor &&& MASK_FOR_GSS_VERTEX >>> BITS_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure
    let rsmState = int32 (descriptor &&& MASK_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure
    Descriptor(inputPos, gssVertex, rsmState)
    
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
        
        let currentDescriptor = unpackDescriptor descriptor
        
        if query.IsFinalState currentDescriptor.RsmState                        
        then
            let startPosition = gss.GetRespectiveInputPosition currentDescriptor.GssVertex
            if Array.contains startPosition startVertices
            then reachableVertices.Add (startPosition, currentDescriptor.InputPosition)
            
            gss.Pop currentDescriptor.GssVertex currentDescriptor.InputPosition
            |> ResizeArray.iter (fun gssEdge -> packDescriptor currentDescriptor.InputPosition gssEdge.GssVertex gssEdge.RSMState |> addDescriptor)
            
        let outgoingCallEdgesInGraph = graph.OutgoingCallEdges currentDescriptor.InputPosition
        let outgoingReturnEdgesInGraph = graph.OutgoingReturnEdges currentDescriptor.InputPosition
        let outgoingCFGEdgesInGraph = graph.OutgoingCFGEdges currentDescriptor.InputPosition
            
        let outgoingNonTerminalEdgeInRSM = query.OutgoingNonTerminalEdge currentDescriptor.RsmState
        let outgoingCallEdgesInRSM = query.OutgoingCallEdges currentDescriptor.RsmState
        let outgoingReturnEdgesInRSM = query.OutgoingReturnEdges currentDescriptor.RsmState
        let outgoingCFGEdgesInRSM = query.OutgoingCFGEdges currentDescriptor.RsmState
        
        match outgoingNonTerminalEdgeInRSM with
        | Some nextRSMState ->
               let newGSSVertex, positionsForPops = gss.AddEdge(currentDescriptor.GssVertex, nextRSMState, currentDescriptor.InputPosition)
               packDescriptor currentDescriptor.InputPosition newGSSVertex query.StartState
               |> addDescriptor
               positionsForPops
               |> ResizeArray.iter (fun pos -> packDescriptor pos currentDescriptor.GssVertex nextRSMState |> addDescriptor)
        | None -> ()
        
        outgoingCallEdgesInRSM
        |> Array.iter (fun e1 ->
            outgoingCallEdgesInGraph
            |> Array.iter (fun e2 ->
                let graphEdge = unpackInputGraphCallEdge e2
                let rsmEdge = unpackRSMCallEdge e1
                if graphEdge.CallSymbol = rsmEdge.CallSymbol
                then packDescriptor graphEdge.Vertex currentDescriptor.GssVertex rsmEdge.State |> addDescriptor))
            
        outgoingReturnEdgesInRSM
        |> Array.iter (fun e1 ->
            outgoingReturnEdgesInGraph
            |> Array.iter (fun e2 ->
                let graphEdge = unpackInputGraphReturnEdge e2
                let rsmEdge = unpackRSMReturnEdge e1
                if graphEdge.ReturnSymbol = rsmEdge.ReturnSymbol
                then packDescriptor graphEdge.Vertex currentDescriptor.GssVertex rsmEdge.State |> addDescriptor))
        
        outgoingCFGEdgesInRSM
        |> Array.iter (fun e1 ->
            outgoingCFGEdgesInGraph
            |> Array.iter (fun e2 ->
                let nextPosition = unpackInputGraphCFGEdge e2
                let nextState = unpackRSMCFGEdge e1
                packDescriptor nextPosition currentDescriptor.GssVertex nextState |> addDescriptor))
    
    let startTime = System.DateTime.Now    
    
    while descriptorToProcess.Count > 0 do
        descriptorToProcess.Pop()
        |> handleDescriptor
    
    printfn $"Query processing total time: %A{(System.DateTime.Now - startTime).TotalMilliseconds} milliseconds"
    printfn $"Total descriptors handled: %A{handledDescriptors.Count}"
    printfn $"Average throughput: %A{float handledDescriptors.Count / (System.DateTime.Now - startTime).TotalSeconds} descriptors per second."
        
    reachableVertices
