module CFPQ_GLL.GSS
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open FSharpx.Collections

[<Measure>] type gssVertex
[<Measure>] type gssEdge

let MASK_FOR_INPUT_POSITION = int64 (System.UInt64.MaxValue >>> BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE <<< BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE)
let MASK_FOR_RSM_STATE = int64 (System.UInt64.MaxValue >>> 2 * BITS_FOR_GRAPH_VERTICES)
let packGSSEdge (targetGssVertex:int<gssVertex>) (rsmState:int<rsmState>) : int64<gssEdge> =
    let _targetGssVertex = (int64 targetGssVertex) <<< (BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE)
    let _rsmState = int64 rsmState
    (_targetGssVertex ||| _rsmState) |> LanguagePrimitives.Int64WithMeasure

let unpackGSSEdge (gssEdge:int64<gssEdge>) : int<gssVertex> * int<rsmState> =
    let gssEdge = int64 gssEdge
    let gssVertex = int32 (gssEdge &&& MASK_FOR_INPUT_POSITION >>> BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure    
    let rsmState = int32 (gssEdge &&& MASK_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure
    gssVertex, rsmState
    
[<Struct>]
type GssVertexContent =
    val OutputEdges : ResizeArray<int64<gssEdge>>
    val Popped : ResizeArray<int<graphVertex>>
    new (outputEdges, popped) = {OutputEdges = outputEdges; Popped = popped}

type GSS() =
    let vertices = System.Collections.Generic.Dictionary<int<gssVertex>,GssVertexContent>()
    member this.AddNewVertex (inputPosition:int<graphVertex>) =
        let vId:int<gssVertex> = int32 inputPosition |> LanguagePrimitives.Int32WithMeasure
        if vertices.ContainsKey vId
        then vId
        else
            vertices.Add(vId, GssVertexContent(ResizeArray<_>(),ResizeArray<_>()))
            vId
    member this.GetRespectiveInputPosition (gssVertex:int<gssVertex>) : int<graphVertex> =
        int gssVertex |> LanguagePrimitives.Int32WithMeasure
    member this.AddEdge (currentGSSVertex:int<gssVertex>, rsmStateToReturn:int<rsmState>, inputPositionToContinue:int<graphVertex>) =
        let newGSSVertex = this.AddNewVertex inputPositionToContinue
        let newGSSVertexContent = vertices.[newGSSVertex]
        let newEdge = packGSSEdge currentGSSVertex rsmStateToReturn
        
        if not <| newGSSVertexContent.OutputEdges.Contains newEdge
        then newGSSVertexContent.OutputEdges.Add newEdge
        
        let pops =
            newGSSVertexContent.Popped
            |> ResizeArray.map (fun poppedInputPositions -> poppedInputPositions)
        
        newGSSVertex, pops 
    member this.Pop (currentGSSVertex:int<gssVertex>) (currentInputPosition:int<graphVertex>) =
        let gssVertexContent = vertices.[currentGSSVertex]
        gssVertexContent.OutputEdges
        |> ResizeArray.map (fun gssEdge ->
            let targetVertex, rsmState =  unpackGSSEdge gssEdge
            gssVertexContent.Popped.Add currentInputPosition
            targetVertex, rsmState
            )
    