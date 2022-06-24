module CFPQ_GLL.GSS
open System.Collections.Generic
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF
open FSharpx.Collections

[<Measure>] type gssVertex
[<Measure>] type gssEdge
[<Measure>] type descriptorWithoutGSSVertex
[<Measure>] type gssEdgeContent

[<Struct>]
type GssVertex<'inputVertex> =
    val InputPosition: 'inputVertex
    val RSMState: int<rsmState>
    new (inputPosition, rsmState) =
        {InputPosition = inputPosition; RSMState = rsmState}

[<Struct>]
type Descriptor<'inputVertex> =
    val InputPosition: 'inputVertex
    val GSSVertex: GssVertex<'inputVertex>
    val RSMState: int<rsmState>
    val MatchedRange: Option<MatchedRangeWithType<'inputVertex>>
    new(inputPosition, gssVertex, rsmState, matchedRange) =
        {
            InputPosition = inputPosition
            GSSVertex = gssVertex
            RSMState = rsmState
            MatchedRange = matchedRange
        }

[<Struct>]
type GSSEdge<'inputVertex> =
    val GSSVertex : GssVertex<'inputVertex>
    val RSMState : int<rsmState>
    val Info : Option<MatchedRangeWithType<'inputVertex>>
    new(gssVertex, rsmState, info) =
        {
            GSSVertex = gssVertex
            RSMState = rsmState
            Info = info
        }

// gssEdge = |gssVertex|rsmState|
// gssVertex = |...|InputPosition|rsmState|
let MASK_FOR_GSS_VERTEX = int64 ((System.UInt64.MaxValue >>> BITS_FOR_GRAPH_VERTICES) <<< BITS_FOR_RSM_STATE)
let MASK_FOR_RSM_STATE = int64 (System.UInt64.MaxValue >>> (2 * BITS_FOR_GRAPH_VERTICES))

let MAX_VALUE_FOR_GSS_VERTEX:int64<gssVertex> =
    System.UInt64.MaxValue >>> (64 - BITS_FOR_GRAPH_VERTICES - BITS_FOR_RSM_STATE)
    |> int64
    |> fun x -> x - 1L
    |> LanguagePrimitives.Int64WithMeasure
 
[<Struct>]
type GssVertexContent<'inputVertex> =
    val OutgoingEdges : ResizeArray<GSSEdge<'inputVertex>>
    val Popped : ResizeArray<MatchedRangeWithType<'inputVertex>>
    val HandledDescriptors : HashSet<Descriptor<'inputVertex>>
    new (outputEdges, popped, handledDescriptors) =
        {
            OutgoingEdges = outputEdges
            Popped = popped
            HandledDescriptors = handledDescriptors
        }

type GSS<'inputVertex when 'inputVertex: equality> () =
    let vertices = Dictionary<GssVertex<'inputVertex>,GssVertexContent<'inputVertex>>()    
    member this.AddNewVertex (inputPosition: 'inputVertex, rsmState:int<rsmState>) =
        let gssVertex = GssVertex(inputPosition, rsmState)
        if vertices.ContainsKey gssVertex
        then gssVertex
        else
            vertices.Add(gssVertex, GssVertexContent(ResizeArray<_>(), ResizeArray<_>(), HashSet<_>()))
            gssVertex
   
    member this.AddEdge (currentGSSVertex: GssVertex<'inputVertex>
                         , rsmStateToReturn: int<rsmState>
                         , inputPositionToContinue: 'inputVertex
                         , rsmStateToContinue: int<rsmState>
                         , matchedRange: Option<MatchedRangeWithType<'inputVertex>>) =
        let newGSSVertex = this.AddNewVertex (inputPositionToContinue, rsmStateToContinue)
        let newGSSVertexContent = vertices.[newGSSVertex]
        let newEdge = GSSEdge(currentGSSVertex, rsmStateToReturn, matchedRange)
        
        // There is no need to check GSS edges duplication.
        // "Faster, Practical GLL Parsing", Ali Afroozeh and Anastasia Izmaylova
        // p.13: "There is at most one call to the create function with the same arguments.
        // Thus no check for duplicate GSS edges is needed."
        newGSSVertexContent.OutgoingEdges.Add newEdge
        newGSSVertex, newGSSVertexContent.Popped
        
    member this.Pop (currentDescriptor:Descriptor<'inputVertex>, matchedRange) =
        let gssVertexContent = vertices.[currentDescriptor.GSSVertex]                
        gssVertexContent.Popped.Add matchedRange         
        gssVertexContent.OutgoingEdges
        
    member this.IsThisDescriptorAlreadyHandled (descriptor:Descriptor<'inputVertex>) =
        vertices.[descriptor.GSSVertex].HandledDescriptors.Contains descriptor
    
    member this.AddDescriptorToHandled (descriptor:Descriptor<'inputVertex>) =
        vertices.[descriptor.GSSVertex].HandledDescriptors.Add descriptor
        |> ignore