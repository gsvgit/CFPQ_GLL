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
type GssVertex =
    val InputPosition: int<inputGraphVertex>
    val RSMState: int<rsmState>
    new (inputPosition, rsmState) =
        {InputPosition = inputPosition; RSMState = rsmState}

[<Struct>]
type Descriptor =
    val InputPosition: int<inputGraphVertex>
    val GSSVertex: GssVertex
    val RSMState: int<rsmState>
    val MatchedRange: MatchedRangeWithType
    new(inputPosition, gssVertex, rsmState, matchedRange) =
        {
            InputPosition = inputPosition
            GSSVertex = gssVertex
            RSMState = rsmState
            MatchedRange = matchedRange
        }

[<Struct>]
type DescriptorPart =
    val InputPosition: int<inputGraphVertex>
    val RSMState: int<rsmState>
    new (descriptor: Descriptor) =
        {
            InputPosition = descriptor.InputPosition
            RSMState = descriptor.RSMState
        }
        
[<Struct>]
type GSSEdge =
    val GSSVertex : GssVertex
    val RSMState : int<rsmState>
    val Info : MatchedRangeWithType
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
type GssVertexContent =
    val OutgoingEdges : ResizeArray<GSSEdge>
    val Popped : ResizeArray<MatchedRangeWithType>
    val HandledDescriptors : HashSet<DescriptorPart>
    new (outputEdges, popped, handledDescriptors) =
        {
            OutgoingEdges = outputEdges
            Popped = popped
            HandledDescriptors = handledDescriptors
        }

type GSS () =
    let vertices = Dictionary<GssVertex, GssVertexContent>()    
    member this.AddNewVertex (inputPosition: int<inputGraphVertex>, rsmState:int<rsmState>) =
        let gssVertex = GssVertex(inputPosition, rsmState)
        if vertices.ContainsKey gssVertex
        then gssVertex
        else
            vertices.Add(gssVertex, GssVertexContent(ResizeArray<_>(), ResizeArray<_>(), HashSet<_>()))
            gssVertex
   
    member this.AddEdge (currentGSSVertex: GssVertex
                         , rsmStateToReturn: int<rsmState>
                         , inputPositionToContinue: int<inputGraphVertex>
                         , rsmStateToContinue: int<rsmState>
                         , matchedRange: MatchedRangeWithType) =
        let newGSSVertex = this.AddNewVertex (inputPositionToContinue, rsmStateToContinue)
        let newGSSVertexContent = vertices.[newGSSVertex]
        let newEdge = GSSEdge(currentGSSVertex, rsmStateToReturn, matchedRange)
        
        // There is no need to check GSS edges duplication.
        // "Faster, Practical GLL Parsing", Ali Afroozeh and Anastasia Izmaylova
        // p.13: "There is at most one call to the create function with the same arguments.
        // Thus no check for duplicate GSS edges is needed."
        newGSSVertexContent.OutgoingEdges.Add newEdge
        newGSSVertex, newGSSVertexContent.Popped
        
    member this.Pop (currentDescriptor:Descriptor, matchedRange) =
        let gssVertexContent = vertices.[currentDescriptor.GSSVertex]                
        gssVertexContent.Popped.Add matchedRange         
        gssVertexContent.OutgoingEdges
        
    member this.IsThisDescriptorAlreadyHandled (descriptor:Descriptor) =
        vertices.[descriptor.GSSVertex].HandledDescriptors.Contains (DescriptorPart descriptor)
    
    member this.AddDescriptorToHandled (descriptor:Descriptor) =
        vertices.[descriptor.GSSVertex].HandledDescriptors.Add (DescriptorPart descriptor)
        |> ignore