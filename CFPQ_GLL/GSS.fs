module CFPQ_GLL.GSS
open System.Collections.Generic
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF
open FSharpx.Collections

[<Measure>] type gssVertex

[<Struct>]
type GssVertex<'inputGraphVertex> =
    val InputPosition: 'inputGraphVertex
    val RSMState: int<rsmState>
    new (inputPosition, rsmState) =
        {InputPosition = inputPosition; RSMState = rsmState}

[<Struct>]
type Descriptor<'inputGraphVertex> =
    val InputPosition: 'inputGraphVertex
    val GSSVertex: GssVertex<'inputGraphVertex>
    val RSMState: int<rsmState>
    val MatchedRange: MatchedRangeWithType<'inputGraphVertex>
    new(inputPosition, gssVertex, rsmState, matchedRange) =
        {
            InputPosition = inputPosition
            GSSVertex = gssVertex
            RSMState = rsmState
            MatchedRange = matchedRange
        }

[<Struct>]
type DescriptorPart<'inputGraphVertex> =
    val InputPosition: 'inputGraphVertex
    val RSMState: int<rsmState>
    new (descriptor: Descriptor<'inputGraphVertex>) =
        {
            InputPosition = descriptor.InputPosition
            RSMState = descriptor.RSMState
        }
        
[<Struct>]
type GSSEdge<'inputGraphVertex> =
    val GSSVertex : GssVertex<'inputGraphVertex>
    val RSMState : int<rsmState>
    val Info : MatchedRangeWithType<'inputGraphVertex>
    new(gssVertex, rsmState, info) =
        {
            GSSVertex = gssVertex
            RSMState = rsmState
            Info = info
        }

[<Struct>]
type GssVertexContent<'inputGraphVertex> =
    val OutgoingEdges : ResizeArray<GSSEdge<'inputGraphVertex>>
    val Popped : ResizeArray<MatchedRangeWithType<'inputGraphVertex>>
    val HandledDescriptors : HashSet<DescriptorPart<'inputGraphVertex>>
    new (outputEdges, popped, handledDescriptors) =
        {
            OutgoingEdges = outputEdges
            Popped = popped
            HandledDescriptors = handledDescriptors
        }

type GSS<'inputGraphVertex when 'inputGraphVertex : equality> () =
    let vertices = Dictionary<GssVertex<'inputGraphVertex>, GssVertexContent<'inputGraphVertex>>()    
    member this.AddNewVertex (inputPosition: 'inputGraphVertex, rsmState:int<rsmState>) =
        let gssVertex = GssVertex(inputPosition, rsmState)
        if vertices.ContainsKey gssVertex
        then gssVertex
        else
            vertices.Add(gssVertex, GssVertexContent(ResizeArray<_>(), ResizeArray<_>(), HashSet<_>()))
            gssVertex
   
    member this.AddEdge (currentGSSVertex: GssVertex<'inputGraphVertex>
                         , rsmStateToReturn: int<rsmState>
                         , inputPositionToContinue: 'inputGraphVertex
                         , rsmStateToContinue: int<rsmState>
                         , matchedRange: MatchedRangeWithType<'inputGraphVertex>) =
        let newGSSVertex = this.AddNewVertex (inputPositionToContinue, rsmStateToContinue)
        let newGSSVertexContent = vertices.[newGSSVertex]
        let newEdge = GSSEdge(currentGSSVertex, rsmStateToReturn, matchedRange)
        
        // There is no need to check GSS edges duplication.
        // "Faster, Practical GLL Parsing", Ali Afroozeh and Anastasia Izmaylova
        // p.13: "There is at most one call to the create function with the same arguments.
        // Thus no check for duplicate GSS edges is needed."
        newGSSVertexContent.OutgoingEdges.Add newEdge
        newGSSVertex, newGSSVertexContent.Popped
        
    member this.Pop (currentDescriptor:Descriptor<'inputGraphVertex>, matchedRange) =
        let gssVertexContent = vertices.[currentDescriptor.GSSVertex]                
        gssVertexContent.Popped.Add matchedRange         
        gssVertexContent.OutgoingEdges
        
    member this.IsThisDescriptorAlreadyHandled (descriptor:Descriptor<'inputGraphVertex>) =
        vertices.[descriptor.GSSVertex].HandledDescriptors.Contains (DescriptorPart descriptor)
    
    member this.AddDescriptorToHandled (descriptor:Descriptor<'inputGraphVertex>) =
        vertices.[descriptor.GSSVertex].HandledDescriptors.Add (DescriptorPart descriptor)
        |> ignore