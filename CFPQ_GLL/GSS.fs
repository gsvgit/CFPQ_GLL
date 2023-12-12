module CFPQ_GLL.GSS
open System
open System.Collections.Generic
open CFPQ_GLL.Common

type GSSEdge<'token when 'token: equality> (gssVertex: IGssVertex<'token>, rsmState: IRsmState<'token>, matchedRange: MatchedRangeWithNode<'token>) =
    interface IGssEdge<'token> with
        member this.GssVertex = gssVertex
        member this.RsmState = rsmState
        member this.MatchedRange = matchedRange

and GssVertex<'token when 'token: equality> (inputPosition: IInputGraphVertex<'token>, rsmState: IRsmState<'token>, minWeightOfLeftPart:int<weight>) =
    let mutable minimalWeightOfLeftPart = minWeightOfLeftPart
    let outgoingEdges = ResizeArray<IGssEdge<'token>>()
    let popped = ResizeArray<MatchedRangeWithNode<'token>>()
    let handledDescriptors = HashSet<Descriptor<'token>>()
    interface IGssVertex<'token> with
        member this.MinimalWeightOfLeftPart
            with get () = minimalWeightOfLeftPart
            and set v = minimalWeightOfLeftPart <- v
        member this.InputPosition = inputPosition
        member this.RsmState = rsmState
        member this.OutgoingEdges = outgoingEdges
        member this.Popped = popped
        member this.HandledDescriptors = handledDescriptors

[<Struct>]
type GssVertexId<'token when 'token: equality> =
    val InputPosition: IInputGraphVertex<'token>
    val RsmState: IRsmState<'token>
    new(inputPosition, rsmState) = {InputPosition = inputPosition; RsmState = rsmState}

type GSS<'token when 'token: equality> () =
    let vertices = Dictionary<GssVertexId<'token>, GssVertex<'token>>()
    member this.AddNewVertex (inputPosition: IInputGraphVertex<'token>, rsmState:IRsmState<'token>, weight:int<weight>) =
        let gssVertexId = GssVertexId(inputPosition, rsmState)
        let exists, gssVertex = vertices.TryGetValue gssVertexId
        if exists
        then
            let _gssVertex = (gssVertex :> IGssVertex<'token>) 
            if _gssVertex.MinimalWeightOfLeftPart > weight
            then _gssVertex.MinimalWeightOfLeftPart <- weight
            gssVertex
        else
            let gssVertex = GssVertex(inputPosition, rsmState, weight)
            vertices.Add(gssVertexId, gssVertex)
            gssVertex

    member this.AddEdge (currentGSSVertex: IGssVertex<'token>
                         , rsmStateToReturn: IRsmState<'token>
                         , inputPositionToContinue: IInputGraphVertex<'token>
                         , rsmStateToContinue: IRsmState<'token>
                         , matchedRange: MatchedRangeWithNode<'token>) =
        let weight =
            match matchedRange.Node with
            | Some n -> n.Weight
            | None -> 0<weight>
        let newGSSVertex = this.AddNewVertex (inputPositionToContinue, rsmStateToContinue, currentGSSVertex.MinimalWeightOfLeftPart + weight) :> IGssVertex<'token>
        let newEdge = GSSEdge(currentGSSVertex, rsmStateToReturn, matchedRange)
        matchedRange.Node
        |> Option.iter (fun n ->
            let isAlive, n = n.TryGetTarget()
            if isAlive && n.IsAlive
            then n.GssEdges.Add((newGSSVertex,newEdge))
            else failwith "An attempt to create GSS edge with invalid matched range." )         

        // There is no need to check GSS edges duplication.
        // "Faster, Practical GLL Parsing", Ali Afroozeh and Anastasia Izmaylova
        // p.13: "There is at most one call to the create function with the same arguments.
        // Thus no check for duplicate GSS edges is needed."
        newGSSVertex.OutgoingEdges.Add newEdge
        let count = newGSSVertex.Popped.RemoveAll (fun r -> r.IsAlive())
        newGSSVertex, newGSSVertex.Popped

    member this.Pop (currentDescriptor:Descriptor<'token>, matchedRange) =
        currentDescriptor.GssVertex.Popped.Add matchedRange
        currentDescriptor.GssVertex.OutgoingEdges

    member this.IsThisDescriptorAlreadyHandled (descriptor:Descriptor<'token>) =
        let count = descriptor.GssVertex.HandledDescriptors.RemoveWhere(fun d -> not d.IsAlive)
        let exists, handledDescriptor = descriptor.GssVertex.HandledDescriptors.TryGetValue descriptor
        let result = exists && handledDescriptor.Weight <= descriptor.Weight
        result

    member this.AddDescriptorToHandled (descriptor:Descriptor<'token>) =
        let weakRefToDescriptor = WeakReference<_> descriptor
        let added = descriptor.InputPosition.Descriptors.Add weakRefToDescriptor
        assert added
        descriptor.RsmState.Descriptors.Add weakRefToDescriptor
        descriptor.GssVertex.HandledDescriptors.Add descriptor
        |> ignore

    member this.ToDot (file:string) =
        let dotEdges = ResizeArray<_>()
        let dotVertices = Dictionary<_,_>()
        let mutable firstFreeDotVertexId = 0
        for kvp in vertices do
            //dotVertices.Add $"{firstFreeDotVertexId} [label={kvp.Key.InputPosition}]"
            dotVertices.Add(kvp.Value, firstFreeDotVertexId)
            firstFreeDotVertexId <- firstFreeDotVertexId + 1
        for kvp in vertices do
            for e in (kvp.Value :> IGssVertex<'token>).OutgoingEdges do
                dotEdges.Add $"{dotVertices[kvp.Value]} -> {dotVertices[e.GssVertex :?> GssVertex<'token>]}"
                
        [
            "digraph g{"
            yield! dotEdges
            "}"
        ]
        |> fun lines -> System.IO.File.WriteAllLines(file, lines)
            
    