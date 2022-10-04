module Tests.InputGraph

open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.Common
open CFPQ_GLL.InputGraph

[<Measure>] type inputGraphTerminalEdge

[<Struct>]
type InputGraphEdge =
    val TerminalSymbol: int<terminalSymbol>
    val TargetVertex: int<inputGraphVertex>
    val Weight: int<edgeWeight>
    new (terminal, targetVertex, weight) = {TerminalSymbol = terminal; TargetVertex = targetVertex; Weight = weight}

type DemoInputGraphEdge =
    | TerminalEdge of int<inputGraphVertex>*int<terminalSymbol>*int<inputGraphVertex>
    | EpsilonEdge of int<inputGraphVertex>*int<inputGraphVertex>
    | ErrorEpsilonEdge of int<inputGraphVertex>*int<inputGraphVertex> // delete
    | ErrorTerminalEdge of int<inputGraphVertex>*int<terminalSymbol>*int<inputGraphVertex> // delete

[<Struct>]
type InputGraphTerminalEdge = // Usages don't found
    val Vertex : int<inputGraphVertex>
    val TerminalSymbol : int<terminalSymbol>
    new (vertex, terminalSymbol) = {Vertex = vertex; TerminalSymbol = terminalSymbol}

[<Struct>]
type InputGraphVertexMutableContent =
    val OutgoingTerminalEdges : ResizeArray<InputGraphEdge>
    new (terminalEdges) = {OutgoingTerminalEdges = terminalEdges}

type InputGraph (edges, enableErrorRecovering) =
    let vertices = System.Collections.Generic.Dictionary<int<inputGraphVertex>, InputGraphVertexMutableContent>()

    let addVertex v =
        if not <| vertices.ContainsKey v
        then vertices.Add(v,InputGraphVertexMutableContent(ResizeArray<_>()))
        vertices.[v]

    let addEdges edges =
        let errorEdgeWeight = 1<edgeWeight>
        let defaultEdgeWeight = if enableErrorRecovering then 0<edgeWeight> else 1<edgeWeight>
        edges
        |> Array.iter (function
                        | TerminalEdge (_from, smb, _to) ->
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            InputGraphEdge(smb, _to, defaultEdgeWeight)  |> vertexContent.OutgoingTerminalEdges.Add
                        | EpsilonEdge(_from, _to) ->
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            InputGraphEdge(Epsilon, _to, defaultEdgeWeight) |> vertexContent.OutgoingTerminalEdges.Add
                        | ErrorTerminalEdge(_from, smb, _to) ->
                            if not enableErrorRecovering then failwith "Unexpected error edge without error recovering"
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            InputGraphEdge(smb, _to, errorEdgeWeight)  |> vertexContent.OutgoingTerminalEdges.Add
                        | ErrorEpsilonEdge(_from, _to) ->
                            if not enableErrorRecovering then failwith "Unexpected error edge without error recovering"
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            InputGraphEdge(Epsilon, _to, errorEdgeWeight) |> vertexContent.OutgoingTerminalEdges.Add
                       )

    do addEdges edges

    new (enableErrorRecovering) = InputGraph([||], enableErrorRecovering)


    member this.OutgoingTerminalEdges v =
        vertices.[v].OutgoingTerminalEdges
    member this.NumberOfVertices() = vertices.Count
    member this.AllVertices() = vertices.Keys |> Array.ofSeq
    member this.ToDot(drawSink, file) =
        seq{
            yield "digraph InputGraph{"
            yield "node [shape = plaintext]"

            for kvp in vertices do
                for edge in kvp.Value.OutgoingTerminalEdges do
                    yield $"%i{kvp.Key} -> %i{edge.TargetVertex} [label=%i{edge.TerminalSymbol}]"

            yield "}"
        }
        |> fun data -> System.IO.File.WriteAllLines(file, data)

    member this.RemoveOutgoingEdges v = vertices.[v].OutgoingTerminalEdges.Clear()
    member this.AddEdges edges =
        addEdges edges

    member this.AddVertex v = addVertex v |> ignore

    member this.ToCfpqCoreGraph (startVertices: HashSet<int<inputGraphVertex>>) =
        let mutable firstFreeVertexId = 0
        let verticesMapping = Dictionary<int<inputGraphVertex>, IInputGraphVertex>()
        let newStartVertices = HashSet<IInputGraphVertex>()
        let getVertex vertexId =
            let exists, vertex = verticesMapping.TryGetValue vertexId
            let res =
                if exists
                then vertex
                else
                    let vertex = InputGraphVertexBase()
                    firstFreeVertexId <- firstFreeVertexId + 1
                    verticesMapping.Add(vertexId, vertex)
                    vertex
            if startVertices.Contains vertexId
            then newStartVertices.Add res |> ignore
            res
        for kvp in vertices do
            let vertex = (getVertex kvp.Key)
            for edge in kvp.Value.OutgoingTerminalEdges do
                let targetVertex = getVertex edge.TargetVertex
                let exists, edges = vertex.OutgoingEdges.TryGetValue edge.TerminalSymbol
                if exists
                then
                    let added = TerminalEdgeTarget(targetVertex, edge.Weight) |> edges.Add
                    assert added
                else
                    vertex.OutgoingEdges.Add(edge.TerminalSymbol, HashSet<_>[|TerminalEdgeTarget(targetVertex, edge.Weight)|])

        newStartVertices,verticesMapping
