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
    new (terminal, targetVertex) = {TerminalSymbol = terminal; TargetVertex = targetVertex}

type DemoInputGraphEdge =
    | TerminalEdge of int<inputGraphVertex>*int<terminalSymbol>*int<inputGraphVertex>
    | EpsilonEdge of int<inputGraphVertex>*int<inputGraphVertex>

[<Struct>]
type InputGraphTerminalEdge =
    val Vertex : int<inputGraphVertex>
    val TerminalSymbol : int<terminalSymbol>
    new (vertex, terminalSymbol) = {Vertex = vertex; TerminalSymbol = terminalSymbol}

[<Struct>]
type InputGraphVertexMutableContent =
    val OutgoingTerminalEdges : ResizeArray<InputGraphEdge>
    new (terminalEdges) = {OutgoingTerminalEdges = terminalEdges}

type InputGraph (edges) =
    let vertices = System.Collections.Generic.Dictionary<int<inputGraphVertex>, InputGraphVertexMutableContent>()

    let addVertex v =
        if not <| vertices.ContainsKey v
        then vertices.Add(v,InputGraphVertexMutableContent(ResizeArray<_>()))
        vertices.[v]

    let addEdges edges =
        edges
        |> Array.iter (function
                        | TerminalEdge (_from, smb, _to) ->
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            InputGraphEdge(smb, _to)  |> vertexContent.OutgoingTerminalEdges.Add
                        | EpsilonEdge(_from, _to) ->
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            InputGraphEdge(Epsilon, _to) |> vertexContent.OutgoingTerminalEdges.Add
                       )

    do addEdges edges

    new () = InputGraph([||])


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
                    let added = edges.Add targetVertex
                    assert added
                else
                    vertex.OutgoingEdges.Add(edge.TerminalSymbol, HashSet<_>[|targetVertex|])

        newStartVertices,verticesMapping
