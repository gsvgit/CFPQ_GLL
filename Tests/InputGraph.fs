module Tests.InputGraph

open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.Common
open CFPQ_GLL.InputGraph

[<Measure>] type inputGraphTerminalEdge

[<Struct>]
type InputGraphEdge =
    val TerminalSymbol: Char
    val TargetVertex: int<inputGraphVertex>
    val Weight: int<weight>
    new (terminal, targetVertex, weight) = {TerminalSymbol = terminal; TargetVertex = targetVertex; Weight = weight}

type DemoInputGraphEdge =
    | TerminalEdge of int<inputGraphVertex>*Char*int<inputGraphVertex>*int<weight>
    | EpsilonEdge of int<inputGraphVertex>*int<inputGraphVertex>*int<weight>

let DefaultTerminalEdge(_from, terminal, _to) = TerminalEdge(_from, terminal, _to, 0<weight>)
let ErrorTerminalEdge(_from, terminal, _to) = TerminalEdge(_from, terminal, _to, 1<weight>)
let DefaultEpsilonEdge(_from, _to) = EpsilonEdge(_from, _to, 0<weight>)
let ErrorEpsilonEdge(_from, _to) = EpsilonEdge(_from, _to, 1<weight>)

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
        edges
        |> Array.iter (function
                        | TerminalEdge (_from, smb, _to, weight) ->
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            InputGraphEdge(smb, _to, weight)  |> vertexContent.OutgoingTerminalEdges.Add
                        | EpsilonEdge(_from, _to, weight) ->
                            let vertexContent = addVertex _from
                            addVertex _to |> ignore
                            InputGraphEdge(Epsilon, _to, weight) |> vertexContent.OutgoingTerminalEdges.Add
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
                    yield $"%i{kvp.Key} -> %i{edge.TargetVertex} [label={string edge.TerminalSymbol}]"

            yield "}"
        }
        |> fun data -> System.IO.File.WriteAllLines(file, data)

    member this.RemoveOutgoingEdges v = vertices.[v].OutgoingTerminalEdges.Clear()
    member this.AddEdges edges =
        addEdges edges

    member this.AddVertex v = addVertex v |> ignore

    member this.ToCfpqCoreGraph (startVertex: int<inputGraphVertex>) =
        this.ToDot (0,"coreGraph.dot")
        let mutable firstFreeVertexId = 0
        let verticesMapping = Dictionary<int<inputGraphVertex>, LinearInputGraphVertexBase>()
        let getVertex vertexId =
            let exists, vertex = verticesMapping.TryGetValue vertexId
            let res =
                if exists
                then vertex
                else
                    let vertex = LinearInputGraphVertexBase(int vertexId)
                    firstFreeVertexId <- firstFreeVertexId + 1
                    verticesMapping.Add(vertexId, vertex)
                    vertex
            res
        let newStartVertex = getVertex startVertex
        for kvp in vertices do
            let vertex = getVertex kvp.Key
            for edge in kvp.Value.OutgoingTerminalEdges do
                let targetVertex = getVertex edge.TargetVertex
                (vertex :?> LinearInputGraphVertexBase).AddOutgoingEdge (edge.TerminalSymbol, TerminalEdgeTarget(targetVertex, edge.Weight))

        newStartVertex,verticesMapping
