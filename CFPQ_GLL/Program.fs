// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open CFPQ_GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    let graph = InputGraph([|InputGraph.CFGEdge(0<graphVertex>,1<graphVertex>)|])
    let startV = [|0<graphVertex>|]
    let q = RSM(0<rsmState>, System.Collections.Generic.HashSet([1<rsmState>]),[|CFGEdge(0<rsmState>,1<rsmState>)|])
    let reachable = GLL.eval graph startV q
    printfn "Reachable: %A" reachable
    0 // return an integer exit code