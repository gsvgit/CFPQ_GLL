// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open CFPQ_GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

let example1 () =
    let graph = InputGraph([|InputGraph.CFGEdge(0<graphVertex>,1<graphVertex>)|])
    let startV = [|0<graphVertex>|]
    let q = RSM(0<rsmState>, System.Collections.Generic.HashSet([1<rsmState>]),[|CFGEdge(0<rsmState>,1<rsmState>)|])
    let reachable = GLL.eval graph startV q
    printfn "Reachable: %A" reachable
    
let example2 () =
    let graph = InputGraph([|InputGraph.CFGEdge(0<graphVertex>,1<graphVertex>)|])
    let startV = [|0<graphVertex>|]
    let q = RSM(0<rsmState>, System.Collections.Generic.HashSet([0<rsmState>]),[|CFGEdge(0<rsmState>,0<rsmState>)|])
    let reachable = GLL.eval graph startV q
    printfn "Reachable: %A" reachable

let example3 () =
    let graph = InputGraph([|InputGraph.CFGEdge(0<graphVertex>,0<graphVertex>)|])
    let startV = [|0<graphVertex>|]
    let q = RSM(0<rsmState>, System.Collections.Generic.HashSet([0<rsmState>]),[|CFGEdge(0<rsmState>,0<rsmState>)|])
    let reachable = GLL.eval graph startV q
    printfn "Reachable: %A" reachable
    
let example4 () =
    let graph = InputGraph([|InputGraph.CallEdge(0<graphVertex>,0<callSymbol>,1<graphVertex>)
                             InputGraph.ReturnEdge(1<graphVertex>,0<returnSymbol>,2<graphVertex>) |])
    let startV = [|0<graphVertex>|]
    let q = RSM(0<rsmState>, System.Collections.Generic.HashSet([0<rsmState>]),
                [|CFGEdge(0<rsmState>,0<rsmState>)
                  CallEdge(0<rsmState>,0<callSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,2<rsmState>)
                  ReturnEdge(2<rsmState>,0<returnSymbol>,0<rsmState>)|])
    let reachable = GLL.eval graph startV q
    printfn "Reachable: %A" reachable    

let example5 startV =
    let graph = InputGraph([|InputGraph.CallEdge(0<graphVertex>,0<callSymbol>,1<graphVertex>)
                             InputGraph.CallEdge(1<graphVertex>,0<callSymbol>,2<graphVertex>)
                             InputGraph.CallEdge(2<graphVertex>,0<callSymbol>,0<graphVertex>)
                             InputGraph.ReturnEdge(0<graphVertex>,0<returnSymbol>,3<graphVertex>)
                             InputGraph.ReturnEdge(3<graphVertex>,0<returnSymbol>,0<graphVertex>) |])
    let q = RSM(0<rsmState>, System.Collections.Generic.HashSet([0<rsmState>]),
                [|CFGEdge(0<rsmState>,0<rsmState>)
                  CallEdge(0<rsmState>,0<callSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,2<rsmState>)
                  ReturnEdge(2<rsmState>,0<returnSymbol>,0<rsmState>)|])
    let reachable = GLL.eval graph startV q
    printfn "Reachable: %A" reachable    

let example6 () =
    let graph = InputGraph([|InputGraph.CallEdge(0<graphVertex>,0<callSymbol>,1<graphVertex>)
                             InputGraph.CallEdge(1<graphVertex>,0<callSymbol>,2<graphVertex>)
                             InputGraph.ReturnEdge(2<graphVertex>,0<returnSymbol>,3<graphVertex>)
                             InputGraph.ReturnEdge(3<graphVertex>,0<returnSymbol>,4<graphVertex>) |])
    let startV = [|0<graphVertex>|]
    let q = RSM(0<rsmState>, System.Collections.Generic.HashSet([0<rsmState>]),
                [|CFGEdge(0<rsmState>,0<rsmState>)
                  CallEdge(0<rsmState>,0<callSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,2<rsmState>)
                  ReturnEdge(2<rsmState>,0<returnSymbol>,0<rsmState>)|])
    let reachable = GLL.eval graph startV q
    printfn "Reachable: %A" reachable    

let example7 () =
    let graph = InputGraph([|InputGraph.CallEdge(0<graphVertex>,0<callSymbol>,0<graphVertex>)
                             InputGraph.ReturnEdge(0<graphVertex>,0<returnSymbol>,1<graphVertex>)
                             InputGraph.ReturnEdge(1<graphVertex>,0<returnSymbol>,0<graphVertex>) |])
    let startV = [|0<graphVertex>|]
    let q = RSM(0<rsmState>, System.Collections.Generic.HashSet([0<rsmState>]),
                [|CFGEdge(0<rsmState>,0<rsmState>)
                  CallEdge(0<rsmState>,0<callSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,2<rsmState>)
                  ReturnEdge(2<rsmState>,0<returnSymbol>,0<rsmState>)|])
    let reachable = GLL.eval graph startV q
    printfn "Reachable: %A" reachable    

    
[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    example5 [|1<graphVertex>|]
    0 // return an integer exit code