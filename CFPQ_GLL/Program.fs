// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open CFPQ_GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM

// Define a function to construct a message to print
let from whom =
    $"from %s{whom}"

let example1 () =
    let graph = InputGraph([|InputGraph.CFGEdge(0<graphVertex>,1<graphVertex>)|])
    let startV = [|0<graphVertex>|]
    let q = RSM(0<rsmState>, System.Collections.Generic.HashSet([1<rsmState>]),[|CFGEdge(0<rsmState>,1<rsmState>)|])
    let reachable = GLL.eval graph startV q
    printfn $"Reachable: %A{reachable}"
    
let example2 () =
    let graph = InputGraph([|InputGraph.CFGEdge(0<graphVertex>,1<graphVertex>)|])
    let startV = [|0<graphVertex>|]
    let q = RSM(0<rsmState>, System.Collections.Generic.HashSet([0<rsmState>]),[|CFGEdge(0<rsmState>,0<rsmState>)|])
    let reachable = GLL.eval graph startV q
    printfn $"Reachable: %A{reachable}"

let example3 () =
    let graph = InputGraph([|InputGraph.CFGEdge(0<graphVertex>,0<graphVertex>)|])
    let startV = [|0<graphVertex>|]
    let q = RSM(0<rsmState>, System.Collections.Generic.HashSet([0<rsmState>]),[|CFGEdge(0<rsmState>,0<rsmState>)|])
    let reachable = GLL.eval graph startV q
    printfn $"Reachable: %A{reachable}"
    
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
    printfn $"Reachable: %A{reachable}"    

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
    printfn $"Reachable: %A{reachable}"    

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
    printfn $"Reachable: %A{reachable}"    

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
    printfn $"Reachable: %A{reachable}"    


let example8 startV =
    let graph = InputGraph([|InputGraph.CFGEdge(0<graphVertex>,1<graphVertex>)
                             InputGraph.CFGEdge(1<graphVertex>,4<graphVertex>)
                             InputGraph.CFGEdge(2<graphVertex>,1<graphVertex>)
                             InputGraph.CFGEdge(2<graphVertex>,3<graphVertex>)
                             InputGraph.CFGEdge(5<graphVertex>,2<graphVertex>)
                             InputGraph.CallEdge(4<graphVertex>,0<callSymbol>,6<graphVertex>)
                             InputGraph.ReturnEdge(10<graphVertex>,0<returnSymbol>,5<graphVertex>)
                             
                             InputGraph.CFGEdge(6<graphVertex>,7<graphVertex>)
                             InputGraph.CFGEdge(7<graphVertex>,9<graphVertex>)
                             InputGraph.CFGEdge(8<graphVertex>,9<graphVertex>)
                             InputGraph.CFGEdge(9<graphVertex>,10<graphVertex>)
                             InputGraph.CallEdge(7<graphVertex>,1<callSymbol>,6<graphVertex>)
                             InputGraph.ReturnEdge(10<graphVertex>,1<returnSymbol>,8<graphVertex>)
                             
                             InputGraph.CFGEdge(11<graphVertex>,12<graphVertex>)
                             InputGraph.CFGEdge(12<graphVertex>,13<graphVertex>)
                             InputGraph.CFGEdge(14<graphVertex>,12<graphVertex>)
                             InputGraph.CFGEdge(14<graphVertex>,15<graphVertex>)
                             InputGraph.CallEdge(13<graphVertex>,2<callSymbol>,6<graphVertex>)
                             InputGraph.ReturnEdge(10<graphVertex>,2<returnSymbol>,14<graphVertex>)
                             
                             |])
    
    let q = RSM(0<rsmState>, System.Collections.Generic.HashSet([0<rsmState>]),
                [|CFGEdge(0<rsmState>,0<rsmState>)
                  CallEdge(0<rsmState>,0<callSymbol>,1<rsmState>)
                  NonTerminalEdge(1<rsmState>,2<rsmState>)
                  ReturnEdge(2<rsmState>,0<returnSymbol>,0<rsmState>)
                  
                  CallEdge(0<rsmState>,1<callSymbol>,3<rsmState>)
                  NonTerminalEdge(3<rsmState>,4<rsmState>)
                  ReturnEdge(4<rsmState>,1<returnSymbol>,0<rsmState>)
                  
                  CallEdge(0<rsmState>,2<callSymbol>,5<rsmState>)
                  NonTerminalEdge(5<rsmState>,6<rsmState>)
                  ReturnEdge(6<rsmState>,2<returnSymbol>,0<rsmState>)
                  
                  CallEdge(0<rsmState>,0<callSymbol>,0<rsmState>)
                  CallEdge(0<rsmState>,1<callSymbol>,0<rsmState>)
                  CallEdge(0<rsmState>,2<callSymbol>,0<rsmState>)
                  |])
    let reachable = GLL.eval graph startV q
    printfn "Reachable:"
    reachable |> Seq.iter (printf "%A, ")    

    
[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    example1 ()
    example2 ()
    example3 ()
    example4 ()
    example5 [|1<graphVertex>|]
    example6 ()
    example7 ()    
    example8 [|0<graphVertex>; 11<graphVertex>; 6<graphVertex>|]
    0 // return an integer exit code