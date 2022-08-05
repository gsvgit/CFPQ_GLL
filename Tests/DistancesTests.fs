module Tests.DistancesTests

open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.Common
open CFPQ_GLL.GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open Expecto
open Tests.InputGraph

let tests =  
  testList "GLL tests with shortest distances computation." [
    testCase "Special graph for VSharp, special RSM for VSharp. Distances via specified states." <| fun () ->
        let graph = InputGraph([|TerminalEdge(0<inputGraphVertex>, 0<terminalSymbol>, 1<inputGraphVertex>)
                                 TerminalEdge(1<inputGraphVertex>, 1<terminalSymbol>, 2<inputGraphVertex>)
                                 TerminalEdge(2<inputGraphVertex>, 0<terminalSymbol>, 3<inputGraphVertex>)
                                 TerminalEdge(2<inputGraphVertex>, 0<terminalSymbol>, 4<inputGraphVertex>)
                                 TerminalEdge(4<inputGraphVertex>, 2<terminalSymbol>, 5<inputGraphVertex>)
                                 TerminalEdge(5<inputGraphVertex>, 0<terminalSymbol>, 6<inputGraphVertex>)
                                 TerminalEdge(6<inputGraphVertex>, 3<terminalSymbol>, 7<inputGraphVertex>)
                                 TerminalEdge(7<inputGraphVertex>, 0<terminalSymbol>, 8<inputGraphVertex>)
                                 TerminalEdge(8<inputGraphVertex>, 4<terminalSymbol>, 9<inputGraphVertex>)
                                 TerminalEdge(9<inputGraphVertex>, 0<terminalSymbol>, 10<inputGraphVertex>)
                                 TerminalEdge(10<inputGraphVertex>, 5<terminalSymbol>, 11<inputGraphVertex>)
                                 TerminalEdge(11<inputGraphVertex>, 0<terminalSymbol>, 12<inputGraphVertex>)
                                 
                                 TerminalEdge(3<inputGraphVertex>, 6<terminalSymbol>, 13<inputGraphVertex>)
                                 TerminalEdge(13<inputGraphVertex>, 0<terminalSymbol>, 14<inputGraphVertex>)
                                 TerminalEdge(14<inputGraphVertex>, 7<terminalSymbol>, 15<inputGraphVertex>)
                                 TerminalEdge(15<inputGraphVertex>, 0<terminalSymbol>, 12<inputGraphVertex>)
                                 
                                 TerminalEdge(12<inputGraphVertex>, 8<terminalSymbol>, 16<inputGraphVertex>)
                                 TerminalEdge(16<inputGraphVertex>, 0<terminalSymbol>, 17<inputGraphVertex>)
                               |])
        let startV = [|2<inputGraphVertex>; 13<inputGraphVertex>; 5<inputGraphVertex>; 7<inputGraphVertex>|]
        let rsm =
            let bBox,m =
                GLLTests.makeRsmBox (
                    Dictionary(),
                    0<rsmState>,
                    HashSet([0<rsmState>]),
                    [|
                      RSM.TerminalEdge(0<rsmState>,0<terminalSymbol>,0<rsmState>)                  
                    |])
                
            let sBox,m =
                GLLTests.makeRsmBox (
                    m,
                    1<rsmState>,
                    HashSet([2<rsmState>; 4<rsmState>; 6<rsmState>; 9<rsmState>; 12<rsmState>]),
                    [|
                      RSM.NonTerminalEdge(1<rsmState>,0<rsmState>,2<rsmState>)
                      RSM.TerminalEdge(2<rsmState>,8<terminalSymbol>,3<rsmState>)
                      RSM.NonTerminalEdge(3<rsmState>,0<rsmState>,4<rsmState>)
                      
                      RSM.TerminalEdge(2<rsmState>,7<terminalSymbol>,5<rsmState>)
                      RSM.NonTerminalEdge(5<rsmState>,0<rsmState>,6<rsmState>)
                      RSM.TerminalEdge(6<rsmState>,8<terminalSymbol>,7<rsmState>)
                      RSM.NonTerminalEdge(7<rsmState>,0<rsmState>,4<rsmState>)
                      
                      RSM.TerminalEdge(2<rsmState>,5<terminalSymbol>,8<rsmState>)
                      RSM.NonTerminalEdge(8<rsmState>,0<rsmState>,9<rsmState>)
                      RSM.TerminalEdge(9<rsmState>,8<terminalSymbol>,10<rsmState>)
                      RSM.NonTerminalEdge(10<rsmState>,0<rsmState>,4<rsmState>)
                      
                      RSM.TerminalEdge(2<rsmState>,4<terminalSymbol>,11<rsmState>)
                      RSM.NonTerminalEdge(11<rsmState>,0<rsmState>,12<rsmState>)
                      RSM.TerminalEdge(12<rsmState>,5<terminalSymbol>,13<rsmState>)
                      RSM.NonTerminalEdge(13<rsmState>,0<rsmState>,9<rsmState>)
                    |])
                
            RSM ([|sBox; bBox|], sBox)
          
        let startVertices,_ = graph.ToCfpqCoreGraph (HashSet startV)    
        let res = eval startVertices rsm AllPaths
        match res with
        | QueryResult.ReachabilityFacts _ -> failwith "Inconsistent result!"
        | QueryResult.MatchedRanges ranges -> () (*
            let distances = 
                ranges.GetShortestDistances(
                    rsm,
                    HashSet [|12<rsmState>; 9<rsmState>; 4<rsmState>; 2<rsmState>|],
                    HashSet [|11<rsmState>|],
                    Dictionary<_,_>(),
                    7<inputGraphVertex>,
                    [|8<inputGraphVertex>; 10<inputGraphVertex>; 17<inputGraphVertex>|])
            for distance in distances do
                printfn $"Distance: %A{distance}" *)
  ]