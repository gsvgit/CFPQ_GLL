module Tests.DistancesTests

open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.Common
open CFPQ_GLL.GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF
open Expecto
open Tests.InputGraph

let tests =  
  testList "GLL tests on graph changes" [
    testCase "Simple call" <| fun () ->
        let terminalForCFGEdge = 0<terminalSymbol>
        let graphEntryPoint = InputGraphVertexBase() :> IInputGraphVertex
        let graphExit = InputGraphVertexBase() :> IInputGraphVertex
        graphEntryPoint.OutgoingEdges.Add(0<terminalSymbol>, HashSet [|graphExit|])
        
        let balancedBracketsRsmBoxStartState = RsmState(true,true) :> IRsmState
        let balancedBracketsRsmBoxFinalState = balancedBracketsRsmBoxStartState
        let callImbalanceRsmBoxStartState = RsmState(true,false) :> IRsmState
        let callImbalanceRsmBoxFinalState = RsmState(false,true) :> IRsmState
        let historyRsmBoxStartState = RsmState(true,false) :> IRsmState
        let historyRsmBoxDefaultFinalState = RsmState(false,true) :> IRsmState
        let historyRsmBoxHistoryStartState = RsmState(false,true) :> IRsmState
        
        let historyRsmBox =
            let box = RSMBox()
            box.AddState historyRsmBoxStartState
            box.AddState historyRsmBoxDefaultFinalState
            box.AddState historyRsmBoxHistoryStartState
            historyRsmBoxStartState.OutgoingNonTerminalEdges.Add(callImbalanceRsmBoxStartState, HashSet [|historyRsmBoxDefaultFinalState|])
            historyRsmBoxStartState.OutgoingNonTerminalEdges.Add(balancedBracketsRsmBoxStartState, HashSet [|historyRsmBoxHistoryStartState|])        
            box
        let balancedBracketsRsmBox =         
            let box = RSMBox()
            box.AddState balancedBracketsRsmBoxStartState
            balancedBracketsRsmBoxStartState.OutgoingTerminalEdges.Add(terminalForCFGEdge, HashSet [|balancedBracketsRsmBoxFinalState|])
            box 
        let callImbalanceBracketsRsmBox =         
            let box = RSMBox()
            box.AddState callImbalanceRsmBoxStartState
            box.AddState callImbalanceRsmBoxFinalState
            callImbalanceRsmBoxStartState.OutgoingNonTerminalEdges.Add(balancedBracketsRsmBoxStartState, HashSet [|callImbalanceRsmBoxFinalState|])
            box
            
        let q = RSM([|historyRsmBox; balancedBracketsRsmBox; callImbalanceBracketsRsmBox|], historyRsmBox)
          
        let expected =
          let nodes = Dictionary<_,_>()
          nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,0<inputGraphVertex>))
          nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,0<inputGraphVertex>,0<rsmState>,0<rsmState>))
          nodes.Add(2, TriplesStoredSPPFNode.EpsilonNode (0<inputGraphVertex>,0<rsmState>))
          nodes.Add(3, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,0<terminalSymbol>,0<inputGraphVertex>))
          
          let edges = ResizeArray<_>([|(0,1); (1,2); (1,3);|])
          let distances = [|0<distance>|]
          (nodes,edges,distances)
          
        GLLTests.runGLLAndCheckResultForManuallyCreatedGraph testName (HashSet [|graphEntryPoint|]) q expected
         
        
  ]