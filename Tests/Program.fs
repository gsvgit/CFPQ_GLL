open System.Collections.Generic
open CFPQ_GLL.GSS
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open Expecto
open Tests

let config = {FsCheckConfig.defaultConfig with maxTest = 10000}

let properties =
  testList "FsCheck samples" [
    testProperty "unpackGSSEdge - packGSSEdge is identity" <| fun a ->
      let dummyEdgeInfo = Dictionary<_,_>()
      dummyEdgeInfo.Add(a, 10)
      let unpacked = unpackGSSEdge dummyEdgeInfo a
      let packedBack = packGSSEdge (packGSSVertex unpacked.GSSVertex) unpacked.RSMState
      a = packedBack
      
    testProperty "packGSSEdge - unpackGSSEdge is identity" <| fun inputPosition rsmStateForGSS rsmState ->
      if inputPosition >= 0<graphVertex> && inputPosition <= GRAPH_VERTEX_MAX_VALUE
         && rsmState >= 0<rsmState> && rsmState <= RSM_VERTEX_MAX_VALUE
         && rsmStateForGSS >= 0<rsmState> && rsmStateForGSS <= RSM_VERTEX_MAX_VALUE
         
      then
        
        let packed = packGSSEdge (packGSSVertex (GSSVertex(inputPosition,rsmStateForGSS))) rsmState
        let dummyEdgeInfo = Dictionary<_,_>()
        dummyEdgeInfo.Add(packed, 10)
        let unpackedBack = unpackGSSEdge dummyEdgeInfo packed
        unpackedBack.GSSVertex.InputPosition = inputPosition
        && unpackedBack.GSSVertex.RSMState = rsmStateForGSS
        && unpackedBack.RSMState = rsmState
      else true
      
    testProperty "unpackGSSVertex - packGSSVertex is identity" <| fun a ->
      let unpacked = unpackGSSVertex a
      let packedBack = packGSSVertex unpacked
      a = packedBack

    testProperty "packGSSVertex - unpackGSSVertex is identity" <| fun inputPosition rsmState ->
      if inputPosition >= 0<graphVertex> && inputPosition <= CFPQ_GLL.InputGraph.GRAPH_VERTEX_MAX_VALUE
         && rsmState >= 0<rsmState> && rsmState <= CFPQ_GLL.RSM.RSM_VERTEX_MAX_VALUE
      then
        let packed = CFPQ_GLL.GSS.packGSSVertex (GSSVertex(inputPosition, rsmState))
        let unpackedBack = CFPQ_GLL.GSS.unpackGSSVertex packed
        inputPosition = unpackedBack.InputPosition
        && rsmState = unpackedBack.RSMState
      else true

  ]
  
[<EntryPoint>]
let main argv =    
    Tests.runTestsWithCLIArgs [] [||] (testList "all tests" [properties; GLLTests.properties])
    