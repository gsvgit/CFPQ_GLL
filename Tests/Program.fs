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
      let unpacked = unpackGSSEdge (a,None)
      let packedBack = packGSSEdge (packGSSVertex unpacked.GSSVertex) unpacked.RSMState
      a = packedBack
      
    testProperty "packGSSEdge - unpackGSSEdge is identity" <| fun inputPosition rsmStateForGSS rsmState ->
      if inputPosition >= 0<graphVertex> && inputPosition <= GRAPH_VERTEX_MAX_VALUE
         && rsmState >= 0<rsmState> && rsmState <= RSM_VERTEX_MAX_VALUE
         && rsmStateForGSS >= 0<rsmState> && rsmStateForGSS <= RSM_VERTEX_MAX_VALUE
         
      then
        
        let packed = packGSSEdge (packGSSVertex (GSSVertex(inputPosition,rsmStateForGSS))) rsmState        
        let unpackedBack = unpackGSSEdge (packed,None)
        unpackedBack.GSSVertex.InputPosition = inputPosition
        && unpackedBack.GSSVertex.RSMState = rsmStateForGSS
        && unpackedBack.RSMState = rsmState
      else true
      
    testCase "packGSSEdge - unpackGSSEdge for maximal values" <| fun () ->
      let inputPosition = GRAPH_VERTEX_MAX_VALUE
      let rsmState = RSM_VERTEX_MAX_VALUE
      let rsmStateForGSS = RSM_VERTEX_MAX_VALUE
         
             
      let packed = packGSSEdge (packGSSVertex (GSSVertex(inputPosition,rsmStateForGSS))) rsmState        
      let unpackedBack = unpackGSSEdge (packed,None)
      Expect.equal unpackedBack.GSSVertex.InputPosition inputPosition ""
      Expect.equal unpackedBack.GSSVertex.RSMState rsmStateForGSS ""
      Expect.equal unpackedBack.RSMState rsmState "RSM states should be equal."
      
      
    testProperty "unpackGSSVertex - packGSSVertex is identity" <| fun a ->
      if a > 0L<gssVertex> && a <= MAX_VALUE_FOR_GSS_VERTEX
      then
        let unpacked = unpackGSSVertex a
        let packedBack = packGSSVertex unpacked
        a = packedBack
      else true

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
    Tests.runTestsWithCLIArgs [] [||] (testList "all tests" [properties; GLLTests.tests])
    