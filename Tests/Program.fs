
open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.Common
open CFPQ_GLL.GLL
open CFPQ_GLL.InputGraph
open Tests.InputGraph
open CFPQ_GLL.SPPF
open Expecto
open CFPQ_GLL.RsmBuilder
open Tests.LinearGraphReader


let config = {FsCheckConfig.defaultConfig with maxTest = 10000}

let test (graph: InputGraph) q =

    let startV = [|0<inputGraphVertex>|]
    graph.ToDot (0, "graph.dot")
    let startVertices,_ = graph.ToCfpqCoreGraph (HashSet startV)
    let result = GLL.defaultEval startVertices q GLL.AllPaths

    match result with
    | QueryResult.MatchedRanges ranges ->
      let sppf = q.OriginalStartState.NonTerminalNodes.ToArray()
      let actual = TriplesStoredSPPF (sppf, Dictionary())

      actual.ToDot "sppf.dot"

      Tests.GLLTests.dumpResultToConsole actual
    | _ -> failwith "Unexpected result."
    0

let test2 (graph: InputGraph) q =

    let startV = [|0<inputGraphVertex>|]
    let finalV = [|LanguagePrimitives.Int32WithMeasure<inputGraphVertex>(graph.NumberOfVertices() - 1)|]
    graph.ToDot (0, "graph.dot")
    let startVertices,mapping = graph.ToCfpqCoreGraph (HashSet startV)
    let finalVertices = Array.map (fun x -> mapping[x]) finalV |> HashSet
    let start = System.DateTime.Now
    let result = GLL.errorRecoveringEval finalVertices startVertices q GLL.AllPaths
    printfn $"Parsed in %A{(System.DateTime.Now - start).TotalMilliseconds} ms"
    
    match result with
    | QueryResult.MatchedRanges ranges ->
      let sppf = q.OriginalStartState.NonTerminalNodes.ToArray()
      let root = sppf |> Array.filter (fun n -> startVertices.Contains n.LeftPosition && finalVertices.Contains n.RightPosition) |> Array.minBy(fun n -> n.Distance)            
      printfn $"Root weight = %A{root.Distance}"
      let actual = TriplesStoredSPPF([|root|], Dictionary())
      //let actual = TriplesStoredSPPF (sppf, Dictionary())

      actual.ToDot "sppf.dot"

      //Tests.GLLTests.dumpResultToConsole actual
    | _ -> failwith "Unexpected result."
    0




[<EntryPoint>]
let main argv =
    (*
    let tests =
        testList "All tests" [
            Tests.LinearGraphReader.``Linear graph creating tests``
            Tests.RSMCalculator.``Calculator RSM tests``
            Tests.EpsilonEdge.``Epsilon edges tests``
            Tests.ErrorRecoveringTest.``Error recovering tests``
        ] |> testSequenced
    Tests.runTestsWithCLIArgs [] [||] tests
*)
    //Tests.runTestsWithCLIArgs [] [||] (testList "debug tests" [Tests.GLLTests.``Form V#``])
    let re1 = t "a" ++ many (t "b")
    let re2 = many (t "a" ++ many (t "b"))                
    let re3 = many (t "a" ++ t "b")           
    let re4 = many((t "a" ++ t "a") *|* (t "b" ++ t "b"))
    let re5 = opt((t "a" ++ t "a") *|* (t "b" ++ t "b"))
    let re6 = (t "a" *|* t "b" *|* t "c")
              ++ (t "1" *|* t "2" *|* t "3")
              ++ (t "x" *|* t "y" *|* t "z")
              |> many
              
    let ws x =
            let space = t " " //*|* t "\n"
            many space ++ x ++ many space
            
    let rsmTest =
        [
            nt "S" => (ws (opt(t ">" *|* t "<") ++ opt (t "=")))
        ] |> build
    
    let rsm =
        let Num = nt "Num"
        let S = nt "S"
        [
            S   =>     (Num ++ t "+" ++ S)
                     *|* Num
                     *|* (t "(" ++ S ++ t ")")
            Num =>    ([|1..9|] |> Array.map (string >> t) |> Array.reduce ( *|* ))
                     ++ many ([|0..9|] |> Array.map (string >> t) |> Array.reduce ( *|* ))
        ]
        |> build
        
    let rsm2 =
        let Num = nt "Num"        
        let Expr = nt "Expr"
        let Var = nt "Var"
        let Atom = nt "Atom"
        let Prod = nt "Prod"
        let Stmt = nt "Stmt"
        let Program = nt "Program"
       
        [
            Program =>  nonemptyList Stmt (t ";")
            
            Num  => ([|1..9|] |> Array.map (string >> t) |> Array.reduce ( *|* ))
                     ++ many ([|0..9|] |> Array.map (string >> t) |> Array.reduce ( *|* ))            
            Var  => ([|'a'..'z'|] |> Array.map (string >> t) |> Array.reduce ( *|* ))
                    ++ many (    ([|'a'..'z'|] |> Array.map (string >> t) |> Array.reduce ( *|* ))
                             *|* ([|0..9|] |> Array.map (string >> t) |> Array.reduce ( *|* )))            
            Atom => Num *|* Var *|* (t "(" ++ Expr ++ t ")")
            Prod => nonemptyList Atom (t "*" *|* t "/")
            Expr => nonemptyList Prod (t "+" *|* t "-")
            Stmt => literal "let" ++ t "=" ++ Expr
        ]
        |> build
        
    //rsm2.ToDot "rsm2.dot"
    let miniML,terminalMapping =
        let Num = nt "Num"        
        let Arithmetic = nt "Arithmetic"
        let Var = nt "Var"
        let Atom = nt "Atom"
        let Prod = nt "Prod"        
        let Program = nt "Program"
        let Expr = nt "Expr"
        let FinalExpr = nt "FinalExpr"
        let Compare = nt "Compare"
        let space = t " " *|* t "\n" *|* t "\r" *|* t "\t"
        let ws x = many space ++ x 
       
        [
            Program => Expr // many Expr ++ many space
                        
            Num  => ws(([|1..9|] |> Array.map (string >> t) |> Array.reduce ( *|* ))
                     ++ many ([|0..9|] |> Array.map (string >> t) |> Array.reduce ( *|* )))            
            Var  => ws(([|'a'..'z'|] |> Array.map (string >> t) |> Array.reduce ( *|* ))
                    ++ many (    ([|'a'..'z'|] |> Array.map (string >> t) |> Array.reduce ( *|* ))
                             *|* ([|0..9|] |> Array.map (string >> t) |> Array.reduce ( *|* ))))            
            Atom =>  Num
                    *|* (Var ++ many (space ++ Atom))
                    *|* (ws (t "(") ++ Expr ++ ws (t ")"))
                    *|* (Var ++ t "[" ++ Expr ++ t"]")
            Prod => nonemptyList Atom (ws (t "*" *|* t "/"))
            Arithmetic => nonemptyList Prod (ws(t "+" *|* t "-"))
            Compare => nonemptyList Arithmetic (ws ((opt (t"!") ++ t "=") *|* ((t ">" *|* t "<") ++ opt (t "="))))
            FinalExpr => 
                Compare
                *|* (ws(literal "if") ++ Expr ++ ws(literal "then") ++ space ++ Expr ++ opt (ws(literal "else") ++ space  ++ Expr))
                *|* (ws(literal "while") ++ Expr ++ ws(literal "do") ++ Expr)
            Expr => FinalExpr
                    *|* (ws(literal "let")  ++ opt(space ++ ws(literal "rec")) ++ space ++ Var ++ many (space ++ Var) ++ ws(t "=") ++ FinalExpr ++ ws(literal "in") ++ Expr) 
        ]
        |> build
    miniML.ToDot "rsmMiniML.dot"
    //let g = mkLinearGraph id terminalMapping Config.LinearGraphWithDeletionsAndInsertions "let x = 2 in lt y x in y"
    //let g = mkLinearGraph id terminalMapping Config.LinearGraphWithDeletionsAndInsertions "let x = 2 in let x = 2 in let x = 2 in let x = 2 in let x = 2 in let x = 2 in let x = 2 in lt y x in let x = 2 in let x = 2 in let x = 2 in let x = 2 in let x = 2 in let x = 2 in let x = 2 in let x = 2 in let x = 2 in y"
    //let g = mkLinearGraph id terminalMapping Config.LinearGraphWithDeletionsAndInsertions
    //           "let rec y x=if x>1 then x+2 else y (x-1) in lt z x=if x<1 then x+2 else (x-1) in y z 2)"
    
    let str2 =
        "let y = 1
         in
         let x = 2
         in
         let x = 2
         in lt x = 2 in let x = 2 in let x = 2 in let x = 2 in y"
    let str3 =
        "lt y = 1
         in
         let x = 3
         in
         y"
    (*
    let str = "
    let rec y x =
        if x > 1
        then x + 2
        else y (x - 1)
    in
    let z x =
        if x < 1
        then x + 2
        else (x - 1)
    in
    let rec y x =
        if x > 1
        then x + 2
        else y (x - 1)
    in
    let z x =
        if x < 1
        then x + 2
        else (x - 1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in lt z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
y z 2)"
*)
    let g = mkLinearGraph id terminalMapping Config.LinearGraphWithDeletionsAndInsertions str3
                

    printfn $"input length = %A{str3.Length}"
    
    //g.ToDot (false, "input.dot")
    
    test2 g miniML
    //test2 g miniML
    //0
    //Tests.runTestsWithCLIArgs [] [||] (testList "debug tests" [Tests.DynamicTests.``Simple call``])
   
    //Tests.runTestsWithCLIArgs [] [||] (testList "all tests" [Tests.GLLTests.tests])
    //Tests.runTestsWithCLIArgs [] [||] (testList "all tests" [Tests.DistancesTests.tests])
    //go ()

