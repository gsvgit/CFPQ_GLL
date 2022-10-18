
open System.Collections.Generic
open CFPQ_GLL
open CFPQ_GLL.Common
open CFPQ_GLL.GLL
open CFPQ_GLL.InputGraph
open Tests.InputGraph
open CFPQ_GLL.SPPF
open Expecto
open CFPQ_GLL.RsmBuilder


let config = {FsCheckConfig.defaultConfig with maxTest = 10000}

let test (graph: InputGraph) q =

    let startV = [|0<inputGraphVertex>|]
    graph.ToDot (0, "graph.dot")
    let startVertices,_ = graph.ToCfpqCoreGraph (HashSet startV)
    let result = GLL.eval startVertices q GLL.AllPaths

    match result with
    | QueryResult.MatchedRanges ranges ->
      let sppf = q.OriginalStartState.NonTerminalNodes.ToArray()
      let actual = TriplesStoredSPPF (sppf, Dictionary())

      actual.ToDot "sppf.dot"

      Tests.GLLTests.dumpResultToConsole actual
    | _ -> failwith "Unexpected result."
    0


[<EntryPoint>]
let main argv =
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
        let Compare = nt "Compare"
        let space = t " " *|* t "\n" *|* t "\r" *|* t "\t"
        let ws x = many space ++ x 
       
        [
            Program =>  many Expr ++ many space
                        
            Num  => ws(([|1..9|] |> Array.map (string >> t) |> Array.reduce ( *|* ))
                     ++ many ([|0..9|] |> Array.map (string >> t) |> Array.reduce ( *|* )))            
            Var  => ws(([|'a'..'z'|] |> Array.map (string >> t) |> Array.reduce ( *|* ))
                    ++ many (    ([|'a'..'z'|] |> Array.map (string >> t) |> Array.reduce ( *|* ))
                             *|* ([|0..9|] |> Array.map (string >> t) |> Array.reduce ( *|* ))))            
            Atom => Num
                    *|* (Var ++ many (space ++ Atom))
                    *|* (ws (t "(") ++ Expr ++ ws (t ")"))
                    *|* (Var ++ t "[" ++ Expr ++ t"]")
            Prod => nonemptyList Atom (ws (t "*" *|* t "/"))
            Arithmetic => nonemptyList Prod (ws(t "+" *|* t "-"))
            Compare => nonemptyList Arithmetic (ws ((opt (t"!") ++ t "=") *|* ((t ">" *|* t "<") ++ opt (t "="))))
            Expr =>
                Compare
                *|* (ws(literal "let") ++ opt(ws(literal "rec")) ++ space ++ Var ++ many (space ++ Var) ++ ws(t "=") ++ Expr ++ ws(literal "in") ++ Expr)
                *|* (ws(literal "if") ++ Expr ++ ws(literal "then") ++ Expr ++ opt (ws(literal "else") ++ Expr))
                *|* (ws(literal "while") ++ Expr ++ ws(literal "do") ++ Expr)                 
        ]
        |> build
    miniML.ToDot "rsmMiniML.dot"
    let g = InputGraph([|
        TerminalEdge(0<inputGraphVertex>, terminalMapping["l"], 1<inputGraphVertex>)
        TerminalEdge(1<inputGraphVertex>, terminalMapping["e"], 2<inputGraphVertex>)
        TerminalEdge(2<inputGraphVertex>, terminalMapping["t"], 3<inputGraphVertex>)
        TerminalEdge(3<inputGraphVertex>, terminalMapping[" "], 4<inputGraphVertex>)
        TerminalEdge(4<inputGraphVertex>, terminalMapping["x"], 5<inputGraphVertex>)
        TerminalEdge(5<inputGraphVertex>, terminalMapping["="], 6<inputGraphVertex>)
        TerminalEdge(6<inputGraphVertex>, terminalMapping["2"], 7<inputGraphVertex>)
    |])
    //g.ToDot ("input.dot")
    test g miniML
    //0
    //Tests.runTestsWithCLIArgs [] [||] (testList "debug tests" [Tests.DynamicTests.``Simple call``])
   
    //Tests.runTestsWithCLIArgs [] [||] (testList "all tests" [Tests.GLLTests.tests])
    //Tests.runTestsWithCLIArgs [] [||] (testList "all tests" [Tests.DistancesTests.tests])
    //go ()

