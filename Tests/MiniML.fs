module Tests.MiniML

open CFPQ_GLL.RsmBuilder
open Tests.LinearGraphReader

let re1 = t "a" ** many (t "b")
let re2 = many (t "a" ** many (t "b"))
let re3 = many (t "a" ** t "b")
let re4 = many((t "a" ** t "a") +|+ (t "b" ** t "b"))
let re5 = opt((t "a" ** t "a") +|+ (t "b" ** t "b"))
let re6 = (t "a" +|+ t "b" +|+ t "c")
          ** (t "1" +|+ t "2" +|+ t "3")
          ** (t "x" +|+ t "y" +|+ t "z")
          |> many

let ws x =
        let space = t " " //+|+ t "\n"
        many space ** x ** many space

let rsmTest =
    [
        nt "S" => (ws (opt(t ">" +|+ t "<") ** opt (t "=")))
    ] |> build []

let rsm =
    let Num = nt "Num"
    let S = nt "S"
    [
        S   =>     (Num ** t "+" ** S)
                 +|+ Num
                 +|+ (t "(" ** S ** t ")")
        Num =>    ([|1..9|] |> Array.map (string >> t) |> Array.reduce ( +|+ ))
                 ** many ([|0..9|] |> Array.map (string >> t) |> Array.reduce ( +|+ ))
    ]
    |> build []

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

        Num  => ([|1..9|] |> Array.map (string >> t) |> Array.reduce ( +|+ ))
                 ** many ([|0..9|] |> Array.map (string >> t) |> Array.reduce ( +|+ ))
        Var  => ([|'a'..'z'|] |> Array.map (string >> t) |> Array.reduce ( +|+ ))
                ** many (    ([|'a'..'z'|] |> Array.map (string >> t) |> Array.reduce ( +|+ ))
                         +|+ ([|0..9|] |> Array.map (string >> t) |> Array.reduce ( +|+ )))
        Atom => Num +|+ Var +|+ (t "(" ** Expr ** t ")")
        Prod => nonemptyList Atom (t "*" +|+ t "/")
        Expr => nonemptyList Prod (t "+" +|+ t "-")
        Stmt => literal "let" ** t "=" ** Expr
    ]
    |> build []

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
    let space = t " " +|+ t "\n" +|+ t "\r" +|+ t "\t"
    let ws x = many space ** x

    [
        Program => Expr // many Expr ** many space

        Num  => ws(([|1..9|] |> Array.map (string >> t) |> Array.reduce ( +|+ ))
                 ** many ([|0..9|] |> Array.map (string >> t) |> Array.reduce ( +|+ )))
        Var  => ws(([|'a'..'z'|] |> Array.map (string >> t) |> Array.reduce ( +|+ ))
                ** many (    ([|'a'..'z'|] |> Array.map (string >> t) |> Array.reduce ( +|+ ))
                         +|+ ([|0..9|] |> Array.map (string >> t) |> Array.reduce ( +|+ ))))
        Atom =>  Num
                +|+ (Var ** many (space ** Atom))
                +|+ (ws (t "(") ** Expr ** ws (t ")"))
                +|+ (Var ** t "[" ** Expr ** t"]")
        Prod => nonemptyList Atom (ws (t "*" +|+ t "/"))
        Arithmetic => nonemptyList Prod (ws(t "+" +|+ t "-"))
        Compare => nonemptyList Arithmetic (ws ((opt (t"!") ** t "=") +|+ ((t ">" +|+ t "<") ** opt (t "="))))
        FinalExpr =>
            Compare
            +|+ (ws(literal "if") ** Expr ** ws(literal "then") ** space ** Expr ** opt (ws(literal "else") ** space  ** Expr))
            +|+ (ws(literal "while") ** Expr ** ws(literal "do") ** Expr)
        Expr => FinalExpr
                +|+ (ws(literal "let")  ** opt(space ** ws(literal "rec")) ** space ** Var ** many (space ** Var) ** ws(t "=") ** FinalExpr ** ws(literal "in") ** Expr)
    ]
    |> build []
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
        else (x - 1)
    in
    let rec y x =
        if x > 1
        then (x + 2)
        else y (x - 1)
    in
    let z x =
        if x < 1
        then x + 2
        else (x - 1)
    in
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
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
let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in lt rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in let rec y x=if x>1 then x+2 else y (x-1) in let z x=if x<1 then x+2 else (x-1) in
y z 2"

let g = mkLinearGraph id terminalMapping str
