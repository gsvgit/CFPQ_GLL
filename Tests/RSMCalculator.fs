module Tests.RSMCalculator

open CFPQ_GLL.RsmBuilder

let calculatorRSM () =
    // Language grammar
    // Num -> (1..9) (0..9)*
    // Var -> (a..z) | (a..z) Var
    // Expr -> Expr '+' Term | Expr '-' Term | Term
    // Term -> Term '*' Power | Power
    // Power -> Factor '^' Power | Factor
    // Factor -> Var | Num | '(' Expr ')'
    // Statement -> Var '=' Expr
    // Program -> Statement ';' | Statement ';' Program

    let Num = nt "Num"
    let Var = nt "Var"
    let Expr = nt "Expr"
    let Term = nt "Term"
    let Power = nt "Power"
    let Factor = nt "Factor"
    let Statement = nt "Statement"
    let Program = nt "Program"

    let mkAlt x = List.map (string >> t) x |> List.reduce ( +|+ )

    let numeralsEx0 = [1..9] |> mkAlt
    let numerals = [1..9] |> mkAlt
    let letters = ['a'..'z'] |> mkAlt

    [
        Num       =>  numeralsEx0 ** many numerals
        Var       =>  some letters
        Expr      =>  Expr ** t "+" ** Term
                      +|+ Expr ** t "-" ** Term
                      +|+ Term
        Term      =>  Term ** t "*" ** Power
                      +|+ Power
        Power     =>  Factor ** t "^" ** Power
                      +|+ Factor
        Factor    =>  Var +|+ Num +|+ t "(" ** Expr ** t ")"
        Statement =>  Var ** t "=" ** Expr
        Program   =>  Statement ** t ";"
                      +|+ Statement ** t ";" ** Program
    ] |> build []
