module Tests.GolangRSM

open CFPQ_GLL.RsmBuilder

let golangRSM, mapping =
    let Num = nt "Num"
    let Var = nt "Var"

    let IntExpr = nt "IntExpr"
    let IntTerm = nt "IntTerm"
    let Power = nt "Power"
    let Factor = nt "Factor"

    let BoolExpr = nt "BoolExpr"
    let BoolTerm = nt "BoolTerm"
    let Comp = nt "Comp"
    let BoolVals = nt "BoolVals"

    let Expr = nt "Expr"
    let ExprList = nt "ExprList"

    let Assign = nt "Assign"

    let ArgDecl = nt "ArgDecl"
    let VarDecl = nt "VarDecl"
    let ArgDeclList = nt "VarDeclList"
    let VarType = nt "VarType"

    let If = nt "If"
    let While = nt "While"
    let For = nt "For"

    let FuncDecl = nt "FuncDecl"

    let Statement = nt "Statement"
    let Block = nt "Block"
    let Program = nt "Program"

    let mkAlt x = List.map (string >> t) x |> List.reduce ( +|+ )

    let numeralsEx0 = [1..9] |> mkAlt
    let numerals = [1..9] |> mkAlt
    let letters = ['a'..'z'] |> mkAlt
    let trueVal = literal "true"
    let falseVal = literal "false"

    let boolType = literal "bool"
    let intType = literal "int"


    [
        Num       =>  numeralsEx0 ** many numerals
        Var       =>  some letters

        IntExpr   =>  IntExpr ** t "+" ** IntTerm
                      +|+ IntExpr ** t "-" ** IntTerm
                      +|+ IntTerm
        IntTerm   =>  IntTerm ** t "*" ** Power  +|+ Power
        Power     =>  Factor ** t "^" ** Power +|+ Factor
        Factor    =>  Var +|+ Num +|+ t "(" ** IntExpr ** t ")"


        BoolExpr  =>  BoolExpr ** literal "||" ** BoolTerm +|+ BoolTerm
        BoolTerm  =>  BoolTerm ** literal "&&" ** Comp +|+ Comp
        Comp      =>  IntExpr ** t "<" ** IntExpr
                      +|+ IntExpr ** t ">" ** IntExpr
                      +|+ IntExpr ** literal "<=" ** IntExpr
                      +|+ IntExpr ** literal ">=" ** IntExpr
                      +|+ IntExpr ** literal "==" ** IntExpr
                      +|+ IntExpr ** literal "!=" ** IntExpr
                      +|+ BoolVals
        BoolVals  =>  Var +|+ trueVal +|+ falseVal +|+ t "(" ** BoolExpr ** t ")"

        Expr => BoolExpr +|+ IntExpr
        ExprList => list Expr (t ",")

        ArgDecl     =>  VarType ** t " " ** Var
        ArgDeclList =>  list ArgDecl (t ",")
        VarType     =>  intType +|+ boolType


        If    => literal "if" ** t "(" ** BoolExpr ** t ")" ** t "{" ** Block ** t "}"
        For   => literal "for" ** t "(" ** ArgDecl ** t ";" ** BoolExpr ** t ";" ** Statement ** t ")" ** t "{" ** Block ** t "}" // Fixme
        While => literal "while" ** t "(" ** BoolExpr ** t ")" ** t "{" ** Block ** t "}"

        FuncDecl => literal "func" ** t " " ** Var ** t "(" ** ArgDeclList ** t ")" ** VarType ** t "{" ** Block ** t "}"
        VarDecl  => intType ** t " " ** Var ** t "=" ** IntExpr +|+ boolType ** t " " ** Var ** t "=" ** BoolExpr

        Assign => Var ** t "=" ** Expr

        Block     =>  many Statement
        Statement =>  VarDecl ** t ";"
                      +|+ IntExpr ** t ";"
                      +|+ BoolExpr ** t ";"
                      +|+ Var ** t "(" ** ExprList ** t ")" ** t ";"
                      +|+ Assign ** t ";"
                      +|+ If
                      +|+ For
                      +|+ While

        Program   =>  many FuncDecl
    ] |> build [" "; "\t"; "\n"; "\r"]

