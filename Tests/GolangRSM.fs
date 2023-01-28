module Tests.GolangRSM

open CFPQ_GLL.RsmBuilder

let golangSrc () =
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

    let mkAlt x = List.map t x |> List.reduce ( +|+ )

    let numeralsEx0 = ['1'..'9'] |> mkAlt
    let numerals = ['0'..'9'] |> mkAlt
    let letters = (List.concat [['a'..'z']; ['A' .. 'Z']]) |> mkAlt
    let trueVal = literal "true"
    let falseVal = literal "false"

    let boolType = literal "bool"
    let intType = literal "int"

    let returnLit = literal "return"

    let space = [' '; '\t'; '\n'; '\r'] |> mkAlt
    let Spaces = nt "Spaces"

    [
        Program   =>  many space ** many FuncDecl
        Spaces   =>  many space
        Num       =>  protect (numeralsEx0 ** many numerals)
        Var       =>  protect (some letters)

        IntExpr   =>  IntExpr ** t '+' ** IntTerm
                      +|+ IntExpr ** t '-' ** IntTerm
                      +|+ IntTerm
        IntTerm   =>  IntTerm ** t '*' ** Power  +|+ Power
        Power     =>  Factor ** t '^' ** Power +|+ Factor
        Factor    =>  BoolExpr +|+ Num +|+ t '(' ** IntExpr ** t ')'


        BoolExpr  =>  BoolExpr ** literal "||" ** BoolTerm +|+ BoolTerm
        BoolTerm  =>  BoolTerm ** literal "&&" ** Comp +|+ Comp
        Comp      =>  IntExpr ** t '<' ** IntExpr
                      +|+ IntExpr ** t '>' ** IntExpr
                      +|+ IntExpr ** literal "<=" ** IntExpr
                      +|+ IntExpr ** literal ">=" ** IntExpr
                      +|+ IntExpr ** literal "==" ** IntExpr
                      +|+ IntExpr ** literal "!=" ** IntExpr
                      +|+ BoolVals
        BoolVals  =>  Var +|+ trueVal +|+ falseVal +|+ t '(' ** BoolExpr ** t ')'

        Expr => IntExpr
        ExprList => list Expr (t ',')

        ArgDecl     =>  protect (Var ** t ' ') ** VarType
        ArgDeclList =>  list ArgDecl (t ',')
        VarType     =>  intType +|+ boolType


        If    => literal "if" ** t '(' ** BoolExpr ** t ')' ** t '{' ** Block ** t '}'
        For   => literal "for" ** t '(' ** Assign ** t ';' ** BoolExpr ** t ';' ** Assign ** t ')' ** t '{' ** Block ** t '}'
        While => literal "while" ** t '(' ** BoolExpr ** t ')' ** t '{' ** Block ** t '}'

        FuncDecl => protect(literal "func" ** t ' ') ** Var ** t '(' ** ArgDeclList ** t ')' ** VarType ** t '{' ** Block ** t '}'
        VarDecl  => protect(literal "var" ** t ' ') ** protect(Var ** t ' ') ** (intType +|+ boolType) ** t '=' ** Expr

        Assign => Var ** t '=' ** Expr

        Block     =>  many Statement
        Statement =>  VarDecl ** t ';'
                      +|+ IntExpr ** t ';'
                      +|+ BoolExpr ** t ';'
                      +|+ Var ** t '(' ** ExprList ** t ')' ** t ';'
                      +|+ Assign ** t ';'
                      +|+ If
                      +|+ For
                      +|+ While
                      +|+ returnLit ** Expr ** t ';'
    ] |> build [' '; '\t'; '\n'; '\r']

let _, terminalMapping, nonTerminalMapping = golangSrc ()

let golangRSM () = golangSrc () |> fun (rsm, _, _) -> rsm

let functionSample = """
func longFuncName(x int, y bool, longName int) int {
    if (y) {
        return x + longName;
    }
    return x - longName;
}
"""

let cycleSample = """
func cycles(x int, y int) int {
    while ( y > x ) {
        if (x > y) {
            return x * x * x;
        }
    }
    for (i = 1; i < y; i = i + 1) {
        x = x + 1;
    }
    return x;
}
"""

let expressionSample = """
func expression(x int, y int) int {
    var expr int = (x + y) ^ z * (x - y ^ z);
    var expr bool = x && (y || z);
    if (x < y) {
        return x + y;
    }
    return x - y;
}
"""
