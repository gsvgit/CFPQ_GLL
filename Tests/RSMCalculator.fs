module Tests.RSMCalculator

open System.Collections.Generic
open CFPQ_GLL.Common
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF
open CFPQ_GLL
open Expecto
open FSharpx.Collections
open GLLTests
open Tests.InputGraph

let numerals = [|for i in [0..9] do LanguagePrimitives.Int32WithMeasure<terminalSymbol> i|]
let character =  [|for i in [10..35] do LanguagePrimitives.Int32WithMeasure<terminalSymbol> i|]
let plus, minus, mult, pow, eq = 36<terminalSymbol>, 37<terminalSymbol>, 38<terminalSymbol>, 39<terminalSymbol>, 40<terminalSymbol>
let openingBracket, closingBracket, dotComma = 41<terminalSymbol>, 42<terminalSymbol>, 43<terminalSymbol>

let calculatorRSM () =
    // Language grammar
    // Num -> 0 | (1..9) Num
    // Var -> (a..z) | (a..z) Var
    // Expr -> Expr '+' Term | Expr '-' Term | Term
    // Term -> Term '*' Power | Power
    // Power -> Factor '^' Power | Factor
    // Factor -> Var | Num | '(' Expr ')'
    // Statement -> Var '=' Expr
    // Program -> Statement ';' | Statement ';' Program

    let startNum, startVar, startExpr, startTerm, startFactor, startPower, startStatement, startProgram =
        RsmState(true, false), //:> IRsmState,
        RsmState(true, false), //:> IRsmState,
        RsmState(true, false), //:> IRsmState,
        RsmState(true, false), //:> IRsmState,
        RsmState(true, false),  //:> IRsmState,
        RsmState(true, false), //:> IRsmState,
        RsmState(true, false), //:> IRsmState,
        RsmState(true, false) //:> IRsmState
    let finishNum1, finishNum2, finishVar, finishExpr, finishTerm, finishFactor, finishPower, finishStatement, finishProgram =
        RsmState(false, true),// :> IRsmState,
        RsmState(false, true),// :> IRsmState,
        RsmState(false, true),// :> IRsmState,
        RsmState(false, true),// :> IRsmState,
        RsmState(false, true),// :> IRsmState,
        RsmState(false, true),// :> IRsmState,
        RsmState(false, true),// :> IRsmState,
        RsmState(false, true),// :> IRsmState,
        RsmState(false, true)// :> IRsmState

    let numBox =
        let box = RSMBox()
        box.AddState startNum
        box.AddState finishNum1
        box.AddState finishNum2

        startNum.OutgoingTerminalEdges.Add(numerals[0], HashSet([|finishNum1|]))
        for i in [1..numerals.Length - 1] do
            startNum.OutgoingTerminalEdges.Add(numerals[i], HashSet([|finishNum2|]))
        for i in [0..numerals.Length - 1] do
            finishNum2.OutgoingTerminalEdges.Add(numerals[i], HashSet([|finishNum2|]))
        box

    let varBox =
        let box = RSMBox()
        box.AddState startVar
        box.AddState finishVar

        for t in character do
            startVar.OutgoingTerminalEdges.Add(t, HashSet([|finishVar|]))
            finishVar.OutgoingTerminalEdges.Add(t, HashSet([|finishVar|]))
        box

    let factorBox =
        let box = RSMBox()
        let waitExprAfterOpeningBracket = RsmState(false, false) //:> IRsmState
        let waitClosingBracketAfterExpr = RsmState(false, false) //:> IRsmState
        box.AddState startFactor
        box.AddState finishFactor
        box.AddState waitExprAfterOpeningBracket
        box.AddState waitClosingBracketAfterExpr

        startFactor.OutgoingNonTerminalEdges.Add(startNum, HashSet([|finishFactor|]))
        startFactor.OutgoingNonTerminalEdges.Add(startVar, HashSet([|finishFactor|]))

        startFactor.OutgoingTerminalEdges.Add(openingBracket, HashSet([|waitExprAfterOpeningBracket|]))
        waitExprAfterOpeningBracket.OutgoingNonTerminalEdges.Add(startExpr, HashSet([|waitClosingBracketAfterExpr|]))
        waitClosingBracketAfterExpr.OutgoingTerminalEdges.Add(closingBracket, HashSet([|finishFactor|]))

        box

    let powerBox =
        let box = RSMBox()
        let waitPowerSymbolAfterFactor = RsmState(false, true) //:> IRsmState
        let waitPower = RsmState(false, false)// :> IRsmState

        box.AddState startPower
        box.AddState finishPower
        box.AddState waitPowerSymbolAfterFactor
        box.AddState waitPower

        startPower.OutgoingNonTerminalEdges.Add(startFactor, HashSet([|waitPowerSymbolAfterFactor|]))
        waitPowerSymbolAfterFactor.OutgoingTerminalEdges.Add(pow, HashSet([|waitPower|]))
        waitPower.OutgoingNonTerminalEdges.Add(startPower, HashSet([|finishPower|]))

        box

    let termBox =
        let box = RSMBox()
        let waitMultSymbol = RsmState(false, false) //:> IRsmState
        let waitPower = RsmState(false, false) //:> IRsmState

        box.AddState startTerm
        box.AddState finishTerm
        box.AddState waitMultSymbol
        box.AddState waitPower

        startTerm.OutgoingNonTerminalEdges.Add(startPower, HashSet([|finishTerm|]))
        startTerm.OutgoingNonTerminalEdges.Add(startTerm, HashSet([|waitMultSymbol|]))
        waitMultSymbol.OutgoingTerminalEdges.Add(mult, HashSet([waitPower]))
        waitPower.OutgoingNonTerminalEdges.Add(startPower, HashSet([|finishTerm|]))
        box

    let exprBox =
        let box = RSMBox()
        let waitExpr = RsmState(false, false) //:> IRsmState
        let waitPlusOrMinusAfterExpr = RsmState(false, false) //:> IRsmState
        let waitTermAfterOperation = RsmState(false, false) //:> IRsmState
        box.AddState startExpr
        box.AddState finishExpr
        box.AddState waitExpr
        box.AddState waitPlusOrMinusAfterExpr
        box.AddState waitTermAfterOperation

        startExpr.OutgoingNonTerminalEdges.Add(startTerm, HashSet([|finishExpr|]))

        startExpr.OutgoingNonTerminalEdges.Add(startExpr, HashSet([|waitPlusOrMinusAfterExpr|]))
        waitPlusOrMinusAfterExpr.OutgoingTerminalEdges.Add(plus, HashSet([|waitTermAfterOperation|]))
        waitPlusOrMinusAfterExpr.OutgoingTerminalEdges.Add(minus, HashSet([|waitTermAfterOperation|]))
        waitTermAfterOperation.OutgoingNonTerminalEdges.Add(startTerm, HashSet([|finishExpr|]))

        box

    let statementBox =
        let box = RSMBox()
        let waitEqAfterVar = RsmState(false, false) //:> IRsmState
        let waitExprAfterEq = RsmState(false, false) //:> IRsmState
        box.AddState startStatement
        box.AddState finishStatement
        box.AddState waitEqAfterVar
        box.AddState waitExprAfterEq

        startStatement.OutgoingNonTerminalEdges.Add(startVar, HashSet([|waitEqAfterVar|]))
        waitEqAfterVar.OutgoingTerminalEdges.Add(eq, HashSet([|waitExprAfterEq|]))
        waitExprAfterEq.OutgoingNonTerminalEdges.Add(startExpr, HashSet([|finishStatement|]))

        box

    let programBox =
        let box = RSMBox()
        let waitDotComma = RsmState(false, false)// :> IRsmState
        box.AddState startProgram
        box.AddState finishProgram
        box.AddState waitDotComma

        startProgram.OutgoingNonTerminalEdges.Add(startStatement, HashSet([|waitDotComma|]))
        waitDotComma.OutgoingTerminalEdges.Add(dotComma, HashSet([|finishProgram|]))
        finishProgram.OutgoingNonTerminalEdges.Add(startStatement, HashSet([|waitDotComma|]))

        box

    RSM([|numBox; varBox; termBox; factorBox; powerBox; exprBox; statementBox; programBox|], programBox)

