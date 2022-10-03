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
        RsmState(true, false) :> IRsmState,
        RsmState(true, false) :> IRsmState,
        RsmState(true, false) :> IRsmState,
        RsmState(true, false) :> IRsmState,
        RsmState(true, false)  :> IRsmState,
        RsmState(true, false) :> IRsmState,
        RsmState(true, false) :> IRsmState,
        RsmState(true, false)  :> IRsmState
    let finishNum1, finishNum2, finishVar, finishExpr, finishTerm, finishFactor, finishPower, finishStatement, finishProgram =
        RsmState(false, true) :> IRsmState,
        RsmState(false, true) :> IRsmState,
        RsmState(false, true) :> IRsmState,
        RsmState(false, true) :> IRsmState,
        RsmState(false, true) :> IRsmState,
        RsmState(false, true) :> IRsmState,
        RsmState(false, true) :> IRsmState,
        RsmState(false, true) :> IRsmState,
        RsmState(false, true) :> IRsmState

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
        let waitExprAfterOpeningBracket = RsmState(false, false) :> IRsmState
        let waitClosingBracketAfterExpr = RsmState(false, false) :> IRsmState
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
        let waitPowerSymbolAfterFactor = RsmState(false, true) :> IRsmState
        let waitPower = RsmState(false, false) :> IRsmState

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
        let waitMultSymbol = RsmState(false, false) :> IRsmState
        let waitPower = RsmState(false, false) :> IRsmState

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
        let waitExpr = RsmState(false, false) :> IRsmState
        let waitPlusOrMinusAfterExpr = RsmState(false, false) :> IRsmState
        let waitTermAfterOperation = RsmState(false, false) :> IRsmState
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
        let waitEqAfterVar = RsmState(false, false) :> IRsmState
        let waitExprAfterEq = RsmState(false, false) :> IRsmState
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
        let waitDotComma = RsmState(false, false) :> IRsmState
        box.AddState startProgram
        box.AddState finishProgram
        box.AddState waitDotComma

        startProgram.OutgoingNonTerminalEdges.Add(startStatement, HashSet([|waitDotComma|]))
        waitDotComma.OutgoingTerminalEdges.Add(dotComma, HashSet([|finishProgram|]))
        finishProgram.OutgoingNonTerminalEdges.Add(startStatement, HashSet([|waitDotComma|]))

        box

    RSM([|numBox; varBox; termBox; factorBox; powerBox; exprBox; statementBox; programBox|], programBox)

let ``Calculator RSM tests`` =

    let ``Calculator RSM tests on linear input`` =

        let makeLinearInputGraph (arr: int<terminalSymbol> array) =
            Array.mapi
                (fun i t -> TerminalEdge(
                    LanguagePrimitives.Int32WithMeasure<inputGraphVertex> i,
                    t,
                    LanguagePrimitives.Int32WithMeasure<inputGraphVertex> (i + 1)
                ))
                arr |> InputGraph

        let ``a = 1 + 2;`` =
            let testName = "a = 1 + 2;"
            testCase testName <| fun () ->
                let graph = makeLinearInputGraph [|
                    character[0]
                    eq
                    numerals[1]
                    plus
                    numerals[2]
                    dotComma
                |]
                let startV = [|0<inputGraphVertex>|]
                let q = calculatorRSM ()
                let expected =
                    let nodes = Dictionary<_,_>()
                    nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                    nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                    nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                    nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(4, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,3<rsmState>,2<inputGraphVertex>))
                    nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,3<rsmState>,4<rsmState>))
                    nodes.Add(6, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,5<rsmState>))
                    nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,3<rsmState>,5<rsmState>))
                    nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,6<rsmState>))
                    nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,3<rsmState>,6<rsmState>))
                    nodes.Add(10, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,7<rsmState>,4<inputGraphVertex>))
                    nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,7<rsmState>,8<rsmState>))
                    nodes.Add(12, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,10<terminalSymbol>,4<inputGraphVertex>))
                    nodes.Add(13, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,6<rsmState>,5<rsmState>))
                    nodes.Add(14, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,40<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(15, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,5<rsmState>,4<rsmState>))
                    nodes.Add(16, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,9<rsmState>,2<inputGraphVertex>))
                    nodes.Add(17, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,9<rsmState>,10<rsmState>))
                    nodes.Add(18, TriplesStoredSPPFNode.IntermediateNode (5<inputGraphVertex>,11<rsmState>))
                    nodes.Add(19, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,5<inputGraphVertex>,9<rsmState>,11<rsmState>))
                    nodes.Add(20, TriplesStoredSPPFNode.IntermediateNode (6<inputGraphVertex>,12<rsmState>))
                    nodes.Add(21, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,6<inputGraphVertex>,9<rsmState>,12<rsmState>))
                    nodes.Add(22, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,9<rsmState>,6<inputGraphVertex>))
                    nodes.Add(23, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,6<inputGraphVertex>,9<rsmState>,10<rsmState>))
                    nodes.Add(24, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,13<rsmState>,6<inputGraphVertex>))
                    nodes.Add(25, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,6<inputGraphVertex>,13<rsmState>,14<rsmState>))
                    nodes.Add(26, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,15<rsmState>,6<inputGraphVertex>))
                    nodes.Add(27, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,6<inputGraphVertex>,15<rsmState>,16<rsmState>))
                    nodes.Add(28, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,17<rsmState>,6<inputGraphVertex>))
                    nodes.Add(29, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,6<inputGraphVertex>,17<rsmState>,18<rsmState>))
                    nodes.Add(30, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,19<rsmState>,6<inputGraphVertex>))
                    nodes.Add(31, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,6<inputGraphVertex>,19<rsmState>,20<rsmState>))
                    nodes.Add(32, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,1<terminalSymbol>,6<inputGraphVertex>))
                    nodes.Add(33, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,5<inputGraphVertex>,12<rsmState>,11<rsmState>))
                    nodes.Add(34, TriplesStoredSPPFNode.TerminalNode (6<inputGraphVertex>,36<terminalSymbol>,5<inputGraphVertex>))
                    nodes.Add(35, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,2<inputGraphVertex>,11<rsmState>,10<rsmState>))
                    nodes.Add(36, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,13<rsmState>,2<inputGraphVertex>))
                    nodes.Add(37, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,2<inputGraphVertex>,13<rsmState>,14<rsmState>))
                    nodes.Add(38, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,15<rsmState>,2<inputGraphVertex>))
                    nodes.Add(39, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,2<inputGraphVertex>,15<rsmState>,16<rsmState>))
                    nodes.Add(40, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,17<rsmState>,2<inputGraphVertex>))
                    nodes.Add(41, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,2<inputGraphVertex>,17<rsmState>,18<rsmState>))
                    nodes.Add(42, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,19<rsmState>,2<inputGraphVertex>))
                    nodes.Add(43, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,2<inputGraphVertex>,19<rsmState>,20<rsmState>))
                    nodes.Add(44, TriplesStoredSPPFNode.TerminalNode (5<inputGraphVertex>,2<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(45, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                    nodes.Add(46, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,43<terminalSymbol>,1<inputGraphVertex>))

                    let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (6,7); (7,8); (8,9); (9,10)
                                                 (10,11); (11,12); (8,13); (13,14); (6,15); (15,16); (16,17); (17,18)
                                                 (18,19); (19,20); (20,21); (21,22); (22,23); (23,24); (24,25); (25,26)
                                                 (26,27); (27,28); (28,29); (29,30); (30,31); (31,32); (20,33); (33,34)
                                                 (18,35); (35,36); (36,37); (37,38); (38,39); (39,40); (40,41); (41,42)
                                                 (42,43); (43,44); (2,45); (45,46);|])
                    let distances = [|6<distance>|]
                    (nodes,edges,distances)
                runDefaultGLLAndCheckResult testName graph startV q expected

        let ``ab = 11 + 22;`` =
            let testName = "ab = 11 + 22;"
            testCase testName <| fun () ->
                let graph = makeLinearInputGraph [|
                    character[0]
                    character[1]
                    eq
                    numerals[1]
                    numerals[1]
                    plus
                    numerals[2]
                    numerals[2]
                    dotComma
                |]
                let startV = [|0<inputGraphVertex>|]
                let q = calculatorRSM ()
                let expected =
                    let nodes = Dictionary<_,_>()
                    nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                    nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                    nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                    nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(4, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,3<rsmState>,2<inputGraphVertex>))
                    nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,3<rsmState>,4<rsmState>))
                    nodes.Add(6, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,5<rsmState>))
                    nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,3<rsmState>,5<rsmState>))
                    nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,6<rsmState>))
                    nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,3<rsmState>,6<rsmState>))
                    nodes.Add(10, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,7<rsmState>,4<inputGraphVertex>))
                    nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,7<rsmState>,8<rsmState>))
                    nodes.Add(12, TriplesStoredSPPFNode.IntermediateNode (5<inputGraphVertex>,8<rsmState>))
                    nodes.Add(13, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,5<inputGraphVertex>,7<rsmState>,8<rsmState>))
                    nodes.Add(14, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,10<terminalSymbol>,5<inputGraphVertex>))
                    nodes.Add(15, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,4<inputGraphVertex>,8<rsmState>,8<rsmState>))
                    nodes.Add(16, TriplesStoredSPPFNode.TerminalNode (5<inputGraphVertex>,11<terminalSymbol>,4<inputGraphVertex>))
                    nodes.Add(17, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,6<rsmState>,5<rsmState>))
                    nodes.Add(18, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,40<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(19, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,5<rsmState>,4<rsmState>))
                    nodes.Add(20, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,9<rsmState>,2<inputGraphVertex>))
                    nodes.Add(21, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,9<rsmState>,10<rsmState>))
                    nodes.Add(22, TriplesStoredSPPFNode.IntermediateNode (6<inputGraphVertex>,11<rsmState>))
                    nodes.Add(23, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,6<inputGraphVertex>,9<rsmState>,11<rsmState>))
                    nodes.Add(24, TriplesStoredSPPFNode.IntermediateNode (7<inputGraphVertex>,12<rsmState>))
                    nodes.Add(25, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,7<inputGraphVertex>,9<rsmState>,12<rsmState>))
                    nodes.Add(26, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,9<rsmState>,7<inputGraphVertex>))
                    nodes.Add(27, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,7<inputGraphVertex>,9<rsmState>,10<rsmState>))
                    nodes.Add(28, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,13<rsmState>,7<inputGraphVertex>))
                    nodes.Add(29, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,7<inputGraphVertex>,13<rsmState>,14<rsmState>))
                    nodes.Add(30, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,15<rsmState>,7<inputGraphVertex>))
                    nodes.Add(31, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,7<inputGraphVertex>,15<rsmState>,16<rsmState>))
                    nodes.Add(32, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,17<rsmState>,7<inputGraphVertex>))
                    nodes.Add(33, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,7<inputGraphVertex>,17<rsmState>,18<rsmState>))
                    nodes.Add(34, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,19<rsmState>,7<inputGraphVertex>))
                    nodes.Add(35, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,7<inputGraphVertex>,19<rsmState>,20<rsmState>))
                    nodes.Add(36, TriplesStoredSPPFNode.IntermediateNode (8<inputGraphVertex>,20<rsmState>))
                    nodes.Add(37, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,8<inputGraphVertex>,19<rsmState>,20<rsmState>))
                    nodes.Add(38, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,1<terminalSymbol>,8<inputGraphVertex>))
                    nodes.Add(39, TriplesStoredSPPFNode.RangeNode (8<inputGraphVertex>,7<inputGraphVertex>,20<rsmState>,20<rsmState>))
                    nodes.Add(40, TriplesStoredSPPFNode.TerminalNode (8<inputGraphVertex>,1<terminalSymbol>,7<inputGraphVertex>))
                    nodes.Add(41, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,6<inputGraphVertex>,12<rsmState>,11<rsmState>))
                    nodes.Add(42, TriplesStoredSPPFNode.TerminalNode (7<inputGraphVertex>,36<terminalSymbol>,6<inputGraphVertex>))
                    nodes.Add(43, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,2<inputGraphVertex>,11<rsmState>,10<rsmState>))
                    nodes.Add(44, TriplesStoredSPPFNode.NonTerminalNode (6<inputGraphVertex>,13<rsmState>,2<inputGraphVertex>))
                    nodes.Add(45, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,2<inputGraphVertex>,13<rsmState>,14<rsmState>))
                    nodes.Add(46, TriplesStoredSPPFNode.NonTerminalNode (6<inputGraphVertex>,15<rsmState>,2<inputGraphVertex>))
                    nodes.Add(47, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,2<inputGraphVertex>,15<rsmState>,16<rsmState>))
                    nodes.Add(48, TriplesStoredSPPFNode.NonTerminalNode (6<inputGraphVertex>,17<rsmState>,2<inputGraphVertex>))
                    nodes.Add(49, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,2<inputGraphVertex>,17<rsmState>,18<rsmState>))
                    nodes.Add(50, TriplesStoredSPPFNode.NonTerminalNode (6<inputGraphVertex>,19<rsmState>,2<inputGraphVertex>))
                    nodes.Add(51, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,2<inputGraphVertex>,19<rsmState>,20<rsmState>))
                    nodes.Add(52, TriplesStoredSPPFNode.IntermediateNode (9<inputGraphVertex>,20<rsmState>))
                    nodes.Add(53, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,9<inputGraphVertex>,19<rsmState>,20<rsmState>))
                    nodes.Add(54, TriplesStoredSPPFNode.TerminalNode (6<inputGraphVertex>,2<terminalSymbol>,9<inputGraphVertex>))
                    nodes.Add(55, TriplesStoredSPPFNode.RangeNode (9<inputGraphVertex>,2<inputGraphVertex>,20<rsmState>,20<rsmState>))
                    nodes.Add(56, TriplesStoredSPPFNode.TerminalNode (9<inputGraphVertex>,2<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(57, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                    nodes.Add(58, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,43<terminalSymbol>,1<inputGraphVertex>))

                    let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (6,7); (7,8); (8,9); (9,10)
                                                 (10,11); (11,12); (12,13); (13,14); (12,15); (15,16); (8,17); (17,18)
                                                 (6,19); (19,20); (20,21); (21,22); (22,23); (23,24); (24,25); (25,26)
                                                 (26,27); (27,28); (28,29); (29,30); (30,31); (31,32); (32,33); (33,34)
                                                 (34,35); (35,36); (36,37); (37,38); (36,39); (39,40); (24,41); (41,42)
                                                 (22,43); (43,44); (44,45); (45,46); (46,47); (47,48); (48,49); (49,50)
                                                 (50,51); (51,52); (52,53); (53,54); (52,55); (55,56); (2,57); (57,58);|])
                    let distances = [|9<distance>|]
                    (nodes,edges,distances)
                runDefaultGLLAndCheckResult testName graph startV q expected

        let ``a = b + 2 ^ 5 * 3;`` =
            let testName = "a = b + 2 ^ 5 * 3;"
            testCase testName <| fun () ->
                let graph = makeLinearInputGraph [|
                    character[0]
                    eq
                    character[1]
                    plus
                    numerals[2]
                    pow
                    numerals[5]
                    mult
                    numerals[3]
                    dotComma
                |]
                let startV = [|0<inputGraphVertex>|]
                let q = calculatorRSM ()
                let expected =
                    let nodes = Dictionary<_,_>()
                    nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                    nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                    nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                    nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(4, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,3<rsmState>,2<inputGraphVertex>))
                    nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,3<rsmState>,4<rsmState>))
                    nodes.Add(6, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,5<rsmState>))
                    nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,3<rsmState>,5<rsmState>))
                    nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,6<rsmState>))
                    nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,3<rsmState>,6<rsmState>))
                    nodes.Add(10, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,7<rsmState>,4<inputGraphVertex>))
                    nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,7<rsmState>,8<rsmState>))
                    nodes.Add(12, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,10<terminalSymbol>,4<inputGraphVertex>))
                    nodes.Add(13, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,6<rsmState>,5<rsmState>))
                    nodes.Add(14, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,40<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(15, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,5<rsmState>,4<rsmState>))
                    nodes.Add(16, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,9<rsmState>,2<inputGraphVertex>))
                    nodes.Add(17, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,9<rsmState>,10<rsmState>))
                    nodes.Add(18, TriplesStoredSPPFNode.IntermediateNode (5<inputGraphVertex>,11<rsmState>))
                    nodes.Add(19, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,5<inputGraphVertex>,9<rsmState>,11<rsmState>))
                    nodes.Add(20, TriplesStoredSPPFNode.IntermediateNode (6<inputGraphVertex>,12<rsmState>))
                    nodes.Add(21, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,6<inputGraphVertex>,9<rsmState>,12<rsmState>))
                    nodes.Add(22, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,9<rsmState>,6<inputGraphVertex>))
                    nodes.Add(23, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,6<inputGraphVertex>,9<rsmState>,10<rsmState>))
                    nodes.Add(24, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,13<rsmState>,6<inputGraphVertex>))
                    nodes.Add(25, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,6<inputGraphVertex>,13<rsmState>,14<rsmState>))
                    nodes.Add(26, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,15<rsmState>,6<inputGraphVertex>))
                    nodes.Add(27, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,6<inputGraphVertex>,15<rsmState>,16<rsmState>))
                    nodes.Add(28, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,17<rsmState>,6<inputGraphVertex>))
                    nodes.Add(29, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,6<inputGraphVertex>,17<rsmState>,18<rsmState>))
                    nodes.Add(30, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,7<rsmState>,6<inputGraphVertex>))
                    nodes.Add(31, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,6<inputGraphVertex>,7<rsmState>,8<rsmState>))
                    nodes.Add(32, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,11<terminalSymbol>,6<inputGraphVertex>))
                    nodes.Add(33, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,5<inputGraphVertex>,12<rsmState>,11<rsmState>))
                    nodes.Add(34, TriplesStoredSPPFNode.TerminalNode (6<inputGraphVertex>,36<terminalSymbol>,5<inputGraphVertex>))
                    nodes.Add(35, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,2<inputGraphVertex>,11<rsmState>,10<rsmState>))
                    nodes.Add(36, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,13<rsmState>,2<inputGraphVertex>))
                    nodes.Add(37, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,2<inputGraphVertex>,13<rsmState>,14<rsmState>))
                    nodes.Add(38, TriplesStoredSPPFNode.IntermediateNode (7<inputGraphVertex>,19<rsmState>))
                    nodes.Add(39, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,7<inputGraphVertex>,13<rsmState>,19<rsmState>))
                    nodes.Add(40, TriplesStoredSPPFNode.IntermediateNode (8<inputGraphVertex>,20<rsmState>))
                    nodes.Add(41, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,8<inputGraphVertex>,13<rsmState>,20<rsmState>))
                    nodes.Add(42, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,13<rsmState>,8<inputGraphVertex>))
                    nodes.Add(43, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,8<inputGraphVertex>,13<rsmState>,14<rsmState>))
                    nodes.Add(44, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,15<rsmState>,8<inputGraphVertex>))
                    nodes.Add(45, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,8<inputGraphVertex>,15<rsmState>,21<rsmState>))
                    nodes.Add(46, TriplesStoredSPPFNode.IntermediateNode (9<inputGraphVertex>,22<rsmState>))
                    nodes.Add(47, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,9<inputGraphVertex>,15<rsmState>,22<rsmState>))
                    nodes.Add(48, TriplesStoredSPPFNode.IntermediateNode (10<inputGraphVertex>,16<rsmState>))
                    nodes.Add(49, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,10<inputGraphVertex>,15<rsmState>,16<rsmState>))
                    nodes.Add(50, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,17<rsmState>,10<inputGraphVertex>))
                    nodes.Add(51, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,10<inputGraphVertex>,17<rsmState>,18<rsmState>))
                    nodes.Add(52, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,23<rsmState>,10<inputGraphVertex>))
                    nodes.Add(53, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,10<inputGraphVertex>,23<rsmState>,24<rsmState>))
                    nodes.Add(54, TriplesStoredSPPFNode.TerminalNode (5<inputGraphVertex>,2<terminalSymbol>,10<inputGraphVertex>))
                    nodes.Add(55, TriplesStoredSPPFNode.RangeNode (10<inputGraphVertex>,9<inputGraphVertex>,16<rsmState>,22<rsmState>))
                    nodes.Add(56, TriplesStoredSPPFNode.TerminalNode (10<inputGraphVertex>,39<terminalSymbol>,9<inputGraphVertex>))
                    nodes.Add(57, TriplesStoredSPPFNode.RangeNode (9<inputGraphVertex>,8<inputGraphVertex>,22<rsmState>,21<rsmState>))
                    nodes.Add(58, TriplesStoredSPPFNode.NonTerminalNode (9<inputGraphVertex>,15<rsmState>,8<inputGraphVertex>))
                    nodes.Add(59, TriplesStoredSPPFNode.RangeNode (9<inputGraphVertex>,8<inputGraphVertex>,15<rsmState>,16<rsmState>))
                    nodes.Add(60, TriplesStoredSPPFNode.NonTerminalNode (9<inputGraphVertex>,17<rsmState>,8<inputGraphVertex>))
                    nodes.Add(61, TriplesStoredSPPFNode.RangeNode (9<inputGraphVertex>,8<inputGraphVertex>,17<rsmState>,18<rsmState>))
                    nodes.Add(62, TriplesStoredSPPFNode.NonTerminalNode (9<inputGraphVertex>,23<rsmState>,8<inputGraphVertex>))
                    nodes.Add(63, TriplesStoredSPPFNode.RangeNode (9<inputGraphVertex>,8<inputGraphVertex>,23<rsmState>,24<rsmState>))
                    nodes.Add(64, TriplesStoredSPPFNode.TerminalNode (9<inputGraphVertex>,5<terminalSymbol>,8<inputGraphVertex>))
                    nodes.Add(65, TriplesStoredSPPFNode.RangeNode (8<inputGraphVertex>,7<inputGraphVertex>,20<rsmState>,19<rsmState>))
                    nodes.Add(66, TriplesStoredSPPFNode.TerminalNode (8<inputGraphVertex>,38<terminalSymbol>,7<inputGraphVertex>))
                    nodes.Add(67, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,2<inputGraphVertex>,19<rsmState>,14<rsmState>))
                    nodes.Add(68, TriplesStoredSPPFNode.NonTerminalNode (7<inputGraphVertex>,15<rsmState>,2<inputGraphVertex>))
                    nodes.Add(69, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,2<inputGraphVertex>,15<rsmState>,16<rsmState>))
                    nodes.Add(70, TriplesStoredSPPFNode.NonTerminalNode (7<inputGraphVertex>,17<rsmState>,2<inputGraphVertex>))
                    nodes.Add(71, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,2<inputGraphVertex>,17<rsmState>,18<rsmState>))
                    nodes.Add(72, TriplesStoredSPPFNode.NonTerminalNode (7<inputGraphVertex>,23<rsmState>,2<inputGraphVertex>))
                    nodes.Add(73, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,2<inputGraphVertex>,23<rsmState>,24<rsmState>))
                    nodes.Add(74, TriplesStoredSPPFNode.TerminalNode (7<inputGraphVertex>,3<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(75, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                    nodes.Add(76, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,43<terminalSymbol>,1<inputGraphVertex>))

                    let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (6,7); (7,8); (8,9); (9,10)
                                                 (10,11); (11,12); (8,13); (13,14); (6,15); (15,16); (16,17); (17,18)
                                                 (18,19); (19,20); (20,21); (21,22); (22,23); (23,24); (24,25); (25,26)
                                                 (26,27); (27,28); (28,29); (29,30); (30,31); (31,32); (20,33); (33,34)
                                                 (18,35); (35,36); (36,37); (37,38); (38,39); (39,40); (40,41); (41,42)
                                                 (42,43); (43,44); (44,45); (45,46); (46,47); (47,48); (48,49); (49,50)
                                                 (50,51); (51,52); (52,53); (53,54); (48,55); (55,56); (46,57); (57,58)
                                                 (58,59); (59,60); (60,61); (61,62); (62,63); (63,64); (40,65); (65,66)
                                                 (38,67); (67,68); (68,69); (69,70); (70,71); (71,72); (72,73); (73,74)
                                                 (2,75); (75,76);|])
                    let distances = [|10<distance>|]
                    (nodes,edges,distances)
                runDefaultGLLAndCheckResult testName graph startV q expected

        let ``a = (1 + 2) ^ 5; b = 3; c = 4 * 8;`` =
            let testName = "a = (1 + 2) ^ 5; b = 3; c = 4 * 8;"
            testCase testName <| fun () ->
                let graph = makeLinearInputGraph [|
                    character[0]
                    eq
                    openingBracket
                    numerals[1]
                    plus
                    numerals[2]
                    closingBracket
                    pow
                    numerals[5]
                    dotComma
                    character[1]
                    eq
                    numerals[3]
                    dotComma
                    character[2]
                    eq
                    numerals[4]
                    mult
                    numerals[8]
                    dotComma
                |]
                let startV = [|0<inputGraphVertex>|]
                let q = calculatorRSM ()
                let expected =
                    let nodes = Dictionary<_,_>()
                    nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                    nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                    nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                    nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(4, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,3<rsmState>,2<inputGraphVertex>))
                    nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,3<rsmState>,4<rsmState>))
                    nodes.Add(6, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,5<rsmState>))
                    nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,3<rsmState>,5<rsmState>))
                    nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,6<rsmState>))
                    nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,3<rsmState>,6<rsmState>))
                    nodes.Add(10, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,7<rsmState>,4<inputGraphVertex>))
                    nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,7<rsmState>,8<rsmState>))
                    nodes.Add(12, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,10<terminalSymbol>,4<inputGraphVertex>))
                    nodes.Add(13, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,6<rsmState>,5<rsmState>))
                    nodes.Add(14, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,40<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(15, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,5<rsmState>,4<rsmState>))
                    nodes.Add(16, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,9<rsmState>,2<inputGraphVertex>))
                    nodes.Add(17, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,9<rsmState>,10<rsmState>))
                    nodes.Add(18, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,11<rsmState>,2<inputGraphVertex>))
                    nodes.Add(19, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,11<rsmState>,12<rsmState>))
                    nodes.Add(20, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,13<rsmState>,2<inputGraphVertex>))
                    nodes.Add(21, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,13<rsmState>,14<rsmState>))
                    nodes.Add(22, TriplesStoredSPPFNode.IntermediateNode (5<inputGraphVertex>,15<rsmState>))
                    nodes.Add(23, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,5<inputGraphVertex>,13<rsmState>,15<rsmState>))
                    nodes.Add(24, TriplesStoredSPPFNode.IntermediateNode (6<inputGraphVertex>,16<rsmState>))
                    nodes.Add(25, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,6<inputGraphVertex>,13<rsmState>,16<rsmState>))
                    nodes.Add(26, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,17<rsmState>,6<inputGraphVertex>))
                    nodes.Add(27, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,6<inputGraphVertex>,17<rsmState>,18<rsmState>))
                    nodes.Add(28, TriplesStoredSPPFNode.IntermediateNode (7<inputGraphVertex>,19<rsmState>))
                    nodes.Add(29, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,7<inputGraphVertex>,17<rsmState>,19<rsmState>))
                    nodes.Add(30, TriplesStoredSPPFNode.IntermediateNode (8<inputGraphVertex>,20<rsmState>))
                    nodes.Add(31, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,8<inputGraphVertex>,17<rsmState>,20<rsmState>))
                    nodes.Add(32, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,41<terminalSymbol>,8<inputGraphVertex>))
                    nodes.Add(33, TriplesStoredSPPFNode.RangeNode (8<inputGraphVertex>,7<inputGraphVertex>,20<rsmState>,19<rsmState>))
                    nodes.Add(34, TriplesStoredSPPFNode.NonTerminalNode (8<inputGraphVertex>,9<rsmState>,7<inputGraphVertex>))
                    nodes.Add(35, TriplesStoredSPPFNode.RangeNode (8<inputGraphVertex>,7<inputGraphVertex>,9<rsmState>,10<rsmState>))
                    nodes.Add(36, TriplesStoredSPPFNode.IntermediateNode (9<inputGraphVertex>,21<rsmState>))
                    nodes.Add(37, TriplesStoredSPPFNode.RangeNode (8<inputGraphVertex>,9<inputGraphVertex>,9<rsmState>,21<rsmState>))
                    nodes.Add(38, TriplesStoredSPPFNode.IntermediateNode (10<inputGraphVertex>,22<rsmState>))
                    nodes.Add(39, TriplesStoredSPPFNode.RangeNode (8<inputGraphVertex>,10<inputGraphVertex>,9<rsmState>,22<rsmState>))
                    nodes.Add(40, TriplesStoredSPPFNode.NonTerminalNode (8<inputGraphVertex>,9<rsmState>,10<inputGraphVertex>))
                    nodes.Add(41, TriplesStoredSPPFNode.RangeNode (8<inputGraphVertex>,10<inputGraphVertex>,9<rsmState>,10<rsmState>))
                    nodes.Add(42, TriplesStoredSPPFNode.NonTerminalNode (8<inputGraphVertex>,11<rsmState>,10<inputGraphVertex>))
                    nodes.Add(43, TriplesStoredSPPFNode.RangeNode (8<inputGraphVertex>,10<inputGraphVertex>,11<rsmState>,12<rsmState>))
                    nodes.Add(44, TriplesStoredSPPFNode.NonTerminalNode (8<inputGraphVertex>,13<rsmState>,10<inputGraphVertex>))
                    nodes.Add(45, TriplesStoredSPPFNode.RangeNode (8<inputGraphVertex>,10<inputGraphVertex>,13<rsmState>,16<rsmState>))
                    nodes.Add(46, TriplesStoredSPPFNode.NonTerminalNode (8<inputGraphVertex>,17<rsmState>,10<inputGraphVertex>))
                    nodes.Add(47, TriplesStoredSPPFNode.RangeNode (8<inputGraphVertex>,10<inputGraphVertex>,17<rsmState>,18<rsmState>))
                    nodes.Add(48, TriplesStoredSPPFNode.NonTerminalNode (8<inputGraphVertex>,23<rsmState>,10<inputGraphVertex>))
                    nodes.Add(49, TriplesStoredSPPFNode.RangeNode (8<inputGraphVertex>,10<inputGraphVertex>,23<rsmState>,24<rsmState>))
                    nodes.Add(50, TriplesStoredSPPFNode.TerminalNode (8<inputGraphVertex>,1<terminalSymbol>,10<inputGraphVertex>))
                    nodes.Add(51, TriplesStoredSPPFNode.RangeNode (10<inputGraphVertex>,9<inputGraphVertex>,22<rsmState>,21<rsmState>))
                    nodes.Add(52, TriplesStoredSPPFNode.TerminalNode (10<inputGraphVertex>,36<terminalSymbol>,9<inputGraphVertex>))
                    nodes.Add(53, TriplesStoredSPPFNode.RangeNode (9<inputGraphVertex>,7<inputGraphVertex>,21<rsmState>,10<rsmState>))
                    nodes.Add(54, TriplesStoredSPPFNode.NonTerminalNode (9<inputGraphVertex>,11<rsmState>,7<inputGraphVertex>))
                    nodes.Add(55, TriplesStoredSPPFNode.RangeNode (9<inputGraphVertex>,7<inputGraphVertex>,11<rsmState>,12<rsmState>))
                    nodes.Add(56, TriplesStoredSPPFNode.NonTerminalNode (9<inputGraphVertex>,13<rsmState>,7<inputGraphVertex>))
                    nodes.Add(57, TriplesStoredSPPFNode.RangeNode (9<inputGraphVertex>,7<inputGraphVertex>,13<rsmState>,16<rsmState>))
                    nodes.Add(58, TriplesStoredSPPFNode.NonTerminalNode (9<inputGraphVertex>,17<rsmState>,7<inputGraphVertex>))
                    nodes.Add(59, TriplesStoredSPPFNode.RangeNode (9<inputGraphVertex>,7<inputGraphVertex>,17<rsmState>,18<rsmState>))
                    nodes.Add(60, TriplesStoredSPPFNode.NonTerminalNode (9<inputGraphVertex>,23<rsmState>,7<inputGraphVertex>))
                    nodes.Add(61, TriplesStoredSPPFNode.RangeNode (9<inputGraphVertex>,7<inputGraphVertex>,23<rsmState>,24<rsmState>))
                    nodes.Add(62, TriplesStoredSPPFNode.TerminalNode (9<inputGraphVertex>,2<terminalSymbol>,7<inputGraphVertex>))
                    nodes.Add(63, TriplesStoredSPPFNode.RangeNode (7<inputGraphVertex>,6<inputGraphVertex>,19<rsmState>,18<rsmState>))
                    nodes.Add(64, TriplesStoredSPPFNode.TerminalNode (7<inputGraphVertex>,42<terminalSymbol>,6<inputGraphVertex>))
                    nodes.Add(65, TriplesStoredSPPFNode.RangeNode (6<inputGraphVertex>,5<inputGraphVertex>,16<rsmState>,15<rsmState>))
                    nodes.Add(66, TriplesStoredSPPFNode.TerminalNode (6<inputGraphVertex>,39<terminalSymbol>,5<inputGraphVertex>))
                    nodes.Add(67, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,2<inputGraphVertex>,15<rsmState>,14<rsmState>))
                    nodes.Add(68, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,13<rsmState>,2<inputGraphVertex>))
                    nodes.Add(69, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,2<inputGraphVertex>,13<rsmState>,16<rsmState>))
                    nodes.Add(70, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,17<rsmState>,2<inputGraphVertex>))
                    nodes.Add(71, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,2<inputGraphVertex>,17<rsmState>,18<rsmState>))
                    nodes.Add(72, TriplesStoredSPPFNode.NonTerminalNode (5<inputGraphVertex>,23<rsmState>,2<inputGraphVertex>))
                    nodes.Add(73, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,2<inputGraphVertex>,23<rsmState>,24<rsmState>))
                    nodes.Add(74, TriplesStoredSPPFNode.TerminalNode (5<inputGraphVertex>,5<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(75, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                    nodes.Add(76, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,43<terminalSymbol>,1<inputGraphVertex>))
                    nodes.Add(77, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,11<inputGraphVertex>))
                    nodes.Add(78, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,11<inputGraphVertex>,0<rsmState>,1<rsmState>))
                    nodes.Add(79, TriplesStoredSPPFNode.IntermediateNode (12<inputGraphVertex>,2<rsmState>))
                    nodes.Add(80, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,12<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(81, TriplesStoredSPPFNode.IntermediateNode (1<inputGraphVertex>,1<rsmState>))
                    nodes.Add(82, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,12<inputGraphVertex>,1<rsmState>,2<rsmState>))
                    nodes.Add(83, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,3<rsmState>,12<inputGraphVertex>))
                    nodes.Add(84, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,12<inputGraphVertex>,3<rsmState>,4<rsmState>))
                    nodes.Add(85, TriplesStoredSPPFNode.IntermediateNode (13<inputGraphVertex>,5<rsmState>))
                    nodes.Add(86, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,13<inputGraphVertex>,3<rsmState>,5<rsmState>))
                    nodes.Add(87, TriplesStoredSPPFNode.IntermediateNode (14<inputGraphVertex>,6<rsmState>))
                    nodes.Add(88, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,14<inputGraphVertex>,3<rsmState>,6<rsmState>))
                    nodes.Add(89, TriplesStoredSPPFNode.NonTerminalNode (1<inputGraphVertex>,7<rsmState>,14<inputGraphVertex>))
                    nodes.Add(90, TriplesStoredSPPFNode.RangeNode (1<inputGraphVertex>,14<inputGraphVertex>,7<rsmState>,8<rsmState>))
                    nodes.Add(91, TriplesStoredSPPFNode.TerminalNode (1<inputGraphVertex>,11<terminalSymbol>,14<inputGraphVertex>))
                    nodes.Add(92, TriplesStoredSPPFNode.RangeNode (14<inputGraphVertex>,13<inputGraphVertex>,6<rsmState>,5<rsmState>))
                    nodes.Add(93, TriplesStoredSPPFNode.TerminalNode (14<inputGraphVertex>,40<terminalSymbol>,13<inputGraphVertex>))
                    nodes.Add(94, TriplesStoredSPPFNode.RangeNode (13<inputGraphVertex>,12<inputGraphVertex>,5<rsmState>,4<rsmState>))
                    nodes.Add(95, TriplesStoredSPPFNode.NonTerminalNode (13<inputGraphVertex>,9<rsmState>,12<inputGraphVertex>))
                    nodes.Add(96, TriplesStoredSPPFNode.RangeNode (13<inputGraphVertex>,12<inputGraphVertex>,9<rsmState>,10<rsmState>))
                    nodes.Add(97, TriplesStoredSPPFNode.NonTerminalNode (13<inputGraphVertex>,11<rsmState>,12<inputGraphVertex>))
                    nodes.Add(98, TriplesStoredSPPFNode.RangeNode (13<inputGraphVertex>,12<inputGraphVertex>,11<rsmState>,12<rsmState>))
                    nodes.Add(99, TriplesStoredSPPFNode.NonTerminalNode (13<inputGraphVertex>,13<rsmState>,12<inputGraphVertex>))
                    nodes.Add(100, TriplesStoredSPPFNode.RangeNode (13<inputGraphVertex>,12<inputGraphVertex>,13<rsmState>,16<rsmState>))
                    nodes.Add(101, TriplesStoredSPPFNode.NonTerminalNode (13<inputGraphVertex>,17<rsmState>,12<inputGraphVertex>))
                    nodes.Add(102, TriplesStoredSPPFNode.RangeNode (13<inputGraphVertex>,12<inputGraphVertex>,17<rsmState>,18<rsmState>))
                    nodes.Add(103, TriplesStoredSPPFNode.NonTerminalNode (13<inputGraphVertex>,23<rsmState>,12<inputGraphVertex>))
                    nodes.Add(104, TriplesStoredSPPFNode.RangeNode (13<inputGraphVertex>,12<inputGraphVertex>,23<rsmState>,24<rsmState>))
                    nodes.Add(105, TriplesStoredSPPFNode.TerminalNode (13<inputGraphVertex>,3<terminalSymbol>,12<inputGraphVertex>))
                    nodes.Add(106, TriplesStoredSPPFNode.RangeNode (12<inputGraphVertex>,11<inputGraphVertex>,2<rsmState>,1<rsmState>))
                    nodes.Add(107, TriplesStoredSPPFNode.TerminalNode (12<inputGraphVertex>,43<terminalSymbol>,11<inputGraphVertex>))
                    nodes.Add(108, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,15<inputGraphVertex>))
                    nodes.Add(109, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,15<inputGraphVertex>,0<rsmState>,1<rsmState>))
                    nodes.Add(110, TriplesStoredSPPFNode.IntermediateNode (16<inputGraphVertex>,2<rsmState>))
                    nodes.Add(111, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,16<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(112, TriplesStoredSPPFNode.IntermediateNode (11<inputGraphVertex>,1<rsmState>))
                    nodes.Add(113, TriplesStoredSPPFNode.RangeNode (11<inputGraphVertex>,16<inputGraphVertex>,1<rsmState>,2<rsmState>))
                    nodes.Add(114, TriplesStoredSPPFNode.NonTerminalNode (11<inputGraphVertex>,3<rsmState>,16<inputGraphVertex>))
                    nodes.Add(115, TriplesStoredSPPFNode.RangeNode (11<inputGraphVertex>,16<inputGraphVertex>,3<rsmState>,4<rsmState>))
                    nodes.Add(116, TriplesStoredSPPFNode.IntermediateNode (17<inputGraphVertex>,5<rsmState>))
                    nodes.Add(117, TriplesStoredSPPFNode.RangeNode (11<inputGraphVertex>,17<inputGraphVertex>,3<rsmState>,5<rsmState>))
                    nodes.Add(118, TriplesStoredSPPFNode.IntermediateNode (18<inputGraphVertex>,6<rsmState>))
                    nodes.Add(119, TriplesStoredSPPFNode.RangeNode (11<inputGraphVertex>,18<inputGraphVertex>,3<rsmState>,6<rsmState>))
                    nodes.Add(120, TriplesStoredSPPFNode.NonTerminalNode (11<inputGraphVertex>,7<rsmState>,18<inputGraphVertex>))
                    nodes.Add(121, TriplesStoredSPPFNode.RangeNode (11<inputGraphVertex>,18<inputGraphVertex>,7<rsmState>,8<rsmState>))
                    nodes.Add(122, TriplesStoredSPPFNode.TerminalNode (11<inputGraphVertex>,12<terminalSymbol>,18<inputGraphVertex>))
                    nodes.Add(123, TriplesStoredSPPFNode.RangeNode (18<inputGraphVertex>,17<inputGraphVertex>,6<rsmState>,5<rsmState>))
                    nodes.Add(124, TriplesStoredSPPFNode.TerminalNode (18<inputGraphVertex>,40<terminalSymbol>,17<inputGraphVertex>))
                    nodes.Add(125, TriplesStoredSPPFNode.RangeNode (17<inputGraphVertex>,16<inputGraphVertex>,5<rsmState>,4<rsmState>))
                    nodes.Add(126, TriplesStoredSPPFNode.NonTerminalNode (17<inputGraphVertex>,9<rsmState>,16<inputGraphVertex>))
                    nodes.Add(127, TriplesStoredSPPFNode.RangeNode (17<inputGraphVertex>,16<inputGraphVertex>,9<rsmState>,10<rsmState>))
                    nodes.Add(128, TriplesStoredSPPFNode.NonTerminalNode (17<inputGraphVertex>,11<rsmState>,16<inputGraphVertex>))
                    nodes.Add(129, TriplesStoredSPPFNode.RangeNode (17<inputGraphVertex>,16<inputGraphVertex>,11<rsmState>,12<rsmState>))
                    nodes.Add(130, TriplesStoredSPPFNode.IntermediateNode (19<inputGraphVertex>,25<rsmState>))
                    nodes.Add(131, TriplesStoredSPPFNode.RangeNode (17<inputGraphVertex>,19<inputGraphVertex>,11<rsmState>,25<rsmState>))
                    nodes.Add(132, TriplesStoredSPPFNode.IntermediateNode (20<inputGraphVertex>,26<rsmState>))
                    nodes.Add(133, TriplesStoredSPPFNode.RangeNode (17<inputGraphVertex>,20<inputGraphVertex>,11<rsmState>,26<rsmState>))
                    nodes.Add(134, TriplesStoredSPPFNode.NonTerminalNode (17<inputGraphVertex>,11<rsmState>,20<inputGraphVertex>))
                    nodes.Add(135, TriplesStoredSPPFNode.RangeNode (17<inputGraphVertex>,20<inputGraphVertex>,11<rsmState>,12<rsmState>))
                    nodes.Add(136, TriplesStoredSPPFNode.NonTerminalNode (17<inputGraphVertex>,13<rsmState>,20<inputGraphVertex>))
                    nodes.Add(137, TriplesStoredSPPFNode.RangeNode (17<inputGraphVertex>,20<inputGraphVertex>,13<rsmState>,16<rsmState>))
                    nodes.Add(138, TriplesStoredSPPFNode.NonTerminalNode (17<inputGraphVertex>,17<rsmState>,20<inputGraphVertex>))
                    nodes.Add(139, TriplesStoredSPPFNode.RangeNode (17<inputGraphVertex>,20<inputGraphVertex>,17<rsmState>,18<rsmState>))
                    nodes.Add(140, TriplesStoredSPPFNode.NonTerminalNode (17<inputGraphVertex>,23<rsmState>,20<inputGraphVertex>))
                    nodes.Add(141, TriplesStoredSPPFNode.RangeNode (17<inputGraphVertex>,20<inputGraphVertex>,23<rsmState>,24<rsmState>))
                    nodes.Add(142, TriplesStoredSPPFNode.TerminalNode (17<inputGraphVertex>,4<terminalSymbol>,20<inputGraphVertex>))
                    nodes.Add(143, TriplesStoredSPPFNode.RangeNode (20<inputGraphVertex>,19<inputGraphVertex>,26<rsmState>,25<rsmState>))
                    nodes.Add(144, TriplesStoredSPPFNode.TerminalNode (20<inputGraphVertex>,38<terminalSymbol>,19<inputGraphVertex>))
                    nodes.Add(145, TriplesStoredSPPFNode.RangeNode (19<inputGraphVertex>,16<inputGraphVertex>,25<rsmState>,12<rsmState>))
                    nodes.Add(146, TriplesStoredSPPFNode.NonTerminalNode (19<inputGraphVertex>,13<rsmState>,16<inputGraphVertex>))
                    nodes.Add(147, TriplesStoredSPPFNode.RangeNode (19<inputGraphVertex>,16<inputGraphVertex>,13<rsmState>,16<rsmState>))
                    nodes.Add(148, TriplesStoredSPPFNode.NonTerminalNode (19<inputGraphVertex>,17<rsmState>,16<inputGraphVertex>))
                    nodes.Add(149, TriplesStoredSPPFNode.RangeNode (19<inputGraphVertex>,16<inputGraphVertex>,17<rsmState>,18<rsmState>))
                    nodes.Add(150, TriplesStoredSPPFNode.NonTerminalNode (19<inputGraphVertex>,23<rsmState>,16<inputGraphVertex>))
                    nodes.Add(151, TriplesStoredSPPFNode.RangeNode (19<inputGraphVertex>,16<inputGraphVertex>,23<rsmState>,24<rsmState>))
                    nodes.Add(152, TriplesStoredSPPFNode.TerminalNode (19<inputGraphVertex>,8<terminalSymbol>,16<inputGraphVertex>))
                    nodes.Add(153, TriplesStoredSPPFNode.RangeNode (16<inputGraphVertex>,15<inputGraphVertex>,2<rsmState>,1<rsmState>))
                    nodes.Add(154, TriplesStoredSPPFNode.TerminalNode (16<inputGraphVertex>,43<terminalSymbol>,15<inputGraphVertex>))

                    let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (6,7); (7,8); (8,9); (9,10)
                                                 (10,11); (11,12); (8,13); (13,14); (6,15); (15,16); (16,17); (17,18)
                                                 (18,19); (19,20); (20,21); (21,22); (22,23); (23,24); (24,25); (25,26)
                                                 (26,27); (27,28); (28,29); (29,30); (30,31); (31,32); (30,33); (33,34)
                                                 (34,35); (35,36); (36,37); (37,38); (38,39); (39,40); (40,41); (41,42)
                                                 (42,43); (43,44); (44,45); (45,46); (46,47); (47,48); (48,49); (49,50)
                                                 (38,51); (51,52); (36,53); (53,54); (54,55); (55,56); (56,57); (57,58)
                                                 (58,59); (59,60); (60,61); (61,62); (28,63); (63,64); (24,65); (65,66)
                                                 (22,67); (67,68); (68,69); (69,70); (70,71); (71,72); (72,73); (73,74)
                                                 (2,75); (75,76); (77,78); (78,79); (79,80); (80,81); (81,1); (81,82)
                                                 (82,83); (83,84); (84,85); (85,86); (86,87); (87,88); (88,89); (89,90)
                                                 (90,91); (87,92); (92,93); (85,94); (94,95); (95,96); (96,97); (97,98)
                                                 (98,99); (99,100); (100,101); (101,102); (102,103); (103,104); (104,105)
                                                 (79,106); (106,107); (108,109); (109,110); (110,111); (111,112); (112,78)
                                                 (112,113); (113,114); (114,115); (115,116); (116,117); (117,118); (118,119)
                                                 (119,120); (120,121); (121,122); (118,123); (123,124); (116,125); (125,126)
                                                 (126,127); (127,128); (128,129); (129,130); (130,131); (131,132); (132,133)
                                                 (133,134); (134,135); (135,136); (136,137); (137,138); (138,139); (139,140)
                                                 (140,141); (141,142); (132,143); (143,144); (130,145); (145,146); (146,147)
                                                 (147,148); (148,149); (149,150); (150,151); (151,152); (110,153); (153,154)|])
                    let distances = [|10<distance>; 14<distance>; 20<distance>|]
                    (nodes,edges,distances)
                runDefaultGLLAndCheckResult testName graph startV q expected

        testList "Calculator RSM tests on linear input" [
            ``a = 1 + 2;``
            ``ab = 11 + 22;``
            ``a = b + 2 ^ 5 * 3;``
            ``a = (1 + 2) ^ 5; b = 3; c = 4 * 8;``
        ] |> testSequenced

    let ``Calculator RSM tests on linear input with symbol deletions`` =

        let makeLinearInputGraphWithSymbolDeletions (arr: int<terminalSymbol> array) =
            Array.append
                (Array.mapi
                    (fun i t -> TerminalEdge(
                        LanguagePrimitives.Int32WithMeasure<inputGraphVertex> i,
                        t,
                        LanguagePrimitives.Int32WithMeasure<inputGraphVertex> (i + 1)
                    ))
                    arr)
                (Array.mapi
                    (fun i t -> EpsilonEdge(
                        LanguagePrimitives.Int32WithMeasure<inputGraphVertex> i,
                        LanguagePrimitives.Int32WithMeasure<inputGraphVertex> (i + 1)
                    ))
                    arr)
           |> InputGraph

        let ``a =; 1;`` =
            let testName = "a =; 1;"
            testCase testName <| fun () ->
                let graph = makeLinearInputGraphWithSymbolDeletions [|
                    character[0]
                    eq
                    dotComma
                    numerals[1]
                    dotComma
                |]
                let startV = [|0<inputGraphVertex>|]
                let q = calculatorRSM ()
                let expected =
                    let nodes = Dictionary<_,_>()
                    nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                    nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                    nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                    nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(4, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,3<rsmState>,2<inputGraphVertex>))
                    nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,3<rsmState>,4<rsmState>))
                    nodes.Add(6, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,5<rsmState>))
                    nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,3<rsmState>,5<rsmState>))
                    nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,5<rsmState>))
                    nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,3<rsmState>,5<rsmState>))
                    nodes.Add(10, TriplesStoredSPPFNode.IntermediateNode (5<inputGraphVertex>,6<rsmState>))
                    nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,5<inputGraphVertex>,3<rsmState>,6<rsmState>))
                    nodes.Add(12, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,7<rsmState>,5<inputGraphVertex>))
                    nodes.Add(13, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,5<inputGraphVertex>,7<rsmState>,8<rsmState>))
                    nodes.Add(14, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,10<terminalSymbol>,5<inputGraphVertex>))
                    nodes.Add(15, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,4<inputGraphVertex>,6<rsmState>,5<rsmState>))
                    nodes.Add(16, TriplesStoredSPPFNode.TerminalNode (5<inputGraphVertex>,40<terminalSymbol>,4<inputGraphVertex>))
                    nodes.Add(17, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,5<rsmState>,5<rsmState>))
                    nodes.Add(18, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,-1<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(19, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,5<rsmState>,4<rsmState>))
                    nodes.Add(20, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,9<rsmState>,2<inputGraphVertex>))
                    nodes.Add(21, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,9<rsmState>,10<rsmState>))
                    nodes.Add(22, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,11<rsmState>,2<inputGraphVertex>))
                    nodes.Add(23, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,11<rsmState>,12<rsmState>))
                    nodes.Add(24, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,13<rsmState>,2<inputGraphVertex>))
                    nodes.Add(25, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,13<rsmState>,14<rsmState>))
                    nodes.Add(26, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,15<rsmState>,2<inputGraphVertex>))
                    nodes.Add(27, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,15<rsmState>,16<rsmState>))
                    nodes.Add(28, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,17<rsmState>,2<inputGraphVertex>))
                    nodes.Add(29, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,17<rsmState>,18<rsmState>))
                    nodes.Add(30, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,1<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(31, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,5<rsmState>))
                    nodes.Add(32, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,2<inputGraphVertex>,5<rsmState>,4<rsmState>))
                    nodes.Add(33, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,9<rsmState>,2<inputGraphVertex>))
                    nodes.Add(34, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,2<inputGraphVertex>,9<rsmState>,10<rsmState>))
                    nodes.Add(35, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,9<rsmState>))
                    nodes.Add(36, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,9<rsmState>,9<rsmState>))
                    nodes.Add(37, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,-1<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(38, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,11<rsmState>,2<inputGraphVertex>))
                    nodes.Add(39, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,2<inputGraphVertex>,11<rsmState>,12<rsmState>))
                    nodes.Add(40, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,11<rsmState>))
                    nodes.Add(41, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,11<rsmState>,11<rsmState>))
                    nodes.Add(42, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,-1<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(43, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,13<rsmState>,2<inputGraphVertex>))
                    nodes.Add(44, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,2<inputGraphVertex>,13<rsmState>,14<rsmState>))
                    nodes.Add(45, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,13<rsmState>))
                    nodes.Add(46, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,13<rsmState>,13<rsmState>))
                    nodes.Add(47, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,-1<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(48, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,15<rsmState>,2<inputGraphVertex>))
                    nodes.Add(49, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,2<inputGraphVertex>,15<rsmState>,16<rsmState>))
                    nodes.Add(50, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,15<rsmState>))
                    nodes.Add(51, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,15<rsmState>,15<rsmState>))
                    nodes.Add(52, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,-1<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(53, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,17<rsmState>,2<inputGraphVertex>))
                    nodes.Add(54, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,2<inputGraphVertex>,17<rsmState>,18<rsmState>))
                    nodes.Add(55, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,17<rsmState>))
                    nodes.Add(56, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,17<rsmState>,17<rsmState>))
                    nodes.Add(57, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,-1<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(58, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                    nodes.Add(59, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,43<terminalSymbol>,1<inputGraphVertex>))


                    let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (6,7); (7,8); (8,9); (9,10)
                                                 (10,11); (11,12); (12,13); (13,14); (10,15); (15,16); (8,17); (17,18)
                                                 (6,19); (19,20); (20,21); (21,22); (22,23); (23,24); (24,25); (25,26)
                                                 (26,27); (27,28); (28,29); (29,30); (5,31); (31,9); (31,32); (32,33)
                                                 (33,34); (34,35); (35,36); (36,37); (35,21); (34,38); (38,39); (39,40)
                                                 (40,41); (41,42); (40,23); (39,43); (43,44); (44,45); (45,46); (46,47)
                                                 (45,25); (44,48); (48,49); (49,50); (50,51); (51,52); (50,27); (49,53)
                                                 (53,54); (54,55); (55,56); (56,57); (55,29); (2,58); (58,59); |])
                    let distances = [|5<distance>|]
                    (nodes,edges,distances)
                runDefaultGLLAndCheckResult testName graph startV q expected

        let ``a = a1;`` =
            let testName = "a = a1;"
            testCase testName <| fun () ->
                let graph = makeLinearInputGraphWithSymbolDeletions [|
                    character[0]
                    eq
                    character[0]
                    numerals[1]
                    dotComma
                |]
                let startV = [|0<inputGraphVertex>|]
                let q = calculatorRSM ()
                let expected =
                    let nodes = Dictionary<_,_>()
                    nodes.Add(0, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,0<rsmState>,1<inputGraphVertex>))
                    nodes.Add(1, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,1<inputGraphVertex>,0<rsmState>,1<rsmState>))
                    nodes.Add(2, TriplesStoredSPPFNode.IntermediateNode (2<inputGraphVertex>,2<rsmState>))
                    nodes.Add(3, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(4, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,3<rsmState>,2<inputGraphVertex>))
                    nodes.Add(5, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,2<inputGraphVertex>,3<rsmState>,4<rsmState>))
                    nodes.Add(6, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,5<rsmState>))
                    nodes.Add(7, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,3<rsmState>,5<rsmState>))
                    nodes.Add(8, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,5<rsmState>))
                    nodes.Add(9, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,4<inputGraphVertex>,3<rsmState>,5<rsmState>))
                    nodes.Add(10, TriplesStoredSPPFNode.IntermediateNode (5<inputGraphVertex>,6<rsmState>))
                    nodes.Add(11, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,5<inputGraphVertex>,3<rsmState>,6<rsmState>))
                    nodes.Add(12, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,7<rsmState>,5<inputGraphVertex>))
                    nodes.Add(13, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,5<inputGraphVertex>,7<rsmState>,8<rsmState>))
                    nodes.Add(14, TriplesStoredSPPFNode.TerminalNode (0<inputGraphVertex>,10<terminalSymbol>,5<inputGraphVertex>))
                    nodes.Add(15, TriplesStoredSPPFNode.RangeNode (5<inputGraphVertex>,4<inputGraphVertex>,6<rsmState>,5<rsmState>))
                    nodes.Add(16, TriplesStoredSPPFNode.TerminalNode (5<inputGraphVertex>,40<terminalSymbol>,4<inputGraphVertex>))
                    nodes.Add(17, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,5<rsmState>,5<rsmState>))
                    nodes.Add(18, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,-1<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(19, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,5<rsmState>,4<rsmState>))
                    nodes.Add(20, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,9<rsmState>,2<inputGraphVertex>))
                    nodes.Add(21, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,9<rsmState>,10<rsmState>))
                    nodes.Add(22, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,11<rsmState>,2<inputGraphVertex>))
                    nodes.Add(23, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,11<rsmState>,12<rsmState>))
                    nodes.Add(24, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,13<rsmState>,2<inputGraphVertex>))
                    nodes.Add(25, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,13<rsmState>,14<rsmState>))
                    nodes.Add(26, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,15<rsmState>,2<inputGraphVertex>))
                    nodes.Add(27, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,15<rsmState>,16<rsmState>))
                    nodes.Add(28, TriplesStoredSPPFNode.NonTerminalNode (3<inputGraphVertex>,17<rsmState>,2<inputGraphVertex>))
                    nodes.Add(29, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,17<rsmState>,18<rsmState>))
                    nodes.Add(30, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,1<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(31, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,5<rsmState>))
                    nodes.Add(32, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,2<inputGraphVertex>,5<rsmState>,4<rsmState>))
                    nodes.Add(33, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,9<rsmState>,2<inputGraphVertex>))
                    nodes.Add(34, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,2<inputGraphVertex>,9<rsmState>,10<rsmState>))
                    nodes.Add(35, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,9<rsmState>))
                    nodes.Add(36, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,9<rsmState>,9<rsmState>))
                    nodes.Add(37, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,-1<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(38, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,11<rsmState>,2<inputGraphVertex>))
                    nodes.Add(39, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,2<inputGraphVertex>,11<rsmState>,12<rsmState>))
                    nodes.Add(40, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,11<rsmState>))
                    nodes.Add(41, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,11<rsmState>,11<rsmState>))
                    nodes.Add(42, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,-1<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(43, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,13<rsmState>,2<inputGraphVertex>))
                    nodes.Add(44, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,2<inputGraphVertex>,13<rsmState>,14<rsmState>))
                    nodes.Add(45, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,13<rsmState>))
                    nodes.Add(46, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,13<rsmState>,13<rsmState>))
                    nodes.Add(47, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,-1<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(48, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,15<rsmState>,2<inputGraphVertex>))
                    nodes.Add(49, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,2<inputGraphVertex>,15<rsmState>,16<rsmState>))
                    nodes.Add(50, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,7<rsmState>,2<inputGraphVertex>))
                    nodes.Add(51, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,2<inputGraphVertex>,7<rsmState>,8<rsmState>))
                    nodes.Add(52, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,8<rsmState>))
                    nodes.Add(53, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,7<rsmState>,8<rsmState>))
                    nodes.Add(54, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,10<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(55, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,8<rsmState>,8<rsmState>))
                    nodes.Add(56, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(57, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,15<rsmState>))
                    nodes.Add(58, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,15<rsmState>,15<rsmState>))
                    nodes.Add(59, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,-1<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(60, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,16<rsmState>))
                    nodes.Add(61, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,15<rsmState>,16<rsmState>))
                    nodes.Add(62, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,7<rsmState>,3<inputGraphVertex>))
                    nodes.Add(63, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,16<rsmState>,16<rsmState>))
                    nodes.Add(64, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(65, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,17<rsmState>,2<inputGraphVertex>))
                    nodes.Add(66, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,2<inputGraphVertex>,17<rsmState>,18<rsmState>))
                    nodes.Add(67, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,17<rsmState>))
                    nodes.Add(68, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,17<rsmState>,17<rsmState>))
                    nodes.Add(69, TriplesStoredSPPFNode.TerminalNode (4<inputGraphVertex>,-1<terminalSymbol>,3<inputGraphVertex>))
                    nodes.Add(70, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,14<rsmState>))
                    nodes.Add(71, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,13<rsmState>,14<rsmState>))
                    nodes.Add(72, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,15<rsmState>,3<inputGraphVertex>))
                    nodes.Add(73, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,14<rsmState>,14<rsmState>))
                    nodes.Add(74, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(75, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,12<rsmState>))
                    nodes.Add(76, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,11<rsmState>,12<rsmState>))
                    nodes.Add(77, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,13<rsmState>,3<inputGraphVertex>))
                    nodes.Add(78, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,12<rsmState>,12<rsmState>))
                    nodes.Add(79, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(80, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,10<rsmState>))
                    nodes.Add(81, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,9<rsmState>,10<rsmState>))
                    nodes.Add(82, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,11<rsmState>,3<inputGraphVertex>))
                    nodes.Add(83, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,10<rsmState>,10<rsmState>))
                    nodes.Add(84, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(85, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,4<rsmState>))
                    nodes.Add(86, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,3<rsmState>,4<rsmState>))
                    nodes.Add(87, TriplesStoredSPPFNode.IntermediateNode (4<inputGraphVertex>,5<rsmState>))
                    nodes.Add(88, TriplesStoredSPPFNode.RangeNode (4<inputGraphVertex>,3<inputGraphVertex>,5<rsmState>,4<rsmState>))
                    nodes.Add(89, TriplesStoredSPPFNode.NonTerminalNode (4<inputGraphVertex>,9<rsmState>,3<inputGraphVertex>))
                    nodes.Add(90, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,4<rsmState>,4<rsmState>))
                    nodes.Add(91, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(92, TriplesStoredSPPFNode.IntermediateNode (3<inputGraphVertex>,2<rsmState>))
                    nodes.Add(93, TriplesStoredSPPFNode.RangeNode (0<inputGraphVertex>,3<inputGraphVertex>,0<rsmState>,2<rsmState>))
                    nodes.Add(94, TriplesStoredSPPFNode.NonTerminalNode (0<inputGraphVertex>,3<rsmState>,3<inputGraphVertex>))
                    nodes.Add(95, TriplesStoredSPPFNode.RangeNode (3<inputGraphVertex>,2<inputGraphVertex>,2<rsmState>,2<rsmState>))
                    nodes.Add(96, TriplesStoredSPPFNode.TerminalNode (3<inputGraphVertex>,-1<terminalSymbol>,2<inputGraphVertex>))
                    nodes.Add(97, TriplesStoredSPPFNode.RangeNode (2<inputGraphVertex>,1<inputGraphVertex>,2<rsmState>,1<rsmState>))
                    nodes.Add(98, TriplesStoredSPPFNode.TerminalNode (2<inputGraphVertex>,43<terminalSymbol>,1<inputGraphVertex>))



                    let edges = ResizeArray<_>([|(0,1); (1,2); (2,3); (3,4); (4,5); (5,6); (6,7); (7,8); (8,9); (9,10)
                                                 (10,11); (11,12); (12,13); (13,14); (10,15); (15,16); (8,17); (17,18)
                                                 (6,19); (19,20); (20,21); (21,22); (22,23); (23,24); (24,25); (25,26)
                                                 (26,27); (27,28); (28,29); (29,30); (5,31); (31,9); (31,32); (32,33)
                                                 (33,34); (34,35); (35,36); (36,37); (35,21); (34,38); (38,39); (39,40)
                                                 (40,41); (41,42); (40,23); (39,43); (43,44); (44,45); (45,46); (46,47)
                                                 (45,25); (44,48); (48,49); (49,50); (50,51); (51,52); (52,53); (53,54)
                                                 (52,55); (55,56); (49,57); (57,58); (58,59); (57,27); (49,60); (60,61)
                                                 (61,62); (62,53); (60,63); (63,64); (49,65); (65,66); (66,67); (67,68)
                                                 (68,69); (67,29); (44,70); (70,71); (71,72); (72,61); (70,73); (73,74)
                                                 (39,75); (75,76); (76,77); (77,71); (75,78); (78,79); (34,80); (80,81)
                                                 (81,82); (82,76); (80,83); (83,84); (5,85); (85,86); (86,87); (87,9)
                                                 (87,88); (88,89); (89,81); (85,90); (90,91); (3,92); (92,93); (93,94)
                                                 (94,86); (92,95); (95,96); (2,97); (97,98); |])
                    let distances = [|5<distance>|]
                    (nodes,edges,distances)
                runDefaultGLLAndCheckResult testName graph startV q expected

        testList "Calculator RSM tests on linear input with symbol deletions" [
            ``a =; 1;``
            ``a = a1;``
        ] |> testSequenced

    testList "Calculator RSM tests" [
        ``Calculator RSM tests on linear input``
        ``Calculator RSM tests on linear input with symbol deletions``
    ] |> testSequenced
