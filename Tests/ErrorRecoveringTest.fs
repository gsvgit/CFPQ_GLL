module Tests.ErrorRecoveringTest


open System
open System.Collections.Generic
open CFPQ_GLL.Common
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF
open CFPQ_GLL
open Expecto
open FSharpx.Collections
open GLLTests
open Tests.InputGraph
open Tests.LinearGraphReader
open CFPQ_GLL.RsmBuilder

let reverseDict (dict: Dictionary<'a, 'b>) =
    let x = dict |> Dictionary.KeyCollection |> Seq.map (fun k -> dict[k], k)
    let result = Dictionary()
    for k, v in x do
        result.Add(k, v)
    result

let run
    (graph: InputGraph)
    (q: RSM)
    dotFileName
    =
    let startV = 0<inputGraphVertex>
    let finalV = LanguagePrimitives.Int32WithMeasure<inputGraphVertex>(graph.NumberOfVertices() - 1)

    let startVertices,mapping = graph.ToCfpqCoreGraph startV
    let finalVertices = mapping[finalV]

    let result, cnt = GLL.errorRecoveringEval finalVertices startVertices q GLL.AllPaths

    match result with
    | GLL.QueryResult.MatchedRanges _ ->
        let sppf = q.OriginalStartState.NonTerminalNodes.ToArray()
        let root = sppf |> Array.filter (fun n -> finalVertices = n.RightPosition && startVertices = n.LeftPosition) |> Array.minBy(fun n -> n.Weight)
        let weights = sppf |> Array.filter (fun n -> finalVertices = n.RightPosition && startVertices = n.LeftPosition)  |> Array.map (fun n -> n.Weight) |> Array.sort |> Array.toList

        let actual = TriplesStoredSPPF([|root|], Dictionary())
        actual.ToDot dotFileName

        let output =
            match calculateActual root with
            | a, b, c, d, e, f -> $"({a}, {b}, {c}, {d}, {e}, {f}<weight>)"

        output
    | _ -> failwith "Unexpected result."

let gllTestCase
    lazySource
    (input: string)
    expected =
    let query = lazySource()
    let graphMaker = mkLinearGraph id
    let graph = graphMaker input
    let startV = 0<inputGraphVertex>
    let finalV = LanguagePrimitives.Int32WithMeasure<inputGraphVertex>(graph.NumberOfVertices() - 1)
    testCase input <| fun () -> runErrorRecoveringGLLAndCheckResult graph startV finalV query expected

let generateTestList f (inputs: string seq) =
    inputs
    |> Seq.iteri (fun i s ->
        let query = f()
        let graphMaker = mkLinearGraph id
        printf $"""mkTest "{s}" """
        run (graphMaker s) query $"{i}.dot" |> fun s -> printfn $"%s{s}"
    )

type GllResult = int * int * int * int * int * int<weight>

type GllTestGroup (name: string,  lazyRsm: unit -> RSM ) =
    let testCases = Dictionary<string, GllResult>()
    member this.Name = name
    member this.LazyRsm = lazyRsm
    member this.TestList
        with get () = testList name ( testCases.Keys |> Seq.toList |> List.map (fun input -> gllTestCase this.LazyRsm input testCases[input]))

    member this.AddTestCase (input, expected) = if testCases.ContainsKey input |> not then testCases.Add(input, expected)
    member this.GenerateTest (input: string) (dotFile: string)=
        let expected = run (mkLinearGraph id input) (this.LazyRsm()) dotFile
        $"\"{input}\", {expected}"

    member this.RegenerateTests () =
        printfn $"List.iter {name}.AddTestCase ["
        testCases.Keys |> Seq.toList |> List.iter (fun input -> printfn $"""    {this.GenerateTest input "test.dot"}""")
        printfn "]"

let BracketStarX  =

    let src () =
        let Lst = nt "List"
        let Elem = nt "Elem"
        [
            Lst => t '[' ** Elem
            Elem => t 'x' +|+ Lst
        ] |> build [' ']

    GllTestGroup ("BracketStarX", src)

let CAStarBStar =

    let src () =
        let S = nt "S"
        [
            S => t 'c' ** many (t 'a') ** many (t 'b')
        ] |> build [' ']

    GllTestGroup ("CAStarBStar", src)

let AB =

    let src () =
        let S = nt "S"
        [
            S => t 'a' ** t 'b'
        ] |> build [' ']

    GllTestGroup ("AB", src)

let DyckLang =

    let src () =
        let S = nt "S"
        [
            S => Epsilon +|+ t '(' ** S ** t ')' ** S
        ] |> build [' ']

    GllTestGroup ("DyckLang", src)

let Ambiguous =

    let src () =
        let S = nt "S"
        [
            S => t 'a'
                 +|+ S
                 +|+ S ** S
                 +|+ S ** S ** S
        ] |> build [' ']

    GllTestGroup ("Ambiguous", src)

let MultiDyckLang =

    let src () =
        let S = nt "S"
        let S1 = nt "S1"
        let S2 = nt "S2"
        let S3 = nt "S3"
        [
            S => Epsilon +|+ S1 +|+ S2 +|+ S3
            S1 => t '(' ** S ** t ')' ** S
            S2 => t '{' ** S ** t '}' ** S
            S3 => t '[' ** S ** t ']' ** S
        ] |> build [' ']

    GllTestGroup ("MultiDyckLang", src)

let DyckLangByRegexp =

    let src () =
        let S = nt "S"
        [
            S => many(t '(' ** S ** t ')')
        ] |> build [' ']

    GllTestGroup ("DyckLangByRegexp", src)

let MultiDyckLangByRegexp =

    let src () =
        let S = nt "S"
        [
            S => many (t '(' ** S ** t ')' +|+  t '{' ** S ** t '}' +|+ t '[' ** S ** t ']')
        ] |> build [' ']

    GllTestGroup ("MultiDyckLangByRegexp", src)

let Golang = GllTestGroup ("Golang", GolangRSM.golangSrc)

let SimpleGolang =
    let src () =
        //
        let IntExpr = nt "IntExpr"
        let Statement = nt "Statement"
        let Block = nt "Block"
        let Program = nt "Program"
        [
            Program   => Block
            Block     => many Statement
            Statement => IntExpr ** t ';' +|+ t 'r' ** IntExpr ** t ';'
            IntExpr   => t '1' ** t '+' ** t '1' +|+ t '1'
        ] |> build [' ']

    GllTestGroup ("SimpleGolang", src)

let ProgramLike =
    let src () =
        let S = nt "S"
        let B = nt "B"
        let E = nt "E"
        let C = nt "C"
        [
            S => many B +|+ E
            B => t '{' ** many S ** t '}'
            E => C ** t ';'
            C => literal "aa" ** t 'b' +|+ protect (t 'c' ** t ' ' ** t 'd')
        ] |> build [' ']

    GllTestGroup ("ProgramLike", src)

let initTestCases () =

    List.iter BracketStarX.AddTestCase [
        "[ [" ,(0, 4, 4, 10, 3, 1<weight>)
        "[ [x", (0, 4, 4, 10, 3, 0<weight>)
        "[", (0, 2, 2, 4, 1, 1<weight>)
        " x", (0, 3, 2, 6, 2, 1<weight>)
        "", (0, 2, 2, 4, 1, 2<weight>)
        "[x[", (0, 3, 2, 6, 2, 1<weight>)
    ]

    List.iter CAStarBStar.AddTestCase [
        "", (0, 1, 1, 1, 0, 1<weight>)
        "cab", (0, 3, 1, 5, 2, 0<weight>)
        "caabb", (0, 5, 1, 9, 4, 0<weight>)
        "caaaba", (0, 6, 1, 11, 5, 1<weight>)
        "ab", (0, 3, 1, 5, 2, 1<weight>)
        "ccab", (0, 4, 1, 7, 3, 1<weight>)
    ]

    List.iter AB.AddTestCase [
        "", (0, 2, 1, 3, 1, 2<weight>)
        "ab", (0, 2, 1, 3, 1, 0<weight>)
        "abbbb", (0, 9, 1, 15, 8, 3<weight>)
        "ba", (0, 6, 1, 9, 4, 2<weight>)
        "a", (0, 2, 1, 3, 1, 1<weight>)
        "b", (0, 2, 1, 3, 1, 1<weight>)
        "a b ", (0, 4, 1, 7, 3, 0<weight>)
        "a b b b b ", (0, 16, 1, 29, 15, 3<weight>)
        "b   a ", (0, 7, 1, 13, 6, 2<weight>)
    ]

    List.iter DyckLang.AddTestCase [
        "", (1, 0, 1, 1, 0, 0<weight>)
        "()", (2, 2, 3, 9, 3, 0<weight>)
        "()()", (3, 4, 5, 17, 6, 0<weight>)
        "()(())", (4, 6, 7, 25, 9, 0<weight>)
        "(()())", (4, 6, 7, 25, 9, 0<weight>)
        "(", (1, 3, 2, 9, 3, 1<weight>)
        ")", (0, 1, 1, 1, 0, 1<weight>)
        "(()", (3, 5, 5, 20, 9, 1<weight>)
        "(()()", (5, 7, 8, 30, 12, 1<weight>)
    ]

    List.iter Ambiguous.AddTestCase [
        "", (0, 1, 1, 1, 0, 1<weight>)
        "a", (0, 1, 1, 1, 0, 0<weight>)
        "a ", (0, 2, 1, 3, 1, 0<weight>)
        "aa", (0, 2, 3, 5, 1, 0<weight>)
        "a a", (0, 3, 3, 7, 2, 0<weight>)
        "aaa", (0, 3, 5, 9, 2, 0<weight>)
    ]

    List.iter MultiDyckLang.AddTestCase [
        "{{[[]]}}()", (6, 10, 16, 46, 15, 0<weight>)
        "{[]}{(())()}", (7, 12, 19, 55, 18, 0<weight>)
        "{]", (2, 7, 5, 23, 11, 2<weight>)
        "[(}", (3, 26, 14, 80, 51, 3<weight>)
        "[(])", (4, 14, 11, 47, 21, 2<weight>)
    ]

    List.iter DyckLangByRegexp.AddTestCase [
        "", (1, 0, 1, 1, 0, 0<weight>)
        "()", (1, 2, 2, 6, 2, 0<weight>)
        "()()", (2, 4, 3, 13, 5, 0<weight>)
        "()(())", (2, 6, 4, 18, 7, 0<weight>)
        "(()())", (2, 6, 4, 18, 7, 0<weight>)
        "(", (1, 3, 2, 7, 2, 1<weight>)
        ")", (0, 1, 1, 1, 0, 1<weight>)
        "(()", (2, 5, 4, 16, 7, 1<weight>)
        "(()()", (3, 9, 6, 30, 16, 1<weight>)
    ]

    List.iter MultiDyckLangByRegexp.AddTestCase [
        "", (1, 0, 1, 1, 0, 0<weight>)
        "()", (1, 2, 2, 6, 2, 0<weight>)
        "()()", (2, 4, 3, 13, 5, 0<weight>)
        "()(())", (2, 6, 4, 18, 7, 0<weight>)
        "(()())", (2, 6, 4, 18, 7, 0<weight>)
        "(", (1, 3, 2, 7, 2, 1<weight>)
        ")", (0, 1, 1, 1, 0, 1<weight>)
        "(()", (2, 5, 4, 16, 7, 1<weight>)
        "(()()", (3, 9, 6, 30, 16, 1<weight>)
        "{{[[]]}}()", (2, 10, 6, 28, 11, 0<weight>)
        "{[]}{(())()}", (3, 12, 7, 35, 14, 0<weight>)
        "{]", (3, 11, 6, 27, 13, 2<weight>)
        "[(}", (3, 25, 7, 63, 44, 3<weight>)
        "[(])", (3, 17, 8, 43, 24, 2<weight>)
        "[ { [ ] ] ", (3, 16, 7, 45, 23, 1<weight>)
    ]

    List.iter Golang.AddTestCase [
        GolangRSM.functionSample, (0, 129, 59, 315, 128, 0<weight>)
        GolangRSM.expressionSample, (0, 183, 150, 514, 182, 0<weight>)
        GolangRSM.cycleSample, (0, 199, 156, 552, 198, 0<weight>)
        "func f() int {}", (2, 15, 6, 38, 16, 0<weight>)
        "fnc f() int {}", (2, 15, 6, 38, 16, 1<weight>)
        "fnc f() int {", (2, 15, 6, 38, 16, 2<weight>)
        "fnc f() {}", (2, 14, 6, 36, 15, 4<weight>)
        "unc f() int {}", (2, 15, 6, 38, 16, 1<weight>)
        "", (1, 0, 1, 1, 0, 0<weight>)
        "x", (0, 1, 1, 1, 0, 1<weight>)
        "func f() int { return 1 }", (1, 28, 17, 73, 29, 1<weight>)
        "fu\nnc f() int { return 1; } ", (1, 28, 12, 68, 28, 1<weight>)
    ]

    List.iter SimpleGolang.AddTestCase [
        "1+ ; r1 ; ", (0, 11, 6, 26, 10, 1<weight>)
        "", (1, 0, 2, 2, 0, 0<weight>)
        "1+", (3, 8, 8, 19, 8, 2<weight>)
        "r 1+;", (0, 5, 4, 12, 4, 1<weight>)
        "r;", (0, 3, 4, 8, 2, 1<weight>)
        "1 + 1;;", (0, 7, 4, 16, 6, 1<weight>)
        "rr;", (0, 8, 5, 16, 6, 2<weight>)
    ]

    List.iter ProgramLike.AddTestCase [
        "{{}{}", (4, 9, 9, 34, 18, 1<weight>)
        "{{} aa b {}}", (4, 16, 14, 51, 22, 1<weight>)
        "{{} aa b aa b ; {}}", (5, 23, 18, 70, 30, 1<weight>)
        "{{} aa b {;} aa b {}}", (6, 27, 21, 84, 37, 3<weight>)
        "{{} aa b {;} aa b ; {} } ", (6, 30, 21, 92, 43, 2<weight>)
        "{c d;}", (0, 6, 5, 15, 5, 0<weight>)
        "{c d}", (1, 6, 6, 20, 8, 1<weight>)
        "{aa b; c d}", (2, 12, 10, 37, 15, 1<weight>)
        "{c d c d;}", (2, 13, 11, 39, 16, 1<weight>)
        "{c d c d;", (3, 13, 12, 41, 17, 2<weight>)
        "c d c d;}", (2, 13, 11, 39, 16, 2<weight>)
        "c d c d;", (0, 8, 3, 17, 7, 2<weight>)
        "c d {} c d;", (5, 17, 16, 56, 24, 3<weight>)

    ]

let testGroups = [
    BracketStarX
    CAStarBStar
    AB
    DyckLang
    Ambiguous
    MultiDyckLang
    DyckLangByRegexp
    MultiDyckLangByRegexp
    Golang
]

let generateTests (testGroup: GllTestGroup) (inputs: string seq) =
    inputs |> Seq.iteri (fun i x -> printfn $"""{testGroup.GenerateTest x $"{i}.dot"}""" )

let regenerateTests (testGroup: GllTestGroup) =
    initTestCases ()
    testGroup.RegenerateTests ()

let regenerateAllTests () =
    initTestCases ()
    testGroups |> List.iter (fun g -> g.RegenerateTests())

let tests () =
    initTestCases ()
    let tests = testGroups |> List.map (fun g -> g.TestList)
    testList "Error recovering" tests |> testSequenced
