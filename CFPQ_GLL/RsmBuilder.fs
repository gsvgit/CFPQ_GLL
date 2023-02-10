module CFPQ_GLL.RsmBuilder

open System.Collections.Generic
open CFPQ_GLL.Common
open CFPQ_GLL.RSM
open FSharpx.Collections

type Symbol = Terminal of char | NonTerminal of string
type Regexp =
    | Symbol of Symbol
    | Alternative of Regexp * Regexp
    | Sequence of Regexp * Regexp
    | Many of Regexp
    | Empty
    | Epsilon

let rec derive regexp symbol =
    let mkAlternative l r =
        match l,r with
        | Empty,Empty -> Empty
        | Empty,x | x,Empty -> x
        | Alternative (l,r), x
        | x, Alternative (l,r) when l = x || r = x -> Alternative (l,r)
        | l,r -> if l = r then l else Alternative(l,r)
    match regexp with
    | Empty -> Empty
    | Epsilon -> Empty
    | Symbol x ->
        if x = symbol
        then Epsilon
        else Empty
    | Sequence (hd, tl) ->
        let newHead = derive hd symbol
        let headIsNullable = nullable hd

        match newHead,headIsNullable  with
        | Empty, false -> Empty
        | Epsilon, false -> tl
        | Empty, true -> derive tl symbol
        | Epsilon, true -> mkAlternative tl (derive tl symbol)
        | x, false -> Sequence (x,tl)
        | x, true -> mkAlternative (Sequence (x,tl)) (derive tl symbol)

    | Alternative (left,right) -> mkAlternative (derive left symbol)  (derive right symbol)
    | Many regexp ->
        let newRegexp = derive regexp symbol
        match newRegexp with
        | Epsilon -> Many regexp
        | Empty -> Empty
        | _ -> Sequence (newRegexp, Many regexp)

and nullable regexp =
    match regexp with
    | Epsilon | Many _ -> true
    | Empty | Symbol _ -> false
    | Alternative (left, right) -> nullable left || nullable right
    | Sequence (left, right) -> nullable left && nullable right

let rec getAllSymbols regexp =
    match regexp with
    | Empty | Epsilon -> []
    | Symbol x -> [x]
    | Many regexp -> getAllSymbols regexp
    | Alternative (left,right) | Sequence (left,right) -> getAllSymbols left @ getAllSymbols right

let buildRSMBox ntName regexp =
    let thisEdgesMustBeAddedLater = ResizeArray()
    let box = RSMBox(NonterminalBase ntName)
    let alphabet = HashSet (getAllSymbols regexp)
    let stateToRsmState = Dictionary<_,RsmState>()
    let getRsmState state isStart isFinal =
        if stateToRsmState.ContainsKey state
        then stateToRsmState[state]
        else
            let rsmState = RsmState(isStart, isFinal)
            box.AddState rsmState
            stateToRsmState.Add(state, rsmState)
            rsmState
    let startState = getRsmState regexp true (nullable regexp)
    let statesToProcess = Stack [regexp]

    while statesToProcess.Count > 0 do
        let state = statesToProcess.Pop()
        for symbol in alphabet do
            let newState = derive state symbol
            match newState with
            | Empty -> ()
            | _ ->
                if stateToRsmState.ContainsKey newState |> not
                then statesToProcess.Push newState
                let toRsmState = getRsmState newState false (nullable newState)
                let fromRsmState = stateToRsmState[state]
                match symbol with
                | Terminal x -> fromRsmState.AddTerminalEdge (Char x, toRsmState)
                | NonTerminal x ->
                    fun getNonTerminalStartState -> fromRsmState.AddNonTerminalEdge (getNonTerminalStartState x, toRsmState)
                    |> thisEdgesMustBeAddedLater.Add

    box, thisEdgesMustBeAddedLater

type RegexpWithLayoutConfig =
    | NoLayout of RegexpWithLayoutConfig
    | Symbol of Symbol
    | Alternative of RegexpWithLayoutConfig * RegexpWithLayoutConfig
    | Sequence of RegexpWithLayoutConfig * RegexpWithLayoutConfig
    | Many of RegexpWithLayoutConfig
    | Empty
    | Epsilon

let rec private regexpId (regexp: RegexpWithLayoutConfig): Regexp =
    match regexp with
    | NoLayout x -> regexpId x
    | Symbol x -> Regexp.Symbol x
    | Alternative (left,right) -> Regexp.Alternative (regexpId left, regexpId right)
    | Sequence (left,right) -> Regexp.Sequence (regexpId left, regexpId right)
    | Many x -> Regexp.Many (regexpId x)
    | Empty -> Regexp.Empty
    | Epsilon -> Regexp.Epsilon

type Rule = Rule of string * RegexpWithLayoutConfig

type Grammar = Grammar of string * List<Rule>

let t s = Symbol (Terminal s)
let nt s = Symbol (NonTerminal s)
let ( +|+ ) x y = Alternative (x,y)
let many x = Many x
let some x = Sequence (x, many x)
let ( ** ) x y = Sequence (x,y)
let opt x = Alternative(x, Epsilon)
let literal (x:string) = NoLayout (x.ToCharArray() |> Array.map (Terminal >> Symbol) |> Array.reduce (fun x y -> Sequence (x,y)))
let protect (x: RegexpWithLayoutConfig) = NoLayout x
let (=>) lhs rhs =
    match lhs with
    | Symbol(NonTerminal s) -> Rule(s, rhs)
    | x -> failwithf $"Left hand side of production should be nonterminal, but %A{x} used."

let nonemptyList elem sep = elem ** many (sep ** elem)
let list elem sep = Alternative (nonemptyList elem sep, Epsilon)

let addLayout regexp layoutSymbols =
    match layoutSymbols with
    | [] -> regexpId regexp
    | _ ->
        let layoutRegexp = layoutSymbols |> List.map (Terminal >> Regexp.Symbol) |> List.reduce (fun x y -> Regexp.Alternative(x, y)) |> Regexp.Many
        let rec addLayout regexp =
            match regexp with
            | NoLayout x -> Regexp.Sequence (regexpId x, layoutRegexp)
            | Symbol x ->
                match x with
                | Terminal _ -> Regexp.Sequence (Regexp.Symbol x, layoutRegexp)
                | NonTerminal _ -> Regexp.Symbol x
            | Alternative (left,right) -> Regexp.Alternative (addLayout left, addLayout right)
            | Sequence (left,right) -> Regexp.Sequence (addLayout left, addLayout right)
            | Many x -> Regexp.Many (addLayout x)
            | Empty -> Regexp.Empty
            | Epsilon -> Regexp.Epsilon
        addLayout regexp

let build layoutSymbols rules =

    let nonTerminalToStartState = Dictionary<_,_>()
    let addEdges = ResizeArray()
  
    let boxes =
        rules
        |> Seq.map (fun rule ->
            match rule with
            | Rule (ntName,ntRegex) ->
                let regexp = addLayout ntRegex layoutSymbols
                let box, _addEdges = buildRSMBox ntName regexp
                nonTerminalToStartState.Add (ntName, box.StartState)
                addEdges.AddRange _addEdges
                box
            )
        |> Array.ofSeq

    addEdges |> ResizeArray.iter (fun f -> f (fun x -> nonTerminalToStartState.[x]))

    RSM(boxes, boxes[0])
