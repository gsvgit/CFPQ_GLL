module CFPQ_GLL.RsmBuilder

open System.Collections.Generic
open CFPQ_GLL.Common
open CFPQ_GLL.RSM
open FSharpx.Collections

type Symbol = Terminal of string | NonTerminal of string
type Regexp =
    | Symbol of Symbol
    | Alternative of Regexp * Regexp
    //| Option of Regexp
    | Sequence of Regexp * Regexp
    | Many of Regexp
    | Empty
    | Epsilon

type Rule = Rule of string * Regexp

type Grammar = Grammar of string * List<Rule>

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
    //| Option regexp -> derive regexp symbol
    | Many regexp ->
        let newRegexp = derive regexp symbol
        match newRegexp with
        | Epsilon -> Many regexp
        | Empty -> Empty
        | _ -> Sequence (newRegexp, Many regexp)

and nullable regexp =
    match regexp with
    | Epsilon (*| Option _ *)| Many _ -> true
    | Empty | Symbol _ -> false
    | Alternative (left, right) -> nullable left || nullable right
    | Sequence (left, right) -> nullable left && nullable right

let rec getAllSymbols regexp =
    match regexp with
    | Empty | Epsilon -> []
    | Symbol x -> [x]
    | Many regexp (*| Option regexp*)-> getAllSymbols regexp
    | Alternative (left,right) | Sequence (left,right) -> getAllSymbols left @ getAllSymbols right

let buildRSMBox getTerminalFromString regexp =
    let thisEdgesMustBeAddedLater = ResizeArray()
    let box = RSMBox()
    let alphabet = HashSet (getAllSymbols regexp)
    let stateToRsmState = Dictionary<_,IRsmState>()
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
                | Terminal x -> fromRsmState.AddTerminalEdge (getTerminalFromString x, toRsmState)
                | NonTerminal x ->
                    fun getNonTerminalStartState -> fromRsmState.AddNonTerminalEdge (getNonTerminalStartState x, toRsmState)
                    |> thisEdgesMustBeAddedLater.Add

    box, thisEdgesMustBeAddedLater


let t s = Symbol (Terminal s)
let nt s = Symbol (NonTerminal s)
let ( *|* ) x y = Alternative (x,y)
let many x = Many x
let some x = Sequence (x, many x)
let (++) x y = Sequence (x,y)
let opt x = Alternative(x, Epsilon)
let literal (x:string) = x.ToCharArray() |> Array.map (string >> t) |> Array.reduce (++)
let (=>) lhs rhs =
    match lhs with
    | Symbol(NonTerminal s) -> Rule(s, rhs)
    | x -> failwithf $"Left hand side of production should be nonterminal, but %A{x} used."

let nonemptyList elem sep = elem ++ many (sep ++ elem)

let build rules =
    let nonTerminalToStartState = Dictionary<_,_>()
    let addEdges = ResizeArray()
    let terminalMapping = Dictionary()
    let mutable firstTreeTerminalId = 0<terminalSymbol>
    let getTerminalFromString terminalStr =
        let exists, terminal = terminalMapping.TryGetValue (char terminalStr)
        if exists
        then terminal
        else
            let id = firstTreeTerminalId
            terminalMapping.Add((char terminalStr),id)
            firstTreeTerminalId <- firstTreeTerminalId + 1<terminalSymbol>
            id

    let boxes =
        rules
        |> Seq.map ( fun rule ->
            match rule with
            | Rule (ntName,ntRegex) ->
                let box, _addEdges = buildRSMBox getTerminalFromString ntRegex
                nonTerminalToStartState.Add (ntName, box.StartState)
                addEdges.AddRange _addEdges
                box
            )
        |> Array.ofSeq

    addEdges |> ResizeArray.iter (fun f -> f (fun x -> nonTerminalToStartState.[x]))
    RSM(boxes, boxes[0]), terminalMapping
