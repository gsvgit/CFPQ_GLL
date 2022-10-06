module CFPQ_GLL.RsmBuilder

open System.Collections.Generic
open CFPQ_GLL.Common

type Regexp =
    | Terminal of string
    | NonTerminal of string
    | Alternative of Regexp * Regexp
    | Option of Regexp
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
        | l,r -> Alternative(l,r)
    match regexp with
    | Empty -> Empty
    | Epsilon -> Empty
    | Terminal x 
    | NonTerminal x ->
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
    | Option regexp -> derive regexp symbol
    | Many regexp ->
        let newRegexp = derive regexp symbol
        match newRegexp with
        | Epsilon -> Many regexp
        | Empty -> Empty
        | _ -> Sequence (newRegexp, Many regexp)
        
and nullable regexp =
    match regexp with
    | Epsilon | Option _ | Many _ -> true
    | Empty | Terminal _ | NonTerminal _ -> false
    | Alternative (left, right) -> nullable left || nullable right
    | Sequence (left, right) -> nullable left && nullable right
    
let rec getAllSymbols regexp =
    match regexp with
    | Empty | Epsilon -> []
    | Terminal x | NonTerminal x -> [x]
    | Many regexp | Option regexp-> getAllSymbols regexp
    | Alternative (left,right) | Sequence (left,right) -> getAllSymbols left @ getAllSymbols right
        
let buildRSM regexp =
    let mutable firstFreeStateIndex = 0    
    let finalStates = HashSet()
    let alphabet = HashSet (getAllSymbols regexp)
    let stateToId = Dictionary()
    let edges = ResizeArray ()
    let getId state =
        if stateToId.ContainsKey state
        then stateToId[state]
        else
            let id = firstFreeStateIndex
            stateToId.Add(state, id)
            firstFreeStateIndex <- firstFreeStateIndex + 1
            id
    let startState = getId regexp            
    let statesToProcess = Stack [regexp]
    
    while statesToProcess.Count > 0 do
        let state = statesToProcess.Pop()
        if nullable state
        then finalStates.Add (getId state) |> ignore
        for symbol in alphabet do
            let newState = derive state symbol
            let fromId = getId state
            if stateToId.ContainsKey newState |> not
            then statesToProcess.Push newState
            let toId = getId newState
            edges.Add (fromId, symbol,toId)
            
    startState,finalStates,edges            
    
    
let t s = Terminal s
let nt s = NonTerminal s
let ( *|* ) x y = Alternative (x,y)
let many x = Many x
let (++) x y = Sequence (x,y)
let opt x = Option x
let literal (x:string) = x.ToCharArray() |> Array.map (string >> t) |> Array.reduce (++)
let (=>) lhs rhs = Rule(lhs, rhs)

//let build rules =
    
let d = "S" =>
            t "a" *|* t "b" ++ t "c"