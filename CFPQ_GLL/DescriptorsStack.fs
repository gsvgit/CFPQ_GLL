module CFPQ_GLL.DescriptorsStack

open System.Collections
open System.Collections.Generic
open CFPQ_GLL.Common
open CFPQ_GLL.RSM
open CFPQ_GLL.GSS
open CFPQ_GLL.InputGraph
open CFPQ_GLL.SPPF
open FSharpx.Collections

type IDescriptorsStack =
    abstract Push: Descriptor -> unit
    abstract Pop: unit -> Descriptor
    abstract ContinuationCondition: unit -> bool


type DefaultDescriptorsStack () =
    let stack = Stack<_>()

    interface IDescriptorsStack with
        member this.Push descriptor = stack.Push descriptor
        member this.Pop () = stack.Pop ()
        member this.ContinuationCondition () = stack.Count > 0

type ErrorRecoveringDescriptorsStack () =
    let defaultDescriptorsStack = Stack<Descriptor>()
    let errorRecoveringDescriptorsStacks = SortedDictionary<int<edgeWeight>, Stack<Descriptor>>()

    interface IDescriptorsStack with
        member this.Push descriptor =
            let pathWeight = descriptor.PathWeight
            if pathWeight = 0<edgeWeight> then defaultDescriptorsStack.Push descriptor
            else
                if errorRecoveringDescriptorsStacks.ContainsKey(pathWeight) |> not then
                    errorRecoveringDescriptorsStacks[pathWeight] <- Stack<_>()
                errorRecoveringDescriptorsStacks[pathWeight].Push descriptor

        member this.Pop () =
            if defaultDescriptorsStack.Count > 0 then defaultDescriptorsStack.Pop ()
            else
                let currentMin = errorRecoveringDescriptorsStacks.Keys.GetEnumerator().Current
                let result = errorRecoveringDescriptorsStacks[currentMin].Pop ()
                if errorRecoveringDescriptorsStacks[currentMin].Count = 0 then
                    errorRecoveringDescriptorsStacks.Remove currentMin |> ignore
                result

        member this.ContinuationCondition () = defaultDescriptorsStack.Count > 0

