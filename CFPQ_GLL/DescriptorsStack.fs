module CFPQ_GLL.DescriptorsStack

open System.Collections.Generic
open CFPQ_GLL.Common
open FSharpx.Collections

type IDescriptorsStack<'token when 'token: equality> =
    abstract Push: Descriptor<'token> -> unit
    abstract Pop: unit -> Descriptor<'token>
    abstract IsEmpty: bool with get


type DefaultDescriptorsStack<'token when 'token: equality> (seq: Descriptor<'token> seq) =
    let stack = Stack<_>(seq)
    new () = DefaultDescriptorsStack<'token>(Seq.empty)

    interface IDescriptorsStack<'token> with
        member this.Push descriptor = stack.Push descriptor
        member this.Pop () = stack.Pop ()
        member this.IsEmpty with get () = stack.Count = 0


type ErrorRecoveringDescriptorsStack<'token when 'token: equality> () =
    let defaultDescriptorsStack = Stack<Descriptor<'token>>()
    let errorRecoveringDescriptorsStacks = SortedDictionary<int<weight>, Stack<Descriptor<'token>>>()

    interface IDescriptorsStack<'token> with
        member this.Push descriptor =
            let pathWeight = descriptor.Weight
            if pathWeight = 0<weight> then defaultDescriptorsStack.Push descriptor
            else
                if errorRecoveringDescriptorsStacks.ContainsKey(pathWeight) |> not
                then errorRecoveringDescriptorsStacks[pathWeight] <- Stack<_>()
                errorRecoveringDescriptorsStacks[pathWeight].Push descriptor

        member this.Pop () =
            if defaultDescriptorsStack.Count > 0 then defaultDescriptorsStack.Pop ()
            else
                let mutable enumerator = errorRecoveringDescriptorsStacks.Keys.GetEnumerator()
                let moved = enumerator.MoveNext()
                assert moved
                let currentMin = enumerator.Current
                let result = errorRecoveringDescriptorsStacks[currentMin].Pop ()
                if result.Weight > currentMin then failwith "!!!"
                if errorRecoveringDescriptorsStacks[currentMin].Count = 0
                then errorRecoveringDescriptorsStacks.Remove currentMin |> ignore
                result

        member this.IsEmpty
            with get () = defaultDescriptorsStack.Count = 0

