module CFPQ_GLL.DescriptorsStack

open System.Collections.Generic
open CFPQ_GLL.Common
open FSharpx.Collections

type IDescriptorsStack =
    abstract Push: Descriptor -> unit
    abstract Pop: unit -> Descriptor
    abstract IsEmpty: bool with get


type DefaultDescriptorsStack (seq: Descriptor seq) =
    let stack = Stack<_>(seq)
    new () = DefaultDescriptorsStack(Seq.empty)

    interface IDescriptorsStack with
        member this.Push descriptor = stack.Push descriptor
        member this.Pop () = stack.Pop ()
        member this.IsEmpty with get () = stack.Count = 0


type ErrorRecoveringDescriptorsStack () as this =
    let onDescriptorWeightChanged descriptor =
        printfn "!!!!"
        (this :> IDescriptorsStack).Push descriptor
    let mutable cnt = 0
    let defaultDescriptorsStack = Stack<Descriptor>()
    let errorRecoveringDescriptorsStacks = SortedDictionary<int<weight>, Stack<Descriptor>>()
    
    interface IDescriptorsStack with
        member this.Push descriptor =
            descriptor.WeightChanged.Add onDescriptorWeightChanged
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
                printfn $"Pooped from: {currentMin}"
                //if result.Weight < currentMin
                //then
                    //cnt <- cnt + 1
                    //printfn $"Current min: {currentMin}; result weight: {result.Weight}; cnt: {cnt}"
                if errorRecoveringDescriptorsStacks[currentMin].Count = 0
                then errorRecoveringDescriptorsStacks.Remove currentMin |> ignore
                result.Handled <- true
                result

        member this.IsEmpty
            with get () = defaultDescriptorsStack.Count = 0

