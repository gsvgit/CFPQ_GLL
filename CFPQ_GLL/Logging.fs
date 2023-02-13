module CFPQ_GLL.Logging

open System

type LogTarget =
    | SPPF
    | RSM
    | GSS
    | GLL
    | RSMBuilder
    | DescriptorStack

[<CustomComparison; CustomEquality>]
type LogLevel =
    | Info
    | Debug
    | Trace
    interface IComparable with
        member this.CompareTo(other) =
            assert (other :? LogLevel)
            let other = unbox<LogLevel> other
            match this, other with
            | Info, Info -> 0
            | Debug, Debug -> 0
            | Trace, Trace -> 0
            | Info, _ -> -1
            | Debug, Info -> 1
            | Debug, _ -> -1
            | Trace, _ -> 1
    override this.Equals(other) = (this :> IComparable).CompareTo(other) = 0
    override this.GetHashCode() = match this with | Info -> 0 | Debug -> 1 | Trace -> 2

let mutable currentTextWriter = Console.Out
let public ConfigureWriter writer = currentTextWriter <- writer

let defaultLoggerConfig =
    [SPPF; RSM; GSS; GLL; RSMBuilder; DescriptorStack]
    |> List.map (fun x -> System.Collections.Generic.KeyValuePair(x, Info))

let mutable currentTargets = System.Collections.Generic.Dictionary<LogTarget, LogLevel>(defaultLoggerConfig)
let public AddTarget = currentTargets.Add
let public AddTargets = currentTargets.Add >> ignore |> List.iter
let public ChangeTargetLevel logTarget logLevel = currentTargets[logTarget] <- logLevel

let formatAsLog (target: LogTarget) message =
    $"[%A{target}] [%A{DateTime.Now}] %s{message}"

let private writeLineString (target: LogTarget) (logLevel: LogLevel) message =
    let exist = currentTargets.ContainsKey(target)
    if exist then
        let lvl = currentTargets[target]
        if lvl >= logLevel then
            currentTextWriter.WriteLine(formatAsLog target message)
            currentTextWriter.Flush()

let public printLog logTarget logLevel format =
    Printf.ksprintf (writeLineString logTarget logLevel) format
