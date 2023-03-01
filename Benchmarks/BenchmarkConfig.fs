module Benchmarks.BenchmarkConfig


open BenchmarkDotNet.Columns
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Exporters
open Benchmarks.BenchmarkData

type private DataInfoColumn (columnName, isNumeric, unitType, getter) =

    interface IColumn with
        member this.GetValue(_, benchmarkCase) =
            benchmarkCase.Parameters.Items
            |> Seq.find (fun x -> x.Name = "DataSource")
            |> fun x -> (x.Value :?> BenchmarkData.BenchmarkData |> getter)

        member this.GetValue(summary, benchmarkCase, _) = (this :> IColumn).GetValue (summary, benchmarkCase)
        member this.IsAvailable _ = true
        member this.IsDefault (_, _) = false
        member this.AlwaysShow = true
        member this.Category = ColumnCategory.Params
        member this.ColumnName = columnName
        member this.Id = $"{nameof(DataInfoColumn)}.{columnName}"
        member this.IsNumeric = isNumeric
        member this.Legend = $"{nameof(DataInfoColumn)}.{columnName}"
        member this.PriorityInCategory = 0
        member this.UnitType = unitType

let columns =

    let sizeColumn = DataInfoColumn("Size", true, UnitType.Size, fun x -> x.Size |> string) :> IColumn
    let errorsColumn = DataInfoColumn("Errors", false, UnitType.Dimensionless, fun x -> x.Errors |> String.concat ",") :> IColumn
    let errorsCountColumn = DataInfoColumn("ErrorsCount", true, UnitType.Size, fun x -> x.ErrorsCount |> string) :> IColumn
    let descriptorsCountColumn = DataInfoColumn("DescriptorsCount", true, UnitType.Size, fun x -> x.DescriptorsCount |> string) :> IColumn
    let weightColumn = DataInfoColumn("Weight", true, UnitType.Size, fun x -> x.Weight |> string) :> IColumn
    [| sizeColumn; descriptorsCountColumn; errorsCountColumn; weightColumn; errorsColumn |]


type BenchmarkConfig () as this =
    inherit ManualConfig ()

    do
        (this :> ManualConfig)
            .AddColumn(columns)
            .AddExporter(RPlotExporter.Default)
        |> ignore
