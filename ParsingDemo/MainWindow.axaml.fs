namespace ParsingDemo

open System.Diagnostics
open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.Markup.Xaml
open CFPQ_GLL
open CFPQ_GLL.Common
open CFPQ_GLL.SPPF
open Tests

type MainWindow () as this = 
    inherit Window ()
    
    let defaultText = """func f(x int, y int) int {
    return x - y;
}
"""        
    
    let runProc filename args startDir = 
        let procStartInfo = 
            ProcessStartInfo(
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                FileName = filename,
                Arguments = args
            )
        match startDir with | Some d -> procStartInfo.WorkingDirectory <- d | _ -> ()

        let outputs = System.Collections.Generic.List<string>()
        let errors = System.Collections.Generic.List<string>()
        let outputHandler f (_sender:obj) (args:DataReceivedEventArgs) = f args.Data
        let p = new Process(StartInfo = procStartInfo)
        p.OutputDataReceived.AddHandler(DataReceivedEventHandler (outputHandler outputs.Add))
        p.ErrorDataReceived.AddHandler(DataReceivedEventHandler (outputHandler errors.Add))
        let started = 
            try
                p.Start()
            with | ex ->
                ex.Data.Add("filename", filename)
                reraise()
        if not started then
            failwithf "Failed to start process %s" filename
        printfn "Started %s with pid %i" p.ProcessName p.Id
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        p.WaitForExit()
        //let cleanOut l = l |> Seq.filter (fun o -> o |> not)
        //cleanOut outputs,cleanOut errors
        
    let rsm = GolangRSM.golangRSM ()
    
    let mutable inputCodeTextBox = Unchecked.defaultof<TextBox>
    let mutable parseTreeImage = Unchecked.defaultof<Image>

    do this.InitializeComponent()

    member private this.InitializeComponent() =
#if DEBUG
        this.AttachDevTools()
#endif
        AvaloniaXamlLoader.Load(this)
        inputCodeTextBox <- this.FindControl<TextBox>("InputCodeTextBox")
        inputCodeTextBox.Text <- defaultText
        parseTreeImage <- this.FindControl<Image>("ParseTreeImage")
        
    member this.OnParseButtonClick(sender: obj,e: RoutedEventArgs) =
        //let image = new Avalonia.Media.Imaging.Bitmap("/home/gsv/Downloads/nick-gavrilov-F-rvSJl6qI0-unsplash.jpg");
        //parseTreeImage.Source <- image
        let input = LinearGraphReader.mkLinearGraph id inputCodeTextBox.Text
        let startV = 0<inputGraphVertex>
        let finalV = LanguagePrimitives.Int32WithMeasure<inputGraphVertex>(input.NumberOfVertices() - 1)
        let startVertices,mapping = input.ToCfpqCoreGraph startV
        let finalVertices = mapping[finalV]
        let result = GLL.errorRecoveringEval finalVertices startVertices rsm GLL.AllPaths
        match result with
        | GLL.QueryResult.MatchedRanges _ ->
            let sppf = rsm.OriginalStartState.NonTerminalNodes.ToArray()
            let root = sppf |> Array.filter (fun n -> finalVertices = n.RightPosition && startVertices = n.LeftPosition) |> Array.minBy(fun n -> n.Weight)
            //let weights = sppf |> Array.filter (fun n -> finalVertices = n.RightPosition && startVertices = n.LeftPosition)  |> Array.map (fun n -> n.Weight) |> Array.sort |> Array.toList
            

            let actual = TriplesStoredSPPF([|root|], System.Collections.Generic.Dictionary())
            actual.ToDot "tmp.dot"
            let _ = runProc "dot" "tmp.dot -Tpng -O" None
            
            let image = new Avalonia.Media.Imaging.Bitmap("tmp.dot.png");
            parseTreeImage.Source <- image
        //let x = inputCodeTextBox.Text <- "dsaasdasdasd"
        ()
