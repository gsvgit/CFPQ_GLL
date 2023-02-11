namespace ParsingDemo.ViewModels

open ReactiveUI

type MainWindowViewModel() =
    inherit ViewModelBase()
    let mutable textToParse = ""
    
    let onParseButtonClicked =
        ReactiveCommand.Create<_>(fun x ->
            failwith $"{textToParse}")
    member this.ParseButtonClicked with get () = onParseButtonClicked
        
    member this.TextToParse
        with get () = textToParse
        and set (v:string) =
            textToParse <-v