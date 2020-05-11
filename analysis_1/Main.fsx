

#load "DataCollection.fsx"


open System
open Deedle
open FSharp.Data
open XPlot.GoogleCharts

open DataCollection

/// To show dates in a short format instead of the
/// long .Net format
fsi.AddPrinter<DateTime>(fun dt -> dt.ToShortDateString())

/// Helper function within the pipeline
let (|-*>) (list: List<'T * 'U>) (f: 'U -> 'V option) =
    let pairOption g (a,b) =
        match (a, g b) with
        | a, Some c -> Some (a, c)
        | _, None   -> None
    list
    |> List.choose (pairOption f)


/// List of countries for the analysis
let countriesToAnalyse =
    ["FRA" ; "ESP" ; "ITA" ; "USA" ; "POR" ; "GBR" ; "NLD" ; "DE" ; "Belgium" ; "Sweden"]


/// Graph 1: Death rate
let graphDeathRate =
    countriesToAnalyse
    |> List.map (fun x -> x, x)
    |-*> (|Country|_|)
    |-*> getCovidData
    |-*> (fun ts -> Some ts.DeathRateTS)
    |-*> (Series.observations >> Some)
    |> List.unzip
    |> fun (labels,obs) -> labels, Chart.Line(data = Seq.ofList obs)
    |> fun (labels, chart) -> Chart.WithLabels labels chart

graphDeathRate
|> Chart.WithSize (1200, 700)
|> Chart.Show


/// Graph 2: Same start date defined by the function below
/// The idea is to compare how the situation evolves between
/// different countries using the same starting point, here
/// when the death rate reaches 0.01 death / 1000 Pop.
let filterRule =
    fun _ v -> v >= 0.01<Deaths/``1000 Population``>

let graphDeathRateSameStart =
    (countriesToAnalyse, countriesToAnalyse)
    ||> List.zip
    |-*> (|Country|_|)
    |-*> getCovidData
    |-*> (fun ts -> Some ts.DeathRateTS)
    |-*> Series.tryFilter (filterRule)
    |-*> (Series.indexOrdinally >> Some)
    |-*> (Series.observations >> Some)
    |> List.unzip
    |> fun (labels,obs) -> labels, Chart.Line(data = Seq.ofList obs)
    |> fun (labels, chart) -> Chart.WithLabels labels chart

graphDeathRateSameStart
|> Chart.WithSize (1200, 700)
|> Chart.Show

