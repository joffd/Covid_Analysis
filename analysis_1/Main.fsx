

#load "DataCollection.fsx"

open DataCollection

fsi.AddPrinter<DateTime>(fun dt -> dt.ToShortDateString())

let (|-*>) (list: List<'T * 'U>) (f: 'U -> 'V option) =
    let pairOption g (a,b) =
        match (a, g b) with
        | a, Some c -> Some (a, c)
        | _, None   -> None
    list
    |> List.choose (pairOption f)

let countriesToAnalyse =
    ["FRA" ; "ESP" ; "ITA" ; "USA" ; "POR" ; "GBR" ; "NLD" ; "DE" ; "Belgium"]

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
|> Chart.Show

let filterRule =
    fun _ v -> v >= 0.005<Deaths/``1000 Population``>

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
