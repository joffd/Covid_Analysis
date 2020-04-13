#I "../packages"
#r "FSharp.Data/lib/netstandard2.0/FSharp.Data.dll"
#r "Deedle/lib/netstandard2.0/Deedle.dll"
#r "Google.DataTable.Net.Wrapper/lib/netstandard2.0/Google.DataTable.Net.Wrapper.dll"
#r "XPlot.GoogleCharts/lib/netstandard2.0/XPlot.GoogleCharts.dll"
#r "XPlot.Plotly/lib/netstandard2.0/XPlot.Plotly.dll"
#r "FSharp.Data/lib/netstandard2.0/FSharp.Data.dll"
#r "Newtonsoft.Json/lib/netstandard2.0/Newtonsoft.Json.dll"


open System
open Deedle
open FSharp.Data
open XPlot.GoogleCharts


[<Literal>]
let CsvFile = "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
[<Literal>]
let Schema = "date,int,int,int,float,float,string,string,string,"
[<Literal>]
let Culture = "en-HK"


type [<Measure>] ``1000 Population``
type [<Measure>] Deaths
type [<Measure>] Cases

type Country = {
    Alpha2Code: string
    Alpha3Code: string
    Name: string
    Population: float<``1000 Population``>
}

type CovidTS = {
    Country : Country
    DeathTotal : float<Deaths>
    CaseTotal : float<Cases>
    DeathTS : Series<DateTime, float<Deaths>>
    DeathRateTS :  Series<DateTime, float<Deaths/``1000 Population``>>
    CaseTS :  Series<DateTime, float<Cases>>
    CaseRateTS :  Series<DateTime, float<Cases/``1000 Population``>>
}


module Series =
    let tryFindFirst (predicate: 'K -> 'V -> bool) (series: Series<'K,'V>) =    
        let filtered = Series.filter predicate series
        match (filtered.IsEmpty) with
        | true  -> None
        | false -> (Series.firstKey filtered, Series.firstValue filtered)
                   |> Some

    let tryFilter (predicate: 'K -> 'V -> bool) (series: Series<'K,'V>) =    
        let filtered = Series.filter predicate series
        match (filtered.IsEmpty) with
        | true  -> None
        | false -> Some filtered


module DataCollection =
 
    let (|Date|_|) (str: string) =
        match (DateTime.TryParse(str)) with
        | (true, dt) -> Some dt
        | _ -> None




    type CovidData = FSharp.Data.CsvProvider<CsvFile, Schema = Schema, Culture = "en-HK">
    let covidRows = 
        CovidData.Load(CsvFile).Rows
        |> Seq.filter (fun row -> row.PopData2018.GetValueOrDefault(0) > 0)

    let countries =
        covidRows
        |> Seq.distinctBy (fun row -> row.CountriesAndTerritories)
        |> Seq.map (fun row ->
                    {
                        Alpha2Code = row.GeoId
                        Alpha3Code = row.CountryterritoryCode
                        Name = row.CountriesAndTerritories
                        Population = 
                            row.PopData2018.GetValueOrDefault(0)
                            |> float
                            |> (*) 0.001<``1000 Population``>
                    } )
        |> Seq.toArray

    let prepareData (country: Country) =
        covidRows
        |> Seq.filter (fun row -> row.CountryterritoryCode = country.Alpha3Code)
        |> Seq.sortBy (fun row -> row.DateRep)

    let getDeathNbTimeSeries (country: Country) =
        country
        |> prepareData
        |> Seq.map (fun row -> row.DateRep, row.Deaths * 1.0<Deaths>)
        |> Series.ofObservations

    let getDeathRateTimeSeries (country: Country) =
        country
        |> getDeathNbTimeSeries
        |> Stats.expandingSum
        |> Series.mapValues (fun death -> death * 1.0<Deaths> / country.Population)

    let getCaseNbTimeSeries (country: Country) =
        country
        |> prepareData
        |> Seq.map (fun row -> row.DateRep, row.Cases * 1.0<Cases>)
        |> Series.ofObservations

    let getCaseRateTimeSeries (country: Country) =
        country
        |> getCaseNbTimeSeries
        |> Stats.expandingSum
        |> Series.mapValues (fun case -> case * 1.0<Cases> / country.Population)


    let (|Country|_|) (str: string) =
        countries
        |> (Array.tryFind (fun c -> c.Alpha2Code = str || c.Alpha3Code = str || c.Name = str))

    let calcTimeSerie (country: Country)=
        let deathTS = getDeathNbTimeSeries country
        let caseTS = getCaseNbTimeSeries country 
        
        {
            Country = country
            DeathTotal = (deathTS |> Stats.sum) * 1.<Deaths>
            CaseTotal = (caseTS |> Stats.sum) * 1.<Cases>
            DeathTS = deathTS
            DeathRateTS = getDeathRateTimeSeries country
            CaseTS = caseTS
            CaseRateTS = getCaseRateTimeSeries country
        }

    let covidData =
        countries
        |> Array.Parallel.map calcTimeSerie

    let getCovidData (country: Country) =
        covidData
        |> Array.tryFind (fun c -> c.Country = country)