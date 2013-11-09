
//    This file is part of F-AI.
//
//    F-AI is free software: you can redistribute it and/or modify
//    it under the terms of the GNU Lesser General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    F-AI is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU Lesser General Public License for more details.
//
//    You should have received a copy of the GNU Lesser General Public License
//    along with F-AI.  If not, see <http://www.gnu.org/licenses/>.

module FAI.Loaders.USCensus1990


// namespaces

open System
open System.IO
open System.Text
open FAI.Bayesian


// private functions

let private ParseSample (variableNames:seq<String>) (row:String) = 
    let components = row.Split(',')
    let componentsNum = components |> Seq.length

    let values =
        components
        |> Seq.zip variableNames
        |> Seq.filter (fun (n,t) -> n <> "caseid")
        |> Seq.map
            (fun (n,t) -> 
                n,
                EventValue.Parse(t)
            )
        |> Map.ofSeq
        |> fun s -> new Observation(s)

    values


// functions

let LoadFromFile filePath =
    
    let name = System.IO.Path.GetFileNameWithoutExtension filePath
        
    let rows = 
        File.ReadLines filePath

    let header =
        rows
        |> Seq.head
        |> fun s -> s.Split ','
        |> Seq.map (fun h -> match (h.Chars 0) with 
                                            | 'i' ->  h.Substring(1)
                                            | 'd' ->  h.Substring(1)
                                            | _   -> h)
        |> Seq.toArray

    let samplesAsStrings = 
        rows
        |> Seq.skip 1
        |> Seq.take 10000

    let samples =
        samplesAsStrings
        |> Seq.filter (fun r -> r.Length <> 0)
        |> Seq.map (fun r -> ParseSample header r)
        |> Seq.toArray

    // Build spaces.
    let mutable mins = Map.empty
    let mutable maxs = Map.empty

    let header = header |> Seq.filter (fun h -> h <> "caseid") |> Seq.cache

    for s in samples do
        for h in header do
            let ifNotFound def exp = defaultArg exp def
            let value = Option.get <| s.TryValueForVariable h
            let min' = mins |> Map.tryFind h |> ifNotFound EventValue.MaxValue
            let max' = maxs |> Map.tryFind h |> ifNotFound EventValue.MinValue
            if value < min' then
                mins <- mins |> Map.add h value
            else if value > max' then
                maxs <- maxs |> Map.add h value
            ()

    let mins = mins
    let maxs = maxs

    let makeSpace variableName = 
        let min = mins |> Map.tryFind variableName |> Option.get
        let max = maxs |> Map.tryFind variableName |> Option.get
        Space.MakeIntegerSpace min max

    let allSpaces = 
        header
        |> Seq.map (fun variableName -> variableName, (makeSpace variableName) )
        |> Map.ofSeq
        
    new InMemoryObservationSet (name, allSpaces, samples, filePath)
    


