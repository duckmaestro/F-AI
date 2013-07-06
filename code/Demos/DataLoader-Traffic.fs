
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

module DataLoaderTraffic


// namespaces

open System
open System.IO
open System.Text
open FAI.Bayesian


// private functions

let private ParseSample (variableNames:seq<String>) (row:String) = 
    let components = row.Split(' ')
    let componentsNum = components |> Seq.length

    let values =
        components
        |> Seq.zip variableNames
        |> Seq.map
            (fun (n,t) -> 
                n,
                match t with 
                | "a0" -> 0. 
                | "a1" -> 1.
                | "a2" -> 2.
                | "a3" -> 3.
                | _ -> Real.NaN 
            )
        |> Map.ofSeq
        |> fun s -> new Observation(s)

    values


// functions

let LoadFromFile filename =
    
    let fileAsString:String = File.ReadAllText filename
    let rows = 
        fileAsString.Split '\n'
        |> Seq.map (fun r -> r.Trim ())

    let header =
        rows
        |> Seq.head
        |> fun s -> s.Split ' '
        |> Seq.toArray

    let samplesAsStrings = 
        rows
        |> Seq.skip 1

    let samples =
        samplesAsStrings
        |> Seq.filter (fun r -> r.Length <> 0)
        |> Seq.map (fun r -> ParseSample header r)
        |> Seq.toArray

    new InMemoryObservationSet(samples)
    


