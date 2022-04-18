﻿
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

module FAI.Loaders.TrafficLoader


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
                | "a0" -> 0.f 
                | "a1" -> 1.f
                | "a2" -> 2.f
                | "a3" -> 3.f
                | _ -> EventValue.NaN 
            )
        |> Map.ofSeq
        |> fun s -> new Observation(s)

    values


// functions

let LoadFromFile filePath =
    
    let name = System.IO.Path.GetFileNameWithoutExtension(filePath:string)
    
    let fileAsString:String = File.ReadAllText filePath
    
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

    let defaultSpace = 
        Space.Discrete 
            (Map.ofList [ 0.f,"none" ; 1.f,"light" ; 2.f,"medium" ; 3.f,"heavy" ])

    let allSpaces = 
        header
        |> Seq.map (fun variableName -> variableName,defaultSpace)
        |> Map.ofSeq
        
    new InMemoryObservationSet (name, allSpaces, samples, filePath)
    


