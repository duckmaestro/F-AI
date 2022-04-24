
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

module FAI.Loaders.LoaderUSPS9


// namespaces

open System
open System.IO
open MathNet
open FAI


// private functions

let private ParseSample (row:String) = 
    let components = row.Split(' ')
    let componentsNum = components |> Seq.length

    if componentsNum <= 1 then
        let sample = { Features = vector [||] ; Label = Int32.MinValue }
        sample

    else
    
        let features = 
            components 
            |> Seq.take (componentsNum - 1)
            |> Seq.map (fun c -> Double.Parse(c))
            |> Seq.toArray
            |> vector

        let label =
            components
            |> Seq.skip (componentsNum - 1)
            |> Seq.map (fun c -> Int32.Parse(c))
            |> Seq.head

        let sample = { Features = features; Label = label }
        sample


// functions

let LoadFromFile filename =
    
    let fileAsString:String = File.ReadAllText filename
    let samplesAsString = fileAsString.Split '\n' |> Seq.map (fun r -> r.Trim())
    let samples =
        samplesAsString
        |> Seq.filter (fun r -> r.Length <> 0)
        |> Seq.map (fun r -> ParseSample(r))
        |> Seq.toArray

    samples
    


