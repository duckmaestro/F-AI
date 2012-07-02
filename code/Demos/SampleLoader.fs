
module SampleLoader


// namespaces

open System
open System.IO
open System.Text
open MathNet
open Primitives
open Classifiers


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
    


