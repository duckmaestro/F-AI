
module kNN


// namespaces

open System
open MathNet
open Classifiers


// records

type private SampleWithDistance = {
    Sample : SampleLabeled;
    Distance : float;
    }


// private functions

let private EuclideanDistanceSquared (v1:Vector) (v2:Vector) =
    if v1.Count = 0 || v2.Count = 0
        then failwith "one or both feature arrays empty."
    if v1.Count <> v2.Count
        then failwith "feature arrays have unmatching lengths."

    let difference = v1 - v2
    let distSquared = difference.DotProduct difference
    
    distSquared

let private EuclideanDistance (v1:Vector) (v2:Vector) =
    let distSquared = EuclideanDistanceSquared v1 v2
    let dist = System.Math.Sqrt(distSquared)
    dist

let private NearestSamples (point:Vector) (samples: seq<_>) top =
    let samplesWithDistance = 
        samples
        |> Seq.map (fun s -> { Sample = s; Distance = EuclideanDistanceSquared point s.Features; })

    let nearestSamples = 
        samplesWithDistance
        |> Seq.sortBy (fun s -> s.Distance)
        |> Seq.take top

    nearestSamples


// types

type kNNClassifier(n:int) =
    
    let mutable trainingSamples = Seq.empty<SampleLabeled>

    interface IClassifier with
        member self.Train samples =
            trainingSamples <- samples
            ()

        member self.Classify point =
            if n <= 0 then failwith "n must be odd."

            let rand = new Random()

            let nearestSamples = NearestSamples point trainingSamples n
    
            let nearestSamplesByLabel = 
                nearestSamples 
                |> Seq.groupBy (fun s -> s.Sample.Label) 
                |> Seq.map (fun (k,g) -> (k, g |> Seq.length))
                |> Seq.sortBy (fun (k,c) -> -c)

            let maxVotesPerLabel = nearestSamplesByLabel |> Seq.head |> fun (k,c) -> c
            let tyingLabels = nearestSamplesByLabel |> Seq.filter (fun (k,c) -> c = maxVotesPerLabel) |> Seq.map (fun (k,c) -> k)
            let numTyingLabels = tyingLabels |> Seq.length
            let randomLabelAmongStrongest = tyingLabels |> Seq.skip (rand.Next(0, numTyingLabels)) |> Seq.head

            randomLabelAmongStrongest 
