
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



//
//    k-NN learner.
//

module kNN


// namespaces

open System
open MathNet
open Primitives
open Classifiers


// records

type private SampleWithDistance = {
    Sample : Sample;
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
    
    let mutable trainingSamples = Seq.empty<Sample>

    interface IClassifier with
        member self.Train samples =
            trainingSamples <- samples
            ()

        member self.Classify sample =
            if n <= 0 then failwith "n must be odd."

            let rand = new Random()

            let nearestSamples = NearestSamples sample.Features trainingSamples n
    
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
