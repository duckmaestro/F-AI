
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
//    Decision Tree learner. (WORK IN PROGRESS)
//

module DecisionTree


// namespaces

open System
open MathNet.Numerics.LinearAlgebra
open Classifiers


// records

type private DecisionNode = {
    FeatureId : int;
    Threshold : float;
    }

type private CutEntropy = {
    FeatureId : int;
    Threshold : float;
    Entropy : float;
    }


// private functions
let private EnumerateCandidateCuts samples featureId =
    let candidateCuts = 
        samples 
        |> Seq.map (fun s -> (s.Features.Item featureId))
        |> Seq.pairwise 
        |> Seq.filter (fun (x1,x2) -> x1 <> x2) // todo: use not approximately equal?
        |> Seq.map (fun (x1,x2) -> x1 * 0.5 + x2 * 0.5)
    candidateCuts

let private CutSampleSpace samples featureId threshold = 
    let samplesLessThan = 
        samples 
        |> Seq.filter (fun s -> (s.Features.Item featureId) < threshold)

    let samplesGreaterThan = 
        samples
        |> Seq.filter (fun s -> (s.Features.Item featureId) >= threshold)

    (samplesLessThan, samplesGreaterThan)

let private FindOptimalCutFeature samples featureId =
    let candidateCuts = EnumerateCandidateCuts samples featureId

    let subsetsByCut = 
        candidateCuts 
        |> Seq.map (fun cut -> (cut, CutSampleSpace samples featureId cut))

    let fnEntropyOfCut (s1,s2) = // by subspaces
        (MeasureEntropy s1) + (MeasureEntropy s2)

    let entropyByCut =
        subsetsByCut
        |> Seq.map (
            fun (cut, subspaces) -> cut, fnEntropyOfCut subspaces)

    let optimalCut =
        entropyByCut
        |> Seq.minBy (fun (cut,entropy) -> entropy)

    { FeatureId = featureId; Threshold = fst optimalCut; Entropy = snd optimalCut }

let private FindOptimalCut samples = 
    let dimensionality = samples |> Seq.head |> fun x -> x.Features.Count

    let optimalCutsByFeature = 
        [0 .. dimensionality-1]
        |> Seq.map (fun featureId -> FindOptimalCutFeature samples featureId)

    let optimalCut = optimalCutsByFeature |> Seq.minBy (fun cut -> cut.Entropy)

    optimalCut


// types

type DecisionTreeClassifier() = 
    interface IClassifier with
        member self.Train samples =
            ()

        member self.Classify point =
            0