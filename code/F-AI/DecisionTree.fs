
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
open MathNet
open Primitives
open Classifiers


// records

type private DecisionNode = {
    FeatureId : int;
    Threshold : float;
    LabelLT : int option;
    LabelGT : int option;
    ChildLT : DecisionNode option;
    ChildGT : DecisionNode option;
    }

type private CutEntropy = {
    FeatureId : int;
    Threshold : float;
    EntropyLT : float;
    EntropyGT : float;
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

    let entropyByCut =
        subsetsByCut
        |> Seq.map (
            fun (cut, subspaces) -> cut, (MeasureEntropy (fst subspaces)), (MeasureEntropy (snd subspaces)))

    let optimalCut =
        entropyByCut
        |> Seq.minBy (fun (cut,entropyLT,entropyGT) -> entropyLT + entropyGT)


    optimalCut |> fun(cut,entropyLT,entropyGT) ->
        { FeatureId = featureId; Threshold = cut; EntropyLT = entropyLT; EntropyGT = entropyGT; }

let private FindOptimalCut samples = 
    let dimensionality = samples |> Seq.head |> fun x -> x.Features.Count

    let optimalCutsByFeature = 
        [0 .. dimensionality-1]
        |> Seq.map (fun featureId -> FindOptimalCutFeature samples featureId)

    let optimalCut = optimalCutsByFeature |> Seq.minBy (fun cut -> cut.EntropyLT + cut.EntropyGT)

    optimalCut

let rec private BuildDecisionNode samples currDepth maxDepth : DecisionNode =
    let optimalCut = FindOptimalCut samples
    let featureId = optimalCut.FeatureId
    let threshold = optimalCut.Threshold
    
    let subsetLT = 
        samples 
        |> Seq.filter (fun s -> s.Features.Item(featureId) < threshold)
    let subsetGT =
        samples 
        |> Seq.filter (fun s -> s.Features.Item(featureId) >= threshold)
        
    let fnMajorityLabel samples2 =
        samples2 
        |> Seq.groupBy (fun s -> s.Label)
        |> Seq.map (fun (k,s) -> k, (Seq.length s))
        |> Seq.maxBy (fun (k,s) -> s)
        |> fun (k,s) -> k

    let fnFirstLabel samples2 =
        samples 
        |> Seq.head
        |> fun s -> s.Label

    let labelLT = 
        match optimalCut.EntropyLT with
        | 0.0 -> Some(fnFirstLabel subsetLT)
        | _ -> None
    let labelGT = 
        match optimalCut.EntropyGT with
        | 0.0 -> Some(fnFirstLabel subsetGT)
        | _ -> None

    let childLT = 
        match labelLT with
        | None -> Some(BuildDecisionNode subsetLT (currDepth+1) maxDepth)
        | _ -> None
    let childGT = 
        match labelGT with
        | None -> Some(BuildDecisionNode subsetGT (currDepth+1) maxDepth)
        | _ -> None


    if currDepth+1 = maxDepth then 
        {
            DecisionNode.FeatureId = featureId;
            DecisionNode.Threshold = threshold;
            DecisionNode.LabelLT = Some(fnMajorityLabel subsetLT);
            DecisionNode.LabelGT = Some(fnMajorityLabel subsetGT);
            DecisionNode.ChildLT = None;
            DecisionNode.ChildGT = None;
        }
    else
        {
            FeatureId = featureId;
            Threshold = threshold;
            LabelLT = labelLT;
            LabelGT = labelGT;
            ChildLT = childLT;
            ChildGT = childGT;
        }   



// types

type DecisionTreeClassifier(maxDepth) = 

    let mutable rootNode = { FeatureId = -1; Threshold = 0.0; LabelLT = None; LabelGT = None; ChildLT = None; ChildGT = None; }

    interface IClassifier with
        member self.Train samples =
            rootNode <- BuildDecisionNode samples 0 maxDepth
            ()

        member self.Classify point =
            if rootNode.FeatureId = -1 then failwith "Classifier not trained."
            
            let mutable node = rootNode
            let mutable label = Option<int>.None
            while label.IsNone do
                let isGreaterThan = (point.Item(node.FeatureId) >= node.Threshold)

                if isGreaterThan then
                    if node.LabelGT.IsSome then
                        label <- node.LabelGT
                    else
                        node <- node.ChildGT.Value
                else
                    if node.LabelLT.IsSome then
                        label <- node.LabelLT
                    else node <- node.ChildLT.Value

            label.Value