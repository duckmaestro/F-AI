
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
//    Common functions and types for classification learners.
//

module Classifiers


// namespaces

open MathNet
open Primitives


// interfaces

type IClassifier =
    abstract member Train : seq<Sample> -> unit
    abstract member Classify : Sample -> int


// private functions

let private EnumerateLabels samples =
    let labels = samples |> Seq.groupBy (fun s -> s.Label) |> Seq.map (fun (k,g) -> k) |> Seq.sort
    labels

let private CountSamplesByLabel samples label =
    let count = samples |> Seq.filter (fun s -> s.Label = label) |> Seq.length
    count


// functions

let MeasureError (classifier:IClassifier) samples = 
    
    let mutable errorCount = 0

    for sample in samples do
        let classifiedAs = classifier.Classify sample
        let correctlyClassified = classifiedAs = sample.Label
        
        if correctlyClassified = false then
            errorCount <- errorCount + 1

    let errorPercent = (float)errorCount / (float)(samples |> Seq.length)
    errorPercent

let MeasureConfusion (classifier:IClassifier) samples labels =
    
    let labelsAsArray = labels |> Seq.toArray
    let labelsCount = labels |> Seq.length

    let sampleCountByLabel = labelsAsArray |> Seq.map (fun label -> CountSamplesByLabel samples label) |> Seq.toArray
    
    // initialize confusion matrix
    let mutable confusionMatrix = matrix labelsCount labelsCount

    // count (mis)classifications
    for sample in samples do
        let correctLabel = sample.Label
        let classifiedLabel = classifier.Classify sample

        let i = labelsAsArray |> Seq.findIndex (fun label -> label = classifiedLabel)
        let j = labelsAsArray |> Seq.findIndex (fun label -> label = correctLabel)
        
        confusionMatrix.[i,j] <- confusionMatrix.[i,j] + 1.0
    
    // divide by true label count
    for j in [0 .. 1 .. labelsCount-1] do
        let columnJ = confusionMatrix.Column(j)
        let sampleCountForLabelJ = sampleCountByLabel |> Seq.item j
        let columnJDivided = columnJ.Multiply(1.0 / (float)sampleCountForLabelJ)
        confusionMatrix.SetColumn(j, columnJDivided)

    confusionMatrix

/// Measures the entropy of the given sample set.
let MeasureEntropy samples =
    let sampleSize = samples |> Seq.length
    
    let probabilityOfDrawingLabel = 
        samples
        |> Seq.countBy (fun x -> x.Label)
        |> Seq.map (fun (l,c) -> (l, float(c) / float(sampleSize)))

    let entropy = 
        probabilityOfDrawingLabel
        |> Seq.sumBy (fun (l,c) -> c * System.Math.Log(c))
        |> (fun x -> -x)

    entropy


// types

type NullClassifier() = 
    interface IClassifier with
        member self.Train samples =
            ()
        member self.Classify sample =
            failwith "Not implemented"
