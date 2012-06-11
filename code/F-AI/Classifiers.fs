
module Classifiers

// namespaces

open MathNet


// records

type SampleLabeled = 
    {
        Features : Vector
        Label : int
    }


// interfaces

type IClassifier =
    abstract member Train : seq<SampleLabeled> -> unit
    abstract member Classify : Vector -> int


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
        let classifiedAs = classifier.Classify sample.Features
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
        let classifiedLabel = classifier.Classify sample.Features

        let i = labelsAsArray |> Seq.findIndex (fun label -> label = classifiedLabel)
        let j = labelsAsArray |> Seq.findIndex (fun label -> label = correctLabel)
        
        confusionMatrix.[i,j] <- confusionMatrix.[i,j] + 1.0
    
    // divide by true label count
    for j in [0 .. 1 .. labelsCount-1] do
        let columnJ = confusionMatrix.Column(j)
        let sampleCountForLabelJ = sampleCountByLabel |> Seq.nth j
        let columnJDivided = columnJ.Multiply(1.0 / (float)sampleCountForLabelJ)
        confusionMatrix.SetColumn(j, columnJDivided)

    confusionMatrix


// types

type NullClassifier() = 
    interface IClassifier with
        member self.Train samples =
            ()
        member self.Classify point =
            failwith "Not implemented"
