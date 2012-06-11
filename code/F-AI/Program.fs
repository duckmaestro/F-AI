
// namespaces

open System
open SampleLoader
open Classifiers
open kNN
open Perceptron


// main

// load training samples
let samplesTraining = SampleLoader.LoadFromFile "hw3train.txt"

// load test samples
let samplesTest = SampleLoader.LoadFromFile "hw3test.txt"

// create classifiers
type BootstrapRecord<'T> = {
    Name : string;
    Classifier : IClassifier;
    TrainingError : 'T
    ValidationError : 'T
    TestError : 'T
}
let defaultRecord = { Name = null; Classifier = new NullClassifier() :> IClassifier; TrainingError = 1.0; ValidationError = 1.0; TestError = 1.0; } 
let mutable classifiers = [|
    { defaultRecord with Name = "1-NN"; Classifier = new kNNClassifier(1) :> IClassifier; }
    { defaultRecord with Name = "Perceptron Weighted"; Classifier = new PerceptronClassifier(0, 6, ClassificationMethod.Weighted) :> IClassifier; }
|]

// train
for c in classifiers do
    c.Classifier.Train samplesTraining

// measure training error
classifiers <-
    classifiers
    |> Seq.map (fun c -> 
        { 
            c with TrainingError = MeasureError c.Classifier samplesTraining; 
        })
    |> Seq.toArray

// measure test error
classifiers <-
    classifiers
    |> Seq.map (fun c ->
        {
            c with TestError = MeasureError c.Classifier samplesTest
        })
    |> Seq.toArray


// find best classifers
let bestClassifierByValidation = classifiers |> Seq.sortBy (fun c -> c.ValidationError) |> Seq.head
let bestClassifierByTest = classifiers |> Seq.sortBy (fun c -> c.TestError) |> Seq.head

// print classifiers and error
for c in classifiers do
    System.Console.WriteLine("Classifier: {0}. Training Error: {1:P3}. Test Error: {2:P3}.", c.Name, c.TrainingError, c.TestError)
    

// done. wait for input.
let mutable key = Console.ReadLine()

