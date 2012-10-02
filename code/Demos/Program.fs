
// namespaces

open System
open SampleLoader
open Classifiers
open RegressionPredictors
open kNN
open Perceptron
open DecisionTree
open PCARegression


// records

type BootstrapRecord<'T> = {
    Name : string;
    Classifier : IClassifier;
    RegressionPredictor  : IRegressionPredictor;
    TrainingError : 'T
    ValidationError : 'T
    TestError : 'T
}
let defaultRecord = 
    { 
        Name = null; 
        Classifier = new NullClassifier() :> IClassifier; 
        RegressionPredictor = new NullRegressionPredictor() :> IRegressionPredictor; 
        TrainingError = 1.0; 
        ValidationError = 1.0; 
        TestError = 1.0; 
    } 


//
// main
//

// load training samples
let samplesTraining = SampleLoader.LoadFromFile "hw3train.txt"

// load test samples
let samplesTest = SampleLoader.LoadFromFile "hw3test.txt"


let performSupervisedTests = false
let performUnsupervisedTests = true


//
// supervised
//

if performSupervisedTests then
    let mutable classifiers = [|
        { defaultRecord with Name = "1-NN"; Classifier = new kNNClassifier(1) :> IClassifier; }
        { defaultRecord with Name = "5-NN"; Classifier = new kNNClassifier(5) :> IClassifier; }
        { defaultRecord with Name = "Perceptron Weighted"; Classifier = new PerceptronClassifier(0, 6, ClassificationMethod.Weighted) :> IClassifier; }
        { defaultRecord with Name = "Decision Tree (Max Depth 1)"; Classifier = new DecisionTreeClassifier(1) :> IClassifier; }
        { defaultRecord with Name = "Decision Tree (Max Depth 3)"; Classifier = new DecisionTreeClassifier(3) :> IClassifier; }
        { defaultRecord with Name = "Decision Tree (Max Depth 7)"; Classifier = new DecisionTreeClassifier(7) :> IClassifier; }
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


    // print classifiers and error
    for c in classifiers do
        System.Console.WriteLine("Classifier: {0}. Training Error: {1:P3}. Test Error: {2:P3}.", c.Name, c.TrainingError, c.TestError)
    


//
// unsupervised
//

if performUnsupervisedTests then

    let mutable regressionPredictor = [|
        { defaultRecord with Name = "PCA"; RegressionPredictor = new PCARegressionPredictor() :> IRegressionPredictor; }
    |]


    // train
    for c in regressionPredictor do
        c.RegressionPredictor.Train samplesTraining




//
// done. 
// 

// wait for input.
let mutable key = Console.ReadLine()

