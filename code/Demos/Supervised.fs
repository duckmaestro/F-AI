
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


module Supervised

open System
open FAI
open Classifiers
open Predictors
open kNN
open Perceptron
open DecisionTree
open NoisyOr
open PCARegression
open FAI.Bayesian
open FAI.NeuralNetworks
open FAI.Loaders


//
// Records
// 

type BootstrapRecord = {
    Name : string
    Classifier : IClassifier
    RegressionPredictor  : IPredictor
    TrainingError : float
    TrainingTime : TimeSpan
    ValidationError : float
    TestError : float
    SampleMapper : seq<Sample> -> seq<Sample>
}
let defaultRecord = { 
    Name = null; 
    Classifier = new NullClassifier() :> IClassifier; 
    RegressionPredictor = new NullRegressionPredictor() :> IPredictor; 
    TrainingError = 1.0; 
    TrainingTime = TimeSpan.Zero
    ValidationError = 1.0; 
    TestError = 1.0; 
    SampleMapper = fun x -> x
} 


let doDemoSupervised = 


    // Load usps9 data set.
    let dataSetUSPS9Training = LoaderUSPS9.LoadFromFile "hw2train.txt" :> seq<Sample>
    let dataSetUSPS9Test = LoaderUSPS9.LoadFromFile "hw2test.txt" :> seq<Sample>
    let featureLength = dataSetUSPS9Training |> Seq.head |> fun x -> x.Features.Count
    let classMin,classMax = 
        dataSetUSPS9Training 
        |> Seq.fold 
            (fun s e -> min (fst s) e.Label, max (snd s) e.Label) 
            (Int32.MaxValue, Int32.MinValue)
    // Hack, assume class count is the maxValue + 1
    let classCount = classMax + 1

    // Data set helpers.
    let reduceVectorToBinary v threshold =
        let asSeq = v :> seq<float>
        let finalValue = MathNet.vector (v |> Seq.map (fun x -> if x >= threshold then 1.0 else 0.0))
        finalValue

    let reduceSamplesToBinary labelOfInterest samples = 
        samples 
        |> Seq.map (fun s-> { Features = reduceVectorToBinary s.Features 128.0; Label = if s.Label = labelOfInterest then 1 else 0 })
        |> Seq.cache

    let reduceSamplesToTwoClasses class1 class2 samples =
        samples
        |> Seq.where (fun s -> s.Label = class1 || s.Label = class2)
        |> Seq.cache


    //
    // List of tests.
    // 
    let mutable classifiers = [|
        // Nearest neighbor
        { defaultRecord with Name = "1-NN"; Classifier = new kNNClassifier(1) :> IClassifier; }
        { defaultRecord with Name = "5-NN"; Classifier = new kNNClassifier(5) :> IClassifier; }

        // Perceptron
        { defaultRecord with Name = "Perceptron Weighted (*\"Labels 0 vs. 6\")"; 
                                Classifier = new PerceptronClassifier(0, 6, ClassificationMethod.Weighted) :> IClassifier; 
                                SampleMapper = reduceSamplesToTwoClasses 0 6; }
        
        // Decision Tree
        { defaultRecord with Name = "Decision Tree (Max Depth 2)"; Classifier = new DecisionTreeClassifier(2) :> IClassifier; }
        { defaultRecord with Name = "Decision Tree (Max Depth 8)"; Classifier = new DecisionTreeClassifier(8) :> IClassifier; }
        
        // Noisy-Or
        { defaultRecord with Name = "Noisy-Or (5 Iterations) (*\"Labels 0 vs. other\")"; 
                                Classifier = new NoisyOrClassifier(5, true) :> IClassifier; 
                                SampleMapper = reduceSamplesToBinary 0; }
        
        // Neural Network
        { defaultRecord with Name = "1 Layer Neural Network"; 
                                Classifier = new MulticlassClassifier(featureLength, classCount, 1); }
        { defaultRecord with Name = "2 Layer Neural Network"; 
                                Classifier = new MulticlassClassifier(featureLength, classCount, 2); }
    |]


    //
    // Train.
    //
    Console.WriteLine("Training classifiers...");
    classifiers <-
        classifiers
        |> Seq.map (fun c ->
            System.Console.WriteLine("Training '" + c.Name + "'...")
            let time1 = DateTime.UtcNow
            c.Classifier.Train (c.SampleMapper dataSetUSPS9Training)
            let time2 = DateTime.UtcNow
            let time = time2 - time1
            Console.WriteLine("\t{0:G}", time)
            Console.WriteLine()

            { c with TrainingTime = time })
        |> Seq.toArray

    //
    // Measure training error.
    //
    Console.WriteLine("Measuring training error...");
    classifiers <-
        classifiers
        |> Seq.map (fun c -> 
            { 
                c with TrainingError = MeasureError c.Classifier (c.SampleMapper dataSetUSPS9Training); 
            })
        |> Seq.toArray


    //
    // Measure test error.
    //
    Console.WriteLine("Measuring test error...");
    classifiers <-
        classifiers
        |> Seq.map (fun c ->
            {
                c with TestError = MeasureError c.Classifier (c.SampleMapper dataSetUSPS9Test)
            })
        |> Seq.toArray


    //
    // Print classifiers and error.
    //
    Console.WriteLine("Printing results...");
    for c in classifiers do
        Console.WriteLine(
            "Classifier: {0}. 
            \tTraining Error: {1:P3}. \tTest Error: {2:P3}. \tTraining Time: {3:G}", c.Name, c.TrainingError, c.TestError, c.TrainingTime)
        Console.WriteLine()
    


