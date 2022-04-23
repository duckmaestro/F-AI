
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

// namespaces

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
open FAI.Loaders


// records

type BootstrapRecord<'T> = {
    Name : string;
    Classifier : IClassifier;
    RegressionPredictor  : IPredictor;
    TrainingError : 'T
    ValidationError : 'T
    TestError : 'T
    SampleMapper : seq<Sample> -> seq<Sample>
}
let defaultRecord = { 
    Name = null; 
    Classifier = new NullClassifier() :> IClassifier; 
    RegressionPredictor = new NullRegressionPredictor() :> IPredictor; 
    TrainingError = 1.0; 
    ValidationError = 1.0; 
    TestError = 1.0; 
    SampleMapper = fun x -> x
} 


let doDemoSupervised = 


    // load usps9 data set
    let dataSetUSPS9Training = LoaderUSPS9.LoadFromFile "hw3train.txt" :> seq<Sample>
    let dataSetUSPS9Test = LoaderUSPS9.LoadFromFile "hw3test.txt" :> seq<Sample>

    let reduceVectorToBinary v threshold =
        let asSeq = v :> seq<float>
        let finalValue = MathNet.vector (v |> Seq.map (fun x -> if x >= threshold then 1.0 else 0.0))
        finalValue

    let reduceSamplesToBinary labelOfInterest samples = 
        samples 
        |> Seq.map (fun s-> { Features = reduceVectorToBinary s.Features 128.0; Label = if s.Label = labelOfInterest then 1 else 0 })
        |> Seq.cache

    let mutable classifiers = [|
        { defaultRecord with Name = "1-NN"; Classifier = new kNNClassifier(1) :> IClassifier; }
        { defaultRecord with Name = "5-NN"; Classifier = new kNNClassifier(5) :> IClassifier; }
        { defaultRecord with Name = "Perceptron Weighted"; Classifier = new PerceptronClassifier(0, 6, ClassificationMethod.Weighted) :> IClassifier; }
        { defaultRecord with Name = "Decision Tree (Max Depth 1)"; Classifier = new DecisionTreeClassifier(1) :> IClassifier; }
        { defaultRecord with Name = "Decision Tree (Max Depth 3)"; Classifier = new DecisionTreeClassifier(3) :> IClassifier; }
        { defaultRecord with Name = "Decision Tree (Max Depth 7)"; Classifier = new DecisionTreeClassifier(7) :> IClassifier; }
        { defaultRecord with Name = "Noisy-Or (*\"Zero Detector\": inputs reduced to {other, 0})"; 
                                Classifier = new NoisyOrClassifier(5, true) :> IClassifier; 
                                SampleMapper = reduceSamplesToBinary 0; }
    |]


    //
    // Train.
    //
    System.Console.WriteLine("Training classifiers...");
    for c in classifiers do
        System.Console.WriteLine("Training '" + c.Name + "'...");
        c.Classifier.Train (c.SampleMapper dataSetUSPS9Training)


    //
    // Measure training error.
    //
    System.Console.WriteLine("Measuring training error...");
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
    System.Console.WriteLine("Measuring test error...");
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
    System.Console.WriteLine("Printing results...");
    for c in classifiers do
        System.Console.WriteLine("Classifier: {0}. Training Error: {1:P3}. Test Error: {2:P3}.", c.Name, c.TrainingError, c.TestError)
    


