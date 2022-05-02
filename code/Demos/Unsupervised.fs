
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


module Unsupervised

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

let doDemoUnsupervised = 

    // load usps9 data set
    let dataSetUSPS9Training = LoaderUSPS9.LoadFromFile "hw3train.txt" false :> seq<Sample>
    let dataSetUSPS9Test = LoaderUSPS9.LoadFromFile "hw3test.txt" false :> seq<Sample>


    let mutable regressionPredictor = [|
        { defaultRecord with Name = "PCA"; RegressionPredictor = new PCARegressionPredictor() :> IPredictor; }
    |]

    // train
    for c in regressionPredictor do
        c.RegressionPredictor.Train dataSetUSPS9Training


