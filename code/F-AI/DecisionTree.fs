
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


// private functions


// types

type DecisionTreeClassifier() = 
    interface IClassifier with
        member self.Train samples =
            ()

        member self.Classify point =
            0