﻿
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


namespace FAI.NeuralNetworks

open Helpers
open ActivationFunctions
open MathNet

type public Connectivity =
    | FullyConnected

type public Activation =
    | Linear
    | Sigmoid
    | ReLU

type public Weights =
    | Zero
    | Random of min:Scalar * max:Scalar * seed:int
    | Provided of matrix:Matrix


type public LayerEvaluationRecord = {
    Input : Vector
    WeightedOutput : Vector 
    ActivatedOutput : Vector
}

///
/// A single layer.
///
type public Layer(
                name,
                connectivity, 
                weights,
                activationType, 
                inputSize, 
                outputSize) =

    // Weight-matrix for this layer. We will right-multiply 
    // the input against this.
    let mutable weightMatrix = 
        match weights with
        | Provided (m) -> m
        | Zero -> matrix outputSize inputSize
        | Random (min, max, seed) -> randomizeMatrix outputSize inputSize min max (Some (new System.Random(seed)))

    // Activation function.
    let activation =
        match activationType with
        | Linear -> new ActivationLinear() :> IActivationFunction
        | Sigmoid -> new ActivationSigmoid()
        | ReLU -> new ActivationReLU()


    //
    // Basic stuff.
    // 

    override _.ToString() =
        name

    member public self.Name
        with get() = name


    ///
    /// Evaluates the layer.
    ///
    member public self.Evaluate(input:Vector) : Vector = 
        let outputRecord = self.Evaluate2(input)
        outputRecord.ActivatedOutput


    ///
    /// Evaluates the layer and returns the values of each step.
    ///
    member public self.Evaluate2(input:Vector)  =
        // Check dimensions.
        assert (input.Count = inputSize)
        assert (inputSize = weightMatrix.ColumnCount)

        // Linear result.
        let outputLinear = weightMatrix.Multiply(input)
        
        // Apply activation.
        let outputScaled = activation.Evaluate(outputLinear)
        
        // Done
        { Input = input; WeightedOutput = outputLinear; ActivatedOutput = outputScaled }


    ///
    /// Evaluates the gradiant from the layer weights.
    ///
    member public self.EvaluateGradiantFromWeights(input:Vector) : Matrix =
        let weights = weightMatrix
        let z = weights.Multiply input
        //let a = activation.Evaluate z
        let da_dz = activation.EvaluateDerivative(z).ToRowMatrix()
        let gradiantFromWeights = da_dz.Multiply(weights)

        gradiantFromWeights


    ///
    /// 
    /// 
    member public self.AdjustWeights(deltaMatrix) : unit =
        weightMatrix <- weightMatrix + deltaMatrix