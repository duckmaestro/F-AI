
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

type public Connectivity =
    | FullyConnected

type public Activation =
    | Linear
    | ReLU

type public Weights =
    | Zero
    | Random of min:float * max:float * seed:int
    | Provided of matrix:Matrix


    

/// A single layer.
type public Layer(
                connectivity, 
                weights,
                activationType, 
                inputSize, 
                outputSize) =

    // Weight-matrix for this layer. We will right-multiply 
    // the input against this.
    let weightMatrix = 
        match weights with
        | Provided (m) -> m
        | Zero -> new Matrix(outputSize, inputSize)
        | Random (min, max, seed) -> randomizeMatrix outputSize inputSize min max (Some (new System.Random(seed)))

    // Activation function.
    let activation =
        match activationType with
        | Linear -> new ActivationLinear() :> IActivationFunction
        | ReLU -> new ActivationReLU()

    /// Evaluates the layer.
    member public self.Evaluate(input:Vector) : Vector =
        // Check dimensions.
        assert (input.Count = inputSize)
        assert (inputSize = weightMatrix.ColumnCount)

        // Linear result.
        let outputLinear = weightMatrix.Multiply(input)
        
        // Apply activation.
        let outputScaled = activation.Evaluate(outputLinear)
        
        // Done
        outputScaled
