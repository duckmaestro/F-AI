
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

module ActivationFunctions

open Helpers
open MathNet

///
/// Standard interface into activation functions.
///
[<Interface>]
type IActivationFunction =
    abstract Evaluate: x:#Vector -> Vector
    abstract EvaluateDerivative: x:#Vector -> Vector 

///
/// A linear activator.
///
type ActivationLinear() =
    interface IActivationFunction with
        member self.Evaluate (x:#Vector) =
            x.Clone()

        member self.EvaluateDerivative (x:#Vector) =
            // TODO: Don't do this every call. Cache or something.
            vector([|for i in 1..x.Count do 1.0 |])

///
/// A ReLU activator.
///
type ActivationReLU() =
    interface IActivationFunction with
        member self.Evaluate (x:#Vector) =
            x.PointwiseMaximum(0)

        member self.EvaluateDerivative (x:#Vector) =
            let d = vectorFromCount x.Count
            for i in 0..d.Count-1 do
                let v = x[i]
                d[i] <-
                    if v > 0 then 1
                    elif v < 0 then 0
                    else 0.5
            d
    
///
/// A sigmoid activator.
///
type ActivationSigmoid() =
    
    // Sigmoid.
    let sigmoid x =
        1.0 / (1.0 + (approximateExponential -x ))

    // Derivative of sigmoid
    let sigmoid' x =
        let s = (sigmoid x)
        s * (1.0 - s)


    interface IActivationFunction with
        member self.Evaluate (x:#Vector) =
            let x' = x.Clone()
            for i in 0..x'.Count-1 do
                x'[i] <- sigmoid x[i]
            x'

        member self.EvaluateDerivative (x:#Vector) = 
            let x' = x.Clone() 
            for i in 0..x'.Count-1 do 
                x'[i] <- sigmoid' x[i]
            x'