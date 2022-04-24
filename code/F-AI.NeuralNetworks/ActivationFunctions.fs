
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

open FAI.NeuralNetworks

/// Standard interface into activation functions.
[<Interface>]
type IActivationFunction =
    abstract Evaluate: x:float -> float 
    abstract Evaluate: x:Vector -> Vector 

/// A linear activator.
type ActivationLinear() =
    interface IActivationFunction with
        member self.Evaluate (x:float) =
            x

        member self.Evaluate (x:Vector) =
            x.Clone()

/// A ReLU activator.
type ActivationReLU() =
    interface IActivationFunction with
        member self.Evaluate (x:float) =
            if x < 0 then 0.0
            else x

        member self.Evaluate (x:Vector) =
            x.PointwiseMaximum(0)
            
