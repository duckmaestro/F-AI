
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



//
//    Common functions and types for unlabeled  
//

module Predictors


// namespaces

open MathNet
open Primitives


// interfaces

type IPredictor =
    abstract member Train : seq<Sample> -> unit
    abstract member Predict : Sample -> Sample


// types

type NullRegressionPredictor() = 
    interface IPredictor with
        member self.Train samples =
            ()
        member self.Predict sample =
            failwith "Not implemented"
