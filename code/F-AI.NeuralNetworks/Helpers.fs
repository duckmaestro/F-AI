
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


module Helpers

open FAI.NeuralNetworks

/// Generates a matrix populated with randomly valued elements.
let randomizeMatrix rows cols min max randomizer =
    let rng = 
        match randomizer with
        | Some(rng_) -> rng_
        | None -> new System.Random(0)

    let matrix = new Matrix(rows,cols)
    let range = max - min
    for i in 0..matrix.Values.Length-1 do
        matrix.Values[i] <- rng.NextDouble() * range + min

    matrix
    
