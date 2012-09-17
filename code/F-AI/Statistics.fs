
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
//    Statistics-related methods.
//

module Statistics


// namespaces

open System
open MathNet
open Primitives


// private methods

/// Calculates covariance between two variables.
let Covariance (variable1:seq<_>) (variable2:seq<_>) =
    let n = float (variable1 |> Seq.length)
    assert ((n) = float (variable2 |> Seq.length))
    
    let mean1 = (variable1 |> Seq.sum) / (float n)
    let mean2 = (variable2 |> Seq.sum) / (float n)
    
    let zipped = variable1 |> Seq.zip variable2

    let covariance 
        = zipped 
        |> Seq.map (fun (x1,x2) -> (x1 - mean1) * (x2 - mean2))
        |> Seq.sum
        |> (fun sum -> sum / n)
        
    covariance


// public methods

/// Calculate covariance matrix of a sample set.
/// Uses the first element in the sample set to auto-detect dimensionality.
let CovarianceMatrix (samples:Vector seq) = 

    // detect dimensionality
    let dimensionality = (samples |> Seq.head).Count
    assert (samples |> Seq.forall (fun s -> s.Count = dimensionality))

    // prepare output 
    let mutable covarianceMatrix = matrix dimensionality dimensionality

    // build list of element indices
    let indices = [|0..(dimensionality-1)|]
    let indicesGreaterThan i = indices |> Seq.filter (fun j -> j > i)
    let pairs 
        = indices 
        // pairs of (i,j) where i < j
        |> Seq.collect (fun i -> indicesGreaterThan i |> Seq.map (fun j -> i,j))
        // + pairs of (i,i)
        |> Seq.append (indices |> Seq.map (fun i -> i,i))
    
    // calculate covariance
    for pair in pairs do
        let i = fst pair
        let j = snd pair
        let samplesi = samples |> Seq.map (fun s -> s.Item(i))
        let samplesj = samples |> Seq.map (fun s -> s.Item(j))
        let covariance = Covariance samplesi samplesj
        covarianceMatrix.At(i, j, covariance)
        covarianceMatrix.At(j, i, covariance)

    // done
    covarianceMatrix
        

