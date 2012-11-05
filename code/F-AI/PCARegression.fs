
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
//    PCA learner.
//

module PCARegression


// namespaces

open System
open MathNet
open Primitives
open Statistics
open Predictors


// types

type PCARegressionPredictor() =
    
    let mutable eigenVectors = Array.empty<Vector>
    let mutable eigenValues = Array.empty<float>

    interface IPredictor with
        member self.Train samples = 
            
            // convert samples to matrix
            let samplesAsMatrix = matrixFromRows (samples |> Seq.map (fun s -> s.Features))

            // calculate covariance matrix
            let covarianceMatrix = Statistics.CovarianceMatrix(samplesAsMatrix.RowEnumerator() |> Seq.map (fun r -> snd r))

            // calculate eigen vectors/values
            let eigenDecomposition = new MathNet.Numerics.LinearAlgebra.Double.Factorization.DenseEvd(samplesAsMatrix)

            assert (eigenDecomposition.EigenValues() |> Seq.forall (fun e -> e.Imaginary = 0.0))

            let eigenPairs = Seq.zip (eigenDecomposition.EigenValues()) (eigenDecomposition.EigenVectors().RowEnumerator())
            let orderedEigenPairs = eigenPairs |> Seq.sortBy (fun p -> -(fst p).Real)
            eigenValues <- orderedEigenPairs |> Seq.map (fun p -> (fst p).Real) |> Seq.toArray
            eigenVectors <- orderedEigenPairs |> Seq.map (fun p -> (snd (snd p))) |> Seq.toArray

            () // todo

        member self.Predict sample =
            sample // todo



