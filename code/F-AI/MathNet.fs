
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
//    Some convenience functions and type aliases
//    for interfacing with the MathNet library.
//

module MathNet


// namespaces

open MathNet.Numerics.FSharp
open MathNet.Numerics.LinearAlgebra.Generic
open MathNet.Numerics.LinearAlgebra.Double


// type aliases

type Vector = Vector<float>
type Matrix = Matrix<float>
type MatrixDense = DenseMatrix
type MatrixSparse = SparseMatrix


// types

//type MatrixEnumerated(rows:Vector seq) = 
//    class
//        inherit MathNet.Numerics.LinearAlgebra.Generic.Matrix<float>(
//            rows |> Seq.length, 
//            (rows |> Seq.head).Count)
//        
//        override self.DoDivide(scalar:float, matrix:Matrix) = 
//            raise (System.NotImplementedException("Not supported."))
//
//        override self.DoPointwiseDivide(matrixA, matrixB) = 
//            raise (System.NotImplementedException("Not supported."))
//
//        override self.DoSubtract(matrixA:Matrix, matrixB:Matrix) =
//            raise (System.NotImplementedException("Not supported."))
//
//        override self.DoMultiply(matrixA:Matrix, matrixB:Matrix) = 
//            raise (System.NotImplementedException("Not supported."))
//
//        override self.DoMultiply(scalar:float, matrix:Matrix) =
//            raise (System.NotImplementedException("Not supported."))
//
//        override self.DoMultiply(vectorA:Vector, vectorB:Vector) = 
//            raise (System.NotImplementedException("Not supported."))
//
//
//        override self.DoAdd(matrixA:Matrix, matrixB:Matrix) =
//            raise (System.NotImplementedException("Not supported."))
//
//        override self.At(i:int, j:int) =
//            let value = rows |> Seq.skip(i) |> Seq.head |> (fun r -> r.Item(j))
//            value
//
//        override self.At(i:int, j:int, value:float) = 
//            raise (System.NotImplementedException("Not supported."))
//
//        override self.CreateMatrix(i, j) = 
//            raise (System.NotImplementedException("Not supported."))
//
//        override self.CreateVector(size) = 
//            raise (System.NotImplementedException("Not supported."))
//
//        override self.ConjugateTranspose() = 
//            raise (System.NotImplementedException("Not supported."))
//
//        override self.L1Norm() =
//            raise (System.NotImplementedException("Not supported."))
//
//        override self.FrobeniusNorm() =
//            raise (System.NotImplementedException("Not supported."))
//
//        override self.InfinityNorm() = 
//            raise (System.NotImplementedException("Not supported."))
//
//        override self.Trace() = 
//            raise (System.NotImplementedException("Not supported."))
//    end


// functions

let matrix rows columns = 
    new DenseMatrix(rows, columns) :> Matrix

let vector (values:float seq) =
    DenseVector.ofSeq values :> Vector

let vectorFromCount (count:int) = 
    new DenseVector(count) :> Vector

let matrixFromRows (rows:Vector seq) =
    let numRows = rows |> Seq.length
    let numCols = rows |> Seq.head |> (fun e->e.Count)

    let matrix = new MatrixDense(numRows, numCols)
    for indexedRow in rows |> Seq.mapi (fun i r -> i,r) do
        let index = fst indexedRow
        let row = snd indexedRow
        matrix.SetRow(index, row)
    
    matrix
    