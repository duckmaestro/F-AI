
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


// conversion functions

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
    