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



//
//    Some convenience functions and type aliases
//    for interfacing with the MathNet library.
//

// TODO: Rename to MathNet types to avoid confusion with official namespace.
module MathNet

open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double


//
// Type aliases.
// 

type Vector = Vector<float>
type Matrix = Matrix<float>
type MatrixDense = DenseMatrix
type MatrixSparse = SparseMatrix


// 
// Helper functions.
// 

// TODO: Move  into a 'MakeNew' static class for easier interop with LanguagePrimitives generic number values?

/// Creates a new dense matrix using the provided dimensions.
let matrix rows columns = 
    new DenseMatrix(rows, columns)

/// Creates a new vector using the provided sequence.
let vector (values:float seq) : Vector =
    DenseVector.ofSeq values

/// Creates a new vector using the provided dimension.
let vectorFromCount (count:int) = 
    new DenseVector(count)

/// Creates a new matrix using the provided sequence of rows.
let matrixFromRows (rows:Vector seq) =
    let numRows = rows |> Seq.length
    let numCols = rows |> Seq.head |> (fun e->e.Count)

    let matrix = new MatrixDense(numRows, numCols)
    for indexedRow in rows |> Seq.mapi (fun i r -> i,r) do
        let index = fst indexedRow
        let row = snd indexedRow
        matrix.SetRow(index, row)
    
    matrix
    