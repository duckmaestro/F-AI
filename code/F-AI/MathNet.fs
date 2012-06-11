
module MathNet


// namespaces

open MathNet.Numerics.FSharp
open MathNet.Numerics.LinearAlgebra.Generic
open MathNet.Numerics.LinearAlgebra.Double


// type aliases

type Vector = Vector<float>
type Matrix = Matrix<float>


// functions

let matrix rows columns = 
    new DenseMatrix(rows, columns) :> Matrix

let vector (values:float seq) =
    DenseVector.ofSeq values :> Vector

let vectorFromCount (count:int) = 
    new DenseVector(count) :> Vector