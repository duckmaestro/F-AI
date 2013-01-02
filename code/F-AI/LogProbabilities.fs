
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
//    Methods for working with log-probabilities.
//

module LogProbabilities


// namespaces

open System
open MathNet


// aliases

type LogVector = Vector
type LogScalar = float


// private methods

let private CheckVector (v:Vector) =
    // Check for infinities
    let hasInfinityValue = (v |> Seq.exists (fun z -> Double.IsInfinity(z)))
    assert not hasInfinityValue

    // Check for NaNs
    let hasNaNValue = (v |> Seq.exists (fun z -> Double.IsNaN(z)))
    assert not hasNaNValue

    // Done
    ()

let private CheckLogVector (v:LogVector) = 
    // Check for positive values
    let hasPositiveValue = (v |> Seq.exists (fun z -> z > 0.0))
    assert not hasPositiveValue

    // Check for infinities
    let hasInfinityValue = (v |> Seq.exists (fun z -> Double.IsInfinity(z)))
    assert not hasInfinityValue

    // Check for NaNs
    let hasNaNValue = (v |> Seq.exists (fun z -> Double.IsNaN(z)))
    assert not hasNaNValue
    
    // Done
    ()

let private CheckDidUnderflow (v:Vector) =
    // Check for any zero values.
    let hasZero = (v |> Seq.exists (fun z -> z = 0.0))
    assert not hasZero
    ()

let private CheckScalar z =
    assert not (Double.IsInfinity z)
    assert not (Double.IsNaN z)
    ()

let private IsSameMagnitude (z1:float) (z2:float) =
    let z1log = int (Math.Round (Math.Log (Math.Abs (z1))))
    let z2log = int (Math.Round (Math.Log (Math.Abs (z2))))

    z1log = z2log


/// The "naive" approach, for reference.
let private SumProbabilitiesSimple (x:LogVector) : LogScalar =
    
    CheckLogVector x

    // Calculate e^x_i
    let exp_x = 
        x
        |> Seq.map (fun xi -> System.Math.Exp(xi))
        |> vector

    CheckDidUnderflow exp_x

    // Sum
    let sum = 
        exp_x
        |> Seq.sum

    CheckScalar sum

    // Log
    let log = 
        Math.Log(sum)

    CheckScalar log

    // Done
    log


/// Computes the log-sum of the components in the provided log-vector.
let SumProbabilities (x:LogVector) : LogScalar =
    let check = false;

    if check then CheckLogVector x

    // This approach utilizes the fact that 
    // log Σ e^x_i = log [Σ e^(x_i + α)] - α.
    
    // Determine largest value α.
    let n = float x.Count
    let largestX = x |> Seq.max
    let doubleMax = (float Single.MaxValue) * 0.999
    let doubleMax = (float Double.MaxValue) * 0.999

    // Determine largest possible shift value α.
    // The sum of e^y_i should not exceed Double.MaxValue.
    // Let's simply assume every y_i is the max, thus n * e^max
    // should not exceed Double.MaxValue. Thus 
    // max = log (Double.MaxValue / n)
    let largestXiAfterShift = Math.Log (doubleMax / n)
    let α = largestXiAfterShift - largestX

    // Shift x by alpha.
    let shifted_x =
        x 
        |> Seq.map (fun xi -> xi + α)
        |> vector

    if check then CheckVector shifted_x

    // Exponentiate.
    let exp_shifted_x = 
        shifted_x
        |> Seq.map (fun xi -> System.Math.Exp(xi))
        |> vector

    if check then CheckDidUnderflow exp_shifted_x

    // Sum.
    // To sum we will first divide into positive 
    // and negative groups, and then build a sum for
    // each group using increasing magnitude order.
    // This reduces the number of potential catastrophic
    // subtractions down to one (the final sum)
    // which is dealt with later.
    let posSum = 
        exp_shifted_x 
        |> Seq.filter (fun z -> z >= 0.0)
        |> Seq.sort
        |> Seq.sum
    let negSum = 
        exp_shifted_x 
        |> Seq.filter (fun z -> z < 0.0)  
        |> Seq.sortBy (fun z-> -z)
        |> Seq.sum

    let totalSum =
        if posSum = 0.0 || negSum = 0.0 || (IsSameMagnitude posSum negSum) then
            let sum = posSum + negSum
            CheckScalar sum
            let log = Math.Log sum
            log

        else
            // from: http://en.wikipedia.org/wiki/Log_identities
            // log(x + y) = log(x) + log(1 + y/x)
            let xy = if posSum > Math.Abs(negSum) then posSum,negSum else negSum,posSum
            let x = fst xy
            let y = snd xy
            let log1 = Math.Log(x)
            let log2 = Math.Log(1.0 + y / x)
            let sumLogs = log1 + log2
            sumLogs

    if check then CheckScalar totalSum

    // Unshift
    let finalValue = 
        totalSum - α

    if check then CheckScalar finalValue

    // Done.
    finalValue


/// Tests 
let Test = 
    let testValues = 
        [|
            vector [| -1.0; -1.0; -1.0; -1.0; -1.0; -1.0 |]
            vector [| -1.0; -1.0; -1.0; -1.0; -1.0; -100.0 |]
            vector [| -1.0; -100.0; -100.0; -100.0; -100.0; -100.0 |]
            vector [| -100.0; -100.0; -100.0; -100.0; -100.0; -100.0 |]
            vector [| -1000.0; -1000.0; -1000.0; -1000.0; -1000.0; -1000.0; |]
            vector [| -20000.0; -20000.0; -20000.0; -20000.0; -20000.0 |]
            vector [| -2.0; -2000000000.0; -2000000000.0; -2000000000.0; -2000000000.0 |]
            vector [| -2000000000.0; -2000000000.0; -2000000000.0; -2000000000.0; -2000000000.0 |]
        |]

    for v in testValues do
        let sumNaive = SumProbabilitiesSimple v
        let sumSmarter = SumProbabilities v

        Console.WriteLine("Input: {0}", v.ToString())
        Console.WriteLine("Naive: {0} \t Smarter: {1}", sumNaive, sumSmarter)
        Console.WriteLine()

    ()