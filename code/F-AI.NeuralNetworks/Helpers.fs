
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

open System
open MathNet

/// Generates a matrix populated with randomly valued elements.
let randomizeMatrix rows cols min max randomizer =
    let rng = 
        match randomizer with
        | Some(rng_) -> rng_
        | None -> new System.Random(0)

    let m = matrix rows cols
    let range = max - min
    for i in 0..m.Values.Length-1 do
        m.Values[i] <- rng.NextDouble() * range + min

    m
    
/// More accurate exponential.
let exponential (x:float) = 
    Math.Exp(x)

/// Approximated exponential.
let approximateExponential (x:float) =
    // https://stackoverflow.com/questions/412019/math-optimization-in-c-sharp
    // https://www.schraudolph.org/pubs/Schraudolph99.pdf
    // TODO: Use generic numbers? (LanguagePrimitives.GenericOne)


    let clamp x a b = 
        if x > a && x < b then x 
        else if x < a then a
        else b

    let x' = clamp x -700.0 +700.0

    let tmp = (int64)(1512775.0 * x' + 1072693248.0 - 60801.0)
    let tmpShifted = tmp <<< 32
    let converted = BitConverter.Int64BitsToDouble(tmpShifted)

    //let test = exponential (x)
   
    converted
