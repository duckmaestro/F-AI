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


module FAI.Bayesian.SimpleSampler

// Randomizer.
let private randomizer = new System.Random (10)

///
/// Generates a sample from a simple distribution.
///
let getSample (distribution:DiscreteDistribution) = 
    
    let point = randomizer.NextDouble ()
    let mutable accumulation = 0.
    let mutable event = None

    for eventMass in distribution.MassesSorted do
        if event.IsSome then ()
        else
            accumulation <- accumulation + eventMass.Value
            if point <= accumulation then
                event <- Some eventMass.Key

    Option.get <| event

///
/// Generates a sample from an unnormalized, simple distribution.
///
let getSampleUnnormalized (distribution:DiscreteDistribution) =

    let sum = distribution.Masses |> Seq.sumBy (fun valueMass -> valueMass.Value)

    let point = randomizer.NextDouble () * sum
    let mutable accumulation = 0.
    let mutable event = None

    for eventMass in distribution.Masses do
        if event.IsNone then
            accumulation <- accumulation + eventMass.Value
            if point <= accumulation then
                event <- Some eventMass.Key

    Option.get <| event

///
/// Samples an integer value between 1 and n using a uniform distribution.
///
let getSampleUniformNatural n = 
    let value = randomizer.Next n
    value + 1

///
/// Samples a value from the provided space using a uniform distribution.
///
let getSampleUniform (space:Space) = 
    let i = getSampleUniformNatural space.Size
    let value = space.Values |> Seq.skip (i-1) |> Seq.head
    value

/// Samples a multidimensional value from the provided spaces using uniform,
/// independent distributions.
let getSampleUniformMultidimensional (space:seq<Space>) =
    space
    |> Seq.map getSampleUniform
    |> List.ofSeq

