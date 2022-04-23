
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
//    Stochastic Gradient Ascent
//

module StochasticGradientAscent


// namespaces

open MathNet


// functions

/// Performs gradient ascent using one sample.
let GradientAscentOneSample (objFuncGrad:Vector->_->Vector) (theta:Vector) sample (learningRate:float) =
    // Compute gradient at theta for given sample.
    let gradientAtSample = objFuncGrad theta sample

    // Climb aradient
    let theta' = theta + gradientAtSample * learningRate
    
    // Return new parameter vector.
    theta'


/// Performs gradient ascent for each sample in the sample sequence.
let GradientAscentSampleSequence objFuncGrad theta sampleSequence learningRate = 

    // Prepare a fold op. This takes an existing theta, 
    //  computes one sample gradient ascent, and returns 
    //  a new theta.
    let foldOp = fun theta' s -> GradientAscentOneSample objFuncGrad theta' s learningRate
    
    // Step through 
    let theta'' = sampleSequence |> Seq.fold foldOp theta

    theta''


/// Performs gradient ascent for multiple epochs over the provided sample set.
let GradientAscentSampleSet objFuncGrad theta sampleSet learningRate epochs randomizerSeed = 

    // Init randomizer if needed.
    let randomizer
        = match randomizerSeed with
            | Some(x) -> new System.Random(x)
            | _ -> null

    // Randomize if needed.
    let sampleSet'
        = match randomizer with
            | null -> sampleSet
            | _ -> sampleSet |> Seq.sortBy randomizer.Next |> Seq.cache

    // Prepare a fold op. This will compute gradient ascent for one epoch.
    let foldOp = fun theta' _ -> GradientAscentSampleSequence objFuncGrad theta' sampleSet' learningRate
    
    // Perform multiple gradient ascents.
    let theta'' = [|1..epochs|] |> Seq.fold foldOp theta

    theta''




