
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
//    Noisy-Or learner.
//

module NoisyOr


// namespaces

open System
open System.Linq
open System.Collections.Generic
open MathNet
open Primitives
open Classifiers


/// Calculates the probability of y given x and noisy-or parameters p.
let private ProbOfYGivenX (y:float) (x:Vector) (p:Vector) =
    assert (y = 1.0 || y = 0.0)
    assert (x.Count = p.Count)

    let n = x.Count
    let product
        = [|0..n-1|]
        |> Seq.map (fun i -> if x.[i] = 1.0 then (1.0 - p.[i]) else 1.0)
        |> Seq.reduce (fun a q -> a * q)

    if y = 1.0 then 1.0 - product else product



/// <summary>A binary value Noisy-Or classifier.</summary>
/// <remarks>
/// This class is (temporarily?) reusing Sample(+Label) and IClassifier,
/// due to some structural similarity, in spite of semantic dissimilarity.
///
/// TODO: Include this class in the overall refactoring toward types other than 
/// simply classifiers and predictors?
///
/// This model uses:   
///    X_i -> Z_i -> Y bayes net where Y is deterministic OR.
///    p(Z_i = 1 | X_i = 0) = 0
///    P(Z_i = 1 | X_i = 1) = p_i

/// </remarks>
type NoisyOrClassifier(?maxIterations, ?haltIfConvergent, ?initialProbabilities:Vector, ?randomSampleCountTraining, ?randomSampleCountMeasuring) =

    let maxIterations = defaultArg maxIterations 1000
    let haltIfConvergent = defaultArg haltIfConvergent true
    let randomSampleCountTraining = defaultArg randomSampleCountTraining 0
    let randomSampleCountMeasuring = defaultArg randomSampleCountMeasuring 0
   
    let mutable numFeatures:int = 0
    let mutable initialProbabilities = defaultArg initialProbabilities null
    let mutable samples:seq<Sample> = null
    let mutable pAfterEachStep:List<Vector> = null
    let mutable llAfterEachStep:List<float> = null
    let randomizer = new Random(0)
    
    let LogLikelihoodsAfterEachTrainingStep () = 
        llAfterEachStep

    let ParametersAfterEachTrainingStep () =
        pAfterEachStep

    interface IClassifier with
        member self.Train (samples0:seq<Sample>) =
    
            samples <- samples0
            numFeatures <- samples |> Seq.head |> fun x -> x.Features.Count
            initialProbabilities <- 
                if initialProbabilities = null then 
                    // If initial probabilities were not provided, initialize to a uniform value of 
                    // 1 / numFeatures.
                    vector ([|1.0..(float numFeatures)|] |> Seq.map (fun x -> 1.0 / (float numFeatures)))
                else 
                    initialProbabilities

            assert (maxIterations >= 1)
            assert (initialProbabilities.Count = numFeatures)
    
            pAfterEachStep <- new List<Vector>()
            llAfterEachStep <- new List<float>()

            let samplesRandomized = samples |> Seq.sortBy (fun e -> randomizer.NextDouble()) |> Seq.cache

            // Decide samples to use for learning.
            let samplesForLearning = 
                if randomSampleCountTraining > 0 then
                    samplesRandomized
                    |> Seq.take randomSampleCountTraining
                    |> Seq.cache
                else
                    samples

            // Decide samples to use for measuring.
            let samplesForMeasuring =
                if randomSampleCountMeasuring > 0 then
                    samplesRandomized
                    |> Seq.skip randomSampleCountTraining
                    |> Seq.take randomSampleCountMeasuring
                    |> Seq.cache
                else
                    samplesForLearning
    
            // Count the occurrences of 1 for each Xi. Save to T.  
            let mutable Ti = vector (Array.zeroCreate numFeatures)
            for sample in samplesForLearning do
                let sampleXs = sample.Features
                
                // Check sample.
                let sampleXsAsSeq = sampleXs :> seq<float>
                if false = (sampleXsAsSeq |> Seq.filter (fun x -> x <> 0.0 && x <> 1.0) |> Seq.isEmpty) then
                    failwith "Detected a non-binary valued sample."
                
                // Ok. Add.
                Ti <- Ti + sampleXs

            // Computes the normalized conditional log likelihood.
            let LogLikelihood (samples:seq<Sample>) p =
                let mutable L = 0.0
                for sample in samples do
                    let y = float sample.Label
                    let x = sample.Features

                    let prob = ProbOfYGivenX y x p
                    let ll = Math.Log prob
                    L <- L + ll
                L <- L / float(samples |> Enumerable.Count)
                L

            // Seed our p list with the given initial p.
            pAfterEachStep.Add initialProbabilities

            // EM iteration
            let mutable isConvergent = false
            for iter in [|1..maxIterations|] do
                if isConvergent = false then

                    // The latest p.
                    let pCurr = pAfterEachStep |> Enumerable.Reverse |> Seq.head

                    // For our record keeping, calculating log likelihood.
                    let ll = LogLikelihood samplesForMeasuring pCurr
                    llAfterEachStep.Add ll

                    // Initialize a new p.
                    let mutable pNew = vector (Array.zeroCreate numFeatures)
        
                    // Calculate new pi for each Xi
                    for i in [|1..numFeatures|] do
            
                        let mutable piNew = 0.0
                        let occurancesOfIeq1 = Ti.[i-1]

                        if occurancesOfIeq1 = 0.0 then
                            // If there are no occurrences of xi = 1, thus we don't have a way to infer
                            // p(Z_i=1|X_i=1). Let's try to pick a reasonable value, like 1/numFeatures.
                            piNew <- 1.0 / (float numFeatures)
                        else
                            // Sum over all samples
                            for sample in samplesForLearning do
                
                                let y = float sample.Label
                                let x = sample.Features
                                let xi = x.[i-1]
                                let piCurr = pCurr.[i-1]

                                let numerator = y * xi * piCurr
                                let denominator = ProbOfYGivenX 1.0 x pCurr
                                let denominatorAdjusted = if denominator = 0.0 then Double.Epsilon else denominator
                            
                                piNew <- piNew + (numerator / denominatorAdjusted)        
        
                            // Normalize
                            piNew <- piNew / occurancesOfIeq1

                        // Assign
                        pNew.[i-1] <- piNew

                    // Record new p
                    pAfterEachStep.Add pNew

                    // Are we convergent?
                    if haltIfConvergent = true && llAfterEachStep.Count >= 2 then
                        // todo: optimize this by memoizing the last pair.
                        let llPrev2 = llAfterEachStep |> Seq.pairwise |> Seq.reduce (fun _ x -> x)
                        let ll1 = fst llPrev2
                        let ll2 = snd llPrev2
                        if Math.Abs(ll1 - ll2) < 0.00001 then
                            isConvergent <- true
            // end EM iteration

            // Compute one more log likelihood.
            let pCurr = pAfterEachStep |> Enumerable.Reverse |> Seq.head

            // For our record keeping, calculating log likelihood.
            let LL = LogLikelihood samplesForMeasuring pCurr
            llAfterEachStep.Add LL

            ()

        member self.Classify point =
            let features = point.Features
            let pValues = pAfterEachStep.Item (pAfterEachStep.Count - 1)
            let probYeq0 = ProbOfYGivenX 0.0 features pValues
            let probYeq1 = ProbOfYGivenX 1.0 features pValues
            
            let classification = if probYeq0 > probYeq1 then 0 else 1
            
            classification
