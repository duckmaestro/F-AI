
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


namespace FAI.Bayesian

open System.Collections.Generic

///
/// Contains a inference query against a Bayesian network, and its results.
///
type public InferenceQuery (network:BayesianNetwork, evidence) =

    // Memoized posteriors.
    let mutable posteriors = Map.empty
    
    let mutable previousParticle = new Observation()
    let mutable sampleCountRaw = 0
    let mutable sampleCountKept = 0
    let mutable valueCounts = Map.empty
    let mutable warmupSize = 250
    let mutable particleSeparation = 1

    // Random orderings of the variables.
    let variableOrders
        = {1..100} 
        |> Seq.map (fun _ -> network.VariablesOrdered 
                            |> List.sortBy (fun _ -> SimpleSampler.getSampleUniformNatural 100))
        |> Seq.toArray


    // Constructor
    do
        if evidence :> obj = null then
            invalidArg "evidence" "Evidence must be non-null."

    ///
    /// The target Bayesian network for this query.
    ///
    member public self.Network 
        with get() : BayesianNetwork    =   network

    ///
    /// The evidence set for this query.
    ///
    member public self.Evidence
        with get () : Observation   = evidence

    ///
    /// The resulting sample distributions.
    ///
    member public self.Results
        with get () =   posteriors

    ///
    /// The size of the warmup period. The warmup period is unaffected by choice of particle
    /// separation.
    ///
    member public self.WarmupSize
        with get ()     =   warmupSize
        and set value   =   warmupSize <- value

    ///
    /// The minimumum distance in sequence between samples for samples
    /// that will be retained and used.
    ///
    member public self.ParticleSeparation
        with get ()     =   particleSeparation
        and set value   =   particleSeparation <- value

    ///
    /// The refinement count. Returns zero until the warmup period is past.
    ///
    member public self.RefinementCount
        with get ()     =   sampleCountKept


    ///
    /// Computes or refines results for this query.
    ///
    member public self.RefineResults steps =

        // Ensure we generate enough samples for at least one period of sample
        // separation, ending on a retained sample.
        let separationPeriod = particleSeparation + 1
        let steps = 
            if steps <= separationPeriod then separationPeriod
            else steps
        
        let rvs = network.Variables
        let getVariableOrder () = variableOrders.[SimpleSampler.getSampleUniformNatural variableOrders.Length - 1]

        // Init with first particle.
        if previousParticle.IsEmpty then
            let rvsNames = rvs |> Seq.map (fun kvp -> kvp.Key)
            let rvsSpaces = rvs |> Seq.map (fun kvp -> kvp.Value.Space)
            let particleValues =
                SimpleSampler.getSampleUniformMultidimensional rvsSpaces
            let firstParticle =
                particleValues 
                |> Seq.zip rvsNames
                |> Map.ofSeq
                |> fun p -> new Observation(p)

            previousParticle <- firstParticle

        // Warmup.
        if sampleCountRaw < warmupSize then
            for _ in { sampleCountRaw .. warmupSize } do
                let order = getVariableOrder ()
                previousParticle <- GibbsSampler.getNextSample rvs previousParticle self.Evidence order
                sampleCountRaw <- sampleCountRaw + 1
                ()

        // Generate new particles.
        for localStepNumber in { 1 .. steps } do
            let order = getVariableOrder ()
            let nextParticle = GibbsSampler.getNextSample rvs previousParticle self.Evidence order
            previousParticle <- nextParticle
            sampleCountRaw <- sampleCountRaw + 1

            // Keep the particle. Increase counts.
            if localStepNumber % separationPeriod = 0 then            
                for vv in nextParticle do
                    let countsForVariable = valueCounts |> Map.tryFind vv.Key
                    let countsForVariable = defaultArg countsForVariable Map.empty
                    let countForValue = countsForVariable |> Map.tryFind vv.Value
                    let countForValue = defaultArg countForValue 0

                    let countsForVariable = countsForVariable |> Map.add vv.Value (countForValue + 1)
                    valueCounts <- valueCounts |> Map.add vv.Key countsForVariable

                sampleCountKept <- sampleCountKept + 1

        // Recompute marginal distributions.
        let sampleCount = float sampleCountKept
        for rv in rvs |> Seq.map (fun kvp -> kvp.Value) do            
            
            let rvValueInEvidence = evidence.TryValueForVariable rv.Name
            let rvValueCountsSampled = valueCounts |> Map.find rv.Name

            let postieror = 
                
                // If this variable has a value from evidence.
                if Option.isSome rvValueInEvidence then
                    let mutable distribution = Map.empty
                    let rvValueInEvidence = Option.get <| rvValueInEvidence
                    for valueInSpace in rv.Space.Values do
                        if valueInSpace = rvValueInEvidence then
                            distribution <- distribution |> Map.add valueInSpace 1.
                        else
                            distribution <- distribution |> Map.add valueInSpace 0.
                    new DiscreteDistribution(distribution)

                // If this variable does not have a value from evidence.
                else
                    // Start with 0 counts.
                    let mutable distribution = 
                        rv.Space.Values 
                        |> Seq.map (fun v -> v, 0.)
                        |> Map.ofSeq 

                    // Add non-zero counts.
                    for vc in rvValueCountsSampled do
                        let mass = (float vc.Value) / sampleCount
                        distribution <- distribution |> Map.add vc.Key mass

                    assert ((distribution |> Seq.length) = (rv.Space.Values |> Seq.length))
            
                    new DiscreteDistribution(distribution)

            // Store public results.
            posteriors <- posteriors |> Map.add rv.Name postieror

        // Done.
        ()

