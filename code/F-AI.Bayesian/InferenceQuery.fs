
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
type public InferenceQuery (network, evidence) =

    // Memoized posteriors.
    let mutable posteriors = Map.empty
    
    let mutable particleHistory = [ ]
    let mutable warmupSize = 100
    let mutable particleSeparation = 1

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
    /// The results.
    ///
    member public self.Results
        with get () =   posteriors

    ///
    /// The size of the warmup period.
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
        with get () =   
            let rawHistoryLength = particleHistory.Length
            if rawHistoryLength <= warmupSize then
                0
            else
                rawHistoryLength - warmupSize

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

        // Init with first particle.
        if particleHistory = [ ] then
            let firstParticle = (ForwardSampler.getSample rvs)
            particleHistory <- [ firstParticle ]

        // Generate new particles.
        let mutable previousParticle = particleHistory.Head
        for step in { 1..steps } do
            let nextParticle =
                GibbsSampler.getNextSample rvs previousParticle self.Evidence

            if step % separationPeriod = 0 then            
                particleHistory <- nextParticle :: particleHistory
            else
                ()

            previousParticle <- nextParticle

        // Decide how many recent particles to use.
        let numParticlesToUse = 
            if particleHistory.Length > warmupSize * 2 then
                particleHistory.Length - warmupSize
            else if particleHistory.Length > warmupSize then
                warmupSize
            else
                particleHistory.Length
            
        // Recompute marginal distributions.
        for rv in rvs do            
            
            let rvValueInEvidence = evidence.TryValueForVariable rv.Name

            let postieror = 
                
                // If this variable has a value from evidence.
                if Option.isSome rvValueInEvidence then
                    let distribution = new DiscreteDistribution()
                    let rvValueInEvidence = Option.get <| rvValueInEvidence
                    for valueInSpace in rv.Space.Values do
                        if valueInSpace = rvValueInEvidence then
                            distribution.SetMass valueInSpace 1.
                        else
                            distribution.SetMass valueInSpace 0.
                    distribution

                // If this variable does not have a value from evidence.
                else

                    // Grab the prior.
                    let prior = defaultArg rv.Prior (new DirichletDistribution())

                    // Prepare count query.
                    let valueCounts = // A sequence of value,count tuples.
                        particleHistory
                        |> Seq.take numParticlesToUse
                        |> Seq.map (fun p -> Option.get (p.TryValueForVariable rv.Name))
                        |> Seq.groupBy (fun value -> value)
                        |> Seq.map (fun (key,group) -> (key, group |> Seq.length |> float))
                        |> Seq.cache
            
                    // Incorporate prior distribution.
                    let valueCounts' = 
                        seq { 
                            for valueInSpace in rv.Space.Values do    
                                let countFromParticles = valueCounts |> Seq.tryFind (fun (v,k) -> v = valueInSpace)
                                let countFromPrior = defaultArg (prior.GetParameter (valueInSpace)) 0.
                        
                                let adjustedCount = 
                                    match countFromParticles with
                                        | None          ->  countFromPrior
                                        | Some parVC    ->  snd parVC + countFromPrior

                                yield valueInSpace,adjustedCount
                        }
                        |> Seq.toArray

                    // Adjusted value count.
                    let totalCount = 
                        (float numParticlesToUse)
                        + (prior.Parameters |> Seq.sumBy (fun kvp -> kvp.Value))
            
                    // Build a posterior distribution from particle value counts.
                    let posterior = new DiscreteDistribution()
                    for (value,count) in valueCounts' do
                        let mass = count / totalCount
                        posterior.SetMass value mass

                    assert ((posterior.Masses |> Seq.length) = (rv.Space.Values |> Seq.length))
            
                    posterior


            // Store postieror for this variable
            posteriors <- posteriors |> Map.add rv.Name postieror

        // Done.
        ()

