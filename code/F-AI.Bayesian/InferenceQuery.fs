
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

    // Memoized value of the current best estimate for marginal distributions.
    let mutable posteriors = Map.empty

    // Complete history of particles encountered for this query.
    let mutable particleHistory = [ ]

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
    /// The refinement count.
    ///
    member public self.RefinementCount
        with get () =   particleHistory.Length

    ///
    /// Computes or refines results for this query.
    ///
    member public self.RefineResults steps =

        let rvs = network.GetTopologicalOrdering ()

        // Init with first particle.
        if particleHistory = [ ] then
            let firstParticle = (ForwardSampler.getSample rvs)
            particleHistory <- [ firstParticle ]

        // Generate new particles.
        for _ in { 1..steps } do
            let nextParticle = GibbsSampler.getNextSample rvs particleHistory.Head self.Evidence
            particleHistory <- nextParticle :: particleHistory

        // Recompute marginal distributions.
        for rv in rvs do
            let valueCounts = 
                particleHistory
                |> Seq.map (fun p -> Option.get (p.TryValueForVariable rv.Name))
                |> Seq.groupBy (fun value -> value)
                |> Seq.map (fun (key,group) -> (key, group |> Seq.length |> float))
                |> Seq.toArray

            let totalCount = float particleHistory.Length
            
            // Build a posterior distribution.
            let posterior = new DiscreteDistribution()
            for (value,count) in valueCounts do
                let mass = count / totalCount
                posterior.SetMass value mass
            
            // Store distribution.
            posteriors <- posteriors |> Map.add rv.Name posterior

        // Done.
        ()

