
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

open System
open System.Collections.Generic


///
/// An immutable set of distributions, indexed by observation. This is a 
/// generalization of conditional probability tables (CPTs), though most often 
/// this type is used in fact as a CPT.
///
type DistributionSet(?distributions) =

    // Internal storage of each distribution, indexed by observation.
    let distributions = defaultArg distributions Map.empty

    // Helper. Ensures the observation has no null variable names or missing 
    // values.
    let ensureStrictObservation (observation:Observation) =
        if observation |> Seq.exists (fun kvp -> Real.IsNaN kvp.Value) then
            failwith "Observation value cannot be missing." 
        else
            ()

    ///
    /// Constructs an empty distribution set.
    ///
    new() =
        DistributionSet(Map.empty)

    ///
    /// Constructs a distributionset with one distribution using an empty
    /// observation for its key.
    ///
    new(distribution:DiscreteDistribution) =
        let distributions = 
            Map.empty |> Map.add (new Observation()) distribution
        DistributionSet(distributions)

    ///
    /// Constructs a distribution set from a sequence of distributions.
    ///
    new(distributions:seq<System.Tuple<Observation,DiscreteDistribution>>) =
        let distributions =
            distributions
            |> Seq.map (fun t -> t.Item1, t.Item2)
            |> Map.ofSeq
        DistributionSet(distributions)
        
    ///
    /// Retrives the conditional distribution for the given observation.
    ///
    member public self.TryGetDistribution observation =

        // Check observation.
        ensureStrictObservation observation

        // Search the table.
        distributions |> Map.tryFind observation


    ///
    /// Stores a conditional distribution for the given observation.
    ///
    member public self.CloneAndAddDistribution (conditionedOnObservation)
                                               (distribution:DiscreteDistribution) =
        
        // Check observation.
        ensureStrictObservation conditionedOnObservation

        // Create new.
        new DistributionSet(distributions |> Map.add conditionedOnObservation distribution)

    ///
    /// Returns a sequence over the distributions in this table.
    ///
    member public self.EnumerateDistributions () =
        distributions
        |> Map.toSeq
