
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
/// A table of distributions, indexed by observation.
///
type ConditionalProbabilityTable() =
    
    // Ensures the observation has no null variable names or missing values.
    let ensureStrictObservation (observation:Observation) =
        if observation |> Seq.exists (fun kvp -> Real.IsNaN kvp.Value) then
            failwith "Observation value cannot be missing." 
        else
            ()


    // Internal storage of each distribution, indexed by observation.
    let mutable table = Map.empty // Observation * DiscreteDistribution>();


    ///
    /// Retrives the conditional distribution for the given observation.
    ///
    member public   self.TryGetConditionalDistribution observation =

        // Check observation.
        ensureStrictObservation observation

        // Search the table.
        table |> Map.tryFind observation


    ///
    /// Stores a conditional distribution for the given observation.
    ///
    member public   self.SetConditionalDistribution observation (distribution:DiscreteDistribution) =
        
        // Check observation.
        ensureStrictObservation observation

        // Add.
        table <- table |> Map.add observation distribution

        // Done.
        ()
