
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



/// Describes a space of permitted values.
type RandomVariableSpace = 
    | Continuous of Real * Real     // The min, max bounds on the region.
    | Discrete of array<Real>       // A list of valid values.


///
/// A table of distributions, indexed by observation.
///
type ConditionalProbabilityTable() =
    
    /// Helper. Ensures the observation has no null variable names or missing values.
    let ensureStrictObservation (observation:Observation) =
        if observation.Keys |> Seq.exists (fun key -> key = null) then
            failwith "Missing variable name encountered in observation"
        else if observation.Values |> Seq.exists (fun value -> Real.IsNaN value) then
            failwith "Observation value cannot be missing."
        else
            ()

    /// Helper. Checks whether two observations are equivalent.
    let isMatchingObservation (obs1:Observation) (obs2:Observation) =
        if (obs1.Count <> obs2.Count) then
            false
        else
            let sorter (kvp:KeyValuePair<_,_>) = kvp.Key

            let zippedPairs = 
                (obs1 |> Seq.sortBy sorter) 
                |> Seq.zip <| 
                (obs2 |> Seq.sortBy sorter)

            let firstMismatch = 
                zippedPairs 
                |> Seq.tryFind 
                    (fun (p1,p2) -> p1.Key <> p2.Key || p1.Value <> p2.Value)

            let doesMatch = Option.isNone firstMismatch
            doesMatch



    /// Internal storage of each distribution, indexed by observation.
    member private  self._ConditionalDistributions = 
        new List<Observation * DiscreteDistribution>();

    /// Retrives the conditional distribution for the given observation.
    member public   self.TryGetConditionalDistribution observation =

        // Check observation.
        ensureStrictObservation observation

        // Search the table.
        let record = 
            self._ConditionalDistributions 
            |> Seq.tryFind
                (fun p -> isMatchingObservation (fst p) observation)

        // Return the distribution if found.
        match record with
            | Some(match')  -> Some (snd match')
            | None          -> None


    /// Stores a conditional distribution for the given observation.
    member public   self.SetConditionalDistribution observation distribution =
        
        // Check observation.
        ensureStrictObservation observation

        // Is there an existing record?
        let recordIndex = 
            self._ConditionalDistributions
            |> Seq.tryFindIndex
                (fun p-> isMatchingObservation (fst p) observation)

        // Remove existing.
        match recordIndex with
            | Some(index)   -> self._ConditionalDistributions.RemoveAt(index)
            | None          -> ()


        // Add new.
        self._ConditionalDistributions.Add (observation,distribution)

        // Done.
        ()


///
/// A random variable.
/// Exposes mutable dependency list and mutable distribution.
///
type public RandomVariable(name', space', distribution') =
    
    let mutable distribution = distribution'
    let mutable name = name'
    let mutable space = space'
    let dependencies = new HashSet<RandomVariable>()

    member public self.Name 
        with get() : String = name

    member public self.Space 
        with get() : RandomVariableSpace = space

    member public self.Distribution 
        with get() : DiscreteDistribution = distribution

    member public self.AddDependency rv = 
        dependencies.Add rv

    member public self.RemoveDependency rv =
        dependencies.Remove rv |> ignore

    member public self.Dependencies
        with get() = dependencies :> seq<_>
    
//    interface IComparable 
//        with member self.CompareTo other = 
//            match other with
//            | :? RandomVariable as other' -> String.Compare(self.Name, other'.Name)
//            | _                           -> -1


        

