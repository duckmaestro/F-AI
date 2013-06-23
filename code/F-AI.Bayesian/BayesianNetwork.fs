
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


/// A continous variable.
type Real = System.Double

/// A possible or observed value of some random variable.
type VariableValue = Option<System.Single>

/// A human-readable label for a random variable value.
type VariableValueLabel = System.String

/// A human-readable name for a random variable.
type VariableName = System.String

/// A collection of observed values, indexed by random variable name.
type Observation = Dictionary<VariableName, VariableValue>


///
/// A discrete probability distribution.
///
type DiscreteDistribution() = 

    /// Internal storage of the probability masses, indexed by
    /// value.
    member private      self._Masses = 
        new Dictionary<VariableValue, Real>()

    /// Assign a probability mass to a particular value.
    member public       self.SetMass value mass =

        match value with
            | Some _    -> self._Masses.Item(value) <- mass
            | None      -> invalidArg "value" "Value must not be missing." 

    /// Get the probability mass of a particular value.
    member public       self.GetMass value =

        let mutable mass = 0.0
        if self._Masses.TryGetValue(value, ref mass) then 
            mass
        else 
            invalidArg "value" "The given value does not have a known mass."


///
/// A table of distributions, indexed by observation.
///
type ConditionalProbabilityTable() =
    
    /// Helper. Ensures the observation has no null variable names or missing values.
    let ensureStrictObservation (observation:Observation) =
        if observation.Keys |> Seq.exists (fun key -> key = null) then
            failwith "Missing variable name encountered in observation"
        else if observation.Values |> Seq.exists (fun value -> Option.isNone value) then
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
/// A random variable part of a Bayesian network.
///
type RandomVariable(name, network) = 
    let inEdges = new List<RandomVariable>()
    let outEdges = new List<RandomVariable>()
    let cpt = new ConditionalProbabilityTable()

    member public self.Name
        with get() = name
    member public self.BayesianNetwork
        with get() = network
    member public self.CPT
        with get() = cpt
    member public self.IncomingEdges
        with get() = inEdges :> seq<RandomVariable>
    member public self.OutgoingEdges
        with get() = outEdges :> seq<RandomVariable>
    

///
/// A Bayesian network
///
type BayesianNetwork() =
    let rvs = new List<RandomVariable>()

    member public self.RandomVariables
        with get() = rvs :> seq<RandomVariable>


