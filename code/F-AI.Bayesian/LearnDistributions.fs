
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


module FAI.Bayesian.LearnDistributions

open System.Collections.Generic
open FAI.Bayesian

///
/// Learns a conditional distributions for a dependent variable.
/// Assumes no missing information.
/// 
let public learnConditionalDistribution 
            (dependentVariable:RandomVariable) 
            (independentVariables:RandomVariable seq)
            (sufficientStatistics:SufficientStatistics) =
    
    //
    // Helper functions.
    //

    // From a list of random variables, generates each possible instantiation
    // as a sequence values in corresponding order to the random variable list.
    let rec enumeratePermutations' (variableList:list<RandomVariable>) =
        match variableList with
        | [ ]       ->  [ ]
        | v :: vs   ->  // Promote each value to a singleton list.
                        let v1_perms =  v.Space.Values |> Seq.map (fun x -> [x]) |> List.ofSeq 
                        
                        // Recurse.
                        let vs_perms =  enumeratePermutations' (variableList |> List.tail) 
                        
                        // Build cross product.
                        let cross = 
                            match vs_perms with
                            | [ ]       ->  v1_perms
                            | vs_perms  ->  v1_perms
                                            |> List.collect
                                                (fun v1_perm -> vs_perms |> List.map (fun vs -> List.append v1_perm vs))
                        // Done.
                        cross
    
    // From a list of random variables, generates each possible instantiation
    // in an Observation.
    let enumeratePermutations variables =
        let variablesAsList = variables |> List.ofSeq
        let barePermutations = enumeratePermutations' variablesAsList
        let namedPermutations =
            barePermutations
            |> Seq.map (
                fun p -> 
                    p 
                    |> Seq.zip (variablesAsList |> Seq.map (fun rv -> rv.Name))
                    |> Map.ofSeq
                    |> (fun x -> new Observation(x))
                )
        namedPermutations

    // Given a map of variable values to counts (which may be fractional due to 
    // effects of a prior), forms a discrete distribution.
    let makeDistribution (counts:Map<EventValue,float>) =
        let totalFromData = counts |> Map.fold (fun s k v -> s + float v) 0.0
        let total = totalFromData
        
        // Not enough data and no prior.
        if total = 0.0 then 
            None

        // We have enough data and/or a prior.
        else
            let mutable distribution = Map.empty
            for kvp in counts do
                let rvValue = kvp.Key
                let rvValueCount = kvp.Value
                let mass = rvValueCount / total
                distribution <- distribution |> Map.add rvValue mass
            Some (new DiscreteDistribution(distribution))


    //
    // Algorithm.
    //

    let dv = dependentVariable
    let ivs = independentVariables

    // Enumerate permutations of independent variables.
    let parentConfigs = enumeratePermutations ivs
        
    // Prepare conditional distributions.
    let distributions =
        let parentConfigs = 
            if parentConfigs |> Seq.length = 0 then
                Seq.singleton (new Observation())
            else
                parentConfigs

        let countConditionalObservations (fixedCondition:Observation) =
            dv.Space.Values
            |> Seq.map (fun v -> v, fixedCondition .+. (dv.Name,v))
            |> Seq.map (fun (v,q) -> v, sufficientStatistics.GetObservationCount q)
            |> Map.ofSeq

        let distributions =
            parentConfigs
            |> Seq.map (fun config -> config, countConditionalObservations config)
            |> Seq.map (fun (config,count) -> config, makeDistribution count)
            |> Map.ofSeq

        distributions

    // Done.
    distributions


///
/// Learns the distributions for the given variables, according to the given
/// sufficient statistics and the network structure.
/// 
let public learnDistributions 
            (variables:Map<Identifier,RandomVariable>) 
            (sufficientStatistics:SufficientStatistics) = 

    let mutable results = Map.empty

    // For each random variable, learn its conditional distributions.
    for dv in variables |> Map.toSeq |> Seq.map (fun (k,v) -> v) do
        let ivs = dv.Parents |> Seq.map (fun p -> variables |> Map.find p)

        // Learn conditional distributions for this variable.
        let conditionalDistributions = 
            learnConditionalDistribution dv ivs sufficientStatistics

        // Copy distributions into a set.
        let cpt =
            conditionalDistributions
            |> Map.map (fun k v -> v.Value)
            |> fun ds -> new DistributionSet(ds)
        
        // Associate CPT with this variable.
        results <- results |> Map.add dv.Name cpt

    results