
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

    // Given a map of variable values to counts, forms a 
    // discrete distribution.
    let makeDistribution (counts:Map<Real,Integer>) (prior:DirichletDistribution) =
        let totalFromData = counts |> Map.fold (fun s k v -> s + float v) 0.0
        let totalFromPrior = prior.Parameters |> Seq.sumBy (fun kvp -> kvp.Value)
        let total = totalFromData + totalFromPrior
        
        // Not enough data and no prior.
        if total = 0.0 then 
            None

        // We have enough data and/or a prior.
        else
            let distribution = new DiscreteDistribution ()
            for kvp in counts do
                let rvValue = kvp.Key
                let rvValueCount = float kvp.Value
                let rvPrior = float (defaultArg (prior.GetParameter rvValue) 0.)
                let mass = (rvValueCount + rvPrior) / total
                distribution.SetMass rvValue mass
            Some distribution


    //
    // Algorithm.
    //

    let dv = dependentVariable
    let ivs = independentVariables

    // Enumerate permutations of independent variables.
    let parentConfigs = enumeratePermutations ivs
        
    // Prepare conditional distributions.
    let distributions =
        let prior = defaultArg dv.Prior (new DirichletDistribution())
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
            |> Seq.map (fun (config,count) -> config, makeDistribution count prior)
            |> Map.ofSeq

        distributions

    // Done.
    distributions


///
/// Learns the distributions for the given variables, according to the given
/// observation set and the variables' structural parents.
/// Mutates the distribution on each variable.
/// 
let public learnDistributions 
            (variables:seq<RandomVariable>) 
            (sufficientStatistics:SufficientStatistics) = 

    // For each random variable, learn its conditional distributions.
    for dv in variables do
        let ivs = dv.Parents

        // Learn conditional distributions for this variable.
        let conditionalDistribution = 
            learnConditionalDistribution dv ivs sufficientStatistics

        // Copy distributions into a CPT.
        let cpt = new DistributionSet()
        for distribution in conditionalDistribution do
            let parentInstantiation = distribution.Key
            let distribution = distribution.Value

            match distribution with
                | Some d    ->  cpt.SetConditionalDistribution parentInstantiation d
                | _         ->  failwith "A neccessary distribution was not learned."                 

        // Associate CPT with this variable.
        dv.Distributions <- cpt

    ()