
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


module LearnDistributions

open System.Collections.Generic
open FAI.Bayesian

///
/// Learns a set of conditional distributions.
/// Assumes no missing information.
/// 
let public learnConditionalDistributions 
            (dependentVariable:RandomVariable) 
            (independentVariables:RandomVariable seq)
            (observationSet:IObservationSet) =
    
    //
    // Support functions.
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
        
    // Initializes a new occurrence counter. For each possible value
    // of the random variable, initializes a 0 count.
    let emptyOccurrenceCounter (rv:RandomVariable) =
        let permutations = rv.Space.Values

        let occurrenceCounter =
            permutations
            |> Seq.map (fun p -> p, 0)
            |> Map.ofSeq
        occurrenceCounter

    // Increases the occurrence counter's value for the
    // given occurrence value.
    let addOccurrence occurrenceCounter occurrenceValue =
        let oldCount = occurrenceCounter |> Map.find occurrenceValue
        let newCount = oldCount + 1
        occurrenceCounter |> Map.add occurrenceValue newCount

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
    let ivsNames = ivs |> Seq.map (fun rv -> rv.Name)

    // Enumerate permutations of independent variables.
    let parentConfigs = enumeratePermutations ivs
    let mutable dvCountsByParentConfig = 
        if (parentConfigs |> Seq.length) <> 0 then
            parentConfigs
            |> Seq.map (fun p -> p, emptyOccurrenceCounter dv)
            |> Map.ofSeq
        else
            Seq.singleton (new Observation(), emptyOccurrenceCounter dv)
            |> Map.ofSeq

    // Step over each observation.
    let mutable doLoop = true
    while doLoop do
        let observation0 = observationSet.Next ()
        if Option.isNone observation0 then
            doLoop <- false
        else
            let observation = observation0.Value

            // Prepare observation using parent nodes only.
            let ivsObservation = observation .&. ivsNames

            // Lookup counts for the parent config for this observation.
            let dvCountsForParentConfig = 
                dvCountsByParentConfig 
                |> Map.find ivsObservation

            // Lookup observation value for dependent variable.
            let valForDV = Option.get (observation.TryValueForVariable dv.Name)

            // Increase count for this observation
            let dvCountsForParentConfig' = addOccurrence dvCountsForParentConfig valForDV
            dvCountsByParentConfig <- dvCountsByParentConfig |> Map.add ivsObservation dvCountsForParentConfig'
        
    // Prepare conditional distributions.
    let distributions =
        let prior = defaultArg dv.Prior (new DirichletDistribution())

        dvCountsByParentConfig 
        |> Map.map (fun parentInstance dvCounts -> makeDistribution dvCounts prior)

    // Done.
    distributions
