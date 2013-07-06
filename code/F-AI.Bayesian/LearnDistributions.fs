
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


module internal LearnDistributions

open System.Collections.Generic
open FAI.Bayesian

///
/// Learns a set of conditional distributions.
/// Assumes no missing information.
/// 
let learnConditionalDistributions (dependentVariable:RandomVariable) independentVariables (observationSet:IObservationSet) =
    
    //
    // Support functions.
    //

    // From a list of random variables, generates each possible instantiation.
    let rec enumeratePermutations' (variableList:list<RandomVariable>) : Real list list =
        match variableList with
        | [ ]       ->  [ ]
        | v :: vs   ->  let v1_perms =  match v.Space with 
                                        | Discrete xs   ->  xs |> Seq.map (fun x -> [x]) |> List.ofSeq
                                        | _             ->  failwith "Continous variables not supported."
                        let vs_perms =  enumeratePermutations' (variableList |> List.tail)
                        
                        let cross = 
                            v1_perms
                            |> List.collect
                                (fun v1 -> vs_perms |> List.map (fun vs -> List.append v1 vs))
                        cross
    let enumeratePermutations variableList =
        let barePermutations = enumeratePermutations' variableList
        let namedPermutations =
            barePermutations
            |> Seq.map (
                fun p -> 
                    p 
                    |> Seq.zip (variableList |> Seq.map (fun rv -> rv.Name))
                    |> Map.ofSeq
                    |> (fun x -> new Observation(x))
                )
        namedPermutations
        

    // Initializes a new occurrence counter. For each possible value
    // of the random variable, initializes a 0 count.
    let emptyOccurrenceCounter (rv:RandomVariable) =
        let permutations = 
            match rv.Space with 
            | Discrete vals ->  vals 
            | _             ->  failwith "Variable must be discrete-valued."

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


    //let subtract obs1 obs2 =

    //
    // Algorithm.
    //

    let dv = dependentVariable
    let ivs = independentVariables

    // Enumerate permutations of independent variables.
    let parentConfigs = enumeratePermutations ivs
    let mutable dvCountsByParentConfig = 
        parentConfigs
        |> Seq.map (fun p -> p, emptyOccurrenceCounter dv)
        |> Map.ofSeq

    // Step over each observation.
    let mutable observation0 = observationSet.Next ()
    while Option.isSome observation0 do
        let observation = observation0.Value

        // Prepare observation using parent nodes only.
        let ivsObservation = observation - dv.Name

        // Lookup counts for the parent config for this observation.
        let dvCountsForParentConfig = 
            dvCountsByParentConfig 
            |> Map.find ivsObservation

        // Lookup observation value for dependent variable.
        let valForDV = Option.get (observation.TryValueForVariable dv.Name)

        // Increase count for this observation
        let dvCountsForParentConfig' = addOccurrence dvCountsForParentConfig valForDV
        dvCountsByParentConfig <- dvCountsByParentConfig |> Map.add ivsObservation dvCountsForParentConfig'
        

    // Todo:
    failwith "Not implemented yet."
