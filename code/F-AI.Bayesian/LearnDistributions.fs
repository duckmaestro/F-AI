
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
let learnConditionalDistributions conditionalVariable independentVariables observationSet =
    
    // Helpers
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
                )
        namedPermutations
        

    // Enumerate permutations of independent variables.
    let parentPermutations = enumeratePermutations independentVariables

    // Todo:
    failwith "Not implemented yet."
