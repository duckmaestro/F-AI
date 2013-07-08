
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


module Bayesian

// namespaces

open System
open FAI.Bayesian

open DataLoaderTraffic


let doDemoBayesian = 

    // Load traffic data set.
    let dataSetTraffic = DataLoaderTraffic.LoadFromFile "traffic.txt" :> IObservationSet

    // Print first sample's value for 'a5'.
    let firstSample = Option.get (dataSetTraffic.Next ())
    printfn "%f" (firstSample.TryValueForVariable "a5" |> Option.get)

    // Grab variable names.
    let variableNames = firstSample.VariableNames

    // Set state space.
    let stateSpace = RandomVariableSpace.Discrete [| 0. .. 3. |]

    // Build a Bayesian network.
    let bn = new BayesianNetwork ()
    for variableName in variableNames do
        let dist = Distribution.ConditionalDiscrete (new ConditionalProbabilityTable ())
        let rv = new RandomVariable (variableName, stateSpace, dist)
        bn.AddVariable rv
    ()

    // Connections (arbitrary test).
    for variablePair in 
        bn.Variables 
        |> Seq.skip 1
        |> Seq.sortBy (fun v -> v.Name)
        |> Seq.pairwise 
        |> Seq.mapi (fun i v -> i,v)
        |> Seq.filter (fun i_v -> fst i_v % 2 = 0)
        |> Seq.map (fun i_v -> snd i_v) 
        do
        
        //let v0 = bn.Variables |> Seq.head
        let v1 = fst variablePair
        let v2 = snd variablePair
        
        //v2.AddDependency v0
        v2.AddDependency v1
        



    // Learn CPTs.
    bn.LearnDistributions dataSetTraffic

    // Done.
    ()
