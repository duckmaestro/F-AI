
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
open FAI.Loaders


let doDemoBayesian = 

    // Load traffic data set.
    let dataSetTraffic = TrafficLoader.LoadFromFile "traffic.txt" :> IObservationSet

    // Print first sample's value for 'a5'.
    let firstSample = Option.get (dataSetTraffic.Next ())
    printfn "%f" (firstSample.TryValueForVariable "a5" |> Option.get)

    // Grab variable names.
    let variableNames = firstSample.VariableNames

    // Set state space.
    let stateSpace = 
        Space.Discrete 
            (Map.ofList [ 0.,"none" ; 1.,"light" ; 2.,"medium" ; 3.,"heavy" ])

    // Build a Bayesian network.
    let bn = new BayesianNetwork ()
    let prior = new DirichletDistribution (Map.ofList [ 0.,1. ; 1.,1. ; 2.,1. ; 3.,1. ])
    for variableName in variableNames do
        let dist = Distribution.ConditionalDiscrete (new ConditionalProbabilityTable ())
        let rv = new RandomVariable (variableName, stateSpace, dist)

        rv.Prior <- Some prior
        bn.AddVariable rv
    ()

    bn.GenerateStructure Random


    // Learn CPTs.
    bn.LearnDistributions dataSetTraffic

    // Test ordering.
    let ordering = bn.GetTopologicalOrdering ()

    // Test sampling
    let samples = [|0..20|] |> Seq.map (fun _ -> bn.Sample ()) |> Seq.toArray

    // Done.
    ()
