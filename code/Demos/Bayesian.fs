
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
    let firstSample = dataSetTraffic |> Seq.head
    printfn "%f" (firstSample.TryValueForVariable "a5" |> Option.get)

    // Grab variable names.
    let variableNames = dataSetTraffic.Variables |> Seq.map (fun kvp -> kvp.Key)

    // Define prior parameters.
    let prior = new DirichletDistribution (Map.ofList [ 0.,1. ; 1.,1. ; 2.,1. ; 3.,1. ])
    let priors = 
        variableNames 
        |> Seq.map (fun name -> name, prior)
        |> Map.ofSeq

    // Prepare sufficient statistics object.
    let sufficientStatistics = new SufficientStatistics(dataSetTraffic, priors)
  
    // Build a Bayesian network.
    let bn = new BayesianNetwork "Traffic"
    for variableName in variableNames do
        let space = dataSetTraffic.Variables |> Map.find variableName
        let rv = new RandomVariable(variableName, space)
        bn.AddVariable rv
    ()

    // Learn a tree structure.
    bn.LearnStructure (sufficientStatistics, Tree)

    // Learn CPTs.
    bn.LearnDistributions sufficientStatistics

    // Test topological ordering.
    let ordering = bn.VariablesOrdered

    // Test sampling
    let samples = { 0..20 } |> Seq.map (fun _ -> bn.Sample ()) |> Seq.toArray

    // Done.
    ()
