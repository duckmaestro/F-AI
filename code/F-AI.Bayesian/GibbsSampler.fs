
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


module FAI.Bayesian.GibbsSampler

let getNextSample (variables:RandomVariable seq) (currentSample:Observation) (evidence:Observation) = 
    
    let mutable newSample = currentSample

    // For each variable in the network, generate a new value from the 
    // conditional distribution from defined by the remainder of the 
    // observation.
    for rv in variables do

        // Helper sampler.
        let getNewValue (fromSample:Observation) = 

            // Build the conditional distribution p( RV | BN-RV = sample).
            let distribution = new DiscreteDistribution ()

            // For each possible value of rv, find its conditional mass.
            for value in rv.Space.Values do

                let fromSampleWithValue = fromSample .+. (rv.Name,value)

                // Using the Markov blanket, we need to lookup conditional 
                // probabilities for each child, and for this node.

                // Child probabilities are p( CHILD | Pa(CHILD)+RV = sample+value )
                let mutable factors = [ ]

                for child in rv.Children do
                    let childValue = Option.get <| fromSample.TryValueForVariable child.Name
                    let childParentsValues = fromSample .&. (child.Parents |> Seq.map (fun p -> p.Name))
                    let childDistribution = Option.get <| child.Distributions.TryGetDistribution childParentsValues
                    let childProbability = Option.get <| childDistribution.GetMass childValue
                    factors <- childProbability :: factors 

                    ()

                // RV probability is p( RV = value | Pa(RV) = sample )
                let rvValue = value
                let rvParentsValues = fromSample .&. (rv.Parents |> Seq.map (fun p -> p.Name))
                let rvDistribution = Option.get <| rv.Distributions.TryGetDistribution rvParentsValues
                let rvProbability = Option.get <| rvDistribution.GetMass rvValue
                factors <- rvProbability :: factors

                // Compute & store unnormalized mass.
                let mass = factors |> List.fold (fun a f -> a * f) 1.
                distribution.SetMass value mass

            // Sample from our distribution.
            SimpleSampler.getSampleUnnormalized distribution

        // Is this variable in the evidence set.
        let rvNewValue =
            match evidence.TryValueForVariable rv.Name with
                | Some value    ->  value
                | None          ->  getNewValue newSample

        newSample <- newSample .+. (rv.Name,rvNewValue)
        
    // Done.
    newSample
