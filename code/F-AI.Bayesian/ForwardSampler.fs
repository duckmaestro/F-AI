
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


module FAI.Bayesian.ForwardSampler

// Randomizer.
let private randomizer = new System.Random (0)

///
/// Generates a sample.
///
let getSample (variables:RandomVariable seq) = 
    
    // TODO: Enforce/check a topological ordering on incoming variables.

    let mutable sample = new Observation ()
    let ordering = variables
            
    for rv in ordering do
        let sampleParentsOnly = sample .&. (rv.Parents |> Seq.map (fun p -> p.Name))

        let distribution = rv.Distributions.TryGetDistribution sampleParentsOnly |> Option.get

        let sampleForRV = SimpleSampler.getSample distribution
        sample <- sample .+. (rv.Name, sampleForRV)

    sample