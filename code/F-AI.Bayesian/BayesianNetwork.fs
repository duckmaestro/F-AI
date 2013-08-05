
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


namespace FAI.Bayesian

open LearnStructure
open LearnDistributions
open System.Collections.Generic

type GenerateStructureMode = | Sequential | Random | PairwiseSingle

///
/// A Bayesian network.
///
type public BayesianNetwork() =

    // The variables in this network.
    let mutable rvs = List.empty

    ///
    /// The list of variables in this network.
    ///
    member public self.Variables
        with get() = rvs :> IEnumerable<_>

    ///
    /// Adds a variable to this network.
    ///
    member public self.AddVariable (rv:RandomVariable) =
        if rvs |> List.exists (fun rv' -> rv' = rv) then
            ()
        else
            rvs <- rv :: rvs
    
    ///
    /// Removes a variable from the network.
    ///
    member public self.RemoveVariable rv =
        rvs <- rvs |> List.partition (fun rv' -> rv' <> rv) |> fst

    ///
    /// Generates an arbitrary DAG structure over the variables currently in 
    /// this network. Useful for testing.
    ///
    member public self.GenerateStructure (mode, ?seed, ?parentLimit) =
        let seed = defaultArg seed 0
        let parentLimit = defaultArg parentLimit 3
        let random = new System.Random(seed)
       
        let generatePairwiseSingle () =
            // Randomize the rv order, then form disjoint parent-child pairs.
            for variablePair in 
                self.Variables 
                |> Seq.sortBy (fun _ -> random.Next())
                |> Seq.pairwise 
                |> Seq.mapi (fun i v -> i,v)
                |> Seq.filter (fun i_v -> fst i_v % 2 = 0)
                |> Seq.map (fun i_v -> snd i_v) 
                do
                    let v1 = fst variablePair
                    let v2 = snd variablePair
                    v2.AddParent v1

        let generateRandom () =
            // Randomize the rv order, then for each node pick
            // random parents from earlier in the node list.
            let variables = 
                self.Variables 
                |> Seq.sortBy (fun _ -> random.Next()) 
                |> Seq.toArray
            for vid in [|1..variables.Length-1|] do
                let variable = variables.[vid]
                let numParents = random.Next (parentLimit + 1)
                for _ in [|1..numParents|] do
                    let pid = random.Next (0, vid)
                    variable.AddParent variables.[pid]
                    
        match mode with
            | Sequential        ->  failwith "Not implemented yet."
            | Random            ->  generateRandom ()
            | PairwiseSingle    ->  generatePairwiseSingle ()

    ///
    /// Learns a structure for the variables in this
    /// network based on the given training set of
    /// observations.
    ///
    member public self.LearnStructure observations =
        failwith "Not implemented yet."

    ///
    /// Learns conditional distributions for the variables
    /// in this network based on the currently configured
    /// network structure.
    ///
    member public self.LearnDistributions (observations:IObservationSet) = 
        
        // For each random variable, learn its conditional distributions.
        for dv in self.Variables do
            let ivs = dv.Parents

            // HACK: The current learning algorithm is not friendly to
            //       observation set streaming, and the observation
            //       set position must be reset before each variable.
            observations.Reset ()

            // Learn conditional distributions for this variable.
            let conditionalDistributions = learnConditionalDistributions dv ivs observations

            // Copy distributions into a CPT.
            let cpt = new ConditionalProbabilityTable()
            for conditionalDistribution in conditionalDistributions do
                let parentInstantiation = conditionalDistribution.Key
                let distribution = conditionalDistribution.Value

                match distribution with
                    | Some d    ->  cpt.SetConditionalDistribution parentInstantiation d
                    | _         ->  failwith "A neccessary distribution was not learned." 
                                    (* TODO: Decide how to fill in missing distributions. *) 
                

            // Associate CPT with this variable.
            dv.Distribution <- ConditionalDiscrete cpt
        ()
