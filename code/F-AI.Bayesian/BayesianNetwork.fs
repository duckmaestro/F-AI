
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
open ListHelpers
open GraphAlgorithms
open System.Collections.Generic

type GenerateStructureMode = | Sequential | Random | PairwiseSingle

///
/// A Bayesian network.
///
[<System.Diagnostics.DebuggerDisplay("{Name}")>]
type public BayesianNetwork(name) =
    
    let mutable name = name
    let mutable rvs : RandomVariable list = [ ]

    // A cache of the topological ordering.
    let mutable topologicalOrdering = Some []

    // Published events.
    let eventStructureChanged = new Event<_>()


    let onVariableAddedRemoved = fun () -> do topologicalOrdering <- None;
                                              eventStructureChanged.Trigger ();


    ///
    /// The name of this network, e.g. Weather Predictor.
    ///
    member public self.Name 
        with get() : Identifier = name

    ///
    /// The list of variables in this network, in a topological order.
    ///
    member public self.Variables
        with get() : list<RandomVariable> = 

            // FIXME: Restore cache feature once edges are added/removed via
            //         the BN.

//            // Refresh topological ordering if needed.
//            if topologicalOrdering.IsNone then
//                topologicalOrdering <- Some (self.GetTopologicalOrdering ())
//
//            // Return variable list.
//            Option.get <| topologicalOrdering :> IEnumerable<RandomVariable>

            self.GetTopologicalOrdering ()

    ///
    /// Adds a variable to this network.
    ///
    member public self.AddVariable (rv:RandomVariable) =
        if rvs |> List.exists (fun rv' -> rv' = rv) then
            ()
        else
            rvs <- rv :: rvs

        onVariableAddedRemoved ()
        ()
    
    ///
    /// Removes a variable from the network.
    ///
    member public self.RemoveVariable rv =
        rvs <- rvs |> List.partition (fun rv' -> rv' <> rv) |> fst

        onVariableAddedRemoved ()
        ()

    ///
    /// Raised when an edge is added or removed, i.e. when a parent 
    /// or child is added or removed.
    ///
    [<CLIEvent>]
    member self.StructureChanged = eventStructureChanged.Publish

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
            // Randomize the rv order, then for each node pick random parents 
            // from earlier in the node list.
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
    /// Learns a structure for the variables in this network based on the 
    /// given training set of observations.
    ///
    member public self.LearnStructure sufficientStatistics =
        // For now, only tree structure is supported.
        LearnStructure.learnTreeStructure 
            self.Variables 
            sufficientStatistics
            (Some (fun _ -> ()))

        ()

    ///
    /// Learns conditional distributions for the variables in this network 
    /// based on the currently configured network structure.
    ///
    member public self.LearnDistributions sufficientStatistics = 
        LearnDistributions.learnDistributions self.Variables sufficientStatistics
        ()

    // Retrieves the variables of this network in a topological ordering.
    member private self.GetTopologicalOrdering () =
        
        // TODO: Perform this check in effect while building the ordering?
        #if DEBUG
        // Check for cycles.
        let isAcyclic = 
            GraphAlgorithms.isAcyclicDirected 
                (rvs |> Seq.map (fun rv -> rv.Name) |> Set.ofSeq)
                (rvs |> Seq.collect (fun rv -> rv.Children |> Seq.map 
                                                (fun c -> { Vertex1 = rv.Name; Vertex2 = c.Name; Weight = 0. }))
                     |> Set.ofSeq)
        if isAcyclic = false then
            failwith "Graph not acyclic."
        #endif

        // The ordering.
        let mutable ordering = [ rvs |> Seq.head ]
                
        // Build ordering.
        for rv in rvs |> Seq.skip 1 do
            let eldestDescendent = ordering |> List.tryFind (fun v -> rv.HasDescendant v)
            let youngestAncestor = ordering |> List.rev |> List.tryFind (fun v -> rv.HasAncestor v)

            match eldestDescendent, youngestAncestor with
                | None, None        ->  do 
                    ordering <- insertAfter ordering (ordering.Head) rv

                | Some d, Some a    ->  do 
                    ordering <- insertAfter ordering a rv

                | None, Some a      ->  do 
                    ordering <- insertAfter ordering a rv

                | Some d, None      ->  do 
                    ordering <- insertBefore ordering d rv


        #if DEBUG
        // Check results.
        for i in [|0..ordering.Length-1|] do
            let rv = ordering.Item i
            for j in [|0..i-1|] do
                let a = ordering.Item j
                if rv.HasDescendant a then
                    failwith "Ordering not correct."
            for j in [|i+1..ordering.Length-1|] do
                let d = ordering.Item j
                if rv.HasAncestor d then
                    failwith "Ordering not correct."
        #endif

        // Done.
        ordering

    ///
    /// Samples a particle from the network using forward sampling.
    ///
    member public self.Sample () = 
        let rvs = self.Variables
        let sample = ForwardSampler.getSample rvs
        sample
