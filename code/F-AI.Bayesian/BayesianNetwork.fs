
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

///
/// Structure learning classes.
///
type GenerateStructureMode = | Sequential | Random | PairwiseSingle | Tree | General

///
/// A Bayesian network. Network structure is mutable. Variable instances are
/// constant but replaceable by certain operations.
///
[<System.Diagnostics.DebuggerDisplay("{Name}")>]
type public BayesianNetwork(name, ?variables) =
    
    let mutable name = name
    let mutable rvsSearchable = defaultArg variables Map.empty
    let mutable rvsOrdered = None

    // Published events.
    let eventStructureChanged = new Event<BayesianNetwork>()


    ///
    /// Constructs an empty Bayesian network.
    ///
    new(name) = 
        new BayesianNetwork(name, Map.empty)

    ///
    /// The name of this network, e.g. Weather Predictor.
    ///
    member public self.Name 
        with get() : Identifier = name

    ///
    /// Clones the Bayesian network. Registered event handlers are ignored.
    ///
    member public self.Clone () =
        new BayesianNetwork(name, rvsSearchable)

    ///
    /// Adds a variable to this network.
    ///
    member public self.AddVariable (rv:RandomVariable) =
        if rvsSearchable |> Map.containsKey rv.Name then
            ()
        else
            rvsSearchable <- rvsSearchable |> Map.add rv.Name rv
            rvsOrdered <- None
            self.RaiseStructureChanged ()
            ()
    
    ///
    /// Removes a variable from the network.
    ///
    member public self.RemoveVariable (rv:RandomVariable) =
        rvsSearchable <- rvsSearchable |> Map.remove rv.Name
        rvsOrdered <- None
        self.RaiseStructureChanged ()
        ()

    ///
    /// Modifies the network so that a parent-child connection is made between
    /// the two variables.
    ///
    member public self.ConnectVariables parentName childName =
        let rvParent = rvsSearchable |> Map.tryFind parentName
        let rvChild = rvsSearchable |> Map.tryFind childName
        if rvParent.IsNone || rvChild.IsNone then
            failwith "One or more variables not found in the network."
        
        let rvParent = rvParent.Value
        let rvChild = rvChild.Value

        let rvParent' = rvParent.CloneAndAddChild childName
        let rvChild' = rvChild.CloneAndAddParent parentName

        // TODO: Test for cycles.

        rvsSearchable <- rvsSearchable |> Map.add parentName rvParent'
        rvsSearchable <- rvsSearchable |> Map.add childName rvChild'
        rvsOrdered <- None

        self.RaiseStructureChanged ()
        ()

    ///
    /// Breaks the child-parent relationship between two variables.
    ///
    member public self.DisconnectVariables parentName childName = 
        let rvParent = rvsSearchable |> Map.tryFind parentName
        let rvChild = rvsSearchable |> Map.tryFind childName
        if rvParent.IsNone || rvChild.IsNone then
            failwith "One or more variables not found in the network."
        
        let rvParent = rvParent.Value
        let rvChild = rvChild.Value

        let rvParent' = rvParent.CloneAndRemoveChild childName
        let rvChild' = rvChild.CloneAndRemoveParent parentName

        rvsSearchable <- rvsSearchable |> Map.add parentName rvParent'
        rvsSearchable <- rvsSearchable |> Map.add childName rvChild'
        rvsOrdered <- None

        self.RaiseStructureChanged ()
        ()

    ///
    /// Disconnects all variables in this network, leaving no parent-child 
    /// structure.
    ///
    member public self.DisconnectAllVariables () =
        rvsSearchable <- rvsSearchable |> Map.map (fun k v -> v.CloneAndDisconnect ())
        rvsOrdered <- None

        self.RaiseStructureChanged ()
        ()

    ///
    /// Assigns a new local distribution to a variable in this network.
    /// 
    member public self.SetLocalDistribution variableName distribution =
        let variable = rvsSearchable |> Map.find variableName

        rvsSearchable <- 
            rvsSearchable 
            |> Map.add variableName (variable.CloneAndSetDistribution distribution)
        rvsOrdered <- None

        ()

    ///
    /// Gets a variable by name.
    ///
    member public self.GetVariable variableName =
        rvsSearchable |> Map.find variableName

    ///
    /// Returns true if a variable exists in the network by the given name.
    member public self.HasVariable variableName =
        rvsSearchable |> Map.containsKey variableName

    ///
    /// The variables in this network as a map.
    ///
    member public self.Variables
        with get() : Map<Identifier, RandomVariable> =
            rvsSearchable

    ///
    /// The list of variables in this network, in a topological order.
    ///
    member public self.VariablesOrdered
        with get() : list<RandomVariable> = 

            // Refresh topological ordering if needed.
            if rvsOrdered.IsNone then
                rvsOrdered <- Some (self.GetTopologicalOrdering ())

            // Return variable list.
            rvsOrdered.Value

    ///
    /// A description of all edges from parents to children.
    ///
    member public self.VariableEdges
        with get() =
            
            rvsSearchable 
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.collect (fun rv -> 
                                    rv.Children 
                                    |> Seq.map (fun c -> rv.Name, c ))

    ///
    /// Tests whether a two variables are related by ancestry.
    ///
    member public self.IsAncestor ancestor descendant =
        // TODO: Move this into generic graph algorithms.

        // Helper.
        let rec hasDescendent start desc = 
            let rvStart = rvsSearchable |> Map.find start

            // Is descendant if child directly is, or a child has the 
            // descendant.
            rvStart.Children |> Seq.exists (fun c -> c = desc)
            || rvStart.Children |> Seq.exists (fun c -> self.IsAncestor c desc)
        
        // Begin.
        hasDescendent ancestor descendant        

    ///
    /// Raised when an edge is added or removed, i.e. when a parent 
    /// or child is added or removed.
    ///
    [<CLIEvent>]
    member self.StructureChanged = eventStructureChanged.Publish

    // Helper to raise StructureChanged event.
    member private self.RaiseStructureChanged () =
        do eventStructureChanged.Trigger self

    ///
    /// Generates an arbitrary DAG structure over the variables currently in 
    /// this network. Useful for testing.
    ///
    member public self.GenerateStructure (mode, ?seed, ?parentLimit) =
        let seed = defaultArg seed 0
        let parentLimit = defaultArg parentLimit 3
        let random = new System.Random(seed)

        // Reset structure.
        self.DisconnectAllVariables ()
        
        // Method 1.
        let generatePairwiseSingle () =
            // Randomize the rv order, then form disjoint parent-child pairs.
            for variablePair in 
                self.Variables 
                |> Seq.map  (fun kvp -> kvp.Key)
                |> Seq.sortBy (fun _ -> random.Next())
                |> Seq.pairwise 
                |> Seq.mapi (fun i v -> i,v)
                |> Seq.filter (fun i_v -> fst i_v % 2 = 0)
                |> Seq.map (fun i_v -> snd i_v) 
                do
                    let v1 = fst variablePair
                    let v2 = snd variablePair
                    self.ConnectVariables v1 v2

        // Method 2.
        let generateRandom () =
            // Randomize the rv order, then for each node pick random parents 
            // from earlier in the node list.
            let variables = 
                self.Variables 
                |> Seq.map  (fun kvp -> kvp.Key)
                |> Seq.sortBy (fun _ -> random.Next()) 
                |> Seq.toArray
            for vid in { 1..variables.Length-1 } do
                let variable = variables.[vid]
                let numParents = random.Next (parentLimit + 1)
                for _ in [|1..numParents|] do
                    let pid = random.Next (0, vid)
                    let parent = variables.[pid]
                    self.ConnectVariables parent variable
                    
        // Switch on mode/method.
        match mode with
            | Sequential        ->  failwith "Not implemented yet."
            | Random            ->  do generateRandom ();
            | PairwiseSingle    ->  do generatePairwiseSingle ();
            | _                 ->  failwith "Invalid option for this method."

        // Done.
        ()

    ///
    /// Learns a structure for the variables in this network based on the 
    /// given training set of observations.
    ///
    member public self.LearnStructure sufficientStatistics =
        
        // Disconnect all variables, so we trigger a structure change 
        // immediately.
        self.DisconnectAllVariables ()

        // Structure learning callback.
        let learningCallback network =
            rvsSearchable <- network
            rvsOrdered <- None    
            self.RaiseStructureChanged ()

        // For now, only tree structure is supported.
        let structure =    
            LearnStructure.learnTreeStructure 
                self.Variables 
                sufficientStatistics
                (Some learningCallback)

        // Done.
        ()        

    ///
    /// Learns conditional distributions for the variables in this network 
    /// based on the currently configured network structure.
    ///
    member public self.LearnDistributions sufficientStatistics = 

        // Learn.
        let distributions = 
            LearnDistributions.learnDistributions 
                self.Variables 
                sufficientStatistics

        // Update variables.
        for rv,d in distributions |> Map.toSeq do
            self.SetLocalDistribution rv d

        ()

    // Retrieves the variables of this network in a topological ordering.
    member private self.GetTopologicalOrdering () =
        
        if rvsSearchable.Count = 0 then [ ]
        else


        #if DEBUG
        // Check for cycles.
        // TODO: Perform this check in effect while building the ordering?
        let isAcyclic = 
            let variables = 
                self.Variables 
                |> Seq.map (fun kvp -> kvp.Key) 
                |> Set.ofSeq
            let edges = 
                self.VariableEdges 
                |> Seq.map (fun (v1,v2) -> { Vertex1 = v1; Vertex2 = v2; Weight = 0. }) 
                |> Set.ofSeq

            GraphAlgorithms.isAcyclicDirected 
                variables
                edges

        if isAcyclic = false then
            failwith "Graph not acyclic."
        #endif


        // The ordering.
        let mutable ordering = [ rvsSearchable |> Seq.head |> fun rv -> rv.Value ]
                
        // Build ordering.
        for rv in rvsSearchable |> Seq.map (fun kvp -> kvp.Value) |> Seq.skip 1 do
            let eldestDescendent =
                ordering 
                |> List.tryFind (fun v -> self.IsAncestor rv.Name v.Name)
            let youngestAncestor = 
                ordering 
                |> List.rev 
                |> List.tryFind (fun v -> self.IsAncestor v.Name rv.Name)

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
                if self.IsAncestor rv.Name a.Name then
                    failwith "Ordering not correct."
            for j in [|i+1..ordering.Length-1|] do
                let d = ordering.Item j
                if self.IsAncestor d.Name rv.Name then
                    failwith "Ordering not correct."
        #endif

        // Done.
        ordering

    ///
    /// Samples a particle from the network using forward sampling.
    ///
    member public self.Sample () = 
        let rvs = self.Variables
        let sample = ForwardSampler.getSample self.Variables self.VariablesOrdered
        sample
