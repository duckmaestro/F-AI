
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


module FAI.Bayesian.GraphAlgorithms

///
/// An identifier for a unique vertex.
///
type Vertex = string

///
/// A edge between two vertex. This edge may be undirected or directed, and
/// unweighted or weighted, depending on the context and function in which it
/// is used. When directed, the field Vertex1 is the source while Vertex2 is
/// the destination.
///
type Edge = { Vertex1:string; Vertex2:string; Weight:float; }

///
/// Finds the maximum weight spanning tree for an undirected graph.
///
let findMaximumWeightSpanningTree vertices edges (progressCallback:Option<_>) = 
    let edges = edges |> Set.toSeq
    let mutable markedVertices = Set.singleton <| (vertices |> Set.toSeq |> Seq.head)
    let mutable unmarkedVertices = vertices - markedVertices
    let mutable treeEdges = Set.empty

    let isCandidateEdge markedNodes unmarkedNodes edge =
        (markedNodes |> Set.contains edge.Vertex1) && 
        (unmarkedNodes |> Set.contains edge.Vertex2)
            ||
        (markedNodes |> Set.contains edge.Vertex2) &&
        (unmarkedNodes |> Set.contains edge.Vertex1)

    let getEdgeWeight edge =
        edge.Weight
        
    while markedVertices <> vertices do
        let candidateEdges = edges |> Seq.filter (isCandidateEdge markedVertices unmarkedVertices)
        let bestEdge = candidateEdges |> Seq.maxBy getEdgeWeight
        
        markedVertices <- 
            markedVertices 
            |> Set.add bestEdge.Vertex1 
            |> Set.add bestEdge.Vertex2
        unmarkedVertices <- 
            unmarkedVertices 
            |> Set.remove bestEdge.Vertex1 
            |> Set.remove bestEdge.Vertex2
        
        treeEdges <- treeEdges |> Set.add bestEdge

        // Raise progress callback.
        match progressCallback with
        | Some fn   ->  fn treeEdges
        | None      ->  ()
        
    // Done.
    treeEdges

///
/// Returns true if the provided directed graph is acyclic.
///
let isAcyclicDirected vertices edges = 
    
    // If no edges, the graph is trivially acyclic.
    if edges |> Set.isEmpty then
        true
    else
        // Helper. Does this vertex have any inbound edges.
        let hasInbound vertex edges =
            edges 
            |> Set.exists (fun e -> e.Vertex2 = vertex)
        
        // Helper. Find a root/source vertex.
        let findRoot vertices edges = 
            let roots = 
                vertices
                |> Seq.filter (fun v -> false = hasInbound v edges)
            if roots |> Seq.isEmpty then
                None
            else
                Some (roots |> Seq.head)

        // Helper. Gather the outbound edges for this vertex.
        let getOutEdges vertex edges =
            edges |> Set.filter (fun e -> e.Vertex1 = vertex)

        // Algorithm.
        let unseenVertices = ref vertices

        // Recursion logic. Returns true if no cycle was found.
        let rec search nextVertex edges history = 
            // Add newest vertex to history.
            let history = nextVertex :: history

            // Record that we've seen this vertex.
            unseenVertices := !unseenVertices |> Set.remove nextVertex

            // Find outgoing edges from new vertex.
            let outEdges = getOutEdges nextVertex edges

            // If no outgoing edges, we're done and no cycle was found.
            if outEdges.IsEmpty then 
                true
            else
                // This function checks the history for an already visited 
                // vertex, and if none, proceeds to deepen the search.
                let hasNoCycles nextEdge =
                    if history |> List.exists (fun x -> x = nextEdge.Vertex2) then
                        false
                    else
                        search nextEdge.Vertex2 edges history 

                // For each outgoing edge, check for cycle. Evaluates to 
                // true if no cycles found.
                outEdges
                |> Set.exists (fun e -> false = hasNoCycles e)
                |> ((<>) true)

        // For each root node in the forest, perform a fresh search for cycles.
        let mutable isAcyclic = true
        while !unseenVertices <> Set.empty && isAcyclic <> false do
            let root = findRoot !unseenVertices edges
            
            // If there are unvisited vertices but we can't find a root,
            // then by necessity this graph is cyclic.
            if root.IsNone then 
                isAcyclic <- false
            // Perform an acyclic test for this sub-graph.
            else
                isAcyclic <- search root.Value edges []

        // Done.
        isAcyclic

            
            
