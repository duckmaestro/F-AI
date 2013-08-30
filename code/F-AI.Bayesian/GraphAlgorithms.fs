
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

type Vertex = string
type Edge = { Vertex1:string; Vertex2:string; Weight:float; }


///
/// Finds the maximum weight spanning tree for an undirected graph.
///
let findMaximumWeightSpanningTree nodes edges = 
    let edges = edges |> Set.toSeq
    let mutable markedNodes = Set.singleton <| (nodes |> Set.toSeq |> Seq.head)
    let mutable unmarkedNodes = nodes - markedNodes
    let mutable treeEdges = Set.empty

    let isCandidateEdge markedNodes unmarkedNodes edge =
        (markedNodes |> Set.contains edge.Vertex1) && 
        (unmarkedNodes |> Set.contains edge.Vertex2)
            ||
        (markedNodes |> Set.contains edge.Vertex2) &&
        (unmarkedNodes |> Set.contains edge.Vertex1)

    let getEdgeWeight edge =
        edge.Weight
        
    while markedNodes <> nodes do
        let candidateEdges = edges |> Seq.filter (isCandidateEdge markedNodes unmarkedNodes)
        let bestEdge = candidateEdges |> Seq.maxBy getEdgeWeight
        
        markedNodes <- 
            markedNodes 
            |> Set.add bestEdge.Vertex1 
            |> Set.add bestEdge.Vertex2
        unmarkedNodes <- 
            unmarkedNodes 
            |> Set.remove bestEdge.Vertex1 
            |> Set.remove bestEdge.Vertex2
        
        treeEdges <- treeEdges |> Set.add bestEdge
        
    treeEdges
