
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


module internal FAI.Bayesian.LearnStructure

open GraphAlgorithms


///
///
///
let computeMutualInformation () = ()


///
/// Using the provided variables and their parent structure, computes the log 
/// likelihood of the observation set.
///
let computeLogLikelihoodFromData 
        (variables:seq<RandomVariable>)
        (observations:IObservationSet) =
    
    // A helper to select variable names.
    let getName (rv:RandomVariable) = rv.Name
    let getParentNames (rv:RandomVariable) = rv.Parents |> Seq.map getName

    // Begin.
    let mutable logLikelihoodTotal = 0.
    
    // Compute the log likelihood of each observation.    
    for obs in observations do

        let mutable logLikelihoodObservation = 0.

        // Compute the contribution of log likelihood from each variable.
        for rv in variables do
        
            let rvValue = Option.get <| obs.TryValueForVariable rv.Name
            let parentValues = obs .&. (getParentNames rv)

            let distribution = 
                Option.get <| rv.Distributions.TryGetDistribution parentValues

            let probability = Option.get <| distribution.GetMass rvValue
            let logProbability = System.Math.Log probability

            // Accumuluate for this observation.            
            logLikelihoodObservation <- 
                logLikelihoodObservation + logProbability

        // Accumulate with total likelihood.
        logLikelihoodTotal <-
            logLikelihoodTotal + logLikelihoodObservation

    // Done.
    logLikelihoodTotal


    
///
///
///
let computeFamilyScore (rv:RandomVariable) (observations:IObservationSet) =
    
    // TODO: switch to mutual information to be faster.
    
    computeLogLikelihoodFromData (Seq.singleton rv) observations



///
/// Learns a best tree structure for the random variables.
///
let learnTreeStructure (rvs:seq<RandomVariable>) (observations:IObservationSet) = 

    // Clear existing structure.
    for rv in rvs do
        rv.Distributions <- new DistributionSet ()
        for parent in rv.Parents |> Seq.cache do
            rv.RemoveParent parent            

    // Prepare n (n - 1) / 2 possible parentings.
    let selfJoin = seq { 
        for rv1 in rvs do
            for rv2 in rvs do
                if rv1 <> rv2 && rv1.Name < rv2.Name then
                    yield rv1,rv2
        }

    let variableParentPermutations = 
        selfJoin 
        |> Seq.map (fun (rv1,rv2) -> 
                let rv1' = new RandomVariable(rv1.Name, rv1.Space)
                let rv2' = new RandomVariable(rv2.Name, rv2.Space)
                rv1'.Prior <- rv1.Prior
                rv2'.Prior <- rv2.Prior
                rv1'.AddChild(rv2') 
                rv2'
            )
            
    // Helpers.
    let distributionMapToSet map =
        let set = new DistributionSet()
        for o,d in map |> Map.toSeq do
            let d = Option.get <| d
            set.SetConditionalDistribution o d
        set

    let nameFirstParent (rv:RandomVariable) = rv.Parents |> Seq.map (fun rv-> rv.Name) |> Seq.head
    
    let normalizeUnorderedEdge e = 
        if e.Vertex1 > e.Vertex2 then
            { Vertex1 = e.Vertex2; Vertex2 = e.Vertex1; Weight = e.Weight }
        else
            e

    let selectOriginalVariable name =
        rvs |> Seq.find (fun rv -> rv.Name = name)


    // Learn mle distributions.
    let distributions =
        variableParentPermutations
        |> Seq.map (fun rv -> 
            rv, 
            (
                LearnDistributions.learnConditionalDistribution rv rv.Parents observations |> distributionMapToSet)
            )
        |> Seq.cache

    // Store distributions.
    for (rv,d) in distributions do
        rv.Distributions <- d

    // Measure family score
    let familyScores = 
        distributions
        |> Seq.map (fun (rv,_) -> rv, computeFamilyScore rv observations)
        |> Seq.cache

    // Prepare as fully connected graph.
    let vertices = 
        rvs
        |> Seq.map (fun rv -> rv.Name)
        |> Set.ofSeq

    let edges =
        familyScores
        |> Seq.map (fun (rv,score) -> 
            normalizeUnorderedEdge {   
                Vertex1 = rv.Name; 
                Vertex2 = nameFirstParent <| rv; 
                Weight = score
            })
        |> Set.ofSeq

    // Find maximum spanning tree, undirected.
    let bestEdges = 
        GraphAlgorithms.findMaximumWeightSpanningTree vertices edges

    // Choose a root node to convert to directed.
    // Arbitrary criterion: node with most edges.
    let rootNodeName =
        vertices
        |> Seq.maxBy (fun v -> 
            bestEdges
            |> Seq.filter (fun e -> e.Vertex1 = v || e.Vertex2 = v)
            |> Seq.length)

    let rootVariable = selectOriginalVariable rootNodeName

    // Build rest of structure.
    let rec appendNextVariables (lastVariable:RandomVariable) = 
        let otherName edge = 
            if lastVariable.Name = edge.Vertex1 then
                edge.Vertex2
            else if lastVariable.Name = edge.Vertex2 then
                edge.Vertex1
            else
                failwith "Inconsistent edge data."

        let isWithParent edge = 
            lastVariable.Parents 
            |> Seq.exists (fun rv -> rv.Name = edge.Vertex1 || rv.Name = edge.Vertex2)
        
        let nextEdges = 
            bestEdges 
            |> Seq.filter (fun e -> 
                e.Vertex1 = lastVariable.Name ||
                e.Vertex2 = lastVariable.Name)
            |> Seq.filter (fun e -> isWithParent e = false)

        let children = 
            nextEdges 
            |> Seq.map (fun e -> selectOriginalVariable (otherName e))
            |> List.ofSeq

        for child in children do
            child.AddParent lastVariable
            appendNextVariables child

        ()

    // Begin recursion to form tree, starting at root.
    appendNextVariables rootVariable

    // Done.
    () 
