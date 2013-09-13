
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
/// Computes the empirical mutual information between two variables.
///
let computeEmpiricalMutualInformation 
        (x:Identifier * Space)
        (ys:list<Identifier * Space>)
        (sufficientStatistics:SufficientStatistics) =

    // TODO: Add support for a prior to help with unseen observations.
    
    let mutable sum = 0.
    let normalizer = 1. / (float <| sufficientStatistics.GetObservationCount (new Observation()))

    let xName = fst x
    let xSpace = snd x

    for xValue in xSpace.Values do
        let xObservation = new Observation(xName, xValue)
        let xCount = float <| sufficientStatistics.GetObservationCount xObservation
        let xCount = if xCount > 0. then xCount else 1. // HACK: until priors supported.
        let xProb = xCount * normalizer

        // Recursion to explode y values and sum over them.
        let rec doSum (ys:list<Identifier * Space>) (yObservationAcc:Observation) =

            match ys with
            | []    ->
                let xyObservation = xObservation .|. yObservationAcc
            
                let yCount = float <| sufficientStatistics.GetObservationCount yObservationAcc
                let yCount = if yCount > 0. then yCount else 1. // HACK: until priors supported.
                let yProb = yCount * normalizer
                let xyCount = float <| sufficientStatistics.GetObservationCount xyObservation
                let xyCount = if xyCount > 0. then xyCount else 1. // HACK: until priors supported.
                let xyProb = xyCount * normalizer

                assert (yProb > 0.)
                assert (xProb > 0.)
                assert (xyProb > 0.)

                let summand = xyCount * System.Math.Log (xyProb / (xProb * yProb))
                summand

            | y::ys -> 
                let yName = fst y
                let ySpace = snd y

                let summand = 
                    ySpace.Values
                    |> Seq.sumBy 
                        (fun yValue -> doSum ys (yObservationAcc .+. (yName, yValue)))
                summand

        let summand = doSum ys (new Observation())
        sum <- sum + summand

            
    let final = sum * normalizer

    assert (final >= 0.)

    final

///
/// Computes the entropy of a variable.
///
let computeEmpiricalEntropy 
        variableName
        (variableSpace:Space)
        (sufficientStatistics:SufficientStatistics) =

    let mutable sum = 0.
    let M = float <| sufficientStatistics.GetObservationCount (new Observation())
    let normalizer = 1. / M
    let innerConstant = System.Math.Log M

    for v in variableSpace.Values do
        let count = float <| sufficientStatistics.GetObservationCount 
                                (new Observation(variableName, v))
        let logCount = System.Math.Log count
        let summand = count * (innerConstant - logCount)
        sum <- sum + summand

    let final = sum * normalizer
    final

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
/// Computes the family score of a given random variable and its parents.
///
let computeFamilyScore 
        (rv:RandomVariable) 
        (sufficientStatistics:SufficientStatistics) =
   
    let M = float sufficientStatistics.ObservationSet.Size.Value

    let x = (rv.Name, rv.Space)
    let parents = 
        rv.Parents 
        |> Seq.map (fun p -> p.Name, p.Space)
        |> List.ofSeq

    let mutualInformation = 
            computeEmpiricalMutualInformation 
                x
                parents
                sufficientStatistics

    let entropy =
        computeEmpiricalEntropy
            rv.Name
            rv.Space
            sufficientStatistics

    let familyScore = M * (mutualInformation - entropy)
    familyScore



///
/// Learns a best tree structure for the random variables.
///
let learnTreeStructure (rvs:seq<RandomVariable>) 
                       (sufficientStatistics:SufficientStatistics)
                       (progressCallback:Option<_>) =     

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
    let distributionMapToSet (map:Map<_,Option<_>>) =        
        let set =
            map
            |> Map.map (fun k v -> v.Value)
            |> fun ds -> new DistributionSet(ds)

        set

    let nameFirstParent (rv:RandomVariable) = rv.Parents |> Seq.map (fun rv-> rv.Name) |> Seq.head
    
    let normalizeUnorderedEdge e = 
        if e.Vertex1 > e.Vertex2 then
            { Vertex1 = e.Vertex2; Vertex2 = e.Vertex1; Weight = e.Weight }
        else
            e

    let selectOriginalVariable name =
        rvs |> Seq.find (fun rv -> rv.Name = name)

    // Measure family score
    let familyScores = 
        variableParentPermutations
        |> Seq.map (fun rv -> 
            rv, computeFamilyScore rv sufficientStatistics)
        |> Seq.cache

    // Prepare as fully connected graph.
    let vertices = 
        rvs
        |> Seq.map (fun rv -> rv.Name)
        |> Set.ofSeq

    let edgesFullyConnected =
        familyScores
        |> Seq.map (fun (rv,score) -> 
            normalizeUnorderedEdge {   
                Vertex1 = rv.Name; 
                Vertex2 = nameFirstParent <| rv; 
                Weight = score
            })
        |> Set.ofSeq

        
    // Updates the source variables according to the edge list.
    let buildGraphFromEdges edges =

        // Find the existing root node.
        let existingRootVariable =
            rvs
            |> Seq.filter (fun rv -> rv.Parents |> Seq.isEmpty)
            |> Seq.sortBy (fun rv -> - (rv.Children |> Seq.length))
            |> Seq.tryFind (fun _ -> true)

        // Choose a root node to convert to directed.
        // Arbitrary criterion: node with most edges.
        let rootNodeName =
            vertices
            |> Seq.maxBy (fun v -> 
                edges
                |> Seq.filter (fun e -> e.Vertex1 = v || e.Vertex2 = v)
                |> Seq.length)

        let rootVariable = selectOriginalVariable rootNodeName

        // If root changed, reset all edges.
        if existingRootVariable.IsSome && rootVariable <> existingRootVariable.Value then
            // Clear existing structure.
            for rv in rvs do
                rv.Distributions <- new DistributionSet ()
                for parent in rv.Parents |> Seq.toList do
                    rv.RemoveParent parent

        // Build rest of structure.
        let rec appendNextVariables (lastVariable:RandomVariable) = 
            // Helper. Selects the "other" variable given an edge with the 
            // current variable.
            let otherName edge = 
                if lastVariable.Name = edge.Vertex1 then
                    edge.Vertex2
                else if lastVariable.Name = edge.Vertex2 then
                    edge.Vertex1
                else
                    failwith "Inconsistent edge data."

            // Helper.
            let isWithParent edge = 
                lastVariable.Parents 
                |> Seq.exists (fun rv -> rv.Name = edge.Vertex1 || rv.Name = edge.Vertex2)
        
            // Helper.
            let nextEdges = 
                edges 
                |> Seq.filter (fun e -> 
                    e.Vertex1 = lastVariable.Name ||
                    e.Vertex2 = lastVariable.Name)
                |> Seq.filter (fun e -> isWithParent e = false)

            // Helper.
            let children = 
                nextEdges 
                |> Seq.map (fun e -> selectOriginalVariable (otherName e))
                |> List.ofSeq

            // For each child (from outgoing edges), parent them to the current 
            // variable.
            for child in children do

                // If existing parents that aren't the new parent, remove them.
                let existingParents = child.Parents |> Seq.cache
                let numExistingParents = existingParents |> Seq.length
                if numExistingParents >= 2 || 
                    (numExistingParents = 1 
                        && existingParents |> Seq.head <> lastVariable) then
                    for p in existingParents do
                        child.RemoveParent p
                   
                // Add the new parent.
                child.AddParent lastVariable

                // Recurse.
                appendNextVariables child

            ()

        // Begin recursion to form tree, starting at root.
        appendNextVariables rootVariable


    // Initialize progress callback. If the caller provided a callback, we will 
    // build a new graph after each progress event, then raise their callback.
    let spanningTreeProgressCallback = 
        match progressCallback with
        | Some cb   ->  Some (fun edges -> do buildGraphFromEdges edges; cb ();)
        | None      ->  None
    
    // Begin tree structure search algorithm.
    let finalEdges = 
        GraphAlgorithms.findMaximumWeightSpanningTree 
            vertices 
            edgesFullyConnected
            spanningTreeProgressCallback

    // Build graph structure using final edges if not done already.
    if progressCallback.IsNone then
        do buildGraphFromEdges finalEdges;

    // Done.
    () 

///
/// Learns a general structure for the random variables given the sufficient
/// statistics.
///
let learnGeneralStructure (rvs:seq<RandomVariable>) 
                          (sufficientStatistics:SufficientStatistics) 
                          (progressCallback:Option<_>) = 

    failwith "Not implemented yet."