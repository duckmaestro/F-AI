
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
    
    // Begin.
    let mutable logLikelihoodTotal = 0.
    
    // Compute the log likelihood of each observation.    
    for obs in observations do

        let mutable logLikelihoodObservation = 0.

        // Compute the contribution of log likelihood from each variable.
        for rv in variables do
        
            let rvValue = Option.get <| obs.TryValueForVariable rv.Name
            let parentValues = obs .&. rv.Parents

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
        (parents:seq<RandomVariable>)
        (sufficientStatistics:SufficientStatistics) =
   
    let M = float sufficientStatistics.ObservationSet.Size.Value

    let x = (rv.Name, rv.Space)
    let parents = 
        parents
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
let learnTreeStructure (rvs:Map<Identifier,RandomVariable>) 
                       (sufficientStatistics:SufficientStatistics)
                       (progressCallback:Option<_>) =     

    let rvsList = rvs |> Map.toSeq |> Seq.map (fun (k,v) -> v)

    // Prepare n (n - 1) / 2 possible parentings.
    let selfJoin = seq { 
        for rv1 in rvsList do
            for rv2 in rvsList do
                if rv1 <> rv2 && rv1.Name < rv2.Name then
                    yield rv1,rv2
        }

    let variableParentPermutations = 
        selfJoin 
        |> Seq.map (fun (rv1,rv2) -> 
                let rv1' = rv1.CloneAndDisconnect ()
                let rv2' = rv2.CloneAndDisconnect ()
                do rv1'.Prior <- rv1.Prior // Hacky
                do rv2'.Prior <- rv2.Prior
                rv2',rv1' // Child,Parent
            )
            
    // Helpers.
    let distributionMapToSet (map:Map<_,Option<_>>) =        
        let set =
            map
            |> Map.map (fun k v -> v.Value)
            |> fun ds -> new DistributionSet(ds)

        set
    
    let normalizeUnorderedEdge e = 
        if e.Vertex1 > e.Vertex2 then
            { Vertex1 = e.Vertex2; Vertex2 = e.Vertex1; Weight = e.Weight }
        else
            e

    // Measure family score
    let familyScores = 
        variableParentPermutations
        |> Seq.map (fun (rv,parent) -> 
                        rv.Name,parent.Name,computeFamilyScore rv (Seq.singleton parent) sufficientStatistics)
        |> Seq.cache

    // Prepare as fully connected graph.
    let vertices = 
        rvsList
        |> Seq.map (fun rv -> rv.Name)
        |> Set.ofSeq

    let edgesFullyConnected =
        familyScores
        |> Seq.map (fun (rvName,parentName,score) -> 
            normalizeUnorderedEdge {   
                Vertex1 = rvName; 
                Vertex2 = parentName;
                Weight = score
            })
        |> Set.ofSeq

        
    // Updates the source variables according to the edge list.
    let buildNetwork (variables:Map<Identifier,RandomVariable>) 
                     edges =

        // The network we're going to build.
        let network = ref Map.empty

        // Choose a root node to convert to directed.
        // Arbitrary criterion: node with most edges.
        let rootNodeName =
            variables
            |> Map.toSeq
            |> Seq.map (fun (k,v) -> k)
            |> Seq.maxBy (fun name -> 
                edges
                |> Seq.filter (fun e -> e.Vertex1 = name || e.Vertex2 = name)
                |> Seq.length)

        let rootVariable = rvs |> Map.find rootNodeName
        let rootVariable = rootVariable.CloneAndDisconnect ()
        network := !network |> Map.add rootVariable.Name rootVariable

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
            let isEdgeWithParent edge = 
                lastVariable.Parents 
                |> Seq.exists (fun parentName -> parentName = edge.Vertex1 
                                                    || parentName = edge.Vertex2)
        
            // Helper.
            let nextEdges = 
                edges 
                |> Seq.filter (fun e -> 
                    e.Vertex1 = lastVariable.Name ||
                    e.Vertex2 = lastVariable.Name)
                |> Seq.filter (fun e -> isEdgeWithParent e = false)

            // Helper.
            let nextChildren = 
                nextEdges 
                |> Seq.map (fun e -> variables |> Map.find (otherName e))
                |> List.ofSeq

            // For each child (from outgoing edges), parent them to the current 
            // variable.
            let mutable parent = lastVariable
            for child in nextChildren do
                let child = child.CloneAndDisconnect ()
                let child = child.CloneAndAddParent parent.Name
                parent <- parent.CloneAndAddChild child.Name
                
                network := !network 
                            |> Map.add child.Name child
                            |> Map.add parent.Name parent

                // Recurse.
                appendNextVariables child

            ()

        // Begin recursion to form tree, starting at root.
        appendNextVariables rootVariable

        // Done.
        !network


    // Initialize progress callback. If the caller provided a callback, we will 
    // build a new graph after each progress event, then raise their callback.
    let spanningTreeProgressCallback = 
        match progressCallback with
        | Some cb   ->  None //Some (fun edges -> do buildGraphFromEdges edges; cb ();)
        | None      ->  None
    
    // Begin tree structure search algorithm.
    let finalEdges = 
        GraphAlgorithms.findMaximumWeightSpanningTree 
            vertices 
            edgesFullyConnected
            spanningTreeProgressCallback

    // Build graph structure using final edges if not done already.
    //if spanningTreeProgressCallback.IsNone then
        //buildNetwork rvs finalEdges;

    let network = buildNetwork rvs finalEdges

    // Done.
    network

///
/// Learns a general structure for the random variables given the sufficient
/// statistics.
///
let learnGeneralStructure (rvs:seq<RandomVariable>) 
                          (sufficientStatistics:SufficientStatistics) 
                          (progressCallback:Option<_>) = 

    failwith "Not implemented yet."