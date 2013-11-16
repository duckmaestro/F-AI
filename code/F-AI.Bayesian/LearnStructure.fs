
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
/// Describes an action to take in the process of searching for optimal 
/// structure.
///
type private StructureSearchAction =
    | AddEdge of Identifier * Identifier
    | RemoveEdge of Identifier * Identifier
    | ReverseEdge of Identifier * Identifier

    member self.Variables =
        match self with
        | AddEdge (a,b)     -> [| a; b; |]
        | RemoveEdge (a,b)  -> [| a; b; |]
        | ReverseEdge (a,b) -> [| a; b; |]


type private StructureSearchRecord = { 
        // The variables and their relationships at this stage of search.
        Variables : Map<Identifier, RandomVariable>
        // The computed family scores for each variable at this stage of search.
        FamilyScores : Map<Identifier, float>
        // The total score of this structure.
        TotalScore : float
        // Cache of actions to delta improvement to total score.
        ActionScoresCache : Map<StructureSearchAction, float>
    }


///
/// Computes the empirical mutual information between two variables.
///
let computeEmpiricalMutualInformation 
        (x:Identifier * Space)
        (ys:list<Identifier * Space>)
        (sufficientStatistics:SufficientStatistics) =
    
    let mutable sum = 0.
    let normalizer = 1. / (float <| sufficientStatistics.GetObservationCount (new Observation()))

    let xName = fst x
    let xSpace = snd x

    for xValue in xSpace.Values do
        let xObservation = new Observation(xName, xValue)
        let xCount = float <| sufficientStatistics.GetObservationCount xObservation
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

    // Compute mutual information of x with its parents.
    let mutualInformation = 
            computeEmpiricalMutualInformation 
                x
                parents
                sufficientStatistics

    // Compute the entropy of x.
    let entropy =
        computeEmpiricalEntropy
            rv.Name
            rv.Space
            sufficientStatistics

    // Compute complexity penalty.
    let freeParameterCount = 
        let paramsPerPermutation = rv.Space.Size - 1
        let permutations =
            parents
            |> Seq.map (fun (r,s) -> s.Size)
            |> Seq.fold (fun a s -> a * s) 1
        float <| permutations * paramsPerPermutation
    let complexityPenalty = (log M) * 0.5 * freeParameterCount

    // Compute family score.
    let familyScore = M * (mutualInformation - entropy) - complexityPenalty
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
    
    // Saved result from each progress cb.
    let network = ref None
    let spanningTreeProgressCallback rvs = 
        match progressCallback with
        | Some cb   ->  Some (fun (edges:Set<_>) -> 
                                // Build and remember the network.
                                network := Some (buildNetwork rvs edges);
                                // Raise callback.
                                do cb (Option.get <| !network);)
        | None      ->  None
    
    // Begin tree structure search algorithm.
    let finalEdges = 
        GraphAlgorithms.findMaximumWeightSpanningTree 
            vertices 
            edgesFullyConnected
            (spanningTreeProgressCallback rvs)

    // Build graph structure using final edges if not done already.
    if Option.isNone <| !network then
        network := Some (buildNetwork rvs finalEdges)

    // Done.
    Option.get <| !network

///
/// Learns a general structure for the random variables given the sufficient
/// statistics.
///
let learnGeneralStructure (rvs:Map<Identifier,RandomVariable>) 
                          (sufficientStatistics:SufficientStatistics)
                          (parentLimit)
                          (progressCallback:Option<_>) =     

    // Helper: Wrapper around the DAG acyclic check.
    let computeIsAcyclic (rvs:Map<Identifier,RandomVariable>) =
        let vertices = 
            rvs 
            |> Map.toSeq 
            |> Seq.map (fun kvp -> fst kvp) 
            |> Set.ofSeq
        let edges = 
            rvs 
            |> Map.toSeq 
            |> Seq.collect (fun (_,rv) -> rv.Children 
                                          |> Seq.map (fun c -> { Vertex1 = rv.Name; Vertex2 = c; Weight = 0.; })
                            )
            |> Set.ofSeq
        GraphAlgorithms.isAcyclicDirected vertices edges

    // Helper: Tallies total family score.
    let computeTotalScore familyScores =
        familyScores |> Map.toSeq |> Seq.sumBy (fun v_s -> snd v_s)

    // Helper: Creates a new set of variables after applying the specified action,
    //         or returns None if the application results in a cyclic graph.
    let computeActionApplication action (variables:Map<Identifier,RandomVariable>) =
        
        let mutable variablesNew = variables
        let mutable isAcyclic = false
        
        // Alter structure.
        match action with
        | AddEdge (a,b) ->
            let va = variables |> Map.find a
            let vb = variables |> Map.find b

            let va' = va.CloneAndAddChild b
            let vb' = vb.CloneAndAddParent a

            variablesNew <- variablesNew |> Map.add a va'
            variablesNew <- variablesNew |> Map.add b vb'

            isAcyclic <- computeIsAcyclic variablesNew

        | RemoveEdge (a,b) ->
            let va = variables |> Map.find a
            let vb = variables |> Map.find b

            let va' = va.CloneAndRemoveChild b
            let vb' = vb.CloneAndRemoveParent a

            variablesNew <- variablesNew |> Map.add a va'
            variablesNew <- variablesNew |> Map.add b vb'

            isAcyclic <- computeIsAcyclic variablesNew

        | ReverseEdge (a,b) ->
            let va = variables |> Map.find a
            let vb = variables |> Map.find b

            // Remove edge from a to b.
            let va' = va.CloneAndRemoveChild b
            let vb' = vb.CloneAndRemoveParent a
            
            // Add edge from b to a.
            let vb' = vb'.CloneAndAddChild a
            let va' = va'.CloneAndAddParent b

            variablesNew <- variablesNew |> Map.add a va'
            variablesNew <- variablesNew |> Map.add b vb'

            isAcyclic <- computeIsAcyclic variablesNew

        // Done.
        if isAcyclic then
            Some variablesNew
        else
            None

    // Helper: Computes updated family scores.
    let computeUpdatedFamilyScores (familyScoresOld) 
                                   (action:StructureSearchAction) 
                                   (structure:Map<Identifier,RandomVariable>) =
    
        let mutable familyScoresNew = familyScoresOld

        // Compute updated scores for variables involved.
        for variableName in action.Variables do
            let variable = structure |> Map.find variableName
            let parents = variable.GetParentVariables structure
            let score = computeFamilyScore variable parents sufficientStatistics
            familyScoresNew <- familyScoresNew |> Map.add variableName score

        familyScoresNew
        
    // Helper: Given the current search state, picks the next best move.
    let computeNextBestAction searchRecord = 
        
        let variables = searchRecord.Variables
        let familyScoresOld = searchRecord.FamilyScores
        let totalScoreOld = searchRecord.TotalScore
        let variablesAsSeq = variables |> Map.toSeq |> Seq.map (fun (k,v) -> v)
        let mutable actionScoresCache = searchRecord.ActionScoresCache
        
        let mutable bestAction = None
        let mutable bestActionFamilyScores = None
        let mutable bestActionActionScore = 0.0
        let mutable bestActionTotalScore = None
        let mutable bestActionStructure = None

        // For each variable in the network, consider options.
        let candidateActions = seq {
            for v1 in variablesAsSeq do
                // Candidate edge additions.
                for v2 in variablesAsSeq do
                    if v1.HasChild v2.Name 
                        || v1.HasParent v2.Name 
                        || v1.Name = v2.Name 
                        || v2.Parents |> Seq.length >= parentLimit then ()
                    else
                        let action = AddEdge (v1.Name, v2.Name)
                        yield action
           
                // Candidate edge removals.
                for v2Name in v1.Children do
                    let action = RemoveEdge (v1.Name, v2Name)
                    yield action

                // Candidate edge reversals.
                if v1.Parents |> Seq.length >= parentLimit then ()
                else
                    for v2Name in v1.Children do
                        let action = ReverseEdge (v1.Name, v2Name)
                        yield action
        }

        // Choose best candidate action.
        for action in candidateActions do
            let structure = computeActionApplication action searchRecord.Variables

            // If structure is not cyclic.
            if structure.IsSome then
                let structure = structure.Value

                // Check action score cache for this action.
                let actionScore = actionScoresCache |> Map.tryFind action

                // Action score previously computed.
                if actionScore.IsSome then

                    let actionScore = actionScore.Value

                    // Better action?
                    if actionScore > bestActionActionScore then
                        bestAction <- Some action
                        bestActionActionScore <- actionScore
                        bestActionStructure <- Some structure
                        bestActionTotalScore <- None    // Computed later if truly best.
                        bestActionFamilyScores <- None  // Computed later if truly best.

                // Action score not previously computed.
                else
                    let familyScoresNew = computeUpdatedFamilyScores familyScoresOld action structure
                        
                    let totalScoreNew = computeTotalScore familyScoresNew
                    let actionScore = totalScoreNew - totalScoreOld

                    // Better action?
                    if actionScore > bestActionActionScore
                    then
                        bestAction <- Some action
                        bestActionActionScore <- actionScore
                        bestActionStructure <- Some structure
                        bestActionFamilyScores <- Some familyScoresNew
                        bestActionTotalScore <- Some totalScoreNew
                    else
                        // Store work in action cache.
                        actionScoresCache <- 
                            actionScoresCache 
                            |> Map.add action actionScore

        // Decide how to return.
        if bestAction.IsNone then
            
            // No better action. Done.
            None

        else
            // Gather info about the action.
            let bestAction = bestAction.Value
            let bestActionStructure = bestActionStructure.Value
            let bestActionFamilyScores =
                match bestActionFamilyScores with
                | Some s    ->  s
                | None      ->  computeUpdatedFamilyScores familyScoresOld bestAction bestActionStructure
            let bestActionTotalScore =
                match bestActionTotalScore with
                | Some s    ->  s
                | None      ->  computeTotalScore bestActionFamilyScores

            // Check.
            assert (System.Math.Abs ((bestActionActionScore + totalScoreOld) - bestActionTotalScore) < 0.01)

            // Cleanup action scores cache.
            // Remove cached action scores for actions involving the variables of the current action.
            let actionScoreVariablesToInvalidate = 
                match bestAction with
                | AddEdge (f, t) -> [| t |]
                | RemoveEdge (f, t) -> [| t |]
                | ReverseEdge (f, t) -> [| f; t; |]
            let contains v vs = 
                if Array.length vs = 2 then
                    vs.[0] = v || vs.[1] = v
                else 
                    vs.[0] = v
            for a,s in actionScoresCache |> Map.toSeq do
                let vs = a.Variables
                if actionScoreVariablesToInvalidate |> contains vs.[0]
                    || actionScoreVariablesToInvalidate |> contains vs.[1]
                then
                    actionScoresCache <- actionScoresCache |> Map.remove a

            // Done. Return new search state record.
            Some    { 
                        Variables = bestActionStructure;
                        FamilyScores = bestActionFamilyScores;
                        TotalScore = bestActionTotalScore;
                        ActionScoresCache = actionScoresCache
                    }


    //
    // Algorithm.
    //

    let mutable startScores = Map.empty
    for rv in rvs |> Map.toSeq |> Seq.map (fun kvp -> snd kvp) do
        let parents = rv.GetParentVariables rvs
        let score = computeFamilyScore rv parents sufficientStatistics
        startScores <- startScores |> Map.add rv.Name score

    let mutable searchState = { 
                                Variables = rvs; 
                                FamilyScores = startScores; 
                                TotalScore = computeTotalScore startScores;
                                ActionScoresCache = Map.empty;
                            }
    let mutable continueSearch = true

    while continueSearch do
        let searchStateNew = computeNextBestAction searchState
        
        match searchStateNew with
        | None                  ->  
            // At a local maxima. Halt.
            do continueSearch <- false;

        | Some searchStateNew   ->  
            // Store new state & raise callback.
            do searchState <- searchStateNew
            match progressCallback with
            | None      ->  do ();
            | Some cb   ->  do cb searchStateNew.Variables

    // Done.
    searchState.Variables