
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

type private StructureSearchRecord = { 
        Variables : Map<Identifier, RandomVariable>
        FamilyScores : Map<Identifier, Real>
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


    // Helper: Computes new structure record given a traversal action.
    //         Returns none if the action would result in an invalid DAG.
    let computeStructureRecord structureOld action = 
        
        // Grab input.
        let variablesOld = structureOld.Variables
        let scoresOld = structureOld.FamilyScores
        
        // Prepare output.
        let mutable dirtyVariables = []
        let mutable variablesNew = variablesOld
        let mutable scoresNew = scoresOld
        let mutable isAcyclic = false

        // Alter structure.
        match action with
        | AddEdge (a,b) ->
            let va = variablesOld |> Map.find a
            let vb = variablesOld |> Map.find b

            let va' = va.CloneAndAddChild b
            let vb' = vb.CloneAndAddParent a

            variablesNew <- variablesNew |> Map.add a va'
            variablesNew <- variablesNew |> Map.add b vb'

            isAcyclic <- computeIsAcyclic variablesNew
            dirtyVariables <- a :: b :: []

        | RemoveEdge (a,b) ->
            let va = variablesOld |> Map.find a
            let vb = variablesOld |> Map.find b

            let va' = va.CloneAndRemoveChild b
            let vb' = vb.CloneAndRemoveParent a

            variablesNew <- variablesNew |> Map.add a va'
            variablesNew <- variablesNew |> Map.add b vb'

            isAcyclic <- computeIsAcyclic variablesNew
            dirtyVariables <- a :: b :: []

        | ReverseEdge (a,b) ->
            let va = variablesOld |> Map.find a
            let vb = variablesOld |> Map.find b

            // Remove edge from a to b.
            let va' = va.CloneAndRemoveChild b
            let vb' = vb.CloneAndRemoveParent a
            
            // Add edge from b to a.
            let vb' = vb'.CloneAndAddChild a
            let va' = va'.CloneAndAddParent b

            variablesNew <- variablesNew |> Map.add a va'
            variablesNew <- variablesNew |> Map.add b vb'

            isAcyclic <- computeIsAcyclic variablesNew
            dirtyVariables <- a :: b :: []


        // If cyclic, quit now.
        if isAcyclic <> true then
            None
        else
            // Compute updated scores for variables involved.
            for variableName in dirtyVariables do
                let variable = variablesNew |> Map.find variableName
                let parents = variable.GetParentVariables variablesNew
                let score = computeFamilyScore variable parents sufficientStatistics
                scoresNew <- scoresNew |> Map.add variableName score

            // Done.
            Some { Variables = variablesNew; FamilyScores = scoresNew }


    // Helper: Tallies total family score.
    let computeTotalScore searchRecord =
        searchRecord.FamilyScores |> Map.toSeq |> Seq.sumBy (fun v_s -> snd v_s)

    // Helper: Given the current search state, picks the next best move.
    let computeNextBestAction searchRecord = 
        let variables = searchRecord.Variables
        let totalScoreOld = computeTotalScore searchRecord
        let candidates = ref Map.empty // Sorted by negative total score.
        let variablesAsSeq = variables |> Map.toSeq |> Seq.map (fun (k,v) -> v)

        // For each variable in the network, consider options.
        for v1 in variablesAsSeq do

            // Helper: Adds a candidate if not empty, to the 'candidates' map.
            let addCandidateChecked candidate =
                match candidate with
                    | None          ->  () // Invalid DAG.
                    | Some record   ->  
                        let score = computeTotalScore record
                        candidates := !candidates |> Map.add (-score) record

            // Candidate edge additions.
            for v2 in variablesAsSeq do
                if v1.HasChild v2.Name 
                    || v1.HasParent v2.Name 
                    || v1.Name = v2.Name 
                    || v2.Parents |> Seq.length >= parentLimit then ()
                else
                    let candidate = computeStructureRecord searchRecord (AddEdge (v1.Name, v2.Name))
                    addCandidateChecked candidate
            
            // Candidate edge removals.
            for v2Name in v1.Children do
                let candidate = computeStructureRecord searchRecord (RemoveEdge (v1.Name, v2Name))
                addCandidateChecked candidate

            // Candidate edge reversals.
            if v1.Children |> Seq.length >= parentLimit then ()
            else
                for v2Name in v1.Children do
                    let candidate = computeStructureRecord searchRecord (ReverseEdge (v1.Name, v2Name))
                    addCandidateChecked candidate
        

        // Given the candidates, pick the best.
        let bestCandidateScore,bestCandidateRecord = !candidates |> Map.toSeq |> Seq.head
        let bestCandidateScore = -bestCandidateScore // Undo score negation.

        // Check score, and return best choice if better than present.
        if bestCandidateScore > totalScoreOld then
            Some bestCandidateRecord
        else
            None

    //
    // Algorithm.
    //

    let mutable startScores = Map.empty
    for rv in rvs |> Map.toSeq |> Seq.map (fun kvp -> snd kvp) do
        let parents = rv.GetParentVariables rvs
        let score = computeFamilyScore rv parents sufficientStatistics
        startScores <- startScores |> Map.add rv.Name score

    let mutable searchState = { Variables = rvs; FamilyScores = startScores; }
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