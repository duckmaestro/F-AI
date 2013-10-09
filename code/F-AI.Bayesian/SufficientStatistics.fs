
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

open System.Collections.Generic

///
/// A mutable cache of sufficient statistics for Bayesian network learning.
/// Supports an optional Dirichlet prior for modified, virtual counts.
///
type SufficientStatistics(observations:IObservationSet, ?dirichletParameters:IDictionary<Identifier,DirichletDistribution>) =

    // The cache of partial observation counts.
    let mutable countCache = Map.empty

    // A list of partial observation variable name tuples which have been
    // scanned for already.
    let mutable countedVariableTuples = Set.empty

    // Dirichlet parameters.
    let mutable dirichletParameters = 
        match dirichletParameters with
        | Some parameters   ->  parameters
                                    |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
                                    |> Map.ofSeq
        | None              ->  Map.empty
    

    ///
    /// Gets a reference to the observation set associated with this sufficient
    /// statistics cache.
    ///
    member public self.ObservationSet 
        with get() = observations

    ///
    /// Retrieves the count sufficient statistic for the desired partial 
    /// observation. Caches the result if not present already.
    ///
    member public self.GetObservationCount (observation:Observation) = 

        // If empty observation, return count of entire set.
        if observation.IsEmpty then float <| (Option.get <| observations.Size) else
           
        // Helper. Gets the count adjustment to use for the count of this 
        // observation.
        let getDirichletContribution (observation:Observation) = 

            // To hold results Dirichlet contribution per variable.
            let mutable contributionByVariable = Map.empty

            // Enumerate the size of the joint event space.
            let eventSpaceSize = 
                observation.VariableNames
                |> Seq.map (fun name -> observations.Variables |> Map.find name)
                |> Seq.map (fun space -> space.Size)
                |> Seq.fold (fun acc value -> acc * value) 1
                |> float

            // Compute contribution by variable.
            for variableName in observation.VariableNames do
                // Grab Dirichlet for this variable.
                let variablePrior = 
                    dirichletParameters |> Map.tryFind variableName
                // Get value for this variable.
                let variableValue = 
                    Option.get <| observation.TryValueForVariable variableName
                // Compute variable space size.
                let variableSpaceSize =
                    observations.Variables 
                    |> Map.find variableName
                    |> fun space -> space.Size
                    |> float
                // Lookup Dirichlet alpha.
                let alpha = 
                    match variablePrior with
                    | Some dirichlet    ->  defaultArg (dirichlet.GetParameter variableValue) 0.
                    | None              ->  0.
                // Compute and store weighted alpha.
                let alphaWeighted = alpha * variableSpaceSize / eventSpaceSize
                contributionByVariable <- 
                    contributionByVariable
                    |> Map.add variableName alphaWeighted

            // TODO: Justify. Add? Average? Refactor out of SufficientStatistics?
            let sum = 
                contributionByVariable 
                |> Seq.sumBy (fun kvp -> kvp.Value)

            // Compute average and Done.
            sum / (float contributionByVariable.Count)
                 
                 
        // Helper. Cache lookup. Returns None if miss.
        let getObservationCountFromCache (observation:Observation) =
            let count = countCache |> Map.tryFind observation
            match count with
                | Some count    ->  
                    // Cache hit.
                    Some count
                | None          ->  
                    // Cache miss.
                    let names = observation.VariableNames |> Seq.toArray
                    let isScanned = 
                        countedVariableTuples 
                        |> Seq.exists (fun t -> t = names)

                    // If we've scanned this variable tuple before but didn't 
                    // found no true occurrences for the associated values.
                    // Compute contribution from the prior, and at last store 
                    // in cache.
                    if isScanned then 
                        let dirichletContribution = getDirichletContribution observation
                        let count = 0. + dirichletContribution
                        countCache <- 
                            countCache 
                            |> Map.add observation count
                        Some count

                    // Not even scanned. Complete cache miss.
                    else None   

        // Check cache first.
        let count = getObservationCountFromCache observation

        // If cache hit.
        if Option.isSome count then
            Option.get count

        // If cache miss.
        else
            // While we're here, let's count all occurrences tuples for the 
            // variable names in the given observation, not just the counts of 
            // the specific values requested.
            let variableNames = observation.VariableNames |> Seq.toArray
            let mutable counts = Map.empty

            // Scan the observation set and build counts.
            for o in observations do
                let o = o .&. variableNames
                let count = defaultArg (counts |> Map.tryFind o) 0
                let count = count + 1
                counts <- counts |> Map.add o count

            // Combine counts and adjustments, and add to count cache.
            for count in counts do
                let key = count.Key
                let countRaw = float count.Value
                let countAdjustment = getDirichletContribution key
                let countFinal = countRaw + countAdjustment

                countCache <-
                    countCache |> Map.add key countFinal
            
            // Record that this variable tuple is counted, even though we may 
            // not have stored a 0 for the corresponding observation's values 
            // yet. Will be resolved in the cache helper method.
            countedVariableTuples <- 
                countedVariableTuples
                |> Set.add variableNames

            let count = getObservationCountFromCache observation
            Option.get <| count

            
