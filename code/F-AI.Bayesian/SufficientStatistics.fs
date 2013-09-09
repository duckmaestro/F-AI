
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

///
/// A mutable cache of sufficient statistics for Bayesian network learning.
///
type SufficientStatistics(observations:IObservationSet) =

    // The cache of partial observation counts.
    let mutable countCache = Map.empty

    // A list of partial observation variable name tuples which have been
    // scanned for already.
    let mutable countedVariableTuples = Set.empty
    

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

        // Check argument.
        if observation.IsEmpty then Option.get <| observations.Size else
            
        // Helper.
        let getObservationCountFromCache (observation:Observation) =
            let count = countCache |> Map.tryFind observation
            match count with
                | Some count    ->  Some count
                | None          ->  
                    let names = observation.VariableNames |> Seq.toArray
                    let isScanned = 
                        countedVariableTuples 
                        |> Seq.exists (fun t -> t = names)
                    if isScanned then Some 0
                    else None
                    

        // Check cache first.
        let count = getObservationCountFromCache observation

        // If cache hit.
        if Option.isSome count then
            Option.get count

        // If cache miss.
        else
            // While we're here, let's count all occurrences tuples for the 
            // variable names in the given observation.
            let variableNames = observation.VariableNames |> Seq.toArray
            let mutable counts = Map.empty

            // Scan the observation set and build counts.
            for o in observations do
                let o = o .&. variableNames
                let count = defaultArg (counts |> Map.tryFind o) 0
                let count = count + 1
                counts <- counts |> Map.add o count
            
            // Add to count cache.
            for count in counts do
                countCache <-
                    countCache 
                    |> Map.add count.Key count.Value
            
            // Record that this variable tuple is counted.
            countedVariableTuples <- 
                countedVariableTuples
                |> Set.add variableNames

            let count = getObservationCountFromCache observation
            Option.get <| count

            
