
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

open System.Collections
open System.Collections.Generic


///
/// A simple, in-memory observation set.
/// 
type public InMemoryObservationSet 
    (   observations:seq<Observation>,
        name,
        sourceUri
    ) =

    let observations = observations |> Seq.toArray

    interface IObservationSet with
        member self.Size with get() = Some observations.Length
        member self.Name with get() = name
        member self.SourceUri with get() = sourceUri
    
    interface IEnumerable<Observation> with
        member self.GetEnumerator () =
            observations |> Seq.cast |> (fun s -> s.GetEnumerator())

    interface IEnumerable with 
        member self.GetEnumerator () =
            observations.GetEnumerator ()

