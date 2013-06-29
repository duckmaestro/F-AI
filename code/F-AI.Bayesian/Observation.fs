
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

open System
open System.Collections.Generic


/// A collection of observed values, indexed by random variable name.
type public Observation = IDictionary<String, Real>

///
/// Describes access to an observation data.
///
type public IObservationSet = 
    abstract Size : Integer with get
    abstract Next : unit -> Observation


///
/// In-place observation set.
/// 
type public BasicObservationSet(observations') =
    let mutable position = 0

    let observations = observations' |> Seq.toArray

    interface IObservationSet with
        member self.Size with get() = observations.Length
        member self.Next () =     
            let value = 
                if position >= observations.Length then null
                else observations.[position]

            if value <> null then position <- position + 1

            value