
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
/// Exposes access to a set of observations.
///
[<Interface>]
type public IObservationSet = 
    inherit IEnumerable<Observation> 

    ///
    /// The size of the set, if known.
    ///
    abstract Size : Option<Integer> with get

    ///
    /// The name of the set, if known.
    ///
    abstract Name : string with get

    ///
    /// The source uri of the set, if known.
    ///
    abstract SourceUri : string with get

    ///
    /// The variables and their state spaces.
    ///
    abstract Variables : Map<Identifier, Space> with get


