
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


///
/// Describes a space of permitted values.
///
type RandomVariableSpace = 
    | Continuous of Real * Real     // The min, max bounds on the region.
    | Discrete of array<Real>       // A list of valid values.



///
/// A random variable.
/// Contains a mutable dependency list 
/// and mutable distribution.
///
type public RandomVariable(name', space', distribution') =
    
    let mutable distribution = distribution'
    let mutable name = name'
    let mutable space = space'
    let dependencies = new HashSet<RandomVariable>()

    member public self.Name 
        with get() : String = name

    member public self.Space 
        with get() : RandomVariableSpace = space

    member public self.Distribution 
        with get() : Distribution   =   distribution
        and set(value)              =   distribution <- value

    member public self.AddDependency rv = 
        dependencies.Add rv

    member public self.RemoveDependency rv =
        dependencies.Remove rv |> ignore

    member public self.Dependencies
        with get() = dependencies :> seq<_>
    
        

