
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
/// A random variable.
/// Contains a mutable dependency list 
/// and mutable distribution.
///
type public RandomVariable(name', space', distribution') =
    
    let mutable distribution = distribution'
    let mutable name = name'
    let mutable space = space'
    let mutable prior = None
    let mutable userData = null
    
    let parents = new HashSet<RandomVariable>()
    let children = new HashSet<RandomVariable>()

    ///
    /// The name of this variable, e.g. Earthquake.
    ///
    member public self.Name 
        with get() : String = name

    ///
    /// The space of possible values for this variable.
    ///
    member public self.Space 
        with get() : Space = space

    ///
    /// The probability distribution associated with this
    /// variable.
    ///
    member public self.Distribution 
        with get() : Distribution   =   distribution
        and set(value)              =   distribution <- value

    ///
    /// The Dirichlet prior parameters.
    ///
    member public self.Prior
        with get() : option<DirichletDistribution>  =   prior
        and set(value)                              =   prior <- value

    ///
    /// Links two variables together, using the given
    /// variable as a parent.
    ///
    member public self.AddParent rv = 
        if self.Parents |> Seq.exists (fun p -> p = rv) then
            ()
        else if self = rv then
            invalidArg "rv" "Cannot a variable as a parent to itself."
        else
            self.AddParent' rv
            rv.AddChild' self
            ()

    member private self.AddParent' (rv:RandomVariable) =
        parents.Add rv |> ignore

    ///
    /// Separates two variables, breaking the connection
    /// to the given parent.
    ///
    member public self.RemoveParent rv =
        self.RemoveParent' rv
        rv.RemoveChild' self

    member private self.RemoveParent' (rv:RandomVariable) =
        parents.Remove rv |> ignore

    ///
    /// The parent variables to this variable.
    ///
    member public self.Parents
        with get() = parents :> IEnumerable<_>

    ///
    /// Links two variables together, using the given
    /// variable as a child.
    ///
    member public self.AddChild rv =
        if self.Children |> Seq.exists (fun c -> c = rv) then
            ()
        else if self = rv then
            invalidArg "rv" "Cannot a variable as a child to itself."
        else
            self.AddChild' rv
            rv.AddParent' self
            ()

    member private self.AddChild' rv =
        children.Add rv |> ignore

    ///
    /// Separates two variables, breaking the connection
    /// to the given child.
    ///
    member public self.RemoveChild rv =
        self.RemoveChild' rv
        rv.RemoveParent' self

    member private self.RemoveChild' rv =
        children.Remove rv |> ignore

    ///
    /// The child variables to this variable.
    ///
    member public self.Children
        with get() = children :> IEnumerable<_>
    
    ///
    /// An arbitrary object reference, kept but ignored.
    ///
    member public self.UserData
        with get()      =   userData
        and set(value)  =   userData <- value
    
