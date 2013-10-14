
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
[<System.Diagnostics.DebuggerDisplay("{Name}")>]
type public RandomVariable(name, space, ?distributions, ?parents, ?children, ?userData) =
    
    let mutable distributions = defaultArg distributions (new DistributionSet ())
    let mutable name = name
    let mutable space = space
    let mutable userData = defaultArg userData null
    
    let parents = defaultArg parents Set.empty
    let children = defaultArg children Set.empty

    ///
    /// Initializes an empty random variable with just a name and a space.
    ///
    new(name, space) =
        RandomVariable(name, space, new DistributionSet(), Set.empty, Set.empty)

    ///
    /// Initializes a random variable with a name, space, and distribution set.
    ///
    new(name, space, distributions) =
        RandomVariable(name, space, distributions, Set.empty, Set.empty)

    ///
    /// Clones the random variable and adds a new parent.
    ///
    member public self.CloneAndAddParent parentName =
        if parentName = name then
            failwith "Cannot add self as parent."

        let parents = parents |> Set.add parentName
        new RandomVariable(name, space, distributions, parents, children, userData)

    ///
    /// Clones the random variable and removes a parent.
    ///
    member public self.CloneAndRemoveParent parentName =
        let parents = parents |> Set.remove parentName
        new RandomVariable(name, space, distributions, parents, children, userData)

    ///
    /// Clones the random variable and adds a new child.
    ///
    member public self.CloneAndAddChild childName =
        if childName = name then
            failwith "Cannot add self as child."

        let children = children |> Set.add childName
        new RandomVariable(name, space, distributions, parents, children, userData)

    ///
    /// Clones the random variable and removes a child.
    ///
    member public self.CloneAndRemoveChild childName =
        let children = children |> Set.remove childName
        new RandomVariable(name, space, distributions, parents, children, userData)

    ///
    /// Clones the random variable and removes all parents and children.
    /// 
    member public self.CloneAndDisconnect () =
        new RandomVariable(name, space, distributions, Set.empty, Set.empty, userData)

    ///
    /// Clones the random variable and assigns a new local distribution.
    ///
    member public self.CloneAndSetDistribution distribution =
        new RandomVariable(name, space, distribution, parents, children, userData)

    ///
    /// The name of this variable, e.g. Earthquake.
    ///
    member public self.Name 
        with get() : Identifier = name

    ///
    /// The space of possible values for this variable.
    ///
    member public self.Space 
        with get() : Space = space

    ///
    /// The local probability distribution(s) associated with this variable.
    ///
    member public self.Distributions
        with get() : DistributionSet    =   distributions

    ///
    /// Retrieves the local conditional distribution for this variable given a
    /// parent instantiation.
    ///
    member public self.GetConditionalDistribution (parentValues:Observation) =
        let parentValues = parentValues .&. self.Parents
        let distribution = distributions.TryGetDistribution parentValues
        match distribution with
        | None      ->  failwith "Distribution not found."
        | Some d    ->  d

    ///
    /// The parent variables to this variable.
    ///
    member public self.Parents
        with get() = parents :> IEnumerable<_>

    ///
    /// The child variables to this variable.
    ///
    member public self.Children
        with get() = children :> IEnumerable<_>

    ///
    /// Returns true if the given variable is a child of this variable.
    ///
    member public self.HasChild variableName =
        children |> Set.contains variableName

    ///
    /// Returns true if the given variable is a parent to this variable.
    ///
    member public self.HasParent variableName =
        parents |> Set.contains variableName

    ///
    /// Resolves the parent variables using the provided variable map.
    ///
    member public self.GetParentVariables (map:IDictionary<Identifier,RandomVariable>) =
        seq {
            for parentName in parents do
                let parentVariable = map.Item(parentName)
                yield parentVariable
        }

    ///
    /// Resolves the children variables using the provided variable map.
    ///
    member public self.GetChildrenVariables (map:IDictionary<Identifier,RandomVariable>) =
        seq {
            for childName in children do
                let childVariable = map.Item(childName)
                yield childVariable
        }
    
    ///
    /// An arbitrary object reference, kept but ignored.
    ///
    member public self.UserData
        with get()      =   userData
        and set(value)  =   userData <- value
    
