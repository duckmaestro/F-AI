
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
/// A discrete probability distribution.
///
type DiscreteDistribution() = 
    
    // Internal storage of the probability masses, indexed by
    // random variable value.
    let mutable masses = Map.empty

    ///
    /// Assign a probability mass to a particular value.
    ///
    member public self.SetMass (value:Real) (mass:Real) =
        if Real.IsNaN value then 
            invalidArg "value" "Value must not be missing." 
        else 
            masses <- masses |> Map.add value mass

    ///
    /// Get the probability mass of a particular value.
    ///
    member public self.GetMass value =
        masses |> Map.tryFind value

    ///
    /// Removes a mass from the distribution.
    ///
    member public self.RemoveMass value =
        masses <- masses |> Map.remove value

    ///
    /// Retrieves known masses.
    ///
    member public self.Masses 
        with get() = 
            masses :> IEnumerable<_>

    ///
    /// Equality based on masses.
    ///
    override self.Equals other =
        0 = (self :> IComparable).CompareTo other

    ///
    /// Hash code based on masses.
    ///        
    override self.GetHashCode () =
        self.Masses
        |> Seq.fold (fun s m -> s + m.GetHashCode()) 0

    ///
    /// Comparison based on masses.
    ///
    interface IComparable with
        member self.CompareTo other = 
            match other with
            | :? DiscreteDistribution as other -> 
                let otherMasses = other.Masses :?> Map<_,_>
                (masses :> IComparable).CompareTo(otherMasses)
            | _ -> -1
