
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
/// A discrete probability distribution.
///
type DiscreteDistribution() = 
    
    // Internal storage of the probability masses, indexed by
    // random variable value.
    let mutable masses = Map.empty

    ///
    /// Assign a probability mass to a particular value.
    ///
    member public self.SetMass value (mass:Real) =
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
