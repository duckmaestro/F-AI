
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
    
    /// Internal storage of the probability masses, indexed by
    /// random variable value.
    let masses = new Dictionary<Real, Real>()

    /// Assign a probability mass to a particular value.
    member public self.SetMass value mass =

        if Real.IsNaN value then 
            invalidArg "value" "Value must not be missing." 
        else 
            masses.Item value <- mass

    /// Get the probability mass of a particular value.
    member public self.GetMass value =

        let mutable mass = 0.0

        if masses.TryGetValue(value, ref mass) then 
            mass
        else 
            invalidArg "value" "The given value does not have a known mass."


