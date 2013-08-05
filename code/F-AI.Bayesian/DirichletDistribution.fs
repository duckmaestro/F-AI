
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
/// A dirichlet distribution.
///
type DirichletDistribution (parameters:Map<Real,Real>) = 

    //
    // The parameters, indexed by event value.
    //
    let mutable parameters = parameters


    ///
    /// Parameter-less constructor.
    ///
    new() =
        DirichletDistribution (Map.empty)

    ///
    /// Assigns a Dirichlet parameter for the given event value.
    ///
    member public self.SetParameter value parameter =
        if Real.IsNaN value then 
            invalidArg "value" "Value must not be missing." 
        else if parameter <= 0. then
            invalidArg "parameter" "Parameter must be positive."
        else 
            parameters <- parameters |> Map.add value parameter

    ///
    /// Gets a Dirichlet parameter for a given event value.
    ///
    member public self.GetParameter value =
        parameters |> Map.tryFind value
        
    ///
    /// Retrieves all parameters.
    ///
    member public self.Parameters
        with get() = 
            parameters :> IEnumerable<_>
