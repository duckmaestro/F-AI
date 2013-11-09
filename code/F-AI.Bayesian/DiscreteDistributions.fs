
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
/// An immutable discrete probability distribution.
///
type DiscreteDistribution(?masses) = 

    // Internal storage of the probability masses, indexed by
    // random variable value.
    let masses = defaultArg masses Map.empty

    ///
    /// Initializes an empty distribution.
    /// 
    new() =
        DiscreteDistribution(Map.empty)

    ///
    /// Initializes a distribution using a sequence of tuples.
    ///
    new(masses:seq<System.Tuple<EventValue,ProbabilityMass>>) =
        let masses = 
            masses 
            |> Seq.map (fun t -> t.Item1 ,t.Item2)
            |> Map.ofSeq
        DiscreteDistribution(masses)
  
    ///
    /// Assign a probability mass to a particular value.
    ///
    member public self.CloneAndSetMass (value:EventValue) (mass:ProbabilityMass) =
        if EventValue.IsNaN value then 
            invalidArg "value" "Value must not be missing." 
        else if ProbabilityMass.IsNaN mass || ProbabilityMass.IsInfinity mass then
            invalidArg "mass" "Mass must be finite."
        else 
            new DiscreteDistribution (masses |> Map.add value mass)

    ///
    /// Get the probability mass of a particular value.
    ///
    member public self.GetMass value =
        masses |> Map.tryFind value

    ///
    /// Removes a mass from the distribution.
    ///
    member public self.CloneAndRemoveMass value =
        new DiscreteDistribution (masses |> Map.remove value)

    ///
    /// Retrieves known masses.
    ///
    member public self.Masses 
        with get() = 
            masses :> IEnumerable<_>

    ///
    /// Computes the asymmetric KL-divergence D_kl(this||other). If the 
    /// distributions do not share the same events, the result is NaN. If any 
    /// other(i) is 0 while this(i) is non-0, the result is +Inf.
    ///
    member public self.KLDivergence (other:DiscreteDistribution) = 
        let eventsFromDist (dist:DiscreteDistribution) =
            dist.Masses |> Seq.map (fun kvp -> kvp.Key) |> Set.ofSeq
            
        let eventsSelf = eventsFromDist self
        let eventsOther = eventsFromDist other

        if eventsSelf <> eventsOther then
            // If event space is different, return NaN.
            System.Double.NaN
        else
            // Compute: ln (P(i) / Q(i)) * P(i)
            let klSummand probP probQ = 
                if probP = 0. then 
                    0.
                else if probQ = 0. then 
                    System.Double.PositiveInfinity
                else
                    let dividend = probP / probQ
                    let log = Math.Log dividend
                    let weighted = log * probP
                    weighted

            // Compute and sum each term.
            let kl = 
                eventsSelf
                |> Seq.map (fun event -> self.GetMass event |> Option.get, other.GetMass event |> Option.get)
                |> Seq.map (fun (pi, qi) -> klSummand pi qi)
                |> Seq.sum

            kl

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
