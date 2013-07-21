
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

open System.Collections
open System.Collections.Generic


///
/// A collection of observed values, indexed by variable name.
/// Immutable, and has some operator overloads.
///
[<System.Diagnostics.DebuggerDisplayAttribute("Variables: {_variableValues}")>]
type public Observation(?variableValues) =

    member private self._variableValues = defaultArg variableValues Map.empty

    ///
    /// Performs the union of this observation with another, 
    /// returning a new observation. Right operand
    /// values take precedence.
    ///
    static member (.|.) (o1:Observation, o2:Observation) =
        let mutable o1' = o1._variableValues
        for kvp in o2._variableValues do
            o1' <- o1' |> Map.add kvp.Key kvp.Value
        new Observation(o1')

    ///
    /// Adds a new variable and value, returning a new observation.
    ///
    static member (.+.) (o1:Observation, (name, value)) =
        let o1' = o1._variableValues |> Map.add name value
        new Observation(o1')

    ///
    /// Removes the right operand variables from the left operand,
    /// returning a new observation.
    ///
    static member (.-.) (o1:Observation, o2:Observation) =
        let mutable o1' = o1._variableValues
        for kvp in o2._variableValues do
            o1'  <- o1' |> Map.remove kvp.Key
        new Observation(o1')

    ///
    /// Removes the named variable from the observation, 
    /// returning a new observation.
    ///
    static member (.-.) (o1:Observation, name:VariableName) =
        let o1' = 
            o1._variableValues
            |> Map.remove name
        new Observation(o1')

    /// 
    /// Performs the intersection of this observation with the 
    /// list of provided variable names.
    /// 
    static member (.&.) (o1:Observation, names:VariableName seq) = 
        let mutable o1' = o1._variableValues
        for kvp in o1' do
            if (Seq.exists (fun n -> n = kvp.Key) names) = false then
                o1' <- o1' |> Map.remove kvp.Key
        new Observation(o1')
        

    ///
    /// If both observations have the same keys and values, then they are equal.
    ///
    override o1.Equals o2 =
        let o2' = o2 :?> Observation

        let countsDiffer = o1._variableValues.Count <> o2'._variableValues.Count
        if countsDiffer then
            false
        else
            let hasDifference = 
                o1._variableValues
                |> Seq.zip o2'._variableValues
                |> Seq.exists (fun z -> (fst z).Key <> (snd z).Key || (fst z).Value <> (snd z).Value)
            hasDifference = false

    interface System.IComparable with
      member o1.CompareTo o2 =
          match o2 with
          | :? Observation as o2'   ->  Unchecked.compare o1._variableValues o2'._variableValues
          | _                       ->  failwith "Invalid argument type."

    override o1.GetHashCode () =
        Unchecked.hash o1._variableValues

    interface IEnumerable<KeyValuePair<VariableName, Real>> with
        member self.GetEnumerator () =
            let vv = self._variableValues :> IEnumerable<KeyValuePair<VariableName, Real>>
            vv.GetEnumerator ()

    interface IEnumerable with
        member self.GetEnumerator () =
            let vv = self._variableValues :> IEnumerable
            vv.GetEnumerator ()

    ///
    /// Returns the value associated with the given variable name.
    ///
    member self.TryValueForVariable name =
        self._variableValues.TryFind name

    ///
    /// Returns the list of variable names in this observation
    ///
    member self.VariableNames 
        with get() : VariableName seq = self._variableValues |> Seq.map (fun kvp -> kvp.Key)
