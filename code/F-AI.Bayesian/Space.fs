﻿
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
type Space = 
    | Discrete of Map<EventValue,String>      // A list of valid values and their labels.

    member public self.Size
        with get() = 
            match self with
            | Discrete map      -> map.Count

    member public self.Values
        with get() = 
            match self with
            | Discrete map      -> map |> Map.toSeq |> Seq.map (fun (k,v) -> k)

    ///
    /// Returns the label associated with the given value from this space.
    ///
    member public self.GetLabel value =
        let map = 
            match self with
            | Discrete (labels)     -> Some labels
        
        match map with
        | Some map'     -> map' |> Map.find value
        | None          -> ""

    ///
    /// Returns true if the given value is defined in this space.
    ///
    member public self.IsDefined value =
        let map = 
            match self with
            | Discrete (labels)     -> labels

        map |> Map.containsKey value
            
    static member public MakeIntegerSpace min max =
        let values = { min .. 1.0f .. max }
        let names = values |> Seq.map (fun v -> v.ToString())
        let map = Map.ofSeq (Seq.zip values names)
        Discrete map
        