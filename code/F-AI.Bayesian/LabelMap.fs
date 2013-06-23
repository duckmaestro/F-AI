
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


type LabelMap(labels) =
    static member public MissingValue = 0    

    member private  this._Labels = labels |> (fun ls -> Seq.singleton "MissingValue" |> Seq.append ls) |> Seq.toArray
    member public   this.ToLabel id = 
        let label = if id < 0 || id >= this._Labels.Length then
                        "UnknownLabel"
                    else
                        this._Labels.[id]
        label
    member public   this.ToId label =
        match this._Labels |> Seq.tryFindIndex (fun l -> l.Equals label) with
            | Some(id)  -> id
            | None      -> -1
