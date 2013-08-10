
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


module ListHelpers


let splitListAt list element =
    let indexOfElement = list |> List.findIndex (fun item -> item = element)
    let split1 = list |> List.mapi (fun i e -> i,e) |> List.filter (fun (i,e) -> i < indexOfElement) |> List.map (fun (i,e) -> e)
    let split2 = list |> List.mapi (fun i e -> i,e) |> List.filter (fun (i,e) -> i >= indexOfElement) |> List.map (fun (i,e) -> e)
    split1,split2

let insertBefore list before value =
    let l1,l2 = splitListAt list before
    List.append (l1) (value::l2)

let insertAfter list after value =
    let l1,l2 = splitListAt list after
    let l1' = List.append l1 [ l2.Head ]
    let l2' = match l2 with | h::rest -> rest | _ -> []
    List.append (l1') (value::l2')

