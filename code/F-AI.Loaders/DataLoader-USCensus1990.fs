
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

module FAI.Loaders.USCensus1990


// namespaces

open System
open System.IO
open System.Text
open FAI.Bayesian


// private functions

let private ParseSample (variableNames:seq<String>) (ignoreList) (row:String) = 
    let components = row.Split(',')
    let componentsNum = components |> Seq.length

    let values =
        components
        |> Seq.zip variableNames
        |> Seq.filter (fun (n,t) -> false = (ignoreList |> Set.contains n))
        |> Seq.map
            (fun (n,t) -> 
                n,
                EventValue.Parse(t)
            )
        |> Map.ofSeq
        |> fun s -> new Observation(s)

    values


// functions

let LoadFromFile filePath =
    
    let name = System.IO.Path.GetFileNameWithoutExtension filePath
        
    let rows = 
        File.ReadLines filePath

    // Gather and rewrite headers.
    let header =
        rows
        |> Seq.head
        |> fun s -> s.Split ','
        |> Seq.map (fun h -> h.ToLower())
        |> Seq.map (fun h ->    match (h.Chars 0) with 
                                | 'i' -> h.Substring(1)
                                | 'd' -> h.Substring(1)
                                | _   -> h
                    )
        |> Seq.map (fun h ->    match h with
                                | "ragechld"    -> "agechld_rm"
                                | "rearning"    -> "earning_rm"
                                | "remplpar"    -> "emplpar_rm"
                                | "rfaminc"     -> "faminc_rm"
                                | "rfampers"    -> "fampers_rm"
                                | "rfarm"       -> "farm_rm"
                                | "rhhlang"     -> "hhlang_rm"
                                | "rlabor"      -> "labor_rm"
                                | "rownchld"    -> "ownchld_rm"
                                | "rpincome"    -> "pincome_rm"
                                | "rpob"        -> "pob_rm"
                                | "rrelchld"    -> "relchld_rm"
                                | "rspouse"     -> "spouse_rm"
                                | "rsubfam"     -> "subfam_rm"
                                | "rvetserv"    -> "vetserv_rm"
                                | "rwrkr89"     -> "wrkr89_rm"
                                | "rtaxamt"     -> "taxamt_rm"
                                | _             -> h
                    )
        |> Seq.toArray

    let headerIgnore = Set.ofList [ "caseid"; "vetserv_rm" ]
    let headerFiltered = header |> Seq.filter (fun h -> false = (headerIgnore |> Set.contains h)) |> Seq.cache

    let samplesAsStrings = 
        rows
        |> Seq.skip 1
        |> Seq.take 20000   // TEMPORARY LIMIT.

    let samples =
        samplesAsStrings
        |> Seq.filter (fun r -> r.Length <> 0)
        |> Seq.map (fun r -> ParseSample header headerIgnore r)
        |> Seq.toArray

    // Build spaces.
    let mutable mins = Map.empty
    let mutable maxs = Map.empty

    for s in samples do
        for h in headerFiltered do
            let ifNotFound def exp = defaultArg exp def
            let value = Option.get <| s.TryValueForVariable h
            let min' = mins |> Map.tryFind h |> ifNotFound EventValue.MaxValue
            let max' = maxs |> Map.tryFind h |> ifNotFound EventValue.MinValue
            if value < min' then
                mins <- mins |> Map.add h value
            else if value > max' then
                maxs <- maxs |> Map.add h value
            ()

    let mins = mins
    let maxs = maxs

    // Defines spaces.
    let makeSpace (variableName:Identifier) = 
        let pairList lst = Discrete (lst |> Seq.map (fun (v,k) -> float32(v),k) |> Map.ofSeq)
        let labelList vstart vend lst = Discrete (lst |> Seq.zip { (float32 vstart) .. 1.f .. (float32 vend) } |> Map.ofSeq)

        match variableName.ToLower () with
        | "sex" -> labelList 0 1 [  "male";
                                    "female"; ]
        | "relat1" -> labelList 0 13 [  "Householder";
                                        "Husband/wife";
                                        "Son/daughter";
                                        "Stepson/stepdaughter";
                                        "Brother/sister";
                                        "Father/mother";
                                        "Grandchild";
                                        "Other not related";
                                        "Roomer/boarder/foster child";
                                        "Housemate/roommate";
                                        "Unmarried partner";
                                        "Other nonrelative group quarters";
                                        "Institutionalized person";
                                        "Other persons in group quarters"; ]
        | "agechld_rm" -> labelList 0 4 [   "N/A (male)";
                                            "Own children < 6 yrs";
                                            "Own children 6 to 17 yrs";
                                            "Own children both < 6 yrs and 6 to 17 yrs";
                                            "4"; ]
        | "spouse_rm" -> labelList 0 6 [    "N/A (less than 15 yrs old)";
                                            "Now married, spouse present";
                                            "Now married, spouse absent";
                                            "Widowed";
                                            "Divorced";
                                            "Separated";
                                            "Never married"; ]
        | "emplpar_rm" -> pairList [    0, "N/A";
                                        111, "Both parents 35 hrs+";
                                        112, "Both parents Father only 35 hrs+";
                                        113, "Both parents Mother only 35 hrs+";
                                        114, "Both parents Neither 35 hrs+";
                                        121, "Both parents Father 35 hrs+";
                                        122, "Both parents Father not 35 hrs+";
                                        133, "Both parents Mother 35 hrs+";
                                        134, "Both parents Mother not 35 hrs+";
                                        141, "Both parents Neither in labor force";
                                        211, "One parent Father 35 hrs+";
                                        212, "One parent Father not 35 hrs+";
                                        213, "One parent Father not in labor force";
                                        221, "One parent Mother 35 hrs+";
                                        222, "One parent Mother not 35 hrs+";
                                        223, "One parent Mother not in labor force"; ]
        | "fertil" -> labelList 0 13 [  "Male or < 15 yrs";
                                        "No children";
                                        "1 child";
                                        "2 children";
                                        "3 children";
                                        "4 children";
                                        "5 children";
                                        "6 children";
                                        "7 children";
                                        "8 children";
                                        "9 children";
                                        "10 children";
                                        "11 children"; 
                                        "12+ children"; ]
        | "yearssch" -> labelList 0 17 [    "N/A (less than 3 yrs old)";
                                            "No school completed";
                                            "Nursery school";
                                            "Kindergarten";
                                            "1st, 2nd, 3rd, or 4th grade";
                                            "5th, 6th, 7th, or 8th grade";
                                            "9th grade";
                                            "10th grade";
                                            "11th grade";
                                            "12 grade no diploma";
                                            "high school graduate or GED";
                                            "some college, no degree";
                                            "associate degree, occupational";
                                            "associate degree, academic";
                                            "bachelor's degree";
                                            "master's degree";
                                            "professional degree";
                                            "doctorate degree"; ]
        | "pob_rm" -> pairList [    10, "Born in state of residence";
                                    21, "Northeast";
                                    22, "Midwest";
                                    23, "South";
                                    24, "West";
                                    31, "Puerto Rico";
                                    32, "American Samoa";
                                    33, "Guam";
                                    34, "Northern Marianas";
                                    35, "US Virgin Islands";
                                    36, "Elsewhere U.S. outlying";
                                    40, "Born abroad American parents";
                                    51, "Naturalized citizen";
                                    52, "Not a citizen"; ]
        | "labor_rm" -> labelList 0 6 [ "N/A (less than 16 yrs old)";
                                        "Civilian employed, at work";
                                        "Civilian employed, not at work";
                                        "Unemployed";
                                        "Armed forces, at work";
                                        "Armed forces, not at work";
                                        "Not in labor force"; ]
        | "class" -> labelList 0 9 [    "N/A less than 16 yrs";
                                        "employee private for-profit";
                                        "employee private not-for-profit";
                                        "employee local government";
                                        "employee state government";
                                        "employee federal government";
                                        "self-employed not incorporated";
                                        "self-employed incorporated";
                                        "family business or farm";
                                        "unemployed worked <= 1948"; ]
        | "means" -> labelList 0 12 [   "N/A (not a worker)";
                                        "Car, truck, or van";
                                        "Bus or trolley bus";
                                        "Streetcar or trolley car";
                                        "Subway or elevated";
                                        "Railroad";
                                        "Ferryboat";
                                        "Taxicab";
                                        "Motorcycle";
                                        "Bicycle";
                                        "Walked";
                                        "Worked at home";
                                        "Other"; ]
        | _ ->
            let min = mins |> Map.tryFind variableName |> Option.get
            let max = maxs |> Map.tryFind variableName |> Option.get
            Space.MakeIntegerSpace min max

    // Gather spaces.
    let allSpaces = 
        headerFiltered
        |> Seq.map (fun variableName -> variableName, (makeSpace variableName) )
        |> Map.ofSeq
        
    // Store in observation set.
    new InMemoryObservationSet (name, allSpaces, samples, filePath)
    
