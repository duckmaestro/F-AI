
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



//
//    Perceptron learner.
//

module Perceptron


// namespaces

open System
open MathNet
open FAI
open Classifiers


// records

type private NormalWithWeight = {
    Normal : Vector;
    Weight : float;
}


// private functions

let private OneExtend (v:Vector) = 
    let components = v :> seq<float>
    let componentsOneExtended = 
        v 
        |> Seq.append (Seq.singleton 1.0) 

    let vExt = vector componentsOneExtended
    vExt


// types

type ClassificationMethod = 
    | Simple = 0
    | Voted = 1
    | Weighted = 2

type PerceptronClassifier(firstLabel, secondLabel, classificationMethod:ClassificationMethod) =

    let mutable hyperplanes = Array.empty<NormalWithWeight>
    let mutable d = 0

    let negativeLabel = firstLabel
    let positiveLabel = secondLabel
    
    let RemapLabel label =
        match label with
        | x when x = positiveLabel -> 1
        | x when x = negativeLabel -> -1
        | _ -> failwith "Unexpected label."

    let RemapLabelInv value =
        match value with
        | 1 -> positiveLabel
        | -1 -> negativeLabel
        | _ -> failwith "Unexpected value."

    interface IClassifier with
        member self.Train samples =

            // 1-extended and remap labels
            let samples1Extended = 
                samples 
                |> Seq.map (fun s -> { Features = OneExtend s.Features; Label = RemapLabel s.Label })

            // remember dimension
            d <- samples1Extended |> Seq.head |> (fun s -> s.Features.Count)

            // initialize normal history
            let mutable wListTemp = new System.Collections.Generic.List<NormalWithWeight>()


            // grab the first sample
            let sampleFirst = samples1Extended |> Seq.head

            // initialize w
            let mutable w = sampleFirst.Features.Multiply(float(sampleFirst.Label))

            // initialize success count
            let mutable c = 1.0


            // main training loop
            for sample in samples1Extended |> Seq.skip(1) do
                let dotProduct = sample.Features.DotProduct w

                let correctlyClassifies = Math.Sign (dotProduct * float(sample.Label))

                if correctlyClassifies >= 0 then
                    c <- c + 1.0
                else
                    // remember most recent normal
                    wListTemp.Add {Normal = w; Weight = c;}

                    // calculate new normal
                    let sampleTimesLabel = sample.Features.Multiply(float(sample.Label))
                    w <- w.Add sampleTimesLabel
                    
                    // reset count 
                    c <- 1.0
            
            // push last w
            wListTemp.Add {Normal = w; Weight = c;}

            // save list
            hyperplanes <- wListTemp.ToArray()

            // done
            ()

        member self.Classify point =

            let floatToLabel (value:float) =
                let sign = Math.Sign value
                match sign with
                    | 1 -> RemapLabelInv 1
                    | 0 -> RemapLabelInv 1
                    | -1 -> RemapLabelInv -1
                    | _ -> failwith "Unexpected sign."    

            let classifySimple point = 
                let pointExt = OneExtend point
                let lastNormal = hyperplanes |> Seq.toArray |> Array.rev |> Seq.head
                let strength = lastNormal.Normal.DotProduct pointExt
                floatToLabel strength

            let classifyVoted point =
                let pointExt = OneExtend point

                // sum the (signed) votes
                let votesTotal = 
                    hyperplanes
                    |> Seq.map (fun w -> int(w.Weight) * Math.Sign((w.Normal.DotProduct pointExt)))
                    |> Seq.sum
                    |> float

                floatToLabel votesTotal

            let classifyWeighted point =
                let pointExt = OneExtend point
                
                let averageNormal =
                    hyperplanes
                    |> Seq.map (fun w -> w.Normal.Multiply(w.Weight))
                    |> Seq.fold (fun (sum:Vector) w -> sum.Add w) (vector <| Array.zeroCreate d)

                let strength = averageNormal.DotProduct pointExt

                floatToLabel strength
           
            match classificationMethod with
                | ClassificationMethod.Simple -> classifySimple point.Features
                | ClassificationMethod.Voted -> classifyVoted point.Features
                | ClassificationMethod.Weighted -> classifyWeighted point.Features
                | _ -> failwith "Unexpected classification method."
            

    override self.ToString () =
        let buffer = new System.Text.StringBuilder()
        ignore <| buffer.AppendLine("Normals:")
        for v in hyperplanes do
            ignore <| buffer.AppendFormat( "\tnormal:{0}\tweight:{1}\r\n", v.Normal, v.Weight)
        
        buffer.ToString ()
