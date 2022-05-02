
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


namespace FAI.NeuralNetworks

open MathNet

///
/// A network of connected layers.
///
type public Network(layers:Layer list) =

    let layersRev = layers |> List.rev
    
    ///
    /// Evaluate the network.
    ///
    member public self.Evaluate(input:Vector) : LayerEvaluationRecord array = 
    
        // Compute our first layer's output.
        let layer1 = List.head layers
        let mutable valuesRev = [layer1.Evaluate2(input)]
        for layer in List.tail layers do
            // Grab the most recent output value.
            let valueLast = List.head valuesRev

            // Feed forward into the next layer.
            let valueNext = layer.Evaluate2(valueLast.Output)

            // Store
            valuesRev <- valueNext :: valuesRev

        let results = valuesRev |> List.toArray |> Array.rev
        results


    ///
    /// Evaluate the gradiant of the network at the provided point with respect to its weights.
    ///
    member public self.EvaluateGradiantFromWeights(evaluationRecords: LayerEvaluationRecord array, lossLocalGradiant) : Matrix list =

        // For each layer moving backward in the network we compute its gradiant.
        let mutable gradiants = [ ]
        let mutable downstreamError =  lossLocalGradiant

        for layer, result  in Seq.zip layersRev (evaluationRecords |> Array.rev)  do

            let gradiant = layer.EvaluateGradiantFromWeights(result.Input, result.WeightedInput, downstreamError )
            gradiants <- gradiant :: gradiants
            downstreamError <- gradiant.LocalError

        gradiants
        |> List.map (fun gr-> gr.Gradiant)


    ///
    /// 
    ///
    member public self.MinimizeLoss(input, output, rate:float) =
        
        let valueDesired = output
        let valuesComputed = self.Evaluate(input)
        let difference =  valuesComputed |> Array.last |> (fun f -> f.Output - valueDesired)
        
        let gradiant = self.EvaluateGradiantFromWeights(valuesComputed, difference)

        for layer,gradiant in List.zip layers gradiant do

            layer.AdjustWeights ( gradiant.Multiply( rate ))

        ()