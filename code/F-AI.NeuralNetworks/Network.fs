
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
    member public self.Evaluate(input:Vector) : Vector = 
    
        let mutable v = input
    
        for layer in layers do
            v <- layer.Evaluate(v)

        v


    ///
    /// Evaluate the gradiant of the network at the provided point with respect to its weights.
    ///
    member public self.EvaluateGradiantFromWeights(input:Vector) : Matrix =
        
        // For each layer moving forward in the network, evaluate.
        let layer1 = List.head layers
        let mutable valuesRev = [layer1.Evaluate2(input)]
        for layer in List.tail layers do
            let value = List.head valuesRev
            valuesRev <- layer.Evaluate2(value.ActivatedOutput) :: valuesRev

        // For each layer moving backward in the network we compute its gradiant.
        let mutable gradiants = [ ]
        for layer in layersRev do
            // Pop off the last value.
            let value = List.head valuesRev
            valuesRev <- List.tail valuesRev

            let gradiant = layer.EvaluateGradiantFromWeights(value.Input)
            gradiants <- gradiant :: gradiants

        // Combine for total gradiant.
        let totalGradiant =
            gradiants
            |> List.reduce(fun x y -> x.PointwiseMultiply y)

        totalGradiant


    ///
    /// 
    ///
    member public self.MinimizeLoss(input, output, rate:float) =
        
        let outputDesired = output
        let outputComputed = self.Evaluate(input)
        let gradiant = self.EvaluateGradiantFromWeights(input)

        for layer in layers do
            layer.AdjustWeights ( gradiant.Multiply( rate ))

        ()