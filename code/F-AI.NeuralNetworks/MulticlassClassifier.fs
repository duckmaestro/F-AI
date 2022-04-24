
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

open System
open FAI

type public MulticlassClassifier(featureLength, classesSize, layers:int) =

    do 
        assert (layers >= 1)

    let a (x:array<_>) (y) = x.Length - y
    let z = 1 |> a [|0|]

    let connectivity = Connectivity.FullyConnected
    let weights = Weights.Random (-1.0, +1.0, 0)
    let activationOutput = Activation.Sigmoid
    let activationMiddle = Activation.ReLU

    let layers = 
        [new Layer(connectivity, weights, activationOutput, featureLength, classesSize)]
        |> List.append 
            (
                [1..layers-1] |> List.map (fun _ -> new Layer(connectivity, weights, activationMiddle, featureLength, featureLength)) 
            )

    let network = new Network(layers)

    interface IClassifier with
        member self.Train samples =
            ()
        member self.Classify sample =
            assert (sample.Features.Count = featureLength)
            assert (sample.Label >= 0 && sample.Label < classesSize)

            let output = network.Evaluate(sample.Features)
            let maxIndex = output.MaximumIndex()

            maxIndex

