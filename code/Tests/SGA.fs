
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

module TestSGA

open MathNet
open StochasticGradientAscent

let Run = 
    
    // Obj Func         = -(x-2)^2 - (y+10)^2 - 50
    // Obj Func Grad    = < -2 * (x - 2) , -2 (y + 10) >

    let objFuncGrad (theta:Vector) sample = 
        vector [| -2. * ((theta.Item 0) - 2.); -2. * ((theta.Item 1) + 10.); |]
    
    let sampleSet = [|1..1000|]
    let thetaCorrect = vector [| 2.; -10. |]
    let theta = vector [| 100.; -100. |]
    let theta' = GradientAscentSampleSet objFuncGrad theta sampleSet 0.01 10 None

    assert ( (theta' - thetaCorrect).Norm(2.) < 0.001 )

    ()