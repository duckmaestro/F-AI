
open MathNet
open StochasticGradientAscent


// Test SGA
let TestSGA = 
    
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