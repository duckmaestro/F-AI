
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

module TestLBFGS

open Microsoft.SolverFoundation.Services
open Microsoft.SolverFoundation.Solvers
open System

let Run = 

    // http://www.wolframalpha.com/input/?i=minimize+f(x%2Cy)+%3D+100+(y+-+x%5E2)%5E2+%2B+(1-x)%5E2

    let solverParams = new CompactQuasiNewtonSolverParams()
    let solver = new CompactQuasiNewtonSolver()
    
    // http://msdn.microsoft.com/en-us/library/dd233186.aspx
    let rowid = ref 0
    let varid1 = ref 0
    let varid2 = ref 0
    solver.AddVariable(null, varid1) |> ignore
    solver.AddVariable(null, varid2) |> ignore
    solver.AddRow(null, rowid) |> ignore
    solver.AddGoal(!rowid, 0, true) |> ignore

    solver.FunctionEvaluator <- 
        fun model rowId theta isFirst -> 
            Math.Pow(1.0 - theta.Item 1, 2.0) 
                + 100.0 * (Math.Pow(theta.Item 2 - (theta.Item 1 * theta.Item 1), 2.0))

    solver.GradientEvaluator <-
        fun model rowId theta isFirst out -> 
            out.Item 1 <- -2.0 * (1.0 - theta.Item 1) - 400.0 * theta.Item 1 * (theta.Item 2 - (theta.Item 1 * theta.Item 1))
            out.Item 2 <- 200.0 * (theta.Item 2 - (theta.Item 1 * theta.Item 1))
            ()


    let solution = solver.Solve solverParams
    let varvalue1 = solver.GetValue(!varid1).ToDouble()
    let varvalue2 = solver.GetValue(!varid2).ToDouble()

    assert (Math.Abs(varvalue1 - 1.0) < 0.0001)
    assert (Math.Abs(varvalue2 - 1.0) < 0.0001)

    ()

