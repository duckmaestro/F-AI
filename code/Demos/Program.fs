
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


// namespaces

open System


// options
let performSupervisedTests = true
let performUnsupervisedTests = false
let performBayesianNetworkTests = false


//
// Supervised learners.
//
if performSupervisedTests then
    do Supervised.doDemoSupervised


//
// Unsupervised learners.
//
if performUnsupervisedTests then
    do Unsupervised.doDemoUnsupervised


//
// Bayesian networks.
//
if performBayesianNetworkTests then
    do Bayesian.doDemoBayesian


//
// Done. 
// 

// Notify user.
Console.WriteLine("Finished demo(s). Press enter to quit.");


