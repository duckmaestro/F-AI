
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


module internal LearnDistributions

open System.Collections.Generic
open FAI.Bayesian

///
/// Uses the given Bayesian network structure to learn
/// conditional probability distributions. Returns a dictionary
/// of distributions indexed by variable.
/// 
let LearnDistributions bayesianNetwork observationSet =
    
    let distributions = new Dictionary<RandomVariable, DiscreteDistribution>()

    failwith "Not implemented yet."
