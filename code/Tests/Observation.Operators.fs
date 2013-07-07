
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

module TestObservation

open FAI.Bayesian

let Run =
    let obs1 = new Observation()
    let obs1x = obs1 + ("X", 2.0)

    let obs2 = new Observation()
    let obs2x = obs2 + ("X", 2.00)

    let obs2nx = obs2x - "X"

    let obs3xy = new Observation() + ("X", 3.00) + ("Y", 0.00)
    let obs3nxny = obs3xy - "X" - "Y"

    let obs1xx = obs1x + ("X", 33.0)

    assert (obs1 = obs2)
    assert (obs1x = obs2x)
    assert (obs2nx = obs2)
    assert (obs3xy <> obs1x)
    assert (obs3xy <> obs2x)
    assert (obs3xy <> obs3nxny)
    assert (obs3nxny = obs1)
    assert (obs3nxny = obs2)

    assert (obs1xx <> obs1x)
    assert (obs1xx <> obs2x)

    ()
