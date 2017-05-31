// --- hassell.fsx --- 
// F# script for approximating limits of the Hassell equation
// Part of an exercise for IAS, see: http://www.cs.uu.nl/docs/vakken/ias/

// Load libraries for plotting
#load "packages/FSharp.Charting/FSharp.Charting.fsx"
open FSharp.Charting

// Define alpha and b values for Hassell equation
let alpha = 1.0
let b = 4.0

// Hassell equation (as function with x_{t-1} and lambda as parameters)
let hassell prev lambda = (lambda * prev) / ((1.0 + alpha * prev) ** b)

// Calculate population at time t (apply Hassell function t times)
let populationAt start time lambda = 
    let rec helper count x = 
        if count = time then x
        else helper (count + 1) (hassell x lambda)
    helper 0 start

// Partially applied functions for approximating limit (999 and 1000 times)
let approxLimit1000 = populationAt 1.0 1000
let approxLimit999 = populationAt 1.0 999

// Draw bifurcation diagrams for 0 <= lambda <= 999 and 0 <= lambda <= 999
let line1 = [0.0 .. 100.0] |> List.map approxLimit1000 |> Chart.Line
let line2 = [0.0 .. 100.0] |> List.map approxLimit999 |> Chart.Line
[line1; line2] |> Chart.Combine