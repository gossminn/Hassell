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

// Approximate the limit of x_t (apply Hassell function 1000 times)
let approximateLimit start lambda = 
    let limit = 1000
    let rec helper count x = 
        if count >= limit then x
        else helper (count + 1) (hassell x lambda)
    helper 0 start

// Draw bifurcation diagram for 0 <= lambda <= 100
{0.0 .. 100.0} |> Seq.map (approximateLimit 1.0) |> Chart.Line

