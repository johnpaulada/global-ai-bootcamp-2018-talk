module Statistics
open System
open LinearAlgebra

let square x = x * x
let mean (x: list<float>) = (x |> List.sum) / float (x |> List.length)

// let sumOfSquares (x: list<float>) =
//   x
//     |> List.map square
//     |> List.reduce (+)

let deMean (x: list<float>) =
  let xBar = x |> mean
  x |> List.map (fun xi -> xi - xBar)

let variance (x: list<float>) = 
  let n = x |> List.length
  let deviations = x |> deMean
  (deviations |> sumOfSquares) / float (n-1)

let standardDeviation (x: list<float>) = 
  x |> variance |> Math.Sqrt

let covariance x y =
  let n = x |> List.length
  dot (deMean x) (deMean y) / float (n-1)

let correlation x y = 
  let stdevX = x |> standardDeviation
  let stdevY = y |> standardDeviation

  if stdevX > 0.0 && stdevY > 0.0 then
    covariance x y / stdevX / stdevY
  else
    0.0