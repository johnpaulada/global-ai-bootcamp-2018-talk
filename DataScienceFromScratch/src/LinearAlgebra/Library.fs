module LinearAlgebra
open System

let add (x:float) (y:float) = x + y

let multiply (x: float) (y:float) = x * y

let addTuple (x:float, y:float) = x + y

let subtractTuple (x:float, y:float) = x - y

let multiplyTuple (x:float, y:float) = x * y

let vectorAdd v w =
  (List.zip v w)
   |> List.map addTuple

let vectorSubtract v w =
  (List.zip v w)
   |> List.map subtractTuple

let vectorSum vectors =
  vectors
    |> List.reduce vectorAdd 

let scalarMultiply (c:float) v = v |> List.map (fun vi -> vi * c)

let vectorMean vectors = 
  let n = vectors |> List.length
  scalarMultiply (1.0 / (n |> float)) (vectorSum vectors)

let dot (v: list<float>) (w: list<float>) =
  (List.zip v w)
    |> List.sumBy multiplyTuple

let sumOfSquares v = dot v v

let magnitude v = v |> sumOfSquares |> Math.Sqrt

let squaredDistance v w =
  (vectorSubtract v w)
    |> sumOfSquares

let distance v w = 
  (vectorSubtract v w)
    |> magnitude