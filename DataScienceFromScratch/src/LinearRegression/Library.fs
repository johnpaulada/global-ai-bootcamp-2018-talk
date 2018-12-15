module LinearRegression
open Statistics

let square x:float = x * x

let predict (alpha:float) (beta:float) (xi:float) = beta * xi + alpha

let error (alpha:float) (beta:float) (xi:float) (yi:float) =
    yi - predict alpha beta xi

let sumOfSquaredErrors (alpha:float) (beta:float) (x:list<float>) (y:list<float>) =
    (List.zip x y)
        |> List.sumBy (fun (xi, yi) -> (error alpha beta xi yi) |> square )

let leastSquaresFit x y =
    let beta = correlation x y * standardDeviation y / standardDeviation x
    let alpha = mean y - beta * mean x
    alpha, beta
