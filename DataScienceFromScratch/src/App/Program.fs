open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open FSharp.Data
open FSharp.Data.JsonExtensions
open Suave.CORS

let getData form =
  let (body, _) = form |> List.head
  let parsed = JsonValue.Parse(body)
  parsed?data

let predictLR array1 array2 x =
  let list1 = array1 |> Array.toList
  let list2 = array2 |> Array.toList
  let (alpha, beta) = LinearRegression.leastSquaresFit list1 list2
  alpha, beta, LinearRegression.predict alpha beta x

let handleLR data =
  let innerData = data?data.AsArray()
  let x = data?x.AsFloat()
  
  match innerData with
    | [|list1; list2|] ->
      match list1 with
        | JsonValue.Array l1 ->
          match list2 with
            | JsonValue.Array l2 -> predictLR (l1 |> Array.map (fun xi -> match xi with | i -> i.AsFloat())) (l2 |> Array.map (fun xi -> match xi with | i -> i.AsFloat())) x
            | _ -> (0.0, 0.0, 0.0)
        | _ -> (0.0, 0.0, 0.0)
    | _ -> (0.0, 0.0, 0.0)

let corsConfig = { defaultCORSConfig with allowedUris = InclusiveOption.Some [ "http://localhost:3000" ] }

let makeHandler p handler = path p >=> cors corsConfig >=> request (fun r -> OK (string (r.form |> getData |> handler) ))

[<EntryPoint>]
let main _argv =
  let app = choose [
    GET >=> choose [
       path "/lr" >=> OK "Linear Regression Endpoint"
    ]
    POST >=> choose [
       makeHandler "/lr" handleLR
    ]
  ]

  startWebServer defaultConfig app

  0