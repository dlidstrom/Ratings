open System
open System.IO
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Distributions

type UserRating = {
  UserId : int
  MovieId : int
  Rating : float
}

let splitString (line : string) =
  line.Split("\t", StringSplitOptions.RemoveEmptyEntries)

let parseParts (parts : string[]) =
  {
    UserId = int parts.[0]
    MovieId = int parts.[1]
    Rating = float parts.[2]
  }

// base test
[<EntryPoint>]
let main argv =
  let data =
    File.ReadAllLines(argv.[0])
    // |> Seq.truncate 5
    |> Seq.map (splitString >> parseParts)
    |> Seq.toList

  let nUsers = (Set.count (data |> Seq.map (fun ur -> ur.UserId) |> Set.ofSeq))
  let nItems = (Seq.maxBy (fun ur -> ur.MovieId) data).MovieId
  printfn $"nUsers=%d{nUsers} nItems=%d{nItems}"
  let nFactors = 10
  let alpha = 0.1
  let nEpochs = 10

  let dist = Normal(0., 0.1)
  let p = DenseMatrix.random<float> (nUsers + 1) nFactors dist
  let q = DenseMatrix.random<float> (nItems + 1) nFactors dist

  for _ in 1..nEpochs do
    for d in data do
      let oldP = p.Row(d.UserId)
      let oldQ = q.Row(d.MovieId)
      let err = d.Rating - oldP * oldQ

      p.SetRow(d.UserId, oldP + alpha * err * oldQ)
      q.SetRow(d.MovieId, oldQ + alpha * err * oldP)

  // compare
  let test =
    File.ReadAllLines(argv.[1])
    |> Seq.map (splitString >> parseParts)
    |> Seq.toList

  let mutable errSum = 0.
  for d in (test) do
    let estimate = p.Row(d.UserId) * q.Row(d.MovieId)
    let err = estimate - d.Rating
    errSum <- errSum + err * err
    printfn $"Actual=%f{d.Rating} Estimate=%.2f{estimate} Err=%.2f{err}"
  let rmse = Math.Sqrt (errSum / float test.Length)
  printfn $"RMSE=%.2f{rmse}"
  0
