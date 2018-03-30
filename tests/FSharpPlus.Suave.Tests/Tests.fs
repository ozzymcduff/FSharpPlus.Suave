module Tests


open Expecto
//open Suave
//open Suave.Filters
//open Suave.Operators
//open Suave.RequestErrors
//open Suave.Successful
//open Suave.Writers
open FSharpPlus.Suave
open FSharpPlus.Suave.Successful
open FSharpPlus.Suave.Filters
open FSharpPlus

let webPart ()=
  printfn "init webpart"
  let mutable counter =0
  let overview =
    GET >=> (fun (ctx) ->
            monad {
              counter<-counter+1
              printfn "opening overview %i" counter
              return! OK (sprintf "overview %i" counter) ctx
            })
  let register =
    POST >=> fun (ctx) ->
            monad {
              printfn "opening register"
              return! OK "register" ctx
            }

  WebPart.choose [ path "/" >=> (OK "/")
                   path "/note" >=> register
                   path "/notes" >=> overview ]

open Suave
open Suave.Testing
open FSharpPlus.Data
[<Tests>]
let tests cfg=
  let runWithConfig = runWith cfg
  let requestOverview =req HttpMethod.GET "/notes" None
  let requestIndex =req HttpMethod.GET "/" None
  let runningWebp = webPart() >> OptionT.run |> runWithConfig

  testList "simple usage" [
    testCase "Be able to return index" <| fun _ ->
      let res = runningWebp
                |> requestIndex
      Expect.equal res "/" "Should return /"
    testCase "Be able to return request overview twice" <| fun _ ->
      printfn "first request"
      let res1 = runningWebp |> requestOverview
      Expect.equal res1 "overview 1" "Should return overview 1"
  ]
