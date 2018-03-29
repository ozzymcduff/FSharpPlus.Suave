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
  let mutable counter =0
  let overview =
    GET >=> (fun (ctx) ->
            monad {
              counter<-counter+1
              return! OK (sprintf "overview %i" counter) ctx
            })
  let register =
    POST >=> fun (ctx) ->
            monad {
              counter<-counter+1
              return! OK (sprintf "register %i" counter) ctx
            }

  WebPart.choose [ path "/" >=> (OK "/")
                   path "/notes" >=> overview
                   path "/note" >=> register]

open Suave
open Suave.Testing
[<Tests>]
let tests cfg=
  let requestOverview =req HttpMethod.GET "/notes" None
  let requestIndex =req HttpMethod.GET "/" None

  let runWithConfig = runWith cfg

  testList "simple usage" [
    testCase "Be able to return index" <| fun _ ->
      let runningWebp = webPart() >> SuaveTask.unwrap |> runWithConfig
      let res = runningWebp
                |> requestIndex
      Expect.equal res "/" "Should return /"
    testCase "Be able to return request overview twice" <| fun _ ->
      let runningWebp = webPart() >> SuaveTask.unwrap |> runWithConfig

      let res1 = runningWebp |> requestOverview
      Expect.equal res1 "overview 1" "Should return overview 1"
      let res = runningWebp |> requestOverview

      Expect.equal res "overview 2" "Should return overview 2"
  ]
