module Tests


open Expecto
open Suave
open Suave.Filters
//open Suave.Operators
open Suave.RequestErrors
open Suave.Successful
open Suave.Writers
open FSharpPlus.Suave
open FSharpPlus
let webPart ()=
  let overview =
    GET >=> fun (ctx) ->
            async {
              return! OK ("") ctx
            }
  let register =
    POST >=> fun (ctx) ->
            async {
              return! OK ("") ctx
            }

  WebPart.choose [ path "/" >=> (OK "")
                   path "/notes" >=> overview
                   path "/note" >=> register]

[<Tests>]
let tests =
  testList "samples" [
    testCase "Say hello all" <| fun _ ->
      let subject = "all"
      Expect.equal subject "Hello all" "You didn't say hello"
  ]
