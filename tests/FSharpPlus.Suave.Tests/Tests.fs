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
  let overview =
    GET >=> WebPart.wrap (fun (ctx) ->
            monad {
              return! OK ("") ctx
            })
  let register =
    POST >=> fun (ctx) ->
            monad {
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
