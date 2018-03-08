module Tests


open Expecto
open Suave
open Suave.Filters
//open Suave.Operators
//open Suave.RequestErrors
open Suave.Successful
//open Suave.Writers
open FSharpPlus.Suave
open FSharpPlus
let webPart ()=
  let overview =


    GET >=> (fun (ctx) ->
            monad {
              match ctx with
              | Some c-> return! OK ("") c
              | None -> return None
            })
  let register =
    POST >=> WebPart.wrap( fun (ctx) ->
            monad {
              return! OK ("") ctx
            })

  WebPart.choose [ path "/" >=> WebPart.wrap(OK "")
                   path "/notes" >=> WebPart.wrap overview
                   path "/note" >=> WebPart.wrap register]

[<Tests>]
let tests =
  testList "samples" [
    testCase "Say hello all" <| fun _ ->
      let subject = "all"
      Expect.equal subject "Hello all" "You didn't say hello"
  ]
