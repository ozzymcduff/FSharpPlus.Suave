module Tests


open Expecto
//open Suave
//open Suave.Filters
//open Suave.Operators
//open Suave.RequestErrors
//open Suave.Successful
//open Suave.Writers
open Suave
open FSharpPlus.Suave
open FSharpPlus.Suave.Successful
open FSharpPlus.Suave.Filters
open FSharpPlus
open FSharpPlus.Data
let createCounter () =
  MailboxProcessor<AsyncReplyChannel<int>>.Start(fun inbox ->
        let rec loop n =
            async { let! msg = inbox.Receive()
                    let next= n+1
                    msg.Reply(next)
                    return! loop(next) }
        loop 0)
let webPart ()=
  let counter = createCounter()
  printfn "init webpart"
  let overview =
    GET >=> (fun (ctx) ->
            monad {
              let count=counter.PostAndReply id
              printfn "opening overview %i" count
              return! OK (sprintf "overview %i" count) ctx
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

open SuaveTest
[<Tests>]
let tests =

  let requestOverview =req HttpMethod.GET "/notes"
  let requestIndex =req HttpMethod.GET "/"
  let runningWebp = webPart() >> OptionT.run

  testList "simple usage" [
    testCase "Be able to return index" <| fun _ ->
      let res = runningWebp requestIndex
                |> extractContext |> contentAsString
      Expect.equal res "/" "Should return /"

    testCase "Be able to return request overview twice" <| fun _ ->
      printfn "first request"
      let res1 = runningWebp requestOverview
                 |> extractContext |> contentAsString
      Expect.equal res1 "overview 1" "Should return overview 1"
      let res2 = runningWebp requestOverview
                 |> extractContext |> contentAsString
      Expect.equal res2 "overview 2" "Should return overview 2"
  ]
