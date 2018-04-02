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

module SuaveTest=
  // Code inspired by blog post
  // http://mattjolson.github.io/2016/12/12/testing-suave-webparts.html

  /// extract content syncronously
  let extractContext maybeHc =
    maybeHc |> Async.RunSynchronously |> Option.get

  let contentAsString (hc:Suave.Http.HttpContext) =
      match hc.response.content with
      | Bytes b -> b |> System.Text.Encoding.Default.GetString
      | NullContent -> ""
      | SocketTask st -> "SOCKET"
  /// request without data
  let req method u =
    let uri = new System.Uri("http://some.random.tld" + u)
    let rawQuery = uri.Query.TrimStart('?')
    let req = { HttpRequest.empty with url = uri ;``method`` = method ; rawQuery = rawQuery }
    { HttpContext.empty with request = req }


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
  ]
