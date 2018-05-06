module SuaveTest
open System
open Suave
open FSharpPlus
open FSharpPlus.Suave.Writers
// Code inspired by blog post
// http://mattjolson.github.io/2016/12/12/testing-suave-webparts.html
//

/// extract content syncronously
let extractContext maybeHc =
  maybeHc |> Async.RunSynchronously |> Option.get

let contentAsString (hc:HttpContext) =
    match hc.response.content with
    | Bytes b -> b |> System.Text.Encoding.Default.GetString
    | NullContent -> ""
    | SocketTask st -> "SOCKET"
/// request without data
let req method u =
  let uri = Uri("http://some.random.tld"+ u)
  let rawQuery = uri.Query.TrimStart('?')
  let req = { HttpRequest.empty with url = uri ;``method`` = method ; rawQuery = rawQuery }
  { HttpContext.empty with request = req }
let withBody context body=
    { context with request = { context.request with rawForm=form } }
