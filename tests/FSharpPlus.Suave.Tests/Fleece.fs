module FSharpPlus.Suave.Tests.Fleece
open FSharpPlus
open FSharpPlus.Operators
open FSharpPlus.Data
open FSharpPlus.Suave
open FSharpPlus.Suave.Http
open FSharpPlus.Suave.Successful
open FSharpPlus.Suave.RequestErrors
open FSharpPlus.Suave.Writers
open Fleece
open Fleece.FSharpData
open FSharp.Data

let inline JSON v : WebPart=
  OK ((toJson v).ToString())
  >=> setMimeType "application/json; charset=utf-8"

let inline ``JSONorBAD_REQUEST`` (result) : WebPart=
  match result with
  | Ok v -> JSON v
  | Error err ->
    ((toJson err).ToString())
    |> BAD_REQUEST
    >=> setMimeType "application/json; charset=utf-8"

let inline getBodyAsJSON (ctx : Suave.Http.HttpContext)=
  let getStringFromBytes rawForm = System.Text.Encoding.UTF8.GetString(rawForm)
  let str = ctx.request.rawForm |> getStringFromBytes
  ofJson (JsonValue.Parse str)
