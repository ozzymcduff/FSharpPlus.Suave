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

module Json=
  let inline OK v : WebPart=
    OK ((toJson v).ToString())
    >=> setMimeType "application/json; charset=utf-8"
  let inline BAD_REQUEST v : WebPart=
    BAD_REQUEST ((toJson v).ToString())
    >=> setMimeType "application/json; charset=utf-8"

  let inline ``OK_or_BAD_REQUEST`` (result) : WebPart=
    match result with
    | Ok v -> OK v
    | Error err -> BAD_REQUEST err

  let inline getBody (ctx : Suave.Http.HttpContext)=
    let str = ctx.request.rawForm |> System.Text.Encoding.UTF8.GetString
    ofJson (JsonValue.Parse str)
