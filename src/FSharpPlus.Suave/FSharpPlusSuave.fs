module FSharpPlus.Suave
open FSharpPlus
open Suave
open FSharpPlus.Data

/// The base monad in Suave is SuaveTask<HttpContext>
type SuaveTask<'a> = OptionT<Async<'a option>>
module SuaveTask=

  // ('a -> SuaveTask<'b>) -> SuaveTask<'a> -> SuaveTask<'b>
  let bind = Suave.WebPart.bind // this is a bit confusing


type WebPart'<'a> = 'a -> OptionT<Async<'a option>>

module WebPart=

  let choose (options : WebPart'<'a> list) =fun x -> choice (List.map ( (|>) x) options)

module Successful=
  module S = Suave.Successful
  let OK s= OptionT<< (S.OK s )

module Filters=
  module F = Suave.Filters
  let GET=  OptionT << F.GET
  let POST= OptionT << F.POST
  let DELETE= OptionT << F.DELETE
  let PUT= OptionT << F.PUT
  let HEAD= OptionT << F.HEAD
  let PATCH= OptionT << F.PATCH
  let OPTIONS= OptionT << F.OPTIONS

  let path s= OptionT << (F.path s)
  let pathStarts s=OptionT << (F.pathStarts s)
  let isSecure =OptionT << F.isSecure
  let pathRegex s=OptionT << (F.pathRegex s)
  let pathScan format ctx=OptionT << (F.pathScan format ctx)
open FSharpPlus.Lens
module Writers=
  module W=Suave.Writers

  //
  let inline _response f ctx = map (fun a' -> { ctx with response=a' }) (f ctx.response)
  module Result =
    let inline _status f (resp:HttpResult) = map (fun a' -> { resp with status=a' }) (f resp.status)
    //let inline _headers f (resp:HttpResult) = map (fun a' -> { resp with headers=a' }) (f resp.headers)

  module Status =
    let inline _code f status = map (fun a' -> { status with code = a' }) (f status.code)
    let inline _reason f status = map (fun a' -> { status with reason = a' }) (f status.reason)

//  let setStatus (s : HttpCode) = OptionT << W.setStatus s
  let setHeader t = OptionT << W.setHeader t
  let setHeaderValue t = OptionT << W.setHeaderValue t
  let setMimeType t = OptionT << W.setMimeType t

