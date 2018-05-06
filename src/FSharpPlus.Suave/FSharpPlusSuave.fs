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
module Http=
  module H=Suave.Http
  let request f x=OptionT<< H.request f x
module Successful=
  module S = Suave.Successful
  let OK s= OptionT<< (S.OK s )
module RequestErrors=
  module RE = Suave.RequestErrors
  let BAD_REQUEST s= OptionT<< (RE.BAD_REQUEST s )
  let NOT_ACCEPTABLE s= OptionT<< (RE.NOT_ACCEPTABLE s )
  let METHOD_NOT_ALLOWED s= OptionT<< (RE.METHOD_NOT_ALLOWED s )
  let FORBIDDEN s= OptionT<< (RE.FORBIDDEN s )
  let NOT_FOUND s= OptionT<< (RE.NOT_FOUND s )

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
  let inline _request f ctx = map (fun a' -> { ctx with request=a' }) (f ctx.request)
  module Result =
    let inline _status f (resp:HttpResult) = map (fun a' -> { resp with status=a' }) (f resp.status)
    //let inline _headers f (resp:HttpResult) = map (fun a' -> { resp with headers=a' }) (f resp.headers)
  module Request =
    let inline _url f (req:HttpRequest) = map (fun a' -> { req with url=a' }) (f req.url)
    let inline _method f (req:HttpRequest) = map (fun a' -> { req with method=a' }) (f req.method)
    let inline _rawForm f (req:HttpRequest) = map (fun a' -> { req with rawForm=a' }) (f req.rawForm)

  module Status =
    let inline _code f status = map (fun a' -> { status with code = a' }) (f status.code)
    let inline _reason f status = map (fun a' -> { status with reason = a' }) (f status.reason)

//  let setStatus (s : HttpCode) = OptionT << W.setStatus s
  let setHeader k v = OptionT << W.setHeader k v
  let setHeaderValue k v = OptionT << W.setHeaderValue k v
  let setMimeType t = OptionT << W.setMimeType t
