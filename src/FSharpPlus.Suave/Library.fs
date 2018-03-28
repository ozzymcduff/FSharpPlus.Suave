module FSharpPlus.Suave
open FSharpPlus
open Suave

/// The base monad in Suave is SuaveTask<HttpContext>
type SuaveTask<'a> = SuaveTask of (Async<'a option>)
module SuaveTask=

  let unwrap (SuaveTask a) = a
  // ('a -> SuaveTask<'b>) -> SuaveTask<'a> -> SuaveTask<'b>
  let bind = Suave.WebPart.bind // this is a bit confusing
  let map (f: 'a -> 'b) (a: Async<'a option>) : Async<'b option>= async {
    let! p = a
    match p with
    | None ->
      return None
    | Some q ->
      let r = f q
      return Some r
    }

type SuaveTask<'a> with

  static member Return x= SuaveTask <| async { return Some x }
  static member inline get_Empty () = SuaveTask <| async { return None }
  static member inline (>>=) (SuaveTask x, f:'a->SuaveTask<'b>) =SuaveTask <| SuaveTask.bind (f>>SuaveTask.unwrap) x

  static member inline Map (SuaveTask x, f:'a->'b) =SuaveTask <| SuaveTask.map f x

type WebPart'<'a> = 'a -> SuaveTask<'a>

type Option<'T> with
    static member inline hoist (x:Option<'T>) = SuaveTask (result x)

module WebPart=
  //let unwrap (WebPart a) = a
  //let wrap a =WebPart a
  let wrapSuave (a:'a->Async<'a option>) :'a->SuaveTask<'a> = a >> SuaveTask

  let choose (options : WebPart'<'a> list) =
    options
      |> List.map (fun f -> f >> SuaveTask.unwrap)
      |> WebPart.choose
      |> wrapSuave


module Successful=
  module S = Suave.Successful
  let OK s= WebPart.wrapSuave (S.OK s )

module Filters=
  module F = Suave.Filters
  let GET=WebPart.wrapSuave F.GET
  let POST=WebPart.wrapSuave F.POST
  let DELETE=WebPart.wrapSuave F.DELETE
  let PUT=WebPart.wrapSuave F.PUT
  let HEAD=WebPart.wrapSuave F.HEAD
  let PATCH=WebPart.wrapSuave F.PATCH
  let OPTIONS=WebPart.wrapSuave F.OPTIONS

  let path s=WebPart.wrapSuave (F.path s)
  let pathStarts s=WebPart.wrapSuave (F.pathStarts s)
  let isSecure =WebPart.wrapSuave F.isSecure
  let pathRegex s=WebPart.wrapSuave (F.pathRegex s)
  let pathScan format ctx=WebPart.wrapSuave (F.pathScan format ctx)
