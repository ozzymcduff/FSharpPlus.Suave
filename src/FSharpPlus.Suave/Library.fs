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

  let choose (options : WebPart'<'a> list) =
    options
      |> List.map (fun f -> f >> SuaveTask.unwrap)
      |> WebPart.choose
      >> SuaveTask


module Successful=
  module S = Suave.Successful
  let OK s= SuaveTask<< (S.OK s )

module Filters=
  module F = Suave.Filters
  let GET=  SuaveTask << F.GET
  let POST= SuaveTask << F.POST
  let DELETE= SuaveTask << F.DELETE
  let PUT= SuaveTask << F.PUT
  let HEAD= SuaveTask << F.HEAD
  let PATCH= SuaveTask << F.PATCH
  let OPTIONS= SuaveTask << F.OPTIONS

  let path s= SuaveTask << (F.path s)
  let pathStarts s=SuaveTask << (F.pathStarts s)
  let isSecure =SuaveTask << F.isSecure
  let pathRegex s=SuaveTask << (F.pathRegex s)
  let pathScan format ctx=SuaveTask << (F.pathScan format ctx)
