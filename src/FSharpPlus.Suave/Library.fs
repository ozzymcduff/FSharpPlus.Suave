namespace FSharpPlus.Suave
open FSharpPlus
open Suave
open Suave.WebPart
open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Net.Http

// two types:
// WebPart: 'a -> Async<'a option>
// type WebPart = HttpContext -> Async<HttpContext option>
// WebPartResult: Async<'a option>
//type WebPart<'a> = WebPart of ('a->Async<'a option>)
type SuaveTask<'a> = SuaveTask of (Async<'a option>)
module WebPart=
  // looks a little like a bind
  // ('a -> SuaveTask<'a>) -> 'a option -> SuaveTask<'a>
  // or a lift of 'a option to a SuaveTask<'a>
  let inline wrap (a:'a->Async<'a option>) =  fun (ctx) ->
      monad {
        match ctx with
        | Some c-> return! a c
        | None -> return None
      }
module SuaveTask=
  module WP = Suave.WebPart

  let unwrap (SuaveTask a) = a
  // ('a -> SuaveTask<'b>) -> SuaveTask<'a> -> SuaveTask<'b>
  let bind = WP.bind
  let map (f: 'a -> 'b) (a: Async<'a option>) : Async<'b option>= async {
    let! p = a
    match p with
    | None ->
      return None
    | Some q ->
      let r = f q
      return Some r
    }

(*
module Successful=
  module S = Suave.Successful
  let OK s= WebPart.wrap <| S.OK s

module Filters=
  module F = Suave.Filters
  let GET=WebPart.wrap F.GET
  let POST=WebPart.wrap F.POST
*)
type SuaveTask<'a> with

  [<Extension>]static member Return x= SuaveTask <| async { return Some x }
  [<Extension>]static member inline get_Empty () = SuaveTask <| async { return None }
  [<Extension>]static member inline Bind (SuaveTask x, f:'a->SuaveTask<'b>) =SuaveTask <| SuaveTask.bind (f>>SuaveTask.unwrap) x

  [<Extension>]static member inline Map (SuaveTask x, f:'a->'b) =SuaveTask <| SuaveTask.map f x

(*
type WebPart<'a> with

  [<Extension>]static member Return x= WebPart.wrap <| WebPart.succeed
  [<Extension>]static member inline get_Empty () = WebPart.wrap  <| fun _ -> async { return None }

  [<Extension>]static member inline Bind (WebPart x, f) = WebPart <| WebPart.bind (f>>WebPart.unwrap) x
  [<Extension>]static member inline Map (WebPart x, f) = WebPart <| WebPart.map f x
*)
