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
type WebPart<'a> = WebPart of ('a->Async<'a option>)
type SuaveTask<'a> = SuaveTask of (Async<'a option>)
module WebPart=
  let unwrap (WebPart a) = a
  let wrap a =WebPart a
  let bind (f: 'x-> 'a -> Async<'b option>) (a: 'a-> Async<'a option>) :'a -> Async<'b option> = fun x-> async {
    let! p = a x
    match p with
    | None ->
      return None
    | Some q ->
      let r = f q
      return! r x
    }

module SuaveTask=
  module WP = Suave.WebPart

  let unwrap (SuaveTask a) = a
  let bind = WP.bind

module Successful=
  module S = Suave.Successful
  let OK s= WebPart <| S.OK s

module Filters=
  module F = Suave.Filters
  let GET=WebPart <| F.GET
  let POST=WebPart <| F.POST

[<Extension;Sealed>]
type SuaveTaskExtensions=

  [<Extension>]static member Return x= SuaveTask <| async { return Some x }
  [<Extension>]static member inline get_Empty () = SuaveTask <| async { return None }
  [<Extension>]static member inline Bind (SuaveTask x, f) =SuaveTask <| SuaveTask.bind (f>>SuaveTask.unwrap) x

[<Extension;Sealed>]
type WebPartExtensions=

  [<Extension>]static member Return x= WebPart.wrap <| WebPart.succeed
  [<Extension>]static member inline get_Empty () = WebPart.wrap  <| fun _ -> async { return None }

  [<Extension>]static member inline Bind (WebPart x, f) = WebPart <| WebPart.bind (f>>WebPart.unwrap) x
