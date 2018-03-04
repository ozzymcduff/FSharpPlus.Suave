namespace FSharpPlus.Suave
open FSharpPlus
open Suave
open Suave.WebPart
open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
module internal WebPart=
(* how it's defined in suave:
  let bind (f: 'a -> Async<'b option>) (a: Async<'a option>) = async {
    let! p = a
    match p with
    | None ->
      return None
    | Some q ->
      let r = f q
      return! r
    }
  let compose (first : 'a -> Async<'b option>) (second : 'b -> Async<'c option>)
            : 'a -> Async<'c option> =
    fun x ->
      bind second (first x)
  //'a -> Async<'a option>
  *)
  let map (f:'T->'U) (x:WebPart) = failwith "!"
    //let inline map (f:'T->'U)=function
    //|AccFailure e -> AccFailure e
    //|AccSuccess a -> AccSuccess (f a)
  let apply (f:'T->'U) (x:WebPart) :WebPart =
    //WebPart.bind (fun x1 -> WebPart.bind (fun x2 -> async {return x1 x2}) x) f
    failwith "!"

//'a -> Async<'a option>
[<Extension;Sealed>]
type WebPartExtensions=

  // as Applicative
  [<Extension>]static member Return x = WebPart.succeed x
  // as Alternative (inherits from Applicative)
  [<Extension>]static member inline get_Empty () = WebPart.never()
  [<Extension>]static member (<*>) (f:'T->'U, x:WebPart) : WebPart = WebPart.apply f x
  [<Extension>]static member inline Append (x:WebPart, y:WebPart) :WebPart =WebPart.compose x y
  [<Extension>]static member inline Bind (x:WebPart, f:'T->WebPart) =WebPart.bind f x
