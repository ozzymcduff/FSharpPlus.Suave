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
type WebPartResult<'a> = Async<'a option>
//type WebPartResult = Async<HttpContent option>
module WebPartResult=
  let map (f:'T->'U) (x:WebPartResult<'T>) : WebPartResult<_>=
    async {
      let! x'=x
      match x' with
      | Some t-> return Some (f t)
      | None -> return None
    }
  let bind (f:_->WebPartResult<_>) (a: WebPartResult<_>) = WebPart.bind f a
  let append (x:WebPartResult<'a>) (y:WebPartResult<'a>) =
    failwith "!"
  let apply (f:'T->'U) (x:WebPartResult<'a>) :WebPartResult<'a> =
    failwith "!"


[<Extension;Sealed>]
type WebPartExtensions=

  // as Applicative
  [<Extension>]static member Return x : WebPartResult<'a>= async { return Some x }
  // as Alternative (inherits from Applicative)
  [<Extension>]static member inline get_Empty () = async { return None }
  [<Extension>]static member (<*>) (f:'T->'U, x:WebPartResult<_>) : WebPartResult<_> = WebPartResult.apply f x
  [<Extension>]static member inline Append (x:WebPartResult<'a>, y:WebPartResult<'a>) :WebPartResult<'a> = WebPartResult.append x y
  [<Extension>]static member inline Bind (x:WebPartResult<_>, f:'T->WebPartResult<_>) =WebPartResult.bind f x
