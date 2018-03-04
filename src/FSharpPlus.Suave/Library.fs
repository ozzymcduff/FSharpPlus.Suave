namespace FSharpPlus.Suave
open FSharpPlus
open Suave

module Say =
    let hello name =
        sprintf "Hello %s" name
