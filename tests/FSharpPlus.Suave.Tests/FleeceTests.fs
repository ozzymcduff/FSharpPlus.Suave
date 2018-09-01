module FSharpPlus.Suave.Tests.FleeceTests

open FSharpPlus.Suave.Tests.Fleece
open Expecto
open SuaveTest
open Suave.Http
open FSharpPlus
open FSharpPlus.Data

open Fleece
open Fleece.FSharpData
open Fleece.FSharpData.Operators
open FSharp.Data
open FSharpPlus.Suave
open FSharpPlus.Suave.Http
open FSharpPlus.Suave.Successful
open FSharpPlus.Suave.Filters
open FSharpPlus.Suave.RequestErrors
open FSharpPlus.Suave.Writers
open FSharpPlus.Operators

type Note={ text:string }
  with static member create n={text=n}
       static member OfJson json =
         match json with
         | JObject o -> Note.create <!> (o .@ "text")
         | x -> Failure (sprintf "Expected note, found %A" x)

       static member ToJson (x: Note) =
         jobj [
            "text" .= x.text
         ]
let sampleNotes=["hej"; "svejs";] |> map Note.create
let webPart ()=
  let overview =
    GET >=> (fun (ctx) ->
            monad {
              return! Json.OK sampleNotes ctx
            })
  let register =
    POST >=> fun (ctx) ->
            monad {
              let body : Note ParseResult = Json.getBody ctx
              return! Json.OK_or_BAD_REQUEST body ctx
            }
  let getNote (id:int) :WebPart=
    GET >=> fun (ctx) ->
            monad {
              printfn "get note"
              return! Json.OK (List.head sampleNotes) ctx
            }

  WebPart.choose [ path "/" >=> (OK "/")
                   path "/note" >=> register
                   pathScan "/note/%d" getNote
                   path "/notes" >=> overview ]
[<Tests>]
let tests =

  let requestOverview =req HttpMethod.GET "/notes"
  let requestIndex =req HttpMethod.GET "/"
  let requestNote id =req HttpMethod.GET (sprintf "/note/%d" id)
  let runningWebp = webPart() >> OptionT.run

  testList "Using Fleece Json" [
    testCase "Be able to return index" <| fun _ ->
      let res = runningWebp requestIndex
                |> extractContext |> contentAsString
      Expect.equal res "/" "Should return /"

    testCase "Be able to return single note" <| fun _ ->
      let res : Note ParseResult
                = runningWebp ( requestNote 1 )
                 |> extractContext |> contentAsString
                 |> JsonValue.Parse
                 |> ofJson
      Expect.equal res (Ok {text="hej"}) "Should return /note/1"

    testCase "Be able to request returning json" <| fun _ ->
      let res1: Note list ParseResult
                = runningWebp requestOverview
                 |> extractContext |> contentAsString
                 |> JsonValue.Parse
                 |> ofJson

      Expect.equal res1 (Ok sampleNotes) "Should return sample notes"
  ]
