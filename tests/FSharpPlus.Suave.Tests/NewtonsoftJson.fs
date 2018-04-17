module NewtonsoftJson
open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open Suave
open Expecto
open FSharpPlus
open FSharpPlus.Suave
open FSharpPlus.Suave.Successful
open FSharpPlus.Suave.RequestErrors
open FSharpPlus.Suave.Writers
open FSharpPlus.Suave.Filters

open System.Text
module Json=
  let jsonSerializerSettings = JsonSerializerSettings()
  jsonSerializerSettings.ContractResolver <- CamelCasePropertyNamesContractResolver()

  let stringify v=
    JsonConvert.SerializeObject(v, jsonSerializerSettings)

let JSON v =
  (Json.stringify v |> OK)
  >=> setMimeType "application/json; charset=utf-8"

let ``JSONorBAD_REQUEST`` (result:Result<_,_>) =
  match result with
  | Ok v -> JSON v
  | Error err ->
    Json.stringify err
    |> BAD_REQUEST
    >=> setMimeType "application/json; charset=utf-8"

let private getStringFromBytes rawForm = Encoding.UTF8.GetString(rawForm)

let getBodyAsJSON<'a> (ctx : HttpContext) =
  let str = ctx.request.rawForm |> getStringFromBytes
  try
    Ok(JsonConvert.DeserializeObject<'a> str)
  with exn -> Error exn

open SuaveTest
open FSharpPlus.Data
type Note={ text:string }
  with static member create n={text=n}
let sampleNotes=["hej"; "svejs";] |> map Note.create
let webPart ()=
  let overview =
    GET >=> (fun (ctx) ->
            monad {
              return! JSON sampleNotes ctx
            })
  let register =
    POST >=> fun (ctx) ->
            monad {
              let body=getBodyAsJSON<Note>(ctx)
              return! JSONorBAD_REQUEST body ctx
            }

  WebPart.choose [ path "/" >=> (OK "/")
                   path "/note" >=> register
                   path "/notes" >=> overview ]
[<Tests>]
let tests =

  let requestOverview =req HttpMethod.GET "/notes"
  let requestIndex =req HttpMethod.GET "/"
  let runningWebp = webPart() >> OptionT.run

  testList "Using Newtonsoft Json" [
    testCase "Be able to return index" <| fun _ ->
      let res = runningWebp requestIndex
                |> extractContext |> contentAsString
      Expect.equal res "/" "Should return /"

    testCase "Be able to request returning json" <| fun _ ->
      let res1 = runningWebp requestOverview
                 |> extractContext |> contentAsString
                 |> JsonConvert.DeserializeObject<Note list>
      Expect.equal res1 sampleNotes "Should return sample notes"
  ]
