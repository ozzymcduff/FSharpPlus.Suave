module NewtonsoftJsonTests
open Expecto
open Suave.Http

open FSharpPlus
open FSharpPlus.Suave
open FSharpPlus.Suave.Successful
open FSharpPlus.Suave.RequestErrors
open FSharpPlus.Suave.Writers
open FSharpPlus.Suave.Filters
open FSharpPlus.Data

open NewtonsoftJson
open Newtonsoft.Json

open SuaveTest

type Note={ text:string }
  with static member create n={text=n}
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
              let body=Json.getBody<Note>(ctx)
              return! Json.OK_or_BAD_REQUEST body ctx
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
