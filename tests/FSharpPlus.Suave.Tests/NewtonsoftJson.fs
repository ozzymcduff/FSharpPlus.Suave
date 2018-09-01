module NewtonsoftJson
open Newtonsoft.Json
open Newtonsoft.Json.Serialization
//open Suave
open Expecto
open FSharpPlus
open FSharpPlus.Operators
open FSharpPlus.Data
open FSharpPlus.Suave
open FSharpPlus.Suave.Http
open FSharpPlus.Suave.Successful
open FSharpPlus.Suave.RequestErrors
open FSharpPlus.Suave.Writers
open Suave.Http

open System.IO
open System.Text
module Json=
  let private jsonSerializerSettings = JsonSerializerSettings()
  jsonSerializerSettings.ContractResolver <- CamelCasePropertyNamesContractResolver()

  let private stringify v=
    JsonConvert.SerializeObject(v, jsonSerializerSettings)


  /// Convert the object to a JSON representation inside a byte array (can be made string of)
  let private toJson<'T> (o: 'T) =
    JsonConvert.SerializeObject(o, jsonSerializerSettings)

  /// Transform the byte array representing a JSON object to a .Net object
  let private fromJson<'T> (bytes : byte []) =
    use ms = new MemoryStream()
    use reader = new StreamReader(ms)
    ms.Write(bytes, 0, bytes.Length)
    ms.Seek(0L, SeekOrigin.Begin) |> ignore
    JsonConvert.DeserializeObject<'T>( reader.ReadToEnd() )

  let OK v =
    (toJson v |> OK)
    >=> setMimeType "application/json; charset=utf-8"
  let BAD_REQUEST v =
    (toJson v |> BAD_REQUEST)
    >=> setMimeType "application/json; charset=utf-8"

  let ``OK_or_BAD_REQUEST`` (result:Result<_,_>) =
    match result with
    | Ok v -> OK v
    | Error err -> BAD_REQUEST err

  let getBody<'a> (ctx : HttpContext) =
    let str = ctx.request.rawForm |> Encoding.UTF8.GetString
    try
      Ok(JsonConvert.DeserializeObject<'a> str)
    with exn -> Error exn
