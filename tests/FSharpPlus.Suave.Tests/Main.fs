module ExpectoTemplate
open Expecto
open System.Reflection
open Suave.Http
open Suave.Web
open Suave.Logging
open System
//open Suave.LibUv
open ExpectoExtensions

module AssemblyInfo =

    let metaDataValue  (mda : AssemblyMetadataAttribute) = mda.Value
    let getMetaDataAttribute (assembly : Assembly) key =
        assembly.GetCustomAttributes(typedefof<AssemblyMetadataAttribute>)
                              |> Seq.cast<AssemblyMetadataAttribute>
                              |> Seq.find(fun x -> x.Key = key)

    let getReleaseDate assembly =
        "ReleaseDate"
        |> getMetaDataAttribute assembly
        |> metaDataValue

    let getGitHash assembly =
        "GitHash"
        |> getMetaDataAttribute assembly
        |> metaDataValue

[<EntryPoint>]
let main argv =
  if argv |> Seq.contains ("--version") then
      let assembly =  Assembly.GetEntryAssembly()
      let name = assembly.GetName()
      let version = assembly.GetName().Version
      let releaseDate = AssemblyInfo.getReleaseDate assembly
      let githash  = AssemblyInfo.getGitHash assembly
      printfn "%s - %A - %s - %s" name.Name version releaseDate githash
  let arch s = if s then "64-bit" else "32-bit"

  Console.WriteLine("OSVersion: {0}; running {1} process on {2} operating system."
    , Environment.OSVersion.ToString()
    , arch Environment.Is64BitProcess
    , arch Environment.Is64BitOperatingSystem)

  let testConfig =
    { defaultConfig with
        bindings = [ HttpBinding.createSimple HTTP "127.0.0.1" 9001 ]
        logger   = Targets.create Warn [| "Suave"; "Tests" |] }

  defaultMainThisAssemblyWithParam testConfig argv
