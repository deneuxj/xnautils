﻿#r "System.Xml.Linq"
#r "System.Windows.Forms"

#load "../Core/Misc.fs"

open System.Xml.Linq
open System.Windows.Forms

open CleverRake.XnaUtils.Maybe

[<Literal>]
let NS = "http://schemas.microsoft.com/developer/msbuild/2003"

type Configuration = Config of string | AnyConfig
type Platform = Platform of string | AnyPlatform

type PropertyMap = PropertyMap of Map<Configuration * string * string, string>

type PropertyFixer = FixerMap of Map<Configuration * string * string, string -> string>

let fixProperties (FixerMap fixer) (PropertyMap props) =
    props
    |> Map.map (fun k v ->
        match Map.tryFind k fixer with
        | Some f -> f v
        | None -> v)
    |> PropertyMap

let concatProperties maps =
    maps
    |> Seq.map (function PropertyMap m -> Map.toSeq m)
    |> Seq.concat
    |> Map.ofSeq
    |> PropertyMap

module Xml =
    let xdoc (el : #seq<XElement>) = new XDocument(Array.map box (Array.ofSeq el))
    let xname n = XName.Get(n, NS)
    let xelem s el = new XElement(s, box el)
    let xatt a b = new XAttribute(a, b) |> box
    let xstr s = box s

    let (|Named|_|) (elem : XElement) =
        if elem.Name.Namespace.NamespaceName = NS then
            Some elem.Name.LocalName
        else
            None

    let (|Valued|_|) (att : XAttribute) =
        if att.Value <> null then
            Some att.Value
        else
            None

    let (|AttrNamed|_|) (attr : XAttribute) =
        if attr.Name.Namespace.NamespaceName = NS then
            Some attr.Name.LocalName
        else
            None

module Parsing =
    let (|Prefixed|_|) (prefix) (s : string) =
        if s.StartsWith(prefix) then
            Some (s.Remove(0, prefix.Length))
        else
            None

    let (|WPrefixed|_|) (prefix) (s : string) =
        let s = s.TrimStart()
        if s.StartsWith(prefix) then
            Some (s.Remove(0, prefix.Length))
        else
            None

    let expect prefix data =
        match data with
        | Some (Prefixed prefix rest, sem) -> Some (rest, sem)
        | Some (s, _) -> eprintfn "%s" s ; None
        | _ -> None

    let trimExpect prefix data =
        match data with
        | Some (WPrefixed prefix rest, sem) -> Some (rest, sem)
        | Some (s, _) -> eprintfn "%s" s ; None
        | _ -> None

    let takeUntil (stop : string) data =
        match data with
        | Some (s : string, sem) ->
            let idx = s.IndexOf(stop)
            if idx >= 0 then Some (s.Remove(0, idx), (s.Substring(0, idx) |> box) :: sem)
            else eprintfn "%s" s ; None
        | _ -> None

    let close data =
        match data with
        | Some ("", data) -> Some data
        | _ -> None

    let parseCondition (s : string) =
        let dollars = trimExpect "'$(Configuration)|$(Platform)'"
        let equals = trimExpect "=="
        let openQuote = trimExpect "'"
        let valueConfiguration = takeUntil "|"
        let pipe = expect "|"
        let valuePlatform = takeUntil "'"
        let trail = expect "'" >> trimExpect ""

        match
            Some (s, [])
            |> dollars
            |> equals
            |> openQuote
            |> valueConfiguration
            |> pipe
            |> valuePlatform
            |> trail
            |> close
            with
            | Some [config ; platform] -> Some (unbox<string> config, unbox<string> platform)
            | _ -> None

open Xml
open Parsing

let dup (elem : XElement) (newSubs : seq<XElement>) =
    let subs = Seq.concat [ newSubs |> Seq.map box ; elem.Attributes() |> Seq.map box ]
    new XElement(elem.Name, subs)

let dumpProperties (PropertyMap props) =
    props
    |> Map.toSeq
    |> Seq.map (fun ((config, ns, ln), value) -> (config, (ns, ln, value)))
    |> Seq.groupBy fst
    |> Seq.map (fun (config, props) ->
        let condition =
            match config with
            | AnyConfig ->
                sprintf "'$(Platform)' == 'Xbox360'"
            | Config c ->
                sprintf "'$(Configuration)|$(Platform)' == '%s|Xbox360'" c
        props
        |> Seq.map (fun (_, (ns, ln, value)) ->
            xelem (XName.Get(ln, ns)) (xstr value)
            |> box)
        |> fun subs ->
            xelem (xname "PropertyGroup") (Seq.append [ xatt (xname "Condition") (xstr condition) ] subs)
    )

let parsePropertyGroup (elem : XElement) =
    match elem with
    | Named "PropertyGroup" ->
        maybe {
            let! config, platform =
                match elem.Attribute(xname "Condition") with
                | null -> Some (AnyConfig, AnyPlatform)
                | attr ->
                    match parseCondition attr.Value with
                    | Some (config, platform) ->
                        Some (Config config, Platform platform)
                    | None ->
                        None
            
            return
                match platform with
                | AnyPlatform | Platform "AnyCPU" | Platform "x86" ->
                    let props =
                        elem.Elements()
                        |> Seq.map (fun sub -> (config, sub.Name.Namespace.NamespaceName, sub.Name.LocalName), sub.Value)
                    List.ofSeq props
                | _ ->
                    []
        }

    | _ -> failwithf "Expected PropertyGroup, got %s" elem.Name.NamespaceName

let extractProperties (elem : XElement) =
    elem
    |> parsePropertyGroup
    |> function Some x -> x | None -> failwith "Failed to parse property group"
    |> Map.ofList
    |> PropertyMap

let setXboxProperties =
    let debug = Config "Debug"
    let release = Config "Release"
    let common = AnyConfig

    let overWriteWith x _ = x
    let addIfMissing x (s : string) =
        let items = s.Split [| ';' |]
        match items |> Seq.tryFind ((=) x) with
        | Some _ -> s
        | None -> s + ";" + x

    Map.empty
    |> Map.add (common, NS, "Tailcalls") (overWriteWith "false")
    |> Map.add (common, NS, "NoStdLib") (overWriteWith "true")
    |> Map.add (common, NS, "XnaFrameworkVersion") (overWriteWith "4.0")
    |> Map.add (common, NS, "XnaPlatform") (overWriteWith "Xbox 360")
    |> Map.add (common, NS, "XnaOututType") (overWriteWith "Library")
    |> Map.add (debug, NS, "OutputPath") (overWriteWith "bin\\Xbox 360\\Debug")
    |> Map.add (debug, NS, "DebugSymbols") (overWriteWith "true")
    |> Map.add (debug, NS, "DebugType") (overWriteWith "full")
    |> Map.add (debug, NS, "Optimize") (overWriteWith "false")
    |> Map.add (debug, NS, "DefineConstants") (addIfMissing "DEBUG" >> addIfMissing "TRACE" >> addIfMissing "XBOX" >> addIfMissing "XBOX360")
    |> Map.add (release, NS, "OutputPath") (overWriteWith "bin\\Xbox 360\\Release")
    |> Map.add (release, NS, "DebugType") (overWriteWith "pdbonly")
    |> Map.add (release, NS, "Optimize") (overWriteWith "true")
    |> Map.add (release, NS, "DefineConstants") (addIfMissing "TRACE" >> addIfMissing "XBOX" >> addIfMissing "XBOX360")
    |> FixerMap

let setProjectGuid (PropertyMap props) =
    let guid = System.Guid.NewGuid()

    props
    |> Map.add (AnyConfig, NS, "ProjectGuid") ("{" + (guid.ToString()) + "}")
    |> PropertyMap

let handleItem item =
    let provided =
        [ "Microsoft.Xna.Framework.Avatar" ;
          "Microsoft.Xna.Framework" ;
          "Microsoft.Xna.Framework.Game" ;
          "Microsoft.Xna.Framework.GamerServices" ;
          "Microsoft.Xna.Framework.Graphics" ;
          "Microsoft.Xna.Framework.Input.Touch" ;
          "Microsoft.Xna.Framework.Net" ;
          "Microsoft.Xna.Framework.Storage" ;
          "Microsoft.Xna.Framework.Video" ;
          "Microsoft.Xna.Framework.Xact" ;
          "mscorlib" ;
          "System.Core" ;
          "System" ;
          "System.Net" ;
          "System.Xml" ;
          "System.Xml.Linq" ;
          "System.Xml.Serialization" ]
        |> Set.ofList

    let path x = @"$(MSBuildExtensionsPath32)\..\Microsoft XNA\XNA Game Studio\v4.0\References\Xbox360\" + x |> Some

    eprintfn "**** %A" item

    match item with
    | Named "Reference" ->
        let hintOverride =
            match item.Attribute(xname "Include") with
            | null -> None
            | Valued dll when provided.Contains(dll) -> path dll
            | Valued "FSharp.Core" ->
                Some @"Dependencies\FShap.Core.dll"
            | _ -> None

        let requiredTargetFrameworkOverride =
            match item.Attribute(xname "Include") with
            | null -> None
            | Valued "System.Core" ->
                Some "4.0"
            | _ -> None

        let subs =
            item.Elements()
            |> Seq.map (fun child ->
                match child with
                | Named "HintPath" ->
                    match hintOverride with
                    | None -> child
                    | Some x -> xelem (xname "HintPath") [ xstr x ]
                | _ -> child)
            |> Seq.append (
                    match requiredTargetFrameworkOverride with
                    | Some x ->
                        [ xelem (xname "RequiredTargetFramework") [ xstr x ] ]
                    | None ->
                        [])

        dup item subs
    | _ -> item

let handleItemGroup (group : XElement) =
    let subs =
        group.Elements()
        |> Seq.map handleItem

    dup group subs

let handleProject (project : XElement) =
    match project with
    | Named "Project" ->
        let properties =
            project.Elements()
            |> Seq.filter (function Named "PropertyGroup" -> true | _ -> false)
            |> Seq.map extractProperties
            |> concatProperties
            |> fixProperties setXboxProperties
            |> dumpProperties

        let itemGroups =
            project.Elements()
            |> Seq.filter (function Named "ItemGroup" -> true | _ -> false)
            |> Seq.map handleItemGroup

        let imports =
            project.Elements()
            |> Seq.filter (function Named "Import" -> true | _ -> false)
            |> Seq.append [xelem (xname "Import") [ xatt (xname "Project") @"$(MSBuildExtensionsPath)\Microsoft\XNA Game Studio\Microsoft.Xna.GameStudio.targets" ] ]

        let rest =
            project.Elements()
            |> Seq.filter (function Named "Import" | Named "PropertyGroup" | Named "ItemGroup" -> false | _ -> true)

        dup project (Seq.concat [ properties ; imports ; itemGroups ; rest ])
    | _ -> failwithf "Expected element Project, got %s" project.Name.NamespaceName

let handleDoc (data : XDocument) =
    seq {
        for child in data.Elements() do
            yield handleProject child
    }
    |> xdoc

let doc2 =
    XDocument.Load(@"C:\Users\johann\Documents\xnautils\XNAUtils\XNAUtils.fsproj")
    |> handleDoc

printfn "%s" (doc2.ToString())

()