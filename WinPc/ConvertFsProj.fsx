#r "System.Xml.Linq"

#load "../Core/Misc.fs" // For maybe workflow

open System.Xml.Linq

module Xml =
    [<Literal>]
    let NS = "http://schemas.microsoft.com/developer/msbuild/2003"

    let xdoc (el : #seq<XElement>) = new XDocument(Array.map box (Array.ofSeq el))
    let xNsName n = XName.Get(n, NS)
    let xname n = XName.op_Implicit n
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
        | Some ("", data) -> data |> List.rev |> Some
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

module Conversion =
    open Xml
    open Parsing
    open CleverRake.XnaUtils.Maybe

    type ConversionConfig =
        { fsCorePath : string }

    type Configuration = Config of string | AnyConfig
    type Platform = Platform of string | AnyPlatform

    type PropertyMap = PropertyMap of Map<Configuration * string * string, string * XAttribute seq > // Configuration, item name, item namespace, value, attributes

    type PropertyFixer = FixerMap of Map<Configuration * string * string, string -> string> // Configuration, item name, item namespace, value correction function

    let concatMaps maps =
        maps
        |> Seq.map Map.toSeq
        |> Seq.concat
        |> Map.ofSeq

    let concatProperties maps =
        maps
        |> Seq.map (function PropertyMap m -> m)
        |> concatMaps
        |> PropertyMap

    let fixProperties (FixerMap fixer) (PropertyMap props) =
        let fixerKeys =
            fixer
            |> Map.toSeq
            |> Seq.map fst
            |> Set.ofSeq

        let providedKeys =
            props
            |> Map.toSeq
            |> Seq.map fst
            |> Set.ofSeq

        let keysToAdd = Set.difference fixerKeys providedKeys
        let addedProps =
            keysToAdd
            |> Seq.map (fun k -> (k, ("", Seq.empty)))
            |> Map.ofSeq
            
        [ props ; addedProps ]
        |> concatMaps
        |> Map.map (fun ((config, ln, ns) as k) (v, att) ->
            match Map.tryFind k fixer with
            | Some f -> f v
            | None ->
                match Map.tryFind (AnyConfig, ln, ns) fixer with
                | Some f -> f v
                | None -> v
            ,
            att)
        |> PropertyMap

    let dup (elem : XElement) (newSubs : seq<XElement>) =
        let subs = Seq.concat [ newSubs |> Seq.map box ; elem.Attributes() |> Seq.map box ]
        new XElement(elem.Name, subs)

    let dumpProperties (PropertyMap props) =
        props
        |> Map.toSeq
        |> Seq.map (fun ((config, ns, ln), value) -> (config, (ns, ln, value)))
        |> Seq.groupBy fst
        |> Seq.sortBy (function (AnyConfig, _) -> "" | (Config config, _) -> config)
        |> Seq.map (fun (config, props) ->
            let condition =
                match config with
                | AnyConfig ->
                    None
                | Config c ->
                    Some <| sprintf "'$(Configuration)|$(Platform)' == '%s|Xbox 360'" c
            props
            |> Seq.map (fun (_, (ns, ln, (value, att))) ->
                xelem (XName.Get(ln, ns)) (Seq.append [(xstr value)] (att |> Seq.map box))
                |> box)
            |> fun subs ->
                xelem
                    (xNsName "PropertyGroup")
                    (match condition with
                     | Some condition -> Seq.append [ xatt (xname "Condition") (xstr condition) ] subs
                     | None -> subs)
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
                            |> Seq.map (fun sub -> (config, sub.Name.Namespace.NamespaceName, sub.Name.LocalName), (sub.Value, sub.Attributes()))
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
        |> Map.add (common, NS, "XnaFrameworkVersion") (overWriteWith "v4.0")
        |> Map.add (common, NS, "XnaPlatform") (overWriteWith "Xbox 360")
        |> Map.add (common, NS, "XnaOututType") (overWriteWith "Library")
        |> Map.add (common, NS, "Platform") (overWriteWith "Xbox 360")
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

    let setProjectGuid (FixerMap props) =
        let guid = System.Guid.NewGuid()

        props
        |> Map.add (AnyConfig, NS, "ProjectGuid") (fun _ -> "{" + (guid.ToString()) + "}")
        |> FixerMap

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

        let (|ValuedDll|_|) att =
            let getDll (dllSpec : string) =
                dllSpec.Split [|','|]
                |> Seq.head
                |> fun x -> x.Trim()

            match att with
            | Valued spec ->
                spec |> getDll |> Some
            | _ -> None

        match item with
        | Named "Reference" ->
            let hintOverride =
                match item.Attribute(xname "Include") with
                | null -> None
                | ValuedDll dll when provided.Contains(dll) -> None //path dll
                | ValuedDll "FSharp.Core" ->
                    Some @"Dependencies\FShap.Core.dll"
                | _ -> None

            let requiredTargetFrameworkOverride =
                match item.Attribute(xname "Include") with
                | null -> None
                | ValuedDll "System.Core" ->
                    Some "4.0"
                | _ -> None

            let subs =
                item.Elements()
                |> Seq.choose (fun child -> // Remove HintPath and RequiredTargetFramework if we have overriding values.
                    match child with
                    | Named "HintPath" ->
                        match hintOverride with
                        | None -> Some child
                        | Some _ -> None
                    | Named "RequiredTargetFramework" ->
                        match requiredTargetFrameworkOverride with
                        | None -> Some child
                        | Some _ -> None
                    | _ -> Some child)
                |> Seq.append ( // Set HintPath if override available.
                        match hintOverride with
                        | Some x ->
                            [ xelem (xNsName "HintPath") [ xstr x ] ]
                        | None ->
                            [])
                |> Seq.append ( // Set RequiredTargetFramework if override available.
                        match requiredTargetFrameworkOverride with
                        | Some x ->
                            [ xelem (xNsName "RequiredTargetFramework") [ xstr x ] ]
                        | None ->
                            [])

            dup item subs
        | _ -> item

    let handleItemGroup (group : XElement) =
        let subs =
            group.Elements()
            |> Seq.map handleItem

        dup group subs

    let handleProject opts (project : XElement) =
        match project with
        | Named "Project" ->
            let properties =
                project.Elements()
                |> Seq.filter (function Named "PropertyGroup" -> true | _ -> false)
                |> Seq.map extractProperties
                |> concatProperties
                |> fixProperties (setXboxProperties |> setProjectGuid)
                |> dumpProperties

            let itemGroups =
                project.Elements()
                |> Seq.filter (function Named "ItemGroup" -> true | _ -> false)
                |> Seq.map handleItemGroup

            let imports =
                project.Elements()
                |> Seq.filter (function Named "Import" -> true | _ -> false)
                |> Seq.append [xelem (xNsName "Import") [ xatt (xname "Project") @"$(MSBuildExtensionsPath)\Microsoft\XNA Game Studio\Microsoft.Xna.GameStudio.targets" ] ]

            let rest =
                project.Elements()
                |> Seq.filter (function Named "Import" | Named "PropertyGroup" | Named "ItemGroup" -> false | _ -> true)

            let fsDeps =
                xelem (xNsName "ItemGroup")
                    [ xelem (xNsName "None") [ xatt (xname "Include") (opts.fsCorePath + @"\FSharp.Core.dll") ] ;
                      xelem (xNsName "None") [ xatt (xname "Include") (opts.fsCorePath + @"\FSharp.Core.optdata") ] ;
                      xelem (xNsName "None") [ xatt (xname "Include") (opts.fsCorePath + @"\FSharp.Core.sigdata") ] ;
                      xelem (xNsName "None") [ xatt (xname "Include") (opts.fsCorePath + @"\FSharp.Core.xml") ] ]
                |> Seq.singleton

            dup project (Seq.concat [ properties ; imports ; itemGroups ; fsDeps ; rest ])
        | _ -> failwithf "Expected element Project, got %s" project.Name.NamespaceName

    let handleDoc opts (data : XDocument) =
        seq {
            for child in data.Elements() do
                yield handleProject opts child
        }
        |> xdoc

open Conversion

let here = @"C:\Users\johann\Documents\xnautils"

let paths =
    [ (@"Core", "Core.fsproj", "CoreXbox360.fsproj") ;
      (@"XNAUtils", "XNAUtils.fsproj", "XNAUtilsXbox360.fsproj") ;
      (@"CoopMultitasking", "CoopMultitasking.fsproj", "CoopMultitaskingXbox360.fsproj") ;
      (@"Samples\CoopMultitaskingSample\Lib", "LibWinPc.fsproj", "LibXbox360.fsproj") ]

let mkDotDot (path : string) =
    let countUp (path : string) =
        let goUp (path : string) =
            System.IO.Path.GetDirectoryName(path)

        let rec work path n =
            match goUp path with
            | "" -> None
            | p when p = here -> Some n
            | p -> work p (n + 1)
        if path = here then
            Some 0
        else
            work path 1

    match countUp path with
    | Some n -> Array.create n ".." |> fun xs -> System.String.Join("\\", xs)
    | None -> failwith "Could not compute path to FSharp.Core.dll relative to here"

for (path, inName, outName) in paths do
    let path = System.IO.Path.Combine(here, path)

    let doc2 =
        XDocument.Load(System.IO.Path.Combine(path, inName))
        |> handleDoc { fsCorePath = mkDotDot path + @"\FSharpCore" }

    let outPath = System.IO.Path.Combine(path, outName)
    let overwrite = true
    if overwrite || not <| System.IO.File.Exists(outPath) then
        doc2.Save(outPath)
    else
        eprintfn "Output file already exists."
