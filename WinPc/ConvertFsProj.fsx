#r "System.Xml.Linq"
#r "System.Windows.Forms"

open System.Xml.Linq
open System.Windows.Forms

[<Literal>]
let NS = "http://schemas.microsoft.com/developer/msbuild/2003"

type Configuration = Config of string

type PcProperties = PcProps of Map<Configuration * string, string>

module Xml =
    let xdoc (el : #seq<XElement>) = new XDocument(Array.map box (Array.ofSeq el))
    let xname = XName.op_Implicit
    let xelem s el = new XElement(s, box el)
    let xatt a b = new XAttribute(a, b) |> box
    let xstr s = box s

    let (|Named|_|) (elem : XElement) =
        if elem.Name.Namespace.NamespaceName = NS then
            Some elem.Name.LocalName
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

let dup (elem : XElement) (newSubs : seq<XElement>) =
    let subs = Seq.concat [ newSubs |> Seq.map box ; elem.Attributes() |> Seq.map box ]
    new XElement(elem.Name, subs)

let parseProperties (elem : XElement) =
    match elem with
    | Named "PropertyGroup" ->
        match elem.Attribute(xname (NS + "/Condition")) with
        | null -> ()
        | attr -> ()

        elem
    | _ -> failwithf "Expected PropertyGroup, got %s" elem.Name.NamespaceName

let replaceProjectGuid (elem : XElement) =
    let guid = System.Guid.NewGuid()

    match elem with
    | Named "ProjectGuid" -> xelem (elem.Name) [ xstr ("{" + (guid.ToString()) + "}") ]
    | _ -> elem

let handleGroup (group : XElement) =
    match group with
    | Named "PropertyGroup" ->
        let subs =
            group.Elements()
            |> Seq.map replaceProjectGuid

        dup group subs
    | _ -> group

let handleProject (project : XElement) =
    match project with
    | Named "Project" ->
        let subs =
            project.Elements()
            |> Seq.map handleGroup

        dup project subs
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