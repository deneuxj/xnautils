module Dialogs

open System.Windows.Forms
open System.IO
open System

type FolderBrowser(title, root) as this =
    inherit Form(Text = title)

    do printfn "DBG: %A" root

    let button_panel = new Panel(Dock = DockStyle.Bottom, Height = 32)

    let ok_btn = new Button(Text = "OK", Dock = DockStyle.Right)
    let cancel_btn = new Button(Text = "Cancel", Dock = DockStyle.Left)

    let treeview = new TreeView(Dock = DockStyle.Fill)
    let root_nodes =
        match root with
        | None ->
            DriveInfo.GetDrives()
            |> Seq.filter(fun di -> di.IsReady)
            |> Seq.map (fun di -> new TreeNode(Text = di.Name, Tag = di.RootDirectory))
            |> Array.ofSeq
        | Some path ->
            let di = DirectoryInfo(path)
            [| new TreeNode(Text = di.Name, Tag = di) |]

    do treeview.Nodes.AddRange(root_nodes)

    let addSubDirs =
        treeview.NodeMouseClick.Subscribe(fun (args : TreeNodeMouseClickEventArgs) ->
            treeview.SelectedNode <- args.Node
            if args.Node.Nodes.Count = 0 then
                match args.Node.Tag with
                | :? DirectoryInfo as di ->
                    try
                        Directory.EnumerateDirectories(di.FullName)
                        |> Seq.map (fun path -> DirectoryInfo(path))
                        |> Seq.map (fun di -> new TreeNode(Text = di.Name, Tag = di))
                        |> Array.ofSeq
                        |> args.Node.Nodes.AddRange
                    with
                    | :? UnauthorizedAccessException -> ()
                | _ -> ()
        )

    let getSelected() =
        if treeview.SelectedNode <> null then
            match treeview.SelectedNode.Tag with
            | :? DirectoryInfo as di -> printfn "DBG >> %s" di.FullName; Some di.FullName
            | _ -> None
        else
            None

    do base.Controls.AddRange [|treeview ; button_panel|]
       button_panel.Controls.AddRange [| cancel_btn; ok_btn |]
       cancel_btn.Click.Add(fun _ -> treeview.SelectedNode <- null; this.Close())
       ok_btn.Click.Add(fun _ -> this.Close())

    member this.Selected = getSelected()
