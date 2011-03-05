module Dialogs

open System.Windows.Forms
open System.IO
open System
open System.Threading.Tasks

// NOTE: Works only if module vars are initialized from the GUI thread, which seems to be the case.
let gui_context = Threading.SynchronizationContext.Current
let single_h = 32

type FolderBrowser(title, root) as this =
    inherit Form(Text = title)

    do printfn "DBG: %A" root

    let button_panel = new Panel(Dock = DockStyle.Bottom, Height = single_h)

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


type MessageBox(title: string, text: string, button_texts : string seq, focus : int) as this =
    inherit Form(Text = title)

    let main_text = new TextBox(Multiline = true, ReadOnly = true, Dock = DockStyle.Fill)
    do main_text.Lines <- text.Split([|'\n'|])
    do this.Controls.Add(main_text)

    let layout = new FlowLayoutPanel(FlowDirection = FlowDirection.LeftToRight, Dock = DockStyle.Bottom, Height = single_h)
    do this.Controls.Add(layout)

    let selected = ref None

    let buttons =
        button_texts
        |> Seq.mapi (fun i txt ->
            let button = new Button(Text = txt)
            button.Click.Add(fun _ -> selected := Some i; this.Close())            
            button
            )
        |> Array.ofSeq
    do for btn in buttons do layout.Controls.Add(btn)

    do this.AcceptButton <- buttons.[focus]

    member this.Selected = !selected


type SingleSignInDialog() as this =
    inherit Form(Text = "Sign in")

    let flow = new FlowLayoutPanel(FlowDirection = FlowDirection.LeftToRight, Dock = DockStyle.Fill, Height = single_h)

    let label = new Label(Text = "Your gamertag: ")
    let input = new TextBox()
    do flow.Controls.AddRange [| label; input |]

    let button_panel = new Panel(Dock = DockStyle.Bottom, Height = single_h)

    let ok_btn = new Button(Text = "OK", Dock = DockStyle.Right)
    let cancel_btn = new Button(Text = "Cancel", Dock = DockStyle.Left)
    do button_panel.Controls.AddRange [|ok_btn; cancel_btn|]

    do this.Controls.AddRange [| flow; button_panel |]


let doThenMaybeCallback(task : Task<'T>, cb : AsyncCallback) =
    let context = Threading.SynchronizationContext.Current

    task.Start()

    if cb <> null then
        async {
            Async.AwaitTask(task) |> ignore
            do! Async.SwitchToContext(context)
            cb.Invoke(task)
        }
        |> Async.Start
