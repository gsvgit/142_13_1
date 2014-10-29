namespace Problem

open System
open System.Windows.Forms
open System.Drawing
open MenuItem

module Forms =
    type MainForm () =
        inherit Form ()
        let createMenu (charts : array<string * _>) =
            let l = charts.Length
            let arr = Array.zeroCreate l
            let rec iter i =
                if i = l
                then arr
                else
                    arr.[i] <- new CustomMenuItem (fst charts.[i], i)
                    iter (i + 1)
            iter 0
        let cc = Charts.getChartControls ()
        let menuLst = createMenu cc
        let menu = new MenuStrip ()
        let next = new ToolStripMenuItem ("Next")
        let prev = new ToolStripMenuItem ("Prev")
        let select = new ToolStripMenuItem ("Select")
        let selected = ref 0
        let ctrl = base.Controls

        let handleNext e =
            let l = Array.length cc
            let s = !selected
            menuLst.[s].Checked <- false
            ctrl.Remove (snd cc.[s])
            let s = if s = l - 1 then 0 else s + 1
            selected := s
            menuLst.[s].Checked <- true
            ctrl.Add (snd cc.[s])
            ()

        let handlePrev e =
            let l = Array.length cc
            let s = !selected
            menuLst.[s].Checked <- false
            ctrl.Remove (snd cc.[s])
            let s = if s = 0 then l - 1 else s - 1
            selected := s
            menuLst.[s].Checked <- true
            ctrl.Add (snd cc.[s])
            ()

        let handleSelect (e : EventArgs) =
            let l = Array.length cc
            let s = !selected
            menuLst.[s].Checked <- false
            ctrl.Remove (snd cc.[s])
            let s = (e :?> CustomEventArgs).Position
            selected := s
            menuLst.[s].Checked <- true
            ctrl.Add (snd cc.[s])
            ()

        do
            base.Text <- "Charts"
            base.Size <- new Size (800, 600)
            base.StartPosition <- FormStartPosition.CenterScreen
            menu.Items.Add (prev) |> ignore
            menu.Items.Add (select) |> ignore
            menu.Items.Add (next) |> ignore
            menuLst.[!selected].Checked <- true
            for i in 0 .. menuLst.Length - 1 do
                select.DropDownItems.Add (menuLst.[i]) |> ignore
                menuLst.[i].Click.Add (handleSelect)
            ctrl.Add (menu)
            ctrl.Add (snd cc.[!selected])
            next.Click.Add (handleNext)
            prev.Click.Add (handlePrev)
                        
        member private this.handleNext e =
            let l = Array.length cc
            let s = !selected
            menuLst.[s].Checked <- false
            ctrl.Remove (snd cc.[s])
            let s = if s = l - 1 then 0 else s + 1
            selected := s
            menuLst.[s].Checked <- true
            ctrl.Add (snd cc.[s])
            ()
