namespace Problem

open System
open System.Windows.Forms
open Forms

module Main =
    [<EntryPoint>]
    [<STAThread>]
    let main argv = 
        Application.EnableVisualStyles()
        Application.SetCompatibleTextRenderingDefault false
 
        Application.Run (new MainForm ())

        0 
