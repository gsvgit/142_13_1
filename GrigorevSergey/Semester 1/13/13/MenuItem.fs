namespace Problem

open System
open System.Windows.Forms

module MenuItem =
    type CustomEventArgs (n : int) =
        inherit EventArgs ()
        member this.Position = n
        
    type CustomMenuItem (text : string, number) =
        inherit ToolStripMenuItem (text)
        override this.OnClick (e) =
            base.OnClick (new CustomEventArgs (number))

