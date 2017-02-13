// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open TinyMod
open TinyParser
open Microsoft.FSharp.Text

[<EntryPoint>]
let main argv = 
    printfn "TINY language interpreter. Select file in dialog to interpret ..."
    let mutable o:System.Windows.Forms.OpenFileDialog = new System.Windows.Forms.OpenFileDialog()
    o.Filter <- "TINY language code (*.tiny)|*.tiny|Any file (*.*)|*.*"
    if o.ShowDialog() = System.Windows.Forms.DialogResult.OK 
        then 
            let lexbuf = Microsoft.FSharp.Text.Lexing.LexBuffer<_>.FromTextReader(System.IO.File.OpenText(o.FileName))
            try
                let y = TinyParser.program TinyLex.tokenize lexbuf   
                printfn "%A" y
                let startupMem = memoryState()
                let w = processList y startupMem
                dumpMemoryState w
            with
            | n -> printfn "An exception appears %A" n
    0 // return an integer exit code

