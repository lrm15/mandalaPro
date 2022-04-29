open System.IO
open CS334

[<EntryPoint>]
let main argv = 

    use sw = new StreamWriter("newpractice.svg")

    sw.WriteLine("<svg version='1.1'\n\t width='300' height='200'\n\t xmlns='http://www.w3.org/2000/svg'>")

    // sw.WriteLine("<svg>") // start tag
    sw.WriteLine(eval (Shapes [Circle; Circle; Circle]) 3)
    sw.WriteLine("</svg>") // end tag

    0
