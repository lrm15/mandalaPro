open System 
open System.IO
open Parser
open ProjectParser 
open ProjectInterpreter


[<EntryPoint>]
let main argv = 
    let input = argv.[0]
    let input' = prepare input 

    match (parse input') with 
    | Some ast -> printfn "%A" (prettyprint ast)
    | None -> printfn "Invalid program."

    use sw = new StreamWriter("circle.svg")
    sw.WriteLine("<svg version='1.1'\n\t width='500' height='500'\n\t xmlns='http://www.w3.org/2000/svg'>")

    // sw.WriteLine("<svg>") // start tag
    // sw.WriteLine(eval (Mandala (Circle (Blue, Num 100)))) 
    sw.WriteLine(eval (parse input'))
    sw.WriteLine("</svg>") // end tag


    0
