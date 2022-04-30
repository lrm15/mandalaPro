open System 
open Parser
open ProjectParser 
open ProjectInterpreter

[<EntryPoint>]
let main argv = 
    let input = argv.[0]
    let input' = prepare input 

    let asto = parse input'
    // parse input'

    match asto with 
    | Some ast -> printfn "%A" (prettyprint ast)
    | None -> printfn "Invalid program."

    0
