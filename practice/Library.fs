module CS334
open Parser

type Shape =
| Circle 

type Expr = 
| Shapes of Shape list

//| Rectangle

let pcircle = pstr "circle" |>> (fun _ -> Circle)
let pcircles = pmany1 pcircle
let grammar = pleft pcircles peof

let rec eval(e: Expr) (x: int) : string =
    match e with 
    | Shapes [] -> ""
    | Shapes (s::ss) -> "<circle x = " + (string x) + " y = 1 >\n " + (eval (Shapes ss) (x+10))
//| r::rr -> "<rect x = " + (string x) + "y = " + (string y) + "height = " + (string h) + ">\n"
    // | _ -> failwith "Not implemented"

 //f in front of print f prints to a file