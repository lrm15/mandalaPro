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

let rec eval(e: Expr) (cx: int) (cy: int) (r: int): string =
    match e with 
    | Shapes [] -> ""
    // | Shapes (s::ss) -> "\t<circle cx='" + (string x) + "' cy='100' r='80' fill='green' />\n" + (eval (Shapes ss) (x + 100)) 
    | Shapes (s::ss) -> "\t<circle cx='" + (string cx) + "' cy='" + (string cy) + "' r='" + (string r) + "' fill='none' stroke='black' />\n" + (eval (Shapes ss) cx cy (r / 2)) 
    
