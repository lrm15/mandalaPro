module CS334
open Parser

type Shape =
| Circle 
| Rect 

type Expr = 
| Shapes of Shape list

//| Rectangle

let pcircle = pstr "circle" |>> (fun _ -> Circle)
let pcircles = pmany1 pcircle
let grammar = pleft pcircles peof

let rec eval(expr: Expr) (cx: int) (cy: int) (r: int): string =
    match expr with 
    | Shapes [] -> ""
    // | Shapes (s::ss) -> "\t<circle cx='" + (string x) + "' cy='100' r='80' fill='green' />\n" + (eval (Shapes ss) (x + 100)) 
    | Shapes (s::ss) -> 
        match s with 
        | Circle -> "\t<circle cx='" + (string cx) + "' cy='" + (string cy) + "' r='" + (string r) + "' fill='none' stroke='black' />\n" + (eval (Shapes ss) cx cy (r / 2)) 
        | Rect -> "\t<rect width='" + (string 200) + "' height='" + (string 200) + "' fill='none' stroke='black' />\n" 
        | _ -> failwith "not a circle or a rectangle idk man"


// let rec eval(expr: Expr) (cx: int) (cy: int) (r: int): string =
//     match expr with 
//     | Shapes [] -> ""
//     // | Shapes (s::ss) -> "\t<circle cx='" + (string x) + "' cy='100' r='80' fill='green' />\n" + (eval (Shapes ss) (x + 100)) 
//     | Shapes (s::ss) -> 
//         match s with 
//         | Circle -> "\t<circle cx='" + (string cx) + "' cy='" + (string cy) + "' r='" + (string r) + "' fill='none' stroke='black' />\n" + (eval (Shapes ss) cx cy (r / 2)) 
//         // | Rect -> "\t<rect width='" + 
//         | _ -> failwith "not a circle idk man"

    
