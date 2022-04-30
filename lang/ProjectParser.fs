module ProjectParser 

open Parser 

(* A primitive type representing permissible colors *)
type Color = 
| Blue 
// | Grey 
// | Indigo 
// | Green
// | Turquoise
// | Blueviolet
// | Red
// | White
// | Pink
// | Purple
// | Yellow 
// | Black 

(* A primitive type representing the size of a shape *)
type Size = 
| Num of int

(* A primitive type representing permissible shapes *)
type Shape = 
| Circle of Color * Size
| Square of Color * Size 

(* A combining form representing a mandala *)
type Expr = 
| Empty 
| Mandala of Shape 

let pexpr, pexprImpl = recparser()

(*
 * Helper method to parse the color blue. 
 *
 * @param 
 * @return   
 *)
let pblue = pstr "blue" |>> (fun _ -> Blue)

(* 
 * Helper method to parse any permissible color. 
 * 
 * @param 
 * @return 
 *)
let pcolor = pblue // <|> etc

(*
 * Helper method to parse the fill color for a shape. 
 *  
 * @param 
 * @return 
 *)
let pfillcolor =
    pbetween 
        (pstr "fill_color = '")
        (pstr "';")
        pcolor 

(*
 * Helper method to parse the stroke color for a shape. 
 *  
 * @param 
 * @return 
 *)
let pstrokecolor =
    pbetween 
        (pstr "stroke_color = '")
        (pstr "'; ")
        pcolor 

let pnumber =
    pmany1 pdigit
    |>> (fun ds -> Num (int (stringify ds)))
    <!> "number"

// let rec psizehelper2 num xs = 
//     match xs with
//     | [] -> num  
//     | x::xs' -> (num * 10) + x + (psizehelper2 num xs') 

(*
 * Helper method to parse the size information for a shape. 
 * 
 * @param 
 * @return 
 *)
let psize = 
    pbetween 
        (pstr "size = ")
        (pchar ';')
        pnumber

(*
 * Helper method to parse the color and size information for a shape. 
 * ex: Circle (fill_color = 'blue'; stroke_color = 'black'; size = 100;)
 * 
 * @param 
 * @return  
 *)
// let pcircle = 
//     pbetween 
//         (pstr "Circle (")
//         (pchar ')')
//         (pseq 
//             (psize)
//             (pseq 
//                 (pfillcolor)
//                 (pstrokecolor)
//                //  (psize)
//                 (fun (x, y) -> (x, y)))                
//             (fun (x, (y, z)) -> Circle (x, y, z)))

let pcircle = 
    pbetween 
        (pstr "Circle (")
        (pchar ')')
        (pseq 
            (pstrokecolor)
            (psize)
            (fun (x, y) -> Circle (x, y))
        )

let pempty = 
    pstr ""


(* 
 * Helper method to parse a circle and its information. 
 *
 * @param 
 * @return 
 *)
// let pcircle = 
//     pbetween 
//         (pstr "Circle (")
//         (pchar ')')
//         pspecs |>> Circle (x, y, z)

(* 
 * Helper method to parse a shape. 
 * 
 * @param 
 * @return 
 *)
let pshape = pcircle // <|> psquare ...

let pmandala = pshape |>> (fun x -> Mandala (x))

(* 
 * Helper method to parse a Mandala of Expr type. A Mandala contains a list of Shapes and their information. 
 * 
 * @param 
 * @return   
 *)
// pexprImpl := pmany1 pshape |>> (fun x -> Mandala(x)) // want to apply pshape to every shape within a list  
pexprImpl := pmandala

let grammar: Parser<Expr> = pleft pexpr peof 

(*
 * Evaluates whether the given input is a valid part of the grammar of type Parser<Expr>. 
 *  
 * @param A string. 
 * @return Some res if successful and None if failure. 
 *)
let parse(s) : Expr option = 
    match (grammar s) with 
    | Success(res,_) -> Some res 
    | Failure(_, _) -> None 
// let parse(s) = 
//     match (grammar s) with 
//     | Success(res,_) -> printf("success")
//     | Failure(_, _) -> printf("failure")

(*
* Turns an abstract syntax tree (AST) into a string. 
*
* @param  An AST
* @return A string representation of the AST fed into the function.
*)
let rec prettyprint(e: Expr) : string =
    match e with
    | Empty -> ""
    | Mandala(shape) -> 
        match shape with 
        | Circle(x, y) -> "Circle (stroke_color = '" + x.ToString() + "'; size = " + y.ToString() + ";) " // + (prettyprint (Mandala (shapelist.Tail))) 


