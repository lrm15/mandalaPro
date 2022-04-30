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
| Mandala of Shape list  

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
        (pchar "';")
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
        (pstr "';")
        pcolor 

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
        pdigit 

(*
 * Helper method to parse the color and size information for a shape. 
 * ex: Circle (fill_color = 'blue'; stroke_color = 'black'; size = 100;)
 * 
 * @param 
 * @return  
 *)
let pspecs = 
    pseq 
        pfillcolor
        pseq 
            pstrokecolor
            psize 
            (fun something ?)
        (want to return specs in a form that can be passed to a shape constructor)


(* 
 * Helper method to parse a circle and its information. 
 *
 * @param 
 * @return 
 *)
let pcircle = 
    pbetween 
        (pstr "Circle (")
        (pchar ')')
        pspecs |>> Circle (x, y, z)

(* 
 * Helper method to parse a shape. 
 * 
 * @param 
 * @return 
 *)
let pshape = pcircle // <|> psquare ...

(* 
 * Helper method to parse a Mandala of Expr type. A Mandala contains a list of Shapes and their information. 
 * 
 * @param 
 * @return   
 *)
pexprImpl := pmany1 pshape // want to apply pshape to every shape within a list  

let grammar: Parser<Expr> = pleft pexpr peof 

(*
 * Evaluates whether the given string is a valid part of the grammar of type Parser<Expr>. 
 *  
 * @param A string. 
 * @return Some res if successful and None if failure. 
 *)
 let parse(s : string) = 
    match grammar s with 
    | Success(res,_) -> Some res
    | Failure(_, _) -> None 


