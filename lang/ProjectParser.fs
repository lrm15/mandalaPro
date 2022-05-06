module ProjectParser 

open Parser 

(* A primitive type representing permissible colors *)
type Color = 
| Blue 
| Grey 
| Indigo 
| Green
| Turquoise
// | Blueviolet
| Red
| White
| Pink
| Purple
| Yellow 
| Black 

(* A primitive type representing the size of a shape *)
type Size = 
| Num of int

(* A primitive type representing permissible shapes *)
type Shape = 
| Circle of Color * Size
| Square of Color * Size 
| Triangle of Color * Size 

(* A combining form representing a mandala *)
type Expr = 
| Empty 
| Mandala of Shape list 

let pexpr, pexprImpl = recparser()

(*
 * Helper method to parse the color blue. 
 *
 * @param   A string. 
 * @return  The Color Blue. 
 *)
let pblue = pstr "blue" |>> (fun _ -> Blue)

(*
 * Helper method to parse the color grey. 
 *
 * @param   A string. 
 * @return  The Color Grey. 
 *)
let pgrey = pstr "grey" |>> (fun _ -> Grey)

(*
 * Helper method to parse the color indigo. 
 *
 * @param   A string. 
 * @return  The Color Indigo. 
 *)
let pindigo = pstr "indigo" |>> (fun _ -> Indigo)

(*
 * Helper method to parse the color turquoise. 
 *
 * @param   A string. 
 * @return  The Color Turquoise. 
 *)
let pturquoise = pstr "turquoise" |>> (fun _ -> Turquoise)

(*
 * Helper method to parse the color blueviolet. 
 *
 * @param   A string. 
 * @return  The Color Blueviolet. 
 *)
// let pblueviolet = pstr "blueviolet" |>> (fun _ -> Blueviolet)

(*
 * Helper method to parse the color red. 
 *
 * @param   A string. 
 * @return  The Color Red. 
 *)
let pred = pstr "red" |>> (fun _ -> Red)

(*
 * Helper method to parse the color white. 
 *
 * @param   A string. 
 * @return  The Color White. 
 *)
let pwhite = pstr "white" |>> (fun _ -> White)

(*
 * Helper method to parse the color pink. 
 *
 * @param   A string. 
 * @return  The Color Pink. 
 *)
let ppink = pstr "pink" |>> (fun _ -> Pink)

(*
 * Helper method to parse the color purple. 
 *
 * @param   A string. 
 * @return  The Color Purple. 
 *)
let ppurple = pstr "purple" |>> (fun _ -> Purple)

(*
 * Helper method to parse the color green. 
 *
 * @param   A string. 
 * @return  The Color Green. 
 *)
let pgreen = pstr "green" |>> (fun _ -> Green)

(*
 * Helper method to parse the color yellow. 
 *
 * @param   A string. 
 * @return  The Color Yellow. 
 *)
let pyellow = pstr "yellow" |>> (fun _ -> Yellow)

(*
 * Helper method to parse the color black. 
 *
 * @param   A string. 
 * @return  The Color Black. 
 *)
let pblack = pstr "black" |>> (fun _ -> Black)


(* 
 * Helper method to parse any permissible color. 
 * 
 * @param    A string.  
 * @return   A Color. 
 *)
let pcolor = pblue <|> pred <|> pgreen <|> pgrey <|> pindigo <|> pturquoise <|> pwhite <|> ppink <|> ppurple <|> pyellow <|> pblack // <|> pblueviolet 

(*
 * Helper method to parse the fill color for a shape. 
 *  
 * @param    A string. 
 * @return   A Color. 
 *)
let pfillcolor =
    pbetween 
        (pstr "fill_color = '")
        (pstr "';")
        pcolor 

(*
 * Helper method to parse the stroke color for a shape. 
 *  
 * @param   A string. 
 * @return  A Color. 
 *)
let pstrokecolor =
    pbetween 
        (pstr "stroke_color = '")
        (pstr "'; ")
        pcolor 

(* 
 * Helper method to parse a number. 
 * 
 * @param    A string. 
 * @return   A Num. 
 *)
let pnumber =
    pmany1 pdigit
    |>> (fun ds -> Num (int (stringify ds)))
    <!> "number"

(*
 * Helper method to parse the size information for a shape. 
 * 
 * @param    A string. 
 * @return   A Num. 
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

(* 
 * A helper method to parse a Circle and the parameters for the Circle constructor. 
 * 
 * @param    A string. 
 * @return   A Circle with the specified color and size. 
 *)
let pcircle = 
    pbetween 
        (pstr "Circle (")
        (pchar ')')
        (pseq 
            (pstrokecolor)
            (psize)
            (fun (x, y) -> Circle (x, y))
        )

(* 
 * A helper method to parse a Square and the parameters for the Square constructor. 
 * 
 * @param    A string. 
 * @return   A Square with the specified color and size. 
 *)
let psquare = 
    pbetween 
        (pstr "Square (")
        (pchar ')')
        (pseq 
            (pstrokecolor)
            (psize)
            (fun (x, y) -> Square (x, y))
        )

(* 
 * A helper method to parse a Triangle and the parameters for the Triangle constructor. 
 * 
 * @param    A string. 
 * @return   A Triangle with the specified color and size. 
 *)
let ptriangle = 
    pbetween 
        (pstr "Triangle (")
        (pchar ')')
        (pseq 
            (pstrokecolor)
            (psize)
            (fun (x, y) -> Triangle (x, y))
        )

(* 
 * A helper method to parse an empty Mandala. 
 * 
 * @param    A string. 
 * @return   The empty string. 
 *)
let pempty = 
    pstr ""

(* 
 * Helper method to parse a shape. 
 * 
 * @param    A string. 
 * @return   A Shape. 
 *)
let pshape = pcircle <|> psquare <|> ptriangle

let pmandalahelper = 
    pmany1 pshape |>> (fun xs -> Mandala (xs)) 

(* 
 * A helper method to parse a Mandala of Expr type. A Mandala consists of a Shape and its specifications. 
 * 
 * @param    A string. 
 * @return   A Mandala. 
 *)
let pmandala = 
    pbetween 
        (pstr "Mandala {")
        (pchar '}')
        pmandalahelper 

// pshape |>> (fun x -> Mandala (x))

pexprImpl := pmandala

let grammar: Parser<Expr> = pleft pexpr peof 

(*
 * Evaluates whether the given input is a valid part of the grammar of type Parser<Expr>. 
 *  
 * @param  A string. 
 * @return Some res if successful and None if failure. 
 *)
let parse(s) : Expr option = 
    match (grammar s) with 
    | Success(res, _) -> Some res 
    | Failure(_, _) -> None

(*
 * Turns an abstract syntax tree (AST) into a string. 
 *
 * @param  An Expr. 
 * @return A string representation of the AST fed into the function.
 *)
let rec prettyprint(e: Expr) : string =
    match e with
    | Empty -> ""
    | Mandala(xs) ->
        match xs with 
        | x::xs' -> 
            match x with 
            | Circle (x, y) -> "(Mandala (Circle (" + x.ToString() + ", " + y.ToString() + "))))" + (prettyprint (Mandala (xs')))// + (prettyprint (Mandala (shapelist.Tail))) 
            | Square (x, y) -> "(Mandala (Square (" + x.ToString() + ", " + y.ToString() + "))))" + (prettyprint (Mandala (xs')))
            | Triangle (x, y) -> "(Mandala (Triangle (" + x.ToString() + ", " + y.ToString() + "))))" +  (prettyprint (Mandala (xs')))
        | _ -> ""



     