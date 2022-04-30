module ProjectInterpreter

open ProjectParser
open Parser
open System 

(* 
 * Method to evaluate the output from the parser. Translates the parser output to an SVG file to generate a Mandala. 
 * 
 * @param   An Expr option. 
 * @return  A string to be written to an SVG file. 
 *)
let rec eval (e: Expr option) : string = 
    match e with 
    | e when e.IsSome = false -> ""
    | e when e.IsSome = true -> 
        match e.Value with 
        | Empty -> ""
        | Mandala (shape) -> 
            match shape with 
            | Circle (color, num) -> 
                match (color, num) with 
                | (Color, Num (x)) -> "\t<circle cx='" + (string 250) + "' cy='" + (string 250) + "' r='" + (string x) + "' fill='none' stroke='" + (string Color).ToLower() + "' />\n"
            | Square (color, num) -> 
                match (color, num) with 
                | (Color, Num (x)) -> "\t<rect x='" + (string 150) + "' y='" + (string 150) + "' width='" + (string x) + "' height='" + (string x) + "' stroke='" + (string Color).ToLower() + "' fill='none' />\n"
