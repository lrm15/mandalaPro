module ProjectInterpreter

open ProjectParser
open Parser
open System 

(* 
 * Method to extract an Expr value from an Expr option. 
 * 
 * @param   An Expr option. 
 * @return  An Expr. 
 *)
let extractExpr (e: Expr option) : Expr = 
    match e with 
    // | e when e.IsSome = false -> Empty 
    | e when e.IsSome = true -> e.Value 
    | _ -> Empty 

(* 
 * Method to evaluate the output from the parser. Translates the parser output to an SVG file to generate a Mandala. 
 * 
 * @param   An Expr. 
 * @return  A string to be written to an SVG file. 
 *)
let rec eval (e: Expr) : string = 
        match e with 
        | Empty -> ""
        | Mandala (xs) ->
            match xs with 
            | x::xs' -> 
                match x with 
                | Circle (color, Num (x)) -> "\t<circle cx='" + (string 250) + "' cy='" + (string 250) + "' r='" + (string x) + "' fill='none' stroke='" + (string color).ToLower() + "' />\n" + (eval (Mandala (xs')))
                | Square (color, Num (x)) -> "\t<rect x='" + (string (250 - (x / 2))) + "' y='" + (string (250 - (x / 2))) + "' width='" + (string x) + "' height='" + (string x) + "' stroke='" + (string color).ToLower() + "' fill='none' />\n" + (eval (Mandala (xs')))
                // | Square (color, Num (x)) -> "\t<rect x='" + (string (250 - (x))) + "' y='" + (string (250 - (x))) + "' width='" + (string (2 * x)) + "' height='" + (string (2 * x)) + "' stroke='" + (string color).ToLower() + "' fill='none' />\n" + (eval (Mandala (xs')))
                | Triangle (color, Num (x)) -> "\t<polygon points = '" + (string 250) + " " + (string (250 + (x / 2))) + ", " + (string (250 + (x / 2))) + " " + (string (250 - (x / 2))) + ", " + (string (250 - (x / 2))) + " " + (string (250 - (x / 2))) + "' stroke='" + (string color).ToLower() + "' fill='none' />\n" + (eval (Mandala (xs')))
                // | Triangle (color, Num (x)) -> "\t<polygon points = '" + (string 250) + " " + (string (250 + (x))) + ", " + (string (250 + (x))) + " " + (string (250 - (x)) + ", " + (string (250 - (x / 2))) + " " + (string (250 - (x / 2))) + "' stroke='" + (string color).ToLower() + "' fill='none' />\n" + (eval (Mandala (xs')))
                | Diamond (color, Num (x)) -> "\t<polygon points = '" + (string 250) + " " + (string (250 - (x / 2))) + ", " + (string (250 + (x / 2))) + " " + (string 250) + ", " + (string 250) + " " + (string (250 + (x / 2))) + ", " + (string (250 - (x / 2))) + " " + (string 250) + "' stroke='" + (string color).ToLower() + "' fill='none' />\n" + (eval (Mandala (xs')))
            | _ -> ""


