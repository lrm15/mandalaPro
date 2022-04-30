module ProjectInterpreter

open ProjectParser
open Parser
open System 

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

