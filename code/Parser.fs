module Parser

open AST
open Combinator



let expr, exprImpl = recparser()

(* Helper function for stripping arbitrary whitespace between words *)
let pad p = pbetween pws0 p pws0

(* Helper function for parsing dates--DO I NEED THIS? *)
let date_val =
  pseq 
    (pchar '0' <|> pchar '1' <|> pchar '2' <|> pchar '3')
    (pdigit)
    (fun (a,b) -> [a;b])
  |>> stringify

(* Returns a 3-tupe consisting of month, day, year *)
let date = 
  pseq
    (pleft date_val (pchar '/'))
    (pseq
      (pleft date_val (pchar '/'))
      date_val
      (fun (a,b) -> (int a, int b)))
    (fun (a,(b,c)) ->(int a, int b, int c))

(* Parses digits and returns a Num *)
let number = 
  pmany1 pdigit |>> stringify |>> (fun s -> int(s)) |>> Num

(* Helper funtion that parses letters and returns a word *)
let word = 
  pmany1 pletter |>> stringify

(* Helper function to join list of words *)
let rec join_words (strs: string list): string =
  match strs with
  | [] -> ""
  | s::[] -> s
  | s::ss -> s + " " + join_words (ss)

(* Parses input of the form (word1 word2 word3) and returns string "word1 word2 word3" *)
let phrase =
  pbetween
   (pad (pchar '('))
   (pmany1 ((pad word) <|> (pad (pstr ".")) <|> (pad (pstr ",")) <|> (pad (pstr """'"""))))
   (pad (pchar ')'))
   |>> join_words
   

(* Returns a Name *)
let name = 
  word |>> (fun s -> Name(s))

(* Parses input of the form "location (name) [int or string]" and returns a Location *)
let location = 
  pbetween
    (pstr "location")
    (pseq
      (pad (word <|> phrase))
      (pad expr)
      (fun(a,b) -> Location(a,b))
    )
    (pws0)

(* Timescale parser *)
let timescale =
  ((pstr "Minute" |>> (fun _ -> Minute)) <|>
  (pstr "Hour" |>> (fun _ -> Hour)) <|>
  (pstr "Day" |>> (fun _ -> Day)) <|>
  (pstr "Week" |>> (fun _ -> Week)) <|>
  (pstr "Month" |>> (fun _ -> Month)) <|>
  (pstr "Year" |>> (fun _ -> Year)))  

(* Parses input of the form "action [name] [frequency] [Timescale]" and returns an Action *)
let action =
  pright 
    (pstr "action")
    (pseq
      (pad word)
      (pseq
        (pad (pmany1 pdigit |>> stringify |>> int))
        (pad timescale)
        (fun (a,b) -> (a,b))
      )
      (fun (a,(b,c)) -> Action(a,b,c))
    )

let ptype = 
  (((pstr "Kid") |>> (fun _ -> Kid)) <|> 
    ((pstr "Dog") |>> (fun _ -> Dog)) <|>
    ((pstr "Cat") |>> (fun _ -> Cat)) <|>
    ((pstr "House") |>> (fun _ -> House)) <|>
    ((pstr "Plant") |>> (fun _ -> Plant)) <|>
    ((pstr "Fish") |>> (fun _ -> Fish))
  )

(* Parses input of the form "new [Type] [Name]"*)
let instance =
  pright
    (pad (pstr "new"))
    (pseq
      (pad ptype)
      (pad phrase)
      (fun(a,b) -> (a,b))
    )
    |>> Instance

// let sequence = 
//   pseq
//     (pad expr)
//     (pright pws1 expr)
//     (fun(a,b) -> [a;b])
//   |>> Sequence


(* Parses one line (one expr) of the program *)
let oneline =
  pbetween 
      (pws0)
      (number <|> location <|> action <|> instance <|> name <|> expr)
      (pws0)

exprImpl := oneline

//let exprs = pmany1 (pad expr)

let grammar = pleft expr peof

let parse (input)=
  let i = prepare input
  match grammar i with
  | Success(ast, _) -> Some ast
  | Failure(_,_) -> None

