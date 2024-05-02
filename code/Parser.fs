module Parser

open AST
open Combinator



//let expr, exprImpl = recparser()

(* Helper function for stripping arbitrary whitespace between words *)
let pad p = pbetween pwsNoNL0 p pwsNoNL0

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
    (fun (a,(b,c)) ->({mon = int a; day = int b; year = int c} ))

(* TEST PARSER: Parses digits and returns a Num *)
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
  | s1::s2::ss -> 
      match (s1,s2) with
      | ("'",_) -> s1 + join_words(s2::ss)
      | (_,".") -> s1 + s2 + join_words(ss)
      | (_,",") -> s1 + s1 + join_words(ss)
      | _ -> s1 + " " + join_words (s2::ss)

(* Helper function to join words without parens *)
let pOKwords =
  (pmany1 ((pad word) <|> (pad (pstr ".")) <|> (pad (pstr ",")) <|> (pad (pstr """'""")))) 

(* Parses input of the form (word1 word2 word3) and returns string "word1 word2 word3" *)
let phrase =
  pbetween
   (pad (pchar '('))
   pOKwords
   (pad (pchar ')'))
   |>> join_words
   
(* TEST PARSER: Returns a Name *)
let name = 
  word |>> (fun s -> Name(s))

let pcaretaker = 
  pright (pstr "Caretaker: ") name

(* Helper function for pfields: Parses a single field of the form [words1]: [words2] *)
let pfield = 
  pseq
    (pleft
      (pOKwords |>> join_words)
      (pchar ':')
      )
    (pleft
      (pOKwords |>> join_words)
      (pchar ';')
    ) 
    (fun (a,b) -> (a,b))

let pfields = 
  (pmany0 (pfield))
  |>> (fun a -> {info = a})

(* Parses input of the form "location (name) [Fields]" and returns a Location *)
let location = 
  pbetween
    (pstr "location")
    (pseq
      (pad (word <|> phrase))
      (pad pfields)
      (fun(a,b) -> Location(a, b))
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
      (pseq
        (pad phrase)
        (pfields)
        (fun (a,b) -> (a,b))
      )
      (fun(a,(b,c)) -> (a,b,c))
    )
    |>> Instance

(* Parses one line (one expr) of the program *)
let oneline =
  pbetween 
      (pws0)
      (number <|> location <|> action <|> instance <|> name)// <|> expr)
      (pmany0 pnl)

//exprImpl := oneline

let expr = pmany1 oneline

let grammar = pleft expr peof

let parse (input)=
  let i = prepare input
  match grammar i with
  | Success(ast, _) -> Some ast
  | Failure(_,_) -> None

