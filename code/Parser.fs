module Parser

open AST
open Combinator


(* Helper function for stripping arbitrary whitespace between words *)
let pad p = pbetween pwsNoNL0 p pwsNoNL0

(* Helper function for parsing dates of the form month/day/year *)
let date = 
  pmany1 (pdigit <|> (pchar '/') <|> pchar ':' <|> pchar ' ') |>> stringify |>> Date

(* Helper function for parsing date ranges *)
let dateRange = 
  pright
    (pad (pstr "define date range"))
    (pseq
      date
      (pright (pad (pchar '-')) date)
      (fun (a,b) -> (a,b)))
  |>> DateRange

(* Helper funtion that parses letters and returns a word *)
let word = 
  pmany1 (pletter <|> pdigit) |>> stringify

(* Helper function to join list of words *)
let rec join_words (strs: string list): string =
  match strs with
  | [] -> ""
  | s::[] -> s
  | s1::s2::ss -> 
      match (s1,s2) with
      | (_,"'") -> s1 + s2 + join_words(ss)
      | ("'",_) -> s1 + s2 + join_words(ss)
      | (_,".") -> s1 + s2 + " " + join_words(ss)
      | (_,",") -> s1 + s2 + " " + join_words(ss)
      | (_,"-") -> s1 + s2 + join_words(ss)
      | ("-", _) -> s1 + s2 + join_words(ss)
      | _ -> s1 + " " + join_words (s2::ss)

(* Helper function to join words without parens *)
let pOKwords =
  (pmany1 (pad ((word) <|> ((pstr ".")) <|> ((pstr ",")) <|> ((pstr "-")) <|> ((pstr """'"""))))) |>> join_words

(* Parses input of the form (word1 word2 word3) and returns string "word1 word2 word3" *)
let phrase =
  pbetween
   (pad (pchar '('))
   pOKwords
   (pad (pchar ')'))
  //  |>> join_words

(* Parses input of the form caretaker ([name(s)]) *)
let pcaretaker = 
  pright (pstr "caretaker ") ((word <|> phrase) |>> Name)

(* Helper function for pfields: Parses a single field of the form [words1]: [words2] *)
let pfield = 
  pseq
    (pleft
      ((pOKwords) <|> phrase)
      (pchar ':')
      )
    (pleft
      ((pOKwords) <|> phrase)
      (pchar ';')
    ) 
    (fun (a,b) -> (a,b))

(* Parses multiple inputs of the form [words1] : [words2] *)
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
  (((pstr "minutes") <|> (pstr "minute")) |>> (fun _ -> Minute)) <|>
  (((pstr "hours") <|> (pstr "hour")) |>> (fun _ -> Hour)) <|>
  (((pstr "days") <|> (pstr "day")) |>> (fun _ -> Day)) <|>
  (((pstr "weeks") <|> (pstr "week")) |>> (fun _ -> Week)) <|>
  (((pstr "months") <|> (pstr "month")) |>> (fun _ -> Month)) <|>
  (((pstr "years") <|> (pstr "year")) |>> (fun _ -> Year))

(* Parses input of the form "action [name] [frequency] [Timescale]" and returns an Action *)
let action =
  pright 
    (pstr "action")
    (pseq
      (pad (phrase <|> word))
      (pseq
        (pad (pmany1 pdigit |>> stringify |>> int))
        (pad timescale)
        (fun (a,b) -> (a,b))
      )
      (fun (a,(b,c)) -> Action(a,b,c))
    )

let ptype = 
  (((pstr "kid") |>> (fun _ -> Kid)) <|> 
    ((pstr "dog") |>> (fun _ -> Dog)) <|>
    ((pstr "cat") |>> (fun _ -> Cat)) <|>
    ((pstr "house") |>> (fun _ -> House)) <|>
    ((pstr "plant") |>> (fun _ -> Plant)) <|>
    ((pstr "fish") |>> (fun _ -> Fish)) <|>
    ((pstr "adult") |>> (fun _ -> Adult))
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
      (dateRange <|> location <|> action <|> instance <|> pcaretaker)
      (pmany0 pnl)

let expr = pmany1 oneline

let grammar = pleft expr peof

let parse (input)=
  let i = prepare input
  match grammar i with
  | Success(ast, _) -> Some ast
  | Failure(_,_) -> None