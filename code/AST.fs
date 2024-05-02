module AST



(*
    <expr>      ::= define <timeframe> <expr>
                 |  new <new_thing> <expr>
                 |  add <action> <instance> <expr>
                 |  add <instance> <location> <expr>
                 |  <empty>
    <new_thing> ::= <type>
                 |  <instance>
                 |  <action>
                 |  <location>
    <type>      ::= string                            // FOR NOW
    <instance>  ::= string                            // FOR NOW
    <action>    ::= string * int * <Timescale>
    <Timescale> ::= Minute | Hour | Day | Week | Month | Year
    <location>  ::= string * <instance> list
    <timeframe> ::= <date> * <date>
    <date>      ::= int/int/int

*)




type Timescale =
| Minute
| Hour
| Day
| Week
| Month
| Year

type Type =
| Kid
| Plant 
| Dog
| Cat
| House
| Fish

type Date = {mon: int; day: int; year: int}

type Fields = {info: (string * string) list}

type Expr =
| Name of string
| Num of int
| Location of string * Fields
| Action of string * int * Timescale
| Instance of Type * string  * Fields
//| New_Thing //of Expr

//type Doc = Expr list
