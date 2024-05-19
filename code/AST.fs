module AST



(*
    <expr>          ::= caretaker <name>
                     |  location <location>
                     |  action <action>
                     |  new <instance>
                     |  define date range <date_range>
    <name>          ::= string
    <location>      ::= string <fields>
    <action>        ::= string * int * <timescale>
    <instance>      ::= <category> * string * <fields>
    <date_range>    ::= <date> * <date>
    <fields>        ::= string: string; *
    <date>          ::= int/int/int int:int
    <catergory> ::= Kid | Plant | Dog | Cat | House | Fish | Adult
    <timescale> ::= Minute | Hour | Day | Week | Month | Year

*)




type Timescale =
| Minute
| Hour
| Day
| Week
| Month
| Year

type Category =
| Kid
| Plant 
| Dog
| Cat
| House
| Fish
| Adult

type Date = string

type Fields = {info: (string * string) list}

type Expr =
| Name of string
| Location of string * Fields
| Action of string * int * Timescale
| Instance of Category * string  * Fields
| DateRange of Date * Date