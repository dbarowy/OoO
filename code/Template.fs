module Template
open AST
open System.IO


let open_items = """\begin{itemize}""" + "\n"
let close_items = """\end{itemize}""" + "\n"


let section (title: string) =
    """\section{""" + title + """}""" + "\n"


let setup = 
  """\documentclass{article}"""+ "\n" +
    """\usepackage{graphicx}"""+ "\n" +
    """\usepackage{times,graphicx,epstopdf,fancyhdr,amsfonts,amsthm,amsmath,algorithm,algorithmic,xspace,hyperref}"""+ "\n" +
    """\usepackage[left=1in,top=1in,right=1in,bottom=1in]{geometry}"""+ "\n" +
    """\usepackage{sect sty}"""+ "\n" +
    """\usepackage{enumerate}"""+ "\n" +
    """\usepackage{epsfig}"""+ "\n" +
    """\usepackage[space]{grffile}"""+ "\n" +
    """\usepackage{booktabs}"""+ "\n" +
    """\usepackage{forest}"""+ "\n" +
    """\usepackage{color}"""+ "\n" +

    """\title{*REPLACE*}"""+ "\n" +
    """\author{*REPLACE*}"""+ "\n" +
    """\date{*REPLACE*}"""+ "\n" +

    """\begin{document}"""+ "\n" +

    """\maketitle"""+ "\n"

let wrapup =   "\n" + """\end{document}"""
