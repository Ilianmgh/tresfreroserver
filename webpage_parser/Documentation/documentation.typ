#import "@preview/curryst:0.6.0" : *
#import "@preview/tdtr:0.5.0" : *
#let todo = highlight[TODO]

#set text(font:"Libertinus Sans")
#set page(margin:2em)
#let embed_page(body) = box.with(baseline:30%,stroke:stroke(thickness:.1em), inset:.3em)(body)
#let embed_page_math(body) = box.with(baseline:25%,stroke:stroke(thickness:.1em), inset:.7em)(align(center + horizon, body))

#show link: it => text(blue, underline(it))

#let remark(..args) = {
    if args.pos().len() == 2 [
      *Remark.* _#args.pos().at(0)_: #args.pos().at(1)
    ] else if args.pos().len() == 1 [
      *Remark.* #args.pos().at(0)
    ] else {
      highlight[At most 2 elements expected in remark]
    }
  }

#let example(..args) = {
    if args.pos().len() == 2 [
      *Example.* _#args.pos().at(0)_: #args.pos().at(1)
    ] else if args.pos().len() == 1 [
      *Example.* #args.pos().at(0)
    ] else {
      highlight[At most 2 elements expected in remark]
    }
  }

#let mllike = smallcaps[Ml-Like for the web]
#let temp = text.with(fill:rgb(100,100,100))

#align(center + horizon)[The #mllike Documentation & Manual]

#pagebreak()

#outline()

#pagebreak()

= Introduction

#mllike is a functional language for writing dynamic webpages, evaluated on the server side.

= Language Syntax

#let extern_word(body) = text(style: "italic", weight: "bold")[#body]
#let grammar(body) = text(font: "DejaVu Sans Mono", size:.8em, body)
#let tab = h(1em)
== Webpage

A _dynamic webpage_ (i.e. the programs we evaluate) consists of a list of _elements_.

Each element can be of three sorts:
- pure HTML code ; or
- #mllike code, which can be either:
  - a list of _global declarations_ ; or
  - an expression.

#grammar[
bodyexp: #extern_word[anyHtmlCode] (<{#extern_word[exp] | (#extern_word[global_declaration])\*}> #extern_word[anyHtmlCode])\*
]

== Global declaration

A global declaration declares and defines a variable, whose scope is (the remainder of) the entire webpage. They (currently) are of two kinds: either a simple global declaration (with #grammar[let]) or a declaration of a session variable #grammar[Session.let].

#grammar[declarator: let | Session.let]

#grammar[global_declaration: #extern_word[declarator] #extern_word[id] = #extern_word[exp]]

#temp[For now, only expressions can be declared by the user. At some point, we would like to allow user-defined types and modules.]

== Expressions

#columns(3)[
  === General expressions:

  #grammar[
    exp: e, e', e'', ... ::=\
      #tab | let #extern_word[id] = e in e'\
      #tab | fun #extern_word[id] -> e\
      #tab | fixfun #extern_word[id] #extern_word[id] -> e\
      #tab | e e'\
      #tab | if e then e' else e''\
      #tab | e;e'\
      #tab | #extern_word[id]\
      #tab | #extern_word[aexp] | #extern_word[bexp] | #extern_word[sexp]\
      #tab | #extern_word[texp]\
      #tab | #extern_word[uexp]\
      #tab | #extern_word[html] | #extern_word[dbexp]\
      #tab | (e)\
      #tab | begin e end
  ]

  #colbreak()

  === Arithmetic expressions:

  #grammar[
    aexp:\
      #tab |  #extern_word[exp] + #extern_word[exp]\
      #tab | #extern_word[exp] - #extern_word[exp]\
      #tab | #extern_word[exp] \* #extern_word[exp]\
      #tab | #extern_word[exp] / #extern_word[exp]\
      #tab | #extern_word[exp] ^ #extern_word[exp]\
      #tab | #extern_word[int_literal]
  ]

  #colbreak()

  === Boolean expressions:

  #grammar[
    bexp:\
      #tab | #extern_word[exp] < #extern_word[exp]\
      #tab | #extern_word[exp] > #extern_word[exp]\
      #tab | #extern_word[exp] <= #extern_word[exp]\
      #tab | #extern_word[exp] >= #extern_word[exp]\
      #tab | #extern_word[exp] = #extern_word[exp]\
      #tab | #extern_word[exp] <> #extern_word[exp]\
      #tab | #extern_word[exp] && #extern_word[exp]\
      #tab | #extern_word[exp] || #extern_word[exp]\
      #tab | not #extern_word[exp]\
      #tab | #extern_word[bool_literal]
  ]
]

String expressions:

#grammar[sexp: #extern_word[exp] ++ #extern_word[exp] | #extern_word[string_literal]/*| #extern_word[fstring_literal]*/]

Tuple expressions:

#grammar[texp: #extern_word[exp], #extern_word[exp]]

#temp[For now, only couples are allowed, and `(x1, x2, x3, x4)` is parsed as `(x1, (x2, (x3, x4)))`.]

// Unit expression:

// #grammar[uexp: ()] #todo

HTML:

#grammar[html: <[ #extern_word[anyHtmlCode] ]>]


== Identifiers

In #mllike, identifers are _namespaced_. An identifier is a sequence of namespace and the actual variable name.

#grammar[
value_name: (\_|[a-z])(\_|'|[0-9]|[a-z]|[A-Z])\*
]

Whereas a path is a sequence of modules name separated by a dot:

#grammar[module_name : [A-Z](\_|'|[0-9]|[a-z]|[A-Z])\*]

#grammar[path: M, M' ::= #extern_word[module_name] | #extern_word[module_name]\.M']

A identifier (path to a value) is then:

#grammar[id: (#extern_word[path].)?#extern_word()[value_name]]

#example[
- variable
- Session.my\_function_n_362
- \_MyFunction
- A.B.C.D.E.myVariable067

But not:
- MyFunction
- SomeModule.01var
]

== Literals

=== Integers

For better readability for the programmer, we allow underscores in numbers.

#grammar[int_literal: [0-9]([0-9]|\_)\*]

#example[
  - 123
  - 100\_000
  - 1\_2\_\_\_\_\_3\_\_\_\_
]
=== Strings

#grammar[string_literal: "..."]

// === Format strings

// Format strings are delimited by: `f"..."`. A formatter can be inserted in a format string with `%(value)` #todo not implemented _yet_.

=== Booleans

#grammar[bool_literal: true, false]

#pagebreak()

= Type system

#mllike is strongly, statistically typed. #temp[For now, user type annotation are not allowed.]

== Types

The only atomic native types represent integers, booleans, strings, unit, html code and databases.
They can be combined with type constructors #sym.arrow for functions and #sym.times for tuples. #temp[For now, sum types are not implemented, but should be with user-defined types.]

#grammar[tlit: int | bool | string | unit | html | db]

#grammar[#sym.alpha, #sym.beta, ... ::= #sym.alpha -> #sym.beta | #sym.alpha \* #sym.beta]

== Typing rules

The only lax rule is the typing rule of sequences: if the left handside of the #grammar[;] is not of type unit, it will only raise a warning.

#align(center, stack(spacing: 1em,
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        $Gamma tack #grammar[e] : alpha$,
        $Gamma, #grammar[x] : alpha tack #grammar[e'] : beta$,
        $Gamma tack #grammar[let x = e in e'] : beta$
      )),
      prooftree(rule(
        $Gamma, #grammar[x] : alpha tack #grammar[e] : beta$,
        $Gamma tack #grammar[fun x -> e] : alpha -> beta$
      )),
      prooftree(rule(
        $Gamma, #[f] : alpha -> beta, #grammar[x] : alpha tack #grammar[e] : beta$,
        $Gamma tack #grammar[fixfun f x -> e] : alpha -> beta$
      ))
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        $Gamma tack #grammar[e] : alpha -> beta$,
        $Gamma tack #grammar[e'] : alpha$,
        $Gamma tack #grammar[e e'] : beta$
      )),
      prooftree(rule(
        $Gamma tack #grammar[e] : #grammar[bool]$,
        $Gamma tack #grammar[e'] : alpha$,
        $Gamma tack #grammar[e''] : alpha$,
        $Gamma tack #grammar[if e then e' else e''] : alpha$
      )),
      prooftree(rule(
        $Gamma tack #grammar[e] : #grammar[unit]$,
        $Gamma tack #grammar[e'] : alpha$,
        $Gamma tack #grammar[e;e'] : alpha$
      ))
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(label: [#sym.ast.op.o: #grammar[+, -, \*, /,] or #grammar[^]],
        $Gamma tack #grammar[e] : #grammar[int]$,
        $Gamma tack #grammar[e'] : #grammar[int]$,
        $Gamma tack #grammar[e #sym.ast.op.o e'] : #grammar[int]$
      )),
      prooftree(rule(label: [#sym.ast.op.o: #grammar[>, <, >=, <=, =] or #grammar[<>]],
        $Gamma tack #grammar[e] : alpha$,
        $Gamma tack #grammar[e'] : alpha$,
        $Gamma tack #grammar[e #sym.ast.op.o e'] : #grammar[bool]$
      ))
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(label: [#sym.ast.op.o: #grammar[&&] or #grammar[||]],
        $Gamma tack #grammar[e] : #grammar[bool]$,
        $Gamma tack #grammar[e'] : #grammar[bool]$,
        $Gamma tack #grammar[e] #sym.ast.op.o #grammar[e'] : #grammar[bool]$
      )),
      prooftree(rule(
        $Gamma tack #grammar[e] : #grammar[bool]$,
        $Gamma tack #grammar[not e] : #grammar[bool]$
      )),
      prooftree(rule(label: $Gamma(x) = alpha$,
        $Gamma tack x : alpha$
      ))
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        $Gamma tack #grammar[b] : #grammar[bool]$
      )),
      prooftree(rule(
        $Gamma tack #grammar[n] : #grammar[int]$
      )),
      prooftree(rule(
        $Gamma tack #grammar[<(f)string literal>] : #grammar[string]$
      )),
      prooftree(rule(
        $Gamma tack #grammar[<[ html code ]>] : #grammar[html]$
      ))
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        $Gamma tack #grammar[e] : alpha$,
        $Gamma tack #grammar[e'] : beta$,
        $Gamma tack #grammar[(e, e')] : alpha times beta$
      )),
      prooftree(rule(
        $Gamma tack #grammar[fst] : alpha times beta -> alpha$
      )),
      prooftree(rule(
        $Gamma tack #grammar[snd e] : alpha times beta -> beta$
      )),
      prooftree(rule(
        $Gamma tack #grammar[()] : #grammar[unit]$
      )),
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        $Gamma tack #grammar[sqlite_opendb] : #grammar[string -> db]$
      )),
      prooftree(rule(
        $Gamma tack #grammar[sqlite_closedb] : #grammar[db -> bool]$
      )),
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        $Gamma tack #grammar[sqlite_exec] : #grammar[db -> (html -> html -> html) -> (html -> string -> string -> html) -> bool]$
      )),
    ),
  )
)

= Program semantics

== Values

#grammar[
values: v, v', ... ::= #sym.chevron.l\E, #extern_word[function]#sym.chevron.r | n | true | false | #extern_word[string_literal] | (v, v') | <vdb> | pure_html_code | #embed_page[evald_page]
]

#grammar[function: fun x -> e | fixfun x -> e]

#grammar[evald_page: [v1; v2; ...; vn]] #emoji.warning it's _not_ #mllike list.

#grammar[vdb: a value representing a database in the language]

An evaluated webpage can be injected in a value (via the frame). 

#example[
  This happens when we evaluate
  #grammar[<{ <[htmlcode <{let x = 1 in x}> somemorehtmlcode]> }>]
]

== Evaluation rules

A dynamic webpage to evaluate is seen as a list of either:
- Pure html code ;
- An expression ;
- A global declaration.

The interpreter evaluates following a big-step call-by-value semantics.

=== Expression

==== Functions 

Functions are values; when they evaluate the variables needed to their evaluation are automatically captured. Functions can be passed as argument to other functions.

To write recursive functions, you must use #grammar[fixfun]; it binds two variables: the first one is the name of the function (for the recursive call), the second one is the name of the argument.

#example[
  The following code represents the factorial function:

  `fixfun fact n -> if n = 0 then 1 else n * fact (n - 1)`

  #emoji.warning if you declare a function, you mustn't mistake the name of the resulting function in the let-binding and the name used for recursing.
  For instance:
  ```
  let fact = fixfun fact n -> if n = 0 then 1 else n * fact (n-1)
  let fact = fixfun f n -> if n = 0 then 1 else n * f (n-1)
  ```
  both defines `fact` as the factorial function. Although:
  ```
  let fact = fixfun f n -> if n = 0 then 1 else n * fact (n-1)
  ```
  is incorrect, and will raise an error because `fact` is not defined when used here.
]

There is currently no support for mutually recursive functions.

==== Declarations

A declaration can either be _global_ i.e. its scope is the remainder of the file or _local_ i.e. its scope is restricted to the expression after the `in` keyword.

Furthermore, global session declarations have a specific semantics: `Session.let x = e` will firstly have the same effect as `let x = e`, except:
- the bound variable is stored within the `Session` module i.e. to access it somewhere else, we need to write `Session.x`;
- it defines a _session variables_ i.e. it will be accessible across all the pages from now on, for the duration of the session #link("https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/Cookies")[(see bullet point Session Cookie)].

==== Operators

The language provides some operators on basic datatypes:
- And/or/not on booleans: `&&`, `||`, `not`.
- Addition, multiplication, substraction, division, exponentiaion: `+`, `*`, `-` ,`/` ,`^`. The unary version of `-` is also available.
- comparison operators less than, greater than, less or equal than, greater or equal than, equal, not equal: `<`, `>`, `<=`, `>=`, `=`, `<>`. These are _polymorphic_ i.e. they typecheck for any two expressions, provided that they are of the same type. It will fail at execution and raise an error on types for which it's not implemented. Comparison is currently implemented only for basic types: integers, booleans, strings and _content_ i.e. #mllike code evaluated to HTML.
- Concatenation of two strings: `++`.

==== Predefined functions

===== Projections

To get the first or the second element of a _pair_, `fst` and `snd` are pre-defined.

===== Arguments from the command line

You can have pre-defined values before even starting to evaluate the page.

To do that, you can pass them directly in the command line:
```
Usage: <program> <source> <dest> [<argoption> <arguments>]*
  <source>: can either be a path to a file, or -stdin.
  <dest>: can either be a path to a file, or -stdout.
  You can pass argument to the program to pre-load a variable environment before evaluating the dynamic webpage.
  <argoption>: the way the program should interpret the arguments immediatly following this flag. Can be any of the following options:
    -argrepr: interpret each value following the built-in repr function. This allows to pass argument of any type.
    -argstr: interpret each value as a string. 
  <arguments>: A list of bindings, of th following form: METHOD&name1=arg1&...&namen=argn.
    These variable will be available in the dynamic webpage within the namespace Method.
```

This will create a module containing all the defined variables.

#example[
  `<program> file -stdout -argstr MODULE&x=astring&y=anotherstring` will evaluate `file`, but if `file` uses variable `Module.x` (resp. `Module.y`), it will be globally defined and its value will be `"astring"` (resp. `"anotherstring"`).
]

===== Database operations

Three built-in functions are provided to interact with databases using #link("https://sqlite.org/")[SQLite] via the #link("https://mmottl.github.io/sqlite3-ocaml/")[sqlite3 ocaml library].

One function opens a database, one to close one and one to perform SQL on them.
All these functions are available in the module `Sqlite`.

`Sqlite.open "path/to/adb"` evaluates to a value representing the database at path `path/to/db`, with which we can now interact using `exec`.

`Sqlite.exec vdb fl fc query = processed_table` where `vdb` is a value obtained from `open`.
`query` is a string corresponding to a SQL query, which is executed on the database `vdb`.
If it has query statements, then `sqlite_exec` allow to process the resulting table with a double _left-folding_ function applied on the resulting table i.e. let the resulting table be:
#align(center,
  grid(columns:(1fr,.1fr,1fr,.1fr,1fr),
    table(columns:3,
      [header_1], [...], [header_n],
      table.hline(stroke: 2pt),
      [data_1_1], [...], [data_1_n],  
      align(center, sym.dots.v), align(center, sym.dots.v), align(center, sym.dots.v),  
      [data_n_1], [...], [data_n_n],  
    ),
    align(center + horizon, sym.arrow.squiggly),
    table(columns:1,
      [headers],
      table.hline(stroke: 2pt),
      [processed_line_1],  
      align(center, sym.dots.v),
      [processed_line_n],
    ),
    align(center + horizon, sym.arrow.squiggly),
    table(columns:1,
      [headers],
      table.hline(stroke: 2pt),
      [processed_table],
    ),
  )
)

Firstly, `fc` is used to _fold_ each line into a single HTML value `processed_line_i`. More precisely,

`processed_line_i = fc (... (fc EmptyHtmlCode header_1 data_i_1) ...) header_n data_i_n`.

And now, we then again fold the resulting column into a single HTML value, using the `fl` function:

`processed table = fl (... (fl EmptyHtmlCode line_1) ...) line_n`.

#example[
  If you want to get the resulting relation of SQL query `query` on `vdb` as a HTML table, you can write the following code:
  ```
  <table>
  <{
    Sqlite.exec vdb
      (fun acc -> fun new_line -> <[ <{acc}> <tr> <{new_line}> </tr> ]>)
      begin fun acc -> fun hd -> fun content ->
        <[
          <{ acc }>
          <td> <{content}> </td>
        ]>
      end query
  }>
  </table>
  ```
]

`Sqlite.close vdb` closes the databases `vdb`. Returns a boolean telling whether the database could be closed. If `false`, the the database _remains open_. See #link("https://mmottl.github.io/sqlite3-ocaml/api/sqlite3/Sqlite3/index.html#general-database-operations")[here] for reason the database couldn't be closed, and more specifications of the sqlite functions.

== Namespacing, modules and session variables

At some point, we would like to implement modules contained inside another module, maybe even functors if possible, and records types. When this is done, a full variable name will be `((modulevar.)*(expr.)*varname)` where each `expr` must be of type record.

For instance, if module A contains module B which itself contains a record `r : {r' : rec'}` where `rec'` itself is a record with a field `x`, then `A.B.r.r'.x` designates the field `x` of the farthest nested record.

A modular typing (resp. evaluation) environment therefore becomes a tree, where each edge is labelled with a module name, and each node is labelled with a typing (resp. evaluation) environment.

#grid(columns:2,
  tidy-tree-graph(compact: true)[
    - `x : int, y : string`
      + `Sqlite`
      - `open_db : string -> db, ...`
      + `A`
      - `x : string * int`
        + `B`
        - `y : string * string * string`
  ],
  tidy-tree-graph(compact: true)[
    - `x` #sym.mapsto `1`, `y` #sym.mapsto `"hey"`
      + `Sqlite`
      - `open_db` #sym.mapsto `something_outside_mllike`
      + `A`
      - `x` #sym.mapsto `("in A", 3)`
        + `B`
        - `x` #sym.mapsto `("in B", " and ", "fine")`
  ]
)

#pagebreak()

= TODO

#sym.ballot Implement escape characters in strings

#sym.ballot Implement fstrings

#sym.ballot.check Implement percent-encoding 

#sym.ballot Don't lex ml located in html comment

#sym.ballot Add comments within ML

#sym.ballot.check Allow HTML brackets to contain any dynpage e.g. <[somehtml <{"coucou"}> somemorehtml]>

#sym.ballot Add syntactic sugar for multiple variables functions.

#sym.ballot Add t-uples

#sym.ballot Add pattern-matching

#sym.ballot.check Add superglobal variables

#sym.ballot.check Add user-defined global variables

#sym.ballot At some point, we would like to implement modules contained inside another module, and records types.

#sym.ballot Add user-defined types

#sym.ballot Once it's done, implement basic types such as list directly within the language.

#sym.ballot Allow type annotations from the user

#sym.ballot Allow importing other ml files (as modules ?)

#sym.ballot Keep line number information on parsed term for better typing error messages (?)