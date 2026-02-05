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

= Language Syntax

== Webpage

We first have the syntax of the body of the webpage ```<body>...</body>```, 

```
bodyexp: anyHtmlCode> (<{exp}> anyHtmlCode)*
```

== Expressions

#columns(3)[
=== General expressions:

```
exp: e, e', e'', ... ::=
    let <id> = e in e'
  | fun <id> -> e
  | fixfun <id> <id> -> e
  | e e'
  | if e then e' else e''
  | e;e'
  | <id>
  | <aexp> | <bexp> | <sexp>
  | <texp>
  | <uexp>
  | <html> | <dbexp>
  | (e)
  | begin e end
```

#colbreak()

=== Arithmetic expressions:

```
aexp:
    <exp> + <exp>
  | <exp> - <exp>
  | <exp> * <exp>
  | <exp> / <exp>
  | <exp> ^ <exp>
  | <int literal>
```

#colbreak()

=== Boolean expressions:

```
bexp:
    <exp> < <exp>
  | <exp> > <exp>
  | <exp> <= <exp>
  | <exp> >= <exp>
  | <exp> = <exp>
  | <exp> <> <exp>
  | <exp> && <exp>
  | <exp> || <exp>
  | not <exp>
  | <boolean literal>
```
]

String expressions:

```
sexp: <exp> ++ <exp> | <fstring literal>
```

Tuple expressions:

```
texp: fst <exp> | snd <exp> | <exp>, <exp>
```

Unit expression:

```
uexp: ()
``` #todo

HTML:

```
html: <[ anyHtmlCode ]>
```

Database expression:

```
dbexp: sqlite_opendb | sqlite_closedb | sqlite_exec
```

For the time being, only couples are allowed, and `(x1, x2, x3, x4)` is parsed as `(x1, (x2, (x3, x4)))`.

== Identifiers (variable and function names)

```
(_|[a-z])(_|'|[0-9]|[a-z]|[A-Z])*
```

Examples:
- variable
- my\_function_n_362
- \_MyFunction
- myVariable067

But not:
- MyFunction
- 01var

== Literals

=== Integers

For better readability for the programmer, we allow underscores in numbers.

`[0-9]([0-9]|_)*`

Examples:
- 123
- 100\_000
- 1\_2\_\_\_\_\_3\_\_\_\_

=== Strings

Strings are delimited by quotes: `"..."`.

=== Format strings

Format strings are delimited by: `f"..."`. A formatter can be inserted in a format string with `%(value)` #todo not implemented _yet_.

=== Booleans

`true`, `false`

#pagebreak()

= Namespacing

Modules' names starts with a capital letter but otherwise are the same as variable names.

Variables are namespaced, more precisely:

A variable is a variable name preceded by the module where it's defined e.g. `Sqlite.exec`.

At some point, we would like to implement modules contained inside another module, maybe even functors if possible, and records types. When this is done, a full variable name will be `((modulevar.)*(expr.)*varname)` where each `expr` must be of type record.

For instance, if module A contains module B which itself contains a record `r : {r' : rec'}` where `rec'` itself is a record with a field `x`, then `A.B.r.r'.x` designate the field `x` of the farthest nested record.

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
#remark[
  For now, user declarated modules are not implemented, thus the only modules are Get, Post, and Sqlite.
]


= Type system

== Types

`<tlit> : int | bool | string | unit | html | db`

$alpha, beta, ...$` ::= `$alpha -> beta$` | `$alpha times beta$

== Typing rules

#align(center, stack(spacing: 1em,
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        $Gamma tack #[`e`] : alpha$,
        $Gamma, #[`x`] : alpha tack #[`e'`] : beta$,
        $Gamma tack #[`let x = e in e'`] : beta$
      )),
      prooftree(rule(
        $Gamma, #[`x`] : alpha tack #[`e`] : beta$,
        $Gamma tack #[`fun x -> e`] : alpha -> beta$
      )),
      prooftree(rule(
        $Gamma, #[f] : alpha -> beta, #[`x`] : alpha tack #[`e`] : beta$,
        $Gamma tack #[`fixfun f x -> e`] : alpha -> beta$
      ))
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        $Gamma tack #[`e`] : alpha -> beta$,
        $Gamma tack #[`e'`] : alpha$,
        $Gamma tack #[`e e'`] : beta$
      )),
      prooftree(rule(
        $Gamma tack #[`e`] : #[`bool`]$,
        $Gamma tack #[`e'`] : alpha$,
        $Gamma tack #[`e''`] : alpha$,
        $Gamma tack #[`if e then e' else e''`] : alpha$
      )),
      prooftree(rule(
        $Gamma tack #[`e`] : #[`unit`]$,
        $Gamma tack #[`e'`] : alpha$,
        $Gamma tack #[`e;e'`] : alpha$
      ))
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(label: [#sym.ast.op.o: `+, -, *, /,` or `^`],
        $Gamma tack #[`e`] : #[`int`]$,
        $Gamma tack #[`e'`] : #[`int`]$,
        $Gamma tack #[`e `#sym.ast.op.o` e'`] : #[`int`]$
      )),
      prooftree(rule(label: [#sym.ast.op.o: `>, <, >=, <=, =` or `<>`],
        $Gamma tack #[`e`] : alpha$,
        $Gamma tack #[`e'`] : alpha$,
        $Gamma tack #[`e `#sym.ast.op.o` e'`] : #[`bool`]$
      ))
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(label: [#sym.ast.op.o: `&&` or `||`],
        $Gamma tack #[`e`] : #[`bool`]$,
        $Gamma tack #[`e'`] : #[`bool`]$,
        $Gamma tack #[`e `#sym.ast.op.o` e'`] : #[`bool`]$
      )),
      prooftree(rule(
        $Gamma tack #[`e`] : #[`bool`]$,
        $Gamma tack #[`not e`] : #[`bool`]$
      )),
      prooftree(rule(label: $Gamma(x) = alpha$,
        $Gamma tack x : alpha$
      ))
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        $Gamma tack #[`b`] : #[`bool`]$
      )),
      prooftree(rule(
        $Gamma tack #[`n`] : #[`int`]$
      )),
      prooftree(rule(
        $Gamma tack #[`<(f)string literal>`] : #[`string`]$
      )),
      prooftree(rule(
        $Gamma tack #[`<[ html code ]>`] : #[`html`]$
      ))
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        $Gamma tack #[`e`] : alpha$,
        $Gamma tack #[`e'`] : beta$,
        $Gamma tack #[`(e, e')`] : alpha times beta$
      )),
      prooftree(rule(
        $Gamma tack #[`fst`] : alpha times beta -> alpha$
      )),
      prooftree(rule(
        $Gamma tack #[`snd e`] : alpha times beta -> beta$
      )),
      prooftree(rule(
        $Gamma tack #[`()`] : #[`unit`]$
      )),
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        $Gamma tack #[`sqlite_opendb`] : #[`string`] -> #[`db`]$
      )),
      prooftree(rule(
        $Gamma tack #[`sqlite_closedb`] : #[`db`] -> #[`bool`]$
      )),
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        $Gamma tack #[`sqlite_exec`] : #[`db`] -> (#[`html`] -> #[`html`] -> #[`html`]) -> (#[`html`] -> #[`string`] -> #[`string`] -> #[`html`]) -> #[`bool`]$
      )),
    ),
  )
)

= Program semantics

== Values

`
values: v, v', ... ::= `#sym.chevron.l`E, <function>`#sym.chevron.r` | n | true | false | <string literal> | (v, v') | <vdb> | pure_html_code |` #embed_page[`evald_page`]

`function: fun x -> e | fixfun x -> e`

`evald_page: [v1; v2; ...; vn]` #emoji.warning it's a list in the meta-language.

`vdb: a value representing a database in the language`

An evaluated webpage can be injected in a value (via the frame). This happens when we evaluate, e.g.

`<{ <[htmlcode <{let x = 1 in x}> somemorehtmlcode]> }>`

== Evaluation rules

A dynamic webpage to evaluate is seen as a list of either:
- Pure html code ;
- An expression ;
- A global declaration.

The top-level interpreted page is a dynamic webpage, as well as the content between HTML opening/closing bracket.

The interpreter evaluates following a big-step call-by-value semantics. We define two mutually recursive relations to evaluate expressions and dynamic webpages.

#let pageEvaluatesTo(env, expr, val) = $#env tack #expr arrow.triple.b #val$
#let evaluatesTo(env, expr, val) = $#env tack #expr arrow.double.b #val$

=== Expression

#align(center, stack(spacing: 1em,
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        evaluatesTo(`E`, `n`, `n`)
      )),
      prooftree(rule(
        evaluatesTo(`E`, `true`, `true`)
      )),
      prooftree(rule(
        evaluatesTo(`E`, `false`, `false`)
      )),
      prooftree(rule(
        evaluatesTo(`E`, `"<string>"`, `"<string>"`)
      )),
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        evaluatesTo(`E`, `fun x -> e`, [#sym.chevron.l `E`, `fixfun f x -> e` #sym.chevron.r])
      )),
      prooftree(rule(
        evaluatesTo(`E`, `fixfun f x -> e`, [#sym.chevron.l `E`, `fun x -> e` #sym.chevron.r])
      )),
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        evaluatesTo(`E`, `e`, `v`),
        evaluatesTo(`E`, `e'`, `v'`),
        evaluatesTo(`E`, `(e, e')`, `(v, v')`)
      )),
      prooftree(rule(
        evaluatesTo(`E`, `e`, $n$),
        evaluatesTo(`E`, `e'`, $m$),
        evaluatesTo(`E`, [`e` #sym.ast.op.o `e'`], [$n overline(ast.op.o) n'$])
      )),
      prooftree(rule(
        evaluatesTo(`E`, `e`, `n`),
        evaluatesTo(`E`, `-e`, $-n$)
      )),
      prooftree(rule(
        evaluatesTo(`E`, `e`, `b`),
        evaluatesTo(`E`, `e'`, `b'`),
        evaluatesTo(`E`, [`e` #sym.ast.op.o `e'`], $b overline(ast.op.o) b'$)
      )),
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        evaluatesTo(`E`, `e`, `b`),
        evaluatesTo(`E`, `not e`, $not b$)
      )),
      prooftree(rule(
        evaluatesTo(`E`, `e`, `s`),
        evaluatesTo(`E`, `e'`, `s'`),
        evaluatesTo(`E`, [`e` #sym.plus.double `e'`], [$s$ #overline[#sym.plus.double] $s'$])
      )),
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        evaluatesTo(`E`, `e`, [#sym.chevron.l `E'`, `fun x -> e_f` #sym.chevron.r]),
        evaluatesTo(`E`, `e'`, `v`),
        evaluatesTo([`E', x`#sym.mapsto`v`], `e_f`, `v'`),
        evaluatesTo(`E`, [`e e'`], `v'`)
      )),
      prooftree(rule(
        evaluatesTo(`E`, `e'`, `v'`),
        evaluatesTo([`E, x`#sym.mapsto`v'`], `e'`, `v`),
        evaluatesTo(`E`, [`let x = e in e'`], `v`)
      )),
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        evaluatesTo(`E`, `e`, [#sym.chevron `E', fixfun f x -> e_f` #sym.chevron.r]),
        evaluatesTo(`E`, `e'`, `v`),
        evaluatesTo([`E, f`#sym.mapsto`fixfun f x -> e_f, x`#sym.mapsto`v`], `e_f`, `v'`),
        evaluatesTo(`E`, [`e e'`], `v'`)
      )),
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        evaluatesTo(`E`, `e`, `v`),
        evaluatesTo(`E`, `e'`, `v'`),
        evaluatesTo(`E`, [`e;e'`], `v'`)
      )),
      prooftree(rule(
        evaluatesTo(`E`, `e`, `true`),
        evaluatesTo(`E`, `e'`, `v'`),
        evaluatesTo(`E`, [`if e then e' else e''`], `v'`)
      )),
      prooftree(rule(
        evaluatesTo(`E`, `e`, `false`),
        evaluatesTo(`E`, `e''`, `v''`),
        evaluatesTo(`E`, [`if e then e' else e''`], `v''`)
      )),
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        evaluatesTo(`E`, `e`, `(v, v')`),
        evaluatesTo(`E`, [`fst e`], `v`)
      )),
      prooftree(rule(
        evaluatesTo(`E`, `e`, `(v, v')`),
        evaluatesTo(`E`, [`snd e`], `v'`)
      )),
      prooftree(rule(label: `E(x) = v`,
        evaluatesTo(`E`, `e`, `x`),
        evaluatesTo(`E`, [`e`], `v`)
      )),
      prooftree(rule(
        pageEvaluatesTo(`E`, `dynamic_webpage`, `evald_page`),
        evaluatesTo(`E`, `<[ dynamic_webpage ]>`, embed_page_math[`evald_page`])
      )),
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(label: [`vdb` is a projection within the langhage of the db at path `s`],
        evaluatesTo(`E`, `e`, `s`),
        evaluatesTo(`E`, [`sqlite_opendb e`], `vdb`)
      )),
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(label: [If the corresponding db could be closed],
        evaluatesTo(`E`, `e`, `vdb`),
        evaluatesTo(`E`, [`sqlite_closedb e`], `true`)
      )),
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(label: [If the corresponding db couldn't be closed (it remains oen)],
        evaluatesTo(`E`, `e`, `vdb`),
        evaluatesTo(`E`, [`sqlite_closedb e`], `true`)
      )),
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        evaluatesTo(`E`, `e`, `vdb`),
        evaluatesTo(`E`, [a closure for function `fl`], `vdb`),
        evaluatesTo(`E`, [a closure for function `fc`], `vdb`),
        evaluatesTo(`E`, `e'`, `s`),
        evaluatesTo(`E`, [`sqlite_exec e f1 f2 e'`], `processed s*`)
      )),
    ),
  )
)

\*The semantic of exec is as follows: `s` is a string corresponding to a SQL command, which is executed on the database `vdb`.
If it has query statements, then `sqlite_exec` allow to process the result with a double fold left function applied on the resulting table i.e. let:
#table(columns:3,
  [header_1], [...], [header_n],
  [data_1], [...], [data_n],  
)
be a line of the resulting table of the query.
We first allow to process one line in the following manner: the value `line_i` corresponding to the table above is: `fc (... (fc EmptyHtmlCode header_1 data_1) ...) header_n data_n`.

Similarly, the result of each line is combined in the followind manner to form `processed s`:\
`processed s = fl (... (fl EmptyHtmlCode line_1) ...) line_n`.

Actually, `fl` and `fc` can't be any functions for weird reasons. #todo explain or fix + see if it leads to actual limitations.

See #link("https://mmottl.github.io/sqlite3-ocaml/api/sqlite3/Sqlite3/index.html#general-database-operations")[here] for reason the db couldn't be closed, and more specifications of the sqlite functions.

=== Dynamic webpage


#align(center, stack(spacing: 1em,
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        evaluatesTo(`E`, `e`, `v`),
        pageEvaluatesTo([`E, x` #sym.mapsto `v`], `page`, `evald`),
        pageEvaluatesTo(`E`, `(let x = e) :: page`, `evald`)
      )),
      prooftree(rule(
        pageEvaluatesTo(`E`, `page`, `evald`),
        pageEvaluatesTo(`E`, `pure_html :: page`, `pure_html :: evald`)
      )),
    ),
    stack(dir:ltr, spacing: 1em,
      prooftree(rule(
        evaluatesTo(`E`, `e`, `v`),
        pageEvaluatesTo([`E`], `page`, `evald`),
        pageEvaluatesTo(`E`, `e :: page`, `v :: evald`)
      )),
    ),
  )
)

#todo write documentation for session variables

#pagebreak()

= TODO

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