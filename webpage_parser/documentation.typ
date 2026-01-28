#import "@preview/curryst:0.6.0" : *

= Language Syntax

== Webpage

We first have the syntax of the body of the webpage ```<body>...</body>```, 

```
bodyexp: anyHtmlCode> (<{exp}> anyHtmlCode)*
```

== Expressions

General expressions:

```
exp: e, e', e'', ... ::=
    let <identifier> = e in e'
  | fun <identifier> -> e
  | fixfun <identifier> <identifier> -> e
  | e e'
  | if e then e' else e''
  | e;e'
  | <identifier>
  | <aexp>
  | <bexp>
  | <sexp>
  | <texp>
  | <uexp>
  | <html>
  | (e)
  | begin e end
```

Arithmetic expressions:

```
aexp:
    <exp> + <exp>
  | <exp> - <exp>
  | <exp> * <exp>
  | <exp> / <exp>
  | <exp> ^ <exp>
  | <int literal>
```

Boolean expressions:

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
```

HTML:

```
html: <[ anyHtmlCode ]>
```

For now, only couples are allowed, and `(x1, x2, x3, x4)` is parsed as `(x1, (x2, (x3, x4)))`.

== Identifiers (variable and function names)

```
(_|[a-z])(_|'|[0-9]|[a-z]|[A-Z])*
```

Examples:
- variable
- my\_function
- \_MyFunction
- myVariable

But not:
- MyFunction
- 01var

== Literals

=== Integers

For readability for the programmer, we allow underscores in numbers.

`[0-9]([0-9]|_)*`

Examples:
- 123
- 100\_000
- 1\_2\_\_\_\_\_3\_\_\_\_

=== Strings

Strings are delimited by quotes: `"..."`.

=== Format strings

Format strings are delimited by: `f"..."`. A formatter can be inserted in a format string with `%(value)`

=== Booleans

`true`, `false`

= Type system

== Types

`<tlit> : int | bool | string | unit | html`

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
        $Gamma tack #[`e`] : alpha times beta$,
        $Gamma tack #[`fst e`] : alpha$
      )),
      prooftree(rule(
        $Gamma tack #[`e`] : alpha times beta$,
        $Gamma tack #[`snd e`] : beta$
      )),
      prooftree(rule(
        $Gamma tack #[`()`] : #[`unit`]$
      )),
    ),
  )
)

= Program semantics

== Values

`
values: v, v', ... ::= `#sym.chevron.l`E, <function>`#sym.chevron.r` | n | true | false | <string literal> | (v, v')

function: fun x -> e | fixfun x -> e
`

== Evaluation rules

We implement a big-step call-by-value semantics.

#let evaluatesTo(env, expr, val) = [#env #sym.tack #expr #sym.arrow.double.b #val]

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
        evaluatesTo(`E`, `e`, `n`),
        evaluatesTo(`E`, `e'`, `m`),
        evaluatesTo(`E`, [`e` #sym.ast.op.o `e'`], [$n$ #overline[#sym.ast.op.o] $n'$])
      )),
      prooftree(rule(
        evaluatesTo(`E`, `e`, `n`),
        evaluatesTo(`E`, `-e`, $-n$)
      )),
      prooftree(rule(
        evaluatesTo(`E`, `e`, `b`),
        evaluatesTo(`E`, `e'`, `b'`),
        evaluatesTo(`E`, [`e` #sym.ast.op.o `e'`], [$b$ #overline[#sym.ast.op.o] $b'$])
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
    ),
  )
)

= TODO

#sym.ballot Don't lex ml located in html comment

#sym.ballot Add syntactic sugar for multiple variables functions.

#sym.ballot Add t-uples

#sym.ballot Add pattern-matching

#sym.ballot Add superglobal variables (e.g. given in argument of the interpreter in a yaml format)

#sym.ballot Add user-defined global variables

#sym.ballot Add user-defined types

#sym.ballot Once it's done, implement basic types such as list directly within the language.

#sym.ballot Maybe revisit sequence's semantics. We may want <{"\<tag>";"hey</tag>"}> to produce the html code ```html <tag>hey</tag> ```.

#sym.ballot Allow type annotations from the user

#sym.ballot Allow importing other ml files (as modules ?)

#sym.ballot Keep line number information on parsed term for better typing error messages (?)