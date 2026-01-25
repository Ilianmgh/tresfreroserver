#import "@preview/curryst:0.6.0" : *

= Language Syntax

== Webpage

We first have the syntax of the body of the webpage ```<body>...</body>```, 

```
bodyexp: <any html code> | <any html code> <{exp}> <bodyexp>
```

Example:

```html
<body>
  <h1>Exampel</h1>
  <{
    let x = 1 in
    if x = 2 then
  }>
    2
  <{
    else
  }>
    what?
  <{}>
</body> 
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
html: }> any html code <{
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

`<tlit> ::= int | bool | string | unit | html`

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
        $Gamma tack #[`<string literal>`] : #[`string`]$
      )),
      prooftree(rule(
        $Gamma tack #[`<fstring literal>`] : #[`string`]$
      ))
    ),
  )
)

= TODO

#sym.ballot c.f. example at the beginning of the document, we want to be able to consider `if b then }> ... <{ else }> ...` as a valid if-then-else, although `<{f}> ...` shouldn't be understood as the application of f to something... Or should it ? Investigate.

#sym.ballot Add syntactic sugar for multiple variables functions.

#sym.ballot Add t-uples

#sym.ballot Add pattern-matching

#sym.ballot Add global from outside the function variables

#sym.ballot Add user-defined global variables

#sym.ballot Add user-defined types

#sym.ballot Once it's done, implement basic types such as list directly within the language.

#sym.ballot Maybe revisit sequence's semantics. We may want <{"\<tag>";"hey</tag>"}> to produce the html code ```html <tag>hey</tag> ```.
