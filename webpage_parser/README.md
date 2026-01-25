# Syntax

## Webpage

We first have the syntax of the body of the webpage ```<body>...</body>```, `bodyexp`:

`bodyexp: (<html code>exp<html code>)*`

## Expressions

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

`sexp: <exp> ++ <exp> | <string literal>`

## Identifiers (variable and function names)

`(_|[a-z])(_|'|[0-9]|[a-z]|[A-Z])*`

Examples:
- variable
- my_function
- _MyFunction
- myVariable

But not:
- MyFunction
- 01var

## Literals

### Integers

For readability for the programmer, we allow underscores in numbers.

`[0-9]([0-9]|_)*`

Examples:
- 123
- 100_000
- 1_2_____3____

### Strings

Strings are delimited by quotes: `"..."`.

### Format strings

Format strings are delimited by: `f"..."`. A formatter can be inserted in a format string with `%(value)`

### Booleans

`true`, `false`