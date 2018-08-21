## OCaml Programming Patterns

### Purpose

This package contains some random programming tricks, "design patterns",
and other helpful or at least inspiring ideas of achieving a high level of
abstraction in OCaml programs that I have come across over time.  Some may
demonstrate how to implement concepts of more or less theoretical interest
(e.g. arrows, monads), others show more practical hints on how to structure
code to make it more reusable (e.g. abstract lexers, extensible ASTs).

### Contents

The package currently contains the following:

  * [Abstract Lexer](#abstract-lexer)
  * [Extensible ASTs](#extensible-asts)
  * [Arrows](#arrows)
  * [Union find](#union-find)

#### Abstract Lexer

##### Quick introduction to syntactic analysis

The first step in the process of compilation or interpretation of computer
programs or other formal languages is typically lexical and syntactic analysis,
or in other terms:

  1. Lexing
  2. Parsing

The process of lexing transforms a stream of _characters_ (e.g. ANSI,
Unicode, etc.) to a stream of _tokens_, thus providing a more accessible
representation of the elements in the input.  This step might, for example,
identify keywords, numbers, operators, etc.  The process of parsing assigns
a grammatical structure to these elements, thus grouping them in ways that
allow us to interpret the input more easily.

##### Purpose of the abstract lexer

The purpose of the _abstract lexer_ is to fully separate steps one and two
above when using `ocamllex` to generate lexers.

Programmers typically implement lexers such that they generate a token
stream for a particular parser.  This is usually all they need so there is
no problem with that.  But sometimes requirements change and we may want to
use different parsers with the same lexer.

For example, the requirement for one parser might be optimum efficiency.
It may not want to deal with comments in an input file and rather ignore those
already during lexing.  We may want to avoid having to implement grammar
rules that take into account comment syntax in that case.  Other parsers,
however, might want to keep comments, for example to pretty-print transformed
input without losing this valuable information.

The _abstract lexer_ achieves this separation by wrapping lexers generated by
`ocamllex` into a functor that abstracts over the types of values ("tokens")
returned by the lexer.

A lexer specification usually consists of several _rules_ (functions).
These functions take the current state of the lexer, which specifies the
position in the input stream, and try to match one ore more _patterns_
(regular expressions) at the current location in the input stream.  If a
pattern matches, an associated action will be executed.

Instead of returning a specific parser token from within an action, which
would be the usual thing to do, abstract lexers call a function in the
functor argument and pass it whatever lexeme (or relevant part of a lexeme)
the lexer has just identified.  This function may then return a parser token
for whatever parser it is intended for.

Sometimes lexer rules may also call other lexer rules recursively.  In the
abstract lexer design, however, we never call other rules explicitly.
There is hence no explicit recursion.  This is important, because some
parsers may want to just let the lexer continue matching further input rather
than return a token, whereas others might want to see a token to relate it
grammatically to others.

##### Example implementation

The `abstract_lexer` directory contains the following files:

  * `lexer.mll`
  * `lexers.ml`
  * `abstract_lexer.ml`
  * `test.dat`

The `ocamllex` file `lexer.mll` demonstrates how to wrap a lexer into a
functor.  The signature of the functor argument is `Spec`.  This specification
introduces a module for each rule in the lexer (e.g. `Any_char`) containing
an abstract type `t`.  All rule actions have to return the same type anyway,
and here this type is completely abstract rather than a particular type of
parser tokens.

Now we introduce a function for each pattern action,
e.g. `Any_char.handle_char`.  It has to take the current `lexbuf` as argument
so that an instance of the lexer can extract additional lexeme information
(e.g. location information if required), or to allow recursive calls to
other lexer rules.  We may often want to also pass additional arguments,
e.g. particular parts of the lexeme that we have already extracted.  This is
useful if, for example, we attach identifiers to sub-patterns in the lexer
rule.

The functor in `lexer.mll` is introduced in the header part of the lexer
specification and closed in the trailer, thus wrapping the automatically
generated lexer code into its body.

An example instance of this lexer is given in file `lexers.ml`.  It is called
`Lexers.Alternating` and demonstrates how to specify recursive lexer rules.
This is achieved by making the module `Alternating` itself recursive.

The file `abstract_lexer.ml` will start lexing from standard input with rule
`Lexers.Alternating.any_char`.  Valid example input can be found in
file `test.dat`.  You can compile and test the example by going to the
`abstract_lexer` directory and executing:

```sh
dune exec ./abstract_lexer.exe < test.dat
```

##### Fazit

It seems recommendable to write new lexers in an abstract style as demonstrated
above.  This will allow you to completely and cleanly separate the stages
of lexical and syntactic analysis.  If, for example, future requirements
ask for a new parser, you won't have to pollute old parser specifications
with new tokens and dummy rules.

The performance impact of this abstraction will generally be neglible,
assuming the lexer is well-written.  This requires that as much work as
possible is assigned to the lexing engine rather than to pattern actions.
E.g. rules containing a pattern that matches a single character and which
are called recursively to handle input in this piece-wise fashion should be
rewritten to match one complex pattern and perform one action only instead.
This will generally give a great boost to lexer performance, especially if
it is abstract.

#### Extensible ASTs

This simple example shows how to implement extensible abstract syntax trees
(ASTs).  It uses polymorphic variants to achieve open recursion and to easily
compose multiple recursive "languages".

See the file `ast.ml` in directory `extensible_ast`, which you can compile
and run as follows:

```sh
dune exec ./ast.exe
```

#### Arrows

This project in directory `arrows` mostly translates the Haskell-code
presented in the following paper to OCaml:

    Generalising Monads to Arrows
    John Hughes
    Science of Computer Programming 37, pp67-111, May 2000

The project contains the following files:

  * `arrow.mli` and `arrow.ml`
  * `arr.ml`

The module `Arrow` has a fully documented API and provides several simple
implementations for arrows, which can be extended to arrows with more
convenience functions.  The signature of simple arrows specifies the type
of arrows and the following functions:

  * `arr` - creates arrows from ordinary functions
  * `>>>` - the arrow composition operator
  * `app` - arrow application
  * `run` - a function to "chase arrows"

The `arr`-function and composition operator are at the core of arrows, but
are not sufficient to give them the same power as e.g. monads.  Adding arrow
application restores this power and allows us to enrich them with numerous
other functions that provide useful programming idioms, e.g. for dealing
with tuples or choice.  Please refer to the above-mentioned paper for details.

The simplest arrow implementation in module `SimpleArrow` just uses ordinary
functions as representation of arrows.  It suffers from stack overflows
if arrow composition is nested too deeply.  The module `SimpleContArrow`
fixes this problem by representing arrows with continuations.  Module
`SimpleDataContArrow` uses sum tags for representing the structure of arrows
and their compositions.  It also uses continuations to avoid stack overflows.

The functor `MkArrow` takes a simple arrow and enriches it with more
functions as described in John Hughes' paper.  Module `Arrow` finally also
implements monads by showing how we can obtain one from an arrow supporting
arrow application and vice versa, thus proving their equivalence in terms
of expressive power.

#### Union find

The example in directory `union_find` demonstrates how to implement the
union-find algorithm.  The example uses _Generalized Algebraic Datatypes_
(GADTs) together with _mutable inline records_ to implement highly efficient
datastructures with fewer indirections and a smaller memory footprint than
usual records and algebraic datatypes would allow.  You will need at least
OCaml 4.04 to unbox away unnecessary data.

You can build and run the code using:

```sh
dune exec ./test_union_find.exe
```

### Contact Information and Contributing

Please submit bugs reports, feature requests, contributions and similar to
the [GitHub issue tracker](https://github.com/mmottl/ocaml-prog-pats/issues).

Up-to-date information is available at:
<https://mmottl.github.io/ocaml-prog-pats>
