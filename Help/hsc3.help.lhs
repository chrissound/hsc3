> import Control.Monad
> import Sound.SC3.ID
> import qualified Sound.SC3.Monadic as M

* Abstract

This document describes the hsc3 haskell
bindings to the supercollider synthesis
server.

The bindings allow haskell to be used
to write unit generator graphs, to control
the supercollider synthesiser interactively
while it is running, and to write scores for
offline rendering.

For detailed introductory materials on
haskell and supercollider, see

  http://haskell.org/
  http://audiosynth.com/

* Questions, Dartmouth, 2002

| What should a computer music language do?
| ...
| Is a specialized computer music language
| even necessary? (McCartney, 2002)

These questions are asked in a paper that
documents a reimplementation of the supercollider
language for real time audio synthesis (McCartney,
1998).

The redesigned system consists of two parts, an
elegant, efficient, and musically neutral real
time audio synthesiser in the music-n family
(Mathews, 1961), and a language interpreter in the
smalltalk family (Goldberg, 1983).

The interpreter and synthesiser communicate using
the open sound control protocol (Wright & Freed,
1997).

Using this model of discrete communicating
processes, the computer music language is relieved
of many onerous tasks.

In part the question is rhetorical, given an
appropriately designed and implemented
synthesiser, the control language need not be
particularly specialised.

* What needs to be done

The requirements are rather minimal.

An open sound control protocol implementation and
a usable notation for server commands.

A unit generator graph protocol implementation and
a usable notation for writing graphs.

For interactive use a suitably responsive run time
system, where suitable is a function of the kind
of work being done.

* Questions, San Dimas, 1965

| (1) What are declarative languages?
| ...
| (4) How can we use them to program?
| (5) How can we implement them?
| (Strachey, in Landin, 1966)

(1) Haskell is a non-strict (Wadler, 1996) and
    purely functional (Sabry, 1993) language, one
    result of many years of research into these
    questions (Hudak et al, 2007).

(4) Computation in haskell is structured using a
    small number of simple type classes;monads
    (Wadler, 1990), applicative functors (McBride
    and Paterson, 2007) & arrows (Hughes, 2000).

(5) The glasgow haskell system includes both an
    optimizing compiler generating efficient
    machine programs and a bytecode generator and
    intepreter for interative use.

In the authors experience the glasgow run-time
system is adequate for real-time control of the
supercollider synthesiser, capable of generating
high density & low latency control streams such as
those required for waveset synthesis etc.

* Types, Unit Generators, Parametric Polymorphism

In haskell polymorphism is provided by type
classes (Wadler & Blott, 1989).

Type class polymorphism is parametric, as distinct
from the ad hoc polymorphism of supercollider
language (Strachey, 1967).

Since unit generators are a sort of numerical
value, we wish to make their representation
amenable to the standard haskell numerical type
classes.

These give signatures such as:

>| (+) :: (Num a) => a -> a -> a

meaning that a value can only be summed with a
value of the same type, and that the resulting
value must also be of the same type.

This implies that the type of a unit generator
must be inclusive, since we wish to combine
constants, control inputs, and actual unit
generators operating at varying rates and with
varying numbers of input and output ports.

This leads us to a representation that is simple
but somewhat uninformative, and delays evaluating
unit generator graph correctness to run-time.

We note that a more rigorous type representation
is possible, either in standard haskell or using
one of the many implemented type system
extensions, and could be layered either above or
below the current representation.

* Multiple channel expansion

The supercollider language implements a very
elegant rule for composing graphs from nodes with
different numbers of channels.  The model is
referred to as multiple channel expansion, a
behaviour that, although it can become confusing
in deeply nested uses, is very intuitive for
simple cases.

The simple type representation of unit generators
allows us to implement the multiple channel
expansion model in much the same way as in the
supercollider language

Unit generators with multiple outputs, such
as pan2, are represented as a specific kind
of unit generator value, an ordered set of
proxies.

We can also write these sets directly using
the 'mce' function.

Multiple channel expansion flows downward
through unit generator graphs.

In the expression below, the frequency input
causes two sinOsc unit generators to be created.

> let {x = mouseX' KR (-1) 1 Linear 0.1
>     ;o1 = pulse AR 440 0.1
>     ;o2 = sinOsc AR (mce [110, 2300]) 0 * 0.1}
> in audition (out 0 (pan2 o1 x 0.1 + o2))

This is turn causes the (*) function to
expand and perform channel matching, that is
to duplicate the right hand side input as
required.

The (+) function is also expanded, since the
left and right hand sides are of equal degree
there is not replication of inputs.

The out function does not expand, since it is
defined to flatten one layer of mce values at
it's second input to support a variable number
of input channels;it would however expand on
mce at the first argument, or nested mce at the
second.

Equal inputs do also push the expansion
downwards, however in complex graphs this
seems occasionally unreliable.

> let f = mce2 440 440
> in audition (out 0 (sinOsc AR f 0 * 0.1))

The expansion is recursive, which can require care.
The graph below has four oscillators, the mceSum
function generates a graph with one 'out' UGen where
the channel layout is [(440,441),(660,661)], without it
there would be two 'out' UGens and the layout would be
[(440,660),(441,661)].

> let {f = mce2 (mce2 440 660) (mce2 441 661)
>     ;o = sinOsc AR f 0 * 0.1}
> in audition (out 0 (mceSum o))

* Multiply add inputs, Haskell Curry, and cloning

The supercollider language provides optional multiply
and add inputs for most unit generator constructors.

Optional arguments do not interact well with the
haskell behaviour of treating functions as monadic.

That is, one way to write the number thirteen is:

> let {sum_squares x y = x * x + y * y
>     ;f = sum_squares 2}
> in f 3

The absent multiply add inputs can in most cases be
simply re-written using (*) and (+).

The expression:

| {Out.ar(0, SinOsc.ar(440, 0, 0.1, 0.05))}.play

is equivalent to:

> audition (out 0 (sinOsc AR 440 0 * 0.1 + 0.05))

However there is a subtle distinction in behaviour
relating to multiple channel expansion.

The supercollider language expression:

| {var a = WhiteNoise.ar([0.1, 0.05])
| ;var b = PinkNoise.ar * [0.1, 0.05]
| ;Out.ar(0, a + b)}.play

describes a graph with two WhiteNoise nodes
and a single PinkNoise node.

We note that this distinction is only relevant
for non-deterministic unit generators.

To write this simple graph in haskell we can use the
clone function.  In this file we import the monadic
noise unit generator functions qualified.

> let f = liftM (* mce [0.1, 0.05])
> in do {a <- f (clone 2 (M.whiteNoise AR))
>       ;b <- f (M.pinkNoise AR)
>       ;audition (out 0 (a + b))}

which is defined in relation to the standard
monad functions replicateM and liftM.

>| clone :: (UId m) => Int -> m UGen -> m UGen
>| clone n u = liftM mce (replicateM n u)

* Multiple Root Graphs

The mrg function, pronounced multiple root graph,
allows us to write unit generator graphs with
multiple sink nodes.

Consider the freeSelf unit generator:

> do {n <- M.dust KR 0.5
>    ;let {a = freeSelf n
>         ;b = out 0 (sinOsc AR 440 0 * 0.1)}
>      in audition (mrg [a, b])}

In order to allow multiple root graphs to be
freely composed we implement a leftmost rule,
whereby the leftmost root need not be a sink
node, in which case the mrg node may be used
as an input node.

Consider a simple ping pong delay filter:

> let ppd s = let {a = localIn 2 AR + mce [s, 0]
>                 ;b = delayN a 0.2 0.2
>                 ;c = mceEdit reverse b * 0.8}
>             in mrg [b, localOut c]
> in do {n <- M.whiteNoise AR
>       ;let s = decay (impulse AR 0.3 0) 0.1 * n * 0.2
>        in audition (out 0 (ppd s))}

* Literals, Overloading, Coercion, Constants

This is a somewhat subtle distinction.  Numeric
literals in haskell are overloaded, not coerced.
The numerical type classes provide two functions:

>| fromInteger :: (Num a) => Integer -> a

and

>| fromRational :: (Fractional a) => Rational -> a

which are implicitly applied to all integer and
rational literals respectively.

It is for this reason that we can write:

> sinOsc AR 440.0 0 * 0.1

but must explicitly construct constants from values
of a concrete numerical type using the constant
function.

> let {f = 440.0 :: Double
>     ;p = 0 :: Int
>     ;a = 0.1 :: Float}
> in sinOsc AR (constant f) (constant p) * (constant a)

The most common case requiring constant annotations
is buffer numbers, which must be provided to unit
generator graphs as values of type 'UGen' and to
the server command constructors as values of type
'Int'.

* Unit generators are comparable

In haskell the Eq and Ord type classes define
equality and ordering operators.

In unit generator graphs these operators have a
somewhat different meaning, and require a different
type signature.

For instance the greater-than operator defines a
unit generator that is zero for sample values
where the comparison fails, and one when it
succeeds.

Since the Ord type gives the signature:

>| (>) :: (Ord a) => a -> a -> Bool

we define a variant with a star suffix, such
that:

> let {x = mouseX' KR 3 45 Exponential 0.1
>     ;t = sinOsc AR x 0 >* 0
>     ;d = envTriangle 0.01 0.1
>     ;e = envGen AR t 1 0 1 DoNothing d
>     ;f = 220 + 880 * (toggleFF t)
>     ;o = sinOsc AR f 0}
> in audition (out 0 (o * e))

is a sequence of low and high tones.

For functions where the signature is
consistent with the meaning of the unit
generator operator we use the haskell name.

>| max :: (Ord a) => a -> a -> a

> let {l = fSinOsc AR 500 0 * 0.25
>     ;r = fSinOsc AR 0.5 0 * 0.23}
> in audition (out 0 (l `max` r))

* Observable Sharing, Pure Noise

The haskell expression:

> let {a = sinOsc AR 440 0
>     ;b = sinOsc AR 440 0
>     ;c = a - b}
> in audition (out 0 c)

denotes a graph that has three nodes: sinOsc, (-)
and out.

  # UGens                     Int 3
  # Synths                    Int 1

The graph constructor, when traversing the
structure denoted by (out 0 c), cannot distinguish
between a and b, they are the same value.

In other words, it is the same graph as if we had
written:

> let {x = sinOsc AR 440 0
>     ;y = x - x}
> in audition (out 0 y)

Expressions with the same notation have the same
value.

This is acceptable for deterministic unit
generators, such as sinOsc, but of course fails
for non-deterministic unit generators such as
whiteNoise, and also for demand rate sources
such as dseq.

In supercollider language, the graph

| {var a = WhiteNoise.ar
| ;var b = WhiteNoise.ar
| ;var c = a - b
| ;Out.ar(0, c * 0.1)}.play

does not describe silence, it describes noise.

We read WhiteNoise.ar as a computation that
constructs a value, not as an expression that
denotes a value.

In procedural languages we are familiar with many
different types of equality.  Scheme has eq?, eqv?
and equal?, supercollider language has == and ===.

| {var a = "x"
| ;var b = "x"
| ;[a == b, a === b]}.value

In a purely functional language expressions denote
values, and equal expressions denote the same
value.  Therefore the graph given by the haskell
expression:

> let {z = 'α'
>     ;n = whiteNoise z
>     ;a = n AR
>     ;b = n AR
>     ;c = a - b}
> in audition (out 0 (c * 0.1))

describes silence.  To describe noise we
would need to distinguish a and b, which can only
be done by providing non-equal identifiers in
place of z.

> let {a = whiteNoise 'a' AR
>     ;b = whiteNoise 'b' AR
>     ;c = a - b}
> in audition (out 0 (c * 0.05))

Note that the whiteNoise function used above & provided
by the module Sound.SC3.ID is a quite different
function to the whiteNoise provided by the module
Sound.SC3.Monadic.  That function has the signature:

>| whiteNoise :: (UId m) => Rate -> m UGen

where the type-class UId is defined as:

>| class (Monad m) => UId m where
>|     generateUId :: m Int

The signature indicates that whiteNoise is a
function from a Rate value to an (m UGen)
value.

Note also for expressions that differ in parts other than the
identifier multiple values are denoted.

> let f = mce [rand 'a' 330 550,rand 'a' 660 990]
> in audition (out 0 (sinOsc AR f 0 * 0.1))

* Non-determinism, monadic structure, do notation

It is quite clear that a value of type (m UGen) is
not of type UGen.

Compare the monadic whiteNoise signature with that
of the deterministic sin oscillator:

>| sinOsc :: Rate -> UGen -> UGen -> UGen

We can write a white noise graph using this
function and the haskell 'do' notation as:

> do {a <- M.whiteNoise AR
>    ;b <- M.whiteNoise AR
>    ;let c = a - b
>      in audition (out 0 (c * 0.05))}

which brings us more or less to the supercollider
language notation, with the exception that there
are two distinct binding notations, one for
computations and one for expressions.

The type system does not allow us to confuse these
two bindings.

The do notation allows us to write expressions
that involve computations using a familiar and
readable right to left binding notation.

The above expression is equal to:

> M.whiteNoise AR >>= \a ->
> M.whiteNoise AR >>= \b ->
> let c = a - b
> in audition (out 0 (c * 0.05))

where (>>=) is the monadic bind function, and (\x
-> y) is the notation for lambda expressions
(ie. for function definition, ie. {|x| y} in
supercollider language).  The signature for bind is:

>| (>>=) :: (Monad m) => m a -> (a -> m b) -> m b

which indicates that the value bound in the
function definition can only be accessed in a
function that produces a value in the same monad.

The audition function has an appropriate
signature:

>| audition :: UGen -> IO ()

since IO is an instance of the UId class.

It is the type of audition that determines the
type of a, the type is inferred so there is no
need to write it.

> let {(|>) = flip (.)
>     ;a >>=* b = a >>= b |> return
>     ;u1 = sinOsc AR 440 0 * 0.1
>     ;u2 = M.pinkNoise AR >>=* (* 0.1)
>     ;u3 = pinkNoise 'α' AR * 1
>     ;u4 = resonz u3 (440 * 4) 0.1
>     ;g = u2 >>=* (+ (u1 + u4))}
> in g >>= out 0 |> audition

* Unsafe unit generator constructors

Haskell provides a mechanism to force values
from the IO monad, unsafePerformIO.

Using this we can write unit generator graphs
that have non-deterministic nodes using only
orindary let binding.

> import System.IO.Unsafe

> let {u = unsafePerformIO
>     ;a = u (M.whiteNoise AR)
>     ;b = u (M.whiteNoise AR)
>     ;c = a - b}
> in audition (out 0 (c * 0.05))

This is hardly more convenient than do notation,
however we can also insert non-determinstic nodes
directly into function arguments.  The package
hsc3-unsafe provides unsafe variant unit generator
constructors.

> let {n = Sound.SC3.UGen.Unsafe.whiteNoise
>     ;x = n AR - n AR}
> in audition (out 0 (x * 0.05))

The above uses the unsafe unit generator functions
provided at Sound.SC3.UGen.Unsafe, and avoids the
lifting operations which, for functions of many
arguments, can be cumbersome.

> let n = M.whiteNoise
> in do {x <- liftM2 (-) (n AR) (n AR)
>       ;audition (out 0 (x * 0.05))}

There also Control.Monad.ap which can be more readable
in some contexts.

> let n = M.whiteNoise
> in do {x <- return (-) `ap` n AR `ap` n AR
>       ;audition (out 0 (x * 0.05))}

* Demand Rate, Sharing Again

Demand rate UGens are similarly not functions only
of their arguments.

In the supercollider language expression below the
left and right channels have different signals,
despite each receiving the same input unit
generator.

| {var a = Dseq([1, 3, 2, 7, 8], 3)
| ;var t = Impulse.kr(5,0)
| ;var f = Demand.kr(t, 0, [a, a]) * 30 + 340
| ;Out.ar(0, SinOsc.ar(f, 0) * 0.1)}.play

The distinction here concerns multiple
reads from a single demand rate source, ie.
it is not that the source is non-deterministic,
it is rather that each read request consumes
the value it reads.

Therefore in haskell demand rate unit generators have
similar constructor functions to non-deterministic
unit generators, in order that we can distinguish:

> do {a <- M.dseq 3 (mce [1, 3, 2, 7, 8])
>    ;let {t = impulse KR 5 0
>         ;f = demand t 0 (mce [a, a]) * 30 + 340}
>      in audition (out 0 (sinOsc AR f 0 * 0.1))}

which is the same graph as given in supercollider
language above, from:

> do {a <- clone 2 (M.dseq 3 (mce [1, 3, 2, 7, 8]))
>    ;let {t = impulse KR 5 0
>         ;f = demand t 0 a * 30 + 340}
>      in audition (out 0 (sinOsc AR f 0 * 0.1))}

which gives an equal sequence of tones in each
channel.

* Composition of graphs, user specified identifiers

udup is a shorthand for writing out an MCE node with
multiple copies of a unit generator graph.  That is it
traverses the UGen graph and increments each user
supplied identifier.

> ugenIds (udup 2 (whiteNoise 'a' AR))

> let n e = whiteNoise e AR * 0.1
> in udup 2 (n 'a') == mce [n 'a',n 'b']

upar is a related function that transforms user
identifiers into 'protected' system identifiers instead
of incrementing them.

> ugenIds (upar 'a' 2 (whiteNoise 'a' AR))

The ID input to upar allows multiple instances to
generate distinct graphs.

> let n = whiteNoise 'a' AR
> in ugenIds (upar 'a' 2 n + upar 'b' 2 n)

ucmp is left to right composition of UGen processing
functions where at each stage user identifiers are
lifted to protected system identifiers.  useq is a
variant that replicates the same function for
composition.

> let f i = allpassN i 0.050 (rand 'a' 0 0.05) 1
> in ugenIds (useq 'a' 4 f (dust 'a' AR 10))

* References

+ A. Goldberg and D. Robson.  Smalltalk-80: The
  language and its implementation.
  Addison-Wesley, Reading, MA, 1983.

+ P. Hudak, J. Hughes, S. P. Jones, and P. Wadler.
  A History of Haskell: being lazy with class.  In
  The Third ACM SIGPLAN History of Programming
  Languages Conference, San Diego, California,
  June 2007.  Association for Computing Machinery.

+ John Hughes. Generalising monads to arrows.
  Sci. Comput. Program., 37(1-3):67-111, 2000.

+ P. Landin.  The next 700 programming languages.
  Communications of the ACM, 9(3):157-164, March
  1966.  Presented at the ACM Programming and
  Pragmatics Conference, August 1965.

+ M. V. Mathews.  An Acoustical Compiler for Music
  and Psychological Stimuli.  AT&T Bell
  Laboratories Technical Journal, 40:677-694,
  1961.

+ C. McBride and R. Paterson.  Applicative
  Programming with Effects.  Journal of Functional
  Programming, 17(4), 2007.

+ J. McCarthy.  Recursive functions of symbolic
  expressions and their computation by machine.
  Communications of the ACM, 3(4):184-195, 1960.

+ J. McCartney.  Continued evolution of the
  SuperCollider real time synthesis environment.
  In Proceedings of the International Computer
  Music Conference, pages 133-136. International
  Computer Music Association, 1998.

+ J. McCartney.  Rethinking the Computer Music
  Language: SuperCollider.  Computer Music
  Journal, 26(4):61-68, 2002.

+ Amr Sabry. What is a Purely Functional Language?
  Journal of Functional Programming, 1(1), 1993.

+ C. Strachey.  Fundamental Concepts in
  Programming Languages.  Higher-Order and
  Symbolic Computation, 13:11-49, 2000.

+ Philip Wadler. Lazy versus strict.  ACM
  Comput. Surv., 28(2):318-320, 1996.

+ P. Wadler.  Comprehending Monads.  In Conference
  on Lisp and Funcional Programming, Nice, France,
  June 1990. ACM.

+ P. Wadler and S. Blott.  How to make ad hoc
  polymorphism less ad hoc.  In Proceedings of
  16th ACM Symposium on Principles of Programming
  Languages, pages 60-76, January 1989.

+ M. Wright and A. Freed.  Open Sound Control: A
  New Protocol for Communicating with Sound
  Synthesizers.  In Proceedings of the
  International Computer Music Conference, pages
  101-104.  International Computer Music
  Association, 1997.
