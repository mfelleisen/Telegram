
## Several solutions to Michael Jackson's telegram problem

Source: Principles of Program Design, p. 155

### Constraints

The blocked file is to be transformed into a series of words. A second
program analyzes the series of words as telegrams to gather the
require data: the number of words plus the number of overly long
words.

```
                 +----------------+               +--------------------+                +----------------+
 BlockedFile --> | discover words | --> word* --> | discover telegrams | --> count* --> | format & print | --> TeleInfo
                 +----------------+               +--------------------+                +----------------+
```

Each such transformation is to use at most one "record" to deliver
information. Until this information is consumed, it pauses. When it is
consumed, the transformation generates the next "record" until the end
of the file is reached. At this point the transformation signals the
end of the series.

### Generators

In a language or an ecosystem that implements _generators_, the
transformation is straightforward. Each of them is a generator that
gathers data until it can use `yield` to deliver it. This `yield`
instruction causes the program to pause. 

- [functional-generators](functional-generators.rkt): solution
  exploiting a `yield` that is like `return` in languages,
  transferring one value from the generator to its consumer.

- [imperative-generators](imperative-generators.rkt): a variant of the
  first one that uses assignment statements to a single variable per
  generator to validate that only one piece of data is in flight at
  any time.

### Lists and Streams

One can think of a file of words as a list of words. But generating
the complete lists of words consumes space for all elements when only
one is needed. Ditto for the file of "records" about telegrams.

- [lists](list.rkt): this is a functional solution that fails to
  satisfy the constraint; but it serves as a first step toward
  implementing a stream-based solution.

- [streams](streams.rkt): replace all `cons` etc. operations with
  `stream-cons` etc. in `lists.rkt` and neither transformer generates
  more than one word or telegram at a time. Like the generator
  solutions, it does set up a thunk to compute the rest, on demand.

- [lazy](lazy.rkt): the `lazy` language in Racket's ecosystem should,
  in principle, turn the `lists` program into a stream-based one,
  with all the benefits already spelled out. But, `lazy` is a teaching
  language and its interaction with strict libraries is ad hoc and
  occasionally broken. Hence the rewrite from `lists` to `lazy` is
  larger than expected. (I assigned this problem as a dissertation
  years ago, but it remains unsolved.) 


The difference between generators and streams is that generators hide
the state of the computation while streams make it explicit in the
tail of `cons` (or `append`) operations. Hence a generator solution
uses control _state_, while the stream solution is essentially
algebraic. 

### Continuation Passing Style (Program Inversion)

Each transformer can consume an additional functional argument: the
function that consumes the one word or the one telegram record. This
extra argument is the continuation relative to the value passed on,
that is, it deals with this one value and then hands control back to
the transformer. 


- [cps](cps.rkt): use a continuation-passing style to go back and
  forth between producers and consumers of values 

*Note* This solution comes pretty close to the program-inversion
 technique in Jackson's book. His use of COBOL obscures the
 continuation "paragraphs" and there are other small differences.

### Teaching

The exercise is suitable for structural recursion if the instructor
provides `read-block` (which uses generative recursion with
accumulator) as a library.

*Note* `read-block` could be written w/o gen rec, as a "parsing"
exercise. See Part V early example.

- [f-1](f-1.rkt): a very accumulator-oriented solution, and it's
  natural to write them this way. *Exercise* Rewrite those. 

### read-block

- a simulation of block reading from files 
- sample strings that can be used as a "blocked" file of telegrams
  and some leading words of this "blocked file"
