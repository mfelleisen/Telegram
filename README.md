
## Several solutions to Michael Jackson's telegram problem

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

- [lists](list.rkt): this is a functional solution that fails to satisfy the
  constraint; but it serves as a first step toward implementing a
  stream-based solution.

- [streams](streams.rkt): replace all `cons` etc. operations with
  `stream-cons` etc. in `lists.rkt` and neither transformer generates
  more than one word or telegram at a time. Like the generator
  solutions, it does set up a thunk to compute the rest, on demand.

The difference between generators and streams is that generators hide
the state of the computation while streams make it explicit in the
tail of `cons` (or `append`) operations. Hence a generator solution
uses control _state_, while the stream solution is essentially
algebraic. 

### read-block

- a simulation of block reading from files 
- sample strings that can be used as a "blocked" file of telegrams
  and some leading words of this "blocked file"


