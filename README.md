
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

- `functional-generators`: solution exploiting a `yield` that is like
  `return` in languages, transferring one value from the generator to
  its consumer.

- `imperative-generators`: a variant of the first one that uses
  assignment statements to a single variable per generator to validate
  that only one piece of data is in flight at any time. 


### read-block

- a simulation of block reading from files 
- sample strings that can be used as a "blocked" file of telegrams
  and some leading words of this "blocked file"


