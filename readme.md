## Overview

This is an exercise for me to try out Scala 3, and to learn more about CRDT. I am using a typeclass approach, this allow easy code reuse.

I implemented some common CRDTs, documented in this [survey study](https://hal.inria.fr/inria-00555588/document)

So far, I've implemented the following (with test)
* Multi-Value Register
* Counter
* Replicated Growable Array (aka RGA)

## Todo

- [x] Implement CRDT for Text
- [ ] Implement CRDT for JSON, described [here](https://arxiv.org/pdf/1608.03960.pdf)
