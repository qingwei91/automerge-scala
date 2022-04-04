## WIP

Intent: I wish to replicate [Automerge](https://github.com/automerge/automerge) in scala, for learning and for fun.

I am starting by implementing some common CRDT, documented in this [survey study](https://hal.inria.fr/inria-00555588/document)

Currently I've only implemented the following CRDT (with Test!) 

* Multi-Value Register
* Counter
* Replicated Growable Array (aka RGA)

## Todo

- [x] Implement CRDT for Text
- [ ] Implement CRDT for JSON, described [here](https://arxiv.org/pdf/1608.03960.pdf)
- [ ] Implement Automerge
