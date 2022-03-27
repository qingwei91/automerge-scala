package crdt

opaque type Counter = Double
object Counter {
  def apply(d: Double): Counter = 0.0
  extension (x: Counter) {
    def value: Double = x
  }

  given counterCRDT: CmRDT[Counter] with {
    type Increment         = Double
    override type RemoteOp = Increment
    override type LocalOp  = Increment

    extension (counter: Counter) {
      override def syncRemote(op: Increment): Counter = counter.value + op

      override def change(delta: LocalOp): (RemoteOp, Counter) = {
        delta -> (counter.value + delta)
      }
    }

  }
}
