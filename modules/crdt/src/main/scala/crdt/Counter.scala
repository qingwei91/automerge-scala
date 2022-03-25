package crdt

case class Counter(v: Double = 0.0) extends CmRDT {
  type Increment         = Double
  override type RemoteOp = Increment
  override type LocalOp  = Increment

  override def syncRemote(op: Increment): Counter = Counter(v + op)

  def change(delta: LocalOp): (RemoteOp, Counter) = {
    delta -> Counter(v + delta)
  }
}
