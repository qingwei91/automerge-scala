package crdt

case class Counter(v: Double = 0.0) extends CmRDT {
  case class Increment(v: Double)
  override type Op = Increment

  override def sync(op: Increment): Counter = Counter(v + op.v)

  def change(delta: Double): (Op, Counter) = {
    Increment(delta) -> Counter(v + delta)
  }
}
