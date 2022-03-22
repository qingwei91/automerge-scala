package crdt
import crdt.VectorClock._

case class MVRegister[A](
    pid: ProcessId,
    existing: Set[(A, VectorClock.Clock)] = Set.empty,
    clock: VectorClock.Clock = VectorClock.empty
) extends CmRDT {
  override type Op = (A, VectorClock.Clock)

  override def sync(op: (A, Clock)): MVRegister[A] = {
    val (value, opClock) = op
    val syncedClock      = clock.merge(opClock)
    if (existing.isEmpty) {
      MVRegister[A](pid, Set(op), syncedClock)
    } else {
      val updatedSet = existing.flatMap { case v @ (_, clock) =>
        clock.compare(opClock) match {
          case VectorClock.IsAfter      => Set(op)
          case VectorClock.IsBefore     => Set(v)
          case VectorClock.IsEqual      => Set(v)
          case VectorClock.IsConcurrent => Set(v, op)
        }
      }

      MVRegister(pid, updatedSet, syncedClock.increment(pid))
    }
  }

  def change(newVal: A): (Op, MVRegister[A]) = {
    val updatedClock = clock.increment(pid)
    // when update from local, we are sure this update is not concurrent to any existing value, it happens after
    // so we can throw them away
    val op          = (newVal, updatedClock)
    val newRegister = this.copy(existing = Set(newVal -> updatedClock), clock = updatedClock)
    op -> newRegister
  }
}
