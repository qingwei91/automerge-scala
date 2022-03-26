package crdt
import crdt.VectorClock._

case class MVRegister[A](
    pid: ProcessId,
    existing: Set[(A, VectorClock.Clock)] = Set.empty,
    clock: VectorClock.Clock = VectorClock.empty
)

given mvRegCRDT[A]: CmRDT[MVRegister[A]] with {
  type SetVal            = A
  override type LocalOp  = SetVal
  override type RemoteOp = (A, VectorClock.Clock)

  extension (register: MVRegister[A]) {
    override def syncRemote(op: (A, Clock)): MVRegister[A] = {
      val (value, opClock) = op
      val syncedClock      = register.clock.merge(opClock)
      if (register.existing.isEmpty) {
        MVRegister[A](register.pid, Set(op), syncedClock)
      } else {
        val updatedSet = register.existing.flatMap { case v @ (_, clock) =>
          clock.compare(opClock) match {
            case VectorClock.IsAfter      => Set(op)
            case VectorClock.IsBefore     => Set(v)
            case VectorClock.IsEqual      => Set(v)
            case VectorClock.IsConcurrent => Set(v, op)
          }
        }

        MVRegister(register.pid, updatedSet, syncedClock.increment(register.pid))
      }
    }

    def change(newVal: SetVal): (RemoteOp, MVRegister[A]) = {
      val updatedClock = register.clock.increment(register.pid)
      // when update from local, we are sure this update is not concurrent to any existing value, it happens after
      // so we can throw them away
      val op          = (newVal, updatedClock)
      val newRegister = register.copy(existing = Set(newVal -> updatedClock), clock = updatedClock)
      op -> newRegister
    }
  }
}
