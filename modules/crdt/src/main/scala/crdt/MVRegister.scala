package crdt

import cats.kernel.Order
import crdt.VersionVector._

case class MVRegister[A](
    pid: ProcessId,
    existing: Set[(A, VersionVector.Clock)],
    clock: VersionVector.Clock
)

object MVRegister {
  def apply[A](pid: ProcessId, a: A): MVRegister[A] = MVRegister(
    pid = pid,
    existing = Set(a -> VersionVector.init(pid)),
    clock = VersionVector.init(pid)
  )

  given mvRegCRDT[A]: CmRDT[MVRegister[A]] with {
    type SetVal                = A
    override type LocalOp      = SetVal
    override type RemoteOp     = (A, VersionVector.Clock)
    override type ProjectValue = Either[A, Set[(A, VersionVector.Clock)]]

    extension (register: MVRegister[A]) {
      override def syncRemote(op: (A, Clock)): MVRegister[A] = {
        val (value, opClock) = op
        val syncedClock      = register.clock.merge(opClock).increment(register.pid)

        /** Updating vector clock has 2 steps
          *
          *   1. We replace existing clock iff the remote clock is strictly larger than existing
          *      one.
          *
          * 2. We add remote clock to existing if remote clock is concurrent to ALL existing one
          *
          * It is splitted in 2 steps, because we should only ever add clock into the set if it is
          * concurrent to everyone else, if this isnt true, then it implies there is an existing
          * clock that isnt concurrent with remote, meaning we only need one of them
          */

        val updateLargeClock = register.existing.map { case v @ (_, clock) =>
          if (clock.compare(opClock) == VersionVector.IsAfter) {
            op
          } else {
            v
          }
        }
        val allConcurrent = updateLargeClock.forall { case (_, clock) =>
          clock.compare(opClock) == VersionVector.IsConcurrent
        }
        val updatedSet = if (allConcurrent) {
          updateLargeClock.incl(op)
        } else {
          updateLargeClock
        }
        MVRegister(register.pid, updatedSet, syncedClock)

      }

      override def change(newVal: SetVal): (RemoteOp, MVRegister[A]) = {
        val updatedClock = register.clock.increment(register.pid)
        // when update from local, we are sure this update is not concurrent to any existing value, it happens after
        // so we can throw them away
        val op = (newVal, updatedClock)
        val newRegister =
          register.copy(existing = Set(newVal -> updatedClock), clock = updatedClock)
        op -> newRegister
      }

      override def read: ProjectValue = {
        if (register.existing.size == 1) {
          Left(register.existing.head._1)
        } else {
          Right(register.existing)
        }
      }
    }
  }
}
