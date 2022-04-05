package crdt

import scala.collection.immutable.TreeMap
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric._
import eu.timepit.refined.auto._

type PosInt = Int Refined Positive

// this data structure has to be unique within the context of a single CRDT
case class RGAIndex(pid: String, idx: PosInt, clock: PosInt) {
  def increaseClock: RGAIndex = {
    val updatedClock = refineV[Positive].unsafeFrom(clock + 1) // this is safe
    copy(clock = updatedClock)
  }
}
object RGAIndex {
  given rgaTSOrdering: Ordering[RGAIndex] with {
    override def compare(x: RGAIndex, y: RGAIndex): Int = {
      if (x.idx != y.idx) {
        Ordering[Int].compare(x.idx, y.idx)
      } else if (x.clock != y.clock) {
        Ordering[Int].compare(x.clock, y.clock)
      } else {
        Ordering[String].compare(x.pid, y.pid)
      }
    }
  }
}

/** This trait expresses the idea of Replicated Growable Array described in
  * https://hal.inria.fr/inria-00555588/document
  *
  * It is a multi-value data type, ie. a collection of data, it supports adding a value into the
  * collection and removal of the value.
  *
  * In theory, it is convergent iff there exists a partial order of the value type `A`, when A has a
  * partial order, there are a few concurrent scenario that we need to consider:
  *
  *   1. Concurrent Add and Remove of the same value
  *   1. Concurrent add of multiple values that depends on the same prior value
  *   1. Concurrent add and removal of values with dependency
  *
  * The 1st case is solved by 2 rules:
  *
  *   1. Only execute remove if a value has been added.
  *   1. Values should be unique, for example 2 at T0 and 2 and T2 should not be considered equal in
  *      this context.
  *
  * The 2nd and 3rd case are solved by partial order (or total order depending on implementation).
  * If there exists a partial order, then we know that which value will come next even if a prior
  * element has been modified concurrently. For example, given 1 -> 2 -> 5 -> 6, if we concurrently
  * remove 2 and 5, we know that 6 should follow 1 because or partial ordering.
  *
  * Note that while Partial Order is enough to reason about the behavior, for RGA, we use Total
  * Order, meaning there's a strict order. I havent figure out how to encode the more abstract
  * Add-Remove Partial Order datatype in code without committing to concrete implementation, would
  * be a good exercise.
  */
case class ReplicatedGrowableArray[K: Ordering, A](
    pid: String,
    localTime: PosInt,
    internal: TreeMap[K, Option[A]]
) {
  // only expose existing values
  val externals: List[A] = internal.collect { case (k, Some(v)) => v }.toList
}

object ReplicatedGrowableArray {
  sealed trait RGALocal[+A]
  case class LocalInsertAfterA[A](anchor: RGAIndex, value: A) extends RGALocal[A]
  case class LocalRemove(key: RGAIndex)                       extends RGALocal[Nothing]

  sealed trait RGARemote[+A]
  case class RemoteInsertByKey[A](key: RGAIndex, value: A) extends RGARemote[A]
  case class RemoteRemove(key: RGAIndex)                   extends RGARemote[Nothing]
  case object Noop                                         extends RGARemote[Nothing]

  given rgaCmRDT[A]: CmRDT[RGA[A]] with {

    override type ProjectValue = List[A]
    override type LocalOp      = RGALocal[A]
    override type RemoteOp     = RGARemote[A]

    extension (rga: RGA[A]) {
      override def read: ProjectValue = rga.externals

      override def change(op: LocalOp): (RemoteOp, RGA[A]) = op match {
        case LocalInsertAfterA(anchor, toInsert) =>
          val newTime = refineV[Positive].unsafeFrom(rga.localTime + 1)
          // this guarantee the idx is unique, as we always monotonically increases it
          // unless we have integer overflow
          val newKey = RGAIndex(
            rga.pid,
            refineV[Positive].unsafeFrom(anchor.idx + 1),
            newTime
          )
          val updatedRGA = rga.copy(
            localTime = newTime,
            internal = rga.internal.updated(newKey, Some(toInsert))
          )
          RemoteInsertByKey(newKey, toInsert) -> updatedRGA

        case LocalRemove(toRemove) =>
          if (rga.internal.contains(toRemove)) {
            RemoteRemove(toRemove) -> rga.copy(internal = rga.internal.updated(toRemove, None))
          } else {
            // dont allow removal when the key does not exists
            Noop -> rga
          }
      }

      override def syncRemote(op: RemoteOp): RGA[A] = {
        op match {
          case Noop => rga
          case RemoteInsertByKey(key, value) =>
            val updatedInternal = rga.internal.updatedWith(key) {
              case a @ Some(writtenVal) => a
              case None                 => Some(Some(value))
            }
            rga.copy(internal = updatedInternal)
          case RemoteRemove(toRemove) => rga.copy(internal = rga.internal.updated(toRemove, None))
        }
      }
    }
  }
}

type RGA[A] = ReplicatedGrowableArray[RGAIndex, A]

object RGA {
  def apply[A](pid: String): RGA[A] = {
    val initClock = refineV[Positive].unsafeFrom(1)
    ReplicatedGrowableArray(
      pid,
      initClock,
      // initialize internal with a tombstone, this trick allows us to restrict our protocol to `insertAfterX` and `remove`
      // while this might seems hacky, it should be fine because its never exposed to client
      internal = TreeMap(RGAIndex(pid, refineV[Positive].unsafeFrom(1), initClock) -> None)
    )
  }
}
