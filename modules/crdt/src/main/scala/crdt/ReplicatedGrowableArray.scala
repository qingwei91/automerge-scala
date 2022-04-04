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

// source: https://hal.inria.fr/inria-00555588/document
case class ReplicatedGrowableArray[K: Ordering, A](
    pid: String,
    localTime: PosInt,
    internal: TreeMap[K, Option[A]]
) {
  // only expose existing values
  val externals: List[A] = internal.collect { case (k, Some(v)) => v }.toList
}

object ReplicatedGrowableArray {
  given rgaCmRDT[A]: PartialOrderCRDT[ReplicatedGrowableArray, RGAIndex, A] with {

    override type ProjectValue = List[A]

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
