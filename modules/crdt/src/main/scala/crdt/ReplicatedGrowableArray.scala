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
case class ReplicatedGrowableArray[K, A](
    pid: String,
    localTime: PosInt,
    internal: TreeMap[K, Option[A]]
) {
  val externals: List[A] = internal.values.collect { case Some(v) => v }.toList
}

object ReplicatedGrowableArray {
  type RGA[A] = ReplicatedGrowableArray[RGAIndex, A]
  given rgaCmRDT[A]: PartialOrderCRDT[ReplicatedGrowableArray, RGAIndex, A] with {
    
    override type ProjectValue = List[A]

    override def insertXAfterA(
        col: ReplicatedGrowableArray[RGAIndex, A],
        anchor: RGAIndex,
        addedValue: A
    ): ReplicatedGrowableArray[RGAIndex, A] = {
      val newTime = refineV[Positive].unsafeFrom(col.localTime + 1)
      val newIdx = RGAIndex(
        col.pid,
        refineV[Positive].unsafeFrom(anchor.idx + 1),
        newTime
      )
      col.copy(
        localTime = newTime,
        internal = col.internal.updated(newIdx, Some(addedValue))
      )
    }

    override def remove(
        col: ReplicatedGrowableArray[RGAIndex, A],
        toRemove: RGAIndex
    ): ReplicatedGrowableArray[RGAIndex, A] = {
      col.copy(internal = col.internal.updated(toRemove, None))
    }

    extension (rga: RGA[A]) {
      override def read: ProjectValue = rga.externals
    }

  }
}

type RGA[A] = ReplicatedGrowableArray[RGAIndex, A]

object RGA {
  def apply[A](pid: String): RGA[A] = ReplicatedGrowableArray(
    pid,
    refineV[Positive].unsafeFrom(1),
    TreeMap.empty
  )
}
