package crdt

object VectorClock {
  type ProcessId    = String
  opaque type Clock = Map[ProcessId, Int]

  given ClockEQ: CanEqual[Clock, Clock] = CanEqual.derived

  def empty: Clock = Map.empty

  sealed trait Causality derives CanEqual
  case object IsAfter      extends Causality
  case object IsBefore     extends Causality
  case object IsEqual      extends Causality
  case object IsConcurrent extends Causality

  extension (v1: Clock) {
    def compare(v2: Clock): Causality = {
      if (v1 == v2) {
        IsEqual
      } else {
        var v2Smaller = if (v1.size > v2.size) true else false
        var v2Larger  = if (v1.size < v2.size) true else false

        for ((key, v2V) <- v2) {
          v1.get(key) match {
            case Some(v1V) =>
              val diff = v2V - v1V
              if (diff > 0) {
                v2Larger = true
              } else if (diff < 0) {
                v2Smaller = true
              }
            case None =>
              // key in v2 but not in v1 means it is larger
              v2Larger = true
          }
        }

        if (v2Smaller && v2Larger) {
          IsConcurrent
        } else if (v2Smaller) {
          IsBefore
        } else {
          IsAfter
        }
      }
    }
    def increment(pid: ProcessId): Clock = {
      v1.updatedWith(pid) {
        case Some(value) => Some(value + 1)
        case None        => Some(1)
      }
    }
    def merge(v2: Clock): Clock = {
      val allKeys = v1.keySet ++ v2.keySet
      val updatedTuples = allKeys.flatMap { pid =>
        (v1.get(pid), v2.get(pid)) match {
          case (Some(a), Some(b)) =>
            if (a > b) { Set(pid -> a) }
            else { Set(pid -> b) }
          case (None, Some(b)) => Set(pid -> b)
          case (Some(a), None) => Set(pid -> a)
          case (None, None)    => Set.empty
        }
      }

      updatedTuples.toMap
    }
  }
}
