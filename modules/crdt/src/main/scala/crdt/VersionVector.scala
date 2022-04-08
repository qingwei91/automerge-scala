package crdt

object VersionVector {
  type ProcessId    = String
  opaque type Clock = Map[ProcessId, Int] // refine this to PositiveInt

  given ClockEQ: CanEqual[Clock, Clock] = CanEqual.derived

  // todo: empty clock does not makes sense
  def empty: Clock                = Map.empty
  def init(pid: ProcessId): Clock = Map(pid -> 1)

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
        val v1HasLarger = v1.exists { case (id, i1) => i1 > v2.getOrElse(id, 0) }
        val v2HasLarger = v2.exists { case (id, i2) => i2 > v1.getOrElse(id, 0) }

        (v1HasLarger, v2HasLarger) match {
          case (true, true) => IsConcurrent
          case (false, false) =>
            throw new IllegalStateException("This should not happen, it means v1 and v2 are equal")
          case (true, false) => IsBefore
          case (false, true) => IsAfter
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
          case (None, None)    => Set.empty // should not happen
        }
      }

      updatedTuples.toMap
    }
  }
}
