package crdt

import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object VectorClockSpec extends SimpleIOSuite with Checkers {
  pureTest("VectorClock empty should be eq to empty") {
    val eq = VectorClock.empty.compare(VectorClock.empty)
    expect(eq == VectorClock.IsEqual)
  }
  pureTest("VectorClock empty should before any non empty clock") {
    val eq = VectorClock.empty
      .increment("A")
      .compare(VectorClock.empty)
    expect(eq == VectorClock.IsBefore)
  }
  pureTest("VectorClock should detect concurrency") {
    val a1  = VectorClock.empty.increment("A")
    val b1  = VectorClock.empty.increment("B")
    val res = a1.compare(b1)
    expect(res == VectorClock.IsConcurrent)
  }

  pureTest("VectorClock merge should merge correctly") {
    val a = VectorClock.empty.increment("A")
    val b = VectorClock.empty.increment("B")

    expect(a.merge(b) == b.merge(a)) and expect(
      a.merge(b) == VectorClock.empty.increment("A").increment("B")
    )

  }
}
