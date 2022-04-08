package crdt

import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object VersionVectorSpec extends SimpleIOSuite with Checkers {
  pureTest("VectorClock empty should be eq to empty") {
    val eq = VersionVector.empty.compare(VersionVector.empty)
    expect(eq == VersionVector.IsEqual)
  }
  pureTest("VectorClock empty should before any non empty clock") {
    val eq = VersionVector.empty
      .increment("A")
      .compare(VersionVector.empty)
    expect(eq == VersionVector.IsBefore)
  }
  pureTest("VectorClock should detect concurrency") {
    val a1  = VersionVector.empty.increment("A")
    val b1  = VersionVector.empty.increment("B")
    val res = a1.compare(b1)
    expect(res == VersionVector.IsConcurrent)
  }

  pureTest("VectorClock merge should merge correctly") {
    val a = VersionVector.empty.increment("A")
    val b = VersionVector.empty.increment("B")

    expect(a.merge(b) == b.merge(a)) and expect(
      a.merge(b) == VersionVector.empty.increment("A").increment("B")
    )

  }
}
