package crdt

import weaver._
import weaver.scalacheck._

object CmRDTSpec extends SimpleIOSuite with Checkers {
  // no need to prove associativity because CmRDT operation are not binary operations,
  // ie. its 2 domain are not the same
  test("CmRDT operations are commutative") {}
}

trait CmRDTTestModule {
  type Data <: CmRDT

}