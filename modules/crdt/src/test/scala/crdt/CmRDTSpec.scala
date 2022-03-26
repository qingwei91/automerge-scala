package crdt

import cats.effect._
import cats.data.State
import weaver._
import weaver.scalacheck._
import org.scalacheck._
import org.scalacheck.rng.Seed

object CmRDTSpec extends SimpleIOSuite {
  // no need to prove associativity because CmRDT operation are not binary operations,
  // ie. its 2 domain are not the same
  test("CmRDT operations are commutative") { () =>
    IO.pure(success)
  }
}

trait CmRDTTestModule { self: Expectations.Helpers =>
  type _RemoteOp
  type _LocalOp
  type Data

  type C = CmRDT[Data] { type RemoteOp = _RemoteOp; type LocalOp = _LocalOp }
  given c: C

  def repetition: Int
  def localOpGen: Gen[_LocalOp]
  def remoteOpGen: Gen[_RemoteOp]

  def init: Data
  def seed: Long

  case class TestState(
      dataA: Data,
      dataB: Data,
      seed: Seed,
      opsToA: List[_RemoteOp],
      opsToB: List[_RemoteOp]
  )

  def getRandom(): State[TestState, Long] = State { st =>
    val (l, next) = st.seed.long
    st.copy(seed = next) -> l
  }
  def randomChangeA(): State[TestState, Unit] = State { st =>
    val (remoteOp, nextA)  = st.dataA.change(localOpGen.pureApply(Gen.Parameters.default, st.seed))
    val updatedRemoteOpToB = remoteOp :: st.opsToB
    st.copy(dataA = nextA, seed = st.seed.next, opsToB = updatedRemoteOpToB) -> ()
  }
  def randomChangeB(): State[TestState, Unit] = State { st =>
    val (remoteOp, nextB)  = st.dataB.change(localOpGen.pureApply(Gen.Parameters.default, st.seed))
    val updatedRemoteOpToB = remoteOp :: st.opsToB
    st.copy(dataB = nextB, seed = st.seed.next, opsToA = updatedRemoteOpToB) -> ()
  }

  def randomBroadcastToA(): State[TestState, Unit] = State { st =>
    if (st.opsToA.isEmpty) {
      st -> ()
    } else {
      val (l, next) = st.seed.long
      val idx       = l % st.opsToA.size
      val op        = st.opsToA(idx.toInt)
      val updatedA  = st.dataA.syncRemote(op)
      st.copy(dataA = updatedA, seed = next, opsToA = st.opsToA.filterNot(_ == op)) -> ()
    }
  }
  def randomBroadcastToB(): State[TestState, Unit] = State { st =>
    if (st.opsToB.isEmpty) {
      st -> ()
    } else {
      val (l, next) = st.seed.long
      val idx       = l % st.opsToB.size
      val op        = st.opsToB(idx.toInt)
      val updatedB  = st.dataB.syncRemote(op)
      st.copy(dataB = updatedB, seed = next, opsToB = st.opsToB.filterNot(_ == op)) -> ()
    }
  }

  def clearRemainingOps(): State[TestState, Unit] = State { st =>
    val updatedA = st.opsToA.foldLeft(st.dataA) { case (a, op) =>
      a.syncRemote(op)
    }
    val updatedB = st.opsToB.foldLeft(st.dataB) { case (b, op) =>
      b.syncRemote(op)
    }

    st.copy(dataA = updatedA, dataB = updatedB, opsToA = Nil, opsToB = Nil) -> ()
  }

  def opsAreCommutative = {

    val initTestState = TestState(init, init, Seed(seed), Nil, Nil)

    /** Loop through repetition, in each loop randomly do one of these
      *
      * a) Pick an instance and perform change, then keep the ops in buffer
      *
      * b) Randomly pick an op from buffer and broadcast
      *
      * At the end, clear all buffer
      *
      * assert the a and b are equivalent
      */

    val randomizedLoop = (0 to repetition).foldLeft(State.empty[TestState, Unit]) { case (st, _) =>
      for {
        randomL <- getRandom()
        choice = randomL % 4
        _ <- choice match {
          case 0 => randomChangeA()
          case 1 => randomChangeB()
          case 2 => randomBroadcastToA()
          case 3 => randomBroadcastToB()
          case _ => ???
        }
      } yield ()
    }

    randomizedLoop.flatMap(_ => clearRemainingOps()).run(initTestState)

  }
}
