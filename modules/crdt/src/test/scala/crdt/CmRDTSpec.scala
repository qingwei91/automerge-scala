package crdt

import cats.effect._
import cats.data.State
import cats.Eval
import cats.syntax.traverse._
import cats.instances.list._
import weaver._
import weaver.scalacheck._
import org.scalacheck._
import org.scalacheck.rng.Seed
import crdt.UnreliableNetwork

object CmRDTSpec extends SimpleIOSuite with Checkers {
  // no need to prove associativity because CmRDT operation are not binary operations,
  // ie. its 2 domain are not the same
  test("Counter is CRDT") { () =>
    val counterIsCrDT =
      new CmRDTTestModule[Counter](
        initData = List(Counter(0.0), Counter(0.0), Counter(0.0)),
        seed = 1000L,
        repetition = 20
      ) with Expectations.Helpers {
        override def localOpGen(dt: Counter): Gen[crdtEvi.LocalOp] =
          Gen.double.map(_.asInstanceOf[crdtEvi.LocalOp])
      }

    IO {
      counterIsCrDT.opsAreCommutative
    }
  }

  test("MVRegister is CRDT") { () =>
    val registerIsCRDT = new CmRDTTestModule[MVRegister[Int]](
      initData = List(
        MVRegister[Int]("1", 0),
        MVRegister[Int]("2", 0),
        MVRegister[Int]("3", 0)
      ),
      seed = 1002L,
      repetition = 20
    ) with Expectations.Helpers {
      override def localOpGen(dt: MVRegister[Int]): Gen[crdtEvi.LocalOp] =
        Gen.long.map(_.toInt.asInstanceOf[crdtEvi.LocalOp])
    }
    IO {
      registerIsCRDT.opsAreCommutative
    }
  }

  pureTest("MVRegister handle concurrent update") {
    val a = MVRegister[Int]("1", 0)
    val b = MVRegister[Int]("2", 0)

    val (remote20, a20) = a.change(20)
    val (remote10, b10) = b.change(10)
    val updatedB        = b10.syncRemote(remote20)
    val updatedA        = a20.syncRemote(remote10)

    expect.same(updatedA.existing, updatedB.existing)
  }

  test("RGA is CRDT") {
    import ReplicatedGrowableArray._
    val rgaIsCRDT = new CmRDTTestModule[RGA[Int]](
      initData = List(
        RGA("1"),
        RGA("2"),
        RGA("3")
      ),
      seed = 1030L,
      repetition = 20
    ) with Expectations.Helpers {
      override def localOpGen(dt: RGA[Int]): Gen[crdtEvi.LocalOp] = {
        val randomInsert = for {
          pickAKey  <- Gen.oneOf(dt.internal.map { case (k, v) => k })
          randomVal <- Gen.long.map(_.toInt)
        } yield {
          LocalInsertAfterA(pickAKey, randomVal).asInstanceOf[crdtEvi.LocalOp]
        }

        val randomDel = for {
          pickAKey <- Gen.oneOf(dt.internal.map { case (k, v) => k })
        } yield {
          LocalRemove(pickAKey).asInstanceOf[crdtEvi.LocalOp]
        }

        Gen.oneOf[crdtEvi.LocalOp](randomInsert, randomDel)
      }
    }
    IO {
      rgaIsCRDT.opsAreCommutative
    }
  }
}

/** This test works by randomly generating operations, and randomly applying them Then after all
  * operations has been applied to all data, we expect all copies of data converge (ie. be
  * equivalent)
  *
  * Note that CmRDT in general apply the same changes more than once, thus every operation is only
  * applied once. CvRDT otoh can tolerate replay but typically is more expensive to send over the
  * network
  * @tparam Data
  */
trait CmRDTTestModule[Data](initData: List[Data], seed: Long, repetition: Int)(using
    val crdtEvi: CmRDT[Data],
    eq: CanEqual[Data, Data]
) {
  self: Expectations.Helpers =>

  def localOpGen(dt: Data): Gen[crdtEvi.LocalOp]

  case class TestState(
      seed: Seed,
      dataWithNetwork: List[(Data, UnreliableNetwork[crdtEvi.RemoteOp])] = List.empty
  )

  private val initTestState = {
    var _seed = Seed(seed)
    val dtNetwork = initData.map { dt =>
      val network = new UnreliableNetwork[crdtEvi.RemoteOp](_seed.next)
      _seed = _seed.next
      dt -> network
    }
    TestState(
      _seed,
      dtNetwork
    )
  }

  def randomPositiveLong: State[TestState, Int] = State { st =>
    val (l, next) = st.seed.long
    st.copy(seed = next) -> math.abs(l.toInt)
  }
  def randomIntBetween(start: Int, end: Int): State[TestState, Int] = randomPositiveLong.map {
    randL =>
      val range  = end - start
      val resInt = (randL % range) + start
      resInt
  }

  def randomLocalUpdate: State[TestState, Unit] = {
    randomPositiveLong.flatMap { randL =>
      State { st =>
        val dataIdx               = randL % st.dataWithNetwork.size
        val (targetData, network) = st.dataWithNetwork(dataIdx)
        val (remoteOp, updatedData) = targetData.change(
          localOpGen(targetData).pureApply(Gen.Parameters.default, st.seed)
        )
        val updated = st.dataWithNetwork.map {
          case (data, netw: UnreliableNetwork[crdtEvi.RemoteOp]) if data != targetData =>
            data -> netw.insert(remoteOp :: Nil)
          case (td, net) => updatedData -> net
        }
        st.copy(seed = st.seed.next, dataWithNetwork = updated) -> ()
      }
    }
  }
  def randomRemoteSync: State[TestState, Unit] = {
    for {
      st          <- State.get[TestState]
      pointToTake <- randomIntBetween(0, st.dataWithNetwork.size)
      noOfData    <- randomIntBetween(pointToTake, st.dataWithNetwork.size)
      sliceToSync = pointToTake.to(pointToTake + noOfData)
      _ <- State.modify[TestState] { st =>
        val updatedNetworks = st.dataWithNetwork.zipWithIndex.map {
          case ((data, network), idx) if sliceToSync.contains(idx) =>
            val (consumed, updatedNetwork) = network.consumeRandom
            val updatedData = consumed.foldLeft(data) { case (dt, remoteOp) =>
              dt.syncRemote(remoteOp)
            }
            updatedData -> updatedNetwork
          case (pair, idx) => pair
        }
        st.copy(dataWithNetwork = updatedNetworks)
      }
    } yield {}

  }

  def randomDoSomething: State[TestState, Unit] = {
    randomIntBetween(0, 2).flatMap { i =>
      if (i == 0) {
        randomLocalUpdate
      } else {
        randomRemoteSync
      }
    }
  }

  def clearRemainingOps(): State[TestState, Unit] = State { st =>
    val updatedNetworks = st.dataWithNetwork.map { case (dt, network) =>
      val updatedDt = network.ls.foldLeft(dt) { case (_dt, op) =>
        _dt.syncRemote(op)
      }
      updatedDt -> network
    }
    st.copy(dataWithNetwork = updatedNetworks) -> ()
  }

  def opsAreCommutative: Expectations = {
    val randomizedLoop: State[TestState, Unit] =
      (0 to repetition).foldLeft(State.empty[TestState, Unit]) { case (st, _) =>
        st.flatMap(_ => randomDoSomething)
      }

    val (resultState: TestState, _) =
      randomizedLoop.flatMap(_ => clearRemainingOps()).run(initTestState).value

    val finalData = resultState.dataWithNetwork.map(_._1.read)
    expect(finalData.toSet.size == 1)
  }
}
