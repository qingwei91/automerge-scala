package crdt

import org.scalacheck.rng.Seed
import scala.collection.mutable.ListBuffer

class UnreliableNetwork[T](val seed: Seed, val ls: List[T] = List.empty) {

  def consumeRandom: (List[T], UnreliableNetwork[T]) = {
    if (ls.isEmpty) {
      Nil -> this
    } else {
      val (randL, nextSeed) = seed.long
      val newInstance       = new UnreliableNetwork[T](nextSeed, ls)
      newInstance.consumeAtMostN(randL.toInt % ls.size)
    }
  }

  def consumeAtMostN(n: Int): (List[T], UnreliableNetwork[T]) = {
    val (randL, nextSeed)            = seed.long
    val consumeStartPoint            = randL.toInt % ls.size
    val (toRetain1, afterStartPoint) = ls.splitAt(consumeStartPoint + 1)
    val (toConsume, toRetain2)       = afterStartPoint.splitAt(n)

    val updatedNetwork = new UnreliableNetwork[T](nextSeed, toRetain1 ++ toRetain2)
    toConsume -> updatedNetwork
  }

  def insert(items: List[T]): UnreliableNetwork[T] = {
    val (randL, nextSeed) = seed.long
    val insertPoint       = randL.toInt % ls.size
    val (a, b)            = ls.splitAt(insertPoint)
    val updated           = a ++ items ++ b
    new UnreliableNetwork[T](nextSeed, updated)
  }
}
