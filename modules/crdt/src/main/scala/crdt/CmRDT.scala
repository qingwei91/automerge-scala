package crdt

trait CmRDT {
  type Op
  def sync(op: Op): CmRDT
}
