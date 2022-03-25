package crdt

trait CmRDT {
  type RemoteOp
  type LocalOp
  def syncRemote(op: RemoteOp): O
  def change(op: LocalOp): (RemoteOp, O)
}
