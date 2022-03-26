package crdt

trait CmRDT[A] {
  type RemoteOp
  type LocalOp
  extension (a: A) {
    def syncRemote(op: RemoteOp): A
    def change(op: LocalOp): (RemoteOp, A)
  }
}
