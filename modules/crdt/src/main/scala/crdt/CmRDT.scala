package crdt

trait CmRDT[A] {
  type RemoteOp
  type LocalOp
  type ProjectValue
  extension (a: A) {
    def syncRemote(op: RemoteOp): A
    def change(op: LocalOp): (RemoteOp, A)
    def read: ProjectValue
  }
}
