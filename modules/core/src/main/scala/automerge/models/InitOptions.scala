package automerge.models

case class InitOptions(
    actorId: ActorId,
    deferActorId: Boolean,
    freeze: Boolean
)
