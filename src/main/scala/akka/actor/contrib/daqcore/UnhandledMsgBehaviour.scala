// Modified version of TypedActor.scala from akka-2.0-RC1

package akka.actor.contrib.daqcore
import akka.actor._


// Has to be implemented under the akka namespace, because
// DeathPactException is private[akka]

object UnhandledMsgBehaviour {
  def unhandled(message: Any, sender: ActorRef, context: ActorContext) {
    message match {
      case Terminated(dead) =>
        val e: Throwable = DeathPactException(dead)
        throw e
      case _                => context.system.eventStream.publish(UnhandledMessage(message, sender, context.self))
    }
  }
}
