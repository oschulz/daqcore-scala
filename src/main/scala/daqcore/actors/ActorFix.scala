package scala.actors
package fix_daqcore


/** Temporary fix/hack for the bugs in scala-2.8-beta1 !!-operator for actors
(respond on returned Future is broken). ActorFix.!!% contains the improved
implementation from scala-2.8-r21389. Hopefully will no longer be needed
once scala-2.8-beta2 (or whatever the next step will be) is released. */

class ActorFix(wrapped: Actor) {
  def !!%(msg: Any): Future[Any] =
    this !!% (msg, { case x => x })

  def !!%[A](msg: Any, handler: PartialFunction[Any, A]): Future[A] = {
    val myself = Actor.rawSelf(wrapped.scheduler)
    val ftch = new ReactChannel[A](myself)
    val res = new scala.concurrent.SyncVar[A]

    val out = new OutputChannel[Any] {
      def !(msg: Any) = {
        val msg1 = handler(msg)
        ftch ! msg1
        res set msg1
      }
      def send(msg: Any, replyTo: OutputChannel[Any]) = {
        val msg1 = handler(msg)
        ftch.send(msg1, replyTo)
        res set msg1
      }
      def forward(msg: Any) = {
        val msg1 = handler(msg)
        ftch forward msg1
        res set msg1
      }
      def receiver =
        myself.asInstanceOf[Actor]
    }

    wrapped.send(msg, out)

    new Future[A](ftch) {
      def apply() = {
        if (!isSet)
          fvalue = Some(res.get)
        
        fvalueTyped
      }
      def respond(k: A => Unit): Unit =
        if (isSet) k(fvalueTyped)
        else inputChannel.react {
          case any => fvalue = Some(any); k(fvalueTyped)
        }
      def isSet =
        !fvalue.isEmpty
    }
  }
}
