import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

class LuxData {
  val data: BigDecimal
  // add ZonedDateTime
}

class TensionData {
  val data: BigDecimal
  // add ZonedDateTime
}

object LuxReader {
  final case class ReadLux(replyTo: ActorRef[LuxData])
  
  def apply(): Behavior[ReadLux] = Behaviors.receive {

  }
}

object TensionReader {
  final case class ReadTension(replyTo: ActorRef[TensionData])

  def apply(): Behavior[ReadTension] = Behaviors.receive {

  }
}

