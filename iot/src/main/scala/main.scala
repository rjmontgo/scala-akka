package main

import scala.io.Source
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors

sealed trait Data

object TensionReader {

  sealed trait Command
  case class ReadTension(replyTo: ActorRef[RespondTension]) extends Command
  case class RespondTension(value: Option[Double], from: ActorRef[ReadTension]) extends Data

  def apply(): Behavior[Command] = {
    Behaviors.setup(context => new TensionReader(context))
  }
}

class TensionReader(context: ActorContext[TensionReader.Command]) extends AbstractBehavior[TensionReader.Command](context) {
  import TensionReader._
  val iterator = Source.fromResource("tension.data").getLines()

  def onMessage(msg: Command): Behavior[Command] = 
    msg match {
      case ReadTension(replyTo) =>
        if (iterator.hasNext) {
          replyTo ! RespondTension(Some(iterator.next().toDouble), this.context.self)
          this
        } else {
          Behaviors.stopped
        }
      case _ => Behaviors.unhandled
    }
}


object LuxReader {

  sealed trait Command
  case class ReadLux(replyTo: ActorRef[RespondLux]) extends Command
  case class RespondLux(value: Option[Double], from: ActorRef[ReadLux]) extends Data

  def apply(): Behavior[Command] = {
    Behaviors.setup(context => new LuxReader(context))
  }
}

class LuxReader(context: ActorContext[LuxReader.Command]) extends AbstractBehavior[LuxReader.Command](context) {
  import LuxReader._
  val iterator = Source.fromResource("lux.data").getLines()

  def onMessage(msg: Command): Behavior[Command] = {
    msg match {
      case ReadLux(replyTo) =>
        if (iterator.hasNext) {
          replyTo ! RespondLux(Some(iterator.next().toDouble), this.context.self)
          this
        } else {
          Behaviors.stopped
        }
      case _ => Behaviors.unhandled
    }
  }
}

object DataAggregator {
  def apply(): Behavior[Data] = {
    Behaviors.setup(context => new DataAggregator(context))
  }
}

class DataAggregator(context: ActorContext[Data]) extends AbstractBehavior[Data](context) {
  import LuxReader._
  import TensionReader._
  context.spawn(LuxReader(), "LuxReader") ! ReadLux(context.self)
  context.spawn(TensionReader(), "TensionReader") ! ReadTension(context.self)

  def onMessage(msg: Data): Behavior[Data] = {
    msg match {
      case RespondLux(value, replyTo) => 
        replyTo ! ReadLux(this.context.self)
        context.log.info("lux " + value.toString)
        this
      case RespondTension(value, replyTo) =>
        replyTo ! ReadTension(this.context.self)
        context.log.info("kPa " + value.toString)
        this
    }
  }
}

object akkaMain extends App {
  val system = ActorSystem(DataAggregator(), "main")
}