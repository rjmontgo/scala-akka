import akka.actor.typed.Behavior
import akka.actor.typed.ActorSystem
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model._
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.actor.typed.DispatcherSelector

import scala.io.StdIn
import scala.io.Source
import scala.util.Random
import scala.util.{Try, Success, Failure}
import scala.concurrent._
import scala.concurrent.duration._


object Reader {

  // put routes in here
  // become()
  sealed trait Read
  case class ReadNumber(replyTo: ActorRef[String]) extends Read

  def apply(file: String, port: Int): Behavior[Read] =
    Behaviors.setup(context => new Reader(context, file, port))
}

class Reader(context: ActorContext[Reader.Read], file: String, port: Int) extends AbstractBehavior[Reader.Read](context) {
  import Reader._
  import akka.util.Timeout

  implicit val actorSystem = context.system.toClassic
  implicit val materializer = ActorMaterializer()
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  implicit val timeout: Timeout = 3.seconds
  implicit val scheduler = context.system.scheduler

  val lineIterator = Source.fromResource(file).getLines()

  // val routes = FooRouter.route ~ BarRouter.route

  // separate routes --> put in companion object for example
  val route =
      concat(path("read") {
        get {
          complete(HttpEntity(ContentTypes.`application/json`, file + " " + getNextLine(lineIterator).getOrElse("Done")))
        }
      })

  val bindingFuture = Http().bindAndHandle(route, "localhost", port)

  def onMessage(msg: Read): Behavior[Read] = {
    msg match {
      case ReadNumber(replyTo: ActorRef[String]) => {
        replyTo ! new Random().nextInt(20).toString // roll a D-20
      }
    }
    this
  }

  // take(1)
  // .iterator.next()

  // wrap ZIO task
  def getNextLine(lineItr: Iterator[String]): Option[String] = {
    if (lineIterator.hasNext)
      Some(lineIterator.next)
    else 
      None
  }

}


object AkkaHttpMain {
  def apply(): Behavior[String] = 
    Behaviors.setup(context => new AkkaHttpMain(context))
}

class AkkaHttpMain(context: ActorContext[String]) extends AbstractBehavior[String](context) {
  import Reader._
  import akka.http.scaladsl.unmarshalling.Unmarshal

  implicit val actorSystem = context.system.toClassic
  implicit val materializer = ActorMaterializer()
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  val luxReader = context.spawn(Reader("lux.data", 54322), "read-data")
  val tensionReader = context.spawn(Reader("tension.data", 54321), "tension-data")

  def onMessage(msg: String) = {
    context.log.info(msg)
    this
  }
  
  loopRequest("http://localhost:54322/read")
  loopRequest("http://localhost:54321/read")

  def loopRequest(uri: String): Unit = {
    Http().singleRequest(HttpRequest(uri = uri)).onComplete(tri => tri match {
      case Success(res) => {
          Unmarshal(res.entity)
            .to[String]
            .onComplete(tryStr => 
              tryStr.fold(fail => context.log.info(fail.toString), 
                pass => {
                  val split = pass.split(" ")
                  if (split(1) != "Done") {
                    context.log.info(pass)
                    loopRequest(uri)
                  }
                }))
        }
        
      case Failure(_)   => sys.error("something wrong")
    })
  }
}

object AkkaHttp extends App {

  implicit val actorSystem = ActorSystem(AkkaHttpMain(), "http-system")

  try {
    StdIn.readLine()
  } finally {
    actorSystem.terminate()
  }
}