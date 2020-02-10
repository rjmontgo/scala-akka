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
import scala.util.Random
import scala.util.{Try, Success, Failure}
import scala.concurrent._
import scala.concurrent.duration._

object Reader {

  sealed trait Read
  case class ReadNumber(replyTo: ActorRef[String]) extends Read

  def apply(): Behavior[Read] =
    Behaviors.setup(context => new Reader(context))
}

class Reader(context: ActorContext[Reader.Read]) extends AbstractBehavior[Reader.Read](context) {
  import Reader._

  def onMessage(msg: Read): Behavior[Read] = {
    msg match {
      case ReadNumber(replyTo: ActorRef[String]) => {
        replyTo ! new Random().nextInt(20).toString // roll a D-20
      }
    }
    this
  }
}


object AkkaHttpMain {
  def apply(): Behavior[String] = 
    Behaviors.setup(context => new AkkaHttpMain(context))
}

class AkkaHttpMain(context: ActorContext[String]) extends AbstractBehavior[String](context) {
  import Reader._

  implicit val actorSystem = context.system.toClassic
  implicit val materializer = ActorMaterializer()
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global


  import akka.util.Timeout
  

  // asking someone requires a timeout if the timeout hits without response
  // the ask is failed with a TimeoutException
  implicit val timeout: Timeout = 3.seconds
  implicit val scheduler = context.system.scheduler

  val randomNumber = context.spawn(Reader(), "read-data")

  val route =
      concat(path("hello") {
        get {
          // randomNumber ! ReadNumber(this.context.self)
          val result: Future[String] = randomNumber.ask(ref => ReadNumber(ref))

          //complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>Say hello to akka-http</h1>"))
          onComplete(result) {
            case Success(s) => complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>" + s + "</h1>"))
            case Failure(f) => complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>Fail</h1>"))
          }
        }
      })

  val bindingFuture = Http().bindAndHandle(route, "localhost", 54321)

  def onMessage(msg: String) = {
    context.log.info(msg)
    this
  }


  StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => context.system.terminate()) // and shutdown when done
}

object AkkaHttp extends App {

  implicit val actorSystem = ActorSystem(AkkaHttpMain(), "http-system")

}