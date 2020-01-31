import scala.io.Source

object logReader extends App {
  val iterator = Source.fromResource("myapp.log").getLines()
  var kPaCount = 0 
  var kPaSum = 0.0
  var luxCount = 0
  var luxSum = 0.0

  while (iterator.hasNext) {
    val line = iterator.next
    val t = line.drop(41).slice(0, 3)
    if (t == "kPa") {
      kPaSum += line.drop(50).dropRight(1).toDouble
      kPaCount += 1
    } else if (t == "lux") {
      luxSum += line.drop(50).dropRight(1).toDouble
      luxCount += 1
    }
  }
  println("kPaSum " + (kPaSum / kPaCount).toString)
  println("luxSum " + (luxSum / luxCount).toString)
}