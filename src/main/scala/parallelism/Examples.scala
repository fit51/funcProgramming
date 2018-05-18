package parallelism

import java.util.concurrent.Executors

object Examples extends App {
  val s = Executors.newFixedThreadPool(4)
  val echoer = Actor[String](s) {
    msg => println(s"Got messsage $msg")
  }

  echoer ! "hello!"
  echoer ! "good buy!"
}
