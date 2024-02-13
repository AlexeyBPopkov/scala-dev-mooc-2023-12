package module1.futures_test

import module1.futures.task_futures_sequence._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Failure}

object test {
  implicit val ex: ExecutionContext = ExecutionContext.global

  val testFutures: List[Future[Int]] = List(
    Future.successful(1),
    Future.successful(2),
    Future.failed(new RuntimeException("Test error 1")),
    Future.successful(3),
    Future.failed(new RuntimeException("Test error 2"))
  )

  fullSequence(testFutures).onComplete {
    case Success((successes, failures)) =>
      println(s"Successes: $successes")
      println(s"Failures: ${failures.map(_.getMessage)}")
    case Failure(exception) =>
      println(s"Unexpected failure: ${exception.getMessage}")
  }

  Thread.sleep(2000)
}