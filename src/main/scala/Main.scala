import module1.futures_test.test._

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}


object Main {

  def main(args: Array[String]): Unit = {
    println(s"Hello from ${Thread.currentThread().getName}")

    println("testFutures: " + testFutures)


  }
}