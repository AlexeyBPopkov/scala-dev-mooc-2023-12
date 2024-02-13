package module1.futures

import scala.concurrent.{ExecutionContext, Future}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */

  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    // Преобразуем каждый Future в Future, который всегда завершается успешно,
    // но возвращает Either с Left для ошибок и Right для успешных результатов.
    val futuresOfEither: List[Future[Either[Throwable, A]]] = futures.map { future =>
      future.map(Right(_)).recover { case e => Left(e) }
    }

    // Используем Future.traverse для преобразования списка Future[Either] в Future списка Either
    Future.traverse(futuresOfEither)(identity).map { listOfEither => {
      // Разделяем список Either на два списка: один для успешных результатов, другой для ошибок
      val (lefts, rights) = listOfEither.partition(_.isLeft)
      (rights.flatMap(_.toOption), lefts.flatMap(_.left.toOption))
      }
    }
  }


}
