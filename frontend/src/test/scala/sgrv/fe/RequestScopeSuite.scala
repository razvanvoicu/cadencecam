package sgrv.fe

import munit.FunSuite

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Promise

class RequestScopeSuite extends FunSuite:
  test("an invalidated scope ignores a completion from the screen it replaced"):
    val result = Promise[Int]()
    val scope = RequestScope()
    var observed = Option.empty[Int]

    scope.run(result.future)(value => observed = Some(value))
    scope.invalidate()
    result.success(7)

    settled(result.future).map(_ => assertEquals(observed, None))

  test("latest ignores the older request and accepts the replacement"):
    val older = Promise[Int]()
    val newer = Promise[Int]()
    val scope = RequestScope()
    var observed = Vector.empty[Int]

    scope.latest(older.future)(value => observed :+= value)
    scope.latest(newer.future)(value => observed :+= value)
    older.success(1)
    newer.success(2)

    settled(Future.sequence(Seq(older.future, newer.future))).map(_ => assertEquals(observed, Vector(2)))

  private def settled[A](future: Future[A]): Future[Unit] =
    future.flatMap(_ => Future.unit).flatMap(_ => Future.unit)
