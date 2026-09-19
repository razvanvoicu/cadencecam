package sgrv.fe

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

/** Suppresses the completion of a request after its owner has moved on.
  *
  * A Scala [[Future]] cannot cancel browser `fetch` once it is running. Invalidating the scope gives UI lifecycle code
  * the useful half of cancellation: the response may still arrive, but it cannot update an unmounted screen or replace
  * newer data. [[latest]] also invalidates an older request when a new one supersedes it.
  */
private[fe] final class RequestScope:
  private var generation = 0L

  def invalidate(): Unit = generation += 1

  def run[A](request: Future[A])(use: A => Unit)(using executionContext: ExecutionContext): Unit =
    val startedIn = generation
    request.foreach: result =>
      if generation == startedIn then use(result)

  def latest[A](request: Future[A])(use: A => Unit)(using executionContext: ExecutionContext): Unit =
    invalidate()
    run(request)(use)
