package sgrv.be.auth

import com.google.api.core.{ApiFuture, ApiFutureCallback, ApiFutures}
import com.google.common.util.concurrent.MoreExecutors
import zio.{Task, ZIO}

import scala.language.postfixOps

private[auth] object GoogleFuture:
  def fromApiFuture[A](make: => ApiFuture[A]): Task[A] =
    ZIO
      .attempt(make)
      .flatMap: future =>
        ZIO.asyncInterrupt: complete =>
          ApiFutures.addCallback(
            future,
            new ApiFutureCallback[A]:
              override def onSuccess(result: A): Unit = complete(ZIO.succeed(result))
              override def onFailure(error: Throwable): Unit = complete(ZIO.fail(error))
            ,
            MoreExecutors.directExecutor()
          )
          Left(ZIO.succeed(future.cancel(true)).unit)
