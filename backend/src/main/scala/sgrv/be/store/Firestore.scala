package sgrv.be.store

import com.google.api.core.{ApiFuture, ApiFutureCallback, ApiFutures}
import com.google.cloud.firestore.{Firestore, FirestoreOptions}
import com.google.common.util.concurrent.MoreExecutors
import sgrv.be.auth.AppConfig
import zio.{Task, ZIO, ZLayer}

/** Bridges a Google `ApiFuture` into an interruptible ZIO effect rather than blocking a worker thread on `get`. */
private[be] object GoogleFuture:
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

/** The host's Firestore client, offered to plugins as the generic `firestore` capability.
  *
  * Deliberately one client for the whole process: each `Firestore` instance owns a gRPC channel and its credential
  * resolution, so a second one would cost another channel on every cold start. Plugins build their own collection
  * adapters on top of this rather than asking the host for a collection-specific service.
  */
private[be] object FirestoreClient:
  val live: ZLayer[AppConfig, Throwable, Firestore] =
    ZLayer.scoped:
      for
        config <- ZIO.service[AppConfig]
        firestore <- ZIO.acquireRelease(
          ZIO.attemptBlocking:
            FirestoreOptions
              .newBuilder()
              .setProjectId(config.firestore.projectId)
              .setDatabaseId(config.firestore.databaseId)
              .build()
              .getService
        )(value => ZIO.attemptBlocking(value.close()).ignore)
      yield firestore
