package sgrv.be.sessions

import com.google.cloud.firestore.Firestore
import java.time.Instant
import zio.{Task, ZIO, ZLayer}

/** The workout-side operations that logout must complete before authentication disappears.
  *
  * Kept as a service rather than making the authentication route manipulate workout documents itself. The production
  * implementation is backed by the same Firestore account store as Start and Stop. Account logout always closes and
  * releases the counter; device logout does so only when that device owns it. The logout route can test their ordering
  * and failure behavior without pretending to be Firestore.
  */
private[be] trait WorkoutSessions:
  def closeOnLogout(email: String, at: Instant): Task[Unit]
  def closeOnDeviceLogout(email: String, sessionKey: String, at: Instant): Task[Unit]
  def closeOnSessionExpiry(email: String, sessionKey: String, at: Instant): Task[Unit] =
    closeOnDeviceLogout(email, sessionKey, at)

private[be] object WorkoutSessions:
  def closeOnLogout(email: String, at: Instant): ZIO[WorkoutSessions, Throwable, Unit] =
    ZIO.serviceWithZIO[WorkoutSessions](_.closeOnLogout(email, at))

  def closeOnDeviceLogout(
      email: String,
      sessionKey: String,
      at: Instant
  ): ZIO[WorkoutSessions, Throwable, Unit] =
    ZIO.serviceWithZIO[WorkoutSessions](_.closeOnDeviceLogout(email, sessionKey, at))

  val live: ZLayer[Firestore, Nothing, WorkoutSessions] =
    ZLayer.fromFunction: (firestore: Firestore) =>
      new WorkoutSessions:
        override def closeOnLogout(email: String, at: Instant): Task[Unit] =
          AccountKey
            .of(email)
            .flatMap:
              case None       => ZIO.unit
              case Some(name) => AccountSessions.store(firestore).flatMap(_.close(name, SessionEnd.LoggedOut, at)).unit

        override def closeOnDeviceLogout(email: String, sessionKey: String, at: Instant): Task[Unit] =
          closeFor(email, sessionKey, SessionEnd.LoggedOut, at)

        override def closeOnSessionExpiry(email: String, sessionKey: String, at: Instant): Task[Unit] =
          closeFor(email, sessionKey, SessionEnd.Idle, at)

        private def closeFor(email: String, sessionKey: String, why: SessionEnd, at: Instant): Task[Unit] =
          AccountKey
            .of(email)
            .flatMap:
              case None       => ZIO.unit
              case Some(name) =>
                AccountSessions
                  .store(firestore)
                  .flatMap(
                    _.closeForCounter(
                      name,
                      CountingSessionListener.browserSession(sessionKey),
                      why,
                      at
                    )
                  )
                  .unit
