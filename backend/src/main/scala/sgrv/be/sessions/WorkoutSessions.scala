package sgrv.be.sessions

import com.google.cloud.firestore.Firestore
import java.time.Instant
import zio.{Task, ZIO, ZLayer}

/** The workout-side operation that account logout must complete before authentication disappears.
  *
  * Kept as a service rather than making the authentication route manipulate workout documents itself. The production
  * implementation is backed by the same Firestore account store as Start and Stop, while the logout route can test its
  * ordering and failure behavior without pretending to be Firestore.
  */
private[be] trait WorkoutSessions:
  def closeOnLogout(email: String, at: Instant): Task[Unit]

private[be] object WorkoutSessions:
  def closeOnLogout(email: String, at: Instant): ZIO[WorkoutSessions, Throwable, Unit] =
    ZIO.serviceWithZIO[WorkoutSessions](_.closeOnLogout(email, at))

  val live: ZLayer[Firestore, Nothing, WorkoutSessions] =
    ZLayer.fromFunction: (firestore: Firestore) =>
      new WorkoutSessions:
        override def closeOnLogout(email: String, at: Instant): Task[Unit] =
          AccountKey.of(email).flatMap:
            case None       => ZIO.unit
            case Some(name) => AccountSessions.store(firestore).flatMap(_.close(name, SessionEnd.LoggedOut, at)).unit
