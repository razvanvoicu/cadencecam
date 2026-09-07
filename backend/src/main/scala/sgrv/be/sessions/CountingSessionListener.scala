package sgrv.be.sessions

import com.google.cloud.Timestamp
import com.google.cloud.firestore.Firestore
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.time.Instant
import scala.jdk.CollectionConverters.*
import sgrv.be.BackendCapabilities
import sgrv.be.core.{CapabilitySet, LoginEvent, LoginListener}
import sgrv.be.store.GoogleFuture
import zio.{Task, ZIO}

private[sessions] object CountingSessionSchema:
  val collection = "CountingSessions"
  val userEmail = "userEmail"
  val startedAt = "startedAt"

/** The listener's private adapter over the host's generic `firestore` capability.
  *
  * A counting session is one login's workout: the record opened when the user signs in, and the place the acquirer's
  * state is meant to be written for the dashboard to read. It is a different thing from the browser session in the
  * `Access` collection, which exists only to authenticate requests.
  */
private[sessions] final class CountingSessionStore(firestore: Firestore):
  private def document(id: String) = firestore.collection(CountingSessionSchema.collection).document(id)

  /** Idempotent per login, so a replayed callback cannot open a second record for the same session. */
  def open(id: String, userEmail: String, startedAt: Instant): Task[Unit] =
    GoogleFuture
      .fromApiFuture(document(id).get())
      .flatMap: snapshot =>
        if snapshot.exists then ZIO.unit
        else GoogleFuture.fromApiFuture(document(id).create(fields(userEmail, startedAt).asJava)).unit

  private def fields(userEmail: String, startedAt: Instant): Map[String, AnyRef] =
    Map[String, AnyRef](
      CountingSessionSchema.userEmail -> userEmail,
      CountingSessionSchema.startedAt -> Timestamp.ofTimeSecondsAndNanos(startedAt.getEpochSecond, startedAt.getNano)
    )

/** Opens a counting session whenever a login succeeds.
  *
  * Nothing in the frontend asks for this and no route exposes it: the record exists by the time the browser is
  * redirected home, because the host raises the event inside the OAuth callback.
  */
object CountingSessionListener extends LoginListener:
  type Requires = Firestore

  override val id = "counting-session"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.one(BackendCapabilities.firestore)

  override def onLogin(event: LoginEvent): ZIO[Requires, Throwable, Unit] =
    for
      firestore <- ZIO.service[Firestore]
      _ <- CountingSessionStore(firestore).open(documentId(event.sessionKey), event.user.email, event.at)
      _ <- ZIO.logInfo(s"Opened counting session for ${event.user.email}")
    yield ()

  /** Derives the record's id from the browser session key by hashing, so one login maps to exactly one counting session
    * without the opaque session key being copied into a second collection.
    */
  private[sessions] def documentId(sessionKey: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(sessionKey.getBytes(UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
