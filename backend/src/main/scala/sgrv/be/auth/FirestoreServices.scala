package sgrv.be.auth

import com.google.cloud.Timestamp
import com.google.cloud.firestore.Firestore
import java.time.Instant
import scala.jdk.CollectionConverters.*
import sgrv.be.store.GoogleFuture
import zio.{Task, ZIO, ZLayer}

private[auth] object SessionSchema:
  val collection = "Access"
  val expiresAt = "expiresAt"

trait SessionStore:
  def create(
      sessionKey: String,
      user: SessionUser,
      createdAt: Instant,
      expiresAt: Instant
  ): Task[Unit]
  def find(sessionKey: String, now: Instant): Task[Option[SessionUser]]

  /** Reads a retained session record without applying its active-session expiry, so its refresh token can be validated
    * before renewal. Firestore TTL may already have removed the record.
    */
  def findForRefresh(sessionKey: String): Task[Option[SessionUser]]

  /** Extends the active-session expiry only after Google has accepted the stored refresh token. */
  def renew(sessionKey: String, expiresAt: Instant): Task[Unit]
  def invalidate(sessionKey: String): Task[Unit]

private[be] object SessionStore:
  def create(
      sessionKey: String,
      user: SessionUser,
      createdAt: Instant,
      expiresAt: Instant
  ): ZIO[SessionStore, Throwable, Unit] =
    ZIO.serviceWithZIO[SessionStore](_.create(sessionKey, user, createdAt, expiresAt))

  def find(sessionKey: String, now: Instant): ZIO[SessionStore, Throwable, Option[SessionUser]] =
    ZIO.serviceWithZIO[SessionStore](_.find(sessionKey, now))

  def findForRefresh(sessionKey: String): ZIO[SessionStore, Throwable, Option[SessionUser]] =
    ZIO.serviceWithZIO[SessionStore](_.findForRefresh(sessionKey))

  def renew(sessionKey: String, expiresAt: Instant): ZIO[SessionStore, Throwable, Unit] =
    ZIO.serviceWithZIO[SessionStore](_.renew(sessionKey, expiresAt))

  def invalidate(sessionKey: String): ZIO[SessionStore, Throwable, Unit] =
    ZIO.serviceWithZIO[SessionStore](_.invalidate(sessionKey))

  val live: ZLayer[Firestore, Nothing, SessionStore] = ZLayer.fromFunction(Live(_))

  private final case class Live(firestore: Firestore) extends SessionStore:
    override def create(
        sessionKey: String,
        user: SessionUser,
        createdAt: Instant,
        expiresAt: Instant
    ): Task[Unit] =
      for
        fields <- ZIO.attempt(documentFields(sessionKey, user, createdAt, expiresAt))
        _ <- GoogleFuture.fromApiFuture(
          firestore.collection(SessionSchema.collection).document(sessionKey).create(fields.asJava)
        )
      yield ()

    override def find(sessionKey: String, now: Instant): Task[Option[SessionUser]] =
      read(sessionKey).map(_.collect { case (user, expiresAt) if now.isBefore(expiresAt) => user })

    override def findForRefresh(sessionKey: String): Task[Option[SessionUser]] =
      read(sessionKey).map(_.map(_._1))

    override def renew(sessionKey: String, expiresAt: Instant): Task[Unit] =
      normalized(sessionKey) match
        case None      => ZIO.fail(new IllegalArgumentException("The session key is empty"))
        case Some(key) =>
          GoogleFuture
            .fromApiFuture(
              firestore
                .collection(SessionSchema.collection)
                .document(key)
                .update(SessionSchema.expiresAt, timestamp(expiresAt))
            )
            .unit

    private def read(sessionKey: String): Task[Option[(SessionUser, Instant)]] =
      normalized(sessionKey) match
        case None      => ZIO.none
        case Some(key) =>
          GoogleFuture
            .fromApiFuture(firestore.collection(SessionSchema.collection).document(key).get())
            .map: snapshot =>
              Option
                .when(snapshot.exists)(snapshot)
                .flatMap: session =>
                  val storedKey = normalized(session.getString("sessionKey"))
                  val expiresAt = Option(session.getTimestamp(SessionSchema.expiresAt)).map(_.toDate.toInstant)
                  val email = normalized(session.getString("email"))
                  val name = normalized(session.getString("name"))
                  val refreshToken = normalized(session.getString("refreshToken"))
                  val accessTokenForRevocation = normalized(session.getString("accessTokenForRevocation"))
                  if storedKey.contains(key) then
                    for
                      address <- email
                      expiry <- expiresAt
                    yield SessionUser(
                      address,
                      name.getOrElse(address),
                      refreshToken,
                      accessTokenForRevocation
                    ) -> expiry
                  else None

    override def invalidate(sessionKey: String): Task[Unit] =
      normalized(sessionKey) match
        case None      => ZIO.fail(new IllegalArgumentException("The session key is empty"))
        case Some(key) =>
          GoogleFuture.fromApiFuture(firestore.collection(SessionSchema.collection).document(key).delete()).unit

  private[auth] def documentFields(
      sessionKey: String,
      user: SessionUser,
      createdAt: Instant,
      expiresAt: Instant
  ): Map[String, AnyRef] =
    val session = Map[String, AnyRef](
      "email" -> user.email,
      "name" -> user.name,
      "createdAt" -> timestamp(createdAt),
      "sessionKey" -> sessionKey,
      SessionSchema.expiresAt -> timestamp(expiresAt)
    )
    // At most one revocation credential exists; the absent one is omitted rather than stored as an empty field.
    val revocation =
      user.refreshToken.map(token => "refreshToken" -> token) ++
        user.accessTokenForRevocation.map(token => "accessTokenForRevocation" -> token)
    session ++ revocation

  private def normalized(value: String): Option[String] =
    Option(value).map(_.trim).filter(_.nonEmpty)

  private def timestamp(instant: Instant): Timestamp =
    Timestamp.ofTimeSecondsAndNanos(instant.getEpochSecond, instant.getNano)
