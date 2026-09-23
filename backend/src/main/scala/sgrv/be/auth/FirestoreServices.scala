package sgrv.be.auth

import com.google.cloud.Timestamp
import com.google.cloud.firestore.Firestore
import java.time.Instant
import scala.jdk.CollectionConverters.*
import sgrv.be.sessions.WorkoutSessions
import sgrv.be.store.GoogleFuture
import zio.{Cause, Task, ZIO, ZLayer}

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

  /** The expiry attached to an application credential, including one close enough to expiry that a Counter lease must
    * not outlive it. Implementations used by older plugins may omit this optional capability.
    */
  def expiresAt(sessionKey: String): Task[Option[Instant]] =
    ZIO.succeed(sessionKey).as(Option.empty[Instant])

  /** Reads a retained session record without applying its active-session expiry. Kept for administrative/internal
    * inspection only: interactive renewal must use [[find]] so a sleeping client cannot resurrect an expired login.
    * Firestore TTL may already have removed the record.
    */
  def findForRefresh(sessionKey: String): Task[Option[SessionUser]]

  /** Extends the active-session expiry only after Google has accepted the stored refresh token. */
  def renew(sessionKey: String, expiresAt: Instant): Task[Unit]

  /** Invalidates only the application credential presented by one device. This is deliberately separate from the
    * account-wide logout below: a device which declines the Counter role must be able to leave without interrupting the
    * Counter or dashboards already using the account.
    */
  def invalidate(sessionKey: String): Task[Unit]

  /** Invalidates every application credential currently belonging to the account. Logout is account-wide: a phone,
    * tablet, and browser signed into the same email must not retain contradictory authentication states.
    */
  def invalidateAll(accountId: String): Task[Unit]

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

  def expiresAt(sessionKey: String): ZIO[SessionStore, Throwable, Option[Instant]] =
    ZIO.serviceWithZIO[SessionStore](_.expiresAt(sessionKey))

  def renew(sessionKey: String, expiresAt: Instant): ZIO[SessionStore, Throwable, Unit] =
    ZIO.serviceWithZIO[SessionStore](_.renew(sessionKey, expiresAt))

  def invalidate(sessionKey: String): ZIO[SessionStore, Throwable, Unit] =
    ZIO.serviceWithZIO[SessionStore](_.invalidate(sessionKey))

  def invalidateAll(accountId: String): ZIO[SessionStore, Throwable, Unit] =
    ZIO.serviceWithZIO[SessionStore](_.invalidateAll(accountId))

  val live: ZLayer[Firestore & WorkoutSessions, Nothing, SessionStore] =
    ZLayer.fromFunction(Live(_, _))

  private final case class Live(firestore: Firestore, workouts: WorkoutSessions) extends SessionStore:
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
      read(sessionKey).flatMap:
        case Some((user, expiresAt)) if now.isBefore(expiresAt) => ZIO.some(user)
        case Some((user, _))                                    =>
          // Expiry has no explicit Logout request to perform the workout-side cleanup. The first request made by the
          // expired client therefore closes any workout it owned and removes its Counter lease before authentication
          // answers 401. This makes an active Counter stop and sign out together instead of leaving a ghost owner.
          workouts
            .closeOnSessionExpiry(user.account, sessionKey, now)
            .catchAll(error =>
              ZIO.logErrorCause("Could not close the Counter owned by an expired session", Cause.fail(error))
            )
            .as(None)
        case None => ZIO.none

    override def expiresAt(sessionKey: String): Task[Option[Instant]] =
      read(sessionKey).map(_.map(_._2))

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

    override def invalidate(sessionKey: String): Task[Unit] =
      normalized(sessionKey) match
        case None      => ZIO.fail(new IllegalArgumentException("The session key is empty"))
        case Some(key) =>
          GoogleFuture
            .fromApiFuture(firestore.collection(SessionSchema.collection).document(key).delete())
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
                  val identityProvider = normalized(session.getString("identityProvider"))
                    .flatMap(IdentityProvider.fromStored)
                    .getOrElse(IdentityProvider.Google)
                  val providerSubject = normalized(session.getString("providerSubject"))
                  val accountId = normalized(session.getString("accountId"))
                  if storedKey.contains(key) then
                    for
                      address <- email
                      expiry <- expiresAt
                    yield SessionUser(
                      address,
                      name.getOrElse(address),
                      refreshToken,
                      accessTokenForRevocation,
                      identityProvider,
                      providerSubject,
                      accountId
                    ) -> expiry
                  else None

    override def invalidateAll(accountId: String): Task[Unit] =
      normalized(accountId) match
        case None          => ZIO.fail(new IllegalArgumentException("The account email is empty"))
        case Some(identity) =>
          // The email query retains compatibility with sessions created before accountId was stored. New sessions are
          // found by accountId even when Apple supplies a private relay address or an invitation has no email at all.
          ZIO
            .foreach(Seq("accountId", "email")): field =>
              GoogleFuture.fromApiFuture(
                firestore.collection(SessionSchema.collection).whereEqualTo(field, identity).get()
              )
            .map(_.flatMap(_.getDocuments.asScala).distinctBy(_.getId))
            .flatMap: found =>
              ZIO.foreachDiscard(found.grouped(400).toSeq): documents =>
                val batch = firestore.batch()
                documents.foreach(document => { val _ = batch.delete(document.getReference) })
                GoogleFuture.fromApiFuture(batch.commit()).unit

  private[auth] def documentFields(
      sessionKey: String,
      user: SessionUser,
      createdAt: Instant,
      expiresAt: Instant
  ): Map[String, AnyRef] =
    val session = Map[String, AnyRef](
      "email" -> user.email,
      "name" -> user.name,
      "accountId" -> user.account,
      "identityProvider" -> IdentityProvider.stored(user.identityProvider),
      "createdAt" -> timestamp(createdAt),
      "sessionKey" -> sessionKey,
      SessionSchema.expiresAt -> timestamp(expiresAt)
    )
    // At most one revocation credential exists; the absent one is omitted rather than stored as an empty field.
    val revocation =
      user.refreshToken.map(token => "refreshToken" -> token) ++
        user.accessTokenForRevocation.map(token => "accessTokenForRevocation" -> token) ++
        user.providerSubject.map(subject => "providerSubject" -> subject)
    session ++ revocation

  private def normalized(value: String): Option[String] =
    Option(value).map(_.trim).filter(_.nonEmpty)

  private def timestamp(instant: Instant): Timestamp =
    Timestamp.ofTimeSecondsAndNanos(instant.getEpochSecond, instant.getNano)
