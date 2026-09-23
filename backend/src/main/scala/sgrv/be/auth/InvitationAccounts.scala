package sgrv.be.auth

import com.google.cloud.Timestamp
import com.google.cloud.firestore.{FieldValue, Firestore}
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.time.Instant
import scala.jdk.CollectionConverters.*
import sgrv.api.InvitationCredential
import sgrv.be.store.GoogleFuture
import zio.{Task, ZIO, ZLayer}

private[auth] final case class NewInvitation(
    username: String,
    accessCode: String,
    accountId: String,
    displayName: String
)

/** Persistent invitation identities.
  *
  * The plaintext access code exists only in the creation response. Firestore holds its SHA-256 digest; the codes this
  * application creates carry 192 random bits, so an offline database reader cannot feasibly recover one. A deleted
  * invitation remains as a tombstone: its old credentials can never authenticate again and the same username cannot be
  * accidentally recreated for somebody else.
  */
trait InvitationAccounts:
  def create(invitations: Seq[NewInvitation], at: Instant): Task[Seq[InvitationCredential]]
  def authenticate(username: String, accessCode: String, at: Instant): Task[SessionUser]
  def isActive(subject: String): Task[Boolean]
  def revoke(username: String, at: Instant): Task[Unit]
  def delete(subject: String, at: Instant): Task[Unit]

private[be] object InvitationAccounts:
  private val Collection = "Invitations"
  private val Available = "available"
  private val Active = "active"
  private val Revoked = "revoked"
  private val Deleted = "deleted"

  def create(
      invitations: Seq[NewInvitation],
      at: Instant
  ): ZIO[InvitationAccounts, Throwable, Seq[InvitationCredential]] =
    ZIO.serviceWithZIO[InvitationAccounts](_.create(invitations, at))

  def authenticate(
      username: String,
      accessCode: String,
      at: Instant
  ): ZIO[InvitationAccounts, Throwable, SessionUser] =
    ZIO.serviceWithZIO[InvitationAccounts](_.authenticate(username, accessCode, at))

  def isActive(subject: String): ZIO[InvitationAccounts, Throwable, Boolean] =
    ZIO.serviceWithZIO[InvitationAccounts](_.isActive(subject))

  def revoke(username: String, at: Instant): ZIO[InvitationAccounts, Throwable, Unit] =
    ZIO.serviceWithZIO[InvitationAccounts](_.revoke(username, at))

  def delete(subject: String, at: Instant): ZIO[InvitationAccounts, Throwable, Unit] =
    ZIO.serviceWithZIO[InvitationAccounts](_.delete(subject, at))

  val live: ZLayer[Firestore, Nothing, InvitationAccounts] = ZLayer.fromFunction(Live(_))

  private final case class Live(firestore: Firestore) extends InvitationAccounts:
    override def create(invitations: Seq[NewInvitation], at: Instant): Task[Seq[InvitationCredential]] =
      if invitations.isEmpty then ZIO.succeed(Seq.empty)
      else
        ZIO
          .attempt:
            invitations.foreach(invitation => validateUsername(invitation.username))
            val batch = firestore.batch()
            invitations.foreach: invitation =>
              val normalized = normalizeUsername(invitation.username)
              batch.create(
                firestore.collection(Collection).document(subject(normalized)),
                Map[String, AnyRef](
                  "username" -> normalized,
                  "displayName" -> invitation.displayName.trim,
                  "accountId" -> invitation.accountId,
                  "codeDigest" -> codeDigest(invitation.accessCode),
                  "status" -> Available,
                  "createdAt" -> timestamp(at)
                ).asJava
              )
            batch
          .flatMap(batch => GoogleFuture.fromApiFuture(batch.commit()))
          .as(
            invitations
              .map(invitation => InvitationCredential(normalizeUsername(invitation.username), invitation.accessCode))
          )

    override def authenticate(username: String, accessCode: String, at: Instant): Task[SessionUser] =
      val attempted = for
        normalized <- ZIO.attempt(validateUsername(username))
        code <- ZIO
          .fromOption(Option(accessCode).map(_.trim).filter(value => value.length >= 20 && value.length <= 200))
          .orElseFail(new IllegalArgumentException("Invalid invitation credentials"))
        reference = firestore.collection(Collection).document(subject(normalized))
        snapshot <- GoogleFuture.fromApiFuture(reference.get())
        status = Option(snapshot.getString("status")).getOrElse("")
        expected = Option(snapshot.getString("codeDigest")).getOrElse("")
        _ <- ZIO.unless(snapshot.exists && Set(Available, Active).contains(status) && sameDigest(expected, code))(
          ZIO.fail(new IllegalArgumentException("Invalid invitation credentials"))
        )
        accountId <- required(snapshot.getString("accountId"), "The invitation has no account identity")
        storedUsername <- required(snapshot.getString("username"), "The invitation has no username")
        displayName = Option(snapshot.getString("displayName")).map(_.trim).filter(_.nonEmpty).getOrElse(storedUsername)
        _ <- ZIO.when(status == Available)(
          GoogleFuture.fromApiFuture(
            reference.update(Map[String, AnyRef]("status" -> Active, "activatedAt" -> timestamp(at)).asJava)
          )
        )
      yield SessionUser(
        email = storedUsername,
        name = displayName,
        identityProvider = IdentityProvider.Invitation,
        providerSubject = Some(reference.getId),
        accountId = Some(accountId)
      )
      attempted

    override def isActive(subject: String): Task[Boolean] =
      normalizedSubject(subject) match
        case None       => ZIO.succeed(false)
        case Some(name) =>
          GoogleFuture
            .fromApiFuture(firestore.collection(Collection).document(name).get())
            .map(snapshot => snapshot.exists && Option(snapshot.getString("status")).contains(Active))

    override def revoke(username: String, at: Instant): Task[Unit] =
      for
        normalized <- ZIO.attempt(validateUsername(username))
        _ <- GoogleFuture.fromApiFuture(
          firestore
            .collection(Collection)
            .document(subject(normalized))
            .update(Map[String, AnyRef]("status" -> Revoked, "revokedAt" -> timestamp(at)).asJava)
        )
      yield ()

    override def delete(subject: String, at: Instant): Task[Unit] =
      normalizedSubject(subject) match
        case None       => ZIO.fail(new IllegalArgumentException("The invitation identity is missing"))
        case Some(name) =>
          GoogleFuture
            .fromApiFuture(
              firestore
                .collection(Collection)
                .document(name)
                .update(
                  Map[String, AnyRef](
                    "status" -> Deleted,
                    "deletedAt" -> timestamp(at),
                    "codeDigest" -> FieldValue.delete()
                  ).asJava
                )
            )
            .unit

  private[auth] def normalizeUsername(username: String): String =
    Option(username).map(_.trim.toLowerCase).getOrElse("")

  private[auth] def validateUsername(username: String): String =
    val normalized = normalizeUsername(username)
    if normalized.length < 3 || normalized.length > 48 ||
      !normalized.forall(character => character.isLetterOrDigit || character == '-' || character == '_')
    then throw new IllegalArgumentException("Invitation usernames use 3–48 letters, numbers, dashes or underscores")
    normalized

  private[auth] def subject(username: String): String = hex(digest(normalizeUsername(username)))

  private def codeDigest(code: String): String = hex(digest(code.trim))

  private def sameDigest(expected: String, suppliedCode: String): Boolean =
    val actual = codeDigest(suppliedCode)
    expected.nonEmpty && MessageDigest.isEqual(expected.getBytes(UTF_8), actual.getBytes(UTF_8))

  private def digest(value: String): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(value.getBytes(UTF_8))

  private def hex(bytes: Array[Byte]): String = bytes.map(byte => f"${byte & 0xff}%02x").mkString

  private def required(value: String, message: String): Task[String] =
    ZIO.fromOption(Option(value).map(_.trim).filter(_.nonEmpty)).orElseFail(new IllegalStateException(message))

  private def normalizedSubject(value: String): Option[String] =
    Option(value)
      .map(_.trim.toLowerCase)
      .filter(subject => subject.length == 64 && subject.forall("0123456789abcdef".contains(_)))

  private def timestamp(instant: Instant): Timestamp =
    Timestamp.ofTimeSecondsAndNanos(instant.getEpochSecond, instant.getNano)
