package sgrv.be.sessions

import java.nio.charset.StandardCharsets.UTF_8
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import zio.{Cause, System, ZIO}

/** Turns an account's email into the name of its document, without putting the email in the path.
  *
  * Keyed rather than a plain digest. An unkeyed hash of an email is not an anonymisation: the space of addresses is
  * small enough to walk, so anyone holding the database could confirm whether a given person has an account by hashing
  * their address and looking. A secret the database does not contain removes that, and the key lives where the other
  * secrets live -- outside the repository, handed to the running service as an environment variable.
  *
  * Fails closed. Without the key there is no safe name to write under, so the session simply does not open; inventing
  * an unkeyed fallback would silently give up the property this exists for.
  */
private[sessions] object AccountKey:
  val variable = "ACCOUNT_KEY"

  private val algorithm = "HmacSHA256"

  /** The document name for an account, or nothing when no key is configured. */
  def of(email: String): ZIO[Any, Nothing, Option[String]] =
    System
      .env(variable)
      .catchAll: error =>
        ZIO.logErrorCause(s"Could not read $variable", Cause.fail(error)).as(None)
      .flatMap:
        case Some(secret) if secret.trim.nonEmpty => ZIO.succeed(Some(name(email, secret.trim)))
        case _ =>
          ZIO
            .logWarning(s"$variable is not configured; counting sessions cannot be opened")
            .as(None)

  /** The keyed digest itself, lower-case hex. Separated from reading the environment so it can be tested directly. */
  private[sessions] def name(email: String, secret: String): String =
    val mac = Mac.getInstance(algorithm)
    mac.init(SecretKeySpec(secret.getBytes(UTF_8), algorithm))
    // Normalised, because an address that differs only in case or in surrounding space is the same account.
    val digest = mac.doFinal(email.trim.toLowerCase.getBytes(UTF_8))
    digest.map(byte => f"${byte & 0xff}%02x").mkString
