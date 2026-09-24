package sgrv.be.auth

import zio.ZIO

/** Generates the secret material for a batch of invitations.
  *
  * Both the owner-only HTTP route and the direct Firestore command-line tool use this code. Keeping generation here
  * prevents the two administration paths from quietly acquiring different username, access-code, or account-id
  * policies.
  */
private[auth] object InvitationProvisioning:
  val MaximumBatchSize = 20

  def generate(prefix: String, count: Int): ZIO[TokenGenerator, Throwable, Seq[NewInvitation]] =
    for
      normalizedPrefix <- ZIO.attempt(InvitationAccounts.validateUsername(prefix))
      _ <- validateCount(count)
      invitations <- ZIO.foreach(1 to count): _ =>
        for
          suffix <- TokenGenerator.generate(5)
          code <- TokenGenerator.generate(24)
          account <- TokenGenerator.generate(18)
        yield NewInvitation(
          username = s"$normalizedPrefix-${suffix.toLowerCase}",
          accessCode = code,
          accountId = s"invitation:$account",
          displayName = normalizedPrefix
        )
    yield invitations

  private def validateCount(count: Int): ZIO[Any, IllegalArgumentException, Unit] =
    ZIO.unless(count >= 1 && count <= MaximumBatchSize)(
      ZIO.fail(new IllegalArgumentException(s"Invitation count must be from 1 to $MaximumBatchSize"))
    ).unit
