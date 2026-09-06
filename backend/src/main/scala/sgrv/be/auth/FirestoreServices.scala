package sgrv.be.auth

import com.google.api.gax.core.NoCredentialsProvider
import com.google.api.gax.rpc.NotFoundException
import com.google.cloud.Timestamp
import com.google.cloud.firestore.v1.{FirestoreAdminClient, FirestoreAdminSettings}
import com.google.cloud.firestore.{Firestore, FirestoreOptions}
import com.google.firestore.admin.v1.{
  CreateDatabaseRequest,
  Database,
  Field,
  GetDatabaseRequest,
  GetFieldRequest,
  UpdateFieldRequest
}
import com.google.protobuf.FieldMask
import java.time.Instant
import zio.{System, Task, ZIO, ZLayer}

private[auth] object SessionSchema:
  val collection = "Access"
  val expiresAt = "expiresAt"

private[be] trait DatabaseAdmin:
  def ensureDatabase: Task[Unit]
  def ensureSessionTtl: Task[Unit]

private[be] object DatabaseAdmin:
  def ensureDatabase: ZIO[DatabaseAdmin, Throwable, Unit] =
    ZIO.serviceWithZIO[DatabaseAdmin](_.ensureDatabase)

  def ensureSessionTtl: ZIO[DatabaseAdmin, Throwable, Unit] =
    ZIO.serviceWithZIO[DatabaseAdmin](_.ensureSessionTtl)

  val live: ZLayer[AppConfig, Throwable, DatabaseAdmin] =
    ZLayer.scoped:
      for
        config <- ZIO.service[AppConfig]
        emulatorHost <- System.env("FIRESTORE_EMULATOR_HOST").map(_.map(_.trim).filter(_.nonEmpty))
        client <- ZIO.acquireRelease(ZIO.attemptBlocking(adminClient(emulatorHost)))(value =>
          ZIO.attemptBlocking(value.close()).ignore
        )
      yield Live(config.firestore, client, emulatorHost.nonEmpty)

  // FirestoreOptions (used by SessionStore below) auto-detects FIRESTORE_EMULATOR_HOST, but the raw
  // FirestoreAdminClient GAPIC client does not; without this, ensureDatabase silently checks/creates the
  // database against real GCP instead of the emulator, leaving the emulator's copy uninitialized.
  private def adminClient(emulatorHost: Option[String]): FirestoreAdminClient =
    emulatorHost match
      case None       => FirestoreAdminClient.create()
      case Some(host) =>
        val channelProvider = FirestoreAdminSettings
          .defaultGrpcTransportProviderBuilder()
          .setChannelConfigurator(_.usePlaintext())
          .build()
        val settings = FirestoreAdminSettings
          .newBuilder()
          .setCredentialsProvider(NoCredentialsProvider.create())
          .setTransportChannelProvider(channelProvider)
          .setEndpoint(host)
          .build()
        FirestoreAdminClient.create(settings)

  private final case class Live(config: FirestoreConfig, client: FirestoreAdminClient, usingEmulator: Boolean)
      extends DatabaseAdmin:
    override def ensureDatabase: Task[Unit] =
      val name = s"projects/${config.projectId}/databases/${config.databaseId}"
      ZIO
        .attemptBlocking(client.getDatabase(GetDatabaseRequest.newBuilder().setName(name).build()))
        .unit
        .catchSome:
          case _: NotFoundException =>
            GoogleFuture
              .fromApiFuture:
                client.createDatabaseAsync(
                  CreateDatabaseRequest
                    .newBuilder()
                    .setParent(s"projects/${config.projectId}")
                    .setDatabaseId(config.databaseId)
                    .setDatabase(
                      Database
                        .newBuilder()
                        .setType(Database.DatabaseType.FIRESTORE_NATIVE)
                        .setLocationId(config.location)
                    )
                    .build()
                )
              .unit

    override def ensureSessionTtl: Task[Unit] =
      if usingEmulator then ZIO.unit
      else
        val field = ZIO
          .attemptBlocking(
            client.getField(GetFieldRequest.newBuilder().setName(sessionTtlFieldName(config)).build())
          )
          .map(Some(_))
          .catchSome:
            case _: NotFoundException => ZIO.none
        field.flatMap:
          case Some(existing) if !sessionTtlNeedsUpdate(existing) => ZIO.unit
          case _                                                  =>
            ZIO.attemptBlocking(client.updateFieldCallable().call(sessionTtlUpdateRequest(config))).unit *>
              ZIO.logInfo(s"Enabled Firestore TTL on ${SessionSchema.collection}.${SessionSchema.expiresAt}")

  private[auth] def sessionTtlFieldName(config: FirestoreConfig): String =
    s"projects/${config.projectId}/databases/${config.databaseId}/collectionGroups/${SessionSchema.collection}" +
      s"/fields/${SessionSchema.expiresAt}"

  private[auth] def sessionTtlUpdateRequest(config: FirestoreConfig): UpdateFieldRequest =
    UpdateFieldRequest
      .newBuilder()
      .setField(
        Field
          .newBuilder()
          .setName(sessionTtlFieldName(config))
          .setTtlConfig(Field.TtlConfig.getDefaultInstance)
      )
      .setUpdateMask(FieldMask.newBuilder().addPaths("ttl_config"))
      .build()

  private[auth] def sessionTtlNeedsUpdate(field: Field): Boolean =
    !field.hasTtlConfig || field.getTtlConfig.getState == Field.TtlConfig.State.NEEDS_REPAIR

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

  val live: ZLayer[AppConfig, Throwable, SessionStore] =
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
      yield Live(firestore)

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
          firestore.collection(SessionSchema.collection).document(sessionKey).create(fields)
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
  ): java.util.Map[String, AnyRef] =
    val values = new java.util.HashMap[String, AnyRef]
    values.put("email", user.email)
    values.put("name", user.name)
    values.put("createdAt", timestamp(createdAt))
    values.put("sessionKey", sessionKey)
    values.put(SessionSchema.expiresAt, timestamp(expiresAt))
    user.refreshToken.foreach(value => values.put("refreshToken", value))
    user.accessTokenForRevocation.foreach(value => values.put("accessTokenForRevocation", value))
    values

  private def normalized(value: String): Option[String] =
    Option(value).map(_.trim).filter(_.nonEmpty)

  private def timestamp(instant: Instant): Timestamp =
    Timestamp.ofTimeSecondsAndNanos(instant.getEpochSecond, instant.getNano)
