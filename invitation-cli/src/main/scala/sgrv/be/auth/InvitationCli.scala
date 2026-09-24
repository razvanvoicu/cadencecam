package sgrv.be.auth

import com.google.cloud.firestore.{Firestore, FirestoreOptions}
import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption, StandardOpenOption}
import scala.annotation.tailrec
import scala.util.Try
import sgrv.api.{InvitationCredential, InvitationCredentials}
import zio.json.*
import zio.{Clock, Console, ExitCode, Scope, System, Task, UIO, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer}

/** Creates invitation identities directly in Firestore without calling the deployed web service.
  *
  * The command deliberately stages the one-time plaintext credentials in an owner-readable temporary file before
  * committing the batch. If Firestore rejects the batch, the temporary file is removed. After a successful commit it
  * is atomically renamed to the requested path, so the operator normally sees either no output file or the complete
  * credential set.
  */
object InvitationCli extends ZIOAppDefault:
  override def run: ZIO[ZIOAppArgs, Any, Any] =
    val executed = for
      arguments <- getArgs
      environment <- System.envs
      command <- ZIO.fromEither(InvitationCliArguments.parse(arguments.toList, environment))
      _ <- command match
        case InvitationCliCommand.Help           => Console.printLine(InvitationCliArguments.usage)
        case InvitationCliCommand.Create(config) => create(config)
    yield ()

    executed.foldZIO(
      error => Console.printLineError(s"Invitation creation failed: ${message(error)}").as(ExitCode.failure),
      _ => ZIO.succeed(ExitCode.success)
    )

  private def create(config: InvitationCliConfig): Task[Unit] = ZIO.scoped:
    for
      seeds <- InvitationProvisioning.generate(config.prefix, config.count).provideLayer(TokenGenerator.live)
      credentials = InvitationCredentials(
        seeds.map(invitation => InvitationCredential(invitation.username, invitation.accessCode))
      )
      staged <- StagedCredentials.create(config.output, credentials.toJsonPretty)
      firestore <- openFirestore(config.projectId, config.databaseId)
      now <- Clock.instant
      _ <- InvitationAccounts
        .create(seeds, now)
        .provideLayer(ZLayer.succeed(firestore) >>> InvitationAccounts.live)
        .onError(_ => staged.discard)
      _ <- staged.publish.catchAll: error =>
        ZIO.fail(
          new IOException(
            s"Firestore contains the new invitations, but the credential file could not be moved to " +
              s"${config.output}. Recover it from ${staged.temporary}.",
            error
          )
        )
      _ <- Console.printLine(
        s"Created ${credentials.invitations.size} invitation(s) in ${config.projectId}/${config.databaseId}. " +
          s"Credentials: ${config.output}"
      )
    yield ()

  private def openFirestore(projectId: String, databaseId: String): ZIO[Scope, Throwable, Firestore] =
    ZIO.acquireRelease(
      ZIO.attemptBlocking:
        FirestoreOptions.newBuilder().setProjectId(projectId).setDatabaseId(databaseId).build().getService
    )(firestore => ZIO.attemptBlocking(firestore.close()).ignore)

  private def message(error: Throwable): String =
    Iterator
      .iterate(error)(_.getCause)
      .takeWhile(_ != null)
      .map(_.getMessage)
      .find(value => value != null && value.trim.nonEmpty)
      .getOrElse(error.getClass.getSimpleName)

private[auth] enum InvitationCliCommand:
  case Help
  case Create(config: InvitationCliConfig)

private[auth] final case class InvitationCliConfig(
    prefix: String,
    count: Int,
    output: Path,
    projectId: String,
    databaseId: String
)

private[auth] object InvitationCliArguments:
  val usage: String =
    """Usage:
      |  sbt 'createInvitations --prefix NAME --count NUMBER --output FILE [--allow-emulator]'
      |
      |Creates 1–20 invitation users directly in the configured Firestore database. FILE must not already exist.
      |The file is created with owner-only permissions and is the only place plaintext access codes are retained.
      |
      |Options:
      |  --prefix NAME       Username prefix (3–48 letters, numbers, dashes or underscores)
      |  --count NUMBER      Number of invitations to create (1–20)
      |  --output FILE       New JSON file that receives the one-time credentials
      |  --allow-emulator    Permit FIRESTORE_EMULATOR_HOST; omitted by default to catch accidental emulator writes
      |  --help              Show this help
      |""".stripMargin

  private final case class Parsed(
      prefix: Option[String] = None,
      count: Option[Int] = None,
      output: Option[Path] = None,
      allowEmulator: Boolean = false
  )

  def parse(arguments: List[String], environment: Map[String, String]): Either[IllegalArgumentException, InvitationCliCommand] =
    if arguments == List("--help") || arguments == List("-h") then Right(InvitationCliCommand.Help)
    else
      for
        parsed <- options(arguments, Parsed())
        prefix <- parsed.prefix.toRight(error("Missing --prefix"))
        count <- parsed.count.toRight(error("Missing --count"))
        output <- parsed.output.toRight(error("Missing --output"))
        project <- requiredEnvironment(environment, "GCP_PROJECT_ID")
        database <- requiredEnvironment(environment, "FIRESTORE_DATABASE_ID")
        _ <- Either.cond(
          parsed.allowEmulator || environment.get("FIRESTORE_EMULATOR_HOST").forall(_.trim.isEmpty),
          (),
          error("FIRESTORE_EMULATOR_HOST is set; pass --allow-emulator only when this is intentional")
        )
        normalizedPrefix <- Try(InvitationAccounts.validateUsername(prefix)).toEither.left.map(asArgumentError)
        _ <- Either.cond(
          count >= 1 && count <= InvitationProvisioning.MaximumBatchSize,
          (),
          error(s"Invitation count must be from 1 to ${InvitationProvisioning.MaximumBatchSize}")
        )
        normalizedOutput <- validateOutput(output)
      yield InvitationCliCommand.Create(
        InvitationCliConfig(normalizedPrefix, count, normalizedOutput, project, database)
      )

  @tailrec
  private def options(
      remaining: List[String],
      parsed: Parsed
  ): Either[IllegalArgumentException, Parsed] = remaining match
    case Nil => Right(parsed)
    case "--prefix" :: value :: tail if parsed.prefix.isEmpty => options(tail, parsed.copy(prefix = Some(value)))
    case "--count" :: value :: tail if parsed.count.isEmpty =>
      Try(value.toInt).toEither match
        case Right(count) => options(tail, parsed.copy(count = Some(count)))
        case Left(_)      => Left(error(s"Invalid --count '$value'"))
    case "--output" :: value :: tail if parsed.output.isEmpty =>
      Try(Path.of(value)).toEither match
        case Right(path) => options(tail, parsed.copy(output = Some(path)))
        case Left(_)     => Left(error(s"Invalid --output '$value'"))
    case "--allow-emulator" :: tail if !parsed.allowEmulator => options(tail, parsed.copy(allowEmulator = true))
    case option :: Nil if Set("--prefix", "--count", "--output").contains(option) =>
      Left(error(s"Missing value after $option"))
    case option :: _ => Left(error(s"Unknown or repeated option '$option'"))

  private def requiredEnvironment(
      environment: Map[String, String],
      name: String
  ): Either[IllegalArgumentException, String] =
    environment.get(name).map(_.trim).filter(_.nonEmpty).toRight(error(s"Environment variable $name is missing"))

  private def validateOutput(path: Path): Either[IllegalArgumentException, Path] =
    val normalized = path.toAbsolutePath.normalize
    val parent = Option(normalized.getParent)
    if Files.exists(normalized) then Left(error(s"Output file already exists: $normalized"))
    else if parent.forall(value => !Files.isDirectory(value)) then
      Left(error(s"Output directory does not exist: ${parent.fold(normalized)(identity)}"))
    else Right(normalized)

  private def asArgumentError(error: Throwable): IllegalArgumentException = error match
    case argument: IllegalArgumentException => argument
    case other                              => new IllegalArgumentException(other.getMessage, other)

  private def error(message: String): IllegalArgumentException =
    new IllegalArgumentException(s"$message\n\n$usage")

private[auth] final case class StagedCredentials(temporary: Path, destination: Path):
  def discard: UIO[Unit] = ZIO.attemptBlocking(Files.deleteIfExists(temporary)).unit.ignore

  def publish: Task[Unit] = ZIO.attemptBlocking:
    try Files.move(temporary, destination, StandardCopyOption.ATOMIC_MOVE)
    catch case _: AtomicMoveNotSupportedException => Files.move(temporary, destination)
  .unit

private[auth] object StagedCredentials:
  private val OwnerReadWrite = PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))

  def create(destination: Path, json: String): Task[StagedCredentials] = ZIO.attemptBlocking:
    val parent = destination.getParent
    val prefix = s".${destination.getFileName}."
    val temporary =
      try Files.createTempFile(parent, prefix, ".tmp", OwnerReadWrite)
      catch
        case _: UnsupportedOperationException =>
          val value = Files.createTempFile(parent, prefix, ".tmp")
          val file = value.toFile
          val _ = file.setReadable(false, false)
          val _ = file.setWritable(false, false)
          val _ = file.setExecutable(false, false)
          val _ = file.setReadable(true, true)
          val _ = file.setWritable(true, true)
          value
    try
      Files.writeString(temporary, json + java.lang.System.lineSeparator(), UTF_8, StandardOpenOption.TRUNCATE_EXISTING)
      StagedCredentials(temporary, destination)
    catch
      case error: Throwable =>
        val _ = Files.deleteIfExists(temporary)
        throw error
