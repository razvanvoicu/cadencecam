package sgrv.be.auth

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.Files
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*
import zio.{Runtime, Task, UIO, Unsafe, ZIO, ZLayer}

class InvitationCliSuite extends munit.FunSuite:
  private val environment = Map("GCP_PROJECT_ID" -> "project-one", "FIRESTORE_DATABASE_ID" -> "(default)")

  private def run[A](effect: Task[A]): A =
    Unsafe.unsafe { implicit unsafe => Runtime.default.unsafe.run(effect).getOrThrowFiberFailure() }

  test("parses and normalizes a production creation command"):
    val directory = Files.createTempDirectory("cadencecam-cli-arguments")
    try
      val output = directory.resolve("review.json")
      val parsed = InvitationCliArguments.parse(
        List("--prefix", " Apple-Review ", "--count", "10", "--output", output.toString),
        environment
      )

      assertEquals(
        parsed,
        Right(
          InvitationCliCommand.Create(
            InvitationCliConfig("apple-review", 10, output.toAbsolutePath, "project-one", "(default)")
          )
        )
      )
    finally
      val _ = Files.deleteIfExists(directory)

  test("refuses accidental emulator writes unless they are explicitly allowed"):
    val directory = Files.createTempDirectory("cadencecam-cli-emulator")
    try
      val arguments = List("--prefix", "review", "--count", "1", "--output", directory.resolve("one.json").toString)
      val withEmulator = environment.updated("FIRESTORE_EMULATOR_HOST", "localhost:8880")

      assert(InvitationCliArguments.parse(arguments, withEmulator).left.exists(_.getMessage.contains("--allow-emulator")))
      assert(InvitationCliArguments.parse(arguments :+ "--allow-emulator", withEmulator).isRight)
    finally
      val _ = Files.deleteIfExists(directory)

  test("validates all arguments before connecting to Firestore"):
    val directory = Files.createTempDirectory("cadencecam-cli-validation")
    val existing = directory.resolve("existing.json")
    val _ = Files.writeString(existing, "do not replace")
    try
      val invalidCount = InvitationCliArguments.parse(
        List("--prefix", "review", "--count", "21", "--output", directory.resolve("new.json").toString),
        environment
      )
      val existingOutput = InvitationCliArguments.parse(
        List("--prefix", "review", "--count", "1", "--output", existing.toString),
        environment
      )

      assert(invalidCount.left.exists(_.getMessage.contains("from 1 to 20")))
      assert(existingOutput.left.exists(_.getMessage.contains("already exists")))
      assertEquals(Files.readString(existing), "do not replace")
    finally
      val _ = Files.deleteIfExists(existing)
      val _ = Files.deleteIfExists(directory)

  test("uses the production invitation secret policy"):
    val generated = new ConcurrentLinkedQueue[String](
      Seq("SuffixA", "AccessA", "AccountA", "SuffixB", "AccessB", "AccountB").asJava
    )
    val tokens = new TokenGenerator:
      override def generate(bytes: Int): UIO[String] =
        ZIO.succeed:
          assert(Set(5, 18, 24).contains(bytes))
          generated.remove()

    val invitations = run(InvitationProvisioning.generate(" Review ", 2).provideLayer(ZLayer.succeed(tokens)))

    assertEquals(
      invitations,
      Seq(
        NewInvitation("review-suffixa", "AccessA", "invitation:AccountA", "review"),
        NewInvitation("review-suffixb", "AccessB", "invitation:AccountB", "review")
      )
    )
    assert(generated.isEmpty)

  test("stages complete JSON with owner-only permissions and publishes without replacing files"):
    val directory = Files.createTempDirectory("cadencecam-cli-output")
    val output = directory.resolve("invitations.json")
    try
      val staged = run(StagedCredentials.create(output, "{\"secret\":\"one-time\"}"))

      assert(!Files.exists(output))
      assertEquals(Files.readString(staged.temporary).trim, "{\"secret\":\"one-time\"}")
      if Files.getFileStore(staged.temporary).supportsFileAttributeView("posix") then
        assertEquals(
          Files.getPosixFilePermissions(staged.temporary),
          PosixFilePermissions.fromString("rw-------")
        )

      run(staged.publish)
      assertEquals(Files.readString(output).trim, "{\"secret\":\"one-time\"}")
      assert(!Files.exists(staged.temporary))
    finally
      val _ = Files.deleteIfExists(output)
      val leftovers = Files.list(directory)
      try leftovers.forEach(path => { val _ = Files.deleteIfExists(path) })
      finally leftovers.close()
      val _ = Files.deleteIfExists(directory)
