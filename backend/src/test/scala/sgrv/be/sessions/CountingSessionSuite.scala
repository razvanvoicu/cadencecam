package sgrv.be.sessions

import sgrv.api.{CountingSession, RepProgress}
import sgrv.be.auth.SessionUser
import sgrv.be.core.{CapabilityRegistry, CurrentUserContributors, PluginStatus, RequestContext, RouteDiscovery}
import zio.*
import zio.http.{Cookie, Request, URL}
import zio.json.*
import zio.json.ast.Json

class CountingSessionSuite extends munit.FunSuite:

  private def run[A](effect: ZIO[Any, Any, A]): A =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe
        .run(effect.mapError(error => new RuntimeException(error.toString)))
        .getOrThrowFiberFailure()
    }

  private def context(sessionKey: Option[String]): RequestContext.Authenticated =
    val base = Request.get(URL.decode("/me").toOption.get)
    val request = sessionKey.fold(base)(key => base.addCookie(Cookie.Request("session", key)))
    RequestContext.Authenticated(request, SessionUser("jane@example.com", "Jane"))

  private val details =
    CurrentUserContributors.fromStatuses(
      Seq(
        CurrentUserContributors
          .activate(CountingSessionContributor, "CountingSessionContributor", CapabilityRegistry.empty)
      )
    )

  test("the id is a stable, opaque derivation of the session key"):
    val id = CountingSessionListener.documentId("session-key")

    assertEquals(id, CountingSessionListener.documentId("session-key"))
    assertNotEquals(id, CountingSessionListener.documentId("another-key"))
    assertEquals(id.length, 64)
    assert(id.forall(character => character.isDigit || ('a' to 'f').contains(character)), id)
    // The opaque session key must not be recoverable from, or present in, the derived id.
    assert(!id.contains("session-key"))

  test("the contributor reports the id derived from the session cookie"):
    assertEquals(
      run(details.extras(context(Some("session-key")))),
      Map(CountingSession.Key -> Json.Obj("sessionId" -> Json.Str(CountingSessionListener.documentId("session-key"))))
    )

  test("no session cookie means no counting session is named"):
    assertEquals(run(details.extras(context(None))), Map.empty[String, Json])

  test("a progress report is read from the body"):
    assertEquals(CountingSessionProgress.reps(RepProgress(57).toJson), Right(57))
    assertEquals(CountingSessionProgress.reps("""{"reps":0}"""), Right(0))

  test("a report that is not a rep count is rejected rather than guessed at"):
    assert(CountingSessionProgress.reps("").isLeft)
    assert(CountingSessionProgress.reps("57").isLeft)
    assert(CountingSessionProgress.reps("""{"count":57}""").isLeft)
    // The shared type forbids extra fields, so a caller and this route cannot silently disagree about the shape.
    assert(CountingSessionProgress.reps("""{"reps":57,"device":"phone"}""").isLeft)

  test("a negative total is rejected rather than clamped, since nothing legitimate produces one"):
    assertEquals(CountingSessionProgress.reps("""{"reps":-1}"""), Left("Negative rep count -1"))

  test("the route writes to the session named by the cookie, not by the caller"):
    // The document id is never accepted from the request body: it is derived from the browser's own session key,
    // so a client can report its count without being able to choose whose count it is.
    val request = context(Some("session-key")).request

    assertEquals(CountingSessionListener.documentId(request), Some(CountingSessionListener.documentId("session-key")))

  test("the progress route is discovered on the classpath, and asks the host for Firestore"):
    // Discovery is a classpath scan, so a plugin can compile perfectly and still never be reached. Against a registry
    // offering nothing, the plugin is found and set aside for want of its capabilities rather than going unseen —
    // which is the half that cannot be checked by calling the object directly.
    val statuses = run(RouteDiscovery.discover(CapabilityRegistry.empty))

    val skipped = statuses.collectFirst:
      case PluginStatus.Skipped(CountingSessionProgress.id, _, missing) => missing.map(_.id).toSet

    assertEquals(skipped, Some(Set("firestore", "session-store")))
