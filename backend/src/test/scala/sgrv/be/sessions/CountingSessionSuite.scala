package sgrv.be.sessions

import sgrv.api.{CountingSession, RepProgress, SignalTrace}
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

  test("the trace route is discovered on the classpath too"):
    // Same reason: a plugin can compile perfectly and still never be reached by the scan.
    val statuses = run(RouteDiscovery.discover(CapabilityRegistry.empty))

    val skipped = statuses.collectFirst:
      case PluginStatus.Skipped(CountingSessionTrace.id, _, missing) => missing.map(_.id).toSet

    assertEquals(skipped, Some(Set("firestore", "session-store")))

  private def trace(channels: Int = 4, samples: Int = 600, rate: Double = 10.0) =
    SignalTrace(rate, (1 to channels).map(i => s"Q$i" -> Seq.fill(samples)(120.0)).toMap, 30, "searching")

  test("a recording is accepted whole"):
    assertEquals(CountingSessionTrace.parse(trace().toJson).map(_.samples.size), Right(4))
    assertEquals(CountingSessionTrace.parse(trace().toJson).map(_.reps), Right(30))

  test("a recording of nothing is refused rather than filed"):
    assertEquals(
      CountingSessionTrace.parse(trace(channels = 0).toJson),
      Left("A trace with no channels records nothing")
    )

  test("a recording longer than this app makes is refused before it reaches the database"):
    val huge = trace(samples = CountingSessionTrace.maximumSamplesPerChannel + 1)

    assert(CountingSessionTrace.parse(huge.toJson).isLeft)
    // The buffer's own minute is comfortably inside the limit.
    assert(CountingSessionTrace.parse(trace(samples = 600).toJson).isRight)

  test("a nonsensical sample rate is refused, since a replay could not interpret it"):
    assert(CountingSessionTrace.parse(trace(rate = 0.0).toJson).isLeft)
    assert(CountingSessionTrace.parse(trace(rate = -10.0).toJson).isLeft)

  test("anything that is not a recording is refused rather than guessed at"):
    assert(CountingSessionTrace.parse("").isLeft)
    assert(CountingSessionTrace.parse("""{"sampleRateHz":10}""").isLeft)

  test("recording ids are opaque and unguessable, so one session's captures cannot be enumerated"):
    val ids = Seq.fill(50)(CountingSessionTrace.traceId())

    assertEquals(ids.distinct.size, 50)
    assert(ids.forall(id => id.length == 24 && id.forall(c => c.isDigit || ('a' to 'f').contains(c))), ids.head)
