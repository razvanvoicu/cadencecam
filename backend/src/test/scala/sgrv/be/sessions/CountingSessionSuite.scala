package sgrv.be.sessions

import sgrv.api.CountingSession
import sgrv.be.auth.SessionUser
import sgrv.be.core.{CapabilityRegistry, CurrentUserContributors, RequestContext}
import zio.*
import zio.http.{Cookie, Request, URL}
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
