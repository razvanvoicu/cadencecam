package sgrv.be.sheets

import sgrv.api.{SpreadsheetContentResponse, UpsertSpreadsheetResponse}
import zio.{Runtime, Task, Unsafe}
import zio.http.Status
import zio.json.*

class SheetsRoutesSuite extends munit.FunSuite:

  private def run[A](effect: Task[A]): A =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(effect).getOrThrowFiberFailure()
    }

  test("decodes and validates the upsert request with standard JSON escapes"):
    assertEquals(SheetsRoutes.decodeName("""{"name":"My Sheet"}"""), Right("My Sheet"))
    assertEquals(
      SheetsRoutes.decodeName("""{"name":"Line\nUnicode \u0041 and quote \""}"""),
      Right("Line\nUnicode A and quote \"")
    )
    assert(SheetsRoutes.decodeName("""{"name":"  "}""").isLeft)
    assert(SheetsRoutes.decodeName("{}").isLeft)
    assert(SheetsRoutes.decodeName("\"name\":\"accepted by the old regex\"").isLeft)
    assert(SheetsRoutes.decodeName("""{"name":"Sheet","unexpected":true}""").isLeft)
    assert(SheetsRoutes.decodeName("""{"name":"trailing comma",}""").isLeft)
    assert(SheetsRoutes.decodeName("not json").isLeft)

  test("encodes spreadsheet responses with the shared JSON codecs"):
    assertEquals(SpreadsheetContentResponse(Seq.empty).toJson, """{"rows":[]}""")
    assertEquals(
      SpreadsheetContentResponse(Seq(Seq("2026-08-08T00:00:00Z", "Mozilla/5.0 \"test\""))).toJson,
      """{"rows":[["2026-08-08T00:00:00Z","Mozilla/5.0 \"test\""]]}"""
    )
    assertEquals(
      SpreadsheetContentResponse(Seq(Seq("a", "b"), Seq("c", "d"))).toJson,
      """{"rows":[["a","b"],["c","d"]]}"""
    )
    assertEquals(UpsertSpreadsheetResponse("sheet-123").toJson, """{"spreadsheetId":"sheet-123"}""")

  test("classifies unsuccessful Google responses as domain errors"):
    val unauthorized = SheetsClient.unsuccessfulResponse("reading a sheet", 401, "invalid token")
    val forbidden = SheetsClient.unsuccessfulResponse("reading a sheet", 403, "missing scope")
    val rateLimited = SheetsClient.unsuccessfulResponse("reading a sheet", 429, "slow down")
    val unavailable = SheetsClient.unsuccessfulResponse("reading a sheet", 503, "backend down")
    val unexpected = SheetsClient.unsuccessfulResponse("reading a sheet", 400, "bad request")

    assert(unauthorized.isInstanceOf[SheetsError.Unauthenticated])
    assert(forbidden.isInstanceOf[SheetsError.Unauthenticated])
    assert(rateLimited.isInstanceOf[SheetsError.GoogleUnavailable])
    assert(unavailable.isInstanceOf[SheetsError.GoogleUnavailable])
    assert(unexpected.isInstanceOf[SheetsError.UnexpectedGoogleResponse])

  test("returns stable responses without exposing Google response bodies"):
    val upstreamBody = """{"error":{"message":"private upstream detail"}}"""
    val error = SheetsError.UnexpectedGoogleResponse(
      "creating a spreadsheet",
      Some(400),
      Some(upstreamBody)
    )
    val response = SheetsRoutes.responseFor(error)
    val body = run(response.body.asString)

    assertEquals(response.status, Status.BadGateway)
    assertEquals(body, "Google returned an unexpected response.")
    assert(!body.contains("private upstream detail"))
    assert(error.diagnostic.contains(upstreamBody))

  test("maps each domain error to a stable client status and message"):
    val cases = Seq(
      SheetsError.InvalidInput("Missing name") ->
        (Status.BadRequest, "Missing name"),
      SheetsError.Unauthenticated(Some("private authorization detail")) ->
        (Status.Forbidden, "Google authorization is missing or expired; sign out and sign in again."),
      SheetsError.GoogleUnavailable("reading a sheet", Some("private outage detail")) ->
        (Status.ServiceUnavailable, "Google services are temporarily unavailable."),
      SheetsError.UnexpectedGoogleResponse("reading a sheet", Some(418), Some("private response")) ->
        (Status.BadGateway, "Google returned an unexpected response.")
    )

    cases.foreach { entry =>
      val error = entry._1
      val expectedStatus = entry._2._1
      val expectedBody = entry._2._2
      val response = SheetsRoutes.responseFor(error)
      assertEquals(response.status, expectedStatus)
      assertEquals(run(response.body.asString), expectedBody)
    }
