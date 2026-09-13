package sgrv.be.sessions

import com.google.cloud.firestore.{Firestore, QueryDocumentSnapshot}
import scala.jdk.CollectionConverters.*
import sgrv.api.DiscardRun
import sgrv.be.BackendCapabilities
import sgrv.be.auth.SessionStore
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import sgrv.be.store.GoogleFuture
import zio.{Cause, ZIO}
import zio.http.{Header, Method, Request, Response, Routes, Status, handler}
import zio.json.*

/** Removes what an abandoned run filed, leaving nothing behind that reads as a completed one.
  *
  * Scoped to one run of one account, and to the two places a run writes: the events it reported, and the recordings it
  * asked the counting device to capture. The counting session itself is left alone -- it is a login rather than a run,
  * it outlives the suite, and the schema says plainly that those records are kept.
  */
private[sessions] final class DiscardedRuns(firestore: Firestore):

  /** How many documents to delete at once. Firestore caps a batch at five hundred; a suite files far fewer than that,
    * but a run abandoned after several restarts under the same id need not be assumed small.
    */
  private val BatchLimit = 400

  /** Sorts above every character a note can hold, so the half-open range ends where the prefix stops matching. */
  private val HighestCharacter = String.valueOf(Character.MAX_VALUE)

  def discard(runId: String, email: String): zio.Task[(Int, Int)] =
    for
      events <- deleteEvents(runId, email)
      traces <- deleteTraces(runId, email)
    yield (events, traces)

  private def deleteEvents(runId: String, email: String): zio.Task[Int] =
    val query = firestore
      .collection(TestEventSchema.collection)
      .whereEqualTo(TestEventSchema.userEmail, email)
      .whereEqualTo(TestEventSchema.runId, runId)
    GoogleFuture.fromApiFuture(query.get()).flatMap(found => delete(found.getDocuments.asScala.toSeq))

  /** The recordings, found by the note the bench stamped on them.
    *
    * By prefix rather than by a field of its own: the run's identity reaches a recording inside the note the capture
    * command carries, because the note is the one thing that travels from the bench to the counting device and back
    * into the stored document. A half-open range over that prefix is a single-field query, which Firestore serves
    * without an index being declared for it.
    *
    * Per session rather than across all of them: a collection-group query over `traces` would need an index this
    * project does not have, and one account's open sessions are few.
    */
  private def deleteTraces(runId: String, email: String): zio.Task[Int] =
    val prefix = s"bench $runId"
    val sessions = firestore
      .collection(CountingSessionSchema.collection)
      .whereEqualTo(CountingSessionSchema.userEmail, email)
    for
      found <- GoogleFuture.fromApiFuture(sessions.get())
      counts <- ZIO.foreach(found.getDocuments.asScala.toSeq): session =>
        val traces = session.getReference
          .collection(CountingSessionSchema.traces)
          .whereGreaterThanOrEqualTo(CountingSessionSchema.note, prefix)
          // '' sits above every character a note can hold, so the range ends where the prefix stops matching.
          .whereLessThan(CountingSessionSchema.note, prefix + HighestCharacter)
        GoogleFuture.fromApiFuture(traces.get()).flatMap(hits => delete(hits.getDocuments.asScala.toSeq))
    yield counts.sum

  private def delete(documents: Seq[QueryDocumentSnapshot]): zio.Task[Int] =
    ZIO
      .foreach(documents.grouped(BatchLimit).toSeq): group =>
        val batch = firestore.batch()
        group.foreach: document =>
          val _ = batch.delete(document.getReference)
        GoogleFuture.fromApiFuture(batch.commit()).as(group.size)
      .map(_.sum)

/** Accepts a bench's word that a run is to be thrown away.
  *
  * Authenticated and scoped to the caller's own email, so a run id -- which is not a secret, and travels in every note
  * -- cannot be used to delete another account's recordings.
  */
object TestRunDiscards extends BackendPlugin:
  type Requires = Firestore & SessionStore

  override val id = "test-run-discards"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.firestore) ++ CapabilitySet.one(BackendCapabilities.sessionStore)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Authenticated
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.POST / "test" / "run" / "discard" -> handler((request: Request) => apply(request)))

  private def apply(request: Request): ZIO[Requires & RequestContext, Nothing, Response] =
    ZIO.serviceWithZIO[RequestContext]:
      case RequestContext.Authenticated(_, user) => discard(request, user.email)
      case _                                     => ZIO.succeed(noStore(Response.status(Status.Unauthorized)))

  private def discard(request: Request, email: String): ZIO[Requires, Nothing, Response] =
    val removed =
      for
        body <- request.body.asString
        asked <- ZIO.fromEither(body.fromJson[DiscardRun]).mapError(details => IllegalArgumentException(details))
        runId <- ZIO
          .succeed(asked.runId.trim)
          .filterOrFail(_.nonEmpty)(IllegalArgumentException("A run to discard must be named"))
        firestore <- ZIO.service[Firestore]
        counts <- DiscardedRuns(firestore).discard(runId, email)
        // Said out loud: deleting evidence is worth a line in the log even when it is the right thing to do.
        _ <- ZIO.logWarning(s"Discarded run $runId: ${counts._1} events and ${counts._2} recordings")
      yield ()
    removed.foldZIO(
      error =>
        ZIO.logWarningCause("Refused to discard a run", Cause.fail(error)) *>
          ZIO.succeed(noStore(Response.status(Status.BadRequest))),
      _ => ZIO.succeed(noStore(Response.status(Status.NoContent)))
    )

  private def noStore(response: Response): Response = response.addHeader(Header.CacheControl.NoStore)
