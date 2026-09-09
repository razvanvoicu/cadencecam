package sgrv.fe.acquire

import org.scalajs.dom
import zio.json.*

import scala.util.control.NonFatal

/** A rep total and the wall-clock moment it was last changed.
  *
  * Wall clock rather than a monotonic timer: the point is to compare against a reading taken before a page load, and
  * `performance.now()` restarts at zero with every load. That makes the age only as trustworthy as the device clock,
  * which is the trade this needs — a clock that moved is handled by [[RepCountStore.restore]] rather than pretended
  * away.
  */
@jsonNoExtraFields
private[fe] final case class SavedRepCount(count: Int, atMillis: Double)

private[fe] object SavedRepCount:
  given JsonCodec[SavedRepCount] = DeriveJsonCodec.gen[SavedRepCount]

/** Carries the rep total across a page load, so a reload mid-workout resumes the count instead of starting over.
  *
  * A phone left propped against something reloads for reasons that have nothing to do with the user: the tab is
  * evicted under memory pressure, the screen locks and the page is restored, a stray swipe refreshes. None of those
  * mean the set is over, and losing the tally to one is worse than the alternative — a stale count is visible and one
  * button clears it, whereas a lost one cannot be recovered at all.
  *
  * What keeps that from becoming a count that haunts the next session is the retention window: a total is resumed only
  * while it is recent enough to plausibly belong to the workout still in progress.
  */
private[fe] final class RepCountStore(
    storage: dom.Storage,
    retentionMillis: Double = RepCountStore.DefaultRetentionMillis
):

  /** The total to start from, or zero when nothing recent enough was saved. */
  def restore(nowMillis: Double): Int =
    RepCountStore.resumable(read(), nowMillis, retentionMillis)

  def save(count: Int, atMillis: Double): Unit =
    try storage.setItem(RepCountStore.StorageKey, SavedRepCount(count, atMillis).toJson)
    catch
      case NonFatal(error) =>
        dom.console.warn(s"Could not persist the rep count: ${RepCountStore.message(error)}")

  def clear(): Unit =
    try storage.removeItem(RepCountStore.StorageKey)
    catch
      case NonFatal(error) =>
        dom.console.warn(s"Could not clear the persisted rep count: ${RepCountStore.message(error)}")

  private def read(): Option[SavedRepCount] =
    try
      Option(storage.getItem(RepCountStore.StorageKey)).flatMap: encoded =>
        encoded.fromJson[SavedRepCount] match
          case Right(saved)  => Some(saved)
          case Left(details) =>
            dom.console.warn(s"Ignoring an invalid persisted rep count: $details")
            None
    catch
      case NonFatal(error) =>
        dom.console.warn(s"Could not read the persisted rep count: ${RepCountStore.message(error)}")
        None

private[fe] object RepCountStore:
  private val StorageKey = "sgrv.rep-count.v1"

  /** How recently a total must have changed to be resumed. An hour comfortably covers a session including its rests,
    * while still leaving tomorrow's workout to start from zero.
    */
  val DefaultRetentionMillis: Double = 60 * 60 * 1000.0

  /** The total worth resuming from a saved reading, or zero.
    *
    * The age is compared in absolute value, so a saved reading that appears to come from the future is judged by how
    * far into the future it is. A device clock that has been corrected by a few seconds should not cost the user their
    * count, and one that is wrong by more than the retention window should not preserve it indefinitely.
    */
  private[acquire] def resumable(saved: Option[SavedRepCount], nowMillis: Double, retentionMillis: Double): Int =
    saved
      .filter(_.count > 0)
      .filter(reading => math.abs(nowMillis - reading.atMillis) < retentionMillis)
      .fold(0)(_.count)

  private def message(error: Throwable): String =
    Option(error.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)
