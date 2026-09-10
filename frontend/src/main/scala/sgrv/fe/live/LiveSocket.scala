package sgrv.fe.live

import org.scalajs.dom

/** A WebSocket that keeps coming back.
  *
  * Reconnection is not a refinement here, it is the normal case: Cloud Run bounds how long a request may live, and a
  * WebSocket is a request, so a socket open across a workout will be closed by the platform whether or not anything
  * is wrong. A phone locking its screen or losing signal for a moment does the same. So a drop is treated as routine
  * and retried, with a delay that grows only far enough to stop a backend that is genuinely down being hammered.
  *
  * Everything here is best effort by design. A dashboard whose socket is briefly away shows a slightly old count,
  * which is the right failure: the acquirer is the thing keeping the tally, and this only carries copies of it.
  */
private[fe] final class LiveSocket(
    path: String,
    onMessage: String => Unit,
    onOpen: () => Unit = () => (),
    onClosed: () => Unit = () => ()
):
  private var socket = Option.empty[dom.WebSocket]
  private var attempt = 0
  private var wanted = false
  private var pending = Option.empty[Int]

  def connect(): Unit =
    wanted = true
    open()

  /** Stops for good: no further retries, and the socket is closed rather than left to the garbage collector. */
  def close(): Unit =
    wanted = false
    pending.foreach(dom.window.clearTimeout)
    pending = None
    socket.foreach(existing => try existing.close()
    catch case _: Throwable => ())
    socket = None

  /** Sends if the socket happens to be open, and drops the message otherwise.
    *
    * Nothing here is worth queueing. A reading is superseded by the next one a tenth of a second later, and a command
    * that arrived late would act on a set the user has since moved on from.
    */
  def send(text: String): Boolean =
    socket.filter(_.readyState == dom.WebSocket.OPEN) match
      case Some(open) =>
        try
          open.send(text)
          true
        catch case _: Throwable => false
      case None => false

  def isOpen: Boolean = socket.exists(_.readyState == dom.WebSocket.OPEN)

  private def open(): Unit =
    if wanted && socket.isEmpty then
      try
        val created = new dom.WebSocket(LiveSocket.addressOf(path))
        socket = Some(created)
        created.onopen = _ =>
          attempt = 0
          onOpen()
        created.onmessage = event => onMessage(event.data.asInstanceOf[String])
        created.onclose = _ =>
          socket = None
          onClosed()
          retry()
        created.onerror = _ =>
          // Left to onclose, which always follows: retrying from both would open two sockets for one failure.
          ()
      catch
        case _: Throwable =>
          socket = None
          retry()

  private def retry(): Unit =
    if wanted && pending.isEmpty then
      val delay = LiveSocket.delayMillis(attempt)
      attempt += 1
      pending = Some(dom.window.setTimeout(
        () =>
          pending = None
          open()
        ,
        delay.toDouble
      ))

private[fe] object LiveSocket:
  val FirstDelayMillis = 500
  val LongestDelayMillis = 10000

  /** How long to wait before the next attempt: doubling, but bounded.
    *
    * Bounded low, because the common cause of a drop is the platform closing a long-lived request rather than
    * anything being broken, and a workout should not pause for half a minute over a routine reconnection.
    */
  private[live] def delayMillis(attempt: Int): Int =
    val doubled = FirstDelayMillis.toLong << math.min(attempt, 16)
    math.min(doubled, LongestDelayMillis.toLong).toInt

  /** The socket address for a path on this same origin, in whichever scheme the page was served over. */
  private[live] def addressOf(path: String, origin: String): String =
    val scheme = if origin.startsWith("https:") then "wss:" else "ws:"
    s"$scheme${origin.dropWhile(_ != ':').drop(1)}$path"

  private def addressOf(path: String): String = addressOf(path, dom.window.location.origin)
