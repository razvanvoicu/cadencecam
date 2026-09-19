package sgrv.fe

import org.scalajs.dom

import scala.util.control.NonFatal

/** Which screen a device opens on, when its owner has said.
  *
  * A tablet propped by the stair climber is a dashboard and nothing else, and making it pick "Dashboard" from the
  * role screen at every start is a tap nobody needs. So a device can be told to default to the dashboard, and it
  * remembers that in its own storage: this is a fact about the device, not about the account, and the phone signed
  * into the same account must go on opening its camera.
  *
  * The rest of the start-up rules are untouched. Without a preference a device does what it always did -- a fresh
  * login goes to the counter when nothing is counting, and a device returning after a reload takes up the role it
  * already held, which is what keeps a counting phone counting when its page is reloaded mid-set.
  */
private[fe] object StartPreference:

  /** The key the preference is stored under, spelled as the owner asked for it so it can be read in the browser's own
    * storage inspector.
    */
  val DashboardKey = "default_to_dashboard"

  /** Set once, when the preference is turned off, and consumed at the next start.
    *
    * Turning the preference off is a statement about the next start rather than about now: the person tapping it is
    * looking at the dashboard, and taking it away from under them would be the wrong answer. But a device returning
    * after a reload reopens the screen it was last on, which is the dashboard -- so without this, "off" would change
    * nothing anybody could see. This marks the one start that should present the role screen instead.
    */
  val PresentRolesOnceKey = "sgrv.present-roles-on-next-start"

  /** Where the device should open, or nothing to leave the usual rules in charge. */
  def opening(defaultToDashboard: Boolean, presentRolesOnce: Boolean): Option[Screen] =
    if defaultToDashboard then Some(Screen.Dashboard)
    else if presentRolesOnce then Some(Screen.Selection)
    else None

  def defaultsToDashboard(storage: dom.Storage): Boolean = read(storage, DashboardKey).contains("true")

  /** Turns the preference on or off, leaving the storage exactly as the owner described it: the key present with
    * `true`, or absent.
    */
  def setDefaultsToDashboard(storage: dom.Storage, on: Boolean): Unit =
    if on then
      write(storage, DashboardKey, "true")
      remove(storage, PresentRolesOnceKey)
    else
      remove(storage, DashboardKey)
      write(storage, PresentRolesOnceKey, "true")

  /** Where this start should open, consuming the one-shot marker if it was set. */
  def consumeOpening(storage: dom.Storage): Option[Screen] =
    val once = read(storage, PresentRolesOnceKey).contains("true")
    if once then remove(storage, PresentRolesOnceKey)
    opening(defaultsToDashboard(storage), once)

  // Storage can be missing or refuse writes -- a private window, a full quota -- and a preference that cannot be
  // saved is not worth failing the app over. Each access is guarded on its own and logged.

  private def read(storage: dom.Storage, key: String): Option[String] =
    try Option(storage.getItem(key))
    catch
      case NonFatal(error) =>
        dom.console.warn(s"Could not read $key: ${error.getMessage}")
        None

  private def write(storage: dom.Storage, key: String, value: String): Unit =
    try storage.setItem(key, value)
    catch case NonFatal(error) => dom.console.warn(s"Could not save $key: ${error.getMessage}")

  private def remove(storage: dom.Storage, key: String): Unit =
    try storage.removeItem(key)
    catch case NonFatal(error) => dom.console.warn(s"Could not remove $key: ${error.getMessage}")
