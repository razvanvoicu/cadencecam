package sgrv.fe

import com.raquo.laminar.api.L.*
import sgrv.api.AcquirerPresence

import scala.concurrent.ExecutionContext.Implicits.global

/** Chooses the role this device plays and owns the deliberate counter-takeover flow. */
private[fe] final class RoleSelection(api: ApiClient, show: Screen => Unit, benchOffered: Boolean):
  /** Raised while this device waits to be told whether to displace another one that is already counting.
    *
    * One acquirer per account is enforced by the account's record: whichever device took the role last is named as the
    * counter, and the one it replaced stands down when its next report is refused. That is the right outcome and the
    * wrong way to arrive at it unannounced: on a bench with four handsets signed into one account, a phone that
    * silently stopped counting looked like a phone that had crashed. So it is asked for first.
    */
  val takeoverPending: Var[Boolean] = Var(false)

  /** Whether this account already has a device counting, as far as the last answer from the server goes.
    *
    * Decides what a fresh login is shown. With nothing counting there is nothing to choose between -- the account needs
    * a counter before a dashboard has anything to watch -- so the device goes straight to counting. With a counter
    * already running, the choice is real and worth making deliberately.
    */
  private val accountCounting = Var(false)
  private val presenceRequests = RequestScope()

  /** Where a fresh login lands: counting if the account has no counter, the choice of roles if it has.
    *
    * The check can fail -- an offline device, a session that has just expired -- and a failure must not stand between
    * someone and their camera, so it counts. Taking the role is what settles it either way.
    */
  def startByPresence(): Unit =
    presenceRequests.latest(api.acquirerPresence()):
      case Right(AcquirerPresence(true)) => accountCounting.set(true)
      case _                             =>
        accountCounting.set(false)
        show(Screen.Acquirer)

  /** Becomes the acquirer, asking first if the account already has one.
    *
    * The check can fail -- an offline device, a signed-out session -- and a failure here must not stand between someone
    * and their camera, so it proceeds. Taking the role is what settles it: there is only ever one counter, because the
    * account's record names one.
    */
  private def acquireRole(): Unit =
    presenceRequests.latest(api.acquirerPresence()):
      case Right(AcquirerPresence(true)) => takeoverPending.set(true)
      case _                             => show(Screen.Acquirer)

  private def roleChoice(modifier: String, screen: Screen, title: String, description: String): Element =
    button(
      cls := s"mode-button $modifier",
      typ := "button",
      onClick --> (_ => if screen == Screen.Acquirer then acquireRole() else show(screen)),
      span(cls := "mode-title", title),
      span(cls := "mode-description", description)
    )

  def view(displayName: String): Element =
    div(
      cls := "selection",
      h1(cls := "welcome", s"Hello, $displayName!"),
      p(
        cls := "selection-prompt",
        child.text <-- accountCounting.signal.map: counting =>
          if counting then "Another device is counting for this account. Choose what this one does."
          else "Choose what this device does in the next session."
      ),
      div(
        cls := "mode-choices",
        child <-- accountCounting.signal.map: counting =>
          if counting then
            roleChoice(
              "mode-acquirer",
              Screen.Acquirer,
              "Take over counting",
              "Count on this device instead. The device counting now returns to its home screen and releases its camera."
            )
          else
            roleChoice(
              "mode-acquirer",
              Screen.Acquirer,
              "Counter",
              "Aim this device's camera at the movement and let it count the reps."
            )
        ,
        roleChoice(
          "mode-dashboard",
          Screen.Dashboard,
          "Dashboard",
          "Watch the live rep count arriving from the counting device."
        ),
        if !benchOffered then emptyNode
        else
          roleChoice(
            "mode-bench",
            Screen.Bench,
            "Test",
            "Show a movement of known cadence and measure the count against it. For a desktop screen."
          )
      )
    )

  def takeoverDialog: Element =
    div(
      cls := "about-overlay",
      div(
        cls := "about-dialog takeover-dialog",
        role := "alertdialog",
        div(cls := "about-header", h2("Another device is counting")),
        p(
          "This account already has a device counting. Only one can, so taking over will send the other " +
            "one back to its home screen and release its camera."
        ),
        div(
          cls := "takeover-actions",
          button(
            cls := "mode-button",
            typ := "button",
            "Take over",
            onClick --> { _ =>
              takeoverPending.set(false)
              show(Screen.Acquirer)
            }
          ),
          button(
            cls := "back-button",
            typ := "button",
            "Leave it alone",
            onClick --> (_ => takeoverPending.set(false))
          )
        )
      )
    )
