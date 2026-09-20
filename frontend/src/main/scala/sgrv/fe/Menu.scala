package sgrv.fe

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sgrv.api.Documents

/** The hamburger menu, built from one place because every screen carries one and they must not drift apart.
  *
  * The two documents are the reason this exists rather than three copies of the same markup. A privacy policy reachable
  * from three screens and not from the fourth is the kind of gap nobody notices until somebody is looking for it, and
  * the person looking for it is by then already suspicious.
  */
private[fe] object Menu:

  private[fe] final case class Anchor(topPx: Double, rightPx: Double)

  /** Fixed-position coordinates that put the sheet directly below, and right-aligned with, the button that opened it. */
  private[fe] def anchor(buttonRightPx: Double, buttonBottomPx: Double, viewportWidthPx: Double): Anchor =
    Anchor(math.max(0.0, buttonBottomPx), math.max(0.0, viewportWidthPx - buttonRightPx))

  /** What the documents are called on screen. The paths are shared with the backend that serves them; the wording is
    * this side's business.
    */
  val documentLabels: Map[String, String] =
    Map(Documents.Privacy -> "Privacy", Documents.Terms -> "Terms of Service")

  /** The entries every menu in the app ends up carrying, as label and path. */
  val documents: Seq[(String, String)] =
    Documents.All.map(document => documentLabels(document) -> Documents.path(document))

  /** The control that opens a menu. */
  def toggle(open: Var[Boolean]): Element =
    button(
      cls := "menu-button",
      typ := "button",
      aria.label := "Menu",
      aria.expanded <-- open.signal,
      // An SVG rather than U+2630: the three strokes are symmetric around the viewBox centre, independent of the
      // uneven baseline whitespace different system fonts give the Unicode glyph.
      svg.svg(
        svg.cls := "menu-icon",
        svg.viewBox := "0 0 24 24",
        aria.hidden := true,
        svg.path(svg.d := "M3 6H21 M3 12H21 M3 18H21")
      ),
      onClick --> { event =>
        val button = event.currentTarget.asInstanceOf[dom.html.Button]
        val bounds = button.getBoundingClientRect()
        val at = anchor(bounds.right, bounds.bottom, dom.window.innerWidth)
        val rootStyle = dom.document.documentElement.asInstanceOf[dom.html.Element].style
        rootStyle.setProperty("--menu-anchor-top", s"${at.topPx}px")
        rootStyle.setProperty("--menu-anchor-right", s"${at.rightPx}px")
        open.update(shown => !shown)
      }
    )

  /** A backdrop, so a tap anywhere else dismisses the menu -- which is what a phone expects. */
  def backdrop(open: Var[Boolean]): Modifier[HtmlElement] =
    child <-- open.signal.map:
      case false => emptyNode
      case true  => div(cls := "menu-backdrop", onClick --> (_ => open.set(false)))

  /** One entry. Choosing it closes the menu, so whatever it opened is not left behind a sheet. */
  def item(open: Var[Boolean], label: String, act: () => Unit): Element =
    button(cls := "menu-item", typ := "button", label, onClick --> (_ => { open.set(false); act() }))

  /** An entry whose wording changes with what choosing it would do -- a switch, named by the way it will turn. */
  def item(open: Var[Boolean], label: Signal[String], act: () => Unit): Element =
    button(cls := "menu-item", typ := "button", child.text <-- label, onClick --> (_ => { open.set(false); act() }))

  /** An entry that opens a page of its own, in a tab of its own.
    *
    * A new tab rather than this one, deliberately: these are static pages outside the app, and following one in place
    * would unload a running camera along with everything it had counted. `noopener` because a page opened this way has
    * no business reaching back into the one that opened it.
    */
  def document(open: Var[Boolean], label: String, path: String): Element =
    a(
      cls := "menu-item",
      href := path,
      target := "_blank",
      rel := "noopener noreferrer",
      label,
      onClick --> (_ => open.set(false))
    )

  /** The documents, as menu entries. Every menu in the app carries these. */
  def documentItems(open: Var[Boolean]): Seq[Element] =
    documents.map((label, path) => document(open, label, path))

  /** The sheet the entries sit in, with the backdrop that dismisses it. */
  def sheet(open: Var[Boolean], entries: Modifier[HtmlElement]*): Element =
    div(
      cls := "menu-sheet",
      cls("open") <-- open.signal,
      entries
    )
