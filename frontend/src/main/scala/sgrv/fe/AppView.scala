package sgrv.fe

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sgrv.fe.UserState.*
import sgrv.fe.acquire.RepCountStore
import sgrv.fe.bench.BenchView
import sgrv.fe.refreshstate.RefreshStateStore

/** Renders the application shell. Feature state and lifecycle remain in the controllers it composes. */
private[fe] object AppView:
  def apply(
      api: ApiClient,
      storage: dom.Storage,
      stateStore: FrontendStateStore,
      refreshStateStore: RefreshStateStore,
      session: SessionController,
      roles: RoleSelection,
      settingsPanel: SettingsPanel,
      historyPanel: HistoryPanel,
      repCountStore: RepCountStore,
      show: Screen => Unit
  ): Element =
    /** The menu for the screens that have no header of their own to hang one from: the role picker and the bench.
      *
      * A menu rather than the row of links it replaces, so that every screen in the app opens the same sheet with the
      * same entries in it. The two documents are why: whichever screen somebody happens to be on when they go looking
      * for a privacy policy is the screen it has to be on.
      */
    def userActions: Element =
      val open = Var(false)
      div(
        cls := "user-actions",
        Menu.toggle(open),
        Menu.backdrop(open),
        Menu.sheet(
          open,
          Menu.item(open, "About", () => session.openAbout()),
          Menu.documentItems(open),
          Menu.item(open, "Logout", () => session.logout())
        )
      )

    /** The login screen's menu: the same sheet the app's other screens open, minus everything that needs an account.
      *
      * A menu rather than the bare About link it replaces, because the two documents belong in front of someone who has
      * not signed in yet -- that is the moment they are deciding whether to -- and a row of links across the top of a
      * phone is not where anybody would look for them.
      */
    def guestMenu: Element =
      val open = Var(false)
      div(
        cls := "user-actions",
        Menu.toggle(open),
        Menu.backdrop(open),
        Menu.sheet(open, Menu.item(open, "About", () => session.openAbout()), Menu.documentItems(open))
      )

    def signedInView(displayName: String, screen: Screen): Element =
      screen match
        case Screen.Selection => roles.view(displayName)
        case Screen.Acquirer  =>
          CounterScreen(
            api,
            repCountStore,
            settingsPanel,
            show,
            () => session.openAbout(),
            () => session.logout()
          )
        case Screen.Dashboard =>
          DashboardView(
            api,
            settingsPanel,
            historyPanel,
            storage,
            show,
            () => session.openAbout(),
            () => session.logout(),
            () => session.forgetPending.set(true)
          )
        case Screen.Bench => BenchView(api.http, () => show(Screen.Selection))

    def login: Element =
      // Before authentication has been attempted, offer the only thing an anonymous visitor can do.
      div(
        cls := "home",
        // What the app is, for the two people who arrive without knowing: somebody deciding whether to hand over their
        // Google account, and a reviewer who cannot sign in to find out. A login button on an otherwise empty page
        // answers neither.
        div(
          cls := "intro",
          h1(cls := "intro-title", "CadenceCam"),
          p(cls := "intro-lead", "Counts your exercise repetitions using your phone's camera."),
          p(
            cls := "intro-detail",
            "Prop your phone where it can see a repetitive movement — a dumbbell curl, a stair climber's rotating " +
              "wheel, a stationary bicycle's pedal in motion — and it counts the repetitions as you go. A tablet " +
              "or a second phone can show the running count, your pace, and an estimate of the energy you have used."
          ),
          p(
            cls := "intro-detail",
            "The picture is processed on your device and is never uploaded: the app reads only how bright each " +
              "quarter of the frame is, and counts the movement from that. No photo or video is recorded."
          )
        ),
        a(cls := "login-button", href := "/auth/login", "Login with Google")
      )

    def aboutPanel(state: AboutState, user: UserState): Element =
      val account = user match
        case SignedIn(email, _)  => Some(email)
        case Restoring(email, _) => Some(email)
        case _                   => None
      val content = state match
        case AboutState.Loading         => p(cls := "about-status", "Loading build information…")
        case AboutState.Failed(message) => p(cls := "error about-status", message)
        case AboutState.Loaded(information) =>
          dl(
            cls := "about-details",
            dt("App version"),
            dd(information.appVersion),
            dt("Build date"),
            dd(information.buildDate),
            dt("Build OS"),
            dd(information.buildOs),
            dt("Scala version"),
            dd(information.scalaVersion),
            dt("Scala.js version"),
            dd(information.scalaJsVersion),
            // What the browser is actually running, which need not be what the server just served. Stamped into the
            // bundle at packaging time and read back out of this browser's own storage, so a cached build says so
            // plainly instead of being mistaken for the detector misbehaving.
            dt("Frontend build"),
            dd(Device.frontendBuild().getOrElse("not recorded by this browser"))
          )
        // Before signing in: the one field that can be known without a backend, which is also the one that matters on a
        // device that will not behave.
        case AboutState.LocalOnly =>
          dl(
            cls := "about-details",
            dt("Frontend build"),
            dd(Device.frontendBuild().getOrElse("not recorded by this browser"))
          )
        case AboutState.Closed => emptyNode

      div(
        cls := "about-overlay",
        onClick --> (_ => stateStore.update(_.copy(aboutState = AboutState.Closed))),
        div(
          cls := "about-dialog",
          role := "dialog",
          onClick --> (_.stopPropagation()),
          div(
            cls := "about-header",
            h2("About"),
            button(
              cls := "about-close",
              typ := "button",
              title := "Close",
              onClick --> (_ => stateStore.update(_.copy(aboutState = AboutState.Closed))),
              "×"
            )
          ),
          // Shown at once rather than with the build information, which is fetched: the account is known locally, and
          // waiting on a request to say who is signed in would be backwards.
          account.fold(emptyNode)(email => dl(cls := "about-details about-account", dt("Signed in as"), dd(email))),
          content
        )
      )

    def forgetDialog: Element =
      div(
        cls := "about-overlay",
        div(
          cls := "about-dialog takeover-dialog",
          role := "alertdialog",
          div(cls := "about-header", h2("Delete all your data?")),
          p(
            "This removes every workout you have counted, the recordings they produced, and your settings. " +
              "It cannot be undone, and there is nothing left afterwards to recover it from."
          ),
          p(cls := "field-hint", "You will be signed out. You can sign in again and start over."),
          div(
            cls := "takeover-actions",
            button(
              cls := "mode-button forget-confirm",
              typ := "button",
              "Delete everything",
              onClick --> (_ => session.forgetEverything())
            ),
            button(
              cls := "back-button",
              typ := "button",
              "Keep my data",
              onClick --> (_ => session.forgetPending.set(false))
            )
          )
        )
      )

    def expiredDialog: Element =
      div(
        cls := "about-overlay session-expired-overlay",
        div(
          cls := "about-dialog session-expired-dialog",
          role := "alertdialog",
          div(
            cls := "about-header",
            h2("Session expired"),
            button(
              cls := "about-close",
              typ := "button",
              title := "Close",
              onClick --> (_ => refreshStateStore.update(_.copy(expired = false))),
              "×"
            )
          ),
          p(
            cls := "error session-expired-message",
            "Your Google OAuth session has expired. Reload the page to sign in again."
          )
        )
      )

    div(
      cls := "app",
      // The bench and the dashboard are the two screens not held to a hand's width: the bench frames what a camera on a
      // tripod sees, and the dashboard is read across a room from a tablet that may be either way up. Every other view
      // is deliberately kept to a phone's frame, which is the device it is actually used on.
      cls("wide") <-- stateStore.signal
        .map(Shell.of)
        .distinct
        .map:
          case Shell.SignedIn(_, Screen.Bench)     => true
          case Shell.SignedIn(_, Screen.Dashboard) => true
          case _                                   => false
      ,
      child <-- stateStore.signal
        .map(Shell.of)
        .distinct
        .map:
          // Both keep their own About and Logout in the menu, so the global bar would only duplicate them.
          case Shell.SignedIn(_, Screen.Acquirer)  => emptyNode
          case Shell.SignedIn(_, Screen.Dashboard) => emptyNode
          case Shell.SignedIn(_, _)                => userActions
          // Reachable before signing in as well. Which build a browser is running is exactly the thing one wants to
          // check on a device that will not behave, and being signed out is no reason not to be able to look.
          case Shell.Login | Shell.AuthenticationFailed(_) => guestMenu
          case _                                           => emptyNode,
      div(
        cls := "content",
        // Distinct, and on the shell rather than the whole state: without it every write to FrontendState builds a new
        // view, and on the acquirer that means a second camera, a second detector and a second writer to the stored count
        // -- one of them invisible.
        child <-- stateStore.signal
          .map(Shell.of)
          .distinct
          .map:
            case Shell.Blank                         => emptyNode
            case Shell.Login                         => login
            case Shell.AuthenticationFailed(message) =>
              div(cls := "home", p(cls := "error", s"Authentication failed: $message"))
            case Shell.SignedIn(displayName, screen) => signedInView(displayName, screen)
      ),
      child <-- stateStore.signal
        .map(_.logoutState)
        .map:
          case LogoutState.Failed(message) => p(cls := "error logout-error", s"Logout failed: $message")
          case _                           => emptyNode,
      child <-- stateStore.signal
        // The account travels with the panel: a display name does not say which of several accounts this device is
        // signed into, and on a bench with four phones that is the thing one actually needs to know.
        .map(state => (state.aboutState, state.user))
        .map:
          case (AboutState.Closed, _) => emptyNode
          case (state, user)          => aboutPanel(state, user),
      child <-- settingsPanel.isOpen.signal.map:
        case false => emptyNode
        case true  => settingsPanel.view(),
      child <-- historyPanel.isOpen.signal.map:
        case false => emptyNode
        case true  => historyPanel.view(),
      child <-- session.forgetPending.signal.map:
        case false => emptyNode
        case true  => forgetDialog,
      child <-- roles.takeoverPending.signal.map:
        case false => emptyNode
        case true  => roles.takeoverDialog,
      child <-- refreshStateStore.signal
        .map(_.expired)
        .map:
          case false => emptyNode
          case true  => expiredDialog
    )
