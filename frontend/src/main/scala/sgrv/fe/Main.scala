package sgrv.fe

import com.raquo.laminar.api.L.renderOnDomContentLoaded
import org.scalajs.dom
import sgrv.fe.acquire.RepCountStore
import sgrv.fe.refreshstate.{RefreshStateStore, SessionRefreshWorker}

/** The frontend composition root.
  *
  * It creates the browser-backed stores and feature controllers, connects their few cross-feature callbacks, and hands
  * the resulting graph to [[AppView]]. Mutable feature state and browser-resource lifecycles belong to those components
  * rather than to this scope.
  */
object Main:
  def main(args: Array[String]): Unit =
    val storage = dom.window.localStorage
    val stateStore = FrontendStateStore(storage)
    val refreshStateStore = RefreshStateStore(storage)
    val repCountStore = RepCountStore(storage)
    given refreshStore: RefreshStateStore = refreshStateStore

    def show(screen: Screen): Unit = stateStore.update(_.copy(screen = screen))

    lazy val session: SessionController = SessionController(
      api,
      refreshWorker,
      stateStore,
      refreshStateStore,
      storage,
      onSignedIn = () => settingsPanel.load(),
      startByPresence = () => roles.startByPresence()
    )
    lazy val http: HttpService = HttpService(() => session.handleUnauthorized())
    lazy val api: ApiClient = ApiClient(http)
    lazy val refreshWorker: SessionRefreshWorker = SessionRefreshWorker(http)
    lazy val settingsPanel: SettingsPanel = SettingsPanel(api)
    lazy val historyPanel: HistoryPanel = HistoryPanel(api, settingsPanel.settings)
    lazy val roles: RoleSelection =
      RoleSelection(api, show, Screen.benchOffered(dom.window.location.hash))

    // Asked once, at the start: Chrome on Android keeps the handset's model out of its user agent, and the only way to
    // learn it is an explicit request that answers later.
    Device.learn()

    val app = AppView(
      api,
      storage,
      stateStore,
      refreshStateStore,
      session,
      roles,
      settingsPanel,
      historyPanel,
      repCountStore,
      show
    )

    session.start()
    renderOnDomContentLoaded(dom.document.body, app)
