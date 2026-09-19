package sgrv.e2e

import org.openqa.selenium.By
import org.openqa.selenium.chrome.{ChromeDriver, ChromeOptions}
import org.openqa.selenium.support.ui.{ExpectedConditions, WebDriverWait}

import java.net.{InetSocketAddress, Socket}
import java.time.Duration
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Exercises what only a signed-in session can reach -- the menu's About panel, and the walk in and out of each role --
  * against the real backend, attaching to the visible Chrome instance `e2etest/launchTestBrowser` leaves running rather
  * than launching a fresh, signed-out one. Requires having already signed in by hand in that window first (see
  * `launchTestBrowser`'s instructions) — Google blocks WebDriver-controlled browsers from driving its login form
  * directly. Run via `e2etest/testAuthenticated`, not `e2etest/test`.
  *
  * Entering the counter's role is real: it takes the role for the signed-in account, from any other device holding it,
  * and opens a workout in that account's history, exactly as a phone would.
  */
class SignedInE2ESuite extends munit.FunSuite:
  private val baseUrl = sys.props
    .get("e2e.baseUrl")
    .orElse(sys.env.get("E2E_BASE_URL"))
    .getOrElse(throw new IllegalStateException("E2E_BASE_URL is not configured"))
  private val debuggerAddress = sys.props.getOrElse("e2e.debuggerAddress", "127.0.0.1:9222")
  private var driver: ChromeDriver = scala.compiletime.uninitialized

  // `e2etest/test` runs every suite it discovers, including this one, but only `testAuthenticated` arranges the
  // signed-in browser this suite attaches to. Leaving `driver` unset when that browser is absent turns these
  // tests into skips there instead of failures. `testAuthenticated` checks the same port itself before invoking
  // the suite and stops with a clear message, so a genuinely missing browser is never silently ignored.
  override def beforeAll(): Unit =
    if debuggerIsListening then
      val options = new ChromeOptions()
      options.setExperimentalOption("debuggerAddress", debuggerAddress)
      driver = new ChromeDriver(options)

  // Attached via debuggerAddress rather than launched by this driver: quit() only ends this WebDriver session,
  // it does not close the real, user-owned browser window.
  override def afterAll(): Unit =
    if driver != null then driver.quit()

  private def debuggerIsListening: Boolean =
    debuggerAddress.split(":", 2) match
      case Array(host, port) =>
        port.toIntOption.exists: number =>
          Using(new Socket())(socket => socket.connect(new InetSocketAddress(host, number), 2000)).isSuccess
      case _ => false

  private def load(): WebDriverWait =
    assume(driver != null, s"No signed-in test browser on $debuggerAddress; run `sbt e2etest/testAuthenticated`.")
    driver.get(baseUrl)
    new WebDriverWait(driver, Duration.ofSeconds(20))

  /** Waits for a screen to be drawn: the role picker, or one of the roles. A signed-in browser returns to whichever it
    * was last on, and nothing is there until `/me` has answered.
    */
  private def settled(wait: WebDriverWait): Unit =
    val _ = wait.until(ExpectedConditions.presenceOfElementLocated(By.cssSelector(".selection, .screen")))

  /** Opens the current screen's menu and chooses an entry by its label. Every screen carries exactly one menu. */
  private def choose(wait: WebDriverWait, entry: String): Unit =
    wait.until(ExpectedConditions.elementToBeClickable(By.cssSelector(".menu-button"))).click()
    val sheet = wait.until(ExpectedConditions.visibilityOfElementLocated(By.cssSelector(".menu-sheet.open")))
    sheet
      .findElements(By.cssSelector(".menu-item"))
      .asScala
      .find(_.getText == entry)
      .getOrElse(fail(s"No '$entry' in the menu"))
      .click()

  private def rolePicker(wait: WebDriverWait): Unit =
    val _ = wait.until(ExpectedConditions.visibilityOfElementLocated(By.cssSelector(".mode-choices")))

  test("the menu's About opens the build information, and says who is signed in"):
    val wait = load()
    settled(wait)

    choose(wait, "About")
    // Seven once the build information has arrived: the account is shown at once, the rest when it is fetched.
    val labels = wait
      .until(ExpectedConditions.numberOfElementsToBe(By.cssSelector(".about-dialog .about-details dt"), 7))
      .asScala
      .map(_.getText)
      .toSeq
    val values = driver.findElements(By.cssSelector(".about-dialog .about-details dd")).asScala.map(_.getText).toSeq

    assertEquals(
      labels,
      Seq(
        "Signed in as",
        "App version",
        "Build date",
        "Build OS",
        "Scala version",
        "Scala.js version",
        "Frontend build"
      )
    )
    assertEquals(values.size, labels.size)
    values.foreach(value => assert(value.nonEmpty))
    driver.findElement(By.cssSelector(".about-dialog .about-close")).click()

  test("a signed-in session can enter and leave each role from the role picker"):
    val wait = load()
    settled(wait)
    if driver.findElements(By.cssSelector(".mode-choices")).isEmpty then choose(wait, "Back")
    rolePicker(wait)

    val titles = wait
      .until(ExpectedConditions.numberOfElementsToBe(By.cssSelector(".mode-choices .mode-button"), 2))
      .asScala
      .map(_.findElement(By.cssSelector(".mode-title")).getText)
      .toSeq
    // The counter's choice is worded by whether the account already has another device counting.
    assert(Set("Counter", "Take over counting").contains(titles.head), titles.head)
    assertEquals(titles(1), "Dashboard")

    wait.until(ExpectedConditions.elementToBeClickable(By.cssSelector(".mode-dashboard"))).click()
    wait.until(ExpectedConditions.visibilityOfElementLocated(By.cssSelector(".screen.dashboard")))
    choose(wait, "Back")
    rolePicker(wait)

    wait.until(ExpectedConditions.elementToBeClickable(By.cssSelector(".mode-acquirer"))).click()
    wait.until(
      ExpectedConditions.or(
        ExpectedConditions.visibilityOfElementLocated(By.cssSelector(".screen.acquirer")),
        ExpectedConditions.visibilityOfElementLocated(By.cssSelector(".takeover-dialog"))
      )
    )
    // Asked first when another device is counting for the account; taking over is what the role walk is testing.
    driver.findElements(By.cssSelector(".takeover-dialog .mode-button")).asScala.headOption.foreach(_.click())
    val counter = wait.until(ExpectedConditions.visibilityOfElementLocated(By.cssSelector(".screen.acquirer")))
    assertEquals(counter.findElement(By.cssSelector(".screen-title")).getText, "Counter")
    choose(wait, "Back")
    rolePicker(wait)
