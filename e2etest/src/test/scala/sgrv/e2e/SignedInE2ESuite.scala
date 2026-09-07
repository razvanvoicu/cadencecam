package sgrv.e2e

import org.openqa.selenium.By
import org.openqa.selenium.chrome.{ChromeDriver, ChromeOptions}
import org.openqa.selenium.support.ui.{ExpectedConditions, WebDriverWait}

import java.net.{InetSocketAddress, Socket}
import java.time.Duration
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Exercises what only a signed-in session can reach — the role-selection funnel and the About modal — against the real
  * backend, attaching to the visible Chrome instance `e2etest/launchTestBrowser` leaves running rather than launching a
  * fresh, signed-out one. Requires having already signed in by hand in that window first (see `launchTestBrowser`'s
  * instructions) — Google blocks WebDriver-controlled browsers from driving its login form directly. Run via
  * `e2etest/testAuthenticated`, not `e2etest/test`.
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

  test("the About link opens authenticated build information in a modal"):
    val wait = load()

    assertEquals(driver.findElement(By.cssSelector(".logout-link")).getText, "Logout")
    wait.until(ExpectedConditions.elementToBeClickable(By.cssSelector(".about-link"))).click()
    val dialog = wait.until(ExpectedConditions.visibilityOfElementLocated(By.cssSelector(".about-dialog")))
    val labels = dialog.findElements(By.cssSelector(".about-details dt")).asScala.map(_.getText).toSeq
    val values = dialog.findElements(By.cssSelector(".about-details dd")).asScala.map(_.getText).toSeq

    assertEquals(labels, Seq("App version", "Build date", "Build OS", "Scala version", "Scala.js version"))
    assertEquals(values.size, labels.size)
    values.foreach(value => assert(value.nonEmpty))
    dialog.findElement(By.cssSelector(".about-close")).click()

  test("a signed-in session lands on the role selection and can enter and leave each role"):
    val wait = load()

    val choices = wait
      .until(ExpectedConditions.numberOfElementsToBe(By.cssSelector(".mode-choices .mode-button"), 2))
      .asScala
      .toSeq
    assertEquals(choices.map(_.findElement(By.cssSelector(".mode-title")).getText), Seq("Signal acquirer", "Dashboard"))

    Seq(".mode-acquirer" -> "Signal acquirer", ".mode-dashboard" -> "Dashboard").foreach { case (choice, heading) =>
      wait.until(ExpectedConditions.elementToBeClickable(By.cssSelector(choice))).click()
      val screen = wait.until(ExpectedConditions.visibilityOfElementLocated(By.cssSelector(".screen")))
      assertEquals(screen.findElement(By.cssSelector(".screen-title")).getText, heading)
      screen.findElement(By.cssSelector(".back-button")).click()
      wait.until(ExpectedConditions.visibilityOfElementLocated(By.cssSelector(".mode-choices")))
    }
