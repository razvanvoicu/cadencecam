package sgrv.e2e

import org.openqa.selenium.By
import org.openqa.selenium.chrome.{ChromeDriver, ChromeOptions}
import org.openqa.selenium.support.ui.{ExpectedConditions, WebDriverWait}

import java.time.{Duration, Instant}
import scala.jdk.CollectionConverters.*

/** Exercises the authenticated Sheets flow (create/update a spreadsheet, then read its rows back) against the real
  * backend, attaching to the visible Chrome instance `e2etest/launchTestBrowser` leaves running rather than launching a
  * fresh, signed-out one. Requires having already signed in by hand in that window first (see `launchTestBrowser`'s
  * instructions) — Google blocks WebDriver-controlled browsers from driving its login form directly. Run via
  * `e2etest/testAuthenticated`, not `e2etest/test`.
  */
class SampleSpreadsheetE2ESuite extends munit.FunSuite:
  private val baseUrl = sys.props
    .get("e2e.baseUrl")
    .orElse(sys.env.get("E2E_BASE_URL"))
    .getOrElse(throw new IllegalStateException("E2E_BASE_URL is not configured"))
  private val debuggerAddress = sys.props.getOrElse("e2e.debuggerAddress", "127.0.0.1:9222")
  private var driver: ChromeDriver = scala.compiletime.uninitialized

  override def beforeAll(): Unit =
    val options = new ChromeOptions()
    options.setExperimentalOption("debuggerAddress", debuggerAddress)
    driver = new ChromeDriver(options)

  // Attached via debuggerAddress rather than launched by this driver: quit() only ends this WebDriver session,
  // it does not close the real, user-owned browser window.
  override def afterAll(): Unit =
    if driver != null then driver.quit()

  test("the About link opens authenticated build information in a modal"):
    driver.get(baseUrl)
    val wait = new WebDriverWait(driver, Duration.ofSeconds(20))

    assertEquals(driver.findElement(By.cssSelector(".logout-link")).getText, "Logout")
    wait.until(ExpectedConditions.elementToBeClickable(By.cssSelector(".about-link"))).click()
    val dialog = wait.until(ExpectedConditions.visibilityOfElementLocated(By.cssSelector(".about-dialog")))
    val labels = dialog.findElements(By.cssSelector(".about-details dt")).asScala.map(_.getText).toSeq
    val values = dialog.findElements(By.cssSelector(".about-details dd")).asScala.map(_.getText).toSeq

    assertEquals(labels, Seq("App version", "Build date", "Build OS", "Scala version", "Scala.js version"))
    assertEquals(values.size, labels.size)
    values.foreach(value => assert(value.nonEmpty))
    dialog.findElement(By.cssSelector(".about-close")).click()

  test("creating/updating the sample spreadsheet appends a fresh, readable row"):
    driver.get(baseUrl)
    val wait = new WebDriverWait(driver, Duration.ofSeconds(20))

    val nameInput =
      wait.until(ExpectedConditions.presenceOfElementLocated(By.cssSelector(".sheet-row input[type='text']")))
    nameInput.clear()
    nameInput.sendKeys("sample")

    wait.until(ExpectedConditions.elementToBeClickable(By.cssSelector(".sheet-row button"))).click()

    val lastRow =
      wait.until(ExpectedConditions.presenceOfElementLocated(By.cssSelector("table.sheet-table tbody tr:last-child")))
    val timestampText = lastRow.findElement(By.tagName("td")).getText
    val timestamp = Instant.parse(timestampText)
    val age = Duration.between(timestamp, Instant.now()).abs()

    assert(age.getSeconds <= 30, s"Last row's timestamp ($timestampText) is ${age.getSeconds}s old, expected <= 30s")
