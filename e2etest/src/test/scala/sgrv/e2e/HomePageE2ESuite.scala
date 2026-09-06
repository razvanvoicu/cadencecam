package sgrv.e2e

import org.openqa.selenium.By
import org.openqa.selenium.chrome.{ChromeDriver, ChromeOptions}

/** Exercises the real, running backend (started by the e2etest orchestration in build.sbt) through an actual Chrome
  * instance. The build supplies the shared configuration's `LOCAL_BASE_URL` as `E2E_BASE_URL`; the system property
  * remains available as an explicit override.
  */
class HomePageE2ESuite extends munit.FunSuite:
  private val baseUrl = sys.props
    .get("e2e.baseUrl")
    .orElse(sys.env.get("E2E_BASE_URL"))
    .getOrElse(throw new IllegalStateException("E2E_BASE_URL is not configured"))
  private var driver: ChromeDriver = scala.compiletime.uninitialized

  override def beforeAll(): Unit =
    val options = new ChromeOptions()
    options.addArguments("--headless=new")
    driver = new ChromeDriver(options)

  override def afterAll(): Unit =
    if driver != null then driver.quit()

  test("the signed-out home page offers a Google login link"):
    driver.get(baseUrl)
    val loginLink = driver.findElement(By.cssSelector(".login-button"))

    assertEquals(loginLink.getText, "Login with Google")
    val href = loginLink.getDomAttribute("href")
    assert(href.endsWith("/auth/login"), href)
