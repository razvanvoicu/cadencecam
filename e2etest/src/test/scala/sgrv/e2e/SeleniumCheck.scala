package sgrv.e2e

import org.openqa.selenium.chrome.{ChromeDriver, ChromeOptions}

/** Verifies Chrome + a matching driver are actually launchable before the E2E suite bothers starting the backend
  * server. Selenium Manager (bundled in selenium-java 4.6+) auto-resolves chromedriver for whatever Chrome is
  * installed; this just exercises that resolution and a real launch/quit up front, so a missing/broken Chrome install
  * fails fast with a clear message instead of surfacing as a confusing failure inside the first test.
  */
object SeleniumCheck:
  def main(args: Array[String]): Unit =
    val options = new ChromeOptions()
    options.addArguments("--headless=new")
    val driver =
      try new ChromeDriver(options)
      catch
        case error: Throwable =>
          throw new IllegalStateException(
            "Selenium could not launch Chrome. Ensure Google Chrome is installed; Selenium Manager " +
              "auto-downloads a matching chromedriver, but needs the browser itself to already be present.",
            error
          )
    try ()
    finally driver.quit()
