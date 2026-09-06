package sgrv.fe

import org.scalajs.dom

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js.Thenable.Implicits.*

/** The frontend's stateless HTTP boundary. Session lifecycle policy belongs to the callback's owner. */
private[fe] final class HttpService(onUnauthorized: () => Unit):
  def get(url: String): Future[dom.Response] = intercept(dom.fetch(url))

  def send(url: String, init: dom.RequestInit): Future[dom.Response] = intercept(dom.fetch(url, init))

  private def intercept(response: Future[dom.Response]): Future[dom.Response] =
    response.map: value =>
      observeStatus(value.status)
      value

  private[fe] def observeStatus(status: Int): Unit =
    if status == 401 then onUnauthorized()
