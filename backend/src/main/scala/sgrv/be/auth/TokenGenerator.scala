package sgrv.be.auth

import java.security.SecureRandom
import java.util.Base64
import zio.{UIO, ZIO, ZLayer}

trait TokenGenerator:
  def generate(bytes: Int): UIO[String]

private[be] object TokenGenerator:
  def generate(bytes: Int): ZIO[TokenGenerator, Nothing, String] =
    ZIO.serviceWithZIO[TokenGenerator](_.generate(bytes))

  val live: ZLayer[Any, Nothing, TokenGenerator] =
    ZLayer.succeed:
      val random = SecureRandom()
      new TokenGenerator:
        override def generate(bytes: Int): UIO[String] =
          ZIO.succeed:
            val value = new Array[Byte](bytes)
            random.nextBytes(value)
            Base64.getUrlEncoder.withoutPadding.encodeToString(value)
