package sgrv.be.auth

import java.util.Base64
import zio.{Random, UIO, ZIO, ZLayer}

trait TokenGenerator:
  def generate(bytes: Int): UIO[String]

private[be] object TokenGenerator:
  def generate(bytes: Int): ZIO[TokenGenerator, Nothing, String] =
    ZIO.serviceWithZIO[TokenGenerator](_.generate(bytes))

  val live: ZLayer[Any, Nothing, TokenGenerator] =
    ZLayer.succeed:
      new TokenGenerator:
        override def generate(bytes: Int): UIO[String] =
          Random
            .nextBytes(bytes)
            .map: value =>
              Base64.getUrlEncoder.withoutPadding.encodeToString(value.toArray)
