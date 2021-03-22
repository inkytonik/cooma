package org.bitbucket.inkytonik.cooma

import cats.effect.{ConcurrentEffect, ExitCode, IO, IOApp, Timer}
import fs2.Stream
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext.global

object HttpServer extends IOApp {

    override def run(args : List[String]) : IO[ExitCode] =
        HttpServer.stream[IO].compile.drain.as(ExitCode.Success)

    def stream[F[_] : ConcurrentEffect](implicit timer : Timer[F]) : Stream[F, Nothing] = {
        val routes = {
            val dsl = new Http4sDsl[F] {}; import dsl._
            HttpRoutes.of[F] {
                case DELETE -> Root      => Ok("DELETE / response")
                case GET -> Root         => Ok("GET / response")
                case POST -> Root        => Ok("POST / response")
                case PUT -> Root         => Ok("PUT / response")
                case GET -> Root / "foo" => Ok("GET /foo response")
            }
        }
        BlazeServerBuilder(global)
            .bindHttp(8080, "0.0.0.0")
            .withHttpApp(routes.orNotFound)
            .serve
            .drain
    }

}
