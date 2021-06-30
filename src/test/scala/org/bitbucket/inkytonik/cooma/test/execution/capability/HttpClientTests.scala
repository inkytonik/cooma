package org.bitbucket.inkytonik.cooma.test.execution.capability

import cats.effect.{ConcurrentEffect, ExitCode, IO, IOApp, Timer}
import fs2.Stream
import org.bitbucket.inkytonik.cooma.test.ExecutionTests
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import scalaj.http.Http

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.global
import scala.util.Try

class HttpClientTests extends ExecutionTests {

    object HttpServer extends IOApp {
        override def run(args : List[String]) : IO[ExitCode] =
            stream[IO].compile.drain.as(ExitCode.Success)

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

    lazy val thread =
        new Thread(() => Try(HttpServer.main(Array())))

    {
        // start the thread and wait for the server to become reachable
        thread.start()
        @tailrec
        def aux() : Unit =
            if (Try(Http("http://localhost:8080").asBytes).isSuccess) ()
            else {
                Thread.sleep(100L)
                aux()
            }
        aux()
    }

    {
        val filename = "src/test/resources/capability/httpGet.cooma"
        val name = s"single HTTP capability ($filename)"
        val args = Seq("http://localhost:8080")

        test(s"run: $name") { implicit bc =>
            val result = runFile(filename, Seq("-r"), args)
            result shouldBe "< Right = { code = 200, body = \"GET / response\" } >\n"
        }
    }

    {
        val filename = "src/test/resources/capability/httpGetFoo.cooma"
        val name = s"single HTTP capability with suffix ($filename)"
        val args = Seq("http://localhost:8080")

        test(s"run: $name") { implicit bc =>
            val result = runFile(filename, Seq("-r"), args)
            result shouldBe "< Right = { code = 200, body = \"GET /foo response\" } >\n"
        }
    }

    {
        val filename = "src/test/resources/capability/httpGetPostPut.cooma"
        val name = s"multiple HTTP capabilities ($filename)"
        val args = Seq("http://localhost:8080")

        test(s"run: $name") { implicit bc =>
            val result = runFile(filename, Seq("-r"), args)
            result shouldBe "{ x0 = \"GET / response\", x1 = \"POST / response\", x2 = \"PUT / response\" }\n"
        }
    }

    {
        val filename = "src/test/resources/capability/httpNotPermitted.cooma"
        val name = s"HTTP capability not permitted ($filename)"
        val args = Seq("http://localhost:8080")

        test(s"run: $name") { implicit bc =>
            val result = runFile(filename, Seq("-r"), args)
            result shouldBe
                """|src/test/resources/capability/httpNotPermitted.cooma:2:16:error: delete is not a field of record type {
                   |  get : (suffix : String) <
                   |    Left : String,
                   |    Right : {
                   |      code : Int,
                   |      body : String
                   |    }
                   |  >,
                   |  put : (suffix : String) <
                   |    Left : String,
                   |    Right : {
                   |      code : Int,
                   |      body : String
                   |    }
                   |  >
                   |}
                   |    httpClient.delete("")
                   |               ^
                   |""".stripMargin
        }
    }

    override def finalize() : Unit =
        thread.interrupt()

}
