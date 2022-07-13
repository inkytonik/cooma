package org.bitbucket.inkytonik.cooma.primitive

import java.net.InetSocketAddress

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer => Server}
import org.bitbucket.inkytonik.cooma.Backend
import org.bitbucket.inkytonik.cooma.Config
import java.io.ByteArrayOutputStream

trait HttpServer {

    self : Backend =>

    case class Instance(
        port : Int,
        env : Env,
        endpoints : Map[String, Value]
    ) {

        val server = Server.create(new InetSocketAddress(port), 0)

        val handler =
            new HttpHandler {
                override def handle(xchg : HttpExchange) : Unit = {
                    val body = {
                        val stream = xchg.getRequestBody()
                        val result = new ByteArrayOutputStream
                        val buffer = new Array[Byte](1024)
                        def aux() : String = {
                            val length = stream.read(buffer)
                            if (length == -1)
                                result.toString("UTF-8")
                            else {
                                result.write(buffer, 0, length)
                                aux()
                            }
                        }
                        aux()
                    }
                    endpoints.get(xchg.getRequestURI.getPath) match {
                        case Some(endpoint) =>
                            val term =
                                letC(
                                    "$hk1",
                                    "$hf",
                                    letC(
                                        "$hk2",
                                        "$hx",
                                        letC(
                                            "$hk3",
                                            "$hr4",
                                            appC(haltC(), "$hr4"),
                                            appF("f", "$hk3", "$hx")
                                        ),
                                        letV(
                                            "$hs5",
                                            strV(body),
                                            appC(idnC("$hk2"), "$hs5")
                                        )
                                    ),
                                    letV(
                                        "$hf6",
                                        endpoint, // handler goes here
                                        appC(idnC("$hk1"), "$hf6")
                                    )
                                )
                            val (response, code) =
                                interpret(term, env, Seq.empty, new Config(Seq("-r"))) match {
                                    case Right(Result(_, out)) =>
                                        isStrR(out) match {
                                            case Some(out) => (out, 200)
                                            case None      => ("expected String", 500)
                                        }
                                    case Left(message) =>
                                        (message, 500)
                                }
                            xchg.sendResponseHeaders(code, response.length)
                            val os = xchg.getResponseBody
                            os.write(response.getBytes)
                            os.close()
                        case None =>
                            xchg.sendResponseHeaders(404, 0)
                            val os = xchg.getResponseBody
                            os.close()
                    }
                }
            }

    }

    def serverStart(port : Int, env : Env) : ValueR = {
        val server = Server.create(new InetSocketAddress(port), 0)
        val handler =
            new HttpHandler {
                override def handle(xchg : HttpExchange) : Unit = {
                    val response = "Hello, world!"
                    xchg.sendResponseHeaders(200, response.length)
                    val os = xchg.getResponseBody
                    os.write(response.getBytes)
                    os.close()
                }
            }
        server.createContext("/", handler)
        server.setExecutor(null)
        server.start()
        System.in.read()
        server.stop(0)
        uniR
    }

    def serverGetResponse(term : Term, env : Env) : Either[String, String] =
        interpret(term, env, Seq.empty, new Config(Seq("-r")))
            .flatMap { case Result(_, out) => isStrR(out).toRight("expected String") }

    /**
     * Creates the term to evaluate.
     *
     * @param f    the name of the endpoint function in the environment
     * @param x    the body of the request
     * @return     the term to evaluate for this request
     */
    protected def createTerm(f : String, x : String) : Term = ???

}
