package org.bitbucket.inkytonik.cooma.primitive

import java.net.InetSocketAddress

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer => Server}
import org.bitbucket.inkytonik.cooma.Backend

trait HttpServer {

    self : Backend =>

    def serverStart(port : Int) : ValueR = {
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

}
