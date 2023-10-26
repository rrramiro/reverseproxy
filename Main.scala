//> using scala 2.13
//> using lib "org.http4s::http4s-ember-server:0.23.11"
//> using lib "org.http4s::http4s-ember-client:0.23.11"
//> using lib "org.http4s::http4s-circe:0.23.11"
//> using lib "org.slf4j:slf4j-simple:1.7.36"


import cats.~>
import cats.implicits._
import cats.arrow.FunctionK
import cats.data.OptionT
import cats.data.Kleisli
import cats.effect._
import cats.effect.implicits._
import cats.effect.kernel.Outcome

import scala.concurrent.ExecutionContext
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.circe.middleware.JsonDebugErrorHandler
import org.http4s.headers.{Host => HostHeader}
import org.http4s.headers.`User-Agent`
import org.http4s._
import com.comcast.ip4s._
import org.slf4j.LoggerFactory


object Main extends IOApp {

  private val accessLogger = LoggerFactory.getLogger("access")
  private def accessLog[G[_]: MonadCancelThrow: Clock, F[_]: Sync](
      fk: F ~> G
  )(http: Http[G, F]): Http[G, F] =
    Kleisli { request =>
      (
        for {
          rt <- Clock[G].realTimeInstant
          response <- http(request)
          ts <- Clock[G].realTimeInstant
          responseTime = (ts.toEpochMilli - rt.toEpochMilli).toDouble / 1000.0d
          userAgent = request.headers.get[`User-Agent`].map(_.toString()).getOrElse("-")
          _ <- accessLogger.info(s"""${request.serverAddr.map(_.toString()).getOrElse("-")} [${ts.toString()}] "${request.uri.toString()}" ${response.status.code} ${response.contentLength.getOrElse(-1L)} ${responseTime} ${request.method.name} ; ${userAgent} """).pure[G]
        } yield response
      ).guaranteeCase {
        case Outcome.Errored(e) =>
          fk(Sync[F].delay(accessLogger.error("service raised an error", e)))
        case Outcome.Canceled() =>
          fk(Sync[F].delay(accessLogger.error("service canceled response for request")))
        case Outcome.Succeeded(_) => MonadCancelThrow[G].unit
      }
    }


  val targetUri = Uri.unsafeFromString("https://httpbin.org")

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      client <- EmberClientBuilder.default[IO].build
      server <- EmberServerBuilder
        .default[IO]
        .withHost(host"0.0.0.0")
        .withPort(port"9000")
        .withHttp2
        .withHttpApp(
          JsonDebugErrorHandler(
            accessLog(OptionT.liftK[IO]) (
            HttpRoutes.of[IO] { req =>
              client.toHttpApp(
                req
                  .removeHeader[HostHeader]
                  .withUri(targetUri.addPath(req.uri.path.renderString))
              )
            }
           ),
            _ => false
          ).orNotFound
        )
        .build
    } yield ExitCode.Success
  }.useForever

}
