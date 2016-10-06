package controllers

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File}
import java.net.URLDecoder
import java.sql.Timestamp
import java.util.{Base64, Calendar, TimeZone}
import javax.inject.{Inject, Singleton}

import akka.stream.scaladsl.{FileIO, Source}
import akka.util.{ByteString, Timeout}
import models._
import org.apache.pdfbox.util.PDFMergerUtility
import play.api.db.slick.DatabaseConfigProvider
import play.api.http.{HttpChunk, HttpEntity}
import play.api.i18n.{I18nSupport, Lang, MessagesApi}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.{AnyContent, _}
import slick.driver.JdbcProfile

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import akka.actor.ActorSystem
import dele.book.common.NNet
import org.joda.time.DateTime
import play.api.libs.Files.TemporaryFile
import play.api.libs.iteratee.Iteratee
import play.api.libs.streams.Accumulator
import play.api.mvc.MultipartFormData.FilePart
import play.core.parsers.Multipart.{FileInfo, FilePartHandler}
import play.mvc.Http.MultipartFormData.FileInfo

import scala.util.{Failure, Success}

@Singleton
class Application @Inject()(
                            actorSys:ActorSystem,
                            //pdfDownloader:PDFDownloader,
                            protected val dbConfigProvider: DatabaseConfigProvider,
                            val messagesApi: MessagesApi,
                            implicit val webJarAssets: WebJarAssets
                           )
                           extends Controller with I18nSupport {

  def index = Action {
    /** change the template here to use a different way of compilation and loading of the ts ng2 app.
      * index()  :    does no ts compilation in advance. the ts files are download by the browser and compiled there to js.
      * index1() :    compiles the ts files to individual js files. Systemjs loads the individual files.
      * index2() :    add the option -DtsCompileMode=stage to your sbt task . F.i. 'sbt ~run -DtsCompileMode=stage' this will produce the app as one single js file.
      */
    Ok(views.html.index1())
  }


  def initNNet = Action(parse.multipartFormData) { req =>
    req.body.file("p").map{ p =>
      val f = p.ref.file
      //println(f.getName)
      val j = scala.io.Source.fromFile(f).mkString
      //println(j.length)
      //println(j.substring(0, 20))

      GlobalTest.nnet = NNet.fromJson(j)
      Ok(s"NNet: ${GlobalTest.nnet.layerNodeCounts}\n")

    }.getOrElse(InternalServerError("Failed to initialize NNet\n"))
  }

  def eval = Action.async{ req =>
    if (GlobalTest.nnet == null) {
      Future.successful(InternalServerError("NNet not initialized, post model to /init-nnet first"))
    }
    else {
      val j = req.body.asText.get
      val d = j.split(",")
      val expected = d(0).toInt
      val pixels = d.slice(1, d.length).map(_.toDouble / 255)
      val res = GlobalTest.nnet.evaluate(pixels)
      Future.successful(Ok(s"Result(Expected): $res($expected), match? ${res == expected}\n"))
    }
  }

  def nnet = Action {
    /** change the template here to use a different way of compilation and loading of the ts ng2 app.
      * index()  :    does no ts compilation in advance. the ts files are download by the browser and compiled there to js.
      * index1() :    compiles the ts files to individual js files. Systemjs loads the individual files.
      * index2() :    add the option -DtsCompileMode=stage to your sbt task . F.i. 'sbt ~run -DtsCompileMode=stage' this will produce the app as one single js file.
      */
    val nnet = new NNet(IndexedSeq(2, 2, 2))
    Ok(s"NNet: ${nnet.layerNodeCounts}")
  }
  def redditEdit = Action {
    /** change the template here to use a different way of compilation and loading of the ts ng2 app.
      * index()  :    does no ts compilation in advance. the ts files are download by the browser and compiled there to js.
      * index1() :    compiles the ts files to individual js files. Systemjs loads the individual files.
      * index2() :    add the option -DtsCompileMode=stage to your sbt task . F.i. 'sbt ~run -DtsCompileMode=stage' this will produce the app as one single js file.
      */
    Ok(views.html.redditEdit())
  }
  //val helloActor = actorSys.actorOf(TestActor.props, "hello-actor")

  val dbConfig = dbConfigProvider.get[JdbcProfile]
  val db = dbConfig.db

  implicit val messages = messagesApi.preferred(Seq(Lang("zhs")))

  import play.api.libs.concurrent.Execution.Implicits.defaultContext
  import akka.pattern.ask
  implicit val timeout:Timeout = 5.seconds

  def sayHi = Action.async {implicit rs =>
    Future.successful(Ok("Helloworld!, Hello from Gordon"))
  }

  def testCharts = Action.async {implicit rs =>
    Future.successful(Ok(views.html.chartsTest()))
  }

  val testData =
    """
      |  {
      |    "chart": {
      |      "caption": "Sales - 2012 v 2013",
      |      "numberprefix": "$",
      |      "plotgradientcolor": "",
      |      "bgcolor": "FFFFFF",
      |      "showalternatehgridcolor": "0",
      |      "divlinecolor": "CCCCCC",
      |      "showvalues": "0",
      |      "showcanvasborder": "0",
      |      "canvasborderalpha": "0",
      |      "canvasbordercolor": "CCCCCC",
      |      "canvasborderthickness": "1",
      |      "yaxismaxvalue": "30000",
      |      "captionpadding": "30",
      |      "linethickness": "3",
      |      "yaxisvaluespadding": "15",
      |      "legendshadow": "0",
      |      "legendborderalpha": "0",
      |      "palettecolors": "#f8bd19,#008ee4,#33bdda,#e44a00,#6baa01,#583e78",
      |      "showborder": "0"
      |    },
      |    "categories": [
      |      {
      |        "category": [
      |          {
      |            "label": "Jan"
      |          },
      |          {
      |            "label": "Feb"
      |          },
      |          {
      |            "label": "Mar"
      |          },
      |          {
      |            "label": "Apr"
      |          },
      |          {
      |            "label": "May"
      |          },
      |          {
      |            "label": "Jun"
      |          },
      |          {
      |            "label": "Jul"
      |          },
      |          {
      |            "label": "Aug"
      |          },
      |          {
      |            "label": "Sep"
      |          },
      |          {
      |            "label": "Oct"
      |          },
      |          {
      |            "label": "Nov"
      |          },
      |          {
      |            "label": "Dec"
      |          }
      |        ]
      |      }
      |    ],
      |    "dataset": [
      |      {
      |        "seriesname": "2013",
      |        "data": [
      |          {
      |            "value": "22400"
      |          },
      |          {
      |            "value": "24800"
      |          },
      |          {
      |            "value": "21800"
      |          },
      |          {
      |            "value": "21800"
      |          },
      |          {
      |            "value": "24600"
      |          },
      |          {
      |            "value": "27600"
      |          },
      |          {
      |            "value": "26800"
      |          },
      |          {
      |            "value": "27700"
      |          },
      |          {
      |            "value": "23700"
      |          },
      |          {
      |            "value": "25900"
      |          },
      |          {
      |            "value": "26800"
      |          },
      |          {
      |            "value": "24800"
      |          }
      |        ]
      |      },
      |      {
      |        "seriesname": "2012",
      |        "data": [
      |          {
      |            "value": "10000"
      |          },
      |          {
      |            "value": "11500"
      |          },
      |          {
      |            "value": "12500"
      |          },
      |          {
      |            "value": "15000"
      |          },
      |          {
      |            "value": "16000"
      |          },
      |          {
      |            "value": "17600"
      |          },
      |          {
      |            "value": "18800"
      |          },
      |          {
      |            "value": "19700"
      |          },
      |          {
      |            "value": "21700"
      |          },
      |          {
      |            "value": "21900"
      |          },
      |          {
      |            "value": "22900"
      |          },
      |          {
      |            "value": "20800"
      |          }
      |        ]
      |      }
      |    ]
      |  }
      |
    """.stripMargin
  def getData = {

  }



  val ContentType_AppJson = "application/json"

  private def profile(msg:String, prevTs:Option[Long] = None):Long = {
    val t = new DateTime().getMillis
    if (prevTs.nonEmpty) {
      val diff = t - prevTs.get
      println(s"[PROFILING] $msg: $diff ms")
    }
    else {
      println(s"[PROFILING] $msg")
    }
    t
  }


  val encoding = "UTF-8"
}
