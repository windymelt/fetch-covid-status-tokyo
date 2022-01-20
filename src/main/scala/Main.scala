import org.jsoup.nodes.DocumentType
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.browser.Browser
import java.net.URL
import java.net.URI
import io.lemonlabs.uri.Url
import scala.sys.process

object Main extends App {
  val browser = JsoupBrowser()
  val tokyolgjp =
    "https://www.fukushihoken.metro.tokyo.lg.jp/hodo/saishin/hassei.html"
  val tokyolgjpUrl = Url.parse(tokyolgjp)
  def toCurrentStatus(
      doc: browser.DocumentType
  ) = {
    import net.ruippeixotog.scalascraper.dsl.DSL._
    import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
    import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

    import net.ruippeixotog.scalascraper.model._

    val latestLink = doc >?> element("a.innerLink") >> attr("href")

    latestLink.flatMap { li =>
      val url =
        tokyolgjpUrl.withPathParts(li)

      Some(browser.get(url.toString()))
    }
  }
  def siblingPath(base: Url, vertex: Url): Url = {
    import io.lemonlabs.uri.UrlPath

    val basePathWithoutFile =
      base.path.parts.filterNot(_.isEmpty()).reverse.tail.reverse
    base.withPath(UrlPath(basePathWithoutFile)).addPathParts(vertex.path.parts)
  }

  def getPdf(doc: browser.DocumentType) = {
    import net.ruippeixotog.scalascraper.dsl.DSL._
    import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
    import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

    import net.ruippeixotog.scalascraper.model._

    val link =
      (doc >?> elementList("a.resourceLink")).headOption.flatMap(_.headOption)
    val docPath =
      link.map(li =>
        siblingPath(Url.parse(doc.location), Url.parse(li.attr("href")))
      )
    docPath.map(_.toJavaURI)
  }

  def fetchPdf(url: Url): process.ProcessBuilder = {
    import scala.sys.process.Process

    Process(List("curl", url.toString()))
  }

  def pdf2txt(proc: process.ProcessBuilder): process.ProcessBuilder = {
    import scala.sys.process.Process
    Process(List("pdftotext", "-", "-")).#<(proc)
  }

  def zen2num(c: Char): Int = c match {
    case '１' => 1
    case '２' => 2
    case '３' => 3
    case '４' => 4
    case '５' => 5
    case '６' => 6
    case '７' => 7
    case '８' => 8
    case '９' => 9
    case '０' => 0
  }
  import net.ruippeixotog.scalascraper.browser.JsoupBrowser

  val doc = browser.get(tokyolgjpUrl.toString())

  val current = toCurrentStatus(doc)
  val pdfLink = current.flatMap(getPdf).map(url => Url.parse(url.toString()))

  println(
    pdfLink
      .map(fetchPdf)
      .map(pdf2txt)
      .map(_.!!)
      .flatMap(_.split("\n").toSeq.filter(_.contains("本日判明分：")).headOption)
      .map(_.substring(6).reverse.substring(1).filterNot(_ == '，'))
      .flatMap(_.map(zen2num).mkString.toIntOption)
  )
}
