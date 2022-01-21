import io.lemonlabs.uri.Url
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import org.jsoup.nodes.DocumentType

import scala.sys.process.ProcessBuilder
import io.lemonlabs.uri.AbsoluteUrl
import io.lemonlabs.uri.UrlWithAuthority

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

    val div = doc >> element("div#main-nosub")
    val latestLink = div >?> element("a") >> attr("href")

    latestLink.map { li =>
      val vertex = Url.parse(li)
      val url = vertex match {
        case UrlWithAuthority(_, _, _, _) => vertex
        case otherwise =>
          tokyolgjpUrl.withPathParts(vertex.path.parts)
      }
      browser.get(url.toString())
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
    val alternateLink =
      (doc >?> elementList("a.icon_pdf")).headOption.flatMap(_.headOption)
    val docPath =
      (link orElse alternateLink).map(li =>
        siblingPath(Url.parse(doc.location), Url.parse(li.attr("href")))
      )
    docPath.map(_.toJavaURI)
  }

  def fetchPdf(url: Url): ProcessBuilder = {
    import scala.sys.process.Process

    Process(List("curl", url.toString()))
  }

  def pdf2txt(proc: ProcessBuilder): ProcessBuilder = {
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

  val doc = browser.get(tokyolgjpUrl.toString())

  val current = toCurrentStatus(doc)
  val pdfLink = current.flatMap(getPdf).map(url => Url.parse(url.toString()))

  val num = for {
    pdfLink1 <- pdfLink
    pdfProc <- Some(pdf2txt(fetchPdf(pdfLink1)).!!)
    notSanitizedCount <- pdfProc
      .split("\n")
      .toSeq
      .filter(_.contains("本日判明分："))
      .headOption
    sanitizedCount <-
      Some(
        notSanitizedCount.substring(6).reverse.substring(1).filterNot(_ == '，')
      )
    num <- sanitizedCount.map(zen2num).mkString.toIntOption
  } yield num

  num match {
    case Some(value) => println(value); sys.exit(0)
    case None        => sys.exit(1)
  }

}
