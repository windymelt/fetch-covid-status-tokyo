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
        case UrlWithAuthority(_, _, _, _) => vertex // 絶対リンクの場合
        case otherwise => // 相対リンクなので調整する
          tokyolgjpUrl.withPathParts(vertex.path.parts)
      }
      browser.get(url.toString())
    }
  }

  // ./foo.pdf のようなURLを絶対URLとして解決する
  def siblingPath(base: Url, relativeUrl: Url): Url = {
    import io.lemonlabs.uri.UrlPath

    val basePathWithoutFile =
      base.path.parts.filterNot(_.isEmpty()).reverse.tail.reverse
    base
      .withPath(UrlPath(basePathWithoutFile))
      .addPathParts(relativeUrl.path.parts)
  }

  def getPdfLink(doc: browser.DocumentType) = {
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

    Process(List("curl", "-sSl", url.toString()))
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

  def extractCount(s: String): Option[String] = {
    val pattern = """本日判明分：([１２３４５６７８９０，]+).*""".r
    s match {
      case pattern(count) => Some(count.filterNot(_ == '，'))
      case _              => None
    }
  }

  val doc = browser.get(tokyolgjpUrl.toString())

  val current = toCurrentStatus(doc)
  val maybePdfLink =
    current.flatMap(getPdfLink).map(url => Url.parse(url.toString()))

  val num = for {
    pdfLink <- maybePdfLink
    pdfTxt <- Some((fetchPdf _).andThen(pdf2txt)(pdfLink).!!)
    zenkakuCount <- pdfTxt
      .split("\n")
      .flatMap(extractCount)
      .headOption
    num <- zenkakuCount.map(zen2num).mkString.toIntOption
  } yield num

  num match {
    case Some(value) => println(value); sys.exit(0)
    case None        => sys.exit(1)
  }

}
