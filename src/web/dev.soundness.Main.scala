package dev.soundness

import scala.collection.mutable as scm

import soundness.*
import honeycomb.*
import punctuation.*
import scintillate.*
import telekinesis.{HttpRequest as _, HttpResponse as _, *}

import logFormats.ansiStandard
import classloaders.scala
import charEncoders.utf8
import charDecoders.utf8
import sanitization.skip
import orphanDisposal.cancel
import threadModels.platform
import serpentine.hierarchies.simple
import stdioSources.virtualMachine.ansi
import htmlRenderers.scalaSyntax

given Realm = realm"soundness"
given Message is Loggable = safely(supervise(Log.route(Out))).or(Log.silent)
given Online = Online

def page(content: Html[Flow]*): HtmlDoc =
  HtmlDoc(Html
   (Head
     (Title(t"Soundness.dev"),
      Meta(charset = enc"UTF-8"),
      Link(rel = Rel.Stylesheet, href = % / p"styles.css"),
      Link(rel = Rel.Icon, href = % / p"images" / p"logo2.svg")),
    Body
     (Nav(Ul
       (Li(A(href = %)(t"home")),
        Li(A(href = url"https://github.com/propensive/soundness")(t"contribute")),
        Li(A(href = url"https://discord.com/invite/MBUrkTgMnA")(t"discuss")))),
      Main(content),
      Footer(t"© Copyright 2024 Propensive"))))

val libraries = List
 (t"kaleidoscope", t"quantitative", t"dendrology", t"contingency", t"vacuous", t"iridescence",
  t"turbulence", t"symbolism", t"nettlesome", t"larceny", t"metamorphose", t"wisteria").sorted

object Slogan:
  private val cache: scm.HashMap[Text, Text] = scm.HashMap()
  def apply(library: Text): Text raises HttpError = cache.establish(library):
    url"https://raw.githubusercontent.com/propensive/$library/main/doc/slogan.md".get().as[Text]

def home = page
 (H1
   (t"Correctness, security and performance",
    Br,
    t"Invariant from prototype to production"),
  Section(P.intro
   (B(t"Soundness"), t" is an ecosystem of libraries for advanced software development in ",
         B(t"direct-style Scala 3"), t" prioritizing ", B(t"typesafety"), t" and ",
         B(t"correctness"), t" enforced pervasively through ", B(t"expressive"), t" and ",
         B(t"aesthetic"), t" code. Projects bult on Soundness are ", B(t"safe"), t", ",
         B(t"maintainable"), t" and ", B(t"sound"))),
  Main
   (Div(libraries.map: library =>
      Div.library
       (A(href = unsafely(% / Name(library)))(Img(src = url"https://raw.githubusercontent.com/propensive/$library/main/doc/logo.svg")),
        H3(library.capitalize),
        P(quash { case _: HttpError => t"" }.within(Slogan(library)))))))

@main
def server(): Unit =
  quash:
    case ConcurrencyError(reason) =>
      Out.println(m"There was a concurrency error")
      ExitStatus.Fail(2).terminate()

  .within:
    supervise(tcp"8080".serve[Http](handle))

class Service() extends JavaServlet(handle)

object Data:
  private val cache: scm.HashMap[HttpUrl, HtmlDoc] = scm.HashMap()

  def apply(project: Text): HtmlDoc raises HttpError raises MarkdownError =
    val url = url"https://raw.githubusercontent.com/propensive/$project/main/doc/basics.md"
    cache.establish(url)(page((H2(project.capitalize) +: Markdown.parse(url.get().as[Text]).html)*))


def handle(using HttpRequest): HttpResponse[?] =
  quash:
    case MarkdownError(detail) =>
      HttpResponse(page(Aside, H1(t"Bad markdown")))

    case ClasspathError(path) =>
      HttpResponse(page(Aside, H1(t"Path $path not found")))

    case PathError(path, reason) =>
      HttpResponse(page(Aside, P(t"$path is not valid because $reason")))

    case HttpError(_, _) =>
      HttpResponse(page(Aside, P(t"Could not download the remote page")))

  .within:
    request.path match
      case % =>
        HttpResponse(home)

      case % / p"styles.css" =>
        HttpResponse(Classpath / p"styles.css")

      case % / p"images" / Name(image) =>
        val resource = Classpath / p"images" / Name(image)

        if resource.exists() then HttpResponse(resource)
        else HttpResponse(NotFound(page(H1(t"Not found"), P(t"The image was not found."))))

      case % / Name(project) =>
        HttpResponse(Data(project))

      case _ =>
        HttpResponse(page(Div, H1(t"Not found"), P(t"This page does not exist.")))
