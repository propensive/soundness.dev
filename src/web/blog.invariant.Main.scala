package blog.invariant

import anticipation.*
import contingency.*
import digression.*
import escapade.*
import eucalyptus.*
import fulminate.*
import gesticulate.*
import gossamer.*
import harlequin.*
import hellenism.*
import hieroglyph.*
import honeycomb.*
import nettlesome.*
import parasite.*
import punctuation.*
import rudiments.*
import scintillate.*
import serpentine.*
import spectacular.*
import turbulence.*
import vacuous.*

import logFormats.ansiStandard
import classloaders.scala
import charEncoders.utf8
import charDecoders.utf8
import encodingMitigation.skip
import asyncOptions.cancelOrphans
import threadModels.platform
import hierarchies.simple
import stdioSources.virtualMachine.ansi
import htmlRendering.scalaSyntax

given Realm = realm"invariant"
given Message is Loggable = safely(supervise(Log(Out))).or(Log.silent)

given AppError is Fatal = error =>
  Out.println(error.stackTrace.teletype)
  ExitStatus.Fail(1)

case class AppError(detail: Message) extends Error(detail)

def page(menu: Html[Flow], content: Html[Flow]*): HtmlDoc =
  HtmlDoc(Html
   (Head
     (Title(t"Invariant.blog"),
      Meta(charset = enc"UTF-8"),
      Link(rel = Rel.Stylesheet, href = % / p"styles.css"),
      Link(rel = Rel.Icon, href = % / p"images" / p"logo.svg")),
    Body
     (Nav(Ul
       (Li(A(href = %)(t"Home")),
        Li(A(href = % / p"about")(t"About")),
        Li(A(href = % / p"contact")(t"Contact")))),
      Header(Img(src = % / p"images" / p"panorama2.webp")),
      Main
       (menu, Article(content*)),
      Footer
       (Img(src = % / p"images" / p"panorama.webp"),
        P(t"© Copyright 2024 Jon Pretty & Propensive")))))

@main
def server(): Unit =
  quash:
    case ConcurrencyError(reason) =>
      Out.println(m"There was a concurrency error")
      ExitStatus.Fail(2).terminate()
    case AppError(message) =>
      Out.println(message)
      ExitStatus.Fail(1).terminate()
  .within:
    supervise(tcp"8080".serve[Http](handle))

object Service extends Servlet(handle)

def handle(using HttpRequest): HttpResponse[?] =
  quash:
    case MarkdownError(detail) =>
      HttpResponse(page(Aside, H1(t"Bad markdown")))
    case ClasspathError(path) =>
      HttpResponse(page(Aside, H1(t"Path $path not found")))
    case PathError(path, reason) =>
      HttpResponse(page(Aside, P(t"The path $path is not valid because $reason")))
  .within:
    request.path match
      case % / p"images" / image =>
        HttpResponse(Classpath / p"images" / Name(image.render))

      case % / p"styles.css" =>
        HttpResponse(Classpath / p"styles.css")

      case % / Name(post) =>
        val markdown = Markdown.parse((Classpath / p"posts" / Name(t"$post.md"))())
        HttpResponse(page(Aside, markdown.html*))

      case _ =>
        HttpResponse(page(Div, H1(t"Not found"), P(t"This page does not exist.")))
