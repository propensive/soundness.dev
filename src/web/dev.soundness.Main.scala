package dev.soundness

import scala.collection.mutable as scm

import soundness.*
import honeycomb.{Sub as _, Del as _, Ins as _, *}
import jacinta.*
import xylophone.*
import amok.*
import merino.*
import scintillate.*
import telekinesis.{HttpRequest as _, HttpResponse as _, *}

import logFormats.ansiStandard
import classloaders.scala
import charEncoders.utf8
import charDecoders.utf8
import orphanDisposal.cancel
import threadModels.platform
import pathHierarchies.simple
import stdioSources.virtualMachine.ansi
import textSanitizers.skip

given (using Tactic[XmlParseError]) => HtmlConverter = HtmlConverter(ScalaRenderer, JavaRenderer,
    AmokRenderer, mathMlRenderer, monoRenderer)

given Realm = realm"soundness"
given Message is Loggable = safely(supervise(Log.route(Out))).or(Log.silent)
given Online = Online
erased given ConcurrencyError is Unchecked = ###
erased given JsonParseError is Unchecked = ###
erased given JsonError is Unchecked = ###

given InitError is Fatal = error => ExitStatus.Fail(1)

case class InitError(msg: Message) extends Error(msg)

val count = Counter(0)

case class Transform
    (`match`: Selection,
     before:  Optional[Text],
     after:   Optional[Text],
     replace: Optional[Text]):

  def apply(text: Text): Text = `match`.findIn(text).lay(text): (start, end) =>
    val replacement = t"${before.or(t"")}${replace.or(text.slice(Ordinal.zerary(start) ~ Ordinal.natural(end)))}${after.or(t"")}"
    t"${text.s.substring(0, start).nn}$replacement${text.s.substring(end).nn}"

case class Fragment
    (syntax:    Text,
     highlight: List[Highlight],
     error:     List[Highlight],
     caution:   List[Highlight],
     transform: Optional[Transform])

case class Selection(start: Text, end: Optional[Text]):
  def findIn(text: Text): Optional[(Int, Int)] =
    val startIndex = text.s.indexOf(start.s)

    if startIndex < 0 then Unset else
      end.lay((startIndex, startIndex + start.length)): rangeEnd =>
        val endIndex = text.s.indexOf(rangeEnd, startIndex + start.length)
        if endIndex < 0 then Unset else (startIndex, endIndex + rangeEnd.length)

object Selection:
  given Decoder[Selection] =
    case r"$start(.*)\.\.$end(.*)" => Selection(start, end)
    case other                     => Selection(other, Unset)

case class Highlight(selection: Selection, caption: Optional[Text]):
  def rangeIn(text: Text, style: Markup.Style): Optional[Range] =
    selection.findIn(text).let { case (start, end) => Range(start, end, style, caption) }

case class Range(start: Int, end: Int, style: Markup.Style, caption: Optional[Text]):
  def length: Int = end - start

object Markup:
  enum Style:
    case Erroneous, Highlight, Caution

  export Style.*

case class Markup(tokens: List[SourceToken], style: Markup.Style, caption: Optional[Text])

def monoRenderer = new Renderer(t"mono"):
  def render(meta: Optional[Text], content: Text): Seq[Html[Flow]] = List(Div.mono(Div.amok(Pre(content))))

def mathMlRenderer: Renderer raises XmlParseError = new Renderer(t"mathml"):
  def render(meta: Optional[Text], content: Text): Seq[Html[Flow]] =
    List(HtmlXml(Xml.parse(content)))


def page(aside: Html[Flow], content: Html[Flow]*): HtmlDoc =
  HtmlDoc(Html
   (Head
     (Title(t"Soundness.dev"),
      Meta(charset = enc"UTF-8"),
      Link(rel = Rel.Stylesheet, href = % / p"styles.css"),
      Link(rel = Rel.Stylesheet, href = % / p"amok.css"),
      Link(rel = Rel.Icon, href = % / p"images" / p"logo2.svg")),
    Body
     (Nav(Ul
       (Li(A(href = %)(t"home")),
        Li(A(href = url"https://github.com/propensive/soundness")(t"contribute")),
        Li(A(href = url"https://discord.com/invite/MBUrkTgMnA")(t"discuss")))),
      Aside(aside),
      content,
      Footer(t"© Copyright 2024 Propensive"))))

val projects = Set
 (t"kaleidoscope", t"quantitative", t"dendrology", t"contingency", t"vacuous", t"iridescence",
  t"turbulence", t"symbolism", t"nettlesome", t"larceny", t"metamorphose", t"wisteria",
  t"capricious", t"dissonance", t"escritoire", t"gossamer", t"abacist", t"ethereal")

object Slogan:
  private val cache: scm.HashMap[Text, Text] = scm.HashMap()
  def apply(library: Text): Text raises HttpError = cache.establish(library):
    url"https://raw.githubusercontent.com/propensive/$library/main/doc/slogan.md".get().as[Text]

def home = page
 (Aside,
  H1
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
   (Div(projects.to(List).sorted.map: library =>
      Div.library
       (A(href = unsafely(% / Name(library)))(Img(src = url"https://raw.githubusercontent.com/propensive/$library/main/doc/logo.svg")),
        H3(library.capitalize),
        P(mend { case _: HttpError => t"" }.within(Slogan(library)))))))

@main
def server(): Unit = supervise(tcp"8080".serve[Http](handle))
class Service() extends JavaServlet(handle)

object Data:
  private val cache: scm.HashMap[HttpUrl, HtmlDoc] = scm.HashMap()
  lazy val repos: Map[Text, Repo] =
    tend:
      case _: HttpError => InitError(m"Could not access GitHub")
    .within:
      GitHub.repos(t"propensive").indexBy(_.name)

  def apply(project: Text): HtmlDoc raises HttpError raises MarkdownError =
    val intro = url"https://raw.githubusercontent.com/propensive/$project/main/doc/intro.md"
    val basics = url"https://raw.githubusercontent.com/propensive/$project/main/doc/basics.md"
    val status = url"https://raw.githubusercontent.com/propensive/$project/main/doc/status.md"
    val slogan = url"https://raw.githubusercontent.com/propensive/$project/main/doc/slogan.md"
    val loc = url"https://raw.githubusercontent.com/propensive/$project/main/doc/lines.md"

    cache.establish(basics):
      val repo = repos(project)
      val name = project.capitalize
      val modules = repo.modules()
      val tags = repo.tags().view.filter(MavenCentral.published(t"dev.soundness", t"$project-${modules.prim.or(t"core")}", _))
      val latest: Optional[Tag] = tags.take(1).to(List).prim

      val published = latest.lay(Nil): tag =>
        modules.flatMap: module =>
          val size = MavenCentral.binarySize(t"dev.soundness", t"$project-$module", tag).or(ByteSize(0))
          List
           (H3.module(t"$project-$module"),
            H4(t"Maven"),
            P(Span.mark(), Span.hover(t"pkg:maven/dev.soundness"), t"""/$project-$module@${tag.name}"""),
            H4(t"Gradle"),
            P(Span.mark(), Span.hover(t"dev.soundness"), t":$project-$module:${tag.name}"),
            H4(t"sbt"),
            P(Span.mark(), Span.hover(t"\"dev.soundness\""), t""" % "$project-$module" % "${tag.name}""""),
            H4(t"Fury"),
            P(t"include $project/$module"),
            H4(t"Binary size"),
            P(t"${size.long/1024}kiB"))

      val introBody = Markdown.parse(intro.get().as[Text])
      val body = Markdown.parse(basics.get().as[Text])
      val outline = htmlRenderers.outline.convert(body.nodes)
      val github = A(href = url"https://github.com/propensive/$project")(t"$name on GitHub")
      val discord = A(href = url"https://discord.gg/MBUrkTgMnA")(t"Discuss $name on Discord")
      val twitter = A(href = url"https://x.com/propensive")(t"Follow @propensive on X")

      val aside = Div
       (H3(t"Contents"),
        outline,
        H3(t"About ${project.capitalize}"),
        H4(t"Description"),
        P(slogan.get().as[Text]),
        H4(t"Size"),
        P(repo.size.show, t"kB repository", safely(t"; ${loc.get().as[Text]} lines of code").or(t"")),
        if repo.watchers > 10 then List(H4(t"Watchers"), P(repo.watchers.show)) else Nil,
        H4(t"Status"),
        P(status.get().as[Text]),
        H4(t"License"),
        P(A(href = url"https://www.apache.org/licenses/LICENSE-2.0")(t"Apache 2.0")),
        H4(t"Keywords"),
        P(repo.topics.flatMap { word => List(Span.topic(word.unkebab.join(t"\u00a0")), t" ") }),
        H4(t"Latest release"),
        latest.lay(List(P(t"not yet available"))) { tag => P(tag.name) },
        H3(t"Modules for Scala 3.5.0"),
        published,
        H3(t"Links"),
        Ul.links(Li.github(github), Li.discord(discord), Li.x(twitter)))

      val logo = Img.logo(src = url"https://raw.githubusercontent.com/propensive/$project/main/doc/logo.svg")
      erased given XmlParseError is Unchecked = ###
      page(aside, Article((H2(project.capitalize) +: logo +: (introBody.html ++ body.html))*))



def handle(using HttpRequest): HttpResponse[?] =
  mend:
    case MarkdownError(detail) =>
      HttpResponse(page(Div, Article(H2(t"Error"), P(t"The source contained bad markdown content."))))

    case XmlParseError(line, column) =>
      HttpResponse(page(Div, Article(H2(t"Error"), P(t"The source contained bad XML."))))

    case ClasspathError(path) =>
      HttpResponse(page(Div, Article(H2(t"Path Not Found"), P(t"The path was not found"))))

    case PathError(path, reason) =>
      HttpResponse(page(Div, Article(H2(t"Path Error"), P(t"$path is not valid because $reason"))))

    case error@HttpError(_, _) =>
      HttpResponse(page(Div, Article(H2(t"Error"), P(t"Could not download the remote page"))))

  .within:
     request.path match
      case %                 => HttpResponse(home)
      case % / p"styles.css" => HttpResponse(Classpath / p"styles.css")
      case % / p"amok.css"   => HttpResponse(Classpath / p"amok" / p"styles.css")
      case % / p"images" / Name(image) =>
        val resource = Classpath / p"images" / Name(image)

        if resource.exists() then HttpResponse(resource) else
          HttpResponse
           (NotFound(page(Div, Article(H2(t"Not Found"), P(t"The image was not found.")))))

      case % / p"sample" =>
        HttpResponse(page(Div, Article(Markdown.parse(Classpath / p"example.md").html)))

      case % / Name(project) if projects.contains(project) =>
        HttpResponse(Data(project))

      case _ =>
        HttpResponse(page(Div, Article(H2(t"Not Found"), P(t"This page does not exist."))))
