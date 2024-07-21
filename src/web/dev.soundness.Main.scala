package dev.soundness

import scala.collection.mutable as scm

import soundness.*
import honeycomb.*
import jacinta.*
import merino.*
import punctuation.*
import scintillate.*
import telekinesis.{HttpRequest as _, HttpResponse as _, *}

import logFormats.ansiStandard
import classloaders.scala
import charEncoders.utf8
import orphanDisposal.cancel
import threadModels.platform
import pathHierarchies.simple
import stdioSources.virtualMachine.ansi
import htmlRenderers.scalaSyntax

given Realm = realm"soundness"
given Message is Loggable = safely(supervise(Log.route(Out))).or(Log.silent)
given Online = Online
erased given ConcurrencyError is Unchecked = ###
erased given JsonParseError is Unchecked = ###
erased given JsonError is Unchecked = ###

given InitError is Fatal = error => ExitStatus.Fail(1)

case class InitError(msg: Message) extends Error(msg)

def page(aside: Html[Flow], content: Html[Flow]*): HtmlDoc =
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
      Aside(aside),
      content,
      Footer(t"© Copyright 2024 Propensive"))))

val projects = Set
 (t"kaleidoscope", t"quantitative", t"dendrology", t"contingency", t"vacuous", t"iridescence",
  t"turbulence", t"symbolism", t"nettlesome", t"larceny", t"metamorphose", t"wisteria",
  t"capricious", t"dissonance")

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
            P(t"dev.soundness:$project-$module:${tag.name}"),
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
        H4(t"Size"),
        P(repo.size.show, t"kB", safely(t", ${loc.get().as[Text]} lines of Scala").or(t"")),
        if repo.watchers > 10 then List(H4(t"Watchers"), P(repo.watchers.show)) else Nil,
        H4(t"Status"),
        P(status.get().as[Text]),
        H4(t"License"),
        P(A(href = url"https://www.apache.org/licenses/LICENSE-2.0")(t"Apache 2.0")),
        H4(t"Keywords"),
        P(repo.topics.flatMap { word => List(Span.topic(word.unkebab.join(t"\u00a0")), t" ") }),
        H4(t"Latest release"),
        latest.lay(List(P(t"not yet available"))) { tag => P(tag.name) },
        H3(t"Modules"),
        published,
        H3(t"Links"),
        Ul.links(Li.github(github), Li.discord(discord), Li.x(twitter)))

      val logo = Img.logo(src = url"https://raw.githubusercontent.com/propensive/$project/main/doc/logo.svg")
      page(aside, Article((H2(project.capitalize) +: logo +: (introBody.html ++ body.html))*))

def handle(using HttpRequest): HttpResponse[?] =

  mend:
    case MarkdownError(detail) =>
      HttpResponse(page(Div, Article(H2(t"Error"), P(t"The source contained bad markdown content."))))

    case ClasspathError(path) =>
      HttpResponse(page(Div, Article(H2(t"Path Not Found"), P(t"The path was not found"))))

    case PathError(path, reason) =>
      HttpResponse(page(Div, Article(H2(t"Path Error"), P(t"$path is not valid because $reason"))))

    case error@HttpError(_, _) =>
      HttpResponse(page(Div, Article(H2(t"Error"), P(t"Could not download the remote page"))))

  .within:
    request.path match
      case % =>
        HttpResponse(home)

      case % / p"styles.css" =>
        HttpResponse(Classpath / p"styles.css")

      case % / p"images" / Name(image) =>
        val resource = Classpath / p"images" / Name(image)

        if resource.exists() then HttpResponse(resource)
        else HttpResponse(NotFound(page(Div, Article(H2(t"Not Found"), P(t"The image was not found.")))))

      case % / Name(project) if projects.contains(project) =>
        HttpResponse(Data(project))

      case _ =>
        HttpResponse(page(Div, Article(H2(t"Not Found"), P(t"This page does not exist."))))

case class Commit(sha: Text)
case class Tag(name: Text, commit: Commit)

case class Owner(login: Text, id: Int):
  def repos(): List[Repo] raises HttpError =
    url"https://api.github.com/users/$login/repos".get(GitHub.headers*).as[Json].as[List[Repo]]
