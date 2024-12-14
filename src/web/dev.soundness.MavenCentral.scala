package dev.soundness

import soundness.*
import scintillate.*

import environments.virtualMachine
import dynamicJsonAccess.enabled
import strategies.throwUnsafely
import errorDiagnostics.stackTraces

object MavenCentral:
  val base = url"https://central.sonatype.com/api/v1/publisher/published"

  val token: Text =
    tend:
      case EnvironmentError(variable) => InitError(m"Could not read the variable $variable")

    . within(Environment.mavenToken[Text])

  val headers: List[RequestHeader.Value] = List
   (RequestHeader.Authorization(Auth.Bearer(token)),
    RequestHeader.Accept(media"application/json"))

  def published(namespace: Text, name: Text, version: Tag): Boolean raises HttpError =
    val url = unsafely(Url.parse(t"$base?namespace=$namespace&name=$name&version=${version.name}"))
    url.get(headers*).as[Json].published.as[Boolean]

  def binarySize(namespace: Text, name: Text, tag: Tag): Optional[Memory] raises HttpError =
    val group = namespace.sub(t".", t"/")
    val version = tag.name
    val file = t"$name-$version.jar"
    val url = url"https://repo1.maven.org/maven2/$group/$name/$version/$file"
    mend:
      case NumberError(_, _) => Memory(0)

    . within(url.head()(ResponseHeader.ContentLength).prim)
