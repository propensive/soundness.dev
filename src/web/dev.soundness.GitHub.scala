package dev.soundness

import soundness.*
import jacinta.*
import merino.*
import telekinesis.{HttpRequest as _, HttpResponse as _, *}

import environments.virtualMachine
import dynamicJsonAccess.enabled

object GitHub:
  val base = url"https://api.github.com"

  val token: Text =
    tend:
      case EnvironmentError(variable) =>
        InitError(m"Could not read the environment variable $variable")
    .within(Environment.githubToken[Text])

  val headers: List[RequestHeader.Value] = List
   (RequestHeader.Authorization(Auth.Bearer(token)),
    RequestHeader.Accept(media"application/vnd.github+json"),
    SimpleRequestHeader["X-GitHub-Api-Version"]()(t"2022-11-28"))

  def repos(login: Text): List[Repo] raises HttpError =
    def recur(page: Int = 1, all: List[List[Repo]] = Nil): List[List[Repo]] =
      val url = unsafely(Url.parse(t"https://api.github.com/users/$login/repos?per_page=100&page=$page"))
      val repos = url.get(headers*).as[Json].as[List[Repo]]
      if repos.isEmpty then all else recur(page + 1, repos :: all)

    recur().flatten

case class Repo
   (id:          Int,
    name:        Text,
    owner:       Owner,
    fork:        Boolean,
    topics:      List[Text],
    open_issues: Int,
    watchers:    Int,
    forks:       Int,
    size:        Int):

  def tags(): List[Tag] raises HttpError raises JsonError =
    val url = url"https://api.github.com/repos/${owner.login}/$name/tags?per_page=100"
    url.get(GitHub.headers*).as[Json].as[List[Tag]]

  def modules(): List[Text] raises HttpError raises JsonError =
    val url = url"https://api.github.com/repos/${owner.login}/$name/contents/etc"
    url.get(GitHub.headers*).as[Json].as[List[Json]].map(_.name.as[Text])

  def raw: HttpUrl = url"https://raw.githubusercontent.com/${owner.login}/$name"

case class Commit(sha: Text)
case class Tag(name: Text, commit: Commit)

case class Owner(login: Text, id: Int):
  def repos(): List[Repo] raises HttpError =
    url"https://api.github.com/users/$login/repos".get(GitHub.headers*).as[Json].as[List[Repo]]
