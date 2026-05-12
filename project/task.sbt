import scala.collection.concurrent.TrieMap
import lmcoursier.internal.shaded.coursier
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse.BodyHandlers
import java.util.Locale
import java.nio.file.Files
import java.nio.file.StandardOpenOption
import play.api.libs.json.Json
import play.api.libs.json.JsObject
import scala.util.Using

InputKey[Unit]("pluginUrlList") := {
  val result = Using
    .resource(HttpClient.newHttpClient()) { httpClient =>
      libraryDependencies.value.filter { x =>
        PartialFunction.cond(x.crossVersion) { case c: CrossVersion.Binary =>
          c.prefix == "sbt2_"
        }
      }.flatMap { x =>
        val dependency = coursier.Dependency(
          coursier.Module(
            coursier.Organization(x.organization),
            coursier.ModuleName(s"${x.name}_sbt2_3")
          ),
          coursier.version.VersionConstraint(x.revision)
        )

        try {
          coursier
            .Fetch()
            .addDependencies(dependency)
            .withClassifiers(Set(lmcoursier.internal.shaded.coursier.core.Classifier.sources))
            .runResult()
            .detailedArtifacts0
            .find { a =>
              val r = a._1
              (r.module.organization.value == x.organization) && (r.module.name.value == dependency.module.name.value)
            }
            .toSeq
            .flatMap { a =>
              a._4.getAbsolutePath +:
                IO.withTemporaryDirectory { dir =>
                  IO.unzip(a._4, dir)
                  (dir ** "*.scala").get().flatMap { scalaFile =>
                    import scala.meta._
                    val src = Input.File(scalaFile)
                    val p = implicitly[parsers.Parse[Source]]
                    val tree = p.apply(src, dialects.Scala3).get
                    tree.collect {
                      case t @ Defn.Val(
                            _,
                            List(
                              Pat.Var(_: Term.Name)
                            ),
                            None,
                            Term.Apply.After_4_6_0(
                              Term.ApplyType.After_4_6_0(
                                Term.Name("taskKey"),
                                Type.ArgClause(
                                  List(
                                    tpe
                                  )
                                )
                              ),
                              Term.ArgClause(
                                _,
                                None
                              )
                            )
                          ) if tpe.toString.contains("File") && t.mods.find(_.is[Mod.Annot]).isEmpty =>
                        t.toString
                    }
                  }
                }
            }
        } catch {
          case e =>
            Nil
        }
      }
    }
    .mkString("```\n", "\n", "\n```\n")

  sys.env.get("GITHUB_STEP_SUMMARY").map(new File(_)).filter(_.isFile) match {
    case Some(summaryFile) =>
      Files.writeString(summaryFile.toPath, result, StandardOpenOption.APPEND)
    case _ =>
      println(result)
  }
}

lazy val githubTokenOpt: Option[String] = sys.env.get("GITHUB_TOKEN")

val githubStarCache: TrieMap[String, Int] = TrieMap.empty[String, Int]

val invalidGitHubRepos: Set[(String, String)] = Set(
  ("shuwarifrica", "version"), // https://github.com/cheleb/sbt-plantuml/pull/169
  ("cheleb", "plantuml-sbt-plugin"), // https://github.com/shuwariafrica/version/pull/94
  ("olafurpg", "sbt-ci-release"), // https://github.com/sbt/sbt-ci-release/pull/412
)

def pomToString(f: (String, File), x: ModuleID, httpClient: HttpClient): String = {
  val pomUrl = f._1
  val pom = scala.xml.XML.loadFile(f._2)
  val url = (pom \ "url").text.trim match {
    case s"${prefix}/" => prefix
    case s"http://${suffix}" => s"https://${suffix}"
    case other => other
  }
  val scmUrl = (pom \ "scm" \ "url").text.trim match {
    case s"git@github.com:${x1}/${x2}.git" =>
      s"https://github.com/${x1}/${x2}"
    case s"git://github.com:${x1}/${x2}.git" =>
      s"https://github.com/${x1}/${x2}"
    case s"git://github.com/${x1}/${x2}.git" =>
      s"https://github.com/${x1}/${x2}"
    case s"${prefix}/" =>
      prefix
    case other =>
      other
  }
  val description = (pom \ "description").text.trim
  val header = s"""### `addSbtPlugin("${x.organization}" % "${x.name}" % "${x.revision}")`"""

  val starOption: Option[Int] = githubTokenOpt.flatMap(token =>
    Seq(url, scmUrl).iterator.collect {
      case s"https://github.com/${x1}/${x2}" if !invalidGitHubRepos((x1, x2)) =>
        getStar(
          s"https://api.github.com/repos/${x1}/${x2}",
          token,
          httpClient
        )
    }.flatten.nextOption
  )

  Seq(
    starOption.map("GitHub Star: " + _),
    Option(url),
    Option(pomUrl),
    Option.when(url.toLowerCase(Locale.ROOT) != scmUrl.toLowerCase(Locale.ROOT))(scmUrl),
    Option.when((description != x.name) && (description != "plugin"))(description)
  ).flatten.map("- " + _).mkString(header + "\n", "\n", "\n")
}

def getStar(uri: String, token: String, httpClient: HttpClient): Option[Int] = {
  try {
    Option(
      githubStarCache.getOrElseUpdate(
        uri, {
          val request = HttpRequest
            .newBuilder()
            .uri(
              new URI(uri)
            )
            .header(
              "authorization",
              s"Bearer ${token}"
            )
            .header(
              "content-type",
              "application/json"
            )
            .build()
          val response = httpClient.send(request, BodyHandlers.ofString()).body()

          Json.parse(response).as[JsObject].value.apply("stargazers_count").as[Int]
        }
      )
    )
  } catch {
    case e =>
      println((uri, e))
      None
  }
}
