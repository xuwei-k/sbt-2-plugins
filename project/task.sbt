import java.nio.file.Files
import java.nio.file.StandardOpenOption
import lmcoursier.internal.shaded.coursier

InputKey[Unit]("pluginUrlList") := {
  val result = libraryDependencies.value.filter { x =>
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

    val resOption = try {
      coursier
        .Fetch()
        .addDependencies(dependency)
        .runResult()
        .detailedArtifacts0
        .find { a =>
          val r = a._1
          (r.module.organization.value == x.organization) && (r.module.name.value == dependency.module.name.value)
        }
        .map(_._4.getAbsolutePath.dropRight(".jar".length) + ".pom")
        .map(new File(_))
    } catch {
      case e =>
        None
    }

    if (resOption.isEmpty) {
      println("error " + x)
    }

    resOption.map(pomToString(_, x))
  }.mkString("\n")

  sys.env.get("GITHUB_STEP_SUMMARY").map(new File(_)).filter(_.isFile) match {
    case Some(summaryFile) =>
      Files.writeString(summaryFile.toPath, result, StandardOpenOption.APPEND)
    case _ =>
      println(result)
  }
}

def pomToString(f: File, x: ModuleID): String = {
  val pom = scala.xml.XML.loadFile(f)
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

  Seq(
    Option(url),
    Option.when(url != scmUrl)(scmUrl),
    Option.when((description != x.name) && (description != "plugin"))(description)
  ).flatten.map("- " + _).mkString(header + "\n", "\n", "\n")
}
