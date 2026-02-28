conflictWarning := {
  if (scalaBinaryVersion.value == "3") {
    ConflictWarning("warn", Level.Warn, false)
  } else {
    conflictWarning.value
  }
}

Global / onLoad += { (s1: State) =>
  val (s2, values) =
    Project.extract(s1).runTask(Compile / externalDependencyClasspath, s1)
  values
    .flatMap(_.get(Keys.moduleIDStr))
    .map(Classpaths.moduleIdJsonKeyFormat.read)
    .filter(x =>
      x.organization == "org.scala-lang" && x.name == "scala-library"
    )
    .foreach(x =>
      assert(VersionNumber(x.revision) == VersionNumber("3.8.1"), x)
    )
  s2
}
