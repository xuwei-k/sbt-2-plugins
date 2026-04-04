conflictWarning := {
  if (scalaBinaryVersion.value == "3") {
    // https://github.com/scalameta/sbt-scalafmt/issues/433
    ConflictWarning("warn", Level.Warn, false)
  } else {
    conflictWarning.value
  }
}
