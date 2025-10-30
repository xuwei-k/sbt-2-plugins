conflictWarning := {
  if (scalaBinaryVersion.value == "3") {
    ConflictWarning("warn", Level.Warn, false)
  } else {
    conflictWarning.value
  }
}
