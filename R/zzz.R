.onLoad <- function(libname, pkgname) {
  # Always register our as.party.C5.0 method
  # lorax and C50 provides methods, we will use the one here
  registerS3method(
    "as.party",
    "C5.0",
    as.party.C5.0,
    envir = asNamespace(pkgname)
  )

  # Set up hooks to re-register our method after C50 loads/attaches
  # This ensures lorax's implementation takes precedence even if C50 loads after lorax
  setHook(
    packageEvent("C50", "onLoad"),
    function(...) {
      lorax_ns <- asNamespace("lorax")
      registerS3method(
        "as.party",
        "C5.0",
        get("as.party.C5.0", envir = lorax_ns),
        envir = lorax_ns
      )
    },
    action = "append"
  )

  setHook(
    packageEvent("C50", "attach"),
    function(...) {
      lorax_ns <- asNamespace("lorax")
      registerS3method(
        "as.party",
        "C5.0",
        get("as.party.C5.0", envir = lorax_ns),
        envir = lorax_ns
      )
    },
    action = "append"
  )
}

.onAttach <- function(libname, pkgname) {
  # Re-register when package is attached
  registerS3method(
    "as.party",
    "C5.0",
    as.party.C5.0,
    envir = asNamespace(pkgname)
  )
}
