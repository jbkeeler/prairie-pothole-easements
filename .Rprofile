if (interactive()) {
  suppressMessages(source("code/constants.R"))
  suppressMessages(source("code/utils.R"))
}

# fix vscode R extension bug
if (interactive() && Sys.getenv("RSTUDIO") == "") {
  init_path <- file.path(
    Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"),
    ".vscode-R",
    "init.R"
  )
  source(init_path)
  .First.sys()
}
