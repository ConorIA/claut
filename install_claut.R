if("git2r" %in% rownames(installed.packages()) == FALSE) {
  install.packages("git2r", repos = "https://cloud.r-project.org/")
}
if("remotes" %in% rownames(installed.packages()) == TRUE) {
  remotes::install_github("r-pkgs/remotes")
} else {
  source("https://raw.githubusercontent.com/r-pkgs/remotes/master/install-github.R")$value("r-pkgs/remotes")
}
remotes::install_git("https://gitlab.com/ConorIA/claut.git")
