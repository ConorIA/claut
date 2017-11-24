cat("##### PSA ##### \n
You are about to run remote code! It is always a good idea to review remote code \
before running, as a malicious actor could use a script like this to do naughty \
things on your computer.\n \
In this case, we are going to install the claut package, and (if necessary) a \
couple of packages that we need to install it.\n\n")

proceed <- readline(prompt = "Would you like to proceed? (y/N)")

if (proceed == "Y" || proceed == "y") {
  helper <- c("devtools", "remotes") %in% rownames(installed.packages())
  if (sum(helper) == 0) {
    proceed <- readline(prompt = "We need devtools to install this package! Proceed? (y/N)")
    if (proceed == "Y" || proceed == "y") {
      install.packages("devtools", repos == "https://cloud.r-project.org/")
    } else {
      stop("Ok. Please install manually.")
    }
  } else if (sum(helper) >= 1){
    if (helper[1]) {
      devtools::install_git("https://gitlab.com/ConorIA/claut.git")
    } else if (helper[2]) {
      remotes::install_git("https://gitlab.com/ConorIA/claut.git")
    }
  }
}
