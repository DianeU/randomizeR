# setwd("C:/Users/duschner/randomizeR/")
# setwd("D:/David_lokal/Github/Rpackage/randomizeR")
# setwd("U:/IDeAl/IDeAlGrit/randomizeR")
# setwd("C:/Users/mmanolov/Documents/GitHub/rmbct/randomizeR")

library(knitr)
library(devtools)
library(testthat)

# For new Packages use this
# create("randomizeR")


current.code <- as.package("randomizeR")
# devtools::use_vignette("randomizeR")
# in case something was deleted or renamed, run (twice)
# load_all(current.code, recompile = TRUE)
document(current.code)
load_all(current.code)
#test(current.code)
#load_all(current.code)
#run_examples(current.code)
#build_vignettes(current.code)
#use_build_ignore("NEWS.md")
check(current.code)

build(current.code)

# generate manual
# if (file.exists("./randomizeR.pdf")) file.remove("./randomizeR.pdf")
# system(paste('R CMD Rd2pdf ',  'randomizeR'))


install.packages("randomizeR_1.1.9000.tar.gz", repos = NULL, type = "source")


# require(randomizeR)
# vignette(package = "randomizeR")
# vignette()
