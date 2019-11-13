# setwd("C:/Users/duschner/randomizeR/")
# setwd("D:/David_lokal/Github/Rpackage/randomizeR")
# setwd("U:/IDeAl/IDeAlGrit")
# setwd("C:/Users/mmanolov/Documents/GitHub/randomizeR")
# setwd("C:/Users/kfuge/Documents/GitHub/randomizeR")
# setwd("C:/Users/Marcia/Documents/randomizeR-Logrank")
<<<<<<< HEAD
# setwd("C:\\Users\\49171\\Documents\\randomizeR")
setwd ('C:\\Users\\mmartini\\Desktop\\randomizeR')
=======
setwd("C:\\Users\\49171\\Documents\\randomizeR")

>>>>>>> 00d3bf337b0a3842efc59026257fb48b1bbc23a2

library(knitr)
library(devtools)
library(testthat)
# devtools::session_info() ## Check package information

# For new Packages use this
#create("randomizeR")

### Current work around
#devtools::install_github("gustavdelius/roxygen") 

current.code <- as.package("randomizeR")
#devtools::use_vignette("randomizeR")
# in case something was deleted or renamed, run (twice)
<<<<<<< HEAD
load_all(current.code, recompile = TRUE)
document(current.code)
load_all(current.code) 
test(current.code)
=======
#load_all(current.code, recompile = TRUE)
document(current.code)
load_all(current.code) 
#test(current.code)
>>>>>>> 00d3bf337b0a3842efc59026257fb48b1bbc23a2
load_all(current.code)
run_examples(current.code)
build_vignettes(current.code)
check(current.code)
#build(current.code, path = 'C:\\Users\\mmartini\\Desktop\\randomizeR')
### devtools::release(pkg=current.code) ### Publish package on CRAN

# generate manual
if (file.exists("./randomizeR.pdf")) file.remove("./randomizeR.pdf")
   system(paste('R CMD Rd2pdf ',  'randomizeR'))


#install.packages("randomizeR_2.0.tar.gz", repos = NULL, type = "source")

# library(randomizeR)
# vignette(package = "randomizeR")
# vignette(topic = 'article', package = "randomizeR")
# vignette()
