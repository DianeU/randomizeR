# setwd("C:/Users/duschner/randomizeR/")
# setwd("D:/David_lokal/Github/Rpackage/randomizeR")
# setwd("U:/IDeAl/IDeAlGrit")
# setwd("C:/Users/mmanolov/Documents/GitHub/randomizeR")
# setwd("C:/Users/kfuge/Documents/GitHub/randomizeR")
# setwd("C:/Users/Marcia/Documents/randomizeR-Logrank")

# setwd("C:\\Users\\49171\\Documents\\randomizeR")
setwd("C:\\Users\\mmartini\\Desktop\\randomizeR")


library(knitr)
library(devtools)
library(testthat)
library(spelling)

# devtools::session_info() ## Check package information

# For new Packages use this
#create("randomizeR")

current.code <- as.package("randomizeR")
#devtools::use_vignette("randomizeR")

# in case something was deleted or renamed, run (twice)
load_all(current.code, recompile = TRUE)
document(current.code)
load_all(current.code) 
test(current.code)
## Fix notes
# Sys.setenv(R_GSCMD = "C:\\Program Files (x86)\\gs\\gs9.07\\bin\\gswin32c.exe") ## Set ghostscript path

# in case something was deleted or renamed, run (twice)
#load_all(current.code, recompile = TRUE)
document(current.code)
load_all(current.code) 
#test(current.code)
## Runs a Spell checker on all Files including the Vignettes, Ignores words saved in the Wordlist, run document beforehand to catch changes
#print(spell_check(current.code))
## Update the wordlist through invokation of a spell_checker call
#update_wordlist(current.code)
load_all(current.code) 
test(current.code)
load_all(current.code)
run_examples(current.code)
build_vignettes(current.code)
check(current.code)

## Release of new package 
#check(current.code, run_dont_test=TRUE, manual=TRUE, remote=TRUE, incoming=TRUE, force_suggests=TRUE) ## more detailed checks
#build(current.code) ## Funktioniert aktuell nicht
#build(pkg="C:\\Users\\mmartini\\Desktop\\randomizeR\\randomizeR")
#devtools::release_checks(current.code)
#validate_email(email = "Diane.Uschner@gmail.com", token = "cce2a98051a3445c8518223f6a00110b")
#check_rhub(current.code,email="mmartini@ukaachen.de") # check on R-Hub
#check_win_devel(current.code, email="mmartini@ukaachen.de")# Check win-builder
#setwd("C:\\Users\\mmartini\\Desktop\\randomizeR\\randomizeR")
#devtools::release(pkg = current.code) ### Publish package on CRAN

# Switch back to WD if previously switched
#setwd("C:\\Users\\mmartini\\Desktop\\randomizeR")

# generate manual

if (file.exists("./randomizeR.pdf")) file.remove("./randomizeR.pdf")
     system(paste('R CMD Rd2pdf ',  'randomizeR'))



#install.packages("randomizeR_2.0.tar.gz", repos = NULL, type = "source")

# library(randomizeR)
# vignette(package = "randomizeR")
# vignette(topic = 'article', package = "randomizeR")
# vignette()
