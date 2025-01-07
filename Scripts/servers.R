library("NethackLogfiles")

file <- "https://eu.hardfought.org/xlogfiles/nethack36/xlogfile"
txt <- readLines(file)
log <- read_xlogfile(file, iconv.sub = "_")


## xx <- iconv(txt[[967]], from = "cp1252", sub = "_")
## strsplit(xx, "\t")
