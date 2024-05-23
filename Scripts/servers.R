library("NethackLogfiles")

txt <- readLines("https://eu.hardfought.org/xlogfiles/nethack36/xlogfile")

log <- read_xlogfile("https://eu.hardfought.org/xlogfiles/nethack36/xlogfile",
                     iconv.sub = "_")




xx <- iconv(txt[[967]], from = "cp1252", sub = "_")
strsplit(xx, "\t")

## x <- "fa\xE7ile"
xx <- iconv(lf, "latin1", "UTF-8")
xx <- iconv(xx, "UTF-8", "ASCII", "c99")
