read_xlogfile <- function(file,
                          version = NULL,
                          expand.abbrevs = TRUE,
                          verbose = TRUE,
                          sep = "\t",
                          iconv.sub = NULL) {

    txt <- readLines(file)
    if (!is.null(iconv.sub)) {
        txt <- iconv(txt, sub = iconv.sub)
    }

    spl <- strsplit(txt, sep, fixed = TRUE)

    ## http://bernoulli.atspace.com/nethack/patches/xlog-v3.patch
    ## L26 <- lengths(spl) == 26L
    ## x1 <- lapply(spl[L26], strsplit, "=", fixed = TRUE)
    ## x1 <- lapply(x1, `[[`, 2)
    ## dim(x1) <- c(26*2, sum(L26))

    ## x2 <- unlist(lapply(spl[!L26], strsplit, "=", fixed = TRUE))
    ## dim(x2) <- c(27*2, sum(!L26))


    field <- function(s, field, sep)
        gsub(paste0(".*", field, "=([^", sep, "]+).*"), "\\1", s)

    version       <- field(txt, "version", sep)
    score         <- as.numeric(field(txt, "points", sep))
    dun.num       <- as.numeric(field(txt, "deathdnum", sep))
    dun.level     <- as.numeric(field(txt, "deathlev", sep))
    dun.level.max <- as.numeric(field(txt, "maxlvl", sep))
    hp            <- as.numeric(field(txt, "hp", sep))
    hp.max        <- as.numeric(field(txt, "maxhp", sep))
    ndeaths       <- as.numeric(field(txt, "deaths", sep))
    end.date      <- as.Date(field(txt, "deathdate", sep), format = "%Y%m%d")
    start.date    <- as.Date(field(txt, "birthdate", sep), format = "%Y%m%d")
    uid           <- as.numeric(field(txt, "uid", sep))
    role          <- field(txt, "role", sep)
    race          <- field(txt, "race", sep)
    gender        <- field(txt, "gender", sep)
    alignment     <- field(txt, "align", sep)
    name          <- field(txt, "name", sep)
    cause         <- field(txt, "death", sep)
    while_ <- character(length(txt))
    w_ <- grepl("while=", txt)
    while_[w_] <- field(txt[w_], "while", sep)
    conduct <- field(txt, "conduct", sep)
    turns <- field(txt, "turns", sep)
    achieve <- field(txt, "achieve", sep)
    realtime <- field(txt, "realtime", sep)
    starttime <- field(txt, "starttime", sep)
    endtime <- field(txt, "endtime", sep)
    gender0 <- field(txt, "gender0", sep)
    align0 <- field(txt, "align0", sep)
    flags <- field(txt, "flags", sep)

    if (expand.abbrevs) {


        role[role == "Arc"] <- "archeologist"
        role[role == "Bar"] <- "barbarian"
        role[role == "Cav"] <- "caveman"
        role[role == "Hea"] <- "healer"
        role[role == "Kni"] <- "knight"
        role[role == "Mon"] <- "monk"
        role[role == "Pri"] <- "priest"
        role[role == "Ran"] <- "ranger"
        role[role == "Rog"] <- "rogue"
        role[role == "Sam"] <- "samurai"
        role[role == "Tou"] <- "tourist"
        role[role == "Val"] <- "valkyrie"
        role[role == "Wiz"] <- "wizard"
        ##
        gender[gender == "Mal"] <- "male"
        gender[gender == "Fem"] <- "female"
        gender0[gender0 == "Mal"] <- "male"
        gender0[gender0 == "Fem"] <- "female"
        ##
        alignment[alignment == "Neu"] <- "neutral"
        alignment[alignment == "Cha"] <- "chaotic"
        alignment[alignment == "Law"] <- "lawful"
        ##
        race[race == "Dwa"] <- "dwarf"
        race[race == "Elf"] <- "elf"
        race[race == "Gno"] <- "gnome"
        race[race == "Hum"] <- "human"
        race[race == "Orc"] <- "orc"
    }

    data.frame(version       =  version,
               name          =  name,
               score         =  score,
               dun.num       =  dun.num,
               dun.level     =  dun.level,
               dun.level.max =  dun.level.max,
               hp            =  hp,
               hp.max        =  hp.max,
               ndeaths       =  ndeaths,
               death.date    =  end.date,
               birth.date    =  start.date,
               uid           =  uid,
               role          =  role,
               race          =  race,
               gender        =  gender,
               alignment     =  alignment,
               cause         =  cause,
               `while`       =  while_,
               stringsAsFactors = FALSE,
               check.names = FALSE)

}

read_logfile <- function(file, version = NULL, expand.abbrevs = TRUE,
                         verbose = TRUE) {

    field <- function(x, n)
        unlist(lapply(x, `[[`, n))

    txt <- readLines(file)

    ## version number present? [version number since 3.2.0]
    ver <- grep("^[0-9]+[.][0-9]+[.][0-9]+ .*", txt[1L], perl = TRUE)
    ver <- if (length(ver))
               gsub("^([0-9]+[.][0-9]+[.][0-9]+).*", "\\1", txt)
           else
               NA

    if (!is.null(version)) {
        version <- package_version(version)
        if (is.na(ver[1L]) && verbose) {
            message("no version information in logfile")
            txt <- character(0)
        } else {
            ver <- package_version(ver)
            txt <- txt[version == ver]
        }
    }
    if (!length(txt))
        return(.no.record)

    if (is.null(version) ||
        version >= package_version("3.4.3")) {

        comma <- regexpr(",", txt, fixed = TRUE)
        txt0 <- substr(txt, 1, comma - 1L)
        tmp <- strsplit(txt0, " ", fixed = TRUE)

        M <- do.call(rbind, lapply(tmp, "[", 1:16))

        space.names.i <- lengths(tmp) > 16L
        if (any(space.names.i)) {
            space.names <- lapply(tmp[space.names.i],
                                  function(x) x[-seq_len(15)])
            space.names <-
                unlist(lapply(space.names, paste, collapse = " "))

            M[space.names.i, 16] <- space.names
        }

        M <- as.data.frame(M)

        cause <- substr(txt, comma + 1L, nchar(txt))
        wh <- character(length(cause))
        w <- grep(", while", cause)
        if (any(w)) {
            wh[w] <- sub(".*, while (.*)", "\\1", cause[w], perl = TRUE)
            cause[w] <- sub(", while.*", "", cause[w], perl = TRUE)
        }
        M <- cbind(M, cause, wh)

        colnames(M) <- c("version", "score", "dun.num",
                         "dun.level", "dun.level.max",
                         "hp", "hp.max", "ndeaths",
                         "death.date", "birth.date",
                         "uid", "role", "race",
                         "gender", "alignment", "name",
                         "cause", "while")

        num.fields <- c("score",
                        "dun.num",
                        "dun.level",
                        "dun.level.max",
                        "hp",
                        "hp.max",
                        "ndeaths",
                        "uid")

        date.fields <- c("death.date",
                         "birth.date")

        for (f in num.fields) {
            M[[f]] <- as.numeric(M[[f]])
        }

        for (f in date.fields) {
            M[[f]] <- as.Date(M[[f]], format = "%Y%m%d")
        }

        if (expand.abbrevs) {

            D <- c("Arc" = "archeologist",
                   "Bar" = "barbarian",
                   "Cav" = "caveman",
                   "Hea" = "healer",
                   "Kni" = "knight",
                   "Mon" = "monk",
                   "Pri" = "priest",
                   "Ran" = "ranger",
                   "Rog" = "rogue",
                   "Sam" = "samurai",
                   "Tou" = "tourist",
                   "Val" = "valkyrie",
                   "Wiz" = "wizard")
            M[["role"]] <- D[M[["role"]]]

            D <- c("Mal" = "male",
                   "Fem" = "female")
            M[["gender"]] <- D[M[["gender"]]]

            D <- c("Neu" = "neutral",
                   "Cha" = "chaotic",
                   "Law" = "lawful")
            M[["alignment"]] <- D[M[["alignment"]]]

            D <- c("Dwa" = "dwarf",
                   "Elf" = "elf",
                   "Gno" = "gnome",
                   "Hum" = "human",
                   "Orc" = "orc")
            M[["race"]] <- D[M[["race"]]]

        }

    } else if (version == package_version("3.1.0")) {

        comma <- regexpr(",", txt, fixed = TRUE)
        txt0 <- substr(txt, 1, comma - 1L)
        tmp <- strsplit(txt0, " ")

        ## version       <- version
        end.date      <- as.Date(field(tmp, 1), format = "%y%m%d")
        start.date    <- NA
        uid           <- as.numeric(field(tmp, 2))
        dun.num       <- as.numeric(field(tmp, 3))
        dun.level     <- as.numeric(field(tmp, 4))
        dun.level.max <- as.numeric(field(tmp, 5))
        hp            <- as.numeric(field(tmp, 6))
        hp.max        <- as.numeric(field(tmp, 7))
        score         <- as.numeric(field(tmp, 8))
        role          <- substr(field(tmp, 9),1,1)
        gender        <- substr(field(tmp, 9),2,2)
        name          <- field(tmp, 10)
        ndeaths       <- NA
        race          <- NA
        alignment     <- NA
        cause         <- substr(txt, comma + 1L, nchar(txt))
    }


    M
}


.no.record <- data.frame(version       =  character(0),
                         name          =  character(0),
                         score         =  numeric(0),
                         dun.num       =  numeric(0),
                         dun.level     =  numeric(0),
                         dun.level.max =  numeric(0),
                         hp            =  numeric(0),
                         hp.max        =  numeric(0),
                         ndeaths       =  numeric(0),
                         death.date    =  numeric(0),
                         birth.date    =  numeric(0),
                         uid           =  numeric(0),
                         role          =  character(0),
                         race          =  character(0),
                         gender        =  character(0),
                         alignment     =  character(0),
                         cause         =  character(0),
                         "while"       =  character(0),
                         stringsAsFactors = FALSE,
                         check.names = FALSE)
