read_xlogfile <- function(file,
                          version = NULL,
                          expand.abbrevs = TRUE,
                          verbose = TRUE,
                          sep = "\t") {

    txt <- readLines(file)

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
    ver <- grep("^[0-9]+[.][0-9]+[.][0-9]+.*", txt[1L], perl = TRUE)
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
        tmp <- strsplit(txt0, " ")

        version       <- field(tmp, 1)
        score         <- as.numeric(field(tmp, 2))
        dun.num       <- as.numeric(field(tmp, 3))
        dun.level     <- as.numeric(field(tmp, 4))
        dun.level.max <- as.numeric(field(tmp, 5))
        hp            <- as.numeric(field(tmp, 6))
        hp.max        <- as.numeric(field(tmp, 7))
        ndeaths       <- as.numeric(field(tmp, 8))
        end.date      <- as.Date(field(tmp, 9), format = "%Y%m%d")
        start.date    <- as.Date(field(tmp, 10), format = "%Y%m%d")
        uid           <- as.numeric(field(tmp, 11))
        role          <- field(tmp, 12)
        race          <- field(tmp, 13)
        gender        <- field(tmp, 14)
        alignment     <- field(tmp, 15)
        name          <- field(tmp, 16)
        cause         <- substr(txt, comma + 1L, nchar(txt))


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



    df0 <- data.frame(version       =  as.character(version),
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
                      `while`       =  NA,
                      stringsAsFactors = FALSE,
                      check.names = FALSE)

    df0
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
                         `while`       =  character(0),
                         stringsAsFactors = FALSE,
                         check.names = FALSE)
