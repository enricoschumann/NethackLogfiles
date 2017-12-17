## file <- "/var/games/nethack/logfile"

## file <- "~/Downloads/Nethack/games/lib/nethackdir/xlogfile"
read_xlogfile <- function(file, version = "auto", expand.abbrevs = TRUE) {

    txt <- readLines(file)
    ver <- grep("version=", txt, perl = TRUE)
    version_ <- if (length(ver))
                    gsub("version=([0-9]+[.][0-9]+[.][0-9]+).*",
                         "\\1", txt[min(ver)])
                else
                    NA

    if (version == "auto") {
        if (!length(ver))
            stop("no version information in logfile")
        if (grepl("[0-9]+[.][0-9]+[.][0-9]+", version_))
            version <- version_
        else
            stop("no version information in logfile")            
    } else if (!is.na(version_) && (version_ != version))
        warning("logfile version is ", sQuote(version_))


    field <- function(s, field) {
        ans <- lapply(tmp, strsplit, "=")
        unlist(ans, `[[`, 1L)
    }
    
    if (version %in% c("3.4.3", "3.6.0")) {
        tmp <- strsplit(txt, "\t")
        
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
        cause         <- substr(txt[ver], comma + 1L, nchar(txt))
        
        
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
        
    } else
        stop("not sure how to handle this version")

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
               stringsAsFactors = FALSE)
    
}

read_logfile <- function(file, version = "auto", expand.abbrevs = TRUE) {

    field <- function(x, n)
        unlist(lapply(x, `[[`, n))

    txt <- readLines(file)

    ## version number present? [version number since 3.2.0]
    ver <- grep("^[0-9]+[.][0-9]+[.][0-9]+.*", txt, perl = TRUE)
    version_ <- if (length(ver))
                    gsub("^([0-9]+[.][0-9]+[.][0-9]+).*", "\\1", txt[min(ver)])
                else
                    NA

    if (version == "auto") {
        if (!length(ver))
            stop("no version information in logfile")
        if (grepl("[0-9]+[.][0-9]+[.][0-9]+", version_))
            version <- version_
        else
            stop("no version information in logfile")            
    } else if (!is.na(version_) && (version_ != version))
        warning("logfile version is ", sQuote(version_))
    
    if (version %in% c("3.4.3", "3.6.0")) {
        comma <- regexpr(",", txt, fixed = TRUE)    
        txt0 <- substr(txt[ver], 1, comma - 1L)
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
        cause         <- substr(txt[ver], comma + 1L, nchar(txt))
        
        
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
        
    } else if (version %in% c("3.1.0")) {

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
    


    df0 <- data.frame(version       =  version, 
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
                      stringsAsFactors = FALSE)

    df0
}
