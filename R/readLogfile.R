## file <- "/var/games/nethack/logfile"
## readLogfile(file)

readLogfile <- function(file, version = "3.4.3", expand.abbrevs = TRUE) {

    field <- function(x, n)
        unlist(lapply(x,`[[`, n))
    
    txt <- readLines(file)

    ## version number present?
    ver <- grep("^[0-9][.][0-9][.][0-9].*", txt)

    if (ver != version)
        warning("logfile version is ", sQuote(ver))
    
    ## (i) with version number (since 3.2.0)
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

    df0 <- data.frame(version       =  version, 
                      score         =  score, 
                      dun.num       =  dun.num, 
                      dun.level     =  dun.level, 
                      dun.level.max =  dun.level.max, 
                      hp            =  hp, 
                      hp.max        =  hp.max, 
                      ndeaths       =  ndeaths, 
                      end.date      =  end.date, 
                      start.date    =  start.date, 
                      uid           =  uid, 
                      role          =  role, 
                      race          =  race, 
                      gender        =  gender, 
                      alignment     =  alignment, 
                      name          =  name, 
                      cause         =  cause,
                      stringsAsFactors = FALSE)

    df0
}
