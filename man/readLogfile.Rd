\name{NethackLogfiles}
\alias{NethackLogfiles}
\alias{NethackLogfiles-package}
\alias{read_logfile}
\alias{read_xlogfile}
\title{
  Parse Nethack Logfiles
}
\description{
  Read logfiles of the ancient game of Nethack.
}
\usage{
read_logfile(file, version = "auto", expand.abbrevs = TRUE)
read_xlogfile(file, version = "auto", expand.abbrevs = TRUE)
}
\arguments{
  \item{file}{
    character; the logfile (or record file)
}
  \item{version}{
    character
  }
  \item{expand.abbrevs}{
    logical; should abbreviated rolenames etc. be expanded? Default is \code{TRUE}.
  }  
}
\details{

  Currently only version 3.4.3 and newer are
  supported (i.e. regularly tested). Logfiles
  for version 3.1.0 should also work.

}
\value{
  A \code{\link{data.frame}}.  

     \item{version}{Nethack version} 
     \item{name}{player name} 
     \item{score}{score} 
     \item{dun.num}{} 
     \item{dun.level}{dungeon level when killed} 
     \item{dun.level.max}{deepest dungeon level visited} 
     \item{hp}{hitpoints} 
     \item{hp.max}{maximum hitpoints} 
     \item{ndeaths}{number of deaths} 
     \item{death.date}{Date; end of game} 
     \item{birth.date}{Date; start of game} 
     \item{uid}{uid} 
     \item{role}{role} 
     \item{race}{race} 
     \item{gender}{gender: male or female} 
     \item{alignment}{alignment} 
     \item{cause}{cause of death}

}
\references{
  
   \url{http://www.saunalahti.fi/~juklahta/nethack.html}

   \url{http://nethackwiki.com/wiki/Logfile}

   \url{https://metacpan.org/source/SARTAK/NetHack-Logfile-1.00/lib/NetHack/Logfile/Entry/33.pm}

   \url{http://nethack.wikia.com/wiki/NetHack_3.1.0/topten.c}

   \url{https://nethackwiki.com/wiki/Source:NetHack_3.1.0/topten.c}

   \url{http://alt.org/nethack/}

   \url{http://ftp.sunet.se/pub/games/nethack/General/Source/Utils/reclist.c}

   \url{https://github.com/NetHack/NetHack}

   https://github.com/enricoschumann/NethackLogfiles

   https://scoreboard.xd.cm/about.html
   
}
\author{
  Enrico Schumann
}
\examples{
\dontrun{
require("NethackLogfiles")
nh <- read_logfile("/var/games/nethack/logfile")
nh <- read_logfile("/home/es/Documents/Nethack/3.6.0/logfile")
boxplot(nh$score ~ nh$role)
boxplot(nh$dun.level.max~nh$role)
plot(nh$death.date, nh$score, type = "h")
}
}
