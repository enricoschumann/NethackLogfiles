\name{NethackLogfiles}
\alias{NethackLogfiles}
\alias{NethackLogfiles-package}
\alias{readLogfile}
\title{
  Parse Nethack Logfiles
}
\description{
  Read logfiles of the ancient game of Nethack.
}
\usage{
readLogfile(file, version = "3.4.3", expand.abbrevs = TRUE)
}
\arguments{
  \item{file}{
    character; the logfile (or record file)
}
  \item{version}{
    character or \code{NA}
  }
  \item{expand.abbrevs}{
    logical; should abbreviated rolenames etc. be expanded? Default is \code{TRUE}.
  }  
}
\details{
  Currently only version 3.4.3 and never are supported (i.e. tested).
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

   \url{http://alt.org/nethack/}

   \url{http://ftp.sunet.se/pub/games/nethack/General/Source/Utils/reclist.c}
}
\author{
  Enrico Schumann
}
\examples{
\dontrun{
require("NethackLogfiles")
nh <- readLogfile("/var/games/nethack/logfile")
boxplot(nh$score ~ nh$role)
boxplot(nh$dun.level.max~nh$role)
plot(nh$death.date, nh$score, type = "h")
}
}
