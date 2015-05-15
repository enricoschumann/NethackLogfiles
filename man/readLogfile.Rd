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
  Currently only version 3.4.3 is supported.
}
\value{
  A data.frame.  For version 3.4.3

      %% df0 <- data.frame(version       =  version, 
      %%                 score         =  score, 
      %%                 dun.num       =  dun.num, 
      %%                 dun.level     =  dun.level, 
      %%                 dun.level.max =  dun.level.max, 
      %%                 hp            =  hp, 
      %%                 hp.max        =  hp.max, 
      %%                 ndeaths       =  ndeaths, 
      %%                 end.date      =  end.date, 
      %%                 start.date    =  start.date, 
      %%                 uid           =  uid, 
      %%                 role          =  role, 
      %%                 race          =  race, 
      %%                 gender        =  gender, 
      %%                 alignment     =  alignment, 
      %%                 name          =  name, 
      %%                 cause         =  cause)

 %% \item{version}{Description of 'comp1'}
 %% \item{score}{Description of 'comp2'}

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
readLogfile("/var/games/nethack/logfile")}
}
