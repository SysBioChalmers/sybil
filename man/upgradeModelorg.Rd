\name{upgradeModelorg}
\alias{upgradeModelorg}

\encoding{utf8}

\title{
  Upgrade modelorg to newer version.
}
\description{
  Performs necessary changes to the object to promote it to a newer version.
}
\usage{
  upgradeModelorg(object)
}

\arguments{
  \item{object}{
    An object of class \code{\linkS4class{modelorg}}.
  }
}

\details{
	This method performs the necessary changes on a modelorg object to promote 
	it to a newer version.
	
	Changes from previous modelorg version (no version slot set) to version 2.0:
	Representation in the gprRules slot is now incompatible to the earlier versions.
}

\value{
  An object of class \code{\linkS4class{modelorg}}, matching the current 
  version requirements used by sybil.
}

\author{
  Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
  
  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\examples{
	data(Ec_core)
	upgradeModelorg(Ec_core)
}

\keyword{upgrade, version}

