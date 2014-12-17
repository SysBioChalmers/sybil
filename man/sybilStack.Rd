\name{sybilStack}

\alias{sybilStack}
\alias{stinit}
\alias{stclear}
\alias{stpush}
\alias{stpop}
\alias{stunshift}
\alias{stshift}
\alias{stseek}
\alias{stfirst}
\alias{stlist}
\alias{stlength}
\alias{stexists}

\title{
  A Data Type Providing Stack (LIFO) And Queue (FIFO) Functionality
}

\description{
  These functions implement simple stack or queue functionality.
}

\usage{
  stinit(stname)
  stclear(stname)
  stpush(stname, value)
  stpop(stname)
  stunshift(stname, value)
  stshift(stname)
  stseek(stname)
  stfirst(stname)
  stlist(stname)
  stlength(stname)
  stexists(stname)
}

\arguments{
  \item{stname}{
    A single character string, giving the name of the stack or queue.
  }
  \item{value}{
    Value to add to the stack or queue.
  }
}

\details{
  The funtion \code{stinit} creates an empty stack named \code{stname}.
  
  The funtion \code{stclear} removes the stack named \code{stname}.
  
  The funtion \code{stpush} appends element \code{value} at the end of the
  stack named \code{stname}.

  The funtion \code{stpop} removes the last element of the stack named
  \code{stname} and returns it invisible.

  The funtion \code{stunshift} appends element \code{value} at the beginning of
  the stack \code{stname}.

  The funtion \code{stshift} removes the first element of the stack named
  \code{stname} and returns it invisible.

  The funtion \code{stseek} returns the last element of the stack named
  \code{stname} but does not remove it.

  The funtion \code{stfirst} returns the first element of the stack named
  \code{stname} but does not remove it.

  The funtion \code{stlist} returns the stack named \code{stname} as list.

  The funtion \code{stlength} returns the number of elements stored in the stack
  named \code{stname}.

  The funtion \code{stexists} returns \code{TRUE} if a stack named
  \code{stname} exists, otherwise \code{FALSE}.
}

\value{
  The functions \code{stpop} and \code{stshift} return the last/first element
  of the stack invisibly. The functions \code{stseek} and \code{stfirst} just
  return the last/first element.
}

\author{
  Gabriel Gelius-Dietrich

  Maintainer: Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>
}

\examples{
  ## initialize empty stack named test
  stinit("test")

  ## add a few elemets
  stpush("test", 9)
  stpush("test", 3)
  stpush("test", 7)

  ## get last element
  stpop("test")

  ## remove stack
  stclear("test")
}
