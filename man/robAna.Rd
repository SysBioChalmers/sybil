\name{robAna}
\alias{robAna}

\encoding{utf8}

\title{
  Robustness Analysis
}

\description{
  Performs robustness analysis for a given metabolic model.
}

\usage{
  robAna(model, ctrlreact, rng = NULL,
         numP = 20, verboseMode = 1, ...)
}

\arguments{
  \item{model}{
    An object of class \code{\linkS4class{modelorg}}.
  }
  \item{ctrlreact}{
    An object of class \code{\linkS4class{reactId}}, character or integer.
    Specifies the control reaction -- the parameter to vary.
  }
  \item{rng}{
    A numeric vector of length two, giving the lower and upper bound of the
    control reaction. If set to \code{NULL} (the default), the range will be
    computed by flux variability analysis for the reaction given in
    \code{ctrlreact}.\cr
    Default: \code{NULL}
  }
  \item{numP}{
    The number of points to analyse.\cr
    Default: \code{20}
  }
  \item{verboseMode}{
    An integer value indicating the amount of output to stdout, see
    \code{\link{optimizer}} for details.\cr
    Default: \code{1}.
  }
  \item{\dots}{
    Further arguments passed to \code{\link{optimizer}}.
  }
}

\details{
  The function \code{robAna} performs a robustness analysis with a given
  model. The flux of \code{ctrlreact} will be varied in \code{numP} steps
  between the maximum and minimum value the flux of \code{ctrlreact} can reach.
  For each of the \code{numP} datapoints the followong lp problem is solved
  \deqn{%
      \begin{array}{rll}%
          \max            & \mbox{$c$}^{\mathrm{T}}
                                         \mbox{$v$} \\[1ex]
          \mathrm{s.\,t.} & \mbox{$Sv$} = 0         \\[1ex]
                          & v_j = c_k                                   \\[1ex]
                          & \alpha_i \leq v_i \leq \beta_i
                          & \quad \forall i \in \{1, \ldots, n\}, i \neq j\\[1ex]
      \end{array}%
  }{
      max  c^T v
      s.t. Sv = 0
           v_j = c_k
           a_i <= v_i <= b_i  for i = 1, ..., n, i != j
  }
  with \eqn{\bold{S}}{S} being the stoichiometric matrix, \eqn{\alpha_i}{a_i}
  and \eqn{\beta_i}{b_i} being the lower and upper bounds for flux (variable)
  \eqn{i}.  The total number of variables of the optimization problem is denoted
  by \eqn{n}. The parameter \eqn{c_k} is varied \code{numP} times in the range
  of \eqn{v_{j,\mathrm{min}}}{v_i,min} to \eqn{v_{j,\mathrm{max}}}{v_i,max}.
  The result of the optimization is returned as object of class
  \code{\linkS4class{optsol_robAna}} containing the objective
  value for each datapoint.
  
  The extreme points of the range for \code{ctrlreact} are calculated via flux
  balance analysis (see also
  \code{\linkS4class{sysBiolAlg_fba}}) with the objective
  function being minimization and maximization of the flux through
  \code{ctrlreact}.
}

\value{
  An object of class \code{\linkS4class{optsol_robAna}}.
}

\references{
  Becker, S. A., Feist, A. M., Mo, M. L., Hannum, G., Palsson, B. Ø.  and
  Herrgard, M. J. (2007) Quantitative prediction of cellular metabolism with
  constraint-based models: the COBRA Toolbox. \emph{Nat Protoc} \bold{2},
  727--738.

  Schellenberger, J., Que, R., Fleming, R. M. T., Thiele, I., Orth, J. D.,
  Feist, A. M., Zielinski, D. C., Bordbar, A., Lewis, N. E., Rahmanian, S.,
  Kang, J., Hyduke, D. R. and Palsson, B. Ø. (2011) Quantitative prediction of
  cellular metabolism with constraint-based models: the COBRA Toolbox v2.0.
  \emph{Nat Protoc} \bold{6}, 1290--1307.

  Bernhard Ø. Palsson (2006).
  \emph{Systems Biology: Properties of Reconstructed Networks}.
  Cambridge University Press.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\examples{
  data(Ec_core)
  rb <- robAna(Ec_core, ctrlreact = "EX_o2(e)")
  plot(rb)
}

\keyword{optimize}

