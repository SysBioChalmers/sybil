\name{ypd}
\alias{ypd}

\encoding{utf8}

\title{
  In Sillico YPD Medium
}
\description{
  Apply in sillico medium to bakers yeast metabolic network model iND750 by
  Duarte et al. 2004.
}
\usage{
  ypd(model, def_bnd = SYBIL_SETTINGS("MAXIMUM"), ver = "harrison2007")
}

\arguments{
  \item{model}{
    An object of class \code{\link{modelorg}}.
  }
  \item{def_bnd}{
    A single numeric value. Absolute value for uppper and lower bounds for
    reaction bounds.\cr
    Default: \code{SYBIL_SETTINGS("MAXIMUM")}.
  }
  \item{ver}{
    A single character string giving the version of the YPD medium. Can be set
    to \code{harrison2007} or \code{bilu2006} (see Details below).\cr
    Default: \code{harrison2007}.
  }
}

\details{
  The function \code{ypd} identifies exchange reactions via the function
  \code{\link{findExchReact}}. The lower bounds of all exchange fluxes is set
  to zero (not allowing any flux into the network) and the upper bounds are set
  to the value of \code{def_bnd} (default: output is unbounded). The lower bound
  input of the input fluxes is set like in the table below.
  
  Two different versions of YPD medium are available: Harrison et al. 2007 and
  Bilu et al. 2006.

  Harrison et al 2007:
  \tabular{ll}{
    \code{EX_ala_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_arg_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_asn_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_asp_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_chol(e)}    \tab \eqn{-0.5}          \cr
    \code{EX_cys_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_dcyt(e)}    \tab \eqn{-0.5}          \cr
    \code{EX_ergst(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_glc(e)}     \tab \eqn{-20}           \cr
    \code{EX_glu_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_gly(e)}     \tab \eqn{-0.5}          \cr
    \code{EX_gua(e)}     \tab \eqn{-0.5}          \cr
    \code{EX_h(e)}       \tab \code{def_bnd * -1} \cr
    \code{EX_hdca(e)}    \tab \eqn{-0.5}          \cr
    \code{EX_his_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_leu_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_lys_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_met_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_nh4(e)}     \tab \code{def_bnd * -1} \cr
    \code{EX_o2(e)}      \tab \eqn{-2}            \cr
    \code{EX_ocdca(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_pi(e)}      \tab \code{def_bnd * -1} \cr
    \code{EX_pro_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_ser_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_so4(e)}     \tab \code{def_bnd * -1} \cr
    \code{EX_thr_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_thymd(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_trp_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_ttdca(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_tyr_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_ura(e)}     \tab \eqn{-0.5}          \cr
  }

  Bilu et al 2006:
  \tabular{ll}{
    \code{EX_nh4(e)}     \tab \code{def_bnd * -1} \cr
    \code{EX_pi(e)}      \tab \code{def_bnd * -1} \cr
    \code{EX_so4(e)}     \tab \code{def_bnd * -1} \cr
    \code{EX_glc(e)}     \tab \eqn{-20}           \cr
    \code{EX_o2(e)}      \tab \eqn{-2}            \cr
    \code{EX_ala_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_arg_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_asn_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_asp_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_cys_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_his_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_leu_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_lys_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_met_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_pro_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_ser_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_thr_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_trp_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_tyr_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_dcyt(e)}    \tab \eqn{-0.5}          \cr
    \code{EX_gly(e)}     \tab \eqn{-0.5}          \cr
    \code{EX_gua(e)}     \tab \eqn{-0.5}          \cr
    \code{EX_thymd(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_h2o(e)}     \tab \code{def_bnd * -1} \cr
    \code{EX_na1(e)}     \tab \code{def_bnd * -1} \cr
    \code{EX_k(e)}       \tab \code{def_bnd * -1} \cr
    \code{EX_co2(e)}     \tab \code{def_bnd * -1} \cr
    \code{EX_ade(e)}     \tab \eqn{-0.5}          \cr
    \code{EX_gln_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_ile_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_phe_L(e)}   \tab \eqn{-0.5}          \cr
    \code{EX_val_L(e)}   \tab \eqn{-0.5}          \cr
  }
}

\value{
  An instance of class \code{\link{modelorg}} with input fluxes set
  corresponding to the desired YPD medium.
}

\references{
  Harrison, R., Papp, B., Pal, C., Oliver, S. G. and Delnert, D. (2007)
  Plasticity of genetic interactions in metabolic networks of yeast.
  \emph{PNAS} \bold{104}, 2307--2312.

  Bilu, Y., Shlomi, T., Barkai, N. and Ruppin, E. (2006) Conservation of
  expression and sequence of metabolic genes is reflected by activity across
  metabolic states. \emph{PLoS Comput Biol} \bold{2}, 932--938.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  \code{\linkS4class{modelorg}}, \code{\link{findExchReact}} and
  \code{\link{SYBIL_SETTINGS}}
}
