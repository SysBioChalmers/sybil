\name{modelorg2tsv}
\alias{modelorg2tsv}

\encoding{utf8}

\title{
  Write an Instance of Class modelorg to File
}

\description{
  The function \code{modelorg2tsv} writes the content of an instance of class
  \code{\link{modelorg}} to text files in a character-separated value format
  adopted from the BiGG database output.
}

\usage{
  modelorg2tsv(model, prefix, suffix, extMetFlag = "b",
               fielddelim = "\t", entrydelim = ", ",
               makeClosedNetwork = FALSE,
               onlyReactionList = FALSE,
               minimalSet = FALSE,
               fpath = SYBIL_SETTINGS("PATH_TO_MODEL"), ...)
}

\arguments{
  \item{model}{
    An object of class \code{\link{modelorg}}.
  }
  \item{prefix}{
    A single character string giving the prefix for three possible output files
    (see Details below).
  }
  \item{suffix}{
    A single character string giving the file name extension. If missing, the
    value of \code{suffix} depends on the argument \code{fielddelim}, see
    Details below.\cr
    Default: \code{"tsv"}.
  }
  \item{extMetFlag}{
    A single character string giving the identificator for metabolites which
    are outside the system boundary.  Only necessary, if the model is a closed
    one.\cr
    Default: \code{"b"}.
  }
  \item{fielddelim}{
    A single character string giving the value separator.\cr
    Default: \code{"\t"}.
  }
  \item{entrydelim}{
    A single character string giving the a separator for values containing
    more than one entry.\cr
    Default: \code{", "}.
  }
  \item{makeClosedNetwork}{
    Boolean. If set to \code{TRUE}, external metabolites (which are outside the
    system boundary) will be added to the model. These metabolites participate
    in reactions, transporting metabolites across the system boundary.
    The metabolite id will be the same as for the metabolite inside the system,
    but the compartment type is set to the value of argument
    \code{extMetFlag}.
    
    For example, most models contain a transport reaction for glucose:
    
    \code{glc[c] <==> }
    
    If \code{makeClosedNetwork} is set to \code{TRUE}, this reaction will be
    written as
    
    \code{glc[c] <==> glc[b]}
    
    with the letter \code{b} being the default value for \code{extMetFlag}.\cr
    Default: \code{FALSE}.
  }
  \item{onlyReactionList}{
    Boolean. If set to \code{TRUE}, only one file containing all reaction
    equations will be produced (output file has one column).\cr
    Default: \code{FALSE}.
  }
  \item{minimalSet}{
    Boolean. If set to \code{TRUE}, only one file containing the fields
    \code{"abbreviation"}, \code{"equation"}, \code{"lowbnd"}, \code{"uppbnd"}      
    and \code{"obj_coef"} will be produced (output file has five columns).\cr
    Default: \code{FALSE}.
  }
  \item{fpath}{
    A single character string giving the path to a certain directory in which
    the output files will be stored.\cr
    Default: \code{SYBIL_SETTINGS("PATH_TO_MODEL")}.
  }
  \item{\dots}{
    Further arguments passed to \code{\link{write.table}}, e.g. the Boolean
    argument \code{quote} can be used here.
  }
}

\details{
  The function \code{modelorg2tsv} produces three output files: a reactions
  list, a metabolites list and a model description file.
  
  The reactions list has the following columns:
  \tabular{ll}{
      \code{"abbreviation"} \tab \code{react_id(model)}   \cr
      \code{"name"}         \tab \code{react_name(model)} \cr
      \code{"equation"}     \tab the reaction equations   \cr
      \code{"reversible"}   \tab \code{react_rev(model)}  \cr
      \code{"compartment"}  \tab reaction compartment(s)  \cr
      \code{"lowbnd"}       \tab \code{lowbnd(model)}     \cr
      \code{"uppbnd"}       \tab \code{uppbnd(model)}     \cr
      \code{"obj_coef"}     \tab \code{obj_coef(model)}   \cr
      \code{"rule"}         \tab \code{gpr(model)}        \cr
      \code{"subsystem"}    \tab \code{subSys(model)}     \cr
  }
  
  The metabolites list has the following columns:
  \tabular{ll}{
      \code{"abbreviation"} \tab \code{met_id(model)}   \cr
      \code{"name"}         \tab \code{met_name(model)} \cr
      \code{"compartment"}  \tab \code{met_comp(model)} \cr
  }
  
  The model description file has the following columns:
  \tabular{ll}{
      \code{"name"}         \tab \code{mod_name(model)}           \cr
      \code{"id"}           \tab \code{mod_id(model)}             \cr
      \code{"description"}  \tab \code{mod_desc(model)}           \cr
      \code{"compartment"}  \tab \code{mod_compart(model)}        \cr
      \code{"abbreviation"} \tab unique compartment abbreviations \cr
      \code{"Nmetabolites"} \tab number of metabolites            \cr
      \code{"Nreactions"}   \tab number of reactions              \cr
      \code{"Ngenes"}       \tab number of independend genes      \cr
      \code{"Nnnz"}         \tab number of non-zero elements in
                                 the stoichiometric matrix        \cr
  }
  
  If \code{onlyReactionList} is set to \code{TRUE}, only the reactions list
  containing the column \code{"equation"} is produced.
  
  Please read the package vignette for detailed information about file formats
  and examples.

  All fields in the output files are in double quotes. In order to read them in
  with \code{\link{readTSVmod}}, set argument \code{quoteChar} to \code{"\""}.
}

\value{
  Returns \code{TRUE} on success.
}

\references{
  The BiGG database \url{http://bigg.ucsd.edu/}.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  \code{\link{read.table}}, \code{\link{modelorg2tsv}}, \code{\link{modelorg}}.
}

\keyword{ IO }
