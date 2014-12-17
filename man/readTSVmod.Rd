\name{readTSVmod}
\alias{readTSVmod}

\encoding{utf8}

\title{
  Read a Metabolic Network in a TSV (CSV) Format
}
\description{
  The function \code{readTSVmod} reads metabolic networks in text files,
  following a character-separated value format. Each line should contain one
  entry; the default value separator is a tab. Output files from the
  BiGG database are compatible.
}
\usage{
  readTSVmod(prefix, suffix,
             reactList, metList = NA, modDesc = NA,
             fielddelim = "\t", entrydelim = ", ", extMetFlag = "b",
             excludeComments = TRUE,
             oneSubSystem = TRUE,
             mergeMet = TRUE,
             balanceReact = TRUE,
             remUnusedMetReact = TRUE,
             singletonMet = FALSE,
             deadEndMet = FALSE,
             remMet = FALSE,
             constrMet = FALSE,
             tol = SYBIL_SETTINGS("TOLERANCE"),
             fpath = SYBIL_SETTINGS("PATH_TO_MODEL"),
             def_bnd = SYBIL_SETTINGS("MAXIMUM"),
             arrowlength = NULL,
             quoteChar = "",
             commentChar, ...)
}

\arguments{
  \item{prefix}{
    A single character string giving the prefix for three possible input files
    (see Details below).
  }
  \item{suffix}{
    A single character string giving the file name extension. If missing, the
    value of \code{suffix} depends on the argument \code{fielddelim}, see
    Details below.\cr
    Default: \code{"tsv"}.
  }
  \item{reactList}{
    A single character vector giving a file name containing a reaction list.
    Only necessary, if argument \code{suffix} is empty.
  }
  \item{metList}{
    A single character vector giving a file name containing a metabolite
    list.\cr
    Default: \code{NA}.
  }
  \item{modDesc}{
    A single character vector giving a file name containing a model
    description.\cr
    Default: \code{NA}.
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
  \item{extMetFlag}{
    A single character string giving the identificator for metabolites which
    are outside the system boundary. Only necessary, if the model is a closed
    one.\cr
    Default: \code{"b"}.
  }
  \item{excludeComments}{
    A Boolean value. Sometimes, the reaction abbreviations and/or the metabolite
    abbreviations contain comments in square brackets. If set to \code{TRUE},
    these comments will be removed. If set to \code{FALSE}, whitespaces included
    in comments in metabolite abbreviations will be removed. Comments in
    reaction abbreviations stay unchanged. A reaction id with comment is, for
    example, the string: \code{pfk [comment]}, with \code{[comment]} being the
    comment. There must be at least one whitespace between id and comment,
    otherwise it will be considered as compartment flag.\cr
    Default: \code{TRUE}.
  }
  \item{oneSubSystem}{
    A Boolean value. Ignore parameter \code{entrydelim} for the field
    \sQuote{subsystem}, if every reaction belongs to exactly one sub system.\cr
    Default: \code{TRUE}.
  }
  \item{mergeMet}{
    Boolean: if set to \code{TRUE}, metabolites used more than once as reactand
    or product in a particular reaction are added up, see details below. If set
    to \code{FALSE}, the last value is used without warning.\cr
    Default: \code{TRUE}.
  }
  \item{balanceReact}{
    Boolean: if set to \code{TRUE}, metabolites used as reactand and product in
    a particular reaction at the same time are balanced, see details below. If
    set to \code{FALSE} the last value is used without warning (reactands before
    products).\cr
    Default: \code{TRUE}.
  }
  \item{remUnusedMetReact}{
    Boolean: if set to TRUE, metabolites and reactions which are not used in the
    stoichiometric matrix will be removed. A metabolite or a reaction is
    considered as unused, if the corresponding element  of \code{rowSums}
    (metabolites) or \code{colSums} (reactions) of the binary version of the
    stoichiometric matrix is zero, see details below. If set to \code{FALSE},
    only a warning is given.\cr
    Default: \code{FALSE}.
  }
  \item{singletonMet}{
    Boolean: if set to TRUE, metabolites appearing only once in the
    stoichiometric matrix are identified. Metabolites appear only
    once, if \code{\link{rowSums}} of the binary stoichiometric matrix is one in
    the corresponding row, see details below.\cr
    Default: \code{FALSE}.
  }
  \item{deadEndMet}{
    Boolean: if set to TRUE, metabolites which are produced but not consumed, or
    vice versa are identified, see details below. If both arguments
    \code{singletonMet} and \code{deadEndMet} are set to \code{TRUE}, the
    function will first look for singleton metabolites, and exclude them (and
    the corresponding reactions) from the search list. Afterwards, dead end
    metabolites are searched only in the smaller model.\cr
    Default: \code{FALSE}.
  }
  \item{remMet}{
    Boolean: if set to TRUE, metabolites identified as singleton or dead end
    metabolites will be removed from the model. Additionally, reactions
    containing such metabolites will be removed also.\cr
    Default: \code{FALSE}.
  }
  \item{constrMet}{
    Boolean: if set to TRUE, reactions containing metabolites identified as
    singleton or dead end metabolites will be constrained to zero.\cr
    Default: \code{FALSE}.
  }
  \item{tol}{
    A single numeric value, giving the smallest positive floating point number
    unequal to zero, see details below.\cr
    Default: \code{SYBIL_SETTINGS("TOLERANCE")}.
  }
  \item{fpath}{
    A single character string giving the path to a certain directory containing
    the model files.\cr
    Default: \code{SYBIL_SETTINGS("PATH_TO_MODEL")}.
  }
  \item{def_bnd}{
    A single numeric value. Absolute value for uppper and lower bounds for
    reaction bounds.\cr
    Default: \code{SYBIL_SETTINGS("MAXIMUM")}.
  }
  \item{arrowlength}{
    A single numeric or character value or \code{NULL}. This argument controls
    the number of \code{"-"} and \code{"="} used in reaction arrows in the
    equation strings. If set to \code{NULL}, one or more symbols are used.
    The regular expression used is \code{"<?[=-]+>"}.
    If numeric, all reaction arrows must consist of exactly \code{arrowlength}
    signs. The regular expression used is \code{"<?[=-]{arrowlength}>"}.
    If character, \code{arrowlength} must be a regular expression and will be
    used as \code{"<?[=-]arrowlength>"}. For example, if \code{arrowlength} is
    \code{"{1,2}"} the regular expression is \code{"<?[=-]{1,2}>"}, meaning the
    reaction arrow can consist of one or two signs. In any case, the completed
    regular expression will always used with argument \code{perl = TRUE}.\cr
    Default: \code{NULL}.
  }
  \item{quoteChar}{
    Set of quoting characters used for the argument \code{quote} in
    \code{\link{read.table}}, see there for details.\cr
    Default: \code{""} (disable quoting).
  }
  \item{commentChar}{
    A single character used for the argument \code{comment.char} in
    \code{\link{read.table}}, see there for details. If a comment char is
    needed, e.g. \dQuote{\code{@}} (at) seems to be a good one.\cr
    Default: \code{""}.
  }
  \item{\dots}{
    Further arguments passed to \code{\link{read.table}}, e.g. argument
    \code{quote}, \code{comment.char} or argument \code{fill}, if some lines do
    not have enough elements. If all fields are in double quotes, for example,
    set \code{quote} to \code{"\""}.
  }
}

\details{
  A metabolic model consists of three input files:
  \enumerate{
    \item \code{<prefix>_react.<suffix>} containing all reactions.
    \item \code{<prefix>_met.<suffix>} containing all metabolites.
    \item \code{<prefix>_desc.<suffix>} containing a model description.
  }
  All of these files must be character separated value files (for a detailed
  format description and examples, see package vignette).  The argument
  \code{prefix} is the part of the filenames, all three have in common (e.g. if
  they where produced by \code{\link{modelorg2tsv}}).
  Alternatively, the arguments \code{reactList}, \code{metList} and
  \code{modDesc} can be used.  A file containing all reactions must be there,
  everything else is optional.

  If \code{suffix} is missing, it is set according to the value of
  \code{fielddelim}:
  \tabular{ll}{
      \code{"\t"}   \tab \code{"tsv"} \cr
      \code{";"}    \tab \code{"csv"} \cr
      \code{","}    \tab \code{"csv"} \cr
      \code{"|"}    \tab \code{"dsv"} \cr
      anything else \tab \code{"dsv"} \cr
  }

  The argument \code{...} is passed to \code{\link{read.table}}.
  
  In some cases, it could be necessary, to turn off quoting
  \code{quoteChar = ""} (default), if e.g. metabolite names contain quoting
  characters \code{"'"} like in \code{3',5'-bisphosphate nucleotidase}. If all
  fields are in quotes (e.g. files generated by \code{\link{modelorg2tsv}}), use
  \code{quoteChar = "\""} for example.

  The input files are read using the function \code{\link{read.table}}.  The
  argument \code{header} is set to \code{TRUE} and the argument \code{sep} is
  set to the value of \code{fielddelim}.  Everything else can be passed via
  the \code{...} argument.

  The header for the reactions list may have the following columns:
  \tabular{ll}{
      \code{"abbreviation"} \tab a unique reaction id                       \cr
      \code{"name"}         \tab a reaction name                            \cr
      \code{"equation"}     \tab the reaction equation                      \cr
      \code{"reversible"}   \tab TRUE, if the reaction is reversible        \cr
      \code{"compartment"}  \tab reaction compartment(s) (currently unused) \cr
      \code{"lowbnd"}       \tab lower bound                                \cr
      \code{"uppbnd"}       \tab upper bound                                \cr
      \code{"obj_coef"}     \tab objective coefficient                      \cr
      \code{"rule"}         \tab gene to reaction association               \cr
      \code{"subsystem"}    \tab subsystem of the reaction                  \cr
  }
  Every entry except for \code{"equation"} is optional. If there are missing
  values in field \code{"lowbnd"}, they will be set to \code{-1 * def_bnd};
  if there are missing values in field \code{"uppbnd"}, they will be set to
  \code{def_bnd}; if there are missing values in field \code{"obj_coef"}, they
  will be set to \code{0}.

  The header for the metabolites list may have the following columns:
  \tabular{ll}{
      \code{"abbreviation"} \tab a unique metabolite id                    \cr
      \code{"name"}         \tab a metabolite name                         \cr
      \code{"compartment"}  \tab metabolite compartment (currently unused) \cr
  }
  If a metabolite list is provided, it is supposed to contain at least the
  entries \code{"abbreviation"} and \code{"name"}.

  The header for the model description file may have the following columns:
  \tabular{ll}{
      \code{"name"}         \tab a name for the model             \cr
      \code{"id"}           \tab a shorter model id               \cr
      \code{"description"}  \tab a model description              \cr
      \code{"compartment"}  \tab the compartments                 \cr
      \code{"abbreviation"} \tab unique compartment abbreviations \cr
      \code{"Nmetabolites"} \tab number of metabolites            \cr
      \code{"Nreactions"}   \tab number of reactions              \cr
      \code{"Ngenes"}       \tab number of independend genes      \cr
      \code{"Nnnz"}         \tab number of non-zero elements in
                                 the stoichiometric matrix        \cr
  }
  If a file contains a certain column name, there must be no empty entries.
  
  If a model description file is provided, it is supposed to contain at least
  the entries \code{"name"} and \code{"id"}.  Otherwise, the filename of the
  reactions list will be used (the filename extension and the string
  \code{_react} at the end of the filename will be removed).

  The compartments in which a reaction takes place is determined by the
  compartment flags of the participating metabolites.

  All fields in the output files of \code{\link{modelorg2tsv}} are in double
  quotes. In order to read them, set argument \code{quoteChar} to \code{"\""}.

  Please read the package vignette for detailed information about input formats
  and examples.

  If a metabolite is used more than once as product or
  reactand of a particular reaction, it is merged:
  \code{a + (2) a} is converted to \code{(3) a}
  and a warning will be given.

  If a metabolite is used first as reactand and then as
  product of a particular reaction, the reaction is
  balanced:
      \code{(2) b + a -> b + c}
  is converted to
      \code{    b + a ->     c}

  A binary version of the stoichiometric matrix \eqn{S} is constructed
  via \eqn{\left|S\right| > tol}{abs(S) > tol}.

  A binary version of the stoichiometric matrix \eqn{S} is scanned for reactions
  and metabolites which are not used in S. If there are some, a warning will be
  given and the corresponding reactions and metabolites will be removed from
  the model if \code{remUnusedMetReact} is set to \code{TRUE}.

  The binary version of the stoichiometric matrix \eqn{S} is scanned for
  metabolites, which are used only once in S. If there are some, at least a
  warning will be given. If either \code{constrMet} or \code{remMet} is set to
  \code{TRUE}, the binary version of \eqn{S} is scanned for paths of singleton
  metabolites. If \code{constrMet} is set to \code{TRUE}, reactions containing
  those metabolites will be constrained to zero; if \code{remMet} is set to
  \code{TRUE}, the metabolites and the reactions containing those metabolites
  will be removed from the network.

  In order to find path of singleton metabolites a binary version of the
  stoichiometric matrix \eqn{S} is used. Sums of rows gives the vector of
  metabolite usage, each element is the number of reactions a metabolite
  participates.  A single metabolite (singleton) is a metabolite with a row sum
  of zero. All columns in \eqn{S} (reactions) containing singleton metabolites
  will be set to zero. And again, singleton metabolites will be searched until
  none are found.

  The algorithm to find dead end metabolites works in a quite similar way, but
  not in the binary version of the stroichiometric matrix. Here, metabolite
  \code{i} is considered as dead end, if it is for example produced by reaction
  \code{j} but not used by any other reaction \code{k}.
}

\value{
  An instance of class \code{\link{modelorg}}.
}

\references{
  The BiGG database \url{http://bigg.ucsd.edu/}.

  Schellenberger, J., Park, J. O., Conrad, T. C., and Palsson, B. Ø., (2010)
  BiGG: a Biochemical Genetic and Genomic knowledgebase of large scale metabolic
  reconstructions. \emph{BMC Bioinformatics} \bold{11}, 213.

  Becker, S. A., Feist, A. M., Mo, M. L., Hannum, G., Palsson, B. Ø.  and
  Herrgard, M. J. (2007) Quantitative prediction of cellular metabolism with
  constraint-based models: the COBRA Toolbox. \emph{Nat Protoc} \bold{2},
  727--738.

  Schellenberger, J., Que, R., Fleming, R. M. T., Thiele, I., Orth, J. D.,
  Feist, A. M., Zielinski, D. C., Bordbar, A., Lewis, N. E., Rahmanian, S.,
  Kang, J., Hyduke, D. R. and Palsson, B. Ø. (2011) Quantitative prediction of
  cellular metabolism with constraint-based models: the COBRA Toolbox v2.0.
  \emph{Nat Protoc} \bold{6}, 1290--1307.
}

\author{
  Gabriel Gelius-Dietrich
}

\seealso{
  \code{\link{read.table}}, \code{\link{modelorg2tsv}}, \code{\link{modelorg}}
}

\examples{
  ## read example dataset
  mp  <- system.file(package = "sybil", "extdata")
  mod <- readTSVmod(prefix = "Ec_core", fpath = mp, quoteChar = "\"")

  ## redirect warnings to a log file
  sink(file = "warn.log")
  mod <- readTSVmod(prefix = "Ec_core", fpath = mp, quoteChar = "\"")
  warnings()
  sink()
  unlink("warn.log")  

  ## print no warnings
  suppressWarnings(
    mod <- readTSVmod(prefix = "Ec_core", fpath = mp, quoteChar = "\""))

  ## print no messages
  suppressMessages(
    mod <- readTSVmod(prefix = "Ec_core", fpath = mp, quoteChar = "\""))

\dontrun{
  ## set number of warnings to keep
  options(nwarnings = 1000)
  
  ## redirect every output to a file
  zz <- file("log.Rout", open = "wt")
  sink(zz)
  sink(zz, type = "message")
  mod <- readTSVmod(prefix = "Ec_core", fpath = mp, quoteChar = "\"")
  warnings()
  sink(type = "message")
  sink()
  close(zz)  
}
}
\keyword{ IO }
