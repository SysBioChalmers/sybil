\name{sybilLog-class}

\Rdversion{1.1}
\docType{class}

\alias{sybilLog-class}
\alias{sybilLog}

\alias{file-class}

\alias{didFoot,sybilLog-method}
\alias{didFoot<-,sybilLog-method}
\alias{didFoot}
\alias{didFoot<-}
\alias{fenc,sybilLog-method}
\alias{fenc<-,sybilLog-method}
\alias{fenc}
\alias{fenc<-}
\alias{fh,sybilLog-method}
\alias{fh<-,sybilLog-method}
\alias{fh}
\alias{fh<-}
\alias{fname,sybilLog-method}
\alias{fname<-,sybilLog-method}
\alias{fname}
\alias{fname<-}
\alias{fpath,sybilLog-method}
\alias{fpath<-,sybilLog-method}
\alias{fpath}
\alias{fpath<-}
\alias{logCall,sybilLog-method}
\alias{logCall}
\alias{logClose<-,sybilLog-method}
\alias{logClose<-}
\alias{logComment,sybilLog-method}
\alias{logComment}
\alias{logError,sybilLog-method}
\alias{logError,sybilLog,ANY,ANY-method}
\alias{logError,sybilLog,ANY,numeric-method}
\alias{logError}
\alias{logFH,sybilLog-method}
\alias{logFH}
\alias{logFoot<-,sybilLog-method}
\alias{logFoot<-}
\alias{logHead,sybilLog-method}
\alias{logHead}
\alias{loglevel,sybilLog-method}
\alias{loglevel<-,sybilLog-method}
\alias{loglevel}
\alias{loglevel<-}
\alias{logMessage,sybilLog-method}
\alias{logMessage}
\alias{logOptimization,sybilLog-method}
\alias{logOptimization}
\alias{logOptimizationTH,sybilLog-method}
\alias{logOptimizationTH}
\alias{logStep<-,sybilLog-method}
\alias{logStep<-}
\alias{logWarning,sybilLog-method}
\alias{logWarning}
\alias{lstname,sybilLog-method}
\alias{lstname}
\alias{verblevel,sybilLog-method}
\alias{verblevel<-,sybilLog-method}
\alias{verblevel}
\alias{verblevel<-}

\alias{logFileFH}


\title{Class \code{"sybilLog"}}

\description{
  Handles log files, messages warnings and errors.
}

\section{Objects from the Class}{
  Objects can be created by calls of the function \code{sybilLog}:

  \code{logObj <- sybilLog(filename)}.
}

\section{Slots}{
  \describe{
    \item{\code{fh}:}{Object of class \code{\link{file}} which is a connection
    to a file to print to.
    }
    \item{\code{fname}:}{Object of class \code{"character"}
      being the name of the file to print to. If set to \code{NA}, no logfile
      is used. Default: \code{NA}.
    }
    \item{\code{fpath}:}{Object of class \code{"character"}
      giving the path to the file mentioned in \code{fname}.
      Default: \code{"."}.
    }
    \item{\code{fenc}:}{Object of class \code{"character"}
      encoding of the log file. Default: \code{""}.
    }
    \item{\code{loglevel}:}{Object of class \code{"integer"}
      controlling the amount of details to log: If set to \code{0},
      nothing will be written to the logfile. If set to \code{> 0}, all
      warnings are logged; if set do \code{> 1}, also messages are logged.
      If \code{loglevel} is \code{> 2}, the used function call will be printed.
      Default: \code{0}.
    }
    \item{\code{verblevel}:}{Object of class \code{"integer"}
      controlling the amount of details to log: If set to \code{0},
      nothing will be written to the standard output connection.
      If set to \code{> 0}, all warnings are logged; if set do \code{> 1}, also
      messages are logged. Default: \code{0}.
    }
    \item{\code{lastStep}:}{Object of class \code{"list"}
      which is a stack, containing character strings describing performed steps.
      See also \code{\link{sybilStack}}.
    }
    \item{\code{lstname}:}{Object of class \code{"list"}
      giving the name of the stack in \code{lastStep}.
    }
    \item{\code{didFoot}:}{Object of class \code{"logical"}
      which is \code{FALSE}, if the footer of the log file is not yet printed,
      otherwise \code{TRUE}. This is useful if the function which is logged,
      stops unexpected.
    }
  }
}

\section{Methods}{
  \describe{
    \item{\code{didFoot}}{\code{signature(object = "sybilLog")}:
      gets the \code{didFoot} slot.
    }
    \item{\code{didFoot<-}}{\code{signature(object = "sybilLog")}:
      sets the \code{didFoot} slot.
    }
    \item{\code{fenc}}{\code{signature(object = "sybilLog")}:
      gets the \code{fenc} slot.
    }
    \item{\code{fenc<-}}{\code{signature(object = "sybilLog")}:
      sets the \code{fenc} slot.
    }
    \item{\code{fh}}{\code{signature(object = "sybilLog")}:
      gets the \code{fh} slot.
    }
    \item{\code{fh<-}}{\code{signature(object = "sybilLog")}:
      sets the \code{fh} slot.
    }
    \item{\code{fname}}{\code{signature(object = "sybilLog")}:
      gets the \code{fname} slot.
    }
    \item{\code{fname<-}}{\code{signature(object = "sybilLog")}:
      sets the \code{fname} slot.
    }
    \item{\code{fpath}}{\code{signature(object = "sybilLog")}:
      gets the \code{fpath} slot.
    }
    \item{\code{fpath<-}}{\code{signature(object = "sybilLog")}:
      sets the \code{fpath} slot.
    }
    \item{\code{loglevel}}{\code{signature(object = "sybilLog")}:
      gets the \code{loglevel} slot.
    }
    \item{\code{loglevel<-}}{\code{signature(object = "sybilLog")}:
      sets the \code{loglevel} slot.
    }
    \item{\code{lstname}}{\code{signature(object = "sybilLog")}:
      gets the \code{lstname} slot.
    }
    \item{\code{verblevel}}{\code{signature(object = "sybilLog")}:
      gets the \code{verblevel} slot.
    }
    \item{\code{verblevel<-}}{\code{signature(object = "sybilLog")}:
      sets the \code{verblevel} slot.
    }

    \item{\code{logCall}}{\code{signature(object = "sybilLog")} (\code{nog}):
      writes all arguments and values of the function call to be
      logged to the log file. Nothing is printed to the standard
      output; \code{verblevel} has no meaning here; \code{verblevel}
      must be \code{> 2}.
      \tabular{rr}{
        \code{nog} \tab number of generations to go back \cr
      }
    }
    \item{\code{logClose<-}}{\code{signature(object = "sybilLog")}:
      close the connection in slot \code{fh} and set it to \code{NA}.
      If slot \code{didFoot} is not \code{TRUE}, it prints a log
      comment to the connection in \code{fh} mentioning, that the
      logging ended unexpected.
    }
    \item{\code{logComment}}{\code{signature(object = "sybilLog")}
      (\code{cmt, commentChar}):
      add a comment to the log file if \code{loglevel} is \code{> 2} and to
      \code{stdout} if \code{verblevel} is \code{> 2}.
      \tabular{rr}{
        \code{cmt}     \tab the comment text \cr
        \code{cmtChar} \tab a string to prefix \code{cmt},
                            default: \code{# } \cr
      }
    }
    \item{\code{logError}}{\code{signature(object = "sybilLog")}
      (\code{msg, num}):
      add an error message to the log file. Returns an object of
      class \code{\linkS4class{sybilError}}.
      \tabular{rr}{
        \code{msg} \tab the error message \cr
        \code{num} \tab an error number \cr
      }
    }
    \item{\code{logFH}}{\code{signature(object = "sybilLog")}:
      Returns \code{TRUE}, if slot \code{fh} is of class \code{file}, otherwise
      \code{FALSE}.
    }
    \item{\code{logFoot<-}}{\code{signature(object = "sybilLog")}:
      Print a head for your log file.
    }
    \item{\code{logHead}}{\code{signature(object = "sybilLog")}:
      Print a foot for your log file.
    }
    \item{\code{logMessage}}{\code{signature(object = "sybilLog")}:
      add a message to the log file if \code{loglevel} is \code{> 1}.
      \tabular{rr}{
        \code{...} \tab strings pasted to the log file \cr
      }
    }
    \item{\code{logOptimization}}{\code{signature(object = "sybilLog")}:
      (\code{ok, stat, obj, del, i}):
      add a row containing results of an optimization to the log file if
      \code{loglevel} is \code{> 2} and to \code{stdout} if \code{verblevel} is
      \code{> 2}.
      \tabular{rr}{
        \code{opt no.}   \tab (integer) a consecutive number \cr
        \code{ret}       \tab (integer) return value of the used solver \cr
        \code{stat}      \tab (integer) solution status after optimization \cr
        \code{obj value} \tab (numeric) value of the objective function after
                                        optimization \cr
        \code{dir}       \tab if not given, it is a global value of the
                              algorithm (here empty), otherwise the current
                              setting of the direction of optimization \cr
        \code{obj c}     \tab if not given, it is a globel value of the model
                              (here empty), otherwise the current setting of the
                              objective coefficients of the variables given in
                              column \sQuote{flux no.} \cr
        \code{flux no.}  \tab fluxes (variables) which are considered in the
                              current optimization \cr
      }
    }
    \item{\code{logOptimizationTH}}{\code{signature(object = "sybilLog")}:
      add a row containing a table header for results of an optimization to the
      log file if \code{loglevel} is \code{> 2} and to \code{stdout} if
      \code{verblevel} is \code{> 2}. This should be used prior
      \code{logOptimization}.
    }
    \item{\code{logStep<-}}{\code{signature(object = "sybilLog")}:
      (\code{value}):
      add a status message to the log file if \code{loglevel} is \code{> 1},
      like \dQuote{performing step x}.
      \tabular{rr}{
        \code{value} \tab strings giving the status \cr
      }
      If \code{is.na(value)} evaluates to \code{TRUE}, the current process is
      assumed to have finished as expected. If \code{verblevel} is \code{> 1},
      \dQuote{\code{OK}} will be printed on the command line end if
      \code{loglevel} is \code{> 1}, \dQuote{\code{# done step x}} will be
      printed to the log file.
    }
    \item{\code{logWarning}}{\code{signature(object = "sybilLog")}:
      (\code{...}):
      add a warning to the log file if \code{loglevel} is \code{> 0}.
      \tabular{rr}{
        \code{...} \tab strings pastes to the log file \cr
      }
    }
  }
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\examples{
  showClass("sybilLog")
}

\keyword{classes}
