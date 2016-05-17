\name{modelorg-class}

\docType{class}

\alias{modelorg-class}
\alias{modelorg}
\alias{allGenes<-,modelorg-method}
\alias{allGenes,modelorg-method}
\alias{allGenes<-}
\alias{allGenes}
\alias{dim,modelorg-method}
\alias{genes<-,modelorg-method}
\alias{genes,modelorg-method}
\alias{genes<-}
\alias{genes}
\alias{gpr<-,modelorg-method}
\alias{gpr,modelorg-method}
\alias{gpr<-}
\alias{gpr}
\alias{gprRules<-,modelorg-method}
\alias{gprRules,modelorg-method}
\alias{gprRules<-}
\alias{gprRules}
\alias{lowbnd<-,modelorg-method}
\alias{lowbnd,modelorg-method}
\alias{lowbnd<-}
\alias{lowbnd}
\alias{met_comp<-,modelorg-method}
\alias{met_comp,modelorg-method}
\alias{met_comp<-}
\alias{met_comp}
\alias{met_de<-,modelorg-method}
\alias{met_de,modelorg-method}
\alias{met_de<-}
\alias{met_de}
\alias{met_id<-,modelorg-method}
\alias{met_id,modelorg-method}
\alias{met_id<-}
\alias{met_id}
\alias{met_name<-,modelorg-method}
\alias{met_name,modelorg-method}
\alias{met_name<-}
\alias{met_name}
\alias{met_num<-,modelorg-method}
\alias{met_num,modelorg-method}
\alias{met_num<-}
\alias{met_num}
\alias{met_single<-,modelorg-method}
\alias{met_single,modelorg-method}
\alias{met_single<-}
\alias{met_single}
\alias{mod_compart<-,modelorg-method}
\alias{mod_compart,modelorg-method}
\alias{mod_compart<-}
\alias{mod_compart}
\alias{mod_desc<-,modelorg-method}
\alias{mod_desc,modelorg-method}
\alias{mod_desc<-}
\alias{mod_desc}
\alias{mod_id<-,modelorg-method}
\alias{mod_id,modelorg-method}
\alias{mod_id<-}
\alias{mod_id}
\alias{mod_key<-,modelorg-method}
\alias{mod_key,modelorg-method}
\alias{mod_key<-}
\alias{mod_key}
\alias{mod_name<-,modelorg-method}
\alias{mod_name,modelorg-method}
\alias{mod_name<-}
\alias{mod_name}
\alias{obj_coef<-,modelorg-method}
\alias{obj_coef,modelorg-method}
\alias{obj_coef<-}
\alias{obj_coef}
\alias{printObjFunc,modelorg-method}
\alias{printObjFunc}
\alias{react_de<-,modelorg-method}
\alias{react_de,modelorg-method}
\alias{react_de<-}
\alias{react_de}
\alias{react_id<-,modelorg-method}
\alias{react_id,modelorg-method}
\alias{react_id<-}
\alias{react_id}
\alias{react_name<-,modelorg-method}
\alias{react_name,modelorg-method}
\alias{react_name<-}
\alias{react_name}
\alias{react_num<-,modelorg-method}
\alias{react_num,modelorg-method}
\alias{react_num<-}
\alias{react_num}
\alias{react_rev<-,modelorg-method}
\alias{react_rev,modelorg-method}
\alias{react_rev<-}
\alias{react_rev}
\alias{react_single<-,modelorg-method}
\alias{react_single,modelorg-method}
\alias{react_single<-}
\alias{react_single}
\alias{rxnGeneMat<-,modelorg-method}
\alias{rxnGeneMat,modelorg-method}
\alias{rxnGeneMat<-}
\alias{rxnGeneMat}
\alias{show,modelorg-method}
\alias{S<-,modelorg-method}
\alias{S,modelorg-method}
\alias{S<-}
\alias{S}
\alias{Snnz,modelorg-method}
\alias{Snnz}
\alias{subSys<-,modelorg-method}
\alias{subSys,modelorg-method}
\alias{subSys<-}
\alias{subSys}
\alias{uppbnd<-,modelorg-method}
\alias{uppbnd,modelorg-method}
\alias{uppbnd<-}
\alias{uppbnd}
\alias{version<-,modelorg-method}
\alias{version,modelorg-method}
\alias{version<-}
\alias{version}

\encoding{utf8}

\title{Structure of Class "modelorg"}

\description{
  Structure of the class \code{"modelorg"}. Objects of that class are
  returned by functions like \code{\link{readTSVmod}}.
}

\section{Objects from the Class}{
  Objects can be created by calls of the function \code{modelorg}:

  \code{test <- modelorg(id = "foo", name = "bar",
                         subSys = NULL, compartment = NULL)}.
  \describe{
    \item{\code{id}:}{
      a single character string giving the model id.
    }
    \item{\code{name}:}{
      a single character string giving the model name.
    }
    \item{\code{subSys}:}{
      an optional single character string giving the metabolic subsystems
      of the model. Default: \code{NULL}
    }
    \item{\code{compartment}:}{
      an optional single character string giving the compartments of the
      model. Default: \code{NULL}
    }
  }
  This constructor also generates the model key used in slot \code{mod_key}.
}

\section{Slots}{
  \describe{
    \item{\code{mod_desc}:}{
      Object of class \code{"character"} containing a description of the model.
    }
    \item{\code{mod_name}:}{
      Object of class \code{"character"} indicating the model name.
    }
    \item{\code{version}:}{
      Object of class \code{"character"} indicating the model version.
    }
    \item{\code{mod_id}:}{
      Object of class \code{"character"} indicating the model id.
    }
    \item{\code{mod_key}:}{
      Object of class \code{"character"} containing a single character string
      functioning as a unique key to a model object.
    }
    \item{\code{mod_compart}:}{
      Object of class \code{"character"} containing the model compartments.
    }
    \item{\code{met_num}:}{
      Object of class \code{"integer"} indicating the number of metabolites.
    }
    \item{\code{met_id}:}{
      Object of class \code{"character"} containing the metabolite id's.
    }
    \item{\code{met_name}:}{
      Object of class \code{"character"} containing the metabolite names.
    }
    \item{\code{met_comp}:}{
      Object of class \code{"integer"} containing the metabolites compartment.
    }
    \item{\code{met_single}:}{
      Object of class \code{"logical"} with length \code{met_num}. Element
      \code{i} is \code{TRUE}, if metabolite \code{i} appears only once in S.
    }
    \item{\code{met_de}:}{
      Object of class \code{"logical"} with length \code{met_num}. Element
      \code{i} is \code{TRUE}, if metabolite \code{i} is a dead end metabolite.
    }
    \item{\code{react_num}:}{
      Object of class \code{"integer"} indicating the number of reactions.
    }
    \item{\code{react_rev}:}{
      Object of class \code{"logical"} indicating whether a reaction is
      reversible or not.
    }
    \item{\code{react_id}:}{
      Object of class \code{"character"} containing the reaction id's.
    }
    \item{\code{react_name}:}{
      Object of class \code{"character"} containing the reaction names.
    }
    \item{\code{react_single}:}{
      Object of class \code{"logical"} with length \code{react_num}. Element
      \code{i} is \code{TRUE}, if reaction \code{i} uses metabolites appearing
      only once in S.
    }
    \item{\code{react_de}:}{
      Object of class \code{"logical"} with length \code{react_num}. Element
      \code{i} is \code{TRUE}, if reaction \code{i} uses dead end metabolites.
    }
    \item{\code{S}:}{
      Object of class \code{"matrix"} containing the stoichiometric matrix.
    }
    \item{\code{lowbnd}:}{
      Object of class \code{"numeric"} containing the reactions lower bounds.
    }
    \item{\code{uppbnd}:}{
      Object of class \code{"numeric"} containing the reactions upper bounds.
    }
    \item{\code{obj_coef}:}{
      Object of class \code{"numeric"} containing the objective coefficients.
    }
    \item{\code{gprRules}:}{
      Object of class \code{"character"} containing the gene-reaction
      association rules in computable form.
    }
    \item{\code{genes}:}{
      Object of class \code{"list"} containing the genes corresponding to each
      reaction. Every list element is a vector of the type \code{character}.
    }
    \item{\code{gpr}:}{
      Object of class \code{"character"} containing the gene-reaction
      association rules for each reaction.
    }
    \item{\code{allGenes}:}{
      Object of class \code{"character"} containing a unique list of all genes.
    }
    \item{\code{rxnGeneMat}:}{
      Object of class \code{"matrix"} containing a reaction to gene mapping.
    }
    \item{\code{subSys}:}{
      Object of class \code{"matrix"} giving one or more subsystem name for each
      reaction.
    }
  }
}

\section{Methods}{
  \describe{
    \item{\code{allGenes<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{allGenes} slot.
    }
    \item{\code{allGenes}:}{
      \code{signature(object = "modelorg")}: gets the \code{allGenes} slot.
    }
    \item{\code{dim}:}{
      \code{signature(object = "modelorg")}: gets the \code{dimension} attribute
      of slot \code{S}.
    }
    \item{\code{genes<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{genes} slot.
    }
    \item{\code{genes}:}{
      \code{signature(object = "modelorg")}: gets the \code{genes} slot.
    }
    \item{\code{gpr<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{gpr} slot.
    }
    \item{\code{gpr}:}{
      \code{signature(object = "modelorg")}: gets the \code{gpr} slot.
    }
    \item{\code{gprRules<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{gprRules} slot.
    }
    \item{\code{gprRules}:}{
      \code{signature(object = "modelorg")}:  gets the \code{gprRules} slot.
    }
    \item{\code{lowbnd<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{lowbnd} slot.
    }
    \item{\code{lowbnd}:}{
      \code{signature(object = "modelorg")}: gets the \code{lowbnd} slot.
    }
    \item{\code{met_comp<-}:}{
      \code{signature(object = "modelorg")}:  sets the \code{met_comp} slot.
    }
    \item{\code{met_comp}:}{
      \code{signature(object = "modelorg")}:  gets the \code{met_comp} slot.
    }
    \item{\code{met_de<-}:}{
      \code{signature(object = "modelorg")}:  sets the \code{met_de} slot.
    }
    \item{\code{met_de}:}{
      \code{signature(object = "modelorg")}:  gets the \code{met_de} slot.
    }
    \item{\code{met_id<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{met_id} slot.
    }
    \item{\code{met_id}:}{
      \code{signature(object = "modelorg")}: gets the \code{met_id} slot.
    }
    \item{\code{met_name<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{met_name} slot.
    }
    \item{\code{met_name}:}{
      \code{signature(object = "modelorg")}: gets the \code{met_name} slot.
    }
    \item{\code{met_num<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{met_num} slot.
    }
    \item{\code{met_num}:}{
      \code{signature(object = "modelorg")}: gets the \code{met_num} slot.
    }
    \item{\code{met_single<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{met_single} slot.
    }
    \item{\code{met_single}:}{
      \code{signature(object = "modelorg")}: gets the \code{met_single} slot.
    }
    \item{\code{mod_compart<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{mod_compart} slot.
    }
    \item{\code{mod_compart}:}{
      \code{signature(object = "modelorg")}: gets the \code{mod_compart} slot.
    }
    \item{\code{mod_desc<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{mod_desc} slot.
    }
    \item{\code{mod_desc}:}{
      \code{signature(object = "modelorg")}: gets the \code{mod_desc} slot.
    }
    \item{\code{mod_id<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{mod_id} slot.
    }
    \item{\code{mod_id}:}{
      \code{signature(object = "modelorg")}: gets the \code{mod_id} slot.
    }
    \item{\code{mod_key<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{mod_key} slot.
    }
    \item{\code{mod_key}:}{
      \code{signature(object = "modelorg")}: gets the \code{mod_key} slot.
    }
    \item{\code{mod_name<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{mod_name} slot.
    }
    \item{\code{mod_name}:}{
      \code{signature(object = "modelorg")}: gets the \code{mod_name} slot.
    }
    \item{\code{obj_coef<-}:}{
      \code{signature(object = "modelorg")}:  sets the \code{obj_coef} slot.
    }
    \item{\code{obj_coef}:}{
      \code{signature(object = "modelorg")}: gets the \code{obj_coef} slot.
    }
    \item{\code{printObjFunc}:}{
      \code{signature(object = "modelorg")}: prints the objective function in
      a human readable way.
    }
    \item{\code{react_de<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{react_de} slot.
    }
    \item{\code{react_de}:}{
      \code{signature(object = "modelorg")}: gets the \code{react_de} slot.
    }
    \item{\code{react_id<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{react_id} slot.
    }
    \item{\code{react_id}:}{
      \code{signature(object = "modelorg")}: gets the \code{react_id} slot.
    }
    \item{\code{react_name<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{react_name} slot.
    }
    \item{\code{react_name}:}{
      \code{signature(object = "modelorg")}: gets the \code{react_name} slot.
    }
    \item{\code{react_num<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{react_num} slot.
    }
    \item{\code{react_num}:}{
      \code{signature(object = "modelorg")}: gets the \code{react_num} slot.
    }
    \item{\code{react_rev<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{react_rev} slot.
    }
    \item{\code{react_rev}:}{
      \code{signature(object = "modelorg")}: gets the \code{react_rev} slot.
    }
    \item{\code{react_single<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{react_single} slot.
    }
    \item{\code{react_single}:}{
      \code{signature(object = "modelorg")}: gets the \code{react_single} slot.
    }
    \item{\code{rxnGeneMat<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{rxnGeneMat} slot.
    }
    \item{\code{rxnGeneMat}:}{
      \code{signature(object = "modelorg")}: gets the \code{rxnGeneMat} slot.
    }
    \item{\code{show}:}{
      \code{signature(object = "modelorg")}: prints some details specific to the
      instance of class \code{modelorg}.
    }
    \item{\code{Snnz}:}{
      \code{signature(object = "modelorg")}: prints the number of non-zero
      elements in \code{S}.
    }
    \item{\code{S<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{S} slot as matrix,
      see Details below.
    }
    \item{\code{S}:}{
      \code{signature(object = "modelorg")}: gets the \code{S} slot as matrix.
    }
    \item{\code{subSys<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{subSys} slot.
    }
    \item{\code{subSys}:}{
      \code{signature(object = "modelorg")}: gets the \code{subSys} slot.
    }
    \item{\code{uppbnd<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{uppnds} slot.
    }
    \item{\code{uppbnd}:}{
      \code{signature(object = "modelorg")}: gets the \code{uppbnd} slot.
    }
    \item{\code{version<-}:}{
      \code{signature(object = "modelorg")}: sets the \code{version} slot.
    }
    \item{\code{version}:}{
      \code{signature(object = "modelorg")}: gets the \code{version} slot.
    }
  }
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{
  \code{\linkS4class{modelorg_irrev}} for models in irreversible format.
}

\examples{
  showClass("modelorg")

  ## print human readable version of the objective function
  data(Ec_core)
  printObjFunc(Ec_core)

  ## change objective function and print
  Ec_objf <- changeObjFunc(Ec_core, c("EX_etoh(e)", "ETOHt2r"), c(1, 2))
  printObjFunc(Ec_objf)
}

\keyword{classes}



