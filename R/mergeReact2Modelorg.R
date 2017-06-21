#  mergeReact2Modelorg.R
#  FBA and friends with R.
#
#  Copyright (C) 2010-2017 Claus Jonathan Fritzemeier, Dpt. for Computational Cell Biology,
#  Institute for Informatics, Heinrich-Heine-University, Duesseldorf, Germany.
#  All right reserved.
#  Email: clausjonathan.fritzemeier@uni-duesseldorf.de
#
#  This file is part of sybil.
#
#  Sybil is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  Sybil is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with sybil.  If not, see <http://www.gnu.org/licenses/>.


# mergeReact2Modelorg


################################################
# Function: mergeReact2Modelorg
#
# Merge react objects into modelorg object
#
# Takes a list of react class instances and returns a modelorg object by 
# merging the reactions into one model.


#------------------------------------------------------------------------------#
#               definition of the class mergeReact2Modelorg                    #
#------------------------------------------------------------------------------#


mergeReact2Modelorg <- function(reactList = NULL, id="newModel", name=""){
	
	stopifnot(!is.null(reactList))
	
	morg <- new("modelorg", id=id, name=name)
	
	reacts <- sapply(reactList, react_id)
	if(anyDuplicated(reacts)){
		stop("reaction ids have to be unique")
	}
	react_id(morg) <- reacts
	
	met_id <- unique(unlist(sapply(reactList, met_id)))
	met_id(morg) <- met_id
	
	# collecting metabolite names and verifying identical assignments
	met_name <- character(length(met_id))
	names(met_name) <- met_id
	for(r in reactList){
		stopifnot(all(met_name[met_id(r)] == "" | met_name[met_id(r)] == met_name(r)))
		met_name[met_id(r)] <- met_name(r)
	}
	met_name(morg) <- met_name
	
	# collecting metabolite compartments and verifying identical assignments
	met_comp <- character(length(met_id))
	names(met_comp) <- met_id
	for(r in reactList){
		stopifnot(all(met_comp[met_id(r)] == "" | met_comp[met_id(r)] == met_comp(r)))
		met_comp[met_id(r)] <- met_comp(r)
	}
	mod_compart(morg) <- unique(met_comp)
	met_comp(morg) <- match(met_comp, mod_compart(morg))
	
	S <- Matrix(0, nrow=length(met_id), ncol=length(reacts))
	for(j in seq(along=reactList)){
		r <- reactList[[j]]
		i <- match(met_id(r), met_id)
		S[i, j] <- s(r)
	}
	S(morg) <- S
	
	subs <- lapply(reactList, subSys)
	subsUnique <- unique(unlist(subs))
	
	subSys <- Matrix(F, ncol=length(subsUnique), nrow=length(reacts))
	for(i in seq(along=reactList)){
		if(length(subSys(r))> 0 ){
			j <- match(subSys(r), subsUnique)
			S[i, j] <- T
		}
	}
	subSys(morg) <- subSys
	
	gprRules(morg) <- sapply(reactList, gprRule)
	gpr(morg) <- sapply(reactList, gpr)
	genes(morg) <- lapply(reactList, genes)
	allGenes(morg) <- unqiue(unlist(genes(morg)))
	
	stopifnot(validObject(morg, "modelorg"))
	return(morg)
}






