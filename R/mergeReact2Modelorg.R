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
		stop("reaction ids have to be unique.")
	}
	react_id(morg) <- reacts
	react_num(morg) <- length(reacts)
	
	met_id <- unique(unlist(lapply(reactList, met_id)))
	met_id(morg) <- met_id
	met_num(morg) <- length(met_id(morg))
	
	# collecting metabolite names and verifying identical assignments
	met_name <- character(length(met_id))
	names(met_name) <- met_id
	for(r in reactList){
		if(!all(met_name[met_id(r)] == "" | met_name[met_id(r)] == met_name(r))){
			stop("different metabolite names for same met_id")
		}
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
		ss <- setdiff(subSys(r), "")
		if(length(ss)> 0 ){
			j <- match(ss, subsUnique)
			subSys[i, j] <- T
		}
	}
	colnames(subSys) <- subsUnique
	subSys(morg) <- subSys
	
	gprRules(morg) <- sapply(reactList, gprRule)
	gpr(morg) <- sapply(reactList, gpr)
	genes(morg) <- lapply(reactList, genes)
	allGenes(morg) <- setdiff(unique(unlist(genes(morg))), "")
	
	
	# built react_attr frame:
	if(all(sapply(reactList, function(x) nrow(react_attr(x))==0))){
		react_attr(morg) <- data.frame()
	}else{
		reactAttrList <- lapply(reactList, function(x){
			df <- react_attr(x)
			df$react_id <- react_id(x)
			df
		})
		reactAttr <- Reduce(function(x, y) merge(x, y, by=intersect(colnames(x), colnames(y)), suffixes=c("", ""), all=TRUE), reactAttrList)
		reactAttr <- reactAttr[match(react_id(morg), reactAttr$react_id), ]
		reactAttr <- reactAttr[, setdiff(colnames(reactAttr), "react_id"), drop=F]
		react_attr(morg) <- reactAttr
	}
	
	# built met_attr frame:
	if(all(sapply(reactList, function(x) nrow(met_attr(x))==0))){
		met_attr(morg) <- data.frame()
	}else{
		metAttrList <- lapply(reactList, function(x){
			df <- met_attr(x)
			df$met_id <- met_id(x)
			df
		})
		metAttr <- Reduce(function(x, y) merge(x, y, by=intersect(colnames(x), colnames(y)), suffixes=c("", ""), all=TRUE), metAttrList)
		metAttr <- metAttr[match(met_id(morg), metAttr$met_id), ]
		metAttr <- metAttr[, setdiff(colnames(metAttr), "met_id"), drop=F]
		met_attr(morg) <- metAttr
	}
	
	
	# built comp_attr frame:
	if(all(sapply(reactList, function(x) nrow(comp_attr(x))==0))){
		comp_attr(morg) <- data.frame()
	}else{
		compAttrList <- lapply(reactList, function(x){
			df <- comp_attr(x)
			df
		})
		compAttr <- Reduce(function(x, y) merge(x, y, by=intersect(colnames(x), colnames(y)), suffixes=c("", ""), all=TRUE), compAttrList)
		compAttr <- compAttr[match(mod_compart(morg), compAttr$comp_id), ]
		compAttr <- compAttr[, setdiff(colnames(compAttr), "comp_id"), drop=F]
		comp_attr(morg) <- compAttr
	}
	
	mod_attr(morg)   <- data.frame()
	stopifnot(validObject(morg, "modelorg"))
	return(morg)
}






