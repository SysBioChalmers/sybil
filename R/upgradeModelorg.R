#  upgradeModelorg.R
#  FBA and friends with R.
#
#  Copyright (C) 2010-2016 Claus Jonathan Fritzemeier, Dpt. for Bioinformatics,
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
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with sybil.  If not, see <http://www.gnu.org/licenses/>.


################################################
# Function: upgradeModelorg
#
# Takes an instance of a old modelorg version and returns one updated to the
# current version.
#


upgradeModelorg <- function(object){
	stopifnot(is(object, "modelorg"))
	
	if(!.hasSlot(object, "version") || compareVersion(version(object), "2.0") == -1){
		# object is from a time before versions were introduced.
		
		# just add version slot
		object@version <- "2.0"
		
		# update gprRules to new format
		rules <- lapply(gpr(object), .parseBoolean)
		if(length(rules) == 0){
			genes(object) <- list()
			gprRules(object) <- character(0)
		}else{
			genes(object) <- lapply(rules, "[[", "gene")
			gprRules(object) <- sapply(rules, "[[", "rule")
		}
		
		
		# set attribute slots
		react_attr(object) <- data.frame()
		comp_attr(object) <- data.frame()
		met_attr(object) <- data.frame()
		mod_attr(object) <- data.frame()
		
		#recursively upgrade to latest version.
		return(upgradeModelorg(object))
	}
	
	if(compareVersion(version(object), SYBIL_SETTINGS("MODELORG_VERSION")) == 0){
		stopifnot(validObject(object))
		return(object)
	}
	
	stop("unsupported version of modelorg")
}















