# Validity checking of an object of class react.
# Returns TRUE if the model is valid, otherwise
# a character String containing a description of
# the error.
.validreact <- function(object) {
	# data has to have the same length or to be NULL
	if(!is(object, "react")){
		"object is not of react class"
	}
	if(length(object@id) != 1){
		return("id has to be length 1")
	}
	if(length(object@name) != 1){
		return("id has to be length 1")
	}
	met_count <- length(object@s)
	if(met_comp < 1){
		return("reactions have to have at least one metabolite")
	}
	if(length(object@met_id) == met_count){
		return("s, met_id, met_name, and met_comp have to have the same length")
	}
	if(length(object@met_name) == met_count && !is.null(object@met_name)){
		return("s, met_id, met_name, and met_comp have to have the same length")
	}
	if(length(object@met_comp) == met_count && !is.null(object@met_comp)){
		return("s, met_id, met_name, and met_comp have to have the same length")
	}
	if(length(object@id)!=1){
		return("length of id has to be 1")
	}
	if(length(object@rev)!=1){
		return("length of rev has to be 1")
	}
	if(length(object@name)!=1){
		return("length of name has to be 1")
	}
	if(length(object@lowbnd)!=1){
		return("length of lowbnd has to be 1")
	}
	if(length(object@uppbnd)!=1){
		return("length of uppbnd has to be 1")
	}
	if(length(object@obj_coef)!=1){
		return("length of obj_coef has to be 1")
	}
	if(!is.null(gprRule) && length(object@gprRule)!=1){
		return("if not NULL, the length of gprRule has to be 1")
	}
	if(!is.null(genes) && length(object@genes)!=1){
		return("if not NULL, the length of genes has to be 1")
	}
	if(!is.null(gpr) && length(object@gpr)!=1){
		return("if not NULL, the length of gpr has to be 1")
	}
	if(!is.null(subSys) && length(object@subSys)!=1){
		return("if not NULL, the length of subSys has to be 1")
	}
	return(TRUE)
}
