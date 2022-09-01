checkReactId <- function(model, react) {

  if (is(react, "reactId")) {
      return(react)
  }

  if (!is(model, "modelorg")) {
    stop("needs an object of class modelorg!")
  }

  checkedIds <- NULL

 
# If react is numeric (or integer), we only need to check, whether no element of
# react is larger than the number of reactions and if all elements are positive.
  if (is.numeric(react) || is.integer(react)) {

      if ( (max(react) > react_num(model)) || (min(react) < 1) ) {
      #if ( (max(react) > react_num(model)) || (!all(react > 0)) ) {
          warning("reaction numbers must be in between 1 and ", react_num(model))
      }
      else {
          checkedIds <- new("reactId",
                            mod_id  = mod_id(model),
                            mod_key = mod_key(model),
                            pnt     = react,
                            id      = react_id(model)[react])
      }
  }

#                          if "react" is character                             #
  if (is(react, "character")) {
      reaction_ids <- react_id(model)
    
      # find the vector indices in the model of react
      pos <- match(react, reaction_ids, nomatch = 0)
      if (all(pos != 0)) {
          checkedIds <- new("reactId",
                            mod_id  = mod_id(model),
                            mod_key = mod_key(model),
                            pnt     = pos,
                            id      = react)
          #return(checkedIds)
          #return(list(id = react, pos = pos))
      }
    
      # if we cannot find some entries in react, we try grep
      else {
          #print(pos)
          # only those, we could not find above
          pos_null <- which(pos == 0)
    
          # the grep
          react_null <- sapply(pos_null, function(x) grep(react[x], reaction_ids, ignore.case = TRUE, value = TRUE))
          
          #react  <- sapply(react, function(x) grep(x, Dmodel@react_id, ignore.case = TRUE, value = TRUE))
    
          # check whether all results are unique
          #print(is(react_null))
          #print(react_null)
          len    <- sapply(react_null, length)
          n_uniq <- which(len != 1)
    
          #print(len)
    
          if ((length(n_uniq) != 0) || (is(react_null, "matrix"))) {
              warning("some reaction id's were not found or are ambiguous", call. = FALSE)
              #print(n_uniq)
              #return(FALSE)
              #return(NA)
          }
          else {
              pos_react <- match(react_null, reaction_ids)
              #print(pos_null)
              #print(pos_react)
              #print(react_null)
        
              pos[pos_null]   <- pos_react
              react[pos_null] <- react_null
              #print(pos)
              #print(react)
              #pos <- match(react, model@react_id)
        
              checkedIds <- new("reactId",
                                mod_id  = mod_id(model),
                                mod_key = mod_key(model),
                                pnt     = pos,
                                id      = react)
              #return(checkedIds)
              #return(list(id = react, pos = pos))
          }
      }
  }
  
  return(checkedIds)                            
}
