modelorg2text <- function(model, prefix, suffix, extMetFlag = "b",
                         fielddelim = "\t", genedelim = "/",
                         makeClosedNetwork = FALSE,
                         fpath = SYBIL_SETTINGS("PATH_TO_MODEL"),
                         ...) {

    if (!is(model, "modelorg")) {
        stop("needs an object of class modelorg!")
    }

    # filenames
    if (missing(prefix)) {
        prefix <- gsub("\\s+", "_", mod_id(model))
    }
    
    if (missing(suffix)) {
        suffix <- switch(fielddelim,
            "\t" = { "tsv" },
            ";"  = { "csv" },
            ","  = { "csv" },
            "|"  = { "dsv" },
                   { "dsv" }
        )
    }
    
    fname <- paste(prefix, suffix, sep = ".")
    # path to output file
    textfile <- file.path(fpath, fname)

    prepareRuleStrings <- function(rule, bp, no) {
    }

    # reactions list
    rstr <- .createReactionString(model, makeClosedNetwork)

    # rule    abbreviation    equation    lowbnd    uppbnd    obj_coef
    
    gprFRule <- gsub("&&?|and",    "AND", gpr(model), perl = TRUE)
    gprFRule <- gsub("\\|\\|?|or", "OR",  gprFRule,   perl = TRUE)
    
    gpr_list <- strsplit(gprFRule, "", fixed = TRUE)
    
    check_bp <- mapply(.check_brackets, gpr_list, SIMPLIFY = TRUE)
   
    if ( sum(check_bp) != length(check_bp) ) {
       warning(paste("Wrong gpr rules detected, setting to \"\". ",
                     "Check rule(s) no. ",
                     paste((1:react_num(model))[!check_bp], collapse = ", "),
                     ".", sep = ""))
       gpr_list[!check_bp] <- ""
    }
    
    gpr_bp  <- mapply(.bracket_pairs, gpr_list, SIMPLIFY = FALSE)
    
    gpr_str <- mapply(prepareRuleStrings,
                      gprFRule, gpr_bp, c(1:react_num(model)),
                      SIMPLIFY = FALSE)
    
#    write.table(x = data.frame(
#                    equation = rstr$equat,
#                    lowbnd       = lowbnd(model),
#                    uppbnd       = uppbnd(model),
#                    obj_coef     = obj_coef(model),
#                    ),
#        row.names = FALSE, file = tsvfileR, sep = fielddelim, ...)
    return(TRUE)
}
