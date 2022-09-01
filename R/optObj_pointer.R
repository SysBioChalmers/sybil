#             class definitions for pointers to problem objects                
setClass(Class = "lpExtPtr", contains = "VIRTUAL")     # lpSolveAPI
setClass(Class = "glpkPtr",  contains = "VIRTUAL")     # glpkAPI
setClass(Class = "clpPtr",   contains = "VIRTUAL")     # clpAPI
setClass(Class = "cplexPtr", contains = "VIRTUAL")     # cplexAPI

#                     definition of the class cplexPointer                     
setClass(Class = "cplexPointer",
         representation(
             env = "cplexPtr",
             lp  = "cplexPtr"
         ),
)

#                            default constructor                               
# contructor for class cplexPointer
setMethod(f = "initialize",
          signature = "cplexPointer",
          definition = function(.Object, en, pr) {

              if ( (!missing(en)) || (!missing(pr)) ) {
                  if ( (cplexAPI::isCPLEXenvPointer(en)) &&
                       (cplexAPI::isCPLEXprobPointer(pr)) ) {
                  
                      .Object@env <- en
                      .Object@lp  <- pr

                  }
              }
              return(.Object)
          }
)

#                    definition of the class pointerToProb                     
# pointer representation in class optObj
setClassUnion(name    = "pointerToProb",
              members = c("externalptr",
                          "lpExtPtr",
                          "glpkPtr",
                          "clpPtr",
                          "cplexPointer"
             )
)
