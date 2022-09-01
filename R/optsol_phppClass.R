#                   definition of the class optsol_phpp                        
setClass("optsol_phpp",
         representation(
              ctrlflm  = "matrix",   # fixed flux values of control reactions
              redCosts = "matrix"    # reduced costs
         ),
         contains = "optsol_robAna"
)

#                            setters and getters                               
# ctrlfl
setMethod("ctrlfl", signature(object = "optsol_phpp"),
          function(object) {
              return(object@ctrlflm)
          }
)

setReplaceMethod("ctrlfl", signature(object = "optsol_phpp"),
                 function(object, value) {
                     object@ctrlflm <- value
                     return(object)
                 }
)

# redCost
setMethod("getRedCosts", signature(lp = "optsol_phpp"),
          function(lp) {
              return(lp@redCosts)
          }
)

#                               other methods                                  
setMethod("plot", signature(x = "optsol_phpp", y = "missing"),
          function(x, y,
                   xlab = list(label = react_id(ctrlr(x)[1]),
                               rot = 30,
                               cex = 0.8),
                   ylab = list(label = react_id(ctrlr(x)[2]),
                               rot = -40,
                               cex = 0.8),
                   zlab = list(label = obj_func(x),
                               rot = 90,
                               cex = 0.8),
                   scales = list(arrows = FALSE,
                                 cex = 0.6,
                                 font = 3,
                                 tck = 1,
                                 col = "black"),
                   par.settings = list(axis.line = list(col = "transparent")),
                   shade = TRUE,
                   shade.colors = function(irr, ref, height, w = 0.75) {
                             grey(w * irr + (1 - w) * (1-(1-ref)^0.75)) },
                   ...) {

              pic <- wireframe(lp_obj(x) ~ ctrlfl(x)[,1] * ctrlfl(x)[,2],
                               scales = scales,
                               shade = shade,
                               par.settings = par.settings,
                               shade.colors = shade.colors,
                               xlab = xlab, ylab = ylab, zlab = zlab, ...)
              return(pic)

          }
)


# value for col.regions are the Greys from RColorBrewer:
# brewer.pal(9, "Greys")
# c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696", "#737373",
#   "#525252", "#252525", "#000000")

setMethod("plot", signature(x = "optsol_phpp", y = "character"),
          function(x, y, #rcbp = "Greys",
                   main = paste("Reduced Costs:", y),
                   xlab = react_id(ctrlr(x)[1]),
                   ylab = react_id(ctrlr(x)[2]),
                   shrink = c(0.95, 0.95),
                   #col.regions = colorRampPalette(brewer.pal(brewer.pal.info[rcbp, ][["maxcolors"]], rcbp))(100),
                   col.regions = colorRampPalette(c("#FFFFFF", "#F0F0F0",
                                    "#D9D9D9", "#BDBDBD", "#969696", "#737373",
                                    "#525252", "#252525", "#000000"))(100),
                   ...) {

              if (any(is.na(getRedCosts(x)))) {
                  warning("solution object does not contain reduced costs")
                  pic <- NA
              }
              else {
                  pic <- levelplot(
                      getRedCosts(x)[ ,y] ~ ctrlfl(x)[,1] * ctrlfl(x)[,2],
                      xlab = xlab,
                      ylab = ylab,
                      main = main,
                      col.regions = col.regions,
                      shrink = shrink, ...)
              }
              return(pic)
          }
)
