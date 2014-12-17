### R code from vignette source 'sybil.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: sybil.Rnw:433-434
###################################################
library(sybil)


###################################################
### code chunk number 2: sybil.Rnw:443-444
###################################################
library(help = "sybil")


###################################################
### code chunk number 3: sybil.Rnw:448-449
###################################################
help(doubleGeneDel)


###################################################
### code chunk number 4: sybil.Rnw:453-454
###################################################
help.search("flux variability analysis")


###################################################
### code chunk number 5: sybil.Rnw:457-458 (eval = FALSE)
###################################################
## vignette("sybil")


###################################################
### code chunk number 6: sybil.Rnw:479-480
###################################################
mp  <- system.file(package = "sybil", "extdata")


###################################################
### code chunk number 7: sybil.Rnw:483-484
###################################################
mod <- readTSVmod(prefix = "Ec_core", fpath = mp, quoteChar = "\"")


###################################################
### code chunk number 8: sybil.Rnw:504-505 (eval = FALSE)
###################################################
## modelorg2tsv(mod, prefix = "Ec_core")


###################################################
### code chunk number 9: sybil.Rnw:510-511
###################################################
data(Ec_core)


###################################################
### code chunk number 10: sybil.Rnw:524-525
###################################################
ex <- findExchReact(Ec_core)


###################################################
### code chunk number 11: sybil.Rnw:528-530
###################################################
upt <- uptReact(ex)
ex[upt]


###################################################
### code chunk number 12: sybil.Rnw:536-538
###################################################
mod <- changeBounds(Ec_core, ex[c("EX_glc(e)", "EX_lac_D(e)")], lb = c(0, -10))
findExchReact(mod)


###################################################
### code chunk number 13: sybil.Rnw:557-558
###################################################
optL <- optimizeProb(Ec_core, algorithm = "fba", retOptSol = FALSE)


###################################################
### code chunk number 14: sybil.Rnw:563-564
###################################################
opt <- optimizeProb(Ec_core, algorithm = "fba", retOptSol = TRUE)


###################################################
### code chunk number 15: sybil.Rnw:570-571
###################################################
lp_obj(opt)


###################################################
### code chunk number 16: sybil.Rnw:575-576
###################################################
checkOptSol(opt)


###################################################
### code chunk number 17: sybil.Rnw:590-591
###################################################
fba <- optimizeProb(Ec_core, algorithm = "fba")


###################################################
### code chunk number 18: sybil.Rnw:594-595
###################################################
mod_obj(fba)


###################################################
### code chunk number 19: sybil.Rnw:599-600
###################################################
mtf <- optimizeProb(Ec_core, algorithm = "mtf", wtobj = mod_obj(fba))


###################################################
### code chunk number 20: sybil.Rnw:603-604
###################################################
lp_obj(mtf)


###################################################
### code chunk number 21: sybil.Rnw:609-611
###################################################
nvar(fluxdist(fba))
nvar(fluxdist(mtf))


###################################################
### code chunk number 22: sybil.Rnw:616-618
###################################################
help("sysBiolAlg_fba-class")
help("sysBiolAlg_mtf-class")


###################################################
### code chunk number 23: sybil.Rnw:621-623
###################################################
?fba
?mtf


###################################################
### code chunk number 24: sybil.Rnw:628-630
###################################################
fl <- getFluxDist(mtf)
length(fl)


###################################################
### code chunk number 25: sybil.Rnw:635-637
###################################################
fd <- getFluxDist(mtf, ex)
getNetFlux(fd)


###################################################
### code chunk number 26: sybil.Rnw:642-643
###################################################
mod_obj(mtf)


###################################################
### code chunk number 27: sybil.Rnw:654-655
###################################################
ko <- optimizeProb(Ec_core, gene = "b2276", lb = 0, ub = 0)


###################################################
### code chunk number 28: sybil.Rnw:676-678
###################################################
ko <- optimizeProb(Ec_core, gene = "b2276", lb = 0, ub = 0,
                   algorithm = "lmoma", wtflux = getFluxDist(mtf))


###################################################
### code chunk number 29: sybil.Rnw:694-697 (eval = FALSE)
###################################################
## ko <- optimizeProb(Ec_core, gene = "b2276", lb = 0, ub = 0,
##                    algorithm = "room", wtflux = getFluxDist(mtf),
##                    solverParm = list(PRESOLVE = GLP_ON))


###################################################
### code chunk number 30: sybil.Rnw:729-730
###################################################
opt <- oneGeneDel(Ec_core)


###################################################
### code chunk number 31: sybil.Rnw:744-745
###################################################
checkOptSol(opt)


###################################################
### code chunk number 32: sybil.Rnw:749-750
###################################################
plot(opt, nint = 20)


###################################################
### code chunk number 33: sybil.Rnw:756-759
###################################################
opt <- oneGeneDel(Ec_core, algorithm = "lmoma", wtflux = getFluxDist(mtf))
checkOptSol(opt)
plot(opt, nint = 20)


###################################################
### code chunk number 34: sybil.Rnw:765-766
###################################################
opt <- geneDeletion(Ec_core)


###################################################
### code chunk number 35: sybil.Rnw:769-771 (eval = FALSE)
###################################################
## opt2 <- geneDeletion(Ec_core, combinations = 2)
## opt3 <- geneDeletion(Ec_core, combinations = 3)


###################################################
### code chunk number 36: sybil.Rnw:787-789
###################################################
opt <- fluxVar(Ec_core, percentage = 80, verboseMode = 0)
plot(opt)


###################################################
### code chunk number 37: sybil.Rnw:817-819
###################################################
opt <- robAna(Ec_core, ctrlreact = "EX_o2(e)", verboseMode = 0)
plot(opt)


###################################################
### code chunk number 38: sybil.Rnw:836-843
###################################################
Ec_core_wo_glc <- changeUptake(Ec_core, off = "glc_D[e]")
opt <- phpp(Ec_core_wo_glc,
            ctrlreact = c("EX_succ(e)", "EX_o2(e)"),
            redCosts = TRUE,
            numP = 25,
            verboseMode = 0)
plot(opt)


###################################################
### code chunk number 39: phpp_rf
###################################################
plot(opt, "EX_succ(e)")


###################################################
### code chunk number 40: phpp_rs
###################################################
plot(opt, "EX_o2(e)")


###################################################
### code chunk number 41: sybil.Rnw:877-878
###################################################
opt <- oneGeneDel(Ec_core, algorithm = "fba", fld = "all")


###################################################
### code chunk number 42: sybil.Rnw:881-882
###################################################
sum <- summaryOptsol(opt, Ec_core)


###################################################
### code chunk number 43: sybil.Rnw:896-897
###################################################
printExchange(sum, j = c(1:50), dense = TRUE)


###################################################
### code chunk number 44: sybil.Rnw:912-916
###################################################
ref    <- optimizeProb(Ec_core)
opt    <- oneGeneDel(Ec_core)
let    <- lethal(opt, wt = mod_obj(ref))
nletid <- c(1:length(allGenes(Ec_core)))[! let] 


###################################################
### code chunk number 45: sybil.Rnw:925-926 (eval = FALSE)
###################################################
## gmat <- combn(nletid, 3)


###################################################
### code chunk number 46: sybil.Rnw:931-932 (eval = FALSE)
###################################################
## opt <- multiDel(Ec_core, nProc = 4, todo = "geneDeletion", del1 = gmat)


###################################################
### code chunk number 47: sybil.Rnw:943-944 (eval = FALSE)
###################################################
## mapply(checkOptSol, opt)


###################################################
### code chunk number 48: sybil.Rnw:955-957
###################################################
opt <- optimizeProb(Ec_core, poCmd = list("getRedCosts"))
postProc(opt)


###################################################
### code chunk number 49: sybil.Rnw:993-994 (eval = FALSE)
###################################################
## optimizeProb(Ec_core, method = "exact")


###################################################
### code chunk number 50: sybil.Rnw:997-998 (eval = FALSE)
###################################################
## optimizeProb(Ec_core, solver = "cplexAPI", method = "dualopt")


###################################################
### code chunk number 51: sybil.Rnw:1021-1024 (eval = FALSE)
###################################################
## opt <- oneGeneDel(Ec_core,
##                   solverParm = list(TM_LIM = 1000,
##                                     PRESOLVE = GLP_ON))


###################################################
### code chunk number 52: sybil.Rnw:1037-1041 (eval = FALSE)
###################################################
## opt <- optimizeProb(Ec_core,
##                     solverParm = list(CPX_PARAM_SCRIND = CPX_ON,
##                                       CPX_PARAM_EPRHS = 1E-09),
##                     solver = "cplexAPI")


###################################################
### code chunk number 53: sybil.Rnw:1060-1064 (eval = FALSE)
###################################################
## opt <- optimizeProb(Ec_core,
##                     solverParm = list(verbose = "full",
##                                       timeout = 10),
##                     solver = "lpSolveAPI")


###################################################
### code chunk number 54: sybil.Rnw:1078-1079
###################################################
help(SYBIL_SETTINGS)


###################################################
### code chunk number 55: sybil.Rnw:1101-1102 (eval = FALSE)
###################################################
## SYBIL_SETTINGS("parameter name", value)


###################################################
### code chunk number 56: sybil.Rnw:1107-1108 (eval = FALSE)
###################################################
## SYBIL_SETTINGS("parameter name")


###################################################
### code chunk number 57: sybil.Rnw:1113-1114 (eval = FALSE)
###################################################
## SYBIL_SETTINGS()


###################################################
### code chunk number 58: sybil.Rnw:1129-1130
###################################################
SYBIL_SETTINGS("SOLVER", "cplexAPI", loadPackage = FALSE)


###################################################
### code chunk number 59: sybil.Rnw:1136-1137
###################################################
SYBIL_SETTINGS("METHOD")


###################################################
### code chunk number 60: sybil.Rnw:1140-1141
###################################################
SYBIL_SETTINGS("SOLVER", "glpkAPI")


###################################################
### code chunk number 61: sybil.Rnw:1144-1145
###################################################
SYBIL_SETTINGS("METHOD")


###################################################
### code chunk number 62: sybil.Rnw:1180-1182
###################################################
data(Ec_core)
Ec_core


###################################################
### code chunk number 63: sybil.Rnw:1186-1187
###################################################
help("modelorg")


###################################################
### code chunk number 64: sybil.Rnw:1194-1195
###################################################
react_num(Ec_core)


###################################################
### code chunk number 65: sybil.Rnw:1198-1199
###################################################
id <- react_id(Ec_core)


###################################################
### code chunk number 66: sybil.Rnw:1202-1203
###################################################
react_id(Ec_core)[13] <- "biomass"


###################################################
### code chunk number 67: sybil.Rnw:1207-1209
###################################################
cg <- gray(0:8/8)
image(S(Ec_core), col.regions = c(cg, rev(cg)))


###################################################
### code chunk number 68: sybil.Rnw:1222-1223 (eval = FALSE)
###################################################
## mod <- readTSVmod(reactList = "reactionList.txt")


###################################################
### code chunk number 69: sybil.Rnw:1247-1248
###################################################
help("optsol")


###################################################
### code chunk number 70: sybil.Rnw:1252-1254
###################################################
os <- optimizeProb(Ec_core)
is(os)


###################################################
### code chunk number 71: sybil.Rnw:1257-1258
###################################################
lp_obj(os)


###################################################
### code chunk number 72: sybil.Rnw:1261-1262
###################################################
getFluxDist(os)


###################################################
### code chunk number 73: sybil.Rnw:1307-1308
###################################################
lp <- optObj(solver = "glpkAPI", method = "exact")


###################################################
### code chunk number 74: sybil.Rnw:1332-1333
###################################################
lp <- initProb(lp)


###################################################
### code chunk number 75: sybil.Rnw:1337-1342
###################################################
cm <- Matrix(c(0.5, 2, 1, 1), nrow = 2)
loadLPprob(lp, nCols = 2, nRows = 2, mat = cm,
           lb = c(0, 0), ub = rep(1000, 2), obj = c(1, 1),
           rlb = c(0, 0), rub = c(4.5, 9), rtype = c("U", "U"),
           lpdir = "max")


###################################################
### code chunk number 76: sybil.Rnw:1347-1348
###################################################
lp


###################################################
### code chunk number 77: sybil.Rnw:1351-1352
###################################################
status <- solveLp(lp)


###################################################
### code chunk number 78: sybil.Rnw:1355-1356
###################################################
getMeanReturn(code = status, solver = solver(lp))


###################################################
### code chunk number 79: sybil.Rnw:1359-1361
###################################################
status <- getSolStat(lp)
getMeanStatus(code = status, solver = solver(lp))


###################################################
### code chunk number 80: sybil.Rnw:1365-1367
###################################################
getObjVal(lp)
getFluxDist(lp)


###################################################
### code chunk number 81: sybil.Rnw:1370-1371
###################################################
getRedCosts(lp)


###################################################
### code chunk number 82: sybil.Rnw:1374-1376
###################################################
delProb(lp)
lp


###################################################
### code chunk number 83: sybil.Rnw:1407-1409
###################################################
ec <- sysBiolAlg(Ec_core, algorithm = "fba")
is(ec)


###################################################
### code chunk number 84: sybil.Rnw:1414-1415
###################################################
opt <- optimizeProb(ec)


###################################################
### code chunk number 85: sybil.Rnw:1422-1425
###################################################
ecr <- sysBiolAlg(Ec_core, algorithm = "room", wtflux = opt$fluxes)
is(ecr)
ecr


###################################################
### code chunk number 86: sybil.Rnw:1474-1475 (eval = FALSE)
###################################################
## promptSysBiolAlg(algorithm = "foo")


