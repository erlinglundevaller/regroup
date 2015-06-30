
## fobbost måste vara "ren"

###################################################################################################################################################
##############################################################################################################################################  Län
omlan19982004 <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "lan"), with=F], cutoff = 0.25, startar=1998, slutar=2004)
fobbost[, temp := omlan19982004$newcode[match(fobbost$lan, omlan19982004$New)]]
fobbost[, tempOld := omlan19982004$newcode[match(fobbost$lan, omlan19982004$Old)]]
#fobbost[ ,templan := lan] 
fobbost$lan <- ifelse(fobbost$ar == 2004, fobbost$temp, fobbost$lan)
fobbost$lan <- ifelse(fobbost$ar == 1998, fobbost$tempOld, fobbost$lan)

# table(fobbost$lan, fobbost$temp)
# table(fobbost$lan, fobbost$tempOld)

omlan19921998 <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "lan"), with=F], cutoff = 0.25, startar=1992, slutar=1998)
fobbost[, temp := omlan19921998$newcode[match(fobbost$lan, omlan19921998$New)]]
fobbost[, tempOld := omlan19921998$newcode[match(fobbost$lan, omlan19921998$Old)]]
fobbost$lan <- ifelse( fobbost$ar == 1998, fobbost$temp, fobbost$lan)
fobbost$lan <- ifelse( fobbost$ar == 1992, fobbost$tempOld, fobbost$lan)

omlan19861992 <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "lan"), with=F], cutoff = 0.25, startar=1986, slutar=1992)
fobbost[, temp := omlan19861992$newcode[match(fobbost$lan, omlan19861992$New)]]
fobbost[, tempOld := omlan19861992$newcode[match(fobbost$lan, omlan19861992$Old)]]
fobbost$lan <- ifelse(fobbost$ar == 1992, fobbost$temp, fobbost$lan)
fobbost$lan <- ifelse( fobbost$ar == 1986, fobbost$tempOld, fobbost$lan)
#fobbost$lan <- ifelse( fobbost$ar == 1992 |  fobbost$ar == 1986, omlan19861992$newcode[match(fobbost$lan, omlan19861992$New)] , fobbost$lan)

omlan19801986 <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "lan"), with=F], cutoff = 0.25, startar=1980, slutar=1986)
fobbost[, temp := omlan19801986$newcode[match(fobbost$lan, omlan19801986$New)]]
fobbost[, tempOld := omlan19801986$newcode[match(fobbost$lan, omlan19801986$Old)]]
fobbost$lan <- ifelse(fobbost$ar == 1986, fobbost$temp, fobbost$lan)
fobbost$lan <- ifelse( fobbost$ar == 1980, fobbost$tempOld, fobbost$lan)
#fobbost$lan <- ifelse( fobbost$ar == 1980 |  fobbost$ar == 1980, omlan19801986$newcode[match(fobbost$lan, omlan19801986$New)] , fobbost$lan)

omlan19701980 <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "lan"), with=F], cutoff = 0.35, startar=1970, slutar=1980)
fobbost[, temp := omlan19701980$newcode[match(fobbost$lan, omlan19701980$New)]]
fobbost[, tempOld := omlan19701980$newcode[match(fobbost$lan, omlan19701980$Old)]]
fobbost$lan <- ifelse( fobbost$ar == 1980, fobbost$temp, fobbost$lan)
fobbost$lan <- ifelse( fobbost$ar == 1970 , fobbost$tempOld, fobbost$lan)

#fobbost$lan <- ifelse( fobbost$ar == 1980 |  fobbost$ar == 1970, omlan19701980$newcode[match(fobbost$lan, omlan19701980$New)] , fobbost$lan)

omlan19601970 <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "lan"), with=F], cutoff = 0.35, startar=1960, slutar=1970)
fobbost[, temp := omlan19601970$newcode[match(fobbost$lan, omlan19601970$New)]]
fobbost[, tempOld := omlan19601970$newcode[match(fobbost$lan, omlan19601970$Old)]]
fobbost$lan <- ifelse(fobbost$ar == 1970, fobbost$temp, fobbost$lan)
fobbost$lan <- ifelse(fobbost$ar == 1960 , fobbost$tempOld, fobbost$lan)
#fobbost$lan <- ifelse( fobbost$ar == 1970 |  fobbost$ar == 1960, omlan19601970$newcode[match(fobbost$lan, omlan19601970$New)] , fobbost$lan)
####################################### Tillbaka!!!
omlan19701980Å <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "lan"), with=F], cutoff = 0.35, startar=1970, slutar=1980)
fobbost[, temp := omlan19701980Å$newcode[match(fobbost$lan, omlan19701980Å$New)]]
fobbost[, tempOld := omlan19701980Å$newcode[match(fobbost$lan, omlan19701980Å$Old)]]
fobbost$lan <- ifelse(fobbost$ar == 1980, fobbost$temp, fobbost$lan)
fobbost$lan <- ifelse(fobbost$ar == 1970 , fobbost$tempOld, fobbost$lan)

omlan19801986Å <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "lan"), with=F], cutoff = 0.25, startar=1980, slutar=1986)
fobbost[, temp := omlan19801986Å$newcode[match(fobbost$lan, omlan19801986Å$New)]]
fobbost[, tempOld := omlan19801986Å$newcode[match(fobbost$lan, omlan19801986Å$Old)]]
fobbost$lan <- ifelse(fobbost$ar == 1986, fobbost$temp, fobbost$lan)
fobbost$lan <- ifelse( fobbost$ar == 1980, fobbost$tempOld, fobbost$lan)
# fobbost$newcode <- ifelse( fobbost$ar == 2004 |  fobbost$ar == 1998, omlan19982004$newcode[match(fobbost$lan, omlan19982004$New)] , fobbost$newcode)
omlan19861992Å <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "lan"), with=F], cutoff = 0.25, startar=1986, slutar=1992)
fobbost[, temp := omlan19861992Å$newcode[match(fobbost$lan, omlan19861992Å$New)]]
fobbost[, tempOld := omlan19861992Å$newcode[match(fobbost$lan, omlan19861992Å$Old)]]
fobbost$lan <- ifelse(fobbost$ar == 1992, fobbost$temp, fobbost$lan)
fobbost$lan <- ifelse( fobbost$ar == 1986, fobbost$tempOld, fobbost$lan)
##
omlan19921998Å <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "lan"), with=F], cutoff = 0.25, startar=1992, slutar=1998)
fobbost[, temp := omlan19921998Å$newcode[match(fobbost$lan, omlan19921998Å$New)]]
fobbost[, tempOld := omlan19921998Å$newcode[match(fobbost$lan, omlan19921998Å$Old)]]
fobbost$lan <- ifelse( fobbost$ar == 1998, fobbost$temp, fobbost$lan)
fobbost$lan <- ifelse( fobbost$ar == 1992 , fobbost$tempOld, fobbost$lan)

omlan19982004Å <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "lan"), with=F], cutoff = 0.25, startar=1998, slutar=2004)
fobbost[, temp := omlan19982004Å$newcode[match(fobbost$lan, omlan19982004Å$New)]]
fobbost[, tempOld := omlan19982004Å$newcode[match(fobbost$lan, omlan19982004Å$Old)]]
fobbost$lan <- ifelse(fobbost$ar == 2004, fobbost$temp, fobbost$lan)
fobbost$lan <- ifelse( fobbost$ar == 1998, fobbost$tempOld, fobbost$lan)
##############################################################################################################################################  Län
###################################################################################################################################################

###################################################################################################################################################
##############################################################################################################################################  Kommun
omkommun19982004 <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "kommun"), with=F], cutoff = 0.25, startar=1998, slutar=2004)
fobbost[, temp := omkommun19982004$newcode[match(fobbost$kommun, omkommun19982004$New)]]
fobbost[, tempOld := omkommun19982004$newcode[match(fobbost$kommun, omkommun19982004$Old)]]
#fobbost[ ,tempkommun := kommun] 
fobbost$kommun <- ifelse(fobbost$ar == 2004, fobbost$temp, fobbost$kommun)
fobbost$kommun <- ifelse(fobbost$ar == 1998, fobbost$tempOld, fobbost$kommun)

# table(fobbost$kommun, fobbost$temp)
# table(fobbost$kommun, fobbost$tempOld)

omkommun19921998 <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "kommun"), with=F], cutoff = 0.25, startar=1992, slutar=1998)
fobbost[, temp := omkommun19921998$newcode[match(fobbost$kommun, omkommun19921998$New)]]
fobbost[, tempOld := omkommun19921998$newcode[match(fobbost$kommun, omkommun19921998$Old)]]
fobbost$kommun <- ifelse( fobbost$ar == 1998, fobbost$temp, fobbost$kommun)
fobbost$kommun <- ifelse( fobbost$ar == 1992, fobbost$tempOld, fobbost$kommun)

omkommun19861992 <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "kommun"), with=F], cutoff = 0.25, startar=1986, slutar=1992)
fobbost[, temp := omkommun19861992$newcode[match(fobbost$kommun, omkommun19861992$New)]]
fobbost[, tempOld := omkommun19861992$newcode[match(fobbost$kommun, omkommun19861992$Old)]]
fobbost$kommun <- ifelse(fobbost$ar == 1992, fobbost$temp, fobbost$kommun)
fobbost$kommun <- ifelse( fobbost$ar == 1986, fobbost$tempOld, fobbost$kommun)
#fobbost$kommun <- ifelse( fobbost$ar == 1992 |  fobbost$ar == 1986, omkommun19861992$newcode[match(fobbost$kommun, omkommun19861992$New)] , fobbost$kommun)

omkommun19801986 <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "kommun"), with=F], cutoff = 0.25, startar=1980, slutar=1986)
fobbost[, temp := omkommun19801986$newcode[match(fobbost$kommun, omkommun19801986$New)]]
fobbost[, tempOld := omkommun19801986$newcode[match(fobbost$kommun, omkommun19801986$Old)]]
fobbost$kommun <- ifelse(fobbost$ar == 1986, fobbost$temp, fobbost$kommun)
fobbost$kommun <- ifelse( fobbost$ar == 1980, fobbost$tempOld, fobbost$kommun)
#fobbost$kommun <- ifelse( fobbost$ar == 1980 |  fobbost$ar == 1980, omkommun19801986$newcode[match(fobbost$kommun, omkommun19801986$New)] , fobbost$kommun)

omkommun19701980 <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "kommun"), with=F], cutoff = 0.35, startar=1970, slutar=1980)
fobbost[, temp := omkommun19701980$newcode[match(fobbost$kommun, omkommun19701980$New)]]
fobbost[, tempOld := omkommun19701980$newcode[match(fobbost$kommun, omkommun19701980$Old)]]
fobbost$kommun <- ifelse( fobbost$ar == 1980, fobbost$temp, fobbost$kommun)
fobbost$kommun <- ifelse( fobbost$ar == 1970 , fobbost$tempOld, fobbost$kommun)

#fobbost$kommun <- ifelse( fobbost$ar == 1980 |  fobbost$ar == 1970, omkommun19701980$newcode[match(fobbost$kommun, omkommun19701980$New)] , fobbost$kommun)

omkommun19601970 <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "kommun"), with=F], cutoff = 0.35, startar=1960, slutar=1970)
fobbost[, temp := omkommun19601970$newcode[match(fobbost$kommun, omkommun19601970$New)]]
fobbost[, tempOld := omkommun19601970$newcode[match(fobbost$kommun, omkommun19601970$Old)]]
fobbost$kommun <- ifelse(fobbost$ar == 1970, fobbost$temp, fobbost$kommun)
fobbost$kommun <- ifelse(fobbost$ar == 1960 , fobbost$tempOld, fobbost$kommun)
#fobbost$kommun <- ifelse( fobbost$ar == 1970 |  fobbost$ar == 1960, omkommun19601970$newcode[match(fobbost$kommun, omkommun19601970$New)] , fobbost$kommun)
####################################### Tillbaka!!!
omkommun19701980Å <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "kommun"), with=F], cutoff = 0.35, startar=1970, slutar=1980)
fobbost[, temp := omkommun19701980Å$newcode[match(fobbost$kommun, omkommun19701980Å$New)]]
fobbost[, tempOld := omkommun19701980Å$newcode[match(fobbost$kommun, omkommun19701980Å$Old)]]
fobbost$kommun <- ifelse(fobbost$ar == 1980, fobbost$temp, fobbost$kommun)
fobbost$kommun <- ifelse(fobbost$ar == 1970 , fobbost$tempOld, fobbost$kommun)

omkommun19801986Å <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "kommun"), with=F], cutoff = 0.25, startar=1980, slutar=1986)
fobbost[, temp := omkommun19801986Å$newcode[match(fobbost$kommun, omkommun19801986Å$New)]]
fobbost[, tempOld := omkommun19801986Å$newcode[match(fobbost$kommun, omkommun19801986Å$Old)]]
fobbost$kommun <- ifelse(fobbost$ar == 1986, fobbost$temp, fobbost$kommun)
fobbost$kommun <- ifelse( fobbost$ar == 1980, fobbost$tempOld, fobbost$kommun)
# fobbost$newcode <- ifelse( fobbost$ar == 2004 |  fobbost$ar == 1998, omkommun19982004$newcode[match(fobbost$kommun, omkommun19982004$New)] , fobbost$newcode)
omkommun19861992Å <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "kommun"), with=F], cutoff = 0.25, startar=1986, slutar=1992)
fobbost[, temp := omkommun19861992Å$newcode[match(fobbost$kommun, omkommun19861992Å$New)]]
fobbost[, tempOld := omkommun19861992Å$newcode[match(fobbost$kommun, omkommun19861992Å$Old)]]
fobbost$kommun <- ifelse(fobbost$ar == 1992, fobbost$temp, fobbost$kommun)
fobbost$kommun <- ifelse( fobbost$ar == 1986, fobbost$tempOld, fobbost$kommun)
##
omkommun19921998Å <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "kommun"), with=F], cutoff = 0.25, startar=1992, slutar=1998)
fobbost[, temp := omkommun19921998Å$newcode[match(fobbost$kommun, omkommun19921998Å$New)]]
fobbost[, tempOld := omkommun19921998Å$newcode[match(fobbost$kommun, omkommun19921998Å$Old)]]
fobbost$kommun <- ifelse( fobbost$ar == 1998, fobbost$temp, fobbost$kommun)
fobbost$kommun <- ifelse( fobbost$ar == 1992 , fobbost$tempOld, fobbost$kommun)

omkommun19982004Å <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "kommun"), with=F], cutoff = 0.25, startar=1998, slutar=2004)
fobbost[, temp := omkommun19982004Å$newcode[match(fobbost$kommun, omkommun19982004Å$New)]]
fobbost[, tempOld := omkommun19982004Å$newcode[match(fobbost$kommun, omkommun19982004Å$Old)]]
fobbost$kommun <- ifelse(fobbost$ar == 2004, fobbost$temp, fobbost$kommun)
fobbost$kommun <- ifelse( fobbost$ar == 1998, fobbost$tempOld, fobbost$kommun)
##############################################################################################################################################  Kommun
###################################################################################################################################################
###################################################################################################################################################
##############################################################################################################################################  forsamling
omforsamling19982004 <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "forsamling"), with=F], cutoff = 0.25, startar=1998, slutar=2004)
fobbost[, temp := omforsamling19982004$newcode[match(fobbost$forsamling, omforsamling19982004$New)]]
fobbost[, tempOld := omforsamling19982004$newcode[match(fobbost$forsamling, omforsamling19982004$Old)]]
#fobbost[ ,tempforsamling := forsamling] 
fobbost$forsamling <- ifelse(fobbost$ar == 2004, fobbost$temp, fobbost$forsamling)
fobbost$forsamling <- ifelse(fobbost$ar == 1998, fobbost$tempOld, fobbost$forsamling)

# table(fobbost$forsamling, fobbost$temp)
# table(fobbost$forsamling, fobbost$tempOld)

omforsamling19921998 <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "forsamling"), with=F], cutoff = 0.25, startar=1992, slutar=1998)
fobbost[, temp := omforsamling19921998$newcode[match(fobbost$forsamling, omforsamling19921998$New)]]
fobbost[, tempOld := omforsamling19921998$newcode[match(fobbost$forsamling, omforsamling19921998$Old)]]
fobbost$forsamling <- ifelse( fobbost$ar == 1998, fobbost$temp, fobbost$forsamling)
fobbost$forsamling <- ifelse( fobbost$ar == 1992, fobbost$tempOld, fobbost$forsamling)

omforsamling19861992 <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "forsamling"), with=F], cutoff = 0.25, startar=1986, slutar=1992)
fobbost[, temp := omforsamling19861992$newcode[match(fobbost$forsamling, omforsamling19861992$New)]]
fobbost[, tempOld := omforsamling19861992$newcode[match(fobbost$forsamling, omforsamling19861992$Old)]]
fobbost$forsamling <- ifelse(fobbost$ar == 1992, fobbost$temp, fobbost$forsamling)
fobbost$forsamling <- ifelse( fobbost$ar == 1986, fobbost$tempOld, fobbost$forsamling)
#fobbost$forsamling <- ifelse( fobbost$ar == 1992 |  fobbost$ar == 1986, omforsamling19861992$newcode[match(fobbost$forsamling, omforsamling19861992$New)] , fobbost$forsamling)

omforsamling19801986 <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "forsamling"), with=F], cutoff = 0.25, startar=1980, slutar=1986)
fobbost[, temp := omforsamling19801986$newcode[match(fobbost$forsamling, omforsamling19801986$New)]]
fobbost[, tempOld := omforsamling19801986$newcode[match(fobbost$forsamling, omforsamling19801986$Old)]]
fobbost$forsamling <- ifelse(fobbost$ar == 1986, fobbost$temp, fobbost$forsamling)
fobbost$forsamling <- ifelse( fobbost$ar == 1980, fobbost$tempOld, fobbost$forsamling)
#fobbost$forsamling <- ifelse( fobbost$ar == 1980 |  fobbost$ar == 1980, omforsamling19801986$newcode[match(fobbost$forsamling, omforsamling19801986$New)] , fobbost$forsamling)

omforsamling19701980 <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "forsamling"), with=F], cutoff = 0.35, startar=1970, slutar=1980)
fobbost[, temp := omforsamling19701980$newcode[match(fobbost$forsamling, omforsamling19701980$New)]]
fobbost[, tempOld := omforsamling19701980$newcode[match(fobbost$forsamling, omforsamling19701980$Old)]]
fobbost$forsamling <- ifelse( fobbost$ar == 1980, fobbost$temp, fobbost$forsamling)
fobbost$forsamling <- ifelse( fobbost$ar == 1970 , fobbost$tempOld, fobbost$forsamling)

#fobbost$forsamling <- ifelse( fobbost$ar == 1980 |  fobbost$ar == 1970, omforsamling19701980$newcode[match(fobbost$forsamling, omforsamling19701980$New)] , fobbost$forsamling)

omforsamling19601970 <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "forsamling"), with=F], cutoff = 0.35, startar=1960, slutar=1970)
fobbost[, temp := omforsamling19601970$newcode[match(fobbost$forsamling, omforsamling19601970$New)]]
fobbost[, tempOld := omforsamling19601970$newcode[match(fobbost$forsamling, omforsamling19601970$Old)]]
fobbost$forsamling <- ifelse(fobbost$ar == 1970, fobbost$temp, fobbost$forsamling)
fobbost$forsamling <- ifelse(fobbost$ar == 1960 , fobbost$tempOld, fobbost$forsamling)
#fobbost$forsamling <- ifelse( fobbost$ar == 1970 |  fobbost$ar == 1960, omforsamling19601970$newcode[match(fobbost$forsamling, omforsamling19601970$New)] , fobbost$forsamling)
####################################### Tillbaka!!!
omforsamling19701980Å <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "forsamling"), with=F], cutoff = 0.35, startar=1970, slutar=1980)
fobbost[, temp := omforsamling19701980Å$newcode[match(fobbost$forsamling, omforsamling19701980Å$New)]]
fobbost[, tempOld := omforsamling19701980Å$newcode[match(fobbost$forsamling, omforsamling19701980Å$Old)]]
fobbost$forsamling <- ifelse(fobbost$ar == 1980, fobbost$temp, fobbost$forsamling)
fobbost$forsamling <- ifelse(fobbost$ar == 1970 , fobbost$tempOld, fobbost$forsamling)

omforsamling19801986Å <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "forsamling"), with=F], cutoff = 0.25, startar=1980, slutar=1986)
fobbost[, temp := omforsamling19801986Å$newcode[match(fobbost$forsamling, omforsamling19801986Å$New)]]
fobbost[, tempOld := omforsamling19801986Å$newcode[match(fobbost$forsamling, omforsamling19801986Å$Old)]]
fobbost$forsamling <- ifelse(fobbost$ar == 1986, fobbost$temp, fobbost$forsamling)
fobbost$forsamling <- ifelse( fobbost$ar == 1980, fobbost$tempOld, fobbost$forsamling)
# fobbost$newcode <- ifelse( fobbost$ar == 2004 |  fobbost$ar == 1998, omforsamling19982004$newcode[match(fobbost$forsamling, omforsamling19982004$New)] , fobbost$newcode)
omforsamling19861992Å <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "forsamling"), with=F], cutoff = 0.25, startar=1986, slutar=1992)
fobbost[, temp := omforsamling19861992Å$newcode[match(fobbost$forsamling, omforsamling19861992Å$New)]]
fobbost[, tempOld := omforsamling19861992Å$newcode[match(fobbost$forsamling, omforsamling19861992Å$Old)]]
fobbost$forsamling <- ifelse(fobbost$ar == 1992, fobbost$temp, fobbost$forsamling)
fobbost$forsamling <- ifelse( fobbost$ar == 1986, fobbost$tempOld, fobbost$forsamling)
##
omforsamling19921998Å <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "forsamling"), with=F], cutoff = 0.25, startar=1992, slutar=1998)
fobbost[, temp := omforsamling19921998Å$newcode[match(fobbost$forsamling, omforsamling19921998Å$New)]]
fobbost[, tempOld := omforsamling19921998Å$newcode[match(fobbost$forsamling, omforsamling19921998Å$Old)]]
fobbost$forsamling <- ifelse( fobbost$ar == 1998, fobbost$temp, fobbost$forsamling)
fobbost$forsamling <- ifelse( fobbost$ar == 1992 , fobbost$tempOld, fobbost$forsamling)

omforsamling19982004Å <- omkodningsmaskin(data=fobbost[, c("linneid", "ar", "forsamling"), with=F], cutoff = 0.25, startar=1998, slutar=2004)
fobbost[, temp := omforsamling19982004Å$newcode[match(fobbost$forsamling, omforsamling19982004Å$New)]]
fobbost[, tempOld := omforsamling19982004Å$newcode[match(fobbost$forsamling, omforsamling19982004Å$Old)]]
fobbost$forsamling <- ifelse(fobbost$ar == 2004, fobbost$temp, fobbost$forsamling)
fobbost$forsamling <- ifelse( fobbost$ar == 1998, fobbost$tempOld, fobbost$forsamling)
##############################################################################################################################################  forsamling
###################################################################################################################################################
