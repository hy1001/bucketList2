
#####################################################
# utility functions
#####################################################

rm( id.bckt.subBckt )
require(plyr)
library(RMySQL)

## preset Visits
Visit <- c( "V4", paste( c("V4."), c("5", "6", "7"), sep = "" ),
           paste( c("V5"), c("", ".5", ".6"), sep = "" ),
           paste( c( "V" ), seq(6, 15.5, .5), sep = "" ), "FV"  )

cat( "############################################\n\n" )
cat( "Starting the bucketList2...\n\n" )
cat( "Loading functions...\n\n" )

## grep.randv.ta("100-0001")
grep.randv.ta <- function( x ) {
  randv.ta[ grep( x, randv.ta$PATID), ]
}
grep.discont <- function( x ) {
  discont[ grep( x, discont$PATID), ]
}
## returnIdealVisitDate( randdt, "V15.5" )
## V15.5 date when randdt
returnIdealVisitDate <- function ( x , y ) {
    tmp <- as.Date( as.Date( x ) + cumsum( c(0, rep(30, 6), rep(60, 21) ) ) )
    names( tmp ) <- c( "V4", paste(  c("V4."), c("5", "6", "7"), sep = "" ),
                      paste( c("V5"), c("", ".5", ".6"), sep = "" ),
                      paste( c( "V" ), seq(6, 15.5, .5), sep = "" ), "FV"  )
    tmp[ y ]
    # tmp
}

cat( "#####################################################\n" )
cat( "##       Study drug discontinuation form           ##\n" )
cat( "#####################################################\n" )
discont <- read.csv("csvData/bckt_discont.csv", header = T)
##str(discont)
### discont was not sorted by stopdt. Look at US-U225-0006 stopdates for ref.
### Sorting discont by patid and stopdt
discont$STOPDT <- as.Date( discont$STOPDT, format = "%d-%b-%Y" )
discont$PLSTARTDT <- as.Date( discont$PLSTARTDT, format = "%d-%b-%Y" )
discont$NEXTDT <- as.Date( discont$NEXTDT, format = "%d-%b-%Y" )
discont$STARTDT <- as.Date( discont$STARTDT, format = "%d-%b-%Y" )
discont$WASHDT <- as.Date( discont$WASHDT, format = "%d-%b-%Y" )
discont <- discont[ with(discont, order( PATID, STOPDT ) ), ]

## discont[ discont$PATID == "US-U225-0006", ]$STOPDT
## sort( discont[ discont$PATID == "US-U225-0006", ]$STOPDT )
## order( discont[ discont$PATID == "US-U225-0006", ]$STOPDT )
# catching the most recent event
# tmp[ rev( duplicated( rev(tmp$PATID) ) == F ), ]

### pick the most recent
sdd.latest <- discont[ rev( duplicated( rev(discont$PATID) ) == F ), ]

cat( "#######################################################\n" )
cat( "###           Visit + TA                             ##\n" )
cat( "#######################################################\n" )
randv.ta <- read.csv("csvData/bckt_randv_ta.csv", header = T)

## Might need below for merging all patients and all ta
## http://stackoverflow.com/questions/6709151/how-do-i-combine-two-data-frames-based-on-two-columns
## merge(x, y, by=c("k1","k2")) # NA's match
## df3 = merge(df1, df2, by.x=c("CustomerId", "Hobby"), by.y=c("CustomerId", "like"))

randv.ta$SIGNDT <- strptime( randv.ta$SIGNDT, format = "%d%b%Y:%H:%M:%S")
randv.ta$randdt <- as.Date( randv.ta$randdt, format = "%d-%b-%Y" )
randv.ta$VISITDT <- as.Date( randv.ta$VISITDT, format = "%d-%b-%Y" )
randv.ta$NEXTDT <- as.Date( randv.ta$NEXTDT, format = "%d-%b-%Y" )
# str(randv.ta)
# filtered.randv.ta <- randv.ta[ randv.ta$STATUS == "Completed and signed" | randv.ta$STATUS == "Completed but needs to be signed" | randv.ta$STATUS == "Locked", ]

visit.latest <- randv.ta[ rev( duplicated( rev( randv.ta$PATID ) ) == F ), ]
head( visit.latest[ visit.latest$ONDRUG == "No, permanent stop", ] )

cat( "\n#####################################################\n" )
cat( "# READING\n" )
cat( "# non-recoverable Perm Stop\n" )
cat( "# 1. medical monitor : bckt_medm.csv\n" )
cat( "# 2. adjudicated : bckt_adj.csv\n" )
cat( "# 3. end-point deaths : bckt_death.csv\n" )
cat( "#####################################################\n" )
medmon <- read.csv( "csvData/bckt_medm.csv", header = T )

###most recent medmon report
medmon <- medmon[ rev( duplicated ( rev( medmon$PATID ) ) == F ), ]

medmon.ids <- medmon[ medmon$DRUGPLAN == "Permanent stop", ]$PATID
# medmon[medmom$DRUGPLAN == "Permanent stop",1:13]
## death before adjudicated
endpoint.death <- read.csv( "csvData/bckt_death.csv", header = T )
death.ids <- endpoint.death$PATID
#adj
adjudicated <- read.csv( "csvData/bckt_adj.csv", header = T )
# adj.ids <- adjudicated[ adjudicated$death == "Yes" | adjudicated$mi == "Yes" | adjudicated$stroke == "Yes", c("cec_id", "patid")]
adj.ids <- adjudicated[ adjudicated$cardiodeath == "Yes" | adjudicated$mi == "Yes" | adjudicated$stroke == "Yes", c("cec_id", "patid")]

##### cross reference with cec #### 
cec <- read.csv("csvData/bckt_cec.csv", header = T )
## sort
cec.sorted <- cec[ order( cec$cec_id ), ]
## Get one line for cec_id
cec.singled <- cec.sorted[ duplicated( cec.sorted$cec_id ) == F , ]
## Extract completed only for merge. Then, take those one that can survive after merging.
cec.singled.completed <- cec.singled [ cec.singled$EPISODE_STATUS == "Completed", c("cec_id", "patid", "EPISODE_STATUS")]
adj.ids <- merge( adj.ids, cec.singled.completed, by = "cec_id" )
adj.ids <- adj.ids$patid.x
##### cross reference with cec ####

non.recoverable.PermStop.ids <- union( union( medmon.ids, adj.ids ), death.ids )

cat( ">length( non.recoverable.PermStop.ids )\n" )
length( non.recoverable.PermStop.ids )

cat( "\n#####################################################\n" )
cat( "# recoverable Perm Stop\n" )
cat( "# 1. mi, not adjudicated\n" )
cat( "# 2. stroke, not adjudicated\n" )
cat(" # 3. withdraw\n" )
cat( "# 4. SDD stoptype == \"Permanent stop\"\n" ) 
cat( "# 5. visit.ondrug perm stop (actually, this for temp stop)\n" )
cat( "#####################################################\n" )

mi <- read.csv( "csvData/bckt_mi.csv", header = T )
mi <- mi[ mi$mi_cec != "No", ] ## getting rid of disconfirmed cases
mi.patid <- setdiff( unique( mi$PATID ), medmon[ medmon$DRUGPLAN == "Permanent stop", ]$PATID )

stroke <- read.csv( "csvData/bckt_stroke.csv", header = T )
stroke <- stroke[ stroke$stroke_cec != "No", ] ## getting rid of disconfirmed cases
stroke.patid <- setdiff( unique( stroke$PATID ), medmon[ medmon$DRUGPLAN == "Permanent stop", ]$PATID )

mi.stroke.patid <- union( mi.patid, stroke.patid )

## Consider adj table for the disconfirmed mi/stroke cases ## do not need this any more... due to stroke_cec, mi_cec
#disconfirmed.mi.stroke.ids <- adjudicated[ adjudicated$mi == "No" | adjudicated$stroke == "No" , c( "patid" ) ]
#mi.stroke.patid <- setdiff ( mi.stroke.patid, disconfirmed.mi.stroke.ids )

wdraw <- read.csv( "csvData/bckt_wdraw.csv", header = T )
wdraw.patid <- wdraw[ wdraw$WITHD_TYPE == "Post-randomization", ]$PATID

sdd.PermStop.ids <- sdd.latest[ sdd.latest$STOPTYPE == "Permanent stop", ]$PATID

visit.ondrug.PermStop.ids <- setdiff( visit.latest[ visit.latest$ONDRUG == "No, permanent stop", ]$PATID, sdd.latest[ sdd.latest$STOPTYPE == "Permanent stop", ]$PATID )

recoverable.PermStop.ids <- sort( union( union( mi.stroke.patid, wdraw.patid ), sdd.PermStop.ids ) )

## ## ## ## Consider adj table for the disconfirmed mi/stroke cases
## disconfirmed.mi.stroke.ids <- adjudicated[ adjudicated$mi == "No" | adjudicated$stroke == "No" , c( "patid" ) ]
## recoverable.PermStop.ids <- setdiff ( recoverable.PermStop.ids, disconfirmed.mi.stroke.ids )

cat( ">length(recoverable.PermStop.ids)\n" )
length(recoverable.PermStop.ids)
## recoverable.PermStop.ids
## duplicated( recoverable.PermStop.ids )

cat( "\n#######################################################\n" )
cat( "### Temp Stop\n" )
cat( "### 1. sdd.latest$stoptype == \"Temporary stop\" and not restarted and no startdt\n" )
cat( "### 2. visit.latest$NEXTDOSE == 0\n" )
cat( "### 3. from PermStop #5\n")
cat( "#######################################################\n\n" )

sdd.stoptype.TempStop <- sdd.latest[ sdd.latest$STOPTYPE == "Temporary stop" & sdd.latest$RESTART != "Yes" &  is.na( sdd.latest$STARTDT ) , ]

tempStop.ta.nextdoseZero <- visit.latest[ visit.latest$NEXTDOSE == 0, ]

##################
# select with colnames in data frame.
## head( visit.latest[ visit.latest$NEXTDOSE == 0, c( "PATID", "VISIT", "ONDRUG", "NEXTDT", "PLAN", "CURRENTDOSE", "NEXTDOSE" ) ], 5 )
##################

tempStop.ids <- sort( union( union( sdd.stoptype.TempStop$PATID, tempStop.ta.nextdoseZero$PATID ), visit.ondrug.PermStop.ids ) )

cat(">length( tempStop.ids )\n" )
length( tempStop.ids )    

cat( "\n#####################################################\n" )
cat ( "## intersection with randomized\n" )
cat ( "## bckt_randv_ta has more patients (V4) than bckt_rand\n" )
cat ( "## But, visit.latest$rand == 1 can filter them out.\n" )
cat ("######################################################\n" )

rand.ids <- visit.latest[ visit.latest$rand == 1, ]$PATID
rand.non.recoverable.PermStop.ids <- intersect( rand.ids, non.recoverable.PermStop.ids )
rand.after.non.recoverable.PermStop.ids <- setdiff ( rand.ids, non.recoverable.PermStop.ids )
rand.recoverable.PermStop.ids <- intersect( rand.after.non.recoverable.PermStop.ids, recoverable.PermStop.ids )
rand.after.recoverable.PermStop.ids <- setdiff( rand.after.non.recoverable.PermStop.ids, recoverable.PermStop.ids )
rand.tempStop.ids <- intersect( rand.after.recoverable.PermStop.ids, tempStop.ids )
theRest <- setdiff( rand.after.recoverable.PermStop.ids, tempStop.ids )

cat( ">length( rand.ids )\n" )
length( rand.ids )
cat( ">length( rand.non.recoverable.PermStop.ids )\n" )
length( rand.non.recoverable.PermStop.ids )
cat( ">length( rand.recoverable.PermStop.ids )\n" )
length( rand.recoverable.PermStop.ids )
cat( ">length( rand.tempStop.ids )\n" )
length( rand.tempStop.ids )
cat( ">length( theRest )\n" )
length( theRest )
cat( "sum up all buckets ---\n" )
( length( rand.non.recoverable.PermStop.ids ) + length( rand.recoverable.PermStop.ids )
 + length( rand.tempStop.ids ) + length( theRest ) )

cat ("######################################################\n" )
cat ("##\n" )
cat ("## Reporting\n" )
cat ("##\n")
cat ("######################################################\n" )
cat ("#### non-recoverable PermStop reporting...\n" )
cat ("######################################################\n" )
rand.medmon.patid <- intersect( rand.non.recoverable.PermStop.ids, medmon.ids )
rand.after.medmon.ids <- setdiff( rand.non.recoverable.PermStop.ids, medmon.ids )
id.bckt.subBckt <- data.frame( pid = rand.medmon.patid, bckt = "6_non-recoverable PermStop", subBckt = "S_Medical Monitor" )

## death before adjudicated
rand.death.patid <- intersect( rand.after.medmon.ids, death.ids )
rand.after.death.ids <- setdiff( rand.after.medmon.ids, death.ids )
#id.bckt.subBckt <- rbind( id.bckt.subBckt, data.frame( pid = rand.death.patid, bckt = "6_non-recoverable PermStop", subBckt = "Q_Death" ) )

## cat("== adj. Death==")
## intersect( rand.death.patid, adj.ids )
id.bckt.subBckt <- rbind( id.bckt.subBckt, data.frame( pid = intersect( rand.death.patid, adj.ids ), bckt = "6_non-recoverable PermStop", subBckt = "Q_AdjDeath") )

## cat("== other Death==")
## setdiff( rand.death.patid, adj.ids )
id.bckt.subBckt <- rbind( id.bckt.subBckt, data.frame( pid = setdiff( rand.death.patid, adj.ids )
                                                    , bckt = "6_non-recoverable PermStop", subBckt = "R_OtherDeath") )

rand.adj.patid <- intersect( rand.after.death.ids, adj.ids )
rand.after.adj.ids <- setdiff( rand.after.death.ids, adj.ids )

cat( ">str( rand.after.adj.ids )\n" )
str( rand.after.adj.ids )
cat( "##character(0) # always\n" )
id.bckt.subBckt <- rbind( id.bckt.subBckt, data.frame( pid = rand.adj.patid, bckt = "6_non-recoverable PermStop", subBckt = "P_Adjudicated" ) )

cat( ">length( rand.non.recoverable.PermStop.ids )\n" )
length( rand.non.recoverable.PermStop.ids )
cat( ">str( id.bckt.subBckt )\n" )
str( id.bckt.subBckt )

cat( "#####################################################\n" )
cat( "### recoverable PermStop reporting...\n")
cat( "#####################################################\n")
rand.mi.patid <- intersect( rand.recoverable.PermStop.ids, mi.patid )
rand.after.mi.ids <- setdiff( rand.recoverable.PermStop.ids, mi.patid )

if ( length( rand.mi.patid ) != 0 ) {
    id.bckt.subBckt <- rbind( id.bckt.subBckt, data.frame( pid = rand.mi.patid, bckt = "5_recoverable PermStop", subBckt = "M_MI" ) )
    cat( "mi case --> " )
    length( rand.mi.patid )
} else {
    cat( "mi : 0 case\n" )
}
rand.stroke.patid <- intersect( rand.after.mi.ids, stroke.patid )
rand.after.stroke.ids <- setdiff( rand.after.mi.ids, stroke.patid )

if ( length( rand.stroke.patid ) != 0 ) {
    id.bckt.subBckt <- rbind( id.bckt.subBckt, data.frame( pid = rand.stroke.patid, bckt = "5_recoverable PermStop", subBckt = "N_Stroke" ) )
    cat( "stroke case -> " )
    length( rand.stroke.patid )
} else {
    cat( "stroke : 0 case\n" )
}

cat( "### For Akshaya, mi + stroke output\n" )
( mi.stroke <- id.bckt.subBckt[ id.bckt.subBckt$subBckt == "M_MI" | id.bckt.subBckt$subBckt == "N_Stroke", ] )
 write.csv( mi.stroke , "mi_stroke.csv" )

rand.wdraw.patid <- intersect( rand.after.stroke.ids, wdraw.patid )
cat( "### wdraw --->" )
length( rand.wdraw.patid )
id.bckt.subBckt <- rbind( id.bckt.subBckt, data.frame( pid = rand.wdraw.patid, bckt = "5_recoverable PermStop", subBckt = "O_Withdrawn" ) )

rand.after.wdraw.ids <- setdiff( rand.after.stroke.ids, wdraw.patid )
cat( "###drug discont only --->" )
length( rand.after.wdraw.ids )
id.bckt.subBckt <- rbind( id.bckt.subBckt, data.frame( pid = rand.after.wdraw.ids, bckt = "5_recoverable PermStop", subBckt = "L_By Study Drug Discontinuation Form" ) )

sdd.latest$AE <- ifelse( sdd.latest$AE == "1", "Yes", "No" )
sdd.latest$ENDPOINT <- ifelse( sdd.latest$ENDPOINT == "1", "Yes", "No" )
sdd.latest$SUBJECT <- ifelse( sdd.latest$SUBJECT == "1", "Yes", "No" )

recoverable.PermStop <- merge( sdd.latest[ sdd.latest$PATID %in% rand.after.wdraw.ids, c("PATID", "STATUS", "STOPDT", "STOPTYPE", "AE", "ENDPOINT", "SUBJECT", "OTHERREAS_TXT", "EXCLCRIT") ], visit.latest[ visit.latest$PATID %in% rand.after.wdraw.ids, c( "PATID", "VISITDT", "centno", "liaison" ) ], by="PATID" )

write.csv(recoverable.PermStop[ , c( "PATID", "liaison", "STOPDT", "AE", "ENDPOINT", "SUBJECT", "OTHERREAS_TXT", "EXCLCRIT", "VISITDT") ], "recoverablePermStop.csv")

cat ("###########################\n" )
cat ("#### TempStop reporting...\n" )
cat ("###########################\n" )

rand.sdd.stoptype.TempStop.patid <- intersect( rand.tempStop.ids, sdd.stoptype.TempStop$PATID )
rand.after.sdd.tempStop.ids <- setdiff( rand.tempStop.ids, sdd.stoptype.TempStop$PATID )

cat( ">length( rand.sdd.stoptype.TempStop.patid )\n")
length( rand.sdd.stoptype.TempStop.patid )
cat( ">length( rand.after.sdd.tempStop.ids )\n" )
length( rand.after.sdd.tempStop.ids )

##  # to compare if sdd got restarted after the visit date
##  # if so, move the patient to theRest from tempStop
##  # But let's not do this for now. Too few cases.
##  ### merge( sdd.latest[ sdd.latest$PATID %in% rand.after.sdd.tempStop.ids, c( "PATID", "STOPDT", "RESTART", "STARTDT") ], visit.latest[ visit.latest$PATID %in% rand.after.sdd.tempStop.ids, c( "PATID", "VISITDT", "CURRENTDOSE", "NEXTDOSE" ) ], by = "PATID" )

discont.tmp <- sdd.latest[ sdd.latest$PATID %in% rand.sdd.stoptype.TempStop.patid,
                          c("PATID", "STATUS", "STOPDT", "STOPTYPE", "OTHERREAS_TXT", "AEREASON" ) ]
tempStop.sdd <- merge( discont.tmp, visit.latest[, c("PATID", "centno", "liaison", "VISIT", "VISITDT" ) ], by = "PATID" )

##  # tempStopNotOnSDD i.e., nextDose Zero or PermStop wo SDD
tmp <- randv.ta[ randv.ta$PATID %in% rand.after.sdd.tempStop.ids, c( "PATID", "VISIT", "VISITDT", "ONDRUG", "CURRENTDOSE", "NEXTDOSE" ) ] 

## turn factors to char vector, again to factor to remove all the unused levels.
tmp$PATID <- as.factor( as.character( tmp$PATID ) )

## extract by PATID, then find out the latest visit with non-zero next dose.
tempStopNotOnSDD.tmp <-
  ddply(tmp, "PATID",
        function( x ) {
          ##x <- x[ as.character( x$VISITDT ) != "", ]
          x <- x[ !is.na( x$VISITDT ) , ]
          x$NEXTDOSE[ x$NEXTDOSE == "." ] = 0
          nextDoseRunLength <- rle( as.character( x$NEXTDOSE ) )
          STOPDT <- x[ nrow(x) - nextDoseRunLength$lengths[ length( nextDoseRunLength$values ) ] + 1, ]$VISITDT
          data.frame( STOPDT , STOPTYPE = "Temporary stop", OTHERREAS_TXT = "No study drug discontinuation form")
        }
        )

## ## ### Perfect!!!
## ## ##           PATID      stopdt   stopVisit
## ## ## 1  CA-O603-0028 19-Jan-2015          V6
## ## ## 2  US-U114-0002 24-Mar-2015 Extra visit
## ## ## 3  US-U123-0012 10-Mar-2015          V7
## ## ## 4  US-U123-0018 17-Mar-2015        V4.6
## ## ## 5  US-U135-0006 29-Jul-2014        V4.6
## ## ## 6  US-U167-0009 19-Mar-2015        V5.6
## ## ## 7  US-U209-0006 10-Mar-2015        V6.5
## ## ## 8  US-U221-0006 11-Mar-2015          V7
## ## ## 9  US-U255-0003 16-Mar-2015        V5.6
## ## ## 10 US-U274-0004 17-Mar-2015          V8
## ## ## 11 US-U314-0001  3-Mar-2015          V8
## ## ## 12 US-U320-0010  4-Feb-2015        V5.6
## ## ## 13 US-U345-0015 20-Mar-2015          V5
## ## ## 14 US-V502-0004  2-Jun-2014          V4

tempStopNotOnSDD <- merge( tempStopNotOnSDD.tmp, visit.latest[, c("PATID", "centno", "liaison", "VISIT", "VISITDT" ) ], by = "PATID" )

#########################
cat( "##### tempStop.combined\n" )
#########################
tempStop <- rbind.fill( tempStop.sdd, tempStopNotOnSDD )
write.csv( tempStop[ , c( "PATID", "liaison", "STOPDT", "VISIT", "VISITDT", "OTHERREAS_TXT", "AEREASON" ) ], "tempStop.csv" )

cat( "## tempStop.combined datediff today and stopdt\n" )
daysSinceStopDT <- as.numeric( Sys.Date() - tempStop$STOPDT )
##daysSinceStopDT <- as.numeric( as.Date("2015-09-25") - tempStop$STOPDT )

###########################
cat( ">length( daysSinceStopDT )\n" )
length( daysSinceStopDT )

cat( ">length( which( daysSinceStopDT <= 14 ) ) # <= 2 wks\n" )
length( which( daysSinceStopDT <= 14 ) ) # <= 2 wks

cat( ">length( which( daysSinceStopDT > 14 & daysSinceStopDT <= 42 ) ) # 2 - 6 wks\n" )
length( which( daysSinceStopDT > 14 & daysSinceStopDT <= 42 ) ) # 2 - 6 wks

cat( ">length( which( daysSinceStopDT > 42 ) ) # > 6 wks\n" )
length( which( daysSinceStopDT > 42 ) ) # > 6 wks

id.bckt.subBckt <- rbind( id.bckt.subBckt, data.frame( pid = tempStop[ which( daysSinceStopDT <= 14 ), c("PATID") ], bckt = "4_TempStop", subBckt ="I_< 2 wks" ) )
id.bckt.subBckt <- rbind( id.bckt.subBckt, data.frame( pid = tempStop[ which( daysSinceStopDT > 14 & daysSinceStopDT <= 42 ), c("PATID") ], bckt = "4_TempStop", subBckt ="J_2 - 6 wks" ) )
id.bckt.subBckt <- rbind( id.bckt.subBckt, data.frame( pid = tempStop[ which( daysSinceStopDT > 42 ), c("PATID") ], bckt = "4_TempStop", subBckt ="K_> 6 wks" ) )

################################
cat("#### calculating nextdt ... \n" )
################################

randv.ta$VISIT[ is.na( randv.ta$VISIT ) ] = "V4"
randv.ta$SIGNDT <- strptime( randv.ta$SIGNDT, "%Y-%m-%d %H:%M:%S")
randv.ta$SIGNDT <- as.POSIXct( randv.ta$SIGNDT )

randv.nextdt <- ddply( randv.ta[ randv.ta$rand == 1 , ], "PATID",
      function( x ) {
        nextdt <- tail( x$NEXTDT, 1 )
        t <- tail( x, 1 )
        if ( is.na( nextdt ) ) {
          ## just in case latest visit is Extra Visit,
          ## what I really need is the most recent regular visit
          ## randdt, next regular visit
          nextdt <- returnIdealVisitDate(  t$randdt ,
                                         Visit[ max( which ( Visit %in% x$VISIT ) ) + 1 ] )
        }
        data.frame( nextdt )
    } )


## head( randv.nextdt )

## 14 days of grace period###
daysFromNextDT = as.numeric( Sys.Date() - randv.nextdt$nextdt - 14 )
#daysFromNextDT = as.numeric( as.Date("2015-09-25") - randv.nextdt$nextdt - 14 ) 

randv.nextdt <- cbind( randv.nextdt, daysFromNextDT )
head( randv.nextdt )

### adding onDrug onTrack based on daysFromNextDT
id.bckt.subBckt <- rbind( id.bckt.subBckt, data.frame( pid = randv.nextdt[ randv.nextdt$PATID %in% theRest & randv.nextdt$daysFromNextDT <= 0, c( "PATID" ) ], bckt = "1_OnDrug", subBckt = "A_OnTrack" ) ) 

### adding MissedVisit based on daysFromNextDT
id.bckt.subBckt <-    rbind( id.bckt.subBckt, data.frame( pid = randv.nextdt[ randv.nextdt$PATID %in% theRest & randv.nextdt$daysFromNextDT >  0 & randv.nextdt$daysFromNextDT <= 14, c( "PATID" ) ], bckt = "3_MissedVisit", subBckt = "F_< 2 wks" ) )

id.bckt.subBckt <- rbind( id.bckt.subBckt, data.frame( pid = randv.nextdt[ randv.nextdt$PATID %in% theRest & randv.nextdt$daysFromNextDT >  14 & randv.nextdt$daysFromNextDT <= 28, c( "PATID" ) ], bckt = "3_MissedVisit", subBckt = "G_2 - 4 wks" ) )

id.bckt.subBckt <- rbind( id.bckt.subBckt, data.frame( pid = randv.nextdt[ randv.nextdt$PATID %in% theRest & randv.nextdt$daysFromNextDT >  28, c( "PATID" ) ], bckt = "3_MissedVisit", subBckt = "H_> 4 wks" ) )

id.bckt.subBckt <- id.bckt.subBckt[ order( as.character( id.bckt.subBckt$pid ) ), ]
cat( ">str(id.bckt.subBckt)\n" )
str(id.bckt.subBckt)

cat ("##############################\n")
cat ("###\n")
cat ("### cmp to TA report\n")
cat ("###\n")
cat ("##################################\n")

## ## read TA report
#TAreport <- read.csv( "csvData/TAreport.csv", header = T, stringsAsFactors = F )
TAreport <- read.csv( "csvData/TAreport.csv", header = T, stringsAsFactors = T )
cat( "### all TAreport...\n")
str( TAreport )
taIssues <- merge( id.bckt.subBckt[ id.bckt.subBckt$bckt == "1_OnDrug" , ], TAreport, by.x = c("pid"), by.y = c("PatientID") )
cat( "### TAreport after merging with OnDrug\n" )
str( taIssues )
id.bckt.subBckt$subBckt <- as.character( id.bckt.subBckt$subBckt )


cat( "### Randomized more than 5 wks ago\n" )
length ( which( id.bckt.subBckt$pid %in% taIssues[ grep( "Randomized", taIssues$Problem ) , ]$pid ) )
id.bckt.subBckt[ id.bckt.subBckt$pid %in% taIssues[ grep( "Randomized", taIssues$Problem ) , ]$pid, c("subBckt") ] = c("B_Randomized more than 5 weeks ago")

cat( "### TA Overdues\n" )
length ( which( id.bckt.subBckt$pid %in% taIssues[ taIssues$Problem == "Overdue*", ]$pid ) )
# hold on this.... id.bckt.subBckt[ id.bckt.subBckt$pid %in% taIssues[ taIssues$Problem == "Overdue*" , ]$pid, c("subBckt") ] = c("BA_taOverdue")
taOverdue <- taIssues[ taIssues$Problem == "Overdue*" , ]

taOverdue$DateMostRecentLab <- format( as.Date( taOverdue$DateMostRecentLab ), format = "%m/%d/%Y" )
taOverdue$DateLastVisit <- format( as.Date( taOverdue$DateLastVisit ), format = "%m/%d/%Y" )
taOverdue$DateLastTARun <- format( as.Date( taOverdue$DateLastTARun ), format = "%m/%d/%Y" )
write.csv( taOverdue, "taOverdue.csv")

taIssues <- taIssues[ -1 * grep( "Randomized", taIssues$Problem ), ]
taIssues <- taIssues[ -1 * which( taIssues$Problem == "Overdue*" ), ]

cat( "### TA report after excluding Randomized 5 wks ago\n" )
str( taIssues )

## ## which date are we using? DateLastTARun or DateLastVisit?
## ## Answer : max( taIssues$DateLastVisit, taIssues$DateMostRecentLab ) + 14
## ## therefore : Sys.Date() - max( taIssues$DateLastVisit, taIssues$DateMostRecentLab ) + 14
## ## taDaysDiff <- as.numeric( Sys.Date() - as.Date( tmp$DateLastVisit ) ) ## old and wrong
## ## taDaysDiff <- as.numeric( Sys.Date() - as.Date( taIssues$DateLastTARun ) )

## ## apply( data.frame( as.Date( taIssues$DateLastVisit ), as.Date( taIssues$DateMostRecentLab ) ), 1, max ) # not to use as.Date. instead use as.character. Date would not work with 0000-00-00
## #apply( data.frame( as.character( taIssues$DateLastVisit ), as.character( taIssues$DateMostRecentLab ) ), 1, max ) # ok

taDaysDiff <- as.numeric( Sys.Date() -
#taDaysDiff <- as.numeric( as.Date("2015-09-25") -
                         as.Date( apply( data.frame( as.character( taIssues$DateLastVisit ), as.character( taIssues$DateMostRecentLab ) ), 1, max ) ) 
                         - 14 )

cat (">length( taDaysDiff )\n" )
length( taDaysDiff )

cat (">length( which( taDaysDiff <= 0 ) )\n" )
length( which( taDaysDiff <= 0 ) )

cat (">length( which( taDaysDiff > 0 & taDaysDiff <= 14 ) )\n" )
length( which( taDaysDiff > 0 & taDaysDiff <= 14 ) )

cat (">length( which( taDaysDiff > 14  & taDaysDiff <= 42 ) )\n")
length( which( taDaysDiff > 14  & taDaysDiff <= 42 ) )

cat (">length( which( taDaysDiff > 42  ) \n")
length( which( taDaysDiff > 42  ) )

cat( "### sum of TA issues ---> \n" )
( length( which( taDaysDiff > 0 & taDaysDiff <= 14 ) ) + length( which( taDaysDiff > 14  & taDaysDiff <= 42 ) ) + length( which( taDaysDiff > 42  ) ) )


## aggregate( pid ~ Problem, data = taIssues[ taDaysDiff > 0, ] , length )

taProbs <- taIssues[ taDaysDiff > 0 , ]

taResult <- merge( taProbs, visit.latest[ visit.latest$PATID %in% taProbs$pid, c("PATID", "liaison") ], by.x = "pid", by.y = "PATID")

taResult.tmp <- taResult

taResult.tmp$DateMostRecentLab <- format( as.Date( taResult$DateMostRecentLab ), format = "%m/%d/%Y" )
taResult.tmp$DateLastVisit <- format( as.Date( taResult$DateLastVisit ), format = "%m/%d/%Y" )
taResult.tmp$DateLastTARun <- format( as.Date( taResult$DateLastTARun ), format = "%m/%d/%Y" )

write.csv( taResult.tmp[, c("pid", "liaison", "VisitOfLastTARun", "Problem", "DateMostRecentLab", "DateLastVisit", "DateLastTARun")] , "TAreport_BL2.csv")

id.bckt.subBckt$bckt <- as.character( id.bckt.subBckt$bckt )
id.bckt.subBckt$subBckt <- as.character( id.bckt.subBckt$subBckt )

id.bckt.subBckt[ id.bckt.subBckt$pid %in% taIssues[ taDaysDiff > 0 & taDaysDiff <= 14,]$pid, c("bckt")] <- c("2_TA")
id.bckt.subBckt[ id.bckt.subBckt$pid %in% taIssues[ taDaysDiff > 0 & taDaysDiff <= 14,]$pid, c("subBckt")] <- c("C_< 2 wks")
id.bckt.subBckt[ id.bckt.subBckt$pid %in% taIssues[taDaysDiff > 14 & taDaysDiff <= 42, ]$pid, c("bckt")] <- c("2_TA")
id.bckt.subBckt[ id.bckt.subBckt$pid %in% taIssues[taDaysDiff > 14 & taDaysDiff <= 42, ]$pid, c("subBckt")] <- c("D_2 - 6 wks")
id.bckt.subBckt[ id.bckt.subBckt$pid %in% taIssues[taDaysDiff > 42,]$pid, c("bckt")] <- c("2_TA")
id.bckt.subBckt[ id.bckt.subBckt$pid %in% taIssues[taDaysDiff > 42,]$pid, c("subBckt")] <- c("E_> 6 wks")

cat(">str( id.bckt.subBckt )\n")
str( id.bckt.subBckt )

cat( "### guys trying to catch up....\n" )
visitdt.na <- visit.latest[ is.na( visit.latest$VISITDT ) & visit.latest$PATID %in% id.bckt.subBckt[ id.bckt.subBckt$bckt == "1_OnDrug" , c( "pid" ) ] , ]

visitdt.na.lastVisitdt <- ddply( randv.ta[ randv.ta$PATID %in% visitdt.na$PATID, ], "PATID",
      function( x ) {
        lastVisitdt <-  max( as.character( x$VISITDT ), na.rm = T )
        data.frame( lastVisitdt, daysDiff = as.numeric ( Sys.Date() - as.Date( lastVisitdt ) ) )
        #data.frame( lastVisitdt, daysDiff = as.numeric ( as.Date("2015-09-25") - as.Date( lastVisitdt ) ) )
        # as.Date( lastVisitdt = max( as.character( x$VISITDT ), na.rm = T ) )
      }
      )

## cat( ">length ( which ( visitdt.na.lastVisitdt$daysDiff <= 14 ) )\n" )
## length ( which ( visitdt.na.lastVisitdt$daysDiff <= 14 ) )
## cat( ">length ( which ( visitdt.na.lastVisitdt$daysDiff > 14  & visitdt.na.lastVisitdt$daysDiff <= 28) )\n")
## length ( which ( visitdt.na.lastVisitdt$daysDiff > 14  & visitdt.na.lastVisitdt$daysDiff <= 28) )
## cat(">length ( which ( visitdt.na.lastVisitdt$daysDiff > 28 ) )\n")
## length ( which ( visitdt.na.lastVisitdt$daysDiff > 28 ) )

## id.bckt.subBckt[ id.bckt.subBckt$pid %in% visitdt.na.lastVisitdt[ visitdt.na.lastVisitdt$daysDiff <= 14 , c("PATID") ], c( "bckt" ) ] = c("3_MissedVisit")
## id.bckt.subBckt[ id.bckt.subBckt$pid %in% visitdt.na.lastVisitdt[ visitdt.na.lastVisitdt$daysDiff <= 14 , c("PATID") ], c( "subBckt" ) ] = c("F_< 2 wks")

## id.bckt.subBckt[ id.bckt.subBckt$pid %in% visitdt.na.lastVisitdt[ visitdt.na.lastVisitdt$daysDiff > 14  & visitdt.na.lastVisitdt$daysDiff <= 28, c("PATID") ], c( "bckt" ) ] = c("3_MissedVisit")
## id.bckt.subBckt[ id.bckt.subBckt$pid %in% visitdt.na.lastVisitdt[ visitdt.na.lastVisitdt$daysDiff > 14  & visitdt.na.lastVisitdt$daysDiff <= 28, c("PATID") ], c( "subBckt" ) ] = c("G_2 - 4 wks")

id.bckt.subBckt[ id.bckt.subBckt$pid %in% visitdt.na.lastVisitdt[ visitdt.na.lastVisitdt$daysDiff > 74, c("PATID") ], c( "bckt" ) ] = c("3_MissedVisit")
id.bckt.subBckt[ id.bckt.subBckt$pid %in% visitdt.na.lastVisitdt[ visitdt.na.lastVisitdt$daysDiff > 74, c("PATID") ], c( "subBckt" ) ] = c("H_> 4 wks")

#id.bckt.subBckt[ id.bckt.subBckt$pid %in% visitdt.na.lastVisitdt[ visitdt.na.lastVisitdt$daysDiff > 14  & visitdt.na.lastVisitdt$daysDiff <= 28, c("PATID") ], ] 
id.bckt.subBckt[ id.bckt.subBckt$pid %in% visitdt.na.lastVisitdt[ visitdt.na.lastVisitdt$daysDiff > 28, c("PATID") ], ]

#########################

dbTable <- data.frame( centno = visit.latest[ visit.latest$rand == 1, c("centno") ], id.bckt.subBckt, nextdt = randv.nextdt[ , c( "nextdt" ) ], visit.latest[ visit.latest$rand == 1 , c( "VISIT", "VISITDT", "ONDRUG", "PLAN", "CURRENTDOSE", "NEXTDOSE" ), ]  )

dbTable <- merge( dbTable, sdd.latest[ , c( "PATID", "STOPDT", "STOPTYPE", "RESTART", "STARTDT" ) ], by.x = c( "pid" ), by.y = c( "PATID" ), all.x = T)

##tmp <- merge( visit.latest[ , c( "PATID", "liaison" )], dbTable, by.x = "PATID", by.y = "pid" )
### write.csv( tmp, "dbTable.csv" )
write.csv( dbTable, "pats.csv")

## ########################
## str( dbTable )

## subBckt.all <-  c("A_OnTrack", "B_Randomized more than 5 weeks ago", "C_< 2 wks", "D_2 - 6 wks", "E_> 6 wks", "F_< 2 wks", "G_2 - 4 wks", "H_> 4 wks", "I_< 2 wks", "J_2 - 6 wks", "K_> 6 wks", "L_By Study Drug Discontinuation Form", "M_MI", "N_Stroke", "O_Withdrawn", "P_Adjudicated", "Q_AdjDeath", "R_OtherDeath", "S_Medical Monitor" )

## cur.summary.subBckt <- aggregate( dbTable$pid, by=list( dbTable$subBckt ), FUN = length )
## ( cur.summary.subBckt <- merge( cur.summary.subBckt, data.frame( z = subBckt.all ),  by.y = "z", by.x = "Group.1", all = T) )
## write.csv(cur.summary.subBckt, "BL2_ta_sub.csv")

## ( cur.summary.bckt <- aggregate( dbTable$pid, by=list( dbTable$bckt ), FUN = length ) )
## write.csv( cur.summary.bckt , "BL2_ta_grp.csv")

## ( cur.summary <- rbind(cur.summary.bckt[ 1, ],
##                        cur.summary.subBckt[ c(1, 2), ],
##                        cur.summary.bckt[ 2, ],
##                        cur.summary.subBckt[ c(3:5), ],
##                        cur.summary.bckt[ 3, ],
##                        cur.summary.subBckt[ c(6:8), ],
##                        cur.summary.bckt[ 4, ],
##                        cur.summary.subBckt[ c(9:11), ],
##                        cur.summary.bckt[ 5, ],
##                        cur.summary.subBckt[ c(12:15), ],
##                        cur.summary.bckt[ 6, ],
##                        cur.summary.subBckt[ c(16:19), ]
##                        ) )

## write.csv( cur.summary , "BL2_ta_summary.csv")

### recoverable PermStop by RA
recovPermStop.by.ra <- aggregate( recoverable.PermStop$PATID, by = list( recoverable.PermStop$liaison ), FUN = length )
colnames( recovPermStop.by.ra ) <- c("liaison", "nRecovPermStop")

tempStop.by.ra <- aggregate( tempStop$PATID, by = list( tempStop$liaison ), FUN = length )
colnames( tempStop.by.ra ) <- c("liaison", "nTempStop")

merge( recovPermStop.by.ra, tempStop.by.ra, all = T)
write.csv( merge( recovPermStop.by.ra, tempStop.by.ra, all = T), "nRecovPermStop_nTempStop_by_RA.csv" )


 ##     ##################################################################
 ##     #
 ##     #  DDDD  BBB
 ##     #  D   D B  B
 ##     #  D   D BBB
 ##     #  DDDD  B   B
 ##     #        BBBB
 ##     ##################################################################
 con <- dbConnect(RMySQL::MySQL(), dbname = "", username = "", password = "", host = "")

   ## #############################
   ## ##
   ## ## TAreport DB update
   ## ##
   ## #################################

   dat <- dbGetQuery(con, "TRUNCATE TAReports")

   cat("**** TA report - DB update **********\n")

### just for display only, add all overdue TA's after intersecting with 
## TAreport$PatientID[ grep( "Overdue", TAreport$Problem) ]
## for ( i in taResult$pid ) {
for ( i in intersect( theRest, TAreport$PatientID[ grep( "Overdue", TAreport$Problem) ] ) ) {
    x <- TAreport[ TAreport$PatientID == i, ]
    SQL <- paste( "INSERT TAReports (PatientID, VisitOfLastTARun, Problem, DateMostRecentLab, DateLastVisit, DateLastTARun) VALUES ('", x$PatientID, "', '", x$VisitOfLastTARun, "', '", x$Problem, "', '", x$DateMostRecentLab, "', '", x$DateLastVisit, "', '", x$DateLastTARun, "');" , sep ="")
    ## print (SQL)
    dbGetQuery( con, SQL )
}



 dat <- dbGetQuery(con, "UPDATE TAReports SET SiteID = LEFT(PatientID, 7)")

   #############################
   ##
   ## TempStop DB update
   ##
   #################################

   cat("**** TempStop - DB update **********\n")

   dat <- dbGetQuery(con, "SELECT PatientID FROM TempStops WHERE isCurrent = 1")

   cat("**** Old ones from DB **********\n")

### getting rid of those who out of tempStops
   ( tmp <- setdiff( dat$PatientID, rand.tempStop.ids ) )
  str( tmp )

   for( i in tmp ) {
     SQL <- paste("update TempStops set isCurrent = 0 where PatientID ='", i, "'", sep = "")
     # print ( SQL )
     dbGetQuery( con, SQL )
   }

### For existng ones, need to update comments from tempStop
###psuedo code
( tmp <- intersect( tempStop.sdd$PATID, dat$PatientID ) )
for ( i in tmp ) {
### tempStop.sdd
    cur <- tempStop.sdd[ tempStop.sdd$PATID == i, ]
### update comments/reason from tempStop.csv - concat( OTHERREAS_TXT, AEREASON )
    SQL <- paste( "update TempStops set Comments = \"", cur$OTHERREAS_TXT, "\" where PatientID = '", cur$PATID, "' and isCurrent = 1;", sep = "" )
    print( SQL )
    dbGetQuery( con, SQL )
}

### to add new ones
   cat("**** New ones **********\n")
   ( tmp <- setdiff( rand.tempStop.ids, dat$PatientID ) )
   ( tmp2 <- merge( as.data.frame( tmp ), tempStop, by.x = c( "tmp" ), by.y =( "PATID" ) ) )

   for( i in tmp2$tmp ) {
     x <- tmp2[ tmp2$tmp == i , ]
     ##SQL <- paste("insert TempStops (PatientID, SiteID, StopDate, Comments, MostRecentVisitDate) values ('", x$tmp, "', '", x$centno, "', '", as.Date( x$STOPDT, format = "%d-%b-%Y" ), "', \"", x$OTHERREAS_TXT, "\", '", as.Date( x$VISITDT, format = "%d-%b-%Y"), "')",  sep = "")
     SQL <- paste("insert TempStops (PatientID, SiteID, StopDate, Comments, MostRecentVisitDate, Created) values ('", x$tmp, "', '", x$centno, "', '", as.Date( x$STOPDT, format = "%d-%b-%Y" ), "', \"", x$OTHERREAS_TXT, "\", '", as.Date( x$VISITDT, format = "%d-%b-%Y"), "', NULL)",  sep = "")
     # print ( SQL )
     dbGetQuery( con, SQL )
   }

   #############################
   ##
   ## recoverable Perm Stop DB update
   ##
   #################################

   cat("**** recoverable Perm Stop - \"discont\" - DB update **********\n")

  dat <- dbGetQuery(con, "SELECT PatientID FROM RecoverablePermStops WHERE isCurrent = 1")
  str(dat)

   ##str( rand.recoverable.PermStop.ids )
   str( rand.after.wdraw.ids )

   cat("**** Old ones from DB **********\n")

   ( tmp <- setdiff( dat$PatientID, rand.after.wdraw.ids ) )

   for( i in tmp ) {
     SQL <- paste("update RecoverablePermStops set isCurrent = 0 where PatientID ='", i, "'", sep = "")
     # print ( SQL )
     dbGetQuery( con, SQL )
   }

   cat("**** New ones **********\n")

   ( tmp <- setdiff( rand.after.wdraw.ids, dat$PatientID ) )
   ( tmp2 <- merge( as.data.frame( tmp ), recoverable.PermStop, by.x = c("tmp"), by.y = ("PATID") ) )

   for( i in tmp2$tmp ) {
     x <- tmp2[ tmp2$tmp == i , ]
     SQL <- paste("insert RecoverablePermStops (PatientID, SiteID, StopDate, Comments, MostRecentVisitDate, Created) values ('", x$tmp, "', '", x$centno, "', '", as.Date( x$STOPDT, format = "%d-%b-%Y" ), "', \"", x$OTHERREAS_TXT, "\", '", as.Date( x$VISITDT, format = "%d-%b-%Y"), "', NULL)",  sep = "")
     ## print ( SQL )
     dbGetQuery( con, SQL )
   }


 ######
 ### getting previous week data
  prev <- dbGetQuery(con, "SELECT * FROM PatientStatus WHERE isCurrent = 1")
  dbGetQuery(con, "update PatientStatus set isCurrent = 0;")

 cur <- read.csv("pats.csv", header = T)
 for ( i in  cur$pid ) {
       x <- cur[ cur$pid == i, ]
       SQL <- paste( "INSERT PatientStatus (pid, centno, bckt, subBckt, nextdt, VISIT, VISITDT, ONDRUG, PLAN, CURRENTDOSE, NEXTDOSE, STOPDT, STOPTYPE, RESTART, STARTDT, isCurrent) VALUES ('", x$pid, "', '", x$centno, "', '", x$bckt, "', '", x$subBckt, "', '", x$nextdt, "', '", x$VISIT, "', '", x$VISITDT, "', '", x$ONDRUG,"', '", x$PLAN,"', '", x$CURRENTDOSE,"', '", x$NEXTDOSE,"', '", x$STOPDT, "', '", x$STOPTYPE,  "', '", x$RESTART,  "', '", x$STARTDT, "', 1);" , sep ="")
       ##print (SQL)
       dbGetQuery( con, SQL )
   }


## ### update tempStop most recent visitdt
## qqqqqq <- visit.latest[ visit.latest$PATID %in% tempStop$PATID, c("PATID", "VISITDT") ]
## qqqqqq <- na.omit ( qqqqqq )
## for ( i in qqqqqq$PATID ) {
##     x <- qqqqqq[ qqqqqq$PATID == i, ]
##     SQL <- paste( " update TempStops set MostRecentVisitDate = '" , x$VISITDT,  "' where isCurrent = 1 and PatientID = '", x$PATID, "';", sep = "")
##     #print ( SQL )
##     dbGetQuery( con, SQL )
## }
## No need to do this. Now updates daily.


 ######
 ### missed visit output

 missed <- dbGetQuery( con, "select * from ( SELECT SiteID, Liaison  FROM `SiteMetrics` WHERE `isThisMostRecent` = 1 ) as t
 join 
 ( SELECT pid, centno, bckt, subBckt  FROM `PatientStatus` WHERE `bckt` LIKE '%Missed%' AND `isCurrent` = 1 ) as tt
 on t.SiteID = tt.centno; " )

 write.csv( missed, "missed.csv")

 dbDisconnect( con )




 #####################
 # prev <- read.csv("csvData/prev.csv", header = T)

 subBckt.all <-  c("A_OnTrack", "B_Randomized more than 5 weeks ago", "C_< 2 wks", "D_2 - 6 wks", "E_> 6 wks", "F_< 2 wks", "G_2 - 4 wks", "H_> 4 wks", "I_< 2 wks", "J_2 - 6 wks", "K_> 6 wks", "L_By Study Drug Discontinuation Form", "M_MI", "N_Stroke", "O_Withdrawn", "P_Adjudicated", "Q_AdjDeath", "R_OtherDeath", "S_Medical Monitor" )

 cur.summary.subBckt <- aggregate( cur$pid, by=list( cur$subBckt ), FUN = length )
 cur.summary.subBckt <- merge( data.frame( z = subBckt.all ), cur.summary.subBckt,   by.x = "z", by.y = "Group.1", all = T)
 cur.summary.subBckt[is.na(cur.summary.subBckt)] <- 0
 names( cur.summary.subBckt ) <- c("Group.1","x")

 cur.summary.bckt <- aggregate( cur$pid, by=list( cur$bckt ), FUN = length )

 ( cur.summary <- rbind( data.frame( Group.1= "all_rand", x = dim(cur)[1] ),
                      cur.summary.bckt[ 1, ],
                        cur.summary.subBckt[ c(1, 2), ],
                        cur.summary.bckt[ 2, ],
                        cur.summary.subBckt[ c(3:5), ],
                        cur.summary.bckt[ 3, ],
                        cur.summary.subBckt[ c(6:8), ],
                        cur.summary.bckt[ 4, ],
                        cur.summary.subBckt[ c(9:11), ],
                        cur.summary.bckt[ 5, ],
                        cur.summary.subBckt[ c(12:15), ],
                        cur.summary.bckt[ 6, ],
                        cur.summary.subBckt[ c(16:19), ]
                      )
  )

 prev.summary.subBckt <- aggregate( prev$pid, by=list( prev$subBckt ), FUN = length )
 prev.summary.subBckt <- merge( data.frame( z = subBckt.all ), prev.summary.subBckt,   by.x = "z", by.y = "Group.1", all = T)
 prev.summary.subBckt[is.na(prev.summary.subBckt)] <- 0
 names( prev.summary.subBckt ) <- c("Group.1","x")
 prev.summary.bckt <- aggregate( prev$pid, by=list( prev$bckt ), FUN = length )

 ( prev.summary <- rbind(data.frame( Group.1= "all_rand", x = dim(prev)[1] ),
                       prev.summary.bckt[ 1, ],
                        prev.summary.subBckt[ c(1, 2), ],
                        prev.summary.bckt[ 2, ],
                        prev.summary.subBckt[ c(3:5), ],
                        prev.summary.bckt[ 3, ],
                        prev.summary.subBckt[ c(6:8), ],
                        prev.summary.bckt[ 4, ],
                        prev.summary.subBckt[ c(9:11), ],
                        prev.summary.bckt[ 5, ],
                        prev.summary.subBckt[ c(12:15), ],
                        prev.summary.bckt[ 6, ],
                        prev.summary.subBckt[ c(16:19), ]
                        )
 )

 cur.percent <- round( cur.summary$x / cur.summary$x[1] * 100, 2)
 prev.percent <- round( prev.summary$x / prev.summary$x[1] * 100, 2 )
 delta <- cur.summary$x - prev.summary$x

 write.csv( cbind( prev.summary, prev.percent, cur.summary$x, cur.percent, delta), "prev_cur.csv")

 #### TempStop Comparison

 curTempStop <- cur[ cur$bckt == "4_TempStop", ]
 # head( curTempStop )

 cur.us.ts <- length( grep( "US", curTempStop$pid ) )
 cur.ca.ts <- length( grep( "CA", curTempStop$pid ) )
 cur.ts <- dim( curTempStop )[1]

 prevTempStop <- prev[ prev$bckt == "4_TempStop", ]
 # head( prevTempStop )

 prev.us.ts <- length( grep( "US", prevTempStop$pid ) )
 prev.ca.ts <- length( grep( "CA", prevTempStop$pid ) )
 prev.ts <- dim( prevTempStop )[1]

 # cat("gone (minus)\n")
 off.from.ts <- length( oldTempStop.pid <- setdiff( prevTempStop$pid, curTempStop$pid ) )
 ## str( oldTempStop.pid <- setdiff( prevTempStop$pid, curTempStop$pid ) )
 #length( grep( "CA", oldTempStop.pid ) )
 #length( grep( "US", oldTempStop.pid ) )

 # cat("new (plus)\n")
 on.to.ts <- length( newTempStop.pid <- setdiff( curTempStop$pid, prevTempStop$pid ) )

 ## str( newTempStop.pid <- setdiff( curTempStop$pid, prevTempStop$pid ) )
 #length( grep( "CA", newTempStop.pid ) )
 #length( grep( "US", newTempStop.pid ) )

 # str( sdd.latest[ sdd.latest$PATID %in% newTempStop.pid, ] )

 #### new TempStop reasons

 ### ta_nextdose_Zero cases
 ta.nextdose.Zero.pid <- intersect( newTempStop.pid, rand.after.sdd.tempStop.ids )
 ta.nextdose.Zero <- data.frame( PATID = ta.nextdose.Zero.pid, OTHERREAS_TXT = "TA Next Dose Zero" )

 ### sdd stop type temp stop cases
 sdd.tempStop.pid <- intersect( newTempStop.pid, rand.sdd.stoptype.TempStop.patid )

 sdd.tempStop <- sdd.latest[ sdd.latest$PATID %in% intersect( newTempStop.pid, rand.sdd.stoptype.TempStop.patid ), ]

 ### merge two cases above
 temp <- rbind.fill( sdd.tempStop, ta.nextdose.Zero )

 write.csv( temp, "newTempStopCases.csv" )
 #### Recoverable PermStop Comparison

 curRecovPS <- cur[ cur$bckt == "5_recoverable PermStop", ]
 cat("Recov Perm Stop\n")
 cur.us.recPS <- length( grep( "US", curRecovPS$pid ) )
 cur.ca.recPS <- length( grep( "CA", curRecovPS$pid ) )
 cur.recPS <- dim( curRecovPS )[1]

 prevRecovPS <- prev[ prev$bckt == "5_recoverable PermStop", ]
 prev.us.recPS <- length( grep( "US", prevRecovPS$pid ) )
 prev.ca.recPS <- length( grep( "CA", prevRecovPS$pid ) )
 prev.recPS <- dim( prevRecovPS )[1]

 newRecovPS.pid <- setdiff( curRecovPS$pid, prevRecovPS$pid )
 on.to.recPS <- length( newRecovPS.pid )

 #length( grep( "CA", newRecovPS.pid ) )
 #length( grep( "US", newRecovPS.pid ) )

 recovPS.sdd.pid <- intersect( newRecovPS.pid, rand.after.wdraw.ids )
 sdd.recovPS <- sdd.latest[ sdd.latest$PATID %in% recovPS.sdd.pid, ]
 write.csv( sdd.recovPS, "newRecovPS_SDD.csv" )

 oldRecovPS.pid <- setdiff( prevRecovPS$pid, curRecovPS$pid )
 off.from.recPS <- length( oldRecovPS.pid )

 # cur[ cur$pid %in% newRecovPS.pid, ]
 #length( grep( "CA", oldRecovPS.pid ) )
 #length( grep( "US", oldRecovPS.pid ) )

 write.csv( cur[ cur$pid %in% newRecovPS.pid, ], "newRecovPS.csv" )

 ### pull REASON_TXT from bckt_wdraw.csv here. !! to do !!
 wdraw <- read.csv("csvData/bckt_wdraw.csv", header = T, stringsAsFactors = T)
 curWdrawn <- cur[ cur$subBckt == "O_Withdrawn", ]
 str( curWdrawn )
 prevWdrawn <- prev[ prev$subBckt == "O_Withdrawn", ]
 str( prevWdrawn )

 newWdrawn.pid <- setdiff( curWdrawn$pid, prevWdrawn$pid )
 # wdraw.info <- wdraw[ wdraw$PATID %in% curWdrawn$pid, c("PATID", "WITHD_TYPE", "WITHDT", "REASON", "REASON_TXT" ) ]
 wdraw.info <- wdraw[ wdraw$PATID %in% newWdrawn.pid, c("PATID", "WITHD_TYPE", "WITHDT", "REASON", "REASON_TXT" ) ]

 write.csv( wdraw.info, "wdrawReference.csv")

 #### Non-Recoverable PermStop Comparison

 curNonRecovPS <- cur[ cur$bckt == "6_non-recoverable PermStop", ]
 cur.us.nrPS <- length( grep( "US", curNonRecovPS$pid ) )
 cur.ca.nrPS <- length( grep( "CA", curNonRecovPS$pid ) )
 cur.nrPS <- dim( curNonRecovPS )[1]

 prevNonRecovPS <- prev[ prev$bckt == "6_non-recoverable PermStop", ]

 prev.us.nrPS <- length( grep( "US", prevNonRecovPS$pid ) )
 prev.ca.nrPS <- length( grep( "CA", prevNonRecovPS$pid ) )
 prev.nrPS <- dim( prevNonRecovPS )[1]

 oldNonRecovPS.pid <- setdiff( prevNonRecovPS$pid, curNonRecovPS$pid )
 off.from.nrPS <- length( oldNonRecovPS.pid )

 ##length( grep( "CA", oldNonRecovPS.pid ) )
 #length( grep( "US", oldNonRecovPS.pid ) )

 newNonRecovPS.pid <- setdiff( curNonRecovPS$pid, prevNonRecovPS$pid )
 on.to.nrPS <- length( newNonRecovPS.pid )

 #length( grep( "CA", newNonRecovPS.pid ) )
 #length( grep( "US", newNonRecovPS.pid ) )

 ## cur[ cur$pid %in% newNonRecovPS.pid, ]
 write.csv( cur[ cur$pid %in% newNonRecovPS.pid, ], "newNonRecovPS.csv")

 ###############

 cur.us <- c( cur.us.ts, cur.us.recPS, cur.us.nrPS )
 cur.ca <- c( cur.ca.ts, cur.ca.recPS, cur.ca.nrPS )
 cur.total <- c( cur.ts, cur.recPS, cur.nrPS )
 cur.stops <- cbind( cur.us, cur.ca, cur.total )

 prev.us <- c( prev.us.ts, prev.us.recPS, prev.us.nrPS )
 prev.ca <- c( prev.ca.ts, prev.ca.recPS, prev.ca.nrPS )
 prev.total <- c( prev.ts, prev.recPS, prev.nrPS )
 prev.stops <- cbind( prev.us, prev.ca, prev.total )

 delta.cur.prev.stops <- cur.stops - prev.stops

 tmp <- cbind( cur.stops, delta.cur.prev.stops, prev.stops )

 tmp <- rbind( colSums( tmp ), tmp )

 rownames( tmp ) <- c( "Sum", "TempStop", "recPermStop", "non-recPermStop")

 write.csv( tmp, "cur.prev.stops.csv")

 cat( off.from.ts, " came off temp stops\n" , file = "output.csv", sep=" ", append = T )
 cat( on.to.ts, " came on temp stops\n" , file = "output.csv", sep=" ", append = T )
 cat( off.from.recPS, " came off rec perm stops\n" , file = "output.csv", sep=" ", append = T )
 cat( on.to.recPS, " came on recov perm stops\n" , file = "output.csv", sep=" ", append = T )
 cat( off.from.nrPS, " came off non rec perm stops\n" , file = "output.csv", sep=" ", append = T )
 cat( on.to.nrPS, " came on non rec perm stops\n" , file = "output.csv", sep=" ", append = T )


 ## ###

 ## curAdj <- cur[ cur$subBckt == "P_Adjudicated", ]
 ## str( curAdj )

 ## prevAdj <- prev[ prev$subBckt == "P_Adjudicated", ]
 ## str( prevAdj )

 ## ( newAdj.pid <- setdiff( curAdj$pid, prevAdj$pid ) )

 ## levels(cur$bckt)
 ## [1] "1_OnDrug"                   "2_TA"                      
 ## [3] "3_MissedVisit"              "4_TempStop"                
 ## [5] "5_recoverable PermStop"     "6_non-recoverable PermStop"
 ## >


 ##########################################################
 ### crosstab movement from last week to this week
 ##########################################################

 tmp.bckt.all <- c("1_OnDrug", "4_TempStop", "5_recoverable PermStop", "6_non-recoverable PermStop")

 ## tmp <- cur[ cur$pid %in%  prev[prev$bckt == "6_non-recoverable PermStop", c('pid')], c("pid", "bckt") ]
 ## t <- aggregate( tmp$pid, by=list( tmp$bckt ), FUN = length )
 ## t <- merge( data.frame( z = tmp.bckt.all ), t, by.x = "z", by.y = "Group.1", all= T)
 ## colnames( t ) <- c("category","lastWkNonRecPS")

 ## tmp <- cur[ cur$pid %in%  prev[prev$bckt == "5_recoverable PermStop", c('pid')], c("pid", "bckt") ]
 ## tt <- aggregate( tmp$pid, by=list( tmp$bckt ), FUN = length )
 ## tt <- merge( data.frame( z = tmp.bckt.all ), tt, by.x = "z", by.y = "Group.1", all= T)
 ## colnames( tt ) <- c("category","lastWkRecPS")

 ## tmp <- cur[ cur$pid %in%  prev[prev$bckt == "4_TempStop", c('pid')], c("pid", "bckt") ]
 ## ttt<- aggregate( tmp$pid, by=list( tmp$bckt ), FUN = length )
 ## ttt <- merge( data.frame( z = tmp.bckt.all ), ttt, by.x = "z", by.y = "Group.1", all= T)
 ## colnames( ttt ) <- c("category","lastWktempStop")

 ## tmp <- cur[ cur$pid %in%  prev[ which( prev$bckt == "1_OnDrug" || prev$bckt == "2_TA" || prev$bckt== "3_MissedVisit") , c('pid')], c("pid", "bckt") ]

 ## tmp <- cur[ cur$pid %in%  prev[  prev$bckt %in% c("1_OnDrug", "2_TA", "3_MissedVisit") , c('pid')], c("pid", "bckt") ]
 ## aggregate( tmp$pid, by=list( tmp$bckt ), FUN = length )


 ## prev$bckt == "1_OnDrug"
 ## prev$bckt == "2_TA"
 ## prev$bckt == "3_MissedVisit"

 ## intersect( prev[prev$bckt %in% c("1_OnDrug", "2_TA", "3_MissedVisit") , c('pid')], cur[cur$bckt == "5_recoverable PermStop", c("pid")] )
 ## prev[ prev$pid %in% intersect( prev[prev$bckt %in% c("1_OnDrug", "2_TA", "3_MissedVisit") , c('pid')], cur[cur$bckt == "5_recoverable PermStop", c("pid")] ), ]
 ## dbTable[ dbTable$pid %in% intersect( prev[prev$bckt %in% c("1_OnDrug", "2_TA", "3_MissedVisit") , c('pid')], cur[cur$bckt == "5_recoverable PermStop", c("pid")] ), ]
 ## prev[ prev$pid %in% intersect( prev[prev$bckt == "4_TempStop" , c('pid')], cur[cur$bckt == "5_recoverable PermStop", c("pid")] ), ]
 ## dbTable[ dbTable$pid %in% intersect( prev[prev$bckt == "4_TempStop" , c('pid')], cur[cur$bckt == "5_recoverable PermStop", c("pid")] ), ]
 ## prev[ prev$pid %in% intersect( prev[prev$bckt %in% c("1_OnDrug", "2_TA", "3_MissedVisit") , c('pid')], cur[cur$bckt == "6_non-recoverable PermStop", c("pid")] ), ]
 ## dbTable[ dbTable$pid %in% intersect( prev[prev$bckt %in% c("1_OnDrug", "2_TA", "3_MissedVisit") , c('pid')], cur[cur$bckt == "6_non-recoverable PermStop", c("pid")] ), ]
 ## prev[ prev$pid %in% intersect( prev[prev$bckt == "5_recoverable PermStop" , c('pid')], cur[cur$bckt == "6_non-recoverable PermStop", c("pid")] ), ]
 ## dbTable [ dbTable$pid == intersect( prev[prev$bckt == "5_recoverable PermStop" , c('pid')], cur[cur$bckt == "6_non-recoverable PermStop", c("pid")] ), ]
 ## OnDRUG.TS <- intersect( prev[ prev$bckt %in% c("1_OnDrug", "2_TA", "3_MissedVisit"), c('pid')], cur[cur$bckt == "4_TempStop", c("pid")] )
 ## recPS.TS <- intersect( prev[prev$bckt == "5_recoverable PermStop" , c('pid')], cur[cur$bckt == "4_TempStop", c("pid")] )

 prev.mod <- prev
 prev.mod[ prev.mod$bckt == "2_TA", ]$bckt = "1_OnDrug"
 prev.mod[ prev.mod$bckt == "3_MissedVisit", ]$bckt = "1_OnDrug"
 cur.mod <- cur
 cur.mod[ cur.mod$bckt == "2_TA", ]$bckt = "1_OnDrug"
 cur.mod[ cur.mod$bckt == "3_MissedVisit", ]$bckt = "1_OnDrug"

 tmp <- cur.mod[ cur.mod$pid %in%  prev.mod[prev.mod$bckt == "6_non-recoverable PermStop", c('pid')], c("pid", "bckt") ]
 t <- aggregate( tmp$pid, by=list( tmp$bckt ), FUN = length )
 t <- merge( data.frame( z = tmp.bckt.all ), t, by.x = "z", by.y = "Group.1", all= T)
 colnames( t ) <- c("category","lastWkNonRecPS")

 tmp <- cur.mod[ cur.mod$pid %in%  prev.mod[prev.mod$bckt == "5_recoverable PermStop", c('pid')], c("pid", "bckt") ]
 tt <- aggregate( tmp$pid, by=list( tmp$bckt ), FUN = length )
 tt <- merge( data.frame( z = tmp.bckt.all ), tt, by.x = "z", by.y = "Group.1", all= T)
 colnames( tt ) <- c("category","lastWkRecPS")

 tmp <- cur.mod[ cur.mod$pid %in%  prev.mod[prev.mod$bckt == "4_TempStop", c('pid')], c("pid", "bckt") ]
 ttt<- aggregate( tmp$pid, by=list( tmp$bckt ), FUN = length )
 ttt <- merge( data.frame( z = tmp.bckt.all ), ttt, by.x = "z", by.y = "Group.1", all= T)
 colnames( ttt ) <- c("category","lastWktempStop")

 tmp <- cur.mod[ cur.mod$pid %in%  prev.mod[ which( prev.mod$bckt == "1_OnDrug" ) , c('pid')], c("pid", "bckt") ]

 #tmp <- cur[ cur$pid %in%  prev[  prev$bckt %in% c("1_OnDrug", "2_TA", "3_MissedVisit") , c('pid')], c("pid", "bckt") ]
 #aggregate( tmp$pid, by=list( tmp$bckt ), FUN = length )

 ######

 tmp <- cur.mod[ cur.mod$pid %in%  prev.mod[  prev.mod$bckt %in% c("1_OnDrug") , c('pid')], c("pid", "bckt") ]
 tttt <- aggregate( tmp$pid, by=list( tmp$bckt ), FUN = length )
 tttt <- merge( data.frame( z = tmp.bckt.all ), tttt, by.x = "z", by.y = "Group.1", all= T)
 colnames( tttt ) <- c("category","lastWkOnDrug")

 crossTab <- t(cbind( tttt$lastWkOnDrug, ttt$lastWktempStop, tt$lastWkRecPS, t$lastWkNonRecPS ) )
 colnames( crossTab ) <- c("ThisWkOnDrug","ThisWkTempStop","ThisWkRecPermStop","ThisWkNonRecPermStop")
 rownames( crossTab )<- c("lastWkOnDrug","lastWkTempStop","lastWkRecPermStop","lastWkNonRecPermStop")

 write.csv( crossTab, "crossTab.csv")

 prev.cur <- merge(prev.mod, cur.mod, by = "pid")
 tmp <- prev.cur[ prev.cur$bckt.x != prev.cur$bckt.y , c("pid", "bckt.x", "bckt.y")]
 colnames(tmp) <- c("pid", "lastWk", "curWk")
 write.csv(tmp, "lastWk_curWk_status_change.csv")

q( save = "no")



## ########################
##        ## for( i in tmp ) {
##        ##   SQL <- paste("update TempStops set isCurrent = 0 where PatientID ='", i, "'", sep = "")
##        ##   # print ( SQL )
##        ##   dbGetQuery( con, SQL )
## ## }

## qqqqqq <- visit.latest[ visit.latest$PATID %in% tempStop$PATID, c("PATID", "VISITDT") ]
## qqqqqq <- na.omit ( qqqqqq )
## for ( i in qqqqqq$PATID ) {
##     x <- qqqqqq[ qqqqqq$PATID == i, ]
##     SQL <- paste( " update TempStops set MostRecentVisitDate = '" , x$VISITDT,  "' where isCurrent = 1 and PatientID = '", x$PATID, "';", sep = "")
##     print ( SQL )
## }
