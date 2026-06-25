########################################################################## 
#  
#  LINGRA (LINtul GRAss)  
#
#  LINGRA is simple model to calculate grass growth and production under 
#  potential and water limited conditions. Simulated key processes are 
#  light utilization, leaf formation, leaf elongation, tillering and 
#  carbon partitioning to roots and shoots. Source and sink limited growth 
#  are simulated independently. Sink-limited growth is characterized by 
#  temperature-dependent leaf expansion and tiller development, whereas 
#  source-limited growth is determined by photosynthetic light-use 
#  efficiency of the canopy and th remobilization of stored carbohydrates 
#  in the stubble. At each integration step of 1 day, the available amount 
#  of carbon from the source is compared with the carbon required by the 
#  sink. The actula growth is determined by the minimum value of either 
#  sink or source.
#
#  The LINGRA Source code (in FST), and a model description can be found 
#  at the model and data portal of the Plant Production Systems group of   
#  Wageningen University, The Netherlands:
#  
#  http://models.pps.wur.nl/content/lingra-model-simple-grass-model-
#  potential-and-water-limited-conditions
#
#  Weather data for the LINGRA model are also available on the model and 
#  data portal:
#  http://models.pps.wur.nl/content/weather-file-collection-0
#
#  Contact
#  Author(s): Joost Wolf, Ad Schapendonk
#  Address: Droevendaalse steeg 1, Wageningen
#  Email:
#   joost.wolf@wur.nl
#  Website:
#  Wageningen University, Plant Production Systems group
#   http://www.pps.wur.nl/UK/ 
#
##########################################################################

#  LINGRA in FST requires a Fortran compiler and FST (Fortran Simulation 
#  Translator). FST is a simple programming language that is converted 
#  into the Fortran language.
#
#  The LINGRA model (LINGRATW-new-JRC.fst) was re-written in the open
#  source R programming language by Aart van der Linden 
#  (aart.vanderlinden@wur.nl) in April 2014  


#  Start LINGRA model (LINGRATW-new-JRC.fst):

#* TITLE LINGRA-new-JRC

#* Version adapted for rye grass April 4, 2006, Joost Wolf for
#* FST modelling;
#* Model is based on LINGRA model in CGMS
#* for forage growth and production simulation which was written FORTRAN
#* (GRSIM.pfo) and later in C; Application of model is the
#* Simulation of perennial ryegrass (L. perenne) growth under both
#* potential and water-limited growth conditions.
#*
#* Model is different from LINGRA model in CGMS with respect to:
#*  1) evaporation, transpiration, water balance, root depth growth
#*  and growth reduction by drought stress (TRANRF) are derived from LINGRA
#*  model for thimothee (e.g. subroutine PENMAN, EVAPTR and DRUNIR)
#*  2) running average to calculate soil temperature (SOITMP) is derived from
#*  approach in LINGRA model for thimothee
#*
#*
#*                    name    type     description                               unit
#* Variables:
#*     Biomass in leaves, reserves, etc. in kg DM / ha
#*     Terms of water balance in mm/day
#*
#************************************************************************
#***   1. Initial conditions and run control
#************************************************************************

#INITIAL

ROOTDI  = 0.4
LAII    = 0.1  

TILLI   = 7000.
WREI    = 200.
WRTI    = 4.

#************************************************************************
#***   10. Functions and parameters for grass
#************************************************************************
  
#* Parameters
CO2A   = 360.     # Atmospheric CO2 concentration (ppm)
KDIF   = 0.60     # 
LAICR  = 4.       # Critical LAI 
TBASE  = 0.        
LUEMAX = 3.0      # Light use efficiency (g DM MJ-1 PAR intercepted)
IMOPT  = 1.       # Grass cut at specific weight (1); Grass cut at specific dates (2)
INCUT  = NULL; INCUT[1] = 0.
SLA    = 0.0025   # Specific leaf area (m2 g-1 DM)
CLAI   = 0.8      # LAI after cutting (-)
NITMAX = 3.34     # Maximum nitrogen content (%)
NITR   = 3.34     # Actual nitrogen content (%)
RDRD   = 0.01     # Base death rate (fraction)
TMBAS1 = 3.       # Base temperature perennial ryegrass (degrees Celsius)
CWGHT  = 1800.    # Cutting weight grass under IMOPT = 1

#* Parameters for water relations from LINGRA for thimothee
DRATE  = 50.      # Drainage rate (mm day-1)
IRRIGF = 1.       # Irrigation (0 = no irrigation till 1 = full irrigation)
ROOTDM = 0.4      # Maximum root depth (m)
RRDMAX = 0.012    # Maximum root growth (m day-1)
WCAD   = 0.005    # Air dry water content (fraction) 
WCWP   = 0.12     # Wilting point water content (fraction) 
WCFC   = 0.29     # Field capacity water content (fraction)
WCI    = 0.29     # Initial water content (fraction)
WCWET  = 0.37     # Minimum water content at water logging (fraction) 
WCST   = 0.41     # Saturation water content (fraction)

PI     = 3.1416
RAD    = PI / 180.

FINTIM = 365      # Number of days in simulation (days)

#* Harvest dates
MNDAT <- c(135.,165.,200.,240.,280.)  # Harvest days in the simulation (Only used if IMOPT = 2)
MNDAT0 <- rep(0, FINTIM)

MNDAT0[MNDAT[1]] <- 1; MNDAT0[MNDAT[2]] <- 1; MNDAT0[MNDAT[3]] <- 1
MNDAT0[MNDAT[4]] <- 1; MNDAT0[MNDAT[5]] <- 1; MNDAT0[MNDAT[6]] <- 1

#      Initial available water (mm)
WAI     = 1000. * ROOTDI * WCI
#*     Initial leaf weight is initialized as initial
#*     leaf area divided by initial specific leaf area, kg ha-1
WLVGI   = LAII / SLA

#*     Remaining leaf weight after cutting is initialized at remaining
#*     leaf area after cutting divided by initial specific leaf area, kg ha-1
CWLVG = CLAI/SLA
#*      Maximum site filling new buds (FSMAX) decreases due
#*      to low nitrogen contents, Van Loo and Schapendonk (1992)
#*      Theoretical maximum tillering size = 0.693
FSMAX = NITR/NITMAX*0.693

#************************************************************************
#***   2. Environmental data
#************************************************************************

# Weather file (New users: adapt file directory)
WEATHERCNTR <- NULL
WEATHERCNTR <-read.csv(file="C:/R/NLD1999.csv",head=TRUE,sep=",")

LAT  <- 52                      # Latitude (used to calculate daylength;
                                # daylength not taken into account in 
                                # LINGRA, latitude does not affect yield)
YEAR <- WEATHERCNTR$YR          # Year in weather file
DOY  <- WEATHERCNTR$DOY         # Doy of the year
TMMN <- WEATHERCNTR$MINT        # Minimum temperature (degrees Celsius)
TMMX <- WEATHERCNTR$MAXT        # Maximum temperature (degrees Celsius)
RDD  <- WEATHERCNTR$RAD         # Solar radiation (kJ m-2 day-1)
VP   <- WEATHERCNTR$VPR         # Water vapour pressure (kPa)
WN   <- WEATHERCNTR$WIND        # Average wind speed (m s-1)
RAIN <- WEATHERCNTR$RAIN        # Daily rainfall (mm day-1)
  
#************************************************************************
#***   11. Data
#************************************************************************

LUERD1 <- c(-20.,    0.,  3.,  0.,  8.,  1., 40.,   1.) # Remnant Fortran code, not used in R
LUERD2 <- c(0.,  1.,  10.,  1.,  40.,  0.33)            # Remnant Fortran code, not used in R
FRRTTB <- c(-10., 0.263, 0., 0.263, 1., 0.165)          # Remnant Fortran code, not used in R

# Specifying variables for R:
DAVTMP       = NULL;   DAVTMP       = 0.5 * (TMMN + TMMX)
PHOTMP       = NULL;   PHOTMP       = (TMMN + 3. * TMMX)/4.
DEC          = NULL
DECC         = NULL
DAYL         = NULL
DTR          = NULL;   DTR          = RDD / 1.E+3
PARAV        = NULL
RSOITM       = NULL
SOITMP       = NULL;   SOITMP[1]    = 5. 
EFFTMP       = NULL;   EFFTMP       = max(DAVTMP, TBASE)

# Rate variables

REDTMP       = NULL  # Reduction in light use efficiency due to temperature
REDRDD       = NULL  # Reduction in light use efficiency due to solar radiation
TMEFF        = NULL  # Effective temperature (daily temperature above 0 degrees Celsius)   
PAR          = NULL  # Photosynthetic Active Radiation (MJ PAR m-2 day-1)
FINT         = NULL  # Fraction light intercepted
LUE1         = NULL  # Light use efficiency (g DM MJ-1 PAR intercepted)
PARINT       = NULL  # PAR intercepted (MJ PAR m-2 day-1)
FRT          = NULL  # Fraction assimilates to roots
FLV          = NULL  # Fraction assimulates to leaves




# State variables

PARCU        = NULL;   PARCU[1]  = 0          #*        Cumulative intercepted PAR, MJ PAR intercepted m-2 ha-1
TRACU        = NULL;   TRACU[1]  = 0          #*        Cumulative transpiration, mm
TRAMCU       = NULL;   TRAMCU[1] = 0          #*        Cumulative maximal transpiration, mm
EVACU        = NULL;   EVACU[1]  = 0          #*        Cumulative evaporation, mm
EVAMCU       = NULL;   EVAMCU[1] = 0          #         Cumulative maximum evaporation, mm
IRRCU        = NULL;   IRRCU[1]  = 0          #*        Cumulative irrigation, mm
TSUM         = NULL;   TSUM[1]   = 0          #*        Sum of temperatures above base temperature, gr. C.d
DVS          = NULL                           #*        Hypothetical development stage, 600 gr. C.d taken from subroutine TILSUB
LAI          = NULL;   LAI[1]    = LAII       #*        Leaf area index, ha ha-1
DAHA         = NULL;   DAHA[1]   = 0          #*        Days after HARV, d
TILLER       = NULL;   TILLER[1] = TILLI      #*        Number of tillers, tillers m-2
WLVG         = NULL;   WLVG[1]   = WLVGI      #*        Dry weight of green leaves, kg ha-1
WLVD         = NULL;   WLVD[1]   = 0          #*        Dry weight of dead leaves, kg ha-1 incl. harvests
WLVD1        = NULL                           #*        Dry weight of dead leaves, kg ha-1
HRVBL        = NULL;   HRVBL[1]  = WLVG[1] - CWLVG           #*        Harvestable leaf weight
GRASS        = NULL;   GRASS[1]  = 0          #*        Dry weight of cutted green leaves, kg ha-1
WRE          = NULL;   WRE[1]    = WREI       #*        Dry weight of storage carbohydrates, kg ha-1
WRT          = NULL;   WRT[1]    = WRTI       #*        Dry weight of roots, kg ha-1
TADRW        = NULL                           #*        Total above ground dry weight including harvests, kg ha-1
YIELD        = NULL                           #*        Harvestable part of total above ground dry weight and previous harvests, kg ha-1
LENGTH       = NULL;   LENGTH[1] = 0          #*        Length of leaves, cm
SLAINT       = NULL                           #*        Running specific leaf area in model, ha kg-1
SLAINT[1]    = LAI[1] / WLVG[1]
ROOTD        = NULL;   ROOTD[1]  = ROOTDI     #*        Rooting depth (from LINGRA for timothee)
WA           = NULL;   WA[1]     = WAI        #*        Soil water in rooted zone  (from LINGRA for timothee)
RAINCU       = NULL;   RAINCU[1] = 0          #*        Cumulative rainfall
DRACU        = NULL;   DRACU[1]  = 0          #*        Cumulative drainage (AvdL)
RUNCU        = NULL;   RUNCU[1]  = 0          #*        Cumulative runoff (AvdL)
INTLAI       = NULL;   INTLAI[1] = 0          #*        Cumulative rainfall intercepted by leaves (AvdL)

#* ---------------------------------------------------------------------*
#*     SUBROUTINE PENMAN                                                *
#*     Purpose: Computation of the PENMAN EQUATION                      *
#* ---------------------------------------------------------------------*

DTRJM2       = RDD * 1.E3
BOLTZM       = 5.668E-8
LHVAP        = 2.4E6
PSYCH        = 0.067

BBRAD        = BOLTZM * (DAVTMP+273.)**4 * 86400.
SVP          = 0.611 * exp(17.4 * DAVTMP / (DAVTMP + 239.))  
SLOPE        = 4158.6 * SVP / (DAVTMP + 239.)^2

RLWN         = NULL
NRADS        = NULL
NRADC        = NULL
PENMRS       = NULL
PENMRC       = NULL

WDF          = NULL 
PENMD        = NULL 

PEVAP        = NULL
PTRAN        = NULL
RNINTC       = NULL

#* ---------------------------------------------------------------------*
#*     SUBROUTINE EVAPTR                                                *
#*     Purpose: To compute actual rates of evaporation and transpiration*
#* ---------------------------------------------------------------------*

WC           = NULL
WAAD         = NULL
WAFC         = NULL
EVAP1        = NULL
WCCR = WCWP + 0.5 * (WCFC-WCWP)
FR           = NULL
TRAN         = NULL
AVAILF       = NULL
EVAP         = NULL
TRAN         = NULL

#* ---------------------------------------------------------------------*
#*     SUBROUTINE DRUNIR                                                *
#*     Purpose: To compute rates of drainage, runoff and irrigation     *
#* ---------------------------------------------------------------------*

WAST         = NULL
DRAIN        = NULL
RUNOFF       = NULL
IRRIG        = NULL

#************************************************************************
#***   5. Water balance and root depth growth (from LINGRA for thymothee)
#************************************************************************

RDAHA        = NULL
HARV         = NULL
RROOTD       = NULL
EXPLOR       = NULL
TRANRF       = NULL
RWA          = NULL

LEAFN        = NULL
LERA         = NULL
LERA2        = NULL

# SUBROUTINE TILSUB

DTIL         = NULL
REFTIL       = NULL
DTILD        = NULL

# SUBROUTINE SOSUB

LUED         = NULL
GTWSO        = NULL

GTWSO2       = NULL
DLAIS        = NULL
DRE          = NULL
GTWSI        = NULL
GRE          = NULL
GTW          = NULL
RRE          = NULL

RDRSH        = NULL
RDRSM        = NULL
RDRS         = NULL
RDR          = NULL
GRT          = NULL
GLAI         = NULL
DLAI         = NULL
RLAI         = NULL
DLV          = NULL
GLV          = NULL
RLV          = NULL

NEWBIO = NULL
RRATIO = NULL
LUEYCU = NULL
WRTMIN = NULL

TIME <- seq(from = 1, to = FINTIM, by = 1)

#DYNAMIC

for(i in 1:FINTIM){
  
  VP[i] = min(VP[i], SVP[i])
  
  RLWN[i]         = BBRAD[i] * max(0.,0.55*(1.-VP[i]/SVP[i]))
  NRADS[i]        = DTRJM2[i] * (1.-0.15) - RLWN[i]
  NRADC[i]        = DTRJM2[i] * (1.-0.25) - RLWN[i]
  PENMRS[i]       = NRADS[i] * SLOPE[i]/(SLOPE[i]+PSYCH)
  PENMRC[i]       = NRADC[i] * SLOPE[i]/(SLOPE[i]+PSYCH)
  WDF[i]          = 2.63 * (1.0 + 0.54 * WN[i])
  PENMD[i]        = LHVAP * WDF[i] * (SVP[i]-VP[i]) * PSYCH/(SLOPE[i]+PSYCH)
  
  
  
  
  DEC[i]    = -asin(sin(23.45*RAD)*cos(2.*PI*(TIME[i]+10.)/365.))  # Northern / Southern hemisphere! Warnings from this line!
  
  if(atan(-1./tan(RAD*LAT)) > DEC[i]) DECC[i] <- atan(-1./tan(RAD*LAT)) else
    if(atan(1./tan(RAD*LAT)) < DEC[i]) DECC[i] <- atan(-1./tan(RAD*LAT)) else DECC[i] <- DEC[i] 
  
  DAYL[i]   = 0.5 * ( 1. + 2. * asin(tan(RAD*LAT)*tan(DECC[i])) / PI )
  
  PARAV[i]  = 0.5 * DTR[i] / DAYL[i]
  #*       soil temperature changes
  RSOITM[i] = (DAVTMP[i]-SOITMP[i]) / 10.
  SOITMP[i+1] = SOITMP[i] + RSOITM[i]
  TMEFF[i]    = max(DAVTMP[i] - TMBAS1, 0.)
  
  # Rate variables
  
  if(SOITMP[i] <3) REDTMP[i] <-0 else if(SOITMP[i]>8) REDTMP[i] <- 1 else REDTMP[i] <- (SOITMP[i]-3)*0.2         # LUERD1
  if(DTR[i] <10)   REDRDD[i] <-1 else REDRDD[i] <- ((1+(10/(40-10)*0.67)) - DTR[i] * (0.67/(40-10)))              # LUERD2
  
  #*        Daily photosynthetically active radiation, MJ m-2 d-1
  PAR[i] = DTR[i] * 0.50                                                                                         
  #*        Fraction of light interception
  FINT[i] = (1. - exp(-KDIF*LAI[i]))
  #*        Light use efficiency, g MJ PAR-1
  LUE1[i] = LUEMAX * REDTMP[i] * REDRDD[i]
  #*        Total intercepted photosynthetically active radiation, MJ m-2 d-1       
  PARINT[i] = FINT[i] * PAR[i]
  #*        Fraction of dry matter allocated to roots, kg kg-1
  
  #* ---------------------------------------------------------------------*
  #*     SUBROUTINE PENMAN                                                *
  #*     Purpose: Computation of the PENMAN EQUATION                      *
  #* ---------------------------------------------------------------------*
  
  PEVAP[i]        = max(0.0, exp(-0.5*LAI[i])  * (PENMRS[i] + PENMD[i]) / LHVAP)
  PTRAN[i]        = (1.-exp(-0.5*LAI[i])) * (PENMRC[i] + PENMD[i]) / LHVAP
  RNINTC[i]       = min(RAIN[i], 0.25*LAI[i])
  PTRAN[i]        = max(0.0, PTRAN[i]-0.5*RNINTC[i])
  
  #* ---------------------------------------------------------------------*
  #*     SUBROUTINE EVAPTR                                                *
  #*     Purpose: To compute actual rates of evaporation and transpiration*
  #* ---------------------------------------------------------------------*
  
  WC[i]   = 0.001 * WA[i]   / ROOTD[i]
  WAAD[i] = 1000. * WCAD * ROOTD[i]
  WAFC[i] = 1000. * WCFC * ROOTD[i]
  
  EVAP1[i]  = PEVAP[i] * max(0., min(1.,(WC[i]-WCAD)/(WCFC-WCAD)))
  
  if(WC[i]>WCCR) FR[i] <- max(0.,min(1.,(WCST-WC[i])/(WCST-WCWET))) else FR[i] <- max(0., min(1.,(WC[i]-WCWP)/(WCCR-WCWP))) 
  
  TRAN[i] = PTRAN[i] * FR[i]
  
  AVAILF[i] = min(1., (WA[i]-WAAD[i])/(EVAP1[i]+TRAN[i])) # NOTNUL removed!
  EVAP[i] = EVAP1[i] * AVAILF[i]
  TRAN[i] = TRAN[i] * AVAILF[i]
  
  #* ---------------------------------------------------------------------*
  #*     SUBROUTINE DRUNIR                                                *
  #*     Purpose: To compute rates of drainage, runoff and irrigation     *
  #* ---------------------------------------------------------------------*
    
  WAST[i] = 1000. * WCST * ROOTD[i]
  
  DRAIN[i]  = max(0, min(DRATE,(WA[i]-WAFC[i] + RAIN[i] - RNINTC[i] - EVAP[i] - TRAN[i])))   
    
  RUNOFF[i] = max(0, WA[i]-WAST[i] + RAIN[i] - RNINTC[i] - EVAP[i] - TRAN[i] - DRAIN[i])
                                     
  IRRIG[i]  = IRRIGF * (WAFC[i]-WA[i] - (RAIN[i] - RNINTC[i] - EVAP[i] - TRAN[i] - DRAIN[i] - RUNOFF[i]))
                                            
  #************************************************************************
  #***   5. Water balance and root depth growth (from LINGRA for thymothee)
  #************************************************************************
  
  if(ROOTDM-ROOTD[i] <= 0 | WC[i]-WCWP <=0) RROOTD[i] <- RRDMAX * 0 else RROOTD[i] <- RRDMAX * 1  
  EXPLOR[i] = 1000. * RROOTD[i] * WCFC                        # Assumption that explored layers are at FC!
  
  if(PTRAN[i] <= 0) TRANRF[i] <- 1 else TRANRF[i] = min(1, TRAN[i] / PTRAN[i]) # INSW removed!
                        
  RWA[i]    = (RAIN[i]+EXPLOR[i]+IRRIG[i]) - (RNINTC[i]+RUNOFF[i]+TRAN[i]+EVAP[i]+DRAIN[i])
  
  FRT[i] = min(0.263, 0.263 - TRANRF[i]*(0.263-0.165))        # FRRTTB 
  FLV[i] = 1.-FRT[i]
  
  
  # SUBROUTINE MOWING 
  
  RDAHA[i] <- 1  
  HARV[i] = 0
  INCUT[i+1]=INCUT[i]
  
  #     mowing at criterium of WLVG: CWGHT
  #     reset days after HARV
  
  if(IMOPT == 1 & WLVG[i] > CWGHT) HARV[i]  <- WLVG[i]-CWLVG
  if(IMOPT == 1 & WLVG[i] > CWGHT) RDAHA[i] <- -DAHA[i]
  if(IMOPT == 1 & WLVG[i] > CWGHT) INCUT[i+1] <- INCUT[i] + 1.
  
  #     mowing at observation dates, periodical harvests
  #     reset days after HARV
  
  if(IMOPT == 2 & MNDAT0[i] == 1 & WLVG[i] > CWLVG) HARV[i]  <- WLVG[i]-CWLVG
  if(IMOPT == 2 & MNDAT0[i] == 1 & WLVG[i] > CWLVG) RDAHA[i] <- -DAHA[i]
  if(IMOPT == 2 & MNDAT0[i] == 1 & WLVG[i] > CWLVG) INCUT[i+1] = INCUT[i] + 1.
  
  #     no mowing in current season, do not increase rate
  #     of days after HARV
   
  if(INCUT[i] == 0.) RDAHA[i] <- 0
  
  #     mowing in current season, increase rate of days
  #     after harvests
  
  #*        Temperature dependent leaf appearance rate, according to
  #*        (Davies and Thomas, 1983), soil temperature (SOITMP)is used as
  #*        driving force which is estimated from a 10 day running average
         
  if(REDTMP[i] > 0) LEAFN[i] <- SOITMP[i]*0.01 else LEAFN[i] <- 0
  
  #*        Leaf elongation rate affected by temperature
  #*        cm day-1 tiller-1
  
  if(DAVTMP[i]-TMBAS1 > 0) LERA[i] <- 0.83*log(max(DAVTMP[i], 2.))-0.8924 else LERA[i] <- 0   # log10 or log?
  
  if((HARV[i]-0.1)<0) LERA2[i] <- LERA[i] else LERA2[i] <- -1*LENGTH[i] 
  
  # SUBROUTINE TILSUB
  
  DTIL[i] = 0.
  
  if(DAHA[i] < 8) REFTIL[i] = max(0.,0.335-0.067*LAI[i]) * REDTMP[i] else REFTIL[i] = max(0., min(FSMAX, 0.867-0.183*LAI[i])) * REDTMP[i]
  
  #        Relative rate of tiller formation when defoliation less
  #        than 8 days ago, tiller tiller-1 d-1
  #        Relative rate of tiller formation when defoliation is more
  #        than 8 days ago, tiller tiller-1 d-1
  
  #        Relative death rate of tillers due to self-shading (DTILD),
  #        tiller tiller-1 d-1
  
  DTILD[i] = max(0.01*(1.+TSUM[i]/600.), 0.05 * (LAI[i]-LAICR)/LAICR)
  
  if(TILLER[i] < 14000) DTIL[i] = (REFTIL[i]-DTILD[i]) * LEAFN[i] * TILLER[i] else DTIL[i] = -DTILD[i] * LEAFN[i] * TILLER[i] 
  
  
  # SUBROUTINE SOSUB 
                  
  LUED[i] = min(LUE1[i]*(0.336+0.224*NITR)/(0.336+0.224*NITMAX), LUE1[i]*TRANRF[i])
   
  #     start of growing season
  
  if(HARV[i] == 0) GTWSO[i] <- LUED[i] * PARINT[i] * (1.+0.8*log(CO2A/360.)) * 10. else  GTWSO[i] = 0. 
  
  #*        Rate of sink limited leaf growth, unit of TILLER is tillers m-2 (!),
  #*        1.0E-8 is conversion from cm-2 to ha-1, ha leaf ha ground-1 d-1
  DLAIS[i] = (TILLER[i] * 1.0E4 * (LERA[i] * 0.3)) * 1.0E-8
  
  GTWSO2[i] = GTWSO[i]+WRE[i]
  DRE[i]   = WRE[i]
  
  #*        Conversion to total sink limited carbon demand,
  #*        kg leaf ha ground-1 d-1
  
  if(HARV[i] <= 0) GTWSI[i] <- DLAIS[i] * (1./SLA) * (1./FLV[i]) else GTWSI[i] <- 0 
  
  #*        Actual growth switches between sink- and source limitation
  #*        (more or less dry matter formed than can be stored)
  
  if((GTWSO2[i] - GTWSI[i]) > 0) GRE[i] <- GTWSO2[i] - GTWSI[i] else GRE[i] <- 0
  if((GTWSO2[i] - GTWSI[i]) <= 0) GTW[i] <- GTWSO2[i] else GTW[i] <- GTWSI[i] 
  
  #*        Change in reserves
  RRE[i] = GRE[i]-DRE[i]
  
  #*        Relative death rate of leaves due to self-shading, d-1
  RDRSH[i] = max(0., min(0.03, 0.03 * (LAI[i]-LAICR) /LAICR))
  
  #*        Relative death rate of leaves due to drought stress, d-1
  RDRSM[i] = max(0., min(0.05, 0.05 * (1.-TRANRF[i])))
  
  #*        Maximum of relative death rate of leaves due to
  #*        and drought stres, d-1
  RDRS[i] = max(RDRSH[i], RDRSM[i])
  
  #*        Actual relative death rate of leaves is sum of base death
  #*        rate plus maximum of death rates RDRSM and RDRSH, d-1
  RDR[i] = RDRD + RDRS[i]
  
  #*        Actual growth rate of roots, kg ha-1 d-1
  GRT[i] = GTW[i] * FRT[i]
  
  #*        Actual growth rate of leaf area, ha ha-1 d-1
  GLAI[i] = GTW[i] * FLV[i] * SLA
  
  #*        Actual death rate of leaf area, due to relative death
  #*        rate of leaf area or rate of change due to cutting, ha ha-1 d-1
  
  if(HARV[i] <= 0) DLAI[i] <- LAI[i] * (1. - exp(-RDR[i])) else DLAI[i] <- HARV[i] * SLAINT[i]  
  
  #*        Change in LAI
  RLAI[i]= GLAI[i]-DLAI[i]
  
  #*        Actual death rate of leaves, kg ha-1 d-1 incl. harvested leaves
  DLV[i] = DLAI[i] / SLAINT[i]    # NOTNUL removed!
  
  #*        rate of change of dry weight of green leaves due to
  #*        growth and senescence of leaves or periodical harvest, kg ha-1 d-1
  
  if(HARV[i] <= 0) GLV[i] <- GTW[i]*FLV[i] else GLV[i] <- 0  
  
  #*        Change in green leaf weight
  RLV[i] = GLV[i]-DLV[i]
  
  
  # State variables
  
  PARCU[i+1]      = PARCU[i]  + PARINT[i]           #*        Cumulative intercepted PAR, MJ PAR intercepted m-2 ha-1
  TRACU[i+1]      = TRACU[i]  + TRAN[i]             #*        Cumulative transpiration, mm
  TRAMCU[i+1]     = TRAMCU[i] + PTRAN[i]            #*        Cumulative maximal transpiration, mm
  EVACU[i+1]      = EVACU[i]  + EVAP[i]             #*        Cumulative evaporation, mm
  EVAMCU[i+1]     = EVAMCU[i] + PEVAP[i]            #*        Cumulative maximal evaporation, mm
  IRRCU[i+1]      = IRRCU[i]  + IRRIG[i]            #*        Cumulative irrigation, mm
  TSUM[i+1]       = TSUM[i]   + TMEFF[i]            #*        Sum of temperatures above base temperature, gr. C.d
  DVS[i]          = TSUM[i] / 600.                  #*        Hypothetical development stage, 600 gr. C.d taken from subroutine TILSUB
  LAI[i+1]        = LAI[i]    + RLAI[i]             #*        Leaf area index, ha ha-1
  DAHA[i+1]       = DAHA[i]   + RDAHA[i]            #*        Days after HARV, d
  TILLER[i+1]     = TILLER[i] + DTIL[i]             #*        Number of tillers, tillers m-2
  WLVG[i+1]       = WLVG[i]   + RLV[i]              #*        Dry weight of green leaves, kg ha-1
  WLVD[i+1]       = WLVD[i]   + DLV[i]              #*        Dry weight of dead leaves, kg ha-1 incl. harvests
  WLVD1[i]        = WLVD[i]   - GRASS[i]            #*        Dry weight of dead leaves, kg ha-1
  HRVBL[i+1]      = WLVG[i+1] - CWLVG               #*        Harvestable leaf weight
  GRASS[i+1]      = GRASS[i]  + HARV[i]             #*        Dry weight of cutted green leaves, kg ha-1
  WRE[i+1]        = WRE[i]    + RRE[i]              #*        Dry weight of storage carbohydrates, kg ha-1
  WRT[i+1]        = WRT[i]    + GRT[i]              #*        Dry weight of roots, kg ha-1
  TADRW[i]        = GRASS[i]  + WLVG[i]             #*        Total above ground dry weight including harvests, kg ha-1
  YIELD[i+1]      = GRASS[i+1]+ max(0., HRVBL[i+1]) #*        Harvestable part of total above ground dry weight and previous harvests, kg ha-1
  LENGTH[i+1]     = LENGTH[i] + LERA2[i]            #*        Length of leaves, cm
  SLAINT[i+1]     = LAI[i+1]  / WLVG[i+1]           #*        Running specific leaf area in model, ha kg-1 NOTNUL removed!
  ROOTD[i+1]      = ROOTD[i]  + RROOTD[i]           #*        Rooting depth (from LINGRA for timothee)  
  WA[i+1]         = WA[i]     + RWA[i]              #*        Soil water in rooted zone  (from LINGRA for timothee)
  RAINCU[i+1]     = RAINCU[i] + RAIN[i]             #*        Cumulative rainfall  
  DRACU[i+1]      = DRACU[i]  + DRAIN[i]            #         Cumulative drainage (AvdL)
  RUNCU[i+1]      = RUNCU[i]  + RUNOFF[i]           #         Cumulative runoff (AvdL)
  INTLAI[i+1]     = INTLAI[i] + RNINTC[i]           #         Cumulative rainfall intercepted by leaves (AvdL)

  #************************************************************************
  #  ***   9. Additional variables and parameters for output
  #************************************************************************
  
  NEWBIO[i] = WLVG[i]+WRT[i]+WRE[i] - (WLVGI+WRTI+WREI) + GRASS[i]
  if(NEWBIO[i] == 0.) RRATIO[i] <- 0 else RRATIO[i] <- max(0., min(1., (WRT[i]-WRTI) / NEWBIO[i])) 
  LUEYCU[i] = YIELD[i]  / PARCU[i]
  WRTMIN[i] = -WRT[i]
  
}


# Data processing

WATERINPUT <- RAINCU + IRRCU  
WATEROUTPUT <- TRACU + EVACU + INTLAI + DRACU + RUNCU    

# Output in table and .csv file

OUTPUT <- cbind(YEAR, DAVTMP, DTR, DAYL, LAI, TILLER, YIELD, WLVG, WLVD1, WRE, WRT, WRTMIN, 
                RRATIO, SLA, SLAINT, PARCU, LUEYCU, GRASS, TADRW, VP, NEWBIO, TRANRF, 
                TRAN, PTRAN, EVAP, PEVAP, WC, WA, RAIN, RAINCU, WN,TRAMCU, TRACU, EVAMCU,
                EVACU, IRRCU, GTW, GTWSI, LENGTH)

#write.csv(OUTPUT, file= "M:/R/OUTPUT/LINGRA.csv") # Write OUTPUT to a .csv file

# Graphical output (AvdL)

# Yield and grass harvest
plot(YIELD[1:FINTIM]~TIME[1:FINTIM], ylim = c(0,30000),  type="l", col="khaki4", xlab = "time (d)", ylab = "Cumulative yield (kg DM grass ha-1)",
     main = "Grass production")
lines(GRASS[1:FINTIM]~TIME[1:FINTIM], type="l", col="indianred1")
legend("topleft", legend = c("Yield","Harvested grass"), border=FALSE, lty=c("solid", "solid"), col=c("khaki4","indianred1")) 

# Soil water balance 
plot(TRACU[1:FINTIM]~TIME[1:FINTIM], ylim = c(0,(RAINCU[FINTIM]+IRRCU[FINTIM])),  type="p", col="khaki4", xlab = "time (d)", ylab = "mm water", main="Cumulative water flows")
lines(EVACU[1:FINTIM]~TIME[1:FINTIM], type="p", col="indianred1")
lines(RAINCU[1:FINTIM]~TIME[1:FINTIM], type="p", col="black")
lines(IRRCU[1:FINTIM]~TIME[1:FINTIM], type="p", col="blue1")
lines(INTLAI[1:FINTIM]~TIME[1:FINTIM], type="p", col="darkblue")
lines(DRACU[1:FINTIM]~TIME[1:FINTIM], type="p", col="orange")
lines(RUNCU[1:FINTIM]~TIME[1:FINTIM], type="p", col="green")
legend("topleft", legend = c("Transpiration","Evaporation","Rainfall","Irrigation", "Intercepted rain", "Drainage", "Runoff"), 
       border=FALSE, 
       pch=19,
       col=c("khaki4","indianred1", "black","blue1","darkblue","orange","green")) 

plot(TRAN[1:FINTIM]~TIME[1:FINTIM], ylim = c(0,8),  type="p", col="khaki4", xlab = "time (d)", ylab = "mm water day-1", main="Water flows")
lines(EVAP[1:FINTIM]~TIME[1:FINTIM], type="p", col="indianred1")
lines(RAIN[1:FINTIM]~TIME[1:FINTIM], type="p", col="black")
lines(IRRIG[1:FINTIM]~TIME[1:FINTIM], type="p", col="blue1")
lines(RNINTC[1:FINTIM]~TIME[1:FINTIM], type="p", col="darkblue")
lines(DRAIN[1:FINTIM]~TIME[1:FINTIM], type="p", col="orange")
lines(RUNOFF[1:FINTIM]~TIME[1:FINTIM], type="p", col="green")
legend("topleft", legend = c("Transpiration","Evaporation","Rainfall","Irrigation", "Intercepted rain", "Drainage", "Runoff"), 
       border=FALSE, 
       pch=19,
       col=c("khaki4","indianred1", "black","blue1","darkblue","orange","green")) 

plot(WC[1:FINTIM]~TIME[1:FINTIM], ylim = c(0, WCST + 0.15),  type="p", pch= 19, cex = 0.5, col="khaki4", xlab = "time (d)", ylab = "soil water fraction",
     main="Soil water fraction")     
lines(rep(WCAD,FINTIM)~TIME[1:FINTIM], type="l", col="indianred1") 
lines(rep(WCWP,FINTIM)~TIME[1:FINTIM], type="l", col="black")
lines(rep(WCCR,FINTIM)~TIME[1:FINTIM], type="l", col="blue1")
lines(rep(WCFC,FINTIM)~TIME[1:FINTIM], type="l", col="red")
lines(rep(WCST,FINTIM)~TIME[1:FINTIM], type="l", col="orange")
legend("topleft", legend = c("Actual water content","Air dry water content", "Wilting point water content", "Critical water content", 
                             "Field capacity water content", "Saturation water content"), 
       border=FALSE, lty=c(NA,"solid","solid","solid","solid", "solid", "solid"),
       pch=c(1,NA,NA,NA,NA,NA),
       col=c("khaki4","indianred1", "black","blue1","darkblue","orange"), cex = 0.7)
       

# 
plot(LAI[1:FINTIM]~TIME[1:FINTIM], ylim = c(0, 8),  type="l", col="khaki4", xlab = "time (d)", ylab = "LAI", main="LAI dynamics")     
lines(rep(LAICR,FINTIM)~TIME[1:FINTIM], type="l", col="indianred1")
legend("topleft", legend = c("LAI", "LAI critical"), border=FALSE, lty=c("solid","solid"), col=c("khaki4","indianred1")) 

OUTPUT <- c(TILLER[365], YIELD[365], WLVG[365], WLVD1[365], PARCU[365], GRASS[365], TRACU[365], EVACU[365])

print(OUTPUT)
  