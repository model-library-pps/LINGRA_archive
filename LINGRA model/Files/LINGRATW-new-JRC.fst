DEFINE_CALL MOWING(INPUT,INPUT,INPUT_ARRAY, ...
      INTEGER_INPUT,INPUT,INPUT,INPUT, ...
       INPUT,INPUT,OUTPUT,OUTPUT)
DEFINE_CALL TILSUB(INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,INPUT, ...
        OUTPUT)
DEFINE_CALL SOSUB(INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,INPUT, ...
        OUTPUT,OUTPUT)
DEFINE_CALL PENMAN(INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,   OUTPUT,OUTPUT)
DEFINE_CALL EVAPTR(INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,INPUT, ...
                   INPUT,INPUT,                          OUTPUT,OUTPUT)
DEFINE_CALL DRUNIR(INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,INPUT, ...
                   INPUT,INPUT,INPUT,              OUTPUT,OUTPUT,OUTPUT)

ARRAY MNDAT (1:NH)
ARRAY_SIZE NH=10

TITLE LINGRA-new-JRC

* Version adapted for rye grass April 4, 2006, Joost Wolf for
* FST modelling;
* Model is based on LINGRA model in CGMS
* for forage growth and production simulation which was written FORTRAN
* (GRSIM.pfo) and later in C; Application of model is the
* Simulation of perennial ryegrass (L. perenne) growth under both
* potential and water-limited growth conditions.
*
* Model is different from LINGRA model in CGMS with respect to:
*  1) evaporation, transpiration, water balance, root depth growth
*  and growth reduction by drought stress (TRANRF) are derived from LINGRA
*  model for thimothee (e.g. subroutine PENMAN, EVAPTR and DRUNIR)
*  2) running average to calculate soil temperature (SOITMP) is derived from
*  approach in LINGRA model for thimothee
*
*
*                    name    type     description                               unit
* Variables:
*     Biomass in leaves, reserves, etc. in kg DM / ha
*     Terms of water balance in mm/day
*
************************************************************************
***   1. Initial conditions and run control
************************************************************************

INITIAL

INCON ZERO   = 0.
INCON ROOTDI = 0.4
INCON LAII   = 0.1; SOITMI = 5.; TILLI = 7000.; ...
      WREI  = 200. ; WRTI   =  4.
      WAI    = 1000. * ROOTDI * WCI
*     Initial leaf weight is initialized as initial
*     leaf area divided by initial specific leaf area, kg ha-1
       WLVGI   = LAII / SLA
*
*     Remaining leaf weight after cutting is initialized at remaining
*     leaf area after cutting divided by initial specific leaf area, kg ha-1
      CWLVG = CLAI/SLA
*      Maximum site filling new buds (FSMAX) decreases due
*      to low nitrogen contents, Van Loo and Schapendonk (1992)
*      Theoretical maximum tillering size = 0.693
       FSMAX = NITR/NITMAX*0.693


TIMER STTIME = 1.; FINTIM = 365.; DELT = 1.; PRDEL = 1.; IPFORM = 5
TRANSLATION_GENERAL DRIVER='EUDRIV'
PRINT YEAR, DAVTMP, DTR, DAYL, LAI, TILLER, YIELD, ...
      WLVG, WLVD1, WRE, WRT, WRTMIN, ...
      RRATIO, SLA, SLAINT, PARCU, LUEYCU, RUNNR,GRASS, TADRW, YIELD,...
      VP, NEWBIO, TRANRF, TRAN, PTRAN, EVAP, PEVAP, WC, WA, RAIN,...
      RAINCU, WN,TRAMCU, TRACU, EVAMCU, EVACU, IRRCU, GTW, GTWSI, LENGTH

DYNAMIC

************************************************************************
***   2. Environmental data
************************************************************************

WEATHER CNTR='Nl'; ISTN=1; WTRDIR='D:\Wolf\Asemars\LINGRA\...
          LINGRA-new-JRC\'; IYEAR=1989
        DAVTMP = 0.5 * (TMMN + TMMX)
        PHOTMP= (TMMN + 3. * TMMX)/4.
        PI     = 3.1416
        RAD    = PI / 180.
        DEC    = -ASIN (SIN (23.45*RAD)*COS (2.*PI*(DOY+10.)/365.))
        DECC   = LIMIT( ATAN(-1./TAN(RAD*LAT)), ...
                        ATAN( 1./TAN(RAD*LAT)), DEC)
        DAYL   = 0.5 * ( 1. + 2. * ASIN(TAN(RAD*LAT)*TAN(DECC)) / PI )
*     From J/m2/d to MJ/m2/d
        DTR    = RDD / 1.E+6
        PARAV  = 0.5 * DTR / DAYL
*       soil temperature changes
        RSOITM = (DAVTMP-SOITMP) / 10.
        SOITMP = INTGRL( SOITMI, RSOITM )

        EFFTMP = MAX ( DAVTMP, TBASE )

************************************************************************
***   3. State variables
************************************************************************
*      Cumulative intercepted PAR, MJ PAR intercepted m-2 ha-1
         PARCU = INTGRL (ZERO, PARINT)

*        Cumulative transpiration, mm
         TRACU = INTGRL (ZERO, TRAN)
*
*        Cumulative maximal transpiration, mm
         TRAMCU = INTGRL (ZERO, PTRAN)
         
*       Cumulative evaporation, mm
         EVACU = INTGRL (ZERO, EVAP)
*
*        Cumulative maximal evaporation, mm
         EVAMCU = INTGRL (ZERO, PEVAP)
*
*       Cumulative irrigation, mm
         IRRCU = INTGRL (ZERO, IRRIG)
         
*        Sum of temperatures above base temperature, gr. C.d
         TSUM = INTGRL (ZERO, TMEFF)
*
*        Hypothetical development stage, 600 gr. C.d taken from
*        subroutine TILSUB
         DVS = TSUM / 600.
*
*        Leaf area index, ha ha-1
         LAI = INTGRL (LAII, RLAI)

*        Days after HARV, d
         DAHA = INTGRL (ZERO, RDAHA)
*
*        Number of tillers, tillers m-2
         TILLER = INTGRL (TILLI, DTIL)
*
*        Dry weight of green leaves, kg ha-1
         WLVG = INTGRL (WLVGI, RLV)
*
*        Dry weight of dead leaves, kg ha-1 incl. harvests
         WLVD = INTGRL (ZERO, DLV)
*        Dry weight of dead leaves, kg ha-1
         WLVD1= WLVD - GRASS
         
*        Harvestable leaf weight
         HRVBL=WLVG-CWLVG

*        Dry weight of cutted green leaves, kg ha-1
         GRASS = INTGRL (ZERO, HARV)
*
*        Dry weight of storage carbohydrates, kg ha-1
         WRE = INTGRL (WREI, RRE)
*
*        Dry weight of roots, kg ha-1
         WRT = INTGRL (WRTI, GRT)
*
*        Total above ground dry weight including harvests, kg ha-1
         TADRW = GRASS + WLVG
*
*        Harvestable part of total above ground dry weight
*        and previous harvests, kg ha-1
         YIELD = GRASS + MAX (0., HRVBL)
*
*        Length of leaves, cm
         LENGTH = INTGRL (ZERO, LERA2)
*
*        Running specific leaf area in model, ha kg-1
         SLAINT = LAI / NOTNUL(WLVG)

*        Rooting depth (from LINGRA for timothee)
         ROOTD= INTGRL(ROOTDI, RROOTD)
         
*        Soil water in rooted zone  (from LINGRA for timothee)
         WA     = INTGRL( WAI,RWA )
         
*        Cumulative rainfall
         RAINCU = INTGRL( ZERO, RAIN )
************************************************************************
***   4. Rate variables
************************************************************************
         REDTMP = AFGEN(LUERD1,SOITMP)

         REDRDD = AFGEN (LUERD2,RDD/1.E6)
*
         TMEFF =  MAX (DAVTMP-TMBAS1, 0.)
*

*        Daily photosynthetically active radiation, MJ m-2 d-1
         PAR = RDD/1.0E6 * 0.50
*
*        Fraction of light interception
         FINT = (1.-EXP (-KDIF*LAI))
*
*        Light use efficiency, g MJ PAR-1
         LUE1 = LUEMAX * REDTMP * REDRDD
*
*        Total intercepted photosynthetically active
*        radiation, MJ m-2 d-1
         PARINT = FINT * PAR
*
*        Fraction of dry matter allocated to roots, kg kg-1
         FRT = AFGEN (FRRTTB, TRANRF)
         FLV = 1.-FRT
         
*        Call to subroutine for grassland management options
         CALL MOWING (IMOPT,INCUT,MNDAT,NH,TIME,WLVG, ...
         CWGHT,CWLVG,DAHA,RDAHA,HARV)
*
*        Temperature dependent leaf appearance rate, according to
*        (Davies and Thomas, 1983), soil temperature (SOITMP)is used as
*        driving force which is estimated from a 10 day running
*        average
         LEAFN=   FCNSW(REDTMP, 0., 0.,SOITMP * 0.01 )
*
*        Leaf elongation rate affected by temperature
*        cm day-1 tiller-1
         LERA= FCNSW(DAVTMP-TMBAS1, 0., 0.,...
          0.83*LOG(MAX(DAVTMP, 2.))-0.8924 )
*
         LERA2 = INSW (HARV-0.1, LERA, -LENGTH)

         CALL TILSUB (TILLER,FSMAX,LAI,LAICR,DAHA,LEAFN,TSUM,REDTMP, ...
                      DTIL)

*        Rate of sink limited leaf growth, unit of TILLER is tillers m-2 (!),
*        1.0E-8 is conversion from cm-2 to ha-1, ha leaf ha ground-1 d-1
         DLAIS = (TILLER * 1.0E4 * (LERA * 0.3)) * 1.0E-8

*        Source limited growth rate of crop, kg ha-1 d-1
         CALL SOSUB (PARINT,LUE1,CO2A,NITR,NITMAX,TRANRF, ...
                    HARV,LUE2,GTWSO1)
*
         GTWSO2 = GTWSO1+WRE/DELT
         DRE   = WRE/DELT
*
*        Conversion to total sink limited carbon demand,
*        kg leaf ha ground-1 d-1
         GTWSI= FCNSW(HARV,DLAIS * (1./SLA) * (1./FLV),...
            DLAIS * (1./SLA) * (1./FLV), 0.)
*
*        Actual growth switches between sink- and source limitation
*        (more or less dry matter formed than can be stored)
         GRE= FCNSW(GTWSO2-GTWSI,0.,0., GTWSO2-GTWSI)
         GTW= FCNSW(GTWSO2-GTWSI,GTWSO2,GTWSO2, GTWSI)

*        Change in reserves
         RRE = GRE-DRE

*        Relative death rate of leaves due to self-shading, d-1
         RDRSH = LIMIT (0., 0.03, 0.03 * (LAI-LAICR) /LAICR)
*
*        Relative death rate of leaves due to drought stress, d-1
         RDRSM = LIMIT(0., 0.05, 0.05 * (1.-TRANRF))
*
*        Maximum of relative death rate of leaves due to
*        and drought stres, d-1
         RDRS = MAX (RDRSH, RDRSM)
*
*        Actual relative death rate of leaves is sum of base death
*        rate plus maximum of death rates RDRSM and RDRSH, d-1
         RDR = RDRD + RDRS
*
*        Actual growth rate of roots, kg ha-1 d-1
         GRT = GTW * FRT

*        Actual growth rate of leaf area, ha ha-1 d-1
         GLAI = GTW * FLV * SLA
*
*        Actual death rate of leaf area, due to relative death
*        rate of leaf area or rate of change due to cutting, ha ha-1 d-1
         DLAI= FCNSW(HARV, LAI * (1. - EXP(-RDR * DELT)), ...
          LAI * (1. - EXP(-RDR * DELT)),HARV*SLAINT )

*        Change in LAI
         RLAI= GLAI-DLAI

*        Actual death rate of leaves, kg ha-1 d-1 incl. harvested leaves
         DLV = DLAI / NOTNUL (SLAINT)
*
*        rate of change of dry weight of green leaves due to
*        growth and senescence of leaves or periodical harvest, kg ha-1 d-1
         GLV= FCNSW(HARV,GTW*FLV,GTW*FLV, 0.)

*        Change in green leaf weight
         RLV = GLV-DLV
************************************************************************
***   5. Water balance and root depth growth (from LINGRA for thymothee)
************************************************************************

      CALL PENMAN( DAVTMP,VP,DTR,LAI,WN,RNINTC, ...
                   PEVAP,PTRAN )
      CALL EVAPTR( PEVAP,PTRAN,ROOTD,WA,WCAD,WCWP,WCFC,WCWET,WCST,...
                   DELT,EVAP,TRAN )
      CALL DRUNIR( RAIN,RNINTC,EVAP,TRAN,IRRIGF,...
                   DRATE,DELT,WA,ROOTD,WCFC,WCST,...
                   DRAIN,RUNOFF,IRRIG )

      RROOTD = RRDMAX * REAAND( ROOTDM-ROOTD, WC-WCWP )
      EXPLOR = 1000. * RROOTD * WCFC
      RNINTC = MIN( RAIN, 0.25*LAI )
      TRANRF = INSW( -PTRAN, TRAN / NOTNUL(PTRAN), 1. )
      RWA    = (RAIN+EXPLOR+IRRIG) - (RNINTC+RUNOFF+TRAN+EVAP+DRAIN)
      WC     = 0.001 * WA / ROOTD

************************************************************************
***   9. Additional variables and parameters for output
************************************************************************

      NEWBIO = WLVG+WRT+WRE - (WLVGI+WRTI+WREI)  + GRASS
      RRATIO = LIMIT  ( 0., 1., (WRT-WRTI) / NOTNUL(NEWBIO) )

      LUEYCU = YIELD  / NOTNUL(PARCU)

      WRTMIN = -WRT


PARAM RUN    = 0.
      RUNNR  = RUN

************************************************************************
***   10. Functions and parameters for grass
************************************************************************

* Parameters
PARAM CO2A   = 360. ; KDIF  = 0.60 ; LAICR  = 4. ; TBASE  = 0.
PARAM LUEMAX= 3.0      ; IMOPT= 2. ; INCUT= 0.
PARAM SLA= 0.0025 ; CLAI= 0.8 ; NITMAX= 3.34 ; NITR= 3.34 ; RDRD= 0.01
PARAM TMBAS1= 3. ; CWGHT= 1800.

* Parameters for water relations from LINGRA for thimothee
PARAM DRATE  = 50.   ; IRRIGF = 1.   ; ROOTDM = 0.4   ; RRDMAX = 0.012
PARAM WCAD   = 0.005; WCWP   = 0.12  ; WCFC   = 0.29
PARAM WCI    =  0.29 ; WCWET  = 0.37 ; WCST   = 0.41



* Harvest dates
PARAM MNDAT(1)= 135.; MNDAT(2)=165. ; MNDAT(3)= 200.;  ...
      MNDAT(4)= 240.; MNDAT(5)= 280.;  MNDAT(6:NH)=999.

************************************************************************
***   11. Data
************************************************************************


FUNCTION LUERD1 = -20.,    0.,  3.,  0.,  8.,  1., 40.,   1.
FUNCTION LUERD2 = 0.,  1.,  10.,  1.,  40.,  0.33
FUNCTION FRRTTB = -10., 0.263, 0., 0.263, 1., 0.165

**************************** RERUNS ************************************

* rerun without irrigation
END
PARAM RUN = 1.
PARAM IRRIGF= 0.0
END


STOP

************************** SUBROUTINES *********************************

* ---------------------------------------------------------------------*
*     SUBROUTINE PENMAN                                                *
*     Purpose: Computation of the PENMAN EQUATION                      *
* ---------------------------------------------------------------------*
      SUBROUTINE PENMAN(DAVTMP,VP,DTR,LAI,WN,RNINTC,
     $                  PEVAP,PTRAN)
      IMPLICIT REAL (A-Z)

      DTRJM2 = DTR * 1.E6
      BOLTZM = 5.668E-8
      LHVAP  = 2.4E6
      PSYCH  = 0.067

      BBRAD  = BOLTZM * (DAVTMP+273.)**4 * 86400.
      SVP    = 0.611 * EXP(17.4 * DAVTMP / (DAVTMP + 239.))
      VP     = MIN(VP, SVP)
      SLOPE  = 4158.6 * SVP / (DAVTMP + 239.)**2
      RLWN   = BBRAD * MAX(0.,0.55*(1.-VP/SVP))
      NRADS  = DTRJM2 * (1.-0.15) - RLWN
      NRADC  = DTRJM2 * (1.-0.25) - RLWN
      PENMRS = NRADS * SLOPE/(SLOPE+PSYCH)
      PENMRC = NRADC * SLOPE/(SLOPE+PSYCH)

      WDF    = 2.63 * (1.0 + 0.54 * WN)
      PENMD  = LHVAP * WDF * (SVP-VP) * PSYCH/(SLOPE+PSYCH)

      PEVAP  =  Max(0.0,EXP(-0.5*LAI)  * (PENMRS + PENMD) / LHVAP)
      PTRAN  = (1.-EXP(-0.5*LAI)) * (PENMRC + PENMD) / LHVAP
      PTRAN  = MAX( 0.0, PTRAN-0.5*RNINTC )

      RETURN
      END

* ---------------------------------------------------------------------*
*     SUBROUTINE EVAPTR                                                *
*     Purpose: To compute actual rates of evaporation and transpiration*
* ---------------------------------------------------------------------*
      SUBROUTINE EVAPTR(PEVAP,PTRAN,ROOTD,WA,WCAD,WCWP,WCFC,WCWET,WCST,
     $                  DELT,EVAP2,TRAN)
      IMPLICIT REAL (A-Z)

      WC   = 0.001 * WA   / ROOTD
      WAAD = 1000. * WCAD * ROOTD
      WAFC = 1000. * WCFC * ROOTD

      EVAP1  = PEVAP * LIMIT( 0., 1.,(WC-WCAD)/(WCFC-WCAD) )

         WCCR = WCWP + 0.5 * (WCFC-WCWP)
         IF (WC.GT.WCCR) THEN 
             FR = LIMIT( 0., 1., (WCST-WC)/(WCST-WCWET) )
         ELSE
             FR = LIMIT( 0., 1., (WC-WCWP)/(WCCR-WCWP)  )
         ENDIF
      TRAN = PTRAN * FR

         AVAILF = MIN( 1., ((WA-WAAD)/DELT)/NOTNUL(EVAP1+TRAN) )
      EVAP2 = EVAP1 * AVAILF
      TRAN = TRAN * AVAILF

      RETURN
      END

* ---------------------------------------------------------------------*
*     SUBROUTINE DRUNIR                                                *
*     Purpose: To compute rates of drainage, runoff and irrigation     *
* ---------------------------------------------------------------------*
      SUBROUTINE DRUNIR(RAIN,RNINTC,EVAP,TRAN,IRRIGF,
     $                  DRATE,DELT,WA,ROOTD,WCFC,WCST,
     $                  DRAIN,RUNOFF,IRRIG)
      IMPLICIT REAL (A-Z)

      WC   = 0.001 * WA   / ROOTD
      WAFC = 1000. * WCFC * ROOTD
      WAST = 1000. * WCST * ROOTD

      DRAIN  = LIMIT( 0., DRATE, (WA-WAFC)/DELT +
     $               (RAIN - RNINTC - EVAP - TRAN)                  )

      RUNOFF =          MAX( 0., (WA-WAST)/DELT +
     $               (RAIN - RNINTC - EVAP - TRAN - DRAIN)          )

      IRRIG  = IRRIGF *    (     (WAFC-WA)/DELT -
     $               (RAIN - RNINTC - EVAP - TRAN - DRAIN - RUNOFF) )

      RETURN
      END

C ------------------------------------------------------------------------
C     Author       : A.H.C.M Schapendonk, B.A.M. Bouman, D.W.G. van Kraalingen
C                    and W. Stol              Company : AB-DLO
C     Date  V1.0   : 4 april 1996
C
C     Subroutine    : SOSUB
C
C     Purpose  : Calculation of source-limited growth of total
C                  : weight of perennial ryegrass.
C ------------------------------------------------------------------------                  :
C                    name   type    description                                     unit
C
C     Parameters in: PARINT REAL    Intercepted photosynthetic active radiation
C                                                                         MJ PAR.m-2.d-1
C                    LUE     REAL   Light use efficiency                   g dm MJ PAR-1
C                    CO2A    REAL   Atmospheric CO2 concentration                    ppm
C                    NITR    REAL   Actual nitrogen content                      kg.kg-1
C                    NITMAX  REAL   Maximum nitrogen content                     kg.kg-1
C                    TRANRF  REAL   Transpiration reduction factor                     -
C                    HARV    REAL   Daily harvest rate of dry matter         kg.ha-1.d-1
C
C    Parameters out: GTWSO   REAL   Source-limited growth of total weight    kg.ha-1.d-1
C                    LUED    REAL   Actual light use efficiency            g dm.MJ PAR-1
C
C --------------------------------------------------------------------------
C
      SUBROUTINE SOSUB (PARINT, LUE, CO2A, NITR, NITMAX,
     &                  TRANRF, HARV, LUED, GTWSO)
C -------------------------------------------------------------------------
C      Formal parameters: declaration
C -------------------------------------------------------------------------
      IMPLICIT REAL (A-Z)
C
      LUED = MIN (LUE * (0.336+0.224*NITR)/(0.336+0.224*NITMAX),
     $       LUE*TRANRF)
C
C     start of growing season
      GTWSO = 0.
C
      IF (HARV.EQ.0.) THEN
C        normal growth
C        (10: conversion from g m-2 d-1 to kg ha-1 d-1)
         GTWSO = LUED * PARINT * (1.+0.8*LOG (CO2A/360.)) * 10.
      END IF
C
      RETURN
      END

C -------------------------------------------------------------------------
C      End of SOSUB
C -------------------------------------------------------------------------

C -------------------------------------------------------------------------
C     Authors       : A.H.C.M Schapendonk, B.A.M. Bouman, D.W.G. van Kraalingen
C                    and W. Stol              Company : AB-DLO
C     Date  V1.0   : 4 april 1996
C
C     Subroutine    : TILSUB
C
C     Purpose  : Calculation of tiller growth rate of perennial ryegrass.
C -------------------------------------------------------------------------
C
C                    name   type    description                                     unit
C     Parameters in: TILLER  REAL   Tiller number                             tiller.m-2
C                    FSMAX   REAL   Maximum site filling new buds    tiller.tiller-1.d-1
C                    LAI     REAL   Green leaf area index            ha leaf.ha-1 ground
C                    LAICR   REAL   Critical leaf area index beyond which death to
C                                   self-shading occurs              ha leaf.ha-1 ground
C                    DAHA    REAL   Days after harvest                                 d
C                    LEAFN   REAL   Leaf appearance rate                 leaf.leaf-1.d-1
C                    TSUM    REAL   Temperature sum above base temperature        gr.d-1
C                    RED     REAL   Temperature reduction factor on light use efficiency
C                                                                                      -
C     Parameters out:DTIL    REAL   Rate of tiller emergence              tiller.m-2.d-1
C
C -------------------------------------------------------------------------
      SUBROUTINE TILSUB (TILLER,FSMAX,LAI,LAICR,DAHA,
     &                   LEAFN,TSUM,RED,DTIL)
C -------------------------------------------------------------------------
C      Formal parameters: declaration
C -------------------------------------------------------------------------
      IMPLICIT REAL (A-Z)
C -------------------------------------------------------------------------
C     Local variables, necessary since IMPLICIT NONE
C -------------------------------------------------------------------------
C
      DTIL = 0.

      IF (DAHA.LT.8.) THEN
C        Relative rate of tiller formation when defoliation less
C        than 8 days ago, tiller tiller-1 d-1
         REFTIL = MAX (0., 0.335-0.067*LAI) * RED
      ELSE
C        Relative rate of tiller formation when defoliation is more
C        than 8 days ago, tiller tiller-1 d-1
         REFTIL = LIMIT (0., FSMAX, 0.867-0.183*LAI) * RED
      END IF

C     Relative death rate of tillers due to self-shading (DTILD),
C     tiller tiller-1 d-1
      DTILD = MAX (0.01*(1.+TSUM/600.), 0.05 * (LAI-LAICR)/LAICR)
C
      IF (TILLER.LE.14000.) THEN
         DTIL = (REFTIL-DTILD) * LEAFN * TILLER
      ELSE
         DTIL = -DTILD * LEAFN * TILLER
      END IF

      RETURN
      END
C -------------------------------------------------------------------------
C      End of TILSUB
C -------------------------------------------------------------------------

C ------------------------------------------------------------------------
C     Subroutine MOWING
C     Author       : A.H.C.M Schapendonk, B.A.M. Bouman, D.W.G. van Kraalingen
C                    and W. Stol              Company : AB-DLO
C     Date  V1.0   : 4 april 1996
C
C     Purpose  : Calculation of dry weight of harvested leaves
C                of perennial ryegrass and number of days since harvest.
C ------------------------------------------------------------------------                  :
C
C                    name   type    description                                     unit
C     Parameters in: IMOPT  REAL    Switch variable that defines crop management       -
C                    INCUT  REAL    Number of swards harvested (cuttings)              -
C                    MNDAT  REAL    Data of periodical harvests                        d
C                    NH     INTEGER Maximum number of periodical harvests              -
C                    TIME   REAL    Day number within year of simulation               d
C                    WLVG   REAL    Dry weight of green leaves                   kg.ha-1
C                    CWGHT  REAL    Dry weight of green leaves after which
C                                   cutting of sward is initiated                kg.ha-1
C                    CWLVG  REAL    Remaining dry weight of green leaves after
C                                   cutting of sward                             kg.ha-1
C                    DAHA   REAL    Number of days after harvest                       -
C     Parameters out:RDAHA  REAL    Rate of number of days after harvest               -
C                    HARV   REAL    Dry weight of harvested green leaves         kg.ha-1
C --------------------------------------------------------------------------

      SUBROUTINE MOWING (IMOPT,INCUT,MNDAT,NH,TIME,WLVG,
     &                   CWGHT,CWLVG,DAHA,RDAHA,HARV)
      IMPLICIT NONE
C -------------------------------------------------------------------------
C      Formal parameters: declaration
C -------------------------------------------------------------------------
      INTEGER NH, I1
      REAL MNDAT(NH)
      REAL   TIME,  WLVG, CWGHT, CWLVG, DAHA, RDAHA, HARV, IMOPT, INCUT
      LOGICAL MOWDAY
C
      MOWDAY = .FALSE.
      DO 10 I1 = 1,NH
         IF (TIME .EQ. MNDAT(I1)) MOWDAY = .TRUE.
 10   CONTINUE

C     mowing at criterium of WLVG: CWGHT
C     reset days after HARV
      IF (IMOPT.EQ.1. .AND.WLVG.GE.CWGHT) THEN

         HARV  = WLVG-CWLVG
         RDAHA = -DAHA
         INCUT = INCUT + 1.

C     mowing at observation dates, periodical harvests
C     reset days after HARV
      ELSE IF (IMOPT.EQ.2. .AND.MOWDAY.AND.WLVG.GT.CWLVG) THEN

         HARV  = WLVG-CWLVG
         RDAHA = -DAHA
         INCUT = INCUT + 1.

C     no mowing in current season, do not increase rate
C     of days after HARV
      ELSE IF (INCUT.EQ. 0.) THEN

         HARV  = 0.
         RDAHA = 0.

C     mowing in current season, increase rate of days
C     after harvests
      ELSE IF (INCUT.NE. 0.) THEN

         HARV  = 0.
         RDAHA = 1.

      END IF

      RETURN
      END

C -------------------------------------------------------------------------
C      End of MOWING
C -------------------------------------------------------------------------


