!----------------------------------------------------------------------!
! General info about this file                                         !
!                                                                      !
! Contents      : Generated Fortran program                            !
! Creator       : FST translator version 3.00                          !
! Creation date : 20-Nov-2009, 12:17:05                                !
! Source file   : LINGRATW-NEW-JRC.FST                                 !
!----------------------------------------------------------------------!
 
!----------------------------------------------------------------------!
!                       STANDARD MAIN PROGRAM                          !
!----------------------------------------------------------------------!
  PROGRAM MAIN
 
! Calls subroutine RERUNS, from where EUDRIV or RKDRIV are
! called for all reruns. These drivers run the sumulation.
!
! Libraries used: DRIVERS and TTUTIL
 
! module use
  USE GeneralDrivers, ONLY: GeneralDriversVERSION
 
  IMPLICIT NONE
 
! administration
  CHARACTER(LEN=*), PARAMETER :: PrName    = 'MAIN'
  CHARACTER(LEN=*), PARAMETER :: PrVersion = 'for FST 3.00'
  CHARACTER(LEN=*), PARAMETER :: PrAuthor  = 'Kees Rappoldt 1995 2008'
 
! local
  INTEGER :: OutdatUnit, LogUnit, ITMP, Getun2
  LOGICAL :: TOSCR, TOLOG
 
  INTERFACE
!    interface to MODEL subroutine
     SUBROUTINE Model (ITASK,OUTPUT,TIME,STATE,RATE,SCALE,NDECL,NEQ)
     INTEGER :: ITASK, NDECL, NEQ
     REAL    :: TIME,STATE(NDECL),RATE(NDECL),SCALE(NDECL)
     LOGICAL :: OUTPUT
     END SUBROUTINE Model
  END INTERFACE
 
! open logfile
  call OpenLogF (.false., 'model', PrName, PrVersion, PrAuthor, .true.)
  call GeneralDriversVERSION
  call MESSINQ (TOSCR, TOLOG, LogUnit) ; TOSCR = .true.
  call MESSINI (TOSCR, TOLOG, LogUnit)
 
! open results file
  OutdatUnit = Getun2 (30,39,2)
  call FOPENS (OutdatUnit,'RES.DAT','NEW','DEL')
! all model runs
  call RERUNS (OutdatUnit,'TIMER.DAT','MODEL.DAT',MODEL)
! close results file .and. temporary output (res.bin)
  close (OutdatUnit) ; close (OutdatUnit+1)
 
! close all RD* temporary files using the free unit number OutdatUnit
  call RDDTMP (OutdatUnit)
 
! close logfile
  close (LogUnit)
! check for used units (can be switched on for debugging purposes)
! call USEDUN(10,99)
  END PROGRAM MAIN
!----------------------------------------------------------------------!
!                 TRANSLATED SIMULATION MODEL                          !
!----------------------------------------------------------------------!
  SUBROUTINE Model (ITASK,OUTPUT,TIME,STATE,RATE,SCALE,NDECL,NEQ)
 
! Model subroutine for use with driver EUDRIV or RKDRIV
! This subroutine should be linked with all SUBROUTINES used
! (as far as they are not included in the FST source file), with
! the library containing the simulation DRIVERS and with TTUTIL.
!
! ================ TITLE of the FST model ================
! LINGRA-new-JRC
!
! The STANDARD (!!) parameter list of this model subroutine:
!
! ITASK  - task of model routine                    I
! OUTPUT = .TRUE. output request                    I
! TIME   - time                                     I
! STATE  - state array of model                    I/O
! RATE   - rates of change belonging to STATE      I/O
! SCALE  - size scale of state variables           I/O
! NDECL  - declared size of arrays                  I
! NEQ    - Number of state variables, for ITASK=1   O
!                                       otherwise   I
 
  USE CHART
  USE GeneralDrivers
 
  IMPLICIT NONE
! formal
  INTEGER :: ITASK, NDECL, NEQ
  REAL    :: TIME, STATE(NDECL), RATE(NDECL), SCALE(NDECL)
  LOGICAL :: OUTPUT
 
! Number of state variables NSV
  INTEGER, PARAMETER :: NSV = 20
 
! Array size variables
  INTEGER, PARAMETER :: NH = 10
 
! State variables, initial values and rates
  REAL SOITMP, SOITMI, RSOITM
  REAL PARCU, ZERO, PARINT
  REAL TRACU, TRAN
  REAL TRAMCU, PTRAN
  REAL EVACU, EVAP
  REAL EVAMCU, PEVAP
  REAL IRRCU, IRRIG
  REAL TSUM, TMEFF
  REAL LAI, LAII, RLAI
  REAL DAHA, RDAHA
  REAL TILLER, TILLI, DTIL
  REAL WLVG, WLVGI, RLV
  REAL WLVD, DLV
  REAL GRASS, HARV
  REAL WRE, WREI, RRE
  REAL WRT, WRTI, GRT
  REAL LENGTH, LERA2
  REAL ROOTD, ROOTDI, RROOTD
  REAL WA, WAI, RWA
  REAL RAINCU
 
! Model parameters
  REAL CLAI, CO2A, CWGHT, DRATE, IMOPT, INCUT, IRRIGF, KDIF, LAICR, LUEMAX, NITMAX, NITR, RDRD, ROOTDM, RRDMAX, RUN, SLA, TBASE
  REAL TMBAS1, WCAD, WCFC, WCI, WCST, WCWET, WCWP
  REAL MNDAT(1:NH)
 
! Setting variables (Redefinable in events)
! none
 
! Calculated variables
  REAL CWLVG, DAVTMP, DAYL, DEC, DECC, DLAI, DLAIS, DRAIN, DRE, DTR, DVS, EFFTMP, EXPLOR, FINT, FLV, FRT, FSMAX, GLAI, GLV, GRE
  REAL GTW, GTWSI, GTWSO1, GTWSO2, HRVBL, LEAFN, LERA, LUE1, LUE2, LUEYCU, NEWBIO, PAR, PARAV, PHOTMP, PI, RAD, RDR, RDRS, RDRSH
  REAL RDRSM, REDRDD, REDTMP, RNINTC, RRATIO, RUNNR, RUNOFF, SLAINT, TADRW, TRANRF, WC, WLVD1, WRTMIN, YIELD
 
! Interpolation functions used in AFGEN en CSPLIN functions
  INTEGER :: ILFRRTTB
  INTEGER, PARAMETER :: IMFRRTTB = 40
  REAL, DIMENSION(IMFRRTTB) :: FRRTTB
 
  INTEGER :: ILLUERD1
  INTEGER, PARAMETER :: IMLUERD1 = 40
  REAL, DIMENSION(IMLUERD1) :: LUERD1
 
  INTEGER :: ILLUERD2
  INTEGER, PARAMETER :: IMLUERD2 = 40
  REAL, DIMENSION(IMLUERD2) :: LUERD2
 
! Declarations and values of constants
! none
 
! Variables supplied by the calendar
! Note that iYear is the current year ; NOT the start year
  INTEGER :: StartYear
  REAL    :: StartDOY, OneDay
  INTEGER :: iYear, iDOY, iHourOfDay
  INTEGER :: ClockSec, ClockMin, ClockHour, ClockDay, ClockMonth, ClockYear
  REAL    :: Year, DOY, ClockTime, SimDays, FractSec
  DOUBLE PRECISION TTutilDTDP
! Variables supplied by the weather system
  REAL LAT, LONG, ELEV, RDD, TMMN, TMMX, VP, WN, RAIN
! Variables for check on weather data
  LOGICAL WTRTER
! Code for the use of RDD, TMMN, TMMX, VP, WN, RAIN  (in that order)
! a letter 'U' indicates that the variable is Used in calculations
  CHARACTER(LEN=6), PARAMETER :: WUSED = 'UUUUUU'
 
! other
  INTEGER IUMOD
  INTEGER Getun2
  REAL LINT2, FCNSW, INSW, LIMIT, NOTNUL, REAAND
 
! loop counter for array variables
  INTEGER i
  SAVE
 
! Get weather and calendar data
  if (ITASK==1 .or. ITASK==2 .or. ITASK==4) then
     call Calendar (TimeDP, iYear, iDOY, iHourOfDay,                   &
                 Year, DOY, ClockTime, SimDays, TTutilDTDP, FractSec,  &
                 ClockSec, ClockMin, ClockHour, ClockDay, ClockMonth, ClockYear)
     call WTRINT (ITASK, WUSED, WTRTER, iYear, iDOY, LAT, LONG, ELEV,  &
                  RDD, TMMN, TMMX, VP, WN, RAIN)
     if (WTRTER) then
!       weather data error; prevent initial error on NEQ
        NEQ = NSV
        if (ITASK==1) then
           call FatalERR ('Model','No weather file(s) found')
        else
           TERMNL = .true.
           Return
        end if
     end if
  end if
 
  if (ITASK == 1) then
 
!    Initial section
!    ===============
 
!    Check values of array size variables
     if (NH < 6) call FatalERR ('Model','Array size NH is too small')
 
!    Check size of NDEC against number of states NSV
     if (NDEC.LT.NSV) then
!       driver capacity too low ; stop program
        WRITE (*,'(A,I4,/,A,I4)') '  The number of state variables is',NSV, &
         ' and the capacity of the driver is',NDEC
        call FatalERR ('MODEL','Driver capacity too low')
     end if
 
!    get calendar connection variables
     call CalendarConnection (StartYear, StartDOY, OneDay)
 
!    Open input file
     IUMOD = Getun2 (10,29,2)
     call RDINIT (IUMOD,IULOG, ModelFile)
 
!    Read initial states
     call RDSREA ('LAII', LAII)
     call RDSREA ('ROOTDI', ROOTDI)
     call RDSREA ('SOITMI', SOITMI)
     call RDSREA ('TILLI', TILLI)
     call RDSREA ('WREI', WREI)
     call RDSREA ('WRTI', WRTI)
     call RDSREA ('ZERO', ZERO)
 
!    Read model parameters
     call RDSREA ('CLAI', CLAI)
     call RDSREA ('CO2A', CO2A)
     call RDSREA ('CWGHT', CWGHT)
     call RDSREA ('DRATE', DRATE)
     call RDSREA ('IMOPT', IMOPT)
     call RDSREA ('INCUT', INCUT)
     call RDSREA ('IRRIGF', IRRIGF)
     call RDSREA ('KDIF', KDIF)
     call RDSREA ('LAICR', LAICR)
     call RDSREA ('LUEMAX', LUEMAX)
     call RDSREA ('NITMAX', NITMAX)
     call RDSREA ('NITR', NITR)
     call RDSREA ('RDRD', RDRD)
     call RDSREA ('ROOTDM', ROOTDM)
     call RDSREA ('RRDMAX', RRDMAX)
     call RDSREA ('RUN', RUN)
     call RDSREA ('SLA', SLA)
     call RDSREA ('TBASE', TBASE)
     call RDSREA ('TMBAS1', TMBAS1)
     call RDSREA ('WCAD', WCAD)
     call RDSREA ('WCFC', WCFC)
     call RDSREA ('WCI', WCI)
     call RDSREA ('WCST', WCST)
     call RDSREA ('WCWET', WCWET)
     call RDSREA ('WCWP', WCWP)
     call RDFREA ('MNDAT', MNDAT, NH, NH)
 
!    Read LINT/CSPLIN interpolation functions
     call RDAREA ('FRRTTB', FRRTTB, IMFRRTTB, ILFRRTTB)
     call RDAREA ('LUERD1', LUERD1, IMLUERD1, ILLUERD1)
     call RDAREA ('LUERD2', LUERD2, IMLUERD2, ILLUERD2)
 
!    Read SCALE array and close datafile
     call RDFREA ('SCALExxx',SCALE,NDEC,NSV)
     close (IUMOD)
 
!    Set number of state variables
     NEQ = NSV
 
!    initial calculations
     WAI    = 1000. * ROOTDI * WCI
!      Initial leaf weight is initialized as initial
!      leaf area divided by initial specific leaf area, kg ha-1
     WLVGI   = LAII / SLA
! 
!      Remaining leaf weight after cutting is initialized at remaining
!      leaf area after cutting divided by initial specific leaf area, kg ha-1
     CWLVG = CLAI/SLA
!       Maximum site filling new buds (FSMAX) decreases due
!       to low nitrogen contents, Van Loo and Schapendonk (1992)
!       Theoretical maximum tillering size = 0.693
     FSMAX = NITR/NITMAX*0.693
 
!    initially known variables to output
     call ChartInitialGroup
     call ChartOutputRealScalar ('TIME',TIME)
     call OUTDAT (2, 0, 'TIME  ', TIME  )
     call OUTDAT (2, 0, 'SLA', SLA)
     call ChartOutputRealScalar('SLA', SLA)
 
!    send title(s) to OUTCOM
     call OUTCOM ('LINGRA-new-JRC')
 
!    Initialize state variables
     SOITMP = SOITMI
     PARCU = ZERO
     TRACU = ZERO
     TRAMCU = ZERO
     EVACU = ZERO
     EVAMCU = ZERO
     IRRCU = ZERO
     TSUM = ZERO
     LAI = LAII
     DAHA = ZERO
     TILLER = TILLI
     WLVG = WLVGI
     WLVD = ZERO
     GRASS = ZERO
     WRE = WREI
     WRT = WRTI
     LENGTH = ZERO
     ROOTD = ROOTDI
     WA = WAI
     RAINCU = ZERO
 
!    assign local variable names to state array
     STATE(1) = SOITMP
     STATE(2) = PARCU
     STATE(3) = TRACU
     STATE(4) = TRAMCU
     STATE(5) = EVACU
     STATE(6) = EVAMCU
     STATE(7) = IRRCU
     STATE(8) = TSUM
     STATE(9) = LAI
     STATE(10) = DAHA
     STATE(11) = TILLER
     STATE(12) = WLVG
     STATE(13) = WLVD
     STATE(14) = GRASS
     STATE(15) = WRE
     STATE(16) = WRT
     STATE(17) = LENGTH
     STATE(18) = ROOTD
     STATE(19) = WA
     STATE(20) = RAINCU
 
  else if (ITASK == 2) then
 
!    rates of change section
!    =======================
!    Assign state array to local variable names
     SOITMP = STATE(1)
     PARCU = STATE(2)
     TRACU = STATE(3)
     TRAMCU = STATE(4)
     EVACU = STATE(5)
     EVAMCU = STATE(6)
     IRRCU = STATE(7)
     TSUM = STATE(8)
     LAI = STATE(9)
     DAHA = STATE(10)
     TILLER = STATE(11)
     WLVG = STATE(12)
     WLVD = STATE(13)
     GRASS = STATE(14)
     WRE = STATE(15)
     WRT = STATE(16)
     LENGTH = STATE(17)
     ROOTD = STATE(18)
     WA = STATE(19)
     RAINCU = STATE(20)
 
!    dynamic calculations
     DAVTMP = 0.5 * (TMMN + TMMX)
     PHOTMP= (TMMN + 3. * TMMX)/4.
     PI     = 3.1416
!      From J/m2/d to MJ/m2/d
     DTR    = RDD / 1.E+6
! 
!         Hypothetical development stage, 600 gr. C.d taken from
!         subroutine TILSUB
     DVS = TSUM / 600.
!         Dry weight of dead leaves, kg ha-1
     WLVD1= WLVD - GRASS
  
!         Harvestable leaf weight
     HRVBL=WLVG-CWLVG
! 
!         Total above ground dry weight including harvests, kg ha-1
     TADRW = GRASS + WLVG
! 
!         Running specific leaf area in model, ha kg-1
     SLAINT = LAI / NOTNUL(WLVG)
!    ***********************************************************************
!    **   4. Rate variables
!    ***********************************************************************
     REDTMP = LINT2('LUERD1',LUERD1,ILLUERD1,SOITMP)
  
     REDRDD = LINT2 ('LUERD2',LUERD2,ILLUERD2,RDD/1.E6)
! 
  
!         Daily photosynthetically active radiation, MJ m-2 d-1
     PAR = RDD/1.0E6 * 0.50
! 
!         Fraction of light interception
     FINT = (1.-EXP (-KDIF*LAI))
  
!         Call to subroutine for grassland management options
     CALL MOWING (IMOPT,INCUT,MNDAT,NH,TIME,WLVG, CWGHT,CWLVG,DAHA,RDAHA,HARV)
     DRE   = WRE/DELT
  
!         Relative death rate of leaves due to self-shading, d-1
     RDRSH = LIMIT (0., 0.03, 0.03 * (LAI-LAICR) /LAICR)
     RNINTC = MIN( RAIN, 0.25*LAI )
     WC     = 0.001 * WA / ROOTD
     
!    ***********************************************************************
!    **   9. Additional variables and parameters for output
!    ***********************************************************************
     
     NEWBIO = WLVG+WRT+WRE - (WLVGI+WRTI+WREI)  + GRASS
  
     WRTMIN = -WRT
     RUNNR  = RUN
! 
!         Light use efficiency, g MJ PAR-1
     LUE1 = LUEMAX * REDTMP * REDRDD
! 
!         Total intercepted photosynthetically active
!         radiation, MJ m-2 d-1
     PARINT = FINT * PAR
! 
!         Temperature dependent leaf appearance rate, according to
!         (Davies and Thomas, 1983), soil temperature (SOITMP)is used as
!         driving force which is estimated from a 10 day running
!         average
     LEAFN=   FCNSW(REDTMP, 0., 0.,SOITMP * 0.01 )
! 
!         Leaf elongation rate affected by temperature
!         cm day-1 tiller-1
     LERA= FCNSW(DAVTMP-TMBAS1, 0., 0.,0.83*LOG(MAX(DAVTMP, 2.))-0.8924 )
     RAD    = PI / 180.
! 
!         Harvestable part of total above ground dry weight
!         and previous harvests, kg ha-1
     YIELD = GRASS + MAX (0., HRVBL)
!    ***********************************************************************
!    **   5. Water balance and root depth growth (from LINGRA for thymothee)
!    ***********************************************************************
     
     CALL PENMAN( DAVTMP,VP,DTR,LAI,WN,RNINTC, PEVAP,PTRAN )
  
     RROOTD = RRDMAX * REAAND( ROOTDM-ROOTD, WC-WCWP )
!        soil temperature changes
     RSOITM = (DAVTMP-SOITMP) / 10.
  
     EFFTMP = MAX ( DAVTMP, TBASE )
     RRATIO = LIMIT  ( 0., 1., (WRT-WRTI) / NOTNUL(NEWBIO) )
! 
     TMEFF =  MAX (DAVTMP-TMBAS1, 0.)
     CALL EVAPTR( PEVAP,PTRAN,ROOTD,WA,WCAD,WCWP,WCFC,WCWET,WCST,DELT,EVAP,TRAN )
     EXPLOR = 1000. * RROOTD * WCFC
! 
     LERA2 = INSW (HARV-0.1, LERA, -LENGTH)
  
     CALL TILSUB (TILLER,FSMAX,LAI,LAICR,DAHA,LEAFN,TSUM,REDTMP, DTIL)
  
!         Rate of sink limited leaf growth, unit of TILLER is tillers m-2 (!),
!         1.0E-8 is conversion from cm-2 to ha-1, ha leaf ha ground-1 d-1
     DLAIS = (TILLER * 1.0E4 * (LERA * 0.3)) * 1.0E-8
  
     LUEYCU = YIELD  / NOTNUL(PARCU)
     DEC    = -ASIN (SIN (23.45*RAD)*COS (2.*PI*(DOY+10.)/365.))
     DECC   = LIMIT( ATAN(-1./TAN(RAD*LAT)), ATAN( 1./TAN(RAD*LAT)), DEC)
     CALL DRUNIR( RAIN,RNINTC,EVAP,TRAN,IRRIGF,DRATE,DELT,WA,ROOTD,WCFC,WCST,DRAIN,RUNOFF,IRRIG )
     TRANRF = INSW( -PTRAN, TRAN / NOTNUL(PTRAN), 1. )
! 
!         Relative death rate of leaves due to drought stress, d-1
     RDRSM = LIMIT(0., 0.05, 0.05 * (1.-TRANRF))
! 
!         Fraction of dry matter allocated to roots, kg kg-1
     FRT = LINT2 ('FRRTTB',FRRTTB,ILFRRTTB, TRANRF)
     RWA    = (RAIN+EXPLOR+IRRIG) - (RNINTC+RUNOFF+TRAN+EVAP+DRAIN)
  
!         Source limited growth rate of crop, kg ha-1 d-1
     CALL SOSUB (PARINT,LUE1,CO2A,NITR,NITMAX,TRANRF, HARV,LUE2,GTWSO1)
     DAYL   = 0.5 * ( 1. + 2. * ASIN(TAN(RAD*LAT)*TAN(DECC)) / PI )
! 
     GTWSO2 = GTWSO1+WRE/DELT
! 
!         Maximum of relative death rate of leaves due to
!         and drought stres, d-1
     RDRS = MAX (RDRSH, RDRSM)
     FLV = 1.-FRT
     PARAV  = 0.5 * DTR / DAYL
! 
!         Actual relative death rate of leaves is sum of base death
!         rate plus maximum of death rates RDRSM and RDRSH, d-1
     RDR = RDRD + RDRS
! 
!         Conversion to total sink limited carbon demand,
!         kg leaf ha ground-1 d-1
     GTWSI= FCNSW(HARV,DLAIS * (1./SLA) * (1./FLV),DLAIS * (1./SLA) * (1./FLV), 0.)
! 
!         Actual death rate of leaf area, due to relative death
!         rate of leaf area or rate of change due to cutting, ha ha-1 d-1
     DLAI= FCNSW(HARV, LAI * (1. - EXP(-RDR * DELT)), LAI * (1. - EXP(-RDR * DELT)),HARV*SLAINT )
! 
!         Actual growth switches between sink- and source limitation
!         (more or less dry matter formed than can be stored)
     GRE= FCNSW(GTWSO2-GTWSI,0.,0., GTWSO2-GTWSI)
     GTW= FCNSW(GTWSO2-GTWSI,GTWSO2,GTWSO2, GTWSI)
! 
!         Actual growth rate of roots, kg ha-1 d-1
     GRT = GTW * FRT
  
!         Actual growth rate of leaf area, ha ha-1 d-1
     GLAI = GTW * FLV * SLA
! 
!         rate of change of dry weight of green leaves due to
!         growth and senescence of leaves or periodical harvest, kg ha-1 d-1
     GLV= FCNSW(HARV,GTW*FLV,GTW*FLV, 0.)
  
!         Change in reserves
     RRE = GRE-DRE
  
!         Actual death rate of leaves, kg ha-1 d-1 incl. harvested leaves
     DLV = DLAI / NOTNUL (SLAINT)
  
!         Change in LAI
     RLAI= GLAI-DLAI
  
!         Change in green leaf weight
     RLV = GLV-DLV
 
!    finish conditions
     if (KEEP.EQ.1) then
        continue
     end if
 
!    output
     if (OUTPUT) then
        call ChartNewGroup
        call ChartOutputRealScalar ('TIME',TIME)
        call OUTDAT (2, 0, 'TIME  ', TIME  )
        call OUTDAT (2, 0, 'YEAR', YEAR)
        call ChartOutputRealScalar('YEAR', YEAR)
        call OUTDAT (2, 0, 'DAVTMP', DAVTMP)
        call ChartOutputRealScalar('DAVTMP', DAVTMP)
        call OUTDAT (2, 0, 'DTR', DTR)
        call ChartOutputRealScalar('DTR', DTR)
        call OUTDAT (2, 0, 'DAYL', DAYL)
        call ChartOutputRealScalar('DAYL', DAYL)
        call OUTDAT (2, 0, 'LAI', LAI)
        call ChartOutputRealScalar('LAI', LAI)
        call OUTDAT (2, 0, 'TILLER', TILLER)
        call ChartOutputRealScalar('TILLER', TILLER)
        call OUTDAT (2, 0, 'YIELD', YIELD)
        call ChartOutputRealScalar('YIELD', YIELD)
        call OUTDAT (2, 0, 'WLVG', WLVG)
        call ChartOutputRealScalar('WLVG', WLVG)
        call OUTDAT (2, 0, 'WLVD1', WLVD1)
        call ChartOutputRealScalar('WLVD1', WLVD1)
        call OUTDAT (2, 0, 'WRE', WRE)
        call ChartOutputRealScalar('WRE', WRE)
        call OUTDAT (2, 0, 'WRT', WRT)
        call ChartOutputRealScalar('WRT', WRT)
        call OUTDAT (2, 0, 'WRTMIN', WRTMIN)
        call ChartOutputRealScalar('WRTMIN', WRTMIN)
        call OUTDAT (2, 0, 'RRATIO', RRATIO)
        call ChartOutputRealScalar('RRATIO', RRATIO)
        call OUTDAT (2, 0, 'SLAINT', SLAINT)
        call ChartOutputRealScalar('SLAINT', SLAINT)
        call OUTDAT (2, 0, 'PARCU', PARCU)
        call ChartOutputRealScalar('PARCU', PARCU)
        call OUTDAT (2, 0, 'LUEYCU', LUEYCU)
        call ChartOutputRealScalar('LUEYCU', LUEYCU)
        call OUTDAT (2, 0, 'RUNNR', RUNNR)
        call ChartOutputRealScalar('RUNNR', RUNNR)
        call OUTDAT (2, 0, 'GRASS', GRASS)
        call ChartOutputRealScalar('GRASS', GRASS)
        call OUTDAT (2, 0, 'TADRW', TADRW)
        call ChartOutputRealScalar('TADRW', TADRW)
        call OUTDAT (2, 0, 'VP', VP)
        call ChartOutputRealScalar('VP', VP)
        call OUTDAT (2, 0, 'NEWBIO', NEWBIO)
        call ChartOutputRealScalar('NEWBIO', NEWBIO)
        call OUTDAT (2, 0, 'TRANRF', TRANRF)
        call ChartOutputRealScalar('TRANRF', TRANRF)
        call OUTDAT (2, 0, 'TRAN', TRAN)
        call ChartOutputRealScalar('TRAN', TRAN)
        call OUTDAT (2, 0, 'PTRAN', PTRAN)
        call ChartOutputRealScalar('PTRAN', PTRAN)
        call OUTDAT (2, 0, 'EVAP', EVAP)
        call ChartOutputRealScalar('EVAP', EVAP)
        call OUTDAT (2, 0, 'PEVAP', PEVAP)
        call ChartOutputRealScalar('PEVAP', PEVAP)
        call OUTDAT (2, 0, 'WC', WC)
        call ChartOutputRealScalar('WC', WC)
        call OUTDAT (2, 0, 'WA', WA)
        call ChartOutputRealScalar('WA', WA)
        call OUTDAT (2, 0, 'RAIN', RAIN)
        call ChartOutputRealScalar('RAIN', RAIN)
        call OUTDAT (2, 0, 'RAINCU', RAINCU)
        call ChartOutputRealScalar('RAINCU', RAINCU)
        call OUTDAT (2, 0, 'WN', WN)
        call ChartOutputRealScalar('WN', WN)
        call OUTDAT (2, 0, 'TRAMCU', TRAMCU)
        call ChartOutputRealScalar('TRAMCU', TRAMCU)
        call OUTDAT (2, 0, 'TRACU', TRACU)
        call ChartOutputRealScalar('TRACU', TRACU)
        call OUTDAT (2, 0, 'EVAMCU', EVAMCU)
        call ChartOutputRealScalar('EVAMCU', EVAMCU)
        call OUTDAT (2, 0, 'EVACU', EVACU)
        call ChartOutputRealScalar('EVACU', EVACU)
        call OUTDAT (2, 0, 'IRRCU', IRRCU)
        call ChartOutputRealScalar('IRRCU', IRRCU)
        call OUTDAT (2, 0, 'GTW', GTW)
        call ChartOutputRealScalar('GTW', GTW)
        call OUTDAT (2, 0, 'GTWSI', GTWSI)
        call ChartOutputRealScalar('GTWSI', GTWSI)
        call OUTDAT (2, 0, 'LENGTH', LENGTH)
        call ChartOutputRealScalar('LENGTH', LENGTH)
     end if
 
!    assign calculated rates to rate array
     RATE(1) = RSOITM
     RATE(2) = PARINT
     RATE(3) = TRAN
     RATE(4) = PTRAN
     RATE(5) = EVAP
     RATE(6) = PEVAP
     RATE(7) = IRRIG
     RATE(8) = TMEFF
     RATE(9) = RLAI
     RATE(10) = RDAHA
     RATE(11) = DTIL
     RATE(12) = RLV
     RATE(13) = DLV
     RATE(14) = HARV
     RATE(15) = RRE
     RATE(16) = GRT
     RATE(17) = LERA2
     RATE(18) = RROOTD
     RATE(19) = RWA
     RATE(20) = RAIN
 
  else if (ITASK == 4) then
 
!    terminal section
!    ================
!    assign terminal states and rates to local variable names
     SOITMP = STATE(1)
     PARCU = STATE(2)
     TRACU = STATE(3)
     TRAMCU = STATE(4)
     EVACU = STATE(5)
     EVAMCU = STATE(6)
     IRRCU = STATE(7)
     TSUM = STATE(8)
     LAI = STATE(9)
     DAHA = STATE(10)
     TILLER = STATE(11)
     WLVG = STATE(12)
     WLVD = STATE(13)
     GRASS = STATE(14)
     WRE = STATE(15)
     WRT = STATE(16)
     LENGTH = STATE(17)
     ROOTD = STATE(18)
     WA = STATE(19)
     RAINCU = STATE(20)
     RSOITM = RATE(1)
     PARINT = RATE(2)
     TRAN = RATE(3)
     PTRAN = RATE(4)
     EVAP = RATE(5)
     PEVAP = RATE(6)
     IRRIG = RATE(7)
     TMEFF = RATE(8)
     RLAI = RATE(9)
     RDAHA = RATE(10)
     DTIL = RATE(11)
     RLV = RATE(12)
     DLV = RATE(13)
     HARV = RATE(14)
     RRE = RATE(15)
     GRT = RATE(16)
     LERA2 = RATE(17)
     RROOTD = RATE(18)
     RWA = RATE(19)
     RAIN = RATE(20)
 
!    terminal calculations
!    none
 
!    terminal output
!    none
 
!    printplot output
!    none
  end if
 
  Return
  END SUBROUTINE Model
 
  SUBROUTINE FatalERR (MODULE,MESSAG)
! adapted version of FatalERR to write message to a file
  IMPLICIT NONE
 
! FORMAL_PARAMETERS:
  CHARACTER*(*) MODULE, MESSAG
 
! local variables
  INTEGER I,IL1,IL2
  INTEGER UNIT, GETUN
  SAVE
 
! fudge construction to fool the compiler about the return statement
  I = 0
  if (I.EQ.0) then
     IL1 = LEN_TRIM (MODULE)
     IL2 = LEN_TRIM (MESSAG)
     UNIT = GETUN(10,99)
     call FOPENS(UNIT,'MODEL_ERRORS.TXT','NEW','DEL')
     WRITE (*,'(A)') ' Fatal execution error, see file model_errors.txt'
     if (IL1.EQ.0.AND.IL2.EQ.0) then
        WRITE (UNIT,'(A)') ' Fatal execution error'
     else if (IL1.GT.0.AND.IL2.EQ.0) THEN
        WRITE (UNIT,'(2A)') ' Fatal execution error in ',MODULE(1:IL1)
     ELSE
        WRITE (UNIT,'(4A)') ' ERROR in ',MODULE(1:IL1),': ',MESSAG(1:IL2)
     end if
     close(UNIT)
     STOP
  end if
 
  Return
  END SUBROUTINE FatalERR
 
!************************* SUBROUTINES *********************************
 
! ---------------------------------------------------------------------*
!     SUBROUTINE PENMAN                                                *
!     Purpose: Computation of the PENMAN EQUATION                      *
! ---------------------------------------------------------------------*
      SUBROUTINE PENMAN(DAVTMP,VP,DTR,LAI,WN,RNINTC,                    &
                        PEVAP,PTRAN)
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
 
! ---------------------------------------------------------------------*
!     SUBROUTINE EVAPTR                                                *
!     Purpose: To compute actual rates of evaporation and transpiration*
! ---------------------------------------------------------------------*
      SUBROUTINE EVAPTR(PEVAP,PTRAN,ROOTD,WA,WCAD,WCWP,WCFC,WCWET,WCST, &
                        DELT,EVAP2,TRAN)
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
 
! ---------------------------------------------------------------------*
!     SUBROUTINE DRUNIR                                                *
!     Purpose: To compute rates of drainage, runoff and irrigation     *
! ---------------------------------------------------------------------*
      SUBROUTINE DRUNIR(RAIN,RNINTC,EVAP,TRAN,IRRIGF,                   &
                        DRATE,DELT,WA,ROOTD,WCFC,WCST,                  &
                        DRAIN,RUNOFF,IRRIG)
      IMPLICIT REAL (A-Z)
 
      WC   = 0.001 * WA   / ROOTD
      WAFC = 1000. * WCFC * ROOTD
      WAST = 1000. * WCST * ROOTD
 
      DRAIN  = LIMIT( 0., DRATE, (WA-WAFC)/DELT +                       &
                     (RAIN - RNINTC - EVAP - TRAN)                  )
 
      RUNOFF =          MAX( 0., (WA-WAST)/DELT +                       &
                     (RAIN - RNINTC - EVAP - TRAN - DRAIN)          )
 
      IRRIG  = IRRIGF *    (     (WAFC-WA)/DELT -                       &
                     (RAIN - RNINTC - EVAP - TRAN - DRAIN - RUNOFF) )
 
      RETURN
      END
 
! ------------------------------------------------------------------------
!     Author       : A.H.C.M Schapendonk, B.A.M. Bouman, D.W.G. van Kraalingen
!                    and W. Stol              Company : AB-DLO
!     Date  V1.0   : 4 april 1996
!
!     Subroutine    : SOSUB
!
!     Purpose  : Calculation of source-limited growth of total
!                  : weight of perennial ryegrass.
! ------------------------------------------------------------------------                  :
!                    name   type    description                                     unit
!
!     Parameters in: PARINT REAL    Intercepted photosynthetic active radiation
!                                                                         MJ PAR.m-2.d-1
!                    LUE     REAL   Light use efficiency                   g dm MJ PAR-1
!                    CO2A    REAL   Atmospheric CO2 concentration                    ppm
!                    NITR    REAL   Actual nitrogen content                      kg.kg-1
!                    NITMAX  REAL   Maximum nitrogen content                     kg.kg-1
!                    TRANRF  REAL   Transpiration reduction factor                     -
!                    HARV    REAL   Daily harvest rate of dry matter         kg.ha-1.d-1
!
!    Parameters out: GTWSO   REAL   Source-limited growth of total weight    kg.ha-1.d-1
!                    LUED    REAL   Actual light use efficiency            g dm.MJ PAR-1
!
! --------------------------------------------------------------------------
!
      SUBROUTINE SOSUB (PARINT, LUE, CO2A, NITR, NITMAX,                &
                        TRANRF, HARV, LUED, GTWSO)
! -------------------------------------------------------------------------
!      Formal parameters: declaration
! -------------------------------------------------------------------------
      IMPLICIT REAL (A-Z)
!
      LUED = MIN (LUE * (0.336+0.224*NITR)/(0.336+0.224*NITMAX),        &
             LUE*TRANRF)
!
!     start of growing season
      GTWSO = 0.
!
      IF (HARV.EQ.0.) THEN
!        normal growth
!        (10: conversion from g m-2 d-1 to kg ha-1 d-1)
         GTWSO = LUED * PARINT * (1.+0.8*LOG (CO2A/360.)) * 10.
      END IF
!
      RETURN
      END
 
! -------------------------------------------------------------------------
!      End of SOSUB
! -------------------------------------------------------------------------
 
! -------------------------------------------------------------------------
!     Authors       : A.H.C.M Schapendonk, B.A.M. Bouman, D.W.G. van Kraalingen
!                    and W. Stol              Company : AB-DLO
!     Date  V1.0   : 4 april 1996
!
!     Subroutine    : TILSUB
!
!     Purpose  : Calculation of tiller growth rate of perennial ryegrass.
! -------------------------------------------------------------------------
!
!                    name   type    description                                     unit
!     Parameters in: TILLER  REAL   Tiller number                             tiller.m-2
!                    FSMAX   REAL   Maximum site filling new buds    tiller.tiller-1.d-1
!                    LAI     REAL   Green leaf area index            ha leaf.ha-1 ground
!                    LAICR   REAL   Critical leaf area index beyond which death to
!                                   self-shading occurs              ha leaf.ha-1 ground
!                    DAHA    REAL   Days after harvest                                 d
!                    LEAFN   REAL   Leaf appearance rate                 leaf.leaf-1.d-1
!                    TSUM    REAL   Temperature sum above base temperature        gr.d-1
!                    RED     REAL   Temperature reduction factor on light use efficiency
!                                                                                      -
!     Parameters out:DTIL    REAL   Rate of tiller emergence              tiller.m-2.d-1
!
! -------------------------------------------------------------------------
      SUBROUTINE TILSUB (TILLER,FSMAX,LAI,LAICR,DAHA,                   &
                         LEAFN,TSUM,RED,DTIL)
! -------------------------------------------------------------------------
!      Formal parameters: declaration
! -------------------------------------------------------------------------
      IMPLICIT REAL (A-Z)
! -------------------------------------------------------------------------
!     Local variables, necessary since IMPLICIT NONE
! -------------------------------------------------------------------------
!
      DTIL = 0.
 
      IF (DAHA.LT.8.) THEN
!        Relative rate of tiller formation when defoliation less
!        than 8 days ago, tiller tiller-1 d-1
         REFTIL = MAX (0., 0.335-0.067*LAI) * RED
      ELSE
!        Relative rate of tiller formation when defoliation is more
!        than 8 days ago, tiller tiller-1 d-1
         REFTIL = LIMIT (0., FSMAX, 0.867-0.183*LAI) * RED
      END IF
 
!     Relative death rate of tillers due to self-shading (DTILD),
!     tiller tiller-1 d-1
      DTILD = MAX (0.01*(1.+TSUM/600.), 0.05 * (LAI-LAICR)/LAICR)
!
      IF (TILLER.LE.14000.) THEN
         DTIL = (REFTIL-DTILD) * LEAFN * TILLER
      ELSE
         DTIL = -DTILD * LEAFN * TILLER
      END IF
 
      RETURN
      END
! -------------------------------------------------------------------------
!      End of TILSUB
! -------------------------------------------------------------------------
 
! ------------------------------------------------------------------------
!     Subroutine MOWING
!     Author       : A.H.C.M Schapendonk, B.A.M. Bouman, D.W.G. van Kraalingen
!                    and W. Stol              Company : AB-DLO
!     Date  V1.0   : 4 april 1996
!
!     Purpose  : Calculation of dry weight of harvested leaves
!                of perennial ryegrass and number of days since harvest.
! ------------------------------------------------------------------------                  :
!
!                    name   type    description                                     unit
!     Parameters in: IMOPT  REAL    Switch variable that defines crop management       -
!                    INCUT  REAL    Number of swards harvested (cuttings)              -
!                    MNDAT  REAL    Data of periodical harvests                        d
!                    NH     INTEGER Maximum number of periodical harvests              -
!                    TIME   REAL    Day number within year of simulation               d
!                    WLVG   REAL    Dry weight of green leaves                   kg.ha-1
!                    CWGHT  REAL    Dry weight of green leaves after which
!                                   cutting of sward is initiated                kg.ha-1
!                    CWLVG  REAL    Remaining dry weight of green leaves after
!                                   cutting of sward                             kg.ha-1
!                    DAHA   REAL    Number of days after harvest                       -
!     Parameters out:RDAHA  REAL    Rate of number of days after harvest               -
!                    HARV   REAL    Dry weight of harvested green leaves         kg.ha-1
! --------------------------------------------------------------------------
 
      SUBROUTINE MOWING (IMOPT,INCUT,MNDAT,NH,TIME,WLVG,                &
                         CWGHT,CWLVG,DAHA,RDAHA,HARV)
      IMPLICIT NONE
! -------------------------------------------------------------------------
!      Formal parameters: declaration
! -------------------------------------------------------------------------
      INTEGER NH, I1
      REAL MNDAT(NH)
      REAL   TIME,  WLVG, CWGHT, CWLVG, DAHA, RDAHA, HARV, IMOPT, INCUT
      LOGICAL MOWDAY
!
      MOWDAY = .FALSE.
      DO 10 I1 = 1,NH
         IF (TIME .EQ. MNDAT(I1)) MOWDAY = .TRUE.
 10   CONTINUE
 
!     mowing at criterium of WLVG: CWGHT
!     reset days after HARV
      IF (IMOPT.EQ.1. .AND.WLVG.GE.CWGHT) THEN
 
         HARV  = WLVG-CWLVG
         RDAHA = -DAHA
         INCUT = INCUT + 1.
 
!     mowing at observation dates, periodical harvests
!     reset days after HARV
      ELSE IF (IMOPT.EQ.2. .AND.MOWDAY.AND.WLVG.GT.CWLVG) THEN
 
         HARV  = WLVG-CWLVG
         RDAHA = -DAHA
         INCUT = INCUT + 1.
 
!     no mowing in current season, do not increase rate
!     of days after HARV
      ELSE IF (INCUT.EQ. 0.) THEN
 
         HARV  = 0.
         RDAHA = 0.
 
!     mowing in current season, increase rate of days
!     after harvests
      ELSE IF (INCUT.NE. 0.) THEN
 
         HARV  = 0.
         RDAHA = 1.
 
      END IF
 
      RETURN
      END
