


MODULE MODULE_RA_GFDLETA
      USE MODULE_CONFIGURE,ONLY : GRID_CONFIG_REC_TYPE
      USE MODULE_MODEL_CONSTANTS

      USE MODULE_MP_ETANEW, ONLY : FPVS,GPVS

      INTEGER,PARAMETER :: NL=81
      INTEGER,PARAMETER :: NBLY=15
      REAL,PARAMETER :: RTHRESH=1.E-15,RTD=1./DEGRAD

      INTEGER, SAVE, DIMENSION(3)     :: LTOP
      REAL   , SAVE, DIMENSION(37,NL) :: XDUO3N,XDO3N2,XDO3N3,XDO3N4
      REAL   , SAVE, DIMENSION(NL)    :: PRGFDL
      REAL   , SAVE                   :: AB15WD,SKO2D,SKC1R,SKO3R

      REAL   , SAVE :: EM1(28,180),EM1WDE(28,180),TABLE1(28,180),     &
                           TABLE2(28,180),TABLE3(28,180),EM3(28,180), &
                           SOURCE(28,NBLY), DSRCE(28,NBLY)

      REAL   ,SAVE, DIMENSION(5040):: T1,T2,T4,EM1V,EM1VW,EM3V
      REAL   ,SAVE                 :: R1,RSIN1,RCOS1,RCOS2

      REAL,   SAVE, ALLOCATABLE, DIMENSION(:,:) :: CO251,CDT51,CDT58,C2D51,&
                                           C2D58,CO258
      REAL,   SAVE, ALLOCATABLE, DIMENSION(:)   :: STEMP,GTEMP,CO231,CO238, &
                                           C2D31,C2D38,CDT31,CDT38, &
                                           CO271,CO278,C2D71,C2D78, &
                                           CDT71,CDT78
      REAL,   SAVE, ALLOCATABLE, DIMENSION(:)   :: CO2M51,CO2M58,CDTM51,CDTM58, &
                                           C2DM51,C2DM58
      CHARACTER(256) :: ERRMESS





      REAL   ,SAVE, DIMENSION(109) :: PA, XA, CA, ETA, SEXPV
      REAL   ,SAVE, DIMENSION(109,109) :: TRANSA
      REAL   ,SAVE  :: CORE,UEXP,SEXP

      EQUIVALENCE (EM1V(1),EM1(1,1)),(EM1VW(1),EM1WDE(1,1)) 
      EQUIVALENCE (EM3V(1),EM3(1,1))
      EQUIVALENCE (T1(1),TABLE1(1,1)),(T2(1),TABLE2(1,1)), &
                  (T4(1),TABLE3(1,1))
      REAL,SAVE,DIMENSION(4) :: PTOPC



      REAL, PRIVATE,PARAMETER :: XSDmax=3.1, DXSD=.01
      INTEGER, PRIVATE,PARAMETER :: NXSD=XSDmax/DXSD
      REAL, DIMENSION(NXSD),PRIVATE,SAVE :: AXSD
      REAL, PRIVATE :: RSQR
      LOGICAL, PRIVATE,SAVE :: SDprint=.FALSE.


      REAL, PRIVATE, PARAMETER :: RHgrd=1.0
      REAL, PRIVATE, PARAMETER :: T_ice=-40.0





      REAL, PARAMETER ::  &
     &   TRAD_ice=0.5*T_ice      & 
     &,  ABSCOEF_W=800.          & 
     &,  ABSCOEF_I=500.          & 
     &,  SECANG=-1.66            & 

     &,  CLDCOEF_LW=1.5          & 
     &,  ABSCOEF_LW=SECANG*CLDCOEF_LW  & 
     &,  Qconv=0.1e-3            & 
     &,  CTauCW=ABSCOEF_W*Qconv  &
     &,  CTauCI=ABSCOEF_I*Qconv


CONTAINS


      SUBROUTINE GFDLETAINIT(EMISS,SFULL,SHALF,PPTOP,                   &
     &                       JULYR,MONTH,IDAY,GMT,                      &
     &                       CONFIG_FLAGS,ALLOWED_TO_READ,              &
     &                       IDS, IDE, JDS, JDE, KDS, KDE,              &
     &                       IMS, IME, JMS, JME, KMS, KME,              &
     &                       ITS, ITE, JTS, JTE, KTS, KTE              )

      IMPLICIT NONE

      TYPE (GRID_CONFIG_REC_TYPE) :: CONFIG_FLAGS
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
      INTEGER,INTENT(IN) :: JULYR,MONTH,IDAY
      REAL,INTENT(IN) :: GMT,PPTOP
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: SFULL, SHALF
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: EMISS
      LOGICAL,INTENT(IN) :: ALLOWED_TO_READ

      INTEGER :: I,IHRST,J,N
      REAL :: PCLD,XSD,PI,SQR2PI
      REAL :: SSLP=1013.25
      REAL, PARAMETER :: PTOP_HI=150.,PTOP_MID=350.,PTOP_LO=642.,       &
     &                   PLBTM=105000.








      CALL GPVS



      LTOP(1)=0
      LTOP(2)=0
      LTOP(3)=0

      DO N=1,KTE
        PCLD=(SSLP-PPTOP*10.)*SHALF(N)+PPTOP*10.
        IF(PCLD>=PTOP_LO)LTOP(1)=N
        IF(PCLD>=PTOP_MID)LTOP(2)=N
        IF(PCLD>=PTOP_HI)LTOP(3)=N

      ENDDO



      PTOPC(1)=PLBTM
      PTOPC(2)=PTOP_LO*100.
      PTOPC(3)=PTOP_MID*100.
      PTOPC(4)=PTOP_HI*100.




      IF(ALLOWED_TO_READ)THEN
        IF(CONFIG_FLAGS%CO2TF==1)THEN
          CALL CO2O3(SFULL,SHALF,PPTOP,KME-KMS,KME-KMS+1,KME-KMS+2)
        ELSE
          CALL CONRAD(KDS,KDE,KMS,KME,KTS,KTE)
        ENDIF

        CALL O3CLIM
        CALL TABLE
        IHRST=NINT(GMT)

        CALL SOLARD(IHRST,IDAY,MONTH,JULYR)
      ENDIF



      DO J=JTS,JTE
      DO I=ITS,ITE
        EMISS(I,J) = 1.0
      ENDDO
      ENDDO




      PI=ACOS(-1.)
      SQR2PI=SQRT(2.*PI)
      RSQR=1./SQR2PI
      DO I=1,NXSD
        XSD=REAL(I)*DXSD
        AXSD(I)=GAUSIN(XSD)
        if (SDprint) print *,'I, XSD, AXSD =',I,XSD,AXSD(I)
      ENDDO


      END SUBROUTINE GFDLETAINIT




      SUBROUTINE ETARA(DT,THRATEN,THRATENLW,THRATENSW,CLDFRA,PI3D       & 
     &                ,XLAND,P8W,DZ8W,RHO_PHY,P_PHY,T                   &
     &                ,QV,QW,QI,QS                                      & 
     &                ,TSK2D,GLW,RSWIN,GSW,RSWINC                       &
     &                ,RSWTOA,RLWTOA,CZMEAN                             & 
     &                ,GLAT,GLON,HTOP,HBOT,HTOPR,HBOTR,ALBEDO,CUPPT     &
     &                ,VEGFRA,SNOW,G,GMT                                &

     &                ,NSTEPRA,NPHS,ITIMESTEP                           &
     &                ,XTIME,JULIAN                                     &
     &                ,JULYR,JULDAY,GFDL_LW,GFDL_SW                     &
     &                ,CFRACL,CFRACM,CFRACH                             &
     &                ,ACFRST,NCFRST,ACFRCV,NCFRCV                      &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)

      IMPLICIT NONE

      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE,ITIMESTEP           &
     &                     ,NPHS,NSTEPRA
 
      INTEGER,INTENT(IN) :: julyr,julday   
      INTEGER,INTENT(INOUT),DIMENSION(ims:ime,jms:jme) :: NCFRST        & 
                                                         ,NCFRCV          
      REAL,INTENT(IN) :: DT,GMT,G,XTIME,JULIAN

      REAL,INTENT(INOUT),DIMENSION(ims:ime, kms:kme, jms:jme)::         &
                                    THRATEN,THRATENLW,THRATENSW,CLDFRA  
      REAL,INTENT(IN),DIMENSION(ims:ime, kms:kme, jms:jme)::p8w,dz8w,   &
     &                                                      rho_phy,    &
     &                                                      p_phy,      &
     &                                                      PI3D
      REAL, INTENT(IN), DIMENSION(ims:ime, jms:jme):: ALBEDO,SNOW,      &
     &                                                TSK2D,VEGFRA,     &
     &                                                XLAND
      REAL, INTENT(IN), DIMENSION(ims:ime, jms:jme):: GLAT,GLON
      REAL, INTENT(INOUT), DIMENSION(ims:ime, jms:jme):: HTOP,HBOT,HTOPR,HBOTR,CUPPT
      REAL, INTENT(INOUT), DIMENSION(ims:ime, jms:jme):: RSWTOA,        & 
     &                                                   RLWTOA,        & 
     &                                                   ACFRST,        & 
     &                                                   ACFRCV
      REAL,INTENT(INOUT),DIMENSION(ims:ime, jms:jme):: GLW,GSW
      REAL,INTENT(OUT),DIMENSION(ims:ime, jms:jme):: CZMEAN             &
     &                                           ,RSWIN,RSWINC        &
     &                                           ,CFRACL,CFRACM,CFRACH
      REAL, INTENT(IN), DIMENSION(ims:ime, kms:kme, jms:jme):: QS,QV,   &
     &                                                         QW,T
      LOGICAL, INTENT(IN) :: gfdl_lw,gfdl_sw
      REAL, OPTIONAL, INTENT(IN), DIMENSION(ims:ime, kms:kme, jms:jme):: QI

      REAL, DIMENSION(its:ite, kms:kme, jts:jte):: PFLIP,QIFLIP,QFLIP,  &
     &                                             QWFLIP,TFLIP
      REAL, DIMENSION(its:ite, kms:kme, jts:jte)::P8WFLIP
      REAL, DIMENSION(its:ite, kts:kte, jts:jte)::TENDS,TENDL
      REAL, DIMENSION(ims:ime, jms:jme):: CUTOP,CUBOT
      INTEGER :: IDAT(3),IHOUR,Jmonth,Jday
      INTEGER :: I,J,K,KFLIP,IHRST


      integer :: imd,jmd
      real :: FSWrat




      IF(GFDL_LW.AND.GFDL_SW )GO TO 100

      DO J=JMS,JME
        DO K=KMS,KME
          DO I=IMS,IME
            CLDFRA(I,K,J)=0.
          ENDDO
        ENDDO
      ENDDO

      DO K=KMS,KME
         KFLIP=KME+1-K
         DO J=JTS,JTE
         DO I=ITS,ITE
           P8WFLIP(I,K,J)=P8W(I,KFLIP,J)
         ENDDO
         ENDDO
      ENDDO



      DO K=KTS,KTE
        KFLIP=KTE+1-K
        DO J=JTS,JTE
        DO I=ITS,ITE
          TFLIP (I,K,J)=T(I,KFLIP,J)
          QFLIP (I,K,J)=MAX(0.,QV(I,KFLIP,J)/(1.+QV(I,KFLIP,J)))
          QWFLIP(I,K,J)=MAX(QW(I,KFLIP,J),0.)      


          QIFLIP(I,K,J)=MAX(QS(I,KFLIP,J),0.)      
          IF(PRESENT(QI))QIFLIP(I,K,J)=QIFLIP(I,K,J)+QI(I,KFLIP,J)      
          PFLIP (I,K,J)=P_PHY(I,KFLIP,J)



        ENDDO
        ENDDO
      ENDDO

      DO J=JTS,JTE
      DO I=ITS,ITE
        CUBOT(I,J)=KTE+1-HBOT(I,J)
        CUTOP(I,J)=KTE+1-HTOP(I,J)
      ENDDO
      ENDDO

      CALL CAL_MON_DAY(JULDAY,JULYR,JMONTH,JDAY)     

      IDAT(1)=JMONTH
      IDAT(2)=JDAY
      IDAT(3)=JULYR
      IHRST  =NINT(GMT)

      IHOUR  =MOD((IHRST+NINT(XTIME/60.0)),24)

      CALL SOLARD(IHOUR,JDAY,JMONTH,JULYR)

      CALL RADTN (DT,TFLIP,QFLIP,QWFLIP,QIFLIP,                         &
     &            PFLIP,P8WFLIP,XLAND,TSK2D,                            &
     &            GLAT,GLON,CUTOP,CUBOT,ALBEDO,CUPPT,                   &
     &            ACFRCV,NCFRCV,ACFRST,NCFRST,                          &
     &            VEGFRA,SNOW,GLW,GSW,RSWIN,RSWINC,                     &

     &            IDAT,IHRST,XTIME,JULIAN,                              &
     &            NSTEPRA,NSTEPRA,NPHS,ITIMESTEP,                       &
     &            TENDS,TENDL,CLDFRA,RSWTOA,RLWTOA,CZMEAN,              &
     &            CFRACL,CFRACM,CFRACH,                                 &
     &            IDS,IDE,JDS,JDE,KDS,KDE,                              &
     &            IMS,IME,JMS,JME,KMS,KME,                              &
     &            ITS,ITE,JTS,JTE,KTS,KTE                              )


















      IF(GFDL_LW)THEN
        DO J=JTS,JTE
        DO K = KTS,KTE
          KFLIP=KTE+1-K
          DO I=ITS,ITE
            THRATENLW(I,K,J)=TENDL(I,KFLIP,J)/PI3D(I,K,J)
            THRATENSW(I,K,J)=TENDS(I,KFLIP,J)/PI3D(I,K,J)
            THRATEN(I,K,J)  =THRATEN(I,K,J) + THRATENLW(I,K,J)
          ENDDO
        ENDDO
        ENDDO
      ENDIF




      IF(GFDL_SW)THEN
        DO J=JTS,JTE
        DO K=KTS,KTE
          KFLIP=KTE+1-K
          DO I=ITS,ITE
            THRATENSW(I,K,J)=TENDS(I,KFLIP,J)/PI3D(I,K,J)
          ENDDO
        ENDDO
        ENDDO
      ENDIF




      DO J=JTS,JTE
      DO I=ITS,ITE

        HBOTR(I,J)=HBOT(I,J)
        HTOPR(I,J)=HTOP(I,J)
        HBOT(I,J)=REAL(KTE+1)
        HTOP(I,J)=0.
        CUPPT(I,J)=0.
      ENDDO
      ENDDO

  100 IF(GFDL_SW)THEN
        DO J=JTS,JTE
        DO K=KTS,KTE
          KFLIP=KTE+1-K
          DO I=ITS,ITE
            THRATEN(I,K,J)=THRATEN(I,K,J)+THRATENSW(I,K,J)
          ENDDO
        ENDDO
        ENDDO
      ENDIF

  END SUBROUTINE ETARA


      SUBROUTINE RADTN(DT,T,Q,QCW,QICE,                                 &
     &                 PFLIP,P8WFLIP,XLAND,TSK2D,                       &
     &                 GLAT,GLON,CUTOP,CUBOT,ALB,CUPPT,                 &
     &                 ACFRCV,NCFRCV,ACFRST,NCFRST,                     &
     &                 VEGFRC,SNO,GLW,GSW,RSWIN,RSWINC,                 & 

     &                 IDAT,IHRST,XTIME,JULIAN,                         &
     &                 NRADS,NRADL,NPHS,NTSD,                           &
     &                 TENDS,TENDL,CLDFRA,RSWTOA,RLWTOA,CZMEAN,         &
     &                 CFRACL,CFRACM,CFRACH,                            &
     &                 ids,ide, jds,jde, kds,kde,                       &
     &                 ims,ime, jms,jme, kms,kme,                       &
     &                 its,ite, jts,jte, kts,kte                       )

      IMPLICIT NONE




























































































                                                                                                                                              













      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,         &
     &                              ims,ime, jms,jme, kms,kme ,         &
     &                              its,ite, jts,jte, kts,kte
      INTEGER, INTENT(IN)        :: NRADS,NRADL,NTSD,NPHS 

      REAL   , INTENT(IN)        :: DT,XTIME,JULIAN

      INTEGER, INTENT(IN), DIMENSION(3) :: IDAT

      INTEGER            :: LM1,LP1,LM
      INTEGER, INTENT(IN)               :: IHRST


      REAL, PARAMETER :: EPSQ1=1.E-5,EPSQ=1.E-12,EPSO3=1.E-10,H0=0.     &
     &, H1=1.,HALF=.5,T0C=273.15,CUPRATE=24.*1000.,HPINC=HALF*1.E1      &

     &, CLFRmin=0.01, TAUCmax=4.161                                     &

     &, XSDmin=-XSDmax, DXSD1=-DXSD, STSDM=0.01, CVSDM=.04              &
     &, DXSD2=HALF*DXSD, DXSD2N=-DXSD2, PCLDY=0.25

      INTEGER, PARAMETER :: NB=12,KSMUD=0
      INTEGER,PARAMETER :: K15=SELECTED_REAL_KIND(15)
      REAL (KIND=K15) :: DDX,EEX,PROD


      LOGICAL :: SHORT,LONG
      LOGICAL :: BITX,BITY,BITZ,BITW,BIT1,BIT2,BITC,BITCP1,BITSP1
      LOGICAL, SAVE :: CNCLD=.TRUE.
      LOGICAL :: NEW_CLOUD

      REAL, INTENT(IN), DIMENSION(ims:ime,jms:jme) :: XLAND,TSK2D
      REAL, INTENT(IN), DIMENSION(its:ite, kms:kme, jts:jte):: Q,QCW,   &
     &                                                         QICE,T,  &
     &                                                         PFLIP,   &
     &                                                         P8WFLIP


      REAL, INTENT(OUT), DIMENSION(ims:ime, jms:jme):: GLW,GSW,CZMEAN   &
     &                                                ,RSWIN,RSWINC     & 
     &                                                ,CFRACL,CFRACM    &
     &                                                ,CFRACH
      REAL, INTENT(OUT),DIMENSION(ims:ime,kms:kme,jms:jme) :: CLDFRA   




      REAL, INTENT(IN), DIMENSION(ims:ime,jms:jme) :: CUTOP,CUBOT,CUPPT
      REAL,   INTENT(IN   ), DIMENSION(ims:ime,jms:jme)  :: ALB,SNO

      REAL,   INTENT(IN   ), DIMENSION(ims:ime,jms:jme)  :: GLAT,GLON

      REAL,   DIMENSION(ims:ime,jms:jme)  :: CZEN
      INTEGER, DIMENSION(its:ite, jts:jte):: LMH



      REAL,   INTENT(INOUT), DIMENSION(ims:ime,jms:jme) :: ACFRCV,ACFRST &
                                                          ,RSWTOA,RLWTOA
      INTEGER,INTENT(INOUT), DIMENSION(ims:ime,jms:jme) :: NCFRCV,NCFRST

      REAL,   INTENT(IN),   DIMENSION(ims:ime,jms:jme) :: VEGFRC
      REAL,   INTENT(INOUT),DIMENSION(its:ite,kts:kte,jts:jte) :: TENDL,&
     &                                                            TENDS

      REAL :: CTHK(3)
      DATA CTHK/20000.0,20000.0,20000.0/

      REAL,DIMENSION(10),SAVE :: CC,PPT

      REAL,SAVE :: ABCFF(NB)
      INTEGER,DIMENSION(its:ite,jts:jte) :: LVL
      REAL,   DIMENSION(its:ite, jts:jte):: PDSL,FNE,FSE,TL
      REAL,   DIMENSION(  0:kte)  :: CLDAMT
      REAL,   DIMENSION(its:ite,3):: CLDCFR
      INTEGER,   DIMENSION(its:ite,3):: MBOT,MTOP
      REAL,   DIMENSION(its:ite)  :: PSFC,TSKN,ALBEDO,XLAT,COSZ,        &
     &                               SLMSK,FLWUP,                       &
     &                               FSWDN,FSWUP,FSWDNS,FSWUPS,FLWDNS,  &
     &                               FLWUPS,FSWDNSC

      REAL,   DIMENSION(its:ite,kts:kte) :: PMID,TMID
      REAL,   DIMENSION(its:ite,kts:kte) :: QMID,THMID,OZN,POZN
      REAL,   DIMENSION(its:ite,jts:jte) :: TOT 

      REAL,   DIMENSION(its:ite,kts:kte+1) :: PINT,EMIS,CAMT
      INTEGER,DIMENSION(its:ite,kts:kte+1) :: KBTM,KTOP
      INTEGER,DIMENSION(its:ite)   :: NCLDS,KCLD 
      REAL,   DIMENSION(its:ite)   :: TAUDAR
      REAL,   DIMENSION(its:ite,NB,kts:kte+1) ::RRCL,TTCL

      REAL,   DIMENSION(its:ite,kts:kte):: CSMID,CCMID,QWMID,QIMID

      REAL,SAVE :: P400=40000.
      INTEGER,SAVE :: NFILE=14


      REAL    :: CLSTP,TIME,DAYI,HOUR,ADDL,RANG
      REAL    :: TIMES,EXNER,APES,SNOFAC,CCLIMIT,CLIMIT,P1,P2,CC1,CC2
      REAL    :: PMOD,CLFR1,CTAU,WV,ARG,CLDMAX
      REAL    :: CL1,CL2,CR1,DPCL,QSUM,PRS1,PRS2,DELP,TCLD,DD,EE,AA,FF
      REAL    :: BB,GG,FCTR,PDSLIJ,CFRAVG,SNOMM
      REAL    :: THICK,CONVPRATE,CLFR,ESAT,QSAT,RHUM,QCLD
      REAL    :: RHtot,SDM
      REAL    :: TauC,CTauL,CTauS,  CFSmax,CFCmax
      INTEGER :: I,J,MYJS,MYJE,MYIS,MYIE,NTSPH,NRADPP,ITIMSW,ITIMLW,    &
     &           JD,II
      INTEGER :: L,N,LML,LVLIJ,IR,KNTLYR,LL,NC,L400,NMOD,LTROP,IWKL
      INTEGER :: LCNVB,LCNVT
      INTEGER :: NLVL,MALVL,LLTOP,LLBOT,KBT2,KTH1,KBT1,KTH2,KTOP1,KFLIP
      INTEGER :: NBAND,NCLD,LBASE,NKTP,NBTM,KS,MYJS1,MYJS2,MYJE2,MYJE1

      INTEGER :: INDEXS,IXSD
      DATA    CC/0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0/
      DATA    PPT/0.,.14,.31,.70,1.6,3.4,7.7,17.,38.,85./
      DATA ABCFF/2*4.0E-5,.002,.035,.377,1.95,9.40,44.6,190.,989.,      &
     &           2706.,39011./

      integer :: imd,jmd, Jndx
      real :: FSWrat
      imd=(ims+ime)/2
      jmd=(jms+jme)/2




      MYJS=jts
      MYJE=jte
      MYIS=its
      MYIE=ite
      MYJS1=jts 
      MYJE1=jte
      MYJS2=jts
      MYJE2=jte
      LM=kte
      LM1=LM-1
      LP1=LM+1

      DO J=JTS,JTE
      DO I=ITS,ITE
        LMH(I,J)=KME-1
        LVL(I,J)=0
      ENDDO
      ENDDO










      NTSPH=NINT(3600./DT)
      NRADPP=MIN(NRADS,NRADL)
      CLSTP=1.0*NRADPP/NTSPH
      CONVPRATE=CUPRATE/CLSTP




      SHORT=.TRUE. 
      LONG=.TRUE. 
      ITIMSW=0
      ITIMLW=0
      IF(SHORT)ITIMSW=1
      IF(LONG) ITIMLW=1






      TIME=XTIME*60.

      CALL ZENITH(TIME,DAYI,HOUR,IDAT,IHRST,GLON,GLAT,CZEN,             &
     &            MYIS,MYIE,MYJS,MYJE,                                  &
     &            ids,ide, jds,jde, kds,kde,                            &
     &            ims,ime, jms,jme, kms,kme,                            &
     &            its,ite, jts,jte, kts,kte                             ) 


      ADDL=0.
      IF(MOD(IDAT(3),4).EQ.0)ADDL=1.
      RANG=PI2*(DAYI-RLAG)/(365.+ADDL)
      RSIN1=SIN(RANG)
      RCOS1=COS(RANG)
      RCOS2=COS(2.*RANG)


      IF(SHORT)THEN
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          CZMEAN(I,J)=0.
          TOT(I,J)=0.
        ENDDO
        ENDDO

        DO II=0,NRADS,NPHS
          TIMES=XTIME*60.+II*DT
          CALL ZENITH(TIMES,DAYI,HOUR,IDAT,IHRST,GLON,GLAT,CZEN,        &
     &                MYIS,MYIE,MYJS,MYJE,                              &
     &                ids,ide, jds,jde, kds,kde,                        &
     &                ims,ime, jms,jme, kms,kme,                        &
     &                its,ite, jts,jte, kts,kte                         ) 


          DO J=MYJS,MYJE
          DO I=MYIS,MYIE
            IF(CZEN(I,J).GT.0.)THEN
              CZMEAN(I,J)=CZMEAN(I,J)+CZEN(I,J)
              TOT(I,J)=TOT(I,J)+1.
            ENDIF
          ENDDO
          ENDDO
        ENDDO
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          IF(TOT(I,J).GT.0.)CZMEAN(I,J)=CZMEAN(I,J)/TOT(I,J)
        ENDDO
        ENDDO
      ENDIF





      DO L=2,LM
      DO I=MYIS,MYIE
        POZN(I,L)=H1
      ENDDO
      ENDDO






                         DO 700 J = MYJS, MYJE


      DO 125 L=1,LM
      DO I=MYIS,MYIE
        TMID(I,L)=T(I,1,J)
        QMID(I,L)=EPSQ
        QWMID(I,L)=0.
        QIMID(I,L)=0.
        CSMID(I,L)=0.
        CCMID(I,L)=0.
        OZN(I,L)=EPSO3
        TENDS(I,L,J)=0.
        TENDL(I,L,J)=0.
      ENDDO
  125 CONTINUE

      DO 140 N=1,3
      DO I=MYIS,MYIE
        CLDCFR(I,N)=0.
        MTOP(I,N)=0
        MBOT(I,N)=0
      ENDDO
  140 CONTINUE




      DO 200 I=MYIS,MYIE

      LML=LMH(I,J)
      LVLIJ=LVL(I,J)

      DO L=1,LML
        PMID(I,L+LVLIJ)=PFLIP(I,L,J)
        PINT(I,L+LVLIJ+1)=P8WFLIP(I,L+1,J)
        EXNER=(1.E5/PMID(I,L+LVLIJ))**RCP
        TMID(I,L+LVLIJ)=T(I,L,J)
        THMID(I,L+LVLIJ)=T(I,L,J)*EXNER
        QMID(I,L+LVLIJ)=MAX(EPSQ, Q(I,L,J))


        QWMID(I,L+LVLIJ)=QCW(I,L,J)
        QIMID(I,L+LVLIJ)=QICE(I,L,J)
      ENDDO





      IF(LVLIJ.GT.0)THEN
        KNTLYR=0

        DO L=LVLIJ,1,-1
          KNTLYR=KNTLYR+1
          PMID(I,L)=P8WFLIP(I,1,J)-REAL(2*KNTLYR-1)*HPINC
          PINT(I,L+1)=PMID(I,L)+HPINC
          EXNER=(1.E5/PMID(I,L))**RCP
          THMID(I,L)=TMID(I,L)*EXNER
        ENDDO
      ENDIF

      IF(LVLIJ.EQ.0) THEN
         PINT(I,1)=P8WFLIP(I,1,J)
      ELSE
         PINT(I,1)=PMID(I,1)-HPINC
      ENDIF
  200 CONTINUE





      DO 250 I=MYIS,MYIE
      PSFC(I)=P8WFLIP(I,KME,J)
      APES=(PSFC(I)*1.E-5)**RCP

      IF((XLAND(I,J)-1.5).GT.0.)THEN
        TSKN(I)=-TSK2D(I,J)
      ELSE
        TSKN(I)=TSK2D(I,J)
      ENDIF



      SLMSK(I)=XLAND(I,J)-1.



      SNOMM=AMAX1(SNO(I,J),0.)
      SNOFAC=AMIN1(SNOMM/0.02, 1.0)

      ALBEDO(I)=ALB(I,J)

      XLAT(I)=GLAT(I,J)*RTD
      COSZ(I)=CZMEAN(I,J)
  250 CONTINUE









      DO I=MYIS,MYIE
        LML=LMH(I,J)
        LVLIJ=LVL(I,J)
        DO 255 L=1,LML
            LL=L+LVLIJ
            WV=QMID(I,LL)/(1.-QMID(I,LL))       
            QCLD=QWMID(I,LL)+QIMID(I,LL)        
            IF (QCLD .LE. EPSQ) GO TO 255       
            CLFR=H0
            WV=QMID(I,LL)/(1.-QMID(I,LL))       
               
    
    
    

            ESAT=1000.*FPVS(TMID(I,LL))         



            QSAT=EP_2*ESAT/(PMID(I,LL)-ESAT)    
            RHUM=WV/QSAT                        
    
    
    
            RHtot=(WV+QCLD)/QSAT                
            LCNVT=NINT(CUTOP(I,J))+LVLIJ
            LCNVT=MIN(LM,LCNVT)
            LCNVB=NINT(CUBOT(I,J))+LVLIJ
            LCNVB=MIN(LM,LCNVB)
            IF (LL.GE.LCNVT .AND. LL.LE.LCNVB) THEN
               SDM=CVSDM
            ELSE
               SDM=STSDM
            ENDIF
            ARG=(RHtot-RHgrd)/SDM
            IF (ARG.LE.DXSD2 .AND. ARG.GE.DXSD2N) THEN
               CLFR=HALF
            ELSE IF (ARG .GT. DXSD2) THEN
               IF (ARG .GE. XSDmax) THEN
                  CLFR=H1
               ELSE
                  IXSD=INT(ARG/DXSD+HALF)
                  IXSD=MIN(NXSD, MAX(IXSD,1))
                  CLFR=HALF+AXSD(IXSD)
                  if (SDprint)                                          &
     & write(6,"(a,3i3,i4,f8.4,f7.4,2f6.3,f7.3,f6.1,f6.0)")                 &
     & 'I,LL,J,IXSD,ARG,SDM,CLFR,RHtot,QSAT,T,P=', I,LL,J,IXSD,ARG,SDM,CLFR,RHtot     &
     & ,1000.*QSAT,TCLD,.01*PMID(I,LL)
               ENDIF              
            ELSE
               IF (ARG .LE. XSDmin) THEN
                  CLFR=H0
               ELSE
                  IXSD=INT(ARG/DXSD1+HALF)
                  IXSD=MIN(NXSD, MAX(IXSD,1))
                  CLFR=HALF-AXSD(IXSD)
                  if (SDprint)                                          &
     & write(6,"(a,3i3,i4,f8.4,f7.4,2f6.3,f7.3,f6.1,f6.0)")                 &
     & 'I,LL,J,IXSD,ARG,SDM,CLFR,RHtot,QSAT,T,P=', I,LL,J,IXSD,ARG,SDM,CLFR,RHtot     &
     & ,1000.*QSAT,TCLD,.01*PMID(I,LL)
                  IF (CLFR .LT. CLFRmin) CLFR=H0
               ENDIF        
            ENDIF           
            CSMID(I,LL)=CLFR
255       CONTINUE         
      ENDDO                













      IF (CNCLD) THEN
        DO I=MYIS,MYIE




          IF (CUBOT(I,J)-CUTOP(I,J) .GT. 1.0) THEN
 
            CLFR=CC(1)
            PMOD=CUPPT(I,J)*CONVPRATE
            IF (PMOD .GT. PPT(1)) THEN
              DO NC=1,10
                IF(PMOD.GT.PPT(NC)) NMOD=NC
              ENDDO
              IF (NMOD .GE. 10) THEN
                CLFR=CC(10)
              ELSE
                CC1=CC(NMOD)
                CC2=CC(NMOD+1)
                P1=PPT(NMOD)
                P2=PPT(NMOD+1)
                CLFR=CC1+(CC2-CC1)*(PMOD-P1)/(P2-P1)
              ENDIF      
              CLFR=MIN(H1, CLFR)
            ENDIF        
  
  
  
            LVLIJ=LVL(I,J)
            LCNVT=NINT(CUTOP(I,J))+LVLIJ
            LCNVT=MIN(LM,LCNVT)
            LCNVB=NINT(CUBOT(I,J))+LVLIJ
            LCNVB=MIN(LM,LCNVB)








   
   
   
   
   
            DO LL=LCNVT,LCNVB
              ARG=MAX(H0, H1-CSMID(I,LL))
              CCMID(I,LL)=MIN(ARG,CLFR)
            ENDDO           
          ENDIF             
        ENDDO               
      ENDIF                 











       DO 500 I=MYIS,MYIE

       DO L=0,LM
         CLDAMT(L)=0.
       ENDDO



       DO 480 NLVL=1,3
       CLDMAX=0.
       MALVL=LM
       LLTOP=LM+1-LTOP(NLVL)+LVL(I,J)




       IF(LLTOP.GE.LM)GO TO 480

       IF(NLVL.GT.1)THEN
         LLBOT=LM+1-LTOP(NLVL-1)-1+LVL(I,J)
         LLBOT=MIN(LLBOT,LM1)
       ELSE
         LLBOT=LM1
       ENDIF

       DO 435 L=LLTOP,LLBOT
       CLDAMT(L)=AMAX1(CSMID(I,L),CCMID(I,L))
       IF(CLDAMT(L).GT.CLDMAX)THEN
         MALVL=L
         CLDMAX=CLDAMT(L)
       ENDIF
   435 CONTINUE








       CL1=0.0
       CL2=0.0
       KBT1=LLBOT
       KBT2=LLBOT
       KTH1=0
       KTH2=0

       DO 450 LL=LLTOP,LLBOT
       L=LLBOT-LL+LLTOP
       BIT1=.FALSE.
       CR1=CLDAMT(L)
       BITX=(PINT(I,L).GE.PTOPC(NLVL+1)).AND.                           &
      &     (PINT(I,L).LT.PTOPC(NLVL)).AND.                             &
      &     (CLDAMT(L).GT.0.0)
       BIT1=BIT1.OR.BITX
       IF(.NOT.BIT1)GO TO 450








       BITY=BITX.AND.(KTH2.LE.0)
       BITZ=BITX.AND.(KTH2.GT.0)

       IF(BITY)THEN
         KBT2=L
         KTH2=1
       ENDIF

       IF(BITZ)THEN
         KTOP1=KBT2-KTH2+1
         DPCL=PMID(I,KBT2)-PMID(I,KTOP1)
         IF(DPCL.LT.CTHK(NLVL))THEN
           KTH2=KTH2+1
         ELSE
           KBT2=KBT2-1
         ENDIF
       ENDIF
       IF(BITX)CL2=AMAX1(CL2,CR1)





       BIT2=.FALSE.
       BITY=BITX.AND.(CLDAMT(L-1).LE.0.0.OR. &
            PINT(I,L-1).LT.PTOPC(NLVL+1))
       BITZ=BITY.AND.CL1.GT.0.0
       BITW=BITY.AND.CL1.LE.0.0
       BIT2=BIT2.OR.BITY
       IF(.NOT.BIT2)GO TO 450

       IF(BITZ)THEN
         KBT1=INT((CL1*KBT1+CL2*KBT2)/(CL1+CL2))
         KTH1=INT((CL1*KTH1+CL2*KTH2)/(CL1+CL2))+1
         CL1=CL1+CL2-CL1*CL2
       ENDIF

       IF(BITW)THEN
         KBT1=KBT2
         KTH1=KTH2
         CL1=CL2
       ENDIF

       IF(BITY)THEN
         KBT2=LLBOT
         KTH2=0
         CL2=0.0
       ENDIF
   450 CONTINUE

       CLDCFR(I,NLVL)=AMIN1(1.0,CL1)
       MTOP(I,NLVL)=MIN(KBT1,KBT1-KTH1+1)
       MBOT(I,NLVL)=KBT1
   480 CONTINUE
   500 CONTINUE




      DO I=MYIS,MYIE
        TAUDAR(I)=1.0
      ENDDO

















      DO 600 I=MYIS,MYIE
      LML=LMH(I,J)
      LVLIJ=LVL(I,J)





      EMIS(I,1)=1.0
      KTOP(I,1)=LP1
      KBTM(I,1)=LP1
      CAMT(I,1)=1.0
      KCLD(I)=2

      DO NBAND=1,NB
        RRCL(I,NBAND,1)=0.0
        TTCL(I,NBAND,1)=1.0
      ENDDO

      DO 510 L=2,LP1
      CAMT(I,L)=0.0
      KTOP(I,L)=1
      KBTM(I,L)=1
      EMIS(I,L)=0.0

      DO NBAND=1,NB
        RRCL(I,NBAND,L)=0.0
        TTCL(I,NBAND,L)=1.0
      ENDDO
  510 CONTINUE













      NEW_CLOUD=.TRUE.

      DO L=2,LML
        LL=LML-L+1+LVLIJ                        
        CLFR=MAX(CCMID(I,LL),CSMID(I,LL))       
        CLFR1=MAX(CCMID(I,LL+1),CSMID(I,LL+1))  

        IF (CLFR .GE. CLFRMIN) THEN

          IF (NEW_CLOUD) THEN

            IF(L==2.AND.CLFR1>=CLFRmin)THEN
              KBTM(I,KCLD(I))=LL+1
              CAMT(I,KCLD(I))=CLFR1
            ELSE
              KBTM(I,KCLD(I))=LL
              CAMT(I,KCLD(I))=CLFR
            ENDIF
            NEW_CLOUD=.FALSE.
          ELSE

            CAMT(I,KCLD(I))=AMAX1(CAMT(I,KCLD(I)), CLFR)
          ENDIF        
        ELSE IF (CLFR1 .GE. CLFRMIN) THEN

          IF (L .EQ. 2) THEN

            KBTM(I,KCLD(I))=LL+1
            CAMT(I,KCLD(I))=CLFR1
          ENDIF
          KTOP(I,KCLD(I))=LL+1
          NEW_CLOUD=.TRUE.
          KCLD(I)=KCLD(I)+1
          CAMT(I,KCLD(I))=0.0
        ENDIF

      ENDDO      




      NCLDS(I)=KCLD(I)-2
      NCLD=NCLDS(I)



      IF(NCLD.GE.1)THEN



        DO 580 NC=2,NCLD+1

        TauC=0.    
        QSUM=0.0
        NKTP=LP1
        NBTM=0
        BITX=CAMT(I,NC).GE.CLFRMIN
        NKTP=MIN(NKTP,KTOP(I,NC))
        NBTM=MAX(NBTM,KBTM(I,NC))

        DO LL=NKTP,NBTM
          IF(LL.GE.KTOP(I,NC).AND.LL.LE.KBTM(I,NC).AND.BITX)THEN
            PRS1=PINT(I,LL)*0.01
            PRS2=PINT(I,LL+1)*0.01
            DELP=PRS2-PRS1
            TCLD=TMID(I,LL)-T0C
            QSUM=QSUM+QMID(I,LL)*DELP*(PRS1+PRS2)                       &     
     &           /(120.1612*SQRT(TMID(I,LL)))







































            CTau=0.

            IF (CCMID(I,LL) .GE. CLFRmin) THEN
              IF (TCLD .GE. TRAD_ice) THEN
                CTau=CTauCW            
              ELSE
                CTau=CTauCI            
              ENDIF

            ENDIF







            CTau=CTau+ABSCOEF_W*QWMID(I,LL)+ABSCOEF_I*QIMID(I,LL)
            TauC=TauC+DELP*CTau
          ENDIF      
        ENDDO        

        IF(BITX)EMIS(I,NC)=1.0-EXP(ABSCOEF_LW*TauC)
        IF(QSUM.GE.EPSQ1)THEN

          DO 570 NBAND=1,NB
          IF(BITX)THEN
            PROD=ABCFF(NBAND)*QSUM
            DDX=TauC/(TauC+PROD)
            EEX=1.0-DDX
            IF(ABS(EEX).GE.1.E-8)THEN
              DD=DDX
              EE=EEX
              FF=1.0-DD*0.85
              AA=MIN(50.0,SQRT(3.0*EE*FF)*TauC)
              AA=EXP(-AA)
              BB=FF/EE
              GG=SQRT(BB)
              DD=(GG+1.0)*(GG+1.0)-(GG-1.0)*(GG-1.0)*AA*AA
              RRCL(I,NBAND,NC)=MAX(0.1E-5,(BB-1.0)*(1.0-AA*AA)/DD)
              TTCL(I,NBAND,NC)=AMAX1(0.1E-5,4.0*GG*AA/DD)
            ENDIF
          ENDIF
  570     CONTINUE
        ENDIF
  580   CONTINUE

      ENDIF

  600 CONTINUE







      DO I=MYIS,MYIE
        FCTR=PINT(I,2)/(PINT(I,2)-PINT(I,1))
        POZN(I,1)=FCTR*(PMID(I,1)-PINT(I,1))
      ENDDO

      CALL OZON2D(LM,POZN,XLAT,OZN,                                &
                  MYIS,MYIE,                                       &
                  ids,ide, jds,jde, kds,kde,                       &
                  ims,ime, jms,jme, kms,kme,                       &
                  its,ite, jts,jte, kts,kte                        )









      Jndx=J
      CALL RADFS &
     &     (PSFC,PMID,PINT,QMID,TMID,OZN,TSKN,SLMSK,ALBEDO,XLAT         &

     &,     CAMT,KTOP,KBTM,NCLDS,EMIS,RRCL,TTCL                         &
     &,     COSZ,TAUDAR,1                                               &
     &,     1,0                                                         &
     &,     ITIMSW,ITIMLW                                               &
     &,     TENDS(ITS,KTS,J),TENDL(ITS,KTS,J)                           &
     &,     FLWUP,FSWUP,FSWDN,FSWDNS,FSWUPS,FLWDNS,FLWUPS,FSWDNSC       &
     &,     ids,ide, jds,jde, kds,kde                                   &
     &,     ims,ime, jms,jme, kms,kme                                   &

     &,     its,ite, jts,jte, kts,kte                                   &
     &,     imd,jmd, Jndx                                       )


      IF(LONG)THEN






        DO I=MYIS,MYIE
          GLW(I,J)=FLWDNS(I)
          RLWTOA(I,J)=FLWUP(I)
        ENDDO
      ENDIF

      IF(SHORT)THEN







        DO I=MYIS,MYIE
          GSW(I,J)=FSWDNS(I)-FSWUPS(I)
          RSWIN(I,J) =FSWDNS(I)
          RSWINC(I,J)=FSWDNSC(I)
          RSWTOA(I,J)=FSWUP(I)
        ENDDO
      ENDIF









      DO I=MYIS,MYIE
        CFRACL(I,J)=CLDCFR(I,1)
        CFRACM(I,J)=CLDCFR(I,2)
        CFRACH(I,J)=CLDCFR(I,3)
        IF(CNCLD)THEN
          CFSmax=0.   
          CFCmax=0.   
          DO L=1,LMH(I,J)
            LL=L+LVL(I,J)
            CFSmax=MAX(CFSmax, CSMID(I,LL) )
            CFCmax=MAX(CFCmax, CCMID(I,LL) )
          ENDDO
          ACFRST(I,J)=ACFRST(I,J)+CFSmax
          NCFRST(I,J)=NCFRST(I,J)+1
          ACFRCV(I,J)=ACFRCV(I,J)+CFCmax
          NCFRCV(I,J)=NCFRCV(I,J)+1
        ELSE
  
  
          CFRAVG=1.-(1.-CFRACL(I,J))*(1.-CFRACM(I,J))*(1.-CFRACH(I,J))
          ACFRST(I,J)=ACFRST(I,J)+CFRAVG
          NCFRST(I,J)=NCFRST(I,J)+1
        ENDIF

        LML=LMH(I,J)
        DO L=1,LML
          LL=LML-L+1+LVL(I,J)
          CLDFRA(I,L,J)=MAX(CCMID(I,LL),CSMID(I,LL))
        ENDDO
      ENDDO      




  700                          CONTINUE





















      END SUBROUTINE RADTN



      REAL FUNCTION GAUSIN(xsd)
      REAL, PARAMETER :: crit=1.e-3
      REAL A1,A2,RN,B1,B2,B3,SUM




      a1=xsd*RSQR
      a2=exp(-0.5*xsd**2)
      rn=1.
      b1=1.
      b2=1.
      b3=1.
      sum=1.
      do while (b2 .gt. crit)
         rn=rn+1.
         b2=xsd**2/(2.*rn-1.)
         b3=b1*b2
         sum=sum+b3
         b1=b3
      enddo
      GAUSIN=a1*a2*sum
      RETURN
      END FUNCTION GAUSIN



      SUBROUTINE ZENITH(TIMES,DAYI,HOUR,IDAT,IHRST,GLON,GLAT,CZEN,     &
                        MYIS,MYIE,MYJS,MYJE,                           &
                        IDS,IDE, JDS,JDE, KDS,KDE,                     &
                        IMS,IME, JMS,JME, KMS,KME,                     &
                        ITS,ITE, JTS,JTE, KTS,KTE                      )

      IMPLICIT NONE

      INTEGER, INTENT(IN)        :: IDS,IDE, JDS,JDE, KDS,KDE ,        &
                                    IMS,IME, JMS,JME, KMS,KME ,        &
                                    ITS,ITE, JTS,JTE, KTS,KTE
      INTEGER, INTENT(IN)        :: MYJS,MYJE,MYIS,MYIE

      REAL,    INTENT(IN)        :: TIMES
      REAL,    INTENT(OUT)       :: HOUR,DAYI
      INTEGER, INTENT(IN)        :: IHRST

      INTEGER, INTENT(IN), DIMENSION(3) :: IDAT 
      REAL,    INTENT(IN), DIMENSION(IMS:IME,JMS:JME) :: GLAT,GLON
      REAL,    INTENT(OUT), DIMENSION(IMS:IME,JMS:JME) :: CZEN

      REAL,    PARAMETER :: GSTC1=24110.54841,GSTC2=8640184.812866,    &
                            GSTC3=9.3104E-2,GSTC4=-6.2E-6,             &
                            PI=3.1415926,PI2=2.*PI,PIH=0.5*PI,         &

                            DEG2RD=3.1415926/180.,OBLIQ=23.440*DEG2RD, &
                            ZEROJD=2451545.0

      REAL    :: DAY,YFCTR,ADDDAY,STARTYR,DATJUL,DIFJD,SLONM,   &
                 ANOM,SLON,DEC,RA,DATJ0,TU,STIM0,SIDTIM,HRANG
      REAL    :: HRLCL,SINALT
      INTEGER :: KMNTH,KNT,IDIFYR,J,I
      LOGICAL :: LEAP


      INTEGER :: MONTH (12)

      DATA MONTH/31,28,31,30,31,30,31,31,30,31,30,31/


      DAY=0.
      LEAP=.FALSE.
      IF(MOD(IDAT(3),4).EQ.0)THEN
        MONTH(2)=29
        LEAP=.TRUE.
      ENDIF
      IF(IDAT(1).GT.1)THEN
        KMNTH=IDAT(1)-1
        DO 10 KNT=1,KMNTH
        DAY=DAY+REAL(MONTH(KNT))
   10   CONTINUE
      ENDIF




      DAY=DAY+REAL(IDAT(2)-1)+(REAL(IHRST)+TIMES/3600.)/24.
      DAYI=REAL(INT(DAY)+1)
      HOUR=(DAY-DAYI+1.)*24.
      YFCTR=2000.-IDAT(3)






      IDIFYR=IDAT(3)-2000




      IF(IDIFYR.LT.0)THEN
        ADDDAY=REAL(IDIFYR/4)
      ELSE
        ADDDAY=REAL((IDIFYR+3)/4)
      ENDIF
      STARTYR=ZEROJD+IDIFYR*365.+ADDDAY-0.5



      DATJUL=STARTYR+DAY




      DIFJD=DATJUL-ZEROJD



      SLONM=(280.460+0.9856474*DIFJD)*DEG2RD+YFCTR*PI2



      ANOM=(357.528+0.9856003*DIFJD)*DEG2RD



      SLON=SLONM+(1.915*SIN(ANOM)+0.020*SIN(2.*ANOM))*DEG2RD
      IF(SLON.GT.PI2)SLON=SLON-PI2



      DEC=ASIN(SIN(SLON)*SIN(OBLIQ))
      RA=ACOS(COS(SLON)/COS(DEC))
      IF(SLON.GT.PI)RA=PI2-RA




      DATJ0=STARTYR+DAYI-1.
      TU=(DATJ0-2451545.)/36525.
      STIM0=GSTC1+TU*(GSTC2+GSTC3*TU+GSTC4*TU*TU)
      SIDTIM=STIM0/3600.+YFCTR*24.+1.00273791*HOUR
      SIDTIM=SIDTIM*15.*DEG2RD
      IF(SIDTIM.LT.0.)SIDTIM=SIDTIM+PI2
      IF(SIDTIM.GT.PI2)SIDTIM=SIDTIM-PI2
      HRANG=SIDTIM-RA

      DO 100 J=MYJS,MYJE
      DO 100 I=MYIS,MYIE

      HRLCL=HRANG+GLON(I,J)+PI2




      SINALT=SIN(DEC)*SIN(GLAT(I,J))+COS(DEC)*COS(HRLCL)* &
       COS(GLAT(I,J))
      IF(SINALT.LT.0.)SINALT=0.
      CZEN(I,J)=SINALT
  100 CONTINUE





      IF(DAYI.GT.365.)THEN
        IF(.NOT.LEAP)THEN
          DAYI=DAYI-365.
        ELSEIF(LEAP.AND.DAYI.GT.366.)THEN
          DAYI=DAYI-366.
        ENDIF
      ENDIF

      END SUBROUTINE ZENITH


  SUBROUTINE OZON2D (LK,POZN,XLAT,QO3,                                &
                     MYIS,MYIE,                                       &
                     ids,ide, jds,jde, kds,kde,                       &
                     ims,ime, jms,jme, kms,kme,                       &
                     its,ite, jts,jte, kts,kte                        )

 IMPLICIT NONE

      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte  
      INTEGER, INTENT(IN)        :: LK,MYIS,MYIE
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte) :: POZN
      REAL,    INTENT(IN), DIMENSION(its:ite)  :: XLAT
      REAL,    INTENT(INOUT), DIMENSION(its:ite,kts:kte) :: QO3

      INTEGER, PARAMETER ::  NL=81,NLP1=NL+1,LNGTH=37*NL





      INTEGER,DIMENSION(its:ite)    :: JJROW
      REAL,   DIMENSION(its:ite)    :: TTHAN
      REAL,   DIMENSION(its:ite,NL) :: QO3O3

      INTEGER :: I,K,NUMITR,ILOG,IT,NHALF
      REAL    :: TH2,DO3V,DO3VP,APHI,APLO

      DO I=MYIS,MYIE
        TH2=0.2*XLAT(I)
        JJROW(I)=19.001-TH2
        TTHAN(I)=(19-JJROW(I))-TH2
      ENDDO



      DO K=1,NL
      DO I=MYIS,MYIE
        DO3V=XDUO3N(JJROW(I),K)+RSIN1*XDO3N2(JJROW(I),K)  &
                   +RCOS1*XDO3N3(JJROW(I),K)  &
                   +RCOS2*XDO3N4(JJROW(I),K)
        DO3VP=XDUO3N(JJROW(I)+1,K)+RSIN1*XDO3N2(JJROW(I)+1,K) &
                    +RCOS1*XDO3N3(JJROW(I)+1,K) &
                    +RCOS2*XDO3N4(JJROW(I)+1,K)




        QO3O3(I,K)=1.E-4*(DO3V+TTHAN(I)*(DO3VP-DO3V))
      ENDDO
      ENDDO



      NUMITR=0
      ILOG=NL
   20 CONTINUE
      ILOG=(ILOG+1)/2
        IF(ILOG.EQ.1)GO TO 25
        NUMITR=NUMITR+1
        GO TO 20
   25 CONTINUE

      DO 60 K=1,LK

      NHALF=(NL+1)/2
      DO I=MYIS,MYIE
        JJROW(I)=NHALF
      ENDDO

      DO 40 IT=1,NUMITR
      NHALF=(NHALF+1)/2
      DO I=MYIS,MYIE
        IF(POZN(I,K).LT.PRGFDL(JJROW(I)-1))THEN
          JJROW(I)=JJROW(I)-NHALF
        ELSEIF(POZN(I,K).GE.PRGFDL(JJROW(I)))THEN
          JJROW(I)=JJROW(I)+NHALF
        ENDIF
        JJROW(I)=MIN(JJROW(I),NL)
        JJROW(I)=MAX(JJROW(I),2)
      ENDDO
   40 CONTINUE

      DO 50 I=MYIS,MYIE
      IF(POZN(I,K).LT.PRGFDL(1))THEN
        QO3(I,K)=QO3O3(I,1)
      ELSE IF(POZN(I,K).GT.PRGFDL(NL))THEN
        QO3(I,K)=QO3O3(I,NL)
      ELSE
        APLO=ALOG(PRGFDL(JJROW(I)-1))
        APHI=ALOG(PRGFDL(JJROW(I)))
        QO3(I,K)=QO3O3(I,JJROW(I))+(ALOG(POZN(I,K))-APHI)/ &
                   (APLO-APHI)* &
                   (QO3O3(I,JJROW(I)-1)-QO3O3(I,JJROW(I)))
      ENDIF
   50 CONTINUE

   60 CONTINUE

  END SUBROUTINE OZON2D

























      SUBROUTINE O3INT(PHALF,DDUO3N,DDO3N2,DDO3N3,DDO3N4, &
                 ids,ide, jds,jde, kds,kde,            &
                 ims,ime, jms,jme, kms,kme,            &
                 its,ite, jts,jte, kts,kte             )

 IMPLICIT NONE

      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte









































      INTEGER :: N,NP,NP2,NM1









      REAL, INTENT(OUT), DIMENSION(37,kte):: DDUO3N,DDO3N2,DDO3N3,DDO3N4








      REAL ::   QI(82)
      REAL ::   DDUO3(19,kts:kte),RO31(10,41),RO32(10,41),DUO3N(19,41)
      REAL ::   TEMPN(19)
      REAL ::   O3HI(10,25),O3LO1(10,16),O3LO2(10,16),O3LO3(10,16), &
                O3LO4(10,16)
      REAL ::   O3HI1(10,16),O3HI2(10,9),PH1(45),PH2(37),P1(48),P2(33)
      REAL ::   O35DEG(37,kts:kte)
      REAL ::   RSTD(81),RO3(10,41),RO3M(10,40),RBAR(kts:kte),RDATA(81), &
                PHALF(kts:kte+1),P(81),PH(82)

      INTEGER :: NKK,NK,NKP,K,L,NCASE,ITAPE,IPLACE,NKMM,NKM,KI,KK,KQ,JJ,KEN
      REAL :: O3RD,O3TOT,O3DU

      EQUIVALENCE (O3HI1(1,1),O3HI(1,1)),(O3HI2(1,1),O3HI(1,17))
      EQUIVALENCE (PH1(1),PH(1)),(PH2(1),PH(46))
      EQUIVALENCE (P1(1),P(1)),(P2(1),P(49))
      DATA PH1/      0., &
           0.1027246E-04, 0.1239831E-04, 0.1491845E-04, 0.1788053E-04, &
           0.2135032E-04, 0.2540162E-04, 0.3011718E-04, 0.3558949E-04, &
           0.4192172E-04, 0.4922875E-04, 0.5763817E-04, 0.6729146E-04, &
           0.7834518E-04, 0.9097232E-04, 0.1053635E-03, 0.1217288E-03, &
           0.1402989E-03, 0.1613270E-03, 0.1850904E-03, 0.2119495E-03, &
           0.2423836E-03, 0.2768980E-03, 0.3160017E-03, 0.3602623E-03, &
           0.4103126E-03, 0.4668569E-03, 0.5306792E-03, 0.6026516E-03, &
           0.6839018E-03, 0.7759249E-03, 0.8803303E-03, 0.9987843E-03, &
           0.1133178E-02, 0.1285955E-02, 0.1460360E-02, 0.1660001E-02, &
           0.1888764E-02, 0.2151165E-02, 0.2452466E-02, 0.2798806E-02, &
           0.3197345E-02, 0.3656456E-02, 0.4185934E-02, 0.4797257E-02/
      DATA PH2/ &
           0.5503893E-02, 0.6321654E-02, 0.7269144E-02, 0.8368272E-02, &
           0.9644873E-02, 0.1112946E-01, 0.1285810E-01, 0.1487354E-01, &
           0.1722643E-01, 0.1997696E-01, 0.2319670E-01, 0.2697093E-01, &
           0.3140135E-01, 0.3660952E-01, 0.4274090E-01, 0.4996992E-01, &
           0.5848471E-01, 0.6847525E-01, 0.8017242E-01, 0.9386772E-01, &
           0.1099026E+00, 0.1286765E+00, 0.1506574E+00, 0.1763932E+00, &
           0.2065253E+00, 0.2415209E+00, 0.2814823E+00, 0.3266369E+00, &
           0.3774861E+00, 0.4345638E+00, 0.4984375E+00, 0.5697097E+00, &
           0.6490189E+00, 0.7370409E+00, 0.8344896E+00, 0.9421190E+00, &
           0.1000000E+01/
      DATA P1/ &
           0.9300000E-05, 0.1129521E-04, 0.1360915E-04, 0.1635370E-04, &
           0.1954990E-04, 0.2331653E-04, 0.2767314E-04, 0.3277707E-04, &
           0.3864321E-04, 0.4547839E-04, 0.5328839E-04, 0.6234301E-04, &
           0.7263268E-04, 0.8450696E-04, 0.9793231E-04, 0.1133587E-03, &
           0.1307170E-03, 0.1505832E-03, 0.1728373E-03, 0.1982122E-03, &
           0.2266389E-03, 0.2592220E-03, 0.2957792E-03, 0.3376068E-03, &
           0.3844381E-03, 0.4379281E-03, 0.4976965E-03, 0.5658476E-03, &
           0.6418494E-03, 0.7287094E-03, 0.8261995E-03, 0.9380076E-03, &
           0.1063498E-02, 0.1207423E-02, 0.1369594E-02, 0.1557141E-02, &
           0.1769657E-02, 0.2015887E-02, 0.2295520E-02, 0.2620143E-02, &
           0.2989651E-02, 0.3419469E-02, 0.3909867E-02, 0.4481491E-02, &
           0.5135272E-02, 0.5898971E-02, 0.6774619E-02, 0.7799763E-02/
      DATA P2/ &
           0.8978218E-02, 0.1036103E-01, 0.1195488E-01, 0.1382957E-01, &
           0.1599631E-01, 0.1855114E-01, 0.2151235E-01, 0.2501293E-01, &
           0.2908220E-01, 0.3390544E-01, 0.3952926E-01, 0.4621349E-01, &
           0.5403168E-01, 0.6330472E-01, 0.7406807E-01, 0.8677983E-01, &
           0.1015345E+00, 0.1189603E+00, 0.1391863E+00, 0.1630739E+00, &
           0.1908004E+00, 0.2235461E+00, 0.2609410E+00, 0.3036404E+00, &
           0.3513750E+00, 0.4055375E+00, 0.4656677E+00, 0.5335132E+00, &
           0.6083618E+00, 0.6923932E+00, 0.7845676E+00, 0.8875882E+00, &
           0.1000000E+01/
      DATA O3HI1/ &
       .55,.50,.45,.45,.40,.35,.35,.30,.30,.30, &
       .55,.51,.46,.47,.42,.38,.37,.36,.35,.35, &
       .55,.53,.48,.49,.44,.42,.41,.40,.38,.38, &
       .60,.55,.52,.52,.50,.47,.46,.44,.42,.41, &
       .65,.60,.55,.56,.53,.52,.50,.48,.45,.45, &
       .75,.65,.60,.60,.55,.55,.55,.50,.48,.47, &
       .80,.75,.75,.75,.70,.70,.65,.63,.60,.60, &
       .90,.85,.85,.80,.80,.75,.75,.74,.72,.71, &
       1.10,1.05,1.00,.90,.90,.90,.85,.83,.80,.80, &
       1.40,1.30,1.25,1.25,1.25,1.20,1.15,1.10,1.05,1.00, &
       1.7,1.7,1.6,1.6,1.6,1.6,1.6,1.6,1.5,1.5, &
       2.1,2.0,1.9,1.9,1.9,1.8,1.8,1.8,1.7,1.7, &
       2.4,2.3,2.2,2.2,2.2,2.1,2.1,2.1,2.0,2.0, &
       2.7,2.5,2.5,2.5,2.5,2.5,2.4,2.4,2.3,2.3, &
       2.9,2.8,2.7,2.7,2.7,2.7,2.7,2.7,2.6,2.6, &
       3.1,3.1,3.0,3.0,3.0,3.0,3.0,3.0,2.9,2.8/
      DATA O3HI2/ &
       3.3,3.4,3.4,3.6,3.7,3.9,4.0,4.1,4.0,3.8, &
       3.6,3.8,3.9,4.2,4.7,5.3,5.6,5.7,5.5,5.2, &
       4.1,4.3,4.7,5.2,6.0,6.7,7.0,6.8,6.4,6.2, &
       5.4,5.7,6.0,6.6,7.3,8.0,8.4,7.7,7.1,6.7, &
       6.7,6.8,7.0,7.6,8.3,10.0,9.6,8.2,7.5,7.2, &
       9.2,9.3,9.4,9.6,10.3,10.6,10.0,8.5,7.7,7.3, &
       12.6,12.1,12.0,12.1,11.7,11.0,10.0,8.6,7.8,7.4, &
       14.2,13.5,13.1,12.8,11.9,10.9,9.8,8.5,7.8,7.5, &
       14.3,14.0,13.4,12.7,11.6,10.6,9.3,8.4,7.6,7.3/
      DATA O3LO1/ &
       14.9,14.2,13.3,12.5,11.2,10.3,9.5,8.6,7.5,7.4, &
       14.5,14.1,13.0,11.8,10.5,9.8,9.2,7.9,7.4,7.4, &
       11.8,11.5,10.9,10.5,9.9,9.6,8.9,7.5,7.2,7.2, &
       7.3,7.7,7.8,8.4,8.4,8.5,7.9,7.4,7.1,7.1, &
       4.1,4.4,5.3,6.6,6.9,7.5,7.4,7.2,7.0,6.9, &
       1.8,1.9,2.5,3.3,4.5,5.8,6.3,6.3,6.4,6.1, &
       0.4,0.5,0.8,1.2,2.7,3.6,4.6,4.7,5.0,5.2, &
       .10,.15,.20,.50,1.4,2.1,3.0,3.2,3.5,3.9, &
       .07,.10,.12,.30,1.0,1.4,1.8,1.9,2.3,2.5, &
       .06,.08,.10,.15,.60,.80,1.4,1.5,1.5,1.6, &
       .05,.05,.06,.09,.20,.40,.70,.80,.90,.90, &
       .05,.05,.06,.08,.10,.13,.20,.25,.30,.40, &
       .05,.05,.05,.06,.07,.07,.08,.09,.10,.13, &
       .05,.05,.05,.05,.06,.06,.06,.06,.07,.07, &
       .05,.05,.05,.05,.05,.05,.05,.06,.06,.06, &
       .04,.04,.04,.04,.04,.04,.04,.05,.05,.05/
      DATA O3LO2/ &
       14.8,14.2,13.8,12.2,11.0,9.8,8.5,7.8,7.4,6.9, &
       13.2,13.0,12.5,11.3,10.4,9.0,7.8,7.5,7.0,6.6, &
       10.6,10.6,10.7,10.1,9.4,8.6,7.5,7.0,6.5,6.1, &
       7.0,7.3,7.5,7.5,7.5,7.3,6.7,6.4,6.0,5.8, &
       3.8,4.0,4.7,5.0,5.2,5.9,5.8,5.6,5.5,5.5, &
       1.4,1.6,2.4,3.0,3.7,4.1,4.6,4.8,5.1,5.0, &
       .40,.50,.90,1.2,2.0,2.7,3.2,3.6,4.3,4.1, &
       .07,.10,.20,.30,.80,1.4,2.1,2.4,2.7,3.0, &
       .06,.07,.09,.15,.30,.70,1.2,1.4,1.6,2.0, &
       .05,.05,.06,.12,.15,.30,.60,.70,.80,.80, &
       .04,.05,.06,.08,.09,.15,.30,.40,.40,.40, &
       .04,.04,.05,.055,.06,.09,.12,.13,.15,.15, &
       .03,.03,.045,.052,.055,.06,.07,.07,.06,.07, &
       .03,.03,.04,.051,.052,.052,.06,.06,.05,.05, &
       .02,.02,.03,.05,.05,.05,.04,.04,.04,.04, &
       .02,.02,.02,.04,.04,.04,.03,.03,.03,.03/
      DATA O3LO3/ &
       14.5,14.0,13.5,11.3,11.0,10.0,9.0,8.3,7.5,7.3, &
       13.5,13.2,12.5,11.1,10.4,9.7,8.2,7.8,7.4,6.8, &
       10.8,10.9,11.0,10.4,10.0,9.6,7.9,7.5,7.0,6.7, &
       7.3,7.5,7.8,8.5,9.0,8.5,7.7,7.4,6.9,6.5, &
       4.1,4.5,5.3,6.2,7.3,7.7,7.3,7.0,6.6,6.4, &
       1.8,2.0,2.2,3.8,4.3,5.6,6.2,6.2,6.4,6.2, &
       .30,.50,.60,1.5,2.8,3.7,4.5,4.7,5.5,5.6, &
       .09,.10,.15,.60,1.2,2.1,3.0,3.5,4.0,4.3, &
       .06,.08,.10,.30,.60,1.1,1.9,2.2,2.9,3.0, &
       .04,.05,.06,.15,.45,.60,1.1,1.3,1.6,1.8, &
       .04,.04,.04,.08,.20,.30,.55,.60,.75,.90, &
       .04,.04,.04,.05,.06,.10,.12,.15,.20,.25, &
       .04,.04,.03,.04,.05,.06,.07,.07,.07,.08, &
       .03,.03,.04,.05,.05,.05,.05,.05,.05,.05, &
       .03,.03,.03,.04,.04,.04,.05,.05,.04,.04, &
       .02,.02,.02,.04,.04,.04,.04,.04,.03,.03/
      DATA O3LO4/ &
       14.2,13.8,13.2,12.5,11.7,10.5,8.6,7.8,7.5,6.6, &
       12.5,12.4,12.2,11.7,10.8,9.8,7.8,7.2,6.5,6.1, &
       10.6,10.5,10.4,10.1,9.6,9.0,7.1,6.8,6.1,5.9, &
       7.0,7.4,7.9,7.8,7.6,7.3,6.2,6.1,5.8,5.6, &
       4.2,4.6,5.1,5.6,5.9,5.9,5.9,5.8,5.6,5.3, &
       2.1,2.3,2.6,2.9,3.5,4.3,4.8,4.9,5.1,5.1, &
       0.7,0.8,1.0,1.5,2.0,2.8,3.5,3.6,3.7,4.0, &
       .15,.20,.40,.50,.60,1.4,2.1,2.2,2.3,2.5, &
       .08,.10,.15,.25,.30,.90,1.2,1.3,1.4,1.6, &
       .07,.08,.10,.14,.20,.50,.70,.90,.90,.80, &
       .05,.06,.08,.12,.14,.20,.35,.40,.60,.50, &
       .05,.05,.08,.09,.09,.09,.11,.12,.15,.18, &
       .04,.05,.06,.07,.07,.08,.08,.08,.08,.08, &
       .04,.04,.05,.07,.07,.07,.07,.07,.06,.05, &
       .02,.02,.04,.05,.05,.05,.05,.05,.04,.04, &
       .02,.02,.03,.04,.04,.04,.04,.04,.03,.03/












      N=kte;NP=N+1;NP2=N+2;NM1=N-1

      NKK=41
      NK=81
      NKP=NK+1
      DO 24 K=1,NP

   24 PHALF(K)=PHALF(K)*0.01*1.0E+03

      DO 25 K=1,NK
      PH(K)=PH(K)*1013250.
   25 P(K)=P(K)*1013250.
      PH(NKP)=PH(NKP)*1013250.





      DO 1010 K=1,25
      DO 1010 L=1,10
        RO31(L,K)=O3HI(L,K)
        RO32(L,K)=O3HI(L,K)
1010  CONTINUE

      DO 3000 NCASE=1,4
      ITAPE=NCASE+50
      IPLACE=2
      IF (NCASE.EQ.2) IPLACE=4
      IF (NCASE.EQ.3) IPLACE=1
      IF (NCASE.EQ.4) IPLACE=3




      IF (NCASE.EQ.1.OR.NCASE.EQ.2) THEN
         DO 1011 K=26,41
         DO 1011 L=1,10
           RO31(L,K)=O3LO1(L,K-25)
           RO32(L,K)=O3LO2(L,K-25)
1011     CONTINUE
      ENDIF
      IF (NCASE.EQ.3.OR.NCASE.EQ.4) THEN
         DO 1031 K=26,41
         DO 1031 L=1,10
           RO31(L,K)=O3LO3(L,K-25)
           RO32(L,K)=O3LO4(L,K-25)
1031     CONTINUE
      ENDIF
      DO 30 KK=1,NKK
      DO 31 L=1,10
      DUO3N(L,KK)=RO31(11-L,KK)
   31 DUO3N(L+9,KK)=RO32(L,KK)
      DUO3N(10,KK)=.5*(RO31(1,KK)+RO32(1,KK))
   30 CONTINUE

      IF (NCASE.EQ.2.OR.NCASE.EQ.4) THEN
         DO 1024 KK=1,NKK
         DO 1025 L=1,19
           TEMPN(L)=DUO3N(20-L,KK)
1025     CONTINUE
         DO 1026 L=1,19
           DUO3N(L,KK)=TEMPN(L)
1026     CONTINUE
1024     CONTINUE
      ENDIF




      DO 33 L=1,19
      DO 22 KK=1,NKK
   22 RSTD(KK)=DUO3N(L,KK)
      NKM=NK-1
      NKMM=NK-3

      DO 60 K=4,NKMM,2
      KI=K/2
   60 RDATA(K)=.5*(RSTD(KI)+RSTD(KI+1))-(RSTD(KI+2)-RSTD(KI+1)-RSTD(KI)+ &
      RSTD(KI-1))/16.
      RDATA(2)=.5*(RSTD(2)+RSTD(1))
      RDATA(NKM)=.5*(RSTD(NKK)+RSTD(NKK-1))

      DO 61 K=1,NK,2
      KQ=(K+1)/2
   61 RDATA(K)=RSTD(KQ)



      DO 99 KK=1,N
      RBAR(KK)=0.

      DO 98 K=1,NK
      IF(PH(K+1).LT.PHALF(KK)) GO TO 98
      IF(PH(K).GT.PHALF(KK+1)) GO TO 98
      IF(PH(K+1).LT.PHALF(KK+1).AND.PH(K).LT.PHALF(KK)) RBAR(KK)=RBAR(KK &
      )+RDATA(K)*(PH(K+1)-PHALF(KK))
      IF(PH(K+1).LT.PHALF(KK+1).AND.PH(K).GE.PHALF(KK)) RBAR(KK)=RBAR(KK &
      )+RDATA(K)*(PH(K+1)-PH(K))
      IF(PH(K+1).GT.PHALF(KK+1).AND.PH(K).GT.PHALF(KK)) RBAR(KK)=RBAR(KK &
      )+RDATA(K)*(PHALF(KK+1)-PH(K))
   98 CONTINUE
      RBAR(KK)=RBAR(KK)/(PHALF(KK+1)-PHALF(KK))
      IF(RBAR(KK).GT..0000) GO TO 99




      DO 29 K=1,NK
      IF(PH(K).LT.PHALF(KK).AND.PH(K+1).GE.PHALF(KK+1)) RBAR(KK)=RDATA(K)
   29 CONTINUE
   99 CONTINUE

      O3RD=0.
      DO 89 KK=1,80
   89 O3RD=O3RD+RDATA(KK)*(PH(KK+1)-PH(KK))
      O3RD=O3RD+RDATA(81)*(P(81)-PH(81))
      O3RD=O3RD/980.
      O3TOT=0.
      DO 88 KK=1,N
   88 O3TOT=O3TOT+RBAR(KK)*(PHALF(KK+1)-PHALF(KK))
      O3TOT=O3TOT/980.

      O3DU=O3TOT/2.144



      DO 23 KK=1,N
   23 DDUO3(L,KK)=RBAR(KK)*.01
   33 CONTINUE




      DO 1060 KK=1,N
        DO 1061 L=1,19
          O35DEG(2*L-1,KK)=DDUO3(L,KK)
1061    CONTINUE
        DO 1062 L=1,18
          O35DEG(2*L,KK)=0.5*(DDUO3(L,KK)+DDUO3(L+1,KK))
1062    CONTINUE
1060  CONTINUE



      IF (IPLACE.EQ.1) THEN
      DO 302 JJ=1,37
       DO 302 KEN=1,N
        DDUO3N(JJ,KEN) = O35DEG(JJ,KEN)
  302 CONTINUE
      ELSE IF (IPLACE.EQ.2) THEN
      DO 312 JJ=1,37
       DO 312 KEN=1,N
        DDO3N2(JJ,KEN) = O35DEG(JJ,KEN)
  312 CONTINUE
      ELSE IF (IPLACE.EQ.3) THEN
      DO 322 JJ=1,37
       DO 322 KEN=1,N
        DDO3N3(JJ,KEN) = O35DEG(JJ,KEN)
  322 CONTINUE
      ELSE IF (IPLACE.EQ.4) THEN
      DO 332 JJ=1,37
       DO 332 KEN=1,N
        DDO3N4(JJ,KEN) = O35DEG(JJ,KEN)
  332 CONTINUE
      END IF

3000  CONTINUE

      RETURN
   1  FORMAT(10F4.2)
    2 FORMAT(10X,E14.7,1X,E14.7,1X,E14.7,1X,E14.7,1X)
   3  FORMAT(10E12.5)
  797 FORMAT(10F7.2)
  799 FORMAT(19F6.4)
  800 FORMAT(19F6.2)
  102 FORMAT(' O3 IPLACE=',I4)
 1033 FORMAT(19F6.5)
  101 FORMAT(5X,1H*,F6.5,1H,,F6.5,1H,,F6.5,1H,,F6.5,1H,,F6.5,1H,,F6.5, &
      1H,,F6.5,1H,,F6.5,1H,,F6.5,1H,)
      
      END SUBROUTINE O3INT


  SUBROUTINE CLO89(CLDFAC,CAMT,NCLDS,KBTM,KTOP                  &
      ,          ids,ide, jds,jde, kds,kde                      &
      ,          ims,ime, jms,jme, kms,kme                      &
      ,          its,ite, jts,jte, kts,kte                      )

 IMPLICIT NONE

      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte










      REAL,    INTENT(OUT),DIMENSION(its:ite,kts:kte+1,kts:kte+1) :: CLDFAC
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: CAMT
      INTEGER, INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: KBTM,KTOP
      INTEGER, INTENT(IN), DIMENSION(its:ite)           :: NCLDS

      REAL,    DIMENSION(kts:kte+1,kts:kte+1,64) :: CLDIPT
      REAL,    DIMENSION(kts:kte+1) :: CLDROW
      INTEGER:: IQ,ITOP,I,J,JTOP,IR,IP,K1,K2,KB,K,KP,KT,NC
      REAL   :: XCLD

      INTEGER :: L,LP1,LP2,LP3,LM1,LM2,LM3,MYIS,MYIE

    
    
    
    
    

      L=kte
      LP1=L+1;  LP2=L+2;  LP3=L+3
      LM1=L-1;  LM2=L-2;  LM3=L-3
      MYIS=its; MYIE=ite


      DO 1 IQ=MYIS,MYIE,64
      ITOP=IQ+63
      IF(ITOP.GT.MYIE) ITOP=MYIE
      JTOP=ITOP-IQ+1
      DO 11 IP=1,JTOP
      IR=IQ+IP-1
      IF (NCLDS(IR).EQ.0) THEN
        DO 25 J=1,LP1
        DO 25 I=1,LP1
        CLDIPT(I,J,IP)=1.
25      CONTINUE
      ENDIF
      IF (NCLDS(IR).GE.1) THEN
          XCLD=1.-CAMT(IR,2)
           K1=KTOP(IR,2)+1
           K2=KBTM(IR,2)
          DO 27 J=1,LP1
              CLDROW(J)=1.
27        CONTINUE
          DO 29 J=1,K2
              CLDROW(J)=XCLD
29        CONTINUE
          KB=MAX(K1,K2+1)
          DO 33 K=KB,LP1
          DO 33 KP=1,LP1
               CLDIPT(KP,K,IP)=CLDROW(KP)
33        CONTINUE
          DO 37 J=1,LP1
              CLDROW(J)=1.
37        CONTINUE
          DO 39 J=K1,LP1
              CLDROW(J)=XCLD
39        CONTINUE
          KT=MIN(K1-1,K2)
          DO 43 K=1,KT
          DO 43 KP=1,LP1
              CLDIPT(KP,K,IP)=CLDROW(KP)
43        CONTINUE
          IF(K2+1.LE.K1-1) THEN
            DO 31 J=K2+1,K1-1
            DO 31 I=1,LP1
                CLDIPT(I,J,IP)=1.
31          CONTINUE
          ELSE IF(K1.LE.K2) THEN
            DO 32 J=K1,K2
            DO 32 I=1,LP1
                CLDIPT(I,J,IP)=XCLD
32          CONTINUE
          ENDIF
      ENDIF

      IF (NCLDS(IR).GE.2) THEN
        DO 21 NC=2,NCLDS(IR)
          XCLD=1.-CAMT(IR,NC+1)
           K1=KTOP(IR,NC+1)+1
           K2=KBTM(IR,NC+1)
          DO 47 J=1,LP1
              CLDROW(J)=1.
47        CONTINUE
          DO 49 J=1,K2
              CLDROW(J)=XCLD
49        CONTINUE
          KB=MAX(K1,K2+1)
          DO 53 K=KB,LP1
          DO 53 KP=1,LP1
               CLDIPT(KP,K,IP)=CLDIPT(KP,K,IP)*CLDROW(KP)
53        CONTINUE
          DO 57 J=1,LP1
              CLDROW(J)=1.
57        CONTINUE
          DO 59 J=K1,LP1
              CLDROW(J)=XCLD
59        CONTINUE
          KT=MIN(K1-1,K2)
          DO 63 K=1,KT
          DO 63 KP=1,LP1
              CLDIPT(KP,K,IP)=CLDIPT(KP,K,IP)*CLDROW(KP)
63        CONTINUE
          IF(K1.LE.K2) THEN
            DO 52 J=K1,K2
            DO 52 I=1,LP1
                CLDIPT(I,J,IP)=CLDIPT(I,J,IP)*XCLD
52          CONTINUE
          ENDIF
21        CONTINUE
      ENDIF
11    CONTINUE
      DO 71 J=1,LP1
      DO 71 I=1,LP1
      DO 71 IP=1,JTOP
      IR=IQ+IP-1
      CLDFAC(IR,I,J)=CLDIPT(I,J,IP)
71    CONTINUE
1     CONTINUE

  END SUBROUTINE CLO89


















      SUBROUTINE LWR88(HEATRA,GRNFLX,TOPFLX,                         &
                       PRESS,TEMP,RH2O,QO3,CLDFAC,                   &
                       CAMT,NCLDS,KTOP,KBTM,                         &

                       BO3RND,AO3RND, &
                       APCM,BPCM,ATPCM,BTPCM,ACOMB,BCOMB,BETACM,     &
                       ZERO,ONE,H18E3,P0INV,H6P08108,DIFFCTR,        &
                       GINV,H3M4,BETINW,RATH2OMW,GP0INV,P0,P0XZP8,   &
                       P0XZP2,H3M3,H1M3,H1M2,H25E2,B0,B2,B1,B3,HAF,  &
                       TEN,HP1,FOUR,HM1EZ,                           &
                       RADCON,QUARTR,TWO,                            &
                       HM6666M2,HMP66667,HMP5, HP166666,H41666M2,    &
                       RADCON1,H16E1, H28E1,H44194M2,H1P41819,       &
                       ids,ide, jds,jde, kds,kde,                    &
                       ims,ime, jms,jme, kms,kme,                    &
                       its,ite, jts,jte, kts,kte                     )

 IMPLICIT NONE



      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte  
      REAL,    INTENT(IN)        :: ZERO,ONE,H18E3,P0INV,H6P08108,DIFFCTR
      REAL,    INTENT(IN)        :: GINV,H3M4,BETINW,RATH2OMW,GP0INV
      REAL,    INTENT(IN)        :: P0XZP8,P0XZP2,H3M3,P0,H1M3
      REAL,    INTENT(IN)        :: H1M2,H25E2,B0,B1,B2,B3,HAF

      REAL,    INTENT(IN)        :: TEN,HP1,FOUR,HM1EZ         

      REAL,    INTENT(IN)        :: RADCON,QUARTR,TWO
      REAL,    INTENT(IN)        :: HM6666M2,HMP66667,HMP5, HP166666,H41666M2

      REAL,    INTENT(IN) :: RADCON1,H16E1, H28E1,H44194M2,H1P41819

      REAL, INTENT(IN), DIMENSION(3) :: BO3RND,AO3RND


      REAL,INTENT(IN),DIMENSION(NBLY) :: APCM,BPCM,ATPCM,BTPCM,ACOMB, &
                                         BCOMB,BETACM

      REAL,    INTENT(IN),DIMENSION(its:ite,kts:kte+1,kts:kte+1) :: CLDFAC
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: CAMT
      INTEGER, INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: KBTM,KTOP
      INTEGER, INTENT(IN), DIMENSION(its:ite)           :: NCLDS
     
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: PRESS,TEMP
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte)   :: RH2O,QO3
      REAL,    INTENT(OUT), DIMENSION(its:ite,kts:kte)   :: HEATRA
      REAL,    INTENT(OUT), DIMENSION(its:ite)           :: GRNFLX,TOPFLX





      














      REAL,    DIMENSION(its:ite,kts:kte+1) :: TEXPSL,TOTPHI,TOTO3,CNTVAL,&
                                               TPHIO3,TOTVO2,TSTDAV,TDAV, & 
                                               VSUM3,CO2R1,D2CD21,DCO2D1, &
                                               CO2R2,D2CD22,DCO2D2,CO2SP1,&
                                               CO2SP2,CO2R,DCO2DT,D2CDT2, &
                                               TLSQU,DIFT
      REAL,    DIMENSION(its:ite,kts:kte)   :: DELP2,DELP,CO2NBL,&
                                               QH2O,VV,VAR1,VAR2,VAR3,VAR4
      REAL,    DIMENSION(its:ite,kts:kte+1) :: P,T
      REAL,    DIMENSION(its:ite,kts:kte)   :: CO2MR,CO2MD,CO2M2D
      REAL,    DIMENSION(its:ite,kts:kte*2+1):: EMPL

      REAL,    DIMENSION(its:ite)           :: EMX1,EMX2,VSUM1,VSUM2,A1,A2 
      REAL,    DIMENSION(its:ite,kts:kte+1,kts:kte+1) :: CO21

   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   

   
   
   
   






    
      INTEGER :: K, I,KP
      INTEGER :: L,LP1,LP2,LP3,LM1,LM2,LM3,MYIS,MYIE,LLP1,LL

      L=kte
      LP1=L+1;  LP2=L+2;  LP3=L+3; LLP1 = 2*L + 1
      LM1=L-1;  LM2=L-2;  LM3=L-3; LL = 2*L
      MYIS=its; MYIE=ite


      DO 103 K=2,L
      DO 103 I=MYIS,MYIE
      P(I,K)=HAF*(PRESS(I,K-1)+PRESS(I,K))
      T(I,K)=HAF*(TEMP(I,K-1)+TEMP(I,K))
103   CONTINUE
      DO 105 I=MYIS,MYIE
      P(I,1)=ZERO
      P(I,LP1)=PRESS(I,LP1)
      T(I,1)=TEMP(I,1)
      T(I,LP1)=TEMP(I,LP1)
105   CONTINUE
      DO 107 K=1,L
      DO 107 I=MYIS,MYIE
      DELP2(I,K)=P(I,K+1)-P(I,K)
      DELP(I,K)=ONE/DELP2(I,K)
107   CONTINUE


      DO 125 K=1,LP1
      DO 125 I=MYIS,MYIE
      TEXPSL(I,K)=H18E3/TEMP(I,K)-H6P08108

      TEXPSL(I,K)=EXP(TEXPSL(I,K))
125   CONTINUE







      DO 131 K=1,L
      DO 131 I=MYIS,MYIE
      QH2O(I,K)=RH2O(I,K)*DIFFCTR


      VV(I,K)=HAF*(P(I,K+1)+P(I,K))*P0INV
      VAR1(I,K)=DELP2(I,K)*QH2O(I,K)*GINV
      VAR3(I,K)=DELP2(I,K)*QO3(I,K)*DIFFCTR*GINV
      VAR2(I,K)=VAR1(I,K)*(VV(I,K)+H3M4)
      VAR4(I,K)=VAR3(I,K)*(VV(I,K)+H3M3)







      CNTVAL(I,K)=TEXPSL(I,K)*RH2O(I,K)*VAR2(I,K)*BETINW/ &
                   (RH2O(I,K)+RATH2OMW)
131   CONTINUE

      DO 201 I=MYIS,MYIE
      TOTPHI(I,1)=ZERO
      TOTO3(I,1)=ZERO
      TPHIO3(I,1)=ZERO
      TOTVO2(I,1)=ZERO
201   CONTINUE
      DO 203 K=2,LP1
      DO 203 I=MYIS,MYIE
      TOTPHI(I,K)=TOTPHI(I,K-1)+VAR2(I,K-1)
      TOTO3(I,K)=TOTO3(I,K-1)+VAR3(I,K-1)
      TPHIO3(I,K)=TPHIO3(I,K-1)+VAR4(I,K-1)
      TOTVO2(I,K)=TOTVO2(I,K-1)+CNTVAL(I,K-1)
203   CONTINUE





      DO 801 I=MYIS,MYIE
      EMX1(I)=QH2O(I,L)*PRESS(I,L)*(PRESS(I,L)-P(I,L))*GP0INV
      EMX2(I)=QH2O(I,L)*PRESS(I,L)*(P(I,LP1)-PRESS(I,L))*GP0INV
801   CONTINUE


      DO 811 K=1,L
      DO 811 I=MYIS,MYIE
      EMPL(I,K+1)=QH2O(I,K)*P(I,K+1)*(P(I,K+1)-PRESS(I,K))*GP0INV
811   CONTINUE
      DO 812 K=1,LM1
      DO 812 I=MYIS,MYIE
      EMPL(I,LP2+K-1)=QH2O(I,K+1)*P(I,K+1)*(PRESS(I,K+1)-P(I,K+1)) &
                     *GP0INV
812   CONTINUE
      DO 821 I=MYIS,MYIE
      EMPL(I,1)=VAR2(I,L)
      EMPL(I,LLP1)=EMPL(I,LL)
821   CONTINUE



      DO 161 I=MYIS,MYIE
      TSTDAV(I,1)=ZERO
      TDAV(I,1)=ZERO
161   CONTINUE
      DO 162 K=1,LP1
      DO 162 I=MYIS,MYIE
      VSUM3(I,K)=TEMP(I,K)-STEMP(K)
162   CONTINUE
      DO 163 K=1,L
      DO 165 I=MYIS,MYIE
      VSUM2(I)=GTEMP(K)*DELP2(I,K)
      VSUM1(I)=VSUM2(I)*VSUM3(I,K)
      TSTDAV(I,K+1)=TSTDAV(I,K)+VSUM2(I)
      TDAV(I,K+1)=TDAV(I,K)+VSUM1(I)
165   CONTINUE
163   CONTINUE


      DO 171 I=MYIS,MYIE
      A1(I)=(PRESS(I,LP1)-P0XZP8)/P0XZP2
      A2(I)=(P0-PRESS(I,LP1))/P0XZP2
171   CONTINUE




      DO 184 K=1,LP1
      DO 184 I=MYIS,MYIE
        CO2R1(I,K)=A1(I)*CO231(K)+A2(I)*CO238(K)
        D2CD21(I,K)=H1M3*(A1(I)*C2D31(K)+A2(I)*C2D38(K))
        DCO2D1(I,K)=H1M2*(A1(I)*CDT31(K)+A2(I)*CDT38(K))
        CO2R2(I,K)=A1(I)*CO271(K)+A2(I)*CO278(K)
        D2CD22(I,K)=H1M3*(A1(I)*C2D71(K)+A2(I)*C2D78(K))
        DCO2D2(I,K)=H1M2*(A1(I)*CDT71(K)+A2(I)*CDT78(K))
184   CONTINUE
      DO 190 K=1,L
      DO 190 I=MYIS,MYIE
        CO2MR(I,K)=A1(I)*CO2M51(K)+A2(I)*CO2M58(K)
        CO2MD(I,K)=H1M2*(A1(I)*CDTM51(K)+A2(I)*CDTM58(K))
        CO2M2D(I,K)=H1M3*(A1(I)*C2DM51(K)+A2(I)*C2DM58(K))
190   CONTINUE





      DO 211 KP=2,LP1
      DO 211 I=MYIS,MYIE
      DIFT(I,KP)=TDAV(I,KP)/TSTDAV(I,KP)
211   CONTINUE
      DO 212 I=MYIS,MYIE
      CO21(I,1,1)=1.0
      CO2SP1(I,1)=1.0
      CO2SP2(I,1)=1.0
212   CONTINUE
      DO 215 KP=2,LP1
      DO 215 I=MYIS,MYIE

      CO2R(I,KP)=A1(I)*CO251(KP,1)+A2(I)*CO258(KP,1)
      DCO2DT(I,KP)=H1M2*(A1(I)*CDT51(KP,1)+A2(I)*CDT58(KP,1))
      D2CDT2(I,KP)=H1M3*(A1(I)*C2D51(KP,1)+A2(I)*C2D58(KP,1))
      CO21(I,KP,1)=CO2R(I,KP)+DIFT(I,KP)*(DCO2DT(I,KP)+ &
                   HAF*DIFT(I,KP)*D2CDT2(I,KP))


      CO2R(I,KP)=A1(I)*CO251(1,KP)+A2(I)*CO258(1,KP)
      DCO2DT(I,KP)=H1M2*(A1(I)*CDT51(1,KP)+A2(I)*CDT58(1,KP))
      D2CDT2(I,KP)=H1M3*(A1(I)*C2D51(1,KP)+A2(I)*C2D58(1,KP))
      CO21(I,1,KP)=CO2R(I,KP)+DIFT(I,KP)*(DCO2DT(I,KP)+ &
                   HAF*DIFT(I,KP)*D2CDT2(I,KP))
215   CONTINUE



      DO 250 K=2,LP1
      DO 250 I=MYIS,MYIE
      CO2SP1(I,K)=CO2R1(I,K)+DIFT(I,K)*(DCO2D1(I,K)+HAF*DIFT(I,K)* &
       D2CD21(I,K))
      CO2SP2(I,K)=CO2R2(I,K)+DIFT(I,K)*(DCO2D2(I,K)+HAF*DIFT(I,K)* &
       D2CD22(I,K))
250   CONTINUE


      DO 220 K=2,L
      DO 222 KP=K+1,LP1
      DO 222 I=MYIS,MYIE
      DIFT(I,KP)=(TDAV(I,KP)-TDAV(I,K))/ &
                    (TSTDAV(I,KP)-TSTDAV(I,K))
      CO2R(I,KP)=A1(I)*CO251(KP,K)+A2(I)*CO258(KP,K)
      DCO2DT(I,KP)=H1M2*(A1(I)*CDT51(KP,K)+A2(I)*CDT58(KP,K))
      D2CDT2(I,KP)=H1M3*(A1(I)*C2D51(KP,K)+A2(I)*C2D58(KP,K))
      CO21(I,KP,K)=CO2R(I,KP)+DIFT(I,KP)*(DCO2DT(I,KP)+ &
                   HAF*DIFT(I,KP)*D2CDT2(I,KP))
      CO2R(I,KP)=A1(I)*CO251(K,KP)+A2(I)*CO258(K,KP)
      DCO2DT(I,KP)=H1M2*(A1(I)*CDT51(K,KP)+A2(I)*CDT58(K,KP))
      D2CDT2(I,KP)=H1M3*(A1(I)*C2D51(K,KP)+A2(I)*C2D58(K,KP))
      CO21(I,K,KP)=CO2R(I,KP)+DIFT(I,KP)*(DCO2DT(I,KP)+ &
                   HAF*DIFT(I,KP)*D2CDT2(I,KP))
222   CONTINUE
220   CONTINUE

      DO 206 K=2,LP1
      DO 206 I=MYIS,MYIE
      DIFT(I,K)=HAF*(VSUM3(I,K)+VSUM3(I,K-1))
      CO2R(I,K)=A1(I)*CO251(K,K)+A2(I)*CO258(K,K)
      DCO2DT(I,K)=H1M2*(A1(I)*CDT51(K,K)+A2(I)*CDT58(K,K))
      D2CDT2(I,K)=H1M3*(A1(I)*C2D51(K,K)+A2(I)*C2D58(K,K))
      CO21(I,K,K)=CO2R(I,K)+DIFT(I,K)*(DCO2DT(I,K)+ &
                   HAF*DIFT(I,K)*D2CDT2(I,K))
206   CONTINUE

      DO 260 K=1,L
      DO 260 I=MYIS,MYIE
      CO2NBL(I,K)=CO2MR(I,K)+VSUM3(I,K)*(CO2MD(I,K)+HAF* &
       VSUM3(I,K)*CO2M2D(I,K))
260   CONTINUE

      DO 264 K=1,LP1
      DO 264 I=MYIS,MYIE
      IF (T(I,K).LE.H25E2) THEN
         TLSQU(I,K)=B0+(T(I,K)-H25E2)* &
                            (B1+(T(I,K)-H25E2)* &
                         (B2+B3*(T(I,K)-H25E2)))
      ELSE
         TLSQU(I,K)=B0
      ENDIF
264   CONTINUE

      DO 280 K=1,LP1
      DO 282 KP=1,LP1
      DO 282 I=MYIS,MYIE
      CO21(I,KP,K)=CO21(I,KP,K)*(ONE-TLSQU(I,KP))+TLSQU(I,KP)
282   CONTINUE
280   CONTINUE
      DO 284 K=1,LP1
      DO 286 I=MYIS,MYIE
      CO2SP1(I,K)=CO2SP1(I,K)*(ONE-TLSQU(I,1))+TLSQU(I,1)
      CO2SP2(I,K)=CO2SP2(I,K)*(ONE-TLSQU(I,1))+TLSQU(I,1)
286   CONTINUE
284   CONTINUE
      DO 288 K=1,L
      DO 290 I=MYIS,MYIE
      CO2NBL(I,K)=CO2NBL(I,K)*(ONE-TLSQU(I,K))+TLSQU(I,K)
290   CONTINUE
288   CONTINUE





















      CALL FST88(HEATRA,GRNFLX,TOPFLX, &
                 QH2O,PRESS,P,DELP,DELP2,TEMP,T, &
                 CLDFAC,NCLDS,KTOP,KBTM,CAMT, &
                 CO21,CO2NBL,CO2SP1,CO2SP2, &
                 VAR1,VAR2,VAR3,VAR4,CNTVAL, &
                 TOTO3,TPHIO3,TOTPHI,TOTVO2, &
                 EMX1,EMX2,EMPL, &

                 BO3RND,AO3RND, &

                 APCM,BPCM,ATPCM,BTPCM,ACOMB,BCOMB,BETACM,     &
                 TEN,HP1,HAF,ONE,FOUR,HM1EZ,       &
                 RADCON,QUARTR,TWO,  &
                 HM6666M2,HMP66667,HMP5, &
                 HP166666,H41666M2,RADCON1, &
                 H16E1, H28E1, H25E2, H44194M2,H1P41819, &
                 ids,ide, jds,jde, kds,kde,                    &
                 ims,ime, jms,jme, kms,kme,                    &
                 its,ite, jts,jte, kts,kte                     )

  END SUBROUTINE LWR88





















  SUBROUTINE FST88(HEATRA,GRNFLX,TOPFLX, &
                       QH2O,PRESS,P,DELP,DELP2,TEMP,T, &
                       CLDFAC,NCLDS,KTOP,KBTM,CAMT, &
                       CO21,CO2NBL,CO2SP1,CO2SP2, &
                       VAR1,VAR2,VAR3,VAR4,CNTVAL, &
                       TOTO3,TPHIO3,TOTPHI,TOTVO2, &
                       EMX1,EMX2,EMPL, &
                       BO3RND,AO3RND, &

                       APCM,BPCM,ATPCM,BTPCM,ACOMB,BCOMB,BETACM,     &
                       TEN,HP1,HAF,ONE,FOUR,HM1EZ,       &
                       RADCON,QUARTR,TWO, &
                       HM6666M2,HMP66667,HMP5, &
                       HP166666,H41666M2,RADCON1, &
                       H16E1, H28E1, H25E2, H44194M2,H1P41819, &
                       ids,ide, jds,jde, kds,kde,                    &
                       ims,ime, jms,jme, kms,kme,                    &
                       its,ite, jts,jte, kts,kte                     )

 IMPLICIT NONE



      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte


      REAL,    INTENT(IN)        :: TEN,HP1,HAF,ONE,FOUR,HM1EZ

      REAL,    INTENT(IN)        :: RADCON,QUARTR,TWO
      REAL,    INTENT(IN)        :: HM6666M2,HMP66667,HMP5
      REAL,    INTENT(IN)        :: HP166666,H41666M2,RADCON1,H16E1, H28E1 

      REAL,    INTENT(IN)        :: H25E2,H44194M2,H1P41819

      REAL,INTENT(IN),DIMENSION(NBLY) :: APCM,BPCM,ATPCM,BTPCM,ACOMB, &
                                         BCOMB,BETACM



      REAL, INTENT(IN), DIMENSION(its:ite,kts:kte*2+1) :: EMPL
      REAL, INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: TOTO3,TPHIO3,TOTPHI,CNTVAL,&
                                                        CO2SP1,CO2SP2   

      REAL,    INTENT(IN),DIMENSION(its:ite,kts:kte+1,kts:kte+1) :: CLDFAC
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: CAMT,TOTVO2
      INTEGER, INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: KBTM,KTOP
      INTEGER, INTENT(IN), DIMENSION(its:ite)           :: NCLDS
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte)   :: QH2O
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: PRESS,TEMP
      REAL,    INTENT(OUT), DIMENSION(its:ite,kts:kte)  :: HEATRA
      REAL,    INTENT(OUT), DIMENSION(its:ite)          :: GRNFLX,TOPFLX
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: P,T
      REAL,    INTENT(INOUT), DIMENSION(its:ite,kts:kte+1,kts:kte+1) :: CO21
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte)   :: CO2NBL,DELP2, &
                                                           DELP,&
                                               VAR1,VAR2,VAR3,VAR4
      REAL, INTENT(IN), DIMENSION(3) :: BO3RND,AO3RND
      REAL, INTENT(IN), DIMENSION(its:ite)   :: EMX1,EMX2
      
      REAL, DIMENSION(its:ite,kts:kte*2+1) :: TPL,EMD,ALP,C,CSUB,CSUB2
      REAL, DIMENSION(its:ite,kts:kte*2+1) :: C2
      INTEGER, DIMENSION(its:ite,kts:kte+1) :: IXO
      REAL, DIMENSION(its:ite,kts:kte+1) :: VTMP3,FXO,DT,FXOE2,DTE2, &
                                            SS1,CSOUR,TC,OSS,CSS,DTC,SS2,&
                                            AVEPHI,E1CTS1,E1FLX,  &
                                            E1CTW1,DSORC,EMISS,FAC1,&
                                            TO3SP,OVER1D,CNTTAU,TOTEVV,&
                                            CO2SP,FLX,AVMO3, &
                                            AVPHO3,AVVO2,CONT1D,TO31D,EMISDG,&
                                            DELPR1
      REAL, DIMENSION(its:ite,kts:kte+1) :: EMISSB,DELPR2,CONTDG,TO3DG,HEATEM,&
                                            VSUM1,FLXNET,Z1

      REAL, DIMENSION(its:ite,kts:kte+1,NBLY) :: SORC
      REAL, DIMENSION(its:ite,kts:kte)   :: E1CTS2,E1CTW2,TO3SPC,RLOG,EXCTS,&
                                            CTSO3,CTS
      REAL, DIMENSION(its:ite)   :: GXCTS,FLX1E1
      REAL, DIMENSION(its:ite)   :: PTOP,PBOT,FTOP,FBOT,DELPTC
      REAL, DIMENSION(its:ite,2) :: FXOSP,DTSP,EMSPEC

      INTEGER :: K, I,KP,LLM2,J1,J3,KMAX,KMIN,KCLDS,ICNT,LLM1
      INTEGER :: L,LP1,LP2,LP3,LM1,LM2,LM3,MYIS,MYIE,LLP1,LL,KK,KLEN

      L=kte
      LP1=L+1;  LP2=L+2;  LP3=L+3; LLP1 = 2*L + 1
      LM1=L-1;  LM2=L-2;  LM3=L-3; LL = 2*L
      LLM2 = LL-2; LLM1=LL-1
      MYIS=its; MYIE=ite


      DO 101 K=1,LP1
      DO 101 I=MYIS,MYIE

      VTMP3(I,K)=AINT(TEMP(I,K)*HP1)
      FXO(I,K)=VTMP3(I,K)-9.
      DT(I,K)=TEMP(I,K)-TEN*VTMP3(I,K)

      IXO(I,K)=FXO(I,K)
101   CONTINUE
      DO 103 k=1,L
      DO 103 I=MYIS,MYIE

      VTMP3(I,K)=AINT(T(I,K+1)*HP1)
      FXOE2(I,K)=VTMP3(I,K)-9.
      DTE2(I,K)=T(I,K+1)-TEN*VTMP3(I,K)
103   CONTINUE

      DO 105 I=MYIS,MYIE
      FXOE2(I,LP1)=FXO(I,L)
      DTE2(I,LP1)=DT(I,L)
      FXOSP(I,1)=FXOE2(I,LM1)
      FXOSP(I,2)=FXO(I,LM1)
      DTSP(I,1)=DTE2(I,LM1)
      DTSP(I,2)=DT(I,LM1)
105   CONTINUE


      DO 4114 I=MYIS,MYIE
      DO 4114 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),1)
        DSORC(I,K)=DSRCE(IXO(I,K),1)
4114   CONTINUE
      DO 4112 K=1,LP1
      DO 4112 I=MYIS,MYIE
      SORC(I,K,1)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
4112   CONTINUE

      DO 4214 I=MYIS,MYIE
      DO 4214 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),2)
        DSORC(I,K)=DSRCE(IXO(I,K),2)
4214   CONTINUE
      DO 4212 K=1,LP1
      DO 4212 I=MYIS,MYIE
      SORC(I,K,2)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
4212   CONTINUE

      DO 4314 I=MYIS,MYIE
      DO 4314 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),3)
        DSORC(I,K)=DSRCE(IXO(I,K),3)
4314   CONTINUE
      DO 4312 K=1,LP1
      DO 4312 I=MYIS,MYIE
      SORC(I,K,3)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
4312   CONTINUE

      DO 4414 I=MYIS,MYIE
      DO 4414 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),4)
        DSORC(I,K)=DSRCE(IXO(I,K),4)
4414   CONTINUE
      DO 4412 K=1,LP1
      DO 4412 I=MYIS,MYIE
      SORC(I,K,4)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
4412   CONTINUE

      DO 4514 I=MYIS,MYIE
      DO 4514 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),5)
        DSORC(I,K)=DSRCE(IXO(I,K),5)
4514   CONTINUE
      DO 4512 K=1,LP1
      DO 4512 I=MYIS,MYIE
      SORC(I,K,5)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
4512   CONTINUE

      DO 4614 I=MYIS,MYIE
      DO 4614 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),6)
        DSORC(I,K)=DSRCE(IXO(I,K),6)
4614   CONTINUE
      DO 4612 K=1,LP1
      DO 4612 I=MYIS,MYIE
      SORC(I,K,6)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
4612   CONTINUE

      DO 4714 I=MYIS,MYIE
      DO 4714 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),7)
        DSORC(I,K)=DSRCE(IXO(I,K),7)
4714   CONTINUE
      DO 4712 K=1,LP1
      DO 4712 I=MYIS,MYIE
      SORC(I,K,7)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
4712   CONTINUE

      DO 4814 I=MYIS,MYIE
      DO 4814 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),8)
        DSORC(I,K)=DSRCE(IXO(I,K),8)
4814   CONTINUE
      DO 4812 K=1,LP1
      DO 4812 I=MYIS,MYIE
      SORC(I,K,8)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
4812   CONTINUE

      DO 4914 I=MYIS,MYIE
      DO 4914 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),9)
        DSORC(I,K)=DSRCE(IXO(I,K),9)
4914   CONTINUE
      DO 4912 K=1,LP1
      DO 4912 I=MYIS,MYIE
      SORC(I,K,9)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
4912   CONTINUE

      DO 5014 I=MYIS,MYIE
      DO 5014 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),10)
        DSORC(I,K)=DSRCE(IXO(I,K),10)
5014  CONTINUE
      DO 5012 K=1,LP1
      DO 5012 I=MYIS,MYIE
      SORC(I,K,10)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
5012   CONTINUE

      DO 5114 I=MYIS,MYIE
      DO 5114 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),11)
        DSORC(I,K)=DSRCE(IXO(I,K),11)
5114   CONTINUE
      DO 5112 K=1,LP1
      DO 5112 I=MYIS,MYIE
      SORC(I,K,11)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
5112   CONTINUE

      DO 5214 I=MYIS,MYIE
      DO 5214 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),12)
        DSORC(I,K)=DSRCE(IXO(I,K),12)
5214   CONTINUE
      DO 5212 K=1,LP1
      DO 5212 I=MYIS,MYIE
      SORC(I,K,12)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
5212   CONTINUE

      DO 5314 I=MYIS,MYIE
      DO 5314 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),13)
        DSORC(I,K)=DSRCE(IXO(I,K),13)
5314   CONTINUE
      DO 5312 K=1,LP1
      DO 5312 I=MYIS,MYIE
      SORC(I,K,13)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
5312   CONTINUE

      DO 5414 I=MYIS,MYIE
      DO 5414 K=1,LP1
        VTMP3(I,K)=SOURCE(IXO(I,K),14)
        DSORC(I,K)=DSRCE(IXO(I,K),14)
5414   CONTINUE
      DO 5412 K=1,LP1
      DO 5412 I=MYIS,MYIE
      SORC(I,K,14)=VTMP3(I,K)+DT(I,K)*DSORC(I,K)
5412   CONTINUE









      DO 131 K=1,LP1
      DO 131 I=MYIS,MYIE
      SS1(I,K)=SORC(I,K,11)+SORC(I,K,12)+SORC(I,K,14)
131   CONTINUE
      DO 143 K=1,LP1
      DO 143 I=MYIS,MYIE
      CSOUR(I,K)=SORC(I,K,9)+SORC(I,K,10)
143   CONTINUE





      DO 901 K=1,LP1
      DO 901 I=MYIS,MYIE
      TC(I,K)=TEMP(I,K)*TEMP(I,K)*TEMP(I,K)*TEMP(I,K)
901   CONTINUE
      DO 903 K=1,L
      DO 903 I=MYIS,MYIE
      OSS(I,K+1)=SORC(I,K+1,13)-SORC(I,K,13)
      CSS(I,K+1)=CSOUR(I,K+1)-CSOUR(I,K)
      DTC(I,K+1)=TC(I,K+1)-TC(I,K)
      SS2(I,K+1)=SS1(I,K+1)-SS1(I,K)
903   CONTINUE







































      DO 3021 K=1,L
      DO 3021 I=MYIS,MYIE
      AVEPHI(I,K)=TOTPHI(I,K+1)
3021  CONTINUE





      DO 803 I=MYIS,MYIE
      AVEPHI(I,LP1)=AVEPHI(I,LM1)+EMX1(I)
803   CONTINUE

      CALL E1E290(E1CTS1,E1CTS2,E1FLX,E1CTW1,E1CTW2,EMISS, &
                  FXO,DT,FXOE2,DTE2,AVEPHI,TEMP,T,         &

                  H16E1,TEN,HP1,H28E1,HAF,                 &
                  ids,ide, jds,jde, kds,kde,               &
                  ims,ime, jms,jme, kms,kme,               &
                  its,ite, jts,jte, kts,kte                )

      DO 302 K=1,L
      DO 302 I=MYIS,MYIE
      FAC1(I,K)=BO3RND(2)*TPHIO3(I,K+1)/TOTO3(I,K+1)
      TO3SPC(I,K)=HAF*(FAC1(I,K)* &
          (SQRT(ONE+(FOUR*AO3RND(2)*TOTO3(I,K+1))/FAC1(I,K))-ONE))


      TO3SP(I,K)=EXP(HM1EZ*(TO3SPC(I,K)+SKO3R*TOTVO2(I,K+1)))
      OVER1D(I,K)=EXP(HM1EZ*(SQRT(AB15WD*TOTPHI(I,K+1))+ &
                  SKC1R*TOTVO2(I,K+1)))



      CNTTAU(I,K)=EXP(HM1EZ*TOTVO2(I,K+1))
      TOTEVV(I,K)=1./CNTTAU(I,K)
302   CONTINUE
      DO 3022 K=1,L
      DO 3022 I=MYIS,MYIE
      CO2SP(I,K+1)=OVER1D(I,K)*CO21(I,1,K+1)
3022  CONTINUE
      DO 3023 K=1,L
      DO 3023 I=MYIS,MYIE
      CO21(I,K+1,1)=CO21(I,K+1,1)*OVER1D(I,K)
3023  CONTINUE

      DO 1808 I=MYIS,MYIE
      RLOG(I,1)=OVER1D(I,1)*CO2NBL(I,1)
1808  CONTINUE



      DO 305 K=2,LP1
      DO 305 I=MYIS,MYIE
      FLX(I,K)= (TC(I,1)*E1FLX(I,K) &
                +SS1(I,1)*CNTTAU(I,K-1) &
                +SORC(I,1,13)*TO3SP(I,K-1) &
                +CSOUR(I,1)*CO2SP(I,K)) &
                *CLDFAC(I,1,K)
305   CONTINUE
      DO 307 I=MYIS,MYIE
      FLX(I,1)= TC(I,1)*E1FLX(I,1)+SS1(I,1)+SORC(I,1,13) &
                +CSOUR(I,1)
307   CONTINUE

      DO 303 KP=2,LP1
      DO 303 I=MYIS,MYIE
      FLX(I,1)=FLX(I,1)+(OSS(I,KP)*TO3SP(I,KP-1) &
                        +SS2(I,KP)*CNTTAU(I,KP-1) &
                        +CSS(I,KP)*CO21(I,KP,1) &
                        +DTC(I,KP)*EMISS(I,KP-1))*CLDFAC(I,KP,1)
303   CONTINUE



      CALL SPA88(EXCTS,CTSO3,GXCTS,SORC,CSOUR, &
                 CLDFAC,TEMP,PRESS,VAR1,VAR2, &
                 P,DELP,DELP2,TOTVO2,TO3SP,TO3SPC, &
                 CO2SP1,CO2SP2,CO2SP,              &
                 APCM,BPCM,ATPCM,BTPCM,ACOMB,BCOMB,BETACM,     &
                 H25E2,ONE,H44194M2,H1P41819,HAF,HM1EZ,TWO,    &

                 RADCON,                                 &
                 ids,ide, jds,jde, kds,kde,                    &
                 ims,ime, jms,jme, kms,kme,                    &
                 its,ite, jts,jte, kts,kte                     )







      DO 998 I=MYIS,MYIE
      VTMP3(I,1)=1.
998   CONTINUE
      DO 999 K=1,L
      DO 999 I=MYIS,MYIE
      VTMP3(I,K+1)=CNTTAU(I,K)*CLDFAC(I,K+1,1)
999   CONTINUE
      DO 1001 K=1,L
      DO 1001 I=MYIS,MYIE
      CTS(I,K)=RADCON*DELP(I,K)*(TC(I,K)* &
           (E1CTW2(I,K)*CLDFAC(I,K+1,1)-E1CTW1(I,K)*CLDFAC(I,K,1)) + &
            SS1(I,K)*(VTMP3(I,K+1)-VTMP3(I,K)))
1001  CONTINUE

      DO 1011 K=1,L
      DO 1011 I=MYIS,MYIE
      VTMP3(I,K)=TC(I,K)*(CLDFAC(I,K,1)*(E1CTS1(I,K)-E1CTW1(I,K)) - &
                        CLDFAC(I,K+1,1)*(E1CTS2(I,K)-E1CTW2(I,K)))
1011  CONTINUE
      DO 1012 I=MYIS,MYIE
      FLX1E1(I)=TC(I,LP1)*CLDFAC(I,LP1,1)* &
                (E1CTS1(I,LP1)-E1CTW1(I,LP1))
1012  CONTINUE
      DO 1014 K=1,L
      DO 1013 I=MYIS,MYIE
      FLX1E1(I)=FLX1E1(I)+VTMP3(I,K)
1013  CONTINUE
1014  CONTINUE





      DO 321 K=2,LM1
      KLEN=K

      DO 3218 KK=1,LP1-K
      DO 3218 I=MYIS,MYIE
      AVEPHI(I,KK+K-1)=TOTPHI(I,KK+K)-TOTPHI(I,K)
3218  CONTINUE
      DO 1803 I=MYIS,MYIE
      AVEPHI(I,LP1)=AVEPHI(I,LM1)+EMX1(I)
1803   CONTINUE






      CALL E290(EMISSB,EMISS,AVEPHI,KLEN,FXOE2,DTE2,  &

                       H16E1,HP1,H28E1,HAF,TEN,       &
                       ids,ide, jds,jde, kds,kde,     &
                       ims,ime, jms,jme, kms,kme,     &
                       its,ite, jts,jte, kts,kte      )

      DO 322 KK=1,LP1-K
      DO 322 I=MYIS,MYIE
      AVMO3(I,KK+K-1)=TOTO3(I,KK+K)-TOTO3(I,K)
      AVPHO3(I,KK+K-1)=TPHIO3(I,KK+K)-TPHIO3(I,K)
      AVVO2(I,KK+K-1)=TOTVO2(I,KK+K)-TOTVO2(I,K)
      CONT1D(I,KK+K-1)=CNTTAU(I,KK+K-1)*TOTEVV(I,K-1)
322   CONTINUE

      DO 3221 KK=1,LP1-K
      DO 3221 I=MYIS,MYIE
      FAC1(I,K+KK-1)=BO3RND(2)*AVPHO3(I,K+KK-1)/AVMO3(I,K+KK-1)
      VTMP3(I,K+KK-1)=HAF*(FAC1(I,K+KK-1)* &
        (SQRT(ONE+(FOUR*AO3RND(2)*AVMO3(I,K+KK-1))/ &
         FAC1(I,K+KK-1))-ONE))
      TO31D(I,K+KK-1)=EXP(HM1EZ*(VTMP3(I,K+KK-1) &
                         +SKO3R*AVVO2(I,K+KK-1)))
      OVER1D(I,K+KK-1)=EXP(HM1EZ*(SQRT(AB15WD*AVEPHI(I,K+KK-1))+ &
                  SKC1R*AVVO2(I,K+KK-1)))
      CO21(I,K+KK,K)=OVER1D(I,K+KK-1)*CO21(I,K+KK,K)
3221  CONTINUE
      DO 3223 KP=K+1,LP1
      DO 3223 I=MYIS,MYIE
      CO21(I,K,KP)=OVER1D(I,KP-1)*CO21(I,K,KP)
3223  CONTINUE

      DO 1804 I=MYIS,MYIE
      RLOG(I,K)=OVER1D(I,K)*CO2NBL(I,K)
1804  CONTINUE

      DO 3423 KP=K+1,LP1
      DO 3423 I=MYIS,MYIE
      FLX(I,K)=FLX(I,K)+(OSS(I,KP)*TO31D(I,KP-1) &
                        +SS2(I,KP)*CONT1D(I,KP-1) &
                        +CSS(I,KP)*CO21(I,KP,K) &
                        +DTC(I,KP)*EMISS(I,KP-1))*CLDFAC(I,KP,K)
3423  CONTINUE
      DO 3425 KP=K+1,LP1
      DO 3425 I=MYIS,MYIE
      FLX(I,KP)=FLX(I,KP)+(OSS(I,K)*TO31D(I,KP-1) &
                         +SS2(I,K)*CONT1D(I,KP-1) &
                         +CSS(I,K)*CO21(I,K,KP) &
                         +DTC(I,K)*EMISSB(I,KP-1))*CLDFAC(I,K,KP)
3425  CONTINUE
321   CONTINUE

      DO 821 I=MYIS,MYIE
      TPL(I,1)=TEMP(I,L)
      TPL(I,LP1)=HAF*(T(I,LP1)+TEMP(I,L))
      TPL(I,LLP1)=HAF*(T(I,L)+TEMP(I,L))
821   CONTINUE
      DO 823 K=2,L
      DO 823 I=MYIS,MYIE
      TPL(I,K)=T(I,K)
      TPL(I,K+L)=T(I,K)
823   CONTINUE



      DO 833 I=MYIS,MYIE
      AVEPHI(I,1)=VAR2(I,L)
      AVEPHI(I,2)=VAR2(I,L)+EMPL(I,L)
833   CONTINUE
      CALL E2SPEC(EMISS,AVEPHI,FXOSP,DTSP,                          &

                      H16E1,TEN,H28E1,HP1,                          &
                      ids,ide, jds,jde, kds,kde,                    &
                      ims,ime, jms,jme, kms,kme,                    &
                      its,ite, jts,jte, kts,kte                     )




           CALL E3V88(EMD,TPL,EMPL, &
                      TEN,HP1,H28E1,H16E1,  &
                      ids,ide, jds,jde, kds,kde,                    &
                      ims,ime, jms,jme, kms,kme,                    &
                      its,ite, jts,jte, kts,kte                     )



      DO 851 K=2,L
      DO 851 I=MYIS,MYIE
      EMISDG(I,K)=EMD(I,K+L)+EMD(I,K)
851   CONTINUE



      DO 861 I=MYIS,MYIE
      EMSPEC(I,1)=(EMD(I,1)*EMPL(I,1)-EMD(I,LP1)*EMPL(I,LP1))/ &
       EMX1(I) + QUARTR*(EMISS(I,1)+EMISS(I,2))
      EMISDG(I,LP1)=TWO*EMD(I,LP1)
      EMSPEC(I,2)=TWO*(EMD(I,1)*EMPL(I,1)-EMD(I,LLP1)*EMPL(I,LLP1))/ &
       EMX2(I)
861   CONTINUE
      DO 331 I=MYIS,MYIE
      FAC1(I,L)=BO3RND(2)*VAR4(I,L)/VAR3(I,L)
      VTMP3(I,L)=HAF*(FAC1(I,L)* &
          (SQRT(ONE+(FOUR*AO3RND(2)*VAR3(I,L))/FAC1(I,L))-ONE))
      TO31D(I,L)=EXP(HM1EZ*(VTMP3(I,L)+SKO3R*CNTVAL(I,L)))
      OVER1D(I,L)=EXP(HM1EZ*(SQRT(AB15WD*VAR2(I,L))+ &
                  SKC1R*CNTVAL(I,L)))
      CONT1D(I,L)=CNTTAU(I,L)*TOTEVV(I,LM1)
      RLOG(I,L)=OVER1D(I,L)*CO2NBL(I,L)
331   CONTINUE
      DO 618 K=1,L
      DO 618 I=MYIS,MYIE
      RLOG(I,K)=LOG(RLOG(I,K))
618   CONTINUE
      DO 601 K=1,LM1
      DO 601 I=MYIS,MYIE
      DELPR1(I,K+1)=DELP(I,K+1)*(PRESS(I,K+1)-P(I,K+1))
      ALP(I,LP1+K-1)=-SQRT(DELPR1(I,K+1))*RLOG(I,K+1)
601   CONTINUE
      DO 603 K=1,L
      DO 603 I=MYIS,MYIE
      DELPR2(I,K+1)=DELP(I,K)*(P(I,K+1)-PRESS(I,K))
      ALP(I,K)=-SQRT(DELPR2(I,K+1))*RLOG(I,K)
603   CONTINUE
      DO 625 I=MYIS,MYIE
      ALP(I,LL)=-RLOG(I,L)
      ALP(I,LLP1)=-RLOG(I,L)*SQRT(DELP(I,L)*(P(I,LP1)-PRESS(I,LM1)))
625   CONTINUE






      DO 631 K=1,LLP1
      DO 631 I=MYIS,MYIE
      C(I,K)=ALP(I,K)*(HMP66667+ALP(I,K)*(QUARTR+ALP(I,K)*HM6666M2))
631   CONTINUE
      DO 641 I=MYIS,MYIE
      CO21(I,LP1,LP1)=ONE+C(I,L)
      CO21(I,LP1,L)=ONE+(DELP2(I,L)*C(I,LL)-(PRESS(I,L)-P(I,L))* &
       C(I,LLM1))/(P(I,LP1)-PRESS(I,L))
      CO21(I,L,LP1)=ONE+((P(I,LP1)-PRESS(I,LM1))*C(I,LLP1)- &
       (P(I,LP1)-PRESS(I,L))*C(I,L))/(PRESS(I,L)-PRESS(I,LM1))
641   CONTINUE
      DO 643 K=2,L
      DO 643 I=MYIS,MYIE
      CO21(I,K,K)=ONE+HAF*(C(I,LM1+K)+C(I,K-1))
643   CONTINUE




      DO 651 K=1,LM1
      DO 651 I=MYIS,MYIE
      CSUB(I,K+1)=CNTVAL(I,K+1)*DELPR1(I,K+1)
      CSUB(I,LP1+K-1)=CNTVAL(I,K)*DELPR2(I,K+1)
651   CONTINUE

      DO 655 K=1,LLM2
      DO 655 I=MYIS,MYIE
      CSUB2(I,K+1)=SKO3R*CSUB(I,K+1)
      C(I,K+1)=CSUB(I,K+1)*(HMP5+CSUB(I,K+1)* &
                (HP166666-CSUB(I,K+1)*H41666M2))
      C2(I,K+1)=CSUB2(I,K+1)*(HMP5+CSUB2(I,K+1)* &
                 (HP166666-CSUB2(I,K+1)*H41666M2))
655   CONTINUE
      DO 661 I=MYIS,MYIE
      CONTDG(I,LP1)=1.+C(I,LLM1)
      TO3DG(I,LP1)=1.+C2(I,LLM1)
661   CONTINUE
      DO 663 K=2,L
      DO 663 I=MYIS,MYIE
      CONTDG(I,K)=ONE+HAF*(C(I,K)+C(I,LM1+K))
      TO3DG(I,K)=ONE+HAF*(C2(I,K)+C2(I,LM1+K))
663   CONTINUE



      DO 871 K=2,LP1
      DO 871 I=MYIS,MYIE
      FLX(I,K)=FLX(I,K)+(DTC(I,K)*EMISDG(I,K) &
                       +SS2(I,K)*CONTDG(I,K) &
                       +OSS(I,K)*TO3DG(I,K) &
                       +CSS(I,K)*CO21(I,K,K))*CLDFAC(I,K,K)
871   CONTINUE

      DO 873 I=MYIS,MYIE
      FLX(I,L)=FLX(I,L)+(CSS(I,LP1)*CO21(I,LP1,L) &
                        +DTC(I,LP1)*EMSPEC(I,2) &
                        +OSS(I,LP1)*TO31D(I,L) &
                        +SS2(I,LP1)*CONT1D(I,L))*CLDFAC(I,LP1,L)
      FLX(I,LP1)=FLX(I,LP1)+(CSS(I,L)*CO21(I,L,LP1) &
                            +OSS(I,L)*TO31D(I,L) &
                            +SS2(I,L)*CONT1D(I,L) &
                            +DTC(I,L)*EMSPEC(I,1))*CLDFAC(I,L,LP1)
873   CONTINUE





      DO 1101 K=1,L
      DO 1101 I=MYIS,MYIE
      HEATEM(I,K)=RADCON*(FLX(I,K+1)-FLX(I,K))*DELP(I,K)
1101  CONTINUE

      DO 1103 K=1,L
      DO 1103 I=MYIS,MYIE
      HEATRA(I,K)=HEATEM(I,K)-CTS(I,K)-CTSO3(I,K)+EXCTS(I,K)
1103  CONTINUE


      DO 1111 K=1,L
      DO 1111 I=MYIS,MYIE
      VSUM1(I,K)=HEATRA(I,K)*DELP2(I,K)*RADCON1
1111  CONTINUE
      DO 1115 I=MYIS,MYIE
      TOPFLX(I)=FLX1E1(I)+GXCTS(I)
      FLXNET(I,1)=TOPFLX(I)
1115  CONTINUE


      DO 1123 K=2,LP1
      DO 1123 I=MYIS,MYIE
      FLXNET(I,K)=FLXNET(I,K-1)+VSUM1(I,K-1)
1123  CONTINUE
      DO 1125 I=MYIS,MYIE
      GRNFLX(I)=FLXNET(I,LP1)
1125  CONTINUE






      ICNT=0
      DO 1301 I=MYIS,MYIE
      ICNT=ICNT+NCLDS(I)
1301  CONTINUE
      IF (ICNT.EQ.0) GO TO 6999

      KCLDS=NCLDS(MYIS)
      DO 2106 I=MYIS,MYIE
      KCLDS=MAX(NCLDS(I),KCLDS)
2106  CONTINUE





      DO 1361 KK=1,KCLDS
      KMIN=LP1
      KMAX=0
      DO 1362 I=MYIS,MYIE
        J1=KTOP(I,KK+1)

        J3=KBTM(I,KK+1)
        IF (J3.GT.J1) THEN
          PTOP(I)=P(I,J1)
          PBOT(I)=P(I,J3+1)
          FTOP(I)=FLXNET(I,J1)
          FBOT(I)=FLXNET(I,J3+1)

          DELPTC(I)=(FTOP(I)-FBOT(I))/(PTOP(I)-PBOT(I))
          KMIN=MIN(KMIN,J1)
          KMAX=MAX(KMAX,J3)
        ENDIF
1362  CONTINUE
      KMIN=KMIN+1


      DO 1365 K=KMIN,KMAX
      DO 1363 I=MYIS,MYIE

        IF(KTOP(I,KK+1).LT.K .AND. K.LE.KBTM(I,KK+1)) THEN
          Z1(I,K)=(P(I,K)-PTOP(I))*DELPTC(I)+FTOP(I)


          FLXNET(I,K)=Z1(I,K)
        ENDIF
1363  CONTINUE
1365  CONTINUE
1361  CONTINUE


















6001  CONTINUE
6999  CONTINUE


      DO 6101 K=1,L
      DO 6101 I=MYIS,MYIE
      HEATRA(I,K)=RADCON*(FLXNET(I,K+1)-FLXNET(I,K))*DELP(I,K)
6101  CONTINUE


  END SUBROUTINE FST88



  SUBROUTINE E1E290(G1,G2,G3,G4,G5,EMISS,FXOE1,DTE1,FXOE2,DTE2,      &
                       AVEPHI,TEMP,T,                                &

                       H16E1,TEN,HP1,H28E1,HAF,                      &
                       ids,ide, jds,jde, kds,kde,                    &
                       ims,ime, jms,jme, kms,kme,                    &
                       its,ite, jts,jte, kts,kte                     )

 IMPLICIT NONE

      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte
      REAL,INTENT(IN) :: H16E1,TEN,HP1,H28E1,HAF

      REAL,INTENT(OUT),DIMENSION(its:ite,kts:kte+1) :: G1,G4,G3,EMISS
      REAL,INTENT(IN),DIMENSION(its:ite,kts:kte+1) :: FXOE1,DTE1,FXOE2,DTE2
      REAL,INTENT(IN),DIMENSION(its:ite,kts:kte+1) :: AVEPHI,TEMP,T
      REAL,INTENT(OUT),DIMENSION(its:ite,kts:kte)   :: G2,G5


      REAL,DIMENSION(its:ite,kts:kte+1) :: TMP3,DU,FYO,WW1,WW2
      INTEGER,DIMENSION(its:ite,kts:kte*3+2)   :: IT1
      INTEGER,DIMENSION(its:ite,kts:kte+1) :: IVAL







      INTEGER :: K, I,KP,LLM2,J1,J3,KMAX,KMIN,KCLDS,ICNT,LLM1
      INTEGER :: L,LP1,LP2,LP3,LM1,LM2,LM3,MYIS,MYIE,LLP1,LL,KK,KLEN

      L=kte
      LP1=L+1;  LP2=L+2;  LP3=L+3; LLP1 = 2*L + 1
      LM1=L-1;  LM2=L-2;  LM3=L-3; LL = 2*L
      LLM2 = LL-2; LLM1=LL-1
      MYIS=its; MYIE=ite











      DO 1322 K=1,LP1
      DO 1322 I=MYIS,MYIE
      TMP3(I,K)=LOG10(AVEPHI(I,K))+H16E1
      FYO(I,K)=AINT(TMP3(I,K)*TEN)
      DU(I,K)=TMP3(I,K)-HP1*FYO(I,K)
      FYO(I,K)=H28E1*FYO(I,K)
      IVAL(I,K)=FYO(I,K)+FXOE2(I,K)
      EMISS(I,K)=T1(IVAL(I,K))+DU(I,K)*T2(IVAL(I,K)) &
                              +DTE2(I,K)*T4(IVAL(I,K))
1322  CONTINUE



      DO 1344 I=MYIS,MYIE
      EMISS(I,L)=HAF*(EMISS(I,L)+EMISS(I,LP1))
1344  CONTINUE

















      DO 208 I=MYIS,MYIE
      IT1(I,1)=FXOE1(I,1)
      WW1(I,1)=TEN-DTE1(I,1)
      WW2(I,1)=HP1
208   CONTINUE
      DO 209 K=1,L
      DO 209 I=MYIS,MYIE
      IT1(I,K+1)=FYO(I,K)+FXOE1(I,K+1)
      IT1(I,LP2+K-1)=FYO(I,K)+FXOE1(I,K)
      WW1(I,K+1)=TEN-DTE1(I,K+1)
      WW2(I,K+1)=HP1-DU(I,K)
209   CONTINUE
      DO 211 KP=1,L
      DO 211 I=MYIS,MYIE
      IT1(I,KP+LLP1)=FYO(I,KP)+FXOE1(I,1)
211   CONTINUE



      DO 230 I=MYIS,MYIE
      G1(I,1)=WW1(I,1)*WW2(I,1)*EM1V(IT1(I,1))+ &
              WW2(I,1)*DTE1(I,1)*EM1V(IT1(I,1)+1)
      G3(I,1)=G1(I,1)
230   CONTINUE
      DO 240 K=1,L
      DO 240 I=MYIS,MYIE
      G1(I,K+1)=WW1(I,K+1)*WW2(I,K+1)*EM1V(IT1(I,K+1))+ &
              WW2(I,K+1)*DTE1(I,K+1)*EM1V(IT1(I,K+1)+1)+ &
              WW1(I,K+1)*DU(I,K)*EM1V(IT1(I,K+1)+28)+ &
              DTE1(I,K+1)*DU(I,K)*EM1V(IT1(I,K+1)+29)
      G2(I,K)=WW1(I,K)*WW2(I,K+1)*EM1V(IT1(I,K+LP2-1))+ &
              WW2(I,K+1)*DTE1(I,K)*EM1V(IT1(I,K+LP2-1)+1)+ &
              WW1(I,K)*DU(I,K)*EM1V(IT1(I,K+LP2-1)+28)+ &
              DTE1(I,K)*DU(I,K)*EM1V(IT1(I,K+LP2-1)+29)
240   CONTINUE
      DO 241 KP=2,LP1
      DO 241 I=MYIS,MYIE
      G3(I,KP)=WW1(I,1)*WW2(I,KP)*EM1V(IT1(I,LL+KP))+ &
              WW2(I,KP)*DTE1(I,1)*EM1V(IT1(I,LL+KP)+1)+ &
              WW1(I,1)*DU(I,KP-1)*EM1V(IT1(I,LL+KP)+28)+ &
              DTE1(I,1)*DU(I,KP-1)*EM1V(IT1(I,LL+KP)+29)
241   CONTINUE

      DO 244 I=MYIS,MYIE
      G4(I,1)=WW1(I,1)*WW2(I,1)*EM1VW(IT1(I,1))+ &
              WW2(I,1)*DTE1(I,1)*EM1VW(IT1(I,1)+1)
244   CONTINUE
      DO 242 K=1,L
      DO 242 I=MYIS,MYIE
      G4(I,K+1)=WW1(I,K+1)*WW2(I,K+1)*EM1VW(IT1(I,K+1))+ &
              WW2(I,K+1)*DTE1(I,K+1)*EM1VW(IT1(I,K+1)+1)+ &
              WW1(I,K+1)*DU(I,K)*EM1VW(IT1(I,K+1)+28)+ &
              DTE1(I,K+1)*DU(I,K)*EM1VW(IT1(I,K+1)+29)
      G5(I,K)=WW1(I,K)*WW2(I,K+1)*EM1VW(IT1(I,K+LP2-1))+ &
              WW2(I,K+1)*DTE1(I,K)*EM1VW(IT1(I,K+LP2-1)+1)+ &
              WW1(I,K)*DU(I,K)*EM1VW(IT1(I,K+LP2-1)+28)+ &
              DTE1(I,K)*DU(I,K)*EM1VW(IT1(I,K+LP2-1)+29)
242   CONTINUE

  END SUBROUTINE E1E290



 SUBROUTINE SPA88(EXCTS,CTSO3,GXCTS,SORC,CSOUR,                      &
                       CLDFAC,TEMP,PRESS,VAR1,VAR2,                  &
                       P,DELP,DELP2,TOTVO2,TO3SP,TO3SPC,             &
                       CO2SP1,CO2SP2,CO2SP,                          &
                       APCM,BPCM,ATPCM,BTPCM,ACOMB,BCOMB,BETACM,     &
                       H25E2,ONE,H44194M2,H1P41819,HAF,HM1EZ,TWO,    &

                       RADCON,                                 &
                       ids,ide, jds,jde, kds,kde,                    &
                       ims,ime, jms,jme, kms,kme,                    &
                       its,ite, jts,jte, kts,kte                     )

 IMPLICIT NONE


      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte

      REAL,INTENT(IN) :: H25E2,ONE,H44194M2,H1P41819,HAF,HM1EZ,TWO, &
                         RADCON


      REAL,INTENT(IN),DIMENSION(its:ite,kts:kte+1) :: CSOUR
      REAL,INTENT(OUT),DIMENSION(its:ite,kts:kte)  :: CTSO3
      REAL,INTENT(OUT),DIMENSION(its:ite,kts:kte)  :: EXCTS
      REAL,INTENT(OUT),DIMENSION(its:ite)          :: GXCTS
      REAL,INTENT(IN),DIMENSION(its:ite,kts:kte+1,NBLY) :: SORC
      REAL,INTENT(IN),DIMENSION(its:ite,kts:kte+1,kts:kte+1) :: CLDFAC
      REAL,INTENT(IN), DIMENSION(its:ite,kts:kte+1) :: PRESS,TEMP

      REAL,INTENT(IN),DIMENSION(its:ite,kts:kte) :: VAR1,VAR2 
      REAL,INTENT(IN),DIMENSION(its:ite,kts:kte+1) :: P
      REAL,INTENT(IN),DIMENSION(its:ite,kts:kte)   :: DELP,DELP2,TO3SPC
      REAL,INTENT(IN),DIMENSION(its:ite,kts:kte+1) ::TOTVO2,TO3SP,CO2SP1,&
                                                     CO2SP2,CO2SP
      REAL,INTENT(IN),DIMENSION(NBLY) :: APCM,BPCM,ATPCM,BTPCM,ACOMB, &
                                         BCOMB,BETACM

      REAL,DIMENSION(its:ite,kts:kte+1) ::CTMP,CTMP2,CTMP3
      REAL,DIMENSION(its:ite,kts:kte)   ::X,Y,FAC1,FAC2,F,FF,AG,AGG, &
                                          PHITMP,PSITMP,TOPM,TOPPHI,TT

      INTEGER :: K, I,KP,LLM2,J1,J3,KMAX,KMIN,KCLDS,ICNT,LLM1
      INTEGER :: L,LP1,LP2,LP3,LM1,LM2,LM3,MYIS,MYIE,LLP1,LL,KK,KLEN

      L=kte
      LP1=L+1;  LP2=L+2;  LP3=L+3; LLP1 = 2*L + 1
      LM1=L-1;  LM2=L-2;  LM3=L-3; LL = 2*L
      LLM2 = LL-2; LLM1=LL-1
      MYIS=its; MYIE=ite



      DO 101 K=1,L
      DO 101 I=MYIS,MYIE
      X(I,K)=TEMP(I,K)-H25E2
      Y(I,K)=X(I,K)*X(I,K)
101   CONTINUE


      DO 345 I=MYIS,MYIE
      CTMP(I,1)=ONE
      CTMP2(I,1)=1.
      CTMP3(I,1)=1.
345   CONTINUE







      DO 301 K=1,L
      DO 301 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(1)*X(I,K)+BPCM(1)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(1)*X(I,K)+BTPCM(1)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
301   CONTINUE


      DO 315 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
315   CONTINUE
      DO 319 K=2,L
      DO 317 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
317   CONTINUE
319   CONTINUE

      DO 321 K=1,L
      DO 321 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(1)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(1)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*FAC1(I,K)/SQRT(1.+FAC2(I,K)))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
321   CONTINUE

      DO 353 K=1,L
      DO 353 I=MYIS,MYIE
      EXCTS(I,K)=SORC(I,K,1)*(CTMP(I,K+1)-CTMP(I,K))
353   CONTINUE

      DO 361 I=MYIS,MYIE
      GXCTS(I)=CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,1)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,1)-SORC(I,L,1)))
361   CONTINUE








      DO 401 K=1,L
      DO 401 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(2)*X(I,K)+BPCM(2)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(2)*X(I,K)+BTPCM(2)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
401   CONTINUE


      DO 415 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
415   CONTINUE
      DO 419 K=2,L
      DO 417 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
417   CONTINUE
419   CONTINUE

      DO 421 K=1,L
      DO 421 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(2)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(2)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*FAC1(I,K)/SQRT(1.+FAC2(I,K)))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
421   CONTINUE

      DO 453 K=1,L
      DO 453 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,2)* & 
                   (CTMP(I,K+1)-CTMP(I,K))
453   CONTINUE

      DO 461 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,2)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,2)-SORC(I,L,2)))
461   CONTINUE







      DO 501 K=1,L
      DO 501 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(3)*X(I,K)+BPCM(3)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(3)*X(I,K)+BTPCM(3)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
501   CONTINUE


      DO 515 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
515   CONTINUE
      DO 519 K=2,L
      DO 517 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
517   CONTINUE
519   CONTINUE

      DO 521 K=1,L
      DO 521 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(3)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(3)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*FAC1(I,K)/SQRT(1.+FAC2(I,K)))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
521   CONTINUE

      DO 553 K=1,L
      DO 553 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,3)* &
                   (CTMP(I,K+1)-CTMP(I,K))
553   CONTINUE

      DO 561 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,3)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,3)-SORC(I,L,3)))
561   CONTINUE







      DO 601 K=1,L
      DO 601 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(4)*X(I,K)+BPCM(4)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(4)*X(I,K)+BTPCM(4)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
601   CONTINUE


      DO 615 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
615   CONTINUE
      DO 619 K=2,L
      DO 617 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
617   CONTINUE
619   CONTINUE

      DO 621 K=1,L
      DO 621 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(4)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(4)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*FAC1(I,K)/SQRT(1.+FAC2(I,K)))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
621   CONTINUE

      DO 653 K=1,L
      DO 653 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,4)* &
                   (CTMP(I,K+1)-CTMP(I,K))
653   CONTINUE

      DO 661 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,4)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,4)-SORC(I,L,4)))
661   CONTINUE







      DO 701 K=1,L
      DO 701 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(5)*X(I,K)+BPCM(5)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(5)*X(I,K)+BTPCM(5)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
701   CONTINUE


      DO 715 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
715   CONTINUE
      DO 719 K=2,L
      DO 717 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
717   CONTINUE
719   CONTINUE

      DO 721 K=1,L
      DO 721 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(5)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(5)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*(FAC1(I,K)/SQRT(ONE+FAC2(I,K))+ &
                 BETACM(5)*TOTVO2(I,K+1)*SKO2D))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
721   CONTINUE

      DO 753 K=1,L
      DO 753 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,5)* &
                   (CTMP(I,K+1)-CTMP(I,K))
753   CONTINUE

      DO 761 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,5)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,5)-SORC(I,L,5)))
761   CONTINUE







      DO 801 K=1,L
      DO 801 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(6)*X(I,K)+BPCM(6)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(6)*X(I,K)+BTPCM(6)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
801   CONTINUE


      DO 815 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
815   CONTINUE
      DO 819 K=2,L
      DO 817 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
817   CONTINUE
819   CONTINUE

      DO 821 K=1,L
      DO 821 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(6)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(6)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*(FAC1(I,K)/SQRT(ONE+FAC2(I,K))+ &
                 BETACM(6)*TOTVO2(I,K+1)*SKO2D))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
821   CONTINUE

      DO 853 K=1,L
      DO 853 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,6)* &
                   (CTMP(I,K+1)-CTMP(I,K))
853   CONTINUE

      DO 861 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,6)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,6)-SORC(I,L,6)))
861   CONTINUE







      DO 901 K=1,L
      DO 901 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(7)*X(I,K)+BPCM(7)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(7)*X(I,K)+BTPCM(7)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
901   CONTINUE


      DO 915 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
915   CONTINUE
      DO 919 K=2,L
      DO 917 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
917   CONTINUE
919   CONTINUE

      DO 921 K=1,L
      DO 921 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(7)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(7)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*(FAC1(I,K)/SQRT(ONE+FAC2(I,K))+ &
                 BETACM(7)*TOTVO2(I,K+1)*SKO2D))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
921   CONTINUE

      DO 953 K=1,L
      DO 953 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,k,7)* &
                   (CTMP(I,K+1)-CTMP(I,K))
953   CONTINUE

      DO 961 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,7)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,7)-SORC(I,L,7)))
961   CONTINUE







      DO 1001 K=1,L
      DO 1001 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(8)*X(I,K)+BPCM(8)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(8)*X(I,K)+BTPCM(8)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
1001  CONTINUE


      DO 1015 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
1015  CONTINUE
      DO 1019 K=2,L
      DO 1017 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
1017  CONTINUE
1019  CONTINUE

      DO 1021 K=1,L
      DO 1021 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(8)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(8)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*(FAC1(I,K)/SQRT(ONE+FAC2(I,K))+ &
                 BETACM(8)*TOTVO2(I,K+1)*SKO2D))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
1021  CONTINUE

      DO 1053 K=1,L
      DO 1053 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,8)* &
                   (CTMP(I,K+1)-CTMP(I,K))
1053  CONTINUE

      DO 1061 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,8)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,8)-SORC(I,L,8)))
1061  CONTINUE







      DO 1101 K=1,L
      DO 1101 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(9)*X(I,K)+BPCM(9)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(9)*X(I,K)+BTPCM(9)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
1101  CONTINUE


      DO 1115 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
1115  CONTINUE
      DO 1119 K=2,L
      DO 1117 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
1117  CONTINUE
1119  CONTINUE

      DO 1121 K=1,L
      DO 1121 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(9)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(9)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*(FAC1(I,K)/SQRT(ONE+FAC2(I,K))+ &
                 BETACM(9)*TOTVO2(I,K+1)*SKO2D))*CO2SP1(I,K+1)
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
1121  CONTINUE

      DO 1153 K=1,L
      DO 1153 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,9)* &
                   (CTMP(I,K+1)-CTMP(I,K))
1153  CONTINUE

      DO 1161 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,9)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,9)-SORC(I,L,9)))
1161  CONTINUE







      DO 1201 K=1,L
      DO 1201 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(10)*X(I,K)+BPCM(10)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(10)*X(I,K)+BTPCM(10)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
1201  CONTINUE


      DO 1215 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
1215  CONTINUE
      DO 1219 K=2,L
      DO 1217 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
1217  CONTINUE
1219  CONTINUE

      DO 1221 K=1,L
      DO 1221 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(10)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(10)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*(FAC1(I,K)/SQRT(ONE+FAC2(I,K))+ &
                 BETACM(10)*TOTVO2(I,K+1)*SKO2D))*CO2SP2(I,K+1)
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
1221  CONTINUE

      DO 1253 K=1,L
      DO 1253 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,10)* &
                   (CTMP(I,K+1)-CTMP(I,K))
1253  CONTINUE

      DO 1261 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,10)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,10)-SORC(I,L,10)))
1261  CONTINUE







      DO 1301 K=1,L
      DO 1301 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(11)*X(I,K)+BPCM(11)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(11)*X(I,K)+BTPCM(11)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
1301  CONTINUE


      DO 1315 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
1315  CONTINUE
      DO 1319 K=2,L
      DO 1317 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
1317  CONTINUE
1319  CONTINUE

      DO 1321 K=1,L
      DO 1321 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(11)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(11)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*(FAC1(I,K)/SQRT(ONE+FAC2(I,K))+ &
                 BETACM(11)*TOTVO2(I,K+1)*SKO2D))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
1321  CONTINUE

      DO 1353 K=1,L
      DO 1353 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,11)* &
                   (CTMP(I,K+1)-CTMP(I,K))
1353  CONTINUE

      DO 1361 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,11)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,11)-SORC(I,L,11)))
1361  CONTINUE







      DO 1401 K=1,L
      DO 1401 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(12)*X(I,K)+BPCM(12)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(12)*X(I,K)+BTPCM(12)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
1401  CONTINUE


      DO 1415 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
1415  CONTINUE
      DO 1419 K=2,L
      DO 1417 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
1417  CONTINUE
1419  CONTINUE

      DO 1421 K=1,L
      DO 1421 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(12)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(12)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*(FAC1(I,K)/SQRT(ONE+FAC2(I,K))+ &
                 BETACM(12)*TOTVO2(I,K+1)*SKO2D))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
1421  CONTINUE

      DO 1453 K=1,L
      DO 1453 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,12)* &
                   (CTMP(I,K+1)-CTMP(I,K))
1453  CONTINUE

      DO 1461 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,12)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,12)-SORC(I,L,12)))
1461  CONTINUE







      DO 1501 K=1,L
      DO 1501 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(13)*X(I,K)+BPCM(13)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(13)*X(I,K)+BTPCM(13)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
1501  CONTINUE


      DO 1515 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
1515  CONTINUE
      DO 1519 K=2,L
      DO 1517 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
1517  CONTINUE
1519  CONTINUE

      DO 1521 K=1,L
      DO 1521 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(13)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(13)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*(FAC1(I,K)/SQRT(ONE+FAC2(I,K))+ &
                 BETACM(13)*TOTVO2(I,K+1)*SKO2D+TO3SPC(I,K)))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
1521  CONTINUE

      DO 1553 K=1,L
      DO 1553 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,13)* &
                   (CTMP(I,K+1)-CTMP(I,K))
1553  CONTINUE

      DO 1561 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,13)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,13)-SORC(I,L,13)))
1561  CONTINUE







      DO 1601 K=1,L
      DO 1601 I=MYIS,MYIE
      F(I,K)=H44194M2*(APCM(14)*X(I,K)+BPCM(14)*Y(I,K))
      FF(I,K)=H44194M2*(ATPCM(14)*X(I,K)+BTPCM(14)*Y(I,K))
      AG(I,K)=(H1P41819+F(I,K))*F(I,K)+ONE
      AGG(I,K)=(H1P41819+FF(I,K))*FF(I,K)+ONE
      PHITMP(I,K)=VAR1(I,K)*(((( AG(I,K)*AG(I,K))**2)**2)**2)
      PSITMP(I,K)=VAR2(I,K)*(((( AGG(I,K)*AGG(I,K))**2)**2)**2)
1601  CONTINUE


      DO 1615 I=MYIS,MYIE
      TOPM(I,1)=PHITMP(I,1)
      TOPPHI(I,1)=PSITMP(I,1)
1615  CONTINUE
      DO 1619 K=2,L
      DO 1617 I=MYIS,MYIE
      TOPM(I,K)=TOPM(I,K-1)+PHITMP(I,K)
      TOPPHI(I,K)=TOPPHI(I,K-1)+PSITMP(I,K)
1617  CONTINUE
1619  CONTINUE

      DO 1621 K=1,L
      DO 1621 I=MYIS,MYIE
      FAC1(I,K)=ACOMB(14)*TOPM(I,K)
      FAC2(I,K)=FAC1(I,K)*TOPM(I,K)/(BCOMB(14)*TOPPHI(I,K))
      TT(I,K)=EXP(HM1EZ*(FAC1(I,K)/SQRT(ONE+FAC2(I,K))+ &
                 BETACM(14)*TOTVO2(I,K+1)*SKO2D))
      CTMP(I,K+1)=TT(I,K)*CLDFAC(I,K+1,1)
1621  CONTINUE

      DO 1653 K=1,L
      DO 1653 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)+SORC(I,K,14)* &
                   (CTMP(I,K+1)-CTMP(I,K))
1653  CONTINUE

      DO 1661 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)+CLDFAC(I,LP1,1)*(TT(I,L)*SORC(I,L,14)+ &
         (HAF*DELP(I,L)*(TT(I,LM1)*(P(I,LP1)-PRESS(I,L)) + &
         TT(I,L)*(P(I,LP1)+PRESS(I,L)-TWO*P(I,L)))) * &
         (SORC(I,LP1,14)-SORC(I,L,14)))
1661  CONTINUE







      DO 1731 K=1,L
      DO 1731 I=MYIS,MYIE
      GXCTS(I)=GXCTS(I)-EXCTS(I,K)
1731  CONTINUE



      DO 1741 K=1,L
      DO 1741 I=MYIS,MYIE
      EXCTS(I,K)=EXCTS(I,K)*RADCON*DELP(I,K)
1741  CONTINUE





      DO 1711 K=1,L
      DO 1711 I=MYIS,MYIE
      CTMP2(I,K+1)=CO2SP(I,K+1)*CLDFAC(I,K+1,1)
      CTMP3(I,K+1)=TO3SP(I,K)*CLDFAC(I,K+1,1)
1711  CONTINUE
      DO 1701 K=1,L
      DO 1701 I=MYIS,MYIE
      CTSO3(I,K)=RADCON*DELP(I,K)* &
           (CSOUR(I,K)*(CTMP2(I,K+1)-CTMP2(I,K)) + &
            SORC(I,K,13)*(CTMP3(I,K+1)-CTMP3(I,K)))
1701  CONTINUE

 END SUBROUTINE SPA88


 SUBROUTINE E290(EMISSB,EMISS,AVEPHI,KLEN,FXOE2,DTE2, &

                       H16E1,HP1,H28E1,HAF,TEN,                      &
                       ids,ide, jds,jde, kds,kde,                    &
                       ims,ime, jms,jme, kms,kme,                    &
                       its,ite, jts,jte, kts,kte                     )

 IMPLICIT NONE

      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte
      INTEGER, INTENT(IN)        :: KLEN
      REAL, INTENT(IN) :: H16E1,HP1,H28E1,HAF ,TEN
      REAL, INTENT(OUT),DIMENSION(its:ite,kts:kte+1) :: EMISSB
      REAL, INTENT(IN ),DIMENSION(its:ite,kts:kte+1) :: AVEPHI,FXOE2,DTE2



      REAL, INTENT(INOUT), DIMENSION(its:ite,kts:kte+1) :: EMISS

      REAL, DIMENSION(its:ite,kts:kte+1) :: TMP3,DT,FYO,DU
      INTEGER, DIMENSION(its:ite,kts:kte+1) :: IVAL






      INTEGER :: K, I,KP,LLM2,J1,J3,KMAX,KMIN,KCLDS,ICNT,LLM1
      INTEGER :: L,LP1,LP2,LP3,LM1,LM2,LM3,MYIS,MYIE,LLP1,LL,KK

      L=kte
      LP1=L+1;  LP2=L+2;  LP3=L+3; LLP1 = 2*L + 1
      LM1=L-1;  LM2=L-2;  LM3=L-3; LL = 2*L
      LLM2 = LL-2; LLM1=LL-1
      MYIS=its; MYIE=ite











      DO 132 K=1,LP2-KLEN
      DO 132 I=MYIS,MYIE
      TMP3(I,K)=LOG10(AVEPHI(I,KLEN+K-1))+H16E1
      FYO(I,K)=AINT(TMP3(I,K)*TEN)
      DU(I,K)=TMP3(I,K)-HP1*FYO(I,K)
      FYO(I,K)=H28E1*FYO(I,K)
      IVAL(I,K)=FYO(I,K)+FXOE2(I,KLEN+K-1)
      EMISS(I,KLEN+K-1)=T1(IVAL(I,K))+DU(I,K)*T2(IVAL(I,K)) & 
                                 +DTE2(I,KLEN+K-1)*T4(IVAL(I,K))
132   CONTINUE


      DO 1344 I=MYIS,MYIE
      EMISS(I,L)=HAF*(EMISS(I,L)+EMISS(I,LP1))
1344  CONTINUE









      DO 142 K=1,LP1-KLEN
      DO 142 I=MYIS,MYIE
      DT(I,K)=DTE2(I,KLEN-1)
      IVAL(I,K)=FYO(I,K)+FXOE2(I,KLEN-1)
142   CONTINUE

      DO 234 K=1,LP1-KLEN
      DO 234 I=MYIS,MYIE
      EMISSB(I,KLEN+K-1)=T1(IVAL(I,K))+DU(I,K)*T2(IVAL(I,K)) &
                                      +DT(I,K)*T4(IVAL(I,K))
234   CONTINUE

 END SUBROUTINE E290



  SUBROUTINE E2SPEC(EMISS,AVEPHI,FXOSP,DTSP,                         &

                       H16E1,TEN,H28E1,HP1,                          &
                       ids,ide, jds,jde, kds,kde,                    &
                       ims,ime, jms,jme, kms,kme,                    &
                       its,ite, jts,jte, kts,kte                     )

 IMPLICIT NONE

      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte
      REAL,INTENT(IN ) :: H16E1,TEN,H28E1,HP1  
      REAL,INTENT(INOUT),DIMENSION(its:ite,kts:kte+1) :: EMISS
      REAL,INTENT(IN ),DIMENSION(its:ite,kts:kte+1) :: AVEPHI
      REAL,INTENT(IN ),DIMENSION(its:ite,2) :: FXOSP,DTSP







      INTEGER :: K,I,MYIS,MYIE

      REAL,    DIMENSION(its:ite,kts:kte+1) :: TMP3,FYO,DU
      INTEGER, DIMENSION(its:ite,kts:kte+1) :: IVAL

      MYIS=its
      MYIE=ite

      DO 132 K=1,2
      DO 132 I=MYIS,MYIE
      TMP3(I,K)=LOG10(AVEPHI(I,K))+H16E1
      FYO(I,K)=AINT(TMP3(I,K)*TEN)
      DU(I,K)=TMP3(I,K)-HP1*FYO(I,K)
      IVAL(I,K)=H28E1*FYO(I,K)+FXOSP(I,K)
      EMISS(I,K)=T1(IVAL(I,K))+DU(I,K)*T2(IVAL(I,K))+ &
                               DTSP(I,K)*T4(IVAL(I,K))
132   CONTINUE

  END SUBROUTINE E2SPEC




  SUBROUTINE E3V88(EMV,TV,AV, &
                       TEN,HP1,H28E1,H16E1,  &
                       ids,ide, jds,jde, kds,kde,                    &
                       ims,ime, jms,jme, kms,kme,                    &
                       its,ite, jts,jte, kts,kte                     )

 IMPLICIT NONE

      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte
      REAL,    INTENT(IN)  :: TEN,HP1,H28E1,H16E1 

      REAL, INTENT(OUT), DIMENSION(its:ite,kts:kte*2+1) :: EMV
      REAL, INTENT(IN),  DIMENSION(its:ite,kts:kte*2+1) :: TV,AV


      REAL,DIMENSION(its:ite,kts:kte*2+1) ::FXO,TMP3,DT,WW1,WW2,DU,&
                                            FYO




      INTEGER,DIMENSION(its:ite,kts:kte*2+1) ::IT

      INTEGER :: LLP1,I,K,MYIS,MYIE ,L
      L = kte
      LLP1 = 2*L + 1
      MYIS=its; MYIE=ite




      DO 203 K=1,LLP1
      DO 203 I=MYIS,MYIE
        FXO(I,K)=AINT(TV(I,K)*HP1)
        TMP3(I,K)=LOG10(AV(I,K))+H16E1
        DT(I,K)=TV(I,K)-TEN*FXO(I,K)
        FYO(I,K)=AINT(TMP3(I,K)*TEN)
        DU(I,K)=TMP3(I,K)-HP1*FYO(I,K)


        IT(I,K)=FXO(I,K)+FYO(I,K)*H28E1
        WW1(I,K)=TEN-DT(I,K)
        WW2(I,K)=HP1-DU(I,K)
        EMV(I,K)=WW1(I,K)*WW2(I,K)*EM3V(IT(I,K)-9)+ &
                 WW2(I,K)*DT(I,K)*EM3V(IT(I,K)-8)+ & 
                 WW1(I,K)*DU(I,K)*EM3V(IT(I,K)+19)+ & 
                 DT(I,K)*DU(I,K)*EM3V(IT(I,K)+20)
203   CONTINUE

  END SUBROUTINE E3V88


  SUBROUTINE SWR93(FSWC,HSWC,UFSWC,DFSWC,FSWL,HSWL,UFSWL,             &
                       DFSWL,                                         &
                       PRESS,COSZRO,TAUDAR,RH2O,RRCO2,SSOLAR,QO3,     &
                       NCLDS,KTOPSW,KBTMSW,CAMT,CRR,CTT,              &
                       ALVB,ALNB,ALVD,ALND,GDFVB,GDFNB,GDFVD,GDFND,   &

                       ABCFF,PWTS,                                    &
                       H35E1,H1224E3,ONE,ZERO,HAF,H69766E5,HP219,     &
                       HP816,RRAYAV,GINV,CFCO2,CFO3,                  &
                       TWO,H235M3,HP26,H129M2,H75826M4,H1036E2,       &
                       H1P082,HMP805,H1386E2,H658M2,H2118M2,H42M2,    &
                       H323M4,HM1EZ,DIFFCTR,O3DIFCTR,FIFTY,RADCON,    &
                       ids,ide, jds,jde, kds,kde,                     &
                       ims,ime, jms,jme, kms,kme,                     &
                       its,ite, jts,jte, kts,kte                      )

 IMPLICIT NONE

      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte
      REAL,INTENT(IN) :: RRCO2,SSOLAR
      REAL,INTENT(IN) :: H35E1,H1224E3,ONE,ZERO,HAF,H69766E5,HP219,HP816,RRAYAV,&
                         GINV,CFCO2,CFO3
      REAL,INTENT(IN) :: TWO,H235M3,HP26,H129M2,H75826M4,H1036E2  
      REAL,INTENT(IN) :: H1P082,HMP805,H1386E2,H658M2,H2118M2,H42M2,H323M4,HM1EZ
      REAL,INTENT(IN) :: DIFFCTR,O3DIFCTR,FIFTY,RADCON

      INTEGER, PARAMETER :: NB=12
      REAL,    INTENT(IN ),DIMENSION(its:ite,kts:kte+1) :: PRESS,CAMT
      REAL,    INTENT(IN ),DIMENSION(its:ite,kts:kte) :: RH2O,QO3
      REAL,    INTENT(IN ),DIMENSION(its:ite) :: COSZRO,TAUDAR,ALVB,ALVD,ALNB,ALND
      INTEGER, INTENT(IN ),DIMENSION(its:ite) :: NCLDS
      INTEGER, INTENT(IN ),DIMENSION(its:ite,kts:kte+1) ::KTOPSW,KBTMSW
      REAL, INTENT(IN ),DIMENSION(its:ite,NB,kts:kte+1) ::CRR,CTT
           
      REAL, INTENT(OUT),DIMENSION(its:ite,kts:kte+1) ::     &
                                       FSWC,HSWC,UFSWC,DFSWC,FSWL,HSWL,UFSWL,DFSWL
      REAL, INTENT(OUT),DIMENSION(its:ite) :: GDFVB,GDFVD,GDFNB,GDFND
      REAL, INTENT(IN), DIMENSION(NB) :: ABCFF,PWTS




      REAL, DIMENSION(its:ite,kts:kte*2+2) :: UCO2,UO3
      REAL, DIMENSION(its:ite,kts:kte+1)   :: TUCO2,TUO3,TDO3,TDCO2

      REAL, DIMENSION(its:ite,kts:kte*2+2) :: TCO2,TO3
      REAL, DIMENSION(its:ite,kts:kte+1) :: PP,DP,PR2,DU,DUCO2,DUO3,UD,TTD
      REAL, DIMENSION(its:ite,kts:kte+1) :: UDCO2,UDO3,UR,URCO2,URO3,TTU
      REAL, DIMENSION(its:ite,kts:kte+1) :: DFN,UFN
      REAL, DIMENSION(its:ite,kts:kte+1) :: XAMT,FF,FFCO2,FFO3,CR,CT
      REAL, DIMENSION(its:ite,kts:kte+1) :: PPTOP,DPCLD,TTDB1,TTUB1
      REAL, DIMENSION(its:ite,kts:kte+1) :: TDCL1,TUCL1,TDCL2,DFNTRN,  &
                                            UFNTRN,TCLU,TCLD,ALFA,ALFAU, &
                                            UFNCLU,DFNCLU

      REAL, DIMENSION(its:ite,NB) :: DFNTOP
      REAL, DIMENSION(its:ite) :: SECZ,TMP1,RRAY,REFL,REFL2,CCMAX



















      INTEGER :: K,I,KP,N,IP,MYIS1,KCLDS,NNCLDS,JTOP,KK,J2,J3,J1
      INTEGER :: L,LP1,LP2,LP3,LM1,LM2,LM3,MYIS,MYIE,LLP1,LL
      REAL    :: DENOM,HTEMP,TEMPF,TEMPG

      L=kte
      LP1=L+1;  LP2=L+2;  LP3=L+3; LLP1 = 2*L + 1
      LM1=L-1;  LM2=L-2;  LM3=L-3; LL = 2*L
      MYIS=its; MYIE=ite
      MYIS1=MYIS+1    

      DO 100 I=MYIS,MYIE
        SECZ(I) = H35E1/SQRT(H1224E3*COSZRO(I)*COSZRO(I)+ONE)
        PP(I,1)   = ZERO
        PP(I,LP1) = PRESS(I,LP1)
        TMP1(I)  = ONE/PRESS(I,LP1)
100   CONTINUE
      DO 110 K=1,LM1
      DO 110 I=MYIS,MYIE
        PP(I,K+1) = HAF*(PRESS(I,K+1)+PRESS(I,K))
110   CONTINUE
      DO 120 K=1,L
      DO 120 I=MYIS,MYIE
        DP (I,K) = PP(I,K+1)-PP(I,K)
        PR2(I,K) = HAF*(PP(I,K)+PP(I,K+1))
120   CONTINUE
      DO 130 K=1,L
      DO 130 I=MYIS,MYIE
        PR2(I,K) = PR2(I,K)*TMP1(I)
130   CONTINUE

      DO 140 N=1,NB
      DO 140 IP=MYIS,MYIE
        DFNTOP(IP,N) = SSOLAR*H69766E5*COSZRO(IP)*TAUDAR(IP)*PWTS(N)
140   CONTINUE


      DO 150 I=MYIS,MYIE
        RRAY(I) = HP219/(ONE+HP816*COSZRO(I))
        REFL(I) = RRAY(I) + (ONE-RRAY(I))*(ONE-RRAYAV)*ALVB(I)/ &
                  (ONE-ALVD(I)*RRAYAV)
150   CONTINUE
      DO 155 I=MYIS,MYIE
        RRAY(I) = 0.104/(ONE+4.8*COSZRO(I))
        REFL2(I)= RRAY(I) + (ONE-RRAY(I))*(ONE-0.093)*ALVB(I)/ &
                  (ONE-ALVD(I)*0.093)
155   CONTINUE



      DO 160 K=1,L
      DO 160 I=MYIS,MYIE
        DU   (I,K) = GINV*RH2O(I,K)*DP(I,K)*PR2(I,K)
        DUCO2(I,K) = (RRCO2*GINV*CFCO2)*DP(I,K)*PR2(I,K)
        DUO3 (I,K) = (GINV*CFO3)*QO3(I,K)*DP(I,K)
160   CONTINUE







      DO 200 IP=MYIS,MYIE
        UD   (IP,1) = ZERO
        UDCO2(IP,1) = ZERO
        UDO3 (IP,1) = ZERO

        UO3  (IP,1) = UDO3 (IP,1)
        UCO2 (IP,1) = UDCO2(IP,1)

200   CONTINUE
      DO 210 K=2,LP1
      DO 210 I=MYIS,MYIE
        UD   (I,K) = UD   (I,K-1)+DU   (I,K-1)*SECZ(I)
        UDCO2(I,K) = UDCO2(I,K-1)+DUCO2(I,K-1)*SECZ(I)
        UDO3 (I,K) = UDO3 (I,K-1)+DUO3 (I,K-1)*SECZ(I)

        UO3  (I,K) = UDO3 (I,K)
        UCO2 (I,K) = UDCO2(I,K)

210   CONTINUE
      DO 220 IP=MYIS,MYIE
        UR   (IP,LP1) = UD   (IP,LP1)
        URCO2(IP,LP1) = UDCO2(IP,LP1)
        URO3 (IP,LP1) = UDO3 (IP,LP1)

        UO3  (IP,LP1+LP1) = URO3 (IP,LP1) 
        UCO2 (IP,LP1+LP1) = URCO2(IP,LP1)

220   CONTINUE
      DO 230 K=L,1,-1
      DO 230 IP=MYIS,MYIE
        UR   (IP,K) = UR   (IP,K+1)+DU   (IP,K)*DIFFCTR
        URCO2(IP,K) = URCO2(IP,K+1)+DUCO2(IP,K)*DIFFCTR
        URO3 (IP,K) = URO3 (IP,K+1)+DUO3 (IP,K)*O3DIFCTR

        UO3 (IP,LP1+K) = URO3 (IP,K)
        UCO2(IP,LP1+K) = URCO2(IP,K)

230   CONTINUE







      DO 240 K=1,LL
      DO 240 I=MYIS,MYIE
       TCO2(I,K+1)=ONE-TWO*(H235M3*EXP(HP26*LOG(UCO2(I,K+1)+H129M2)) &
                             -H75826M4)
240   CONTINUE


      DO 241 K=1,L
      DO 241 I=MYIS,MYIE
        TDCO2(I,K+1)=TCO2(I,K+1)
241   CONTINUE
      DO 242 K=1,L
      DO 242 I=MYIS,MYIE
        TUCO2(I,K)=TCO2(I,LP1+K)
242   CONTINUE





      HTEMP = H1036E2*H1036E2*H1036E2
      DO 250 K=1,LL
      DO 250 I=MYIS,MYIE
        TO3(I,K+1)=ONE-TWO*UO3(I,K+1)* &
                  (H1P082*EXP(HMP805*LOG(ONE+H1386E2*UO3(I,K+1)))+ &
                  H658M2/(ONE+HTEMP*UO3(I,K+1)*UO3(I,K+1)*UO3(I,K+1))+ &
                  H2118M2/(ONE+UO3(I,K+1)*(H42M2+H323M4*UO3(I,K+1))))
250   CONTINUE


      DO 251 K=1,L
      DO 251 I=MYIS,MYIE
        TDO3(I,K+1)=TO3(I,K+1)
251   CONTINUE
      DO 252 K=1,L
      DO 252 I=MYIS,MYIE
        TUO3(I,K)=TO3(I,LP1+K)
252   CONTINUE





      DO 260 K=1,L
      DO 260 I=MYIS,MYIE
        TTD(I,K+1) = EXP(HM1EZ*MIN(FIFTY,ABCFF(1)*UD(I,K+1)))
        TTU(I,K) = EXP(HM1EZ*MIN(FIFTY,ABCFF(1)*UR(I,K)))
        DFN(I,K+1) = TTD(I,K+1)*TDO3(I,K+1)
        UFN(I,K) = TTU(I,K)*TUO3(I,K)
260   CONTINUE
      DO 270 I=MYIS,MYIE
        DFN(I,1)   = ONE
        UFN(I,LP1) = DFN(I,LP1)
270   CONTINUE



      DO 280  K=1,LP1
      DO 280  I=MYIS,MYIE
        DFSWL(I,K) =         DFN(I,K)*DFNTOP(I,1)
        UFSWL(I,K) = REFL(I)*UFN(I,K)*DFNTOP(I,1)
280   CONTINUE
      DO 285 I=MYIS,MYIE
        GDFVB(I) = DFSWL(I,LP1)*EXP(-0.15746*SECZ(I))
        GDFVD(I) = ((ONE-REFL2(I))*DFSWL(I,LP1) - &
                    (ONE-ALVB(I)) *GDFVB(I)) / (ONE-ALVD(I))
        GDFNB(I) = ZERO
        GDFND(I) = ZERO
285   CONTINUE




      DO 350 N=2,NB
        IF (N.EQ.2) THEN



          DO 290 K=1,L
          DO 290 I=MYIS,MYIE
            DFN(I,K+1) = TTD(I,K+1)*TDCO2(I,K+1)
            UFN(I,K) = TTU(I,K)*TUCO2(I,K)
290       CONTINUE
        ELSE



          DO 300 K=1,L
          DO 300 I=MYIS,MYIE
            DFN(I,K+1)=EXP(HM1EZ*MIN(FIFTY,ABCFF(N)*UD(I,K+1))) &
                       *TDCO2(I,K+1)
            UFN(I,K)=EXP(HM1EZ*MIN(FIFTY,ABCFF(N)*UR(I,K))) &
                     *TUCO2(I,K)
300       CONTINUE
        ENDIF


        DO 310 I=MYIS,MYIE
          DFN(I,1)   = ONE
          UFN(I,LP1) = DFN(I,LP1)
310     CONTINUE


        DO 320 K=1,LP1
        DO 320 I=MYIS,MYIE
          DFSWL(I,K) = DFSWL(I,K) +         DFN(I,K)*DFNTOP(I,N)
          UFSWL(I,K) = UFSWL(I,K) + ALNB(I)*UFN(I,K)*DFNTOP(I,N)
320     CONTINUE
        DO 330 I=MYIS,MYIE
          GDFNB(I) = GDFNB(I) + DFN(I,LP1)*DFNTOP(I,N)
330     CONTINUE
350   CONTINUE
      DO 360 K=1,LP1
      DO 360 I=MYIS,MYIE
        FSWL(I,K) = UFSWL(I,K)-DFSWL(I,K)
360   CONTINUE
      DO 370 K=1,L
      DO 370 I=MYIS,MYIE
        HSWL(I,K)=RADCON*(FSWL(I,K+1)-FSWL(I,K))/DP(I,K)
370   CONTINUE





      KCLDS=NCLDS(MYIS)
      DO 400 I=MYIS1,MYIE
        KCLDS=MAX(NCLDS(I),KCLDS)
400   CONTINUE
        DO 410 K=1,LP1
        DO 410 I=MYIS,MYIE
          DFSWC(I,K) = DFSWL(I,K)
          UFSWC(I,K) = UFSWL(I,K)
          FSWC (I,K) = FSWL (I,K)
410     CONTINUE
        DO 420 K=1,L
        DO 420 I=MYIS,MYIE
          HSWC(I,K) = HSWL(I,K)
420     CONTINUE

      IF (KCLDS .EQ. 0)  RETURN

      DO 430 K=1,LP1
      DO 430 I=MYIS,MYIE
        XAMT(I,K) = CAMT(I,K)
430   CONTINUE
      DO 470 I=MYIS,MYIE
        NNCLDS   = NCLDS(I)
        CCMAX(I) = ZERO
        IF (NNCLDS .LE. 0) GO TO 470
        CCMAX(I) = ONE
        DO 450 K=1,NNCLDS
          CCMAX(I) = CCMAX(I) * (ONE - CAMT(I,K+1))
450     CONTINUE
        CCMAX(I) = ONE - CCMAX(I)
        IF (CCMAX(I) .GT. ZERO) THEN
          DO 460 K=1,NNCLDS
            XAMT(I,K+1) = CAMT(I,K+1)/CCMAX(I)
460       CONTINUE
        END IF
470   CONTINUE
      DO 480 K=1,LP1
      DO 480 I=MYIS,MYIE
        FF   (I,K) = DIFFCTR
        FFCO2(I,K) = DIFFCTR
        FFO3 (I,K) = O3DIFCTR
480   CONTINUE
      DO 490 IP=MYIS,MYIE
        JTOP = KTOPSW(IP,NCLDS(IP)+1)
      DO 490 K=1,JTOP
        FF   (IP,K) = SECZ(IP)
        FFCO2(IP,K) = SECZ(IP)
        FFO3 (IP,K) = SECZ(IP)
490   CONTINUE
      DO 500 I=MYIS,MYIE
        RRAY(I) = HP219/(ONE+HP816*COSZRO(I))
        REFL(I) = RRAY(I) + (ONE-RRAY(I))*(ONE-RRAYAV)*ALVD(I)/ &
                  (ONE-ALVD(I)*RRAYAV)
500   CONTINUE
      DO 510 IP=MYIS,MYIE
        UD   (IP,1) = ZERO
        UDCO2(IP,1) = ZERO
        UDO3 (IP,1) = ZERO

        UO3  (IP,1) = UDO3 (IP,1)
        UCO2 (IP,1) = UDCO2(IP,1)

510   CONTINUE
      DO 520 K=2,LP1
      DO 520 I=MYIS,MYIE
        UD   (I,K) = UD   (I,K-1)+DU   (I,K-1)*FF   (I,K)
        UDCO2(I,K) = UDCO2(I,K-1)+DUCO2(I,K-1)*FFCO2(I,K)
        UDO3 (I,K) = UDO3 (I,K-1)+DUO3 (I,K-1)*FFO3 (I,K)

        UO3 (I,K)  = UDO3 (I,K)
        UCO2(I,K)  = UDCO2(I,K)

520   CONTINUE
      DO 530 IP=MYIS,MYIE
        UR   (IP,LP1) = UD   (IP,LP1)
        URCO2(IP,LP1) = UDCO2(IP,LP1)
        URO3 (IP,LP1) = UDO3 (IP,LP1)

        UO3  (IP,LP1+LP1) = URO3 (IP,LP1)
        UCO2 (IP,LP1+LP1) = URCO2(IP,LP1)

530   CONTINUE
      DO 540 K=L,1,-1
      DO 540 IP=MYIS,MYIE
        UR   (IP,K) = UR   (IP,K+1)+DU   (IP,K)*DIFFCTR
        URCO2(IP,K) = URCO2(IP,K+1)+DUCO2(IP,K)*DIFFCTR
        URO3 (IP,K) = URO3 (IP,K+1)+DUO3 (IP,K)*O3DIFCTR

        UO3 (IP,LP1+K) = URO3 (IP,K)
        UCO2(IP,LP1+K) = URCO2(IP,K)

540   CONTINUE
      DO 550 K=1,LL
      DO 550 I=MYIS,MYIE
        TCO2(I,K+1)=ONE-TWO*(H235M3*EXP(HP26*LOG(UCO2(I,K+1)+H129M2)) &
                              -H75826M4)
550   CONTINUE

      DO 551 K=1,L
      DO 551 I=MYIS,MYIE
        TDCO2(I,K+1)=TCO2(I,K+1)
551   CONTINUE
      DO 552 K=1,L
      DO 552 I=MYIS,MYIE
        TUCO2(I,K)=TCO2(I,LP1+K)
552   CONTINUE

      DO 560 K=1,LL
      DO 560 I=MYIS,MYIE
        TO3(I,K+1)=ONE-TWO*UO3(I,K+1)* &
                 (H1P082*EXP(HMP805*LOG(ONE+H1386E2*UO3(I,K+1)))+ &
                H658M2/(ONE+HTEMP*UO3(I,K+1)*UO3(I,K+1)*UO3(I,K+1))+ &
                H2118M2/(ONE+UO3(I,K+1)*(H42M2+H323M4*UO3(I,K+1))))
560   CONTINUE

      DO 561 K=1,L
      DO 561 I=MYIS,MYIE
        TDO3(I,K+1)=TO3(I,K+1)
561   CONTINUE
      DO 562 K=1,L
      DO 562 I=MYIS,MYIE
        TUO3(I,K)=TO3(I,LP1+K)
562   CONTINUE





      DO 570 I=MYIS,MYIE
        CR(I,1) = REFL(I)
570   CONTINUE




      DO 581 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 581
      DO 580 KK=2,KCLDS+1
        CR(I,KK) = CRR(I,1,KK)*XAMT(I,KK)
        CT(I,KK) = ONE - (ONE-CTT(I,1,KK))*XAMT(I,KK)
580   CONTINUE
581   CONTINUE




      DO 591 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 591
      DO 590 KK=1,KCLDS
        IF ((KBTMSW(I,KK+1)-1).GT.KTOPSW(I,KK+1)) THEN
           PPTOP(I,KK)=PP(I,KTOPSW(I,KK+1))
           DPCLD(I,KK)=ONE/(PPTOP(I,KK)-PP(I,KBTMSW(I,KK+1)))
        ENDIF
590   CONTINUE
591   CONTINUE
      DO 600 K=1,L
      DO 600 I=MYIS,MYIE
        TTDB1(I,K+1) = EXP(HM1EZ*MIN(FIFTY,ABCFF(1)*UD(I,K+1)))
        TTUB1(I,K) = EXP(HM1EZ*MIN(FIFTY,ABCFF(1)*UR(I,K)))
        TTD  (I,K+1) = TTDB1(I,K+1)*TDO3(I,K+1)
        TTU  (I,K) = TTUB1(I,K)*TUO3(I,K)
600   CONTINUE
      DO 610 I=MYIS,MYIE
        TTD(I,1)   = ONE
        TTU(I,LP1) = TTD(I,LP1)
610   CONTINUE










      DO 620 I=MYIS,MYIE
        TDCL1 (I,1) = TTD(I,LP1)
        TUCL1 (I,1) = TTU(I,LP1)
        TDCL2 (I,1) = TDCL1(I,1)
        DFNTRN(I,1) = ONE/TDCL1(I,1)
        UFNTRN(I,1) = DFNTRN(I,1)
620   CONTINUE
      DO 631 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 631
      DO 630 KK=2,KCLDS+1
        TDCL1(I,KK) = TTD(I,KTOPSW(I,KK))
        TUCL1(I,KK) = TTU(I,KTOPSW(I,KK))
        TDCL2(I,KK) = TTD(I,KBTMSW(I,KK))
630   CONTINUE
631   CONTINUE

      DO 641 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 641

      DO 640 KK=2,KCLDS+1
        DFNTRN(I,KK) = ONE/TDCL1(I,KK)
        UFNTRN(I,KK) = ONE/TUCL1(I,KK)
640   CONTINUE
641   CONTINUE





      DO 651 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 651
      DO 650 KK=1,KCLDS
        TCLU(I,KK) = TDCL1(I,KK)*DFNTRN(I,KK+1)*CT(I,KK+1)
        TCLD(I,KK) = TDCL1(I,KK)/TDCL2(I,KK+1)
650   CONTINUE
651   CONTINUE





      DO 660 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 660
        ALFA (I,1)=CR(I,1)
        ALFAU(I,1)=ZERO
660   CONTINUE

      DO 671 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 671
      DO 670 KK=2,KCLDS+1
        ALFAU(I,KK)= TCLU(I,KK-1)*TCLU(I,KK-1)*ALFA(I,KK-1)/ &
              (ONE - TCLD(I,KK-1)*TCLD(I,KK-1)*ALFA(I,KK-1)*CR(I,KK))
        ALFA (I,KK)= ALFAU(I,KK)+CR(I,KK)
670   CONTINUE
671   CONTINUE







      DO 680 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 680
        UFNCLU(I,KCLDS+1) = ALFA(I,KCLDS+1)*TDCL1(I,KCLDS+1)
        DFNCLU(I,KCLDS+1) = TDCL1(I,KCLDS+1)
680   CONTINUE


      DO 691 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 691
      DO 690 KK=KCLDS,1,-1
        UFNCLU(I,KK) = UFNCLU(I,KK+1)*ALFAU(I,KK+1)/(ALFA(I,KK+1)* &
                       TCLU(I,KK))
        DFNCLU(I,KK) = UFNCLU(I,KK)/ALFA(I,KK)
690   CONTINUE
691   CONTINUE
      DO 701 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 701
      DO 700 KK=1,KCLDS+1
        UFNTRN(I,KK) = UFNCLU(I,KK)*UFNTRN(I,KK)
        DFNTRN(I,KK) = DFNCLU(I,KK)*DFNTRN(I,KK)
700   CONTINUE
701   CONTINUE

      DO 720 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 720
        J2=KBTMSW(I,2)
        DO 710 K=J2,LP1
          UFN(I,K) = UFNTRN(I,1)*TTU(I,K)
          DFN(I,K) = DFNTRN(I,1)*TTD(I,K)
710     CONTINUE
720   CONTINUE

      DO 760 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 760
      DO 755 KK=2,KCLDS+1
        J1=KTOPSW(I,KK)
        J2=KBTMSW(I,KK+1)
        IF (J1.EQ.1) GO TO 755
        DO 730 K=J2,J1
          UFN(I,K) = UFNTRN(I,KK)*TTU(I,K)
          DFN(I,K) = DFNTRN(I,KK)*TTD(I,K)
730     CONTINUE



        J3=KBTMSW(I,KK)
        IF ((J3-J1).GT.1) THEN
          TEMPF = (UFNCLU(I,KK)-UFN(I,J3))*DPCLD(I,KK-1)
          TEMPG = (DFNCLU(I,KK)-DFN(I,J3))*DPCLD(I,KK-1)
          DO 740 K=J1+1,J3-1
            UFN(I,K) = UFNCLU(I,KK)+TEMPF*(PP(I,K)-PPTOP(I,KK-1))
            DFN(I,K) = DFNCLU(I,KK)+TEMPG*(PP(I,K)-PPTOP(I,KK-1))
740       CONTINUE
        ENDIF
755   CONTINUE
760   CONTINUE
      DO 770 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 770
      DO 771 K=1,LP1
        DFSWC(I,K) = DFN(I,K)*DFNTOP(I,1)
        UFSWC(I,K) = UFN(I,K)*DFNTOP(I,1)
771   CONTINUE
770   CONTINUE
      DO 780 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 780
        TMP1(I) = ONE - CCMAX(I)
        GDFVB(I) = TMP1(I)*GDFVB(I)
        GDFNB(I) = TMP1(I)*GDFNB(I)
        GDFVD(I) = TMP1(I)*GDFVD(I) + CCMAX(I)*DFSWC(I,LP1)
780   CONTINUE





      DO 1000 N=2,NB

        DO 791 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 791
        DO 790 K=1,KCLDS+1
          CR(I,K) = CRR(I,N,K)*XAMT(I,K)
          CT(I,K) = ONE - (ONE-CTT(I,N,K))*XAMT(I,K)
790     CONTINUE
791     CONTINUE

        IF (N.EQ.2) THEN


          DO 800 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 800
        DO 801 KK=2,LP1
            TTD(I,KK) = TTDB1(I,KK)*TDCO2(I,KK)
801     CONTINUE
        DO 802 KK=1,L
            TTU(I,KK) = TTUB1(I,KK)*TUCO2(I,KK)
802     CONTINUE
800       CONTINUE
        ELSE
          DO 810 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 810
        DO 811 KK=2,LP1
            TTD(I,KK) = EXP(HM1EZ*MIN(FIFTY,ABCFF(N)*UD(I,KK))) &
                     * TDCO2(I,KK)
811     CONTINUE
        DO 812 KK=1,L
            TTU(I,KK) = EXP(HM1EZ*MIN(FIFTY,ABCFF(N)*UR(I,KK))) &
                     * TUCO2(I,KK)
812     CONTINUE
810       CONTINUE
        ENDIF


        DO 820 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 820
          TTU(I,LP1) = TTD(I,LP1)
          TTD(I,1)   = ONE
820     CONTINUE










        DO 830 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 830
          TDCL1 (I,1) = TTD(I,LP1)
          TUCL1 (I,1) = TTU(I,LP1)
          TDCL2 (I,1) = TDCL1(I,1)
          DFNTRN(I,1) = ONE/TDCL1(I,1)
          UFNTRN(I,1) = DFNTRN(I,1)
830     CONTINUE
        DO 841 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 841
        DO 840 KK=2,KCLDS+1
          TDCL1(I,KK) = TTD(I,KTOPSW(I,KK))
          TUCL1(I,KK) = TTU(I,KTOPSW(I,KK))
          TDCL2(I,KK) = TTD(I,KBTMSW(I,KK))
840     CONTINUE
841     CONTINUE
        DO 851 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 851
        DO 850 KK=2,KCLDS+1
          DFNTRN(I,KK) = ONE/TDCL1(I,KK)
          UFNTRN(I,KK) = ONE/TUCL1(I,KK)
850     CONTINUE
851     CONTINUE
        DO 861 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 861
        DO 860 KK=1,KCLDS
          TCLU(I,KK) = TDCL1(I,KK)*DFNTRN(I,KK+1)*CT(I,KK+1)
          TCLD(I,KK) = TDCL1(I,KK)/TDCL2(I,KK+1)
860     CONTINUE
861     CONTINUE




        DO 870 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 870
          ALFA (I,1) = CR(I,1)
          ALFAU(I,1) = ZERO
870     CONTINUE

        DO 881 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 881
        DO 880 KK=2,KCLDS+1
          ALFAU(I,KK) = TCLU(I,KK-1)*TCLU(I,KK-1)*ALFA(I,KK-1)/(ONE - &
                   TCLD(I,KK-1)*TCLD(I,KK-1)*ALFA(I,KK-1)*CR(I,KK))
          ALFA (I,KK) = ALFAU(I,KK)+CR(I,KK)
880     CONTINUE
881     CONTINUE







        DO 890 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 890
          UFNCLU(I,KCLDS+1) = ALFA(I,KCLDS+1)*TDCL1(I,KCLDS+1)
          DFNCLU(I,KCLDS+1) = TDCL1(I,KCLDS+1)
890     CONTINUE
        DO 901 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 901
        DO 900 KK=KCLDS,1,-1



        DENOM=ALFA(I,KK+1)*TCLU(I,KK)
        IF(DENOM.GT.RTHRESH)THEN
          UFNCLU(I,KK)=UFNCLU(I,KK+1)*ALFAU(I,KK+1)/DENOM
        ELSE
          UFNCLU(I,KK)=0.
        ENDIF
        IF(ALFA(I,KK).GT.RTHRESH)THEN
          DFNCLU(I,KK)=UFNCLU(I,KK)/ALFA(I,KK)
        ELSE
          DFNCLU(I,KK)=0.
        ENDIF
900     CONTINUE
901     CONTINUE

        DO 911 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 911
        DO 910 KK=1,KCLDS+1
          UFNTRN(I,KK) = UFNCLU(I,KK)*UFNTRN(I,KK)
          DFNTRN(I,KK) = DFNCLU(I,KK)*DFNTRN(I,KK)
910     CONTINUE
911     CONTINUE
        DO 930 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 930
          J2=KBTMSW(I,2)
          DO 920 K=J2,LP1
            UFN(I,K) = UFNTRN(I,1)*TTU(I,K)
            DFN(I,K) = DFNTRN(I,1)*TTD(I,K)
920       CONTINUE
930     CONTINUE
        DO 970  I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 970
        DO 965  KK=2,KCLDS+1
          J1 = KTOPSW(I,KK)
          J2 = KBTMSW(I,KK+1)
          IF (J1.EQ.1) GO TO 965
          DO 940 K=J2,J1
            UFN(I,K) = UFNTRN(I,KK)*TTU(I,K)
            DFN(I,K) = DFNTRN(I,KK)*TTD(I,K)
940       CONTINUE
          J3 = KBTMSW(I,KK)
          IF ((J3-J1).GT.1) THEN
            TEMPF = (UFNCLU(I,KK)-UFN(I,J3))*DPCLD(I,KK-1)
            TEMPG = (DFNCLU(I,KK)-DFN(I,J3))*DPCLD(I,KK-1)
            DO 950 K=J1+1,J3-1
              UFN(I,K) = UFNCLU(I,KK)+TEMPF*(PP(I,K)-PPTOP(I,KK-1))
              DFN(I,K) = DFNCLU(I,KK)+TEMPG*(PP(I,K)-PPTOP(I,KK-1))
950         CONTINUE
          ENDIF
965     CONTINUE
970     CONTINUE
        DO 980 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 980
        DO 981 K=1,LP1
          DFSWC(I,K) = DFSWC(I,K) + DFN(I,K)*DFNTOP(I,N)
          UFSWC(I,K) = UFSWC(I,K) + UFN(I,K)*DFNTOP(I,N)
981     CONTINUE
980     CONTINUE
        DO 990 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 990
          GDFND(I) = GDFND(I) + CCMAX(I)*DFN(I,LP1)*DFNTOP(I,N)
990     CONTINUE
1000  CONTINUE
      DO 1100 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 1100
      DO 1101 K=1,LP1
        DFSWC(I,K) = TMP1(I)*DFSWL(I,K) + CCMAX(I)*DFSWC(I,K)
        UFSWC(I,K) = TMP1(I)*UFSWL(I,K) + CCMAX(I)*UFSWC(I,K)
1101  CONTINUE
1100  CONTINUE
      DO 1200 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 1200
        DO 1201 KK=1,LP1
        FSWC(I,KK) = UFSWC(I,KK)-DFSWC(I,KK)
1201    CONTINUE
1200  CONTINUE
      DO 1250 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 1250
        DO 1251 KK=1, L
        HSWC(I,KK) = RADCON*(FSWC(I,KK+1)-FSWC(I,KK))/DP(I,KK)
1251    CONTINUE
1250  CONTINUE

  END SUBROUTINE SWR93


  SUBROUTINE RADFS & 















                (QS,PP,PPI,QQH2O,TT,O3QO3,TSFC,SLMSK,ALBEDO,XLAT &

      ,          CAMT,KTOP,KBTM,NCLDS,EMCLD,RRCL,TTCL &
      ,          COSZRO,TAUDAR,IBEG &
      ,          KO3,KALB &
      ,          ITIMSW,ITIMLW &






































      ,          SWH,HLW &
      ,          FLWUP,FSWUP,FSWDN,FSWDNS,FSWUPS,FLWDNS,FLWUPS,FSWDNSC  &
      ,          ids,ide, jds,jde, kds,kde                      &
      ,          ims,ime, jms,jme, kms,kme                      &

      ,          its,ite, jts,jte, kts,kte                      &
      ,          imd,jmd, Jndx                                  )



























 IMPLICIT NONE


 INTEGER, PARAMETER :: NB=12
 INTEGER, PARAMETER :: NBLX=47
 INTEGER , PARAMETER:: NBLW = 163

 REAL,PARAMETER ::      AMOLWT=28.9644
 REAL,PARAMETER ::      CSUBP=1.00484E7
 REAL,PARAMETER ::      DIFFCTR=1.66
 REAL,PARAMETER ::      G=980.665
 REAL,PARAMETER ::      GINV=1./G
 REAL,PARAMETER ::      GRAVDR=980.0
 REAL,PARAMETER ::      O3DIFCTR=1.90
 REAL,PARAMETER ::      P0=1013250.
 REAL,PARAMETER ::      P0INV=1./P0
 REAL,PARAMETER ::      GP0INV=GINV*P0INV
 REAL,PARAMETER ::      P0XZP2=202649.902
 REAL,PARAMETER ::      P0XZP8=810600.098
 REAL,PARAMETER ::      P0X2=2.*1013250.
 REAL,PARAMETER ::      RADCON=8.427
 REAL,PARAMETER ::      RADCON1=1./8.427
 REAL,PARAMETER ::      RATCO2MW=1.519449738
 REAL,PARAMETER ::      RATH2OMW=.622
 REAL,PARAMETER ::      RGAS=8.3142E7
 REAL,PARAMETER ::      RGASSP=8.31432E7
 REAL,PARAMETER ::      SECPDA=8.64E4



 REAL,PARAMETER ::      HUNDRED=100.
 REAL,PARAMETER ::      HNINETY=90.
 REAL,PARAMETER ::      HNINE=9.0
 REAL,PARAMETER ::      SIXTY=60.
 REAL,PARAMETER ::      FIFTY=50.
 REAL,PARAMETER ::      TEN=10.
 REAL,PARAMETER ::      EIGHT=8.
 REAL,PARAMETER ::      FIVE=5.
 REAL,PARAMETER ::      FOUR=4.
 REAL,PARAMETER ::      THREE=3.
 REAL,PARAMETER ::      TWO=2.
 REAL,PARAMETER ::      ONE=1.
 REAL,PARAMETER ::      HAF=0.5
 REAL,PARAMETER ::      QUARTR=0.25
 REAL,PARAMETER ::      ZERO=0.



 REAL,PARAMETER ::      H83E26=8.3E26
 REAL,PARAMETER ::      H71E26=7.1E26
 REAL,PARAMETER ::      H1E15=1.E15
 REAL,PARAMETER ::      H1E13=1.E13
 REAL,PARAMETER ::      H1E11=1.E11
 REAL,PARAMETER ::      H1E8=1.E8
 REAL,PARAMETER ::      H2E6=2.0E6
 REAL,PARAMETER ::      H1E6=1.0E6
 REAL,PARAMETER ::      H69766E5=6.97667E5
 REAL,PARAMETER ::      H4E5=4.E5
 REAL,PARAMETER ::      H165E5=1.65E5
 REAL,PARAMETER ::      H5725E4=57250.
 REAL,PARAMETER ::      H488E4=48800.
 REAL,PARAMETER ::      H1E4=1.E4
 REAL,PARAMETER ::      H24E3=2400.
 REAL,PARAMETER ::      H20788E3=2078.8
 REAL,PARAMETER ::      H2075E3=2075.
 REAL,PARAMETER ::      H18E3=1800.
 REAL,PARAMETER ::      H1224E3=1224.
 REAL,PARAMETER ::      H67390E2=673.9057
 REAL,PARAMETER ::      H5E2=500.
 REAL,PARAMETER ::      H3082E2=308.2
 REAL,PARAMETER ::      H3E2=300.
 REAL,PARAMETER ::      H2945E2=294.5
 REAL,PARAMETER ::      H29316E2=293.16
 REAL,PARAMETER ::      H26E2=260.0
 REAL,PARAMETER ::      H25E2=250.
 REAL,PARAMETER ::      H23E2=230.
 REAL,PARAMETER ::      H2E2=200.0
 REAL,PARAMETER ::      H15E2=150.
 REAL,PARAMETER ::      H1386E2=138.6
 REAL,PARAMETER ::      H1036E2=103.6
 REAL,PARAMETER ::      H8121E1=81.21
 REAL,PARAMETER ::      H35E1=35.
 REAL,PARAMETER ::      H3116E1=31.16
 REAL,PARAMETER ::      H28E1=28.
 REAL,PARAMETER ::      H181E1=18.1
 REAL,PARAMETER ::      H18E1=18.
 REAL,PARAMETER ::      H161E1=16.1
 REAL,PARAMETER ::      H16E1=16.
 REAL,PARAMETER ::      H1226E1=12.26
 REAL,PARAMETER ::      H9P94=9.94
 REAL,PARAMETER ::      H6P08108=6.081081081
 REAL,PARAMETER ::      H3P6=3.6
 REAL,PARAMETER ::      H3P5=3.5
 REAL,PARAMETER ::      H2P9=2.9
 REAL,PARAMETER ::      H2P8=2.8
 REAL,PARAMETER ::      H2P5=2.5
 REAL,PARAMETER ::      H1P8=1.8
 REAL,PARAMETER ::      H1P4387=1.4387
 REAL,PARAMETER ::      H1P41819=1.418191
 REAL,PARAMETER ::      H1P4=1.4
 REAL,PARAMETER ::      H1P25892=1.258925411
 REAL,PARAMETER ::      H1P082=1.082
 REAL,PARAMETER ::      HP816=0.816
 REAL,PARAMETER ::      HP805=0.805
 REAL,PARAMETER ::      HP8=0.8
 REAL,PARAMETER ::      HP60241=0.60241
 REAL,PARAMETER ::      HP602409=0.60240964
 REAL,PARAMETER ::      HP6=0.6
 REAL,PARAMETER ::      HP526315=0.52631579
 REAL,PARAMETER ::      HP518=0.518
 REAL,PARAMETER ::      HP5048=0.5048
 REAL,PARAMETER ::      HP3795=0.3795
 REAL,PARAMETER ::      HP369=0.369
 REAL,PARAMETER ::      HP26=0.26
 REAL,PARAMETER ::      HP228=0.228
 REAL,PARAMETER ::      HP219=0.219
 REAL,PARAMETER ::      HP166666=.166666
 REAL,PARAMETER ::      HP144=0.144
 REAL,PARAMETER ::      HP118666=0.118666192
 REAL,PARAMETER ::      HP1=0.1

 REAL,PARAMETER ::      H658M2=0.0658
 REAL,PARAMETER ::      H625M2=0.0625
 REAL,PARAMETER ::      H44871M2=4.4871E-2
 REAL,PARAMETER ::      H44194M2=.044194
 REAL,PARAMETER ::      H42M2=0.042
 REAL,PARAMETER ::      H41666M2=0.0416666
 REAL,PARAMETER ::      H28571M2=.02857142857
 REAL,PARAMETER ::      H2118M2=0.02118
 REAL,PARAMETER ::      H129M2=0.0129
 REAL,PARAMETER ::      H1M2=.01
 REAL,PARAMETER ::      H559M3=5.59E-3
 REAL,PARAMETER ::      H3M3=0.003
 REAL,PARAMETER ::      H235M3=2.35E-3
 REAL,PARAMETER ::      H1M3=1.0E-3
 REAL,PARAMETER ::      H987M4=9.87E-4
 REAL,PARAMETER ::      H323M4=0.000323
 REAL,PARAMETER ::      H3M4=0.0003
 REAL,PARAMETER ::      H285M4=2.85E-4
 REAL,PARAMETER ::      H1M4=0.0001
 REAL,PARAMETER ::      H75826M4=7.58265E-4
 REAL,PARAMETER ::      H6938M5=6.938E-5
 REAL,PARAMETER ::      H394M5=3.94E-5
 REAL,PARAMETER ::      H37412M5=3.7412E-5
 REAL,PARAMETER ::      H15M5=1.5E-5
 REAL,PARAMETER ::      H1439M5=1.439E-5
 REAL,PARAMETER ::      H128M5=1.28E-5
 REAL,PARAMETER ::      H102M5=1.02E-5
 REAL,PARAMETER ::      H1M5=1.0E-5
 REAL,PARAMETER ::      H7M6=7.E-6
 REAL,PARAMETER ::      H4999M6=4.999E-6
 REAL,PARAMETER ::      H451M6=4.51E-6
 REAL,PARAMETER ::      H25452M6=2.5452E-6
 REAL,PARAMETER ::      H1M6=1.E-6
 REAL,PARAMETER ::      H391M7=3.91E-7
 REAL,PARAMETER ::      H1174M7=1.174E-7
 REAL,PARAMETER ::      H8725M8=8.725E-8
 REAL,PARAMETER ::      H327M8=3.27E-8
 REAL,PARAMETER ::      H257M8=2.57E-8
 REAL,PARAMETER ::      H1M8=1.0E-8
 REAL,PARAMETER ::      H23M10=2.3E-10
 REAL,PARAMETER ::      H14M10=1.4E-10
 REAL,PARAMETER ::      H11M10=1.1E-10
 REAL,PARAMETER ::      H1M10=1.E-10
 REAL,PARAMETER ::      H83M11=8.3E-11
 REAL,PARAMETER ::      H82M11=8.2E-11
 REAL,PARAMETER ::      H8M11=8.E-11
 REAL,PARAMETER ::      H77M11=7.7E-11
 REAL,PARAMETER ::      H72M11=7.2E-11
 REAL,PARAMETER ::      H53M11=5.3E-11
 REAL,PARAMETER ::      H48M11=4.8E-11
 REAL,PARAMETER ::      H44M11=4.4E-11
 REAL,PARAMETER ::      H42M11=4.2E-11
 REAL,PARAMETER ::      H37M11=3.7E-11
 REAL,PARAMETER ::      H35M11=3.5E-11
 REAL,PARAMETER ::      H32M11=3.2E-11
 REAL,PARAMETER ::      H3M11=3.0E-11
 REAL,PARAMETER ::      H28M11=2.8E-11
 REAL,PARAMETER ::      H24M11=2.4E-11
 REAL,PARAMETER ::      H23M11=2.3E-11
 REAL,PARAMETER ::      H2M11=2.E-11
 REAL,PARAMETER ::      H18M11=1.8E-11
 REAL,PARAMETER ::      H15M11=1.5E-11
 REAL,PARAMETER ::      H14M11=1.4E-11
 REAL,PARAMETER ::      H114M11=1.14E-11
 REAL,PARAMETER ::      H11M11=1.1E-11
 REAL,PARAMETER ::      H1M11=1.E-11
 REAL,PARAMETER ::      H96M12=9.6E-12
 REAL,PARAMETER ::      H93M12=9.3E-12
 REAL,PARAMETER ::      H77M12=7.7E-12
 REAL,PARAMETER ::      H74M12=7.4E-12
 REAL,PARAMETER ::      H65M12=6.5E-12
 REAL,PARAMETER ::      H62M12=6.2E-12
 REAL,PARAMETER ::      H6M12=6.E-12
 REAL,PARAMETER ::      H45M12=4.5E-12
 REAL,PARAMETER ::      H44M12=4.4E-12
 REAL,PARAMETER ::      H4M12=4.E-12
 REAL,PARAMETER ::      H38M12=3.8E-12
 REAL,PARAMETER ::      H37M12=3.7E-12
 REAL,PARAMETER ::      H3M12=3.E-12
 REAL,PARAMETER ::      H29M12=2.9E-12
 REAL,PARAMETER ::      H28M12=2.8E-12
 REAL,PARAMETER ::      H24M12=2.4E-12
 REAL,PARAMETER ::      H21M12=2.1E-12
 REAL,PARAMETER ::      H16M12=1.6E-12
 REAL,PARAMETER ::      H14M12=1.4E-12
 REAL,PARAMETER ::      H12M12=1.2E-12
 REAL,PARAMETER ::      H8M13=8.E-13
 REAL,PARAMETER ::      H46M13=4.6E-13
 REAL,PARAMETER ::      H36M13=3.6E-13
 REAL,PARAMETER ::      H135M13=1.35E-13
 REAL,PARAMETER ::      H12M13=1.2E-13
 REAL,PARAMETER ::      H1M13=1.E-13
 REAL,PARAMETER ::      H3M14=3.E-14
 REAL,PARAMETER ::      H15M14=1.5E-14
 REAL,PARAMETER ::      H14M14=1.4E-14



 REAL,PARAMETER ::      HM2M2=-.02
 REAL,PARAMETER ::      HM6666M2=-.066667
 REAL,PARAMETER ::      HMP5=-0.5
 REAL,PARAMETER ::      HMP575=-0.575
 REAL,PARAMETER ::      HMP66667=-.66667
 REAL,PARAMETER ::      HMP805=-0.805
 REAL,PARAMETER ::      HM1EZ=-1.
 REAL,PARAMETER ::      HM13EZ=-1.3
 REAL,PARAMETER ::      HM19EZ=-1.9
 REAL,PARAMETER ::      HM1E1=-10.
 REAL,PARAMETER ::      HM1597E1=-15.97469413
 REAL,PARAMETER ::      HM161E1=-16.1
 REAL,PARAMETER ::      HM1797E1=-17.97469413
 REAL,PARAMETER ::      HM181E1=-18.1
 REAL,PARAMETER ::      HM8E1=-80.
 REAL,PARAMETER ::      HM1E2=-100.

 REAL,PARAMETER ::      H1M16=1.0E-16
 REAL,PARAMETER ::      H1M20=1.E-20
 REAL,PARAMETER ::      Q19001=19.001
 REAL,PARAMETER ::      DAYSEC=1.1574E-5
 REAL,PARAMETER ::      HSIGMA=5.673E-8
 REAL,PARAMETER ::      TWENTY=20.0
 REAL,PARAMETER ::      HP537=0.537
 REAL,PARAMETER ::      HP2=0.2
 REAL,PARAMETER ::      RCO2=3.3E-4
 REAL,PARAMETER ::      H3M6=3.0E-6
 REAL,PARAMETER ::      PI=3.1415927
 REAL,PARAMETER ::      DEGRAD1=180.0/PI
 REAL,PARAMETER ::      H74E1=74.0
 REAL,PARAMETER ::      H15E1=15.0

 REAL, PARAMETER:: B0 = -.51926410E-4
 REAL, PARAMETER:: B1 = -.18113332E-3
 REAL, PARAMETER:: B2 = -.10680132E-5
 REAL, PARAMETER:: B3 = -.67303519E-7
 REAL, PARAMETER:: AWIDE = 0.309801E+01
 REAL, PARAMETER:: BWIDE = 0.495357E-01
 REAL, PARAMETER:: BETAWD = 0.347839E+02
 REAL, PARAMETER:: BETINW = 0.766811E+01


      INTEGER, INTENT(IN)        :: ids,ide, jds,jde, kds,kde ,      &
                                    ims,ime, jms,jme, kms,kme ,      &
                                    its,ite, jts,jte, kts,kte
      INTEGER, INTENT(IN)        :: IBEG,KO3,KALB,ITIMSW,ITIMLW

































     


      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte):: PP,TT
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte):: QQH2O
      REAL,    INTENT(IN), DIMENSION(its:ite,kts:kte+1):: PPI,CAMT,EMCLD
      REAL,    INTENT(IN), DIMENSION(its:ite):: QS,TSFC,SLMSK,ALBEDO,XLAT
      REAL,    INTENT(IN), DIMENSION(its:ite):: COSZRO,TAUDAR
      REAL,    INTENT(OUT), DIMENSION(its:ite):: FLWUPS
      INTEGER, INTENT(IN), DIMENSION(its:ite):: NCLDS
      INTEGER, INTENT(IN), DIMENSION(its:ite,kts:kte+1):: KTOP,KBTM
      REAL,    INTENT(INOUT), DIMENSION(its:ite,NB,kts:kte+1):: TTCL,RRCL
      REAL, intent(IN), DIMENSION(its:ite,kts:kte):: O3QO3







      REAL,  DIMENSION(3) :: BO3RND,AO3RND
      REAL,  DIMENSION(NBLY) :: APCM,BPCM,ATPCM,BTPCM,ACOMB, &
                                BCOMB,BETACM

      DATA AO3RND / 0.543368E+02,  0.234676E+04,  0.384881E+02/ 
      DATA BO3RND / 0.526064E+01,  0.922424E+01,  0.496515E+01/

      DATA ACOMB  / &
         0.152070E+05,  0.332194E+04,  0.527177E+03,  0.163124E+03, &
         0.268808E+03,  0.534591E+02,  0.268071E+02,  0.123133E+02, &
         0.600199E+01,  0.640803E+00,  0.501549E-01,  0.167961E-01, &
         0.178110E-01,  0.170166E+00,  0.537083E-02/
      DATA BCOMB  / &
         0.152538E+00,  0.118677E+00,  0.103660E+00,  0.100119E+00, &
         0.127518E+00,  0.118409E+00,  0.904061E-01,  0.642011E-01, &
         0.629660E-01,  0.643346E-01,  0.717082E-01,  0.629730E-01, &
         0.875182E-01,  0.857907E-01,  0.214005E+00/
      DATA APCM   / &
        -0.671879E-03,  0.654345E-02,  0.143657E-01,  0.923593E-02, &
         0.117022E-01,  0.159596E-01,  0.181600E-01,  0.145013E-01, &
         0.170062E-01,  0.233303E-01,  0.256735E-01,  0.274745E-01, &
         0.279259E-01,  0.197002E-01,  0.349782E-01/
      DATA BPCM   / &
        -0.113520E-04, -0.323965E-04, -0.448417E-04, -0.230779E-04, &
        -0.361981E-04, -0.145117E-04,  0.198349E-04, -0.486529E-04, &
        -0.550050E-04, -0.684057E-04, -0.447093E-04, -0.778390E-04, &
        -0.982953E-04, -0.772497E-04, -0.748263E-04/
      DATA ATPCM  / &
        -0.106346E-02,  0.641531E-02,  0.137362E-01,  0.922513E-02, &
         0.136162E-01,  0.169791E-01,  0.206959E-01,  0.166223E-01, &
         0.171776E-01,  0.229724E-01,  0.275530E-01,  0.302731E-01, &
         0.281662E-01,  0.199525E-01,  0.370962E-01/
      DATA BTPCM  / &
        -0.735731E-05, -0.294149E-04, -0.505592E-04, -0.280894E-04, &
        -0.492972E-04, -0.341508E-04, -0.362947E-04, -0.250487E-04, &
        -0.521369E-04, -0.746260E-04, -0.744124E-04, -0.881905E-04, &
        -0.933645E-04, -0.664045E-04, -0.115290E-03/
      DATA BETACM / &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.188625E+03,  0.144293E+03,  0.174098E+03,  0.909366E+02, &
         0.497489E+02,  0.221212E+02,  0.113124E+02,  0.754174E+01, &
         0.589554E+01,  0.495227E+01,  0.000000E+00/






       REAL, INTENT(INOUT),DIMENSION(its:ite,kts:kte)::SWH,HLW
       REAL, INTENT(OUT), DIMENSION(its:ite):: FSWUP,FSWUPS,FSWDN, &
                           FSWDNS,FLWUP,FLWDNS,FSWDNSC
      




      REAL, DIMENSION(its:ite):: GDFVBR,GDFNBR,GDFVDR,GDFNDR





      REAL, DIMENSION(its:ite,kts:kte+1)::FSWL,HSWL,UFL,DFL





       REAL, DIMENSION(its:ite,kts:kte+1,kts:kte+1)::CLDFAC
       REAL, DIMENSION(its:ite,kts:kte+1)::EQCMT,PRESS,TEMP,FSW,HSW,UF,DF
       REAL, DIMENSION(its:ite,kts:kte)::RH2O,QO3,HEATRA
       REAL, DIMENSION(its:ite):: COSZEN,TAUDA,GRNFLX,TOPFLX,GRDFLX
       REAL, DIMENSION(kts:kte+1)::PHALF


       REAL,    DIMENSION(NB) :: ABCFF,PWTS

       DATA ABCFF/2*4.0E-5,.002,.035,.377,1.95,9.40,44.6,190., &
                  989.,2706.,39011./
       DATA PWTS/.5000,.121416,.0698,.1558,.0631,.0362,.0243,.0158,.0087, &
                 .001467,.002342,.001075/

       REAL     :: CFCO2,CFO3,REFLO3,RRAYAV

       DATA CFCO2,CFO3/508.96,466.64/
       DATA REFLO3/1.9/
       DATA RRAYAV/0.144/





       REAL,    DIMENSION(its:ite):: TTHAN
       REAL,    DIMENSION(its:ite,kts:kte):: DO3V,DO3VP
       INTEGER, DIMENSION(its:ite):: JJROW









       REAL, DIMENSION(37,kte) :: DDUO3N,DDO3N2,DDO3N3,DDO3N4



      REAL,   DIMENSION(21,20) :: ALBD
      REAL,   DIMENSION(20)    :: ZA
      REAL,   DIMENSION(21)    :: TRN
      REAL,   DIMENSION(19)    :: DZA

      REAL    :: YEAR,TPI,SSOLAR,DATE,TH2,ZEN,DZEN,ALB1,ALB2
      INTEGER :: IR,IQ,JX
      DATA TRN/.00,.05,.10,.15,.20,.25,.30,.35,.40,.45,.50,.55,.60,.65, &
               .70,.75,.80,.85,.90,.95,1.00/

      REAL ::  ALB11(21,7),ALB22(21,7),ALB33(21,6)

      EQUIVALENCE (ALB11(1,1),ALBD(1,1)),(ALB22(1,1),ALBD(1,8)), &
                  (ALB33(1,1),ALBD(1,15))
      DATA ALB11/ .061,.062,.072,.087,.115,.163,.235,.318,.395,.472,.542, &
       .604,.655,.693,.719,.732,.730,.681,.581,.453,.425,.061,.062,.070, &
       .083,.108,.145,.198,.263,.336,.415,.487,.547,.595,.631,.656,.670, &
       .652,.602,.494,.398,.370,.061,.061,.068,.079,.098,.130,.174,.228, &
       .290,.357,.424,.498,.556,.588,.603,.592,.556,.488,.393,.342,.325, &
       .061,.061,.065,.073,.086,.110,.150,.192,.248,.306,.360,.407,.444, &
       .469,.480,.474,.444,.386,.333,.301,.290,.061,.061,.065,.070,.082, &
       .101,.131,.168,.208,.252,.295,.331,.358,.375,.385,.377,.356,.320, &
       .288,.266,.255,.061,.061,.063,.068,.077,.092,.114,.143,.176,.210, &
       .242,.272,.288,.296,.300,.291,.273,.252,.237,.266,.220,.061,.061, &
       .062,.066,.072,.084,.103,.127,.151,.176,.198,.219,.236,.245,.250, &
       .246,.235,.222,.211,.205,.200/
      DATA ALB22/ .061,.061,.061,.065,.071,.079,.094,.113,.134,.154,.173, &
       .185,.190,.193,.193,.190,.188,.185,.182,.180,.178,.061,.061,.061, &
       .064,.067,.072,.083,.099,.117,.135,.150,.160,.164,.165,.164,.162, &
       .160,.159,.158,.157,.157,.061,.061,.061,.062,.065,.068,.074,.084, &
       .097,.111,.121,.127,.130,.131,.131,.130,.129,.127,.126,.125,.122, &
       .061,.061,.061,.061,.062,.064,.070,.076,.085,.094,.101,.105,.107, &
       .106,.103,.100,.097,.096,.095,.095,.095,.061,.061,.061,.060,.061, &
       .062,.065,.070,.075,.081,.086,.089,.090,.088,.084,.080,.077,.075, &
       .074,.074,.074,.061,.061,.060,.060,.060,.061,.063,.065,.068,.072, &
       .076,.077,.076,.074,.071,.067,.064,.062,.061,.061,.061,.061,.061, &
       .060,.060,.060,.060,.061,.062,.065,.068,.069,.069,.068,.065,.061, &
       .058,.055,.054,.053,.052,.052/
      DATA ALB33/ .061,.061,.060,.060,.060,.060,.060,.060,.062,.065,.065, &
       .063,.060,.057,.054,.050,.047,.046,.045,.044,.044,.061,.061,.060, &
       .060,.060,.059,.059,.059,.059,.059,.058,.055,.051,.047,.043,.039, &
       .035,.033,.032,.031,.031,.061,.061,.060,.060,.060,.059,.059,.058, &
       .057,.056,.054,.051,.047,.043,.039,.036,.033,.030,.028,.027,.026, &
       .061,.061,.060,.060,.060,.059,.059,.058,.057,.055,.052,.049,.045, &
       .040,.036,.032,.029,.027,.026,.025,.025,.061,.061,.060,.060,.060, &
       .059,.059,.058,.056,.053,.050,.046,.042,.038,.034,.031,.028,.026, &
       .025,.025,.025,.061,.061,.060,.060,.059,.058,.058,.057,.055,.053, &
       .050,.046,.042,.038,.034,.030,.028,.029,.025,.025,.025/
      DATA ZA/90.,88.,86.,84.,82.,80.,78.,76.,74.,70.,66.,62.,58.,54., &
              50.,40.,30.,20.,10.,0.0/
      DATA DZA/8*2.0,6*4.0,5*10.0/




       REAL,    DIMENSION(its:ite)        :: ALVB,ALNB,ALVD,ALND, &
                                             GDFVB,   &
                                             GDFNB,GDFVD,GDFND,   &
                                             SFCALB

       REAL    :: RRVCO2,RRCO2,TDUM
       REAL    :: ALBD0,ALVD1,ALND1
       INTEGER :: N


       integer :: imd,jmd, Jndx
       real :: FSWrat,FSWrat1,FSWDNS1






      REAL,PARAMETER :: H196=1.96

      INTEGER :: K, I,KP,LLM2,J1,J3,KMAX,KMIN,KCLDS,ICNT,LLM1
      INTEGER :: L,LP1,LP2,LP3,LM1,LM2,LM3,MYIS,MYIE,LLP1,LL,KK,KLEN

      L=kte
      LP1=L+1;  LP2=L+2;  LP3=L+3; LLP1 = 2*L + 1
      LM1=L-1;  LM2=L-2;  LM3=L-3; LL = 2*L
      LLM2 = LL-2; LLM1=LL-1
      MYIS=its; MYIE=ite





      SSOLAR=H196/(R1*R1)







      SSOLAR=SSOLAR*0.97

      DO 40 I=MYIS,MYIE
        IR = I + IBEG - 1
        TH2=HP2*XLAT(IR)
        JJROW(I)=Q19001-TH2
        TTHAN(I)=(19-JJROW(I))-TH2


        SFCALB(I) = ALBEDO(IR)






        PRESS(I,LP1)=QS(IR)*10.0
        TEMP(I,LP1)=ABS(TSFC(IR))
        COSZEN(I) = COSZRO(IR)
        TAUDA(I) = TAUDAR(IR)
   40 CONTINUE




      DO 50 K=1,L
       DO 50 I=MYIS,MYIE
        IR = I + IBEG - 1

        TEMP(I,K) = TT(IR,K)
        PRESS(I,K) = 10.0 * PP(IR,K)

        RH2O(I,K)=QQH2O(IR,K)
        IF(RH2O(I,K).LT.H3M6) RH2O(I,K)=H3M6
   50 CONTINUE

      IF (KO3.EQ.0) GO TO 65

      DO 60 K=1,L
       DO 60 I=MYIS,MYIE
        QO3(I,K) = O3QO3(I+IBEG-1,K)
   60 CONTINUE
   65 CONTINUE

      IF (KALB.GT.0) GO TO 110



      IQ=INT(TWENTY*HP537+ONE)
      DO 105 I=MYIS,MYIE
         IF(COSZEN(I).GT.0.0 .AND. SLMSK(I+IBEG-1).GT.0.5) THEN
           ZEN=DEGRAD1*ACOS(MAX(COSZEN(I),0.0))
           IF(ZEN.GE.H74E1) JX=INT(HAF*(HNINETY-ZEN)+ONE)
           IF(ZEN.LT.H74E1.AND.ZEN.GE.FIFTY) &
              JX=INT(QUARTR*(H74E1-ZEN)+HNINE)
           IF(ZEN.LT.FIFTY) JX=INT(HP1*(FIFTY-ZEN)+H15E1)
           DZEN=-(ZEN-ZA(JX))/DZA(JX)
           ALB1=ALBD(IQ,JX)+DZEN*(ALBD(IQ,JX+1)-ALBD(IQ,JX))
           ALB2=ALBD(IQ+1,JX)+DZEN*(ALBD(IQ+1,JX+1)-ALBD(IQ+1,JX))
           SFCALB(I)=ALB1+TWENTY*(ALB2-ALB1)*(HP537-TRN(IQ))
         ENDIF
  105 CONTINUE
  110 CONTINUE

      IF (KO3.GT.0) GO TO 135



      DO 125 I=MYIS,MYIE

         PHALF(1)=0.
         PHALF(LP1)=PPI(I,kme)
         DO K=1,LM1
            PHALF(K+1)=PP(I,K) 
         ENDDO

         CALL O3INT(PHALF,DDUO3N,DDO3N2,DDO3N3,DDO3N4, &
                 ids,ide, jds,jde, kds,kde,            &
                 ims,ime, jms,jme, kms,kme,            &
                 its,ite, jts,jte, kts,kte             )

         DO 130 K=1,L
          DO3V(I,K)  = DDUO3N(JJROW(I),K) + RSIN1*DDO3N2(JJROW(I),K) &
                      +RCOS1*DDO3N3(JJROW(I),K) &
                      +RCOS2*DDO3N4(JJROW(I),K)
          DO3VP(I,K) = DDUO3N(JJROW(I)+1,K) + RSIN1*DDO3N2(JJROW(I)+1,K) &
                     +RCOS1*DDO3N3(JJROW(I)+1,K) &
                     +RCOS2*DDO3N4(JJROW(I)+1,K)


          QO3(I,K) = H1M4 * (DO3V(I,K)+TTHAN(I)*(DO3VP(I,K)-DO3V(I,K)))
  130   CONTINUE
  125 CONTINUE
  135 CONTINUE

      DO 195 I=MYIS,MYIE

        ALVD(I) = SFCALB(I)
        ALND(I) = SFCALB(I)

        ALVB(I) = SFCALB(I)
        ALNB(I) = SFCALB(I)






        IF (SLMSK(I+IBEG-1).LT.0.5) THEN
         IF (SFCALB(I).LE.0.5) THEN
          ALBD0 = -18.0 * (0.5 - ACOS(COSZEN(I))/PI)
          ALBD0 = EXP (ALBD0)
          ALVD1 = (ALVD(I) - 0.054313) / 0.945687
          ALND1 = (ALND(I) - 0.054313) / 0.945687
          ALVB(I) = ALVD1 + (1.0 - ALVD1) * ALBD0
          ALNB(I) = ALND1 + (1.0 - ALND1) * ALBD0
 
          ALVB(I) = MIN(0.5,ALVB(I))
          ALNB(I) = MIN(0.5,ALNB(I))
         END IF
        END IF
  195 CONTINUE

      DO 200 N=1,2
        DO 200 I=MYIS,MYIE
      RRCL(I,N,1)=ALVD(I)
      TTCL(I,N,1)=ZERO
  200 CONTINUE
      DO 220 N=3,NB
      DO 220 I=MYIS,MYIE
         RRCL(I,N,1)=ALND(I)
         TTCL(I,N,1)=ZERO
  220 CONTINUE





      RRVCO2=RCO2
      RRCO2=RRVCO2*RATCO2MW
  250 IF(ITIMLW .EQ. 0) GO TO 300






      DO 240 K=1,LP1
      DO 240 I=MYIS,MYIE
        EQCMT(I,K)=CAMT(I,K)*EMCLD(I,K)
  240 CONTINUE





      CALL CLO89(CLDFAC,EQCMT,NCLDS,KBTM,KTOP, &
                 ids,ide, jds,jde, kds,kde,    &
                 ims,ime, jms,jme, kms,kme,    &
                 its,ite, jts,jte, kts,kte     )





















      CALL LWR88(HEATRA,GRNFLX,TOPFLX,         &
                 PRESS,TEMP,RH2O,QO3,CLDFAC,   &
                 EQCMT,NCLDS,KTOP,KBTM,        &


                 BO3RND,AO3RND, &
                 APCM,BPCM,ATPCM,BTPCM,ACOMB,BCOMB,BETACM,     &
                 ZERO,ONE,H18E3,P0INV,H6P08108,DIFFCTR,        &
                 GINV,H3M4,BETINW,RATH2OMW,GP0INV,P0,P0XZP8,   &
                 P0XZP2,H3M3,H1M3,H1M2,H25E2,B0,B2,B1,B3,HAF,  &
                 TEN,HP1,FOUR,HM1EZ,                           &
                 RADCON,QUARTR,TWO,                            &
                 HM6666M2,HMP66667,HMP5, HP166666,H41666M2,    &
                 RADCON1,H16E1, H28E1,H44194M2,H1P41819,       &
                 ids,ide, jds,jde, kds,kde,                    &
                 ims,ime, jms,jme, kms,kme,                    &
                  its,ite, jts,jte, kts,kte                    )

















      DO 280 I=MYIS,MYIE
        IR = I + IBEG - 1
        FLWUP(IR) = .001*TOPFLX(I)


        TDUM=.5*(TEMP(I,LP1)+TEMP(I,L))
        FLWUPS(IR)=HSIGMA*TDUM*TDUM*TDUM*TDUM

        FLWDNS(IR)=FLWUPS(IR)-.001*GRNFLX(I)
  280 CONTINUE


      DO I=MYIS,MYIE
         TDUM=.5*(HEATRA(I,L)+HEATRA(I,LM1))
         HEATRA(I,L)=TDUM
         HEATRA(I,LM1)=TDUM
      ENDDO

      DO 290 K=1,L
        DO 290 I=MYIS,MYIE
          HLW(I+IBEG-1,K)=HEATRA(I,K)*DAYSEC
  290 CONTINUE
  300 CONTINUE
      IF(ITIMSW .EQ. 0) GO TO 350

      CALL SWR93(FSW,HSW,UF,DF,FSWL,HSWL,UFL,DFL, &
                 PRESS,COSZEN,TAUDA,RH2O,RRCO2,SSOLAR,QO3, &
                 NCLDS,KTOP,KBTM,CAMT,RRCL,TTCL, &
                 ALVB,ALNB,ALVD,ALND,GDFVB,GDFNB,GDFVD,GDFND, &


                 ABCFF,PWTS,                                    &
                 H35E1,H1224E3,ONE,ZERO,HAF,H69766E5,HP219,     &
                 HP816,RRAYAV,GINV,CFCO2,CFO3,                  &
                 TWO,H235M3,HP26,H129M2,H75826M4,H1036E2,       &
                 H1P082,HMP805,H1386E2,H658M2,H2118M2,H42M2,    &
                 H323M4,HM1EZ,DIFFCTR,O3DIFCTR,FIFTY,RADCON,    &
                 ids,ide, jds,jde, kds,kde,                     &
                 ims,ime, jms,jme, kms,kme,                     &
                 its,ite, jts,jte, kts,kte                      )




      DO 320 I=MYIS,MYIE
       IR = I + IBEG - 1
       FSWUP(IR) = UF(I,1) * 1.E-3
       FSWDN(IR) = DF(I,1) * 1.E-3
       FSWUPS(IR) = UF(I,LP1) * 1.E-3



       FSWDNS(IR) = DF(I,LP1) * 1.E-3
       FSWDNSC(IR) = DFL(I,LP1) * 1.E-3


       GDFVDR(IR) = GDFVD(I) * 1.E-3
       GDFNDR(IR) = GDFND(I) * 1.E-3

       GDFVBR(IR) = GDFVB(I) * 1.E-3
       GDFNBR(IR) = GDFNB(I) * 1.E-3
  320 CONTINUE

      DO 330 K=1,L
        DO 330 I=MYIS,MYIE
          SWH(I+IBEG-1,K)=HSW(I,K)*DAYSEC
  330 CONTINUE
  350 CONTINUE



















      RETURN
 1000 FORMAT(1H ,' YOU ARE CALLING GFDL RADIATION CODE FOR',I5,' PTS', &
                 'AND',I4,' LYRS,WITH KDAPRX,KO3,KCZ,KEMIS,KALB = ',5I2)
 
  END SUBROUTINE RADFS 


    SUBROUTINE O3CLIM





 IMPLICIT NONE














































       INTEGER, PARAMETER :: NL=81,NLP1=NL+1,NLGTH=37*NL,NKK=41,NK=81,NKP=NK+1












       REAL :: PH1(45),PH2(37),P1(48),P2(33),O3HI1(10,16),O3HI2(10,9) &
              ,O3LO1(10,16),O3LO2(10,16),O3LO3(10,16),O3LO4(10,16)

       REAL    :: AVG,A1,B1,B2
       INTEGER :: K,N,NCASE,IPLACE,KK,NKM,NKMM,KI,KQ,JJ,KEN,I,iindex,jindex

       REAL :: PSTD(NL),TEMPN(19),O3O3(37,NL,4),O35DEG(37,NL) &
      ,XRAD1(NLGTH),XRAD2(NLGTH),XRAD3(NLGTH),XRAD4(NLGTH) &
      ,DDUO3N(19,NL),DUO3N(19,41) &
      ,RO3(10,41),RO3M(10,40),RO31(10,41),RO32(10,41) &
      ,O3HI(10,25) &
      ,RSTD(81),RBAR(NL),RDATA(81) &
      ,PHALF(NL),P(81),PH(82)
       REAL   :: PXX(81),PYY(82)                       





                           EQUIVALENCE &
       (O3HI1(1,1),O3HI(1,1)),(O3HI2(1,1),O3HI(1,17)) &
      ,(PH1(1),PYY(1)),(PH2(1),PYY(46)) &               
      ,(P1(1),PXX(1)),(P2(1),PXX(49))                   





                           EQUIVALENCE &
       (XRAD1(1),O3O3(1,1,1)) &
      ,(XRAD2(1),O3O3(1,1,2)) &
      ,(XRAD3(1),O3O3(1,1,3)),(XRAD4(1),O3O3(1,1,4))


      DATA PH1/      0.,     &
           0.1027246E-04, 0.1239831E-04, 0.1491845E-04, 0.1788053E-04,     &
           0.2135032E-04, 0.2540162E-04, 0.3011718E-04, 0.3558949E-04,     &
           0.4192172E-04, 0.4922875E-04, 0.5763817E-04, 0.6729146E-04,     &
           0.7834518E-04, 0.9097232E-04, 0.1053635E-03, 0.1217288E-03,     &
           0.1402989E-03, 0.1613270E-03, 0.1850904E-03, 0.2119495E-03,     &
           0.2423836E-03, 0.2768980E-03, 0.3160017E-03, 0.3602623E-03,     &
           0.4103126E-03, 0.4668569E-03, 0.5306792E-03, 0.6026516E-03,     &
           0.6839018E-03, 0.7759249E-03, 0.8803303E-03, 0.9987843E-03,     &
           0.1133178E-02, 0.1285955E-02, 0.1460360E-02, 0.1660001E-02,     &
           0.1888764E-02, 0.2151165E-02, 0.2452466E-02, 0.2798806E-02,     &
           0.3197345E-02, 0.3656456E-02, 0.4185934E-02, 0.4797257E-02/     
      DATA PH2/     &
           0.5503893E-02, 0.6321654E-02, 0.7269144E-02, 0.8368272E-02,     &
           0.9644873E-02, 0.1112946E-01, 0.1285810E-01, 0.1487354E-01,     &
           0.1722643E-01, 0.1997696E-01, 0.2319670E-01, 0.2697093E-01,     &
           0.3140135E-01, 0.3660952E-01, 0.4274090E-01, 0.4996992E-01,     &
           0.5848471E-01, 0.6847525E-01, 0.8017242E-01, 0.9386772E-01,     &
           0.1099026E+00, 0.1286765E+00, 0.1506574E+00, 0.1763932E+00,     &
           0.2065253E+00, 0.2415209E+00, 0.2814823E+00, 0.3266369E+00,     &
           0.3774861E+00, 0.4345638E+00, 0.4984375E+00, 0.5697097E+00,     &
           0.6490189E+00, 0.7370409E+00, 0.8344896E+00, 0.9421190E+00,     &
           0.1000000E+01/     
      DATA P1/     &
           0.9300000E-05, 0.1129521E-04, 0.1360915E-04, 0.1635370E-04,     &
           0.1954990E-04, 0.2331653E-04, 0.2767314E-04, 0.3277707E-04,     &
           0.3864321E-04, 0.4547839E-04, 0.5328839E-04, 0.6234301E-04,     &
           0.7263268E-04, 0.8450696E-04, 0.9793231E-04, 0.1133587E-03,     &
           0.1307170E-03, 0.1505832E-03, 0.1728373E-03, 0.1982122E-03,     &
           0.2266389E-03, 0.2592220E-03, 0.2957792E-03, 0.3376068E-03,     &
           0.3844381E-03, 0.4379281E-03, 0.4976965E-03, 0.5658476E-03,     &
           0.6418494E-03, 0.7287094E-03, 0.8261995E-03, 0.9380076E-03,     &
           0.1063498E-02, 0.1207423E-02, 0.1369594E-02, 0.1557141E-02,     &
           0.1769657E-02, 0.2015887E-02, 0.2295520E-02, 0.2620143E-02,     &
           0.2989651E-02, 0.3419469E-02, 0.3909867E-02, 0.4481491E-02,     &
           0.5135272E-02, 0.5898971E-02, 0.6774619E-02, 0.7799763E-02/     
      DATA P2/     &
           0.8978218E-02, 0.1036103E-01, 0.1195488E-01, 0.1382957E-01,     &
           0.1599631E-01, 0.1855114E-01, 0.2151235E-01, 0.2501293E-01,     &
           0.2908220E-01, 0.3390544E-01, 0.3952926E-01, 0.4621349E-01,     &
           0.5403168E-01, 0.6330472E-01, 0.7406807E-01, 0.8677983E-01,     &
           0.1015345E+00, 0.1189603E+00, 0.1391863E+00, 0.1630739E+00,     &
           0.1908004E+00, 0.2235461E+00, 0.2609410E+00, 0.3036404E+00,     &
           0.3513750E+00, 0.4055375E+00, 0.4656677E+00, 0.5335132E+00,     &
           0.6083618E+00, 0.6923932E+00, 0.7845676E+00, 0.8875882E+00,     &
           0.1000000E+01/     
      DATA O3HI1/     &
       .55,.50,.45,.45,.40,.35,.35,.30,.30,.30,     &
       .55,.51,.46,.47,.42,.38,.37,.36,.35,.35,     &
       .55,.53,.48,.49,.44,.42,.41,.40,.38,.38,     &
       .60,.55,.52,.52,.50,.47,.46,.44,.42,.41,     &
       .65,.60,.55,.56,.53,.52,.50,.48,.45,.45,     &
       .75,.65,.60,.60,.55,.55,.55,.50,.48,.47,     &
       .80,.75,.75,.75,.70,.70,.65,.63,.60,.60,     &
       .90,.85,.85,.80,.80,.75,.75,.74,.72,.71,     &
       1.10,1.05,1.00,.90,.90,.90,.85,.83,.80,.80,        &
       1.40,1.30,1.25,1.25,1.25,1.20,1.15,1.10,1.05,1.00, &
       1.7,1.7,1.6,1.6,1.6,1.6,1.6,1.6,1.5,1.5,     &
       2.1,2.0,1.9,1.9,1.9,1.8,1.8,1.8,1.7,1.7,     &
       2.4,2.3,2.2,2.2,2.2,2.1,2.1,2.1,2.0,2.0,     &
       2.7,2.5,2.5,2.5,2.5,2.5,2.4,2.4,2.3,2.3,     &
       2.9,2.8,2.7,2.7,2.7,2.7,2.7,2.7,2.6,2.6,     &
       3.1,3.1,3.0,3.0,3.0,3.0,3.0,3.0,2.9,2.8/     
      DATA O3HI2/     &
       3.3,3.4,3.4,3.6,3.7,3.9,4.0,4.1,4.0,3.8,     &
       3.6,3.8,3.9,4.2,4.7,5.3,5.6,5.7,5.5,5.2,     &
       4.1,4.3,4.7,5.2,6.0,6.7,7.0,6.8,6.4,6.2,     &
       5.4,5.7,6.0,6.6,7.3,8.0,8.4,7.7,7.1,6.7,     &
       6.7,6.8,7.0,7.6,8.3,10.0,9.6,8.2,7.5,7.2,     &
       9.2,9.3,9.4,9.6,10.3,10.6,10.0,8.5,7.7,7.3,     &
       12.6,12.1,12.0,12.1,11.7,11.0,10.0,8.6,7.8,7.4, &
       14.2,13.5,13.1,12.8,11.9,10.9,9.8,8.5,7.8,7.5,  &
       14.3,14.0,13.4,12.7,11.6,10.6,9.3,8.4,7.6,7.3/     
      DATA O3LO1/     &
       14.9,14.2,13.3,12.5,11.2,10.3,9.5,8.6,7.5,7.4,  &
       14.5,14.1,13.0,11.8,10.5,9.8,9.2,7.9,7.4,7.4,   &
       11.8,11.5,10.9,10.5,9.9,9.6,8.9,7.5,7.2,7.2,    &
       7.3,7.7,7.8,8.4,8.4,8.5,7.9,7.4,7.1,7.1,     &
       4.1,4.4,5.3,6.6,6.9,7.5,7.4,7.2,7.0,6.9,     &
       1.8,1.9,2.5,3.3,4.5,5.8,6.3,6.3,6.4,6.1,     &
       0.4,0.5,0.8,1.2,2.7,3.6,4.6,4.7,5.0,5.2,     &
       .10,.15,.20,.50,1.4,2.1,3.0,3.2,3.5,3.9,     &
       .07,.10,.12,.30,1.0,1.4,1.8,1.9,2.3,2.5,     &
       .06,.08,.10,.15,.60,.80,1.4,1.5,1.5,1.6,     &
       .05,.05,.06,.09,.20,.40,.70,.80,.90,.90,     &
       .05,.05,.06,.08,.10,.13,.20,.25,.30,.40,     &
       .05,.05,.05,.06,.07,.07,.08,.09,.10,.13,     &
       .05,.05,.05,.05,.06,.06,.06,.06,.07,.07,     &
       .05,.05,.05,.05,.05,.05,.05,.06,.06,.06,     &
       .04,.04,.04,.04,.04,.04,.04,.05,.05,.05/     
      DATA O3LO2/     &
       14.8,14.2,13.8,12.2,11.0,9.8,8.5,7.8,7.4,6.9,   &
       13.2,13.0,12.5,11.3,10.4,9.0,7.8,7.5,7.0,6.6,   &
       10.6,10.6,10.7,10.1,9.4,8.6,7.5,7.0,6.5,6.1,    &
       7.0,7.3,7.5,7.5,7.5,7.3,6.7,6.4,6.0,5.8,     &
       3.8,4.0,4.7,5.0,5.2,5.9,5.8,5.6,5.5,5.5,     &
       1.4,1.6,2.4,3.0,3.7,4.1,4.6,4.8,5.1,5.0,     &
       .40,.50,.90,1.2,2.0,2.7,3.2,3.6,4.3,4.1,     &
       .07,.10,.20,.30,.80,1.4,2.1,2.4,2.7,3.0,     &
       .06,.07,.09,.15,.30,.70,1.2,1.4,1.6,2.0,     &
       .05,.05,.06,.12,.15,.30,.60,.70,.80,.80,     &
       .04,.05,.06,.08,.09,.15,.30,.40,.40,.40,     &
       .04,.04,.05,.055,.06,.09,.12,.13,.15,.15,    &
       .03,.03,.045,.052,.055,.06,.07,.07,.06,.07,  &
       .03,.03,.04,.051,.052,.052,.06,.06,.05,.05,  &
       .02,.02,.03,.05,.05,.05,.04,.04,.04,.04,     &
       .02,.02,.02,.04,.04,.04,.03,.03,.03,.03/     
      DATA O3LO3/     &
       14.5,14.0,13.5,11.3,11.0,10.0,9.0,8.3,7.5,7.3,    &
       13.5,13.2,12.5,11.1,10.4,9.7,8.2,7.8,7.4,6.8,     &
       10.8,10.9,11.0,10.4,10.0,9.6,7.9,7.5,7.0,6.7,     &
       7.3,7.5,7.8,8.5,9.0,8.5,7.7,7.4,6.9,6.5,     &
       4.1,4.5,5.3,6.2,7.3,7.7,7.3,7.0,6.6,6.4,     &
       1.8,2.0,2.2,3.8,4.3,5.6,6.2,6.2,6.4,6.2,     &
       .30,.50,.60,1.5,2.8,3.7,4.5,4.7,5.5,5.6,     &
       .09,.10,.15,.60,1.2,2.1,3.0,3.5,4.0,4.3,     &
       .06,.08,.10,.30,.60,1.1,1.9,2.2,2.9,3.0,     &
       .04,.05,.06,.15,.45,.60,1.1,1.3,1.6,1.8,     &
       .04,.04,.04,.08,.20,.30,.55,.60,.75,.90,     &
       .04,.04,.04,.05,.06,.10,.12,.15,.20,.25,     &
       .04,.04,.03,.04,.05,.06,.07,.07,.07,.08,     &
       .03,.03,.04,.05,.05,.05,.05,.05,.05,.05,     &
       .03,.03,.03,.04,.04,.04,.05,.05,.04,.04,     &
       .02,.02,.02,.04,.04,.04,.04,.04,.03,.03/      
      DATA O3LO4/     &
       14.2,13.8,13.2,12.5,11.7,10.5,8.6,7.8,7.5,6.6,  &
       12.5,12.4,12.2,11.7,10.8,9.8,7.8,7.2,6.5,6.1,   &
       10.6,10.5,10.4,10.1,9.6,9.0,7.1,6.8,6.1,5.9,    &
       7.0,7.4,7.9,7.8,7.6,7.3,6.2,6.1,5.8,5.6,     &
       4.2,4.6,5.1,5.6,5.9,5.9,5.9,5.8,5.6,5.3,     &
       2.1,2.3,2.6,2.9,3.5,4.3,4.8,4.9,5.1,5.1,     &
       0.7,0.8,1.0,1.5,2.0,2.8,3.5,3.6,3.7,4.0,     &
       .15,.20,.40,.50,.60,1.4,2.1,2.2,2.3,2.5,     &
       .08,.10,.15,.25,.30,.90,1.2,1.3,1.4,1.6,     &
       .07,.08,.10,.14,.20,.50,.70,.90,.90,.80,     &
       .05,.06,.08,.12,.14,.20,.35,.40,.60,.50,     &
       .05,.05,.08,.09,.09,.09,.11,.12,.15,.18,     &
       .04,.05,.06,.07,.07,.08,.08,.08,.08,.08,     &
       .04,.04,.05,.07,.07,.07,.07,.07,.06,.05,     &
       .02,.02,.04,.05,.05,.05,.05,.05,.04,.04,     &
       .02,.02,.03,.04,.04,.04,.04,.04,.03,.03/     






      DO K=1,NK


        PH(K)=PYY(K)*1013250.         
        P(K)=PXX(K)*1013250.          
      ENDDO


      PH(NKP)=PYY(NKP)*1013250.       

      DO K=1,NL
        PSTD(K)=P(K)
      ENDDO

      DO K=1,25
      DO N=1,10
        RO31(N,K)=O3HI(N,K)
        RO32(N,K)=O3HI(N,K)
      ENDDO
      ENDDO

      DO 100 NCASE=1,4






      IPLACE=2
      IF(NCASE.EQ.2)IPLACE=4
      IF(NCASE.EQ.3)IPLACE=1
      IF(NCASE.EQ.4)IPLACE=3

      IF(NCASE.EQ.1.OR.NCASE.EQ.2)THEN
        DO K=26,41
        DO N=1,10
          RO31(N,K)=O3LO1(N,K-25)
          RO32(N,K)=O3LO2(N,K-25)
        ENDDO
        ENDDO
      ENDIF

      IF(NCASE.EQ.3.OR.NCASE.EQ.4)THEN
        DO K=26,41
        DO N=1,10
          RO31(N,K)=O3LO3(N,K-25)
          RO32(N,K)=O3LO4(N,K-25)
        ENDDO
        ENDDO
      ENDIF

      DO 25 KK=1,NKK
      DO N=1,10
        DUO3N(N,KK)=RO31(11-N,KK)
        DUO3N(N+9,KK)=RO32(N,KK)
      ENDDO
      DUO3N(10,KK)=0.5*(RO31(1,KK)+RO32(1,KK))
   25 CONTINUE



      IF(NCASE.EQ.2.OR.NCASE.EQ.4)THEN
        DO 50 KK=1,NKK
        DO N=1,19
          TEMPN(N)=DUO3N(20-N,KK)
        ENDDO
         DO N=1,19
           DUO3N(N,KK)=TEMPN(N)
         ENDDO
   50   CONTINUE
      ENDIF






      DO 75 N=1,19

      DO KK=1,NKK
        RSTD(KK)=DUO3N(N,KK)
      ENDDO

      NKM=NK-1
      NKMM=NK-3



      DO K=4,NKMM,2
        KI=K/2
        RDATA(K)=0.5*(RSTD(KI)+RSTD(KI+1))-(RSTD(KI+2)-RSTD(KI+1) &
                                           -RSTD(KI)+RSTD(KI-1))/16.
      ENDDO

      RDATA(2)=0.5*(RSTD(2)+RSTD(1))
      RDATA(NKM)=0.5*(RSTD(NKK)+RSTD(NKK-1))



      DO K=1,NK,2
        KQ=(K+1)/2
        RDATA(K)=RSTD(KQ)
      ENDDO

      DO KK=1,NL
        DDUO3N(N,KK)=RDATA(KK)*.01
      ENDDO

   75 CONTINUE








      DO 90 KK=1,NL

      DO N=1,19
        O35DEG(2*N-1,KK)=DDUO3N(N,KK)
      ENDDO

      DO N=1,18
        O35DEG(2*N,KK)=0.5*(DDUO3N(N,KK)+DDUO3N(N+1,KK))
      ENDDO

   90 CONTINUE

      DO JJ=1,37
      DO KEN=1,NL
        O3O3(JJ,KEN,IPLACE)=O35DEG(JJ,KEN)
      ENDDO
      ENDDO

  100 CONTINUE







      DO I=1,NLGTH
        AVG=0.25*(XRAD1(I)+XRAD2(I)+XRAD3(I)+XRAD4(I))
        A1=0.5*(XRAD2(I)-XRAD4(I))
        B1=0.5*(XRAD1(I)-XRAD3(I))
        B2=0.25*((XRAD1(I)+XRAD3(I))-(XRAD2(I)+XRAD4(I)))






        iindex = 1+mod((I-1),37)
        jindex = 1+(I-1)/37
        XDUO3N(iindex,jindex)=AVG
        XDO3N2(iindex,jindex)=A1
        XDO3N3(iindex,jindex)=B1
        XDO3N4(iindex,jindex)=B2
      ENDDO



      DO N=1,NL
        PRGFDL(N)=PSTD(N)*1.E-1
      ENDDO

    END SUBROUTINE O3CLIM


      SUBROUTINE TABLE 



 IMPLICIT NONE



 INTEGER, PARAMETER :: NB=12
 INTEGER, PARAMETER :: NBLX=47
 INTEGER , PARAMETER:: NBLW = 163

 REAL,PARAMETER ::      AMOLWT=28.9644
 REAL,PARAMETER ::      CSUBP=1.00484E7
 REAL,PARAMETER ::      DIFFCTR=1.66
 REAL,PARAMETER ::      G=980.665
 REAL,PARAMETER ::      GINV=1./G
 REAL,PARAMETER ::      GRAVDR=980.0
 REAL,PARAMETER ::      O3DIFCTR=1.90
 REAL,PARAMETER ::      P0=1013250.
 REAL,PARAMETER ::      P0INV=1./P0
 REAL,PARAMETER ::      GP0INV=GINV*P0INV
 REAL,PARAMETER ::      P0XZP2=202649.902
 REAL,PARAMETER ::      P0XZP8=810600.098
 REAL,PARAMETER ::      P0X2=2.*1013250.
 REAL,PARAMETER ::      RADCON=8.427
 REAL,PARAMETER ::      RADCON1=1./8.427
 REAL,PARAMETER ::      RATCO2MW=1.519449738
 REAL,PARAMETER ::      RATH2OMW=.622
 REAL,PARAMETER ::      RGAS=8.3142E7
 REAL,PARAMETER ::      RGASSP=8.31432E7
 REAL,PARAMETER ::      SECPDA=8.64E4



 REAL,PARAMETER ::      HUNDRED=100.
 REAL,PARAMETER ::      HNINETY=90.
 REAL,PARAMETER ::      HNINE=9.0
 REAL,PARAMETER ::      SIXTY=60.
 REAL,PARAMETER ::      FIFTY=50.
 REAL,PARAMETER ::      TEN=10.
 REAL,PARAMETER ::      EIGHT=8.
 REAL,PARAMETER ::      FIVE=5.
 REAL,PARAMETER ::      FOUR=4.
 REAL,PARAMETER ::      THREE=3.
 REAL,PARAMETER ::      TWO=2.
 REAL,PARAMETER ::      ONE=1.
 REAL,PARAMETER ::      HAF=0.5
 REAL,PARAMETER ::      QUARTR=0.25
 REAL,PARAMETER ::      ZERO=0.



 REAL,PARAMETER ::      H83E26=8.3E26
 REAL,PARAMETER ::      H71E26=7.1E26
 REAL,PARAMETER ::      H1E15=1.E15
 REAL,PARAMETER ::      H1E13=1.E13
 REAL,PARAMETER ::      H1E11=1.E11
 REAL,PARAMETER ::      H1E8=1.E8
 REAL,PARAMETER ::      H2E6=2.0E6
 REAL,PARAMETER ::      H1E6=1.0E6
 REAL,PARAMETER ::      H69766E5=6.97667E5
 REAL,PARAMETER ::      H4E5=4.E5
 REAL,PARAMETER ::      H165E5=1.65E5
 REAL,PARAMETER ::      H5725E4=57250.
 REAL,PARAMETER ::      H488E4=48800.
 REAL,PARAMETER ::      H1E4=1.E4
 REAL,PARAMETER ::      H24E3=2400.
 REAL,PARAMETER ::      H20788E3=2078.8
 REAL,PARAMETER ::      H2075E3=2075.
 REAL,PARAMETER ::      H18E3=1800.
 REAL,PARAMETER ::      H1224E3=1224.
 REAL,PARAMETER ::      H67390E2=673.9057
 REAL,PARAMETER ::      H5E2=500.
 REAL,PARAMETER ::      H3082E2=308.2
 REAL,PARAMETER ::      H3E2=300.
 REAL,PARAMETER ::      H2945E2=294.5
 REAL,PARAMETER ::      H29316E2=293.16
 REAL,PARAMETER ::      H26E2=260.0
 REAL,PARAMETER ::      H25E2=250.
 REAL,PARAMETER ::      H23E2=230.
 REAL,PARAMETER ::      H2E2=200.0
 REAL,PARAMETER ::      H15E2=150.
 REAL,PARAMETER ::      H1386E2=138.6
 REAL,PARAMETER ::      H1036E2=103.6
 REAL,PARAMETER ::      H8121E1=81.21
 REAL,PARAMETER ::      H35E1=35.
 REAL,PARAMETER ::      H3116E1=31.16
 REAL,PARAMETER ::      H28E1=28.
 REAL,PARAMETER ::      H181E1=18.1
 REAL,PARAMETER ::      H18E1=18.
 REAL,PARAMETER ::      H161E1=16.1
 REAL,PARAMETER ::      H16E1=16.
 REAL,PARAMETER ::      H1226E1=12.26
 REAL,PARAMETER ::      H9P94=9.94
 REAL,PARAMETER ::      H6P08108=6.081081081
 REAL,PARAMETER ::      H3P6=3.6
 REAL,PARAMETER ::      H3P5=3.5
 REAL,PARAMETER ::      H2P9=2.9
 REAL,PARAMETER ::      H2P8=2.8
 REAL,PARAMETER ::      H2P5=2.5
 REAL,PARAMETER ::      H1P8=1.8
 REAL,PARAMETER ::      H1P4387=1.4387
 REAL,PARAMETER ::      H1P41819=1.418191
 REAL,PARAMETER ::      H1P4=1.4
 REAL,PARAMETER ::      H1P25892=1.258925411
 REAL,PARAMETER ::      H1P082=1.082
 REAL,PARAMETER ::      HP816=0.816
 REAL,PARAMETER ::      HP805=0.805
 REAL,PARAMETER ::      HP8=0.8
 REAL,PARAMETER ::      HP60241=0.60241
 REAL,PARAMETER ::      HP602409=0.60240964
 REAL,PARAMETER ::      HP6=0.6
 REAL,PARAMETER ::      HP526315=0.52631579
 REAL,PARAMETER ::      HP518=0.518
 REAL,PARAMETER ::      HP5048=0.5048
 REAL,PARAMETER ::      HP3795=0.3795
 REAL,PARAMETER ::      HP369=0.369
 REAL,PARAMETER ::      HP26=0.26
 REAL,PARAMETER ::      HP228=0.228
 REAL,PARAMETER ::      HP219=0.219
 REAL,PARAMETER ::      HP166666=.166666
 REAL,PARAMETER ::      HP144=0.144
 REAL,PARAMETER ::      HP118666=0.118666192
 REAL,PARAMETER ::      HP1=0.1

 REAL,PARAMETER ::      H658M2=0.0658
 REAL,PARAMETER ::      H625M2=0.0625
 REAL,PARAMETER ::      H44871M2=4.4871E-2
 REAL,PARAMETER ::      H44194M2=.044194
 REAL,PARAMETER ::      H42M2=0.042
 REAL,PARAMETER ::      H41666M2=0.0416666
 REAL,PARAMETER ::      H28571M2=.02857142857
 REAL,PARAMETER ::      H2118M2=0.02118
 REAL,PARAMETER ::      H129M2=0.0129
 REAL,PARAMETER ::      H1M2=.01
 REAL,PARAMETER ::      H559M3=5.59E-3
 REAL,PARAMETER ::      H3M3=0.003
 REAL,PARAMETER ::      H235M3=2.35E-3
 REAL,PARAMETER ::      H1M3=1.0E-3
 REAL,PARAMETER ::      H987M4=9.87E-4
 REAL,PARAMETER ::      H323M4=0.000323
 REAL,PARAMETER ::      H3M4=0.0003
 REAL,PARAMETER ::      H285M4=2.85E-4
 REAL,PARAMETER ::      H1M4=0.0001
 REAL,PARAMETER ::      H75826M4=7.58265E-4
 REAL,PARAMETER ::      H6938M5=6.938E-5
 REAL,PARAMETER ::      H394M5=3.94E-5
 REAL,PARAMETER ::      H37412M5=3.7412E-5
 REAL,PARAMETER ::      H15M5=1.5E-5
 REAL,PARAMETER ::      H1439M5=1.439E-5
 REAL,PARAMETER ::      H128M5=1.28E-5
 REAL,PARAMETER ::      H102M5=1.02E-5
 REAL,PARAMETER ::      H1M5=1.0E-5
 REAL,PARAMETER ::      H7M6=7.E-6
 REAL,PARAMETER ::      H4999M6=4.999E-6
 REAL,PARAMETER ::      H451M6=4.51E-6
 REAL,PARAMETER ::      H25452M6=2.5452E-6
 REAL,PARAMETER ::      H1M6=1.E-6
 REAL,PARAMETER ::      H391M7=3.91E-7
 REAL,PARAMETER ::      H1174M7=1.174E-7
 REAL,PARAMETER ::      H8725M8=8.725E-8
 REAL,PARAMETER ::      H327M8=3.27E-8
 REAL,PARAMETER ::      H257M8=2.57E-8
 REAL,PARAMETER ::      H1M8=1.0E-8
 REAL,PARAMETER ::      H23M10=2.3E-10
 REAL,PARAMETER ::      H14M10=1.4E-10
 REAL,PARAMETER ::      H11M10=1.1E-10
 REAL,PARAMETER ::      H1M10=1.E-10
 REAL,PARAMETER ::      H83M11=8.3E-11
 REAL,PARAMETER ::      H82M11=8.2E-11
 REAL,PARAMETER ::      H8M11=8.E-11
 REAL,PARAMETER ::      H77M11=7.7E-11
 REAL,PARAMETER ::      H72M11=7.2E-11
 REAL,PARAMETER ::      H53M11=5.3E-11
 REAL,PARAMETER ::      H48M11=4.8E-11
 REAL,PARAMETER ::      H44M11=4.4E-11
 REAL,PARAMETER ::      H42M11=4.2E-11
 REAL,PARAMETER ::      H37M11=3.7E-11
 REAL,PARAMETER ::      H35M11=3.5E-11
 REAL,PARAMETER ::      H32M11=3.2E-11
 REAL,PARAMETER ::      H3M11=3.0E-11
 REAL,PARAMETER ::      H28M11=2.8E-11
 REAL,PARAMETER ::      H24M11=2.4E-11
 REAL,PARAMETER ::      H23M11=2.3E-11
 REAL,PARAMETER ::      H2M11=2.E-11
 REAL,PARAMETER ::      H18M11=1.8E-11
 REAL,PARAMETER ::      H15M11=1.5E-11
 REAL,PARAMETER ::      H14M11=1.4E-11
 REAL,PARAMETER ::      H114M11=1.14E-11
 REAL,PARAMETER ::      H11M11=1.1E-11
 REAL,PARAMETER ::      H1M11=1.E-11
 REAL,PARAMETER ::      H96M12=9.6E-12
 REAL,PARAMETER ::      H93M12=9.3E-12
 REAL,PARAMETER ::      H77M12=7.7E-12
 REAL,PARAMETER ::      H74M12=7.4E-12
 REAL,PARAMETER ::      H65M12=6.5E-12
 REAL,PARAMETER ::      H62M12=6.2E-12
 REAL,PARAMETER ::      H6M12=6.E-12
 REAL,PARAMETER ::      H45M12=4.5E-12
 REAL,PARAMETER ::      H44M12=4.4E-12
 REAL,PARAMETER ::      H4M12=4.E-12
 REAL,PARAMETER ::      H38M12=3.8E-12
 REAL,PARAMETER ::      H37M12=3.7E-12
 REAL,PARAMETER ::      H3M12=3.E-12
 REAL,PARAMETER ::      H29M12=2.9E-12
 REAL,PARAMETER ::      H28M12=2.8E-12
 REAL,PARAMETER ::      H24M12=2.4E-12
 REAL,PARAMETER ::      H21M12=2.1E-12
 REAL,PARAMETER ::      H16M12=1.6E-12
 REAL,PARAMETER ::      H14M12=1.4E-12
 REAL,PARAMETER ::      H12M12=1.2E-12
 REAL,PARAMETER ::      H8M13=8.E-13
 REAL,PARAMETER ::      H46M13=4.6E-13
 REAL,PARAMETER ::      H36M13=3.6E-13
 REAL,PARAMETER ::      H135M13=1.35E-13
 REAL,PARAMETER ::      H12M13=1.2E-13
 REAL,PARAMETER ::      H1M13=1.E-13
 REAL,PARAMETER ::      H3M14=3.E-14
 REAL,PARAMETER ::      H15M14=1.5E-14
 REAL,PARAMETER ::      H14M14=1.4E-14



 REAL,PARAMETER ::      HM2M2=-.02
 REAL,PARAMETER ::      HM6666M2=-.066667
 REAL,PARAMETER ::      HMP5=-0.5
 REAL,PARAMETER ::      HMP575=-0.575
 REAL,PARAMETER ::      HMP66667=-.66667
 REAL,PARAMETER ::      HMP805=-0.805
 REAL,PARAMETER ::      HM1EZ=-1.
 REAL,PARAMETER ::      HM13EZ=-1.3
 REAL,PARAMETER ::      HM19EZ=-1.9
 REAL,PARAMETER ::      HM1E1=-10.
 REAL,PARAMETER ::      HM1597E1=-15.97469413
 REAL,PARAMETER ::      HM161E1=-16.1
 REAL,PARAMETER ::      HM1797E1=-17.97469413
 REAL,PARAMETER ::      HM181E1=-18.1
 REAL,PARAMETER ::      HM8E1=-80.
 REAL,PARAMETER ::      HM1E2=-100.

 REAL,PARAMETER ::      H1M16=1.0E-16
 REAL,PARAMETER ::      H1M20=1.E-20
 REAL,PARAMETER ::      HP98=0.98
 REAL,PARAMETER ::      Q19001=19.001
 REAL,PARAMETER ::      DAYSEC=1.1574E-5
 REAL,PARAMETER ::      HSIGMA=5.673E-5
 REAL,PARAMETER ::      TWENTY=20.0
 REAL,PARAMETER ::      HP537=0.537
 REAL,PARAMETER ::      HP2=0.2
 REAL,PARAMETER ::      RCO2=3.3E-4
 REAL,PARAMETER ::      H3M6=3.0E-6
 REAL,PARAMETER ::      PI=3.1415927
 REAL,PARAMETER ::      DEGRAD1=180.0/PI
 REAL,PARAMETER ::      H74E1=74.0
 REAL,PARAMETER ::      H15E1=15.0

 REAL, PARAMETER:: B0 = -.51926410E-4
 REAL, PARAMETER:: B1 = -.18113332E-3
 REAL, PARAMETER:: B2 = -.10680132E-5
 REAL, PARAMETER:: B3 = -.67303519E-7
 REAL, PARAMETER:: AWIDE = 0.309801E+01
 REAL, PARAMETER:: BWIDE = 0.495357E-01
 REAL, PARAMETER:: BETAWD = 0.347839E+02
 REAL, PARAMETER:: BETINW = 0.766811E+01







      REAL :: ARNDM(NBLW),BRNDM(NBLW),BETAD(NBLW)
      REAL :: BANDLO(NBLW),BANDHI(NBLW)

      INTEGER :: IBAND(40)

      REAL :: BANDL1(64),BANDL2(64),BANDL3(35)
      REAL :: BANDH1(64),BANDH2(64),BANDH3(35) 















      REAL ::  &
               SUM(28,180),PERTSM(28,180),SUM3(28,180),       &
               SUMWDE(28,180),SRCWD(28,NBLX),SRC1NB(28,NBLW), &
               DBDTNB(28,NBLW)
      REAL ::  &
               ZMASS(181),ZROOT(181),SC(28),DSC(28),XTEMV(28), &
               TFOUR(28),FORTCU(28),X(28),X1(28),X2(180),SRCS(28), &
               SUM4(28),SUM6(28),SUM7(28),SUM8(28),SUM4WD(28),     &
               R1T(28),R2(28),S2(28),T3(28),R1WD(28)
      REAL ::  EXPO(180),FAC(180)
      REAL ::  CNUSB(30),DNUSB(30)
      REAL ::  ALFANB(NBLW),AROTNB(NBLW)
      REAL ::  ANB(NBLW),BNB(NBLW),CENTNB(NBLW),DELNB(NBLW), &
               BETANB(NBLW)

      REAL ::  AB15(2)

      REAL ::   ARNDM1(64),ARNDM2(64),ARNDM3(35)
      REAL ::   BRNDM1(64),BRNDM2(64),BRNDM3(35)
      REAL ::   BETAD1(64),BETAD2(64),BETAD3(35)

      EQUIVALENCE (ARNDM1(1),ARNDM(1)),(ARNDM2(1),ARNDM(65)), &
                  (ARNDM3(1),ARNDM(129))
      EQUIVALENCE (BRNDM1(1),BRNDM(1)),(BRNDM2(1),BRNDM(65)), &
                  (BRNDM3(1),BRNDM(129))
      EQUIVALENCE (BETAD1(1),BETAD(1)),(BETAD2(1),BETAD(65)), &
                  (BETAD3(1),BETAD(129))


      REAL    :: CENT,DEL,BDLO,BDHI,C1,ANU,tmp
      INTEGER :: N,I,ICNT,I1,I2E,I2
      INTEGER :: J,JP,NSUBDS,NSB,IA



      DATA IBAND  / &
          2,   1,   2,   2,   1,   2,   1,   3,   2,   2, &
          3,   2,   2,   4,   2,   4,   2,   3,   3,   2, &
          4,   3,   4,   3,   7,   5,   6,   7,   6,   5, &
          7,   6,   7,   8,   6,   6,   8,   8,   8,   8/

      DATA BANDL1 / &
         0.000000E+00,  0.100000E+02,  0.200000E+02,  0.300000E+02, &
         0.400000E+02,  0.500000E+02,  0.600000E+02,  0.700000E+02, &
         0.800000E+02,  0.900000E+02,  0.100000E+03,  0.110000E+03, &
         0.120000E+03,  0.130000E+03,  0.140000E+03,  0.150000E+03, &
         0.160000E+03,  0.170000E+03,  0.180000E+03,  0.190000E+03, &
         0.200000E+03,  0.210000E+03,  0.220000E+03,  0.230000E+03, &
         0.240000E+03,  0.250000E+03,  0.260000E+03,  0.270000E+03, &
         0.280000E+03,  0.290000E+03,  0.300000E+03,  0.310000E+03, &
         0.320000E+03,  0.330000E+03,  0.340000E+03,  0.350000E+03, &
         0.360000E+03,  0.370000E+03,  0.380000E+03,  0.390000E+03, &
         0.400000E+03,  0.410000E+03,  0.420000E+03,  0.430000E+03, &
         0.440000E+03,  0.450000E+03,  0.460000E+03,  0.470000E+03, &
         0.480000E+03,  0.490000E+03,  0.500000E+03,  0.510000E+03, &
         0.520000E+03,  0.530000E+03,  0.540000E+03,  0.550000E+03, &
         0.560000E+03,  0.670000E+03,  0.800000E+03,  0.900000E+03, &
         0.990000E+03,  0.107000E+04,  0.120000E+04,  0.121000E+04/
      DATA BANDL2 / &
         0.122000E+04,  0.123000E+04,  0.124000E+04,  0.125000E+04, &
         0.126000E+04,  0.127000E+04,  0.128000E+04,  0.129000E+04, &
         0.130000E+04,  0.131000E+04,  0.132000E+04,  0.133000E+04, &
         0.134000E+04,  0.135000E+04,  0.136000E+04,  0.137000E+04, &
         0.138000E+04,  0.139000E+04,  0.140000E+04,  0.141000E+04, &
         0.142000E+04,  0.143000E+04,  0.144000E+04,  0.145000E+04, &
         0.146000E+04,  0.147000E+04,  0.148000E+04,  0.149000E+04, &
         0.150000E+04,  0.151000E+04,  0.152000E+04,  0.153000E+04, &
         0.154000E+04,  0.155000E+04,  0.156000E+04,  0.157000E+04, &
         0.158000E+04,  0.159000E+04,  0.160000E+04,  0.161000E+04, &
         0.162000E+04,  0.163000E+04,  0.164000E+04,  0.165000E+04, &
         0.166000E+04,  0.167000E+04,  0.168000E+04,  0.169000E+04, &
         0.170000E+04,  0.171000E+04,  0.172000E+04,  0.173000E+04, &
         0.174000E+04,  0.175000E+04,  0.176000E+04,  0.177000E+04, &
         0.178000E+04,  0.179000E+04,  0.180000E+04,  0.181000E+04, &
         0.182000E+04,  0.183000E+04,  0.184000E+04,  0.185000E+04/
      DATA BANDL3 / &
         0.186000E+04,  0.187000E+04,  0.188000E+04,  0.189000E+04, &
         0.190000E+04,  0.191000E+04,  0.192000E+04,  0.193000E+04, &
         0.194000E+04,  0.195000E+04,  0.196000E+04,  0.197000E+04, &
         0.198000E+04,  0.199000E+04,  0.200000E+04,  0.201000E+04, &
         0.202000E+04,  0.203000E+04,  0.204000E+04,  0.205000E+04, &
         0.206000E+04,  0.207000E+04,  0.208000E+04,  0.209000E+04, &
         0.210000E+04,  0.211000E+04,  0.212000E+04,  0.213000E+04, &
         0.214000E+04,  0.215000E+04,  0.216000E+04,  0.217000E+04, &
         0.218000E+04,  0.219000E+04,  0.227000E+04/

      DATA BANDH1 / &
         0.100000E+02,  0.200000E+02,  0.300000E+02,  0.400000E+02, &
         0.500000E+02,  0.600000E+02,  0.700000E+02,  0.800000E+02, &
         0.900000E+02,  0.100000E+03,  0.110000E+03,  0.120000E+03, &
         0.130000E+03,  0.140000E+03,  0.150000E+03,  0.160000E+03, &
         0.170000E+03,  0.180000E+03,  0.190000E+03,  0.200000E+03, &
         0.210000E+03,  0.220000E+03,  0.230000E+03,  0.240000E+03, &
         0.250000E+03,  0.260000E+03,  0.270000E+03,  0.280000E+03, &
         0.290000E+03,  0.300000E+03,  0.310000E+03,  0.320000E+03, &
         0.330000E+03,  0.340000E+03,  0.350000E+03,  0.360000E+03, &
         0.370000E+03,  0.380000E+03,  0.390000E+03,  0.400000E+03, &
         0.410000E+03,  0.420000E+03,  0.430000E+03,  0.440000E+03, &
         0.450000E+03,  0.460000E+03,  0.470000E+03,  0.480000E+03, &
         0.490000E+03,  0.500000E+03,  0.510000E+03,  0.520000E+03, &
         0.530000E+03,  0.540000E+03,  0.550000E+03,  0.560000E+03, &
         0.670000E+03,  0.800000E+03,  0.900000E+03,  0.990000E+03, &
         0.107000E+04,  0.120000E+04,  0.121000E+04,  0.122000E+04/
      DATA BANDH2 / &
         0.123000E+04,  0.124000E+04,  0.125000E+04,  0.126000E+04, &
         0.127000E+04,  0.128000E+04,  0.129000E+04,  0.130000E+04, &
         0.131000E+04,  0.132000E+04,  0.133000E+04,  0.134000E+04, &
         0.135000E+04,  0.136000E+04,  0.137000E+04,  0.138000E+04, &
         0.139000E+04,  0.140000E+04,  0.141000E+04,  0.142000E+04, &
         0.143000E+04,  0.144000E+04,  0.145000E+04,  0.146000E+04, &
         0.147000E+04,  0.148000E+04,  0.149000E+04,  0.150000E+04, &
         0.151000E+04,  0.152000E+04,  0.153000E+04,  0.154000E+04, &
         0.155000E+04,  0.156000E+04,  0.157000E+04,  0.158000E+04, &
         0.159000E+04,  0.160000E+04,  0.161000E+04,  0.162000E+04, &
         0.163000E+04,  0.164000E+04,  0.165000E+04,  0.166000E+04, &
         0.167000E+04,  0.168000E+04,  0.169000E+04,  0.170000E+04, &
         0.171000E+04,  0.172000E+04,  0.173000E+04,  0.174000E+04, &
         0.175000E+04,  0.176000E+04,  0.177000E+04,  0.178000E+04, &
         0.179000E+04,  0.180000E+04,  0.181000E+04,  0.182000E+04, &
         0.183000E+04,  0.184000E+04,  0.185000E+04,  0.186000E+04/
      DATA BANDH3 / &
         0.187000E+04,  0.188000E+04,  0.189000E+04,  0.190000E+04, &
         0.191000E+04,  0.192000E+04,  0.193000E+04,  0.194000E+04, &
         0.195000E+04,  0.196000E+04,  0.197000E+04,  0.198000E+04, &
         0.199000E+04,  0.200000E+04,  0.201000E+04,  0.202000E+04, &
         0.203000E+04,  0.204000E+04,  0.205000E+04,  0.206000E+04, &
         0.207000E+04,  0.208000E+04,  0.209000E+04,  0.210000E+04, &
         0.211000E+04,  0.212000E+04,  0.213000E+04,  0.214000E+04, &
         0.215000E+04,  0.216000E+04,  0.217000E+04,  0.218000E+04, &
         0.219000E+04,  0.220000E+04,  0.238000E+04/




      DATA ARNDM1  / &
         0.354693E+00,  0.269857E+03,  0.167062E+03,  0.201314E+04, &
         0.964533E+03,  0.547971E+04,  0.152933E+04,  0.599429E+04, &
         0.699329E+04,  0.856721E+04,  0.962489E+04,  0.233348E+04, &
         0.127091E+05,  0.104383E+05,  0.504249E+04,  0.181227E+05, &
         0.856480E+03,  0.136354E+05,  0.288635E+04,  0.170200E+04, &
         0.209761E+05,  0.126797E+04,  0.110096E+05,  0.336436E+03, &
         0.491663E+04,  0.863701E+04,  0.540389E+03,  0.439786E+04, &
         0.347836E+04,  0.130557E+03,  0.465332E+04,  0.253086E+03, &
         0.257387E+04,  0.488041E+03,  0.892991E+03,  0.117148E+04, &
         0.125880E+03,  0.458852E+03,  0.142975E+03,  0.446355E+03, &
         0.302887E+02,  0.394451E+03,  0.438112E+02,  0.348811E+02, &
         0.615503E+02,  0.143165E+03,  0.103958E+02,  0.725108E+02, &
         0.316628E+02,  0.946456E+01,  0.542675E+02,  0.351557E+02, &
         0.301797E+02,  0.381010E+01,  0.126319E+02,  0.548010E+01, &
         0.600199E+01,  0.640803E+00,  0.501549E-01,  0.167961E-01, &
         0.178110E-01,  0.170166E+00,  0.273514E-01,  0.983767E+00/
      DATA ARNDM2  / &
         0.753946E+00,  0.941763E-01,  0.970547E+00,  0.268862E+00, &
         0.564373E+01,  0.389794E+01,  0.310955E+01,  0.128235E+01, &
         0.196414E+01,  0.247113E+02,  0.593435E+01,  0.377552E+02, &
         0.305173E+02,  0.852479E+01,  0.116780E+03,  0.101490E+03, &
         0.138939E+03,  0.324228E+03,  0.683729E+02,  0.471304E+03, &
         0.159684E+03,  0.427101E+03,  0.114716E+03,  0.106190E+04, &
         0.294607E+03,  0.762948E+03,  0.333199E+03,  0.830645E+03, &
         0.162512E+04,  0.525676E+03,  0.137739E+04,  0.136252E+04, &
         0.147164E+04,  0.187196E+04,  0.131118E+04,  0.103975E+04, &
         0.621637E+01,  0.399459E+02,  0.950648E+02,  0.943161E+03, &
         0.526821E+03,  0.104150E+04,  0.905610E+03,  0.228142E+04, &
         0.806270E+03,  0.691845E+03,  0.155237E+04,  0.192241E+04, &
         0.991871E+03,  0.123907E+04,  0.457289E+02,  0.146146E+04, &
         0.319382E+03,  0.436074E+03,  0.374214E+03,  0.778217E+03, &
         0.140227E+03,  0.562540E+03,  0.682685E+02,  0.820292E+02, &
         0.178779E+03,  0.186150E+03,  0.383864E+03,  0.567416E+01/ 
      DATA ARNDM3  / &
         0.225129E+03,  0.473099E+01,  0.753149E+02,  0.233689E+02, &
         0.339802E+02,  0.108855E+03,  0.380016E+02,  0.151039E+01, &
         0.660346E+02,  0.370165E+01,  0.234169E+02,  0.440206E+00, &
         0.615283E+01,  0.304077E+02,  0.117769E+01,  0.125248E+02, &
         0.142652E+01,  0.241831E+00,  0.483721E+01,  0.226357E-01, &
         0.549835E+01,  0.597067E+00,  0.404553E+00,  0.143584E+01, &
         0.294291E+00,  0.466273E+00,  0.156048E+00,  0.656185E+00, &
         0.172727E+00,  0.118349E+00,  0.141598E+00,  0.588581E-01, &
         0.919409E-01,  0.155521E-01,  0.537083E-02/
      DATA BRNDM1  / &
         0.789571E-01,  0.920256E-01,  0.696960E-01,  0.245544E+00, &
         0.188503E+00,  0.266127E+00,  0.271371E+00,  0.330917E+00, &
         0.190424E+00,  0.224498E+00,  0.282517E+00,  0.130675E+00, &
         0.212579E+00,  0.227298E+00,  0.138585E+00,  0.187106E+00, &
         0.194527E+00,  0.177034E+00,  0.115902E+00,  0.118499E+00, &
         0.142848E+00,  0.216869E+00,  0.149848E+00,  0.971585E-01, &
         0.151532E+00,  0.865628E-01,  0.764246E-01,  0.100035E+00, &
         0.171133E+00,  0.134737E+00,  0.105173E+00,  0.860832E-01, &
         0.148921E+00,  0.869234E-01,  0.106018E+00,  0.184865E+00, &
         0.767454E-01,  0.108981E+00,  0.123094E+00,  0.177287E+00, &
         0.848146E-01,  0.119356E+00,  0.133829E+00,  0.954505E-01, &
         0.155405E+00,  0.164167E+00,  0.161390E+00,  0.113287E+00, &
         0.714720E-01,  0.741598E-01,  0.719590E-01,  0.140616E+00, &
         0.355356E-01,  0.832779E-01,  0.128680E+00,  0.983013E-01, &
         0.629660E-01,  0.643346E-01,  0.717082E-01,  0.629730E-01, &
         0.875182E-01,  0.857907E-01,  0.358808E+00,  0.178840E+00/
      DATA BRNDM2  / &
         0.254265E+00,  0.297901E+00,  0.153916E+00,  0.537774E+00, &
         0.267906E+00,  0.104254E+00,  0.400723E+00,  0.389670E+00, &
         0.263701E+00,  0.338116E+00,  0.351528E+00,  0.267764E+00, &
         0.186419E+00,  0.238237E+00,  0.210408E+00,  0.176869E+00, &
         0.114715E+00,  0.173299E+00,  0.967770E-01,  0.172565E+00, &
         0.162085E+00,  0.157782E+00,  0.886832E-01,  0.242999E+00, &
         0.760298E-01,  0.164248E+00,  0.221428E+00,  0.166799E+00, &
         0.312514E+00,  0.380600E+00,  0.353828E+00,  0.269500E+00, &
         0.254759E+00,  0.285408E+00,  0.159764E+00,  0.721058E-01, &
         0.170528E+00,  0.231595E+00,  0.307184E+00,  0.564136E-01, &
         0.159884E+00,  0.147907E+00,  0.185666E+00,  0.183567E+00, &
         0.182482E+00,  0.230650E+00,  0.175348E+00,  0.195978E+00, &
         0.255323E+00,  0.198517E+00,  0.195500E+00,  0.208356E+00, &
         0.309603E+00,  0.112011E+00,  0.102570E+00,  0.128276E+00, &
         0.168100E+00,  0.177836E+00,  0.105533E+00,  0.903330E-01, &
         0.126036E+00,  0.101430E+00,  0.124546E+00,  0.221406E+00/ 
      DATA BRNDM3  / &
         0.137509E+00,  0.911365E-01,  0.724508E-01,  0.795788E-01, &
         0.137411E+00,  0.549175E-01,  0.787714E-01,  0.165544E+00, &
         0.136484E+00,  0.146729E+00,  0.820496E-01,  0.846211E-01, &
         0.785821E-01,  0.122527E+00,  0.125359E+00,  0.101589E+00, &
         0.155756E+00,  0.189239E+00,  0.999086E-01,  0.480993E+00, &
         0.100233E+00,  0.153754E+00,  0.130780E+00,  0.136136E+00, &
         0.159353E+00,  0.156634E+00,  0.272265E+00,  0.186874E+00, &
         0.192090E+00,  0.135397E+00,  0.131497E+00,  0.127463E+00, &
         0.227233E+00,  0.190562E+00,  0.214005E+00/ 
      DATA BETAD1  / &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.234879E+03,  0.217419E+03,  0.201281E+03,  0.186364E+03, &
         0.172576E+03,  0.159831E+03,  0.148051E+03,  0.137163E+03, &
         0.127099E+03,  0.117796E+03,  0.109197E+03,  0.101249E+03, &
         0.939031E+02,  0.871127E+02,  0.808363E+02,  0.750349E+02, &
         0.497489E+02,  0.221212E+02,  0.113124E+02,  0.754174E+01, &
         0.589554E+01,  0.495227E+01,  0.000000E+00,  0.000000E+00/ 
      DATA BETAD2  / &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00/ 
      DATA BETAD3  / &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00, &
         0.000000E+00,  0.000000E+00,  0.000000E+00/ 









 
      DO I = 1,64
         BANDLO(I)=BANDL1(I)
      ENDDO

      DO I = 65,128
         BANDLO(I)=BANDL2(I-64)
      ENDDO

      DO I = 129,163
         BANDLO(I)=BANDL3(I-128)
      ENDDO

      DO I = 1,64
         BANDHI(I)=BANDH1(I)
      ENDDO

      DO I = 65,128
         BANDHI(I)=BANDH2(I-64)
      ENDDO

      DO I = 129,163
         BANDHI(I)=BANDH3(I-128)
      ENDDO




      DO 101 N=1,NBLW
      ANB(N)=ARNDM(N)
      BNB(N)=BRNDM(N)
      CENTNB(N)=HAF*(BANDLO(N)+BANDHI(N))
      DELNB(N)=BANDHI(N)-BANDLO(N)
      BETANB(N)=BETAD(N)
101   CONTINUE
      AB15(1)=ANB(57)*BNB(57)
      AB15(2)=ANB(58)*BNB(58)

      AB15WD=AWIDE*BWIDE













117   CONTINUE


      SKC1R=BETAWD/BETINW
      SKO3R=BETAD(61)/BETINW
      SKO2D=ONE/BETINW









      ZMASS(1)=H1M16
      DO 201 J=1,180
      JP=J+1
      ZROOT(J)=SQRT(ZMASS(J))
      ZMASS(JP)=ZMASS(J)*H1P25892
201   CONTINUE
      DO 203 I=1,28
      XTEMV(I)=HNINETY+TEN*I
      TFOUR(I)=XTEMV(I)*XTEMV(I)*XTEMV(I)*XTEMV(I)
      FORTCU(I)=FOUR*XTEMV(I)*XTEMV(I)*XTEMV(I)
203   CONTINUE




      DO 205 N=1,NBLY
      DO 205 I=1,28
      SOURCE(I,N)=ZERO
205   CONTINUE
      DO 207 N=1,NBLX
      DO 207 I=1,28
      SRCWD(I,N)=ZERO
207   CONTINUE

      DO 211 N=1,NBLX
        IF (N.LE.46) THEN

          CENT=CENTNB(N+16)
          DEL=DELNB(N+16)
          BDLO=BANDLO(N+16)
          BDHI=BANDHI(N+16)
        ENDIF
        IF (N.EQ.NBLX) THEN

          CENT=CENTNB(NBLW)
          DEL=DELNB(NBLW)
          BDLO=BANDLO(NBLW)
          BDHI=BANDHI(NBLW)
        ENDIF


      NSUBDS=(DEL-H1M3)/10+1
      DO 213 NSB=1,NSUBDS
      IF (NSB.NE.NSUBDS) THEN
        CNUSB(NSB)=TEN*(NSB-1)+BDLO+FIVE
        DNUSB(NSB)=TEN
      ELSE
        CNUSB(NSB)=HAF*(TEN*(NSB-1)+BDLO+BDHI)
        DNUSB(NSB)=BDHI-(TEN*(NSB-1)+BDLO)
      ENDIF
      C1=(H37412M5)*CNUSB(NSB)**3

      DO 215 I=1,28
      X(I)=H1P4387*CNUSB(NSB)/XTEMV(I)
      X1(I)=EXP(X(I))
      SRCS(I)=C1/(X1(I)-ONE)
      SRCWD(I,N)=SRCWD(I,N)+SRCS(I)*DNUSB(NSB)
215   CONTINUE
213   CONTINUE
211   CONTINUE


      DO 221 N=1,40
      DO 221 I=1,28
      SOURCE(I,IBAND(N))=SOURCE(I,IBAND(N))+SRCWD(I,N)
221   CONTINUE
      DO 223 N=9,NBLY
      DO 223 I=1,28
      SOURCE(I,N)=SRCWD(I,N+32)
223   CONTINUE
      DO 225 N=1,NBLY
      DO 225 I=1,27
      DSRCE(I,N)=(SOURCE(I+1,N)-SOURCE(I,N))*HP1
225   CONTINUE
      DO 231 N=1,NBLW
      ALFANB(N)=BNB(N)*ANB(N)
      AROTNB(N)=SQRT(ALFANB(N))
231   CONTINUE





      DO 301 N=1,NBLW
      CENT=CENTNB(N)
      DEL=DELNB(N)








      DO 303 IA=2,2

      ANU=CENT+HAF*(IA-2)*DEL
      C1=(H37412M5)*ANU*ANU*ANU+H1M20

      DO 305 I=1,28
         X(I)=H1P4387*ANU/XTEMV(I)
         X1(I)=EXP(X(I))


         SC(I)=C1/((X1(I)-ONE)+H1M20)

         DSC(I)=SC(I)*SC(I)*X(I)*X1(I)/(XTEMV(I)*C1)
305      CONTINUE
      IF (IA.EQ.2) THEN
         DO 307 I=1,28
         SRC1NB(I,N)=DEL*SC(I)
         DBDTNB(I,N)=DEL*DSC(I)
307      CONTINUE
      ENDIF
303   CONTINUE
301   CONTINUE




      DO 311 I=1,28
      SUM4(I)=ZERO
      SUM6(I)=ZERO
      SUM7(I)=ZERO
      SUM8(I)=ZERO
      SUM4WD(I)=ZERO
311   CONTINUE
      DO 313 N=1,NBLW
      CENT=CENTNB(N)


      IF (CENT.LT.560. .OR. CENT.GT.1200..AND.CENT.LE.2200.) THEN
         DO 315 I=1,28
         SUM4(I)=SUM4(I)+SRC1NB(I,N)
         SUM6(I)=SUM6(I)+DBDTNB(I,N)
         SUM7(I)=SUM7(I)+DBDTNB(I,N)*AROTNB(N)
         SUM8(I)=SUM8(I)+DBDTNB(I,N)*ALFANB(N)
315      CONTINUE
      ENDIF

      IF (CENT.GT.160. .AND. CENT.LT.560.) THEN
         DO 316 I=1,28
         SUM4WD(I)=SUM4WD(I)+SRC1NB(I,N)
316      CONTINUE
      ENDIF
313   CONTINUE
      DO 317 I=1,28
      R1T(I)=SUM4(I)/TFOUR(I)
      R2(I)=SUM6(I)/FORTCU(I)
      S2(I)=SUM7(I)/FORTCU(I)
      T3(I)=SUM8(I)/FORTCU(I)
      R1WD(I)=SUM4WD(I)/TFOUR(I)
317   CONTINUE
      DO 401 J=1,180
      DO 401 I=1,28
      SUM(I,J)=ZERO
      PERTSM(I,J)=ZERO
      SUM3(I,J)=ZERO
      SUMWDE(I,J)=ZERO
401   CONTINUE

      DO 411 N=1,NBLW
      CENT=CENTNB(N)

      IF (CENT.LT.560. .OR. CENT.GT.1200..AND.CENT.LE.2200.) THEN
         DO 413 J=1,180
         X2(J)=AROTNB(N)*ZROOT(J)
         EXPO(J)=EXP(-X2(J))
413      CONTINUE
         DO 415 J=1,180
         IF (X2(J).GE.HUNDRED) THEN
              EXPO(J)=ZERO
         ENDIF
415      CONTINUE
         DO 417 J=121,180
         FAC(J)=ZMASS(J)*(ONE-(ONE+X2(J))*EXPO(J))/(X2(J)*X2(J))
417      CONTINUE
         DO 419 J=1,180
         DO 419 I=1,28
         SUM(I,J)=SUM(I,J)+SRC1NB(I,N)*EXPO(J)
         PERTSM(I,J)=PERTSM(I,J)+DBDTNB(I,N)*EXPO(J)
419      CONTINUE
         DO 421 J=121,180
         DO 421 I=1,28
         SUM3(I,J)=SUM3(I,J)+DBDTNB(I,N)*FAC(J)
421      CONTINUE
      ENDIF

      IF (CENT.GT.160. .AND. CENT.LT.560.) THEN
         DO 420 J=1,180
         DO 420 I=1,28
         SUMWDE(I,J)=SUMWDE(I,J)+SRC1NB(I,N)*EXPO(J)
420      CONTINUE
      ENDIF
411   CONTINUE
      DO 431 J=1,180
      DO 431 I=1,28
      EM1(I,J)=SUM(I,J)/TFOUR(I)
      TABLE1(I,J)=PERTSM(I,J)/FORTCU(I)
431   CONTINUE
      DO 433 J=121,180
      DO 433 I=1,28
      EM3(I,J)=SUM3(I,J)/FORTCU(I)
433   CONTINUE
      DO 441 J=1,179
      DO 441 I=1,28
      TABLE2(I,J)=(TABLE1(I,J+1)-TABLE1(I,J))*TEN
441   CONTINUE
      DO 443 J=1,180
      DO 443 I=1,27
      TABLE3(I,J)=(TABLE1(I+1,J)-TABLE1(I,J))*HP1
443   CONTINUE
      DO 445 I=1,28
      TABLE2(I,180)=ZERO
445   CONTINUE
      DO 447 J=1,180
      TABLE3(28,J)=ZERO
447   CONTINUE
      DO 449 J=1,2
      DO 449 I=1,28
      EM1(I,J)=R1T(I)
449   CONTINUE
      DO 451 J=1,120
      DO 451 I=1,28
      EM3(I,J)=R2(I)/TWO-S2(I)*SQRT(ZMASS(J))/THREE+T3(I)*ZMASS(J)/EIGHT
451   CONTINUE
      DO 453 J=121,180
      DO 453 I=1,28
      EM3(I,J)=EM3(I,J)/ZMASS(J)
453   CONTINUE


      DO 501 J=1,180
      DO 501 I=1,28
      EM1WDE(I,J)=SUMWDE(I,J)/TFOUR(I)
501   CONTINUE
      DO 503 J=1,2
      DO 503 I=1,28
      EM1WDE(I,J)=R1WD(I)
503   CONTINUE
   
      END SUBROUTINE TABLE


    SUBROUTINE SOLARD(IHRST,IDAY,MONTH,JULYR)

    IMPLICIT NONE











































     REAL, PARAMETER :: PI=3.1415926,PI2=2.*PI


      INTEGER, INTENT(IN ) :: IHRST,IDAY,MONTH,JULYR


      INTEGER :: NDM(12),JYR19,JMN
      REAL    :: CCR

      DATA JYR19/1900/, JMN/0/, CCR/1.3E-6/
      DATA NDM/0,31,59,90,120,151,181,212,243,273,304,334/
 




      REAL    :: TPP
      DATA TPP/1.55/

      INTEGER :: JDOR2,JDOR1
      DATA JDOR2/2415020/, JDOR1/2415019/

      REAL    :: DAYINC,DAT,T,YEAR,DATE,EM,E,EC,EP,CR,FJD,FJD1
      INTEGER :: JHR,JD,ITER










      JHR=IHRST

      JD=IDAY-32075                                                     &
             +1461*(JULYR+4800+(MONTH-14)/12)/4                         &
             +367*(MONTH-2-(MONTH-14)/12*12)/12                         &
             -3*((JULYR+4900+(MONTH-14)/12)/100)/4
      IF(JHR.LT.12)THEN
        JD=JD-1
        FJD=.5+.041666667*REAL(JHR)+.00069444444*REAL(JMN)
      ELSE
  7     FJD=.041666667E0*FLOAT(JHR-12)+.00069444444E0*FLOAT(JMN)
      END IF
      DAYINC=JHR/24.0
      FJD1=JD+FJD+DAYINC
      JD=FJD1
      FJD=FJD1-JD



      DAT=REAL(JD-JDOR2)-TPP+FJD



      T=FLOAT(JD-JDOR2)/36525.E0



      YEAR=.25964134E0+.304E-5*T



      EC=.01675104E0-(.418E-4+.126E-6*T)*T
      YEAR=YEAR+365.E0



      DATE = MOD(DAT,YEAR)



      EM=PI2*DATE/YEAR
      E=1.E0
      ITER = 0
 31   EP=E-(E-EC*SIN(E)-EM)/(1.E0-EC*COS(E))
      CR=ABS(E-EP)
      E=EP
      ITER = ITER + 1
      IF(ITER.GT.10) GOTO 1031
      IF(CR.GT.CCR) GO TO 31
 1031 CONTINUE
      R1=1.E0-EC*COS(E)

      WRITE(0,1000)JULYR,MONTH,IDAY,IHRST,R1
 1000 FORMAT('SUN-EARTH DISTANCE CALCULATION FINISHED IN SOLARD'/ &
             'YEAR=',I5,'  MONTH=',I3,'  DAY=',I3,' HOUR=' &
      ,      I3,' R1=',F9.4)



    END SUBROUTINE SOLARD

    SUBROUTINE CAL_MON_DAY(JULDAY,julyr,Jmonth,Jday)     

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: JULDAY,julyr
    INTEGER, INTENT(OUT) :: Jmonth,Jday
    LOGICAL :: LEAP,NOT_FIND_DATE
    INTEGER :: MONTH (12),itmpday,itmpmon,i

    DATA MONTH/31,28,31,30,31,30,31,31,30,31,30,31/

    NOT_FIND_DATE = .true.

    itmpday = JULDAY
    itmpmon = 1
    LEAP=.FALSE.
    IF(MOD(julyr,4).EQ.0)THEN
      MONTH(2)=29
      LEAP=.TRUE.
    ENDIF

    i = 1
    DO WHILE (NOT_FIND_DATE)
       IF(itmpday.GT.MONTH(i))THEN
         itmpday=itmpday-MONTH(i)
       ELSE
         Jday=itmpday
         Jmonth=i
         NOT_FIND_DATE = .false.
       ENDIF
       i = i+1
    END DO

    END SUBROUTINE CAL_MON_DAY



      FUNCTION ANTEMP(L,Z)
      REAL :: ZB(10,7),C(11,7),DELTA(10,7),TSTAR(7)

      DATA (ZB(N,1),N=1,10)/  2.0,   3.0,   16.5,  21.5,  45.0, &
                              51.0,  70.0,  100.,  200.,  300./
      DATA (C(N,1),N=1,11)/ -6.0,  -4.0,  -6.7,   4.0,   2.2,   &
                         1.0,  -2.8,  -.27,   0.0,   0.0,  0.0/
      DATA (DELTA(N,1),N=1,10)/.5,    .5,    .3,    .5,    1.0, &
                              1.0,   1.0,   1.0,   1.0,    1.0/

      DATA (ZB(N,2),N=1,10)/ 1.5,   6.5,  13.0,  18.0,  26.0, &
                              36.0,  48.0,  50.0, 70.0,  100./
      DATA (C(N,2),N=1,11)/ -4.0,  -6.0,  -6.5,   0.0,   1.2, &
                        2.2,   2.5,   0.0,  -3.0,  -0.25,  0.0/
      DATA (DELTA(N,2),N=1,10)/ .5,  1.0,    .5,    .5,   1.0, &
                              1.0,  2.5,    .5,   1.0,   1.0/

      DATA (ZB(N,3),N=1,10)/ 3.0,  10.0,  19.0,  25.0,  32.0, &
                              44.5, 50.0,  71.0,  98.0,  200.0/
      DATA (C(N,3),N=1,11)/ -3.5,  -6.0,  -0.5,  0.0,   0.4, &
                              3.2,   1.6,  -1.8, -0.7,   0.0,   0.0/
      DATA (DELTA(N,3),N=1,10)/ .5,   .5,  1.0,   1.0,   1.0, &
                              1.0,  1.0,  1.0,   1.0,   1.0/

      DATA (ZB(N,4),N=1,10)/ 4.7, 10.0,  23.0,  31.8,  44.0, &
                              50.2, 69.2, 100.0, 102.0, 103.0/
      DATA (C(N,4),N=1,11)/ -5.3, -7.0,   0.0,  1.4,   3.0, &
                               0.7, -3.3,  -0.2,  0.0,   0.0,  0.0/
      DATA (DELTA(N,4),N=1,10)/ .5,   .3,  1.0,   1.0,   2.0, &
                              1.0,  1.5,  1.0,   1.0,   1.0/

      DATA (ZB(N,5),N=1,10)/ 1.0,   3.2,   8.5,   15.5,   25.0, &
                              30.0,  35.0,  50.0,  70.0,  100.0/
      DATA (C(N,5),N=1,11)/ 3.0,  -3.2,  -6.8,  0.0,  -0.6, &
                              1.0,   1.2,   2.5, -0.7,  -1.2,  0.0/
      DATA (DELTA(N,5),N=1,10)/ .4,   1.5,    .3 ,   .5,   1.0, &
                              1.0,   1.0,   1.0,   1.0,   1.0/

      DATA (ZB(N,6),N=1,10)/ 11.0,  20.0,  32.0,  47.0,  51.0, & 
                             71.0,  84.8520,  90.0,  91.0,  92.0/
      DATA (C(N,6),N=1,11)/ -6.5,   0.0,   1.0,   2.80,  0.0, &
                             -2.80, -2.00,  0.0,   0.0,   0.0,  0.0/
      DATA (DELTA(N,6),N=1,10)/ 0.3,   1.0,   1.0,   1.0,   1.0, &
                              1.0,   1.0,   1.0,   1.0,   1.0/


      DATA (ZB(N,7),N=1,10)/ 11.0,  20.0,  32.0,  47.0,  51.0, &
                             71.0,  84.8520,  90.0,  91.0,  92.0/
      DATA (C(N,7),N=1,11)/ -6.5,   0.0,   1.0,   2.80,  0.0, &
                             -2.80, -2.00,  0.0,   0.0,   0.0,  0.0/
      DATA (DELTA(N,7),N=1,10)/ 0.3,   1.0,   1.0,   1.0,   1.0, &
                              1.0,   1.0,   1.0,   1.0,   1.0/

      DATA TSTAR/ 300.0,  294.0,  272.2,  287.0,  257.1, 2*288.15/

      NLAST=10
      TEMP=TSTAR(L)+C(1,L)*Z
      DO 20 N=1,NLAST
      EXPO=(Z-ZB(N,L))/DELTA(N,L)
      EXPP=ZB(N,L)/DELTA(N,L)










      IF(ABS(EXPO)-50.0) 23,23,24
   23 X=EXP(EXPO)
      Y=X+1.0/X
      ZLOG=ALOG(Y)
      GO TO 25
   24 ZLOG=ABS(EXPO)

   25 IF(EXPP-50.0) 27,27,28

   27 FAC=EXP(EXPP)+EXP(-EXPP)
      FACLOG=ALOG(FAC)
      GO TO 29
   28 FACLOG=EXPP


   29 TEMP=TEMP+(C(N+1,L)-C(N,L))*0.5*(Z+DELTA(N,L)* &
           (ZLOG-FACLOG))


   20 CONTINUE
      ANTEMP=TEMP

      END FUNCTION ANTEMP



      SUBROUTINE COEINT(RAT,IR)


















































      REAL RAT,SINV

      REAL PA2


      DIMENSION PATH0(109),ETAP(109),XAP(109),CAP(109)
      DIMENSION SINV(4)
      INTEGER :: IERR
      DATA SINV/2.74992,2.12731,4.38111,0.0832926/




      CORE=5.000
      UEXP=0.90

      DO 902 I=1,109
      PA2=PA(I)*PA(I)
      SEXPV(I)=.505+2.0E-5*PA(I)+.035*(PA2-.25)/(PA2+.25)
902   CONTINUE
      DO 900 I=1,109
      ETA(I)=3.2E-4*EXP(-PA(I)/500.)
      ETAP(I)=ETA(I)
900   CONTINUE
      DO 1200 NP=1,10
      DO 1000 I=3,109
      SEXP=SEXPV(I)
      R=(1.0D0-TRANSA(I,I-2))/(1.0D0-TRANSA(I,I-1))
      REXP=R**(UEXP/SEXP)
      arg1=path(pa(i),pa(i-2),core,eta(i))
      arg2=path(pa(i),pa(i-1),core,eta(i))
      PATHA=(PATH(PA(I),PA(I-2),CORE,ETA(I)))**UEXP
      PATHB=(PATH(PA(I),PA(I-1),CORE,ETA(I)))**UEXP
      XX=2.0D0*(PATHB*REXP-PATHA)/(PATHB*PATHB*REXP-PATHA*PATHA)
      DO 1010 LL=1,20
      F1=DLOG(1.0D0+XX*PATHA)
      F2=DLOG(1.0D0+XX*PATHB)
      F=F1/F2-REXP
      FPRIME=(F2*PATHA/(1.0D0+XX*PATHA)-F1*PATHB/(1.0D0+XX*PATHB))/ &
          (F2*F2)
      XX=XX-F/FPRIME
      CHECK=1.0D0+XX*PATHA

      IF(CHECK.LE.0.)THEN
        WRITE(errmess,360)I,LL,CHECK
        WRITE(errmess,*)' xx=',xx,' patha=',patha
  360   FORMAT(' ERROR,I=',I3,'LL=',I3,'CHECK=',F20.10)
        CALL wrf_error_fatal3("<stdin>",8176,&
errmess )
      ENDIF
 1010 CONTINUE
      CA(I)=(1.0D0-TRANSA(I,I-2))**(UEXP/SEXP)/ &
       (DLOG(1.0D0+XX*PATHA)+1.0D-20)
      XA(I)=XX
1000  CONTINUE
      XA(2)=XA(3)
      XA(1)=XA(3)
      CA(2)=CA(3)
      CA(1)=CA(3)
      DO 1100 I=3,109
      PATH0(I)=(PATH(PA(I),0.,CORE,ETA(I)))**UEXP
      PATH0(I)=1.0D0+XA(I)*PATH0(I)

1100  CONTINUE
      DO 1035 I=1,109
      SEXP=SEXPV(I)
      ETAP(I)=ETA(I)
      ETA(I)=(SINV(IR)/RAT)**(1./SEXP)* &
        (CA(I)*XA(I))**(1./UEXP)
1035  CONTINUE





















1200  CONTINUE
 361  FORMAT (' **WARNING:** 1+XA*PATH(PA(I),0) IS NEGATIVE,I= ',I3,/ &
       20X,'PATH0(I)=',F16.6,' XA(I)=',F16.6)
      RETURN
      END SUBROUTINE COEINT





      SUBROUTINE CO2INS(T22,T23,T66,IQ,L,LP1,iflag)






      DIMENSION T22(LP1,LP1,3),T23(LP1,LP1,3),T66(LP1,LP1,6)
      DIMENSION DCDT8(LP1,LP1),DCDT10(LP1,LP1),CO2PO(LP1,LP1), &
       CO2800(LP1,LP1),CO2PO1(LP1,LP1),CO2801(LP1,LP1),CO2PO2(LP1,LP1), &
       CO2802(LP1,LP1),N(LP1),D2CT8(LP1,LP1),D2CT10(LP1,LP1)




1011  FORMAT (4F20.14)






      DO 300 J=1,LP1
        DO 300 I=1,LP1
          CO2PO(I,J) = T22(I,J,1)

          IF (IQ.EQ.5) GO TO 300

          CO2PO1(I,J) = T22(I,J,2)
          CO2PO2(I,J) = T22(I,J,3)
  300 CONTINUE
      DO 301 J=1,LP1
        DO 301 I=1,LP1
          CO2800(I,J) = T23(I,J,1)

          IF (IQ.EQ.5) GO TO 301

          CO2801(I,J) = T23(I,J,2)
          CO2802(I,J) = T23(I,J,3)
  301 CONTINUE





















      IF (IQ.EQ.1) THEN
         C1=1.5
         C2x=0.5
      ENDIF
      IF (IQ.EQ.2) THEN
        C1=18./11.
        C2x=7./11.
      ENDIF
      IF (IQ.EQ.3) THEN
        C1=18./13.
        C2x=5./13.
      ENDIF
      IF (IQ.EQ.4) THEN
        C1=1.8
        C2x=0.8
      ENDIF

      IF (IQ.EQ.5) THEN
        C1=1.0
        C2x=0.0
      ENDIF

      DO 1021 I=1,LP1
      DO 1021 J=1,LP1
      CO2PO(J,I)=C1*CO2PO(J,I)-C2x
      CO2800(J,I)=C1*CO2800(J,I)-C2x

      IF (IQ.EQ.5) GO TO 1021

      CO2PO1(J,I)=C1*CO2PO1(J,I)-C2x
      CO2801(J,I)=C1*CO2801(J,I)-C2x
      CO2PO2(J,I)=C1*CO2PO2(J,I)-C2x
      CO2802(J,I)=C1*CO2802(J,I)-C2x
1021  CONTINUE

      IF (IQ.GE.1.AND.IQ.LE.4) THEN

      DO 1 J=1,LP1
      DO 1 I=1,LP1
      DCDT8(I,J)=.02*(CO2801(I,J)-CO2802(I,J))*100.
      DCDT10(I,J)=.02*(CO2PO1(I,J)-CO2PO2(I,J))*100.
      D2CT8(I,J)=.0016*(CO2801(I,J)+CO2802(I,J)-2.*CO2800(I,J))*1000.
      D2CT10(I,J)=.0016*(CO2PO1(I,J)+CO2PO2(I,J)-2.*CO2PO(I,J))*1000.
1     CONTINUE

      ENDIF












      IF (IQ.EQ.1.OR.IQ.EQ.4) THEN

      DO 400 J=1,LP1
       DO 400 I=1,LP1
        T66(I,J,1) = DCDT10(I,J)
        T66(I,J,2) = CO2PO(I,J)
        T66(I,J,3) = D2CT10(I,J)
        T66(I,J,4) = DCDT8(I,J)
        T66(I,J,5) = CO2800(I,J)
        T66(I,J,6) = D2CT8(I,J)
  400 CONTINUE

      ELSE
      DO 409 I=1,LP1
        T66(I,1,2) = CO2PO(1,I)
        T66(I,1,5) = CO2800(1,I)
        IF (IQ.EQ.5) GO TO 409
        T66(I,1,1) = DCDT10(1,I)
        T66(I,1,3) = D2CT10(1,I)
        T66(I,1,4) = DCDT8(1,I)
        T66(I,1,6) = D2CT8(1,I)
  409 CONTINUE
      ENDIF


      RETURN
      END SUBROUTINE CO2INS


      SUBROUTINE CO2INT(ITAPE,T15A,T15B,T22,RATIO,IR,NMETHD,NLEVLS,NLP1,NLP2)





















































































































































































































































































      COMMON/INPUT/P1,P2,TRNSLO,IA,JA,N




      DIMENSION TRNS(NLP1,NLP1)
      DIMENSION P(NLP1),PD(NLP2)
      DIMENSION PS(NLP1),PDS(NLP2),PLM(NLP1)
      DIMENSION NRTAB(3)
      DIMENSION T15A(NLP2,2),T15B(NLP1)
      DIMENSION T22(NLP1,NLP1,3)
      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
      DATA NRTAB/1,2,4/


100   FORMAT (4F20.14)
743   FORMAT (F20.14)
201   FORMAT (5E16.9)
202   FORMAT (I3)

203   FORMAT (F12.6)

102   FORMAT (4F20.14)
301   FORMAT (1X,8F15.8)




      REWIND ITAPE





      PA(1)=0.
      FACT15=10.**(1./15.)
      FACT30=10.**(1./30.)
      PA(2)=1.0E-3
      DO 231 I=2,76
      PA(I+1)=PA(I)*FACT15
231   CONTINUE
      DO 232 I=77,108
      PA(I+1)=PA(I)*FACT30
232   CONTINUE

      N=25
      NLV=NLEVLS
      NLP1V=NLP1
      NLP2V=NLP2














      IF (RATIO.EQ.1.0) GO TO 621
      CALL wrf_error_fatal3("<stdin>",8720,&
'SUBROUTINE CO2INT: 8746' )

621   ITAP1=ITAPE

      NTAP=1
      IF (NMETHD.EQ.2) GO TO 502


      DO 300 I=1,NLP1
        P(I)=T15B(I)
  300 CONTINUE
      DO 801 I=1,NLP1
      PS(I)=P(I)
801   CONTINUE
      GO TO 503
502   CONTINUE



      DO 303 I=1,NLP2
        PD(I)=T15A(I,1)
  303 CONTINUE
      DO 302 I=1,NLP1
        PLM(I)=T15A(I,2)
  302 CONTINUE
      DO 802 I=1,NLP1
      PDS(I)=PD(I+1)
      PS(I)=PLM(I)
802   CONTINUE

503   CONTINUE



      ICLOOP = 3
      IF (IR.EQ.4) ICLOOP = 1
      DO 400 KKK=1,ICLOOP


      IF (NMETHD.EQ.2) GO TO 505

      DO 803 I=1,NLP1
      P(I)=PS(I)
803   CONTINUE
      GO TO 506
505   CONTINUE

      DO 804 I=1,NLP1
      PD(I)=PDS(I)
      P(I)=PS(I)
804   CONTINUE

506   CONTINUE
      IA=108
      IAP=IA+1


        IF (NTAP.EQ.1) THEN
           IF ( wrf_dm_on_monitor() ) READ (ITAPE,743) ((TRANSA(I,J),I=1,109),J=1,109)
           CALL wrf_dm_bcast_bytes ( TRANSA , size ( TRANSA ) * 4 )
        ENDIF


	do J=109,1,-6

	enddo



      DO 4 I=1,IAP
      TRANSA(I,I)=1.0
    4 CONTINUE
      CALL COEINT(RATIO,IR)
      DO 805 I=1,NLP1
      DO 805 J=1,NLP1
      TRNS(J,I)=1.00
805   CONTINUE
      DO 10 I=1,NLP1
      DO 20 J=1,I
      IF (I.EQ.J) GO TO 20
      P1=P(J)
      P2=P(I)
      CALL SINTR2
      TRNS(J,I)=TRNSLO
20    CONTINUE
10    CONTINUE
      DO 47 I=1,NLP1
      DO 47 J=I,NLP1
      TRNS(J,I)=TRNS(I,J)
47    CONTINUE

      IF (NMETHD.EQ.1) GO TO 2872

      DO 51 J=1,NLP1
      DO 52 I=2,NLP1
      IA=I
      JA=J
      N=25
      IF (I.NE.J) N=3
      CALL QUADSR(NLV,NLP1V,NLP2V,P,PD,TRNS)
52    CONTINUE
51    CONTINUE

2872  CONTINUE



      DO 304 J=1,NLP1
       DO 304 I=1,NLP1
        T22(I,J,KKK) = TRNS(I,J)
  304 CONTINUE
400   CONTINUE
      RETURN
      END SUBROUTINE CO2INT

      SUBROUTINE CO2IN1(T20,T21,T66,IQ,L,LP1)






      DIMENSION T20(LP1,LP1,3),T21(LP1,LP1,3),T66(L,6)
      DIMENSION DCDT8(LP1,LP1),DCDT10(LP1,LP1),CO2PO(LP1,LP1), &
       CO2800(LP1,LP1),CO2PO1(LP1,LP1),CO2801(LP1,LP1),CO2PO2(LP1,LP1), &
       CO2802(LP1,LP1),N(LP1),D2CT8(LP1,LP1),D2CT10(LP1,LP1)
      ITIN=20
      ITIN1=21


1011  FORMAT (4F20.14)






      DO 300 J=1,LP1
        DO 300 I=1,LP1
          CO2PO(I,J) = T20(I,J,1)

          IF (IQ.EQ.5) GO TO 300

          CO2PO1(I,J) = T20(I,J,2)
          CO2PO2(I,J) = T20(I,J,3)
  300 CONTINUE
      DO 301 J=1,LP1
        DO 301 I=1,LP1
          CO2800(I,J) = T21(I,J,1)

          IF (IQ.EQ.5) GO TO 301

          CO2801(I,J) = T21(I,J,2)
          CO2802(I,J) = T21(I,J,3)
  301 CONTINUE












      IF (IQ.EQ.1) THEN
         C1=1.5
         C2x=0.5
      ENDIF
      IF (IQ.EQ.2) THEN
        C1=18./11.
        C2x=7./11.
      ENDIF
      IF (IQ.EQ.3) THEN
        C1=18./13.
        C2x=5./13.
      ENDIF
      IF (IQ.EQ.4) THEN
        C1=1.8
        C2x=0.8
      ENDIF

      IF (IQ.EQ.5) THEN
        C1=1.0
        C2x=0.0
      ENDIF

      DO 1021 I=1,LP1
      DO 1021 J=1,LP1
      CO2PO(J,I)=C1*CO2PO(J,I)-C2x
      CO2800(J,I)=C1*CO2800(J,I)-C2x

      IF (IQ.EQ.5) GO TO 1021

      CO2PO1(J,I)=C1*CO2PO1(J,I)-C2x
      CO2801(J,I)=C1*CO2801(J,I)-C2x
      CO2PO2(J,I)=C1*CO2PO2(J,I)-C2x
      CO2802(J,I)=C1*CO2802(J,I)-C2x
1021  CONTINUE

      IF (IQ.GE.1.AND.IQ.LE.4) THEN

      DO 1 J=1,LP1
      DO 1 I=1,LP1
      DCDT8(I,J)=.02*(CO2801(I,J)-CO2802(I,J))*100.
      DCDT10(I,J)=.02*(CO2PO1(I,J)-CO2PO2(I,J))*100.
      D2CT8(I,J)=.0016*(CO2801(I,J)+CO2802(I,J)-2.*CO2800(I,J))*1000.
      D2CT10(I,J)=.0016*(CO2PO1(I,J)+CO2PO2(I,J)-2.*CO2PO(I,J))*1000.
1     CONTINUE

      ENDIF












      DO 400 I=1,L
        T66(I,2) = CO2PO(I,I+1)
        T66(I,5) = CO2800(I,I+1)

        IF (IQ.EQ.5) GO TO 400

        T66(I,1) = DCDT10(I,I+1)
        T66(I,3) = D2CT10(I,I+1)
        T66(I,4) = DCDT8(I,I+1)
        T66(I,6) = D2CT8(I,I+1)
  400 CONTINUE
      RETURN
      END SUBROUTINE CO2IN1

      SUBROUTINE CO2PTZ(SGTEMP,T41,T42,T43,T44,SGLVNU,SIGLNU, &
                        SFULL,SHALF,PPTOP,LREAD,NL,NLP,NLP2)











      DIMENSION SGTEMP(NLP,2),T41(NLP2,2),T42(NLP), &
                T43(NLP2,2),T44(NLP)
      DIMENSION SGLVNU(NLP),SIGLNU(NL)
      DIMENSION SFULL(NLP),SHALF(NL)






      CHARACTER*20 PROFIL
      DIMENSION PRESS(NLP),TEMP(NLP),ALT(NLP),WMIX(NLP),O3MIX(NLP)
      DIMENSION WMXINT(NLP,4),WMXOUT(NLP2),OMXINT(NLP,4),OMXOUT(NLP2)
      DIMENSION PD(NLP2),GTEMP(NLP)
      DIMENSION PRS(NLP),TEMPS(NLP),PRSINT(NLP),TMPINT(NLP,4),A(NLP,4)
      DIMENSION PROUT(NLP2),TMPOUT(NLP2),TMPFLX(NLP2),TMPMID(NLP2)


      DATA PROFIL/ &
         'US STANDARD 1976'/
      DATA PSMAX/1013.250/





      NTYPE=0

    5 NLEV=NL
      DELZAP=0.5
      R=8.31432
      G0=9.80665
      ZMASS=28.9644
      AA=6356.766
         ALT(1)=0.0
         TEMP(1)=ANTEMP(6,0.0)

      PSTAR=PSMAX



      LTOP(1)=0
      LTOP(2)=0
      LTOP(3)=0
      DO 30 N=1,NL
        PCLD=(PSTAR-PPTOP*10.)*SHALF(N)+PPTOP*10.
        IF(PCLD.GE.642.)LTOP(1)=N
        IF(PCLD.GE.350.)LTOP(2)=N
        IF(PCLD.GE.150.)LTOP(3)=N

   30 CONTINUE





      NLM=NL-1
      CALL SIGP(PSTAR,PD,GTEMP,T41,T42,T43,T44,SGLVNU,SIGLNU, &
                SFULL,SHALF,PPTOP,LREAD,NL,NLP,NLM,NLP2)
      PD(NLP2)=PSTAR
      DO 40 N=1,NLP
      PRSINT(N)=PD(NLP2+1-N)
 40   CONTINUE

      DO 504 NQ=1,4
      DO 505 N=2,NLP
 505  PRESS(N)=PRSINT(N)+0.25*(NQ-1)*(PRSINT(N-1)-PRSINT(N))
      PRESS(1)=PRSINT(1)

      DO 100 N=1,NLEV




      DLOGP=7.0*ALOG(PRESS(N)/PRESS(N+1))
      NINT=DLOGP/DELZAP
      NINT=NINT+1
      ZNINT=NINT

      DZ=R*DLOGP/(7.0*ZMASS*G0*ZNINT)
      HT=ALT(N)




      DO 200 M=1,NINT
      RK1=ANTEMP(6,HT)*DZ
      RK2=ANTEMP(6,HT+0.5*RK1)*DZ
      RK3=ANTEMP(6,HT+0.5*RK2)*DZ
      RK4=ANTEMP(6,HT+RK3)*DZ

      HT=HT+0.16666667*(RK1+RK2+RK2+RK3+RK3+RK4)
  200 CONTINUE
      ALT(N+1)=HT
      TEMP(N+1)=ANTEMP(6,HT)
  100 CONTINUE
      DO 506 N=1,NLP
      TMPINT(N,NQ)=TEMP(N)
      A(N,NQ)=ALT(N)
506   CONTINUE
504   CONTINUE



      DO 901 N=1,NLP
        SGTEMP(N,1) = TMPINT(NLP2-N,1)
  901 CONTINUE



      DO 902 N=1,NLP
        SGTEMP(N,2) = GTEMP(N)
  902 CONTINUE

      RETURN
      END SUBROUTINE CO2PTZ
      FUNCTION PATH(A,B,C,E)



      PEXP=1./SEXP
      PATH=((A-B)**PEXP*(A+B+C))/(E*(A+B+C)+(A-B)**(PEXP-1.))
      RETURN
      END FUNCTION PATH

      SUBROUTINE QINTRP(XM,X0,XP,FM,F0,FP,X,F)


      D1=(FP-F0)/(XP-X0)
      D2=(FM-F0)/(XM-X0)
      B=(D1-D2)/(XP-XM)
      A=D1-B*(XP-X0)
      DEL=(X-X0)
      F=F0+DEL*(A+DEL*B)
      RETURN
      END SUBROUTINE QINTRP
      SUBROUTINE QUADSR(NLV,NLP1V,NLP2V,P,PD,TRNS)
      COMMON/INPUT/P1,P2,TRNSLO,IA,JA,N
      DIMENSION P(NLP1V),PD(NLP2V),TRNS(NLP1V,NLP1V)
      DIMENSION WT(101)
      N2=2*N
      N2P=2*N+1

      WT(1)=1.
      DO 21 I=1,N
      WT(2*I)=4.
      WT(2*I+1)=1.
21    CONTINUE
      IF (N.EQ.1) GO TO 25
      DO 22 I=2,N
      WT(2*I-1)=2.
22    CONTINUE
25    CONTINUE
      TRNSNB=0.
      DP=(PD(IA)-PD(IA-1))/N2
      PFIX=P(JA)
      DO 1 KK=1,N2P
      PVARY=PD(IA-1)+(KK-1)*DP
      IF (PVARY.GE.PFIX) P2=PVARY
      IF (PVARY.GE.PFIX) P1=PFIX
      IF (PVARY.LT.PFIX) P1=PVARY
      IF (PVARY.LT.PFIX) P2=PFIX
      CALL SINTR2
      TRNSNB=TRNSNB+TRNSLO*WT(KK)
1     CONTINUE
      TRNS(IA,JA)=TRNSNB*DP/(3.*(PD(IA)-PD(IA-1)))
      RETURN
      END SUBROUTINE QUADSR

      SUBROUTINE SIGP(PSTAR,PD,GTEMP,T41,T42,T43,T44,SGLVNU,SIGLNU, &
                      SIGLV,SIGLY,PPTOP,LREAD,KD,KP,KM,KP2)
      DIMENSION Q(KD),QMH(KP),PD(KP2),PLM(KP),GTEMP(KP),PDT(KP2)
      DIMENSION SIGLY(KD),SIGLV(KP)
      DIMENSION CI(KP),SGLVNU(KP),DEL(KD),SIGLNU(KD),CL(KD),RPI(KM)
      DIMENSION IDATE(4)
      DIMENSION T41(KP2,2),T42(KP), &
                T43(KP2,2),T44(KP)














12321 CONTINUE



























  701 FORMAT(F6.2)
  702 FORMAT(7F10.6)
      IF (PPTOP.LE.0.) GO TO 708
      PSFC=100.


      DO 706 K=1,KD
       SIGLY(K) = (SIGLY(K)*(PSFC-PPTOP)+PPTOP)/PSFC
  706 CONTINUE
      DO 707 K=1,KP
       SIGLV(K) = (SIGLV(K)*(PSFC-PPTOP)+PPTOP)/PSFC
  707 CONTINUE
  708 CONTINUE



  703 FORMAT(1H ,'PTOP =',F6.2)
  704 FORMAT(1H ,7F10.6)
      DO 913 K=1,KP
       SGLVNU(K) = SIGLV(K)
       IF (K.LE.KD) SIGLNU(K) = SIGLY(K)
  913 CONTINUE
      DO 77 K=1,KD
         Q(K) = SIGLNU(KD+1-K)
   77 CONTINUE
      PSS=    1013250.
      QMH(1)=0.
      QMH(KP)=1.
      DO 1 K=2,KD
      QMH(K)=0.5*(Q(K-1)+Q(K))
1     CONTINUE
      PD(1)=0.
      PD(KP2)=PSS
      DO 2 K=2,KP
      PD(K)=Q(K-1)*PSS
2     CONTINUE









      PLM(1)=0.
      DO 3 K=1,KM
      PLM(K+1)=0.5*(PD(K+1)+PD(K+2))
3     CONTINUE
      PLM(KP)=PSS
      DO 4 K=1,KD
      GTEMP(K)=PD(K+1)**0.2*(1.+PD(K+1)/30000.)**0.8/1013250.
4     CONTINUE
      GTEMP(KP)=0.





      DO 11 I=1,KP
      PD(I)=PD(I)*1.0E-3
      PLM(I)=PLM(I)*1.0E-3
11    CONTINUE
      PD(KP2)=PD(KP2)*1.0E-3



      DO 300 K=1,KP2
       T41(K,1) = PD(K)
  300 CONTINUE
      DO 301 K=1,KP
       T41(K,2) = PLM(K)
       T42(K) = PLM(K)
  301 CONTINUE

      DO 12 I=1,KP2
      PDT(I)=PD(I)
12    CONTINUE

      PSS=0.8*1013250.
      QMH(1)=0.
      QMH(KP)=1.
      DO 201 K=2,KD
      QMH(K)=0.5*(Q(K-1)+Q(K))
201   CONTINUE
      PD(1)=0.
      PD(KP2)=PSS
      DO 202 K=2,KP
      PD(K)=Q(K-1)*PSS
202   CONTINUE
      PLM(1)=0.
      DO 203 K=1,KM
      PLM(K+1)=0.5*(PD(K+1)+PD(K+2))
203   CONTINUE
      PLM(KP)=PSS




      DO 211 I=1,KP
      PD(I)=PD(I)*1.0E-3
      PLM(I)=PLM(I)*1.0E-3
211   CONTINUE
      PD(KP2)=PD(KP2)*1.0E-3



      DO 302 K=1,KP2
       T43(K,1) = PD(K)
  302 CONTINUE
      DO 303 K=1,KP
       T43(K,2) = PLM(K)
       T44(K) = PLM(K)
  303 CONTINUE

      DO 212 I=1,KP2
      PD(I)=PDT(I)
212   CONTINUE
100   FORMAT (1X,5E20.13)
101   FORMAT (5E16.9)
      RETURN
      END SUBROUTINE SIGP

      SUBROUTINE SINTR2



      COMMON/INPUT/P1,P2,TRNSLO,IA,JA,N



      DO 70 L=1,109
      IP1=L
      IF (P2-PA(L)) 65,65,70
   70 CONTINUE
   65 I=IP1-1
      IF (IP1.EQ.1) IP1=2
      IF (I.EQ.0) I=1
      DO 80 L=1,109
      JP1=L
      IF (P1-PA(L)) 75,75,80
   80 CONTINUE
   75 J=JP1-1
      IF (JP1.EQ.1) JP1=2
      IF (J.EQ.0) J=1
      JJJ=J
      III=I
      J=JJJ
      JP1=J+1
      I=III
      IP1=I+1


      PETA=P2
      DO 90 L=1,109
      IETAP1=L
      IF (PETA-PA(L)) 85,85,90
90    CONTINUE
85    IETA=IETAP1-1
      IF (IETAP1.EQ.1) IETAP1=2
      IF (IETA.EQ.0) IETA=1
      ETAP=ETA(IETA)+(PETA-PA(IETA))*(ETA(IETAP1)-ETA(IETA))/ &
       (PA(IETAP1)-PA(IETA))
      SEXP=SEXPV(IETA)+(PETA-PA(IETA))*(SEXPV(IETAP1)- &
       SEXPV(IETA))/ (PA(IETAP1)-PA(IETA))
      PIPMPI=PA(IP1)-PA(I)
      UP2P1=(PATH(P2,P1,CORE,ETAP))**UEXP
      IF (I-J) 126,126,127
  126 CONTINUE
      TRIP=(CA(IP1)*DLOG(1.0D0+XA(IP1)*UP2P1))**(SEXP/UEXP)
      TRI=(CA(I)*DLOG(1.0D0+XA(I)*UP2P1))**(SEXP/UEXP)
      TRNSLO=1.0D0-((PA(IP1)-P2)*TRI+(P2-PA(I))*TRIP)/PIPMPI
      GO TO 128
  127 TIJ=TRANSA(I,J)
      TIPJ=TRANSA(I+1,J)
      TIJP=TRANSA(I,J+1)
      TIPJP=TRANSA(I+1,J+1)
      UIJ=(PATH(PA(I),PA(J),CORE,ETAP))**UEXP
      UIPJ=(PATH(PA(I+1),PA(J),CORE,ETAP))**UEXP
      UIJP=(PATH(PA(I),PA(J+1),CORE,ETAP))**UEXP
      UIPJP=(PATH(PA(I+1),PA(J+1),CORE,ETAP))**UEXP
      PRODI=CA(I)*XA(I)
      PRODIP=CA(I+1)*XA(I+1)
      PROD=((PA(I+1)-P2)*PRODI+(P2-PA(I))*PRODIP)/PIPMPI
      XINT=((PA(I+1)-P2)*XA(I)+(P2-PA(I))*XA(I+1))/PIPMPI
      CINT=PROD/XINT
      AIJ=(CINT*DLOG(1.0D0+XINT*UIJ))**(SEXP/UEXP)
      AIJP=(CINT*DLOG(1.0D0+XINT*UIJP))**(SEXP/UEXP)
      AIPJ=(CINT*DLOG(1.0D0+XINT*UIPJ))**(SEXP/UEXP)
      AIPJP=(CINT*DLOG(1.0D0+XINT*UIPJP))**(SEXP/UEXP)
      EIJ=TIJ+AIJ
      EIPJ=TIPJ+AIPJ
      EIJP=TIJP+AIJP
      EIPJP=TIPJP+AIPJP
      DTDJ=(EIJP-EIJ)/(PA(J+1)-PA(J))
      DTDPJ=(EIPJP-EIPJ)/(PA(J+1)-PA(J))
      EPIP1=EIJ+DTDJ*(P1-PA(J))
      EPIPP1=EIPJ+DTDPJ*(P1-PA(J))
      EPP2P1=((PA(I+1)-P2)*EPIP1+(P2-PA(I))*EPIPP1)/PIPMPI
      TRNSLO=EPP2P1-(CINT*DLOG(1.0D0+XINT*UP2P1))**(SEXP/UEXP)
      IF (I.GE.108.OR.J.GE.108) GO TO 350
      IF (I-J-2) 350,350,355
355   CONTINUE
      TIP2J=TRANSA(I+2,J)
      TIP2JP=TRANSA(I+2,J+1)
      TI2J2=TRANSA(I+2,J+2)
      TIJP2=TRANSA(I,J+2)
      TIPJP2=TRANSA(I+1,J+2)
      UIP2J=(PATH(PA(I+2),PA(J),CORE,ETAP))**UEXP
      UIJP2=(PATH(PA(I),PA(J+2),CORE,ETAP))**UEXP
      UIPJP2=(PATH(PA(I+1),PA(J+2),CORE,ETAP))**UEXP
      UI2J2=(PATH(PA(I+2),PA(J+2),CORE,ETAP))**UEXP
      UIP2JP=(PATH(PA(I+2),PA(J+1),CORE,ETAP))**UEXP
      AIJP2=(CINT*DLOG(1.0D0+XINT*UIJP2))**(SEXP/UEXP)
      AIPJP2=(CINT*DLOG(1.0D0+XINT*UIPJP2))**(SEXP/UEXP)
      AIP2J=(CINT*DLOG(1.0D0+XINT*UIP2J))**(SEXP/UEXP)
      AIP2JP=(CINT*DLOG(1.0D0+XINT*UIP2JP))**(SEXP/UEXP)
      AI2J2=(CINT*DLOG(1.0D0+XINT*UI2J2))**(SEXP/UEXP)
      EIP2J=TIP2J+AIP2J
      EIP2JP=TIP2JP+AIP2JP
      EIJP2=TIJP2+AIJP2
      EIPJP2=TIPJP2+AIPJP2
      EI2J2=TI2J2+AI2J2
      CALL QINTRP(PA(J),PA(J+1),PA(J+2),EIJ,EIJP,EIJP2,P1,EI)
      CALL QINTRP(PA(J),PA(J+1),PA(J+2),EIPJ,EIPJP,EIPJP2,P1,EP)
      CALL QINTRP(PA(J),PA(J+1),PA(J+2),EIP2J,EIP2JP,EI2J2,P1,EP2)
      CALL QINTRP(PA(I),PA(I+1),PA(I+2),EI,EP,EP2,P2,EPSIL)
      TRNSLO=EPSIL-(CINT*DLOG(1.0D0+XINT*UP2P1))**(SEXP/UEXP)
  350 CONTINUE
  128 CONTINUE
  205 CONTINUE
      RETURN
      END SUBROUTINE SINTR2
      SUBROUTINE CO2O3(SFULL,SHALF,PPTOP,L,LP1,LP2)





      LOGICAL                 :: opened
      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
      CHARACTER*80 errmess


      INTEGER etarad_unit61, etarad_unit62, etarad_unit63,IERROR
      DIMENSION SGTEMP(LP1,2),CO2D1D(L,6),CO2D2D(LP1,LP1,6)

      DIMENSION CO2IQ2(LP1,LP1,6),CO2IQ3(LP1,LP1,6),CO2IQ5(LP1,LP1,6)

      DIMENSION T41(LP2,2),T42(LP1), &
                T43(LP2,2),T44(LP1)
      DIMENSION T20(LP1,LP1,3),T21(LP1,LP1,3)
      DIMENSION T22(LP1,LP1,3),T23(LP1,LP1,3)
      DIMENSION SGLVNU(LP1),SIGLNU(L)
      DIMENSION SFULL(LP1),SHALF(L)






























      IF(ALLOCATED (CO251))DEALLOCATE(CO251)
      IF(ALLOCATED (CDT51))DEALLOCATE(CDT51)
      IF(ALLOCATED (C2D51))DEALLOCATE(C2D51)
      IF(ALLOCATED (CO258))DEALLOCATE(CO258)
      IF(ALLOCATED (CDT58))DEALLOCATE(CDT58)
      IF(ALLOCATED (C2D58))DEALLOCATE(C2D58)
      IF(ALLOCATED (STEMP))DEALLOCATE(STEMP)
      IF(ALLOCATED (GTEMP))DEALLOCATE(GTEMP)
      IF(ALLOCATED (CO231))DEALLOCATE(CO231)
      IF(ALLOCATED (CDT31))DEALLOCATE(CDT31)
      IF(ALLOCATED (C2D31))DEALLOCATE(C2D31)
      IF(ALLOCATED (CO238))DEALLOCATE(CO238)
      IF(ALLOCATED (CDT38))DEALLOCATE(CDT38)
      IF(ALLOCATED (C2D38))DEALLOCATE(C2D38)
      IF(ALLOCATED (CO271))DEALLOCATE(CO271)
      IF(ALLOCATED (CDT71))DEALLOCATE(CDT71)
      IF(ALLOCATED (C2D71))DEALLOCATE(C2D71)
      IF(ALLOCATED (CO278))DEALLOCATE(CO278)
      IF(ALLOCATED (CDT78))DEALLOCATE(CDT78)
      IF(ALLOCATED (C2D78))DEALLOCATE(C2D78)
      IF(ALLOCATED (CO2M51))DEALLOCATE(CO2M51)
      IF(ALLOCATED (CDTM51))DEALLOCATE(CDTM51)
      IF(ALLOCATED (C2DM51))DEALLOCATE(C2DM51)
      IF(ALLOCATED (CO2M58))DEALLOCATE(CO2M58)
      IF(ALLOCATED (CDTM58))DEALLOCATE(CDTM58)
      IF(ALLOCATED (C2DM58))DEALLOCATE(C2DM58)

      ALLOCATE(CO251(LP1,LP1))
      ALLOCATE(CDT51(LP1,LP1))
      ALLOCATE(C2D51(LP1,LP1))
      ALLOCATE(CO258(LP1,LP1))
      ALLOCATE(CDT58(LP1,LP1))
      ALLOCATE(C2D58(LP1,LP1))
      ALLOCATE(STEMP(LP1))
      ALLOCATE(GTEMP(LP1))
      ALLOCATE(CO231(LP1))
      ALLOCATE(CDT31(LP1))
      ALLOCATE(C2D31(LP1))
      ALLOCATE(CO238(LP1))
      ALLOCATE(CDT38(LP1))
      ALLOCATE(C2D38(LP1))
      ALLOCATE(CO271(LP1))
      ALLOCATE(CDT71(LP1))
      ALLOCATE(C2D71(LP1))
      ALLOCATE(CO278(LP1))
      ALLOCATE(CDT78(LP1))
      ALLOCATE(C2D78(LP1))
      ALLOCATE(CO2M51(L))
      ALLOCATE(CDTM51(L))
      ALLOCATE(C2DM51(L))
      ALLOCATE(CO2M58(L))
      ALLOCATE(CDTM58(L))
      ALLOCATE(C2DM58(L))
      IF ( wrf_dm_on_monitor() ) THEN
        DO i = 61,99
          INQUIRE ( i , OPENED = opened )
          IF ( .NOT. opened ) THEN
            etarad_unit61 = i
            GOTO 2061
          ENDIF
        ENDDO
        etarad_unit61 = -1
 2061   CONTINUE
        DO i = 62,99
          INQUIRE ( i , OPENED = opened )
          IF ( .NOT. opened ) THEN
            etarad_unit62 = i
            GOTO 2062
          ENDIF
        ENDDO
        etarad_unit62 = -1
 2062   CONTINUE
        DO i = 63,99
          INQUIRE ( i , OPENED = opened )
          IF ( .NOT. opened ) THEN
            etarad_unit63 = i
            GOTO 2063
          ENDIF
        ENDDO
        etarad_unit63 = -1
 2063   CONTINUE
      ENDIF
      CALL wrf_dm_bcast_bytes ( etarad_unit61 , 4 )
      IF ( etarad_unit61 < 0 ) THEN
        CALL wrf_error_fatal3("<stdin>",9561,&
'module_ra_gfdleta: co2o3: Can not find unused fortran unit to read in lookup table.' )
      ENDIF
      CALL wrf_dm_bcast_bytes ( etarad_unit62 , 4 )
      IF ( etarad_unit62 < 0 ) THEN
        CALL wrf_error_fatal3("<stdin>",9566,&
'module_ra_gfdleta: co2o3: Can not find unused fortran unit to read in lookup table.' )
      ENDIF
      CALL wrf_dm_bcast_bytes ( etarad_unit63 , 4 )
      IF ( etarad_unit63 < 0 ) THEN
        CALL wrf_error_fatal3("<stdin>",9571,&
'module_ra_gfdleta: co2o3: Can not find unused fortran unit to read in lookup table.' )
      ENDIF
        IF ( wrf_dm_on_monitor() ) THEN
          OPEN(etarad_unit61,FILE='tr49t85',                  &
               FORM='FORMATTED',STATUS='OLD',ERR=9061,IOSTAT=IERROR)
        ENDIF
        IF ( wrf_dm_on_monitor() ) THEN
          OPEN(etarad_unit62,FILE='tr49t67',                  &
               FORM='FORMATTED',STATUS='OLD',ERR=9062,IOSTAT=IERROR)
        ENDIF
        IF ( wrf_dm_on_monitor() ) THEN
          OPEN(etarad_unit63,FILE='tr67t85',                  &
               FORM='FORMATTED',STATUS='OLD',ERR=9063,IOSTAT=IERROR)
        ENDIF


      LREAD = 0




      CALL CO2PTZ(SGTEMP,T41,T42,T43,T44,SGLVNU,SIGLNU, &
                  SFULL,SHALF,PPTOP,LREAD,L,LP1,LP2)






      DO K=1,LP1
        STEMP(K)=SGTEMP(K,1)
        GTEMP(K)=SGTEMP(K,2)
      ENDDO





      ICO2TP=etarad_unit61

      IR = 1
      RATIO = 1.0
      NMETHD = 2
      CALL CO2INT(ICO2TP,T41,T42,T22,RATIO,IR,NMETHD,L,LP1,LP2)
      IR = 1
      RATIO = 1.0
      NMETHD = 1
      CALL CO2INT(ICO2TP,T41,T42,T20,RATIO,IR,NMETHD,L,LP1,LP2)
      IR = 1
      RATIO = 1.0
      NMETHD = 2
      CALL CO2INT(ICO2TP,T43,T44,T23,RATIO,IR,NMETHD,L,LP1,LP2)
      IR = 1
      RATIO = 1.0
      NMETHD = 1
      CALL CO2INT(ICO2TP,T43,T44,T21,RATIO,IR,NMETHD,L,LP1,LP2)







      IQ = 1
      CALL CO2IN1(T20,T21,CO2D1D,IQ,L,LP1)



      DO K=1,L
        CDTM51(K)=CO2D1D(K,1)
        CO2M51(K)=CO2D1D(K,2)
        C2DM51(K)=CO2D1D(K,3)
        CDTM58(K)=CO2D1D(K,4)
        CO2M58(K)=CO2D1D(K,5)
        C2DM58(K)=CO2D1D(K,6)
      ENDDO









      CALL CO2INS(T22,T23,CO2D2D,IQ,L,LP1,1)



      DO K1=1,LP1
      DO K2=1,LP1
        CDT51(K1,K2)=CO2D2D(K1,K2,1)
        CO251(K1,K2)=CO2D2D(K1,K2,2)
        C2D51(K1,K2)=CO2D2D(K1,K2,3)
        CDT58(K1,K2)=CO2D2D(K1,K2,4)
        CO258(K1,K2)=CO2D2D(K1,K2,5)
        C2D58(K1,K2)=CO2D2D(K1,K2,6)
      ENDDO
      ENDDO






      ICO2TP=etarad_unit62
      IR = 2
      RATIO = 1.0
      NMETHD = 2
      CALL CO2INT(ICO2TP,T41,T42,T22,RATIO,IR,NMETHD,L,LP1,LP2)
      CALL CO2INT(ICO2TP,T43,T44,T23,RATIO,IR,NMETHD,L,LP1,LP2)
      IQ = 2
      CALL CO2INS(T22,T23,CO2IQ2,IQ,L,LP1,2)



      DO K=1,LP1
        CDT31(K)=CO2IQ2(K,1,1)
        CO231(K)=CO2IQ2(K,1,2)
        C2D31(K)=CO2IQ2(K,1,3)
        CDT38(K)=CO2IQ2(K,1,4)
        CO238(K)=CO2IQ2(K,1,5)
        C2D38(K)=CO2IQ2(K,1,6)
      ENDDO




      ICO2TP=etarad_unit63
      IR = 3
      RATIO = 1.0
      NMETHD = 2
      CALL CO2INT(ICO2TP,T41,T42,T22,RATIO,IR,NMETHD,L,LP1,LP2)
      CALL CO2INT(ICO2TP,T43,T44,T23,RATIO,IR,NMETHD,L,LP1,LP2)
      IQ = 3
      CALL CO2INS(T22,T23,CO2IQ3,IQ,L,LP1,3)




      DO K=1,LP1
        CDT71(K)=CO2IQ3(K,1,1)
        CO271(K)=CO2IQ3(K,1,2)
        C2D71(K)=CO2IQ3(K,1,3)
        CDT78(K)=CO2IQ3(K,1,4)
        CO278(K)=CO2IQ3(K,1,5)
        C2D78(K)=CO2IQ3(K,1,6)
      ENDDO





















         IF ( wrf_dm_on_monitor() ) THEN
           CLOSE (etarad_unit61)
           CLOSE (etarad_unit62)
           CLOSE (etarad_unit63)
         ENDIF

      RETURN
9061 CONTINUE
     WRITE( errmess , '(A49,I4)' ) 'module_ra_gfdleta: error reading tr49t85 on unit ',etarad_unit61
     write(0,*)' IERROR=',IERROR
     CALL wrf_error_fatal3("<stdin>",9751,&
errmess)
9062 CONTINUE
     WRITE( errmess , '(A49,I4)' ) 'module_ra_gfdleta: error reading tr49t67 on unit ',etarad_unit62
     write(0,*)' IERROR=',IERROR
     CALL wrf_error_fatal3("<stdin>",9756,&
errmess)
9063 CONTINUE
     WRITE( errmess , '(A49,I4)' ) 'module_ra_gfdleta: error reading tr67t85 on unit ',etarad_unit63
     write(0,*)' IERROR=',IERROR
     CALL wrf_error_fatal3("<stdin>",9761,&
errmess)
      END SUBROUTINE CO2O3





      SUBROUTINE CONRAD(KDS,KDE,KMS,KME,KTS,KTE)









      IMPLICIT NONE

      INTEGER,INTENT(IN) :: KDS,KDE,KMS,KME,KTS,KTE


      INTEGER :: I,I1,I2,IERROR,IRTN,J,K,KK,L,LP1,N,NUNIT_CO2,RSIZE
      INTEGER,DIMENSION(3) :: RSZE

      REAL,DIMENSION(KMS:KME-1,6) :: CO21D
      REAL,DIMENSION(KMS:KME,2) :: SGTMP
      REAL,DIMENSION(KMS:KME,6) :: CO21D3,CO21D7
      REAL,DIMENSION(KMS:KME,KMS:KME,6) :: CO22D
      REAL,DIMENSION((KME-KMS+1)*(KME-KMS+1)) :: DATA2
      LOGICAL :: OPENED
      LOGICAL,EXTERNAL :: wrf_dm_on_monitor
      CHARACTER*80 errmess
      character*255 message

































































































      L=KME-KMS
      LP1=KME-KMS+1


      IF ( wrf_dm_on_monitor() ) THEN
        DO i = 14,99
      write(message,*)' in CONRAD i=',i,' opened=',opened
      call wrf_debug(1,message)
          INQUIRE ( i , OPENED = opened )
          IF ( .NOT. opened ) THEN
            nunit_co2 = i
            GOTO 2014
          ENDIF
        ENDDO
        nunit_co2 = -1
 2014   CONTINUE
      ENDIF
        IF ( wrf_dm_on_monitor() ) THEN
          OPEN(nunit_co2,FILE='co2_trans',                  &
               FORM='UNFORMATTED',STATUS='OLD',ERR=9014,IOSTAT=IERROR)
          REWIND NUNIT_CO2
        ENDIF





      RSZE(1) = LP1
      RSZE(2) = L
      RSZE(3) = LP1*LP1


      RSIZE = RSZE(1)

      DO KK=1,2
        IF( wrf_dm_on_monitor() )READ(NUNIT_CO2)(SGTMP(I,KK),I=1,RSIZE)
        CALL wrf_dm_bcast_real( SGTMP(1,KK), RSIZE )
      ENDDO



      RSIZE = RSZE(2)

      DO KK=1,6
        IF( wrf_dm_on_monitor() )READ(NUNIT_CO2)(CO21D(I,KK),I=1,RSIZE)
        CALL wrf_dm_bcast_real( CO21D(1,KK), RSIZE )
      ENDDO



      RSIZE = RSZE(3)

      DO KK=1,6
        IF( wrf_dm_on_monitor() )READ(NUNIT_CO2)(DATA2(I),I=1,RSIZE)
        CALL wrf_dm_bcast_real( DATA2(1), RSIZE )
        N=0

        DO I1=1,LP1
        DO I2=1,LP1
          N=N+1
          CO22D(I1,I2,KK)=DATA2(N)
        ENDDO
        ENDDO

      ENDDO




      IF(ALLOCATED (CO251))DEALLOCATE(CO251)
      IF(ALLOCATED (CDT51))DEALLOCATE(CDT51)
      IF(ALLOCATED (C2D51))DEALLOCATE(C2D51)
      IF(ALLOCATED (CO258))DEALLOCATE(CO258)
      IF(ALLOCATED (CDT58))DEALLOCATE(CDT58)
      IF(ALLOCATED (C2D58))DEALLOCATE(C2D58)
      IF(ALLOCATED (STEMP))DEALLOCATE(STEMP)
      IF(ALLOCATED (GTEMP))DEALLOCATE(GTEMP)
      IF(ALLOCATED (CO231))DEALLOCATE(CO231)
      IF(ALLOCATED (CDT31))DEALLOCATE(CDT31)
      IF(ALLOCATED (C2D31))DEALLOCATE(C2D31)
      IF(ALLOCATED (CO238))DEALLOCATE(CO238)
      IF(ALLOCATED (CDT38))DEALLOCATE(CDT38)
      IF(ALLOCATED (C2D38))DEALLOCATE(C2D38)
      IF(ALLOCATED (CO271))DEALLOCATE(CO271)
      IF(ALLOCATED (CDT71))DEALLOCATE(CDT71)
      IF(ALLOCATED (C2D71))DEALLOCATE(C2D71)
      IF(ALLOCATED (CO278))DEALLOCATE(CO278)
      IF(ALLOCATED (CDT78))DEALLOCATE(CDT78)
      IF(ALLOCATED (C2D78))DEALLOCATE(C2D78)
      IF(ALLOCATED (CO2M51))DEALLOCATE(CO2M51)
      IF(ALLOCATED (CDTM51))DEALLOCATE(CDTM51)
      IF(ALLOCATED (C2DM51))DEALLOCATE(C2DM51)
      IF(ALLOCATED (CO2M58))DEALLOCATE(CO2M58)
      IF(ALLOCATED (CDTM58))DEALLOCATE(CDTM58)
      IF(ALLOCATED (C2DM58))DEALLOCATE(C2DM58)



      RSIZE = RSZE(1)

      DO KK=1,6
        IF( wrf_dm_on_monitor() )READ(NUNIT_CO2)(CO21D3(I,KK),I=1,RSIZE)
        CALL wrf_dm_bcast_real( CO21D3(1,KK), RSIZE )
      ENDDO



      DO KK=1,6
        IF( wrf_dm_on_monitor() )READ(NUNIT_CO2)(CO21D7(I,KK),I=1,RSIZE)
        CALL wrf_dm_bcast_real ( CO21D7(1,KK), RSIZE )
      ENDDO


      ALLOCATE(CO251(LP1,LP1))
      ALLOCATE(CDT51(LP1,LP1))
      ALLOCATE(C2D51(LP1,LP1))
      ALLOCATE(CO258(LP1,LP1))
      ALLOCATE(CDT58(LP1,LP1))
      ALLOCATE(C2D58(LP1,LP1))
      ALLOCATE(STEMP(LP1))
      ALLOCATE(GTEMP(LP1))
      ALLOCATE(CO231(LP1))
      ALLOCATE(CDT31(LP1))
      ALLOCATE(C2D31(LP1))
      ALLOCATE(CO238(LP1))
      ALLOCATE(CDT38(LP1))
      ALLOCATE(C2D38(LP1))
      ALLOCATE(CO271(LP1))
      ALLOCATE(CDT71(LP1))
      ALLOCATE(C2D71(LP1))
      ALLOCATE(CO278(LP1))
      ALLOCATE(CDT78(LP1))
      ALLOCATE(C2D78(LP1))
      ALLOCATE(CO2M51(L))
      ALLOCATE(CDTM51(L))
      ALLOCATE(C2DM51(L))
      ALLOCATE(CO2M58(L))
      ALLOCATE(CDTM58(L))
      ALLOCATE(C2DM58(L))


      DO K=1,LP1
        STEMP(K) = SGTMP(K,1)
        GTEMP(K) = SGTMP(K,2)
      ENDDO

      DO K=1,L
        CDTM51(K) = CO21D(K,1)
        CO2M51(K) = CO21D(K,2)
        C2DM51(K) = CO21D(K,3)
        CDTM58(K) = CO21D(K,4)
        CO2M58(K) = CO21D(K,5)
        C2DM58(K) = CO21D(K,6)
      ENDDO

      DO J=1,LP1
      DO I=1,LP1
        CDT51(I,J) = CO22D(I,J,1)
        CO251(I,J) = CO22D(I,J,2)
        C2D51(I,J) = CO22D(I,J,3)
        CDT58(I,J) = CO22D(I,J,4)
        CO258(I,J) = CO22D(I,J,5)
        C2D58(I,J) = CO22D(I,J,6)
      ENDDO
      ENDDO

      DO K=1,LP1
        CDT31(K) = CO21D3(K,1)
        CO231(K) = CO21D3(K,2)
        C2D31(K) = CO21D3(K,3)
        CDT38(K) = CO21D3(K,4)
        CO238(K) = CO21D3(K,5)
        C2D38(K) = CO21D3(K,6)
      ENDDO

      DO K=1,LP1
        CDT71(K) = CO21D7(K,1)
        CO271(K) = CO21D7(K,2)
        C2D71(K) = CO21D7(K,3)
        CDT78(K) = CO21D7(K,4)
        CO278(K) = CO21D7(K,5)
        C2D78(K) = CO21D7(K,6)
      ENDDO


      IF(wrf_dm_on_monitor())WRITE(0,66)NUNIT_CO2
   66 FORMAT('----READ CO2 TRANSMISSION FUNCTIONS FROM UNIT ',I2)

      IF( wrf_dm_on_monitor() )THEN
        CLOSE(nunit_co2)
      ENDIF
      RETURN

9014 CONTINUE
     WRITE(errmess,'(A51,I4)')'module_ra_gfdleta: error reading co2_trans on unit ',nunit_co2
     CALL wrf_error_fatal3("<stdin>",10088,&
errmess)

      END SUBROUTINE CONRAD







      REAL FUNCTION FPVS_new(T)

      IMPLICIT NONE
      REAL, INTENT(IN):: T

      if (T .ge. 273.16) then
         FPVS_new = e_sub_l(T)
      else
         FPVS_new = e_sub_i(T)
      endif

      END FUNCTION FPVS_new





      REAL FUNCTION e_sub_l(T)

      IMPLICIT NONE
      REAL, INTENT(IN):: T
      REAL:: ESL,X
      REAL, PARAMETER:: C0= .611583699E03
      REAL, PARAMETER:: C1= .444606896E02
      REAL, PARAMETER:: C2= .143177157E01
      REAL, PARAMETER:: C3= .264224321E-1
      REAL, PARAMETER:: C4= .299291081E-3
      REAL, PARAMETER:: C5= .203154182E-5
      REAL, PARAMETER:: C6= .702620698E-8
      REAL, PARAMETER:: C7= .379534310E-11
      REAL, PARAMETER:: C8=-.321582393E-13

      X=AMAX1(-80.,T-273.16)

      ESL=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))

      e_sub_l = ESL

      END FUNCTION e_sub_l





      REAL FUNCTION e_sub_i(T)

      IMPLICIT NONE
      REAL, INTENT(IN):: T
      REAL:: ESI,X
      REAL, PARAMETER:: C0= .609868993E03
      REAL, PARAMETER:: C1= .499320233E02
      REAL, PARAMETER:: C2= .184672631E01
      REAL, PARAMETER:: C3= .402737184E-1
      REAL, PARAMETER:: C4= .565392987E-3
      REAL, PARAMETER:: C5= .521693933E-5
      REAL, PARAMETER:: C6= .307839583E-7
      REAL, PARAMETER:: C7= .105785160E-9
      REAL, PARAMETER:: C8= .161444444E-12

      X=AMAX1(-80.,T-273.16)
      ESI=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))

      e_sub_i = ESI

      END FUNCTION e_sub_i





      END MODULE module_RA_GFDLETA



