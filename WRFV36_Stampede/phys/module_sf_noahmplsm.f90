module noahmp_globals

  
  use module_sf_noahlsm, only: &
       &                       SLCATS,     &
       &                       LUCATS,     &
       &                       CSOIL_DATA, & 
       &                       BB,         &
       &                       SATDK,      &
       &                       SATDW,      &
       &                       F11,        &
       &                       SATPSI,     &
       &                       QTZ,        &
       &                       DRYSMC,     &
       &                       MAXSMC,     &
       &                       REFSMC,     &
       &                       WLTSMC,     &
       &                       RSTBL,      &
       &                       RGLTBL,     &
       &                       HSTBL,      &
       &                       NROTBL,     &
       &                       TOPT_DATA,  &
       &                       RSMAX_DATA, &
       &                       ZBOT_DATA,  &
       &                       CZIL_DATA,  &
       &                       FRZK_DATA,  &
       &                       SLOPE_DATA, &
       &                       REFDK_DATA, &
       &                       REFKDT_DATA
       
  implicit none






  REAL, PARAMETER :: GRAV   = 9.80616   
  REAL, PARAMETER :: SB     = 5.67E-08  
  REAL, PARAMETER :: VKC    = 0.40      
  REAL, PARAMETER :: TFRZ   = 273.16    
  REAL, PARAMETER :: HSUB   = 2.8440E06 
  REAL, PARAMETER :: HVAP   = 2.5104E06 
  REAL, PARAMETER :: HFUS   = 0.3336E06 
  REAL, PARAMETER :: CWAT   = 4.188E06  
  REAL, PARAMETER :: CICE   = 2.094E06  
  REAL, PARAMETER :: CPAIR  = 1004.64   
  REAL, PARAMETER :: TKWAT  = 0.6       
  REAL, PARAMETER :: TKICE  = 2.2       
  REAL, PARAMETER :: TKAIR  = 0.023     
  REAL, PARAMETER :: RAIR   = 287.04    
  REAL, PARAMETER :: RW     = 461.269   
  REAL, PARAMETER :: DENH2O = 1000.     
  REAL, PARAMETER :: DENICE = 917.      




  INTEGER :: NROOT        
  REAL    :: RGL          
  REAL    :: RSMIN        
  REAL    :: HS           
  REAL    :: RSMAX        
  REAL    :: TOPT         




























  REAL    :: BEXP         
  REAL    :: SMCDRY       
                          
  REAL    :: F1           
  REAL    :: SMCMAX       
  REAL    :: SMCREF       
  REAL    :: PSISAT       
  REAL    :: DKSAT        
  REAL    :: DWSAT        
  REAL    :: SMCWLT       
  REAL    :: QUARTZ       









  REAL    :: SLOPE       
  REAL    :: CSOIL       
  REAL    :: ZBOT        
  REAL    :: CZIL        

  REAL    :: KDT         
  REAL    :: FRZX        
















  INTEGER :: DVEG    




  INTEGER :: OPT_CRS 






  INTEGER :: OPT_BTR 








  INTEGER :: OPT_RUN 




  INTEGER :: OPT_SFC 




  INTEGER :: OPT_FRZ 





  INTEGER :: OPT_INF 






  INTEGER :: OPT_RAD 




  INTEGER :: OPT_ALB 




  INTEGER :: OPT_SNF 





  INTEGER :: OPT_TBOT 




  INTEGER :: OPT_STC 


  REAL, PARAMETER :: TIMEAN = 10.5   
  REAL, PARAMETER :: FSATMX = 0.38   



  REAL, PARAMETER :: M      = 2.50   
  REAL, PARAMETER :: Z0SNO  = 0.002  
  REAL, PARAMETER :: SSI    = 0.03   
  REAL, PARAMETER :: SWEMX  = 1.00   
                                     








END MODULE NOAHMP_GLOBALS


MODULE NOAHMP_VEG_PARAMETERS

    IMPLICIT NONE

    INTEGER, PARAMETER :: MAX_VEG_PARAMS = 33
    INTEGER, PARAMETER :: MVT   = 27
    INTEGER, PARAMETER :: MBAND = 2

    INTEGER, PRIVATE :: ISURBAN
    INTEGER :: ISWATER
    INTEGER :: ISBARREN
    INTEGER :: ISSNOW
    INTEGER :: EBLFOREST

    REAL :: CH2OP(MVT)       
    REAL :: DLEAF(MVT)       
    REAL :: Z0MVT(MVT)       
    REAL :: HVT(MVT)         
    REAL :: HVB(MVT)         
    REAL :: DEN(MVT)         
    REAL :: RC(MVT)          
    REAL :: SAIM(MVT,12)     
    REAL :: LAIM(MVT,12)     
    REAL :: SLA(MVT)         
    REAL :: DILEFC(MVT)      
    REAL :: DILEFW(MVT)      
    REAL :: FRAGR(MVT)       
    REAL :: LTOVRC(MVT)      

    REAL :: C3PSN(MVT)       
    REAL :: KC25(MVT)        
    REAL :: AKC(MVT)         
    REAL :: KO25(MVT)        
    REAL :: AKO(MVT)         
    REAL :: VCMX25(MVT)      
    REAL :: AVCMX(MVT)       
    REAL :: BP(MVT)          
    REAL :: MP(MVT)          
    REAL :: QE25(MVT)        
    REAL :: AQE(MVT)         
    REAL :: RMF25(MVT)       
    REAL :: RMS25(MVT)       
    REAL :: RMR25(MVT)       
    REAL :: ARM(MVT)         
    REAL :: FOLNMX(MVT)      
    REAL :: TMIN(MVT)        

    REAL :: XL(MVT)          
    REAL :: RHOL(MVT,MBAND)  
    REAL :: RHOS(MVT,MBAND)  
    REAL :: TAUL(MVT,MBAND)  
    REAL :: TAUS(MVT,MBAND)  

    REAL :: MRP(MVT)         
    REAL :: CWPVT(MVT)       

    REAL :: WRRAT(MVT)       
    REAL :: WDPOOL(MVT)      
    REAL :: TDLEF(MVT)       

    INTEGER :: IK,IM
    REAL :: TMP10(MVT*MBAND)
    REAL :: TMP11(MVT*MBAND)
    REAL :: TMP12(MVT*MBAND)
    REAL :: TMP13(MVT*MBAND)
    REAL :: TMP14(MVT*12)
    REAL :: TMP15(MVT*12)
    REAL :: TMP16(MVT*5)

    real slarea(MVT)
    real eps(MVT,5)

CONTAINS
  subroutine read_mp_veg_parameters(DATASET_IDENTIFIER)
    implicit none
    character(len=*), intent(in) :: DATASET_IDENTIFIER
    integer :: ierr

    
    REAL :: TMP10(MVT*MBAND)
    REAL :: TMP11(MVT*MBAND)
    REAL :: TMP12(MVT*MBAND)
    REAL :: TMP13(MVT*MBAND)
    REAL :: TMP14(MVT*12)
    REAL :: TMP15(MVT*12)
    REAL :: TMP16(MVT*5)

    integer :: NVEG
    character(len=256) :: VEG_DATASET_DESCRIPTION

    NAMELIST / noah_mp_usgs_veg_categories / VEG_DATASET_DESCRIPTION, NVEG
    NAMELIST / noah_mp_usgs_parameters / ISURBAN, ISWATER, ISBARREN, ISSNOW, EBLFOREST, &
         CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, RHOL,  RHOS, TAUL, TAUS, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, AVCMX, AQE, &
         LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  BP, MP, QE25, RMS25, RMR25, ARM, &
         FOLNMX, WDPOOL, WRRAT, MRP, SAIM,  LAIM,  SLAREA, EPS

    NAMELIST / noah_mp_modis_veg_categories / VEG_DATASET_DESCRIPTION, NVEG
    NAMELIST / noah_mp_modis_parameters / ISURBAN, ISWATER, ISBARREN, ISSNOW, EBLFOREST, &
         CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, RHOL,  RHOS, TAUL, TAUS, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, AVCMX, AQE, &
         LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  BP, MP, QE25, RMS25, RMR25, ARM, &
         FOLNMX, WDPOOL, WRRAT, MRP, SAIM,  LAIM,  SLAREA, EPS

    
    CH2OP  = -1.E36
    DLEAF  = -1.E36
    Z0MVT  = -1.E36
    HVT    = -1.E36
    HVB    = -1.E36
    DEN    = -1.E36
    RC     = -1.E36
    RHOL   = -1.E36
    RHOS   = -1.E36
    TAUL   = -1.E36
    TAUS   = -1.E36
    XL     = -1.E36
    CWPVT  = -1.E36
    C3PSN  = -1.E36
    KC25   = -1.E36
    AKC    = -1.E36
    KO25   = -1.E36
    AKO    = -1.E36
    AVCMX  = -1.E36
    AQE    = -1.E36
    LTOVRC = -1.E36
    DILEFC = -1.E36
    DILEFW = -1.E36
    RMF25  = -1.E36
    SLA    = -1.E36
    FRAGR  = -1.E36
    TMIN   = -1.E36
    VCMX25 = -1.E36
    TDLEF  = -1.E36
    BP     = -1.E36
    MP     = -1.E36
    QE25   = -1.E36
    RMS25  = -1.E36
    RMR25  = -1.E36
    ARM    = -1.E36
    FOLNMX = -1.E36
    WDPOOL = -1.E36
    WRRAT  = -1.E36
    MRP    = -1.E36
    SAIM   = -1.E36
    LAIM   = -1.E36
    SLAREA = -1.E36
    EPS    = -1.E36

    open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    if (ierr /= 0) then
       write(*,'("****** Error ******************************************************")')
       write(*,'("Cannot find file MPTABLE.TBL")')
       write(*,'("STOP")')
       write(*,'("*******************************************************************")')
       call wrf_error_fatal3("<stdin>",378,&
"STOP in Noah-MP read_mp_veg_parameters")
    endif

    if ( trim(DATASET_IDENTIFIER) == "USGS" ) then
       read(15,noah_mp_usgs_veg_categories)
       read(15,noah_mp_usgs_parameters)
    else if ( trim(DATASET_IDENTIFIER) == "MODIFIED_IGBP_MODIS_NOAH" ) then
       read(15,noah_mp_modis_veg_categories)
       read(15,noah_mp_modis_parameters)
    else
       write(*,'("Unrecognized DATASET_IDENTIFIER in subroutine READ_MP_VEG_PARAMETERS")')
       write(*,'("DATASET_IDENTIFIER = ''", A, "''")') trim(DATASET_IDENTIFIER)
       call wrf_error_fatal3("<stdin>",391,&
"STOP in Noah-MP read_mp_veg_parameters")
    endif
    close(15)

    
    

    if ( MVT > NVEG ) then

       
       
       

       TMP10 = reshape( RHOL, (/ MVT*size(RHOL,2) /))
       TMP11 = reshape( RHOS, (/ MVT*size(RHOS,2) /))
       TMP12 = reshape( TAUL, (/ MVT*size(TAUL,2) /))
       TMP13 = reshape( TAUS, (/ MVT*size(TAUS,2) /))
       TMP14 = reshape( SAIM, (/ MVT*size(SAIM,2) /))
       TMP15 = reshape( LAIM, (/ MVT*size(LAIM,2) /))
       TMP16 = reshape( EPS,  (/ MVT*size(EPS ,2) /))

       RHOL(1:NVEG,:) = reshape( TMP10, (/ NVEG, size(RHOL,2) /))
       RHOS(1:NVEG,:) = reshape( TMP11, (/ NVEG, size(RHOS,2) /))
       TAUL(1:NVEG,:) = reshape( TMP12, (/ NVEG, size(TAUL,2) /))
       TAUS(1:NVEG,:) = reshape( TMP13, (/ NVEG, size(TAUS,2) /))
       SAIM(1:NVEG,:) = reshape( TMP14, (/ NVEG, size(SAIM,2) /))
       LAIM(1:NVEG,:) = reshape( TMP15, (/ NVEG, size(LAIM,2) /))
       EPS(1:NVEG,:)  = reshape( TMP16, (/ NVEG, size(EPS,2)  /))

       RHOL(NVEG+1:MVT,:) = -1.E36
       RHOS(NVEG+1:MVT,:) = -1.E36
       TAUL(NVEG+1:MVT,:) = -1.E36
       TAUS(NVEG+1:MVT,:) = -1.E36
       SAIM(NVEG+1:MVT,:) = -1.E36
       LAIM(NVEG+1:MVT,:) = -1.E36
       EPS( NVEG+1:MVT,:) = -1.E36
    endif

  end subroutine read_mp_veg_parameters

END MODULE NOAHMP_VEG_PARAMETERS


MODULE NOAHMP_RAD_PARAMETERS

    IMPLICIT NONE
 
    INTEGER I                
    INTEGER, PARAMETER :: MSC   = 9
    INTEGER, PARAMETER :: MBAND = 2

    REAL :: ALBSAT(MSC,MBAND)   
    REAL :: ALBDRY(MSC,MBAND)   
    REAL :: ALBICE(MBAND)       
    REAL :: ALBLAK(MBAND)       
    REAL :: OMEGAS(MBAND)       
    REAL :: BETADS              
    REAL :: BETAIS              
    REAL :: EG(2)               


    DATA(ALBSAT(I,1),I=1,8)/0.15,0.11,0.10,0.09,0.08,0.07,0.06,0.05/
    DATA(ALBSAT(I,2),I=1,8)/0.30,0.22,0.20,0.18,0.16,0.14,0.12,0.10/


    DATA(ALBDRY(I,1),I=1,8)/0.27,0.22,0.20,0.18,0.16,0.14,0.12,0.10/
    DATA(ALBDRY(I,2),I=1,8)/0.54,0.44,0.40,0.36,0.32,0.28,0.24,0.20/


    DATA (ALBICE(I),I=1,MBAND) /0.80, 0.55/


    DATA (ALBLAK(I),I=1,MBAND) /0.60, 0.40/


    DATA (OMEGAS(I),I=1,MBAND) /0.8, 0.4/
    DATA BETADS, BETAIS /0.5, 0.5/


      DATA EG /0.97, 0.98/ 

END MODULE NOAHMP_RAD_PARAMETERS


MODULE NOAHMP_ROUTINES
  USE NOAHMP_GLOBALS
  IMPLICIT NONE

  public  :: noahmp_options
  public  :: NOAHMP_SFLX
  public  :: REDPRM
  public  :: FRH2O           

  private :: ATM
  private :: PHENOLOGY
  private :: ENERGY
  private ::       THERMOPROP
  private ::               CSNOW
  private ::               TDFCND
  private ::       RADIATION
  private ::               ALBEDO
  private ::                         SNOW_AGE
  private ::                         SNOWALB_BATS  
  private ::                         SNOWALB_CLASS
  private ::                         GROUNDALB
  private ::                         TWOSTREAM
  private ::               SURRAD
  private ::       VEGE_FLUX
  private ::               SFCDIF1                  
  private ::               SFCDIF2                
  private ::               STOMATA                  
  private ::               CANRES                  
  private ::               ESAT
  private ::               RAGRB
  private ::       BARE_FLUX
  private ::       TSNOSOI
  private ::               HRT
  private ::               HSTEP   
  private ::                         ROSR12
  private ::       PHASECHANGE

  private :: WATER
  private ::       CANWATER
  private ::       SNOWWATER
  private ::               SNOWFALL
  private ::               COMBINE
  private ::               DIVIDE
  private ::                         COMBO
  private ::               COMPACT
  private ::               SNOWH2O
  private ::       SOILWATER
  private ::               ZWTEQ
  private ::               INFIL
  private ::               SRT
  private ::                         WDFCND1        
  private ::                         WDFCND2       

  private ::               SSTEP
  private ::       GROUNDWATER
  private ::       SHALLOWWATERTABLE

  private :: CARBON
  private ::       CO2FLUX



  private :: ERROR

contains



  SUBROUTINE NOAHMP_SFLX (&
                   ILOC    , JLOC    , LAT     , YEARLEN , JULIAN  , COSZ    , & 
                   DT      , DX      , DZ8W    , NSOIL   , ZSOIL   , NSNOW   , & 
                   SHDFAC  , SHDMAX  , VEGTYP  , ISURBAN , ICE     , IST     , & 
                   ISC     , SMCEQ   ,                                         & 
                   IZ0TLND ,                                                   & 
                   SFCTMP  , SFCPRS  , PSFC    , UU      , VV      , Q2      , & 
                   QC      , SOLDN   , LWDN    , PRCP    , TBOT    , CO2AIR  , & 
                   O2AIR   , FOLN    , FICEOLD , PBLH    , ZLVL    ,           & 
                   ALBOLD  , SNEQVO  ,                                         & 
                   STC     , SH2O    , SMC     , TAH     , EAH     , FWET    , & 
                   CANLIQ  , CANICE  , TV      , TG      , QSFC    , QSNOW   , & 
                   ISNOW   , ZSNSO   , SNOWH   , SNEQV   , SNICE   , SNLIQ   , & 
                   ZWT     , WA      , WT      , WSLAKE  , LFMASS  , RTMASS  , & 
                   STMASS  , WOOD    , STBLCP  , FASTCP  , LAI     , SAI     , & 
                   CM      , CH      , TAUSS   ,                               & 
                   SMCWTD  ,DEEPRECH , RECH    ,                               & 
                   FSA     , FSR     , FIRA    , FSH     , SSOIL   , FCEV    , & 
                   FGEV    , FCTR    , ECAN    , ETRAN   , EDIR    , TRAD    , & 
                   TGB     , TGV     , T2MV    , T2MB    , Q2V     , Q2B     , & 
                   RUNSRF  , RUNSUB  , APAR    , PSN     , SAV     , SAG     , & 
                   FSNO    , NEE     , GPP     , NPP     , FVEG    , ALBEDO  , & 
                   QSNBOT  , PONDING , PONDING1, PONDING2, RSSUN   , RSSHA   , & 
                   BGAP    , WGAP    , CHV     , CHB     , EMISSI  ,           & 
		   SHG     , SHC     , SHB     , EVG     , EVB     , GHV     , & 
		   GHB     , IRG     , IRC     , IRB     , TR      , EVC     , & 
		   CHLEAF  , CHUC    , CHV2    , CHB2    , FPICE               &



                   )




  USE NOAHMP_VEG_PARAMETERS
  USE NOAHMP_RAD_PARAMETERS

  implicit none


  INTEGER                        , INTENT(IN)    :: ICE    
  INTEGER                        , INTENT(IN)    :: IST    
  INTEGER                        , INTENT(IN)    :: VEGTYP 
  INTEGER                        , INTENT(IN)    :: ISC    
  INTEGER                        , INTENT(IN)    :: NSNOW  
  INTEGER                        , INTENT(IN)    :: NSOIL  
  INTEGER                        , INTENT(IN)    :: ILOC   
  INTEGER                        , INTENT(IN)    :: JLOC   
  REAL                           , INTENT(IN)    :: DT     
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  
  REAL                           , INTENT(IN)    :: Q2     
  REAL                           , INTENT(IN)    :: SFCTMP 
  REAL                           , INTENT(IN)    :: UU     
  REAL                           , INTENT(IN)    :: VV     
  REAL                           , INTENT(IN)    :: SOLDN  
  REAL                           , INTENT(IN)    :: PRCP   
  REAL                           , INTENT(IN)    :: LWDN   
  REAL                           , INTENT(IN)    :: SFCPRS 
  REAL                           , INTENT(INOUT) :: ZLVL   
  REAL                           , INTENT(IN)    :: COSZ   
  REAL                           , INTENT(IN)    :: TBOT   
  REAL                           , INTENT(IN)    :: FOLN   
  REAL                           , INTENT(IN)    :: SHDFAC 
  INTEGER                        , INTENT(IN)    :: YEARLEN
  REAL                           , INTENT(IN)    :: JULIAN 
  REAL                           , INTENT(IN)    :: LAT    
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: FICEOLD
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: SMCEQ  


  INTEGER                        , INTENT(IN)    :: ISURBAN
  INTEGER                        , INTENT(IN)    :: IZ0TLND
  REAL                           , INTENT(IN)    :: QC     
  REAL                           , INTENT(IN)    :: PBLH   
  REAL                           , INTENT(INOUT)    :: QSFC   
  REAL                           , INTENT(IN)    :: PSFC   
  REAL                           , INTENT(IN)    :: DZ8W   
  REAL                           , INTENT(IN)    :: DX
  REAL                           , INTENT(IN)    :: SHDMAX  







  REAL                           , INTENT(INOUT) :: QSNOW  
  REAL                           , INTENT(INOUT) :: FWET   
  REAL                           , INTENT(INOUT) :: SNEQVO 
  REAL                           , INTENT(INOUT) :: EAH    
  REAL                           , INTENT(INOUT) :: TAH    
  REAL                           , INTENT(INOUT) :: ALBOLD 
  REAL                           , INTENT(INOUT) :: CM     
  REAL                           , INTENT(INOUT) :: CH     
  REAL                           , INTENT(INOUT) :: TAUSS  


  INTEGER                        , INTENT(INOUT) :: ISNOW  
  REAL                           , INTENT(INOUT) :: CANLIQ 
  REAL                           , INTENT(INOUT) :: CANICE 
  REAL                           , INTENT(INOUT) :: SNEQV  
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SMC    
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO  
  REAL                           , INTENT(INOUT) :: SNOWH  
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  
  REAL                           , INTENT(INOUT) :: TV     
  REAL                           , INTENT(INOUT) :: TG     
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   
  REAL                           , INTENT(INOUT) :: ZWT    
  REAL                           , INTENT(INOUT) :: WA     
  REAL                           , INTENT(INOUT) :: WT     
  REAL                           , INTENT(INOUT) :: WSLAKE 
  REAL,                            INTENT(INOUT) :: SMCWTD 
  REAL,                            INTENT(INOUT) :: DEEPRECH 
  REAL,                            INTENT(INOUT) :: RECH 


  REAL                           , INTENT(OUT)   :: FSA    
  REAL                           , INTENT(OUT)   :: FSR    
  REAL                           , INTENT(OUT)   :: FIRA   
  REAL                           , INTENT(OUT)   :: FSH    
  REAL                           , INTENT(OUT)   :: FCEV   
  REAL                           , INTENT(OUT)   :: FGEV   
  REAL                           , INTENT(OUT)   :: FCTR   
  REAL                           , INTENT(OUT)   :: SSOIL  
  REAL                           , INTENT(OUT)   :: TRAD   
  REAL                                           :: TS     
  REAL                           , INTENT(OUT)   :: ECAN   
  REAL                           , INTENT(OUT)   :: ETRAN  
  REAL                           , INTENT(OUT)   :: EDIR   
  REAL                           , INTENT(OUT)   :: RUNSRF 
  REAL                           , INTENT(OUT)   :: RUNSUB 
  REAL                           , INTENT(OUT)   :: PSN    
  REAL                           , INTENT(OUT)   :: APAR   
  REAL                           , INTENT(OUT)   :: SAV    
  REAL                           , INTENT(OUT)   :: SAG    
  REAL                           , INTENT(OUT)   :: FSNO   
  REAL                           , INTENT(OUT)   :: FVEG   
  REAL                           , INTENT(OUT)   :: ALBEDO 
  REAL                                           :: ERRWAT 
  REAL                           , INTENT(OUT)   :: QSNBOT 
  REAL                           , INTENT(OUT)   :: PONDING
  REAL                           , INTENT(OUT)   :: PONDING1
  REAL                           , INTENT(OUT)   :: PONDING2


  REAL                           , INTENT(OUT)     :: T2MV   
  REAL                           , INTENT(OUT)     :: T2MB   
  REAL, INTENT(OUT) :: RSSUN        
  REAL, INTENT(OUT) :: RSSHA        
  REAL, INTENT(OUT) :: BGAP
  REAL, INTENT(OUT) :: WGAP
  REAL, INTENT(OUT) :: TGV
  REAL, INTENT(OUT) :: TGB
  REAL              :: Q1
  REAL, INTENT(OUT) :: EMISSI



  INTEGER                                        :: IZ     
  INTEGER, DIMENSION(-NSNOW+1:NSOIL)             :: IMELT  
  REAL                                           :: CMC    
  REAL                                           :: TAUX   
  REAL                                           :: TAUY   
  REAL                                           :: RHOAIR 

  REAL, DIMENSION(-NSNOW+1:NSOIL)                :: DZSNSO 
  REAL                                           :: THAIR  
  REAL                                           :: QAIR   
  REAL                                           :: EAIR   
  REAL, DIMENSION(       1:    2)                :: SOLAD  
  REAL, DIMENSION(       1:    2)                :: SOLAI  
  REAL                                           :: QPRECC 
  REAL                                           :: QPRECL 
  REAL                                           :: IGS    
  REAL                                           :: ELAI   
  REAL                                           :: ESAI   
  REAL                                           :: BEVAP  
  REAL, DIMENSION(       1:NSOIL)                :: BTRANI 
  REAL                                           :: BTRAN  
  REAL                                           :: HTOP   
  REAL                                           :: QIN    
  REAL                                           :: QDIS   
  REAL, DIMENSION(       1:NSOIL)                :: SICE   
  REAL, DIMENSION(-NSNOW+1:    0)                :: SNICEV 
  REAL, DIMENSION(-NSNOW+1:    0)                :: SNLIQV 
  REAL, DIMENSION(-NSNOW+1:    0)                :: EPORE  
  REAL                                           :: TOTSC  
  REAL                                           :: TOTLB  
  REAL                                           :: T2M    
  REAL                                           :: QDEW   
  REAL                                           :: QVAP   
  REAL                                           :: LATHEA 
  REAL                                           :: SWDOWN 
  REAL                                           :: QMELT  
  REAL                                           :: BEG_WB 
  REAL,INTENT(OUT)                                              :: IRC    
  REAL,INTENT(OUT)                                              :: IRG    
  REAL,INTENT(OUT)                                              :: SHC    
  REAL,INTENT(OUT)                                              :: SHG    
  REAL,INTENT(OUT)                                              :: EVG    
  REAL,INTENT(OUT)                                              :: GHV    
  REAL,INTENT(OUT)                                              :: IRB    
  REAL,INTENT(OUT)                                              :: SHB    
  REAL,INTENT(OUT)                                              :: EVB    
  REAL,INTENT(OUT)                                              :: GHB    
  REAL,INTENT(OUT)                                              :: EVC    
  REAL,INTENT(OUT)                                              :: TR     
  REAL, INTENT(OUT)   :: FPICE   


  REAL                                           :: FSRV
  REAL                                           :: FSRG
  REAL,INTENT(OUT)                               :: Q2V
  REAL,INTENT(OUT)                               :: Q2B
  REAL :: Q2E
  REAL :: QFX
  REAL,INTENT(OUT)                               :: CHV    
  REAL,INTENT(OUT)                               :: CHB    
  REAL,INTENT(OUT)                               :: CHLEAF 
  REAL,INTENT(OUT)                               :: CHUC   
  REAL,INTENT(OUT)                               :: CHV2    
  REAL,INTENT(OUT)                               :: CHB2    




  REAL                           , INTENT(IN)    :: CO2AIR 
  REAL                           , INTENT(IN)    :: O2AIR  


  REAL                        , INTENT(INOUT)    :: LFMASS 
  REAL                        , INTENT(INOUT)    :: RTMASS 
  REAL                        , INTENT(INOUT)    :: STMASS 
  REAL                        , INTENT(INOUT)    :: WOOD   
  REAL                        , INTENT(INOUT)    :: STBLCP 
  REAL                        , INTENT(INOUT)    :: FASTCP 
  REAL                        , INTENT(INOUT)    :: LAI    
  REAL                        , INTENT(INOUT)    :: SAI    


  REAL                          , INTENT(OUT)    :: NEE    
  REAL                          , INTENT(OUT)    :: GPP    
  REAL                          , INTENT(OUT)    :: NPP    
  REAL                                           :: AUTORS 
  REAL                                           :: HETERS 
  REAL                                           :: TROOT  
  REAL                                 :: LATHEAV 
  REAL                                 :: LATHEAG 
  LOGICAL                             :: FROZEN_GROUND 
  LOGICAL                             :: FROZEN_CANOPY 

  
  
  nee = 0.0
  npp = 0.0
  gpp = 0.0




   CALL ATM (SFCPRS ,SFCTMP ,Q2     ,PRCP   ,SOLDN  ,COSZ   ,THAIR  , & 
             QAIR   ,EAIR   ,RHOAIR ,QPRECC ,QPRECL ,SOLAD  ,SOLAI  , &
             SWDOWN )



     DO IZ = ISNOW+1, NSOIL
         IF(IZ == ISNOW+1) THEN
           DZSNSO(IZ) = - ZSNSO(IZ)
         ELSE
           DZSNSO(IZ) = ZSNSO(IZ-1) - ZSNSO(IZ)
         END IF
     END DO



     TROOT  = 0.
     DO IZ=1,NROOT
        TROOT = TROOT + STC(IZ)*DZSNSO(IZ)/(-ZSOIL(NROOT))
     ENDDO


    
     IF(IST == 1) THEN
     BEG_WB = CANLIQ + CANICE + SNEQV + WA
     DO IZ = 1,NSOIL
        BEG_WB = BEG_WB + SMC(IZ) * DZSNSO(IZ) * 1000.
     END DO
     END IF



     CALL PHENOLOGY (VEGTYP , ISURBAN, SNOWH  , TV     , LAT   , YEARLEN , JULIAN , & 
                     LAI    , SAI    , TROOT  , HTOP  , ELAI    , ESAI   ,IGS)















     IF(DVEG == 1) THEN
        FVEG = SHDFAC
        IF(FVEG <= 0.01) FVEG = 0.01
     ELSE IF (DVEG == 2 .or. DVEG == 3) THEN
        FVEG = 1.-EXP(-0.52*(LAI+SAI))
        IF(FVEG <= 0.01) FVEG = 0.01
     ELSE IF (DVEG == 4 .or. DVEG == 5) THEN
        FVEG = SHDMAX
        IF(FVEG <= 0.01) FVEG = 0.01
     ELSE
        WRITE(*,*) "-------- FATAL CALLED IN SFLX -----------"
        CALL wrf_error_fatal3("<stdin>",868,&
"Namelist parameter DVEG unknown") 
     ENDIF
     IF(VEGTYP == ISURBAN .OR. VEGTYP == ISBARREN) FVEG = 0.0
     IF(ELAI+ESAI == 0.0) FVEG = 0.0







    CALL ENERGY (ICE    ,VEGTYP ,IST    ,ISC    ,NSNOW  ,NSOIL  , & 
                 ISNOW  ,NROOT  ,DT     ,RHOAIR ,SFCPRS ,QAIR   , & 
                 SFCTMP ,THAIR  ,LWDN   ,UU     ,VV     ,ZLVL   , & 
                 CO2AIR ,O2AIR  ,SOLAD  ,SOLAI  ,COSZ   ,IGS    , & 
                 EAIR   ,HTOP   ,TBOT   ,ZBOT   ,ZSNSO  ,ZSOIL  , & 
                 ELAI   ,ESAI   ,CSOIL  ,FWET   ,FOLN   ,         & 
                 FVEG   ,                                         & 
                 QSNOW  ,DZSNSO ,LAT    ,CANLIQ ,CANICE ,iloc, jloc , & 
                 IMELT  ,SNICEV ,SNLIQV ,EPORE  ,T2M    ,FSNO   , & 
                 SAV    ,SAG    ,QMELT  ,FSA    ,FSR    ,TAUX   , & 
                 TAUY   ,FIRA   ,FSH    ,FCEV   ,FGEV   ,FCTR   , & 
                 TRAD   ,PSN    ,APAR   ,SSOIL  ,BTRANI ,BTRAN  , & 
                 PONDING,TS     ,LATHEAV , LATHEAG , frozen_canopy,frozen_ground,                         & 
                 TV     ,TG     ,STC    ,SNOWH  ,EAH    ,TAH    , & 
                 SNEQVO ,SNEQV  ,SH2O   ,SMC    ,SNICE  ,SNLIQ  , & 
                 ALBOLD ,CM     ,CH     ,DX     ,DZ8W   ,Q2     , & 
                 TAUSS  ,                                         & 

                 QC     ,PBLH   ,QSFC   ,PSFC   ,ISURBAN,IZ0TLND, & 
                 T2MV   ,T2MB  ,FSRV   , &
                 FSRG   ,RSSUN   ,RSSHA ,BGAP   ,WGAP, TGV,TGB,&
                 Q1     ,Q2V    ,Q2B    ,Q2E    ,CHV   ,CHB     , & 
                 EMISSI,&
		     SHG,SHC,SHB,EVG,EVB,GHV,GHB,IRG,IRC,IRB,TR,EVC,CHLEAF,CHUC,CHV2,CHB2 )                                            


    SICE(:) = MAX(0.0, SMC(:) - SH2O(:))   
    SNEQVO  = SNEQV

    QVAP = MAX( FGEV/LATHEAG, 0.)       
    QDEW = ABS( MIN(FGEV/LATHEAG, 0.))  
    EDIR = QVAP - QDEW



     CALL WATER (VEGTYP ,NSNOW  ,NSOIL  ,IMELT  ,DT     ,UU     , & 
                 VV     ,FCEV   ,FCTR   ,QPRECC ,QPRECL ,ELAI   , & 
                 ESAI   ,SFCTMP ,QVAP   ,QDEW   ,ZSOIL  ,BTRANI , & 
                 FICEOLD,PONDING,TG     ,IST    ,FVEG   ,iloc,jloc , SMCEQ , & 
		 LATHEAV , LATHEAG , frozen_canopy,frozen_ground,                        & 
                 ISNOW  ,CANLIQ ,CANICE ,TV     ,SNOWH  ,SNEQV  , & 
                 SNICE  ,SNLIQ  ,STC    ,ZSNSO  ,SH2O   ,SMC    , & 
                 SICE   ,ZWT    ,WA     ,WT     ,DZSNSO ,WSLAKE , & 
                 SMCWTD ,DEEPRECH,RECH                          , & 
                 CMC    ,ECAN   ,ETRAN  ,FWET   ,RUNSRF ,RUNSUB , & 
                 QIN    ,QDIS   ,QSNOW  ,PONDING1       ,PONDING2,&
                 ISURBAN,QSNBOT,FPICE                             &



                 )  





   IF (DVEG == 2 .OR. DVEG == 5) THEN
    CALL CARBON (NSNOW  ,NSOIL  ,VEGTYP ,NROOT  ,DT     ,ZSOIL  , & 
                 DZSNSO ,STC    ,SMC    ,TV     ,TG     ,PSN    , & 
                 FOLN   ,SMCMAX ,BTRAN  ,APAR   ,FVEG   ,IGS    , & 
                 TROOT  ,IST    ,LAT    ,iloc   ,jloc   ,ISURBAN, & 
                 LFMASS ,RTMASS ,STMASS ,WOOD   ,STBLCP ,FASTCP , & 
                 GPP    ,NPP    ,NEE    ,AUTORS ,HETERS ,TOTSC  , & 
                 TOTLB  ,LAI    ,SAI    )                   
   END IF



     CALL ERROR (SWDOWN ,FSA    ,FSR    ,FIRA   ,FSH    ,FCEV   , & 
                 FGEV   ,FCTR   ,SSOIL  ,BEG_WB ,CANLIQ ,CANICE , & 
                 SNEQV  ,WA     ,SMC    ,DZSNSO ,PRCP   ,ECAN   , & 
                 ETRAN  ,EDIR   ,RUNSRF ,RUNSUB ,DT     ,NSOIL  , & 
                 NSNOW  ,IST    ,ERRWAT ,ILOC   , JLOC  ,FVEG   , &
                 SAV    ,SAG    ,FSRV   ,FSRG   ,ZWT  )   


    QFX = ETRAN + ECAN + EDIR
    IF ( VEGTYP == ISURBAN ) THEN
       QSFC = (QFX/RHOAIR*CH) + QAIR
       Q2B = QSFC
    END IF

    IF(SNOWH <= 1.E-6 .OR. SNEQV <= 1.E-3) THEN
     SNOWH = 0.0
     SNEQV = 0.0
    END IF

    IF(SWDOWN.NE.0.) THEN
      ALBEDO = FSR / SWDOWN
    ELSE
      ALBEDO = -999.9
    END IF
    

  END SUBROUTINE NOAHMP_SFLX

  SUBROUTINE ATM (SFCPRS ,SFCTMP ,Q2     ,PRCP   ,SOLDN  ,COSZ   ,THAIR  , &
                  QAIR   ,EAIR   ,RHOAIR ,QPRECC ,QPRECL ,SOLAD  ,SOLAI  , &
                  SWDOWN )     



  IMPLICIT NONE



  REAL                          , INTENT(IN)  :: SFCPRS 
  REAL                          , INTENT(IN)  :: SFCTMP 
  REAL                          , INTENT(IN)  :: Q2     
  REAL                          , INTENT(IN)  :: SOLDN  
  REAL                          , INTENT(IN)  :: PRCP   
  REAL                          , INTENT(IN)  :: COSZ   



  REAL                          , INTENT(OUT) :: THAIR  
  REAL                          , INTENT(OUT) :: QAIR   
  REAL                          , INTENT(OUT) :: EAIR   
  REAL, DIMENSION(       1:   2), INTENT(OUT) :: SOLAD  
  REAL, DIMENSION(       1:   2), INTENT(OUT) :: SOLAI  
  REAL                          , INTENT(OUT) :: QPRECC 
  REAL                          , INTENT(OUT) :: QPRECL 
  REAL                          , INTENT(OUT) :: RHOAIR 
  REAL                          , INTENT(OUT) :: SWDOWN 



  REAL                                        :: PAIR   



       PAIR   = SFCPRS                   
       THAIR  = SFCTMP * (SFCPRS/PAIR)**(RAIR/CPAIR) 


       QAIR   = Q2                       

       EAIR   = QAIR*SFCPRS / (0.622+0.378*QAIR)
       RHOAIR = (SFCPRS-0.378*EAIR) / (RAIR*SFCTMP)

       QPRECC = 0.10 * PRCP          
       QPRECL = 0.90 * PRCP          

       IF(COSZ <= 0.) THEN 
          SWDOWN = 0.
       ELSE
          SWDOWN = SOLDN
       END IF 

       SOLAD(1) = SWDOWN*0.7*0.5     
       SOLAD(2) = SWDOWN*0.7*0.5     
       SOLAI(1) = SWDOWN*0.3*0.5     
       SOLAI(2) = SWDOWN*0.3*0.5     

  END SUBROUTINE ATM


  SUBROUTINE PHENOLOGY (VEGTYP , ISURBAN, SNOWH  , TV     , LAT   , YEARLEN , JULIAN , & 
                        LAI    , SAI    , TROOT  , HTOP  , ELAI    , ESAI   , IGS)




  USE NOAHMP_VEG_PARAMETERS

  IMPLICIT NONE


  INTEGER                , INTENT(IN   ) :: VEGTYP 
  INTEGER                , INTENT(IN   ) :: ISURBAN
  REAL                   , INTENT(IN   ) :: SNOWH  
  REAL                   , INTENT(IN   ) :: TV     
  REAL                   , INTENT(IN   ) :: LAT    
  INTEGER                , INTENT(IN   ) :: YEARLEN
  REAL                   , INTENT(IN   ) :: JULIAN 
  real                   , INTENT(IN   ) :: TROOT  
  REAL                   , INTENT(INOUT) :: LAI    
  REAL                   , INTENT(INOUT) :: SAI    


  REAL                   , INTENT(OUT  ) :: HTOP   
  REAL                   , INTENT(OUT  ) :: ELAI   
  REAL                   , INTENT(OUT  ) :: ESAI   
  REAL                   , INTENT(OUT  ) :: IGS    



  REAL                                   :: DB     
  REAL                                   :: FB     
  REAL                                   :: SNOWHC 
                                                   

  INTEGER                                :: K       
  INTEGER                                :: IT1,IT2 
  REAL                                   :: DAY     
  REAL                                   :: WT1,WT2 
  REAL                                   :: T       


  IF ( DVEG == 1 .or. DVEG == 3 .or. DVEG == 4 ) THEN

     IF (LAT >= 0.) THEN
        
        DAY = JULIAN
     ELSE
        
        DAY = MOD ( JULIAN + ( 0.5 * YEARLEN ) , REAL(YEARLEN) )
     ENDIF

     T = 12. * DAY / REAL(YEARLEN)
     IT1 = T + 0.5
     IT2 = IT1 + 1
     WT1 = (IT1+0.5) - T
     WT2 = 1.-WT1
     IF (IT1 .LT.  1) IT1 = 12
     IF (IT2 .GT. 12) IT2 = 1

     LAI = WT1*LAIM(VEGTYP,IT1) + WT2*LAIM(VEGTYP,IT2)
     SAI = WT1*SAIM(VEGTYP,IT1) + WT2*SAIM(VEGTYP,IT2)
  ENDIF
  IF (SAI < 0.05) SAI = 0.0                  
  IF (LAI < 0.05 .OR. SAI == 0.0) LAI = 0.0  

  IF ( ( VEGTYP == ISWATER ) .OR. ( VEGTYP == ISBARREN ) .OR. ( VEGTYP == ISSNOW ) .or. ( VEGTYP == ISURBAN) ) THEN
     LAI  = 0.
     SAI  = 0.
  ENDIF



     DB = MIN( MAX(SNOWH - HVB(VEGTYP),0.), HVT(VEGTYP)-HVB(VEGTYP) )
     FB = DB / MAX(1.E-06,HVT(VEGTYP)-HVB(VEGTYP))

     IF(HVT(VEGTYP)> 0. .AND. HVT(VEGTYP) <= 1.0) THEN  
       SNOWHC = HVT(VEGTYP)*EXP(-SNOWH/0.2)             
       FB     = MIN(SNOWH,SNOWHC)/SNOWHC
     ENDIF

     ELAI =  LAI*(1.-FB)
     ESAI =  SAI*(1.-FB)
     IF (ESAI < 0.05) ESAI = 0.0                   
     IF (ELAI < 0.05 .OR. ESAI == 0.0) ELAI = 0.0  

     IF (TV .GT. TMIN(VEGTYP)) THEN
         IGS = 1.
     ELSE
         IGS = 0.
     ENDIF

     HTOP = HVT(VEGTYP)

  END SUBROUTINE PHENOLOGY

  SUBROUTINE ERROR (SWDOWN ,FSA    ,FSR    ,FIRA   ,FSH    ,FCEV   , &
                    FGEV   ,FCTR   ,SSOIL  ,BEG_WB ,CANLIQ ,CANICE , &
                    SNEQV  ,WA     ,SMC    ,DZSNSO ,PRCP   ,ECAN   , &
                    ETRAN  ,EDIR   ,RUNSRF ,RUNSUB ,DT     ,NSOIL  , &
                    NSNOW  ,IST    ,ERRWAT, ILOC   ,JLOC   ,FVEG   , &
                    SAV    ,SAG    ,FSRV   ,FSRG   ,ZWT)



  IMPLICIT NONE


  INTEGER                        , INTENT(IN) :: NSNOW  
  INTEGER                        , INTENT(IN) :: NSOIL  
  INTEGER                        , INTENT(IN) :: IST    
  INTEGER                        , INTENT(IN) :: ILOC   
  INTEGER                        , INTENT(IN) :: JLOC   
  REAL                           , INTENT(IN) :: SWDOWN 
  REAL                           , INTENT(IN) :: FSA    
  REAL                           , INTENT(IN) :: FSR    
  REAL                           , INTENT(IN) :: FIRA   
  REAL                           , INTENT(IN) :: FSH    
  REAL                           , INTENT(IN) :: FCEV   
  REAL                           , INTENT(IN) :: FGEV   
  REAL                           , INTENT(IN) :: FCTR   
  REAL                           , INTENT(IN) :: SSOIL  
  REAL                           , INTENT(IN) :: FVEG
  REAL                           , INTENT(IN) :: SAV
  REAL                           , INTENT(IN) :: SAG
  REAL                           , INTENT(IN) :: FSRV
  REAL                           , INTENT(IN) :: FSRG
  REAL                           , INTENT(IN) :: ZWT

  REAL                           , INTENT(IN) :: PRCP   
  REAL                           , INTENT(IN) :: ECAN   
  REAL                           , INTENT(IN) :: ETRAN  
  REAL                           , INTENT(IN) :: EDIR   
  REAL                           , INTENT(IN) :: RUNSRF 
  REAL                           , INTENT(IN) :: RUNSUB 
  REAL                           , INTENT(IN) :: CANLIQ 
  REAL                           , INTENT(IN) :: CANICE 
  REAL                           , INTENT(IN) :: SNEQV  
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SMC    
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO 
  REAL                           , INTENT(IN) :: WA     
  REAL                           , INTENT(IN) :: DT     
  REAL                           , INTENT(IN) :: BEG_WB 
  REAL                           , INTENT(OUT) :: ERRWAT 

  INTEGER                                     :: IZ     
  REAL                                        :: END_WB 
  
  REAL                                        :: ERRENG 
  REAL                                        :: ERRSW  
  REAL                                        :: FSRVG
  CHARACTER(len=256)                          :: message


   ERRSW   = SWDOWN - (FSA + FSR)


   IF (ABS(ERRSW) > 0.01) THEN            
   WRITE(*,*) "VEGETATION!"
   WRITE(*,*) "SWDOWN*FVEG =",SWDOWN*FVEG
   WRITE(*,*) "FVEG*(SAV+SAG) =",FVEG*SAV + SAG
   WRITE(*,*) "FVEG*(FSRV +FSRG)=",FVEG*FSRV + FSRG
   WRITE(*,*) "GROUND!"
   WRITE(*,*) "(1-.FVEG)*SWDOWN =",(1.-FVEG)*SWDOWN
   WRITE(*,*) "(1.-FVEG)*SAG =",(1.-FVEG)*SAG
   WRITE(*,*) "(1.-FVEG)*FSRG=",(1.-FVEG)*FSRG
   WRITE(*,*) "FSRV   =",FSRV
   WRITE(*,*) "FSRG   =",FSRG
   WRITE(*,*) "FSR    =",FSR
   WRITE(*,*) "SAV    =",SAV
   WRITE(*,*) "SAG    =",SAG
   WRITE(*,*) "FSA    =",FSA

      WRITE(message,*) 'ERRSW =',ERRSW
      call wrf_message(trim(message))
      call wrf_error_fatal3("<stdin>",1212,&
"Stop in Noah-MP")
   END IF

   ERRENG = SAV+SAG-(FIRA+FSH+FCEV+FGEV+FCTR+SSOIL)


   IF(ABS(ERRENG) > 0.01) THEN
      write(message,*) 'ERRENG =',ERRENG
      call wrf_message(trim(message))
      WRITE(message,'(i6,1x,i6,1x,7F10.4)')ILOC,JLOC,FSA,FIRA,FSH,FCEV,FGEV,FCTR,SSOIL
      call wrf_message(trim(message))
      call wrf_error_fatal3("<stdin>",1224,&
"Energy budget problem in NOAHMP LSM")
   END IF

   IF (IST == 1) THEN                                       
        END_WB = CANLIQ + CANICE + SNEQV + WA
        DO IZ = 1,NSOIL
          END_WB = END_WB + SMC(IZ) * DZSNSO(IZ) * 1000.
        END DO
        ERRWAT = END_WB-BEG_WB-(PRCP-ECAN-ETRAN-EDIR-RUNSRF-RUNSUB)*DT


        IF(ABS(ERRWAT) > 0.1) THEN
           if (ERRWAT > 0) then
              call wrf_message ('The model is gaining water (ERRWAT is positive)')
           else
              call wrf_message('The model is losing water (ERRWAT is negative)')
           endif
           write(message, *) 'ERRWAT =',ERRWAT, "kg m{-2} timestep{-1}"
           call wrf_message(trim(message))
           WRITE(message, &
           '("    I      J     END_WB     BEG_WB       PRCP       ECAN       EDIR      ETRAN      RUNSRF     RUNSUB")')
           call wrf_message(trim(message))
           WRITE(message,'(i6,1x,i6,1x,2f15.3,9f11.5)')ILOC,JLOC,END_WB,BEG_WB,PRCP*DT,ECAN*DT,&
                EDIR*DT,ETRAN*DT,RUNSRF*DT,RUNSUB*DT,ZWT
           call wrf_message(trim(message))
           call wrf_error_fatal3("<stdin>",1250,&
"Water budget problem in NOAHMP LSM")
        END IF

   ELSE                 
      ERRWAT = 0.0      
   ENDIF

 END SUBROUTINE ERROR


  SUBROUTINE ENERGY (ICE    ,VEGTYP ,IST    ,ISC    ,NSNOW  ,NSOIL  , & 
                     ISNOW  ,NROOT  ,DT     ,RHOAIR ,SFCPRS ,QAIR   , & 
                     SFCTMP ,THAIR  ,LWDN   ,UU     ,VV     ,ZREF   , & 
                     CO2AIR ,O2AIR  ,SOLAD  ,SOLAI  ,COSZ   ,IGS    , & 
                     EAIR   ,HTOP   ,TBOT   ,ZBOT   ,ZSNSO  ,ZSOIL  , & 
                     ELAI   ,ESAI   ,CSOIL  ,FWET   ,FOLN   ,         & 
                     FVEG   ,                                         & 
                     QSNOW  ,DZSNSO ,LAT    ,CANLIQ ,CANICE ,ILOC   , JLOC, & 
                     IMELT  ,SNICEV ,SNLIQV ,EPORE  ,T2M    ,FSNO   , & 
                     SAV    ,SAG    ,QMELT  ,FSA    ,FSR    ,TAUX   , & 
                     TAUY   ,FIRA   ,FSH    ,FCEV   ,FGEV   ,FCTR   , & 
                     TRAD   ,PSN    ,APAR   ,SSOIL  ,BTRANI ,BTRAN  , & 
                     PONDING,TS     ,LATHEAV , LATHEAG , frozen_canopy,frozen_ground,                       & 
                     TV     ,TG     ,STC    ,SNOWH  ,EAH    ,TAH    , & 
                     SNEQVO ,SNEQV  ,SH2O   ,SMC    ,SNICE  ,SNLIQ  , & 
                     ALBOLD ,CM     ,CH     ,DX     ,DZ8W   ,Q2     , &   
                     TAUSS  ,                                         & 

                     QC     ,PBLH   ,QSFC   ,PSFC   ,ISURBAN,IZ0TLND, & 
                     T2MV   ,T2MB   ,FSRV   , &
                     FSRG   ,RSSUN  ,RSSHA  ,BGAP   ,WGAP,TGV,TGB,&
                     Q1     ,Q2V    ,Q2B    ,Q2E    ,CHV  ,CHB, EMISSI,&
		     SHG,SHC,SHB,EVG,EVB,GHV,GHB,IRG,IRC,IRB,TR,EVC,CHLEAF,CHUC,CHV2,CHB2 )   




  USE NOAHMP_VEG_PARAMETERS
  USE NOAHMP_RAD_PARAMETERS

































  IMPLICIT NONE


  integer                           , INTENT(IN)    :: ILOC
  integer                           , INTENT(IN)    :: JLOC
  INTEGER                           , INTENT(IN)    :: ICE    
  INTEGER                           , INTENT(IN)    :: VEGTYP 
  INTEGER                           , INTENT(IN)    :: IST    
  INTEGER                           , INTENT(IN)    :: ISC    
  INTEGER                           , INTENT(IN)    :: NSNOW  
  INTEGER                           , INTENT(IN)    :: NSOIL  
  INTEGER                           , INTENT(IN)    :: NROOT  
  INTEGER                           , INTENT(IN)    :: ISNOW  
  REAL                              , INTENT(IN)    :: DT     
  REAL                              , INTENT(IN)    :: QSNOW  
  REAL                              , INTENT(IN)    :: RHOAIR 
  REAL                              , INTENT(IN)    :: EAIR   
  REAL                              , INTENT(IN)    :: SFCPRS 
  REAL                              , INTENT(IN)    :: QAIR   
  REAL                              , INTENT(IN)    :: SFCTMP 
  REAL                              , INTENT(IN)    :: THAIR  
  REAL                              , INTENT(IN)    :: LWDN   
  REAL                              , INTENT(IN)    :: UU     
  REAL                              , INTENT(IN)    :: VV     
  REAL   , DIMENSION(       1:    2), INTENT(IN)    :: SOLAD  
  REAL   , DIMENSION(       1:    2), INTENT(IN)    :: SOLAI  
  REAL                              , INTENT(IN)    :: COSZ   
  REAL                              , INTENT(IN)    :: ELAI   
  REAL                              , INTENT(IN)    :: ESAI   
  REAL                              , INTENT(IN)    :: CSOIL  
  REAL                              , INTENT(IN)    :: FWET   
  REAL                              , INTENT(IN)    :: HTOP   
  REAL                              , INTENT(IN)    :: FVEG   
  REAL                              , INTENT(IN)    :: LAT    
  REAL                              , INTENT(IN)    :: CANLIQ 
  REAL                              , INTENT(IN)    :: CANICE 
  REAL                              , INTENT(IN)    :: FOLN   
  REAL                              , INTENT(IN)    :: CO2AIR 
  REAL                              , INTENT(IN)    :: O2AIR  
  REAL                              , INTENT(IN)    :: IGS    

  REAL                              , INTENT(IN)    :: ZREF   
  REAL                              , INTENT(IN)    :: TBOT   
  REAL                              , INTENT(IN)    :: ZBOT   
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: ZSNSO  
  REAL   , DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: DZSNSO 


  INTEGER                           , INTENT(IN)    :: ISURBAN
  INTEGER                           , INTENT(IN)    :: IZ0TLND
  REAL                              , INTENT(IN)    :: QC     
  REAL                              , INTENT(IN)    :: PBLH   
  REAL                              , INTENT(INOUT) :: QSFC   
  REAL                              , INTENT(IN)    :: PSFC   
  REAL                              , INTENT(IN)    :: DX     
  REAL                              , INTENT(IN)    :: DZ8W   
  REAL                              , INTENT(IN)    :: Q2     



  INTEGER, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT)   :: IMELT  
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: SNICEV 
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: SNLIQV 
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: EPORE  
  REAL                              , INTENT(OUT)   :: FSNO   
  REAL                              , INTENT(OUT)   :: QMELT  
  REAL                              , INTENT(OUT)   :: PONDING
  REAL                              , INTENT(OUT)   :: SAV    
  REAL                              , INTENT(OUT)   :: SAG    
  REAL                              , INTENT(OUT)   :: FSA    
  REAL                              , INTENT(OUT)   :: FSR    
  REAL                              , INTENT(OUT)   :: TAUX   
  REAL                              , INTENT(OUT)   :: TAUY   
  REAL                              , INTENT(OUT)   :: FIRA   
  REAL                              , INTENT(OUT)   :: FSH    
  REAL                              , INTENT(OUT)   :: FCEV   
  REAL                              , INTENT(OUT)   :: FGEV   
  REAL                              , INTENT(OUT)   :: FCTR   
  REAL                              , INTENT(OUT)   :: TRAD   
  REAL                              , INTENT(OUT)   :: T2M    
  REAL                              , INTENT(OUT)   :: PSN    
  REAL                              , INTENT(OUT)   :: APAR   
  REAL                              , INTENT(OUT)   :: SSOIL  
  REAL   , DIMENSION(       1:NSOIL), INTENT(OUT)   :: BTRANI 
  REAL                              , INTENT(OUT)   :: BTRAN  

  REAL                              , INTENT(OUT)   :: LATHEAV 
  REAL                              , INTENT(OUT)   :: LATHEAG 
  LOGICAL                           , INTENT(OUT)   :: FROZEN_GROUND 
  LOGICAL                           , INTENT(OUT)   :: FROZEN_CANOPY 


  REAL                              , INTENT(OUT)   :: FSRV    
  REAL                              , INTENT(OUT)   :: FSRG    
  REAL, INTENT(OUT) :: RSSUN        
  REAL, INTENT(OUT) :: RSSHA        



  REAL                              , INTENT(OUT)   :: T2MV   
  REAL                              , INTENT(OUT)   :: T2MB   
  REAL                              , INTENT(OUT)   :: BGAP
  REAL                              , INTENT(OUT)   :: WGAP



  REAL                              , INTENT(INOUT) :: TS     
  REAL                              , INTENT(INOUT) :: TV     
  REAL                              , INTENT(INOUT) :: TG     
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    
  REAL                              , INTENT(INOUT) :: SNOWH  
  REAL                              , INTENT(INOUT) :: SNEQV  
  REAL                              , INTENT(INOUT) :: SNEQVO 
  REAL   , DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   
  REAL   , DIMENSION(       1:NSOIL), INTENT(INOUT) :: SMC    
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  
  REAL                              , INTENT(INOUT) :: EAH    
  REAL                              , INTENT(INOUT) :: TAH    
  REAL                              , INTENT(INOUT) :: ALBOLD 
  REAL                              , INTENT(INOUT) :: TAUSS  
  REAL                              , INTENT(INOUT) :: CM     
  REAL                              , INTENT(INOUT) :: CH     
  REAL                              , INTENT(INOUT) :: Q1

  REAL,                               INTENT(OUT)   :: EMISSI


  INTEGER                                           :: IZ     
  LOGICAL                                           :: VEG    
  REAL                                              :: UR     
  REAL                                              :: ZLVL   
  REAL                                              :: FSUN   
  REAL                                              :: RB     
  REAL                                              :: RSURF  
  REAL                                              :: L_RSURF
  REAL                                              :: D_RSURF
  REAL                                              :: BEVAP  
  REAL                                              :: MOL    
  REAL                                              :: VAI    
  REAL                                              :: CWP    
  REAL                                              :: ZPD    
  REAL                                              :: Z0M    
  REAL                                              :: ZPDG   
  REAL                                              :: Z0MG   
  REAL                                              :: EMV    
  REAL                                              :: EMG    
  REAL                                              :: FIRE   

  REAL                                              :: LAISUN 
  REAL                                              :: LAISHA 
  REAL                                              :: PSNSUN 
  REAL                                              :: PSNSHA 




  REAL                                              :: PARSUN 
  REAL                                              :: PARSHA 

  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: FACT   
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: DF     
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: HCPCT  
  REAL                                              :: BDSNO  
  REAL                                              :: FMELT  
  REAL                                              :: GX     
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: PHI    

  REAL                                              :: GAMMAV  
  REAL                                              :: GAMMAG  
  REAL                                              :: PSI    
  REAL                                              :: RHSUR  



  REAL                                              :: TAUXV  
  REAL                                              :: TAUYV  
  REAL,INTENT(OUT)                                              :: IRC    
  REAL,INTENT(OUT)                                              :: IRG    
  REAL,INTENT(OUT)                                              :: SHC    
  REAL,INTENT(OUT)                                              :: SHG    

  REAL,INTENT(OUT)                                  :: Q2V
  REAL,INTENT(OUT)                                  :: Q2B
  REAL,INTENT(OUT)                                  :: Q2E

  REAL,INTENT(OUT)                                              :: EVC    
  REAL,INTENT(OUT)                                              :: EVG    
  REAL,INTENT(OUT)                                              :: TR     
  REAL,INTENT(OUT)                                              :: GHV    
  REAL,INTENT(OUT)                                  :: TGV    
  REAL                                              :: CMV    
  REAL,INTENT(OUT)                                  :: CHV    



  REAL                                              :: TAUXB  
  REAL                                              :: TAUYB  
  REAL,INTENT(OUT)                                              :: IRB    
  REAL,INTENT(OUT)                                              :: SHB    
  REAL,INTENT(OUT)                                              :: EVB    
  REAL,INTENT(OUT)                                              :: GHB    
  REAL,INTENT(OUT)                                  :: TGB    
  REAL                                              :: CMB    
  REAL,INTENT(OUT)                                  :: CHB    
  REAL,INTENT(OUT)                                  :: CHLEAF 
  REAL,INTENT(OUT)                                  :: CHUC   

  REAL,INTENT(OUT)                                  :: CHV2    
  REAL,INTENT(OUT)                                  :: CHB2    
  REAL                                  :: noahmpres



  REAL, PARAMETER                   :: MPE    = 1.E-6
  REAL, PARAMETER                   :: PSIWLT = -150.  
  REAL, PARAMETER                   :: Z0     = 0.01   




    TAUXV     = 0.    
    TAUYV     = 0.
    IRC       = 0.
    SHC       = 0.
    IRG       = 0.
    SHG       = 0.
    EVG       = 0.       
    EVC       = 0.
    TR        = 0.
    GHV       = 0.       
    PSNSUN    = 0.
    PSNSHA    = 0.
    T2MV      = 0.
    Q2V       = 0.
    CHV       = 0.
    CHLEAF    = 0.
    CHUC      = 0.
    CHV2      = 0.



    UR = MAX( SQRT(UU**2.+VV**2.), 1. )



    VAI = ELAI + ESAI
    VEG = .FALSE.
    IF(VAI > 0.) VEG = .TRUE.



     FSNO = 0.
     IF(SNOWH.GT.0.)  THEN
         BDSNO    = SNEQV / SNOWH
         FMELT    = (BDSNO/100.)**M
         FSNO     = TANH( SNOWH /(2.5* Z0 * FMELT))
     ENDIF



     IF(IST == 2) THEN
       IF(TG .LE. TFRZ) THEN
         Z0MG = 0.01 * (1.0-FSNO) + FSNO * Z0SNO
       ELSE
         Z0MG = 0.01  
       END IF
     ELSE
       Z0MG = Z0 * (1.0-FSNO) + FSNO * Z0SNO
     END IF



     ZPDG  = SNOWH
     IF(VEG) THEN
        Z0M  = Z0MVT(VEGTYP)
        ZPD  = 0.65 * HTOP
        IF(SNOWH.GT.ZPD) ZPD  = SNOWH
     ELSE
        Z0M  = Z0MG
        ZPD  = ZPDG
     END IF

     ZLVL = MAX(ZPD,HTOP) + ZREF
     IF(ZPDG >= ZLVL) ZLVL = ZPDG + ZREF




     CWP = CWPVT(VEGTYP)



  CALL THERMOPROP (NSOIL   ,NSNOW   ,ISNOW   ,IST     ,DZSNSO  , & 
                   DT      ,SNOWH   ,SNICE   ,SNLIQ   ,CSOIL   , & 
                   SMC     ,SH2O    ,TG      ,STC     ,UR      , & 
                   LAT     ,Z0M     ,ZLVL    ,VEGTYP  ,ISURBAN , & 
                   DF      ,HCPCT   ,SNICEV  ,SNLIQV  ,EPORE   , & 
                   FACT    )                              



  CALL  RADIATION (VEGTYP  ,IST     ,ISC     ,ICE     ,NSOIL   , & 
                   SNEQVO  ,SNEQV   ,DT      ,COSZ    ,SNOWH   , & 
                   TG      ,TV      ,FSNO    ,QSNOW   ,FWET    , & 
                   ELAI    ,ESAI    ,SMC     ,SOLAD   ,SOLAI   , & 
                   FVEG    ,ILOC    ,JLOC    ,                   & 
                   ALBOLD  ,TAUSS   ,                            & 
                   FSUN    ,LAISUN  ,LAISHA  ,PARSUN  ,PARSHA  , & 
                   SAV     ,SAG     ,FSR     ,FSA     ,FSRV    , & 
                   FSRG    ,BGAP    ,WGAP    )            



     EMV = 1. - EXP(-(ELAI+ESAI)/1.0)
     IF (ICE == 1) THEN
       EMG = 0.98*(1.-FSNO) + 1.0*FSNO
     ELSE
       EMG = EG(IST)*(1.-FSNO) + 1.0*FSNO
     END IF


   
     BTRAN = 0.

     IF(IST ==1 ) THEN
       DO IZ = 1, NROOT
          IF(OPT_BTR == 1) then                  
            GX    = (SH2O(IZ)-SMCWLT) / (SMCREF-SMCWLT)
          END IF
          IF(OPT_BTR == 2) then                  
            PSI   = MAX(PSIWLT,-PSISAT*(MAX(0.01,SH2O(IZ))/SMCMAX)**(-BEXP) )
            GX    = (1.-PSI/PSIWLT)/(1.+PSISAT/PSIWLT)
          END IF
          IF(OPT_BTR == 3) then                  
            PSI   = MAX(PSIWLT,-PSISAT*(MAX(0.01,SH2O(IZ))/SMCMAX)**(-BEXP) )
            GX    = 1.-EXP(-5.8*(LOG(PSIWLT/PSI))) 
          END IF
       
          GX = MIN(1.,MAX(0.,GX))
          BTRANI(IZ) = MAX(MPE,DZSNSO(IZ) / (-ZSOIL(NROOT)) * GX)
          BTRAN      = BTRAN + BTRANI(IZ)
       END DO
       BTRAN = MAX(MPE,BTRAN)

       BTRANI(1:NROOT) = BTRANI(1:NROOT)/BTRAN
     END IF



     BEVAP = MAX(0.0,SH2O(1)/SMCMAX)
     IF(IST == 2) THEN
       RSURF = 1.          
       RHSUR = 1.0
     ELSE

        
        
        
        L_RSURF = (-ZSOIL(1)) * ( exp ( (1.0 - MIN(1.0,SH2O(1)/SMCMAX)) ** 5 ) - 1.0 ) / ( 2.71828 - 1.0 ) 
        D_RSURF = 2.2E-5 * SMCMAX * SMCMAX * ( 1.0 - SMCWLT / SMCMAX ) ** (2.0+3.0/BEXP)
        RSURF = L_RSURF / D_RSURF

        
        
        

       IF(SH2O(1) < 0.01 .and. SNOWH == 0.) RSURF = 1.E6
       PSI   = -PSISAT*(MAX(0.01,SH2O(1))/SMCMAX)**(-BEXP)   
       RHSUR = FSNO + (1.-FSNO) * EXP(PSI*GRAV/(RW*TG)) 
     END IF


     IF (VEGTYP == ISURBAN .and. SNOWH == 0. ) THEN
        RSURF = 1.E6
     ENDIF



     IF (TV .GT. TFRZ) THEN           
        LATHEAV = HVAP                
	frozen_canopy = .false.
     ELSE
        LATHEAV = HSUB
	frozen_canopy = .true.
     END IF
     GAMMAV = CPAIR*SFCPRS/(0.622*LATHEAV)

     IF (TG .GT. TFRZ) THEN
        LATHEAG = HVAP
	frozen_ground = .false.
     ELSE
        LATHEAG = HSUB
	frozen_ground = .true.
     END IF
     GAMMAG = CPAIR*SFCPRS/(0.622*LATHEAG)










    IF (VEG .AND. FVEG > 0) THEN 
    TGV = TG
    CMV = CM
    CHV = CH
    CALL VEGE_FLUX (NSNOW   ,NSOIL   ,ISNOW   ,VEGTYP  ,VEG     , & 
                    DT      ,SAV     ,SAG     ,LWDN    ,UR      , & 
                    UU      ,VV      ,SFCTMP  ,THAIR   ,QAIR    , & 
                    EAIR    ,RHOAIR  ,SNOWH   ,VAI     ,GAMMAV   ,GAMMAG   , & 
                    FWET    ,LAISUN  ,LAISHA  ,CWP     ,DZSNSO  , & 
                    HTOP    ,ZLVL    ,ZPD     ,Z0M     ,FVEG    , & 
                    Z0MG    ,EMV     ,EMG     ,CANLIQ           , & 
                    CANICE  ,STC     ,DF      ,RSSUN   ,RSSHA   , & 
                    RSURF   ,LATHEAV ,LATHEAG ,PARSUN  ,PARSHA  ,IGS     , & 
                    FOLN    ,CO2AIR  ,O2AIR   ,BTRAN   ,SFCPRS  , & 
                    RHSUR   ,ILOC    ,JLOC    ,Q2      , & 
                    EAH     ,TAH     ,TV      ,TGV     ,CMV     , & 
                    CHV     ,DX      ,DZ8W    ,                   & 
                    TAUXV   ,TAUYV   ,IRG     ,IRC     ,SHG     , & 
                    SHC     ,EVG     ,EVC     ,TR      ,GHV     , & 
                    T2MV    ,PSNSUN  ,PSNSHA  ,                   & 

                    QC      ,PBLH    ,QSFC    ,PSFC    ,ISURBAN , & 
                    IZ0TLND ,Q2V     ,CHV2, CHLEAF, CHUC)               

    END IF

    TGB = TG
    CMB = CM
    CHB = CH
    CALL BARE_FLUX (NSNOW   ,NSOIL   ,ISNOW   ,DT      ,SAG     , & 
                    LWDN    ,UR      ,UU      ,VV      ,SFCTMP  , & 
                    THAIR   ,QAIR    ,EAIR    ,RHOAIR  ,SNOWH   , & 
                    DZSNSO  ,ZLVL    ,ZPDG    ,Z0MG    ,          & 
                    EMG     ,STC     ,DF      ,RSURF   ,LATHEAG  , & 
                    GAMMAG   ,RHSUR   ,ILOC    ,JLOC    ,Q2      , & 
                    TGB     ,CMB     ,CHB     ,                   & 
                    TAUXB   ,TAUYB   ,IRB     ,SHB     ,EVB     , & 
                    GHB     ,T2MB    ,DX      ,DZ8W    ,VEGTYP  , & 

                    QC      ,PBLH    ,QSFC    ,PSFC    ,ISURBAN , & 
                    IZ0TLND ,SFCPRS  ,Q2B,   CHB2)                          






    IF (VEG .AND. FVEG > 0) THEN 
        TAUX  = FVEG * TAUXV     + (1.0 - FVEG) * TAUXB
        TAUY  = FVEG * TAUYV     + (1.0 - FVEG) * TAUYB
        FIRA  = FVEG * IRG       + (1.0 - FVEG) * IRB       + IRC
        FSH   = FVEG * SHG       + (1.0 - FVEG) * SHB       + SHC
        FGEV  = FVEG * EVG       + (1.0 - FVEG) * EVB
        SSOIL = FVEG * GHV       + (1.0 - FVEG) * GHB
        FCEV  = EVC
        FCTR  = TR
        TG    = FVEG * TGV       + (1.0 - FVEG) * TGB
        T2M   = FVEG * T2MV      + (1.0 - FVEG) * T2MB
        TS    = FVEG * TV        + (1.0 - FVEG) * TGB
        CM    = FVEG * CMV       + (1.0 - FVEG) * CMB      
        CH    = FVEG * CHV       + (1.0 - FVEG) * CHB
        Q1    = FVEG * (EAH*0.622/(SFCPRS - 0.378*EAH)) + (1.0 - FVEG)*QSFC
        Q2E   = FVEG * Q2V       + (1.0 - FVEG) * Q2B
    ELSE
        TAUX  = TAUXB
        TAUY  = TAUYB
        FIRA  = IRB
        FSH   = SHB
        FGEV  = EVB
        SSOIL = GHB
        TG    = TGB
        T2M   = T2MB
        FCEV  = 0.
        FCTR  = 0.
        TS    = TG
        CM    = CMB
        CH    = CHB
        Q1    = QSFC
        Q2E   = Q2B
        RSSUN = 0.0
        RSSHA = 0.0
        TGV   = TGB
        CHV   = CHB
    END IF

    FIRE = LWDN + FIRA

    IF(FIRE <=0.) THEN
       WRITE(6,*) 'emitted longwave <0; skin T may be wrong due to inconsistent'
       WRITE(6,*) 'input of SHDFAC with LAI'
       WRITE(6,*) ILOC, JLOC, 'SHDFAC=',FVEG,'VAI=',VAI,'TV=',TV,'TG=',TG
       WRITE(6,*) 'LWDN=',LWDN,'FIRA=',FIRA,'SNOWH=',SNOWH
       call wrf_error_fatal3("<stdin>",1822,&
"STOP in Noah-MP")
    END IF

    
    EMISSI = FVEG * ( EMG*(1-EMV) + EMV + EMV*(1-EMV)*(1-EMG) ) + &
         (1-FVEG) * EMG

    
    
    
    
    TRAD = ( ( FIRE - (1-EMISSI)*LWDN ) / (EMISSI*SB) ) ** 0.25

    
    

    APAR = PARSUN*LAISUN + PARSHA*LAISHA
    PSN  = PSNSUN*LAISUN + PSNSHA*LAISHA



    CALL TSNOSOI (ICE     ,NSOIL   ,NSNOW   ,ISNOW   ,IST     , & 
                  TBOT    ,ZSNSO   ,SSOIL   ,DF      ,HCPCT   , & 
                  ZBOT    ,SAG     ,DT      ,SNOWH   ,DZSNSO  , & 
                  TG      ,ILOC    ,JLOC    ,                   & 
                  STC     )                                       


     IF(OPT_STC == 2) THEN
      IF (SNOWH > 0.05 .AND. TG > TFRZ) THEN
        TGV = TFRZ
        TGB = TFRZ
          IF (VEG .AND. FVEG > 0) THEN
             TG    = FVEG * TGV       + (1.0 - FVEG) * TGB
             TS    = FVEG * TV        + (1.0 - FVEG) * TGB
          ELSE
             TG    = TGB
             TS    = TGB
          END IF
      END IF
     END IF



 CALL PHASECHANGE (NSNOW   ,NSOIL   ,ISNOW   ,DT      ,FACT    , & 
                   DZSNSO  ,HCPCT   ,IST     ,ILOC    ,JLOC    , & 
                   STC     ,SNICE   ,SNLIQ   ,SNEQV   ,SNOWH   , & 
                   SMC     ,SH2O    ,                            & 
                   QMELT   ,IMELT   ,PONDING )                     


  END SUBROUTINE ENERGY

  SUBROUTINE THERMOPROP (NSOIL   ,NSNOW   ,ISNOW   ,IST     ,DZSNSO  , & 
                         DT      ,SNOWH   ,SNICE   ,SNLIQ   ,CSOIL   , & 
                         SMC     ,SH2O    ,TG      ,STC     ,UR      , & 
                         LAT     ,Z0M     ,ZLVL    ,VEGTYP  ,ISURBAN , & 
                         DF      ,HCPCT   ,SNICEV  ,SNLIQV  ,EPORE   , & 
                         FACT    )                                       


  IMPLICIT NONE


  INTEGER                        , INTENT(IN)  :: NSOIL   
  INTEGER                        , INTENT(IN)  :: NSNOW   
  INTEGER                        , INTENT(IN)  :: ISNOW   
  INTEGER                        , INTENT(IN)  :: IST     
  REAL                           , INTENT(IN)  :: DT      
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)  :: SNICE   
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)  :: SNLIQ   
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DZSNSO  
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)  :: SMC     
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)  :: SH2O    
  REAL                           , INTENT(IN)  :: SNOWH   
  REAL                           , INTENT(IN)  :: CSOIL   
  REAL,                            INTENT(IN)  :: TG      
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: STC     
  REAL,                            INTENT(IN)  :: UR      
  REAL,                            INTENT(IN)  :: LAT     
  REAL,                            INTENT(IN)  :: Z0M     
  REAL,                            INTENT(IN)  :: ZLVL    
  INTEGER                        , INTENT(IN)  :: VEGTYP  
  INTEGER                        , INTENT(IN)  :: ISURBAN 


  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: DF      
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: HCPCT   
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNICEV  
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNLIQV  
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: EPORE   
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: FACT    



  INTEGER :: IZ
  REAL, DIMENSION(-NSNOW+1:    0)              :: CVSNO   
  REAL, DIMENSION(-NSNOW+1:    0)              :: TKSNO   
  REAL, DIMENSION(       1:NSOIL)              :: SICE    




    CALL CSNOW (ISNOW   ,NSNOW   ,NSOIL   ,SNICE   ,SNLIQ   ,DZSNSO  , & 
                TKSNO   ,CVSNO   ,SNICEV  ,SNLIQV  ,EPORE   )   

    DO IZ = ISNOW+1, 0
      DF   (IZ) = TKSNO(IZ)
      HCPCT(IZ) = CVSNO(IZ)
    END DO



    DO  IZ = 1, NSOIL
       SICE(IZ)  = SMC(IZ) - SH2O(IZ)
       HCPCT(IZ) = SH2O(IZ)*CWAT + (1.0-SMCMAX)*CSOIL &
                + (SMCMAX-SMC(IZ))*CPAIR + SICE(IZ)*CICE
       CALL TDFCND (DF(IZ), SMC(IZ), SH2O(IZ))
    END DO
       
    IF ( VEGTYP == ISURBAN ) THEN
       DO IZ = 1,NSOIL
         DF(IZ) = 3.24
       END DO
    ENDIF










    IF(IST == 2) THEN
       DO IZ = 1, NSOIL 
         IF(STC(IZ) > TFRZ) THEN
            HCPCT(IZ) = CWAT
            DF(IZ)    = TKWAT  
         ELSE
            HCPCT(IZ) = CICE
            DF(IZ)    = TKICE 
         END IF
       END DO
    END IF



    DO IZ = ISNOW+1,NSOIL
     FACT(IZ) = DT/(HCPCT(IZ)*DZSNSO(IZ))
    END DO



    IF(ISNOW == 0) THEN
       DF(1) = (DF(1)*DZSNSO(1)+0.35*SNOWH)      / (SNOWH    +DZSNSO(1)) 
    ELSE
       DF(1) = (DF(1)*DZSNSO(1)+DF(0)*DZSNSO(0)) / (DZSNSO(0)+DZSNSO(1))
    END IF


  END SUBROUTINE THERMOPROP


  SUBROUTINE CSNOW (ISNOW   ,NSNOW   ,NSOIL   ,SNICE   ,SNLIQ   ,DZSNSO  , & 
                    TKSNO   ,CVSNO   ,SNICEV  ,SNLIQV  ,EPORE   )   



  IMPLICIT NONE



  INTEGER,                          INTENT(IN) :: ISNOW  
  INTEGER                        ,  INTENT(IN) :: NSNOW  
  INTEGER                        ,  INTENT(IN) :: NSOIL  
  REAL, DIMENSION(-NSNOW+1:    0),  INTENT(IN) :: SNICE  
  REAL, DIMENSION(-NSNOW+1:    0),  INTENT(IN) :: SNLIQ  
  REAL, DIMENSION(-NSNOW+1:NSOIL),  INTENT(IN) :: DZSNSO 



  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: CVSNO  
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: TKSNO  
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNICEV 
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNLIQV 
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: EPORE  



  INTEGER :: IZ
  REAL, DIMENSION(-NSNOW+1:    0) :: BDSNOI  




  DO IZ = ISNOW+1, 0
      SNICEV(IZ)   = MIN(1., SNICE(IZ)/(DZSNSO(IZ)*DENICE) )
      EPORE(IZ)    = 1. - SNICEV(IZ)
      SNLIQV(IZ)   = MIN(EPORE(IZ),SNLIQ(IZ)/(DZSNSO(IZ)*DENH2O))
  ENDDO

  DO IZ = ISNOW+1, 0
      BDSNOI(IZ) = (SNICE(IZ)+SNLIQ(IZ))/DZSNSO(IZ)
      CVSNO(IZ) = CICE*SNICEV(IZ)+CWAT*SNLIQV(IZ)

  enddo



  DO IZ = ISNOW+1, 0
     TKSNO(IZ) = 3.2217E-6*BDSNOI(IZ)**2.           




  ENDDO

  END SUBROUTINE CSNOW


  SUBROUTINE TDFCND ( DF, SMC, SH2O)







    IMPLICIT NONE
    REAL, INTENT(IN)       :: SMC    
    REAL, INTENT(IN)       :: SH2O   
    REAL, INTENT(OUT)      :: DF     


    REAL  :: AKE
    REAL  :: GAMMD
    REAL  :: THKDRY
    REAL  :: THKO     
    REAL  :: THKQTZ   
    REAL  :: THKSAT   
    REAL  :: THKS     
    REAL  :: THKW     
    REAL  :: SATRATIO
    REAL  :: XU
    REAL  :: XUNFROZ




























    SATRATIO = SMC / SMCMAX
    THKW = 0.57

    THKO = 2.0


    THKQTZ = 7.7


    THKS = (THKQTZ ** QUARTZ)* (THKO ** (1. - QUARTZ))


    XUNFROZ = SH2O / SMC

    XU = XUNFROZ * SMCMAX


    THKSAT = THKS ** (1. - SMCMAX)* TKICE ** (SMCMAX - XU)* THKW **   &
         (XU)


    GAMMD = (1. - SMCMAX)*2700.

    THKDRY = (0.135* GAMMD+ 64.7)/ (2700. - 0.947* GAMMD)

    IF ( (SH2O + 0.0005) <  SMC ) THEN
       AKE = SATRATIO


    ELSE





       IF ( SATRATIO >  0.1 ) THEN

          AKE = LOG10 (SATRATIO) + 1.0


       ELSE

          AKE = 0.0
       END IF


    END IF

    DF = AKE * (THKSAT - THKDRY) + THKDRY


  end subroutine TDFCND

  SUBROUTINE RADIATION (VEGTYP  ,IST     ,ISC     ,ICE     ,NSOIL   , & 
                        SNEQVO  ,SNEQV   ,DT      ,COSZ    ,SNOWH   , & 
                        TG      ,TV      ,FSNO    ,QSNOW   ,FWET    , & 
                        ELAI    ,ESAI    ,SMC     ,SOLAD   ,SOLAI   , & 
                        FVEG    ,ILOC    ,JLOC    ,                   & 
                        ALBOLD  ,TAUSS   ,                            & 
                        FSUN    ,LAISUN  ,LAISHA  ,PARSUN  ,PARSHA  , & 
                        SAV     ,SAG     ,FSR     ,FSA     ,FSRV    , &
                        FSRG    ,BGAP    ,WGAP)            

  IMPLICIT NONE


  INTEGER, INTENT(IN)                  :: ILOC
  INTEGER, INTENT(IN)                  :: JLOC
  INTEGER, INTENT(IN)                  :: VEGTYP 
  INTEGER, INTENT(IN)                  :: IST    
  INTEGER, INTENT(IN)                  :: ISC    
  INTEGER, INTENT(IN)                  :: ICE    
  INTEGER, INTENT(IN)                  :: NSOIL  

  REAL, INTENT(IN)                     :: DT     
  REAL, INTENT(IN)                     :: QSNOW  
  REAL, INTENT(IN)                     :: SNEQVO 
  REAL, INTENT(IN)                     :: SNEQV  
  REAL, INTENT(IN)                     :: SNOWH  
  REAL, INTENT(IN)                     :: COSZ   
  REAL, INTENT(IN)                     :: TG     
  REAL, INTENT(IN)                     :: TV     
  REAL, INTENT(IN)                     :: ELAI   
  REAL, INTENT(IN)                     :: ESAI   
  REAL, INTENT(IN)                     :: FWET   
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SMC    
  REAL, DIMENSION(1:2)    , INTENT(IN) :: SOLAD  
  REAL, DIMENSION(1:2)    , INTENT(IN) :: SOLAI  
  REAL, INTENT(IN)                     :: FSNO   
  REAL, INTENT(IN)                     :: FVEG   


  REAL,                  INTENT(INOUT) :: ALBOLD 
  REAL,                  INTENT(INOUT) :: TAUSS  


  REAL, INTENT(OUT)                    :: FSUN   
  REAL, INTENT(OUT)                    :: LAISUN 
  REAL, INTENT(OUT)                    :: LAISHA 
  REAL, INTENT(OUT)                    :: PARSUN 
  REAL, INTENT(OUT)                    :: PARSHA 
  REAL, INTENT(OUT)                    :: SAV    
  REAL, INTENT(OUT)                    :: SAG    
  REAL, INTENT(OUT)                    :: FSA    
  REAL, INTENT(OUT)                    :: FSR    


  REAL, INTENT(OUT)                    :: FSRV    
  REAL, INTENT(OUT)                    :: FSRG    
  REAL, INTENT(OUT)                    :: BGAP
  REAL, INTENT(OUT)                    :: WGAP



  REAL                                 :: FAGE   
  REAL, DIMENSION(1:2)                 :: ALBGRD 
  REAL, DIMENSION(1:2)                 :: ALBGRI 
  REAL, DIMENSION(1:2)                 :: ALBD   
  REAL, DIMENSION(1:2)                 :: ALBI   
  REAL, DIMENSION(1:2)                 :: FABD   
  REAL, DIMENSION(1:2)                 :: FABI   
  REAL, DIMENSION(1:2)                 :: FTDD   
  REAL, DIMENSION(1:2)                 :: FTID   
  REAL, DIMENSION(1:2)                 :: FTII   

  REAL, DIMENSION(1:2)                 :: FREVI
  REAL, DIMENSION(1:2)                 :: FREVD
  REAL, DIMENSION(1:2)                 :: FREGI
  REAL, DIMENSION(1:2)                 :: FREGD


  REAL                                 :: FSHA   
  REAL                                 :: VAI    

  REAL,PARAMETER :: MPE = 1.E-6
  LOGICAL VEG  





   CALL ALBEDO (VEGTYP ,IST    ,ISC    ,ICE    ,NSOIL  , & 
                DT     ,COSZ   ,FAGE   ,ELAI   ,ESAI   , & 
                TG     ,TV     ,SNOWH  ,FSNO   ,FWET   , & 
                SMC    ,SNEQVO ,SNEQV  ,QSNOW  ,FVEG   , & 
                ILOC   ,JLOC   ,                         & 
                ALBOLD ,TAUSS                          , & 
                ALBGRD ,ALBGRI ,ALBD   ,ALBI   ,FABD   , & 
                FABI   ,FTDD   ,FTID   ,FTII   ,FSUN   , & 
                FREVI  ,FREVD   ,FREGD ,FREGI  ,BGAP   , & 
                WGAP)



     FSHA = 1.-FSUN
     LAISUN = ELAI*FSUN
     LAISHA = ELAI*FSHA
     VAI = ELAI+ ESAI
     IF (VAI .GT. 0.) THEN
        VEG = .TRUE.
     ELSE
        VEG = .FALSE.
     END IF

   CALL SURRAD (MPE    ,FSUN   ,FSHA   ,ELAI   ,VAI    , & 
                LAISUN ,LAISHA ,SOLAD  ,SOLAI  ,FABD   , & 
                FABI   ,FTDD   ,FTID   ,FTII   ,ALBGRD , & 
                ALBGRI ,ALBD   ,ALBI   ,ILOC   ,JLOC   , & 
                PARSUN ,PARSHA ,SAV    ,SAG    ,FSA    , & 
                FSR    ,                                 & 
                FREVI  ,FREVD  ,FREGD  ,FREGI  ,FSRV   , & 
                FSRG)

  END SUBROUTINE RADIATION


  SUBROUTINE ALBEDO (VEGTYP ,IST    ,ISC    ,ICE    ,NSOIL  , & 
                     DT     ,COSZ   ,FAGE   ,ELAI   ,ESAI   , & 
                     TG     ,TV     ,SNOWH  ,FSNO   ,FWET   , & 
                     SMC    ,SNEQVO ,SNEQV  ,QSNOW  ,FVEG   , & 
                     ILOC   ,JLOC   ,                         & 
                     ALBOLD ,TAUSS                          , & 
                     ALBGRD ,ALBGRI ,ALBD   ,ALBI   ,FABD   , & 
                     FABI   ,FTDD   ,FTID   ,FTII   ,FSUN   , & 
                     FREVI  ,FREVD  ,FREGD  ,FREGI  ,BGAP   , & 
                     WGAP)






  USE NOAHMP_VEG_PARAMETERS

  IMPLICIT NONE


  INTEGER,                  INTENT(IN)  :: ILOC
  INTEGER,                  INTENT(IN)  :: JLOC
  INTEGER,                  INTENT(IN)  :: NSOIL  
  INTEGER,                  INTENT(IN)  :: VEGTYP 
  INTEGER,                  INTENT(IN)  :: IST    
  INTEGER,                  INTENT(IN)  :: ISC    
  INTEGER,                  INTENT(IN)  :: ICE    

  REAL,                     INTENT(IN)  :: DT     
  REAL,                     INTENT(IN)  :: QSNOW  
  REAL,                     INTENT(IN)  :: COSZ   
  REAL,                     INTENT(IN)  :: SNOWH  
  REAL,                     INTENT(IN)  :: TG     
  REAL,                     INTENT(IN)  :: TV     
  REAL,                     INTENT(IN)  :: ELAI   
  REAL,                     INTENT(IN)  :: ESAI   
  REAL,                     INTENT(IN)  :: FSNO   
  REAL,                     INTENT(IN)  :: FWET   
  REAL,                     INTENT(IN)  :: SNEQVO 
  REAL,                     INTENT(IN)  :: SNEQV  
  REAL,                     INTENT(IN)  :: FVEG   
  REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SMC    


  REAL,                  INTENT(INOUT)  :: ALBOLD 
  REAL,                  INTENT(INOUT)  :: TAUSS  


  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBGRD 
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBGRI 
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBD   
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBI   
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FABD   
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FABI   
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FTDD   
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FTID   
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FTII   
  REAL,                     INTENT(OUT) :: FSUN   

  REAL, DIMENSION(1:    2), INTENT(OUT) :: FREVD
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FREVI
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FREGD
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FREGI
  REAL, INTENT(OUT) :: BGAP
  REAL, INTENT(OUT) :: WGAP





  REAL                 :: FAGE     
  REAL                 :: ALB
  INTEGER              :: IB       
  INTEGER              :: NBAND    
  INTEGER              :: IC       

  REAL                 :: WL       
  REAL                 :: WS       
  REAL                 :: MPE      

  REAL, DIMENSION(1:2) :: RHO      
  REAL, DIMENSION(1:2) :: TAU      
  REAL, DIMENSION(1:2) :: FTDI     
  REAL, DIMENSION(1:2) :: ALBSND   
  REAL, DIMENSION(1:2) :: ALBSNI   

  REAL                 :: VAI      
  REAL                 :: GDIR     
  REAL                 :: EXT      



  NBAND = 2
  MPE = 1.E-06
  BGAP = 0.
  WGAP = 0.



  DO IB = 1, NBAND
    ALBD(IB) = 0.
    ALBI(IB) = 0.
    ALBGRD(IB) = 0.
    ALBGRI(IB) = 0.
    FABD(IB) = 0.
    FABI(IB) = 0.
    FTDD(IB) = 0.
    FTID(IB) = 0.
    FTII(IB) = 0.
    IF (IB.EQ.1) FSUN = 0.
  END DO

  IF(COSZ <= 0) GOTO 100



  DO IB = 1, NBAND
    VAI = ELAI + ESAI
    WL  = ELAI / MAX(VAI,MPE)
    WS  = ESAI / MAX(VAI,MPE)
    RHO(IB) = MAX(RHOL(VEGTYP,IB)*WL+RHOS(VEGTYP,IB)*WS, MPE)
    TAU(IB) = MAX(TAUL(VEGTYP,IB)*WL+TAUS(VEGTYP,IB)*WS, MPE)
  END DO



   CALL SNOW_AGE (DT,TG,SNEQVO,SNEQV,TAUSS,FAGE)



  IF(OPT_ALB == 1) &
     CALL SNOWALB_BATS (NBAND, FSNO,COSZ,FAGE,ALBSND,ALBSNI)
  IF(OPT_ALB == 2) THEN
     CALL SNOWALB_CLASS (NBAND,QSNOW,DT,ALB,ALBOLD,ALBSND,ALBSNI,ILOC,JLOC)
     ALBOLD = ALB
  END IF



  CALL GROUNDALB (NSOIL   ,NBAND   ,ICE     ,IST     ,ISC     , & 
                  FSNO    ,SMC     ,ALBSND  ,ALBSNI  ,COSZ    , & 
                  TG      ,ILOC    ,JLOC    ,                   & 
                  ALBGRD  ,ALBGRI  )                              




  DO IB = 1, NBAND
      IC = 0      
      CALL TWOSTREAM (IB     ,IC      ,VEGTYP  ,COSZ    ,VAI    , & 
                      FWET   ,TV      ,ALBGRD  ,ALBGRI  ,RHO    , & 
                      TAU    ,FVEG    ,IST     ,ILOC    ,JLOC   , & 
                      FABD   ,ALBD    ,FTDD    ,FTID    ,GDIR   , &
                      FREVD  ,FREGD   ,BGAP    ,WGAP)

      IC = 1      
      CALL TWOSTREAM (IB     ,IC      ,VEGTYP  ,COSZ    ,VAI    , & 
                      FWET   ,TV      ,ALBGRD  ,ALBGRI  ,RHO    , & 
                      TAU    ,FVEG    ,IST     ,ILOC    ,JLOC   , & 
                      FABI   ,ALBI    ,FTDI    ,FTII    ,GDIR   , & 
                      FREVI  ,FREGI   ,BGAP    ,WGAP)

  END DO



  EXT = GDIR/COSZ * SQRT(1.-RHO(1)-TAU(1))
  FSUN = (1.-EXP(-EXT*VAI)) / MAX(EXT*VAI,MPE)
  EXT = FSUN

  IF (EXT .LT. 0.01) THEN
     WL = 0.
  ELSE
     WL = EXT 
  END IF
  FSUN = WL

100 CONTINUE

  END SUBROUTINE ALBEDO


  SUBROUTINE SURRAD (MPE     ,FSUN    ,FSHA    ,ELAI    ,VAI     , & 
                     LAISUN  ,LAISHA  ,SOLAD   ,SOLAI   ,FABD    , & 
                     FABI    ,FTDD    ,FTID    ,FTII    ,ALBGRD  , & 
                     ALBGRI  ,ALBD    ,ALBI    ,ILOC    ,JLOC    , & 
                     PARSUN  ,PARSHA  ,SAV     ,SAG     ,FSA     , & 
                     FSR     , & 
                     FREVI   ,FREVD   ,FREGD   ,FREGI   ,FSRV    , &
                     FSRG) 


  IMPLICIT NONE



  INTEGER, INTENT(IN)              :: ILOC
  INTEGER, INTENT(IN)              :: JLOC
  REAL, INTENT(IN)                 :: MPE     

  REAL, INTENT(IN)                 :: FSUN    
  REAL, INTENT(IN)                 :: FSHA    
  REAL, INTENT(IN)                 :: ELAI    
  REAL, INTENT(IN)                 :: VAI     
  REAL, INTENT(IN)                 :: LAISUN  
  REAL, INTENT(IN)                 :: LAISHA  

  REAL, DIMENSION(1:2), INTENT(IN) :: SOLAD   
  REAL, DIMENSION(1:2), INTENT(IN) :: SOLAI   
  REAL, DIMENSION(1:2), INTENT(IN) :: FABD    
  REAL, DIMENSION(1:2), INTENT(IN) :: FABI    
  REAL, DIMENSION(1:2), INTENT(IN) :: FTDD    
  REAL, DIMENSION(1:2), INTENT(IN) :: FTID    
  REAL, DIMENSION(1:2), INTENT(IN) :: FTII    
  REAL, DIMENSION(1:2), INTENT(IN) :: ALBGRD  
  REAL, DIMENSION(1:2), INTENT(IN) :: ALBGRI  
  REAL, DIMENSION(1:2), INTENT(IN) :: ALBD    
  REAL, DIMENSION(1:2), INTENT(IN) :: ALBI    

  REAL, DIMENSION(1:2), INTENT(IN) :: FREVD    
  REAL, DIMENSION(1:2), INTENT(IN) :: FREVI    
  REAL, DIMENSION(1:2), INTENT(IN) :: FREGD    
  REAL, DIMENSION(1:2), INTENT(IN) :: FREGI    



  REAL, INTENT(OUT)                :: PARSUN  
  REAL, INTENT(OUT)                :: PARSHA  
  REAL, INTENT(OUT)                :: SAV     
  REAL, INTENT(OUT)                :: SAG     
  REAL, INTENT(OUT)                :: FSA     
  REAL, INTENT(OUT)                :: FSR     
  REAL, INTENT(OUT)                :: FSRV    
  REAL, INTENT(OUT)                :: FSRG    


  INTEGER                          :: IB      
  INTEGER                          :: NBAND   

  REAL                             :: ABS     
  REAL                             :: RNIR    
  REAL                             :: RVIS    
  REAL                             :: LAIFRA  
  REAL                             :: TRD     
  REAL                             :: TRI     
  REAL, DIMENSION(1:2)             :: CAD     
  REAL, DIMENSION(1:2)             :: CAI     

   NBAND = 2



    SAG = 0.
    SAV = 0.
    FSA = 0.



  DO IB = 1, NBAND



    CAD(IB) = SOLAD(IB)*FABD(IB)    
    CAI(IB) = SOLAI(IB)*FABI(IB)
    SAV     = SAV + CAD(IB) + CAI(IB)
    FSA     = FSA + CAD(IB) + CAI(IB)
 


    TRD = SOLAD(IB)*FTDD(IB)
    TRI = SOLAD(IB)*FTID(IB) + SOLAI(IB)*FTII(IB)



    ABS = TRD*(1.-ALBGRD(IB)) + TRI*(1.-ALBGRI(IB))
    SAG = SAG + ABS
    FSA = FSA + ABS
  END DO




     LAIFRA = ELAI / MAX(VAI,MPE)
     IF (FSUN .GT. 0.) THEN
        PARSUN = (CAD(1)+FSUN*CAI(1)) * LAIFRA / MAX(LAISUN,MPE)
        PARSHA = (FSHA*CAI(1))*LAIFRA / MAX(LAISHA,MPE)
     ELSE
        PARSUN = 0.
        PARSHA = (CAD(1)+CAI(1))*LAIFRA /MAX(LAISHA,MPE)
     ENDIF



     RVIS = ALBD(1)*SOLAD(1) + ALBI(1)*SOLAI(1)
     RNIR = ALBD(2)*SOLAD(2) + ALBI(2)*SOLAI(2)
     FSR  = RVIS + RNIR


     FSRV = FREVD(1)*SOLAD(1)+FREVI(1)*SOLAI(1)+FREVD(2)*SOLAD(2)+FREVI(2)*SOLAI(2)
     FSRG = FREGD(1)*SOLAD(1)+FREGI(1)*SOLAI(1)+FREGD(2)*SOLAD(2)+FREGI(2)*SOLAI(2)


  END SUBROUTINE SURRAD


  SUBROUTINE SNOW_AGE (DT,TG,SNEQVO,SNEQV,TAUSS,FAGE)

  IMPLICIT NONE




   REAL, INTENT(IN) :: DT        
   REAL, INTENT(IN) :: TG        
   REAL, INTENT(IN) :: SNEQVO    
   REAL, INTENT(IN) :: SNEQV     


   REAL, INTENT(OUT) :: FAGE     


   REAL, INTENT(INOUT) :: TAUSS      

   REAL            :: TAGE       
   REAL            :: AGE1       
   REAL            :: AGE2       
   REAL            :: AGE3       
   REAL            :: DELA       
   REAL            :: SGE        
   REAL            :: DELS       
   REAL            :: DELA0      
   REAL            :: ARG        



   IF(SNEQV.LE.0.0) THEN
          TAUSS = 0.
   ELSE IF (SNEQV.GT.800.) THEN
          TAUSS = 0.
   ELSE
          DELA0 = 1.E-6*DT
          ARG   = 5.E3*(1./TFRZ-1./TG)
          AGE1  = EXP(ARG)
          AGE2  = EXP(AMIN1(0.,10.*ARG))
          AGE3  = 0.3
          TAGE  = AGE1+AGE2+AGE3
          DELA  = DELA0*TAGE
          DELS  = AMAX1(0.0,SNEQV-SNEQVO) / SWEMX
          SGE   = (TAUSS+DELA)*(1.0-DELS)
          TAUSS = AMAX1(0.,SGE)
   ENDIF

   FAGE= TAUSS/(TAUSS+1.)

  END SUBROUTINE SNOW_AGE


  SUBROUTINE SNOWALB_BATS (NBAND,FSNO,COSZ,FAGE,ALBSND,ALBSNI)

  IMPLICIT NONE



  INTEGER,INTENT(IN) :: NBAND  

  REAL,INTENT(IN) :: COSZ    
  REAL,INTENT(IN) :: FSNO    
  REAL,INTENT(IN) :: FAGE    



  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSND 
  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSNI 



  INTEGER :: IB          

  REAL :: FZEN                 
  REAL :: CF1                  
  REAL :: SL2                  
  REAL :: SL1                  
  REAL :: SL                   
  REAL, PARAMETER :: C1 = 0.2  
  REAL, PARAMETER :: C2 = 0.5  





        ALBSND(1: NBAND) = 0.
        ALBSNI(1: NBAND) = 0.



        SL=2.0
        SL1=1./SL
        SL2=2.*SL
        CF1=((1.+SL1)/(1.+SL2*COSZ)-SL1)
        FZEN=AMAX1(CF1,0.)

        ALBSNI(1)=0.95*(1.-C1*FAGE)         
        ALBSNI(2)=0.65*(1.-C2*FAGE)        

        ALBSND(1)=ALBSNI(1)+0.4*FZEN*(1.-ALBSNI(1))    
        ALBSND(2)=ALBSNI(2)+0.4*FZEN*(1.-ALBSNI(2))    

  END SUBROUTINE SNOWALB_BATS


  SUBROUTINE SNOWALB_CLASS (NBAND,QSNOW,DT,ALB,ALBOLD,ALBSND,ALBSNI,ILOC,JLOC)

  IMPLICIT NONE



  INTEGER,INTENT(IN) :: ILOC 
  INTEGER,INTENT(IN) :: JLOC 
  INTEGER,INTENT(IN) :: NBAND  

  REAL,INTENT(IN) :: QSNOW     
  REAL,INTENT(IN) :: DT        
  REAL,INTENT(IN) :: ALBOLD    



  REAL,                INTENT(INOUT) :: ALB        


  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSND 
  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSNI 



  INTEGER :: IB          




        ALBSND(1: NBAND) = 0.
        ALBSNI(1: NBAND) = 0.



         ALB = 0.55 + (ALBOLD-0.55) * EXP(-0.01*DT/3600.)




         IF (QSNOW > 0.) then
           ALB = ALB + MIN(QSNOW*DT,SWEMX) * (0.84-ALB)/(SWEMX)
         ENDIF

         ALBSNI(1)= ALB         
         ALBSNI(2)= ALB         
         ALBSND(1)= ALB         
         ALBSND(2)= ALB         

  END SUBROUTINE SNOWALB_CLASS


  SUBROUTINE GROUNDALB (NSOIL   ,NBAND   ,ICE     ,IST     ,ISC     , & 
                        FSNO    ,SMC     ,ALBSND  ,ALBSNI  ,COSZ    , & 
                        TG      ,ILOC    ,JLOC    ,                   & 
                        ALBGRD  ,ALBGRI  )                              

  USE NOAHMP_RAD_PARAMETERS

  IMPLICIT NONE



  INTEGER,                  INTENT(IN)  :: ILOC   
  INTEGER,                  INTENT(IN)  :: JLOC   
  INTEGER,                  INTENT(IN)  :: NSOIL  
  INTEGER,                  INTENT(IN)  :: NBAND  
  INTEGER,                  INTENT(IN)  :: ICE    
  INTEGER,                  INTENT(IN)  :: IST    
  INTEGER,                  INTENT(IN)  :: ISC    
  REAL,                     INTENT(IN)  :: FSNO   
  REAL,                     INTENT(IN)  :: TG     
  REAL,                     INTENT(IN)  :: COSZ   
  REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SMC    
  REAL, DIMENSION(1:    2), INTENT(IN)  :: ALBSND 
  REAL, DIMENSION(1:    2), INTENT(IN)  :: ALBSNI 



  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBGRD 
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBGRI 



  INTEGER                               :: IB     
  REAL                                  :: INC    
  REAL                                  :: ALBSOD 
  REAL                                  :: ALBSOI 


  DO IB = 1, NBAND
        INC = MAX(0.11-0.40*SMC(1), 0.)
        IF (IST .EQ. 1)  THEN                     
           ALBSOD = MIN(ALBSAT(ISC,IB)+INC,ALBDRY(ISC,IB))
           ALBSOI = ALBSOD
        ELSE IF (TG .GT. TFRZ) THEN               
           ALBSOD = 0.06/(MAX(0.01,COSZ)**1.7 + 0.15)
           ALBSOI = 0.06
        ELSE                                      
           ALBSOD = ALBLAK(IB)
           ALBSOI = ALBSOD
        END IF



        IF (IST .EQ. 1 .AND. ISC .EQ. 9) THEN
           ALBSOD = ALBSOD + 0.10
           ALBSOI = ALBSOI + 0.10
        end if

        ALBGRD(IB) = ALBSOD*(1.-FSNO) + ALBSND(IB)*FSNO
        ALBGRI(IB) = ALBSOI*(1.-FSNO) + ALBSNI(IB)*FSNO
  END DO

  END SUBROUTINE GROUNDALB


  SUBROUTINE TWOSTREAM (IB     ,IC      ,VEGTYP  ,COSZ    ,VAI    , & 
                        FWET   ,T       ,ALBGRD  ,ALBGRI  ,RHO    , & 
                        TAU    ,FVEG    ,IST     ,ILOC    ,JLOC   , & 
                        FAB    ,FRE     ,FTD     ,FTI     ,GDIR   , & 
                        FREV   ,FREG    ,BGAP    ,WGAP)








  USE NOAHMP_VEG_PARAMETERS
  USE NOAHMP_RAD_PARAMETERS

  IMPLICIT NONE



   INTEGER,              INTENT(IN)  :: ILOC    
   INTEGER,              INTENT(IN)  :: JLOC    
   INTEGER,              INTENT(IN)  :: IST     
   INTEGER,              INTENT(IN)  :: IB      
   INTEGER,              INTENT(IN)  :: IC      
   INTEGER,              INTENT(IN)  :: VEGTYP  

   REAL,                 INTENT(IN)  :: COSZ    
   REAL,                 INTENT(IN)  :: VAI     
   REAL,                 INTENT(IN)  :: FWET    
   REAL,                 INTENT(IN)  :: T       

   REAL, DIMENSION(1:2), INTENT(IN)  :: ALBGRD  
   REAL, DIMENSION(1:2), INTENT(IN)  :: ALBGRI  
   REAL, DIMENSION(1:2), INTENT(IN)  :: RHO     
   REAL, DIMENSION(1:2), INTENT(IN)  :: TAU     
   REAL,                 INTENT(IN)  :: FVEG    



   REAL, DIMENSION(1:2), INTENT(OUT) :: FAB     
   REAL, DIMENSION(1:2), INTENT(OUT) :: FRE     
   REAL, DIMENSION(1:2), INTENT(OUT) :: FTD     
   REAL, DIMENSION(1:2), INTENT(OUT) :: FTI     
   REAL,                 INTENT(OUT) :: GDIR    
   REAL, DIMENSION(1:2), INTENT(OUT) :: FREV    
   REAL, DIMENSION(1:2), INTENT(OUT) :: FREG    


   REAL                              :: OMEGA   
   REAL                              :: OMEGAL  
   REAL                              :: BETAI   
   REAL                              :: BETAIL  
   REAL                              :: BETAD   
   REAL                              :: BETADL  
   REAL                              :: EXT     
   REAL                              :: AVMU    

   REAL                              :: COSZI   
   REAL                              :: ASU     
   REAL                              :: CHIL    

   REAL                              :: TMP0,TMP1,TMP2,TMP3,TMP4,TMP5,TMP6,TMP7,TMP8,TMP9
   REAL                              :: P1,P2,P3,P4,S1,S2,U1,U2,U3
   REAL                              :: B,C,D,D1,D2,F,H,H1,H2,H3,H4,H5,H6,H7,H8,H9,H10
   REAL                              :: PHI1,PHI2,SIGMA
   REAL                              :: FTDS,FTIS,FRES
   REAL                              :: DENFVEG
   REAL                              :: VAI_SPREAD

   REAL                              :: FREVEG,FREBAR,FTDVEG,FTIVEG,FTDBAR,FTIBAR
   REAL                              :: THETAZ





   REAL, PARAMETER :: PAI = 3.14159265 
   REAL :: HD       
   REAL :: BB       
   REAL :: THETAP   
   REAL :: FA       
   REAL :: NEWVAI   

   REAL,INTENT(INOUT) :: BGAP     
   REAL,INTENT(INOUT) :: WGAP     

   REAL :: KOPEN    
   REAL :: GAP      



     VAI_SPREAD = VAI
     if(VAI == 0.0) THEN
         GAP     = 1.0
         KOPEN   = 1.0
     ELSE
         IF(OPT_RAD == 1) THEN
	   DENFVEG = -LOG(MAX(1.0-FVEG,0.01))/(PAI*RC(VEGTYP)**2)
           HD      = HVT(VEGTYP) - HVB(VEGTYP)
           BB      = 0.5 * HD           
           THETAP  = ATAN(BB/RC(VEGTYP) * TAN(ACOS(MAX(0.01,COSZ))) )
           
           BGAP    = EXP(-DENFVEG * PAI * RC(VEGTYP)**2/COS(THETAP) )
           FA      = VAI/(1.33 * PAI * RC(VEGTYP)**3.0 *(BB/RC(VEGTYP))*DENFVEG)
           NEWVAI  = HD*FA
           WGAP    = (1.0-BGAP) * EXP(-0.5*NEWVAI/COSZ)
           GAP     = MIN(1.0-FVEG, BGAP+WGAP)

           KOPEN   = 0.05
         END IF

         IF(OPT_RAD == 2) THEN
           GAP     = 0.0
           KOPEN   = 0.0
         END IF

         IF(OPT_RAD == 3) THEN
           GAP     = 1.0-FVEG
           KOPEN   = 1.0-FVEG
         END IF
     end if








     COSZI  = MAX(0.001, COSZ)
     CHIL   = MIN( MAX(XL(VEGTYP), -0.4), 0.6)
     IF (ABS(CHIL) .LE. 0.01) CHIL = 0.01
     PHI1   = 0.5 - 0.633*CHIL - 0.330*CHIL*CHIL
     PHI2   = 0.877 * (1.-2.*PHI1)
     GDIR   = PHI1 + PHI2*COSZI
     EXT    = GDIR/COSZI
     AVMU   = ( 1. - PHI1/PHI2 * LOG((PHI1+PHI2)/PHI1) ) / PHI2
     OMEGAL = RHO(IB) + TAU(IB)
     TMP0   = GDIR + PHI2*COSZI
     TMP1   = PHI1*COSZI
     ASU    = 0.5*OMEGAL*GDIR/TMP0 * ( 1.-TMP1/TMP0*LOG((TMP1+TMP0)/TMP1) )
     BETADL = (1.+AVMU*EXT)/(OMEGAL*AVMU*EXT)*ASU
     BETAIL = 0.5 * ( RHO(IB)+TAU(IB) + (RHO(IB)-TAU(IB))   &
            * ((1.+CHIL)/2.)**2 ) / OMEGAL



     IF (T .GT. TFRZ) THEN                                
        TMP0 = OMEGAL
        TMP1 = BETADL
        TMP2 = BETAIL
     ELSE
        TMP0 =   (1.-FWET)*OMEGAL        + FWET*OMEGAS(IB)
        TMP1 = ( (1.-FWET)*OMEGAL*BETADL + FWET*OMEGAS(IB)*BETADS ) / TMP0
        TMP2 = ( (1.-FWET)*OMEGAL*BETAIL + FWET*OMEGAS(IB)*BETAIS ) / TMP0
     END IF

     OMEGA = TMP0
     BETAD = TMP1
     BETAI = TMP2



     B = 1. - OMEGA + OMEGA*BETAI
     C = OMEGA*BETAI
     TMP0 = AVMU*EXT
     D = TMP0 * OMEGA*BETAD
     F = TMP0 * OMEGA*(1.-BETAD)
     TMP1 = B*B - C*C
     H = SQRT(TMP1) / AVMU
     SIGMA = TMP0*TMP0 - TMP1
     if ( ABS (SIGMA) < 1.e-6 ) SIGMA = SIGN(1.e-6,SIGMA)
     P1 = B + AVMU*H
     P2 = B - AVMU*H
     P3 = B + TMP0
     P4 = B - TMP0
     S1 = EXP(-H*VAI)
     S2 = EXP(-EXT*VAI)
     IF (IC .EQ. 0) THEN
        U1 = B - C/ALBGRD(IB)
        U2 = B - C*ALBGRD(IB)
        U3 = F + C*ALBGRD(IB)
     ELSE
        U1 = B - C/ALBGRI(IB)
        U2 = B - C*ALBGRI(IB)
        U3 = F + C*ALBGRI(IB)
     END IF
     TMP2 = U1 - AVMU*H
     TMP3 = U1 + AVMU*H
     D1 = P1*TMP2/S1 - P2*TMP3*S1
     TMP4 = U2 + AVMU*H
     TMP5 = U2 - AVMU*H
     D2 = TMP4/S1 - TMP5*S1
     H1 = -D*P4 - C*F
     TMP6 = D - H1*P3/SIGMA
     TMP7 = ( D - C - H1/SIGMA*(U1+TMP0) ) * S2
     H2 = ( TMP6*TMP2/S1 - P2*TMP7 ) / D1
     H3 = - ( TMP6*TMP3*S1 - P1*TMP7 ) / D1
     H4 = -F*P3 - C*D
     TMP8 = H4/SIGMA
     TMP9 = ( U3 - TMP8*(U2-TMP0) ) * S2
     H5 = - ( TMP8*TMP4/S1 + TMP9 ) / D2
     H6 = ( TMP8*TMP5*S1 + TMP9 ) / D2
     H7 = (C*TMP2) / (D1*S1)
     H8 = (-C*TMP3*S1) / D1
     H9 = TMP4 / (D2*S1)
     H10 = (-TMP5*S1) / D2




     IF (IC .EQ. 0) THEN
        FTDS = S2                           *(1.0-GAP) + GAP
        FTIS = (H4*S2/SIGMA + H5*S1 + H6/S1)*(1.0-GAP)
     ELSE
        FTDS = 0.
        FTIS = (H9*S1 + H10/S1)*(1.0-KOPEN) + KOPEN
     END IF
     FTD(IB) = FTDS
     FTI(IB) = FTIS



     IF (IC .EQ. 0) THEN
        FRES   = (H1/SIGMA + H2 + H3)*(1.0-GAP  ) + ALBGRD(IB)*GAP        
        FREVEG = (H1/SIGMA + H2 + H3)*(1.0-GAP  ) 
        FREBAR = ALBGRD(IB)*GAP                   
     ELSE
        FRES   = (H7 + H8) *(1.0-KOPEN) + ALBGRI(IB)*KOPEN        
        FREVEG = (H7 + H8) *(1.0-KOPEN) + ALBGRI(IB)*KOPEN
        FREBAR = 0                                
     END IF
     FRE(IB) = FRES

     FREV(IB) = FREVEG 
     FREG(IB) = FREBAR 



     FAB(IB) = 1. - FRE(IB) - (1.-ALBGRD(IB))*FTD(IB) &
                            - (1.-ALBGRI(IB))*FTI(IB)






  END SUBROUTINE TWOSTREAM

  SUBROUTINE VEGE_FLUX(NSNOW   ,NSOIL   ,ISNOW   ,VEGTYP  ,VEG     , & 
                       DT      ,SAV     ,SAG     ,LWDN    ,UR      , & 
                       UU      ,VV      ,SFCTMP  ,THAIR   ,QAIR    , & 
                       EAIR    ,RHOAIR  ,SNOWH   ,VAI     ,GAMMAV   ,GAMMAG,  & 
                       FWET    ,LAISUN  ,LAISHA  ,CWP     ,DZSNSO  , & 
                       HTOP    ,ZLVL    ,ZPD     ,Z0M     ,FVEG    , & 
                       Z0MG    ,EMV     ,EMG     ,CANLIQ  ,          & 
                       CANICE  ,STC     ,DF      ,RSSUN   ,RSSHA   , & 
                       RSURF   ,LATHEAV ,LATHEAG  ,PARSUN  ,PARSHA  ,IGS     , & 
                       FOLN    ,CO2AIR  ,O2AIR   ,BTRAN   ,SFCPRS  , & 
                       RHSUR   ,ILOC    ,JLOC    ,Q2      , & 
                       EAH     ,TAH     ,TV      ,TG      ,CM      , & 
                       CH      ,DX      ,DZ8W    ,                   & 
                       TAUXV   ,TAUYV   ,IRG     ,IRC     ,SHG     , & 
                       SHC     ,EVG     ,EVC     ,TR      ,GH      , & 
                       T2MV    ,PSNSUN  ,PSNSHA  ,                   & 
                       QC      ,PBLH    ,QSFC    ,PSFC    ,ISURBAN , & 
                       IZ0TLND ,Q2V     ,CAH2,CHLEAF,CHUC)              









  USE NOAHMP_VEG_PARAMETERS
  USE MODULE_MODEL_CONSTANTS

  IMPLICIT NONE


  INTEGER,                         INTENT(IN) :: ILOC   
  INTEGER,                         INTENT(IN) :: JLOC   
  LOGICAL,                         INTENT(IN) :: VEG    
  INTEGER,                         INTENT(IN) :: NSNOW  
  INTEGER,                         INTENT(IN) :: NSOIL  
  INTEGER,                         INTENT(IN) :: ISNOW  
  INTEGER,                         INTENT(IN) :: VEGTYP 
  REAL,                            INTENT(IN) :: FVEG   
  REAL,                            INTENT(IN) :: SAV    
  REAL,                            INTENT(IN) :: SAG    
  REAL,                            INTENT(IN) :: LWDN   
  REAL,                            INTENT(IN) :: UR     
  REAL,                            INTENT(IN) :: UU     
  REAL,                            INTENT(IN) :: VV     
  REAL,                            INTENT(IN) :: SFCTMP 
  REAL,                            INTENT(IN) :: THAIR  
  REAL,                            INTENT(IN) :: EAIR   
  REAL,                            INTENT(IN) :: QAIR   
  REAL,                            INTENT(IN) :: RHOAIR 
  REAL,                            INTENT(IN) :: DT     

  REAL,                            INTENT(IN) :: SNOWH  
  REAL,                            INTENT(IN) :: FWET   
  REAL,                            INTENT(IN) :: HTOP   
  REAL,                            INTENT(IN) :: CWP    

  REAL,                            INTENT(IN) :: VAI    
  REAL,                            INTENT(IN) :: LAISUN 
  REAL,                            INTENT(IN) :: LAISHA 
  REAL,                            INTENT(IN) :: ZLVL   
  REAL,                            INTENT(IN) :: ZPD    
  REAL,                            INTENT(IN) :: Z0M    
  REAL,                            INTENT(IN) :: Z0MG   
  REAL,                            INTENT(IN) :: EMV    
  REAL,                            INTENT(IN) :: EMG    

  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DF     
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO 
  REAL,                            INTENT(IN) :: CANLIQ 
  REAL,                            INTENT(IN) :: CANICE 
  REAL,                            INTENT(IN) :: RSURF  


  REAL,                            INTENT(IN) :: GAMMAV  
  REAL,                            INTENT(IN) :: LATHEAV 
  REAL,                            INTENT(IN) :: GAMMAG  
  REAL,                            INTENT(IN) :: LATHEAG 
  REAL,                            INTENT(IN) :: PARSUN 
  REAL,                            INTENT(IN) :: PARSHA 
  REAL,                            INTENT(IN) :: FOLN   
  REAL,                            INTENT(IN) :: CO2AIR 
  REAL,                            INTENT(IN) :: O2AIR  
  REAL,                            INTENT(IN) :: IGS    
  REAL,                            INTENT(IN) :: SFCPRS 
  REAL,                            INTENT(IN) :: BTRAN  
  REAL,                            INTENT(IN) :: RHSUR  

  INTEGER                        , INTENT(IN) :: ISURBAN
  INTEGER                        , INTENT(IN) :: IZ0TLND
  REAL                           , INTENT(IN) :: QC     
  REAL                           , INTENT(IN) :: PBLH   
  REAL                           , INTENT(IN) :: PSFC   
  REAL                           , INTENT(IN) :: DX     
  REAL                           , INTENT(IN) :: Q2     
  REAL                           , INTENT(IN) :: DZ8W   
  REAL                           , INTENT(INOUT) :: QSFC   


  REAL,                         INTENT(INOUT) :: EAH    
  REAL,                         INTENT(INOUT) :: TAH    
  REAL,                         INTENT(INOUT) :: TV     
  REAL,                         INTENT(INOUT) :: TG     
  REAL,                         INTENT(INOUT) :: CM     
  REAL,                         INTENT(INOUT) :: CH     



  REAL,                           INTENT(OUT) :: TAUXV  
  REAL,                           INTENT(OUT) :: TAUYV  
  REAL,                           INTENT(OUT) :: IRC    
  REAL,                           INTENT(OUT) :: SHC    
  REAL,                           INTENT(OUT) :: EVC    
  REAL,                           INTENT(OUT) :: IRG    
  REAL,                           INTENT(OUT) :: SHG    
  REAL,                           INTENT(OUT) :: EVG    
  REAL,                           INTENT(OUT) :: TR     
  REAL,                           INTENT(OUT) :: GH     
  REAL,                           INTENT(OUT) :: T2MV   
  REAL,                           INTENT(OUT) :: PSNSUN 
  REAL,                           INTENT(OUT) :: PSNSHA 
  REAL,                           INTENT(OUT) :: CHLEAF 
  REAL,                           INTENT(OUT) :: CHUC   

  REAL,                           INTENT(OUT) :: Q2V
  REAL :: CAH    
  REAL :: U10V    
  REAL :: V10V    
  REAL :: WSPD


  REAL :: CW           
  REAL :: FV           
  REAL :: WSTAR        
  REAL :: Z0H          
  REAL :: Z0HG         
  REAL :: RB           
  REAL :: RAMC         
  REAL :: RAHC         
  REAL :: RAWC         
  REAL :: RAMG         
  REAL :: RAHG         
  REAL :: RAWG         

  REAL, INTENT(OUT) :: RSSUN        
  REAL, INTENT(OUT) :: RSSHA        

  REAL :: MOL          
  REAL :: DTV          
  REAL :: DTG          

  REAL :: AIR,CIR      
  REAL :: CSH          
  REAL :: CEV          
  REAL :: CGH          
  REAL :: ATR,CTR      
  REAL :: ATA,BTA      
  REAL :: AEA,BEA      

  REAL :: ESTV         
  REAL :: ESTG         
  REAL :: DESTV        
  REAL :: DESTG        
  REAL :: ESATW        
  REAL :: ESATI        
  REAL :: DSATW        
  REAL :: DSATI        

  REAL :: FM           
  REAL :: FH           
  REAL :: FHG          
  REAL :: HCAN         

  REAL :: A            
  REAL :: B            
  REAL :: CVH          
  REAL :: CAW          
  REAL :: CTW          
  REAL :: CEW          
  REAL :: CGW          
  REAL :: COND         
  REAL :: UC           
  REAL :: KH           
  REAL :: H            
  REAL :: HG           
  REAL :: MOZ          
  REAL :: MOZG         
  REAL :: MOZOLD       
  REAL :: FM2          
  REAL :: FH2          
  REAL :: CH2          
  REAL :: THSTAR          

  REAL :: THVAIR
  REAL :: THAH 
  REAL :: RAHC2        
  REAL :: RAWC2        
  REAL, INTENT(OUT):: CAH2         
  REAL :: CH2V         
  REAL :: CQ2V         
  REAL :: EAH2         
  REAL :: QFX        
  REAL :: E1           


  REAL :: VAIE         
  REAL :: LAISUNE      
  REAL :: LAISHAE      

  INTEGER :: K         
  INTEGER :: ITER      


  INTEGER, PARAMETER :: NITERC = 20   

  INTEGER, PARAMETER :: NITERG = 5   
  INTEGER :: MOZSGN    
  REAL    :: MPE       

  INTEGER :: LITER     


  REAL :: T, TDC       

  character(len=80) ::  message

  TDC(T)   = MIN( 50., MAX(-50.,(T-TFRZ)) )


        MPE = 1E-6
        LITER = 0
        FV = 0.1




        DTV = 0.
        DTG = 0.
        MOZSGN = 0
        MOZOLD = 0.
        HG     = 0.
        H      = 0.
        QFX    = 0.



        VAIE    = MIN(6.,VAI    / FVEG)
        LAISUNE = MIN(6.,LAISUN / FVEG)
        LAISHAE = MIN(6.,LAISHA / FVEG)



        T = TDC(TG)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.) THEN
           ESTG = ESATW
        ELSE
           ESTG = ESATI
        END IF



        QSFC = 0.622*EAIR/(PSFC-0.378*EAIR)  



        HCAN = HTOP
        UC = UR*LOG(HCAN/Z0M)/LOG(ZLVL/Z0M)
        IF((HCAN-ZPD) <= 0.) THEN
          WRITE(message,*) "CRITICAL PROBLEM: HCAN <= ZPD"
          call wrf_message ( message )
          WRITE(message,*) 'i,j point=',ILOC, JLOC
          call wrf_message ( message )
          WRITE(message,*) 'HCAN  =',HCAN
          call wrf_message ( message )
          WRITE(message,*) 'ZPD   =',ZPD
          call wrf_message ( message )
          write (message, *) 'SNOWH =',SNOWH
          call wrf_message ( message )
          call wrf_error_fatal3("<stdin>",3333,&
"CRITICAL PROBLEM IN MODULE_SF_NOAHMPLSM:VEGEFLUX" )
        END IF



        AIR = -EMV*(1.+(1.-EMV)*(1.-EMG))*LWDN - EMV*EMG*SB*TG**4  
        CIR = (2.-EMV*(1.-EMG))*EMV*SB

      loop1: DO ITER = 1, NITERC    

       IF(ITER == 1) THEN
            Z0H  = Z0M  
            Z0HG = Z0MG
       ELSE
            Z0H  = Z0M    
            Z0HG = Z0MG   
       END IF



       IF(OPT_SFC == 1) THEN
          CALL SFCDIF1(ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & 
                       ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & 
                       MPE    ,ILOC   ,JLOC   ,                 & 
                       MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & 
                       CM     ,CH     ,FV     ,CH2     )          
       ENDIF
     
       IF(OPT_SFC == 2) THEN
          CALL SFCDIF2(ITER   ,Z0M    ,TAH    ,THAIR  ,UR     , & 
                       CZIL   ,ZLVL   ,ILOC   ,JLOC   ,         & 
                       CM     ,CH     ,MOZ    ,WSTAR  ,         & 
                       FV     )                                   
          
          
          CH = CH / UR
          CM = CM / UR
       ENDIF

       IF(OPT_SFC == 3) THEN
          CALL SFCDIF3(ILOC   ,JLOC   ,TAH    ,QSFC   ,PSFC   ,&  
                       PBLH   ,Z0M    ,Z0MG   ,VEGTYP ,ISURBAN,&  
                       IZ0TLND,UC     ,ITER   ,NITERC ,SFCTMP ,&  
                       THAIR  ,QAIR   ,QC     ,ZLVL   , &  
                       SFCPRS ,FV     ,CM     ,CH     ,CH2V   ,&  
                       CQ2V   ,MOZ)                               
          
          
          CH   = CH / UR
          CM   = CM / UR
          CH2V = CH2V / UR
       ENDIF

       IF(OPT_SFC == 4) THEN
          CALL SFCDIF4(ILOC   ,JLOC   ,UU     ,VV     ,SFCTMP ,&  
                       SFCPRS ,PSFC   ,PBLH   ,DX     ,Z0M    ,&
                       TAH    ,QAIR   ,ZLVL   ,IZ0TLND,QSFC   ,&
                       H      ,QFX    ,CM     ,CH     ,CH2V   ,& 
                       CQ2V   ,MOZ    ,FV     ,U10V   ,V10V)
          
          
          CH   = CH / UR
          CM   = CM / UR
          CH2V = CH2V / UR
       ENDIF

       RAMC = MAX(1.,1./(CM*UR))
       RAHC = MAX(1.,1./(CH*UR))
       RAWC = RAHC

       IF (OPT_SFC == 3 .OR. OPT_SFC == 4 ) THEN
         RAHC2 = MAX(1.,1./(CH2V*UR))
         RAWC2 = RAHC2
         CAH2 = 1./RAHC2
         CQ2V = CAH2
       ENDIF



       
       CALL RAGRB(ITER   ,VAIE   ,RHOAIR ,HG     ,TAH    , & 
                  ZPD    ,Z0MG   ,Z0HG   ,HCAN   ,UC     , & 
                  Z0H    ,FV     ,CWP    ,VEGTYP ,MPE    , & 
                  TV     ,MOZG   ,FHG    ,ILOC   ,JLOC   , & 
                  RAMG   ,RAHG   ,RAWG   ,RB     )           



       T = TDC(TV)
       CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
       IF (T .GT. 0.) THEN
          ESTV  = ESATW
          DESTV = DSATW
       ELSE
          ESTV  = ESATI
          DESTV = DSATI
       END IF


        
     IF(ITER == 1) THEN
        IF (OPT_CRS == 1) then  
         CALL STOMATA (VEGTYP,MPE   ,PARSUN ,FOLN  ,ILOC  , JLOC , & 
                       TV    ,ESTV  ,EAH    ,SFCTMP,SFCPRS, & 
                       O2AIR ,CO2AIR,IGS    ,BTRAN ,RB    , & 
                       RSSUN ,PSNSUN)                         

         CALL STOMATA (VEGTYP,MPE   ,PARSHA ,FOLN  ,ILOC  , JLOC , & 
                       TV    ,ESTV  ,EAH    ,SFCTMP,SFCPRS, & 
                       O2AIR ,CO2AIR,IGS    ,BTRAN ,RB    , & 
                       RSSHA ,PSNSHA)                         
        END IF

        IF (OPT_CRS == 2) then  
         CALL  CANRES (PARSUN,TV    ,BTRAN ,EAH    ,SFCPRS, & 
                       RSSUN ,PSNSUN,ILOC  ,JLOC   )          

         CALL  CANRES (PARSHA,TV    ,BTRAN ,EAH    ,SFCPRS, & 
                       RSSHA ,PSNSHA,ILOC  ,JLOC   )          
        END IF
     END IF



        CAH  = 1./RAHC
        CVH  = 2.*VAIE/RB
        CGH  = 1./RAHG
        COND = CAH + CVH + CGH
        ATA  = (SFCTMP*CAH + TG*CGH) / COND
        BTA  = CVH/COND
        CSH  = (1.-BTA)*RHOAIR*CPAIR*CVH



        CAW  = 1./RAWC
        CEW  = FWET*VAIE/RB
        CTW  = (1.-FWET)*(LAISUNE/(RB+RSSUN) + LAISHAE/(RB+RSSHA))
        CGW  = 1./(RAWG+RSURF)
        COND = CAW + CEW + CTW + CGW
        AEA  = (EAIR*CAW + ESTG*CGW) / COND
        BEA  = (CEW+CTW)/COND
        CEV  = (1.-BEA)*CEW*RHOAIR*CPAIR/GAMMAV   
        CTR  = (1.-BEA)*CTW*RHOAIR*CPAIR/GAMMAV



        TAH = ATA + BTA*TV               
        EAH = AEA + BEA*ESTV             

        IRC = FVEG*(AIR + CIR*TV**4)
        SHC = FVEG*RHOAIR*CPAIR*CVH * (  TV-TAH)
        EVC = FVEG*RHOAIR*CPAIR*CEW * (ESTV-EAH) / GAMMAV 
        TR  = FVEG*RHOAIR*CPAIR*CTW * (ESTV-EAH) / GAMMAV
	IF (TV > TFRZ) THEN
          EVC = MIN(CANLIQ*LATHEAV/DT,EVC)    
	ELSE
          EVC = MIN(CANICE*LATHEAV/DT,EVC)
	END IF

        B   = SAV-IRC-SHC-EVC-TR                          
        A   = FVEG*(4.*CIR*TV**3 + CSH + (CEV+CTR)*DESTV) 
        DTV = B/A

        IRC = IRC + FVEG*4.*CIR*TV**3*DTV
        SHC = SHC + FVEG*CSH*DTV
        EVC = EVC + FVEG*CEV*DESTV*DTV
        TR  = TR  + FVEG*CTR*DESTV*DTV                               


        TV  = TV + DTV



        H  = RHOAIR*CPAIR*(TAH - SFCTMP) /RAHC        
        HG = RHOAIR*CPAIR*(TG  - TAH)   /RAHG


        QSFC = (0.622*EAH)/(SFCPRS-0.378*EAH)


        IF ( OPT_SFC == 4 ) THEN
           QFX = (QSFC-QAIR)*RHOAIR*CAW 
        ENDIF

        IF (LITER == 1) THEN
           exit loop1 
        ENDIF
        IF (ITER >= 5 .AND. ABS(DTV) <= 0.01 .AND. LITER == 0) THEN
           LITER = 1
        ENDIF

     END DO loop1 



        AIR = - EMG*(1.-EMV)*LWDN - EMG*EMV*SB*TV**4
        CIR = EMG*SB
        CSH = RHOAIR*CPAIR/RAHG
        CEV = RHOAIR*CPAIR / (GAMMAG*(RAWG+RSURF))  
        CGH = 2.*DF(ISNOW+1)/DZSNSO(ISNOW+1)

     loop2: DO ITER = 1, NITERG

        T = TDC(TG)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.) THEN
            ESTG  = ESATW
            DESTG = DSATW
        ELSE
            ESTG  = ESATI
            DESTG = DSATI
        END IF

        IRG = CIR*TG**4 + AIR
        SHG = CSH * (TG         - TAH         )
        EVG = CEV * (ESTG*RHSUR - EAH         )
        GH  = CGH * (TG         - STC(ISNOW+1))

        B = SAG-IRG-SHG-EVG-GH
        A = 4.*CIR*TG**3+CSH+CEV*DESTG+CGH
        DTG = B/A

        IRG = IRG + 4.*CIR*TG**3*DTG
        SHG = SHG + CSH*DTG
        EVG = EVG + CEV*DESTG*DTG
        GH  = GH  + CGH*DTG
        TG  = TG  + DTG

     END DO loop2
     




     IF(OPT_STC == 1) THEN
     IF (SNOWH > 0.05 .AND. TG > TFRZ) THEN
        TG  = TFRZ
        IRG = CIR*TG**4 - EMG*(1.-EMV)*LWDN - EMG*EMV*SB*TV**4
        SHG = CSH * (TG         - TAH)
        EVG = CEV * (ESTG*RHSUR - EAH)
        GH  = SAG - (IRG+SHG+EVG)
     END IF
     END IF



     TAUXV = -RHOAIR*CM*UR*UU
     TAUYV = -RHOAIR*CM*UR*VV









   IF (OPT_SFC == 1 .OR. OPT_SFC == 2) THEN

      CAH2 = FV*VKC/LOG((2.+Z0H)/Z0H)
      CAH2 = FV*VKC/(LOG((2.+Z0H)/Z0H)-FH2)
      CQ2V = CAH2
      IF (CAH2 .LT. 1.E-5 ) THEN
         T2MV = TAH

         Q2V  = QSFC
      ELSE
         T2MV = TAH - (SHG+SHC/FVEG)/(RHOAIR*CPAIR) * 1./CAH2

         Q2V = QSFC - ((EVC+TR)/FVEG+EVG)/(LATHEAV*RHOAIR) * 1./CQ2V
      ENDIF
   ENDIF


   IF (OPT_SFC == 3 .OR. OPT_SFC == 4 ) THEN
         IF (CAH2 .LT. 1.E-5 ) THEN
            T2MV = TAH
            Q2V  = (EAH*0.622/(SFCPRS - 0.378*EAH))
         ELSE
            T2MV = TAH - (SHG+SHC)/(RHOAIR*CPAIR*CAH2)
            Q2V  = (EAH*0.622/(SFCPRS - 0.378*EAH)) - QFX/(RHOAIR*CQ2V)
         ENDIF
   ENDIF  


     CH = CAH
     CHLEAF = CVH
     CHUC = 1./RAHG

  END SUBROUTINE VEGE_FLUX

  SUBROUTINE BARE_FLUX (NSNOW   ,NSOIL   ,ISNOW   ,DT      ,SAG     , & 
                        LWDN    ,UR      ,UU      ,VV      ,SFCTMP  , & 
                        THAIR   ,QAIR    ,EAIR    ,RHOAIR  ,SNOWH   , & 
                        DZSNSO  ,ZLVL    ,ZPD     ,Z0M     ,          & 
                        EMG     ,STC     ,DF      ,RSURF   ,LATHEA  , & 
                        GAMMA   ,RHSUR   ,ILOC    ,JLOC    ,Q2      , & 
                        TGB     ,CM      ,CH      ,          & 
                        TAUXB   ,TAUYB   ,IRB     ,SHB     ,EVB     , & 
                        GHB     ,T2MB    ,DX      ,DZ8W    ,IVGTYP  , & 
                        QC      ,PBLH    ,QSFC    ,PSFC    ,ISURBAN , & 
                        IZ0TLND ,SFCPRS  ,Q2B     ,EHB2)      








  USE NOAHMP_VEG_PARAMETERS
  USE MODULE_MODEL_CONSTANTS

  IMPLICIT NONE


  integer                        , INTENT(IN) :: ILOC   
  integer                        , INTENT(IN) :: JLOC   
  INTEGER,                         INTENT(IN) :: NSNOW  
  INTEGER,                         INTENT(IN) :: NSOIL  
  INTEGER,                         INTENT(IN) :: ISNOW  
  REAL,                            INTENT(IN) :: DT     
  REAL,                            INTENT(IN) :: SAG    
  REAL,                            INTENT(IN) :: LWDN   
  REAL,                            INTENT(IN) :: UR     
  REAL,                            INTENT(IN) :: UU     
  REAL,                            INTENT(IN) :: VV     
  REAL,                            INTENT(IN) :: SFCTMP 
  REAL,                            INTENT(IN) :: THAIR  
  REAL,                            INTENT(IN) :: QAIR   
  REAL,                            INTENT(IN) :: EAIR   
  REAL,                            INTENT(IN) :: RHOAIR 
  REAL,                            INTENT(IN) :: SNOWH  
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO 
  REAL,                            INTENT(IN) :: ZLVL   
  REAL,                            INTENT(IN) :: ZPD    
  REAL,                            INTENT(IN) :: Z0M    
  REAL,                            INTENT(IN) :: EMG    
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DF     
  REAL,                            INTENT(IN) :: RSURF  
  REAL,                            INTENT(IN) :: LATHEA 
  REAL,                            INTENT(IN) :: GAMMA  
  REAL,                            INTENT(IN) :: RHSUR  


  INTEGER                        , INTENT(IN) :: ISURBAN
  INTEGER                        , INTENT(IN) :: IVGTYP
  INTEGER                        , INTENT(IN) :: IZ0TLND
  REAL                           , INTENT(IN) :: QC     
  REAL                           , INTENT(IN) :: PBLH   
  REAL                           , INTENT(INOUT) :: QSFC   
  REAL                           , INTENT(IN) :: PSFC   
  REAL                           , INTENT(IN) :: SFCPRS 
  REAL                           , INTENT(IN) :: DX     
  REAL                           , INTENT(IN) :: Q2     
  REAL                           , INTENT(IN) :: DZ8W   



  REAL,                         INTENT(INOUT) :: TGB    
  REAL,                         INTENT(INOUT) :: CM     
  REAL,                         INTENT(INOUT) :: CH     




  REAL,                           INTENT(OUT) :: TAUXB  
  REAL,                           INTENT(OUT) :: TAUYB  
  REAL,                           INTENT(OUT) :: IRB    
  REAL,                           INTENT(OUT) :: SHB    
  REAL,                           INTENT(OUT) :: EVB    
  REAL,                           INTENT(OUT) :: GHB    
  REAL,                           INTENT(OUT) :: T2MB   

  REAL,                           INTENT(OUT) :: Q2B    
  REAL :: EHB    
  REAL :: U10B    
  REAL :: V10B    
  REAL :: WSPD




  REAL :: TAUX       
  REAL :: TAUY       
  REAL :: FIRA       
  REAL :: FSH        
  REAL :: FGEV       
  REAL :: SSOIL      
  REAL :: FIRE       
  REAL :: TRAD       
  REAL :: TAH        

  REAL :: CW         
  REAL :: FV         
  REAL :: WSTAR      
  REAL :: Z0H        
  REAL :: RB         
  REAL :: RAMB       
  REAL :: RAHB       
  REAL :: RAWB       
  REAL :: MOL        
  REAL :: DTG        

  REAL :: CIR        
  REAL :: CSH        
  REAL :: CEV        
  REAL :: CGH        


  REAL :: RAHB2      
  REAL :: RAWB2      
  REAL,INTENT(OUT) :: EHB2       
  REAL :: CH2B       
  REAL :: CQ2B       
  REAL :: THVAIR     
  REAL :: THGH       
  REAL :: EMB        
  REAL :: QFX        
  REAL :: ESTG2      
  INTEGER :: VEGTYP     
  REAL :: E1


  REAL :: ESTG       
  REAL :: DESTG      
  REAL :: ESATW      
  REAL :: ESATI      
  REAL :: DSATW      
  REAL :: DSATI      

  REAL :: A          
  REAL :: B          
  REAL :: H          
  REAL :: MOZ        
  REAL :: MOZOLD     
  REAL :: FM         
  REAL :: FH         
  INTEGER :: MOZSGN  
  REAL :: FM2          
  REAL :: FH2          
  REAL :: CH2          

  INTEGER :: ITER    
  INTEGER :: NITERB  
  REAL    :: MPE     


  DATA NITERB /5/
  SAVE NITERB
  REAL :: T, TDC     
  TDC(T)   = MIN( 50., MAX(-50.,(T-TFRZ)) )




        MPE = 1E-6
        DTG = 0.
        MOZSGN = 0
        MOZOLD = 0.
        H      = 0.
        QFX    = 0.
        FV     = 0.1

        CIR = EMG*SB
        CGH = 2.*DF(ISNOW+1)/DZSNSO(ISNOW+1)


      loop3: DO ITER = 1, NITERB  

        IF(ITER == 1) THEN
            Z0H = Z0M 
        ELSE
            Z0H = Z0M 
        END IF

        IF(OPT_SFC == 1) THEN
          CALL SFCDIF1(ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & 
                       ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & 
                       MPE    ,ILOC   ,JLOC   ,                 & 
                       MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & 
                       CM     ,CH     ,FV     ,CH2     )          
        ENDIF

        IF(OPT_SFC == 2) THEN
          CALL SFCDIF2(ITER   ,Z0M    ,TGB    ,THAIR  ,UR     , & 
                       CZIL   ,ZLVL   ,ILOC   ,JLOC   ,         & 
                       CM     ,CH     ,MOZ    ,WSTAR  ,         & 
                       FV     )                                   
          
          
          CH = CH / UR
          CM = CM / UR
          IF(SNOWH > 0.) THEN
             CM = MIN(0.01,CM)   
             CH = MIN(0.01,CH)   
          END IF

        ENDIF

        IF(OPT_SFC == 3) THEN
          VEGTYP = ISBARREN
          CALL SFCDIF3(ILOC   ,JLOC   ,TGB    ,QSFC   ,PSFC   ,&  
                       PBLH   ,Z0M    ,Z0M    ,VEGTYP ,ISURBAN,&  
                       IZ0TLND,UR     ,ITER   ,NITERB ,SFCTMP ,&  
                       THAIR  ,QAIR   ,QC     ,ZLVL   ,        &  
                       SFCPRS ,FV     ,CM     ,CH     ,CH2B   ,&  
                       CQ2B   ,MOZ)                               
          
          
          CH   = CH / UR
          CM   = CM / UR
          CH2B = CH2B / UR

          IF(SNOWH > 0.) THEN    
             CM = MIN(0.01,CM)   
             CH = MIN(0.01,CH)   
             CH2B = MIN(0.01,CH2B)
             CQ2B = MIN(0.01,CQ2B)
          END IF
        ENDIF

       IF(OPT_SFC == 4) THEN
          CALL SFCDIF4(ILOC   ,JLOC   ,UU     ,VV     ,SFCTMP ,&  
                       SFCPRS ,PSFC   ,PBLH   ,DX     ,Z0M    ,&
                       TGB    ,QAIR   ,ZLVL   ,IZ0TLND,QSFC   ,&
                       H      ,QFX    ,CM     ,CH     ,CH2B   ,& 
                       CQ2B   ,MOZ    ,FV     ,U10B   ,V10B)
          
          
          CH   = CH / UR
          CM   = CM / UR
          CH2B = CH2B / UR

          IF(SNOWH > 0.) THEN    
             CM = MIN(0.01,CM)   
             CH = MIN(0.01,CH)   
             CH2B = MIN(0.01,CH2B)
             CQ2B = MIN(0.01,CQ2B)
          END IF

       ENDIF

        RAMB = MAX(1.,1./(CM*UR))
        RAHB = MAX(1.,1./(CH*UR))
        RAWB = RAHB


        EMB = 1./RAMB
        EHB = 1./RAHB
        IF (OPT_SFC == 3 .OR. OPT_SFC == 4) THEN
          RAHB2 = MAX(1.,1./(CH2B*UR))
          EHB2 = 1./RAHB2
          CQ2B = EHB2
        END IF



        T = TDC(TGB)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.) THEN
            ESTG  = ESATW
            DESTG = DSATW
        ELSE
            ESTG  = ESATI
            DESTG = DSATI
        END IF

        CSH = RHOAIR*CPAIR/RAHB
        CEV = RHOAIR*CPAIR/GAMMA/(RSURF+RAWB)



        IRB   = CIR * TGB**4 - EMG*LWDN
        SHB   = CSH * (TGB        - SFCTMP      )
        EVB   = CEV * (ESTG*RHSUR - EAIR        )
        GHB   = CGH * (TGB        - STC(ISNOW+1))

        B     = SAG-IRB-SHB-EVB-GHB
        A     = 4.*CIR*TGB**3 + CSH + CEV*DESTG + CGH
        DTG   = B/A

        IRB = IRB + 4.*CIR*TGB**3*DTG
        SHB = SHB + CSH*DTG
        EVB = EVB + CEV*DESTG*DTG
        GHB = GHB + CGH*DTG


        TGB = TGB + DTG


        H = CSH * (TGB - SFCTMP)

        T = TDC(TGB)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.) THEN
            ESTG  = ESATW
        ELSE
            ESTG  = ESATI
        END IF
        QSFC = 0.622*(ESTG*RHSUR)/(PSFC-0.378*(ESTG*RHSUR))

        QFX = (QSFC-QAIR)*CEV*GAMMA/CPAIR

     END DO loop3 




     IF(OPT_STC == 1) THEN
     IF (SNOWH > 0.05 .AND. TGB > TFRZ) THEN
          TGB = TFRZ
          IRB = CIR * TGB**4 - EMG*LWDN
          SHB = CSH * (TGB        - SFCTMP)
          EVB = CEV * (ESTG*RHSUR - EAIR )          
          GHB = SAG - (IRB+SHB+EVB)
     END IF
     END IF


         
     TAUXB = -RHOAIR*CM*UR*UU
     TAUYB = -RHOAIR*CM*UR*VV



     IF(OPT_SFC == 1 .OR. OPT_SFC ==2) THEN
       EHB2  = FV*VKC/LOG((2.+Z0H)/Z0H)
       EHB2  = FV*VKC/(LOG((2.+Z0H)/Z0H)-FH2)
       CQ2B  = EHB2
       IF (EHB2.lt.1.E-5 ) THEN
         T2MB  = TGB
         Q2B   = QSFC
       ELSE
         T2MB  = TGB - SHB/(RHOAIR*CPAIR) * 1./EHB2
         Q2B   = QSFC - EVB/(LATHEA*RHOAIR)*(1./CQ2B + RSURF)
       ENDIF
       IF (IVGTYP == ISURBAN) Q2B = QSFC
     END IF


     IF(OPT_SFC ==3 .OR. OPT_SFC == 4) THEN
       IF (EHB2.lt.1.E-5 ) THEN
          T2MB = TGB
          Q2B  = QSFC
       ELSE
          T2MB = TGB - SHB/(RHOAIR*CPAIR*EHB2)
          Q2B  = QSFC - QFX/(RHOAIR*CQ2B)
       END IF



     END IF


     CH = EHB

  END SUBROUTINE BARE_FLUX

  SUBROUTINE RAGRB(ITER   ,VAI    ,RHOAIR ,HG     ,TAH    , & 
                   ZPD    ,Z0MG   ,Z0HG   ,HCAN   ,UC     , & 
                   Z0H    ,FV     ,CWP    ,VEGTYP ,MPE    , & 
                   TV     ,MOZG   ,FHG    ,ILOC   ,JLOC   , & 
                   RAMG   ,RAHG   ,RAWG   ,RB     )           




  USE NOAHMP_VEG_PARAMETERS

  IMPLICIT NONE



  INTEGER,              INTENT(IN) :: ILOC   
  INTEGER,              INTENT(IN) :: JLOC   
  INTEGER,              INTENT(IN) :: ITER   
  INTEGER,              INTENT(IN) :: VEGTYP 
  REAL,                 INTENT(IN) :: VAI    
  REAL,                 INTENT(IN) :: RHOAIR 
  REAL,                 INTENT(IN) :: HG     
  REAL,                 INTENT(IN) :: TV     
  REAL,                 INTENT(IN) :: TAH    
  REAL,                 INTENT(IN) :: ZPD    
  REAL,                 INTENT(IN) :: Z0MG   
  REAL,                 INTENT(IN) :: HCAN   
  REAL,                 INTENT(IN) :: UC     
  REAL,                 INTENT(IN) :: Z0H    
  REAL,                 INTENT(IN) :: Z0HG   
  REAL,                 INTENT(IN) :: FV     
  REAL,                 INTENT(IN) :: CWP    
  REAL,                 INTENT(IN) :: MPE    



  REAL,              INTENT(INOUT) :: MOZG   
  REAL,              INTENT(INOUT) :: FHG    


  REAL                             :: RAMG   
  REAL                             :: RAHG   
  REAL                             :: RAWG   
  REAL                             :: RB     


  REAL :: KH           
  REAL :: TMP1         
  REAL :: TMP2         
  REAL :: TMPRAH2      
  REAL :: TMPRB        
  real :: MOLG,FHGNEW,CWPC



       MOZG = 0.
       MOLG = 0.

       IF(ITER > 1) THEN
        TMP1 = VKC * (GRAV/TAH) * HG/(RHOAIR*CPAIR)
        IF (ABS(TMP1) .LE. MPE) TMP1 = MPE
        MOLG = -1. * FV**3 / TMP1
        MOZG = MIN( (ZPD-Z0MG)/MOLG, 1.)
       END IF

       IF (MOZG < 0.) THEN
          FHGNEW  = (1. - 15.*MOZG)**(-0.25)
       ELSE
          FHGNEW  = 1.+ 4.7*MOZG
       ENDIF

       IF (ITER == 1) THEN
          FHG = FHGNEW
       ELSE
          FHG = 0.5 * (FHG+FHGNEW)
       ENDIF

       CWPC = (CWP * VAI * HCAN * FHG)**0.5


       TMP1 = EXP( -CWPC*Z0HG/HCAN )
       TMP2 = EXP( -CWPC*(Z0H+ZPD)/HCAN )
       TMPRAH2 = HCAN*EXP(CWPC) / CWPC * (TMP1-TMP2)



       KH  = MAX ( VKC*FV*(HCAN-ZPD), MPE )
       RAMG = 0.
       RAHG = TMPRAH2 / KH
       RAWG = RAHG



       TMPRB  = CWPC*50. / (1. - EXP(-CWPC/2.))
       RB     = TMPRB * SQRT(DLEAF(VEGTYP)/UC)


  END SUBROUTINE RAGRB



  SUBROUTINE SFCDIF1(ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & 
       &             ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & 
       &             MPE    ,ILOC   ,JLOC   ,                 & 
       &             MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & 
       &             CM     ,CH     ,FV     ,CH2     )          



    IMPLICIT NONE


    
    INTEGER,              INTENT(IN) :: ILOC   
    INTEGER,              INTENT(IN) :: JLOC   
    INTEGER,              INTENT(IN) :: ITER   
    REAL,                 INTENT(IN) :: SFCTMP 
    REAL,                 INTENT(IN) :: RHOAIR 
    REAL,                 INTENT(IN) :: H      
    REAL,                 INTENT(IN) :: QAIR   
    REAL,                 INTENT(IN) :: ZLVL   
    REAL,                 INTENT(IN) :: ZPD    
    REAL,                 INTENT(IN) :: Z0H    
    REAL,                 INTENT(IN) :: Z0M    
    REAL,                 INTENT(IN) :: UR     
    REAL,                 INTENT(IN) :: MPE    


    INTEGER,           INTENT(INOUT) :: MOZSGN 
    REAL,              INTENT(INOUT) :: MOZ    
    REAL,              INTENT(INOUT) :: FM     
    REAL,              INTENT(INOUT) :: FH     
    REAL,              INTENT(INOUT) :: FM2    
    REAL,              INTENT(INOUT) :: FH2    



    REAL,                INTENT(OUT) :: CM     
    REAL,                INTENT(OUT) :: CH     
    REAL,                INTENT(OUT) :: FV     
    REAL,                INTENT(OUT) :: CH2    


    REAL    :: MOL                      
    REAL    :: TMPCM                    
    REAL    :: TMPCH                    
    REAL    :: FMNEW                    
    REAL    :: FHNEW                    
    REAL    :: MOZOLD                   
    REAL    :: TMP1,TMP2,TMP3,TMP4,TMP5 
    REAL    :: TVIR                     
    REAL    :: MOZ2                     
    REAL    :: TMPCM2                   
    REAL    :: TMPCH2                   
    REAL    :: FM2NEW                   
    REAL    :: FH2NEW                   
    REAL    :: TMP12,TMP22,TMP32        

    REAL    :: CMFM, CHFH, CM2FM2, CH2FH2



    MOZOLD = MOZ
  
    IF(ZLVL <= ZPD) THEN
       write(*,*) 'critical problem: ZLVL <= ZPD; model stops'
       call wrf_error_fatal3("<stdin>",4160,&
"STOP in Noah-MP")
    ENDIF

    TMPCM = LOG((ZLVL-ZPD) / Z0M)
    TMPCH = LOG((ZLVL-ZPD) / Z0H)
    TMPCM2 = LOG((2.0 + Z0M) / Z0M)
    TMPCH2 = LOG((2.0 + Z0H) / Z0H)

    IF(ITER == 1) THEN
       FV   = 0.0
       MOZ  = 0.0
       MOL  = 0.0
       MOZ2 = 0.0
    ELSE
       TVIR = (1. + 0.61*QAIR) * SFCTMP
       TMP1 = VKC * (GRAV/TVIR) * H/(RHOAIR*CPAIR)
       IF (ABS(TMP1) .LE. MPE) TMP1 = MPE
       MOL  = -1. * FV**3 / TMP1
       MOZ  = MIN( (ZLVL-ZPD)/MOL, 1.)
       MOZ2  = MIN( (2.0 + Z0H)/MOL, 1.)
    ENDIF



    IF (MOZOLD*MOZ .LT. 0.) MOZSGN = MOZSGN+1
    IF (MOZSGN .GE. 2) THEN
       MOZ = 0.
       FM = 0.
       FH = 0.
       MOZ2 = 0.
       FM2 = 0.
       FH2 = 0.
    ENDIF


    IF (MOZ .LT. 0.) THEN
       TMP1 = (1. - 16.*MOZ)**0.25
       TMP2 = LOG((1.+TMP1*TMP1)/2.)
       TMP3 = LOG((1.+TMP1)/2.)
       FMNEW = 2.*TMP3 + TMP2 - 2.*ATAN(TMP1) + 1.5707963
       FHNEW = 2*TMP2


       TMP12 = (1. - 16.*MOZ2)**0.25
       TMP22 = LOG((1.+TMP12*TMP12)/2.)
       TMP32 = LOG((1.+TMP12)/2.)
       FM2NEW = 2.*TMP32 + TMP22 - 2.*ATAN(TMP12) + 1.5707963
       FH2NEW = 2*TMP22
    ELSE
       FMNEW = -5.*MOZ
       FHNEW = FMNEW
       FM2NEW = -5.*MOZ2
       FH2NEW = FM2NEW
    ENDIF




    IF (ITER == 1) THEN
       FM = FMNEW
       FH = FHNEW
       FM2 = FM2NEW
       FH2 = FH2NEW
    ELSE
       FM = 0.5 * (FM+FMNEW)
       FH = 0.5 * (FH+FHNEW)
       FM2 = 0.5 * (FM2+FM2NEW)
       FH2 = 0.5 * (FH2+FH2NEW)
    ENDIF



    FH = MIN(FH,0.9*TMPCH)
    FM = MIN(FM,0.9*TMPCM)
    FH2 = MIN(FH2,0.9*TMPCH2)
    FM2 = MIN(FM2,0.9*TMPCM2)

    CMFM = TMPCM-FM
    CHFH = TMPCH-FH
    CM2FM2 = TMPCM2-FM2
    CH2FH2 = TMPCH2-FH2
    IF(ABS(CMFM) <= MPE) CMFM = MPE
    IF(ABS(CHFH) <= MPE) CHFH = MPE
    IF(ABS(CM2FM2) <= MPE) CM2FM2 = MPE
    IF(ABS(CH2FH2) <= MPE) CH2FH2 = MPE
    CM  = VKC*VKC/(CMFM*CMFM)
    CH  = VKC*VKC/(CMFM*CHFH)
    CH2  = VKC*VKC/(CM2FM2*CH2FH2)
        


    FV = UR * SQRT(CM)
    CH2  = VKC*FV/CH2FH2

  END SUBROUTINE SFCDIF1



  SUBROUTINE SFCDIF2(ITER   ,Z0     ,THZ0   ,THLM   ,SFCSPD , & 
                     CZIL   ,ZLM    ,ILOC   ,JLOC   ,         & 
                     AKMS   ,AKHS   ,RLMO   ,WSTAR2 ,         & 
                     USTAR  )                                   








    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ILOC
    INTEGER, INTENT(IN) :: JLOC
    INTEGER, INTENT(IN) :: ITER
    REAL,    INTENT(IN) :: ZLM, Z0, THZ0, THLM, SFCSPD, CZIL
    REAL, intent(INOUT) :: AKMS
    REAL, intent(INOUT) :: AKHS
    REAL, intent(INOUT) :: RLMO
    REAL, intent(INOUT) :: WSTAR2
    REAL,   intent(OUT) :: USTAR

    REAL     ZZ, PSLMU, PSLMS, PSLHU, PSLHS
    REAL     XX, PSPMU, YY, PSPMS, PSPHU, PSPHS
    REAL     ZILFC, ZU, ZT, RDZ, CXCH
    REAL     DTHV, DU2, BTGH, ZSLU, ZSLT, RLOGU, RLOGT
    REAL     ZETALT, ZETALU, ZETAU, ZETAT, XLU4, XLT4, XU4, XT4

    REAL     XLU, XLT, XU, XT, PSMZ, SIMM, PSHZ, SIMH, USTARK, RLMN,  &
         &         RLMA

    INTEGER  ILECH, ITR

    INTEGER, PARAMETER :: ITRMX  = 5
    REAL,    PARAMETER :: WWST   = 1.2
    REAL,    PARAMETER :: WWST2  = WWST * WWST
    REAL,    PARAMETER :: VKRM   = 0.40
    REAL,    PARAMETER :: EXCM   = 0.001
    REAL,    PARAMETER :: BETA   = 1.0 / 270.0
    REAL,    PARAMETER :: BTG    = BETA * GRAV
    REAL,    PARAMETER :: ELFC   = VKRM * BTG
    REAL,    PARAMETER :: WOLD   = 0.15
    REAL,    PARAMETER :: WNEW   = 1.0 - WOLD
    REAL,    PARAMETER :: PIHF   = 3.14159265 / 2.
    REAL,    PARAMETER :: EPSU2  = 1.E-4
    REAL,    PARAMETER :: EPSUST = 0.07
    REAL,    PARAMETER :: EPSIT  = 1.E-4
    REAL,    PARAMETER :: EPSA   = 1.E-8
    REAL,    PARAMETER :: ZTMIN  = -5.0
    REAL,    PARAMETER :: ZTMAX  = 1.0
    REAL,    PARAMETER :: HPBL   = 1000.0
    REAL,    PARAMETER :: SQVISC = 258.2
    REAL,    PARAMETER :: RIC    = 0.183
    REAL,    PARAMETER :: RRIC   = 1.0 / RIC
    REAL,    PARAMETER :: FHNEU  = 0.8
    REAL,    PARAMETER :: RFC    = 0.191
    REAL,    PARAMETER :: RFAC   = RIC / ( FHNEU * RFC * RFC )





    PSLMU (ZZ)= -0.96* log (1.0-4.5* ZZ)
    PSLMS (ZZ)= ZZ * RRIC -2.076* (1. -1./ (ZZ +1.))
    PSLHU (ZZ)= -0.96* log (1.0-4.5* ZZ)
    PSLHS (ZZ)= ZZ * RFAC -2.076* (1. -1./ (ZZ +1.))

    PSPMU (XX)= -2.* log ( (XX +1.)*0.5) - log ( (XX * XX +1.)*0.5)   &
         &        +2.* ATAN (XX)                                            &
         &- PIHF
    PSPMS (YY)= 5.* YY
    PSPHU (XX)= -2.* log ( (XX * XX +1.)*0.5)
    PSPHS (YY)= 5.* YY








    ILECH = 0


    ZILFC = - CZIL * VKRM * SQVISC
    ZU = Z0
    RDZ = 1./ ZLM
    CXCH = EXCM * RDZ
    DTHV = THLM - THZ0


    DU2 = MAX (SFCSPD * SFCSPD,EPSU2)
    BTGH = BTG * HPBL

    IF(ITER == 1) THEN
        IF (BTGH * AKHS * DTHV .ne. 0.0) THEN
           WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2./3.)
        ELSE
           WSTAR2 = 0.0
        END IF
        USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)
        RLMO = ELFC * AKHS * DTHV / USTAR **3
    END IF
 

    ZT = MAX(1.E-6,EXP (ZILFC * SQRT (USTAR * Z0))* Z0)
    ZSLU = ZLM + ZU
    ZSLT = ZLM + ZT
    RLOGU = log (ZSLU / ZU)
    RLOGT = log (ZSLT / ZT)




    ZETALT = MAX (ZSLT * RLMO,ZTMIN)
    RLMO = ZETALT / ZSLT
    ZETALU = ZSLU * RLMO
    ZETAU = ZU * RLMO
    ZETAT = ZT * RLMO

    IF (ILECH .eq. 0) THEN
       IF (RLMO .lt. 0.)THEN
          XLU4 = 1. -16.* ZETALU
          XLT4 = 1. -16.* ZETALT
          XU4  = 1. -16.* ZETAU
          XT4  = 1. -16.* ZETAT
          XLU  = SQRT (SQRT (XLU4))
          XLT  = SQRT (SQRT (XLT4))
          XU   = SQRT (SQRT (XU4))

          XT = SQRT (SQRT (XT4))
          PSMZ = PSPMU (XU)
          SIMM = PSPMU (XLU) - PSMZ + RLOGU
          PSHZ = PSPHU (XT)
          SIMH = PSPHU (XLT) - PSHZ + RLOGT
       ELSE
          ZETALU = MIN (ZETALU,ZTMAX)
          ZETALT = MIN (ZETALT,ZTMAX)
          PSMZ = PSPMS (ZETAU)
          SIMM = PSPMS (ZETALU) - PSMZ + RLOGU
          PSHZ = PSPHS (ZETAT)
          SIMH = PSPHS (ZETALT) - PSHZ + RLOGT
       END IF



    ELSE
       IF (RLMO .lt. 0.)THEN
          PSMZ = PSLMU (ZETAU)
          SIMM = PSLMU (ZETALU) - PSMZ + RLOGU
          PSHZ = PSLHU (ZETAT)
          SIMH = PSLHU (ZETALT) - PSHZ + RLOGT
       ELSE
          ZETALU = MIN (ZETALU,ZTMAX)
          ZETALT = MIN (ZETALT,ZTMAX)
          PSMZ = PSLMS (ZETAU)
          SIMM = PSLMS (ZETALU) - PSMZ + RLOGU
          PSHZ = PSLHS (ZETAT)
          SIMH = PSLHS (ZETALT) - PSHZ + RLOGT
       END IF

       END IF




       USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)


       ZT = MAX(1.E-6,EXP (ZILFC * SQRT (USTAR * Z0))* Z0)
       ZSLT = ZLM + ZT

       RLOGT = log (ZSLT / ZT)
       USTARK = USTAR * VKRM
       AKMS = MAX (USTARK / SIMM,CXCH)



       AKHS = MAX (USTARK / SIMH,CXCH)

       IF (BTGH * AKHS * DTHV .ne. 0.0) THEN
          WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2./3.)
       ELSE
          WSTAR2 = 0.0
       END IF

       RLMN = ELFC * AKHS * DTHV / USTAR **3



       RLMA = RLMO * WOLD+ RLMN * WNEW

       RLMO = RLMA




  END SUBROUTINE SFCDIF2


  SUBROUTINE SFCDIF3(ILOC   ,JLOC   ,TSK    ,QS     ,PSFC   ,&  
                     PBLH   ,Z0     ,Z0BASE ,VEGTYP ,ISURBAN,&  
                     IZ0TLND,SFCSPD ,ITER   ,ITRMX  ,TLOW   ,&  
                     THLOW  ,QLOW   ,CWMLOW ,ZSL    ,        &  
                     PLOW   ,USTAR  ,AKMS   ,AKHS   ,CHS2   ,&  
                     CQS2   ,RLMO   )                           

  USE MODULE_SF_MYJSFC, ONLY :   &
     &                         EPSU2  , &  
     &                         EPSUST , &
     &                         EPSZT  , &  
     &                         BETA   , &  
     &                         EXCML  , &  
     &                         RIC    , &  
     &                         SQVISC , &
     &                         ZTFC   , &  
     &                         BTG    , &  
     &                         CZIV   , &  
     &                         PI     , &  
     &                         PIHF   , &  
     &                         KZTM   , &  
     &                         KZTM2  , &  
     &                         DZETA1 , &
     &                         DZETA2 , &
     &                         FH01   , &  
     &                         FH02   , &  
     &                         WWST2  , &  
     &                         WWST   , &  
     &                         ZTMAX1 , &
     &                         ZTMAX2 , &
     &                         ZTMIN1 , &
     &                         ZTMIN2 , &
     &                         PSIH1  , &  
     &                         PSIH2  , &  
     &                         PSIM1  , &  
     &                         PSIM2   

  USE MODULE_MODEL_CONSTANTS





   IMPLICIT NONE     


    INTEGER,INTENT(IN) :: ILOC
    INTEGER,INTENT(IN) :: JLOC
    REAL   ,INTENT(IN) :: TSK
    REAL   ,INTENT(IN) :: PSFC
    REAL   ,INTENT(IN) :: PBLH
    INTEGER,INTENT(IN) :: VEGTYP  
    INTEGER,INTENT(IN) :: ISURBAN 
    INTEGER,INTENT(IN) :: IZ0TLND
    REAL   ,INTENT(IN) :: QLOW
    REAL   ,INTENT(IN) :: THLOW
    REAL   ,INTENT(IN) :: TLOW
    REAL   ,INTENT(IN) :: CWMLOW
    REAL   ,INTENT(IN) :: SFCSPD
    REAL   ,INTENT(IN) :: PLOW
    REAL   ,INTENT(IN) :: ZSL
    REAL   ,INTENT(IN) :: Z0BASE
    INTEGER,INTENT(IN) :: ITER
    INTEGER,INTENT(IN) :: ITRMX


    REAL   ,INTENT(OUT) :: CHS2
    REAL   ,INTENT(OUT) :: CQS2
    REAL   ,INTENT(OUT) :: RLMO


    REAL   ,INTENT(INOUT) :: AKHS
    REAL   ,INTENT(INOUT) :: AKMS
    REAL   :: QZ0
    REAL   ,INTENT(INOUT) :: USTAR
    REAL   ,INTENT(IN) :: Z0
    REAL   ,INTENT(INOUT):: QS
    REAL                :: RIB


    INTEGER :: ITR,K
    REAL :: THZ0
    REAL :: THVLOW
    REAL :: CT
    REAL :: BTGH
    REAL :: BTGX
    REAL :: CXCHL
    REAL :: DTHV
    REAL :: DU2
    REAL :: ELFC
    REAL :: PSH02
    REAL :: PSH10
    REAL :: PSHZ
    REAL :: PSHZL
    REAL :: PSM10
    REAL :: PSMZ
    REAL :: PSMZL
    REAL :: RDZ
    REAL :: RDZT
    REAL :: RLMA 
    REAL :: RLMN 
    REAL :: RLOGT
    REAL :: RLOGU
    REAL :: RZ
    REAL :: SIMH
    REAL :: SIMM
    REAL :: USTARK
    REAL :: WSTAR2
    REAL :: WSTAR
    REAL :: CHS
    REAL :: RZSU
    REAL :: RZST
    REAL :: X,XLT,XLT4,XLU,XLU4,XT,XT4,XU,XU4,ZETALT,ZETALU         , &
            ZETAT,ZETAU,ZQ,ZSLT,ZSLU,ZT,ZU,TOPOTERM,ZZIL
    REAL :: AKHS02,AKHS10,AKMS02,AKMS10
    REAL :: ZU10
    REAL :: ZT02
    REAL :: ZT10
    REAL :: RLNU10
    REAL :: RLNT02
    REAL :: RLNT10
    REAL :: ZTAU10
    REAL :: ZTAT02
    REAL :: ZTAT10
    REAL :: SIMM10
    REAL :: SIMH02
    REAL :: SIMH10
    REAL :: ZUUZ
    REAL :: EKMS10
    REAL :: test
    REAL :: E1

   REAL,    PARAMETER :: VKRM    = 0.40
   REAL,    PARAMETER :: CZETMAX = 10.



   REAL :: CZIL
   REAL :: ZILFC


































    THVLOW = THLOW*(1.+EP_1*QLOW)
    THZ0   = TSK*(P1000mb/PSFC)**RCP


    ZU = Z0
    ZT = ZU*ZTFC      
    ZQ = ZT
    QZ0 = QS

    RDZ = 1./ZSL
    CXCHL = EXCML*RDZ
    DTHV = THVLOW-THZ0*(0.608*QZ0+1.)    

    BTGX=GRAV/THLOW
    ELFC=VKRM*BTGX


    IF(PBLH > 1000.)THEN  
       BTGH = BTGX*PBLH
    ELSE
       BTGH = BTGX*1000.
    ENDIF

    DU2 = MAX(SFCSPD*SFCSPD,EPSU2)  
    RIB = BTGX*DTHV*ZSL/DU2         

    ZSLU = ZSL+ZU
    RZSU = ZSLU/ZU
    RLOGU = LOG(RZSU)       

    ZSLT = ZSL + ZU

    IF ( (IZ0TLND==0) .or. (VEGTYP == ISURBAN) ) THEN    
        
        CZIL = 0.1
    ELSE
        
        
        CZIL = 10.0 ** ( -0.40 * ( Z0 / 0.07 ) )
    ENDIF
    ZILFC=-CZIL*VKRM*SQVISC     


    IF(DTHV>0.)THEN
       IF (RIB<RIC) THEN
          ZZIL=ZILFC*(1.0+(RIB/RIC)*(RIB/RIC)*CZETMAX)
       ELSE
          ZZIL=ZILFC*(1.0+CZETMAX)
       ENDIF

    ELSE
       ZZIL=ZILFC
    ENDIF



    ZT=MAX(EXP(ZZIL*SQRT(USTAR*Z0BASE))*Z0BASE,EPSZT)  
    RZST=ZSLT/ZT
    RLOGT=LOG(RZST)




    RLMO=ELFC*AKHS*DTHV/USTAR**3

    ZETALU=ZSLU*RLMO
    ZETALT=ZSLT*RLMO
    ZETAU=ZU*RLMO
    ZETAT=ZT*RLMO

    ZETALU=MIN(MAX(ZETALU,ZTMIN2),ZTMAX2)
    ZETALT=MIN(MAX(ZETALT,ZTMIN2),ZTMAX2)
    ZETAU=MIN(MAX(ZETAU,ZTMIN2/RZSU),ZTMAX2/RZSU)
    ZETAT=MIN(MAX(ZETAT,ZTMIN2/RZST),ZTMAX2/RZST)





    RZ=(ZETAU-ZTMIN2)/DZETA2
    K=INT(RZ)
    RDZT=RZ-REAL(K)
    K=MIN(K,KZTM2)
    K=MAX(K,0)
    PSMZ=(PSIM2(K+2)-PSIM2(K+1))*RDZT+PSIM2(K+1)

    RZ=(ZETALU-ZTMIN2)/DZETA2
    K=INT(RZ)
    RDZT=RZ-REAL(K)
    K=MIN(K,KZTM2)
    K=MAX(K,0)
    PSMZL=(PSIM2(K+2)-PSIM2(K+1))*RDZT+PSIM2(K+1)

    SIMM=PSMZL-PSMZ+RLOGU

    RZ=(ZETAT-ZTMIN2)/DZETA2
    K=INT(RZ)
    RDZT=RZ-REAL(K)
    K=MIN(K,KZTM2)
    K=MAX(K,0)
    PSHZ=(PSIH2(K+2)-PSIH2(K+1))*RDZT+PSIH2(K+1)

    RZ=(ZETALT-ZTMIN2)/DZETA2
    K=INT(RZ)
    RDZT=RZ-REAL(K)
    K=MIN(K,KZTM2)
    K=MAX(K,0)
    PSHZL=(PSIH2(K+2)-PSIH2(K+1))*RDZT+PSIH2(K+1)

    SIMH=(PSHZL-PSHZ+RLOGT)*FH02

    USTARK=USTAR*VKRM
    AKMS=MAX(USTARK/SIMM,CXCHL)
    AKHS=MAX(USTARK/SIMH,CXCHL)





    IF(DTHV<=0.)THEN                                           
      WSTAR2=WWST2*ABS(BTGH*AKHS*DTHV)**(2./3.)                
    ELSE                                                       
      WSTAR2=0.                                                
    ENDIF                                                      

    USTAR=MAX(SQRT(AKMS*SQRT(DU2+WSTAR2)),EPSUST)

    CT=0.






    WSTAR=SQRT(WSTAR2)/WWST




    ZU10=ZU+10.
    ZT02=ZT+02.
    ZT10=ZT+10.

    RLNU10=LOG(ZU10/ZU)
    RLNT02=LOG(ZT02/ZT)
    RLNT10=LOG(ZT10/ZT)

    ZTAU10=ZU10*RLMO
    ZTAT02=ZT02*RLMO
    ZTAT10=ZT10*RLMO

    ZTAU10=MIN(MAX(ZTAU10,ZTMIN2),ZTMAX2)
    ZTAT02=MIN(MAX(ZTAT02,ZTMIN2),ZTMAX2)
    ZTAT10=MIN(MAX(ZTAT10,ZTMIN2),ZTMAX2)


    RZ=(ZTAU10-ZTMIN2)/DZETA2
    K=INT(RZ)
    RDZT=RZ-REAL(K)
    K=MIN(K,KZTM2)
    K=MAX(K,0)
    PSM10=(PSIM2(K+2)-PSIM2(K+1))*RDZT+PSIM2(K+1)

    SIMM10=PSM10-PSMZ+RLNU10

    RZ=(ZTAT02-ZTMIN2)/DZETA2
    K=INT(RZ)
    RDZT=RZ-REAL(K)
    K=MIN(K,KZTM2)
    K=MAX(K,0)
    PSH02=(PSIH2(K+2)-PSIH2(K+1))*RDZT+PSIH2(K+1)

    SIMH02=(PSH02-PSHZ+RLNT02)*FH02

    RZ=(ZTAT10-ZTMIN2)/DZETA2
    K=INT(RZ)
    RDZT=RZ-REAL(K)
    K=MIN(K,KZTM2)
    K=MAX(K,0)
    PSH10=(PSIH2(K+2)-PSIH2(K+1))*RDZT+PSIH2(K+1)

    SIMH10=(PSH10-PSHZ+RLNT10)*FH02


    AKMS10=MAX(USTARK/SIMM10,CXCHL)
    AKHS02=MAX(USTARK/SIMH02,CXCHL)
    AKHS10=MAX(USTARK/SIMH10,CXCHL)



















































        ZUUZ=AMIN1(ZU*0.50,0.18)
        ZU=AMAX1(ZU*0.35,ZUUZ)

        ZU10=ZU+10.
        RZSU=ZU10/ZU
        RLNU10=LOG(RZSU)

        ZETAU=ZU*RLMO
        ZTAU10=ZU10*RLMO

        ZTAU10=MIN(MAX(ZTAU10,ZTMIN2),ZTMAX2)
        ZETAU=MIN(MAX(ZETAU,ZTMIN2/RZSU),ZTMAX2/RZSU)

        RZ=(ZTAU10-ZTMIN2)/DZETA2
        K=INT(RZ)
        RDZT=RZ-REAL(K)
        K=MIN(K,KZTM2)
        K=MAX(K,0)
        PSM10=(PSIM2(K+2)-PSIM2(K+1))*RDZT+PSIM2(K+1)
        SIMM10=PSM10-PSMZ+RLNU10
        EKMS10=MAX(USTARK/SIMM10,CXCHL)















      CHS=AKHS
      CHS2=AKHS02
      CQS2=AKHS02


  END SUBROUTINE SFCDIF3




   SUBROUTINE SFCDIF4(ILOC  ,JLOC  ,UX    ,VX     ,T1D  , &
                      P1D   ,PSFCPA,PBLH  ,DX     ,ZNT  , &
                      TSK   ,QX    ,ZLVL  ,IZ0TLND,QSFC , &
                      HFX   ,QFX   ,CM    ,CHS    ,CHS2 , &
                      CQS2  ,RMOL  ,UST   ,U10    ,V10)
                                                                                                                          
   USE MODULE_SF_SFCLAY                                                                                                   
   USE MODULE_MODEL_CONSTANTS                                                                                             




   IMPLICIT NONE                                                                                                          


   REAL,   PARAMETER     :: XKA=2.4E-5                                                                                    
   REAL,   PARAMETER     :: PRT=1.     
                                                                                                                          

   INTEGER,INTENT(IN )   :: ILOC                                                                                          
   INTEGER,INTENT(IN )   :: JLOC                                                                                          
                                                                                                                          
   REAL,   INTENT(IN )   :: PBLH      
   REAL,   INTENT(IN )   :: TSK       
   REAL,   INTENT(IN )   :: PSFCPA    
   REAL,   INTENT(IN )   :: P1D       
   REAL,   INTENT(IN )   :: T1D       

   REAL,   INTENT(IN )   :: QX        
   REAL,   INTENT(IN )   :: ZLVL      
   REAL,   INTENT(IN )   :: HFX       
   REAL,   INTENT(IN )   :: QFX       
   REAL,   INTENT(IN )   :: DX        
   REAL,   INTENT(IN )   :: UX
   REAL,   INTENT(IN )   :: VX
   REAL,   INTENT(IN )   :: ZNT                                                                                           
   REAL,   INTENT(INOUT ) :: QSFC

   REAL,   INTENT(INOUT) :: RMOL                                                                                          
   REAL,   INTENT(INOUT) :: UST                                                                                           
   REAL,   INTENT(INOUT) :: CHS2                                                                                          
   REAL,   INTENT(INOUT) :: CQS2                                                                                          
   REAL,   INTENT(INOUT) :: CHS                                                                                           
   REAL,   INTENT(INOUT) :: CM                
                                                                                                                          

   REAL,   INTENT(OUT)   :: U10                                                                                           
   REAL,   INTENT(OUT)   :: V10                                                                                           




                                                                                                                          

   INTEGER,OPTIONAL,INTENT(IN ) :: IZ0TLND                                                                                
                                                                                                                          

   INTEGER :: REGIME  
   REAL    :: ZA      
   REAL    :: THVX    
   REAL    :: ZQKL    
   REAL    :: ZQKLP1  
   REAL    :: THX     
   REAL    :: PSIH    
   REAL    :: PSIH2   
   REAL    :: PSIH10  
   REAL    :: PSIM    
   REAL    :: PSIM2   
   REAL    :: PSIM10  
   REAL    :: DENOMQ  
   REAL    :: DENOMQ2 
   REAL    :: DENOMT2 
   REAL    :: WSPDI   
   REAL    :: GZ1OZ0  
   REAL    :: GZ2OZ0  
   REAL    :: GZ10OZ0 
   REAL    :: RHOX    
   REAL    :: GOVRTH  
   REAL    :: TGDSA   

   REAL    :: TVIR    
   REAL    :: THGB    
   REAL    :: PSFC    
   REAL    :: BR      
   REAL    :: CPM                                                                                           
   REAL    :: MOL                                                                                           
   REAL    :: ZOL  
   REAL    :: QGH    
   REAL    :: WSPD                                                                                          
                                                                                                                          
   INTEGER :: N,I,K,KK,L,NZOL,NK,NZOL2,NZOL10                                                                             
                                                                                                                          
   REAL    ::  PL,THCON,TVCON,E1                                                                                          
   REAL    ::  ZL,TSKV,DTHVDZ,DTHVM,VCONV,RZOL,RZOL2,RZOL10,ZOL2,ZOL10                                                    
   REAL    ::  DTG,PSIX,DTTHX,PSIX10,PSIT,PSIT2,PSIQ,PSIQ2,PSIQ10                                                         
   REAL    ::  FLUXC,VSGD,Z0Q,VISC,RESTAR,CZIL,RESTAR2                                                                    

   
   MOL = 1./RMOL
   ZL=0.01                                                                                                                
   PSFC=PSFCPA/1000.                                                                                                      
                                                                                                                          

   TGDSA = TSK                                                                                                            
   THGB  = TSK*(P1000mb/PSFCPA)**RCP                                                                                      
                                                                                                                          

   PL    = P1D/1000.                                                                                                      
   THX   = T1D*(P1000mb*0.001/PL)**RCP                                                                                        
   THVX  = THX*(1.+EP_1*QX)                                                                                                   
   TVIR  = T1D*(1.+EP_1*QX)


   
   IF (QSFC.LE.0.0) THEN
      
      E1=SVP1*EXP(SVP2*(TGDSA-SVPT0)/(TGDSA-SVP3))                                                                           
      QSFC=EP_2*E1/(PSFC-E1)                                                                                
      write(*,*) "JREF: IN SFCDIF4, QSFC WAS NEG. NOW = ",QSFC
   ENDIF





   CPM=CP*(1.+0.8*QX)                                                                                                     
                                                                                                                          

   
   ZA = ZLVL
                                                                                                                          

   RHOX=PSFC*1000./(R_D*TVIR)                                                                                             
   GOVRTH=G/THX                                                                                                           
                                                                                                                          


   GZ1OZ0=ALOG(ZA/ZNT)                                                                                                    
   GZ2OZ0=ALOG(2./ZNT)                                                                                                    
   GZ10OZ0=ALOG(10./ZNT)                                                                                                  
   WSPD=SQRT(UX*UX+VX*VX)                                                                                                 
                                                                                                                          

   TSKV=THGB*(1.+EP_1*QSFC)                                                                                               
   DTHVDZ=(THVX-TSKV)                                                                                                     
                                                                                                                          






                                                                                                                          

  FLUXC = MAX(HFX/RHOX/CP + EP_1*TSKV*QFX/RHOX,0.)                                                                                          
  VCONV = VCONVC*(G/TGDSA*PBLH*FLUXC)**.33                                                                                


                                                                                                                          

   VSGD = 0.32 * (max(dx/5000.-1.,0.))**.33                                                                               
   WSPD=SQRT(WSPD*WSPD+VCONV*VCONV+VSGD*VSGD)                                                                             
   WSPD=AMAX1(WSPD,0.1)                                                                                                   
   BR=GOVRTH*ZA*DTHVDZ/(WSPD*WSPD)                                                                                        

   IF(MOL.LT.0.) BR=AMIN1(BR,0.0)                                                                                         
   RMOL=-GOVRTH*DTHVDZ*ZA*KARMAN                                                                                          
                                                                                                                          






















                                                                                                                          
   IF (BR.GE.0.2) REGIME=1                                                                                                
   IF (BR.LT.0.2 .AND. BR.GT.0.0) REGIME=2                                                                                
   IF (BR.EQ.0.0) REGIME=3                                                                                                
   IF (BR.LT.0.0) REGIME=4                                                                                                
                                                                                                                          
   SELECT CASE(REGIME)                                                                                                    
     CASE(1) 
       PSIM=-10.*GZ1OZ0                                                                                                   

       PSIM=AMAX1(PSIM,-10.)                                                                                              
       PSIH=PSIM                                                                                                          
       PSIM10=10./ZA*PSIM                                                                                                 
       PSIM10=AMAX1(PSIM10,-10.)                                                                                          
       PSIH10=PSIM10                                                                                                      
       PSIM2=2./ZA*PSIM                                                                                                   
       PSIM2=AMAX1(PSIM2,-10.)                                                                                            
       PSIH2=PSIM2                                                                                                        
                                                                                                                          

       IF(UST.LT.0.01)THEN                                                                                                
          RMOL=BR*GZ1OZ0 
       ELSE                                                                                                               
          RMOL=KARMAN*GOVRTH*ZA*MOL/(UST*UST) 
       ENDIF                                                                                                              
       RMOL=AMIN1(RMOL,9.999) 
       RMOL = RMOL/ZA 
                                                                                                                          
     CASE(2) 
       PSIM=-5.0*BR*GZ1OZ0/(1.1-5.0*BR)                                                                                   

       PSIM=AMAX1(PSIM,-10.)                                                                                              

       PSIH=PSIM                                                                                                          
       PSIM10=10./ZA*PSIM                                                                                                 
       PSIM10=AMAX1(PSIM10,-10.)                                                                                          
       PSIH10=PSIM10                                                                                                      
       PSIM2=2./ZA*PSIM                                                                                                   
       PSIM2=AMAX1(PSIM2,-10.)                                                                                            
       PSIH2=PSIM2                                                                                                        
                                                                                                                          
     
     
     
     
       ZOL = BR*GZ1OZ0/(1.00001-5.0*BR)                                                                                   
                                                                                                                          
       IF ( ZOL .GT. 0.5 ) THEN 
        
        
        
          ZOL = ( 1.89*GZ1OZ0 + 44.2 ) * BR*BR    &                                                                       
               + ( 1.18*GZ1OZ0 - 1.37 ) * BR                                                                              
          ZOL=AMIN1(ZOL,9.999)                                                                                            
       END IF                                                                                                             
                                                                                                                          

       RMOL= ZOL/ZA                                                                                                       
                                                                                                                          
     CASE(3)  
       PSIM=0.0                                                                                                           
       PSIH=PSIM                                                                                                          
       PSIM10=0.                                                                                                          
       PSIH10=PSIM10                                                                                                      
       PSIM2=0.                                                                                                           
       PSIH2=PSIM2                                                                                                        
       IF(UST.LT.0.01)THEN                                                                                                
         ZOL=BR*GZ1OZ0                                                                                                    
       ELSE                                                                                                               
         ZOL=KARMAN*GOVRTH*ZA*MOL/(UST*UST)                                                                               
       ENDIF                                                                                                              
                                                                                                                          
       RMOL = ZOL/ZA                                                                                                      
                                                                                                                          
     CASE(4) 
       IF(UST.LT.0.01)THEN                                                                                                
         ZOL=BR*GZ1OZ0                                                                                                    
       ELSE                                                                                                               
         ZOL=KARMAN*GOVRTH*ZA*MOL/(UST*UST)                                                                               
       ENDIF                                                                                                              
       ZOL10=10./ZA*ZOL                                                                                                   
       ZOL2=2./ZA*ZOL                                                                                                     
       ZOL=AMIN1(ZOL,0.)                                                                                                  
       ZOL=AMAX1(ZOL,-9.9999)                                                                                             
       ZOL10=AMIN1(ZOL10,0.)                                                                                              
       ZOL10=AMAX1(ZOL10,-9.9999)                                                                                         
       ZOL2=AMIN1(ZOL2,0.)                                                                                                
       ZOL2=AMAX1(ZOL2,-9.9999)                                                                                           
       NZOL=INT(-ZOL*100.)                                                                                                
       RZOL=-ZOL*100.-NZOL                                                                                                
       NZOL10=INT(-ZOL10*100.)                                                                                            
       RZOL10=-ZOL10*100.-NZOL10                                                                                          
       NZOL2=INT(-ZOL2*100.)                                                                                              
       RZOL2=-ZOL2*100.-NZOL2                                                                                             
       PSIM=PSIMTB(NZOL)+RZOL*(PSIMTB(NZOL+1)-PSIMTB(NZOL))                                                               
       PSIH=PSIHTB(NZOL)+RZOL*(PSIHTB(NZOL+1)-PSIHTB(NZOL))                                                               
       PSIM10=PSIMTB(NZOL10)+RZOL10*(PSIMTB(NZOL10+1)-PSIMTB(NZOL10))                                                     
       PSIH10=PSIHTB(NZOL10)+RZOL10*(PSIHTB(NZOL10+1)-PSIHTB(NZOL10))                                                     
       PSIM2=PSIMTB(NZOL2)+RZOL2*(PSIMTB(NZOL2+1)-PSIMTB(NZOL2))                                                          
       PSIH2=PSIHTB(NZOL2)+RZOL2*(PSIHTB(NZOL2+1)-PSIHTB(NZOL2))                                                          
                                                                                                                          




       PSIH=AMIN1(PSIH,0.9*GZ1OZ0)                                                                                        
       PSIM=AMIN1(PSIM,0.9*GZ1OZ0)                                                                                        
       PSIH2=AMIN1(PSIH2,0.9*GZ2OZ0)                                                                                      
       PSIM10=AMIN1(PSIM10,0.9*GZ10OZ0)                                                                                   

       PSIH10=AMIN1(PSIH10,0.9*GZ10OZ0)                                                                                   
                                                                                                                          
       RMOL = ZOL/ZA                                                                                                      
                                                                                                                          
   END SELECT 
                                                                                                                          

   DTG=THX-THGB                                                                                                           
   PSIX=GZ1OZ0-PSIM                                                                                                       
   PSIX10=GZ10OZ0-PSIM10                                                                                                  
                                                                                                                          


   PSIT=AMAX1(GZ1OZ0-PSIH,2.) 
   PSIQ=ALOG(KARMAN*UST*ZA/XKA+ZA/ZL)-PSIH                                                                                
   PSIT2=GZ2OZ0-PSIH2                                                                                                     
   PSIQ2=ALOG(KARMAN*UST*2./XKA+2./ZL)-PSIH2                                                                              

   PSIQ10=ALOG(KARMAN*UST*10./XKA+10./ZL)-PSIH10                                                                          

















   IF ( PRESENT(IZ0TLND) ) THEN                                                                                           
      IF ( IZ0TLND.EQ.1 ) THEN                                                                                            
         ZL=ZNT                                                                                                           

         VISC=(1.32+0.009*(T1D-273.15))*1.E-5                                                                            
         RESTAR=UST*ZL/VISC                                                                                               

                                                                                                                          
         CZIL = 10.0 ** ( -0.40 * ( ZL / 0.07 ) )                                                                         
                                                                                                                          
         PSIT=GZ1OZ0-PSIH+CZIL*KARMAN*SQRT(RESTAR)                                                                        
         PSIQ=GZ1OZ0-PSIH+CZIL*KARMAN*SQRT(RESTAR)                                                                        
         PSIT2=GZ2OZ0-PSIH2+CZIL*KARMAN*SQRT(RESTAR)                                                                      
         PSIQ2=GZ2OZ0-PSIH2+CZIL*KARMAN*SQRT(RESTAR)                                                                      
      ENDIF                                                                                                               
   ENDIF                                                                                                                  
                                                                                                                          


   UST=0.5*UST+0.5*KARMAN*WSPD/PSIX                                                                                       
   UST=AMAX1(UST,0.1)                                                                                                     

   MOL=KARMAN*DTG/PSIT/PRT                                                                                                
   DENOMQ=PSIQ                                                                                                            
   DENOMQ2=PSIQ2                                                                                                          
   DENOMT2=PSIT2                                                                                                          



                                                                                                                          


   CM =KARMAN*KARMAN/(PSIX*PSIX)

   CHS=UST*KARMAN/DENOMQ                                                                                                  




   CQS2=UST*KARMAN/DENOMQ2                                                                                                
   CHS2=UST*KARMAN/DENOMT2                                                                                                

                                                                                                                         
   U10=UX*PSIX10/PSIX                                                                                                     
   V10=VX*PSIX10/PSIX                                                                                                     
                                                                                                                          




                                                                                                                          
   END SUBROUTINE SFCDIF4                                                                                                 


  SUBROUTINE ESAT(T, ESW, ESI, DESW, DESI)



  IMPLICIT NONE



  REAL, intent(in)  :: T              



  REAL, intent(out) :: ESW            
  REAL, intent(out) :: ESI            
  REAL, intent(out) :: DESW           
  REAL, intent(out) :: DESI           



  REAL :: A0,A1,A2,A3,A4,A5,A6  
  REAL :: B0,B1,B2,B3,B4,B5,B6  
  REAL :: C0,C1,C2,C3,C4,C5,C6  
  REAL :: D0,D1,D2,D3,D4,D5,D6  

  PARAMETER (A0=6.107799961    , A1=4.436518521E-01,  &
             A2=1.428945805E-02, A3=2.650648471E-04,  &
             A4=3.031240396E-06, A5=2.034080948E-08,  &
             A6=6.136820929E-11)

  PARAMETER (B0=6.109177956    , B1=5.034698970E-01,  &
             B2=1.886013408E-02, B3=4.176223716E-04,  &
             B4=5.824720280E-06, B5=4.838803174E-08,  &
             B6=1.838826904E-10)

  PARAMETER (C0= 4.438099984E-01, C1=2.857002636E-02,  &
             C2= 7.938054040E-04, C3=1.215215065E-05,  &
             C4= 1.036561403E-07, C5=3.532421810e-10,  &
             C6=-7.090244804E-13)

  PARAMETER (D0=5.030305237E-01, D1=3.773255020E-02,  &
             D2=1.267995369E-03, D3=2.477563108E-05,  &
             D4=3.005693132E-07, D5=2.158542548E-09,  &
             D6=7.131097725E-12)

  ESW  = 100.*(A0+T*(A1+T*(A2+T*(A3+T*(A4+T*(A5+T*A6))))))
  ESI  = 100.*(B0+T*(B1+T*(B2+T*(B3+T*(B4+T*(B5+T*B6))))))
  DESW = 100.*(C0+T*(C1+T*(C2+T*(C3+T*(C4+T*(C5+T*C6))))))
  DESI = 100.*(D0+T*(D1+T*(D2+T*(D3+T*(D4+T*(D5+T*D6))))))

  END SUBROUTINE ESAT


  SUBROUTINE STOMATA (VEGTYP  ,MPE     ,APAR    ,FOLN    ,ILOC    , JLOC, & 
                      TV      ,EI      ,EA      ,SFCTMP  ,SFCPRS  , & 
                      O2      ,CO2     ,IGS     ,BTRAN   ,RB      , & 
                      RS      ,PSN     )                              

  USE NOAHMP_VEG_PARAMETERS

  IMPLICIT NONE


      INTEGER,INTENT(IN)  :: ILOC   
      INTEGER,INTENT(IN)  :: JLOC   
      INTEGER,INTENT(IN)  :: VEGTYP 

      REAL, INTENT(IN)    :: IGS    
      REAL, INTENT(IN)    :: MPE    

      REAL, INTENT(IN)    :: TV     
      REAL, INTENT(IN)    :: EI     
      REAL, INTENT(IN)    :: EA     
      REAL, INTENT(IN)    :: APAR   
      REAL, INTENT(IN)    :: O2     
      REAL, INTENT(IN)    :: CO2    
      REAL, INTENT(IN)    :: SFCPRS 
      REAL, INTENT(IN)    :: SFCTMP 
      REAL, INTENT(IN)    :: BTRAN  
      REAL, INTENT(IN)    :: FOLN   
      REAL, INTENT(IN)    :: RB     


      REAL, INTENT(OUT)   :: RS     
      REAL, INTENT(OUT)   :: PSN    


      REAL                :: RLB    



      INTEGER :: ITER     
      INTEGER :: NITER    

      DATA NITER /3/
      SAVE NITER

      REAL :: AB          
      REAL :: BC          
      REAL :: F1          
      REAL :: F2          
      REAL :: TC          
      REAL :: CS          
      REAL :: KC          
      REAL :: KO          
      REAL :: A,B,C,Q     
      REAL :: R1,R2       
      REAL :: FNF         
      REAL :: PPF         
      REAL :: WC          
      REAL :: WJ          
      REAL :: WE          
      REAL :: CP          
      REAL :: CI          
      REAL :: AWC         
      REAL :: VCMX        
      REAL :: J           
      REAL :: CEA         
      REAL :: CF          

      F1(AB,BC) = AB**((BC-25.)/10.)
      F2(AB) = 1. + EXP((-2.2E05+710.*(AB+273.16))/(8.314*(AB+273.16)))
      REAL :: T





         CF = SFCPRS/(8.314*SFCTMP)*1.e06
         RS = 1./BP(VEGTYP) * CF
         PSN = 0.

         IF (APAR .LE. 0.) RETURN

         FNF = MIN( FOLN/MAX(MPE,FOLNMX(VEGTYP)), 1.0 )
         TC  = TV-TFRZ
         PPF = 4.6*APAR
         J   = PPF*QE25(VEGTYP)
         KC  = KC25(VEGTYP) * F1(AKC(VEGTYP),TC)
         KO  = KO25(VEGTYP) * F1(AKO(VEGTYP),TC)
         AWC = KC * (1.+O2/KO)
         CP  = 0.5*KC/KO*O2*0.21
         VCMX = VCMX25(VEGTYP) / F2(TC) * FNF * BTRAN * F1(AVCMX(VEGTYP),TC)



         CI = 0.7*CO2*C3PSN(VEGTYP) + 0.4*CO2*(1.-C3PSN(VEGTYP))



         RLB = RB/CF



         CEA = MAX(0.25*EI*C3PSN(VEGTYP)+0.40*EI*(1.-C3PSN(VEGTYP)), MIN(EA,EI) )



       DO ITER = 1, NITER
            WJ = MAX(CI-CP,0.)*J/(CI+2.*CP)*C3PSN(VEGTYP)  + J*(1.-C3PSN(VEGTYP))
            WC = MAX(CI-CP,0.)*VCMX/(CI+AWC)*C3PSN(VEGTYP) + VCMX*(1.-C3PSN(VEGTYP))
            WE = 0.5*VCMX*C3PSN(VEGTYP) + 4000.*VCMX*CI/SFCPRS*(1.-C3PSN(VEGTYP))
            PSN = MIN(WJ,WC,WE) * IGS

            CS = MAX( CO2-1.37*RLB*SFCPRS*PSN, MPE )
            A = MP(VEGTYP)*PSN*SFCPRS*CEA / (CS*EI) + BP(VEGTYP)
            B = ( MP(VEGTYP)*PSN*SFCPRS/CS + BP(VEGTYP) ) * RLB - 1.
            C = -RLB
            IF (B .GE. 0.) THEN
               Q = -0.5*( B + SQRT(B*B-4.*A*C) )
            ELSE
               Q = -0.5*( B - SQRT(B*B-4.*A*C) )
            END IF
            R1 = Q/A
            R2 = C/Q
            RS = MAX(R1,R2)
            CI = MAX( CS-PSN*SFCPRS*1.65*RS, 0. )
       END DO 



         RS = RS*CF

  END SUBROUTINE STOMATA

  SUBROUTINE CANRES (PAR   ,SFCTMP,RCSOIL ,EAH   ,SFCPRS , & 
                     RC    ,PSN   ,ILOC   ,JLOC  )           













    IMPLICIT NONE



    INTEGER,                  INTENT(IN)  :: ILOC   
    INTEGER,                  INTENT(IN)  :: JLOC   
    REAL,                     INTENT(IN)  :: PAR    
    REAL,                     INTENT(IN)  :: SFCTMP 
    REAL,                     INTENT(IN)  :: SFCPRS 
    REAL,                     INTENT(IN)  :: EAH    
    REAL,                     INTENT(IN)  :: RCSOIL 



    REAL,                     INTENT(OUT) :: RC     
    REAL,                     INTENT(OUT) :: PSN    



    REAL                                  :: RCQ
    REAL                                  :: RCS
    REAL                                  :: RCT
    REAL                                  :: FF
    REAL                                  :: Q2     
    REAL                                  :: Q2SAT  
    REAL                                  :: DQSDT2 





    RC     = 0.0
    RCS    = 0.0
    RCT    = 0.0
    RCQ    = 0.0



    Q2 = 0.622 *  EAH  / (SFCPRS - 0.378 * EAH) 
    Q2 = Q2 / (1.0 + Q2)                        

    CALL CALHUM(SFCTMP, SFCPRS, Q2SAT, DQSDT2)



    FF  = 2.0 * PAR / RGL                
    RCS = (FF + RSMIN / RSMAX) / (1.0+ FF)
    RCS = MAX (RCS,0.0001)



    RCT = 1.0- 0.0016* ( (TOPT - SFCTMP)**2.0)
    RCT = MAX (RCT,0.0001)



    RCQ = 1.0/ (1.0+ HS * MAX(0.,Q2SAT-Q2))
    RCQ = MAX (RCQ,0.01)



    RC  = RSMIN / (RCS * RCT * RCQ * RCSOIL)
    PSN = -999.99       

  END SUBROUTINE CANRES

        SUBROUTINE CALHUM(SFCTMP, SFCPRS, Q2SAT, DQSDT2)

        IMPLICIT NONE

        REAL, INTENT(IN)       :: SFCTMP, SFCPRS
        REAL, INTENT(OUT)      :: Q2SAT, DQSDT2
        REAL, PARAMETER        :: A2=17.67,A3=273.15,A4=29.65, ELWV=2.501E6,         &
                                  A23M4=A2*(A3-A4), E0=0.611, RV=461.0,             &
                                  EPSILON=0.622
        REAL                   :: ES, SFCPRSX


        ES = E0 * EXP ( ELWV/RV*(1./A3 - 1./SFCTMP) )

        SFCPRSX = SFCPRS*1.E-3
        Q2SAT = EPSILON * ES / (SFCPRSX-ES)

        Q2SAT = Q2SAT * 1.E3



        DQSDT2=(Q2SAT/(1+Q2SAT))*A23M4/(SFCTMP-A4)**2


        Q2SAT = Q2SAT / 1.E3

        END SUBROUTINE CALHUM

  SUBROUTINE TSNOSOI (ICE     ,NSOIL   ,NSNOW   ,ISNOW   ,IST     , & 
                      TBOT    ,ZSNSO   ,SSOIL   ,DF      ,HCPCT   , & 
                      ZBOT    ,SAG     ,DT      ,SNOWH   ,DZSNSO  , & 
                      TG      ,ILOC    ,JLOC    ,                   & 
                      STC     )                                       





  IMPLICIT NONE



    INTEGER,                         INTENT(IN)  :: ILOC
    INTEGER,                         INTENT(IN)  :: JLOC
    INTEGER,                         INTENT(IN)  :: ICE    
    INTEGER,                         INTENT(IN)  :: NSOIL  
    INTEGER,                         INTENT(IN)  :: NSNOW  
    INTEGER,                         INTENT(IN)  :: ISNOW  
    INTEGER,                         INTENT(IN)  :: IST    

    REAL,                            INTENT(IN)  :: DT     
    REAL,                            INTENT(IN)  :: TBOT   
    REAL,                            INTENT(IN)  :: SSOIL  
    REAL,                            INTENT(IN)  :: SAG    
    REAL,                            INTENT(IN)  :: SNOWH  
    REAL,                            INTENT(IN)  :: ZBOT   
    REAL,                            INTENT(IN)  :: TG     
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: ZSNSO  
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DZSNSO 
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DF     
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: HCPCT  



    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC



    INTEGER                                      :: IZ
    REAL                                         :: ZBOTSNO   
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: AI, BI, CI, RHSTS
    REAL                                         :: EFLXB 
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: PHI   

    REAL, DIMENSION(-NSNOW+1:NSOIL) :: TBEG
    REAL                            :: ERR_EST 
    REAL                            :: SSOIL2  
    REAL                            :: EFLXB2  
    character(len=256)              :: message



    PHI(ISNOW+1:NSOIL) = 0.



    ZBOTSNO = ZBOT - SNOWH    



    DO IZ = ISNOW+1, NSOIL
       TBEG(IZ) = STC(IZ)
    ENDDO



      CALL HRT   (NSNOW     ,NSOIL     ,ISNOW     ,ZSNSO     , &
                  STC       ,TBOT      ,ZBOTSNO   ,DT        , &
                  DF        ,HCPCT     ,SSOIL     ,PHI       , &
                  AI        ,BI        ,CI        ,RHSTS     , &
                  EFLXB     )

      CALL HSTEP (NSNOW     ,NSOIL     ,ISNOW     ,DT        , &
                  AI        ,BI        ,CI        ,RHSTS     , &
                  STC       ) 




    IF(OPT_TBOT == 1) THEN
       EFLXB2  = 0.
    ELSE IF(OPT_TBOT == 2) THEN
       EFLXB2  = DF(NSOIL)*(TBOT-STC(NSOIL)) / &
            (0.5*(ZSNSO(NSOIL-1)+ZSNSO(NSOIL)) - ZBOTSNO)
    END IF

    
    
    return



    ERR_EST = 0.0
    DO IZ = ISNOW+1, NSOIL
       ERR_EST = ERR_EST + (STC(IZ)-TBEG(IZ)) * DZSNSO(IZ) * HCPCT(IZ) / DT
    ENDDO

    if (OPT_STC == 1) THEN   
       ERR_EST = ERR_EST - (SSOIL +EFLXB)
    ELSE                     
       SSOIL2 = DF(ISNOW+1)*(TG-STC(ISNOW+1))/(0.5*DZSNSO(ISNOW+1))   
       ERR_EST = ERR_EST - (SSOIL2+EFLXB2)
    ENDIF

    IF (ABS(ERR_EST) > 1.) THEN    
       WRITE(message,*) 'TSNOSOI is losing(-)/gaining(+) false energy',ERR_EST,' W/m2'
       call wrf_message(trim(message))
       WRITE(message,'(i6,1x,i6,1x,i3,F18.13,5F20.12)') &
            ILOC, JLOC, IST,ERR_EST,SSOIL,SNOWH,TG,STC(ISNOW+1),EFLXB
       call wrf_message(trim(message))
       
    END IF

  END SUBROUTINE TSNOSOI


  SUBROUTINE HRT (NSNOW     ,NSOIL     ,ISNOW     ,ZSNSO     , &
                  STC       ,TBOT      ,ZBOT      ,DT        , &
                  DF        ,HCPCT     ,SSOIL     ,PHI       , &
                  AI        ,BI        ,CI        ,RHSTS     , &
                  BOTFLX    )






    IMPLICIT NONE



    INTEGER,                         INTENT(IN)  :: NSOIL  
    INTEGER,                         INTENT(IN)  :: NSNOW  
    INTEGER,                         INTENT(IN)  :: ISNOW  
    REAL,                            INTENT(IN)  :: TBOT   
    REAL,                            INTENT(IN)  :: ZBOT   
                                                           
    REAL,                            INTENT(IN)  :: DT     
    REAL,                            INTENT(IN)  :: SSOIL  
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: ZSNSO  
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: STC    
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DF     
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: HCPCT  
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: PHI    



    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: RHSTS  
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: AI     
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: BI     
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: CI     
    REAL,                            INTENT(OUT) :: BOTFLX 



    INTEGER                                      :: K
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DDZ
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DZ
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DENOM
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DTSDZ
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: EFLUX
    REAL                                         :: TEMP1


    DO K = ISNOW+1, NSOIL
        IF (K == ISNOW+1) THEN
           DENOM(K)  = - ZSNSO(K) * HCPCT(K)
           TEMP1     = - ZSNSO(K+1)
           DDZ(K)    = 2.0 / TEMP1
           DTSDZ(K)  = 2.0 * (STC(K) - STC(K+1)) / TEMP1
           EFLUX(K)  = DF(K) * DTSDZ(K) - SSOIL - PHI(K)
        ELSE IF (K < NSOIL) THEN
           DENOM(K)  = (ZSNSO(K-1) - ZSNSO(K)) * HCPCT(K)
           TEMP1     = ZSNSO(K-1) - ZSNSO(K+1)
           DDZ(K)    = 2.0 / TEMP1
           DTSDZ(K)  = 2.0 * (STC(K) - STC(K+1)) / TEMP1
           EFLUX(K)  = (DF(K)*DTSDZ(K) - DF(K-1)*DTSDZ(K-1)) - PHI(K)
        ELSE IF (K == NSOIL) THEN
           DENOM(K)  = (ZSNSO(K-1) - ZSNSO(K)) * HCPCT(K)
           TEMP1     =  ZSNSO(K-1) - ZSNSO(K)
           IF(OPT_TBOT == 1) THEN
               BOTFLX     = 0. 
           END IF
           IF(OPT_TBOT == 2) THEN
               DTSDZ(K)  = (STC(K) - TBOT) / ( 0.5*(ZSNSO(K-1)+ZSNSO(K)) - ZBOT)
               BOTFLX    = -DF(K) * DTSDZ(K)
           END IF
           EFLUX(K)  = (-BOTFLX - DF(K-1)*DTSDZ(K-1) ) - PHI(K)
        END IF
    END DO

    DO K = ISNOW+1, NSOIL
        IF (K == ISNOW+1) THEN
           AI(K)    =   0.0
           CI(K)    = - DF(K)   * DDZ(K) / DENOM(K)
           IF (OPT_STC == 1) THEN
              BI(K) = - CI(K)
           END IF                                        
           IF (OPT_STC == 2) THEN
              BI(K) = - CI(K) + DF(K)/(0.5*ZSNSO(K)*ZSNSO(K)*HCPCT(K))
           END IF
        ELSE IF (K < NSOIL) THEN
           AI(K)    = - DF(K-1) * DDZ(K-1) / DENOM(K) 
           CI(K)    = - DF(K  ) * DDZ(K  ) / DENOM(K) 
           BI(K)    = - (AI(K) + CI (K))
        ELSE IF (K == NSOIL) THEN
           AI(K)    = - DF(K-1) * DDZ(K-1) / DENOM(K) 
           CI(K)    = 0.0
           BI(K)    = - (AI(K) + CI(K))
        END IF
           RHSTS(K)  = EFLUX(K)/ (-DENOM(K))
    END DO

  END SUBROUTINE HRT


  SUBROUTINE HSTEP (NSNOW     ,NSOIL     ,ISNOW     ,DT        ,  &
                    AI        ,BI        ,CI        ,RHSTS     ,  &
                    STC       )  



    implicit none



    INTEGER,                         INTENT(IN)    :: NSOIL
    INTEGER,                         INTENT(IN)    :: NSNOW
    INTEGER,                         INTENT(IN)    :: ISNOW
    REAL,                            INTENT(IN)    :: DT


    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: RHSTS
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: AI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: BI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: CI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC


    INTEGER                                        :: K
    REAL, DIMENSION(-NSNOW+1:NSOIL)                :: RHSTSIN
    REAL, DIMENSION(-NSNOW+1:NSOIL)                :: CIIN


    DO K = ISNOW+1,NSOIL
       RHSTS(K) =   RHSTS(K) * DT
       AI(K)    =      AI(K) * DT
       BI(K)    = 1. + BI(K) * DT
       CI(K)    =      CI(K) * DT
    END DO



    DO K = ISNOW+1,NSOIL
       RHSTSIN(K) = RHSTS(K)
       CIIN(K)    = CI(K)
    END DO



    CALL ROSR12 (CI,AI,BI,CIIN,RHSTSIN,RHSTS,ISNOW+1,NSOIL,NSNOW)



    DO K = ISNOW+1,NSOIL
       STC (K) = STC (K) + CI (K)
    END DO

  END SUBROUTINE HSTEP

  SUBROUTINE ROSR12 (P,A,B,C,D,DELTA,NTOP,NSOIL,NSNOW)


















    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: NTOP           
    INTEGER, INTENT(IN)   :: NSOIL,NSNOW
    INTEGER               :: K, KK

    REAL, DIMENSION(-NSNOW+1:NSOIL),INTENT(IN):: A, B, D
    REAL, DIMENSION(-NSNOW+1:NSOIL),INTENT(INOUT):: C,P,DELTA




    C (NSOIL) = 0.0
    P (NTOP) = - C (NTOP) / B (NTOP)



    DELTA (NTOP) = D (NTOP) / B (NTOP)



    DO K = NTOP+1,NSOIL
       P (K) = - C (K) * ( 1.0 / (B (K) + A (K) * P (K -1)) )
       DELTA (K) = (D (K) - A (K)* DELTA (K -1))* (1.0/ (B (K) + A (K)&
            * P (K -1)))
    END DO



    P (NSOIL) = DELTA (NSOIL)



    DO K = NTOP+1,NSOIL
       KK = NSOIL - K + (NTOP-1) + 1
       P (KK) = P (KK) * P (KK +1) + DELTA (KK)
    END DO

  END SUBROUTINE ROSR12


  SUBROUTINE PHASECHANGE (NSNOW   ,NSOIL   ,ISNOW   ,DT      ,FACT    , & 
                          DZSNSO  ,HCPCT   ,IST     ,ILOC    ,JLOC    , & 
                          STC     ,SNICE   ,SNLIQ   ,SNEQV   ,SNOWH   , & 
                          SMC     ,SH2O    ,                            & 
                          QMELT   ,IMELT   ,PONDING )                     



  IMPLICIT NONE



  INTEGER, INTENT(IN)                             :: ILOC   
  INTEGER, INTENT(IN)                             :: JLOC   
  INTEGER, INTENT(IN)                             :: NSNOW  
  INTEGER, INTENT(IN)                             :: NSOIL  
  INTEGER, INTENT(IN)                             :: ISNOW  
  INTEGER, INTENT(IN)                             :: IST    
  REAL, INTENT(IN)                                :: DT     
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)     :: FACT   
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)     :: DZSNSO 
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)     :: HCPCT  


  INTEGER, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: IMELT  
  REAL,                               INTENT(OUT) :: QMELT  
  REAL,                               INTENT(OUT) :: PONDING



  REAL, INTENT(INOUT) :: SNEQV
  REAL, INTENT(INOUT) :: SNOWH
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT)  :: STC    
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT)  :: SH2O   
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT)  :: SMC    
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(INOUT)  :: SNICE  
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(INOUT)  :: SNLIQ  



  INTEGER                         :: J         
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: HM        
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: XM        
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WMASS0
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WICE0 
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WLIQ0 
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: MICE      
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: MLIQ      
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: SUPERCOOL 
  REAL                            :: HEATR     
  REAL                            :: TEMP1     
  REAL                            :: PROPOR
  REAL                            :: SMP       
  REAL                            :: XMF       




    QMELT   = 0.
    PONDING = 0.
    XMF     = 0.

    DO J = -NSNOW+1, NSOIL
         SUPERCOOL(J) = 0.0
    END DO

    DO J = ISNOW+1,0       
         MICE(J) = SNICE(J)
         MLIQ(J) = SNLIQ(J)
    END DO

    DO J = 1, NSOIL               
         MLIQ(J) =  SH2O(J)            * DZSNSO(J) * 1000.
         MICE(J) = (SMC(J) - SH2O(J))  * DZSNSO(J) * 1000.
    END DO

    DO J = ISNOW+1,NSOIL       
         IMELT(J)    = 0
         HM(J)       = 0.
         XM(J)       = 0.
         WICE0(J)    = MICE(J)
         WLIQ0(J)    = MLIQ(J)
         WMASS0(J)   = MICE(J) + MLIQ(J)
    ENDDO

    if(ist == 1) then
      DO J = 1,NSOIL
         IF (OPT_FRZ == 1) THEN
            IF(STC(J) < TFRZ) THEN
               SMP = HFUS*(TFRZ-STC(J))/(GRAV*STC(J))             
               SUPERCOOL(J) = SMCMAX*(SMP/PSISAT)**(-1./BEXP)
               SUPERCOOL(J) = SUPERCOOL(J)*DZSNSO(J)*1000.        
            END IF
         END IF
         IF (OPT_FRZ == 2) THEN
               CALL FRH2O (SUPERCOOL(J),STC(J),SMC(J),SH2O(J))
               SUPERCOOL(J) = SUPERCOOL(J)*DZSNSO(J)*1000.        
         END IF
      ENDDO
    end if

    DO J = ISNOW+1,NSOIL
         IF (MICE(J) > 0. .AND. STC(J) >= TFRZ) THEN  
             IMELT(J) = 1
         ENDIF
         IF (MLIQ(J) > SUPERCOOL(J) .AND. STC(J) < TFRZ) THEN
             IMELT(J) = 2
         ENDIF

         
         IF (ISNOW == 0 .AND. SNEQV > 0. .AND. J == 1) THEN
             IF (STC(J) >= TFRZ) THEN
                IMELT(J) = 1
             ENDIF
         ENDIF
    ENDDO



    DO J = ISNOW+1,NSOIL
         IF (IMELT(J) > 0) THEN
             HM(J) = (STC(J)-TFRZ)/FACT(J)
             STC(J) = TFRZ
         ENDIF

         IF (IMELT(J) == 1 .AND. HM(J) < 0.) THEN
            HM(J) = 0.
            IMELT(J) = 0
         ENDIF
         IF (IMELT(J) == 2 .AND. HM(J) > 0.) THEN
            HM(J) = 0.
            IMELT(J) = 0
         ENDIF
         XM(J) = HM(J)*DT/HFUS                           
    ENDDO



    IF (ISNOW == 0 .AND. SNEQV > 0. .AND. XM(1) > 0.) THEN  
        TEMP1  = SNEQV
        SNEQV  = MAX(0.,TEMP1-XM(1))  
        PROPOR = SNEQV/TEMP1
        SNOWH  = MAX(0.,PROPOR * SNOWH)
        HEATR  = HM(1) - HFUS*(TEMP1-SNEQV)/DT  
        IF (HEATR > 0.) THEN
              XM(1) = HEATR*DT/HFUS             
              HM(1) = HEATR                    
        ELSE
              XM(1) = 0.
              HM(1) = 0.
        ENDIF
        QMELT   = MAX(0.,(TEMP1-SNEQV))/DT
        XMF     = HFUS*QMELT
        PONDING = TEMP1-SNEQV
    ENDIF



    DO J = ISNOW+1,NSOIL
      IF (IMELT(J) > 0 .AND. ABS(HM(J)) > 0.) THEN

         HEATR = 0.
         IF (XM(J) > 0.) THEN                            
            MICE(J) = MAX(0., WICE0(J)-XM(J))
            HEATR = HM(J) - HFUS*(WICE0(J)-MICE(J))/DT
         ELSE IF (XM(J) < 0.) THEN                      
            IF (J <= 0) THEN                             
               MICE(J) = MIN(WMASS0(J), WICE0(J)-XM(J))  
            ELSE                                         
               IF (WMASS0(J) < SUPERCOOL(J)) THEN
                  MICE(J) = 0.
               ELSE
                  MICE(J) = MIN(WMASS0(J) - SUPERCOOL(J),WICE0(J)-XM(J))
                  MICE(J) = MAX(MICE(J),0.0)
               ENDIF
            ENDIF
            HEATR = HM(J) - HFUS*(WICE0(J)-MICE(J))/DT
         ENDIF

         MLIQ(J) = MAX(0.,WMASS0(J)-MICE(J))

         IF (ABS(HEATR) > 0.) THEN
            STC(J) = STC(J) + FACT(J)*HEATR
            IF (J <= 0) THEN                             
               IF (MLIQ(J)*MICE(J)>0.) STC(J) = TFRZ
            END IF
         ENDIF

         XMF = XMF + HFUS * (WICE0(J)-MICE(J))/DT

         IF (J < 1) THEN
            QMELT = QMELT + MAX(0.,(WICE0(J)-MICE(J)))/DT
         ENDIF
      ENDIF
    ENDDO

    DO J = ISNOW+1,0             
       SNLIQ(J) = MLIQ(J)
       SNICE(J) = MICE(J)
    END DO

    DO J = 1, NSOIL              
       SH2O(J) =  MLIQ(J)            / (1000. * DZSNSO(J))
       SMC(J)  = (MLIQ(J) + MICE(J)) / (1000. * DZSNSO(J))
    END DO
   
  END SUBROUTINE PHASECHANGE

  SUBROUTINE FRH2O (FREE,TKELV,SMC,SH2O)




























    IMPLICIT NONE
    REAL, INTENT(IN)     :: SH2O,SMC,TKELV
    REAL, INTENT(OUT)    :: FREE
    REAL                 :: BX,DENOM,DF,DSWL,FK,SWL,SWLK
    INTEGER              :: NLOG,KCOUNT

    REAL, PARAMETER      :: CK = 8.0, BLIM = 5.5, ERROR = 0.005,       &
         DICE = 920.0
    CHARACTER(LEN=80)    :: message






    BX = BEXP




    IF (BEXP >  BLIM) BX = BLIM
    NLOG = 0




    KCOUNT = 0
    IF (TKELV > (TFRZ- 1.E-3)) THEN
       FREE = SMC
    ELSE






       IF (CK /= 0.0) THEN
          SWL = SMC - SH2O



          IF (SWL > (SMC -0.02)) SWL = SMC -0.02



          IF (SWL < 0.) SWL = 0.
1001      Continue
          IF (.NOT.( (NLOG < 10) .AND. (KCOUNT == 0)))   goto 1002
          NLOG = NLOG +1
          DF = ALOG ( ( PSISAT * GRAV / hfus ) * ( ( 1. + CK * SWL )**2.) * &
               ( SMCMAX / (SMC - SWL) )** BX) - ALOG ( - (               &
               TKELV - TFRZ)/ TKELV)
          DENOM = 2. * CK / ( 1. + CK * SWL ) + BX / ( SMC - SWL )
          SWLK = SWL - DF / DENOM



          IF (SWLK > (SMC -0.02)) SWLK = SMC - 0.02
          IF (SWLK < 0.) SWLK = 0.




          DSWL = ABS (SWLK - SWL)



          SWL = SWLK
          IF ( DSWL <= ERROR ) THEN
             KCOUNT = KCOUNT +1
          END IF





          goto 1001
1002      continue
          FREE = SMC - SWL
       END IF








       IF (KCOUNT == 0) THEN
          write(message, '("Flerchinger used in NEW version. Iterations=", I6)') NLOG
          call wrf_message(trim(message))
          FK = ( ( (hfus / (GRAV * ( - PSISAT)))*                    &
               ( (TKELV - TFRZ)/ TKELV))** ( -1/ BX))* SMCMAX
          IF (FK < 0.02) FK = 0.02
          FREE = MIN (FK, SMC)



       END IF
    END IF

  END SUBROUTINE FRH2O




  SUBROUTINE WATER (VEGTYP ,NSNOW  ,NSOIL  ,IMELT  ,DT     ,UU     , & 
                    VV     ,FCEV   ,FCTR   ,QPRECC ,QPRECL ,ELAI   , & 
                    ESAI   ,SFCTMP ,QVAP   ,QDEW   ,ZSOIL  ,BTRANI , & 
                    FICEOLD,PONDING,TG     ,IST    ,FVEG   ,ILOC   ,JLOC ,SMCEQ , & 
		    LATHEAV , LATHEAG , frozen_canopy,frozen_ground,                       & 
                    ISNOW  ,CANLIQ ,CANICE ,TV     ,SNOWH  ,SNEQV  , & 
                    SNICE  ,SNLIQ  ,STC    ,ZSNSO  ,SH2O   ,SMC    , & 
                    SICE   ,ZWT    ,WA     ,WT     ,DZSNSO ,WSLAKE , & 
                    SMCWTD ,DEEPRECH,RECH                          , & 
                    CMC    ,ECAN   ,ETRAN  ,FWET   ,RUNSRF ,RUNSUB , & 
                    QIN    ,QDIS   ,QSNOW  ,PONDING1       ,PONDING2,&
                    ISURBAN,QSNBOT,FPICE                             &



                    )  




  implicit none


  INTEGER,                         INTENT(IN)    :: ILOC    
  INTEGER,                         INTENT(IN)    :: JLOC    
  INTEGER,                         INTENT(IN)    :: VEGTYP  
  INTEGER,                         INTENT(IN)    :: NSNOW   
  INTEGER                        , INTENT(IN)    :: IST     
  INTEGER,                         INTENT(IN)    :: NSOIL   
  INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT   
  REAL,                            INTENT(IN)    :: DT      
  REAL,                            INTENT(IN)    :: UU      
  REAL,                            INTENT(IN)    :: VV      
  REAL,                            INTENT(IN)    :: FCEV    
  REAL,                            INTENT(IN)    :: FCTR    
  REAL,                            INTENT(IN)    :: QPRECC  
  REAL,                            INTENT(IN)    :: QPRECL  
  REAL,                            INTENT(IN)    :: ELAI    
  REAL,                            INTENT(IN)    :: ESAI    
  REAL,                            INTENT(IN)    :: SFCTMP  
  REAL,                            INTENT(IN)    :: QVAP    
  REAL,                            INTENT(IN)    :: QDEW    
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL   
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: BTRANI  
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: FICEOLD 

  REAL                           , INTENT(IN)    :: TG      
  REAL                           , INTENT(IN)    :: FVEG    
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: SMCEQ   


  INTEGER,                         INTENT(INOUT) :: ISNOW   
  REAL,                            INTENT(INOUT) :: CANLIQ  
  REAL,                            INTENT(INOUT) :: CANICE  
  REAL,                            INTENT(INOUT) :: TV      
  REAL,                            INTENT(INOUT) :: SNOWH   
  REAL,                            INTENT(INOUT) :: SNEQV   
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE   
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ   
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC     
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO   
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO  
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O    
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE    
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SMC     
  REAL,                            INTENT(INOUT) :: ZWT     
  REAL,                            INTENT(INOUT) :: WA      
  REAL,                            INTENT(INOUT) :: WT      
                                                            
  REAL,                            INTENT(INOUT) :: WSLAKE  
  REAL                           , INTENT(INOUT) :: PONDING 
  REAL,                            INTENT(INOUT) :: SMCWTD 
  REAL,                            INTENT(INOUT) :: DEEPRECH 
  REAL,                            INTENT(INOUT) :: RECH 


  REAL,                            INTENT(OUT)   :: CMC     
  REAL,                            INTENT(OUT)   :: ECAN    
  REAL,                            INTENT(OUT)   :: ETRAN   
  REAL,                            INTENT(OUT)   :: FWET    
  REAL,                            INTENT(OUT)   :: RUNSRF  
  REAL,                            INTENT(OUT)   :: RUNSUB  
  REAL,                            INTENT(OUT)   :: QIN     
  REAL,                            INTENT(OUT)   :: QDIS    
  REAL,                            INTENT(OUT)   :: QSNOW   
  REAL,                            INTENT(OUT)   :: PONDING1
  REAL,                            INTENT(OUT)   :: PONDING2
  REAL,                            INTENT(OUT)   :: QSNBOT  
  REAL,                            INTENT(OUT)   :: FPICE   
  REAL                              , INTENT(IN)   :: LATHEAV 
  REAL                              , INTENT(IN)   :: LATHEAG 
  LOGICAL                           , INTENT(IN)   :: FROZEN_GROUND 
  LOGICAL                           , INTENT(IN)   :: FROZEN_CANOPY 

  INTEGER,                         INTENT(IN)    :: ISURBAN


  INTEGER                                        :: IZ
  REAL                                           :: QINSUR  
  REAL                                           :: QRAIN   
  REAL                                           :: QSEVA   
  REAL                                           :: QSDEW   
  REAL                                           :: QSNFRO  
  REAL                                           :: QSNSUB  
  REAL                                           :: SNOWHIN 
  REAL, DIMENSION(       1:NSOIL)                :: ETRANI  
  REAL, DIMENSION(       1:NSOIL)                :: WCND   
  REAL                                           :: QDRAIN  
  REAL                                           :: SNOFLOW 
  REAL                                           :: FCRMAX 

  REAL, PARAMETER ::  WSLMAX = 5000.      








   ETRANI(1:NSOIL) = 0.
   SNOFLOW         = 0.
   RUNSUB          = 0.
   QINSUR          = 0.



   CALL CANWATER (VEGTYP ,DT     ,SFCTMP ,UU     ,VV     , & 
                  FCEV   ,FCTR   ,QPRECC ,QPRECL ,ELAI   , & 
                  ESAI   ,IST    ,TG     ,FVEG   ,ILOC   , JLOC, & 
                  FROZEN_CANOPY,                                 & 
                  CANLIQ ,CANICE ,TV     ,                 & 
                  CMC    ,ECAN   ,ETRAN  ,QRAIN  ,QSNOW  , & 
                  SNOWHIN,FWET   ,FPICE   )                           



     QSNSUB = 0.
     IF (SNEQV > 0.) THEN
       QSNSUB = MIN(QVAP, SNEQV/DT)
     ENDIF
     QSEVA = QVAP-QSNSUB

     QSNFRO = 0.
     IF (SNEQV > 0.) THEN
        QSNFRO = QDEW
     ENDIF
     QSDEW = QDEW - QSNFRO

     CALL SNOWWATER (NSNOW  ,NSOIL  ,IMELT  ,DT     ,ZSOIL  , & 
          &          SFCTMP ,SNOWHIN,QSNOW  ,QSNFRO ,QSNSUB , & 
          &          QRAIN  ,FICEOLD,ILOC   ,JLOC   ,         & 
          &          ISNOW  ,SNOWH  ,SNEQV  ,SNICE  ,SNLIQ  , & 
          &          SH2O   ,SICE   ,STC    ,ZSNSO  ,DZSNSO , & 
          &          QSNBOT ,SNOFLOW,PONDING1       ,PONDING2)  

   IF(FROZEN_GROUND) THEN
      SICE(1) =  SICE(1) + (QSDEW-QSEVA)*DT/(DZSNSO(1)*1000.)
      QSDEW = 0.0
      QSEVA = 0.0
      IF(SICE(1) < 0.) THEN
         SH2O(1) = SH2O(1) + SICE(1)
         SICE(1) = 0.
      END IF
   END IF



    
    QINSUR = (PONDING+PONDING1+PONDING2)/DT * 0.001


    IF(ISNOW == 0) THEN
       QINSUR = QINSUR+(QSNBOT + QSDEW + QRAIN) * 0.001
    ELSE
       QINSUR = QINSUR+(QSNBOT + QSDEW) * 0.001
    ENDIF

    QSEVA  = QSEVA * 0.001 

    DO IZ = 1, NROOT
       ETRANI(IZ) = ETRAN * BTRANI(IZ) * 0.001
    ENDDO







    IF (IST == 2) THEN                                        
       RUNSRF = 0.
       IF(WSLAKE >= WSLMAX) RUNSRF = QINSUR*1000.             
       WSLAKE = WSLAKE + (QINSUR-QSEVA)*1000.*DT -RUNSRF*DT   
    ELSE                                                      
       CALL      SOILWATER (NSOIL  ,NSNOW  ,DT     ,ZSOIL  ,DZSNSO , & 
                            QINSUR ,QSEVA  ,ETRANI ,SICE   ,ILOC   , JLOC , & 
                            SH2O   ,SMC    ,ZWT    ,VEGTYP ,ISURBAN, & 
                           SMCWTD, DEEPRECH                       , & 
                            RUNSRF ,QDRAIN ,RUNSUB ,WCND   ,FCRMAX )   
 
       IF(OPT_RUN == 1) THEN 
          CALL GROUNDWATER (NSNOW  ,NSOIL  ,DT     ,SICE   ,ZSOIL  , & 
                            STC    ,WCND   ,FCRMAX ,ILOC   ,JLOC   , & 
                            SH2O   ,ZWT    ,WA     ,WT     ,         & 
                            QIN    ,QDIS   )                           
          RUNSUB       = QDIS          
       END IF

       IF(OPT_RUN == 3 .or. OPT_RUN == 4) THEN 
          RUNSUB       = RUNSUB + QDRAIN        
       END IF

       DO IZ = 1,NSOIL
           SMC(IZ) = SH2O(IZ) + SICE(IZ)
       ENDDO
 
       IF(OPT_RUN == 5) THEN
          CALL SHALLOWWATERTABLE (NSNOW  ,NSOIL, ZSOIL, DT       , & 
                         DZSNSO ,SMCEQ   ,ILOC , JLOC        , & 
                         SMC    ,ZWT    ,SMCWTD ,RECH, QDRAIN  ) 

          SH2O(NSOIL) = SMC(NSOIL) - SICE(NSOIL)
          RUNSUB = RUNSUB + QDRAIN 
          WA = 0.
       ENDIF

    ENDIF

    RUNSUB       = RUNSUB + SNOFLOW         

  END SUBROUTINE WATER

  SUBROUTINE CANWATER (VEGTYP ,DT     ,SFCTMP ,UU     ,VV     , & 
                       FCEV   ,FCTR   ,QPRECC ,QPRECL ,ELAI   , & 
                       ESAI   ,IST    ,TG     ,FVEG   ,ILOC   , JLOC , & 
                       FROZEN_CANOPY,                                 & 
                       CANLIQ ,CANICE ,TV     ,                 & 
                       CMC    ,ECAN   ,ETRAN  ,QRAIN  ,QSNOW  , & 
                       SNOWHIN,FWET   ,FPICE   )                           




  USE NOAHMP_VEG_PARAMETERS

  IMPLICIT NONE


  INTEGER,INTENT(IN)  :: ILOC    
  INTEGER,INTENT(IN)  :: JLOC    
  INTEGER,INTENT(IN)  :: VEGTYP  
  REAL,   INTENT(IN)  :: DT      
  REAL,   INTENT(IN)  :: SFCTMP  
  REAL,   INTENT(IN)  :: UU      
  REAL,   INTENT(IN)  :: VV      
  REAL,   INTENT(IN)  :: FCEV    
  REAL,   INTENT(IN)  :: FCTR    
  REAL,   INTENT(IN)  :: QPRECC  
  REAL,   INTENT(IN)  :: QPRECL  
  REAL,   INTENT(IN)  :: ELAI    
  REAL,   INTENT(IN)  :: ESAI    
  INTEGER,INTENT(IN)  :: IST     
  REAL,   INTENT(IN)  :: TG      
  REAL,   INTENT(IN)  :: FVEG    
  LOGICAL                           , INTENT(IN)   :: FROZEN_CANOPY 


  REAL, INTENT(INOUT) :: CANLIQ  
  REAL, INTENT(INOUT) :: CANICE  
  REAL, INTENT(INOUT) :: TV      


  REAL, INTENT(OUT)   :: CMC     
  REAL, INTENT(OUT)   :: ECAN    
  REAL, INTENT(OUT)   :: ETRAN   
  REAL, INTENT(OUT)   :: QRAIN   
  REAL, INTENT(OUT)   :: QSNOW   
  REAL, INTENT(OUT)   :: SNOWHIN 
  REAL, INTENT(OUT)   :: FWET    
  REAL, INTENT(OUT)   :: FPICE   



  REAL                :: MAXSNO  
  REAL                :: MAXLIQ  
  REAL                :: FP      
  REAL                :: BDFALL  
  REAL                :: QINTR   
  REAL                :: QDRIPR  
  REAL                :: QTHROR  
  REAL                :: QINTS   
  REAL                :: QDRIPS  
  REAL                :: QTHROS  
  REAL                :: QEVAC   
  REAL                :: QDEWC   
  REAL                :: QFROC   
  REAL                :: QSUBC   
  REAL                :: FT      
  REAL                :: FV      
  REAL                :: QMELTC  
  REAL                :: QFRZC   
  REAL                :: RAIN    
  REAL                :: SNOW    
  REAL                :: CANMAS  



      FP      = 0.0
      RAIN    = 0.0
      SNOW    = 0.0 
      QINTR   = 0.
      QDRIPR  = 0.
      QTHROR  = 0.
      QINTR   = 0.
      QINTS   = 0.
      QDRIPS  = 0.0
      QTHROS  = 0.
      QRAIN   = 0.0
      QSNOW   = 0.0
      SNOWHIN = 0.0
      ECAN    = 0.0





     IF(OPT_SNF == 1) THEN
       IF(SFCTMP > TFRZ+2.5)THEN
           FPICE = 0.
       ELSE
         IF(SFCTMP <= TFRZ+0.5)THEN
           FPICE = 1.0
         ELSE IF(SFCTMP <= TFRZ+2.)THEN
           FPICE = 1.-(-54.632 + 0.2*SFCTMP)
         ELSE
           FPICE = 0.6
         ENDIF
       ENDIF
     ENDIF

     IF(OPT_SNF == 2) THEN
       IF(SFCTMP >= TFRZ+2.2) THEN
           FPICE = 0.
       ELSE
           FPICE = 1.0
       ENDIF
     ENDIF

     IF(OPT_SNF == 3) THEN
       IF(SFCTMP >= TFRZ) THEN
           FPICE = 0.
       ELSE
           FPICE = 1.0
       ENDIF
     ENDIF




     BDFALL = MIN(120.,67.92+51.25*EXP((SFCTMP-TFRZ)/2.59))   

     RAIN   = (QPRECC + QPRECL) * (1.-FPICE)
     SNOW   = (QPRECC + QPRECL) * FPICE


   
    IF(QPRECC + QPRECL > 0.) & 
       FP = (QPRECC + QPRECL) / (10.*QPRECC + QPRECL)




      MAXLIQ =  CH2OP(VEGTYP) * (ELAI+ ESAI)



      IF((ELAI+ ESAI).GT.0.) THEN
         QINTR  = FVEG * RAIN * FP  
         QINTR  = MIN(QINTR, (MAXLIQ - CANLIQ)/DT * (1.-EXP(-RAIN*DT/MAXLIQ)) )
         QINTR  = MAX(QINTR, 0.)
         QDRIPR = FVEG * RAIN - QINTR
         QTHROR = (1.-FVEG) * RAIN
      ELSE
         QINTR  = 0.
         QDRIPR = 0.
         QTHROR = RAIN
      END IF



      IF (.NOT.FROZEN_CANOPY) THEN             
        ETRAN = MAX( FCTR/HVAP, 0. )
        QEVAC = MAX( FCEV/HVAP, 0. )
        QDEWC = ABS( MIN( FCEV/HVAP, 0. ) )
        QSUBC = 0.
        QFROC = 0.
      ELSE
        ETRAN = MAX( FCTR/HSUB, 0. )
        QEVAC = 0.
        QDEWC = 0.
        QSUBC = MAX( FCEV/HSUB, 0. )
        QFROC = ABS( MIN( FCEV/HSUB, 0. ) )
      ENDIF




       QEVAC = MIN(CANLIQ/DT,QEVAC)
       CANLIQ=MAX(0.,CANLIQ+(QINTR+QDEWC-QEVAC)*DT)
       IF(CANLIQ <= 1.E-06) CANLIQ = 0.0




      MAXSNO = 6.6*(0.27+46./BDFALL) * (ELAI+ ESAI)

      IF((ELAI+ ESAI).GT.0.) THEN
         QINTS = FVEG * SNOW * FP
         QINTS = MIN(QINTS, (MAXSNO - CANICE)/DT * (1.-EXP(-SNOW*DT/MAXSNO)) )
         QINTS = MAX(QINTS, 0.)
         FT = MAX(0.0,(TV - 270.15) / 1.87E5)
         FV = SQRT(UU*UU + VV*VV) / 1.56E5
         QDRIPS = MAX(0.,CANICE) * (FV+FT)
         QTHROS = (1.0-FVEG) * SNOW + (FVEG * SNOW - QINTS)
      ELSE
         QINTS  = 0.
         QDRIPS = 0.
         QTHROS = SNOW
      ENDIF

      QSUBC = MIN(CANICE/DT,QSUBC) 
      CANICE= MAX(0.,CANICE+(QINTS-QDRIPS)*DT + (QFROC-QSUBC)*DT)
      IF(CANICE.LE.1.E-6) CANICE = 0.
     


      IF(CANICE.GT.0.) THEN
           FWET = MAX(0.,CANICE) / MAX(MAXSNO,1.E-06)
      ELSE
           FWET = MAX(0.,CANLIQ) / MAX(MAXLIQ,1.E-06)
      ENDIF
      FWET = MIN(FWET, 1.) ** 0.667



      QMELTC = 0.
      QFRZC = 0.

      IF(CANICE.GT.1.E-6.AND.TV.GT.TFRZ) THEN
         QMELTC = MIN(CANICE/DT,(TV-TFRZ)*CICE*CANICE/DENICE/(DT*HFUS))
         CANICE = MAX(0.,CANICE - QMELTC*DT)
         CANLIQ = MAX(0.,CANLIQ + QMELTC*DT)
         TV     = FWET*TFRZ + (1.-FWET)*TV
      ENDIF

      IF(CANLIQ.GT.1.E-6.AND.TV.LT.TFRZ) THEN
         QFRZC  = MIN(CANLIQ/DT,(TFRZ-TV)*CWAT*CANLIQ/DENH2O/(DT*HFUS))
         CANLIQ = MAX(0.,CANLIQ - QFRZC*DT)
         CANICE = MAX(0.,CANICE + QFRZC*DT)
         TV     = FWET*TFRZ + (1.-FWET)*TV
      ENDIF



      CMC = CANLIQ + CANICE



      ECAN = QEVAC + QSUBC - QDEWC - QFROC



      QRAIN   = QDRIPR + QTHROR
      QSNOW   = QDRIPS + QTHROS
      SNOWHIN = QSNOW/BDFALL


      IF (IST == 2 .AND. TG > TFRZ) THEN
         QSNOW   = 0.
         SNOWHIN = 0.
      END IF

  END SUBROUTINE CANWATER


  SUBROUTINE SNOWWATER (NSNOW  ,NSOIL  ,IMELT  ,DT     ,ZSOIL  , & 
                        SFCTMP ,SNOWHIN,QSNOW  ,QSNFRO ,QSNSUB , & 
                        QRAIN  ,FICEOLD,ILOC   ,JLOC   ,         & 
                        ISNOW  ,SNOWH  ,SNEQV  ,SNICE  ,SNLIQ  , & 
                        SH2O   ,SICE   ,STC    ,ZSNSO  ,DZSNSO , & 
                        QSNBOT ,SNOFLOW,PONDING1       ,PONDING2)  

  IMPLICIT NONE


  INTEGER,                         INTENT(IN)    :: ILOC   
  INTEGER,                         INTENT(IN)    :: JLOC   
  INTEGER,                         INTENT(IN)    :: NSNOW  
  INTEGER,                         INTENT(IN)    :: NSOIL  
  INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT  
  REAL,                            INTENT(IN)    :: DT     
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  
  REAL,                            INTENT(IN)    :: SFCTMP 
  REAL,                            INTENT(IN)    :: SNOWHIN
  REAL,                            INTENT(IN)    :: QSNOW  
  REAL,                            INTENT(IN)    :: QSNFRO 
  REAL,                            INTENT(IN)    :: QSNSUB 
  REAL,                            INTENT(IN)    :: QRAIN  
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(IN)    :: FICEOLD


  INTEGER,                         INTENT(INOUT) :: ISNOW  
  REAL,                            INTENT(INOUT) :: SNOWH  
  REAL,                            INTENT(INOUT) :: SNEQV  
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE   
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO  
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO 


  REAL,                              INTENT(OUT) :: QSNBOT 
  REAL,                              INTENT(OUT) :: SNOFLOW
  REAL,                              INTENT(OUT) :: PONDING1
  REAL,                              INTENT(OUT) :: PONDING2


  INTEGER :: IZ,i
  REAL    :: BDSNOW  

   SNOFLOW = 0.0
   PONDING1 = 0.0
   PONDING2 = 0.0

   CALL SNOWFALL (NSOIL  ,NSNOW  ,DT     ,QSNOW  ,SNOWHIN, & 
                  SFCTMP ,ILOC   ,JLOC   ,                 & 
                  ISNOW  ,SNOWH  ,DZSNSO ,STC    ,SNICE  , & 
                  SNLIQ  ,SNEQV  )                           



   IF(ISNOW < 0) &        
   CALL  COMPACT (NSNOW  ,NSOIL  ,DT     ,STC    ,SNICE  , & 
                  SNLIQ  ,ZSOIL  ,IMELT  ,FICEOLD,ILOC   , JLOC ,& 
                  ISNOW  ,DZSNSO ,ZSNSO  )                   

   IF(ISNOW < 0) &        
   CALL  COMBINE (NSNOW  ,NSOIL  ,ILOC   ,JLOC   ,         & 
                  ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & 
                  DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,         & 
                  PONDING1       ,PONDING2)                  

   IF(ISNOW < 0) &        
   CALL   DIVIDE (NSNOW  ,NSOIL  ,                         & 
                  ISNOW  ,STC    ,SNICE  ,SNLIQ  ,DZSNSO )   

   CALL  SNOWH2O (NSNOW  ,NSOIL  ,DT     ,QSNFRO ,QSNSUB , & 
                  QRAIN  ,ILOC   ,JLOC   ,                 & 
                  ISNOW  ,DZSNSO ,SNOWH  ,SNEQV  ,SNICE  , & 
                  SNLIQ  ,SH2O   ,SICE   ,STC    ,         & 
                  QSNBOT ,PONDING1       ,PONDING2)           



   do iz = -nsnow+1, isnow
        snice(iz) = 0.
        snliq(iz) = 0.
        stc(iz)   = 0.
        dzsnso(iz)= 0.
        zsnso(iz) = 0.
   enddo


       
   IF(SNEQV > 2000.) THEN   
      BDSNOW      = SNICE(0) / DZSNSO(0)
      SNOFLOW     = (SNEQV - 2000.)
      SNICE(0)    = SNICE(0)  - SNOFLOW 
      DZSNSO(0)   = DZSNSO(0) - SNOFLOW/BDSNOW
      SNOFLOW     = SNOFLOW / DT
   END IF



   IF(ISNOW < 0) THEN  
       SNEQV = 0.
       DO IZ = ISNOW+1,0
             SNEQV = SNEQV + SNICE(IZ) + SNLIQ(IZ)
       ENDDO
   END IF



   DO IZ = ISNOW+1, 0
        DZSNSO(IZ) = -DZSNSO(IZ)
   END DO

   DZSNSO(1) = ZSOIL(1)
   DO IZ = 2,NSOIL
        DZSNSO(IZ) = (ZSOIL(IZ) - ZSOIL(IZ-1))
   END DO

   ZSNSO(ISNOW+1) = DZSNSO(ISNOW+1)
   DO IZ = ISNOW+2 ,NSOIL
       ZSNSO(IZ) = ZSNSO(IZ-1) + DZSNSO(IZ)
   ENDDO

   DO IZ = ISNOW+1 ,NSOIL
       DZSNSO(IZ) = -DZSNSO(IZ)
   END DO

  END SUBROUTINE SNOWWATER

  SUBROUTINE SNOWFALL (NSOIL  ,NSNOW  ,DT     ,QSNOW  ,SNOWHIN , & 
                       SFCTMP ,ILOC   ,JLOC   ,                  & 
                       ISNOW  ,SNOWH  ,DZSNSO ,STC    ,SNICE   , & 
                       SNLIQ  ,SNEQV  )                            




    IMPLICIT NONE



  INTEGER,                            INTENT(IN) :: ILOC   
  INTEGER,                            INTENT(IN) :: JLOC   
  INTEGER,                            INTENT(IN) :: NSOIL  
  INTEGER,                            INTENT(IN) :: NSNOW  
  REAL,                               INTENT(IN) :: DT     
  REAL,                               INTENT(IN) :: QSNOW  
  REAL,                               INTENT(IN) :: SNOWHIN
  REAL,                               INTENT(IN) :: SFCTMP 



  INTEGER,                         INTENT(INOUT) :: ISNOW  
  REAL,                            INTENT(INOUT) :: SNOWH  
  REAL,                            INTENT(INOUT) :: SNEQV  
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO 
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  



  INTEGER :: NEWNODE            

    NEWNODE  = 0



    IF(ISNOW == 0 .and. QSNOW > 0.)  THEN
      SNOWH = SNOWH + SNOWHIN * DT
      SNEQV = SNEQV + QSNOW * DT
    END IF


 
    IF(ISNOW == 0  .AND. QSNOW>0. .AND. SNOWH >= 0.025) THEN 

      ISNOW    = -1
      NEWNODE  =  1
      DZSNSO(0)= SNOWH
      SNOWH    = 0.
      STC(0)   = MIN(273.16, SFCTMP)   
      SNICE(0) = SNEQV
      SNLIQ(0) = 0.
    END IF



    IF(ISNOW <  0 .AND. NEWNODE == 0 .AND. QSNOW > 0.) then
         SNICE(ISNOW+1)  = SNICE(ISNOW+1)   + QSNOW   * DT
         DZSNSO(ISNOW+1) = DZSNSO(ISNOW+1)  + SNOWHIN * DT
    ENDIF


  END SUBROUTINE SNOWFALL

  SUBROUTINE COMBINE (NSNOW  ,NSOIL  ,ILOC   ,JLOC   ,         & 
                      ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & 
                      DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,         & 
                      PONDING1       ,PONDING2)                  

    IMPLICIT NONE



    INTEGER, INTENT(IN)     :: ILOC
    INTEGER, INTENT(IN)     :: JLOC
    INTEGER, INTENT(IN)     :: NSNOW                        
    INTEGER, INTENT(IN)     :: NSOIL                        



    INTEGER,                         INTENT(INOUT) :: ISNOW 
    REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O  
    REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE  
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC   
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE 
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ 
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO
    REAL,                            INTENT(INOUT) :: sneqv 
    REAL,                            INTENT(INOUT) :: snowh 
    REAL,                            INTENT(OUT) :: PONDING1
    REAL,                            INTENT(OUT) :: PONDING2



    INTEGER :: I,J,K,L               
    INTEGER :: ISNOW_OLD             
    INTEGER :: MSSI                  
    INTEGER :: NEIBOR                
    REAL    :: ZWICE                 
    REAL    :: ZWLIQ                 

    REAL    :: DZMIN(3)              

    DATA DZMIN /0.025, 0.025, 0.1/  


       ISNOW_OLD = ISNOW

       DO J = ISNOW_OLD+1,0
          IF (SNICE(J) <= .1) THEN
             IF(J /= 0) THEN
                SNLIQ(J+1) = SNLIQ(J+1) + SNLIQ(J)
                SNICE(J+1) = SNICE(J+1) + SNICE(J)
             ELSE
               IF (ISNOW_OLD < -1) THEN    
                SNLIQ(J-1) = SNLIQ(J-1) + SNLIQ(J)
                SNICE(J-1) = SNICE(J-1) + SNICE(J)
               ELSE
	         IF(SNICE(J) >= 0.) THEN
                  PONDING1 = SNLIQ(J)    
                  SNEQV = SNICE(J)       
                  SNOWH = DZSNSO(J)      
		 ELSE   
		  PONDING1 = SNLIQ(J) + SNICE(J)
		  IF(PONDING1 < 0.) THEN  
		   SICE(1) = MAX(0.0,SICE(1)+PONDING1/(DZSNSO(1)*1000.))
                   PONDING1 = 0.0
		  END IF
                  SNEQV = 0.0
                  SNOWH = 0.0
		 END IF
                 SNLIQ(J) = 0.0
                 SNICE(J) = 0.0
                 DZSNSO(J) = 0.0
               ENDIF


             ENDIF

             
             IF (J > ISNOW+1 .AND. ISNOW < -1) THEN
                DO I = J, ISNOW+2, -1
                   STC(I)   = STC(I-1)
                   SNLIQ(I) = SNLIQ(I-1)
                   SNICE(I) = SNICE(I-1)
                   DZSNSO(I)= DZSNSO(I-1)
                END DO
             END IF
             ISNOW = ISNOW + 1
          END IF
       END DO



       IF(SICE(1) < 0.) THEN
          SH2O(1) = SH2O(1) + SICE(1)
          SICE(1) = 0.
       END IF

       IF(ISNOW ==0) RETURN   

       SNEQV  = 0.
       SNOWH  = 0.
       ZWICE  = 0.
       ZWLIQ  = 0.

       DO J = ISNOW+1,0
             SNEQV = SNEQV + SNICE(J) + SNLIQ(J)
             SNOWH = SNOWH + DZSNSO(J)
             ZWICE = ZWICE + SNICE(J)
             ZWLIQ = ZWLIQ + SNLIQ(J)
       END DO




       IF (SNOWH < 0.025 .AND. ISNOW < 0 ) THEN 

          ISNOW  = 0
          SNEQV = ZWICE
          PONDING2 = ZWLIQ           
          IF(SNEQV <= 0.) SNOWH = 0. 
       END IF










       IF (ISNOW < -1) THEN

          ISNOW_OLD = ISNOW
          MSSI     = 1

          DO I = ISNOW_OLD+1,0
             IF (DZSNSO(I) < DZMIN(MSSI)) THEN

                IF (I == ISNOW+1) THEN
                   NEIBOR = I + 1
                ELSE IF (I == 0) THEN
                   NEIBOR = I - 1
                ELSE
                   NEIBOR = I + 1
                   IF ((DZSNSO(I-1)+DZSNSO(I)) < (DZSNSO(I+1)+DZSNSO(I))) NEIBOR = I-1
                END IF

                
                IF (NEIBOR > I) THEN
                   J = NEIBOR
                   L = I
                ELSE
                   J = I
                   L = NEIBOR
                END IF

                CALL COMBO (DZSNSO(J), SNLIQ(J), SNICE(J), &
                   STC(J), DZSNSO(L), SNLIQ(L), SNICE(L), STC(L) )

                
                IF (J-1 > ISNOW+1) THEN
                   DO K = J-1, ISNOW+2, -1
                      STC(K)   = STC(K-1)
                      SNICE(K) = SNICE(K-1)
                      SNLIQ(K) = SNLIQ(K-1)
                      DZSNSO(K) = DZSNSO(K-1)
                   END DO
                END IF

                
                ISNOW = ISNOW + 1
                IF (ISNOW >= -1) EXIT
             ELSE

                
                MSSI = MSSI + 1

             END IF
          END DO

       END IF

  END SUBROUTINE COMBINE

  SUBROUTINE DIVIDE (NSNOW  ,NSOIL  ,                         & 
                     ISNOW  ,STC    ,SNICE  ,SNLIQ  ,DZSNSO  )  

    IMPLICIT NONE



    INTEGER, INTENT(IN)                            :: NSNOW 
    INTEGER, INTENT(IN)                            :: NSOIL 



    INTEGER                        , INTENT(INOUT) :: ISNOW 
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC   
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE 
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ 
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO



    INTEGER                                        :: J     
    INTEGER                                        :: MSNO  
    REAL                                           :: DRR   
    REAL, DIMENSION(       1:NSNOW)                :: DZ    
    REAL, DIMENSION(       1:NSNOW)                :: SWICE 
    REAL, DIMENSION(       1:NSNOW)                :: SWLIQ 
    REAL, DIMENSION(       1:NSNOW)                :: TSNO  
    REAL                                           :: ZWICE 
    REAL                                           :: ZWLIQ 
    REAL                                           :: PROPOR
    REAL                                           :: DTDZ  


    DO J = 1,NSNOW
          IF (J <= ABS(ISNOW)) THEN
             DZ(J)    = DZSNSO(J+ISNOW)
             SWICE(J) = SNICE(J+ISNOW)
             SWLIQ(J) = SNLIQ(J+ISNOW)
             TSNO(J)  = STC(J+ISNOW)
          END IF
    END DO

       MSNO = ABS(ISNOW)

       IF (MSNO == 1) THEN
          
          IF (DZ(1) > 0.05) THEN
             MSNO = 2
             DZ(1)    = DZ(1)/2.
             SWICE(1) = SWICE(1)/2.
             SWLIQ(1) = SWLIQ(1)/2.
             DZ(2)    = DZ(1)
             SWICE(2) = SWICE(1)
             SWLIQ(2) = SWLIQ(1)
             TSNO(2)  = TSNO(1)
          END IF
       END IF

       IF (MSNO > 1) THEN
          IF (DZ(1) > 0.05) THEN
             DRR      = DZ(1) - 0.05
             PROPOR   = DRR/DZ(1)
             ZWICE    = PROPOR*SWICE(1)
             ZWLIQ    = PROPOR*SWLIQ(1)
             PROPOR   = 0.05/DZ(1)
             SWICE(1) = PROPOR*SWICE(1)
             SWLIQ(1) = PROPOR*SWLIQ(1)
             DZ(1)    = 0.05

             CALL COMBO (DZ(2), SWLIQ(2), SWICE(2), TSNO(2), DRR, &
                  ZWLIQ, ZWICE, TSNO(1))

             
             IF (MSNO <= 2 .AND. DZ(2) > 0.20) THEN  

                MSNO = 3
                DTDZ = (TSNO(1) - TSNO(2))/((DZ(1)+DZ(2))/2.)
                DZ(2)    = DZ(2)/2.
                SWICE(2) = SWICE(2)/2.
                SWLIQ(2) = SWLIQ(2)/2.
                DZ(3)    = DZ(2)
                SWICE(3) = SWICE(2)
                SWLIQ(3) = SWLIQ(2)
                TSNO(3) = TSNO(2) - DTDZ*DZ(2)/2.
                IF (TSNO(3) >= TFRZ) THEN
                   TSNO(3)  = TSNO(2)
                ELSE
                   TSNO(2) = TSNO(2) + DTDZ*DZ(2)/2.
                ENDIF

             END IF
          END IF
       END IF

       IF (MSNO > 2) THEN
          IF (DZ(2) > 0.2) THEN
             DRR = DZ(2) - 0.2
             PROPOR   = DRR/DZ(2)
             ZWICE    = PROPOR*SWICE(2)
             ZWLIQ    = PROPOR*SWLIQ(2)
             PROPOR   = 0.2/DZ(2)
             SWICE(2) = PROPOR*SWICE(2)
             SWLIQ(2) = PROPOR*SWLIQ(2)
             DZ(2)    = 0.2
             CALL COMBO (DZ(3), SWLIQ(3), SWICE(3), TSNO(3), DRR, &
                  ZWLIQ, ZWICE, TSNO(2))
          END IF
       END IF

       ISNOW = -MSNO

    DO J = ISNOW+1,0
             DZSNSO(J) = DZ(J-ISNOW)
             SNICE(J) = SWICE(J-ISNOW)
             SNLIQ(J) = SWLIQ(J-ISNOW)
             STC(J)   = TSNO(J-ISNOW)
    END DO






  END SUBROUTINE DIVIDE



  SUBROUTINE COMBO(DZ,  WLIQ,  WICE, T, DZ2, WLIQ2, WICE2, T2)

    IMPLICIT NONE





    REAL, INTENT(IN)    :: DZ2   
    REAL, INTENT(IN)    :: WLIQ2 
    REAL, INTENT(IN)    :: WICE2 
    REAL, INTENT(IN)    :: T2    
    REAL, INTENT(INOUT) :: DZ    
    REAL, INTENT(INOUT) :: WLIQ  
    REAL, INTENT(INOUT) :: WICE  
    REAL, INTENT(INOUT) :: T     



    REAL                :: DZC   
    REAL                :: WLIQC 
    REAL                :: WICEC 
    REAL                :: TC    
    REAL                :: H     
    REAL                :: H2    
    REAL                :: HC    



    DZC = DZ+DZ2
    WICEC = (WICE+WICE2)
    WLIQC = (WLIQ+WLIQ2)
    H = (CICE*WICE+CWAT*WLIQ) * (T-TFRZ)+HFUS*WLIQ
    H2= (CICE*WICE2+CWAT*WLIQ2) * (T2-TFRZ)+HFUS*WLIQ2

    HC = H + H2
    IF(HC < 0.)THEN
       TC = TFRZ + HC/(CICE*WICEC + CWAT*WLIQC)
    ELSE IF (HC.LE.HFUS*WLIQC) THEN
       TC = TFRZ
    ELSE
       TC = TFRZ + (HC - HFUS*WLIQC) / (CICE*WICEC + CWAT*WLIQC)
    END IF

    DZ = DZC
    WICE = WICEC
    WLIQ = WLIQC
    T = TC

  END SUBROUTINE COMBO


  SUBROUTINE COMPACT (NSNOW  ,NSOIL  ,DT     ,STC    ,SNICE  , & 
                      SNLIQ  ,ZSOIL  ,IMELT  ,FICEOLD,ILOC   , JLOC , & 
                      ISNOW  ,DZSNSO ,ZSNSO )                    


  IMPLICIT NONE


   INTEGER,                         INTENT(IN)    :: ILOC   
   INTEGER,                         INTENT(IN)    :: JLOC   
   INTEGER,                         INTENT(IN)    :: NSOIL  
   INTEGER,                         INTENT(IN)    :: NSNOW  
   INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT  
   REAL,                            INTENT(IN)    :: DT     
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: STC    
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: SNICE  
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: SNLIQ  
   REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: FICEOLD


   INTEGER,                         INTENT(INOUT) :: ISNOW  
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO 
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO  


   REAL, PARAMETER     :: C2 = 21.e-3   
   REAL, PARAMETER     :: C3 = 2.5e-6   
   REAL, PARAMETER     :: C4 = 0.04     
   REAL, PARAMETER     :: C5 = 2.0      
   REAL, PARAMETER     :: DM = 100.0    
   REAL, PARAMETER     :: ETA0 = 0.8e+6 
                                        
   REAL :: BURDEN 
   REAL :: DDZ1   
   REAL :: DDZ2   
   REAL :: DDZ3   
   REAL :: DEXPF  
   REAL :: TD     
   REAL :: PDZDTC 
   REAL :: VOID   
   REAL :: WX     
   REAL :: BI     
   REAL, DIMENSION(-NSNOW+1:0) :: FICE   

   INTEGER  :: J


    BURDEN = 0.0

    DO J = ISNOW+1, 0

        WX      = SNICE(J) + SNLIQ(J)
        FICE(J) = SNICE(J) / WX
        VOID    = 1. - (SNICE(J)/DENICE + SNLIQ(J)/DENH2O) / DZSNSO(J)

        
        IF (VOID > 0.001 .AND. SNICE(J) > 0.1) THEN
           BI = SNICE(J) / DZSNSO(J)
           TD = MAX(0.,TFRZ-STC(J))
           DEXPF = EXP(-C4*TD)

           

           DDZ1 = -C3*DEXPF

           IF (BI > DM) DDZ1 = DDZ1*EXP(-46.0E-3*(BI-DM))

           

           IF (SNLIQ(J) > 0.01*DZSNSO(J)) DDZ1=DDZ1*C5

           

           DDZ2 = -(BURDEN+0.5*WX)*EXP(-0.08*TD-C2*BI)/ETA0 

           

           IF (IMELT(J) == 1) THEN
              DDZ3 = MAX(0.,(FICEOLD(J) - FICE(J))/MAX(1.E-6,FICEOLD(J)))
              DDZ3 = - DDZ3/DT           
           ELSE
              DDZ3 = 0.
           END IF

           

           PDZDTC = (DDZ1 + DDZ2 + DDZ3)*DT
           PDZDTC = MAX(-0.5,PDZDTC)

           

           DZSNSO(J) = DZSNSO(J)*(1.+PDZDTC)
        END IF

        

        BURDEN = BURDEN + WX

    END DO

  END SUBROUTINE COMPACT

  SUBROUTINE SNOWH2O (NSNOW  ,NSOIL  ,DT     ,QSNFRO ,QSNSUB , & 
                      QRAIN  ,ILOC   ,JLOC   ,                 & 
                      ISNOW  ,DZSNSO ,SNOWH  ,SNEQV  ,SNICE  , & 
                      SNLIQ  ,SH2O   ,SICE   ,STC    ,         & 
                      QSNBOT ,PONDING1       ,PONDING2)          




   IMPLICIT NONE



   INTEGER,                         INTENT(IN)    :: ILOC   
   INTEGER,                         INTENT(IN)    :: JLOC   
   INTEGER,                         INTENT(IN)    :: NSNOW  
   INTEGER,                         INTENT(IN)    :: NSOIL  
   REAL,                            INTENT(IN)    :: DT     
   REAL,                            INTENT(IN)    :: QSNFRO 
   REAL,                            INTENT(IN)    :: QSNSUB 
   REAL,                            INTENT(IN)    :: QRAIN  



   REAL,                            INTENT(OUT)   :: QSNBOT 



   INTEGER,                         INTENT(INOUT) :: ISNOW  
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO 
   REAL,                            INTENT(INOUT) :: SNOWH  
   REAL,                            INTENT(INOUT) :: SNEQV  
   REAL, DIMENSION(-NSNOW+1:0),     INTENT(INOUT) :: SNICE  
   REAL, DIMENSION(-NSNOW+1:0),     INTENT(INOUT) :: SNLIQ  
   REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   
   REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE   
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    



   INTEGER                     :: J         
   REAL                        :: QIN       
   REAL                        :: QOUT      
   REAL                        :: WGDIF     
   REAL, DIMENSION(-NSNOW+1:0) :: VOL_LIQ   
   REAL, DIMENSION(-NSNOW+1:0) :: VOL_ICE   
   REAL, DIMENSION(-NSNOW+1:0) :: EPORE     
   REAL :: PROPOR, TEMP
   REAL :: PONDING1, PONDING2




   IF(SNEQV == 0.) THEN
      SICE(1) =  SICE(1) + (QSNFRO-QSNSUB)*DT/(DZSNSO(1)*1000.)  
      IF(SICE(1) < 0.) THEN
         SH2O(1) = SH2O(1) + SICE(1)
         SICE(1) = 0.
      END IF
   END IF






   IF(ISNOW == 0 .and. SNEQV > 0.) THEN
      TEMP   = SNEQV
      SNEQV  = SNEQV - QSNSUB*DT + QSNFRO*DT
      PROPOR = SNEQV/TEMP
      SNOWH  = MAX(0.,PROPOR * SNOWH)

      IF(SNEQV < 0.) THEN
         SICE(1) = SICE(1) + SNEQV/(DZSNSO(1)*1000.)
         SNEQV   = 0.
         SNOWH   = 0.
      END IF
      IF(SICE(1) < 0.) THEN
         SH2O(1) = SH2O(1) + SICE(1)
         SICE(1) = 0.
      END IF
   END IF

   IF(SNOWH <= 1.E-8 .OR. SNEQV <= 1.E-6) THEN
     SNOWH = 0.0
     SNEQV = 0.0
   END IF



   IF ( ISNOW < 0 ) THEN 

      WGDIF = SNICE(ISNOW+1) - QSNSUB*DT + QSNFRO*DT
      SNICE(ISNOW+1) = WGDIF
      IF (WGDIF < 1.e-6 .and. ISNOW <0) THEN
         CALL  COMBINE (NSNOW  ,NSOIL  ,ILOC, JLOC   , & 
              ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & 
              DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,         & 
              PONDING1, PONDING2 )                       
      ENDIF
      
      IF ( ISNOW < 0 ) THEN 
         SNLIQ(ISNOW+1) = SNLIQ(ISNOW+1) + QRAIN * DT
         SNLIQ(ISNOW+1) = MAX(0., SNLIQ(ISNOW+1))
      ENDIF
      
   ENDIF 



   

   DO J = -NSNOW+1, 0
      IF (J >= ISNOW+1) THEN
         VOL_ICE(J)      = MIN(1., SNICE(J)/(DZSNSO(J)*DENICE))
         EPORE(J)        = 1. - VOL_ICE(J)
         VOL_LIQ(J)      = MIN(EPORE(J),SNLIQ(J)/(DZSNSO(J)*DENH2O))
      END IF
   END DO

   QIN = 0.
   QOUT = 0.

   

   DO J = -NSNOW+1, 0
      IF (J >= ISNOW+1) THEN
         SNLIQ(J) = SNLIQ(J) + QIN
         IF (J <= -1) THEN
            IF (EPORE(J) < 0.05 .OR. EPORE(J+1) < 0.05) THEN
               QOUT = 0.
            ELSE
               QOUT = MAX(0.,(VOL_LIQ(J)-SSI*EPORE(J))*DZSNSO(J))
               QOUT = MIN(QOUT,(1.-VOL_ICE(J+1)-VOL_LIQ(J+1))*DZSNSO(J+1))
            END IF
         ELSE
            QOUT = MAX(0.,(VOL_LIQ(J) - SSI*EPORE(J))*DZSNSO(J))
         END IF
         QOUT = QOUT*1000.
         SNLIQ(J) = SNLIQ(J) - QOUT
         QIN = QOUT
      END IF
   END DO



   QSNBOT = QOUT / DT           

  END SUBROUTINE SNOWH2O

  SUBROUTINE SOILWATER (NSOIL  ,NSNOW  ,DT     ,ZSOIL  ,DZSNSO , & 
                        QINSUR ,QSEVA  ,ETRANI ,SICE   ,ILOC   , JLOC, & 
                        SH2O   ,SMC    ,ZWT    ,ISURBAN,VEGTYP ,& 
                        SMCWTD, DEEPRECH                       ,& 
                        RUNSRF ,QDRAIN ,RUNSUB ,WCND   ,FCRMAX )   





  IMPLICIT NONE


  INTEGER,                     INTENT(IN) :: ILOC   
  INTEGER,                     INTENT(IN) :: JLOC   
  INTEGER,                     INTENT(IN) :: NSOIL  
  INTEGER,                     INTENT(IN) :: NSNOW  
  REAL,                        INTENT(IN) :: DT     
  REAL, INTENT(IN)                        :: QINSUR 
  REAL, INTENT(IN)                        :: QSEVA  
  REAL, DIMENSION(1:NSOIL),    INTENT(IN) :: ZSOIL  
  REAL, DIMENSION(1:NSOIL),    INTENT(IN) :: ETRANI 
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO 
  REAL, DIMENSION(1:NSOIL), INTENT(IN)   :: SICE   

  INTEGER,                     INTENT(IN) :: VEGTYP
  INTEGER,                     INTENT(IN) :: ISURBAN


  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SH2O   
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SMC    
  REAL, INTENT(INOUT)                     :: ZWT    
  REAL,                     INTENT(INOUT) :: SMCWTD 
  REAL                    , INTENT(INOUT) :: DEEPRECH


  REAL, INTENT(OUT)                       :: QDRAIN 
  REAL, INTENT(OUT)                       :: RUNSRF 
  REAL, INTENT(OUT)                       :: RUNSUB 
  REAL, INTENT(OUT)                       :: FCRMAX 
  REAL, DIMENSION(1:NSOIL), INTENT(OUT)   :: WCND   


  INTEGER                                 :: K,IZ   
  INTEGER                                 :: ITER   
  REAl                                    :: DTFINE 
  REAL, DIMENSION(1:NSOIL)                :: RHSTT  
  REAL, DIMENSION(1:NSOIL)                :: AI     
  REAL, DIMENSION(1:NSOIL)                :: BI     
  REAL, DIMENSION(1:NSOIL)                :: CI     

  REAL                                    :: FFF    
  REAL                                    :: RSBMX  
  REAL                                    :: PDDUM  
  REAL                                    :: FICE   
  REAL                                    :: WPLUS  
  REAL                                    :: RSAT   
  REAL                                    :: SICEMAX
  REAL                                    :: SH2OMIN
  REAL                                    :: WTSUB  
  REAL                                    :: MH2O   
  REAL                                    :: FSAT   
  REAL, DIMENSION(1:NSOIL)                :: MLIQ   
  REAL                                    :: XS     
  REAL                                    :: WATMIN 
  REAL                                    :: QDRAIN_SAVE 
  REAL                                    :: EPORE  
  REAL, DIMENSION(1:NSOIL)                :: FCR    
  INTEGER                                 :: NITER  
  REAL                                    :: SMCTOT 
  REAL                                    :: DZTOT  
  REAL, PARAMETER :: A = 4.0

    RUNSRF = 0.0
    PDDUM  = 0.0
    RSAT   = 0.0



    DO K = 1,NSOIL
       EPORE   = MAX ( 1.E-4 , ( SMCMAX - SICE(K) ) )
       RSAT    = RSAT + MAX(0.,SH2O(K)-EPORE)*DZSNSO(K)  
       SH2O(K) = MIN(EPORE,SH2O(K))             
    END DO



    DO K = 1,NSOIL
       FICE    = MIN(1.0,SICE(K)/SMCMAX)
       FCR(K)  = MAX(0.0,EXP(-A*(1.-FICE))- EXP(-A)) /  &
                        (1.0              - EXP(-A))
    END DO



    SICEMAX = 0.0
    FCRMAX  = 0.0
    SH2OMIN = SMCMAX
    DO K = 1,NSOIL
       IF (SICE(K) > SICEMAX) SICEMAX = SICE(K)
       IF (FCR(K)  > FCRMAX)  FCRMAX  = FCR(K)
       IF (SH2O(K) < SH2OMIN) SH2OMIN = SH2O(K)
    END DO



    IF(OPT_RUN == 2) THEN 
        FFF   = 2.0
        RSBMX = 4.0
        CALL ZWTEQ (NSOIL  ,NSNOW  ,ZSOIL  ,DZSNSO ,SH2O   ,ZWT)
        RUNSUB = (1.0-FCRMAX) * RSBMX * EXP(-TIMEAN) * EXP(-FFF*ZWT)   
    END IF




    IF ( VEGTYP == ISURBAN ) FCR(1)= 0.95

    IF(OPT_RUN == 1) THEN
       FFF = 6.0
       FSAT   = FSATMX*EXP(-0.5*FFF*(ZWT-2.0))
       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ( (1.0-FCR(1))*FSAT + FCR(1) )
         PDDUM  = QINSUR - RUNSRF                          
       END IF
    END IF

    IF(OPT_RUN == 5) THEN
       FFF = 6.0
       FSAT   = FSATMX*EXP(-0.5*FFF*MAX(-2.0-ZWT,0.))
       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ( (1.0-FCR(1))*FSAT + FCR(1) )
         PDDUM  = QINSUR - RUNSRF                          
       END IF
    END IF

    IF(OPT_RUN == 2) THEN
       FFF   = 2.0
       FSAT   = FSATMX*EXP(-0.5*FFF*ZWT)
       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ( (1.0-FCR(1))*FSAT + FCR(1) )
         PDDUM  = QINSUR - RUNSRF                          
       END IF
    END IF

    IF(OPT_RUN == 3) THEN
       CALL INFIL (NSOIL  ,DT     ,ZSOIL  ,SH2O   ,SICE   , & 
                   SICEMAX,QINSUR ,                         & 
                   PDDUM  ,RUNSRF )                           
    END IF

    IF(OPT_RUN == 4) THEN
       SMCTOT = 0.
       DZTOT  = 0.
       DO K = 1,NSOIL
          DZTOT   = DZTOT  + DZSNSO(K)  
          SMCTOT  = SMCTOT + SMC(K)*DZSNSO(K)
          IF(DZTOT >= 2.0) EXIT
       END DO
       SMCTOT = SMCTOT/DZTOT
       FSAT   = MAX(0.01,SMCTOT/SMCMAX) ** 4.        

       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ((1.0-FCR(1))*FSAT+FCR(1))  
         PDDUM  = QINSUR - RUNSRF                       
       END IF
    END IF



    NITER = 1

    IF(OPT_INF == 1) THEN    
       NITER = 3
       IF (PDDUM*DT>DZSNSO(1)*SMCMAX ) THEN
          NITER = NITER*2
       END IF
    END IF                 

    DTFINE  = DT / NITER



    QDRAIN_SAVE = 0.0
    DO ITER = 1, NITER
       CALL SRT   (NSOIL  ,ZSOIL  ,DTFINE ,PDDUM  ,ETRANI , & 
                   QSEVA  ,SH2O   ,SMC    ,ZWT    ,FCR    , & 
                   SICEMAX,FCRMAX ,ILOC   ,JLOC   ,SMCWTD ,         & 
                   RHSTT  ,AI     ,BI     ,CI     ,QDRAIN , & 
                   WCND   )                                   
  
       CALL SSTEP (NSOIL  ,NSNOW  ,DTFINE ,ZSOIL  ,DZSNSO , & 
                   SICE   ,ILOC   ,JLOC   ,ZWT            ,                 & 
                   SH2O   ,SMC    ,AI     ,BI     ,CI     , & 
                   RHSTT  ,SMCWTD ,QDRAIN ,DEEPRECH,                                 & 
                   WPLUS)                                     
       RSAT =  RSAT + WPLUS
       QDRAIN_SAVE = QDRAIN_SAVE + QDRAIN
    END DO

    QDRAIN = QDRAIN_SAVE/NITER

    RUNSRF = RUNSRF * 1000. + RSAT * 1000./DT  
    QDRAIN = QDRAIN * 1000.






    IF(OPT_RUN == 2) THEN
         WTSUB = 0.
         DO K = 1, NSOIL
           WTSUB = WTSUB + WCND(K)*DZSNSO(K)
         END DO

         DO K = 1, NSOIL
           MH2O    = RUNSUB*DT*(WCND(K)*DZSNSO(K))/WTSUB       
           SH2O(K) = SH2O(K) - MH2O/(DZSNSO(K)*1000.)
         END DO
    END IF




   IF(OPT_RUN /= 1) THEN
      DO IZ = 1, NSOIL
         MLIQ(IZ) = SH2O(IZ)*DZSNSO(IZ)*1000.
      END DO

      WATMIN = 0.01           
      DO IZ = 1, NSOIL-1
          IF (MLIQ(IZ) .LT. 0.) THEN
             XS = WATMIN-MLIQ(IZ)
          ELSE
             XS = 0.
          END IF
          MLIQ(IZ  ) = MLIQ(IZ  ) + XS
          MLIQ(IZ+1) = MLIQ(IZ+1) - XS
      END DO

        IZ = NSOIL
        IF (MLIQ(IZ) .LT. WATMIN) THEN
           XS = WATMIN-MLIQ(IZ)
        ELSE
           XS = 0.
        END IF
        MLIQ(IZ) = MLIQ(IZ) + XS
        RUNSUB   = RUNSUB - XS/DT
        IF(OPT_RUN == 5)DEEPRECH = DEEPRECH - XS*1.E-3

      DO IZ = 1, NSOIL
        SH2O(IZ)     = MLIQ(IZ) / (DZSNSO(IZ)*1000.)
      END DO
   END IF

  END SUBROUTINE SOILWATER

  SUBROUTINE ZWTEQ (NSOIL  ,NSNOW  ,ZSOIL  ,DZSNSO ,SH2O   ,ZWT)



  IMPLICIT NONE



  INTEGER,                         INTENT(IN) :: NSOIL  
  INTEGER,                         INTENT(IN) :: NSNOW  
  REAL, DIMENSION(1:NSOIL),        INTENT(IN) :: ZSOIL  
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO 
  REAL, DIMENSION(1:NSOIL),        INTENT(IN) :: SH2O   



  REAL,                           INTENT(OUT) :: ZWT    



  INTEGER :: K                      
  INTEGER, PARAMETER :: NFINE = 100 
  REAL    :: WD1                    
  REAL    :: WD2                    
  REAL    :: DZFINE                 
  REAL    :: TEMP                   
  REAL, DIMENSION(1:NFINE) :: ZFINE 


   WD1 = 0.
   DO K = 1,NSOIL
     WD1 = WD1 + (SMCMAX-SH2O(K)) * DZSNSO(K) 
   ENDDO

   DZFINE = 3.0 * (-ZSOIL(NSOIL)) / NFINE  
   do K =1,NFINE
      ZFINE(K) = FLOAT(K) * DZFINE
   ENDDO

   ZWT = -3.*ZSOIL(NSOIL) - 0.001   

   WD2 = 0.
   DO K = 1,NFINE
     TEMP  = 1. + (ZWT-ZFINE(K))/PSISAT
     WD2   = WD2 + SMCMAX*(1.-TEMP**(-1./BEXP))*DZFINE
     IF(ABS(WD2-WD1).LE.0.01) THEN
        ZWT = ZFINE(K)
        EXIT
     ENDIF
   ENDDO

  END SUBROUTINE ZWTEQ


  SUBROUTINE INFIL (NSOIL  ,DT     ,ZSOIL  ,SH2O   ,SICE   , & 
                    SICEMAX,QINSUR ,                         & 
                    PDDUM  ,RUNSRF )                           



    IMPLICIT NONE


  INTEGER,                  INTENT(IN) :: NSOIL  
  REAL,                     INTENT(IN) :: DT     
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: ZSOIL  
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SH2O   
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SICE   
  REAL,                     INTENT(IN) :: QINSUR 
  REAL,                     INTENT(IN) :: SICEMAX


  REAL,                    INTENT(OUT) :: RUNSRF 
  REAL,                    INTENT(OUT) :: PDDUM  


  INTEGER :: IALP1, J, JJ,  K
  REAL                     :: VAL
  REAL                     :: DDT
  REAL                     :: PX
  REAL                     :: DT1, DD, DICE
  REAL                     :: FCR
  REAL                     :: SUM
  REAL                     :: ACRT
  REAL                     :: WDF
  REAL                     :: WCND
  REAL                     :: SMCAV
  REAL                     :: INFMAX
  REAL, DIMENSION(1:NSOIL) :: DMAX
  INTEGER, PARAMETER       :: CVFRZ = 3


    IF (QINSUR >  0.0) THEN
       DT1 = DT /86400.
       SMCAV = SMCMAX - SMCWLT



       DMAX(1)= -ZSOIL(1) * SMCAV
       DICE   = -ZSOIL(1) * SICE(1)
       DMAX(1)= DMAX(1)* (1.0-(SH2O(1) + SICE(1) - SMCWLT)/SMCAV)

       DD = DMAX(1)

       DO K = 2,NSOIL
          DICE    = DICE + (ZSOIL(K-1) - ZSOIL(K) ) * SICE(K)
          DMAX(K) = (ZSOIL(K-1) - ZSOIL(K)) * SMCAV
          DMAX(K) = DMAX(K) * (1.0-(SH2O(K) + SICE(K) - SMCWLT)/SMCAV)
          DD      = DD + DMAX(K)
       END DO

       VAL = (1. - EXP ( - KDT * DT1))
       DDT = DD * VAL
       PX  = MAX(0.,QINSUR * DT)
       INFMAX = (PX * (DDT / (PX + DDT)))/ DT



       FCR = 1.
       IF (DICE >  1.E-2) THEN
          ACRT = CVFRZ * FRZX / DICE
          SUM = 1.
          IALP1 = CVFRZ - 1
          DO J = 1,IALP1
             K = 1
             DO JJ = J +1,IALP1
                K = K * JJ
             END DO
             SUM = SUM + (ACRT ** (CVFRZ - J)) / FLOAT(K)
          END DO
          FCR = 1. - EXP (-ACRT) * SUM
       END IF



       INFMAX = INFMAX * FCR




       CALL WDFCND2 (WDF,WCND,SH2O(1),SICEMAX)
       INFMAX = MAX (INFMAX,WCND)
       INFMAX = MIN (INFMAX,PX)

       RUNSRF= MAX(0., QINSUR - INFMAX)
       PDDUM = QINSUR - RUNSRF

    END IF

  END SUBROUTINE INFIL

  SUBROUTINE SRT (NSOIL  ,ZSOIL  ,DT     ,PDDUM  ,ETRANI , & 
                  QSEVA  ,SH2O   ,SMC    ,ZWT    ,FCR    , & 
                  SICEMAX,FCRMAX ,ILOC   ,JLOC   ,SMCWTD ,         & 
                  RHSTT  ,AI     ,BI     ,CI     ,QDRAIN , & 
                  WCND   )                                   





    IMPLICIT NONE



    INTEGER,                  INTENT(IN)  :: ILOC   
    INTEGER,                  INTENT(IN)  :: JLOC   
    INTEGER,                  INTENT(IN)  :: NSOIL
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: ZSOIL
    REAL,                     INTENT(IN)  :: DT
    REAL,                     INTENT(IN)  :: PDDUM
    REAL,                     INTENT(IN)  :: QSEVA
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: ETRANI
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SH2O
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SMC
    REAL,                     INTENT(IN)  :: ZWT    
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: FCR
    REAL, INTENT(IN)                      :: FCRMAX 
    REAL,                     INTENT(IN)  :: SICEMAX
    REAL,                     INTENT(IN)  :: SMCWTD 



    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: RHSTT
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: AI
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: BI
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: CI
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: WCND    
    REAL,                     INTENT(OUT) :: QDRAIN  


    INTEGER                               :: K
    REAL, DIMENSION(1:NSOIL)              :: DDZ
    REAL, DIMENSION(1:NSOIL)              :: DENOM
    REAL, DIMENSION(1:NSOIL)              :: DSMDZ
    REAL, DIMENSION(1:NSOIL)              :: WFLUX
    REAL, DIMENSION(1:NSOIL)              :: WDF
    REAL, DIMENSION(1:NSOIL)              :: SMX
    REAL                                  :: TEMP1
    REAL                                  :: SMXWTD 
    REAL                                  :: SMXBOT  




    IF(OPT_INF == 1) THEN
      DO K = 1, NSOIL
        CALL WDFCND1 (WDF(K),WCND(K),SMC(K),FCR(K))
        SMX(K) = SMC(K)
      END DO
        IF(OPT_RUN == 5)SMXWTD=SMCWTD
    END IF

    IF(OPT_INF == 2) THEN
      DO K = 1, NSOIL
        CALL WDFCND2 (WDF(K),WCND(K),SH2O(K),SICEMAX)
        SMX(K) = SH2O(K)
      END DO
          IF(OPT_RUN == 5)SMXWTD=SMCWTD*SH2O(NSOIL)/SMC(NSOIL)  
    END IF

    DO K = 1, NSOIL
       IF(K == 1) THEN
          DENOM(K) = - ZSOIL (K)
          TEMP1    = - ZSOIL (K+1)
          DDZ(K)   = 2.0 / TEMP1
          DSMDZ(K) = 2.0 * (SMX(K) - SMX(K+1)) / TEMP1
          WFLUX(K) = WDF(K) * DSMDZ(K) + WCND(K) - PDDUM + ETRANI(K) + QSEVA
       ELSE IF (K < NSOIL) THEN
          DENOM(k) = (ZSOIL(K-1) - ZSOIL(K))
          TEMP1    = (ZSOIL(K-1) - ZSOIL(K+1))
          DDZ(K)   = 2.0 / TEMP1
          DSMDZ(K) = 2.0 * (SMX(K) - SMX(K+1)) / TEMP1
          WFLUX(K) = WDF(K  ) * DSMDZ(K  ) + WCND(K  )         &
                   - WDF(K-1) * DSMDZ(K-1) - WCND(K-1) + ETRANI(K)
       ELSE
          DENOM(K) = (ZSOIL(K-1) - ZSOIL(K))
          IF(OPT_RUN == 1 .or. OPT_RUN == 2) THEN
             QDRAIN   = 0.
          END IF
          IF(OPT_RUN == 3) THEN
             QDRAIN   = SLOPE*WCND(K)
          END IF
          IF(OPT_RUN == 4) THEN
             QDRAIN   = (1.0-FCRMAX)*WCND(K)
          END IF
          IF(OPT_RUN == 5) THEN   
             TEMP1    = 2.0 * DENOM(K)
             IF(ZWT < ZSOIL(NSOIL)-DENOM(NSOIL))THEN

                SMXBOT = SMX(K) - (SMX(K)-SMXWTD) *  DENOM(K) * 2./ (DENOM(K) + ZSOIL(K) - ZWT)
             ELSE
                SMXBOT = SMXWTD
             ENDIF
             DSMDZ(K) = 2.0 * (SMX(K) - SMXBOT) / TEMP1
             QDRAIN   = WDF(K  ) * DSMDZ(K  ) + WCND(K  )
          END IF   
          WFLUX(K) = -(WDF(K-1)*DSMDZ(K-1))-WCND(K-1)+ETRANI(K) + QDRAIN
       END IF
    END DO

    DO K = 1, NSOIL
       IF(K == 1) THEN
          AI(K)    =   0.0
          BI(K)    =   WDF(K  ) * DDZ(K  ) / DENOM(K)
          CI(K)    = - BI (K)
       ELSE IF (K < NSOIL) THEN
          AI(K)    = - WDF(K-1) * DDZ(K-1) / DENOM(K)
          CI(K)    = - WDF(K  ) * DDZ(K  ) / DENOM(K)
          BI(K)    = - ( AI (K) + CI (K) )
       ELSE
          AI(K)    = - WDF(K-1) * DDZ(K-1) / DENOM(K)
          CI(K)    = 0.0
          BI(K)    = - ( AI (K) + CI (K) )
       END IF
          RHSTT(K) = WFLUX(K) / (-DENOM(K))
    END DO


  END SUBROUTINE SRT


  SUBROUTINE SSTEP (NSOIL  ,NSNOW  ,DT     ,ZSOIL  ,DZSNSO , & 
                    SICE   ,ILOC   ,JLOC   ,ZWT            ,                 & 
                    SH2O   ,SMC    ,AI     ,BI     ,CI     , & 
                    RHSTT  ,SMCWTD ,QDRAIN ,DEEPRECH,                                 & 
                    WPLUS  )                                   




    IMPLICIT NONE



    INTEGER,                         INTENT(IN) :: ILOC   
    INTEGER,                         INTENT(IN) :: JLOC   
    INTEGER,                         INTENT(IN) :: NSOIL  
    INTEGER,                         INTENT(IN) :: NSNOW  
    REAL, INTENT(IN)                            :: DT
    REAL, INTENT(IN)                            :: ZWT
    REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL
    REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SICE
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO 


    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SH2O
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SMC
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: AI
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: BI
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: CI
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: RHSTT
    REAL                    , INTENT(INOUT) :: SMCWTD
    REAL                    , INTENT(INOUT) :: QDRAIN
    REAL                    , INTENT(INOUT) :: DEEPRECH


    REAL, INTENT(OUT)                       :: WPLUS     


    INTEGER                                 :: K
    REAL, DIMENSION(1:NSOIL)                :: RHSTTIN
    REAL, DIMENSION(1:NSOIL)                :: CIIN
    REAL                                    :: STOT
    REAL                                    :: EPORE
    REAL                                    :: WMINUS

    WPLUS = 0.0

    DO K = 1,NSOIL
       RHSTT (K) =   RHSTT(K) * DT
       AI (K)    =      AI(K) * DT
       BI (K)    = 1. + BI(K) * DT
       CI (K)    =      CI(K) * DT
    END DO



    DO K = 1,NSOIL
       RHSTTIN(k) = RHSTT(K)
       CIIN(k)    = CI(K)
    END DO



    CALL ROSR12 (CI,AI,BI,CIIN,RHSTTIN,RHSTT,1,NSOIL,0)

    DO K = 1,NSOIL
        SH2O(K) = SH2O(K) + CI(K)
    ENDDO





  IF(OPT_RUN == 5) THEN



     IF(ZWT < ZSOIL(NSOIL)-DZSNSO(NSOIL))THEN

        DEEPRECH =  DEEPRECH + DT * QDRAIN
     ELSE
        SMCWTD = SMCWTD + DT * QDRAIN  / DZSNSO(NSOIL)
        WPLUS        = MAX((SMCWTD-SMCMAX), 0.0) * DZSNSO(NSOIL)
        WMINUS       = MAX((1.E-4-SMCWTD), 0.0) * DZSNSO(NSOIL)

        SMCWTD = MAX( MIN(SMCWTD,SMCMAX) , 1.E-4)
        SH2O(NSOIL)    = SH2O(NSOIL) + WPLUS/DZSNSO(NSOIL)


        QDRAIN = QDRAIN - WPLUS/DT
        DEEPRECH = DEEPRECH - WMINUS
     ENDIF

  ENDIF

    DO K = NSOIL,2,-1
      EPORE        = MAX ( 1.E-4 , ( SMCMAX - SICE(K) ) )
      WPLUS        = MAX((SH2O(K)-EPORE), 0.0) * DZSNSO(K)
      SH2O(K)      = MIN(EPORE,SH2O(K))
      SH2O(K-1)    = SH2O(K-1) + WPLUS/DZSNSO(K-1)
    END DO

    EPORE        = MAX ( 1.E-4 , ( SMCMAX - SICE(1) ) )
    WPLUS        = MAX((SH2O(1)-EPORE), 0.0) * DZSNSO(1) 
    SH2O(1)      = MIN(EPORE,SH2O(1))

  END SUBROUTINE SSTEP

  SUBROUTINE WDFCND1 (WDF,WCND,SMC,FCR)



    IMPLICIT NONE


    REAL,INTENT(IN)  :: SMC
    REAL,INTENT(IN)  :: FCR


    REAL,INTENT(OUT) :: WCND
    REAL,INTENT(OUT) :: WDF


    REAL :: EXPON
    REAL :: FACTR
    REAL :: VKWGT




    FACTR = MAX(0.01, SMC/SMCMAX)
    EXPON = BEXP + 2.0
    WDF   = DWSAT * FACTR ** EXPON
    WDF   = WDF * (1.0 - FCR)



    EXPON = 2.0*BEXP + 3.0
    WCND  = DKSAT * FACTR ** EXPON
    WCND  = WCND * (1.0 - FCR)

  END SUBROUTINE WDFCND1

  SUBROUTINE WDFCND2 (WDF,WCND,SMC,SICE)



    IMPLICIT NONE


    REAL,INTENT(IN)  :: SMC
    REAL,INTENT(IN)  :: SICE


    REAL,INTENT(OUT) :: WCND
    REAL,INTENT(OUT) :: WDF


    REAL :: EXPON
    REAL :: FACTR
    REAL :: VKWGT




    FACTR = MAX(0.01, SMC/SMCMAX)
    EXPON = BEXP + 2.0
    WDF   = DWSAT * FACTR ** EXPON

    IF (SICE > 0.0) THEN
    VKWGT = 1./ (1. + (500.* SICE)**3.)
    WDF   = VKWGT * WDF + (1.-VKWGT)*DWSAT*(0.2/SMCMAX)**EXPON
    END IF



    EXPON = 2.0*BEXP + 3.0
    WCND  = DKSAT * FACTR ** EXPON

  END SUBROUTINE WDFCND2


  SUBROUTINE GROUNDWATER(NSNOW  ,NSOIL  ,DT     ,SICE   ,ZSOIL  , & 
                         STC    ,WCND   ,FCRMAX ,ILOC   ,JLOC   , & 
                         SH2O   ,ZWT    ,WA     ,WT     ,         & 
                         QIN    ,QDIS   )                           

  IMPLICIT NONE


  INTEGER,                         INTENT(IN) :: ILOC  
  INTEGER,                         INTENT(IN) :: JLOC  
  INTEGER,                         INTENT(IN) :: NSNOW 
  INTEGER,                         INTENT(IN) :: NSOIL 
  REAL,                            INTENT(IN) :: DT    
  REAL,                            INTENT(IN) :: FCRMAX
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SICE  
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL 
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: WCND  
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC   


  REAL, DIMENSION(    1:NSOIL), INTENT(INOUT) :: SH2O  
  REAL,                         INTENT(INOUT) :: ZWT   
  REAL,                         INTENT(INOUT) :: WA    
  REAL,                         INTENT(INOUT) :: WT    
                                                           

  REAL,                           INTENT(OUT) :: QIN   
  REAL,                           INTENT(OUT) :: QDIS  


  REAL                                        :: FFF   
  REAL                                        :: RSBMX 
  INTEGER                                     :: IZ    
  INTEGER                                     :: IWT   
  REAL,  DIMENSION(    1:NSOIL)               :: DZMM  
  REAL,  DIMENSION(    1:NSOIL)               :: ZNODE 
  REAL,  DIMENSION(    1:NSOIL)               :: MLIQ  
  REAL,  DIMENSION(    1:NSOIL)               :: EPORE 
  REAL,  DIMENSION(    1:NSOIL)               :: HK    
  REAL,  DIMENSION(    1:NSOIL)               :: SMC   
  REAL(KIND=8)                                :: S_NODE
  REAL                                        :: DZSUM 
  REAL                                        :: SMPFZ 
  REAL                                        :: KA    
  REAL                                        :: WH_ZWT
  REAL                                        :: WH    
  REAL                                        :: WS    
  REAL                                        :: WTSUB 
  REAL                                        :: WATMIN
  REAL                                        :: XS    
  REAL, PARAMETER                             :: ROUS = 0.2    
  REAL, PARAMETER                             :: CMIC = 0.20   
                                                               

      QDIS      = 0.0
      QIN       = 0.0




      DZMM(1) = -ZSOIL(1)*1.E3
      DO IZ = 2, NSOIL
         DZMM(IZ)  = 1.E3 * (ZSOIL(IZ - 1) - ZSOIL(IZ))
      ENDDO



      ZNODE(1) = -ZSOIL(1) / 2.
      DO IZ = 2, NSOIL
         ZNODE(IZ)  = -ZSOIL(IZ-1) + 0.5 * (ZSOIL(IZ-1) - ZSOIL(IZ))
      ENDDO



      DO IZ = 1, NSOIL
         SMC(IZ)      = SH2O(IZ) + SICE(IZ)
         MLIQ(IZ)     = SH2O(IZ) * DZMM(IZ)
         EPORE(IZ)    = MAX(0.01,SMCMAX - SICE(IZ))
         HK(IZ)       = 1.E3*WCND(IZ)
      ENDDO




      IWT = NSOIL
      DO IZ = 2,NSOIL
         IF(ZWT   .LE. -ZSOIL(IZ) ) THEN
            IWT = IZ-1
            EXIT
         END IF
      ENDDO



      FFF   = 6.0
      RSBMX = 5.0

      QDIS = (1.0-FCRMAX)*RSBMX*EXP(-TIMEAN)*EXP(-FFF*(ZWT-2.0))



      S_NODE = MIN(1.0,SMC(IWT)/SMCMAX )
      S_NODE = MAX(S_NODE,REAL(0.01,KIND=8))
      SMPFZ  = -PSISAT*1000.*S_NODE**(-BEXP)   
      SMPFZ  = MAX(-120000.0,CMIC*SMPFZ)   



      KA  = HK(IWT)

      WH_ZWT  = - ZWT * 1.E3                          
      WH      = SMPFZ  - ZNODE(IWT)*1.E3              
      QIN     = - KA * (WH_ZWT-WH)  /((ZWT-ZNODE(IWT))*1.E3)
      QIN     = MAX(-10.0/DT,MIN(10./DT,QIN))
     


      WT  = WT + (QIN - QDIS) * DT     

      IF(IWT.EQ.NSOIL) THEN
         WA          = WA + (QIN - QDIS) * DT     
         WT          = WA
         ZWT         = (-ZSOIL(NSOIL) + 25.) - WA/1000./ROUS      
         MLIQ(NSOIL) = MLIQ(NSOIL) - QIN * DT        

         MLIQ(NSOIL) = MLIQ(NSOIL) + MAX(0.,(WA - 5000.))
         WA          = MIN(WA, 5000.)
      ELSE
         
         IF (IWT.EQ.NSOIL-1) THEN
            ZWT = -ZSOIL(NSOIL)                   &
                 - (WT-ROUS*1000*25.) / (EPORE(NSOIL))/1000.
         ELSE
            WS = 0.   
            DO IZ = IWT+2,NSOIL
               WS = WS + EPORE(IZ) * DZMM(IZ)
            ENDDO
            ZWT = -ZSOIL(IWT+1)                  &
                  - (WT-ROUS*1000.*25.-WS) /(EPORE(IWT+1))/1000.
         ENDIF

         WTSUB = 0.
         DO IZ = 1, NSOIL
           WTSUB = WTSUB + HK(IZ)*DZMM(IZ)
         END DO

         DO IZ = 1, NSOIL           
         MLIQ(IZ) = MLIQ(IZ) - QDIS*DT*HK(IZ)*DZMM(IZ)/WTSUB
         END DO
      END IF

      ZWT = MAX(1.5,ZWT)





      WATMIN = 0.01
      DO IZ = 1, NSOIL-1
          IF (MLIQ(IZ) .LT. 0.) THEN
             XS = WATMIN-MLIQ(IZ)
          ELSE
             XS = 0.
          END IF
          MLIQ(IZ  ) = MLIQ(IZ  ) + XS
          MLIQ(IZ+1) = MLIQ(IZ+1) - XS
      END DO

        IZ = NSOIL
        IF (MLIQ(IZ) .LT. WATMIN) THEN
           XS = WATMIN-MLIQ(IZ)
        ELSE
           XS = 0.
        END IF
        MLIQ(IZ) = MLIQ(IZ) + XS
        WA       = WA - XS
        WT       = WT - XS

      DO IZ = 1, NSOIL
        SH2O(IZ)     = MLIQ(IZ) / DZMM(IZ)
      END DO

  END SUBROUTINE GROUNDWATER


  SUBROUTINE SHALLOWWATERTABLE (NSNOW  ,NSOIL  ,ZSOIL, DT    , & 
                         DZSNSO ,SMCEQ ,ILOC   ,JLOC         , & 
                         SMC    ,WTD   ,SMCWTD ,RECH, QDRAIN  )  




  IMPLICIT NONE


  INTEGER,                         INTENT(IN) :: NSNOW 
  INTEGER,                         INTENT(IN) :: NSOIL 
  INTEGER,                         INTENT(IN) :: ILOC,JLOC
  REAL,                            INTENT(IN) :: DT
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL 
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO 
  REAL,  DIMENSION(      1:NSOIL), INTENT(IN) :: SMCEQ  


  REAL,  DIMENSION(      1:NSOIL), INTENT(INOUT) :: SMC   
  REAL,                         INTENT(INOUT) :: WTD   
  REAL,                         INTENT(INOUT) :: SMCWTD   
  REAL,                         INTENT(OUT) :: RECH 
  REAL,                         INTENT(INOUT) :: QDRAIN
    

  INTEGER                                     :: IZ    
  INTEGER                                     :: IWTD   
  INTEGER                                     :: KWTD   
  REAL                                        :: WTDOLD
  REAL                                        :: DZUP
  REAL                                        :: SMCEQDEEP
  REAL,  DIMENSION(       0:NSOIL)            :: ZSOIL0



ZSOIL0(1:NSOIL) = ZSOIL(1:NSOIL)
ZSOIL0(0) = 0.         
 

     DO IZ=NSOIL,1,-1
        IF(WTD + 1.E-6 < ZSOIL0(IZ)) EXIT
     ENDDO
        IWTD=IZ

        
        KWTD=IWTD+1  
        IF(KWTD.LE.NSOIL)THEN    
           WTDOLD=WTD
           IF(SMC(KWTD).GT.SMCEQ(KWTD))THEN
        
               IF(SMC(KWTD).EQ.SMCMAX)THEN 
                      WTD=ZSOIL0(IWTD)
                      RECH=-(WTDOLD-WTD) * (SMCMAX-SMCEQ(KWTD))
                      IWTD=IWTD-1
                      KWTD=KWTD-1
                   IF(KWTD.GE.1)THEN
                      IF(SMC(KWTD).GT.SMCEQ(KWTD))THEN
                      WTDOLD=WTD
                      WTD = MIN( ( SMC(KWTD)*DZSNSO(KWTD) &
                        - SMCEQ(KWTD)*ZSOIL0(IWTD) + SMCMAX*ZSOIL0(KWTD) ) / &
                        ( SMCMAX-SMCEQ(KWTD) ), ZSOIL0(IWTD))
                      RECH=RECH-(WTDOLD-WTD) * (SMCMAX-SMCEQ(KWTD))
                      ENDIF
                   ENDIF
               ELSE  
                      WTD = MIN( ( SMC(KWTD)*DZSNSO(KWTD) &
                        - SMCEQ(KWTD)*ZSOIL0(IWTD) + SMCMAX*ZSOIL0(KWTD) ) / &
                        ( SMCMAX-SMCEQ(KWTD) ), ZSOIL0(IWTD))
                      RECH=-(WTDOLD-WTD) * (SMCMAX-SMCEQ(KWTD))
               ENDIF
           
           ELSE    
               WTD=ZSOIL0(KWTD)
               RECH=-(WTDOLD-WTD) * (SMCMAX-SMCEQ(KWTD))
               KWTD=KWTD+1
               IWTD=IWTD+1

               IF(KWTD.LE.NSOIL)THEN
                   WTDOLD=WTD
                   IF(SMC(KWTD).GT.SMCEQ(KWTD))THEN
                   WTD = MIN( ( SMC(KWTD)*DZSNSO(KWTD) &
                   - SMCEQ(KWTD)*ZSOIL0(IWTD) + SMCMAX*ZSOIL0(KWTD) ) / &
                       ( SMCMAX-SMCEQ(KWTD) ) , ZSOIL0(IWTD) )
                   ELSE
                   WTD=ZSOIL0(KWTD)
                   ENDIF
                   RECH = RECH - (WTDOLD-WTD) * &
                                 (SMCMAX-SMCEQ(KWTD))

                ELSE
                   WTDOLD=WTD





                   SMCEQDEEP = SMCMAX * ( -PSISAT / ( -PSISAT - DZSNSO(NSOIL) ) ) ** (1./BEXP)
                   WTD = MIN( ( SMCWTD*DZSNSO(NSOIL) &
                   - SMCEQDEEP*ZSOIL0(NSOIL) + SMCMAX*(ZSOIL0(NSOIL)-DZSNSO(NSOIL)) ) / &
                       ( SMCMAX-SMCEQDEEP ) , ZSOIL0(NSOIL) )
                   RECH = RECH - (WTDOLD-WTD) * &
                                 (SMCMAX-SMCEQDEEP)
                ENDIF
            
            ENDIF
        ELSEIF(WTD.GE.ZSOIL0(NSOIL)-DZSNSO(NSOIL))THEN

           WTDOLD=WTD
           SMCEQDEEP = SMCMAX * ( -PSISAT / ( -PSISAT - DZSNSO(NSOIL) ) ) ** (1./BEXP)
           IF(SMCWTD.GT.SMCEQDEEP)THEN
               WTD = MIN( ( SMCWTD*DZSNSO(NSOIL) &
                 - SMCEQDEEP*ZSOIL0(NSOIL) + SMCMAX*(ZSOIL0(NSOIL)-DZSNSO(NSOIL)) ) / &
                     ( SMCMAX-SMCEQDEEP ) , ZSOIL0(NSOIL) )
               RECH = -(WTDOLD-WTD) * (SMCMAX-SMCEQDEEP)
           ELSE
               RECH = -(WTDOLD-(ZSOIL0(NSOIL)-DZSNSO(NSOIL))) * (SMCMAX-SMCEQDEEP)
               WTDOLD=ZSOIL0(NSOIL)-DZSNSO(NSOIL)

               DZUP=(SMCEQDEEP-SMCWTD)*DZSNSO(NSOIL)/(SMCMAX-SMCEQDEEP)
               WTD=WTDOLD-DZUP
               RECH = RECH - (SMCMAX-SMCEQDEEP)*DZUP
               SMCWTD=SMCEQDEEP
           ENDIF

         
         ENDIF

IF(IWTD.LT.NSOIL)SMCWTD=SMCMAX

END  SUBROUTINE SHALLOWWATERTABLE




  SUBROUTINE CARBON (NSNOW  ,NSOIL  ,VEGTYP ,NROOT  ,DT     ,ZSOIL  , & 
                     DZSNSO ,STC    ,SMC    ,TV     ,TG     ,PSN    , & 
                     FOLN   ,SMCMAX ,BTRAN  ,APAR   ,FVEG   ,IGS    , & 
                     TROOT  ,IST    ,LAT    ,ILOC   ,JLOC   ,ISURBAN, & 
                     LFMASS ,RTMASS ,STMASS ,WOOD   ,STBLCP ,FASTCP , & 
                     GPP    ,NPP    ,NEE    ,AUTORS ,HETERS ,TOTSC  , & 
                     TOTLB  ,XLAI   ,XSAI   )                   

  USE NOAHMP_VEG_PARAMETERS

      IMPLICIT NONE



  INTEGER                        , INTENT(IN) :: ILOC   
  INTEGER                        , INTENT(IN) :: JLOC   
  INTEGER                        , INTENT(IN) :: VEGTYP 
  INTEGER                        , INTENT(IN) :: ISURBAN
  INTEGER                        , INTENT(IN) :: NSNOW  
  INTEGER                        , INTENT(IN) :: NSOIL  
  INTEGER                        , INTENT(IN) :: NROOT  
  REAL                           , INTENT(IN) :: LAT    
  REAL                           , INTENT(IN) :: DT     
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL  
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO 
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SMC    
  REAL                           , INTENT(IN) :: TV     
  REAL                           , INTENT(IN) :: TG     
  REAL                           , INTENT(IN) :: FOLN   
  REAL                           , INTENT(IN) :: SMCMAX 
  REAL                           , INTENT(IN) :: BTRAN  
  REAL                           , INTENT(IN) :: PSN    
  REAL                           , INTENT(IN) :: APAR   
  REAL                           , INTENT(IN) :: IGS    
  REAL                           , INTENT(IN) :: FVEG   
  REAL                           , INTENT(IN) :: TROOT  
  INTEGER                        , INTENT(IN) :: IST    



  REAL                        , INTENT(INOUT) :: LFMASS 
  REAL                        , INTENT(INOUT) :: RTMASS 
  REAL                        , INTENT(INOUT) :: STMASS 
  REAL                        , INTENT(INOUT) :: WOOD   
  REAL                        , INTENT(INOUT) :: STBLCP 
  REAL                        , INTENT(INOUT) :: FASTCP 



  REAL                          , INTENT(OUT) :: GPP    
  REAL                          , INTENT(OUT) :: NPP    
  REAL                          , INTENT(OUT) :: NEE    
  REAL                          , INTENT(OUT) :: AUTORS 
  REAL                          , INTENT(OUT) :: HETERS 
  REAL                          , INTENT(OUT) :: TOTSC  
  REAL                          , INTENT(OUT) :: TOTLB  
  REAL                          , INTENT(OUT) :: XLAI   
  REAL                          , INTENT(OUT) :: XSAI   




  INTEGER :: J         
  REAL    :: WROOT     
  REAL    :: WSTRES    
  REAL    :: LAPM      


   IF ( ( VEGTYP == ISWATER ) .OR. ( VEGTYP == ISBARREN ) .OR. ( VEGTYP == ISSNOW ) .or. (VEGTYP == ISURBAN) ) THEN
      XLAI   = 0.
      XSAI   = 0.
      GPP    = 0.
      NPP    = 0.
      NEE    = 0.
      AUTORS = 0.
      HETERS = 0.
      TOTSC  = 0.
      TOTLB  = 0.
      LFMASS = 0.
      RTMASS = 0.
      STMASS = 0.
      WOOD   = 0.
      STBLCP = 0.
      FASTCP = 0.

      RETURN
   END IF

      LAPM       = SLA(VEGTYP) / 1000.   



      WSTRES  = 1.- BTRAN

      WROOT  = 0.
      DO J=1,NROOT
        WROOT = WROOT + SMC(J)/SMCMAX *  DZSNSO(J) / (-ZSOIL(NROOT))
      ENDDO

  CALL CO2FLUX (NSNOW  ,NSOIL  ,VEGTYP ,IGS    ,DT     , & 
                DZSNSO ,STC    ,PSN    ,TROOT  ,TV     , & 
                WROOT  ,WSTRES ,FOLN   ,LAPM   ,         & 
                LAT    ,ILOC   ,JLOC   ,FVEG   ,         & 
                XLAI   ,XSAI   ,LFMASS ,RTMASS ,STMASS , & 
                FASTCP ,STBLCP ,WOOD   ,                 & 
                GPP    ,NPP    ,NEE    ,AUTORS ,HETERS , & 
                TOTSC  ,TOTLB  )                           




  END SUBROUTINE CARBON

  SUBROUTINE CO2FLUX (NSNOW  ,NSOIL  ,VEGTYP ,IGS    ,DT     , & 
                      DZSNSO ,STC    ,PSN    ,TROOT  ,TV     , & 
                      WROOT  ,WSTRES ,FOLN   ,LAPM   ,         & 
                      LAT    ,ILOC   ,JLOC   ,FVEG   ,         & 
                      XLAI   ,XSAI   ,LFMASS ,RTMASS ,STMASS , & 
                      FASTCP ,STBLCP ,WOOD   ,                 & 
                      GPP    ,NPP    ,NEE    ,AUTORS ,HETERS , & 
                      TOTSC  ,TOTLB  )                           



  USE NOAHMP_VEG_PARAMETERS

  IMPLICIT NONE




  INTEGER                        , INTENT(IN) :: ILOC   
  INTEGER                        , INTENT(IN) :: JLOC   
  INTEGER                        , INTENT(IN) :: VEGTYP 
  INTEGER                        , INTENT(IN) :: NSNOW  
  INTEGER                        , INTENT(IN) :: NSOIL  
  REAL                           , INTENT(IN) :: DT     
  REAL                           , INTENT(IN) :: LAT    
  REAL                           , INTENT(IN) :: IGS    
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO 
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    
  REAL                           , INTENT(IN) :: PSN    
  REAL                           , INTENT(IN) :: TROOT  
  REAL                           , INTENT(IN) :: TV     
  REAL                           , INTENT(IN) :: WROOT  
  REAL                           , INTENT(IN) :: WSTRES 
  REAL                           , INTENT(IN) :: FOLN   
  REAL                           , INTENT(IN) :: LAPM   
  REAL                           , INTENT(IN) :: FVEG   



  REAL                        , INTENT(INOUT) :: XLAI   
  REAL                        , INTENT(INOUT) :: XSAI   
  REAL                        , INTENT(INOUT) :: LFMASS 
  REAL                        , INTENT(INOUT) :: RTMASS 
  REAL                        , INTENT(INOUT) :: STMASS 
  REAL                        , INTENT(INOUT) :: FASTCP 
  REAL                        , INTENT(INOUT) :: STBLCP 
  REAL                        , INTENT(INOUT) :: WOOD   



  REAL                          , INTENT(OUT) :: GPP    
  REAL                          , INTENT(OUT) :: NPP    
  REAL                          , INTENT(OUT) :: NEE    
  REAL                          , INTENT(OUT) :: AUTORS 
  REAL                          , INTENT(OUT) :: HETERS 
  REAL                          , INTENT(OUT) :: TOTSC  
  REAL                          , INTENT(OUT) :: TOTLB  



  REAL                   :: CFLUX    
  REAL                   :: LFMSMN   
  REAL                   :: RSWOOD   
  REAL                   :: RSLEAF   
  REAL                   :: RSROOT   
  REAL                   :: NPPL     
  REAL                   :: NPPR     
  REAL                   :: NPPW     
  REAL                   :: NPPS     
  REAL                   :: DIELF    

  REAL                   :: ADDNPPLF 
  REAL                   :: ADDNPPST 
  REAL                   :: CARBFX   
  REAL                   :: GRLEAF   
  REAL                   :: GRROOT   
  REAL                   :: GRWOOD   
  REAL                   :: GRSTEM   
  REAL                   :: LEAFPT   
  REAL                   :: LFDEL    
  REAL                   :: LFTOVR   
  REAL                   :: STTOVR   
  REAL                   :: WDTOVR   
  REAL                   :: RSSOIL   
  REAL                   :: RTTOVR   
  REAL                   :: STABLC   
  REAL                   :: WOODF    
  REAL                   :: NONLEF   
  REAL                   :: ROOTPT   
  REAL                   :: WOODPT   
  REAL                   :: STEMPT   
  REAL                   :: RESP     
  REAL                   :: RSSTEM   

  REAL                   :: FSW      
  REAL                   :: FST      
  REAL                   :: FNF      
  REAL                   :: TF       
  REAL                   :: RF       
  REAL                   :: STDEL
  REAL                   :: STMSMN
  REAL                   :: SAPM     
  REAL                   :: DIEST

  REAL                   :: BF       
  REAL                   :: RSWOODC  
  REAL                   :: STOVRC   
  REAL                   :: RSDRYC   
  REAL                   :: RTOVRC   
  REAL                   :: WSTRC    
  REAL                   :: LAIMIN   
  REAL                   :: XSAMIN   
  REAL                   :: SC
  REAL                   :: SD
  REAL                   :: VEGFRAC



  real :: r,x
          r(x) = exp(0.08*(x-298.16))



    RTOVRC  = 2.0E-8        
    RSDRYC  = 40.0          
    RSWOODC = 3.0E-10       
    BF      = 0.90          
    WSTRC   = 100.0
    LAIMIN  = 0.05   
    XSAMIN  = 0.01

    SAPM    = 3.*0.001      
    LFMSMN  = laimin/lapm
    STMSMN  = xsamin/sapm




     IF(IGS .EQ. 0.) THEN
       RF = 0.5
     ELSE
       RF = 1.0
     ENDIF
            
     FNF     = MIN( FOLN/MAX(1.E-06,FOLNMX(VEGTYP)), 1.0 )
     TF      = ARM(VEGTYP)**( (TV-298.16)/10. )
     RESP    = RMF25(VEGTYP) * TF * FNF * XLAI * RF * (1.-WSTRES) 
     RSLEAF  = MIN(LFMASS/DT,RESP*12.e-6)                         
     
     RSROOT  = RMR25(VEGTYP)*(RTMASS*1E-3)*TF *RF* 12.e-6         
     RSSTEM  = RMS25(VEGTYP)*(STMASS*1E-3)*TF *RF* 12.e-6         
     RSWOOD  = RSWOODC * R(TV) * WOOD*WDPOOL(VEGTYP)




     CARBFX  = PSN * 12.e-6              



     LEAFPT = EXP(0.01*(1.-EXP(0.75*XLAI))*XLAI)
     IF(VEGTYP ==EBLFOREST) LEAFPT = EXP(0.01*(1.-EXP(0.50*XLAI))*XLAI)

     NONLEF = 1.0 - LEAFPT
     STEMPT = XLAI/10.0
     LEAFPT = LEAFPT - STEMPT



     IF(WOOD.GT.0) THEN
        WOODF = (1.-EXP(-BF*(WRRAT(VEGTYP)*RTMASS/WOOD))/BF)*WDPOOL(VEGTYP)
     ELSE
        WOODF = 0.
     ENDIF

     ROOTPT = NONLEF*(1.-WOODF)
     WOODPT = NONLEF*WOODF



     LFTOVR = LTOVRC(VEGTYP)*1.E-6*LFMASS
     STTOVR = LTOVRC(VEGTYP)*1.E-6*STMASS
     RTTOVR = RTOVRC*RTMASS
     WDTOVR = 9.5E-10*WOOD




     SC  = EXP(-0.3*MAX(0.,TV-TDLEF(VEGTYP))) * (LFMASS/120.) 
     SD  = EXP((WSTRES-1.)*WSTRC)
     DIELF = LFMASS*1.E-6*(DILEFW(VEGTYP) * SD + DILEFC(VEGTYP)*SC)
     DIEST = STMASS*1.E-6*(DILEFW(VEGTYP) * SD + DILEFC(VEGTYP)*SC)



     GRLEAF = MAX(0.0,FRAGR(VEGTYP)*(LEAFPT*CARBFX - RSLEAF))
     GRSTEM = MAX(0.0,FRAGR(VEGTYP)*(STEMPT*CARBFX - RSSTEM))
     GRROOT = MAX(0.0,FRAGR(VEGTYP)*(ROOTPT*CARBFX - RSROOT))
     GRWOOD = MAX(0.0,FRAGR(VEGTYP)*(WOODPT*CARBFX - RSWOOD))



     ADDNPPLF = MAX(0.,LEAFPT*CARBFX - GRLEAF-RSLEAF)
     ADDNPPST = MAX(0.,STEMPT*CARBFX - GRSTEM-RSSTEM)
     IF(TV.LT.TMIN(VEGTYP)) ADDNPPLF =0.
     IF(TV.LT.TMIN(VEGTYP)) ADDNPPST =0.




     LFDEL = (LFMASS - LFMSMN)/DT
     STDEL = (STMASS - STMSMN)/DT
     DIELF = MIN(DIELF,LFDEL+ADDNPPLF-LFTOVR)
     DIEST = MIN(DIEST,STDEL+ADDNPPST-STTOVR)



     NPPL   = MAX(ADDNPPLF,-LFDEL)
     NPPS   = MAX(ADDNPPST,-STDEL)
     NPPR   = ROOTPT*CARBFX - RSROOT - GRROOT
     NPPW   = WOODPT*CARBFX - RSWOOD - GRWOOD



     LFMASS = LFMASS + (NPPL-LFTOVR-DIELF)*DT
     STMASS = STMASS + (NPPS-STTOVR-DIEST)*DT   
     RTMASS = RTMASS + (NPPR-RTTOVR)      *DT

     IF(RTMASS.LT.0.0) THEN
           RTTOVR = NPPR
           RTMASS = 0.0
     ENDIF
     WOOD = (WOOD+(NPPW-WDTOVR)*DT)*WDPOOL(VEGTYP)



     FASTCP = FASTCP + (RTTOVR+LFTOVR+STTOVR+WDTOVR+DIELF)*DT

     FST = 2.0**( (STC(1)-283.16)/10. )
     FSW = WROOT / (0.20+WROOT) * 0.23 / (0.23+WROOT)
     RSSOIL = FSW * FST * MRP(VEGTYP)* MAX(0.,FASTCP*1.E-3)*12.E-6

     STABLC = 0.1*RSSOIL
     FASTCP = FASTCP - (RSSOIL + STABLC)*DT
     STBLCP = STBLCP + STABLC*DT



     CFLUX  = - CARBFX + RSLEAF + RSROOT + RSWOOD + RSSTEM &
              + RSSOIL + GRLEAF + GRROOT + GRWOOD                



     GPP    = CARBFX                                             
     NPP    = NPPL + NPPW + NPPR                                 
     AUTORS = RSROOT + RSWOOD  + RSLEAF +  &                     
              GRLEAF + GRROOT + GRWOOD                           
     HETERS = RSSOIL                                             
     NEE    = (AUTORS + HETERS - GPP)*44./12.                    
     TOTSC  = FASTCP + STBLCP                                    
     TOTLB  = LFMASS + RTMASS + WOOD                             



     XLAI    = MAX(LFMASS*LAPM,LAIMIN)
     XSAI    = MAX(STMASS*SAPM,XSAMIN)
    
  END SUBROUTINE CO2FLUX


  SUBROUTINE BVOCFLUX(VOCFLX,  VEGTYP,  VEGFRAC,  APAR,   TV )
    use NOAHMP_VEG_PARAMETERS , ONLY : SLAREA, EPS



      implicit none























  integer                     ,INTENT(IN) :: vegtyp  
  real                        ,INTENT(IN) :: vegfrac 
  real                        ,INTENT(IN) :: apar    
  real                        ,INTENT(IN) :: tv      


  real                        ,INTENT(OUT) :: vocflx(5) 



  real, parameter :: R      = 8.314    
  real, parameter :: alpha  = 0.0027   
  real, parameter :: cl1    = 1.066    
  real, parameter :: ct1    = 95000.0  
  real, parameter :: ct2    = 230000.0 
  real, parameter :: ct3    = 0.961    
  real, parameter :: tm     = 314.0    
  real, parameter :: tstd   = 303.0    
  real, parameter :: bet    = 0.09     

  integer ivoc        
  integer ityp        
  real epsilon(5)
  real gamma(5)
  real density
  real elai
  real par,cl,reciprod,ct



    do ivoc = 1, 5
    epsilon(ivoc) = eps(VEGTYP,ivoc)
    end do



      reciprod = 1. / (R * tv * tstd)
      ct = exp(ct1 * (tv - tstd) * reciprod) / &
           (ct3 + exp(ct2 * (tv - tm) * reciprod))

      par = apar * 4.6 
      cl  = alpha * cl1 * par * (1. + alpha * alpha * par * par)**(-0.5)

   gamma(1) = cl * ct 

   do ivoc = 2, 5
   gamma(ivoc) = exp(bet * (tv - tstd))
   end do





   elai    = max(0.0,-6.5/2.5*alog((1.-vegfrac)))
   density = elai / (slarea(VEGTYP) * 0.5)



   do ivoc = 1, 5
   vocflx(ivoc) = epsilon(ivoc) * gamma(ivoc) * density
   end do

   end subroutine bvocflux



  SUBROUTINE REDPRM (VEGTYP,SOILTYP,SLOPETYP,ZSOIL,NSOIL,ISURBAN)
   


    IMPLICIT NONE




































    INTEGER, PARAMETER     :: MAX_SOILTYP=30,MAX_VEGTYP=30


    INTEGER, INTENT(IN)    :: VEGTYP
    INTEGER, INTENT(IN)    :: ISURBAN

    INTEGER, INTENT(IN)    :: SOILTYP

    INTEGER, INTENT(IN)    :: SLOPETYP

    INTEGER, INTENT(IN)    :: NSOIL

    REAL,DIMENSION(NSOIL),INTENT(IN) :: ZSOIL


    REAL    :: REFDK
    REAL    :: REFKDT
    REAL    :: FRZK
    REAL                   :: FRZFACT
    INTEGER                :: I
    CHARACTER(len=256)     :: message


    IF (SOILTYP .gt. SLCATS) THEN
       call wrf_message('SOILTYP must be less than SLCATS:')
       write(message, '("SOILTYP = ", I6, ";    SLCATS = ", I6)') SOILTYP, SLCATS
       call wrf_message(trim(message))
       call wrf_error_fatal3("<stdin>",9162,&
'REDPRM: Error: too many input soil types')
    END IF
    IF (VEGTYP .gt. LUCATS) THEN
       call wrf_message('VEGTYP must be less than LUCATS:')
       write(message, '("VEGTYP = ", I6, ";    LUCATS = ", I6)') VEGTYP, LUCATS
       call wrf_message(trim(message))
       call wrf_error_fatal3("<stdin>",9169,&
'Error: too many input landuse types')
    END IF




    CSOIL  = CSOIL_DATA
    BEXP   = BB (SOILTYP)
    DKSAT  = SATDK (SOILTYP)
    DWSAT  = SATDW (SOILTYP)
    F1     = F11 (SOILTYP)
    PSISAT = SATPSI (SOILTYP)
    QUARTZ = QTZ (SOILTYP)
    SMCDRY = DRYSMC (SOILTYP)
    SMCMAX = MAXSMC (SOILTYP)
    SMCREF = REFSMC (SOILTYP)
    SMCWLT = WLTSMC (SOILTYP)

    IF(VEGTYP==ISURBAN)THEN
       SMCMAX = 0.45 
       SMCREF = 0.42 
       SMCWLT = 0.40 
       SMCDRY = 0.40 
       CSOIL  = 3.E6
    ENDIF




    ZBOT   = ZBOT_DATA
    CZIL   = CZIL_DATA

    FRZK   = FRZK_DATA
    REFDK  = REFDK_DATA
    REFKDT = REFKDT_DATA
    KDT    = REFKDT * DKSAT / REFDK
    SLOPE  = SLOPE_DATA (SLOPETYP)



    if(SOILTYP /= 14) then
      FRZFACT = (SMCMAX / SMCREF) * (0.412 / 0.468)
      FRZX = FRZK * FRZFACT
    end if





    
    TOPT = TOPT_DATA
    RGL = RGLTBL (VEGTYP)
    RSMAX = RSMAX_DATA
    RSMIN = RSTBL (VEGTYP)
    HS = HSTBL (VEGTYP)
    NROOT = NROTBL (VEGTYP)

    IF(VEGTYP==ISURBAN)THEN
       RSMIN=400.0
    ENDIF




    IF (NROOT .gt. NSOIL) THEN
       WRITE (*,*) 'Warning: too many root layers'
       write (*,*) 'NROOT = ', nroot
       write (*,*) 'NSOIL = ', nsoil
       call wrf_error_fatal3("<stdin>",9238,&
"STOP in Noah-MP")
    END IF



  END  SUBROUTINE REDPRM


  subroutine noahmp_options(idveg     ,iopt_crs  ,iopt_btr  ,iopt_run  ,iopt_sfc  ,iopt_frz , & 
                             iopt_inf  ,iopt_rad  ,iopt_alb  ,iopt_snf  ,iopt_tbot, iopt_stc )

  implicit none

  INTEGER,  INTENT(IN) :: idveg     
  INTEGER,  INTENT(IN) :: iopt_crs  
  INTEGER,  INTENT(IN) :: iopt_btr  
  INTEGER,  INTENT(IN) :: iopt_run  
  INTEGER,  INTENT(IN) :: iopt_sfc  
  INTEGER,  INTENT(IN) :: iopt_frz  
  INTEGER,  INTENT(IN) :: iopt_inf  
  INTEGER,  INTENT(IN) :: iopt_rad  
  INTEGER,  INTENT(IN) :: iopt_alb  
  INTEGER,  INTENT(IN) :: iopt_snf  
  INTEGER,  INTENT(IN) :: iopt_tbot 

  INTEGER,  INTENT(IN) :: iopt_stc  
                                    



  dveg = idveg
  
  opt_crs  = iopt_crs  
  opt_btr  = iopt_btr  
  opt_run  = iopt_run  
  opt_sfc  = iopt_sfc  
  opt_frz  = iopt_frz  
  opt_inf  = iopt_inf  
  opt_rad  = iopt_rad  
  opt_alb  = iopt_alb  
  opt_snf  = iopt_snf  
  opt_tbot = iopt_tbot 
  opt_stc  = iopt_stc
  
  end subroutine noahmp_options
 
END MODULE NOAHMP_ROUTINES


MODULE MODULE_SF_NOAHMPLSM

  USE NOAHMP_ROUTINES
  USE NOAHMP_GLOBALS
  USE NOAHMP_VEG_PARAMETERS

END MODULE MODULE_SF_NOAHMPLSM
