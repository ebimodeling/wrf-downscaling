MODULE module_sf_noahmpdrv


  USE module_sf_noahmplsm
  USE module_sf_urban
  USE module_sf_noahdrv, ONLY : SOIL_VEG_GEN_PARM
  USE module_sf_noah_seaice
  USE module_sf_noahmp_glacier
  USE MODULE_RA_GFDLETA, ONLY: CAL_MON_DAY
  USE module_sf_noahmp_groundwater, ONLY : LATERALFLOW






CONTAINS

  SUBROUTINE noahmplsm(ITIMESTEP,        YR,   JULIAN,   COSZIN,   XLATIN,  & 
                  DZ8W,       DT,       DZS,    NSOIL,       DX,            & 
	        IVGTYP,   ISLTYP,    VEGFRA,   VEGMAX,      TMN,            & 
		 XLAND,     XICE,XICE_THRES,    ISICE,  ISURBAN,                      & 
                 IDVEG, IOPT_CRS,  IOPT_BTR, IOPT_RUN, IOPT_SFC, IOPT_FRZ,  & 
              IOPT_INF, IOPT_RAD,  IOPT_ALB, IOPT_SNF,IOPT_TBOT, IOPT_STC,  & 
               IZ0TLND,                                                     & 
                   T3D,     QV3D,     U_PHY,    V_PHY,   SWDOWN,      GLW,  & 
                 P8W3D,   RAINBL,                                           & 
                   TSK,      HFX,      QFX,        LH,   GRDFLX,    SMSTAV, & 
                SMSTOT,SFCRUNOFF, UDRUNOFF,    ALBEDO,    SNOWC,     SMOIS, & 
		  SH2O,     TSLB,     SNOW,     SNOWH,   CANWAT,    ACSNOM, & 
		ACSNOW,    EMISS,     QSFC,                                 & 
               ISNOWXY,     TVXY,     TGXY,  CANICEXY, CANLIQXY,     EAHXY, & 
	         TAHXY,     CMXY,     CHXY,    FWETXY, SNEQVOXY,  ALBOLDXY, & 
               QSNOWXY, WSLAKEXY,    ZWTXY,      WAXY,     WTXY,    TSNOXY, & 
	       ZSNSOXY,  SNICEXY,  SNLIQXY,  LFMASSXY, RTMASSXY,  STMASSXY, & 
	        WOODXY, STBLCPXY, FASTCPXY,    XLAIXY,   XSAIXY,   TAUSSXY, & 
	       SMOISEQ, SMCWTDXY,DEEPRECHXY,   RECHXY,                      & 
	        T2MVXY,   T2MBXY,    Q2MVXY,   Q2MBXY,                      & 
	        TRADXY,    NEEXY,    GPPXY,     NPPXY,   FVEGXY,   RUNSFXY, & 
	       RUNSBXY,   ECANXY,   EDIRXY,   ETRANXY,    FSAXY,    FIRAXY, & 
	        APARXY,    PSNXY,    SAVXY,     SAGXY,  RSSUNXY,   RSSHAXY, & 
		BGAPXY,   WGAPXY,    TGVXY,     TGBXY,    CHVXY,     CHBXY, & 
		 SHGXY,    SHCXY,    SHBXY,     EVGXY,    EVBXY,     GHVXY, & 
		 GHBXY,    IRGXY,    IRCXY,     IRBXY,     TRXY,     EVCXY, & 
              CHLEAFXY,   CHUCXY,   CHV2XY,    CHB2XY,                      & 



               ids,ide,  jds,jde,  kds,kde,                    &
               ims,ime,  jms,jme,  kms,kme,                    &
               its,ite,  jts,jte,  kts,kte                     )

    IMPLICIT NONE




    INTEGER,                                         INTENT(IN   ) ::  ITIMESTEP 
    INTEGER,                                         INTENT(IN   ) ::  YR        
    REAL,                                            INTENT(IN   ) ::  JULIAN    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  COSZIN    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  XLATIN    
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  DZ8W      
    REAL,                                            INTENT(IN   ) ::  DT        
    REAL,    DIMENSION(1:nsoil),                     INTENT(IN   ) ::  DZS       
    INTEGER,                                         INTENT(IN   ) ::  NSOIL     
    REAL,                                            INTENT(IN   ) ::  DX        
    INTEGER, DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  IVGTYP    
    INTEGER, DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  ISLTYP    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  VEGFRA    
    REAL,    DIMENSION( ims:ime ,         jms:jme ), INTENT(IN   ) ::  VEGMAX    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  TMN       
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  XLAND     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  XICE      
    REAL,                                            INTENT(IN   ) ::  XICE_THRES
    INTEGER,                                         INTENT(IN   ) ::  ISICE     
    INTEGER,                                         INTENT(IN   ) ::  ISURBAN   
    INTEGER,                                         INTENT(IN   ) ::  IDVEG     
    INTEGER,                                         INTENT(IN   ) ::  IOPT_CRS  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_BTR  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_RUN  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_SFC  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_FRZ  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_INF  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_RAD  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_ALB  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_SNF  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_TBOT 
    INTEGER,                                         INTENT(IN   ) ::  IOPT_STC  
    INTEGER,                                         INTENT(IN   ) ::  IZ0TLND   
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  T3D       
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  QV3D      
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  U_PHY     
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  V_PHY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SWDOWN    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  GLW       
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  P8W3D     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  RAINBL    






    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  TSK       
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  HFX       
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  QFX       
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  LH        
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  GRDFLX    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SMSTAV    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SMSTOT    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SFCRUNOFF 
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  UDRUNOFF  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ALBEDO    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SNOWC     
    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(INOUT) ::  SMOIS     
    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(INOUT) ::  SH2O      
    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(INOUT) ::  TSLB      
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SNOW      
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SNOWH     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  CANWAT    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ACSNOM    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ACSNOW    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  EMISS     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  QSFC      



    INTEGER, DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ISNOWXY   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  TVXY      
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  TGXY      
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  CANICEXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  CANLIQXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  EAHXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  TAHXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  CMXY      
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  CHXY      
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  FWETXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SNEQVOXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ALBOLDXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  QSNOWXY   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  WSLAKEXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ZWTXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  WAXY      
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  WTXY      
    REAL,    DIMENSION( ims:ime,-2:0,     jms:jme ), INTENT(INOUT) ::  TSNOXY    
    REAL,    DIMENSION( ims:ime,-2:NSOIL, jms:jme ), INTENT(INOUT) ::  ZSNSOXY   
    REAL,    DIMENSION( ims:ime,-2:0,     jms:jme ), INTENT(INOUT) ::  SNICEXY   
    REAL,    DIMENSION( ims:ime,-2:0,     jms:jme ), INTENT(INOUT) ::  SNLIQXY   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  LFMASSXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  RTMASSXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  STMASSXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  WOODXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  STBLCPXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  FASTCPXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  XLAIXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  XSAIXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  TAUSSXY   
    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(INOUT) ::  SMOISEQ   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SMCWTDXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  DEEPRECHXY 
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  RECHXY    



    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  T2MVXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  T2MBXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  Q2MVXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  Q2MBXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  TRADXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  NEEXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  GPPXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  NPPXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  FVEGXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  RUNSFXY   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  RUNSBXY   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  ECANXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  EDIRXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  ETRANXY   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  FSAXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  FIRAXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  APARXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  PSNXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  SAVXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  SAGXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  RSSUNXY   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  RSSHAXY   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  BGAPXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  WGAPXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  TGVXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  TGBXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  CHVXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  CHBXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  SHGXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  SHCXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  SHBXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  EVGXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  EVBXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  GHVXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  GHBXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  IRGXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  IRCXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  IRBXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  TRXY      
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  EVCXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  CHLEAFXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  CHUCXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  CHV2XY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  CHB2XY    
    INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &  
         &                           ims,ime, jms,jme, kms,kme,  &  
         &                           its,ite, jts,jte, kts,kte      






    REAL                                :: COSZ         
    REAL                                :: LAT          
    REAL                                :: Z_ML         
    INTEGER                             :: VEGTYP       
    INTEGER                             :: SOILTYP      
    REAL                                :: FVEG         
    REAL                                :: FVGMAX       
    REAL                                :: TBOT         
    REAL                                :: T_ML         
    REAL                                :: Q_ML         
    REAL                                :: U_ML         
    REAL                                :: V_ML         
    REAL                                :: SWDN         
    REAL                                :: LWDN         
    REAL                                :: P_ML         
    REAL                                :: PSFC         
    REAL                                :: PRCP         



    REAL                                :: FSH          
    REAL                                :: SSOIL        
    REAL                                :: SALB         
    REAL                                :: FSNO         
    REAL,   DIMENSION( 1:NSOIL)         :: SMCEQ        
    REAL,   DIMENSION( 1:NSOIL)         :: SMC          
    REAL,   DIMENSION( 1:NSOIL)         :: SMH2O        
    REAL,   DIMENSION(-2:NSOIL)         :: STC          
    REAL                                :: SWE          
    REAL                                :: SNDPTH       
    REAL                                :: EMISSI       
    REAL                                :: QSFC1D       



    INTEGER                             :: ISNOW        
    REAL                                :: TV           
    REAL                                :: TG           
    REAL                                :: CANICE       
    REAL                                :: CANLIQ       
    REAL                                :: EAH          
    REAL                                :: TAH          
    REAL                                :: CM           
    REAL                                :: CH           
    REAL                                :: FWET         
    REAL                                :: SNEQVO       
    REAL                                :: ALBOLD       
    REAL                                :: QSNOW        
    REAL                                :: WSLAKE       
    REAL                                :: ZWT          
    REAL                                :: WA           
    REAL                                :: WT           
    REAL                                :: SMCWTD       
    REAL                                :: DEEPRECH     
    REAL                                :: RECH         
    REAL, DIMENSION(-2:NSOIL)           :: ZSNSO        
    REAL, DIMENSION(-2:              0) :: SNICE        
    REAL, DIMENSION(-2:              0) :: SNLIQ        
    REAL                                :: LFMASS       
    REAL                                :: RTMASS       
    REAL                                :: STMASS       
    REAL                                :: WOOD         
    REAL                                :: STBLCP       
    REAL                                :: FASTCP       
    REAL                                :: PLAI         
    REAL                                :: PSAI         
    REAL                                :: TAUSS        



    REAL                                :: T2MV         
    REAL                                :: T2MB         
    REAL                                :: Q2MV         
    REAL                                :: Q2MB         
    REAL                                :: TRAD         
    REAL                                :: NEE          
    REAL                                :: GPP          
    REAL                                :: NPP          
    REAL                                :: FVEGMP       
    REAL                                :: RUNSF        
    REAL                                :: RUNSB        
    REAL                                :: ECAN         
    REAL                                :: ETRAN        
    REAL                                :: ESOIL        
    REAL                                :: FSA          
    REAL                                :: FIRA         
    REAL                                :: APAR         
    REAL                                :: PSN          
    REAL                                :: SAV          
    REAL                                :: SAG          
    REAL                                :: RSSUN        
    REAL                                :: RSSHA        
    REAL                                :: BGAP         
    REAL                                :: WGAP         
    REAL                                :: TGV          
    REAL                                :: TGB          
    REAL                                :: CHV          
    REAL                                :: CHB          
    REAL                                :: IRC          
    REAL                                :: IRG          
    REAL                                :: SHC          
    REAL                                :: SHG          
    REAL                                :: EVG          
    REAL                                :: GHV          
    REAL                                :: IRB          
    REAL                                :: SHB          
    REAL                                :: EVB          
    REAL                                :: GHB          
    REAL                                :: TR           
    REAL                                :: EVC          
    REAL                                :: CHLEAF       
    REAL                                :: CHUC         
    REAL                                :: CHV2         
    REAL                                :: CHB2         



    REAL                                :: FPICE        
    REAL                                :: FCEV         
    REAL                                :: FGEV         
    REAL                                :: FCTR         
    REAL                                :: QSNBOT       
    REAL                                :: PONDING      
    REAL                                :: PONDING1     
    REAL                                :: PONDING2     



    REAL                                :: FSR          
    REAL, DIMENSION(-2:0)               :: FICEOLD      
    REAL                                :: CO2PP        
    REAL                                :: O2PP         
    REAL, DIMENSION(1:NSOIL)            :: ZSOIL        
    REAL                                :: FOLN         

    REAL                                :: QC           
    REAL                                :: PBLH         
    REAL                                :: DZ8W1D       

    INTEGER                             :: I
    INTEGER                             :: J
    INTEGER                             :: K
    INTEGER                             :: ICE
    INTEGER                             :: SLOPETYP
    LOGICAL                             :: IPRINT

    INTEGER                             :: ISC          
    INTEGER                             :: IST          
    INTEGER                             :: YEARLEN

    INTEGER, PARAMETER                  :: NSNOW = 3    
    REAL, PARAMETER                     :: CO2 = 395.e-06
    REAL, PARAMETER                     :: O2 = 0.209
    REAL, PARAMETER                     :: undefined_value = -1.E36





    CALL NOAHMP_OPTIONS(IDVEG  ,IOPT_CRS  ,IOPT_BTR  ,IOPT_RUN  ,IOPT_SFC  ,IOPT_FRZ , &
                     IOPT_INF  ,IOPT_RAD  ,IOPT_ALB  ,IOPT_SNF  ,IOPT_TBOT, IOPT_STC )

    IPRINT    =  .false.                     

    YEARLEN = 365                            
    if (mod(YR,4) == 0) then
       YEARLEN = 366
       if (mod(YR,100) == 0) then
          YEARLEN = 365
          if (mod(YR,400) == 0) then
             YEARLEN = 366
          endif
       endif
    endif

    ZSOIL(1) = -DZS(1)                    
    DO K = 2, NSOIL
       ZSOIL(K) = -DZS(K) + ZSOIL(K-1)
    END DO

    JLOOP : DO J=jts,jte

       IF(ITIMESTEP == 1)THEN
          DO I=its,ite
             IF((XLAND(I,J)-1.5) >= 0.) THEN    
                IF(XICE(I,J) == 1. .AND. IPRINT) PRINT *,' sea-ice at water point, I=',I,'J=',J
                SMSTAV(I,J) = 1.0
                SMSTOT(I,J) = 1.0
                DO K = 1, NSOIL
                   SMOIS(I,K,J) = 1.0
                    TSLB(I,K,J) = 273.16
                ENDDO
             ELSE
                IF(XICE(I,J) == 1.) THEN        
                   SMSTAV(I,J) = 1.0
                   SMSTOT(I,J) = 1.0
                   DO K = 1, NSOIL
                      SMOIS(I,K,J) = 1.0
                   ENDDO
                ENDIF
             ENDIF
          ENDDO
       ENDIF                                                               



   ILOOP : DO I = its, ite

    IF (XICE(I,J) >= XICE_THRES) THEN
       ICE = 1                            
    ELSE IF ( IVGTYP(I,J) == ISICE ) THEN
       ICE = -1                           
    ELSE
       ICE=0                              
    ENDIF
    
    IF((XLAND(I,J)-1.5) >= 0.) CYCLE ILOOP   

    IF ( ICE == 1) THEN

       SH2O  (i,1:NSOIL,j) = 1.0
       XLAIXY(i,j)         = 0.01

       CYCLE ILOOP 

    ELSE





       COSZ   = COSZIN  (I,J)                         
       LAT    = XLATIN  (I,J)                         
       Z_ML   = 0.5*DZ8W(I,1,J)                       
       VEGTYP = IVGTYP(I,J)                           
       SOILTYP= ISLTYP(I,J)                           
       FVEG   = VEGFRA(I,J)/100.                      
       FVGMAX = VEGMAX (I,J)/100.                     
       TBOT = TMN(I,J)                                
       T_ML   = T3D(I,1,J)                            
       Q_ML   = QV3D(I,1,J)/(1.0+QV3D(I,1,J))         
       U_ML   = U_PHY(I,1,J)                          
       V_ML   = V_PHY(I,1,J)                          
       SWDN   = SWDOWN(I,J)                           
       LWDN   = GLW(I,J)                              
       P_ML   =(P8W3D(I,KTS+1,J)+P8W3D(I,KTS,J))*0.5  
	                                              
       PSFC   = P8W3D(I,1,J)                          
       PRCP   = RAINBL(I,J)/DT                        



       ISNOW                 = ISNOWXY (I,J)                
       SMC  (      1:NSOIL)  = SMOIS   (I,      1:NSOIL,J)  
       SMH2O(      1:NSOIL)  = SH2O    (I,      1:NSOIL,J)  
       STC  (-NSNOW+1:    0) = TSNOXY  (I,-NSNOW+1:    0,J) 
       STC  (      1:NSOIL)  = TSLB    (I,      1:NSOIL,J)  
       SWE                   = SNOW    (I,J)                
       SNDPTH                = SNOWH   (I,J)                
       QSFC1D                = QSFC    (I,J)



       TV                    = TVXY    (I,J)                
       TG                    = TGXY    (I,J)                
       CANLIQ                = CANLIQXY(I,J)                
       CANICE                = CANICEXY(I,J)                
       EAH                   = EAHXY   (I,J)                
       TAH                   = TAHXY   (I,J)                
       CM                    = CMXY    (I,J)                
       CH                    = CHXY    (I,J)                
       FWET                  = FWETXY  (I,J)                
       SNEQVO                = SNEQVOXY(I,J)                
       ALBOLD                = ALBOLDXY(I,J)                
       QSNOW                 = QSNOWXY (I,J)                
       WSLAKE                = WSLAKEXY(I,J)                
       ZWT                   = ZWTXY   (I,J)                
       WA                    = WAXY    (I,J)                
       WT                    = WTXY    (I,J)                
       ZSNSO(-NSNOW+1:NSOIL) = ZSNSOXY (I,-NSNOW+1:NSOIL,J) 
       SNICE(-NSNOW+1:    0) = SNICEXY (I,-NSNOW+1:    0,J) 
       SNLIQ(-NSNOW+1:    0) = SNLIQXY (I,-NSNOW+1:    0,J) 
       LFMASS                = LFMASSXY(I,J)                
       RTMASS                = RTMASSXY(I,J)                
       STMASS                = STMASSXY(I,J)                
       WOOD                  = WOODXY  (I,J)                
       STBLCP                = STBLCPXY(I,J)                
       FASTCP                = FASTCPXY(I,J)                
       PLAI                  = XLAIXY  (I,J)                
       PSAI                  = XSAIXY  (I,J)                
       TAUSS                 = TAUSSXY (I,J)                
       SMCEQ(       1:NSOIL) = SMOISEQ (I,       1:NSOIL,J)
       SMCWTD                = SMCWTDXY(I,J)
       RECH                  = 0.
       DEEPRECH              = 0.



       FICEOLD = 0.0
       FICEOLD(ISNOW+1:0) = SNICEXY(I,ISNOW+1:0,J) &  
           /(SNICEXY(I,ISNOW+1:0,J)+SNLIQXY(I,ISNOW+1:0,J))
       CO2PP  = CO2 * P_ML                            
       O2PP   = O2  * P_ML                            
       FOLN   = 1.0                                   
       QC     = undefined_value                       
       PBLH   = undefined_value                       
       DZ8W1D = DZ8W (I,1,J)                          
       SLOPETYP  =  1                                 
       IST    = 1                                     
       ISC    = 4                                     


       IF(SOILTYP == 14 .AND. XICE(I,J) == 0.) THEN
          IF(IPRINT) PRINT *, ' SOIL TYPE FOUND TO BE WATER AT A LAND-POINT'
          IF(IPRINT) PRINT *, i,j,'RESET SOIL in surfce.F'
          SOILTYP = 7
       ENDIF

       IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == 31 .or. &
            IVGTYP(I,J) == 32 .or. IVGTYP(I,J) == 33) THEN
          VEGTYP = ISURBAN
       ENDIF
       IF(VEGTYP == 25) FVEG = 0.0                  
       IF(VEGTYP == 25) PLAI = 0.0 
       IF(VEGTYP == 26) FVEG = 0.0                  
       IF(VEGTYP == 26) PLAI = 0.0
       IF(VEGTYP == 27) FVEG = 0.0
       IF(VEGTYP == 27) PLAI = 0.0

       CALL REDPRM (VEGTYP,SOILTYP,SLOPETYP,ZSOIL,NSOIL,ISURBAN)

    IF ( ICE == -1 ) THEN


      CALL NOAHMP_OPTIONS_GLACIER(IDVEG  ,IOPT_CRS  ,IOPT_BTR  ,IOPT_RUN  ,IOPT_SFC  ,IOPT_FRZ , &
                      IOPT_INF  ,IOPT_RAD  ,IOPT_ALB  ,IOPT_SNF  ,IOPT_TBOT, IOPT_STC )
      
      TBOT = MIN(TBOT,263.15)                      
      CALL NOAHMP_GLACIER(       I,       J,    COSZ,   NSNOW,   NSOIL,      DT, & 
                               T_ML,    P_ML,    U_ML,    V_ML,    Q_ML,    SWDN, & 
                               PRCP,    LWDN,    TBOT,    Z_ML, FICEOLD,   ZSOIL, & 
                              QSNOW,  SNEQVO,  ALBOLD,      CM,      CH,   ISNOW, & 
                                SWE,     SMC,   ZSNSO,  SNDPTH,   SNICE,   SNLIQ, & 
                                 TG,     STC,   SMH2O,   TAUSS,  QSFC1D,          & 
                                FSA,     FSR,    FIRA,     FSH,    FGEV,   SSOIL, & 
                               TRAD,   ESOIL,   RUNSF,   RUNSB,     SAG,    SALB, & 
                              QSNBOT,PONDING,PONDING1,PONDING2,    T2MB,    Q2MB, & 
			      EMISSI,  FPICE,    CHB2 )                             

       FSNO   = 1.0       
       TV     = undefined_value     
       TGB    = TG 
       CANICE = undefined_value 
       CANLIQ = undefined_value 
       EAH    = undefined_value 
       TAH    = undefined_value
       FWET   = undefined_value 
       WSLAKE = undefined_value 
       ZWT    = undefined_value 
       WA     = undefined_value 
       WT     = undefined_value 
       LFMASS = undefined_value 
       RTMASS = undefined_value 
       STMASS = undefined_value 
       WOOD   = undefined_value 
       STBLCP = undefined_value 
       FASTCP = undefined_value 
       PLAI   = undefined_value 
       PSAI   = undefined_value 
       T2MV   = undefined_value 
       Q2MV   = undefined_value 
       NEE    = undefined_value 
       GPP    = undefined_value 
       NPP    = undefined_value 
       FVEGMP = 0.0 
       ECAN   = undefined_value 
       ETRAN  = undefined_value 
       APAR   = undefined_value 
       PSN    = undefined_value 
       SAV    = undefined_value 
       RSSUN  = undefined_value 
       RSSHA  = undefined_value 
       BGAP   = undefined_value 
       WGAP   = undefined_value 
       TGV    = undefined_value
       CHV    = undefined_value 
       CHB    = CH 
       IRC    = undefined_value 
       IRG    = undefined_value 
       SHC    = undefined_value 
       SHG    = undefined_value 
       EVG    = undefined_value 
       GHV    = undefined_value 
       IRB    = FIRA
       SHB    = FSH
       EVB    = FGEV
       GHB    = SSOIL
       TR     = undefined_value 
       EVC    = undefined_value 
       CHLEAF = undefined_value 
       CHUC   = undefined_value 
       CHV2   = undefined_value 
       FCEV   = undefined_value 
       FCTR   = undefined_value        
       
       QFX(I,J) = ESOIL
       LH (I,J) = FGEV


    ELSE
    goto 1000
if(i==1.and.j==8) then
    print*,I       , J       , LAT     , YEARLEN , JULIAN  , COSZ
    print*,'DT'
    print*,DT      , DX      , DZ8W1D  , NSOIL   , ZSOIL   , 3
    print*,'FVEG'
    print*,FVEG    , FVGMAX  , VEGTYP  , ISURBAN , ICE     , IST
    print*,ISC     
    print*,IZ0TLND 
    print*,'T_ML'
    print*,T_ML    , P_ML    , PSFC    , U_ML    , V_ML    , Q_ML
    print*,'QC'
    print*,QC      , SWDN    , LWDN    , PRCP    , TBOT    , CO2PP
    print*,'O2PP'
    print*,O2PP    , FOLN    , FICEOLD , PBLH    , Z_ML
    print*,'ALBOLD'
    print*,ALBOLD  , SNEQVO
    print*,'STC'
    print*,STC     , SMH2O   , SMC     , TAH     , EAH     , FWET
    print*,'CANLIQ'
    print*,CANLIQ  , CANICE  , TV      , TG      , QSFC1D  , QSNOW
    print*,'ISNOW'
    print*,ISNOW   , ZSNSO   , SNDPTH  , SWE     , SNICE   , SNLIQ
    print*,'ZWT'
    print*,ZWT     , WA      , WT      , WSLAKE  , LFMASS  , RTMASS
    print*,'STMASS'
    print*,STMASS  , WOOD    , STBLCP  , FASTCP  , PLAI    , PSAI
    print*,'CM'
    print*,CM      , CH      , TAUSS
    print*,'FSA'
    print*,FSA     , FSR     , FIRA    , FSH     , SSOIL   , FCEV
    print*,'FGEV'
    print*,FGEV    , FCTR    , ECAN    , ETRAN   , ESOIL   , TRAD
    print*,'TGB'
    print*, TGB     , TGV     , T2MV    , T2MB    
    print*,'Q2MV'
    print*, Q2MV    , Q2MB    , RUNSF   , RUNSB   , APAR
    print*,'PSN'
    print*,PSN     , SAV     , SAG     , FSNO    , NEE     , GPP
    print*,'NPP'
    print*,NPP     , FVEGMP  , SALB    , QSNBOT   , PONDING , PONDING1
    print*,'PONDING2'
    print*,PONDING2, RSSUN   , RSSHA   , BGAP    , WGAP    
    print*,'CHV'
    print*, CHV     , CHB     , EMISSI
end if

1000 continue

       CALL NOAHMP_SFLX (&
            I       , J       , LAT     , YEARLEN , JULIAN  , COSZ    , & 
            DT      , DX      , DZ8W1D  , NSOIL   , ZSOIL   , NSNOW   , & 
            FVEG    , FVGMAX  , VEGTYP  , ISURBAN , ICE     , IST     , & 
            ISC     , SMCEQ   ,                                         & 
            IZ0TLND ,                                                   & 
            T_ML    , P_ML    , PSFC    , U_ML    , V_ML    , Q_ML    , & 
            QC      , SWDN    , LWDN    , PRCP    , TBOT    , CO2PP   , & 
            O2PP    , FOLN    , FICEOLD , PBLH    , Z_ML    ,           & 
            ALBOLD  , SNEQVO  ,                                         & 
            STC     , SMH2O   , SMC     , TAH     , EAH     , FWET    , & 
            CANLIQ  , CANICE  , TV      , TG      , QSFC1D  , QSNOW   , & 
            ISNOW   , ZSNSO   , SNDPTH  , SWE     , SNICE   , SNLIQ   , & 
            ZWT     , WA      , WT      , WSLAKE  , LFMASS  , RTMASS  , & 
            STMASS  , WOOD    , STBLCP  , FASTCP  , PLAI    , PSAI    , & 
            CM      , CH      , TAUSS   ,                               & 
            SMCWTD  ,DEEPRECH , RECH    ,                               & 
            FSA     , FSR     , FIRA    , FSH     , SSOIL   , FCEV    , & 
            FGEV    , FCTR    , ECAN    , ETRAN   , ESOIL   , TRAD    , & 
            TGB     , TGV     , T2MV    , T2MB    , Q2MV    , Q2MB    , & 
            RUNSF   , RUNSB   , APAR    , PSN     , SAV     , SAG     , & 
            FSNO    , NEE     , GPP     , NPP     , FVEGMP  , SALB    , & 
            QSNBOT  , PONDING , PONDING1, PONDING2, RSSUN   , RSSHA   , & 
            BGAP    , WGAP    , CHV     , CHB     , EMISSI  ,           & 
            SHG     , SHC     , SHB     , EVG     , EVB     , GHV     , & 
	    GHB     , IRG     , IRC     , IRB     , TR      , EVC     , & 
	    CHLEAF  , CHUC    , CHV2    , CHB2    , FPICE               &



            )            
                  
            QFX(I,J) = ECAN + ESOIL + ETRAN
            LH       (I,J)                = FCEV + FGEV + FCTR






   ENDIF 



             TSK      (I,J)                = TRAD
             HFX      (I,J)                = FSH
             GRDFLX   (I,J)                = SSOIL
	     SMSTAV   (I,J)                = 0.0  
             SMSTOT   (I,J)                = 0.0  
             SFCRUNOFF(I,J)                = SFCRUNOFF(I,J) + RUNSF * DT
             UDRUNOFF (I,J)                = UDRUNOFF(I,J)  + RUNSB * DT
             IF ( SALB > -999 ) THEN
                ALBEDO(I,J)                = SALB
             ENDIF
             SNOWC    (I,J)                = FSNO
             SMOIS    (I,      1:NSOIL,J)  = SMC   (      1:NSOIL)
             SH2O     (I,      1:NSOIL,J)  = SMH2O (      1:NSOIL)
             TSLB     (I,      1:NSOIL,J)  = STC   (      1:NSOIL)
             SNOW     (I,J)                = SWE
             SNOWH    (I,J)                = SNDPTH
             CANWAT   (I,J)                = CANLIQ + CANICE
             ACSNOW   (I,J)                = ACSNOW(I,J) + PRCP * FPICE
             ACSNOM   (I,J)                = ACSNOM(I,J) + QSNBOT*DT + PONDING + PONDING1 + PONDING2
             EMISS    (I,J)                = EMISSI
             QSFC     (I,J)                = QSFC1D

             ISNOWXY  (I,J)                = ISNOW
             TVXY     (I,J)                = TV
             TGXY     (I,J)                = TG
             CANLIQXY (I,J)                = CANLIQ
             CANICEXY (I,J)                = CANICE
             EAHXY    (I,J)                = EAH
             TAHXY    (I,J)                = TAH
             CMXY     (I,J)                = CM
             CHXY     (I,J)                = CH
             FWETXY   (I,J)                = FWET
             SNEQVOXY (I,J)                = SNEQVO
             ALBOLDXY (I,J)                = ALBOLD
             QSNOWXY  (I,J)                = QSNOW
             WSLAKEXY (I,J)                = WSLAKE
             ZWTXY    (I,J)                = ZWT
             WAXY     (I,J)                = WA
             WTXY     (I,J)                = WT
             TSNOXY   (I,-NSNOW+1:    0,J) = STC   (-NSNOW+1:    0)
             ZSNSOXY  (I,-NSNOW+1:NSOIL,J) = ZSNSO (-NSNOW+1:NSOIL)
             SNICEXY  (I,-NSNOW+1:    0,J) = SNICE (-NSNOW+1:    0)
             SNLIQXY  (I,-NSNOW+1:    0,J) = SNLIQ (-NSNOW+1:    0)
             LFMASSXY (I,J)                = LFMASS
             RTMASSXY (I,J)                = RTMASS
             STMASSXY (I,J)                = STMASS
             WOODXY   (I,J)                = WOOD
             STBLCPXY (I,J)                = STBLCP
             FASTCPXY (I,J)                = FASTCP
             XLAIXY   (I,J)                = PLAI
             XSAIXY   (I,J)                = PSAI
             TAUSSXY  (I,J)                = TAUSS



             T2MVXY   (I,J)                = T2MV
             T2MBXY   (I,J)                = T2MB
             Q2MVXY   (I,J)                = Q2MV/(1.0 - Q2MV)  
             Q2MBXY   (I,J)                = Q2MB/(1.0 - Q2MB)  
             TRADXY   (I,J)                = TRAD
             NEEXY    (I,J)                = NEE
             GPPXY    (I,J)                = GPP
             NPPXY    (I,J)                = NPP
             FVEGXY   (I,J)                = FVEGMP
             RUNSFXY  (I,J)                = RUNSF
             RUNSBXY  (I,J)                = RUNSB
             ECANXY   (I,J)                = ECAN
             EDIRXY   (I,J)                = ESOIL
             ETRANXY  (I,J)                = ETRAN
             FSAXY    (I,J)                = FSA
             FIRAXY   (I,J)                = FIRA
             APARXY   (I,J)                = APAR
             PSNXY    (I,J)                = PSN
             SAVXY    (I,J)                = SAV
             SAGXY    (I,J)                = SAG
             RSSUNXY  (I,J)                = RSSUN
             RSSHAXY  (I,J)                = RSSHA
             BGAPXY   (I,J)                = BGAP
             WGAPXY   (I,J)                = WGAP
             TGVXY    (I,J)                = TGV
             TGBXY    (I,J)                = TGB
             CHVXY    (I,J)                = CHV
             CHBXY    (I,J)                = CHB
             IRCXY    (I,J)                = IRC
             IRGXY    (I,J)                = IRG
             SHCXY    (I,J)                = SHC
             SHGXY    (I,J)                = SHG
             EVGXY    (I,J)                = EVG
             GHVXY    (I,J)                = GHV
             IRBXY    (I,J)                = IRB
             SHBXY    (I,J)                = SHB
             EVBXY    (I,J)                = EVB
             GHBXY    (I,J)                = GHB
             TRXY     (I,J)                = TR
             EVCXY    (I,J)                = EVC
             CHLEAFXY (I,J)                = CHLEAF
             CHUCXY   (I,J)                = CHUC
             CHV2XY   (I,J)                = CHV2
             CHB2XY   (I,J)                = CHB2
             RECHXY   (I,J)                = RECHXY(I,J) + RECH*1.E3 
             DEEPRECHXY(I,J)               = DEEPRECHXY(I,J) + DEEPRECH
             SMCWTDXY(I,J)                 = SMCWTD

          ENDIF                                                         

      ENDDO ILOOP                                                       
   ENDDO JLOOP                                                          


  END SUBROUTINE noahmplsm


  SUBROUTINE NOAHMP_INIT ( MMINLU, SNOW , SNOWH , CANWAT , ISLTYP ,   IVGTYP, ISURBAN, &
       TSLB , SMOIS , SH2O , DZS , FNDSOILW , FNDSNOWH ,   ISICE,iswater  ,             &
       TSK, isnowxy , tvxy     ,tgxy     ,canicexy ,         TMN,     XICE,   &
       canliqxy ,eahxy    ,tahxy    ,cmxy     ,chxy     ,                     &
       fwetxy   ,sneqvoxy ,alboldxy ,qsnowxy  ,wslakexy ,zwtxy    ,waxy     , &
       wtxy     ,tsnoxy   ,zsnsoxy  ,snicexy  ,snliqxy  ,lfmassxy ,rtmassxy , &
       stmassxy ,woodxy   ,stblcpxy ,fastcpxy ,xsaixy   , &

       t2mvxy   ,t2mbxy   ,chstarxy,            &

       NSOIL, restart,                 &
       allowed_to_read , iopt_run,                         &
       ids,ide, jds,jde, kds,kde,                &
       ims,ime, jms,jme, kms,kme,                &
       its,ite, jts,jte, kts,kte,                &
       smoiseq  ,smcwtdxy ,rechxy   ,deeprechxy, areaxy, dx, dy, msftx, msfty,&     
       wtddt    ,stepwtd  ,dt       ,qrfsxy     ,qspringsxy  , qslatxy    ,  &      
       fdepthxy ,ht     ,riverbedxy ,eqzwt     ,rivercondxy ,pexpxy            )    



    INTEGER, INTENT(IN   )    ::     ids,ide, jds,jde, kds,kde,  &
         &                           ims,ime, jms,jme, kms,kme,  &
         &                           its,ite, jts,jte, kts,kte

    INTEGER, INTENT(IN)       ::     NSOIL, ISICE, ISWATER, ISURBAN,iopt_run

    LOGICAL, INTENT(IN)       ::     restart,                    &
         &                           allowed_to_read

    REAL,    DIMENSION( NSOIL), INTENT(IN)    ::     DZS  
    REAL,    INTENT(IN) , OPTIONAL ::     DX, DY
    REAL,    DIMENSION( ims:ime, jms:jme ) ,  INTENT(IN) , OPTIONAL :: MSFTX,MSFTY

    REAL,    DIMENSION( ims:ime, NSOIL, jms:jme ) ,    &
         &   INTENT(INOUT)    ::     SMOIS,                      &
         &                           SH2O,                       &
         &                           TSLB

    REAL,    DIMENSION( ims:ime, jms:jme ) ,                     &
         &   INTENT(INOUT)    ::     SNOW,                       &
         &                           SNOWH,                      &
         &                           CANWAT

    INTEGER, DIMENSION( ims:ime, jms:jme ),                      &
         &   INTENT(IN)       ::     ISLTYP,  &
                                     IVGTYP

    LOGICAL, INTENT(IN)       ::     FNDSOILW,                   &
         &                           FNDSNOWH

    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: TSK         
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: TMN         
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: XICE         
    INTEGER, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: isnowxy     
    REAL, DIMENSION(ims:ime,-2:NSOIL,jms:jme), INTENT(INOUT) :: zsnsoxy  
    REAL, DIMENSION(ims:ime,-2:              0,jms:jme), INTENT(INOUT) :: tsnoxy   
    REAL, DIMENSION(ims:ime,-2:              0,jms:jme), INTENT(INOUT) :: snicexy  
    REAL, DIMENSION(ims:ime,-2:              0,jms:jme), INTENT(INOUT) :: snliqxy  
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tvxy        
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tgxy        
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: canicexy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: canliqxy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: eahxy       
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tahxy       
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: cmxy        
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: chxy        
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: fwetxy      
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: sneqvoxy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: alboldxy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: qsnowxy     
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: wslakexy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: zwtxy       
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: waxy        
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: wtxy        
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: lfmassxy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: rtmassxy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: stmassxy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: woodxy      
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: stblcpxy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: fastcpxy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: xsaixy      



    REAL, DIMENSION(ims:ime,1:nsoil,jms:jme), INTENT(INOUT) , OPTIONAL :: smoiseq 
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: smcwtdxy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: deeprechxy  
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: rechxy      
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: qrfsxy      
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: qspringsxy  
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: qslatxy     
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: areaxy      
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) , OPTIONAL :: FDEPTHXY    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) , OPTIONAL :: HT          
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) , OPTIONAL :: RIVERBEDXY  
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) , OPTIONAL :: EQZWT       
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) , OPTIONAL :: RIVERCONDXY 
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) , OPTIONAL :: PEXPXY      

    INTEGER,  INTENT(OUT) , OPTIONAL :: STEPWTD
    REAL, INTENT(IN) , OPTIONAL :: DT, WTDDT


    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: t2mvxy        
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: t2mbxy        
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: chstarxy        



    REAL, DIMENSION(1:NSOIL)  :: ZSOIL      
    

    REAL                      :: BX, SMCMAX, PSISAT
    REAL                      :: FK

    REAL, PARAMETER           :: BLIM  = 5.5
    REAL, PARAMETER           :: HLICE = 3.335E5
    REAL, PARAMETER           :: GRAV = 9.81
    REAL, PARAMETER           :: T0 = 273.15

    INTEGER                   :: errflag, i,j,itf,jtf,ns

    character(len=80) :: err_message
    character(len=4)  :: MMINSL
    character(len=*), intent(in) :: MMINLU
    MMINSL='STAS'

    call read_mp_veg_parameters(trim(MMINLU))

    
    
    
    IF ( allowed_to_read ) THEN
       CALL wrf_message( 'INITIALIZE THREE Noah LSM RELATED TABLES' )
       CALL  SOIL_VEG_GEN_PARM( MMINLU, MMINSL )
    ENDIF

    IF( .NOT. restart ) THEN

       itf=min0(ite,ide-1)
       jtf=min0(jte,jde-1)

       
       
       
       IF(.NOT.FNDSNOWH)THEN
          
          CALL wrf_message( 'SNOW HEIGHT NOT FOUND - VALUE DEFINED IN LSMINIT' )
          DO J = jts,jtf
             DO I = its,itf
                SNOWH(I,J)=SNOW(I,J)*0.005               
             ENDDO
          ENDDO
       ENDIF

       errflag = 0
       DO j = jts,jtf
          DO i = its,itf
             IF ( ISLTYP( i,j ) .LT. 1 ) THEN
                errflag = 1
                WRITE(err_message,*)"module_sf_noahlsm.F: lsminit: out of range ISLTYP ",i,j,ISLTYP( i,j )
                CALL wrf_message(err_message)
             ENDIF
          ENDDO
       ENDDO
       IF ( errflag .EQ. 1 ) THEN
          CALL wrf_error_fatal3("<stdin>",1001,&
"module_sf_noahlsm.F: lsminit: out of range value "// &
               "of ISLTYP. Is this field in the input?" )
       ENDIF



       DO J = jts , jtf
          DO I = its , itf
	    IF(IVGTYP(I,J)==ISICE .AND. XICE(I,J) <= 0.0) THEN
              DO NS=1, NSOIL
	        SMOIS(I,NS,J) = 1.0                     
	        SH2O(I,NS,J) = 0.0
	        TSLB(I,NS,J) = MIN(TSLB(I,NS,J),263.15) 
              END DO
	        
		SNOW(I,J) = MAX(SNOW(I,J), 10.0)        
                SNOWH(I,J)=SNOW(I,J)*0.01               
	    ELSE
	      
              BX = BB(ISLTYP(I,J))
              SMCMAX = MAXSMC(ISLTYP(I,J))
              DO NS=1, NSOIL
	        IF ( SMOIS(I,NS,J) > SMCMAX )  SMOIS(I,NS,J) = SMCMAX
              END DO
              PSISAT = SATPSI(ISLTYP(I,J))
              IF ( ( BX > 0.0 ) .AND. ( SMCMAX > 0.0 ) .AND. ( PSISAT > 0.0 ) ) THEN
                DO NS=1, NSOIL
                   IF ( TSLB(I,NS,J) < 273.149 ) THEN    
                      FK=(( (HLICE/(GRAV*(-PSISAT))) *                              &
                           ((TSLB(I,NS,J)-T0)/TSLB(I,NS,J)) )**(-1/BX) )*SMCMAX
                      FK = MAX(FK, 0.02)
                      SH2O(I,NS,J) = MIN( FK, SMOIS(I,NS,J) )
                   ELSE
                      SH2O(I,NS,J)=SMOIS(I,NS,J)
                   ENDIF
                END DO
              ELSE
                DO NS=1, NSOIL
                   SH2O(I,NS,J)=SMOIS(I,NS,J)
                END DO
              ENDIF
            ENDIF
          ENDDO
       ENDDO



       DO J = jts,jtf
          DO I = its,itf
             tvxy       (I,J) = TSK(I,J)
	       if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) tvxy(I,J) = 273.15
             tgxy       (I,J) = TSK(I,J)
	       if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) tgxy(I,J) = 273.15
             CANWAT     (I,J) = 0.0
             canliqxy   (I,J) = CANWAT(I,J)
             canicexy   (I,J) = 0.
             eahxy      (I,J) = 2000. 
             tahxy      (I,J) = TSK(I,J)
	       if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) tahxy(I,J) = 273.15


             t2mvxy     (I,J) = TSK(I,J)
	       if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) t2mvxy(I,J) = 273.15
             t2mbxy     (I,J) = TSK(I,J)
	       if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) t2mbxy(I,J) = 273.15
             chstarxy     (I,J) = 0.1


             cmxy       (I,J) = 0.0
             chxy       (I,J) = 0.0
             fwetxy     (I,J) = 0.0
             sneqvoxy   (I,J) = 0.0
             alboldxy   (I,J) = 0.65
             qsnowxy    (I,J) = 0.0
             wslakexy   (I,J) = 0.0

             if(iopt_run.ne.5) then 
                   waxy       (I,J) = 4900.                                       
                   wtxy       (I,J) = waxy(i,j)                                   
                   zwtxy      (I,J) = (25. + 2.0) - waxy(i,j)/1000/0.2            
             else
                   waxy       (I,J) = 0.
                   wtxy       (I,J) = 0.
                   areaxy     (I,J) = (DX * DY) / ( MSFTX(I,J) * MSFTY(I,J) )
             endif

             lfmassxy   (I,J) = 50.         
             stmassxy   (I,J) = 50.0        
             rtmassxy   (I,J) = 500.0       
             woodxy     (I,J) = 500.0       
             stblcpxy   (I,J) = 1000.0      
             fastcpxy   (I,J) = 1000.0      
             xsaixy     (I,J) = 0.1         

          enddo
       enddo

       
       
       ZSOIL(1)         = -DZS(1)          
       DO NS=2, NSOIL
          ZSOIL(NS)       = ZSOIL(NS-1) - DZS(NS)
       END DO

       
       
       CALL snow_init ( ims , ime , jms , jme , its , itf , jts , jtf , 3 , &
            &           NSOIL , zsoil , snow , tgxy , snowh ,     &
            &           zsnsoxy , tsnoxy , snicexy , snliqxy , isnowxy )

       

       if(iopt_run.eq.5) then
          IF ( PRESENT(smoiseq)     .AND. &
            PRESENT(smcwtdxy)    .AND. &
            PRESENT(rechxy)      .AND. &
            PRESENT(deeprechxy)  .AND. &
            PRESENT(areaxy)      .AND. &
            PRESENT(dx)          .AND. &
            PRESENT(dy)          .AND. &
            PRESENT(msftx)       .AND. &
            PRESENT(msfty)       .AND. &
            PRESENT(wtddt)       .AND. &
            PRESENT(stepwtd)     .AND. &
            PRESENT(dt)          .AND. &
            PRESENT(qrfsxy)      .AND. &
            PRESENT(qspringsxy)  .AND. &
            PRESENT(qslatxy)     .AND. &
            PRESENT(fdepthxy)    .AND. &
            PRESENT(ht)          .AND. &
            PRESENT(riverbedxy)  .AND. &
            PRESENT(eqzwt)       .AND. &
            PRESENT(rivercondxy) .AND. &
            PRESENT(pexpxy)            ) THEN

             STEPWTD = nint(WTDDT*60./DT)
             STEPWTD = max(STEPWTD,1)

              CALL groundwater_init ( & 
      &       nsoil, zsoil , dzs  ,isltyp, ivgtyp, isurban, isice ,iswater ,wtddt , &
      &       fdepthxy, ht, riverbedxy, eqzwt, rivercondxy, pexpxy , areaxy, zwtxy,   &
      &       smois,sh2o, smoiseq, smcwtdxy, deeprechxy, rechxy, qslatxy, qrfsxy, qspringsxy, &
      &       ids,ide, jds,jde, kds,kde,                    &
      &       ims,ime, jms,jme, kms,kme,                    &
      &       its,ite, jts,jte, kts,kte                     )

          ELSE
             CALL wrf_error_fatal3("<stdin>",1149,&
'Not enough fields to use groundwater option in Noah-MP')
          END IF
       endif

    ENDIF
  END SUBROUTINE NOAHMP_INIT




  SUBROUTINE SNOW_INIT ( ims , ime , jms , jme , its , itf , jts , jtf ,                  &
       &                 NSNOW , NSOIL , ZSOIL , SWE , TGXY , SNODEP ,                    &
       &                 ZSNSOXY , TSNOXY , SNICEXY ,SNLIQXY , ISNOWXY )










    IMPLICIT NONE

    INTEGER, INTENT(IN)                              :: ims, ime, jms, jme
    INTEGER, INTENT(IN)                              :: its, itf, jts, jtf
    INTEGER, INTENT(IN)                              :: NSNOW
    INTEGER, INTENT(IN)                              :: NSOIL
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: SWE 
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: SNODEP
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: TGXY
    REAL,    INTENT(IN), DIMENSION(1:NSOIL)          :: ZSOIL

    INTEGER, INTENT(OUT), DIMENSION(ims:ime, jms:jme)                :: ISNOWXY 
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:NSOIL,jms:jme) :: ZSNSOXY 
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: TSNOXY  
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: SNICEXY 
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: SNLIQXY 




    INTEGER                           :: I,J,IZ
    REAL,   DIMENSION(-NSNOW+1:    0) :: DZSNO
    REAL,   DIMENSION(-NSNOW+1:NSOIL) :: DZSNSO



    DO J = jts , jtf
       DO I = its , itf
          IF ( SNODEP(I,J) < 0.025 ) THEN
             ISNOWXY(I,J) = 0
             DZSNO(-NSNOW+1:0) = 0.
          ELSE
             IF ( ( SNODEP(I,J) >= 0.025 ) .AND. ( SNODEP(I,J) <= 0.05 ) ) THEN
                ISNOWXY(I,J)    = -1
                DZSNO(0)  = SNODEP(I,J)
             ELSE IF ( ( SNODEP(I,J) > 0.05 ) .AND. ( SNODEP(I,J) <= 0.10 ) ) THEN
                ISNOWXY(I,J)    = -2
                DZSNO(-1) = SNODEP(I,J)/2.
                DZSNO( 0) = SNODEP(I,J)/2.
             ELSE IF ( (SNODEP(I,J) > 0.10 ) .AND. ( SNODEP(I,J) <= 0.25 ) ) THEN
                ISNOWXY(I,J)    = -2
                DZSNO(-1) = 0.05
                DZSNO( 0) = SNODEP(I,J) - DZSNO(-1)
             ELSE IF ( ( SNODEP(I,J) > 0.25 ) .AND. ( SNODEP(I,J) <= 0.45 ) ) THEN
                ISNOWXY(I,J)    = -3
                DZSNO(-2) = 0.05
                DZSNO(-1) = 0.5*(SNODEP(I,J)-DZSNO(-2))
                DZSNO( 0) = 0.5*(SNODEP(I,J)-DZSNO(-2))
             ELSE IF ( SNODEP(I,J) > 0.45 ) THEN
                ISNOWXY(I,J)     = -3
                DZSNO(-2) = 0.05
                DZSNO(-1) = 0.20
                DZSNO( 0) = SNODEP(I,J) - DZSNO(-1) - DZSNO(-2)
             ELSE
                CALL wrf_error_fatal3("<stdin>",1227,&
"Problem with the logic assigning snow layers.")
             END IF
          END IF

          TSNOXY (I,-NSNOW+1:0,J) = 0.
          SNICEXY(I,-NSNOW+1:0,J) = 0.
          SNLIQXY(I,-NSNOW+1:0,J) = 0.
          DO IZ = ISNOWXY(I,J)+1 , 0
             TSNOXY(I,IZ,J)  = TGXY(I,J)  
             SNLIQXY(I,IZ,J) = 0.00
             SNICEXY(I,IZ,J) = 1.00 * DZSNO(IZ) * (SWE(I,J)/SNODEP(I,J))  
          END DO

          
          DO IZ = ISNOWXY(I,J)+1 , 0
             DZSNSO(IZ) = -DZSNO(IZ)
          END DO

          
          DZSNSO(1) = ZSOIL(1)
          DO IZ = 2 , NSOIL
             DZSNSO(IZ) = (ZSOIL(IZ) - ZSOIL(IZ-1))
          END DO

          
          ZSNSOXY(I,ISNOWXY(I,J)+1,J) = DZSNSO(ISNOWXY(I,J)+1)
          DO IZ = ISNOWXY(I,J)+2 , NSOIL
             ZSNSOXY(I,IZ,J) = ZSNSOXY(I,IZ-1,J) + DZSNSO(IZ)
          ENDDO

       END DO
    END DO

  END SUBROUTINE SNOW_INIT


    SUBROUTINE GROUNDWATER_INIT (   &
            &            NSOIL , ZSOIL , DZS, ISLTYP, IVGTYP, ISURBAN, ISICE ,ISWATER , WTDDT , &
            &            FDEPTH, TOPO, RIVERBED, EQWTD, RIVERCOND, PEXP , AREA ,WTD ,  &
            &            SMOIS,SH2O, SMOISEQ, SMCWTDXY, DEEPRECHXY, RECHXY ,  &
            &            QSLATXY, QRFSXY, QSPRINGSXY,                  &
            &            ids,ide, jds,jde, kds,kde,                    &
            &            ims,ime, jms,jme, kms,kme,                    &
            &            its,ite, jts,jte, kts,kte                     )



  IMPLICIT NONE


    INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
         &                           ims,ime, jms,jme, kms,kme,  &
         &                           its,ite, jts,jte, kts,kte
    INTEGER, INTENT(IN)                              :: NSOIL, ISURBAN, ISWATER ,ISICE
    REAL,   INTENT(IN)                               ::     WTDDT
    REAL,    INTENT(IN), DIMENSION(1:NSOIL)          :: ZSOIL,DZS
    INTEGER, INTENT(IN), DIMENSION(ims:ime, jms:jme) :: ISLTYP, IVGTYP
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: FDEPTH, TOPO, RIVERBED, EQWTD, RIVERCOND, PEXP , AREA
    REAL,    INTENT(INOUT), DIMENSION(ims:ime, jms:jme) :: WTD
    REAL,     DIMENSION( ims:ime , 1:nsoil, jms:jme ), &
         &    INTENT(INOUT)   ::                          SMOIS, &
         &                                                 SH2O, &
         &                                                 SMOISEQ
    REAL,    INTENT(INOUT), DIMENSION(ims:ime, jms:jme) ::  &
                                                           SMCWTDXY, &
                                                           DEEPRECHXY, &
                                                           RECHXY, &
                                                           QSLATXY, &
                                                           QRFSXY, &
                                                           QSPRINGSXY  

    INTEGER  :: I,J,K,ITER,itf,jtf
    REAL :: BX,SMCMAX,PSISAT,SMCWLT,DWSAT,DKSAT
    REAL :: FRLIQ,SMCEQDEEP
    REAL :: DELTAT,RCOND
    REAL :: AA,BBB,CC,DD,DX,FUNC,DFUNC,DDZ,EXPON,SMC,FLUX
    REAL, DIMENSION(1:NSOIL) :: SMCEQ
    REAL,      DIMENSION( ims:ime, jms:jme )    :: QLAT, QRF
    INTEGER,   DIMENSION( ims:ime, jms:jme )    :: LANDMASK 


       itf=min0(ite,ide-1)
       jtf=min0(jte,jde-1)



    DELTAT = WTDDT * 60. 

    WHERE(IVGTYP.NE.ISWATER.AND.IVGTYP.NE.ISICE)
         LANDMASK=1
    ELSEWHERE
         LANDMASK=-1
    ENDWHERE
    


    QLAT = 0.
CALL LATERALFLOW(ISLTYP,WTD,QLAT,FDEPTH,TOPO,LANDMASK,DELTAT,AREA       &
                        ,ids,ide,jds,jde,kds,kde                      & 
                        ,ims,ime,jms,jme,kms,kme                      &
                        ,its,ite,jts,jte,kts,kte                      )
                        



    DO J=jts,jtf
       DO I=its,itf
          IF(LANDMASK(I,J).GT.0)THEN
             IF(WTD(I,J) .GT. RIVERBED(I,J) .AND.  EQWTD(I,J) .GT. RIVERBED(I,J)) THEN
               RCOND = RIVERCOND(I,J) * EXP(PEXP(I,J)*(WTD(I,J)-EQWTD(I,J)))
             ELSE    
               RCOND = RIVERCOND(I,J)
             ENDIF
             QRF(I,J) = RCOND * (WTD(I,J)-RIVERBED(I,J)) * DELTAT/AREA(I,J)

             QRF(I,J) = MAX(QRF(I,J),0.) 
          ELSE
             QRF(I,J) = 0.
          ENDIF
       ENDDO
    ENDDO



       DO J = jts,jtf
          DO I = its,itf
             BX = BB(ISLTYP(I,J))
             SMCMAX = MAXSMC(ISLTYP(I,J))
             SMCWLT = WLTSMC (ISLTYP(I,J))
             IF(IVGTYP(I,J)==ISURBAN)THEN
                 SMCMAX = 0.45         
                 SMCWLT = 0.40         
             ENDIF 
             DWSAT  = SATDW (ISLTYP(I,J))
             DKSAT  = SATDK (ISLTYP(I,J))
             PSISAT = -SATPSI(ISLTYP(I,J))
           IF ( ( bx > 0.0 ) .AND. ( smcmax > 0.0 ) .AND. ( -psisat > 0.0 ) ) THEN
             
                    CALL EQSMOISTURE(NSOIL ,  ZSOIL , SMCMAX , SMCWLT ,DWSAT, DKSAT  ,BX  , & 
                                     SMCEQ                          )  

             SMOISEQ (I,1:NSOIL,J) = SMCEQ (1:NSOIL)


              
             IF(WTD(I,J) < ZSOIL(NSOIL)-DZS(NSOIL)) THEN




                         EXPON = 2. * BX + 3.
                         DDZ = ZSOIL(NSOIL) - WTD(I,J)
                         CC = PSISAT/DDZ
                         FLUX = (QLAT(I,J)-QRF(I,J))/DELTAT

                         SMC = 0.5 * SMCMAX

                         DO ITER = 1, 100
                           DD = (SMC+SMCMAX)/(2.*SMCMAX)
                           AA = -DKSAT * DD  ** EXPON
                           BBB = CC * ( (SMCMAX/SMC)**BX - 1. ) + 1. 
                           FUNC =  AA * BBB - FLUX
                           DFUNC = -DKSAT * (EXPON/(2.*SMCMAX)) * DD ** (EXPON - 1.) * BBB &
                                   + AA * CC * (-BX) * SMCMAX ** BX * SMC ** (-BX-1.)

                           DX = FUNC/DFUNC
                           SMC = SMC - DX
                           IF ( ABS (DX) < 1.E-6)EXIT
                         ENDDO

                  SMCWTDXY(I,J) = MAX(SMC,1.E-4)

             ELSEIF(WTD(I,J) < ZSOIL(NSOIL))THEN
                  SMCEQDEEP = SMCMAX * ( PSISAT / ( PSISAT - DZS(NSOIL) ) ) ** (1./BX)

                  SMCEQDEEP = MAX(SMCEQDEEP,1.E-4)
                  SMCWTDXY(I,J) = SMCMAX * ( WTD(I,J) -  (ZSOIL(NSOIL)-DZS(NSOIL))) + &
                                  SMCEQDEEP * (ZSOIL(NSOIL) - WTD(I,J))

             ELSE 
                  SMCWTDXY(I,J) = SMCMAX
                  DO K=NSOIL,2,-1
                     IF(WTD(I,J) .GE. ZSOIL(K-1))THEN
                          FRLIQ = SH2O(I,K,J) / SMOIS(I,K,J)
                          SMOIS(I,K,J) = SMCMAX
                          SH2O(I,K,J) = SMCMAX * FRLIQ
                     ELSE
                          IF(SMOIS(I,K,J).LT.SMCEQ(K))THEN
                              WTD(I,J) = ZSOIL(K)
                          ELSE
                              WTD(I,J) = ( SMOIS(I,K,J)*DZS(K) - SMCEQ(K)*ZSOIL(K-1) + SMCMAX*ZSOIL(K) ) / &
                                         (SMCMAX - SMCEQ(K))   
                          ENDIF
                          EXIT
                     ENDIF
                  ENDDO
             ENDIF
            ELSE
              SMOISEQ (I,1:NSOIL,J) = SMCMAX
              SMCWTDXY(I,J) = SMCMAX
              WTD(I,J) = 0.
            ENDIF



             DEEPRECHXY(I,J) = 0.
             RECHXY(I,J) = 0.
             QSLATXY(I,J) = 0.
             QRFSXY(I,J) = 0.
             QSPRINGSXY(I,J) = 0.

          ENDDO
       ENDDO




    END  SUBROUTINE GROUNDWATER_INIT


  SUBROUTINE EQSMOISTURE(NSOIL  ,  ZSOIL , SMCMAX , SMCWLT, DWSAT , DKSAT ,BEXP , & 
                         SMCEQ                          )  

  IMPLICIT NONE


  INTEGER,                         INTENT(IN) :: NSOIL 
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL 
  REAL,                            INTENT(IN) :: SMCMAX , SMCWLT, BEXP , DWSAT, DKSAT

  REAL,  DIMENSION(      1:NSOIL), INTENT(OUT) :: SMCEQ  

  INTEGER                                     :: K , ITER
  REAL                                        :: DDZ , SMC, FUNC, DFUNC , AA, BB , EXPON, DX




   DO K=1,NSOIL

            IF ( K == 1 )THEN
                DDZ = -ZSOIL(K+1) * 0.5
            ELSEIF ( K < NSOIL ) THEN
                DDZ = ( ZSOIL(K-1) - ZSOIL(K+1) ) * 0.5
            ELSE
                DDZ = ZSOIL(K-1) - ZSOIL(K)
            ENDIF



            EXPON = BEXP +1.
            AA = DWSAT/DDZ
            BB = DKSAT / SMCMAX ** EXPON

            SMC = 0.5 * SMCMAX

         DO ITER = 1, 100
            FUNC = (SMC - SMCMAX) * AA +  BB * SMC ** EXPON
            DFUNC = AA + BB * EXPON * SMC ** BEXP 

            DX = FUNC/DFUNC
            SMC = SMC - DX
            IF ( ABS (DX) < 1.E-6)EXIT
         ENDDO


             SMCEQ(K) = MIN(MAX(SMC,1.E-4),SMCMAX*0.99)
   ENDDO

END  SUBROUTINE EQSMOISTURE




END MODULE module_sf_noahmpdrv
