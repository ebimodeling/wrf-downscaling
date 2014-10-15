MODULE module_sf_pxlsm
 
  USE module_model_constants
  USE module_sf_pxlsm_data

  INTEGER, PARAMETER   :: NSOLD=20
  REAL, PARAMETER      :: RD     = 287.04,   CPD   = 1004.67,             &
                          CPH2O  = 4.218E+3, CPICE = 2.106E+3,            &
                          LSUBF  = 3.335E+5, SIGMA = 5.67E-8,             &  
                          ROVCP  = RD / CPD
                          
  REAL, PARAMETER      :: CRANKP = 0.5                    
  REAL, PARAMETER      :: RIC    = 0.25                   
  REAL, PARAMETER      :: DENW   = 1000.0                 
  REAL, PARAMETER      :: TAUINV = 1.0 / 86400.0          
  REAL, PARAMETER      :: T2TFAC = 1.0 / 10.0             
  REAL, PARAMETER      :: PI = 3.1415926
  REAL, PARAMETER      :: PR0 = 0.95
  REAL, PARAMETER      :: CZO    = 0.032
  REAL, PARAMETER      :: OZO    = 1.E-4

CONTAINS




   SUBROUTINE pxlsm(U3D, V3D, DZ8W, QV3D,T3D,TH3D, RHO,         &   
                    PSFC, GSW, GLW, RAINBL, EMISS,              &
                    ITIMESTEP,CURR_SECS, NSOIL, DT, ANAL_INTERVAL, &            
                    XLAND, XICE, ALBBCK, ALBEDO, SNOALB,        &
                    SMOIS, TSLB, MAVAIL, TA2, QA2,              &
                    ZS,DZS, PSIH,                               &
                    LANDUSEF,SOILCTOP,SOILCBOT,VEGFRA,VEGF_PX,  &
                    ISLTYP,RA,RS,LAI,NLCAT,NSCAT,               & 
                    HFX,QFX,LH,TSK,SST,ZNT,CANWAT,              &
                    GRDFLX,SHDMIN,SHDMAX,                       &
                    SNOWC,PBLH,RMOL,UST,CAPG,DTBL,              &
                    T2_NDG_OLD, T2_NDG_NEW,                     &     
                    Q2_NDG_OLD, Q2_NDG_NEW,                     & 
                    SN_NDG_OLD, SN_NDG_NEW, SNOW, SNOWH,SNOWNCV,&
                    T2OBS, Q2OBS, PXLSM_SMOIS_INIT,             &           
                    PXLSM_SOIL_NUDGE,                           &
                    ids,ide, jds,jde, kds,kde,                  &
                    ims,ime, jms,jme, kms,kme,                  &
                    its,ite, jts,jte, kts,kte                    )









































































































































































     IMPLICIT NONE



    INTEGER,  INTENT(IN)   ::      ids,ide, jds,jde, kds,kde, &
                                   ims,ime, jms,jme, kms,kme, &
                                   its,ite, jts,jte, kts,kte 

   INTEGER,   INTENT(IN)  ::      NSOIL, ITIMESTEP, NLCAT, NSCAT,             &
                                  ANAL_INTERVAL, PXLSM_SMOIS_INIT, PXLSM_SOIL_NUDGE

   REAL,       INTENT(IN   ),OPTIONAL    ::     curr_secs

   REAL,     INTENT(IN )  ::      DT,DTBL 

   INTEGER,   DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::   ISLTYP


    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ),  INTENT(IN) :: U3D, V3D, RHO, &
                                                                    T3D, TH3D, DZ8W, QV3D           

   REAL,     DIMENSION(1:NSOIL), INTENT(IN)::ZS,DZS
   REAL,     DIMENSION( ims:ime , 1:NSOIL, jms:jme ),  INTENT(INOUT)   ::  SMOIS, TSLB     

   REAL,     DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: RA, RS, LAI, ZNT
   REAL,     DIMENSION( ims:ime , jms:jme ), INTENT(OUT):: GRDFLX, TSK, TA2, QA2

   REAL,     DIMENSION( ims:ime , 1:NLCAT, jms:jme ), INTENT(IN):: LANDUSEF       
   REAL,     DIMENSION( ims:ime , 1:NSCAT, jms:jme ), INTENT(IN):: SOILCTOP, SOILCBOT


   REAL,    DIMENSION( ims:ime, jms:jme ),                                     &
            INTENT(IN) ::                            PSFC, GSW, GLW, RAINBL,   &                
                                                     EMISS, SNOALB,            &
                                                     ALBBCK, SHDMIN, SHDMAX,   &
                                                     PBLH, RMOL, SNOWNCV,      &
                                                     UST, MAVAIL, SST
   REAL,    DIMENSION( ims:ime, jms:jme ),                                     &
            INTENT(IN) ::                            T2_NDG_OLD, T2_NDG_NEW,   &                
                                                     Q2_NDG_OLD, Q2_NDG_NEW,   & 
                                                     SN_NDG_OLD, SN_NDG_NEW   

   REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: T2OBS, Q2OBS                
                                                     
   REAL,    DIMENSION( ims:ime, jms:jme ),                                     &
            INTENT(INOUT) ::           CAPG,CANWAT, QFX, HFX, LH,              &
                                       PSIH,VEGFRA, VEGF_PX, SNOW,             &
                                       SNOWH, SNOWC, ALBEDO, XLAND, XICE


   LOGICAL :: radiation





      
      INTEGER, PARAMETER  :: NSTPS  = 11     
      REAL, PARAMETER     :: DTPBLX = 40.0   

      
      
      INTEGER, DIMENSION( 1: NSTPS ) :: JP
      INTEGER:: J, I, NS, NUDGE, ISTI, WEIGHT
      INTEGER:: NTSPS, IT

      
      REAL,     DIMENSION( ims:ime, jms:jme ) :: XLAI, XLAIMN, RSTMIN, &
                                                 XVEG, XVEGMN, XSNUP, &
                                                 XALB
                                                  
      REAL,     DIMENSION( ims:ime, jms:jme ) :: RADNET, EG, ER, ETR, QST

      REAL:: SFCPRS,TA1,DENS1,QV1,ZLVL,SOLDN,LWDN,    &
            EMISSI,PRECIP,THETA1,VAPPRS,QSBT,         &
            WG,W2,WR,TG,T2,USTAR,MOLX,Z0,             &
            RAIR,CPAIR,IFLAND,ISNOW,                  &
            ES,QSS,BETAP,                             &
            RH2_OLD, RH2_NEW, T2_OLD, T2_NEW,         &
            CORE, CORB, TIME_BETWEEN_ANALYSIS,        &
            G1000, ALN10,RH2OBS, HU, SNOBS,           &
            FWSAT,FWFC,FWWLT,FB,FCGSAT,FJP,FAS,       &
            FSEAS, T2I, HC_SNOW, SNOW_FRA,SNOWALB,    &
            QST12,ZFUNC,ZF1,ZA2,QV2, DT_FDDA,         &
            FC2R,FC1SAT, DTPBL

      CHARACTER (LEN = 6) :: LAND_USE_TYPE




      ALN10  = ALOG(10.0)
      G1000  = g*1.0E-3            
      WEIGHT = 0
      
      IF (NLCAT == 50) THEN
         LAND_USE_TYPE = 'NLCD50'
      ELSE IF (NLCAT == 40) THEN
         LAND_USE_TYPE = 'NLCD40'
      ELSE IF (NLCAT == 20) THEN
         LAND_USE_TYPE = 'MODIS'
      ELSE IF (NLCAT == 24) THEN
         LAND_USE_TYPE = 'USGS'
      ELSE IF (NLCAT == 28) THEN
         LAND_USE_TYPE = 'USGS'
      ELSE
         call wrf_error_fatal3("<stdin>",321,&
"Error: Unknown Land Use Category")
      END IF 
      
      IF (ITIMESTEP .EQ. 1) THEN
       CALL wrf_message( 'PX LSM will use the ' // TRIM(LAND_USE_TYPE) // ' landuse tables' )
       PRINT *, 'The analysis interval for surface soil and temp nudging = ',ANAL_INTERVAL,'sec.'
      ENDIF

      
      
      IF (ANAL_INTERVAL .LE. 0.0 .AND. PXLSM_SOIL_NUDGE .EQ. 1) THEN
       CALL wrf_message('PX LSM Error: The User specified analysis interval is zero or negative.')
       CALL wrf_message('If the PX LSM is used with soil nudging (pxlsm_soil_nudge=1) a wrfsfdda_d0* file is required.')
       CALL wrf_message('Make sure these files are present and')
       CALL wrf_message('Check the namelist to ensure sgfdda_interval_m is set to proper sfc analysis interval')
       STOP
      ENDIF                                                                    
      
      
      IF(PXLSM_SOIL_NUDGE .EQ. 1) THEN
        DT_FDDA = ANAL_INTERVAL * 1.0    
        TIME_BETWEEN_ANALYSIS = MOD(CURR_SECS,DT_FDDA)
        IF (TIME_BETWEEN_ANALYSIS .EQ. 0.0) THEN   
          CORB = 1.0       
          CORE = 0.0    
        ELSE    
          CORE = TIME_BETWEEN_ANALYSIS  / DT_FDDA     
          CORB = 1.0 - CORE  
        ENDIF 
      ENDIF 
      
      
      
      CALL VEGELAND(LANDUSEF, VEGFRA, SHDMIN, SHDMAX,         &
                    SOILCTOP, SOILCBOT, NLCAT, NSCAT,         &
                    ZNT,XLAI,XLAIMN,RSTMIN,XVEG,XVEGMN,XSNUP,&
                    XLAND, XALB,                              &
                    ids,ide, jds,jde, kds,kde,          &
                    ims,ime, jms,jme, kms,kme,          &
                    its,ite, jts,jte, kts,kte, LAND_USE_TYPE   )
      

      
      
      DO J = jts,jte    
       DO I = its,ite   
          
          IFLAND = XLAND(I,J)

          
          IF (IFLAND .LT. 1.5 ) THEN 

          
            CALL SOILPROP (SOILCBOT(I,:,J), WEIGHT,               &
                         ITIMESTEP, MAVAIL(I,J),                  &
                         PXLSM_SMOIS_INIT,                        &
                         FWSAT,FWFC,FWWLT,FB,FCGSAT,              &  
                         FJP,FAS,FC2R,FC1SAT,ISTI,SMOIS(I,2,J)    ) 
          
            ISLTYP(I,J) = ISTI                      
          ELSE
            ISLTYP(I,J) = 14                    
          ENDIF

          
          SFCPRS = PSFC(i,j) / 1000.0                 
          TA1    = T3D(i,1,j)                         
          DENS1  = RHO(I,1,J)                         
          QV1    = QV3D(i,1,j)                        
          QV2    = QV3D(i,2,j)
          ZLVL   = 0.5 * DZ8W(i,1,j)                  
          ZF1    = DZ8W(i,1,j)
          ZA2    = ZF1 + 0.5 * DZ8W(i,2,j)

          LWDN   = GLW(I,J)                           
          EMISSI = EMISS(I,J)                         
          PRECIP = MAX ( 1.0E-3*RAINBL(i,j)/DTBL,0.0) 
                                                      
          WR     = 1.0E-3*CANWAT(I,J)                 
          THETA1 = TH3D(i,1,j)                  
          SNOBS  = SNOW(I,J)                          
                                                      
                                                      
                   
          IF(PXLSM_SOIL_NUDGE .EQ. 1) THEN                   
            
            T2_OLD = T2_NDG_OLD(I,J)
            T2_NEW     = T2_NDG_NEW(I,J)
            VAPPRS     = SVP1 * EXP(SVP2 * (T2_OLD - SVPT0) / ( T2_OLD - SVP3))
            QSBT       = EP_2 * VAPPRS / (SFCPRS - VAPPRS)          
            RH2_OLD    = Q2_NDG_OLD(I,J) / QSBT
            VAPPRS     = SVP1 * EXP(SVP2 * (T2_NEW - SVPT0) / (T2_NEW - SVP3))
            QSBT       = EP_2 * VAPPRS / (SFCPRS - VAPPRS)          
            RH2_NEW    = Q2_NDG_NEW(I,J) / QSBT
            RH2OBS     = CORB * RH2_OLD +  CORE * RH2_NEW  
            T2OBS(I,J) = CORB * T2_OLD +  CORE * T2_NEW
            Q2OBS(I,J) = CORB * Q2_NDG_OLD(I,J) +  CORE * Q2_NDG_NEW(I,J)
            SNOBS = CORB * SN_NDG_OLD(I,J) +  CORE * SN_NDG_NEW(I,J)  
          ENDIF

          USTAR  = MAX(UST(I,J),0.005)
          
          IF (IFLAND .GT. 1.5) THEN 
            ZNT(I,J) = CZO * UST(I,J) * UST(I,J) / G + OZO
          ENDIF                                                              
          
          Z0     = ZNT(I,J)
          CPAIR  = CPD * (1.0 + 0.84 * QV1)            

          
          
          CALL PXSNOW (ITIMESTEP, SNOBS, SNOWNCV(I,J), SNOW(I,J),  &
                       SNOWH(I,J), XSNUP(I,J),  XALB(i,j),         &
                       SNOALB(I,J),VEGF_PX(I,J), SHDMIN(I,J),      &
                       HC_SNOW, SNOW_FRA, SNOWC(I,J),  ALBEDO(I,J) ) 
          
                                 
          
          
          
          IF( (XICE(I,J).GE.0.5) .OR.   &
              (SST(I,J).LE.270.0.AND.XLAND(I,J).GT.1.50) ) THEN
              XLAND(I,J) = 1.0
              IFLAND = 1.0
              ZNT(I,J) = 0.001  
              SMOIS(I,1,J) = 1.0     
              SMOIS(I,2,J) = 1.0     
              XICE(I,J) = 1.0
              ALBEDO(I,J) = 0.7
              SNOWC(I,J) = 1.0
              SNOW_FRA = 1.0
              VEGF_PX(I,J) = 0.0
              LAI(I,J) = 0.0
          ENDIF
          

          
          
          
          T2I = TSLB(I,2,J)                                            

          FSEAS = AMAX1(1.0 - 0.015625 * (290.0 - T2I) ** 2,0.0) 
          IF (T2I .GE. 290.0) FSEAS = 1.0                                          
          
          LAI(I,J)       = XLAIMN(I,J) + FSEAS*(XLAI(I,J) - XLAIMN(I,J))                 
          VEGF_PX(I,J)   = XVEGMN(I,J) + FSEAS*(XVEG(I,J) - XVEGMN(I,J))                       
          
          
          IF (IFLAND .GT. 1.5) THEN                        
             VEGF_PX(I,J) = 0.0         
          ENDIF                                                                   
          


          SOLDN  = GSW(I,J) / (1.0 - ALBEDO(I,J))     
          ISNOW = SNOWC(I,J)


          NUDGE=PXLSM_SOIL_NUDGE
          IF ( J .LE. 2 .OR. J .GE. (jde-1) ) NUDGE=0
          IF ( I .LE. 2 .OR. I .GE. (ide-1) ) NUDGE=0
          
          IF ( RMOL(I,J) .GT. 0.0 )  THEN
              MOLX = AMIN1(1/RMOL(I,J),1000.0)
          ELSE IF ( RMOL(I,J) .LT. 0.0 ) THEN
              MOLX = AMAX1(1/RMOL(I,J),-1000.0)
          ELSE
              MOLX = 1000
          ENDIF   
 
          ZFUNC = ZF1 * (1.0 - ZF1 / MAX(100.,PBLH(I,J))) ** 2
          QST12 = KARMAN * ZFUNC*(QV2-QV1) / (ZA2-ZLVL)
               


          NTSPS = INT(DT / (DTPBLX + 0.000001) + 1.0)                                
          DTPBL = DT / NTSPS                                                       

          DO IT=1,NTSPS                                                     
          
            
            IF ( TSLB(I,1,J) .LE. SVPT0 ) THEN        
              ES = SVP1 * EXP(22.514 - 6.15E3 / TSLB(I,1,J))      
            ELSE
              ES = SVP1 * EXP(SVP2 * (TSLB(I,1,J) - SVPT0) /  (TSLB(I,1,J) - SVP3))
            ENDIF
            QSS  = ES * 0.622 / (SFCPRS - ES)
          
            
            BETAP = 1.0
            IF (IFLAND .LT. 1.5 .AND. ISNOW .LT. 0.5 .AND. SMOIS(I,1,J) .LE. FWFC) THEN       
              BETAP = 0.25 * (1.0 - COS(SMOIS(I,1,J) / FWFC * PI)) ** 2
            ENDIF
            
            
            CALL SURFPX (DTPBL, IFLAND, SNOWC(I,J),  NUDGE, XICE(I,J),          & 
                      SOLDN,  GSW(I,J), LWDN,   EMISSI, ZLVL,                   & 
                      MOLX,    Z0,    USTAR,                                    & 
                      SFCPRS, DENS1,  QV1,    QSS,   TA1,                       & 
                      THETA1,   PRECIP,                                         & 
                      CPAIR, PSIH(I,J),                                         & 
                      RH2OBS,T2OBS(I,J),                                        & 
                      VEGF_PX(I,J), ISTI,   LAI(I,J),   BETAP,                  & 
                      RSTMIN(I,J), HC_SNOW, SNOW_FRA,                           & 
                      FWWLT, FWFC, FCGSAT,  FWSAT, FB,                          & 
                      FC1SAT,FC2R,FAS,FJP,DZS(1),DZS(2),QST12,                  & 
                      RADNET(I,J), GRDFLX(I,J), HFX(I,J), QFX(I,J), LH(I,J),    & 
                      EG(I,J), ER(I,J), ETR(I,J),                               & 
                      QST(I,J), CAPG(I,J), RS(I,J), RA(I,J),                    & 
                      TSLB(I,1,J), TSLB(I,2,J),                                 & 
                      SMOIS(I,1,J), SMOIS(I,2,J), WR, TA2(I,J), QA2(I,J),       &
                      LAND_USE_TYPE )
            
          
          END DO                        

          TSK(I,J)    = TSLB(I,1,J)     
          CANWAT(I,J) = WR * 1000.      
          
       ENDDO 
      ENDDO  
      

   END SUBROUTINE pxlsm






      SUBROUTINE VEGELAND( landusef, vegfra,                      & 
                            shdmin, shdmax,                             &                          
                            soilctop, soilcbot, nlcat, nscat,           &
                            znt,  xlai, xlaimn, rstmin, xveg, xvegmn,  &
                            xsnup, xland, xalb,                         &
                            ids,ide, jds,jde, kds,kde,            &
                            ims,ime, jms,jme, kms,kme,            &
                            its,ite, jts,jte, kts,kte,                  &
                            LAND_USE_TYPE                                 )
















                              
      IMPLICIT NONE

      INTEGER,  INTENT(IN)        ::  ids,ide, jds,jde, kds,kde,  &
                                      ims,ime, jms,jme, kms,kme,  &
                                      its,ite, jts,jte, kts,kte
                                                                            
      INTEGER , INTENT(IN)        ::  NSCAT, NLCAT

      REAL,    DIMENSION( ims:ime , 1:NLCAT, jms:jme ),  INTENT(IN) :: LANDUSEF                        
      REAL,    DIMENSION( ims:ime , 1:NSCAT, jms:jme ),  INTENT(IN) :: SOILCTOP, SOILCBOT
                                                                   
      REAL,    DIMENSION( ims:ime, jms:jme ),            INTENT(IN) ::  VEGFRA, SHDMIN, SHDMAX 

      REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: ZNT

      REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: XLAI, XLAIMN, RSTMIN, XALB, &
                                                              XVEG, XVEGMN, XSNUP, XLAND 
      CHARACTER (LEN = 6), INTENT(IN) :: LAND_USE_TYPE

                                     


      INTEGER :: itf, jtf, k, j, i
      REAL    :: SUMLAI, SUMLMN, SUMRSI, SUMLZ0, SUMVEG, SUMVMN, ALAI, VEGF, SUMSNUP
      REAL    :: VFMX, VFMN, VSEAS, FAREA, FWAT, ZNOTC, SUMALB
      REAL,   DIMENSION( NLCAT )  :: LAIMX, LAIMN, Z0, VEG, VEGMN, SNUP
      REAL, PARAMETER      :: ZNOTCMN = 5.0  
      REAL, PARAMETER      :: ZNOTCMX = 15.0 

      REAL, SAVE, DIMENSION(:), POINTER  :: RSMIN, Z00, VEG0, VEGMN0, LAI0, LAIMN0, SNUP0, ALBF

      

      INTEGER, SAVE :: KWAT
      INTEGER, SAVE :: LIMIT1, LIMIT2
     
      IF (LAND_USE_TYPE == 'USGS') THEN
         KWAT = 16
         RSMIN  => RSMIN_USGS
         Z00    => Z00_USGS  
         VEG0   => VEG0_USGS 
         VEGMN0 => VEGMN0_USGS 
         LAI0   => LAI0_USGS 
         LAIMN0 => LAIMN0_USGS 
         SNUP0  => SNUP0_USGS 
         ALBF   => ALBF_USGS
         LIMIT1 = 2
         LIMIT1 = 6
      ELSE IF (LAND_USE_TYPE == 'NLCD50') THEN
         KWAT = 1
         RSMIN  => RSMIN_NLCD50
         Z00    => Z00_NLCD50  
         VEG0   => VEG0_NLCD50 
         VEGMN0 => VEGMN0_NLCD50 
         LAI0   => LAI0_NLCD50 
         LAIMN0 => LAIMN0_NLCD50 
         SNUP0  => SNUP0_NLCD50 
         ALBF   => ALBF_NLCD50
         LIMIT1 = 20
         LIMIT1 = 43
      ELSE IF (LAND_USE_TYPE == 'NLCD40') THEN
         KWAT = 17
         RSMIN  => RSMIN_NLCD40
         Z00    => Z00_NLCD40  
         VEG0   => VEG0_NLCD40 
         VEGMN0 => VEGMN0_NLCD40 
         LAI0   => LAI0_NLCD40 
         LAIMN0 => LAIMN0_NLCD40 
         SNUP0  => SNUP0_NLCD40 
         ALBF   => ALBF_NLCD40
         LIMIT1 = 20
         LIMIT1 = 43
      ELSE IF (LAND_USE_TYPE == 'MODIS') THEN
         KWAT = 17
         RSMIN  => RSMIN_MODIS
         Z00    => Z00_MODIS  
         VEG0   => VEG0_MODIS 
         VEGMN0 => VEGMN0_MODIS 
         LAI0   => LAI0_MODIS 
         LAIMN0 => LAIMN0_MODIS 
         SNUP0  => SNUP0_MODIS 
         ALBF   => ALBF_MODIS
         LIMIT1 = 12
         LIMIT1 = 14
      END IF
      
      DO J = jts,jte
        DO I = its,ite
          XLAI(I,J)   = 0.0                                                        
          XLAIMN(I,J) = 0.0                                                     
          RSTMIN(I,J) = 9999.0                                                    
          XVEG(I,J)   = 0.0                                                       
          XVEGMN(I,J) = 0.0
          XSNUP(I,J)  = 0.0                                                            
          XALB(I,J)   = 0.0                                                            
        ENDDO   
      ENDDO     
      

      DO J = jts,jte
        DO I = its,ite
      

          
          DO K=1,NLCAT                                                               
            LAIMX(K) = LAI0(K)                                                       
            LAIMN(K) = LAIMN0(K)                                                   
            Z0(K)    = Z00(K)                                                   
            VEG(K)   = VEG0(K)                                              
            VEGMN(K) = VEGMN0(K)
            SNUP(K)  = SNUP0(K)                                            
          ENDDO                                              

          
          SUMLAI = 0.0
          SUMLMN = 0.0
          SUMRSI = 0.0
          SUMLZ0 = 0.0
          SUMVEG = 0.0
          SUMVMN = 0.0
          ALAI   = 0.0
          SUMSNUP= 0.0
          SUMALB = 0.0

          
          VFMX   = SHDMAX(I,J)
          VFMN   = SHDMIN(I,J)
          VEGF   = VEGFRA(I,J) 
                
          
          IF(VFMX.GT.0.0.AND.LANDUSEF(I,KWAT,J).LT.1.00) THEN
            VSEAS = VEGF/VFMX
            IF(VSEAS.GT.1.0.OR.VSEAS.LT.0.0) THEN
              VSEAS = MIN(VSEAS,1.0)
              VSEAS = MAX(VSEAS,0.0)
            ENDIF

            ZNOTC = ZNOTCMN * (1-VSEAS) + ZNOTCMX * VSEAS  
            DO K = 1, NLCAT
              IF (LAND_USE_TYPE == 'MODIS') THEN

                
                IF (K.EQ.12 .OR. K.EQ.14) THEN
                   LAIMX(K) = LAIMN0(K) * (1-VSEAS) + LAI0(K) * VSEAS
                   LAIMN(K) = LAIMX(K)
                   VEG(K)   = VEGMN0(K) * (1-VSEAS) + VEG0(K) * VSEAS
                   VEGMN(K) = VEG(K)
                   
                   IF (K .EQ. 12) THEN
                      Z0(K) = ZNOTC
                   
                   ELSE IF (K .EQ.14) THEN
                      Z0(K)  = 0.5 * (ZNOTC + Z00(K))
                   ENDIF
                ENDIF
              ELSE IF (LAND_USE_TYPE == 'NLCD50') THEN
                
                IF (K.EQ.20 .OR. K.EQ.43 .OR. K.EQ.45) THEN
                   LAIMX(K) = LAIMN0(K) * (1-VSEAS) + LAI0(K) * VSEAS
                   LAIMN(K) = LAIMX(K)
                   VEG(K)   = VEGMN0(K) * (1-VSEAS) + VEG0(K) * VSEAS
                   VEGMN(K) = VEG(K)
                   
                   IF (K.EQ.20 .OR. K.EQ.43) THEN
                      Z0(K) = ZNOTC
                   
                  ELSE IF (K.EQ.45) THEN
                      Z0(K)  = 0.5 * (ZNOTC + Z00(K))
                  ENDIF
                ENDIF
              ELSE IF (LAND_USE_TYPE == 'NLCD40') THEN
                
                IF (K.EQ.12 .OR. K.EQ.14 .OR. K.EQ.38) THEN
                   LAIMX(K) = LAIMN0(K) * (1-VSEAS) + LAI0(K) * VSEAS
                   LAIMN(K) = LAIMX(K)
                   VEG(K)   = VEGMN0(K) * (1-VSEAS) + VEG0(K) * VSEAS
                   VEGMN(K) = VEG(K)
                   
                   IF (K.EQ.12 .OR. K.EQ.38) THEN
                      Z0(K) = ZNOTC
                   
                   ELSE IF (K.EQ.14) THEN
                      Z0(K)  = 0.5 * (ZNOTC + Z00(K))
                  ENDIF
                ENDIF
              ELSE IF (LAND_USE_TYPE == 'USGS') THEN
                
                IF (K .GE. 2 .AND. K .LE. 6) THEN
                   LAIMX(K) = LAIMN0(K) * (1-VSEAS) + LAI0(K) * VSEAS
                   LAIMN(K) = LAIMX(K)
                   VEG(K)   = VEGMN0(K) * (1-VSEAS) + VEG0(K) * VSEAS
                   VEGMN(K) = VEG(K)
                   
                   IF (K .GE. 2 .AND. K .LE. 4) THEN
                      Z0(K) = ZNOTC
                   
                   ELSE IF (K .GE.5 .AND. K .LE. 6) THEN
                      Z0(K)  = 0.5 * (ZNOTC + Z00(K))
                   ENDIF
                ENDIF
              END IF

            ENDDO

          ENDIF       

          
          
          DO K = 1, NLCAT 
            FAREA  = LANDUSEF(I,K,J)
            SUMLAI = SUMLAI + LAIMX(K) * FAREA
            SUMLMN = SUMLMN + LAIMN(K) * FAREA
            ALAI   = ALAI + FAREA
            SUMRSI = SUMRSI + FAREA * LAIMX(K) / RSMIN(K)
            SUMLZ0 = SUMLZ0 + FAREA * ALOG(Z0(K))
            SUMVEG = SUMVEG + FAREA * VEG(K)
            SUMVMN = SUMVMN + FAREA * VEGMN(K)
            SUMSNUP= SUMSNUP+ FAREA * SNUP(K)
            SUMALB = SUMALB + FAREA * ALBF(K)
          ENDDO

          
          FWAT = LANDUSEF(I,KWAT,J)
          IF (FWAT .GT. 0.999) THEN
            XLAI(I,J)   = LAIMX(KWAT)
            XLAIMN(I,J) = LAIMN(KWAT)
            RSTMIN(I,J) = RSMIN(KWAT)
            ZNT(I,J)    = Z0(KWAT)
            XVEG(I,J)   = VEG(KWAT)
            XVEGMN(I,J) = VEGMN(KWAT)
            XSNUP(I,J)  = SNUP(KWAT)
            XALB(I,J)   = ALBF(KWAT)
          ELSE
            IF (FWAT .GT. 0.10) THEN
              ALAI   = ALAI - FWAT
              SUMLZ0 = SUMLZ0 - FWAT * ALOG(Z0(KWAT))
            ENDIF
            XLAI(I,J)   = SUMLAI / ALAI
            XLAIMN(I,J) = SUMLMN / ALAI
            RSTMIN(I,J) = SUMLAI / SUMRSI
            ZNT(I,J)    = EXP(SUMLZ0/ALAI)
            XVEG(I,J)   = SUMVEG / ALAI
            XVEGMN(I,J) = SUMVMN / ALAI
            XSNUP(I,J)  = SUMSNUP/ALAI
            XALB(I,J)   = SUMALB/ALAI
          ENDIF

          IF (FWAT .GT. 0.50) THEN
            ZNT(I,J) = Z0(KWAT)
            XALB(I,J)= ALBF(KWAT)
          ENDIF

          ZNT(I,J)    = ZNT(I,J) * 0.01           
          XVEG(I,J)   = XVEG(I,J) * 0.01          
          XVEGMN(I,J) = XVEGMN(I,J) * 0.01
          XLAND(I,J)  = 1.0 + FWAT
          XALB(I,J)   = XALB(I,J) * 0.01
        
        ENDDO     
      ENDDO       
      

  END SUBROUTINE vegeland



      SUBROUTINE SURFPX(DTPBL,  IFLAND, ISNOW,  NUDGEX, XICE1,               & 
                        SOLDN,  GSW, LWDN,   EMISSI, Z1,                     & 
                        MOL,    ZNT,    UST,                                 & 
                        PSURF,  DENS1,  QV1,    QSS,   TA1,                  & 
                        THETA1, PRECIP,                                      & 
                        CPAIR, PSIH,                                         & 
                        RH2OBS, T2OBS,                                       & 
                        VEGFRC, ISTI,   LAI,   BETAP,                        & 
                        RSTMIN, HC_SNOW, SNOW_FRA,                           & 
                        WWLT, WFC, CGSAT, WSAT, B,                           & 
                        C1SAT, C2R, AS, JP, DS1, DS2,QST12,                  & 
                        RADNET, GRDFLX, HFX, QFX, LH, EG, ER,  ETR,          & 
                        QST,    CAPG,   RS, RA,  TG,     T2,                 & 
                        WG,     W2,     WR, TA2, QA2, LAND_USE_TYPE   )        





































































      IMPLICIT NONE




      INTEGER , INTENT(IN)  :: ISTI, NUDGEX


      REAL ,    INTENT(IN)  :: DTPBL, DS1, DS2
      REAL ,    INTENT(IN)  :: IFLAND, ISNOW, XICE1
      REAL ,    INTENT(IN)  :: SOLDN, GSW, LWDN, EMISSI, Z1
      REAL ,    INTENT(IN)  :: ZNT
      REAL ,    INTENT(IN)  :: PSURF, DENS1, QV1, QSS, TA1,  THETA1, PRECIP
      REAL ,    INTENT(IN)  :: CPAIR
      REAL ,    INTENT(IN)  :: VEGFRC, LAI
      REAL ,    INTENT(IN)  :: RSTMIN, HC_SNOW, SNOW_FRA 
      REAL ,    INTENT(IN)  :: WWLT, WFC, CGSAT, WSAT, B, C1SAT, C2R, AS, JP
      REAL ,    INTENT(IN)  :: RH2OBS,T2OBS
      REAL ,    INTENT(IN)  :: QST12

      REAL ,    INTENT(OUT) :: RADNET, EG, ER, ETR
      REAL ,    INTENT(OUT) :: QST, CAPG, RS, TA2, QA2

      REAL ,    INTENT(INOUT) :: TG, T2, WG, W2, WR, UST, RA, BETAP 
      REAL ,    INTENT(INOUT) :: GRDFLX, QFX, HFX, LH, PSIH, MOL

      CHARACTER (LEN = 5), INTENT(IN) :: LAND_USE_TYPE




      REAL        :: HF, LV, CQ4 
      REAL        :: RAH, RAW, ET, W2CG, CG, CT, SOILFLX, CPOT, THETAG
      REAL        :: ZOL, ZOBOL, ZNTOL, Y, Y0, PSIH15, YNT
      REAL        :: WGNUDG, W2NUDG, ALPH1, ALPH2, BET1, BET2, T1P5
      REAL        :: CQ1, CQ2, CQ3, COEFFNP1, COEFFN, TSNEW, TSHLF, T2NEW
      REAL        :: ROFF, WRMAX, PC, DWR, PNET, TENDWR, WRNEW
      REAL        :: COF1, CFNP1WR, CFNWR, PG, FASS
      REAL        :: TENDW2, W2NEW, W2HLF, W2REL, C1, C2, WEQ, CFNP1, CFN, WGNEW
      REAL        :: ALN10, TMP1, TMP2, TMP3, AA, AB, TST, RBH, CTVEG
      REAL        :: QST1,PHIH,PSIOB
      REAL        :: T2NUD, T2NUDF
      REAL        :: VAPPRS, QSBT, RH2MOD


      REAL        :: ZOBS, GAMAH, BETAH, SIGF, BH, CT_SNOW
      REAL, PARAMETER :: CV = 8.0E-6   
      PARAMETER (ZOBS  = 1.5)      
      PARAMETER (BH    = 15.7)
      PARAMETER (GAMAH = 16. )   
      PARAMETER (BETAH = 5.0 )   
      PARAMETER (SIGF  = 0.5)      
      


      PARAMETER (CT_SNOW  = 2.0E-5)  
      ALN10  = ALOG(10.0)
        RADNET = SOLDN - (EMISSI *(STBOLT *TG **4 - LWDN))        
        
        CPOT= (100.0 / PSURF) ** ROVCP       
        THETAG = TG * CPOT

        ZOL   = Z1/MOL                                                       
        ZOBOL = ZOBS/MOL                                                      
        ZNTOL = ZNT/MOL                              

        
        IF (MOL .LT. 0.0) THEN
          Y      = ( 1.0 - GAMAH * ZOL ) ** 0.5
          Y0     = ( 1.0 - GAMAH * ZOBOL ) ** 0.5
          YNT    = ( 1.0 - GAMAH * ZNTOL ) ** 0.5
          PSIH15 =  2.0 * ALOG((Y + 1.0) / (Y0 + 1.0))
          PSIH   =  2.0 * ALOG((Y + 1.0) / (YNT + 1.0))
          PSIOB  =  2.0 * ALOG((Y0 + 1.0) / (YNT + 1.0))
          PHIH = 1.0 / Y
        ELSE
          IF((ZOL-ZNTOL).LE.1.0) THEN                                         
            PSIH = -BETAH*(ZOL-ZNTOL)                                        
          ELSE                                                               
            PSIH = 1.-BETAH-(ZOL-ZNTOL)                                      
          ENDIF             
          IF ((ZOBOL - ZNTOL) .LE. 1.0) THEN
            PSIOB = -BETAH * (ZOBOL - ZNTOL)
          ELSE
            PSIOB = 1.0 - BETAH - (ZOBOL - ZNTOL)
          ENDIF
          PSIH15 =  PSIH - PSIOB
          IF(ZOL.LE.1.0) THEN
            PHIH = 1.0 + BETAH * ZOL
          ELSE
            PHIH = BETAH + ZOL
          ENDIF
        ENDIF
        
        
        
        
        RA=PR0* ( ALOG(Z1/ZNT) - PSIH )/(KARMAN*UST)
        RAH = RA + 5.0 / UST
        RAW = RA + 4.503 / UST
        
        
        CALL QFLUX( DENS1,  QV1,    TA1,  SOLDN,  RAW, QSS,            &
                    VEGFRC, ISNOW,  ISTI, IFLAND, LAI, BETAP,          &
                    WG,     W2,     WR,                                &
                    RSTMIN, WWLT, WFC,                                 &
                    EG,     ER,     ETR,  CQ4,    RS,  FASS)
        

        
        
        ET  = EG + ER + ETR
        QST = -ET / (DENS1 * UST)
 
        LV  = 2.83E6                         
        IF (ISNOW .LT. 0.5.AND.TG.GT.273.15)                            &                                                                               
                    LV = (2.501 - 0.00237 * (TG - 273.15)) * 1.E6  
        
        
        QFX = ET
        LH  = LV * QFX
        

        
        
        TST = (THETA1 - THETAG ) / (UST*RAH)
        HF  = UST * TST           
        HFX = AMAX1(-DENS1 * CPAIR * HF, -250.0)  
        

        
        
        QST1 = 0.5*(QST+QST12/PHIH)
        TA2  = (THETAG + TST * (PR0 / KARMAN * (ALOG(ZOBS / ZNT) - PSIOB)+5.))/CPOT
        QA2  = QV1 - QST1 * PR0/ KARMAN *  (ALOG(Z1 / ZOBS) - PSIH15)

        IF (QA2.LE.0.0) QA2 = QV1

        
        VAPPRS = SVP1 * EXP(SVP2 * (TA2 - SVPT0) / (TA2 - SVP3))
        QSBT   = EP_2 * VAPPRS / (PSURF - VAPPRS)
        RH2MOD = QA2 / QSBT
        
        IF (IFLAND .LT. 1.5 ) THEN
          W2CG = AMAX1(W2,WWLT)
          CG   = CGSAT * 1.0E-6 * (WSAT/ W2CG) **    &  
                 (0.5 * B / ALN10)
          CT   = 1./((1-VEGFRC)/CG + VEGFRC/CV)
          CT   = 1./(SNOW_FRA/CT_SNOW + (1-SNOW_FRA)/CT)
          CAPG = 1.0/CT          

          SOILFLX = 2.0 * PI * TAUINV * (TG - T2)
          GRDFLX  = SOILFLX / CT
        ENDIF
        

        
        
        
        IF (IFLAND .LT. 1.5) THEN
          IF (NUDGEX .EQ. 0) THEN                                          
            WGNUDG = 0.0                                                        
            W2NUDG = 0.0    
            T2NUD  = 0.0                                    
          ELSE                                                               
            
            CALL SMASS (ISTI,  FASS,  SOLDN,   VEGFRC, RA, WWLT, WFC,   &                       
                        ALPH1, ALPH2, BET1, BET2, T2NUDF)                             

            
            WGNUDG = ALPH1 * (T2OBS - TA2) + ALPH2 * (RH2OBS - RH2MOD) * 100  
            W2NUDG = BET1  * (T2OBS - TA2) + BET2  * (RH2OBS - RH2MOD) * 100
            IF (W2 .GE. WFC)  W2NUDG = AMIN1(W2NUDG,0.0)
            IF (W2 .LE. WWLT) W2NUDG = AMAX1(W2NUDG,0.0)
            T2NUD = T2NUDF * (T2OBS - TA2)
          
          ENDIF
        ENDIF
        

        
        
        IF (IFLAND .LT. 1.5) THEN
          
          
          CQ1      = (1.0 - 0.622 * LV * CRANKP / (r_d * TG)) * QSS
          CQ2      = 0.622 * LV * QSS * CRANKP / (r_d * TG * TG)
          CQ3      = DENS1 * BETAP * (1.0 - VEGFRC) / RAW
          COEFFNP1 = 1.0 + DTPBL * CRANKP * (4.0 * EMISSI *  STBOLT * TG ** 3    &
                     * CT + DENS1 * CPAIR / RAH * CPOT * CT + 2.0 * PI           &
                     * TAUINV ) + DTPBL * (CT * LV * CQ2 * (CQ3 + CQ4))
          COEFFN   = CT * (GSW + EMISSI * (STBOLT * (4.0 * CRANKP - 1.0)         &
                     * TG*TG*TG*TG + LWDN)                                       & 
                     + DENS1 * CPAIR / RAH * (THETA1 - (1.0 - CRANKP) * THETAG)  &
                     - LV * (CQ3 * (CQ1 - QV1) + CQ4 * (CQ1 - QV1)))             & 
                     - 2.0 * PI * TAUINV * ((1.0 - CRANKP) * TG - T2)              
          TSNEW    = (TG + DTPBL * COEFFN) / COEFFNP1
          
          IF (XICE1 .GT. 0.5) TSNEW = AMIN1(TSNEW,273.15)                         
          TSHLF = 0.5 * ( TSNEW + TG)
          T2NEW = (T2 + DTPBL * TAUINV * T2TFAC * (TSHLF - (1 - CRANKP) * T2)     &
                + DTPBL*T2NUD)  &              
                  / (1.0 + DTPBL * TAUINV * T2TFAC * CRANKP)
          
          TG = TSNEW
          T2 = T2NEW             
        ENDIF
        

        
      
        IF (IFLAND .LT. 1.5.AND. XICE1.LT.0.5) THEN  
          
          ROFF  = 0.0
          WRMAX = 0.2E-3 * VEGFRC * LAI                     
          
          IF(WRMAX.GT.0.0) THEN
            
            PC    = VEGFRC * SIGF * PRECIP
            DWR   = (WRMAX - WR) / DTPBL                       
            PNET  = PC - ER/ DENW                              
            IF (PNET .GT. DWR) THEN
              ROFF = PNET - DWR
              PC   = PC - ROFF
            ENDIF
            IF (QSS .LT. QV1) THEN
              TENDWR = PC - ER / DENW
              WRNEW  = WR + DTPBL * TENDWR
            ELSE
              COF1    = DENS1 / DENW * VEGFRC * (QSS - QV1) / RAW
              
              CFNP1WR = 1.0 + DTPBL * COF1 * CRANKP / WRMAX  
              CFNWR   = PC - COF1 * (1.0 - CRANKP) * WR / WRMAX
              WRNEW   = (WR + DTPBL * CFNWR) / CFNP1WR
            ENDIF
          ELSE
            PC=0.0
            WRNEW=0.0  
          ENDIF
          
          
          PG     = DENW * (PRECIP - PC)               
          TENDW2 = 1.0 / (DENW * DS2) * (PG - EG - ETR) &
                   + (W2NUDG + WGNUDG) / DS2                    
          W2NEW  = W2 + DTPBL * TENDW2
          W2NEW  = AMIN1(W2NEW,WSAT)
          W2NEW  = AMAX1(W2NEW,0.05)
          W2HLF  = 0.5 * (W2 + W2NEW)
          
          W2     = W2NEW
          WR     = AMIN1(WRMAX,WRNEW)
        ENDIF
        

        
      
        IF (IFLAND .LT. 1.5.AND. XICE1.LT.0.5) THEN  
        
        
          IF (ISNOW .GT.0.5) THEN
            WG = WSAT
          ELSE
            W2REL = W2HLF / WSAT
            IF (WG .GT. WWLT) THEN
              C1 = C1SAT * (WSAT / WG) ** (0.5 * B + 1.0) 
            ELSE   
              C1 = C1SAT * (WSAT / WWLT) ** (0.5 * B + 1.0)
            ENDIF
            C2 = C2R * W2HLF / (WSAT - W2HLF + 1.E-11) 
            IF (W2HLF .EQ. WSAT) THEN
              WEQ = WSAT
            ELSE
              WEQ = W2HLF - AS * WSAT * W2REL ** JP *         &
                   (1.0 - W2REL ** (8.0 * JP))
            ENDIF

            
            CFNP1 = 1.0 + DTPBL * C2 * TAUINV * CRANKP
            CFN   = C1 / (DENW * DS1) * (PG - EG) - C2 * TAUINV *               &
                    ((1.0 - CRANKP) * WG - WEQ) + WGNUDG/ DS1                                    

            WGNEW = AMAX1((WG + DTPBL * CFN) / CFNP1,0.001)
            
            WG    = AMIN1(WGNEW,WSAT)
            
          ENDIF                  
        ENDIF                    
            
      END SUBROUTINE surfpx





      SUBROUTINE QFLUX (DENS1,  QV1,   TA1,  RG,     RAW, QSS,           & 
                        VEGFRC, ISNOW, ISTI, IFLAND, LAI, BETAP,         & 
                        WG,     W2,    WR,                               & 
                        RSTMIN, WWLT,  WFC,                              &
                        EG,     ER,    ETR,  CQ4,    RS,  FASS)            










































      IMPLICIT NONE


      INTEGER , INTENT(IN)  :: ISTI


      REAL ,    INTENT(IN)  :: ISNOW, IFLAND
      REAL ,    INTENT(IN)  :: DENS1, QV1, TA1, RG, RAW, QSS,            &
                               VEGFRC, LAI,                              &
                               WG, W2, WR, RSTMIN   
      REAL ,    INTENT(INOUT)  :: BETAP
      REAL,     INTENT(IN)   :: WWLT, WFC

      REAL ,    INTENT(OUT) :: EG, ER, ETR, CQ4, RS, FASS




      REAL    :: WRMAX, DELTA, SIGG, RADL, RADF, W2AVAIL, W2MXAV
      REAL    :: FTOT, F1, F2, F3,  F4
      REAL    :: FSHELT, GS, GA, FX




      REAL, PARAMETER :: RSMAX = 5000.0                
      REAL, PARAMETER :: FTMIN = 0.0000001             
      REAL, PARAMETER :: F3MIN = 0.25



      ER  = 0.0
      ETR = 0.0
      CQ4 = 0.0
      

      IF (QSS .LT. QV1) BETAP = 1.0
      EG = DENS1 * (1.0 - VEGFRC) * BETAP * (QSS - QV1) / RAW

      

      IF (IFLAND .LT. 1.5 .AND. VEGFRC .GT. 0.0)  THEN
        WRMAX = 0.2E-3 * VEGFRC * LAI   
        IF (WR .LE. 0.0) THEN
          DELTA = 0.0
        ELSE

          DELTA = WR / WRMAX           
        ENDIF
        
        IF (QSS .GE. QV1) THEN
          SIGG = DELTA
        ELSE
          SIGG = 1.0
        ENDIF

        ER = DENS1 * VEGFRC * SIGG * (QSS - QV1) / RAW  
      ENDIF
      

      
      
      IF (IFLAND .LT. 1.5 .AND. VEGFRC .GT. 0.0)  THEN
        
        
        IF (RSTMIN .GT. 130.0) THEN
          RADL = 30.0                                            
        ELSE
          RADL = 100.0                                           
        ENDIF
        RADF = 1.1 * RG / (RADL * LAI)                  
        F1   = (RSTMIN / RSMAX + RADF) / (1.0 + RADF)

        
        W2AVAIL = W2 - WWLT
        W2MXAV  = WFC - WWLT
        F2      = 1.0 / (1.0 + EXP(-5.0 * ( W2AVAIL / W2MXAV -               &
                  (W2MXAV / 3.0 + WWLT))))    

        
        
        IF (TA1 .LE. 302.15) THEN
          F4 = 1.0 / (1.0 + EXP(-0.41 * (TA1 - 282.05)))
        ELSE
          F4 = 1.0 / (1.0 + EXP(0.5 * (TA1 - 314.0)))
        ENDIF

        FTOT = LAI * F1 * F2 * F4
      ENDIF

      
      IF (IFLAND .LT. 1.5 .AND. VEGFRC .GT. 0.0)  THEN
        FSHELT = 1.0   
        GS     = FTOT / (RSTMIN * FSHELT)
        GA     = 1.0 / RAW
        
        F3 = 0.5 * (GS - GA + SQRT(GA * GA + GA * GS *                       &
             (4.0 * QV1 / QSS - 2.0) + GS * GS)) / GS
        F3 = AMIN1(AMAX1(F3,F3MIN),1.0)
        RS = 1.0 / (GS * F3)
        
        
        
        IF (RG .LT. 0.00001) THEN   
          FX = 0.0
        ELSE
          FX = 30.0 * F1 * F4 * LAI / (RSTMIN * FSHELT)
        ENDIF

        FASS = FX
        ETR = DENS1 * VEGFRC * (1.0 - SIGG) * (QSS - QV1) / (RAW + RS)
        
        CQ4 = DENS1 * VEGFRC * ((1.0 - SIGG) / (RAW + RS) + SIGG / RAW)
      ENDIF         

    END SUBROUTINE qflux





      SUBROUTINE SMASS (ISTI,  FASS,  RG,   VEGFRC, RA,              & 
                        WWLT, WFC,                                   & 
                        ALPH1, ALPH2, BET1, BET2, T2NUDF )             


      IMPLICIT NONE





      INTEGER, PARAMETER             :: NSCAT    = 16     
      
      INTEGER,                   INTENT(IN)   :: ISTI 
      REAL,                      INTENT(IN)   :: FASS, RG, VEGFRC, RA
      REAL,                      INTENT(IN)   :: WWLT, WFC
      REAL,                      INTENT(OUT)  :: ALPH1, ALPH2, BET1, BET2, T2NUDF





      REAL    :: FBET, FALPH, FRA, FTEXT
      REAL,    DIMENSION( 1: NSCAT ) :: WFCX, WWLTX
      

      REAL, PARAMETER  :: A1MAX = -10.E-5, A2MAX = 1.E-5  
      REAL, PARAMETER  :: B1MAX = -10.E-3, B2MAX = 1.E-3  
      REAL, PARAMETER  :: TASSI = 4.6296E-5               
      REAL, PARAMETER  :: RAMIN = 10.0                    


      DATA WFCX   /  0.135, 0.150, 0.195, 0.255, 0.240, 0.255, 0.322,    &
                    0.325, 0.310, 0.370, 0.367, 0.367, 0.367, 0.367, 0.367, 0.367 /


      DATA WWLTX  /  0.068, 0.075, 0.114, 0.179, 0.155, 0.175, 0.218,    &
                    0.250, 0.219, 0.283, 0.286, 0.286, 0.286, 0.286, 0.286, 0.286 /


      FBET  = FASS
      FALPH = RG / 1370.0

      FRA   = RAMIN / RA            
      FTEXT = TASSI * (WWLT + WFC) / (WWLTX(5) + WFCX(5)) * FRA


      ALPH1 = A1MAX * FALPH * (1.0 - VEGFRC) * FTEXT
      ALPH2 = A2MAX * FALPH * (1.0 - VEGFRC) * FTEXT
      BET1  = B1MAX * FBET  *        VEGFRC  * FTEXT
      BET2  = B2MAX * FBET  *        VEGFRC  * FTEXT
      T2NUDF = 1.0E-5 * MAX((1.0 - 5.0 * FALPH),0.0)   

    END SUBROUTINE smass





      SUBROUTINE SOILPROP (SOILCBOT,WEIGHT, ITIMESTEP, MAVAIL, &  
                           PXLSM_SMOIS_INIT,                   &  
                           FWSAT,FWFC,FWWLT,FB,FCGSAT,         &  
                           FJP,FAS,FC2R,FC1SAT,ISTI, W2        )  
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     


      IMPLICIT NONE

     
      INTEGER, PARAMETER             :: NSCAT    = 16     
      INTEGER, PARAMETER             :: NSCATMIN = 16     

      INTEGER,                      INTENT(IN)   :: WEIGHT, ITIMESTEP, PXLSM_SMOIS_INIT
      REAL,                         INTENT(IN)   :: MAVAIL
      REAL,     DIMENSION(1:NSCAT), INTENT(IN)   :: SOILCBOT
      REAL,                         INTENT(OUT)  :: FWSAT,FWFC,FWWLT,FB,FCGSAT,          &
                                                    FJP,FAS,FC2R,FC1SAT
      REAL,                         INTENT(INOUT)  :: W2


      INTEGER,                      INTENT(OUT)  :: ISTI

      CHARACTER*4, AVCLASS
      CHARACTER*4, DIMENSION( 1: NSCAT ) ::  TEXID 

      INTEGER:: S

      REAL:: TFRACBOT, CFRAC, SUMSND, SUMCLY, AVS, AVC, AVSLT
      REAL,    DIMENSION( 1: NSCAT ) :: WSAT, WFC, WWLT, B, CGSAT, AS,  &
                                        JP, C2R, C1SAT

      REAL,    DIMENSION( 1: NSCATMIN ) ::  SAND, CLAY

      DATA SAND /92.5,80.5,61.1,19.6,4.0,40.0,57.1,11.3,26.8,       &
                 52.0,6.5,10.2,1.0,1.0,1.0,1.0/                                                  
      DATA CLAY/2.1,4.1,10.9,19.1,7.3,18.8,23.3,32.2,36.6,          &                     
                43.0,46.2,58.8,1.0,1.0,1.0,1.0/                                                 
      DATA TEXID/'Sand','Lsan','Sloa','Sill','Silt','Loam','Sclo',  &             
                 'Sicl','Cllo','Sacl','Sicy','Clay','Ormt','Wate',  &           
                 'Bedr','Othe'/                                         



      DATA WSAT  /  0.395, 0.410, 0.435, 0.485, 0.451, 0.420, 0.477,    &
                    0.476, 0.426, 0.482, 0.482, 0.482, 0.482, 0.482, 0.482, 0.482 /


      DATA WFC   /  0.135, 0.150, 0.195, 0.255, 0.240, 0.255, 0.322,    &
                    0.325, 0.310, 0.370, 0.367, 0.367, 0.367, 0.367, 0.367, 0.367 /


      DATA WWLT  /  0.068, 0.075, 0.114, 0.179, 0.155, 0.175, 0.218,    &
                    0.250, 0.219, 0.283, 0.286, 0.286, 0.286, 0.286, 0.286, 0.286 /


      DATA B     /  4.05,  4.38,  4.90,  5.30,  5.39,  7.12,  7.75,     &
                    8.52, 10.40, 10.40, 11.40, 11.40, 11.40, 11.40, 11.40, 11.40  /


      DATA CGSAT /  3.222, 3.057, 3.560, 4.418, 4.111, 3.670, 3.593,    &
                    3.995, 3.058, 3.729, 3.600, 3.600, 3.600, 3.600, 3.600, 3.600 /


      DATA JP    /  4,     4,     4,     6,     6,     6,     8,        &
                   10,     8,    10,    12,    12,    12,    12,    12,    12     /


      DATA AS    /  0.387, 0.404, 0.219, 0.105, 0.148, 0.135, 0.127,    &
                    0.084, 0.139, 0.075, 0.083, 0.083, 0.083, 0.083, 0.083, 0.083 /


      DATA C2R   /  3.9,   3.7,   1.8,   0.8,   0.8,   0.8,   0.4,      &
                    0.6,   0.3,   0.3,   0.3,   0.3,   0.3,   0.3,   0.3,   0.3   /


      DATA C1SAT /  0.082, 0.098, 0.132, 0.153, 0.191, 0.213, 0.385,    &
                    0.227, 0.421, 0.375, 0.342, 0.342, 0.342, 0.342, 0.342, 0.342 /



    IF(WEIGHT.GE.1.0) THEN   
     FWSAT =0
     FWFC  =0
     FWWLT =0
     FB    =0
     FCGSAT=0
     FJP   =0
     FAS   =0
     FC2R  =0
     FC1SAT=0
     TFRACBOT =0
     CFRAC=0
     
     DO S=1,NSCAT

       IF(SOILCBOT(S).GE.CFRAC) THEN
         ISTI=S
         CFRAC=SOILCBOT(S)
       ENDIF
      
       TFRACBOT=TFRACBOT+SOILCBOT(S)
       FWSAT =FWSAT  +  WSAT(S)  *SOILCBOT(S)
       FWFC  =FWFC   +  WFC(S)   *SOILCBOT(S)
       FWWLT =FWWLT  +  WWLT(S)  *SOILCBOT(S)
       FB    =FB     +  B(S)     *SOILCBOT(S)
       FCGSAT=FCGSAT +  CGSAT(S) *SOILCBOT(S)
       FJP   =FJP    +  JP(S)    *SOILCBOT(S)
       FAS   =FAS    +  AS(S)    *SOILCBOT(S)
       FC2R  =FC2R   +  C2R(S)   *SOILCBOT(S)
       FC1SAT=FC1SAT +  C1SAT(S) *SOILCBOT(S)
             
     ENDDO

     TFRACBOT = 1/TFRACBOT
     FWSAT =FWSAT   *  TFRACBOT
     FWFC  =FWFC    *  TFRACBOT
     FWWLT =FWWLT   *  TFRACBOT
     FB    =FB      *  TFRACBOT
     FCGSAT=FCGSAT  *  TFRACBOT
     FJP   =FJP     *  TFRACBOT
     FAS   =FAS     *  TFRACBOT
     FC2R  =FC2R    *  TFRACBOT
     FC1SAT=FC1SAT  *  TFRACBOT
    
    ELSE                
    
      CFRAC      = 0.0                                                               
      SUMSND     = 0.0                                                               
      SUMCLY     = 0.0                                                               
      TFRACBOT   = 0.0                                                              
                                                                                
      DO S = 1,12                                                         

        TFRACBOT  = TFRACBOT  + SOILCBOT(S)
        SUMSND    = SUMSND    + SAND(S) * SOILCBOT(S)
        SUMCLY    = SUMCLY    + CLAY(S) * SOILCBOT(S)
         
        IF(SOILCBOT(S).GE.CFRAC) THEN   
          ISTI=S
          CFRAC=SOILCBOT(S)
        ENDIF

      ENDDO
         
      IF(TFRACBOT.GT.0.001) THEN                                                  
        AVS = SUMSND / TFRACBOT                                               
        AVC = SUMCLY / TFRACBOT                                           
        AVSLT = 100 - AVS - AVC                                   
        
        IF(AVS.GT.(85.+ 0.5*AVC)) THEN
          AVCLASS= 'Sand'                                                   
          ISTI = 1                                                          
        ELSE IF(AVS.GT.(70.+ AVC)) THEN
          AVCLASS= 'Lsan'                                             
          ISTI = 2                                                  
        ELSE IF((AVC.LT.20..AND.AVS.GT.52.) &
                .OR.(AVC.LE.7.5.AND.AVSLT.LT.50.)) THEN                
          AVCLASS= 'Sloa'                                             
          ISTI = 3                                                     
        ELSE IF(AVC.LT.35..AND.AVS.GT.45..AND.AVSLT.LT.28.) THEN      
          AVCLASS= 'Sclo'                                                  
          ISTI = 6                                                     
        ELSE IF(AVC.GE.35..AND.AVS.GT.45.) THEN                           
          AVCLASS = 'Sacl'                                                  
          ISTI = 9                                                         
        ELSE IF(AVC.LT.27.0.AND.AVSLT.LT.50.) THEN                        
          AVCLASS= 'Loam'                                                
          ISTI = 5                                                        
        ELSE IF(AVC.LT.12..AND.AVSLT.GT.80.) THEN                
          AVCLASS = 'Silt'                                         
          ISTI = 4                                                           
        ELSE IF(AVC.LT.27.) THEN                                                 
          AVCLASS = 'Sill'                                                       
          ISTI = 4                                                              
        ELSE IF(AVC.LT.40..AND.AVS.GT.20.) THEN                                  
          AVCLASS = 'Cllo'                                                       
          ISTI = 8                                                               
        ELSE IF(AVC.LT.40.) THEN                                                 
          AVCLASS = 'Sicl'                                                       
          ISTI = 7                                                               
        ELSE IF(AVSLT.GE.40.) THEN                                               
          AVCLASS = 'Sicy'                                                       
          ISTI = 10                                                              
        ELSE                                                                     
          AVCLASS = 'Clay'                                                       
          ISTI = 11                                                              
        ENDIF                                                                          
      ELSE
        ISTI=5
        AVCLASS = TEXID(ISTI)  
      ENDIF
  
      FWSAT =WSAT(ISTI)   
      FWFC  =WFC(ISTI)    
      FWWLT =WWLT(ISTI)   
      FB    =B(ISTI)      
      FCGSAT=CGSAT(ISTI)  
      FJP   =JP(ISTI)     
      FAS   =AS(ISTI)     
      FC2R  =C2R(ISTI)   
      FC1SAT=C1SAT(ISTI)  
                            
    ENDIF
    
    
    IF (ITIMESTEP .EQ. 1 .AND. PXLSM_SMOIS_INIT .GT. 0) THEN
       W2 = FWWLT + (0.5*(FWSAT+FWFC) - FWWLT)  * MAVAIL 
    ENDIF                                                                    
    
    END SUBROUTINE soilprop






      SUBROUTINE PXSNOW (ITIMESTEP, ASNOW, CSNOW, SNOW,       &
                         SNOWH, SNUP,                         &
                         ALB, SNOALB, SHDFAC, SHDMIN,         & 
                         HC_SNOW, SNOW_FRA, SNOWC, SNOWALB)                   

















      IMPLICIT NONE


      REAL, PARAMETER  :: W2SN_CONV   =   10.0
      REAL, PARAMETER  :: CS_SNOWPACK = 2092.0
      REAL, PARAMETER  :: SALP        =    2.6

      INTEGER,       INTENT(IN)    :: ITIMESTEP
      REAL,          INTENT(IN)    :: ASNOW, CSNOW, SNUP, ALB, SNOALB, SHDFAC, SHDMIN
      REAL,          INTENT(INOUT) :: SNOW, SNOWH, SNOWC
      REAL,          INTENT(OUT)   :: HC_SNOW, SNOW_FRA, SNOWALB


                                                   


     
     REAL:: CONV_WAT2SNOW, CSNOWW, RHO_SNOWPACK,   &
            LIQSN_RATIO, SNEQV, RSNOW

             
     SNEQV=ASNOW*0.001              
     RHO_SNOWPACK = 450                   
     LIQSN_RATIO  = DENW/RHO_SNOWPACK     
     
     CONV_WAT2SNOW = LIQSN_RATIO/1000     
      
     SNOW = ASNOW                         
     SNOWH= SNOW * CONV_WAT2SNOW          
     
     
     
     SNOWC = 0.0
     IF (SNOWH .GT. 0.005) SNOWC = 1.0  
     
     HC_SNOW = RHO_SNOWPACK * CS_SNOWPACK * SNOWH
     
      IF (SNEQV < SNUP) THEN                                                  
         RSNOW = SNEQV / SNUP                                                    
         SNOW_FRA = 1. - ( EXP ( - SALP * RSNOW) - RSNOW * EXP ( - SALP))          
      ELSE                                                                       
         SNOW_FRA = 1.0                                                            
      END IF   
      
      SNOWC  = SNOW_FRA

      SNOWALB = ALB + SNOWC*(SNOALB-ALB)

    END SUBROUTINE pxsnow



END MODULE module_sf_pxlsm

