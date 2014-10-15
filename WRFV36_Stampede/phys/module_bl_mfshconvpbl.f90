     MODULE MODULE_BL_MFSHCONVPBL




USE MODULE_MODEL_CONSTANTS

REAL,PARAMETER :: XG      = 9.80665

REAL,PARAMETER :: XP00= 1.E5               


REAL,PARAMETER :: XMD= 28.9644E-3
REAL,PARAMETER :: XMV= 18.0153E-3            
REAL,PARAMETER :: XRD=R_D
REAL,PARAMETER :: XRV=R_V        
REAL,PARAMETER :: XCPD=7.* XRD /2.
REAL,PARAMETER :: XCPV=4.* XRV          
REAL,PARAMETER :: XCL= 4.218E+3
REAL,PARAMETER :: XCI= 2.106E+3      
REAL,PARAMETER :: XTT= 273.16        
REAL,PARAMETER :: XLVTT=2.5008E+6              
REAL,PARAMETER :: XLSTT=2.8345E+6   
                               
REAL,PARAMETER :: XGAMW = (XCL - XCPV) / XRV
REAL,PARAMETER :: XBETAW= (XLVTT/XRV) + (XGAMW * XTT)



REAL,PARAMETER :: LOG_611_14 = 6.415326
REAL,PARAMETER :: LOG_XTT    = 5.610058
REAL,PARAMETER :: XALPW= LOG_611_14 + (XBETAW /XTT) + (XGAMW *LOG_XTT)
                                
REAL,PARAMETER :: XGAMI  = (XCI - XCPV) / XRV
REAL,PARAMETER :: XBETAI = (XLSTT/XRV) + (XGAMI * XTT)

REAL,PARAMETER :: XALPI  = LOG_611_14 + (XBETAI /XTT) + (XGAMI *LOG_XTT)
REAL,PARAMETER :: XLINI = 0.32


REAL, PARAMETER :: XALP_PERT   = 0.3  
                   
                   
REAL, PARAMETER ::XABUO       = 1.   
REAL, PARAMETER ::XBENTR      = 1.   
REAL, PARAMETER ::XBDETR      = 0.   


REAL, PARAMETER ::XCMF        = 0.065
                                
REAL, PARAMETER ::XENTR_DRY   = 0.55 
REAL, PARAMETER ::XDETR_DRY   = 10.  
REAL, PARAMETER ::XDETR_LUP   = 1.0   

REAL, PARAMETER ::XENTR_MF    = 0.035
REAL, PARAMETER ::XCRAD_MF    = 50.  
REAL, PARAMETER ::XKCF_MF     = 2.75 
REAL, PARAMETER ::XKRC_MF     = 1.   
REAL, PARAMETER ::XTAUSIGMF   = 600.
REAL, PARAMETER ::XPRES_UV    = 0.5  

REAL, PARAMETER ::XFRAC_UP_MAX= 0.33 




     CONTAINS


      SUBROUTINE MFSHCONVPBL (DT,STEPBL,HT,DZ           &
                    ,RHO,PMID,PINT,TH,EXNER             &
                    ,QV, QC, U, V                       &
                    ,HFX, QFX, TKE                      &
                    ,RUBLTEN,RVBLTEN,RTHBLTEN           &
                    ,RQVBLTEN,RQCBLTEN                  &
                    ,IDS,IDE,JDS,JDE,KDS,KDE            &
                    ,IMS,IME,JMS,JME,KMS,KME            &
                    ,ITS,ITE,JTS,JTE,KTS,KTE,KRR        &
                    ,MASSFLUX_EDKF, ENTR_EDKF, DETR_EDKF & 
                    ,THL_UP, THV_UP, RT_UP, RV_UP       &
                    ,RC_UP, U_UP, V_UP, FRAC_UP, RC_MF  &
                    ,WTHV,PLM_BL89 )                   
 
      IMPLICIT NONE


      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE

      INTEGER,INTENT(IN) :: KRR

      INTEGER,INTENT(IN) :: STEPBL

      REAL,INTENT(IN) :: DT

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: HT, HFX, QFX

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: DZ          &
     &                                                     ,EXNER       &
     &                                                     ,PMID,PINT   &
     &                                                     ,QV,QC,RHO   &
     &                                                     ,TH,U,V,TKE   

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: RQCBLTEN,RQVBLTEN       &
     &                                        ,RTHBLTEN                                &
     &                                        ,RUBLTEN,RVBLTEN        
  
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),OPTIONAL,INTENT(OUT) ::              &
     &                                         MASSFLUX_EDKF, ENTR_EDKF, DETR_EDKF &
     &                                        ,THL_UP, THV_UP, RT_UP, RV_UP        &
     &                                        ,RC_UP, U_UP, V_UP, FRAC_UP

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),OPTIONAL,INTENT(INOUT) ::            &
     &                                         RC_MF

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) ::   WTHV
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) ::   PLM_BL89




INTEGER   :: KRRL      
INTEGER   :: KRRI      
LOGICAL   :: OMIXUV    

REAL   :: PIMPL_MF     
REAL   ::  PTSTEP   
REAL   ::  PTSTEP_MET

REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)   :: PZZ       

REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)   :: PZZM      

REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)   :: PDZZ      
 
REAL, DIMENSION(ITS:ITE,JTS:JTE)   ::  PSFTH,PSFRV
                                            


REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE) ::  PPABSM      

REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE) ::  PEXNM       
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE) ::  PRHODREF    
                                                     
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE) ::  PRHODJ    

REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE) ::  PTKEM       
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE) ::  PUM,PVM     


REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)     ::  PTHM       
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE,KRR) ::  PRM        

REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)     ::  PRUS,PRVS,PRTHS 
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE,KRR) ::  PRRS


REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)     :: PEMF, PENTR, PDETR
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)     :: PTHL_UP, PRT_UP, PRV_UP, PRC_UP
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)     :: PU_UP, PV_UP, PTHV_UP, PFRAC_UP                   
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)     :: PRC_MF
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)     :: WTHV_MF
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)     :: PLM_MF

INTEGER :: I,J,K  
      


DO J=JTS,JTE
   DO K=KTS,KTE
      DO I=ITS,ITE
        IF (K==KTS) PZZ(I,J,K)=0.
        PEMF(I,J,K)=0.
        PENTR(I,J,K)=0.
        PDETR(I,J,K)=0.
        PTHL_UP(I,J,K)=0.
        PTHV_UP(I,J,K)=0.
        PRT_UP(I,J,K)=0.
        PRV_UP(I,J,K)=0.
        PRC_UP(I,J,K)=0.
        PU_UP(I,J,K)=0.
        PV_UP(I,J,K)=0.
        PFRAC_UP(I,J,K)=0.
        WTHV_MF(I,J,K)=0.
        PTHM(I,J,K)=TH(I,K,J)
        PTKEM(I,J,K)=TKE(I,K,J)
        PRM(I,J,K,1)=QV(I,K,J)-RC_MF(I,K,J)
        PRM(I,J,K,2)=RC_MF(I,K,J)
        PUM(I,J,K)=U(I,K,J)
        PVM(I,J,K)=V(I,K,J)
        PRHODREF(I,J,K)=RHO(I,K,J)/(1.+QV(I,K,J))
        PEXNM(I,J,K)=EXNER(I,K,J)
        PPABSM(I,J,K)=PMID(I,K,J)
        IF (K/=KTE) THEN
            PZZ(I,J,K+1)=PZZ(I,J,K)+DZ(I,K,J)
            PZZM(I,J,K)=0.5*(PZZ(I,J,K+1)+PZZ(I,J,K)) 
        ELSE
            PZZM(I,J,K)=PZZ(I,J,K)+0.5*DZ(I,K-1,J) 
        ENDIF
        IF (K==KTS) THEN
           PDZZ(I,J,K)=2*(PZZM(I,J,K))      
        ELSE
           PDZZ(I,J,K)=PZZM(I,J,K)-PZZM(I,J,K-1)      
        ENDIF
     
        PRHODJ(I,J,K)=PRHODREF(I,J,K)*DZ(I,K,J)
  

      ENDDO
   ENDDO
ENDDO


PTHM(:,:,KTE)=PTHM(:,:,KTE-1)
PTKEM(:,:,KTE)=PTKEM(:,:,KTE-1)
PRM(:,:,KTE,1)=PRM(:,:,KTE-1,1)
PRM(:,:,KTE,2)=PRM(:,:,KTE-1,2)
PUM(:,:,KTE)=PUM(:,:,KTE-1)
PVM(:,:,KTE)=PVM(:,:,KTE-1)
PRHODREF(:,:,KTE)=PRHODREF(:,:,KTE-1)
PEXNM(:,:,KTE)=PEXNM(:,:,KTE-1)
PPABSM(:,:,KTE)=PPABSM(:,:,KTE-1)
PRHODJ(:,:,KTE)=PRHODJ(:,:,KTE-1)

PSFTH(:,:)=HFX(ITS:ITE,JTS:JTE)/(PRHODREF(:,:,KTS)*XCPD)
PSFRV(:,:)=QFX(ITS:ITE,JTS:JTE)/(PRHODREF(:,:,KTS))





OMIXUV=.FALSE.
KRRL=1 
KRRI=0 
PIMPL_MF=0.
PTSTEP=DT*STEPBL
PTSTEP_MET=PTSTEP

      CALL MFSHCONVPBL_CORE(KRR,KRRL,KRRI,                            &
                OMIXUV,                                               &
                PIMPL_MF,PTSTEP,PTSTEP_MET,                           &
                PDZZ, PZZ,                                            &
                PRHODJ, PRHODREF,                                     &
                PPABSM, PEXNM,                                        &
                PSFTH,PSFRV,                                          &
                PTHM,PRM,PUM,PVM,PTKEM,                               &
                PRTHS,PRRS,PRUS,PRVS,PEMF, PENTR, PDETR,              &
                PTHL_UP, PRT_UP, PRV_UP, PRC_UP,                      &
                PU_UP, PV_UP, PTHV_UP, PFRAC_UP, PRC_MF, WTHV_MF,PLM_MF  )


DO J=JTS,JTE
   DO K=KTS,KTE
      DO I=ITS,ITE
         RQCBLTEN(I,K,J)=PRRS(I,J,K,2)       
         RQVBLTEN(I,K,J)=PRRS(I,J,K,1)       
         RTHBLTEN(I,K,J)=PRTHS(I,J,K)                
         RUBLTEN(I,K,J)=PRUS(I,J,K)
         RVBLTEN(I,K,J)=PRVS(I,J,K)
         WTHV(I,K,J)=WTHV_MF(I,J,K)
         PLM_BL89(I,K,J)=PLM_MF(I,J,K)
      ENDDO
   ENDDO
ENDDO

      IF ( PRESENT(MASSFLUX_EDKF) ) THEN
         DO J=JTS,JTE
            DO K=KTS,KTE
               DO I=ITS,ITE
                  MASSFLUX_EDKF(I,K,J)=PEMF(I,J,K)
                  ENTR_EDKF(I,K,J)=PENTR(I,J,K)
                  DETR_EDKF(I,K,J)=PDETR(I,J,K)
                  THL_UP(I,K,J)=PTHL_UP(I,J,K)
                  THV_UP(I,K,J)=PTHV_UP(I,J,K)
                  RT_UP(I,K,J)=PRT_UP(I,J,K)
                  RV_UP(I,K,J)=PRV_UP(I,J,K)
                  RC_UP(I,K,J)=PRC_UP(I,J,K)
                  U_UP(I,K,J)=PU_UP(I,J,K)
                  V_UP(I,K,J)=PV_UP(I,J,K)
                  FRAC_UP(I,K,J)=PFRAC_UP(I,J,K)
                  RC_MF(I,K,J)=PRC_MF(I,J,K)
               ENDDO
            ENDDO
         ENDDO
      ENDIF


END SUBROUTINE MFSHCONVPBL




  
      SUBROUTINE MFSHCONVPBL_CORE(KRR,KRRL,KRRI,                            &
                OMIXUV,                       &
                PIMPL_MF,PTSTEP,PTSTEP_MET,                 &
                PDZZ, PZZ,                                            &
                PRHODJ, PRHODREF,                                     &
                PPABSM, PEXNM,                                        &
                PSFTH,PSFRV,                                          &
                PTHM,PRM,PUM,PVM,PTKEM,                               &
                PRTHS,PRRS,PRUS,PRVS,  PEMF, PENTR, PDETR,            &
                PTHL_UP, PRT_UP, PRV_UP, PRC_UP,                      &
                PU_UP, PV_UP, PTHV_UP, PFRAC_UP,  PRC_MF,             &
                PFLXZTHVMF,PLM       )








IMPLICIT NONE

INTEGER,                INTENT(IN)   :: KRR          
INTEGER,                INTENT(IN)   :: KRRL         
INTEGER,                INTENT(IN)   :: KRRI         
LOGICAL,                INTENT(IN)   :: OMIXUV    
             
REAL,                   INTENT(IN)   :: PIMPL_MF     
REAL,                 INTENT(IN)     ::  PTSTEP   
REAL,                 INTENT(IN)     ::  PTSTEP_MET
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PZZ         
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PDZZ        
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PRHODJ      
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PRHODREF    
                                                     
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PPABSM      
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PEXNM       

REAL, DIMENSION(:,:),   INTENT(IN)   ::  PSFTH,PSFRV 
REAL, DIMENSION(:,:,:), INTENT(IN)   ::  PTHM        
REAL, DIMENSION(:,:,:,:), INTENT(IN) ::  PRM         
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PUM,PVM     
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PTKEM       


REAL, DIMENSION(:,:,:),   INTENT(OUT) ::  PRUS,PRVS,PRTHS 
REAL, DIMENSION(:,:,:,:), INTENT(OUT) ::  PRRS 

REAL, DIMENSION(:,:,:), INTENT(OUT) ::  PEMF, PENTR, PDETR
REAL, DIMENSION(:,:,:), INTENT(OUT) ::  PTHL_UP, PRT_UP, PRV_UP, PRC_UP 
REAL, DIMENSION(:,:,:), INTENT(OUT) ::  PU_UP, PV_UP, PTHV_UP, PFRAC_UP
REAL, DIMENSION(:,:,:), INTENT(INOUT) ::  PRC_MF 
REAL, DIMENSION(:,:,:), INTENT(OUT) ::  PFLXZTHVMF 
REAL, DIMENSION(:,:,:), INTENT(OUT) ::  PLM
  




REAL, DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2),SIZE(PTHM,3)) ::     &
          ZEXN,ZCPH,                                      &
          PRV,PRL,PTH,                                    &
          ZTM,                                            &  
          ZLVOCPEXN,                                      &  
          ZCF_MF,                                         & 
          ZLSOCPEXN,                                      &  
          ZAMOIST,                                        &  
          ZATHETA,                                        &  
          ZTHLM,                                          &  
          ZRTM,                                           &  
          ZTHVM,ZTHVREF,ZUMM,ZVMM,                        &  
          ZRI_UP,ZW_UP,                                   & 
          ZEMF_O_RHODREF,                                 & 
          ZTHLDT,ZRTDT,ZUDT,ZVDT,                         & 
          ZFLXZTHMF,ZFLXZRMF,ZFLXZUMF,ZFLXZVMF   


INTEGER,DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2))  :: IKLCL,IKETL,IKCTL 
                    
INTEGER :: IKU, IKB, IKE  
INTEGER :: JI,JJ,JK,JSV                          
INTEGER :: IRESP    







IKU=SIZE(PTHM,3)
IKB=1              
IKE=IKU-1




  ZUMM=PUM   
  ZVMM=PVM   



  CALL COMPUTE_FUNCTION_THERMO_MF( KRR,KRRL,KRRI,                   &
                                   PTHM,PRM,PEXNM,PPABSM,           &
                                   ZTM,ZLVOCPEXN,ZLSOCPEXN,         &
                                   ZAMOIST,ZATHETA                  )





  CALL THL_RT_FROM_TH_R_MF( KRR,KRRL,KRRI,                         &
                                  PTHM, PRM, ZLVOCPEXN, ZLSOCPEXN,       &
                                  ZTHLM, ZRTM                            )





ZTHVM(:,:,:) = PTHM(:,:,:)*((1.+XRV / XRD *PRM(:,:,:,1))/(1.+ZRTM(:,:,:))) 


       ZTHVREF=XG/ZTHVM
       CALL BL89(PZZ,PDZZ,ZTHVREF,ZTHLM,KRR, &
                 PRM,PTKEM,PLM)




      CALL UPDRAFT_SOPE (KRR,KRRL,KRRI,OMIXUV,                     &
                         PZZ,PDZZ,PSFTH,PSFRV,PPABSM,PRHODREF,     &
                         PTKEM,PTHM,PRM,ZTHLM,ZRTM,ZUMM,ZVMM,      &
                         PTHL_UP,PRT_UP,PRV_UP,PU_UP,PV_UP,        &
                         PRC_UP,ZRI_UP,PTHV_UP,ZW_UP,PFRAC_UP,PEMF,&
                         PDETR,PENTR,IKLCL,IKETL,IKCTL )



       ZEMF_O_RHODREF=PEMF/PRHODREF
       CALL MF_TURB(OMIXUV, PIMPL_MF, PTSTEP,PTSTEP_MET,       &
                PDZZ, PRHODJ, ZTHLM,ZTHVM,ZRTM,ZUMM,ZVMM,           &
                ZTHLDT,ZRTDT,ZUDT,ZVDT,                            &
                ZEMF_O_RHODREF,PTHL_UP,PTHV_UP,PRT_UP,PU_UP,PV_UP,&
                ZFLXZTHMF,PFLXZTHVMF,ZFLXZRMF,ZFLXZUMF,ZFLXZVMF   )

               



      CALL COMPUTE_MF_CLOUD(KRRL,ZTHLM,PRC_UP,PFRAC_UP,PDZZ,IKLCL,  &
                            PRC_MF,ZCF_MF                      )




ZEXN(:,:,:)=(PPABSM(:,:,:)/XP00) ** (XRD/XCPD)



PRV(:,:,:)=PRM(:,:,:,1)-PRC_MF(:,:,:)
PRL(:,:,:)=PRC_MF(:,:,:)
      
ZCPH(:,:,:)=XCPD+ XCPV * PRV(:,:,:)+ XCL * PRL(:,:,:) 
      
PTH(:,:,:)=(ZTHLM(:,:,:)+ZTHLDT(:,:,:))+(XLVTT/(ZCPH*ZEXN(:,:,:))*PRL(:,:,:))          

PRTHS(:,:,:)  = ZTHLDT(:,:,:)
PRTHS(:,:,:)  = (PTH(:,:,:)-PTHM(:,:,:))/PTSTEP_MET
PRRS(:,:,:,2) = (PRC_MF-PRM(:,:,:,2))/PTSTEP_MET
PRRS(:,:,:,1) = ZRTDT(:,:,:)-PRRS(:,:,:,2)

PRTHS(:,:,:)  = ZTHLDT(:,:,:)
PRRS(:,:,:,1) = ZRTDT(:,:,:)
PRRS(:,:,:,2) = 0
PRUS(:,:,:)   = ZUDT(:,:,:)
PRVS(:,:,:)   = ZVDT(:,:,:) 
  

END SUBROUTINE MFSHCONVPBL_CORE 



      SUBROUTINE COMPUTE_BL89_ML(PDZZ2D, &
             PTKEM2D,PG_O_THVREF2D,PVPT,KK,OUPORDN,PLWORK)












IMPLICIT NONE



REAL, DIMENSION(:,:),   INTENT(IN)  :: PDZZ2D
REAL, DIMENSION(:,:),   INTENT(IN)  :: PTKEM2D
REAL, DIMENSION(:,:),   INTENT(IN)  :: PG_O_THVREF2D
REAL, DIMENSION(:,:),   INTENT(IN)  :: PVPT
INTEGER,                INTENT(IN)  :: KK
LOGICAL,                INTENT(IN)  :: OUPORDN
REAL, DIMENSION(:),     INTENT(OUT) :: PLWORK



REAL, DIMENSION(SIZE(PTKEM2D,1)) :: ZLWORK1,ZLWORK2 
REAL, DIMENSION(SIZE(PTKEM2D,1)) :: ZINTE,ZPOTE     
                                                    
INTEGER :: IKB,IKE

REAL, DIMENSION(SIZE(PTKEM2D,1),SIZE(PTKEM2D,2)) :: ZDELTVPT,ZHLVPT                                
                      
                      
REAL, DIMENSION(SIZE(PTKEM2D,1)) :: ZTH

INTEGER :: IIJU,IKU             
INTEGER :: J1D                  
INTEGER :: JKK                  
INTEGER :: JRR                  
INTEGER :: JIJK                 
REAL    :: ZTEST,ZTEST0,ZTESTM  




IIJU=SIZE(PTKEM2D,1)

IKB = 1 
IKE = SIZE(PTKEM2D,2)-1 
IKU = SIZE(PTKEM2D,2)

ZDELTVPT(:,2:IKU)=PVPT(:,2:IKU)-PVPT(:,1:IKU-1)
ZDELTVPT(:,1)=0.


WHERE (ABS(ZDELTVPT(:,:))<1.E-10)
  ZDELTVPT(:,:)=1.E-10
END WHERE

ZHLVPT(:,2:IKU)= 0.5 * ( PVPT(:,2:IKU)+PVPT(:,1:IKU-1) )
ZHLVPT(:,1)    = PVPT(:,1)








IF (OUPORDN.EQV..TRUE.) THEN 
 ZINTE(:)=PTKEM2D(:,KK)
 PLWORK=0.
 ZLWORK1=0.
 ZLWORK2=0.
 ZTESTM=1.
 ZTH(:)=PVPT(:,KK)
 DO JKK=KK+1,IKE
    IF(ZTESTM > 0.) THEN
      ZTESTM=0
      DO J1D=1,IIJU
        ZTEST0=0.5+SIGN(0.5,ZINTE(J1D))
        ZPOTE(J1D) = ZTEST0*(PG_O_THVREF2D(J1D,KK)      *      &
            (ZHLVPT(J1D,JKK) - ZTH(J1D)))  * PDZZ2D(J1D,JKK) 
        ZTEST =0.5+SIGN(0.5,ZINTE(J1D)-ZPOTE(J1D))
        ZTESTM=ZTESTM+ZTEST0
        ZLWORK1(J1D)=PDZZ2D(J1D,JKK)
        
        ZLWORK2(J1D)=        ( - PG_O_THVREF2D(J1D,KK) *                     &
            (  PVPT(J1D,JKK-1) - ZTH(J1D) )                              &
          + SQRT (ABS(                                                       &
            ( PG_O_THVREF2D(J1D,KK) * (PVPT(J1D,JKK-1) - ZTH(J1D)) )**2  &
            + 2. * ZINTE(J1D) * PG_O_THVREF2D(J1D,KK)                        &
                 * ZDELTVPT(J1D,JKK) / PDZZ2D(J1D,JKK) ))    ) /             &
        ( PG_O_THVREF2D(J1D,KK) * ZDELTVPT(J1D,JKK) / PDZZ2D(J1D,JKK) ) 
      
        PLWORK(J1D)=PLWORK(J1D)+ZTEST0*(ZTEST*ZLWORK1(J1D)+  &
                                    (1-ZTEST)*ZLWORK2(J1D))
        ZINTE(J1D) = ZINTE(J1D) - ZPOTE(J1D)
      END DO 
    ENDIF
  END DO 
ENDIF





IF (OUPORDN.EQV..FALSE.) THEN 
 ZINTE(:)=PTKEM2D(:,KK)
 PLWORK=0.
 ZLWORK1=0.
 ZLWORK2=0.
 ZTESTM=1.
 ZTH(:)=PVPT(:,KK)
 DO JKK=KK,IKB,-1 
    IF(ZTESTM > 0.) THEN
      ZTESTM=0
      DO J1D=1,IIJU
        ZTEST0=0.5+SIGN(0.5,ZINTE(J1D))
        ZPOTE(J1D) = -ZTEST0*(PG_O_THVREF2D(J1D,KK)      *      &
            (ZHLVPT(J1D,JKK) - ZTH(J1D)))  * PDZZ2D(J1D,JKK) 
        ZTEST =0.5+SIGN(0.5,ZINTE(J1D)-ZPOTE(J1D))
        ZTESTM=ZTESTM+ZTEST0
        ZLWORK1(J1D)=PDZZ2D(J1D,JKK)
        ZLWORK2(J1D)=        ( + PG_O_THVREF2D(J1D,KK) *                     &
            (  PVPT(J1D,JKK) - ZTH(J1D) )                              &
          + SQRT (ABS(                                                       &
            ( PG_O_THVREF2D(J1D,KK) * (PVPT(J1D,JKK) - ZTH(J1D)) )**2  &
            + 2. * ZINTE(J1D) * PG_O_THVREF2D(J1D,KK)                        &
                 * ZDELTVPT(J1D,JKK) / PDZZ2D(J1D,JKK) ))    ) /             &
        ( PG_O_THVREF2D(J1D,KK) * ZDELTVPT(J1D,JKK) / PDZZ2D(J1D,JKK) ) 
      
        PLWORK(J1D)=PLWORK(J1D)+ZTEST0*(ZTEST*ZLWORK1(J1D)+  &
                                    (1-ZTEST)*ZLWORK2(J1D)) 
        ZINTE(J1D) = ZINTE(J1D) - ZPOTE(J1D)
      END DO 
    ENDIF
  END DO 
ENDIF
  
END SUBROUTINE COMPUTE_BL89_ML




          SUBROUTINE COMPUTE_ENTR_DETR(OTEST,OTESTLCL,&
                            HFRAC_ICE,PFRAC_ICE,KK,PPABSM,PZZ,PDZZ,&
                            PTHVM,PTHLM,PRTM,PW_UP2,&
                            PTHL_UP,PRT_UP,PLUP,&
                            PENTR,PDETR,PBUO_INTEG)












IMPLICIT NONE






LOGICAL,DIMENSION(:),INTENT(INOUT)  :: OTEST 
LOGICAL,DIMENSION(:),INTENT(INOUT)  :: OTESTLCL 
CHARACTER*1                         :: HFRAC_ICE 
                                              
                                              
REAL, DIMENSION(:), INTENT(INOUT)   :: PFRAC_ICE 
INTEGER,            INTENT(IN)      :: KK  



REAL, DIMENSION(:,:),   INTENT(IN) ::  PPABSM      
REAL, DIMENSION(:,:),   INTENT(IN) ::  PZZ       
REAL, DIMENSION(:,:),   INTENT(IN) ::  PDZZ       
REAL, DIMENSION(:,:),   INTENT(IN) ::  PTHVM      




REAL, DIMENSION(:),   INTENT(IN)     ::  PTHLM     
REAL, DIMENSION(:),   INTENT(IN)     ::  PRTM      
REAL, DIMENSION(:,:), INTENT(INOUT)  ::  PW_UP2    
REAL, DIMENSION(:),   INTENT(IN)     ::  PTHL_UP,PRT_UP  
REAL, DIMENSION(:),   INTENT(IN)     ::  PLUP      
REAL, DIMENSION(:),   INTENT(INOUT)  ::  PENTR     
REAL, DIMENSION(:),   INTENT(INOUT)  ::  PDETR     
REAL, DIMENSION(:),   INTENT(INOUT)  ::  PBUO_INTEG








REAL, DIMENSION(SIZE(PTHLM))   :: ZKIC           
REAL, DIMENSION(SIZE(PTHLM))   :: ZEPSI,ZDELTA   
REAL, DIMENSION(SIZE(PTHLM))   :: ZEPSI_CLOUD    
REAL, DIMENSION(SIZE(PTHLM))   :: ZCOEFFMF_CLOUD 

REAL, DIMENSION(SIZE(PTHLM))   :: ZMIXTHL,ZMIXRT 

REAL, DIMENSION(SIZE(PTHLM))   :: ZTHMIX,ZTHVMIX 
REAL, DIMENSION(SIZE(PTHLM))   :: ZRVMIX,ZRCMIX,ZRIMIX 

REAL, DIMENSION(SIZE(PTHLM))   :: ZTHMIX_F2     
REAL, DIMENSION(SIZE(PTHLM))   :: ZRVMIX_F2,ZRCMIX_F2,ZRIMIX_F2 

REAL, DIMENSION(SIZE(PTHLM))   :: ZTHV_UP       


REAL, DIMENSION(SIZE(PTHLM))   :: ZTHVMIX_1,ZTHVMIX_2 




REAL, DIMENSION(SIZE(PTHLM))   :: ZBUO_INTEG,&         
                                  ZDZ_HALF,&           
                                  ZDZ_STOP,&           
                                  ZTHV_MINUS_HALF,&    
                                  ZTHV_PLUS_HALF,&     
                                  ZCOEFF_MINUS_HALF,&  
                                  ZCOEFF_PLUS_HALF,&   
                                  ZCOTHVU_MINUS_HALF,& 
                                  ZCOTHVU_PLUS_HALF,&  
                                  ZW2_HALF             

REAL, DIMENSION(SIZE(PTHLM))   :: ZCOPRE_MINUS_HALF,&  
                                  ZCOPRE_PLUS_HALF,&   
                                  ZPRE_MINUS_HALF,&    
                                  ZPRE_PLUS_HALF,&     
                                  ZTHV_UP_F1,&         
                                  ZTHV_UP_F2           
REAL, DIMENSION(SIZE(PTHLM))   :: ZCOEFF_QSAT,&        
                                  ZRC_ORD,&            
                                  ZPART_DRY            

REAL, DIMENSION(SIZE(PTHLM))   :: ZQVSAT 

REAL, DIMENSION(SIZE(PTHLM))   :: PT 

REAL, DIMENSION(SIZE(PTHVM,1),SIZE(PTHVM,2))   ::ZG_O_THVREF

LOGICAL, DIMENSION(SIZE(OTEST,1)) :: GTEST_LOCAL_LCL,& 
                                     GTEST_LOCAL_LCL2  

REAL     :: ZRDORV       
REAL     :: ZRVORD       
INTEGER  :: ILON, ITEST, IKB         



                        


  IKB=1 

  
  ZRDORV   = XRD / XRV   
  ZRVORD   = XRV / XRD   
  ZG_O_THVREF=XG/PTHVM
  
  ZCOEFF_QSAT=0.
  ZRC_ORD=0.
  ZPART_DRY=1.
  GTEST_LOCAL_LCL=.FALSE.
  ZDZ_HALF(:) = (PZZ(:,KK+1)-PZZ(:,KK))/2.
  ZDZ_STOP(:) = ZDZ_HALF(:)
 
  ZKIC(:)=0.1  











  ZMIXTHL(:) = ZKIC(:) * PTHLM(:)+(1. - ZKIC(:))*PTHL_UP(:)
  ZMIXRT(:)  = ZKIC(:) * PRTM(:)+(1. - ZKIC(:))*PRT_UP(:)


  


IF (KK==IKB) THEN 
  ZCOPRE_MINUS_HALF(:) = 0. 
ELSE
  ZCOPRE_MINUS_HALF(:) = ((PPABSM(:,KK)-PPABSM(:,KK-1))/PDZZ(:,KK))
ENDIF
  ZCOPRE_PLUS_HALF(:) = ((PPABSM(:,KK+1)-PPABSM(:,KK))/PDZZ(:,KK+1))
  
IF (KK==IKB) THEN 
  ZPRE_MINUS_HALF(:)= PPABSM(:,KK)
ELSE
  ZPRE_MINUS_HALF(:)= ZCOPRE_MINUS_HALF*0.5*(PZZ(:,KK)-PZZ(:,KK-1))+PPABSM(:,KK-1)
ENDIF
  ZPRE_PLUS_HALF(:) = ZCOPRE_PLUS_HALF*0.5*(PZZ(:,KK+1)-PZZ(:,KK))+PPABSM(:,KK)  


  
  CALL TH_R_FROM_THL_RT_1D(HFRAC_ICE,PFRAC_ICE,&
               PPABSM(:,KK),ZMIXTHL,ZMIXRT,&
               ZTHMIX,ZRVMIX,ZRCMIX,ZRIMIX)
             

  
  ZTHVMIX_1(:) = ZTHMIX(:)*(1.+ZRVORD*ZRVMIX(:))/(1.+ZMIXRT(:))

  
  CALL TH_R_FROM_THL_RT_1D(HFRAC_ICE,PFRAC_ICE,&
               ZPRE_PLUS_HALF,ZMIXTHL,ZMIXRT,&
               ZTHMIX,ZRVMIX,ZRCMIX,ZRIMIX)
             

  
  ZTHVMIX_2(:) = ZTHMIX(:)*(1.+ZRVORD*ZRVMIX(:))/(1.+ZMIXRT(:))










  
  CALL TH_R_FROM_THL_RT_1D(HFRAC_ICE,PFRAC_ICE,&
               ZPRE_MINUS_HALF,PTHL_UP,PRT_UP,&
               ZTHMIX,ZRVMIX,ZRCMIX,ZRIMIX)
  ZTHV_UP_F1(:) = ZTHMIX(:)*(1.+ZRVORD*ZRVMIX(:))/(1.+PRT_UP(:))
 
  
  CALL TH_R_FROM_THL_RT_1D(HFRAC_ICE,PFRAC_ICE,&
               PPABSM(:,KK),PTHL_UP,PRT_UP,&
               ZTHMIX,ZRVMIX,ZRCMIX,ZRIMIX)            
  ZTHV_UP(:) = ZTHMIX(:)*(1.+ZRVORD*ZRVMIX(:))/(1.+PRT_UP(:))

  
  CALL TH_R_FROM_THL_RT_1D(HFRAC_ICE,PFRAC_ICE,&
               ZPRE_PLUS_HALF,PTHL_UP,PRT_UP,&
               ZTHMIX_F2,ZRVMIX_F2,ZRCMIX_F2,ZRIMIX_F2)
  ZTHV_UP_F2(:) = ZTHMIX_F2(:)*(1.+ZRVORD*ZRVMIX_F2(:))/(1.+PRT_UP(:))
 
   






 
 

  WHERE ((ZRCMIX(:)>0.).AND.(.NOT.OTESTLCL)) 



     ZCOEFF_QSAT(:) = (ZRCMIX_F2(:) - ZRCMIX(:))/ ZDZ_HALF(:)
     ZRC_ORD(:) = ZRCMIX(:) - ZCOEFF_QSAT(:) * ZDZ_HALF(:)
     ZDZ_STOP = (- ZRC_ORD(:)/ZCOEFF_QSAT(:))     
     ZPART_DRY(:) = ZDZ_STOP / (PZZ(:,KK+1)-PZZ(:,KK))
     GTEST_LOCAL_LCL(:)=.TRUE.
  
  ENDWHERE

IF (KK==IKB) THEN 
  ZCOEFF_MINUS_HALF = 0.
ELSE
  ZCOEFF_MINUS_HALF = ((PTHVM(:,KK)-PTHVM(:,KK-1))/PDZZ(:,KK))
ENDIF

  ZCOEFF_PLUS_HALF  = ((PTHVM(:,KK+1)-PTHVM(:,KK))/PDZZ(:,KK+1))
  
  ZCOTHVU_MINUS_HALF = (ZTHV_UP(:)-ZTHV_UP_F1(:))/ZDZ_HALF(:)
  ZCOTHVU_PLUS_HALF  = (ZTHV_UP_F2(:)-ZTHV_UP(:))/ZDZ_HALF(:)

IF (KK==IKB) THEN 
  ZTHV_MINUS_HALF = PTHVM(:,KK)
ELSE
  ZTHV_MINUS_HALF = ZCOEFF_MINUS_HALF*0.5*(PZZ(:,KK)-PZZ(:,KK-1))+PTHVM(:,KK-1)
ENDIF

  ZTHV_PLUS_HALF  = ZCOEFF_PLUS_HALF*0.5*(PZZ(:,KK+1)-PZZ(:,KK))+ ZTHV_MINUS_HALF  
 
  
          
  PBUO_INTEG = ZG_O_THVREF(:,KK)*ZDZ_HALF(:)*&
              (0.5*( ZCOTHVU_MINUS_HALF - ZCOEFF_MINUS_HALF)*ZDZ_HALF(:) &
                - ZTHV_MINUS_HALF + ZTHV_UP_F1(:) ) 


 
  WHERE ((OTEST).AND.(.NOT.OTESTLCL))
     PENTR=0.
     PDETR=0.

 
     ZBUO_INTEG = ZG_O_THVREF(:,KK)*ZDZ_STOP(:)*&
                (0.5 * (  - ZCOEFF_MINUS_HALF)* ZDZ_STOP(:) &
                  - ZTHV_MINUS_HALF + ZTHV_UP_F1(:) ) 
           
  
     WHERE (ZBUO_INTEG(:)>=0.)
         PENTR = 0.5/(XABUO-XBENTR*XENTR_DRY)*&
                 LOG(1.+ (2.*(XABUO-XBENTR*XENTR_DRY)/PW_UP2(:,KK))* &
                 ZBUO_INTEG)
         PDETR = 0.
    
         ZW2_HALF = PW_UP2(:,KK) +  2*(XABUO-XBENTR*XENTR_DRY)*(ZBUO_INTEG)
     ELSEWHERE
         PENTR = 0.
         PDETR = 0.5/(XABUO)*&
                 LOG(1.+ (2.*(XABUO)/PW_UP2(:,KK))* &
                 MAX(0.,-ZBUO_INTEG))

         ZW2_HALF = PW_UP2(:,KK) +  2*(XABUO)*(ZBUO_INTEG)
     ENDWHERE
     
 ENDWHERE

 
 ZDZ_STOP(:) = ZDZ_HALF(:)
  


 PBUO_INTEG = PBUO_INTEG + ZG_O_THVREF(:,KK)*ZDZ_HALF(:)*&
                (0.5*(ZCOTHVU_PLUS_HALF - ZCOEFF_PLUS_HALF)* ZDZ_HALF(:) - & 
                PTHVM(:,KK) + ZTHV_UP(:) ) 
              
               
 WHERE ((((ZRCMIX_F2(:)>0.).AND.(ZRCMIX(:)<=0.)).AND.(.NOT.OTESTLCL)).AND.(.NOT.GTEST_LOCAL_LCL(:)))
 
 
 
     PT(:)=ZTHMIX_F2(:)*((PPABSM(:,KK+1)/XP00)**(XRD/XCPD)) 
     ZQVSAT(:)=EXP( XALPW - XBETAW/PT(:) - XGAMW*LOG(PT(:))  )
     ZQVSAT(:)=XRD/XRV*ZQVSAT(:)/PPABSM(:,KK+1)   &
                   / (1.+(XRD/XRV-1.)*ZQVSAT(:)/PPABSM(:,KK+1))   
     ZCOEFF_QSAT(:) = (PRT_UP(:) - ZQVSAT(:) - &
                ZRCMIX(:))/ (0.5* (PZZ(:,KK+2)-PZZ(:,KK+1)))
     ZRC_ORD(:) = ZRCMIX_F2(:) - ZCOEFF_QSAT(:) * ZDZ_HALF(:)
     ZDZ_STOP = (- ZRC_ORD(:)/ZCOEFF_QSAT(:))     
     ZPART_DRY(:) = 0.5+ZDZ_STOP  / (PZZ(:,KK+1)-PZZ(:,KK))
     GTEST_LOCAL_LCL2(:)=.TRUE.     
 ENDWHERE


 WHERE (((OTEST).AND.(.NOT.OTESTLCL)).AND.(.NOT.GTEST_LOCAL_LCL(:)))
   
     ZBUO_INTEG = ZG_O_THVREF(:,KK)*ZDZ_STOP(:)*&
                (0.5*( - ZCOEFF_PLUS_HALF)* ZDZ_STOP(:)&
                - PTHVM(:,KK) + ZTHV_UP(:) )

             

     WHERE (ZW2_HALF>0.)
        WHERE (ZBUO_INTEG(:)>=0.)
           PENTR = PENTR + 0.5/(XABUO-XBENTR*XENTR_DRY)* &
                LOG(1.+ (2.*(XABUO-XBENTR*XENTR_DRY)/ZW2_HALF(:)) * ZBUO_INTEG)
          
           PDETR = PDETR
              
        ELSEWHERE
          PENTR = PENTR
          
          PDETR = PDETR + 0.5/(XABUO)* &
                LOG(1.+ (2.*(XABUO)/ZW2_HALF(:)) * &
                MAX(-ZBUO_INTEG,0.))
        ENDWHERE     
     ELSEWHERE
        
           OTEST=.FALSE.
           PENTR = PENTR 
           PDETR = PDETR 
     ENDWHERE
              
 ENDWHERE
 PENTR = XENTR_DRY*PENTR/(PZZ(:,KK+1)-PZZ(:,KK))    
 PDETR = XDETR_DRY*PDETR/(PZZ(:,KK+1)-PZZ(:,KK))
 PDETR = MAX(ZPART_DRY(:)*XDETR_LUP/(PLUP-0.5*(PZZ(:,KK)+PZZ(:,KK+1))),PDETR)


  




 WHERE ((OTEST).AND.(OTESTLCL))
     ZKIC(:) = MAX(0.,ZTHV_UP(:)-PTHVM(:,KK))*ZKIC(:) /  &  
                 (ZTHV_UP(:)-ZTHVMIX_1(:)+1.E-10)
                       
     ZKIC(:) = MAX(0., MIN(1., ZKIC(:)))
    
     ZEPSI(:) = ZKIC(:) **2.
     ZDELTA(:) = (1.-ZKIC(:))**2.
     ZEPSI_CLOUD=MIN(ZDELTA,ZEPSI)
     ZCOEFFMF_CLOUD(:)=XENTR_MF * XG / XCRAD_MF         
     PENTR(:) = ZCOEFFMF_CLOUD(:)*ZEPSI_CLOUD(:)
     PDETR(:) = ZCOEFFMF_CLOUD(:)*ZDELTA(:)
 ENDWHERE
 




 WHERE (((OTEST).AND.(.NOT.(OTESTLCL))).AND.((GTEST_LOCAL_LCL(:).OR.GTEST_LOCAL_LCL2(:))))
     ZKIC(:) = MAX(0.,ZTHV_UP_F2(:)-ZTHV_PLUS_HALF)*ZKIC(:) /  &  
                       (ZTHV_UP_F2(:)-ZTHVMIX_2(:)+1.E-10)                      
     ZKIC(:) = MAX(0., MIN(1., ZKIC(:)))
     ZEPSI(:) = ZKIC(:) **2.
     ZDELTA(:) = (1.-ZKIC(:))**2.
     ZEPSI_CLOUD=MIN(ZDELTA,ZEPSI)
     ZCOEFFMF_CLOUD(:)=XENTR_MF * XG / XCRAD_MF     
     PENTR(:) = PENTR+(1.-ZPART_DRY(:))*ZCOEFFMF_CLOUD(:)*ZEPSI_CLOUD(:)
     PDETR(:) = PDETR+(1.-ZPART_DRY(:))*ZCOEFFMF_CLOUD(:)*ZDELTA(:)
 ENDWHERE
 


END SUBROUTINE COMPUTE_ENTR_DETR  

                                       

      SUBROUTINE COMPUTE_FUNCTION_THERMO_MF( KRR,KRRL,KRRI,                  &
                                       PTH, PR, PEXN, PPABS,                 &
                                       PT,PLVOCPEXN,PLSOCPEXN,               &
                                       PAMOIST,PATHETA                       )










IMPLICIT NONE




INTEGER,                INTENT(IN)   :: KRR           
INTEGER,                INTENT(IN)   :: KRRL          
INTEGER,                INTENT(IN)   :: KRRI          

REAL, DIMENSION(:,:,:), INTENT(IN)   :: PTH      
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PR       
REAL, DIMENSION(:,:,:)  , INTENT(IN) :: PPABS,PEXN    

REAL, DIMENSION(:,:,:), INTENT(OUT)   :: PT      
REAL, DIMENSION(:,:,:), INTENT(OUT)   :: PLVOCPEXN,PLSOCPEXN     

REAL, DIMENSION(:,:,:), INTENT(OUT)  ::  PAMOIST,PATHETA





REAL                :: ZEPS         
REAL, DIMENSION(SIZE(PTH,1),SIZE(PTH,2),SIZE(PTH,3)) ::     &
          ZCP,                        &  
          ZE,                         &  
          ZDEDT,                      &  
          ZFRAC_ICE,                  &  
          ZAMOIST_W,                  &  
          ZATHETA_W,                  &  
          ZAMOIST_I,                  &  
          ZATHETA_I                      

INTEGER             :: JRR



  ZEPS = XMV / XMD

  PLVOCPEXN(:,:,:) = 0.
  PLSOCPEXN(:,:,:) = 0.
  PAMOIST(:,:,:) = 0.
  PATHETA(:,:,:) = 0.
  ZFRAC_ICE(:,:,:) = 0.0
  ZAMOIST_W(:,:,:) = 0.0
  ZATHETA_W(:,:,:) = 0.0
  ZAMOIST_I(:,:,:) = 0.0
  ZATHETA_I(:,:,:) = 0.0



ZCP=XCPD

IF (KRR > 0) ZCP(:,:,:) = ZCP(:,:,:) + XCPV * PR(:,:,:,1)

DO JRR = 2,1+KRRL  
   ZCP(:,:,:)  = ZCP(:,:,:) + XCL * PR(:,:,:,JRR)
END DO

DO JRR = 2+KRRL,1+KRRL+KRRI 
  ZCP(:,:,:)  = ZCP(:,:,:)  + XCI * PR(:,:,:,JRR)
END DO



PT(:,:,:) =  PTH(:,:,:) * PEXN(:,:,:)




IF ( KRRL >= 1 ) THEN 



  PLVOCPEXN(:,:,:) = (XLVTT + (XCPV-XCL) *  (PT(:,:,:)-XTT) ) / ZCP(:,:,:)



  ZE(:,:,:) =  EXP( XALPW - XBETAW/PT(:,:,:) - XGAMW*ALOG( PT(:,:,:) ) )



  ZE(:,:,:) =  ZE(:,:,:) * ZEPS / ( PPABS(:,:,:) - ZE(:,:,:) )



  ZDEDT(:,:,:) = ( XBETAW / PT(:,:,:)  - XGAMW ) / PT(:,:,:)   &
                 * ZE(:,:,:) * ( 1. + ZE(:,:,:) / ZEPS )



  ZAMOIST_W(:,:,:)=  0.5 / ( 1.0 + ZDEDT(:,:,:) * PLVOCPEXN(:,:,:) )



  ZATHETA_W(:,:,:)= ZAMOIST_W(:,:,:) * PEXN(:,:,:) *                       &
        ( ( ZE(:,:,:) - PR(:,:,:,1) ) * PLVOCPEXN(:,:,:) /                  &
          ( 1. + ZDEDT(:,:,:) * PLVOCPEXN(:,:,:) )           *              &
          (                                                                 &
           ZE(:,:,:) * (1. + ZE(:,:,:)/ZEPS)                                &
                        * ( -2.*XBETAW/PT(:,:,:) + XGAMW ) / PT(:,:,:)**2   &
          +ZDEDT(:,:,:) * (1. + 2. * ZE(:,:,:)/ZEPS)                        &
                        * ( XBETAW/PT(:,:,:) - XGAMW ) / PT(:,:,:)          &
          )                                                                 &
         - ZDEDT(:,:,:)                                                     &
        )




  IF ( KRRI >= 1 ) THEN 




    WHERE(PR(:,:,:,2)+PR(:,:,:,4)>0.0)
      ZFRAC_ICE(:,:,:) = PR(:,:,:,4) / ( PR(:,:,:,2)+PR(:,:,:,4) )
    END WHERE



    PLSOCPEXN(:,:,:) = (XLSTT + (XCPV-XCI) *  (PT(:,:,:)-XTT) ) / ZCP(:,:,:)



    ZE(:,:,:) =  EXP( XALPI - XBETAI/PT(:,:,:) - XGAMI*ALOG( PT(:,:,:) ) )



    ZE(:,:,:) =  ZE(:,:,:) * ZEPS / ( PPABS(:,:,:) - ZE(:,:,:) )



    ZDEDT(:,:,:) = ( XBETAI / PT(:,:,:)  - XGAMI ) / PT(:,:,:)   &
                   * ZE(:,:,:) * ( 1. + ZE(:,:,:) / ZEPS )



    ZAMOIST_I(:,:,:)=  0.5 / ( 1.0 + ZDEDT(:,:,:) * PLSOCPEXN(:,:,:) )



    ZATHETA_I(:,:,:)= ZAMOIST_I(:,:,:) * PEXN(:,:,:) *                     &
        ( ( ZE(:,:,:) - PR(:,:,:,1) ) * PLSOCPEXN(:,:,:) /                  &
          ( 1. + ZDEDT(:,:,:) * PLSOCPEXN(:,:,:) )           *              &
          (                                                                 &
           ZE(:,:,:) * (1. + ZE(:,:,:)/ZEPS)                                &
                        * ( -2.*XBETAI/PT(:,:,:) + XGAMI ) / PT(:,:,:)**2   &
          +ZDEDT(:,:,:) * (1. + 2. * ZE(:,:,:)/ZEPS)                        &
                        * ( XBETAI/PT(:,:,:) - XGAMI ) / PT(:,:,:)          &
          )                                                                 &
         - ZDEDT(:,:,:)                                                     &
        )


  ENDIF

  PAMOIST(:,:,:) = (1.0-ZFRAC_ICE(:,:,:))*ZAMOIST_W(:,:,:) &
                         +ZFRAC_ICE(:,:,:) *ZAMOIST_I(:,:,:)
  PATHETA(:,:,:) = (1.0-ZFRAC_ICE(:,:,:))*ZATHETA_W(:,:,:) &
                         +ZFRAC_ICE(:,:,:) *ZATHETA_I(:,:,:)




  PLVOCPEXN(:,:,:) = PLVOCPEXN(:,:,:) / PEXN(:,:,:)
  PLSOCPEXN(:,:,:) = PLSOCPEXN(:,:,:) / PEXN(:,:,:)

ENDIF

END SUBROUTINE COMPUTE_FUNCTION_THERMO_MF


      SUBROUTINE COMPUTE_UPDRAFT(OMIXUV,PZZ,PDZZ,KK,              &
                                 PSFTH,PSFRV,                     &
                                 PPABSM,PRHODREF,PUM,PVM, PTKEM,  &
                                 PTHM,PRVM,PRCM,PRIM,PTHLM,PRTM,  &
                                 PTHL_UP,PRT_UP,                  &
                                 PRV_UP,PRC_UP,PRI_UP,PTHV_UP,    &
                                 PW_UP,PU_UP, PV_UP,              &
                                 PFRAC_UP,PEMF,PDETR,PENTR,       &
                                 KKLCL,KKETL,KKCTL)








IMPLICIT NONE





LOGICAL,                INTENT(IN) :: OMIXUV    
REAL, DIMENSION(:,:), INTENT(IN)   :: PZZ       
REAL, DIMENSION(:,:), INTENT(IN)   :: PDZZ      
 
INTEGER,              INTENT(IN)   ::  KK        
REAL, DIMENSION(:),   INTENT(IN)   ::  PSFTH,PSFRV


REAL, DIMENSION(:,:),   INTENT(IN) ::  PPABSM     
REAL, DIMENSION(:,:),   INTENT(IN) ::  PRHODREF   
                                                  
REAL, DIMENSION(:,:),   INTENT(IN) ::  PUM        
REAL, DIMENSION(:,:),   INTENT(IN) ::  PVM        
REAL, DIMENSION(:,:),   INTENT(IN) ::  PTKEM      

REAL, DIMENSION(:,:),   INTENT(IN)   ::  PTHM           
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PRVM,PRCM,PRIM 
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PTHLM,PRTM     


REAL, DIMENSION(:,:),   INTENT(OUT)  ::  PTHL_UP,PRT_UP   
REAL, DIMENSION(:,:),   INTENT(OUT)  ::  PU_UP, PV_UP     
REAL, DIMENSION(:,:),   INTENT(OUT)  ::  PRV_UP,PRC_UP, & 
                                         PRI_UP,PTHV_UP,& 
                                         PW_UP,PFRAC_UP   

                                         
REAL, DIMENSION(:,:),   INTENT(OUT)  ::  PEMF,PDETR,PENTR 
                                                          
INTEGER, DIMENSION(:),  INTENT(OUT)  ::  KKLCL,KKETL,KKCTL




REAL, DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2)) ::    &
                        ZTHM_F,ZRVM_F,ZRCM_F,ZRIM_F   
                                                      
REAL, DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2)) ::    &
                        ZRTM_F, ZTHLM_F, ZTKEM_F,&    
                        ZUM_F,ZVM_F,ZRHO_F,      &    
                        ZPRES_F,ZTHVM_F,ZTHVM,   &    
                        ZG_O_THVREF,             &    
                        ZW_UP2                        

                        
REAL, DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2)) ::  &
                        ZTH_UP,                  &    
                        ZFRAC_ICE                     

REAL, DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2)) ::  ZCOEF  
                        
REAL, DIMENSION(SIZE(PSFTH,1) )            ::  ZWTHVSURF  
CHARACTER(LEN=1)                           ::  YFRAC_ICE  
                                                          

REAL  :: ZRDORV       
REAL  :: ZRVORD       


REAL, DIMENSION(SIZE(PTHM,1)) :: ZMIX1,ZMIX2,ZMIX3
REAL, DIMENSION(SIZE(PTHM,1)) :: ZBUO_INTEG           

REAL, DIMENSION(SIZE(PTHM,1)) :: ZLUP         
REAL, DIMENSION(SIZE(PTHM,1)) :: ZDEPTH       

INTEGER  :: IKB,IKE            
                               
INTEGER  :: IKU           
INTEGER  :: JK,JI,JSV          

LOGICAL, DIMENSION(SIZE(PTHM,1)) ::  GTEST,GTESTLCL,GTESTETL
                               
LOGICAL                          ::  GLMIX 
                               
LOGICAL, DIMENSION(SIZE(PTHM,1))              :: GWORK1
LOGICAL, DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2)) :: GWORK2

INTEGER  :: ITEST
REAL  :: ZTMAX,ZRMAX  


ZTMAX=2.0
ZRMAX=1.E-3






IKU=SIZE(PTHM,2)
IKB=1        
IKE=IKU-1      


KKLCL(:)=IKE
KKETL(:)=IKE
KKCTL(:)=IKE



PRV_UP(:,:)=0.
PRC_UP(:,:)=0.

PW_UP(:,:)=0.
PEMF(:,:)=0.
PDETR(:,:)=0.
PENTR(:,:)=0.
ZTH_UP(:,:)=0.
PFRAC_UP(:,:)=0.
PTHV_UP(:,:)=0.


PRI_UP(:,:)=0.
ZFRAC_ICE(:,:)=0.
YFRAC_ICE='T'

ZBUO_INTEG=0.

ZRDORV   = XRD / XRV   
ZRVORD   = (XRV / XRD) 







ZTHM_F (:,IKB+1:IKU) = 0.5*(PTHM(:,IKB:IKU-1)+PTHM(:,IKB+1:IKU))
ZTHLM_F(:,IKB+1:IKU) = 0.5*(PTHLM(:,IKB:IKU-1)+PTHLM(:,IKB+1:IKU))
ZRTM_F (:,IKB+1:IKU) = 0.5*(PRTM(:,IKB:IKU-1)+PRTM(:,IKB+1:IKU))
ZTKEM_F(:,IKB+1:IKU) = 0.5*(PTKEM(:,IKB:IKU-1)+PTKEM(:,IKB+1:IKU))
ZPRES_F(:,IKB+1:IKU) = 0.5*(PPABSM(:,IKB:IKU-1)+PPABSM(:,IKB+1:IKU))
ZRHO_F (:,IKB+1:IKU) = 0.5*(PRHODREF(:,IKB:IKU-1)+PRHODREF(:,IKB+1:IKU))
ZRVM_F (:,IKB+1:IKU) = 0.5*(PRVM(:,IKB:IKU-1)+PRVM(:,IKB+1:IKU))
ZUM_F  (:,IKB+1:IKU) = 0.5*(PUM(:,IKB:IKU-1)+PUM(:,IKB+1:IKU))
ZVM_F  (:,IKB+1:IKU) = 0.5*(PVM(:,IKB:IKU-1)+PVM(:,IKB+1:IKU))



ZTHM_F (:,IKB) = PTHM(:,IKB) 
ZTHLM_F(:,IKB) = PTHLM(:,IKB)
ZRTM_F (:,IKB) = PRTM (:,IKB)
ZTKEM_F(:,IKB) = PTKEM(:,IKB)
ZPRES_F(:,IKB) = PPABSM(:,IKB)
ZRHO_F(:,IKB)  = PRHODREF(:,IKB)
ZRVM_F(:,IKB)  = PRVM(:,IKB)
ZUM_F(:,IKB)   = PUM(:,IKB)
ZVM_F(:,IKB)   = PVM(:,IKB)



ZTHVM_F(:,:)=ZTHM_F(:,:)*((1.+ZRVORD*ZRVM_F(:,:))/(1.+ZRTM_F(:,:)))
ZTHVM(:,:)=PTHM(:,:)*((1.+ZRVORD*PRVM(:,:))/(1.+PRTM(:,:)))




PTHL_UP(:,:)=ZTHLM_F(:,:)
PRT_UP(:,:)=ZRTM_F(:,:)
ZW_UP2(:,:)=0.
PTHV_UP(:,:)=ZTHVM_F(:,:)
PU_UP(:,:)=ZUM_F(:,:)
PV_UP(:,:)=ZVM_F(:,:)








PTHL_UP(:,KK)= ZTHLM_F(:,KK)+MAX(0.,MIN(ZTMAX,(PSFTH(:)/SQRT(ZTKEM_F(:,KK)))*XALP_PERT))
PRT_UP(:,KK) = ZRTM_F(:,KK)+MAX(0.,MIN(ZRMAX,(PSFRV(:)/SQRT(ZTKEM_F(:,KK)))*XALP_PERT)) 



ZW_UP2(:,KK) = MAX(0.0001,(2./3.)*ZTKEM_F(:,KK))




CALL TH_R_FROM_THL_RT_2D(YFRAC_ICE,ZFRAC_ICE(:,KK:KK),ZPRES_F(:,KK:KK), &
             PTHL_UP(:,KK:KK),PRT_UP(:,KK:KK),ZTH_UP(:,KK:KK), &
             PRV_UP(:,KK:KK),PRC_UP(:,KK:KK),PRI_UP(:,KK:KK))


PTHV_UP(:,KK) = ZTH_UP(:,KK)*((1+ZRVORD*PRV_UP(:,KK))/(1+PRT_UP(:,KK))) 
                                                            

ZG_O_THVREF=XG/ZTHVM_F


GLMIX=.TRUE.
ZTKEM_F(:,KK)=0.

CALL COMPUTE_BL89_ML(PDZZ,ZTKEM_F,ZG_O_THVREF,ZTHVM_F,KK,GLMIX,ZLUP)
ZLUP(:)=MAX(ZLUP(:),1.E-10)


ZWTHVSURF(:) = (ZTHVM_F(:,IKB)/ZTHM_F(:,IKB))*PSFTH(:)+     &
                (0.61*ZTHM_F(:,IKB))*PSFRV(:)


WHERE (ZWTHVSURF(:)>0.)
  PEMF(:,KK) = XCMF * ZRHO_F(:,KK) * ((ZG_O_THVREF(:,KK))*ZWTHVSURF*ZLUP)**(1./3.)
  PFRAC_UP(:,KK)=MIN(PEMF(:,KK)/(SQRT(ZW_UP2(:,KK))*ZRHO_F(:,KK)),XFRAC_UP_MAX)
  ZW_UP2(:,KK)=(PEMF(:,KK)/(PFRAC_UP(:,KK)*ZRHO_F(:,KK)))**2
  GTEST(:)=.TRUE.
ELSEWHERE
  PEMF(:,KK) =0.
  GTEST(:)=.FALSE.
ENDWHERE










GTESTLCL(:)=.FALSE.
GTESTETL(:)=.FALSE.



DO JK=KK,IKE-1

ITEST=COUNT(GTEST)
IF (ITEST==0) CYCLE






  WHERE(GTEST)
  WHERE ((PRC_UP(:,JK)>0.).AND.(.NOT.(GTESTLCL)))
      KKLCL(:) = JK           
      GTESTLCL(:)=.TRUE.
  ENDWHERE
  ENDWHERE




 CALL COMPUTE_ENTR_DETR(GTEST,GTESTLCL,YFRAC_ICE,ZFRAC_ICE(:,JK),JK,&
                       PPABSM(:,:),PZZ(:,:),PDZZ(:,:),ZTHVM(:,:),&
                       PTHLM(:,JK),PRTM(:,JK),ZW_UP2(:,:),   &
                       PTHL_UP(:,JK),PRT_UP(:,JK),ZLUP(:), &
                       PENTR(:,JK),PDETR(:,JK),ZBUO_INTEG)
  

  IF (JK==KK) THEN
       PDETR(:,JK)=0.
  ENDIF   

 

  WHERE(GTEST)
    ZMIX1(:)=0.5*(PZZ(:,JK+1)-PZZ(:,JK))*(PENTR(:,JK)-PDETR(:,JK))
    PEMF(:,JK+1)=PEMF(:,JK)*EXP(2*ZMIX1(:))
  ENDWHERE
  
  


  WHERE (GTEST.AND.(PEMF(:,JK+1)<=0.))
    PEMF(:,JK+1)=0.
    GTEST(:)=.FALSE.
    KKCTL(:) = JK+1         
  ENDWHERE



  WHERE(GTEST)     
    ZMIX2(:) = (PZZ(:,JK+1)-PZZ(:,JK))*PENTR(:,JK) 
    ZMIX3(:) = (PZZ(:,JK+1)-PZZ(:,JK))*PDETR(:,JK) 
                
    PTHL_UP(:,JK+1) = (PTHL_UP(:,JK)*(1.-0.5*ZMIX2(:)) + PTHLM(:,JK)*ZMIX2(:)) &
                          /(1.+0.5*ZMIX2(:))   
    PRT_UP(:,JK+1) = (PRT_UP (:,JK)*(1.-0.5*ZMIX2(:)) + PRTM(:,JK)*ZMIX2(:))   &
                          /(1.+0.5*ZMIX2(:))
  ENDWHERE
  

  IF(OMIXUV) THEN
    WHERE(GTEST) 
      PU_UP(:,JK+1) = (PU_UP (:,JK)*(1-0.5*ZMIX2(:)) + PUM(:,JK)*ZMIX2(:)+ &
                        0.5*XPRES_UV*(PZZ(:,JK+1)-PZZ(:,JK))*&
                        ((PUM(:,JK+1)-PUM(:,JK))/PDZZ(:,JK+1)+&
                         (PUM(:,JK)-PUM(:,JK-1))/PDZZ(:,JK))        )   &
                        /(1+0.5*ZMIX2(:))
      PV_UP(:,JK+1) = (PV_UP (:,JK)*(1-0.5*ZMIX2(:)) + PVM(:,JK)*ZMIX2(:)+ &
                        0.5*XPRES_UV*(PZZ(:,JK+1)-PZZ(:,JK))*&
                        ((PVM(:,JK+1)-PVM(:,JK))/PDZZ(:,JK+1)+&
                         (PVM(:,JK)-PVM(:,JK-1))/PDZZ(:,JK))    )   &
                        /(1+0.5*ZMIX2(:))
    ENDWHERE
  ENDIF
  

  CALL TH_R_FROM_THL_RT_2D(YFRAC_ICE,ZFRAC_ICE(:,JK+1:JK+1),ZPRES_F(:,JK+1:JK+1), &
          PTHL_UP(:,JK+1:JK+1),PRT_UP(:,JK+1:JK+1),ZTH_UP(:,JK+1:JK+1), &
          PRV_UP(:,JK+1:JK+1),PRC_UP(:,JK+1:JK+1),PRI_UP(:,JK+1:JK+1))
  


  WHERE(GTEST)
      PTHV_UP(:,JK+1) = ZTH_UP(:,JK+1)*((1+ZRVORD*PRV_UP(:,JK+1))/(1+PRT_UP(:,JK+1)))
   WHERE (.NOT.(GTESTLCL)) 
      WHERE (ZBUO_INTEG(:)>0.)
        ZW_UP2(:,JK+1)  = ZW_UP2(:,JK) + 2.*(XABUO-XBENTR*XENTR_DRY)* ZBUO_INTEG(:)              
      ENDWHERE
      WHERE (ZBUO_INTEG(:)<=0.)
        ZW_UP2(:,JK+1)  = ZW_UP2(:,JK) + 2.*XABUO* ZBUO_INTEG(:)
      ENDWHERE      
   ENDWHERE      
   WHERE (GTESTLCL)
      ZW_UP2(:,JK+1)  = ZW_UP2(:,JK)*(1.-(XBDETR*ZMIX3(:)+XBENTR*ZMIX2(:)))&
            /(1.+(XBDETR*ZMIX3(:)+XBENTR*ZMIX2(:))) &
            +2.*(XABUO)*ZBUO_INTEG/(1.+(XBDETR*ZMIX3(:)+XBENTR*ZMIX2(:)))
   ENDWHERE
 ENDWHERE



  GTESTETL(:)=.FALSE.
  WHERE (GTEST.AND.(ZBUO_INTEG(:)<=0.))
      KKETL(:) = JK+1           
      GTESTETL(:)=.TRUE.
  ENDWHERE


  WHERE (GTEST.AND.((ZW_UP2(:,JK+1)<=0.).OR.(PEMF(:,JK+1)<=0.)))
      ZW_UP2(:,JK+1)=0.
      PEMF(:,JK+1)=0.
      GTEST(:)=.FALSE.
      PTHL_UP(:,JK+1)=ZTHLM_F(:,JK+1)
      PRT_UP(:,JK+1)=ZRTM_F(:,JK+1)
      PRC_UP(:,JK+1)=0.
      PRV_UP(:,JK+1)=0.
      PTHV_UP(:,JK+1)=ZTHVM_F(:,JK+1)
      PFRAC_UP(:,JK+1)=0.
      KKCTL(:)=JK+1
  ENDWHERE
 

  WHERE (GTEST)
      PFRAC_UP(:,JK+1)=PEMF(:,JK+1)/(SQRT(ZW_UP2(:,JK+1))*ZRHO_F(:,JK+1))
  ENDWHERE


  WHERE (GTEST)
      PFRAC_UP(:,JK+1)=MIN(XFRAC_UP_MAX,PFRAC_UP(:,JK+1))
  ENDWHERE



  
  WHERE ((GTEST.AND.GTESTETL).AND.GTESTLCL)
    PFRAC_UP(:,JK+1)=MIN(PFRAC_UP(:,JK+1),PFRAC_UP(:,JK))
  ENDWHERE


  
  PEMF(:,JK+1)=PFRAC_UP(:,JK+1)*SQRT(ZW_UP2(:,JK+1))*ZRHO_F(:,JK+1)

ENDDO


PW_UP(:,:)=SQRT(ZW_UP2(:,:))

PEMF(:,KK) =0.
PEMF(:,IKU)=0. 
PFRAC_UP(:,IKU)=0.


DO JI=1,SIZE(PTHM,1) 
   ZDEPTH(JI) = (PZZ(JI,KKCTL(JI)) -  PZZ(JI,KKLCL(JI)) )
END DO

GWORK1(:)= (GTESTLCL(:) .AND. (ZDEPTH(:) > 3000.) )
GWORK2(:,:) = SPREAD( GWORK1(:), DIM=2, NCOPIES=IKU )
ZCOEF(:,:) = SPREAD( (1.-(ZDEPTH(:)-3000.)/1000.), DIM=2, NCOPIES=IKU)
ZCOEF=MIN(MAX(ZCOEF,0.),1.)

WHERE (GWORK2) 
   PEMF(:,:)     = PEMF(:,:)     * ZCOEF(:,:)
   PFRAC_UP(:,:) = PFRAC_UP(:,:) * ZCOEF(:,:)
ENDWHERE


END SUBROUTINE COMPUTE_UPDRAFT


                

      SUBROUTINE MF_TURB(OMIXUV, PIMPL, PTSTEP,                       &
                PTSTEP_MET,  PDZZ,                          &
                PRHODJ,                                               &
                PTHLM,PTHVM,PRTM,PUM,PVM,                             &
                PTHLDT,PRTDT,PUDT,PVDT,                               &
                PEMF,PTHL_UP,PTHV_UP,PRT_UP,PU_UP,PV_UP,              &
                PFLXZTHMF,PFLXZTHVMF,PFLXZRMF,PFLXZUMF,PFLXZVMF       )












IMPLICIT NONE





LOGICAL,                INTENT(IN)   :: OMIXUV      
REAL,                   INTENT(IN)   :: PIMPL       
REAL,                 INTENT(IN)     ::  PTSTEP   
REAL,                 INTENT(IN)     ::  PTSTEP_MET

REAL, DIMENSION(:,:,:), INTENT(IN)   :: PDZZ        

REAL, DIMENSION(:,:,:), INTENT(IN)   :: PRHODJ      


REAL, DIMENSION(:,:,:), INTENT(IN) ::  PTHLM        
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PRTM         

REAL, DIMENSION(:,:,:), INTENT(IN) ::  PTHVM 

REAL, DIMENSION(:,:,:), INTENT(IN) ::  PUM
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PVM


REAL, DIMENSION(:,:,:),   INTENT(OUT) ::  PTHLDT

REAL, DIMENSION(:,:,:),   INTENT(OUT) ::  PRTDT 

REAL, DIMENSION(:,:,:),   INTENT(OUT) ::  PUDT
REAL, DIMENSION(:,:,:),   INTENT(OUT) ::  PVDT




REAL, DIMENSION(:,:,:), INTENT(IN)   ::  PEMF,PTHL_UP,PTHV_UP,PRT_UP,PU_UP,PV_UP

REAL, DIMENSION(:,:,:), INTENT(OUT)  ::  PFLXZTHMF,PFLXZTHVMF,PFLXZRMF,PFLXZUMF,PFLXZVMF









REAL, DIMENSION(SIZE(PTHLM,1),SIZE(PTHLM,2),SIZE(PTHLM,3)) :: ZVARS










PFLXZTHMF = 0.
PFLXZRMF = 0.
PFLXZTHVMF = 0.
PFLXZUMF = 0.
PFLXZVMF = 0.
PTHLDT = 0.
PRTDT = 0.
PUDT = 0.
PVDT = 0.











PFLXZTHMF(:,:,:) = PEMF(:,:,:)*(PTHL_UP(:,:,:)-MZM(PTHLM(:,:,:)))

PFLXZRMF(:,:,:) =  PEMF(:,:,:)*(PRT_UP(:,:,:)-MZM(PRTM(:,:,:)))

PFLXZTHVMF(:,:,:) = PEMF(:,:,:)*(PTHV_UP(:,:,:)-MZM(PTHVM(:,:,:)))

PFLXZTHVMF(:,:,:) = 9.81/PTHVM(:,:,:)* PEMF(:,:,:)*(PTHV_UP(:,:,:)-MZM(PTHVM(:,:,:))) 

IF (OMIXUV) THEN
  PFLXZUMF(:,:,:) =  PEMF(:,:,:)*(PU_UP(:,:,:)-MZM(PUM(:,:,:)))
  PFLXZVMF(:,:,:) =  PEMF(:,:,:)*(PV_UP(:,:,:)-MZM(PVM(:,:,:)))
ENDIF














CALL TRIDIAG_MASSFLUX(PTHLM,PFLXZTHMF,-PEMF,PTSTEP_MET,PIMPL,  &
                      PDZZ,PRHODJ,ZVARS )

PFLXZTHMF(:,:,:) = PEMF(:,:,:)*(PTHL_UP(:,:,:)-MZM(ZVARS(:,:,:)))



PTHLDT(:,:,:)= (ZVARS(:,:,:)-PTHLM(:,:,:))/PTSTEP_MET




CALL TRIDIAG_MASSFLUX(PRTM(:,:,:),PFLXZRMF,-PEMF,PTSTEP_MET,PIMPL,  &
                                 PDZZ,PRHODJ,ZVARS )

PFLXZRMF(:,:,:) =  PEMF(:,:,:)*(PRT_UP(:,:,:)-MZM(ZVARS(:,:,:)))


PRTDT(:,:,:) = (ZVARS(:,:,:)-PRTM(:,:,:))/PTSTEP_MET










IF (OMIXUV) THEN





 CALL TRIDIAG_MASSFLUX(PUM,PFLXZUMF,-PEMF,PTSTEP,PIMPL,  &
                                 PDZZ,PRHODJ,ZVARS )

PFLXZUMF(:,:,:) = PEMF(:,:,:)*(PU_UP(:,:,:)-MZM(ZVARS(:,:,:)))


  PUDT(:,:,:)= (ZVARS(:,:,:)-PUM(:,:,:))/PTSTEP_MET







  CALL TRIDIAG_MASSFLUX(PVM,PFLXZVMF,-PEMF,PTSTEP,PIMPL,  &
                                 PDZZ,PRHODJ,ZVARS )

PFLXZVMF(:,:,:) = PEMF(:,:,:)*(PV_UP(:,:,:)-MZM(ZVARS(:,:,:)))


  PVDT(:,:,:)= (ZVARS(:,:,:)-PVM(:,:,:))/PTSTEP_MET

ENDIF




END SUBROUTINE MF_TURB   

FUNCTION MZM(PA)  RESULT(PMZM)



IMPLICIT NONE





REAL, DIMENSION(:,:,:), INTENT(IN)                :: PA     
REAL, DIMENSION(SIZE(PA,1),SIZE(PA,2),SIZE(PA,3)) :: PMZM   





INTEGER :: JK             
INTEGER :: IKU            

IKU = SIZE(PA,3)


DO JK=2,IKU
   PMZM(:,:,JK)=0.5*(PA(:,:,JK)+PA(:,:,JK-1))
ENDDO

PMZM(:,:,1)=PA(:,:,2)


END FUNCTION MZM





      SUBROUTINE TH_R_FROM_THL_RT_1D(HFRAC_ICE,PFRAC_ICE,PP,             &
                                  PTHL, PRT, PTH, PRV, PRL, PRI       )












IMPLICIT NONE




CHARACTER*1           , INTENT(IN) :: HFRAC_ICE
REAL, DIMENSION(:), INTENT(INOUT) :: PFRAC_ICE
REAL, DIMENSION(:), INTENT(IN) :: PP          
REAL, DIMENSION(:), INTENT(IN) :: PTHL    
                                             
REAL, DIMENSION(:),INTENT(IN)  :: PRT    
                                             
                                             
REAL, DIMENSION(:), INTENT(OUT):: PTH    
REAL, DIMENSION(:), INTENT(OUT):: PRV    
REAL, DIMENSION(:), INTENT(OUT):: PRL    
REAL, DIMENSION(:), INTENT(OUT):: PRI    






INTEGER                       :: II

REAL                          :: ZCOEF
REAL, DIMENSION(SIZE(PP,1))   :: ZEXN,ZFOES,ZQVSAT
REAL, DIMENSION(SIZE(PTHL,1)) :: ZRVSAT,ZCPH,ZRLTEMP  
REAL, DIMENSION(SIZE(PTHL,1)) :: ZT,ZLOVCPEXN,ZLOSCPEXN






ZCOEF = 0.8
PRL(:)=0.
PRI(:)=0.
ZRLTEMP(:)=0.
PRV(:)=PRT(:)
PTH(:)=PTHL(:)
ZEXN(:)=(PP(:)/XP00) ** (XRD/XCPD)




    DO II=1,20
      ZT(:)=PTH(:)*ZEXN(:)

      WHERE (ZT(:) > 273.15)

      ZFOES(:) = EXP( XALPW - XBETAW/ZT(:) - XGAMW*LOG(ZT(:))  )
      ZQVSAT(:) = XRD/XRV*ZFOES(:)/PP(:)   &
                   / (1.+(XRD/XRV-1.)*ZFOES(:)/PP(:))

      ZRVSAT(:) = (1-ZCOEF)*ZQVSAT(:)*(1+PRT(:))+(ZCOEF)*PRV(:)

      PFRAC_ICE(:)=0.
      ZRLTEMP(:)=MAX(0.,PRV(:)-ZRVSAT(:))
      PRV(:)=PRV(:)-ZRLTEMP(:)
      PRL(:)=PRL(:)+PRI(:)+ZRLTEMP(:)
      PRI(:)    = PFRAC_ICE(:)    * (PRL(:))
      PRL(:)    = (1-PFRAC_ICE(:))* (PRT(:) - PRV(:))
      
      ZCPH(:)=XCPD+ XCPV * PRV(:)+ XCL * PRL(:) + XCI * PRI(:)
      
      
      ZLOVCPEXN(:) = (XLVTT + (XCPV-XCL) * (ZT(:)-XTT)) &
                        /(ZCPH*ZEXN(:))
      ZLOSCPEXN(:) = (XLSTT + (XCPV-XCI) * (ZT(:)-XTT)) &
                        /(ZCPH*ZEXN(:))
      PTH(:)=PTHL(:)+ZLOVCPEXN*PRL(:)+ZLOSCPEXN(:)*PRI(:)          

      ELSEWHERE
             
             
        PRL(:)=0.
        PRI(:)=0.
        PRV(:)=PRT(:)
        PTH(:)=PTHL(:)
      ENDWHERE 
    ENDDO
   
END SUBROUTINE TH_R_FROM_THL_RT_1D

              

      SUBROUTINE TH_R_FROM_THL_RT_2D(HFRAC_ICE,PFRAC_ICE,PP,             &
                                  PTHL, PRT, PTH, PRV, PRL, PRI       )












IMPLICIT NONE




CHARACTER*1         , INTENT(IN) :: HFRAC_ICE
REAL, DIMENSION(:,:), INTENT(INOUT) :: PFRAC_ICE
REAL, DIMENSION(:,:), INTENT(IN) :: PP          
REAL, DIMENSION(:,:), INTENT(IN) :: PTHL    
                                             
REAL, DIMENSION(:,:),INTENT(IN)  :: PRT    
                                             
                                             
REAL, DIMENSION(:,:), INTENT(OUT):: PTH    
REAL, DIMENSION(:,:), INTENT(OUT):: PRV    
REAL, DIMENSION(:,:), INTENT(OUT):: PRL    
REAL, DIMENSION(:,:), INTENT(OUT):: PRI    






INTEGER                :: II

REAL                                         :: ZCOEF
REAL, DIMENSION(SIZE(PP,1),SIZE(PP,2))       :: ZEXN,ZFOES,ZQVSAT
REAL, DIMENSION(SIZE(PTHL,1),SIZE(PTHL,2))   :: ZRVSAT,ZCPH,ZRLTEMP  
REAL, DIMENSION(SIZE(PTHL,1),SIZE(PTHL,2))   :: ZT,ZLOVCPEXN,ZLOSCPEXN






ZCOEF = 0.8
PRL(:,:)=0.
PRI(:,:)=0.
ZRLTEMP(:,:)=0.
PRV(:,:)=PRT(:,:)
PTH(:,:)=PTHL(:,:)
ZEXN(:,:)=(PP(:,:)/XP00) ** (XRD/XCPD)




    DO II=1,20
      ZT(:,:)=PTH(:,:)*ZEXN(:,:)
      WHERE (ZT(:,:) > 273.15)


      ZFOES(:,:) = EXP( XALPW - XBETAW/ZT(:,:) - XGAMW*LOG(ZT(:,:))  )
      ZQVSAT(:,:) = XRD/XRV*ZFOES(:,:)/PP(:,:)   &
                   / (1.+(XRD/XRV-1.)*ZFOES(:,:)/PP(:,:))
      ZRVSAT(:,:) = (1-ZCOEF)*ZQVSAT(:,:)*(1+PRT(:,:))+(ZCOEF)*PRV(:,:)


      PFRAC_ICE(:,:) = 0.
      ZRLTEMP(:,:)=MAX(0.,PRV(:,:)-ZRVSAT(:,:))
      PRV(:,:)=PRV(:,:)-ZRLTEMP(:,:)
      PRL(:,:)=PRL(:,:)+PRI(:,:)+ZRLTEMP(:,:)
      PRI(:,:)    = PFRAC_ICE(:,:)    * (PRL(:,:))
      PRL(:,:)    = (1-PFRAC_ICE(:,:))* (PRT(:,:) - PRV(:,:))
      
        ZCPH(:,:)=XCPD+ XCPV * PRV(:,:)+ XCL * PRL(:,:) + XCI * PRI(:,:)
      
        
        ZLOVCPEXN(:,:) = (XLVTT + (XCPV-XCL) * (ZT(:,:)-XTT)) &
                        /(ZCPH*ZEXN(:,:))
        ZLOSCPEXN(:,:) = (XLSTT + (XCPV-XCI) * (ZT(:,:)-XTT)) &
                        /(ZCPH*ZEXN(:,:))
        PTH(:,:)=PTHL(:,:)+ZLOVCPEXN*PRL(:,:)+ZLOSCPEXN(:,:)*PRI(:,:)          
      ELSEWHERE
        
        
        PRL(:,:)=0.
        PRI(:,:)=0.
        PRV(:,:)=PRT(:,:)
        PTH(:,:)=PTHL(:,:)
       ENDWHERE   
    ENDDO
   
END SUBROUTINE TH_R_FROM_THL_RT_2D


                                       

      SUBROUTINE THL_RT_FROM_TH_R_MF( KRR,KRRL,KRRI,                  &
                                       PTH, PR, PLVOCPEXN, PLSOCPEXN, &
                                       PTHL, PRT                      )













IMPLICIT NONE




INTEGER,                INTENT(IN)   :: KRR           
INTEGER,                INTENT(IN)   :: KRRL          
INTEGER,                INTENT(IN)   :: KRRI          

REAL, DIMENSION(:,:,:), INTENT(IN)   :: PTH      
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PR       
REAL, DIMENSION(:,:,:), INTENT(IN)   :: PLVOCPEXN, PLSOCPEXN      

REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PTHL     
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PRT      












IF ( KRRL >= 1 ) THEN
  IF ( KRRI >= 1 ) THEN 
    
    PRT(:,:,:)  = PR(:,:,:,1)  + PR(:,:,:,2)  + PR(:,:,:,4)
    
    PTHL(:,:,:)  = PTH(:,:,:)  - PLVOCPEXN(:,:,:) * PR(:,:,:,2) &
                               - PLSOCPEXN(:,:,:) * PR(:,:,:,4)
  ELSE
    
    PRT(:,:,:)  = PR(:,:,:,1)  + PR(:,:,:,2) 
    
    PTHL(:,:,:) = PTH(:,:,:)  - PLVOCPEXN(:,:,:) * PR(:,:,:,2)
  END IF
ELSE
    
PRT(:,:,:)  = PR(:,:,:,1)
    
PTHL(:,:,:) = PTH(:,:,:)
END IF

END SUBROUTINE THL_RT_FROM_TH_R_MF



       SUBROUTINE TRIDIAG_MASSFLUX(PVARM,PF,PDFDT,PTSTEP,PIMPL,  &
                                 PDZZ,PRHODJ,PVARP             )












IMPLICIT NONE




REAL, DIMENSION(:,:,:), INTENT(IN) :: PVARM   
REAL, DIMENSION(:,:,:), INTENT(IN) :: PF      
REAL, DIMENSION(:,:,:), INTENT(IN) :: PDFDT   
REAL,                   INTENT(IN) :: PTSTEP  
REAL,                   INTENT(IN) :: PIMPL   
REAL, DIMENSION(:,:,:), INTENT(IN) :: PDZZ    
REAL, DIMENSION(:,:,:), INTENT(IN) :: PRHODJ  

REAL, DIMENSION(:,:,:), INTENT(OUT):: PVARP   




REAL, DIMENSION(SIZE(PVARM,1),SIZE(PVARM,2),SIZE(PVARM,3))  :: ZRHODJ_DFDT_O_DZ
REAL, DIMENSION(SIZE(PVARM,1),SIZE(PVARM,2),SIZE(PVARM,3))  :: ZMZM_RHODJ
REAL, DIMENSION(SIZE(PVARM,1),SIZE(PVARM,2),SIZE(PVARM,3))  :: ZA, ZB, ZC
REAL, DIMENSION(SIZE(PVARM,1),SIZE(PVARM,2),SIZE(PVARM,3))  :: ZY ,ZGAM 
                                         
REAL, DIMENSION(SIZE(PVARM,1),SIZE(PVARM,2))                :: ZBET
                                         
INTEGER                              :: JK            
INTEGER                              :: IKB,IKE       






IKB=1
IKE=SIZE(PVARM,3)-1 

ZMZM_RHODJ  = MZM(PRHODJ)
ZRHODJ_DFDT_O_DZ = ZMZM_RHODJ*PDFDT/PDZZ

ZA=0.
ZB=0.
ZC=0.
ZY=0.





ZY(:,:,IKB) = PRHODJ(:,:,IKB)*PVARM(:,:,IKB)/PTSTEP             &
    - ZMZM_RHODJ(:,:,IKB+1) * PF(:,:,IKB+1)/PDZZ(:,:,IKB+1)     &
    + ZMZM_RHODJ(:,:,IKB  ) * PF(:,:,IKB  )/PDZZ(:,:,IKB  )     &
    + ZRHODJ_DFDT_O_DZ(:,:,IKB+1) * 0.5*PIMPL * PVARM(:,:,IKB+1)    &
    + ZRHODJ_DFDT_O_DZ(:,:,IKB+1) * 0.5*PIMPL * PVARM(:,:,IKB  )

DO JK=IKB+1,IKE-1
  ZY(:,:,JK) = PRHODJ(:,:,JK)*PVARM(:,:,JK)/PTSTEP          &
    - ZMZM_RHODJ(:,:,JK+1) * PF(:,:,JK+1)/PDZZ(:,:,JK+1)    &
    + ZMZM_RHODJ(:,:,JK  ) * PF(:,:,JK  )/PDZZ(:,:,JK  )    &
    + ZRHODJ_DFDT_O_DZ(:,:,JK+1) * 0.5*PIMPL * PVARM(:,:,JK+1)  &
    + ZRHODJ_DFDT_O_DZ(:,:,JK+1) * 0.5*PIMPL * PVARM(:,:,JK  )  &
    - ZRHODJ_DFDT_O_DZ(:,:,JK  ) * 0.5*PIMPL * PVARM(:,:,JK  )  &
    - ZRHODJ_DFDT_O_DZ(:,:,JK  ) * 0.5*PIMPL * PVARM(:,:,JK-1)
END DO

ZY(:,:,IKE) = PRHODJ(:,:,IKE)*PVARM(:,:,IKE)/PTSTEP         &
    - ZMZM_RHODJ(:,:,IKE+1) * PF(:,:,IKE+1)/PDZZ(:,:,IKE+1) &
    + ZMZM_RHODJ(:,:,IKE  ) * PF(:,:,IKE  )/PDZZ(:,:,IKE  ) &
    - ZRHODJ_DFDT_O_DZ(:,:,IKE ) * 0.5*PIMPL * PVARM(:,:,IKE  ) &
    - ZRHODJ_DFDT_O_DZ(:,:,IKE ) * 0.5*PIMPL * PVARM(:,:,IKE-1)





IF ( PIMPL > 1.E-10 ) THEN




  ZB(:,:,IKB) =   PRHODJ(:,:,IKB)/PTSTEP                   &
                + ZRHODJ_DFDT_O_DZ(:,:,IKB+1) * 0.5*PIMPL
  ZC(:,:,IKB) =   ZRHODJ_DFDT_O_DZ(:,:,IKB+1) * 0.5*PIMPL

  DO JK=IKB+1,IKE-1
    ZA(:,:,JK) = - ZRHODJ_DFDT_O_DZ(:,:,JK  ) * 0.5*PIMPL
    ZB(:,:,JK) =   PRHODJ(:,:,JK)/PTSTEP                   &
                 + ZRHODJ_DFDT_O_DZ(:,:,JK+1) * 0.5*PIMPL &
                 - ZRHODJ_DFDT_O_DZ(:,:,JK  ) * 0.5*PIMPL
    ZC(:,:,JK) =   ZRHODJ_DFDT_O_DZ(:,:,JK+1) * 0.5*PIMPL
  END DO

  ZA(:,:,IKE) = - ZRHODJ_DFDT_O_DZ(:,:,IKE  ) * 0.5*PIMPL
  ZB(:,:,IKE) =   PRHODJ(:,:,IKE)/PTSTEP                   &
                - ZRHODJ_DFDT_O_DZ(:,:,IKE  ) * 0.5*PIMPL




  ZBET(:,:) = ZB(:,:,IKB)  
  PVARP(:,:,IKB) = ZY(:,:,IKB) / ZBET(:,:)

  
  DO JK = IKB+1,IKE-1
    ZGAM(:,:,JK) = ZC(:,:,JK-1) / ZBET(:,:)  
                                                    
    ZBET(:,:)    = ZB(:,:,JK) - ZA(:,:,JK) * ZGAM(:,:,JK)
                                                    
    PVARP(:,:,JK)= ( ZY(:,:,JK) - ZA(:,:,JK) * PVARP(:,:,JK-1) ) / ZBET(:,:)
                                        
  END DO 
  
  ZGAM(:,:,IKE) = ZC(:,:,IKE-1) / ZBET(:,:) 
                                                    
  ZBET(:,:)     = ZB(:,:,IKE) - ZA(:,:,IKE) * ZGAM(:,:,IKE)
                                                    
  PVARP(:,:,IKE)= ( ZY(:,:,IKE) - ZA(:,:,IKE) * PVARP(:,:,IKE-1) ) / ZBET(:,:)
                                       




  DO JK = IKE-1,IKB,-1
    PVARP(:,:,JK) = PVARP(:,:,JK) - ZGAM(:,:,JK+1) * PVARP(:,:,JK+1)
  END DO


ELSE


  PVARP(:,:,IKB:IKE) = ZY(:,:,IKB:IKE) * PTSTEP / PRHODJ(:,:,IKB:IKE)

END IF 






PVARP(:,:,IKE+1)=PVARP(:,:,IKE)



END SUBROUTINE TRIDIAG_MASSFLUX



      SUBROUTINE UPDRAFT_SOPE(KRR,KRRL,KRRI,OMIXUV,                   & 
                         PZZ,PDZZ,PSFTH,PSFRV,PPABSM,PRHODREF,        &
                         PTKEM,PTHM,PRM,PTHLM,PRTM,PUM,PVM,           &
                         PTHL_UP,PRT_UP,PRV_UP,PU_UP,PV_UP,           &
                         PRC_UP,PRI_UP,PTHV_UP,PW_UP,PFRAC_UP,PEMF,   &
                         PDETR,PENTR,KKLCL,KKETL,KKCTL )










IMPLICIT NONE




INTEGER,                INTENT(IN)   :: KRR       
INTEGER,                INTENT(IN)   :: KRRL      
INTEGER,                INTENT(IN)   :: KRRI      
LOGICAL,                INTENT(IN)   :: OMIXUV    

REAL, DIMENSION(:,:,:), INTENT(IN)   :: PZZ       

REAL, DIMENSION(:,:,:), INTENT(IN)   :: PDZZ      
 
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PSFTH,PSFRV
                                            


REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PPABSM      
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PRHODREF    
                                                     
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PTKEM       
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PUM,PVM     



REAL, DIMENSION(:,:,:),   INTENT(IN)   ::  PTHM       
REAL, DIMENSION(:,:,:,:), INTENT(IN)   ::  PRM        
REAL, DIMENSION(:,:,:),   INTENT(IN)   ::  PTHLM,PRTM 
REAL, DIMENSION(:,:,:),   INTENT(OUT)  ::  PTHL_UP,PRT_UP  
REAL, DIMENSION(:,:,:),   INTENT(OUT)  ::  PRV_UP,PRC_UP,PRI_UP,&
                                           PW_UP,PFRAC_UP,PEMF, &
                                           PDETR,PENTR,PTHV_UP, &
                                           PU_UP, PV_UP          
                                           
INTEGER, DIMENSION(:,:),  INTENT(OUT)  ::  KKLCL,KKETL,KKCTL     





INTEGER :: IKB



REAL, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2),SIZE(PTKEM,3)) :: ZPABSM,ZRHODREF
       
REAL, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2),SIZE(PTKEM,3)) :: ZZZ,ZDZZ

REAL, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2),SIZE(PTKEM,3)) :: ZTHLM,ZRTM,&
                                                              ZTHM,ZTKEM,&
                                                              ZUM,ZVM
                                                              
REAL, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2),SIZE(PTKEM,3)) :: ZRVM,ZRCM,ZRIM

REAL, DIMENSION(SIZE(PSFTH,1)*SIZE(PSFTH,2))    ::  ZSFTH,ZSFRV

REAL, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2),SIZE(PTKEM,3))  :: ZTHL_UP,ZRT_UP,&
                                      ZRV_UP,ZRC_UP, ZRI_UP, &
                                      ZW_UP,ZU_UP,ZV_UP,ZTHV_UP,           &
                                      ZFRAC_UP,ZEMF_UP,ZENTR_UP,ZDETR_UP

REAL, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2))    :: ZFRAC_GRID
INTEGER, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2)) :: JKETL,JKCTL,JKLCL

INTEGER :: IIU,IJU,IKU
INTEGER :: J1D        
INTEGER :: JK,JKK,JSV 
INTEGER :: JRR        
REAL    :: ZRVORD     







IIU=SIZE(PTKEM,1)
IJU=SIZE(PTKEM,2)

IKB = 1              
IKU = SIZE(PTKEM,3)

ZRVORD = XRV / XRD


DO JK=IKB,IKU
  ZZZ    (:,JK) = RESHAPE(PZZ    (:,:,JK),(/ IIU*IJU /) )
  ZDZZ   (:,JK) = RESHAPE(PDZZ    (:,:,JK),(/ IIU*IJU /) )  
  ZTHM   (:,JK) = RESHAPE(PTHM   (:,:,JK),(/ IIU*IJU /) )
  ZTKEM  (:,JK) = RESHAPE(PTKEM  (:,:,JK),(/ IIU*IJU /) )
  ZPABSM (:,JK) = RESHAPE(PPABSM (:,:,JK),(/ IIU*IJU /) )
  ZRHODREF(:,JK) = RESHAPE(PRHODREF(:,:,JK),(/ IIU*IJU /) )  
  ZRVM   (:,JK) = RESHAPE(PRM    (:,:,JK,1),(/ IIU*IJU /) ) 
  ZTHLM  (:,JK) = RESHAPE(PTHLM   (:,:,JK),(/ IIU*IJU /) )
  ZRTM   (:,JK) = RESHAPE(PRTM    (:,:,JK),(/ IIU*IJU /) )
  ZUM    (:,JK) = RESHAPE(PUM    (:,:,JK),(/ IIU*IJU /) )
  ZVM    (:,JK) = RESHAPE(PVM    (:,:,JK),(/ IIU*IJU /) )
END DO

IF (KRRL>1) THEN
DO JK=1,IKU
  ZRCM   (:,JK) = RESHAPE(PRM    (:,:,JK,2),(/ IIU*IJU /) ) 
END DO
ELSE
  ZRCM   (:,:) =0.
ENDIF
IF (KRRI>1) THEN
DO JK=1,IKU
  ZRIM   (:,JK) = RESHAPE(PRM    (:,:,JK,4),(/ IIU*IJU /) ) 
END DO
ELSE
  ZRIM   (:,:) =0.
ENDIF

ZSFTH(:)=RESHAPE(PSFTH(:,:),(/ IIU*IJU /) )
ZSFRV(:)=RESHAPE(PSFRV(:,:),(/ IIU*IJU /) )


JK=IKB 


CALL COMPUTE_UPDRAFT(OMIXUV,ZZZ,ZDZZ,JK,      &
                     ZSFTH,ZSFRV,ZPABSM,ZRHODREF,ZUM,ZVM,ZTKEM, &
                     ZTHM,ZRVM,ZRCM,ZRIM,ZTHLM,ZRTM,       &
                     ZTHL_UP,ZRT_UP,ZRV_UP,ZRC_UP,ZRI_UP,&
                     ZTHV_UP,ZW_UP,ZU_UP,ZV_UP, &
                     ZFRAC_UP,ZEMF_UP,&
                     ZDETR_UP,ZENTR_UP,&
                     JKLCL,JKETL,JKCTL)


PTHL_UP(:,:,:)= RESHAPE(ZTHL_UP(:,:), (/ IIU,IJU,IKU /))
PRT_UP(:,:,:)=RESHAPE(ZRT_UP(:,:), (/ IIU,IJU,IKU /) )
PRV_UP(:,:,:)=RESHAPE(ZRV_UP(:,:), (/ IIU,IJU,IKU /) )
PRC_UP(:,:,:)=RESHAPE(ZRC_UP(:,:), (/ IIU,IJU,IKU /) )
PRI_UP(:,:,:)=RESHAPE(ZRI_UP(:,:), (/ IIU,IJU,IKU /) )
PW_UP(:,:,:)=RESHAPE(ZW_UP(:,:), (/ IIU,IJU,IKU /) )
PU_UP(:,:,:)=RESHAPE(ZU_UP(:,:), (/ IIU,IJU,IKU /) )
PV_UP(:,:,:)=RESHAPE(ZV_UP(:,:), (/ IIU,IJU,IKU /) )
PEMF(:,:,:)=RESHAPE(ZEMF_UP(:,:), (/ IIU,IJU,IKU /) )
PDETR(:,:,:)=RESHAPE(ZDETR_UP(:,:), (/ IIU,IJU,IKU /) )
PENTR(:,:,:)=RESHAPE(ZENTR_UP(:,:), (/ IIU,IJU,IKU /) )
PTHV_UP(:,:,:)=RESHAPE(ZTHV_UP(:,:), (/ IIU,IJU,IKU /) )
KKETL(:,:)=RESHAPE(JKETL(:),(/ IIU,IJU/) )
KKCTL(:,:)=RESHAPE(JKCTL(:),(/ IIU,IJU/) )
KKLCL(:,:)=RESHAPE(JKLCL(:),(/ IIU,IJU/) )
PFRAC_UP(:,:,:)=RESHAPE(ZFRAC_UP(:,:),(/ IIU,IJU,IKU /) )

END SUBROUTINE UPDRAFT_SOPE


      SUBROUTINE COMPUTE_MF_CLOUD(KRRL, PTHLM,PRC_UP, PFRAC_UP, PDZZ, KKLCL,&
                                  PRC_MF,PCF_MF                     )


































IMPLICIT NONE





INTEGER,                  INTENT(IN)   ::  KRRL         
                                                        
REAL, DIMENSION(:,:,:),   INTENT(IN)   ::  PTHLM 

REAL, DIMENSION(:,:,:),   INTENT(IN)   ::  PRC_UP 
REAL, DIMENSION(:,:,:),   INTENT(IN)   ::  PFRAC_UP          

REAL, DIMENSION(:,:,:),   INTENT(IN)   ::  PDZZ
INTEGER, DIMENSION(:,:),  INTENT(IN)   ::  KKLCL          
REAL, DIMENSION(:,:,:),   INTENT(OUT)  ::  PRC_MF, PCF_MF 
                                                          




REAL, DIMENSION(SIZE(PTHLM,1),SIZE(PTHLM,2),SIZE(PTHLM,3)) ::  ZFLXZ
INTEGER  :: IKU, IKB, IKE, JI,JJ,JK







IKU=SIZE(PTHLM,3)
IKB=1
IKE=IKU-1

PCF_MF = 0.
PRC_MF = 0.










DO JI=1,SIZE(PCF_MF,1)
DO JJ=1,SIZE(PCF_MF,2)

DO JK=KKLCL(JI,JJ),IKE

PCF_MF(JI,JJ,JK ) = XKCF_MF *0.5* (       &
  &    PFRAC_UP(JI,JJ,JK) +  PFRAC_UP(JI,JJ,JK+1) )
PRC_MF(JI,JJ,JK)  = 0.5* XKCF_MF * ( PFRAC_UP(JI,JJ,JK)*PRC_UP(JI,JJ,JK)  &
                         + PFRAC_UP(JI,JJ,JK+1)*PRC_UP(JI,JJ,JK+1) )


END DO
END DO
END DO


END SUBROUTINE COMPUTE_MF_CLOUD



      SUBROUTINE mfshconvpblinit(massflux_EDKF, entr_EDKF, detr_EDKF & 
                                     ,thl_up, thv_up, rt_up       &
                                     ,rv_up, rc_up, u_up, v_up    &
                                     ,frac_up,RESTART,ALLOWED_TO_READ,     &
     &                      IDS,IDE,JDS,JDE,KDS,KDE,                    &
     &                      IMS,IME,JMS,JME,KMS,KME,                    &
     &                      ITS,ITE,JTS,JTE,KTS,KTE                 )


      IMPLICIT NONE

      LOGICAL,INTENT(IN) :: ALLOWED_TO_READ,RESTART
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,                    &
     &                      IMS,IME,JMS,JME,KMS,KME,                    &
     &                      ITS,ITE,JTS,JTE,KTS,KTE

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT),OPTIONAL ::             &
     &                                        MASSFLUX_EDKF, ENTR_EDKF, DETR_EDKF &
     &                                       ,THL_UP, THV_UP, RT_UP, RV_UP        &
     &                                       ,RC_UP, U_UP, V_UP, FRAC_UP 

      INTEGER :: I,J,K,ITF,JTF,KTF


      JTF=MIN0(JTE,JDE-1)
      KTF=MIN0(KTE,KDE-1)
      ITF=MIN0(ITE,IDE-1)


      IF( PRESENT (MASSFLUX_EDKF) ) THEN
        DO J=JTS,JTF
        DO K=KTS,KTF
        DO I=ITS,ITF
          MASSFLUX_EDKF(I,K,J)=0.
          ENTR_EDKF(I,K,J)=0.
          DETR_EDKF(I,K,J)=0.
          THL_UP(I,K,J)=0.
          THV_UP(I,K,J)=0.
          RT_UP(I,K,J)=0.
          RV_UP(I,K,J)=0.
          RC_UP(I,K,J)=0.
          U_UP(I,K,J)=0.
          V_UP(I,K,J)=0.
          FRAC_UP(I,K,J)=0.
        ENDDO
        ENDDO
        ENDDO
      ENDIF

END SUBROUTINE mfshconvpblinit



      SUBROUTINE BL89(PZZ,PDZZ,PTHVREF,PTHLM,KRR, &
                 PRM,PTKEM,PLM)















INTEGER,                INTENT(IN)   :: KRR

REAL, DIMENSION(:,:,:),   INTENT(IN)  :: PZZ
REAL, DIMENSION(:,:,:),   INTENT(IN)  :: PDZZ
REAL, DIMENSION(:,:,:),   INTENT(IN)  :: PTHVREF
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PTKEM       

REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PTHLM     
REAL, DIMENSION(:,:,:,:), INTENT(IN) ::  PRM       
REAL, DIMENSION(:,:,:),   INTENT(OUT) :: PLM       




INTEGER :: IKB,IKE

REAL, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2),SIZE(PTKEM,3)) :: ZRTM
REAL, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2),SIZE(PTKEM,3)) ::ZVPT  
REAL, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2)) ::  ZLWORK

REAL, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2),SIZE(PTKEM,3)) :: ZZZ,ZDZZ,&
                                                              ZG_O_THVREF, &
                                                              ZTHM,ZTKEM,ZLM,&
                                                              ZLMUP,ZLMDN,&
                                                              ZLMTEST

REAL, DIMENSION(SIZE(PRM,1)*SIZE(PRM,2),SIZE(PRM,3),SIZE(PRM,4)) :: ZRM

REAL, DIMENSION(SIZE(PRM,1)*SIZE(PRM,2),SIZE(PRM,3)) :: ZSUM 

REAL   :: ZPOTE,ZLWORK1,ZLWORK2

INTEGER :: IIU,IJU,IKU,IICE
INTEGER :: J1D        
INTEGER :: JK,JKK     
INTEGER :: JRR        
REAL    :: ZRVORD     
LOGICAL :: GUPORDN    
REAL    :: Z2SQRT2


Z2SQRT2=2.*SQRT(2.)
IIU=SIZE(PTKEM,1)
IJU=SIZE(PTKEM,2)

IKB =  1
IKE = SIZE(PTKEM,3)-1
IKU = SIZE(PTKEM,3)
ZRVORD = XRV / XRD





DO JK=1,IKU
  ZZZ    (:,JK)   = RESHAPE(PZZ    (:,:,JK),(/ IIU*IJU /) )
  ZDZZ   (:,JK)   = RESHAPE(PDZZ   (:,:,JK),(/ IIU*IJU /) )
  ZTHM   (:,JK)   = RESHAPE(PTHLM  (:,:,JK),(/ IIU*IJU /) )
  ZTKEM  (:,JK)   = RESHAPE(PTKEM  (:,:,JK),(/ IIU*IJU /) )
  ZG_O_THVREF(:,JK)   = RESHAPE(XG/PTHVREF(:,:,JK),(/ IIU*IJU /) )
  DO JRR=1,KRR
    ZRM  (:,JK,JRR) = RESHAPE(PRM    (:,:,JK,JRR),(/ IIU*IJU /) )
  END DO
END DO





IF(KRR /= 0) THEN
  ZSUM(:,:) = 0.
  DO JRR=1,KRR
    ZSUM(:,:) = ZSUM(:,:)+ZRM(:,:,JRR)
  ENDDO
  ZVPT(:,1:)=ZTHM(:,:) * ( 1. + ZRVORD*ZRM(:,:,1) )  &
                           / ( 1. + ZSUM(:,:) )
ELSE
  ZVPT(:,1:)=ZTHM(:,:)
END IF






DO JK=IKB,IKE




   GUPORDN=.FALSE.
   CALL COMPUTE_BL89_ML(ZDZZ,ZTKEM,ZG_O_THVREF,ZVPT,JK,GUPORDN,ZLWORK)








   ZLMDN(:,JK)=MIN(ZLWORK(:),0.5*(ZZZ(:,JK)+ZZZ(:,JK+1))-ZZZ(:,IKB))





 
   GUPORDN=.TRUE.
   CALL COMPUTE_BL89_ML(ZDZZ,ZTKEM,ZG_O_THVREF,ZVPT,JK,GUPORDN,ZLWORK)

   ZLMUP(:,JK)=ZLWORK(:)



  DO J1D=1,IIU*IJU
    ZLWORK1=MAX(ZLMDN(J1D,JK),1.E-10)
    ZLWORK2=MAX(ZLMUP(J1D,JK),1.E-10)
    ZPOTE = ZLWORK1 / ZLWORK2
    ZLWORK2=1.d0 + ZPOTE**(2./3.)
    ZLM(J1D,JK) = Z2SQRT2*ZLWORK1/(ZLWORK2*SQRT(ZLWORK2))
  END DO

ZLM(:,JK)=( 0.5* (MAX(ZLMDN(:,JK),1.E-10)**(-2./3.)+MAX(ZLMUP(:,JK),1.E-10)**(-2./3.)) )**(-1.5)
ZLM(:,JK)=MAX(ZLM(:,JK),XLINI)




END DO






ZLM(:,IKE)  =ZLM(:,IKE-1)
ZLM(:,IKE+1)=ZLM(:,IKE-1)
ZLMUP(:,IKE)  =ZLMUP(:,IKE-1)
ZLMUP(:,IKE+1)=ZLMUP(:,IKE-1)
ZLMDN(:,IKE)  =ZLMDN(:,IKE-1)
ZLMDN(:,IKE+1)=ZLMDN(:,IKE-1)

  DO JK=1,IKU
    PLM  (:,:,JK)   = RESHAPE(ZLM  (:,JK), (/ IIU,IJU /) )
  END DO

END SUBROUTINE BL89
                        
END  MODULE MODULE_BL_MFSHCONVPBL

