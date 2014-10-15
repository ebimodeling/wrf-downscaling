


























    















































MODULE MODULE_MP_MORR_TWO_MOMENT
   USE     module_wrf_error
   USE module_utility, ONLY: WRFU_Clock, WRFU_Alarm  
   USE module_domain, ONLY : HISTORY_ALARM, Is_alarm_tstep  
   USE module_mp_radar


  use module_model_constants, ONLY: CP, G, R => r_d, RV => r_v, EP_2



   IMPLICIT NONE

   REAL, PARAMETER :: PI = 3.1415926535897932384626434
   REAL, PARAMETER :: SQRTPI = 0.9189385332046727417803297

   PUBLIC  ::  MP_MORR_TWO_MOMENT
   PUBLIC  ::  POLYSVP

   PRIVATE :: GAMMA, DERF1
   PRIVATE :: PI, SQRTPI
   PRIVATE :: MORR_TWO_MOMENT_MICRO







     INTEGER, PRIVATE ::  IACT






     INTEGER, PRIVATE ::  INUM


     REAL, PRIVATE ::      NDCNST





     INTEGER, PRIVATE ::  ILIQ





     INTEGER, PRIVATE ::  INUC















     INTEGER, PRIVATE ::  IBASE





     INTEGER, PRIVATE ::  ISUB      





     INTEGER, PRIVATE ::  IGRAUP






     INTEGER, PRIVATE ::  IHAIL



     REAL, PRIVATE ::      AI,AC,AS,AR,AG 
     REAL, PRIVATE ::      BI,BC,BS,BR,BG 



     REAL, PRIVATE ::      RHOSU       
     REAL, PRIVATE ::      RHOW        
     REAL, PRIVATE ::      RHOI        
     REAL, PRIVATE ::      RHOSN       
     REAL, PRIVATE ::      RHOG        
     REAL, PRIVATE ::      AIMM        
     REAL, PRIVATE ::      BIMM        
     REAL, PRIVATE ::      ECR         
     REAL, PRIVATE ::      DCS         
     REAL, PRIVATE ::      MI0         
     REAL, PRIVATE ::      MG0         
     REAL, PRIVATE ::      F1S         
     REAL, PRIVATE ::      F2S         
     REAL, PRIVATE ::      F1R         
     REAL, PRIVATE ::      F2R         

     REAL, PRIVATE ::      QSMALL      
     REAL, PRIVATE ::      CI,DI,CS,DS,CG,DG 
     REAL, PRIVATE ::      EII         
     REAL, PRIVATE ::      ECI         
     REAL, PRIVATE ::      RIN     

     REAL, PRIVATE ::      CPW     



     REAL, PRIVATE ::      C1     
     REAL, PRIVATE ::      K1     



     REAL, PRIVATE ::      MW      
     REAL, PRIVATE ::      OSM     
     REAL, PRIVATE ::      VI      
     REAL, PRIVATE ::      EPSM    
     REAL, PRIVATE ::      RHOA    
     REAL, PRIVATE ::      MAP     
     REAL, PRIVATE ::      MA      
     REAL, PRIVATE ::      RR      
     REAL, PRIVATE ::      BACT    
     REAL, PRIVATE ::      RM1     
     REAL, PRIVATE ::      RM2     
     REAL, PRIVATE ::      NANEW1  
     REAL, PRIVATE ::      NANEW2  
     REAL, PRIVATE ::      SIG1    
     REAL, PRIVATE ::      SIG2    
     REAL, PRIVATE ::      F11     
     REAL, PRIVATE ::      F12     
     REAL, PRIVATE ::      F21     
     REAL, PRIVATE ::      F22     
     REAL, PRIVATE ::      MMULT   
     REAL, PRIVATE ::      LAMMAXI,LAMMINI,LAMMAXR,LAMMINR,LAMMAXS,LAMMINS,LAMMAXG,LAMMING



     REAL, PRIVATE :: CONS1,CONS2,CONS3,CONS4,CONS5,CONS6,CONS7,CONS8,CONS9,CONS10
     REAL, PRIVATE :: CONS11,CONS12,CONS13,CONS14,CONS15,CONS16,CONS17,CONS18,CONS19,CONS20
     REAL, PRIVATE :: CONS21,CONS22,CONS23,CONS24,CONS25,CONS26,CONS27,CONS28,CONS29,CONS30
     REAL, PRIVATE :: CONS31,CONS32,CONS33,CONS34,CONS35,CONS36,CONS37,CONS38,CONS39,CONS40
     REAL, PRIVATE :: CONS41


CONTAINS


SUBROUTINE MORR_TWO_MOMENT_INIT






      IMPLICIT NONE

      integer n,i












      INUM = 1




      NDCNST = 250.











      IACT = 2














 
      IBASE = 2








      ISUB = 0      







      ILIQ = 0





      INUC = 0





      IGRAUP = 0







      IHAIL = 0






         AI = 700.
         AC = 3.E7
         AS = 11.72
         AR = 841.99667
         BI = 1.
         BC = 2.
         BS = 0.41
         BR = 0.8
         IF (IHAIL.EQ.0) THEN
	 AG = 19.3
	 BG = 0.37
         ELSE 
         AG = 114.5 
         BG = 0.5
         END IF





         RHOSU = 85000./(287.15*273.15)
         RHOW = 997.
         RHOI = 500.
         RHOSN = 100.
         IF (IHAIL.EQ.0) THEN
	 RHOG = 400.
         ELSE
         RHOG = 900.
         END IF
         AIMM = 0.66
         BIMM = 100.
         ECR = 1.
         DCS = 125.E-6
         MI0 = 4./3.*PI*RHOI*(10.E-6)**3
	 MG0 = 1.6E-10
         F1S = 0.86
         F2S = 0.28
         F1R = 0.78


         F2R = 0.308

         QSMALL = 1.E-14
         EII = 0.1
         ECI = 0.7



         CPW = 4187.



         CI = RHOI*PI/6.
         DI = 3.
         CS = RHOSN*PI/6.
         DS = 3.
         CG = RHOG*PI/6.
         DG = 3.


         RIN = 0.1E-6

         MMULT = 4./3.*PI*RHOI*(5.E-6)**3



         LAMMAXI = 1./1.E-6
         LAMMINI = 1./(2.*DCS+100.E-6)
         LAMMAXR = 1./20.E-6

         LAMMINR = 1./2800.E-6

         LAMMAXS = 1./10.E-6
         LAMMINS = 1./2000.E-6
         LAMMAXG = 1./20.E-6
         LAMMING = 1./2000.E-6







              K1 = 0.4
              C1 = 120. 









         MW = 0.018
         OSM = 1.
         VI = 3.
         EPSM = 0.7
         RHOA = 1777.
         MAP = 0.132
         MA = 0.0284
         RR = 8.3187
         BACT = VI*OSM*EPSM*MW*RHOA/(MAP*RHOW)





         RM1 = 0.052E-6
         SIG1 = 2.04
         NANEW1 = 72.2E6
         F11 = 0.5*EXP(2.5*(LOG(SIG1))**2)
         F21 = 1.+0.25*LOG(SIG1)



         RM2 = 1.3E-6
         SIG2 = 2.5
         NANEW2 = 1.8E6
         F12 = 0.5*EXP(2.5*(LOG(SIG2))**2)
         F22 = 1.+0.25*LOG(SIG2)



         CONS1=GAMMA(1.+DS)*CS
         CONS2=GAMMA(1.+DG)*CG
         CONS3=GAMMA(4.+BS)/6.
         CONS4=GAMMA(4.+BR)/6.
         CONS5=GAMMA(1.+BS)
         CONS6=GAMMA(1.+BR)
         CONS7=GAMMA(4.+BG)/6.
         CONS8=GAMMA(1.+BG)
         CONS9=GAMMA(5./2.+BR/2.)
         CONS10=GAMMA(5./2.+BS/2.)
         CONS11=GAMMA(5./2.+BG/2.)
         CONS12=GAMMA(1.+DI)*CI
         CONS13=GAMMA(BS+3.)*PI/4.*ECI
         CONS14=GAMMA(BG+3.)*PI/4.*ECI
         CONS15=-1108.*EII*PI**((1.-BS)/3.)*RHOSN**((-2.-BS)/3.)/(4.*720.)
         CONS16=GAMMA(BI+3.)*PI/4.*ECI
         CONS17=4.*2.*3.*RHOSU*PI*ECI*ECI*GAMMA(2.*BS+2.)/(8.*(RHOG-RHOSN))
         CONS18=RHOSN*RHOSN
         CONS19=RHOW*RHOW
         CONS20=20.*PI*PI*RHOW*BIMM
         CONS21=4./(DCS*RHOI)
         CONS22=PI*RHOI*DCS**3/6.
         CONS23=PI/4.*EII*GAMMA(BS+3.)
         CONS24=PI/4.*ECR*GAMMA(BR+3.)
         CONS25=PI*PI/24.*RHOW*ECR*GAMMA(BR+6.)
         CONS26=PI/6.*RHOW
         CONS27=GAMMA(1.+BI)
         CONS28=GAMMA(4.+BI)/6.
         CONS29=4./3.*PI*RHOW*(25.E-6)**3
         CONS30=4./3.*PI*RHOW
         CONS31=PI*PI*ECR*RHOSN
         CONS32=PI/2.*ECR
         CONS33=PI*PI*ECR*RHOG
         CONS34=5./2.+BR/2.
         CONS35=5./2.+BS/2.
         CONS36=5./2.+BG/2.
         CONS37=4.*PI*1.38E-23/(6.*PI*RIN)
         CONS38=PI*PI/3.*RHOW
         CONS39=PI*PI/36.*RHOW*BIMM
         CONS40=PI/6.*BIMM
         CONS41=PI*PI*ECR*RHOW






         xam_r = PI*RHOW/6.
         xbm_r = 3.
         xmu_r = 0.
         xam_s = CS
         xbm_s = DS
         xmu_s = 0.
         xam_g = CG
         xbm_g = DG
         xmu_g = 0.

         call radar_init



END SUBROUTINE MORR_TWO_MOMENT_INIT
















SUBROUTINE MP_MORR_TWO_MOMENT(ITIMESTEP,                       &
                TH, QV, QC, QR, QI, QS, QG, NI, NS, NR, NG, &
                RHO, PII, P, DT_IN, DZ, HT, W,          &
                RAINNC, RAINNCV, SR,                    &
		SNOWNC,SNOWNCV,GRAUPELNC,GRAUPELNCV,    & 
                refl_10cm, diagflag, do_radar_ref,      & 
                qrcuten, qscuten, qicuten, mu           & 
               ,F_QNDROP, qndrop                        & 
               ,IDS,IDE, JDS,JDE, KDS,KDE               & 
               ,IMS,IME, JMS,JME, KMS,KME               & 
               ,ITS,ITE, JTS,JTE, KTS,KTE               & 

		   ,QLSINK,PRECR,PRECI,PRECS,PRECG &        
                                            )
 





























































   IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::   ids, ide, jds, jde, kds, kde , &
                                       ims, ime, jms, jme, kms, kme , &
                                       its, ite, jts, jte, kts, kte


   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT):: &
                          qv, qc, qr, qi, qs, qg, ni, ns, nr, TH, NG   

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), optional,INTENT(INOUT):: qndrop

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), optional,INTENT(INOUT):: QLSINK, &
                          PRECI,PRECS,PRECG,PRECR 


   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN):: &
                          pii, p, dz, rho, w 
   REAL, INTENT(IN):: dt_in
   INTEGER, INTENT(IN):: ITIMESTEP

   REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT):: &
                          RAINNC, RAINNCV, SR, &

                          SNOWNC,SNOWNCV,GRAUPELNC,GRAUPELNCV

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT)::       &  
                          refl_10cm

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN) ::       ht

   

   REAL, DIMENSION(its:ite, kts:kte, jts:jte)::                     &
                      effi, effs, effr, EFFG

   REAL, DIMENSION(its:ite, kts:kte, jts:jte)::                     &
                      T, WVAR, EFFC

   REAL, DIMENSION(kts:kte) ::                                                                & 
                            QC_TEND1D, QI_TEND1D, QNI_TEND1D, QR_TEND1D,                      &
                            NI_TEND1D, NS_TEND1D, NR_TEND1D,                                  &
                            QC1D, QI1D, QR1D,NI1D, NS1D, NR1D, QS1D,                          &
                            T_TEND1D,QV_TEND1D, T1D, QV1D, P1D, W1D, WVAR1D,         &
                            EFFC1D, EFFI1D, EFFS1D, EFFR1D,DZ1D,   &
   
                            QG_TEND1D, NG_TEND1D, QG1D, NG1D, EFFG1D, &


                            QGSTEN,QRSTEN, QISTEN, QNISTEN, QCSTEN, &

                            QRCU1D, QSCU1D, QICU1D



   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN):: &
      qrcuten, qscuten, qicuten
   REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN):: &
      mu

  LOGICAL, INTENT(IN), OPTIONAL ::                F_QNDROP  
  LOGICAL :: flag_qndrop  
  integer :: iinum 


   REAL, DIMENSION(kts:kte) :: nc1d, nc_tend1d,C2PREC,CSED,ISED,SSED,GSED,RSED    

   REAL, DIMENSION(kts:kte) :: dBZ
                          
   REAL PRECPRT1D, SNOWRT1D, SNOWPRT1D, GRPLPRT1D 

   INTEGER I,K,J

   REAL DT

   LOGICAL, OPTIONAL, INTENT(IN) :: diagflag
   INTEGER, OPTIONAL, INTENT(IN) :: do_radar_ref


   flag_qndrop = .false.
   IF ( PRESENT ( f_qndrop ) ) flag_qndrop = f_qndrop


   
   
   DT = DT_IN   

   DO I=ITS,ITE
   DO J=JTS,JTE
   DO K=KTS,KTE
       T(I,K,J)        = TH(i,k,j)*PII(i,k,j)




       WVAR(I,K,J)     = 0.5



   END DO
   END DO
   END DO

   do i=its,ite      
   do j=jts,jte      
   
   
   



      do k=kts,kte   

          QC_TEND1D(k)  = 0.
          QI_TEND1D(k)  = 0.
          QNI_TEND1D(k) = 0.
          QR_TEND1D(k)  = 0.
          NI_TEND1D(k)  = 0.
          NS_TEND1D(k)  = 0.
          NR_TEND1D(k)  = 0.
          T_TEND1D(k)   = 0.
          QV_TEND1D(k)  = 0.
          nc_tend1d(k) = 0. 

          QC1D(k)       = QC(i,k,j)
          QI1D(k)       = QI(i,k,j)
          QS1D(k)       = QS(i,k,j)
          QR1D(k)       = QR(i,k,j)

          NI1D(k)       = NI(i,k,j)

          NS1D(k)       = NS(i,k,j)
          NR1D(k)       = NR(i,k,j)

          QG1D(K)       = QG(I,K,j)
          NG1D(K)       = NG(I,K,j)
          QG_TEND1D(K)  = 0.
          NG_TEND1D(K)  = 0.

          T1D(k)        = T(i,k,j)
          QV1D(k)       = QV(i,k,j)
          P1D(k)        = P(i,k,j)
          DZ1D(k)       = DZ(i,k,j)
          W1D(k)        = W(i,k,j)
          WVAR1D(k)     = WVAR(i,k,j)

          qrcu1d(k)     = qrcuten(i,k,j)/mu(i,j)
          qscu1d(k)     = qscuten(i,k,j)/mu(i,j)
          qicu1d(k)     = qicuten(i,k,j)/mu(i,j)
      end do  

   IF (flag_qndrop .AND. PRESENT( qndrop )) THEN
      iact = 3
      DO k = kts, kte
         nc1d(k)=qndrop(i,k,j)
         iinum=0
      ENDDO
   ELSE
      DO k = kts, kte
         nc1d(k)=0. 
         iinum=1
      ENDDO
   ENDIF



      call MORR_TWO_MOMENT_MICRO(QC_TEND1D, QI_TEND1D, QNI_TEND1D, QR_TEND1D,            &
       NI_TEND1D, NS_TEND1D, NR_TEND1D,                                                  &
       QC1D, QI1D, QS1D, QR1D,NI1D, NS1D, NR1D,                                          &
       T_TEND1D,QV_TEND1D, T1D, QV1D, P1D, DZ1D, W1D, WVAR1D,                   &
       PRECPRT1D,SNOWRT1D,                                                               &
       SNOWPRT1D,GRPLPRT1D,                 & 
       EFFC1D,EFFI1D,EFFS1D,EFFR1D,DT,                                                   &
                                            IMS,IME, JMS,JME, KMS,KME,                   &
                                            ITS,ITE, JTS,JTE, KTS,KTE,                   & 
                                    QG_TEND1D,NG_TEND1D,QG1D,NG1D,EFFG1D, &
                                    qrcu1d, qscu1d, qicu1d, &

                                  QGSTEN,QRSTEN,QISTEN,QNISTEN,QCSTEN, &
                                  nc1d, nc_tend1d, iinum, C2PREC,CSED,ISED,SSED,GSED,RSED & 
                       )

   
   
   
      do k=kts,kte





          QC(i,k,j)        = QC1D(k)
          QI(i,k,j)        = QI1D(k)
          QS(i,k,j)        = QS1D(k)
          QR(i,k,j)        = QR1D(k)
          NI(i,k,j)        = NI1D(k)
          NS(i,k,j)        = NS1D(k)          
          NR(i,k,j)        = NR1D(k)
	  QG(I,K,j)        = QG1D(K)
          NG(I,K,j)        = NG1D(K)

          T(i,k,j)         = T1D(k)
          TH(I,K,J)        = T(i,k,j)/PII(i,k,j) 
          QV(i,k,j)        = QV1D(k)

          EFFC(i,k,j)      = EFFC1D(k)
          EFFI(i,k,j)      = EFFI1D(k)
          EFFS(i,k,j)      = EFFS1D(k)
          EFFR(i,k,j)      = EFFR1D(k)
	  EFFG(I,K,j)      = EFFG1D(K)


          IF (flag_qndrop .AND. PRESENT( qndrop )) THEN
             qndrop(i,k,j) = nc1d(k)

          END IF
          IF ( PRESENT( QLSINK ) ) THEN
             if(qc(i,k,j)>1.e-10) then
                QLSINK(I,K,J)  = C2PREC(K)/QC(I,K,J)
             else
                QLSINK(I,K,J)  = 0.0
             endif
          END IF
          IF ( PRESENT( PRECR ) ) PRECR(I,K,J) = RSED(K)
          IF ( PRESENT( PRECI ) ) PRECI(I,K,J) = ISED(K)
          IF ( PRESENT( PRECS ) ) PRECS(I,K,J) = SSED(K)
          IF ( PRESENT( PRECG ) ) PRECG(I,K,J) = GSED(K)







      end do


      RAINNC(i,j) = RAINNC(I,J)+PRECPRT1D
      RAINNCV(i,j) = PRECPRT1D

      SNOWNC(i,j) = SNOWNC(I,J)+SNOWPRT1D
      SNOWNCV(i,j) = SNOWPRT1D
      GRAUPELNC(i,j) = GRAUPELNC(I,J)+GRPLPRT1D
      GRAUPELNCV(i,j) = GRPLPRT1D
      SR(i,j) = SNOWRT1D/(PRECPRT1D+1.E-12)


         IF ( PRESENT (diagflag) ) THEN
         if (diagflag .and. do_radar_ref == 1) then
          call refl10cm_hm (qv1d, qr1d, nr1d, qs1d, ns1d, qg1d, ng1d,   &
                      t1d, p1d, dBZ, kts, kte, i, j)
          do k = kts, kte
             refl_10cm(i,k,j) = MAX(-35., dBZ(k))
          enddo
         endif
         ENDIF


   end do
   end do   

END SUBROUTINE MP_MORR_TWO_MOMENT



      SUBROUTINE MORR_TWO_MOMENT_MICRO(QC3DTEN,QI3DTEN,QNI3DTEN,QR3DTEN,         &
       NI3DTEN,NS3DTEN,NR3DTEN,QC3D,QI3D,QNI3D,QR3D,NI3D,NS3D,NR3D,              &
       T3DTEN,QV3DTEN,T3D,QV3D,PRES,DZQ,W3D,WVAR,PRECRT,SNOWRT,            &
       SNOWPRT,GRPLPRT,                & 
       EFFC,EFFI,EFFS,EFFR,DT,                                                   &
                                            IMS,IME, JMS,JME, KMS,KME,           &
                                            ITS,ITE, JTS,JTE, KTS,KTE,           & 
                        QG3DTEN,NG3DTEN,QG3D,NG3D,EFFG,qrcu1d,qscu1d, qicu1d,    &
                        QGSTEN,QRSTEN,QISTEN,QNISTEN,QCSTEN, &
                        nc3d,nc3dten,iinum, & 
				c2prec,CSED,ISED,SSED,GSED,RSED  &  
                        )




















      IMPLICIT NONE








      INTEGER, INTENT( IN)  :: IMS,IME, JMS,JME, KMS,KME,          &
                               ITS,ITE, JTS,JTE, KTS,KTE

      REAL, DIMENSION(KTS:KTE) ::  QC3DTEN            
      REAL, DIMENSION(KTS:KTE) ::  QI3DTEN            
      REAL, DIMENSION(KTS:KTE) ::  QNI3DTEN           
      REAL, DIMENSION(KTS:KTE) ::  QR3DTEN            
      REAL, DIMENSION(KTS:KTE) ::  NI3DTEN            
      REAL, DIMENSION(KTS:KTE) ::  NS3DTEN            
      REAL, DIMENSION(KTS:KTE) ::  NR3DTEN            
      REAL, DIMENSION(KTS:KTE) ::  QC3D               
      REAL, DIMENSION(KTS:KTE) ::  QI3D               
      REAL, DIMENSION(KTS:KTE) ::  QNI3D              
      REAL, DIMENSION(KTS:KTE) ::  QR3D               
      REAL, DIMENSION(KTS:KTE) ::  NI3D               
      REAL, DIMENSION(KTS:KTE) ::  NS3D               
      REAL, DIMENSION(KTS:KTE) ::  NR3D               
      REAL, DIMENSION(KTS:KTE) ::  T3DTEN             
      REAL, DIMENSION(KTS:KTE) ::  QV3DTEN            
      REAL, DIMENSION(KTS:KTE) ::  T3D                
      REAL, DIMENSION(KTS:KTE) ::  QV3D               
      REAL, DIMENSION(KTS:KTE) ::  PRES               
      REAL, DIMENSION(KTS:KTE) ::  DZQ                
      REAL, DIMENSION(KTS:KTE) ::  W3D                
      REAL, DIMENSION(KTS:KTE) ::  WVAR               

      REAL, DIMENSION(KTS:KTE) ::  nc3d
      REAL, DIMENSION(KTS:KTE) ::  nc3dten
      integer, intent(in) :: iinum


      REAL, DIMENSION(KTS:KTE) ::  QG3DTEN            
      REAL, DIMENSION(KTS:KTE) ::  NG3DTEN            
      REAL, DIMENSION(KTS:KTE) ::  QG3D            
      REAL, DIMENSION(KTS:KTE) ::  NG3D            



      REAL, DIMENSION(KTS:KTE) ::  QGSTEN            
      REAL, DIMENSION(KTS:KTE) ::  QRSTEN            
      REAL, DIMENSION(KTS:KTE) ::  QISTEN            
      REAL, DIMENSION(KTS:KTE) ::  QNISTEN           
      REAL, DIMENSION(KTS:KTE) ::  QCSTEN            


        REAL, DIMENSION(KTS:KTE) ::   qrcu1d
        REAL, DIMENSION(KTS:KTE) ::   qscu1d
        REAL, DIMENSION(KTS:KTE) ::   qicu1d



        REAL PRECRT                
        REAL SNOWRT                

        REAL SNOWPRT      
	REAL GRPLPRT	  

        REAL, DIMENSION(KTS:KTE) ::   EFFC            
        REAL, DIMENSION(KTS:KTE) ::   EFFI            
        REAL, DIMENSION(KTS:KTE) ::   EFFS            
        REAL, DIMENSION(KTS:KTE) ::   EFFR            
        REAL, DIMENSION(KTS:KTE) ::   EFFG            



        REAL DT         







     REAL, DIMENSION(KTS:KTE) :: LAMC          
     REAL, DIMENSION(KTS:KTE) :: LAMI          
     REAL, DIMENSION(KTS:KTE) :: LAMS          
     REAL, DIMENSION(KTS:KTE) :: LAMR          
     REAL, DIMENSION(KTS:KTE) :: LAMG          
     REAL, DIMENSION(KTS:KTE) :: CDIST1        
     REAL, DIMENSION(KTS:KTE) :: N0I           
     REAL, DIMENSION(KTS:KTE) :: N0S           
     REAL, DIMENSION(KTS:KTE) :: N0RR          
     REAL, DIMENSION(KTS:KTE) :: N0G           
     REAL, DIMENSION(KTS:KTE) :: PGAM          



     REAL, DIMENSION(KTS:KTE) ::  NSUBC     
     REAL, DIMENSION(KTS:KTE) ::  NSUBI     
     REAL, DIMENSION(KTS:KTE) ::  NSUBS     
     REAL, DIMENSION(KTS:KTE) ::  NSUBR     
     REAL, DIMENSION(KTS:KTE) ::  PRD       
     REAL, DIMENSION(KTS:KTE) ::  PRE       
     REAL, DIMENSION(KTS:KTE) ::  PRDS      
     REAL, DIMENSION(KTS:KTE) ::  NNUCCC    
     REAL, DIMENSION(KTS:KTE) ::  MNUCCC    
     REAL, DIMENSION(KTS:KTE) ::  PRA       
     REAL, DIMENSION(KTS:KTE) ::  PRC       
     REAL, DIMENSION(KTS:KTE) ::  PCC       
     REAL, DIMENSION(KTS:KTE) ::  NNUCCD    
     REAL, DIMENSION(KTS:KTE) ::  MNUCCD    
     REAL, DIMENSION(KTS:KTE) ::  MNUCCR    
     REAL, DIMENSION(KTS:KTE) ::  NNUCCR    
     REAL, DIMENSION(KTS:KTE) ::  NPRA      
     REAL, DIMENSION(KTS:KTE) ::  NRAGG     
     REAL, DIMENSION(KTS:KTE) ::  NSAGG     
     REAL, DIMENSION(KTS:KTE) ::  NPRC      
     REAL, DIMENSION(KTS:KTE) ::  NPRC1      
     REAL, DIMENSION(KTS:KTE) ::  PRAI      
     REAL, DIMENSION(KTS:KTE) ::  PRCI      
     REAL, DIMENSION(KTS:KTE) ::  PSACWS    
     REAL, DIMENSION(KTS:KTE) ::  NPSACWS   
     REAL, DIMENSION(KTS:KTE) ::  PSACWI    
     REAL, DIMENSION(KTS:KTE) ::  NPSACWI   
     REAL, DIMENSION(KTS:KTE) ::  NPRCI     
     REAL, DIMENSION(KTS:KTE) ::  NPRAI     
     REAL, DIMENSION(KTS:KTE) ::  NMULTS    
     REAL, DIMENSION(KTS:KTE) ::  NMULTR    
     REAL, DIMENSION(KTS:KTE) ::  QMULTS    
     REAL, DIMENSION(KTS:KTE) ::  QMULTR    
     REAL, DIMENSION(KTS:KTE) ::  PRACS     
     REAL, DIMENSION(KTS:KTE) ::  NPRACS    
     REAL, DIMENSION(KTS:KTE) ::  PCCN      
     REAL, DIMENSION(KTS:KTE) ::  PSMLT     
     REAL, DIMENSION(KTS:KTE) ::  EVPMS     
     REAL, DIMENSION(KTS:KTE) ::  NSMLTS    
     REAL, DIMENSION(KTS:KTE) ::  NSMLTR    

     REAL, DIMENSION(KTS:KTE) ::  PIACR     
     REAL, DIMENSION(KTS:KTE) ::  NIACR     
     REAL, DIMENSION(KTS:KTE) ::  PRACI     
     REAL, DIMENSION(KTS:KTE) ::  PIACRS     
     REAL, DIMENSION(KTS:KTE) ::  NIACRS     
     REAL, DIMENSION(KTS:KTE) ::  PRACIS     
     REAL, DIMENSION(KTS:KTE) ::  EPRD      
     REAL, DIMENSION(KTS:KTE) ::  EPRDS     

     REAL, DIMENSION(KTS:KTE) ::  PRACG    
     REAL, DIMENSION(KTS:KTE) ::  PSACWG    
     REAL, DIMENSION(KTS:KTE) ::  PGSACW    
     REAL, DIMENSION(KTS:KTE) ::  PGRACS    
     REAL, DIMENSION(KTS:KTE) ::  PRDG    
     REAL, DIMENSION(KTS:KTE) ::  EPRDG    
     REAL, DIMENSION(KTS:KTE) ::  EVPMG    
     REAL, DIMENSION(KTS:KTE) ::  PGMLT    
     REAL, DIMENSION(KTS:KTE) ::  NPRACG    
     REAL, DIMENSION(KTS:KTE) ::  NPSACWG    
     REAL, DIMENSION(KTS:KTE) ::  NSCNG    
     REAL, DIMENSION(KTS:KTE) ::  NGRACS    
     REAL, DIMENSION(KTS:KTE) ::  NGMLTG    
     REAL, DIMENSION(KTS:KTE) ::  NGMLTR    
     REAL, DIMENSION(KTS:KTE) ::  NSUBG    
     REAL, DIMENSION(KTS:KTE) ::  PSACR    
     REAL, DIMENSION(KTS:KTE) ::  NMULTG    
     REAL, DIMENSION(KTS:KTE) ::  NMULTRG    
     REAL, DIMENSION(KTS:KTE) ::  QMULTG    
     REAL, DIMENSION(KTS:KTE) ::  QMULTRG    



     REAL, DIMENSION(KTS:KTE) ::   KAP   
     REAL, DIMENSION(KTS:KTE) ::   EVS   
     REAL, DIMENSION(KTS:KTE) ::   EIS   
     REAL, DIMENSION(KTS:KTE) ::   QVS   
     REAL, DIMENSION(KTS:KTE) ::   QVI   
     REAL, DIMENSION(KTS:KTE) ::   QVQVS 
     REAL, DIMENSION(KTS:KTE) ::   QVQVSI
     REAL, DIMENSION(KTS:KTE) ::   DV    
     REAL, DIMENSION(KTS:KTE) ::   XXLS  
     REAL, DIMENSION(KTS:KTE) ::   XXLV  
     REAL, DIMENSION(KTS:KTE) ::   CPM   
     REAL, DIMENSION(KTS:KTE) ::   MU    
     REAL, DIMENSION(KTS:KTE) ::   SC    
     REAL, DIMENSION(KTS:KTE) ::   XLF   
     REAL, DIMENSION(KTS:KTE) ::   RHO   
     REAL, DIMENSION(KTS:KTE) ::   AB    
     REAL, DIMENSION(KTS:KTE) ::   ABI    



     REAL, DIMENSION(KTS:KTE) ::   DAP    
     REAL    NACNT                    
     REAL    FMULT                    
     REAL    COFFI                    



      REAL, DIMENSION(KTS:KTE) ::    DUMI,DUMR,DUMFNI,DUMG,DUMFNG
      REAL UNI, UMI,UMR
      REAL, DIMENSION(KTS:KTE) ::    FR, FI, FNI,FG,FNG
      REAL RGVM
      REAL, DIMENSION(KTS:KTE) ::   FALOUTR,FALOUTI,FALOUTNI
      REAL FALTNDR,FALTNDI,FALTNDNI,RHO2
      REAL, DIMENSION(KTS:KTE) ::   DUMQS,DUMFNS
      REAL UMS,UNS
      REAL, DIMENSION(KTS:KTE) ::   FS,FNS, FALOUTS,FALOUTNS,FALOUTG,FALOUTNG
      REAL FALTNDS,FALTNDNS,UNR,FALTNDG,FALTNDNG
      REAL, DIMENSION(KTS:KTE) ::    DUMC,DUMFNC
      REAL UNC,UMC,UNG,UMG
      REAL, DIMENSION(KTS:KTE) ::   FC,FALOUTC,FALOUTNC
      REAL FALTNDC,FALTNDNC
      REAL, DIMENSION(KTS:KTE) ::   FNC,DUMFNR,FALOUTNR
      REAL FALTNDNR
      REAL, DIMENSION(KTS:KTE) ::   FNR



      REAL, DIMENSION(KTS:KTE) ::    AIN,ARN,ASN,ACN,AGN









     REAL DUM,DUM1,DUM2,DUMT,DUMQV,DUMQSS,DUMQSI,DUMS



     REAL DQSDT    
     REAL DQSIDT   
     REAL EPSI     
     REAL EPSS     
     REAL EPSR     
     REAL EPSG     


     REAL TAUC     
     REAL TAUR     
     REAL TAUI     
     REAL TAUS     
     REAL TAUG     
     REAL DUMACT,DUM3



     INTEGER K,NSTEP,N 





      INTEGER LTRUE




     REAL    CT      
     REAL    TEMP1   
     REAL    SAT1    
     REAL    SIGVL   
     REAL    KEL     
     REAL    KC2     

       REAL CRY,KRY   



     REAL DUMQI,DUMNI,DC0,DS0,DG0
     REAL DUMQC,DUMQR,RATIO,SUM_DEP,FUDGEF


     REAL WEF



      REAL ANUC,BNUC



        REAL AACT,GAMM,GG,PSI,ETA1,ETA2,SM1,SM2,SMAX,UU1,UU2,ALPHA



        REAL DLAMS,DLAMR,DLAMI,DLAMC,DLAMG,LAMMAX,LAMMIN

        INTEGER IDROP


	REAL, DIMENSION(KTS:KTE)::C2PREC,CSED,ISED,SSED,GSED,RSED 









         LTRUE = 0


         DO K = KTS,KTE


               NC3DTEN(K) = 0.


		C2PREC(K)=0.
		CSED(K)=0.
		ISED(K)=0.
		SSED(K)=0.
		GSED(K)=0.
		RSED(K)=0.



            XXLV(K) = 3.1484E6-2370.*T3D(K)



            XXLS(K) = 3.15E6-2370.*T3D(K)+0.3337E6

            CPM(K) = CP*(1.+0.887*QV3D(K))




            EVS(K) = min(0.99*pres(k),POLYSVP(T3D(K),0))   
            EIS(K) = min(0.99*pres(k),POLYSVP(T3D(K),1))   



            IF (EIS(K).GT.EVS(K)) EIS(K) = EVS(K)

            QVS(K) = EP_2*EVS(K)/(PRES(K)-EVS(K))
            QVI(K) = EP_2*EIS(K)/(PRES(K)-EIS(K))

            QVQVS(K) = QV3D(K)/QVS(K)
            QVQVSI(K) = QV3D(K)/QVI(K)



            RHO(K) = PRES(K)/(R*T3D(K))






            IF (QRCU1D(K).GE.1.E-10) THEN
            DUM=1.8e5*(QRCU1D(K)*DT/(PI*RHOW*RHO(K)**3))**0.25
            NR3D(K)=NR3D(K)+DUM
            END IF
            IF (QSCU1D(K).GE.1.E-10) THEN
            DUM=3.e5*(QSCU1D(K)*DT/(CONS1*RHO(K)**3))**(1./(DS+1.))
            NS3D(K)=NS3D(K)+DUM
            END IF
            IF (QICU1D(K).GE.1.E-10) THEN
            DUM=QICU1D(K)*DT/(CI*(80.E-6)**DI)
            NI3D(K)=NI3D(K)+DUM
            END IF




             IF (QVQVS(K).LT.0.9) THEN
               IF (QR3D(K).LT.1.E-8) THEN
                  QV3D(K)=QV3D(K)+QR3D(K)
                  T3D(K)=T3D(K)-QR3D(K)*XXLV(K)/CPM(K)
                  QR3D(K)=0.
               END IF
               IF (QC3D(K).LT.1.E-8) THEN
                  QV3D(K)=QV3D(K)+QC3D(K)
                  T3D(K)=T3D(K)-QC3D(K)*XXLV(K)/CPM(K)
                  QC3D(K)=0.
               END IF
             END IF

             IF (QVQVSI(K).LT.0.9) THEN
               IF (QI3D(K).LT.1.E-8) THEN
                  QV3D(K)=QV3D(K)+QI3D(K)
                  T3D(K)=T3D(K)-QI3D(K)*XXLS(K)/CPM(K)
                  QI3D(K)=0.
               END IF
               IF (QNI3D(K).LT.1.E-8) THEN
                  QV3D(K)=QV3D(K)+QNI3D(K)
                  T3D(K)=T3D(K)-QNI3D(K)*XXLS(K)/CPM(K)
                  QNI3D(K)=0.
               END IF
               IF (QG3D(K).LT.1.E-8) THEN
                  QV3D(K)=QV3D(K)+QG3D(K)
                  T3D(K)=T3D(K)-QG3D(K)*XXLS(K)/CPM(K)
                  QG3D(K)=0.
               END IF
             END IF



            XLF(K) = XXLS(K)-XXLV(K)




       IF (QC3D(K).LT.QSMALL) THEN
         QC3D(K) = 0.
         NC3D(K) = 0.
         EFFC(K) = 0.
       END IF
       IF (QR3D(K).LT.QSMALL) THEN
         QR3D(K) = 0.
         NR3D(K) = 0.
         EFFR(K) = 0.
       END IF
       IF (QI3D(K).LT.QSMALL) THEN
         QI3D(K) = 0.
         NI3D(K) = 0.
         EFFI(K) = 0.
       END IF
       IF (QNI3D(K).LT.QSMALL) THEN
         QNI3D(K) = 0.
         NS3D(K) = 0.
         EFFS(K) = 0.
       END IF
       IF (QG3D(K).LT.QSMALL) THEN
         QG3D(K) = 0.
         NG3D(K) = 0.
         EFFG(K) = 0.
       END IF



      QRSTEN(K) = 0.
      QISTEN(K) = 0.
      QNISTEN(K) = 0.
      QCSTEN(K) = 0.
      QGSTEN(K) = 0.





            MU(K) = 1.496E-6*T3D(K)**1.5/(T3D(K)+120.)



            DUM = (RHOSU/RHO(K))**0.54




            AIN(K) = (RHOSU/RHO(K))**0.35*AI
            ARN(K) = DUM*AR
            ASN(K) = DUM*AS


            ACN(K) = G*RHOW/(18.*MU(K))

            AGN(K) = DUM*AG


            LAMI(K)=0.





            IF (QC3D(K).LT.QSMALL.AND.QI3D(K).LT.QSMALL.AND.QNI3D(K).LT.QSMALL &
                 .AND.QR3D(K).LT.QSMALL.AND.QG3D(K).LT.QSMALL) THEN
                 IF (T3D(K).LT.273.15.AND.QVQVSI(K).LT.0.999) GOTO 200
                 IF (T3D(K).GE.273.15.AND.QVQVS(K).LT.0.999) GOTO 200
            END IF




            KAP(K) = 1.414E3*MU(K)



            DV(K) = 8.794E-5*T3D(K)**1.81/PRES(K)




            SC(K) = MU(K)/(RHO(K)*DV(K))





            DUM = (RV*T3D(K)**2)

            DQSDT = XXLV(K)*QVS(K)/DUM
            DQSIDT =  XXLS(K)*QVI(K)/DUM

            ABI(K) = 1.+DQSIDT*XXLS(K)/CPM(K)
            AB(K) = 1.+DQSDT*XXLV(K)/CPM(K)






            IF (T3D(K).GE.273.15) THEN






         IF (iinum.EQ.1) THEN

            NC3D(K)=NDCNST*1.E6/RHO(K)
         END IF




       IF (QNI3D(K).LT.1.E-6) THEN
          QR3D(K)=QR3D(K)+QNI3D(K)
          NR3D(K)=NR3D(K)+NS3D(K)
          T3D(K)=T3D(K)-QNI3D(K)*XLF(K)/CPM(K)
          QNI3D(K) = 0.
          NS3D(K) = 0.
       END IF
       IF (QG3D(K).LT.1.E-6) THEN
          QR3D(K)=QR3D(K)+QG3D(K)
          NR3D(K)=NR3D(K)+NG3D(K)
          T3D(K)=T3D(K)-QG3D(K)*XLF(K)/CPM(K)
          QG3D(K) = 0.
          NG3D(K) = 0.
       END IF

       IF (QC3D(K).LT.QSMALL.AND.QNI3D(K).LT.1.E-8.AND.QR3D(K).LT.QSMALL.AND.QG3D(K).LT.1.E-8) GOTO 300



      NS3D(K) = MAX(0.,NS3D(K))
      NC3D(K) = MAX(0.,NC3D(K))
      NR3D(K) = MAX(0.,NR3D(K))
      NG3D(K) = MAX(0.,NG3D(K))




      IF (QR3D(K).GE.QSMALL) THEN
      LAMR(K) = (PI*RHOW*NR3D(K)/QR3D(K))**(1./3.)
      N0RR(K) = NR3D(K)*LAMR(K)





      IF (LAMR(K).LT.LAMMINR) THEN

      LAMR(K) = LAMMINR

      N0RR(K) = LAMR(K)**4*QR3D(K)/(PI*RHOW)

      NR3D(K) = N0RR(K)/LAMR(K)
      ELSE IF (LAMR(K).GT.LAMMAXR) THEN
      LAMR(K) = LAMMAXR
      N0RR(K) = LAMR(K)**4*QR3D(K)/(PI*RHOW)

      NR3D(K) = N0RR(K)/LAMR(K)
      END IF
      END IF






      IF (QC3D(K).GE.QSMALL) THEN

         DUM = PRES(K)/(287.15*T3D(K))
         PGAM(K)=0.0005714*(NC3D(K)/1.E6*DUM)+0.2714
         PGAM(K)=1./(PGAM(K)**2)-1.
         PGAM(K)=MAX(PGAM(K),2.)
         PGAM(K)=MIN(PGAM(K),10.)



      LAMC(K) = (CONS26*NC3D(K)*GAMMA(PGAM(K)+4.)/   &
                 (QC3D(K)*GAMMA(PGAM(K)+1.)))**(1./3.)




      LAMMIN = (PGAM(K)+1.)/60.E-6
      LAMMAX = (PGAM(K)+1.)/1.E-6

      IF (LAMC(K).LT.LAMMIN) THEN
      LAMC(K) = LAMMIN

      NC3D(K) = EXP(3.*LOG(LAMC(K))+LOG(QC3D(K))+              &
                LOG(GAMMA(PGAM(K)+1.))-LOG(GAMMA(PGAM(K)+4.)))/CONS26
      ELSE IF (LAMC(K).GT.LAMMAX) THEN
      LAMC(K) = LAMMAX

      NC3D(K) = EXP(3.*LOG(LAMC(K))+LOG(QC3D(K))+              &
                LOG(GAMMA(PGAM(K)+1.))-LOG(GAMMA(PGAM(K)+4.)))/CONS26

      END IF

      END IF




      IF (QNI3D(K).GE.QSMALL) THEN
      LAMS(K) = (CONS1*NS3D(K)/QNI3D(K))**(1./DS)
      N0S(K) = NS3D(K)*LAMS(K)





      IF (LAMS(K).LT.LAMMINS) THEN
      LAMS(K) = LAMMINS
      N0S(K) = LAMS(K)**4*QNI3D(K)/CONS1

      NS3D(K) = N0S(K)/LAMS(K)

      ELSE IF (LAMS(K).GT.LAMMAXS) THEN

      LAMS(K) = LAMMAXS
      N0S(K) = LAMS(K)**4*QNI3D(K)/CONS1

      NS3D(K) = N0S(K)/LAMS(K)
      END IF
      END IF




      IF (QG3D(K).GE.QSMALL) THEN
      LAMG(K) = (CONS2*NG3D(K)/QG3D(K))**(1./DG)
      N0G(K) = NG3D(K)*LAMG(K)



      IF (LAMG(K).LT.LAMMING) THEN
      LAMG(K) = LAMMING
      N0G(K) = LAMG(K)**4*QG3D(K)/CONS2

      NG3D(K) = N0G(K)/LAMG(K)

      ELSE IF (LAMG(K).GT.LAMMAXG) THEN

      LAMG(K) = LAMMAXG
      N0G(K) = LAMG(K)**4*QG3D(K)/CONS2

      NG3D(K) = N0G(K)/LAMG(K)
      END IF
      END IF




            PRC(K) = 0.
            NPRC(K) = 0.
            NPRC1(K) = 0.
            PRA(K) = 0.
            NPRA(K) = 0.
            NRAGG(K) = 0.
            PSMLT(K) = 0.
            NSMLTS(K) = 0.
            NSMLTR(K) = 0.
            EVPMS(K) = 0.
            PCC(K) = 0.
            PRE(K) = 0.
            NSUBC(K) = 0.
            NSUBR(K) = 0.
            PRACG(K) = 0.
            NPRACG(K) = 0.
            PSMLT(K) = 0.
            PGMLT(K) = 0.
            EVPMG(K) = 0.
            PRACS(K) = 0.
            NPRACS(K) = 0.
            NGMLTG(K) = 0.
            NGMLTR(K) = 0.














         IF (QC3D(K).GE.1.E-6) THEN




                PRC(K)=1350.*QC3D(K)**2.47*  &
           (NC3D(K)/1.e6*RHO(K))**(-1.79)




        NPRC1(K) = PRC(K)/CONS29
        NPRC(K) = PRC(K)/(QC3D(k)/NC3D(K))


                NPRC(K) = MIN(NPRC(K),NC3D(K)/DT)
                NPRC1(K) = MIN(NPRC1(K),NPRC(K))

         END IF





         IF (QR3D(K).GE.1.E-8.AND.QNI3D(K).GE.1.E-8) THEN

            UMS = ASN(K)*CONS3/(LAMS(K)**BS)
            UMR = ARN(K)*CONS4/(LAMR(K)**BR)
            UNS = ASN(K)*CONS5/LAMS(K)**BS
            UNR = ARN(K)*CONS6/LAMR(K)**BR




            dum=(rhosu/rho(k))**0.54
            UMS=MIN(UMS,1.2*dum)
            UNS=MIN(UNS,1.2*dum)
            UMR=MIN(UMR,9.1*dum)
            UNR=MIN(UNR,9.1*dum)











            PRACS(K) = CONS41*(((1.2*UMR-0.95*UMS)**2+                   &
                  0.08*UMS*UMR)**0.5*RHO(K)*                      &
                  N0RR(K)*N0S(K)/LAMR(K)**3*                              &
                  (5./(LAMR(K)**3*LAMS(K))+                    &
                  2./(LAMR(K)**2*LAMS(K)**2)+                  &				 
                  0.5/(LAMR(k)*LAMS(k)**3)))








         END IF





         IF (QR3D(K).GE.1.E-8.AND.QG3D(K).GE.1.E-8) THEN

            UMG = AGN(K)*CONS7/(LAMG(K)**BG)
            UMR = ARN(K)*CONS4/(LAMR(K)**BR)
            UNG = AGN(K)*CONS8/LAMG(K)**BG
            UNR = ARN(K)*CONS6/LAMR(K)**BR



            dum=(rhosu/rho(k))**0.54
            UMG=MIN(UMG,20.*dum)
            UNG=MIN(UNG,20.*dum)
            UMR=MIN(UMR,9.1*dum)
            UNR=MIN(UNR,9.1*dum)


            PRACG(K) = CONS41*(((1.2*UMR-0.95*UMG)**2+                   &
                  0.08*UMG*UMR)**0.5*RHO(K)*                      &
                  N0RR(K)*N0G(K)/LAMR(K)**3*                              &
                  (5./(LAMR(K)**3*LAMG(K))+                    &
                  2./(LAMR(K)**2*LAMG(K)**2)+				   &
				  0.5/(LAMR(k)*LAMG(k)**3)))



            DUM = PRACG(K)/5.2E-7

            NPRACG(K) = CONS32*RHO(K)*(1.7*(UNR-UNG)**2+            &
                0.3*UNR*UNG)**0.5*N0RR(K)*N0G(K)*              &
                (1./(LAMR(K)**3*LAMG(K))+                      &
                 1./(LAMR(K)**2*LAMG(K)**2)+                   &
                 1./(LAMR(K)*LAMG(K)**3))




            NPRACG(K)=NPRACG(K)-DUM

	    END IF






         IF (QR3D(K).GE.1.E-8 .AND. QC3D(K).GE.1.E-8) THEN




           DUM=(QC3D(K)*QR3D(K))
           PRA(K) = 67.*(DUM)**1.15
           NPRA(K) = PRA(K)/(QC3D(K)/NC3D(K))

         END IF






         IF (QR3D(K).GE.1.E-8) THEN

            dum1=300.e-6
            if (1./lamr(k).lt.dum1) then
            dum=1.
            else if (1./lamr(k).ge.dum1) then
            dum=2.-exp(2300.*(1./lamr(k)-dum1))
            end if

            NRAGG(K) = -5.78*dum*NR3D(K)*QR3D(K)*RHO(K)
         END IF




      IF (QR3D(K).GE.QSMALL) THEN
        EPSR = 2.*PI*N0RR(K)*RHO(K)*DV(K)*                           &
                   (F1R/(LAMR(K)*LAMR(K))+                       &
                    F2R*(ARN(K)*RHO(K)/MU(K))**0.5*                      &
                    SC(K)**(1./3.)*CONS9/                   &
                (LAMR(K)**CONS34))
      ELSE
      EPSR = 0.
      END IF



           IF (QV3D(K).LT.QVS(K)) THEN
              PRE(K) = EPSR*(QV3D(K)-QVS(K))/AB(K)
              PRE(K) = MIN(PRE(K),0.)
           ELSE
              PRE(K) = 0.
           END IF







          IF (QNI3D(K).GE.1.E-8) THEN




             DUM = -CPW/XLF(K)*(T3D(K)-273.15)*PRACS(K)

             PSMLT(K)=2.*PI*N0S(K)*KAP(K)*(273.15-T3D(K))/       &
                    XLF(K)*RHO(K)*(F1S/(LAMS(K)*LAMS(K))+        &
                    F2S*(ASN(K)*RHO(K)/MU(K))**0.5*                      &
                    SC(K)**(1./3.)*CONS10/                   &
                   (LAMS(K)**CONS35))+DUM



      IF (QVQVS(K).LT.1.) THEN
        EPSS = 2.*PI*N0S(K)*RHO(K)*DV(K)*                            &
                   (F1S/(LAMS(K)*LAMS(K))+                       &
                    F2S*(ASN(K)*RHO(K)/MU(K))**0.5*                      &
                    SC(K)**(1./3.)*CONS10/                   &
               (LAMS(K)**CONS35))

        EVPMS(K) = (QV3D(K)-QVS(K))*EPSS/AB(K)    
        EVPMS(K) = MAX(EVPMS(K),PSMLT(K))
        PSMLT(K) = PSMLT(K)-EVPMS(K)
      END IF
      END IF







          IF (QG3D(K).GE.1.E-8) THEN




             DUM = -CPW/XLF(K)*(T3D(K)-273.15)*PRACG(K)

             PGMLT(K)=2.*PI*N0G(K)*KAP(K)*(273.15-T3D(K))/ 		 &
                    XLF(K)*RHO(K)*(F1S/(LAMG(K)*LAMG(K))+                &
                    F2S*(AGN(K)*RHO(K)/MU(K))**0.5*                      &
                    SC(K)**(1./3.)*CONS11/                   &
                   (LAMG(K)**CONS36))+DUM



      IF (QVQVS(K).LT.1.) THEN
        EPSG = 2.*PI*N0G(K)*RHO(K)*DV(K)*                                &
                   (F1S/(LAMG(K)*LAMG(K))+                               &
                    F2S*(AGN(K)*RHO(K)/MU(K))**0.5*                      &
                    SC(K)**(1./3.)*CONS11/                   &
               (LAMG(K)**CONS36))

        EVPMG(K) = (QV3D(K)-QVS(K))*EPSG/AB(K)
        EVPMG(K) = MAX(EVPMG(K),PGMLT(K))
        PGMLT(K) = PGMLT(K)-EVPMG(K)
      END IF
      END IF






      PRACG(K) = 0.
      PRACS(K) = 0.









      DUM = (PRC(K)+PRA(K))*DT

      IF (DUM.GT.QC3D(K).AND.QC3D(K).GE.QSMALL) THEN

        RATIO = QC3D(K)/DUM

        PRC(K) = PRC(K)*RATIO
        PRA(K) = PRA(K)*RATIO

        END IF



        DUM = (-PSMLT(K)-EVPMS(K)+PRACS(K))*DT

        IF (DUM.GT.QNI3D(K).AND.QNI3D(K).GE.QSMALL) THEN


        RATIO = QNI3D(K)/DUM

        PSMLT(K) = PSMLT(K)*RATIO
        EVPMS(K) = EVPMS(K)*RATIO
        PRACS(K) = PRACS(K)*RATIO

        END IF



        DUM = (-PGMLT(K)-EVPMG(K)+PRACG(K))*DT

        IF (DUM.GT.QG3D(K).AND.QG3D(K).GE.QSMALL) THEN


        RATIO = QG3D(K)/DUM

        PGMLT(K) = PGMLT(K)*RATIO
        EVPMG(K) = EVPMG(K)*RATIO
        PRACG(K) = PRACG(K)*RATIO

        END IF




        DUM = (-PRACS(K)-PRACG(K)-PRE(K)-PRA(K)-PRC(K)+PSMLT(K)+PGMLT(K))*DT

        IF (DUM.GT.QR3D(K).AND.QR3D(K).GE.QSMALL) THEN

        RATIO = (QR3D(K)/DT+PRACS(K)+PRACG(K)+PRA(K)+PRC(K)-PSMLT(K)-PGMLT(K))/ &
                        (-PRE(K))
        PRE(K) = PRE(K)*RATIO
        
        END IF



      QV3DTEN(K) = QV3DTEN(K)+(-PRE(K)-EVPMS(K)-EVPMG(K))

      T3DTEN(K) = T3DTEN(K)+(PRE(K)*XXLV(K)+(EVPMS(K)+EVPMG(K))*XXLS(K)+&
                    (PSMLT(K)+PGMLT(K)-PRACS(K)-PRACG(K))*XLF(K))/CPM(K)

      QC3DTEN(K) = QC3DTEN(K)+(-PRA(K)-PRC(K))
      QR3DTEN(K) = QR3DTEN(K)+(PRE(K)+PRA(K)+PRC(K)-PSMLT(K)-PGMLT(K)+PRACS(K)+PRACG(K))
      QNI3DTEN(K) = QNI3DTEN(K)+(PSMLT(K)+EVPMS(K)-PRACS(K))
      QG3DTEN(K) = QG3DTEN(K)+(PGMLT(K)+EVPMG(K)-PRACG(K))




      NC3DTEN(K) = NC3DTEN(K)+ (-NPRA(K)-NPRC(K))
      NR3DTEN(K) = NR3DTEN(K)+ (NPRC1(K)+NRAGG(K)-NPRACG(K))



	C2PREC(K) = PRA(K)+PRC(K)
      IF (PRE(K).LT.0.) THEN
         DUM = PRE(K)*DT/QR3D(K)
           DUM = MAX(-1.,DUM)
         NSUBR(K) = DUM*NR3D(K)/DT
      END IF

        IF (EVPMS(K)+PSMLT(K).LT.0.) THEN
         DUM = (EVPMS(K)+PSMLT(K))*DT/QNI3D(K)
           DUM = MAX(-1.,DUM)
         NSMLTS(K) = DUM*NS3D(K)/DT
        END IF
        IF (PSMLT(K).LT.0.) THEN
          DUM = PSMLT(K)*DT/QNI3D(K)
          DUM = MAX(-1.0,DUM)
          NSMLTR(K) = DUM*NS3D(K)/DT
        END IF
        IF (EVPMG(K)+PGMLT(K).LT.0.) THEN
         DUM = (EVPMG(K)+PGMLT(K))*DT/QG3D(K)
           DUM = MAX(-1.,DUM)
         NGMLTG(K) = DUM*NG3D(K)/DT
        END IF
        IF (PGMLT(K).LT.0.) THEN
          DUM = PGMLT(K)*DT/QG3D(K)
          DUM = MAX(-1.0,DUM)
          NGMLTR(K) = DUM*NG3D(K)/DT
        END IF

         NS3DTEN(K) = NS3DTEN(K)+(NSMLTS(K))
         NG3DTEN(K) = NG3DTEN(K)+(NGMLTG(K))
         NR3DTEN(K) = NR3DTEN(K)+(NSUBR(K)-NSMLTR(K)-NGMLTR(K))

 300  CONTINUE





      DUMT = T3D(K)+DT*T3DTEN(K)
      DUMQV = QV3D(K)+DT*QV3DTEN(K)

      dum=min(0.99*pres(k),POLYSVP(DUMT,0))
      DUMQSS = EP_2*dum/(PRES(K)-dum)
      DUMQC = QC3D(K)+DT*QC3DTEN(K)
      DUMQC = MAX(DUMQC,0.)



      DUMS = DUMQV-DUMQSS
      PCC(K) = DUMS/(1.+XXLV(K)**2*DUMQSS/(CPM(K)*RV*DUMT**2))/DT
      IF (PCC(K)*DT+DUMQC.LT.0.) THEN
           PCC(K) = -DUMQC/DT
      END IF

      QV3DTEN(K) = QV3DTEN(K)-PCC(K)
      T3DTEN(K) = T3DTEN(K)+PCC(K)*XXLV(K)/CPM(K)
      QC3DTEN(K) = QC3DTEN(K)+PCC(K)























         ELSE  






         IF (iinum.EQ.1) THEN

            NC3D(K)=NDCNST*1.E6/RHO(K)
         END IF




      NI3D(K) = MAX(0.,NI3D(K))
      NS3D(K) = MAX(0.,NS3D(K))
      NC3D(K) = MAX(0.,NC3D(K))
      NR3D(K) = MAX(0.,NR3D(K))
      NG3D(K) = MAX(0.,NG3D(K))




      IF (QI3D(K).GE.QSMALL) THEN
         LAMI(K) = (CONS12*                 &
              NI3D(K)/QI3D(K))**(1./DI)
         N0I(K) = NI3D(K)*LAMI(K)





      IF (LAMI(K).LT.LAMMINI) THEN

      LAMI(K) = LAMMINI

      N0I(K) = LAMI(K)**4*QI3D(K)/CONS12

      NI3D(K) = N0I(K)/LAMI(K)
      ELSE IF (LAMI(K).GT.LAMMAXI) THEN
      LAMI(K) = LAMMAXI
      N0I(K) = LAMI(K)**4*QI3D(K)/CONS12

      NI3D(K) = N0I(K)/LAMI(K)
      END IF
      END IF




      IF (QR3D(K).GE.QSMALL) THEN
      LAMR(K) = (PI*RHOW*NR3D(K)/QR3D(K))**(1./3.)
      N0RR(K) = NR3D(K)*LAMR(K)





      IF (LAMR(K).LT.LAMMINR) THEN

      LAMR(K) = LAMMINR

      N0RR(K) = LAMR(K)**4*QR3D(K)/(PI*RHOW)

      NR3D(K) = N0RR(K)/LAMR(K)
      ELSE IF (LAMR(K).GT.LAMMAXR) THEN
      LAMR(K) = LAMMAXR
      N0RR(K) = LAMR(K)**4*QR3D(K)/(PI*RHOW)

      NR3D(K) = N0RR(K)/LAMR(K)
      END IF
      END IF






      IF (QC3D(K).GE.QSMALL) THEN

         DUM = PRES(K)/(287.15*T3D(K))
         PGAM(K)=0.0005714*(NC3D(K)/1.E6*DUM)+0.2714
         PGAM(K)=1./(PGAM(K)**2)-1.
         PGAM(K)=MAX(PGAM(K),2.)
         PGAM(K)=MIN(PGAM(K),10.)



      LAMC(K) = (CONS26*NC3D(K)*GAMMA(PGAM(K)+4.)/   &
                 (QC3D(K)*GAMMA(PGAM(K)+1.)))**(1./3.)




      LAMMIN = (PGAM(K)+1.)/60.E-6
      LAMMAX = (PGAM(K)+1.)/1.E-6

      IF (LAMC(K).LT.LAMMIN) THEN
      LAMC(K) = LAMMIN

      NC3D(K) = EXP(3.*LOG(LAMC(K))+LOG(QC3D(K))+              &
                LOG(GAMMA(PGAM(K)+1.))-LOG(GAMMA(PGAM(K)+4.)))/CONS26
      ELSE IF (LAMC(K).GT.LAMMAX) THEN
      LAMC(K) = LAMMAX
      NC3D(K) = EXP(3.*LOG(LAMC(K))+LOG(QC3D(K))+              &
                LOG(GAMMA(PGAM(K)+1.))-LOG(GAMMA(PGAM(K)+4.)))/CONS26

      END IF



        CDIST1(K) = NC3D(K)/GAMMA(PGAM(K)+1.)

      END IF




      IF (QNI3D(K).GE.QSMALL) THEN
      LAMS(K) = (CONS1*NS3D(K)/QNI3D(K))**(1./DS)
      N0S(K) = NS3D(K)*LAMS(K)





      IF (LAMS(K).LT.LAMMINS) THEN
      LAMS(K) = LAMMINS
      N0S(K) = LAMS(K)**4*QNI3D(K)/CONS1

      NS3D(K) = N0S(K)/LAMS(K)

      ELSE IF (LAMS(K).GT.LAMMAXS) THEN

      LAMS(K) = LAMMAXS
      N0S(K) = LAMS(K)**4*QNI3D(K)/CONS1

      NS3D(K) = N0S(K)/LAMS(K)
      END IF
      END IF




      IF (QG3D(K).GE.QSMALL) THEN
      LAMG(K) = (CONS2*NG3D(K)/QG3D(K))**(1./DG)
      N0G(K) = NG3D(K)*LAMG(K)





      IF (LAMG(K).LT.LAMMING) THEN
      LAMG(K) = LAMMING
      N0G(K) = LAMG(K)**4*QG3D(K)/CONS2

      NG3D(K) = N0G(K)/LAMG(K)

      ELSE IF (LAMG(K).GT.LAMMAXG) THEN

      LAMG(K) = LAMMAXG
      N0G(K) = LAMG(K)**4*QG3D(K)/CONS2

      NG3D(K) = N0G(K)/LAMG(K)
      END IF
      END IF




            MNUCCC(K) = 0.
            NNUCCC(K) = 0.
            PRC(K) = 0.
            NPRC(K) = 0.
            NPRC1(K) = 0.
            NSAGG(K) = 0.
            PSACWS(K) = 0.
            NPSACWS(K) = 0.
            PSACWI(K) = 0.
            NPSACWI(K) = 0.
            PRACS(K) = 0.
            NPRACS(K) = 0.
            NMULTS(K) = 0.
            QMULTS(K) = 0.
            NMULTR(K) = 0.
            QMULTR(K) = 0.
            NMULTG(K) = 0.
            QMULTG(K) = 0.
            NMULTRG(K) = 0.
            QMULTRG(K) = 0.
            MNUCCR(K) = 0.
            NNUCCR(K) = 0.
            PRA(K) = 0.
            NPRA(K) = 0.
            NRAGG(K) = 0.
            PRCI(K) = 0.
            NPRCI(K) = 0.
            PRAI(K) = 0.
            NPRAI(K) = 0.
            NNUCCD(K) = 0.
            MNUCCD(K) = 0.
            PCC(K) = 0.
            PRE(K) = 0.
            PRD(K) = 0.
            PRDS(K) = 0.
            EPRD(K) = 0.
            EPRDS(K) = 0.
            NSUBC(K) = 0.
            NSUBI(K) = 0.
            NSUBS(K) = 0.
            NSUBR(K) = 0.
            PIACR(K) = 0.
            NIACR(K) = 0.
            PRACI(K) = 0.
            PIACRS(K) = 0.
            NIACRS(K) = 0.
            PRACIS(K) = 0.

            PRACG(K) = 0.
            PSACR(K) = 0.
	    PSACWG(K) = 0.
	    PGSACW(K) = 0.
            PGRACS(K) = 0.
	    PRDG(K) = 0.
	    EPRDG(K) = 0.
	    NPRACG(K) = 0.
	    NPSACWG(K) = 0.
	    NSCNG(K) = 0.
 	    NGRACS(K) = 0.
	    NSUBG(K) = 0.







        IF (QC3D(K).GE.QSMALL .AND. T3D(K).LT.269.15) THEN






           NACNT = EXP(-2.80+0.262*(273.15-T3D(K)))*1000.











            DUM = 7.37*T3D(K)/(288.*10.*PRES(K))/100.




            DAP(K) = CONS37*T3D(K)*(1.+DUM/RIN)/MU(K)
 
           MNUCCC(K) = CONS38*DAP(K)*NACNT*EXP(LOG(CDIST1(K))+   &
                   LOG(GAMMA(PGAM(K)+5.))-4.*LOG(LAMC(K)))
           NNUCCC(K) = 2.*PI*DAP(K)*NACNT*CDIST1(K)*           &
                    GAMMA(PGAM(K)+2.)/                         &
                    LAMC(K)












           MNUCCC(K) = MNUCCC(K)+CONS39*                   &
                  EXP(LOG(CDIST1(K))+LOG(GAMMA(7.+PGAM(K)))-6.*LOG(LAMC(K)))*             &
                   (EXP(AIMM*(273.15-T3D(K)))-1.)

           NNUCCC(K) = NNUCCC(K)+                                  &
            CONS40*EXP(LOG(CDIST1(K))+LOG(GAMMA(PGAM(K)+4.))-3.*LOG(LAMC(K)))              &
                *(EXP(AIMM*(273.15-T3D(K)))-1.)




           NNUCCC(K) = MIN(NNUCCC(K),NC3D(K)/DT)

        END IF











         IF (QC3D(K).GE.1.E-6) THEN




                PRC(K)=1350.*QC3D(K)**2.47*  &
           (NC3D(K)/1.e6*RHO(K))**(-1.79)




        NPRC1(K) = PRC(K)/CONS29
        NPRC(K) = PRC(K)/(QC3D(K)/NC3D(K))


                NPRC(K) = MIN(NPRC(K),NC3D(K)/DT)
                NPRC1(K) = MIN(NPRC1(K),NPRC(K))

         END IF







         IF (QNI3D(K).GE.1.E-8) THEN
             NSAGG(K) = CONS15*ASN(K)*RHO(K)**            &
            ((2.+BS)/3.)*QNI3D(K)**((2.+BS)/3.)*                  &
            (NS3D(K)*RHO(K))**((4.-BS)/3.)/                       &
            (RHO(K))
         END IF








         IF (QNI3D(K).GE.1.E-8 .AND. QC3D(K).GE.QSMALL) THEN

           PSACWS(K) = CONS13*ASN(K)*QC3D(K)*RHO(K)*               &
                  N0S(K)/                        &
                  LAMS(K)**(BS+3.)
           NPSACWS(K) = CONS13*ASN(K)*NC3D(K)*RHO(K)*              &
                  N0S(K)/                        &
                  LAMS(K)**(BS+3.)

         END IF




         IF (QG3D(K).GE.1.E-8 .AND. QC3D(K).GE.QSMALL) THEN

           PSACWG(K) = CONS14*AGN(K)*QC3D(K)*RHO(K)*               &
                  N0G(K)/                        &
                  LAMG(K)**(BG+3.)
           NPSACWG(K) = CONS14*AGN(K)*NC3D(K)*RHO(K)*              &
                  N0G(K)/                        &
                  LAMG(K)**(BG+3.)
	    END IF








         IF (QI3D(K).GE.1.E-8 .AND. QC3D(K).GE.QSMALL) THEN




            IF (1./LAMI(K).GE.100.E-6) THEN

           PSACWI(K) = CONS16*AIN(K)*QC3D(K)*RHO(K)*               &
                  N0I(K)/                        &
                  LAMI(K)**(BI+3.)
           NPSACWI(K) = CONS16*AIN(K)*NC3D(K)*RHO(K)*              &
                  N0I(K)/                        &
                  LAMI(K)**(BI+3.)
           END IF
         END IF





         IF (QR3D(K).GE.1.E-8.AND.QNI3D(K).GE.1.E-8) THEN

            UMS = ASN(K)*CONS3/(LAMS(K)**BS)
            UMR = ARN(K)*CONS4/(LAMR(K)**BR)
            UNS = ASN(K)*CONS5/LAMS(K)**BS
            UNR = ARN(K)*CONS6/LAMR(K)**BR




            dum=(rhosu/rho(k))**0.54
            UMS=MIN(UMS,1.2*dum)
            UNS=MIN(UNS,1.2*dum)
            UMR=MIN(UMR,9.1*dum)
            UNR=MIN(UNR,9.1*dum)

            PRACS(K) = CONS41*(((1.2*UMR-0.95*UMS)**2+                   &
                  0.08*UMS*UMR)**0.5*RHO(K)*                      &
                  N0RR(K)*N0S(K)/LAMR(K)**3*                              &
                  (5./(LAMR(K)**3*LAMS(K))+                    &
                  2./(LAMR(K)**2*LAMS(K)**2)+                  &				 
                  0.5/(LAMR(k)*LAMS(k)**3)))

            NPRACS(K) = CONS32*RHO(K)*(1.7*(UNR-UNS)**2+            &
                0.3*UNR*UNS)**0.5*N0RR(K)*N0S(K)*              &
                (1./(LAMR(K)**3*LAMS(K))+                      &
                 1./(LAMR(K)**2*LAMS(K)**2)+                   &
                 1./(LAMR(K)*LAMS(K)**3))





            PRACS(K) = MIN(PRACS(K),QR3D(K)/DT)






            IF (QNI3D(K).GE.0.1E-3.AND.QR3D(K).GE.0.1E-3) THEN
            PSACR(K) = CONS31*(((1.2*UMR-0.95*UMS)**2+              &
                  0.08*UMS*UMR)**0.5*RHO(K)*                     &
                 N0RR(K)*N0S(K)/LAMS(K)**3*                               &
                  (5./(LAMS(K)**3*LAMR(K))+                    &
                  2./(LAMS(K)**2*LAMR(K)**2)+                  &
                  0.5/(LAMS(K)*LAMR(K)**3)))            
            END IF


         END IF





         IF (QR3D(K).GE.1.E-8.AND.QG3D(K).GE.1.E-8) THEN

            UMG = AGN(K)*CONS7/(LAMG(K)**BG)
            UMR = ARN(K)*CONS4/(LAMR(K)**BR)
            UNG = AGN(K)*CONS8/LAMG(K)**BG
            UNR = ARN(K)*CONS6/LAMR(K)**BR



            dum=(rhosu/rho(k))**0.54
            UMG=MIN(UMG,20.*dum)
            UNG=MIN(UNG,20.*dum)
            UMR=MIN(UMR,9.1*dum)
            UNR=MIN(UNR,9.1*dum)

            PRACG(K) = CONS41*(((1.2*UMR-0.95*UMG)**2+                   &
                  0.08*UMG*UMR)**0.5*RHO(K)*                      &
                  N0RR(K)*N0G(K)/LAMR(K)**3*                              &
                  (5./(LAMR(K)**3*LAMG(K))+                    &
                  2./(LAMR(K)**2*LAMG(K)**2)+				   &
				  0.5/(LAMR(k)*LAMG(k)**3)))

            NPRACG(K) = CONS32*RHO(K)*(1.7*(UNR-UNG)**2+            &
                0.3*UNR*UNG)**0.5*N0RR(K)*N0G(K)*              &
                (1./(LAMR(K)**3*LAMG(K))+                      &
                 1./(LAMR(K)**2*LAMG(K)**2)+                   &
                 1./(LAMR(K)*LAMG(K)**3))





            PRACG(K) = MIN(PRACG(K),QR3D(K)/DT)

	    END IF













         IF (QNI3D(K).GE.0.1E-3) THEN
         IF (QC3D(K).GE.0.5E-3.OR.QR3D(K).GE.0.1E-3) THEN
         IF (PSACWS(K).GT.0..OR.PRACS(K).GT.0.) THEN
            IF (T3D(K).LT.270.16 .AND. T3D(K).GT.265.16) THEN

               IF (T3D(K).GT.270.16) THEN
                  FMULT = 0.
               ELSE IF (T3D(K).LE.270.16.AND.T3D(K).GT.268.16)  THEN
                  FMULT = (270.16-T3D(K))/2.
               ELSE IF (T3D(K).GE.265.16.AND.T3D(K).LE.268.16)   THEN
                  FMULT = (T3D(K)-265.16)/3.
               ELSE IF (T3D(K).LT.265.16) THEN
                  FMULT = 0.
               END IF





               IF (PSACWS(K).GT.0.) THEN
                  NMULTS(K) = 35.E4*PSACWS(K)*FMULT*1000.
                  QMULTS(K) = NMULTS(K)*MMULT




                  QMULTS(K) = MIN(QMULTS(K),PSACWS(K))
                  PSACWS(K) = PSACWS(K)-QMULTS(K)

               END IF



               IF (PRACS(K).GT.0.) THEN
                   NMULTR(K) = 35.E4*PRACS(K)*FMULT*1000.
                   QMULTR(K) = NMULTR(K)*MMULT




                   QMULTR(K) = MIN(QMULTR(K),PRACS(K))

                   PRACS(K) = PRACS(K)-QMULTR(K)

               END IF

            END IF
         END IF
         END IF
         END IF













         IF (QG3D(K).GE.0.1E-3) THEN
         IF (QC3D(K).GE.0.5E-3.OR.QR3D(K).GE.0.1E-3) THEN
         IF (PSACWG(K).GT.0..OR.PRACG(K).GT.0.) THEN
            IF (T3D(K).LT.270.16 .AND. T3D(K).GT.265.16) THEN

               IF (T3D(K).GT.270.16) THEN
                  FMULT = 0.
               ELSE IF (T3D(K).LE.270.16.AND.T3D(K).GT.268.16)  THEN
                  FMULT = (270.16-T3D(K))/2.
               ELSE IF (T3D(K).GE.265.16.AND.T3D(K).LE.268.16)   THEN
                  FMULT = (T3D(K)-265.16)/3.
               ELSE IF (T3D(K).LT.265.16) THEN
                  FMULT = 0.
               END IF





               IF (PSACWG(K).GT.0.) THEN
                  NMULTG(K) = 35.E4*PSACWG(K)*FMULT*1000.
                  QMULTG(K) = NMULTG(K)*MMULT




                  QMULTG(K) = MIN(QMULTG(K),PSACWG(K))
                  PSACWG(K) = PSACWG(K)-QMULTG(K)

               END IF



               IF (PRACG(K).GT.0.) THEN
                   NMULTRG(K) = 35.E4*PRACG(K)*FMULT*1000.
                   QMULTRG(K) = NMULTRG(K)*MMULT




                   QMULTRG(K) = MIN(QMULTRG(K),PRACG(K))
                   PRACG(K) = PRACG(K)-QMULTRG(K)

               END IF
               END IF
               END IF
            END IF
            END IF






	   IF (PSACWS(K).GT.0.) THEN

              IF (QNI3D(K).GE.0.1E-3.AND.QC3D(K).GE.0.5E-3) THEN


	     PGSACW(K) = MIN(PSACWS(K),CONS17*DT*N0S(K)*QC3D(K)*QC3D(K)* &
                          ASN(K)*ASN(K)/ &
                           (RHO(K)*LAMS(K)**(2.*BS+2.))) 


	     DUM = MAX(RHOSN/(RHOG-RHOSN)*PGSACW(K),0.) 


	     NSCNG(K) = DUM/MG0*RHO(K)

             NSCNG(K) = MIN(NSCNG(K),NS3D(K)/DT)


             PSACWS(K) = PSACWS(K) - PGSACW(K)
             END IF
	   END IF



	   IF (PRACS(K).GT.0.) THEN

              IF (QNI3D(K).GE.0.1E-3.AND.QR3D(K).GE.0.1E-3) THEN

	      DUM = CONS18*(4./LAMS(K))**3*(4./LAMS(K))**3 &    
                   /(CONS18*(4./LAMS(K))**3*(4./LAMS(K))**3+ &  
                   CONS19*(4./LAMR(K))**3*(4./LAMR(K))**3)
              DUM=MIN(DUM,1.)
              DUM=MAX(DUM,0.)
	      PGRACS(K) = (1.-DUM)*PRACS(K)
            NGRACS(K) = (1.-DUM)*NPRACS(K)

            NGRACS(K) = MIN(NGRACS(K),NR3D(K)/DT)
            NGRACS(K) = MIN(NGRACS(K),NS3D(K)/DT)


            PRACS(K) = PRACS(K) - PGRACS(K)
            NPRACS(K) = NPRACS(K) - NGRACS(K)

            PSACR(K)=PSACR(K)*(1.-DUM)
            END IF
	   END IF






         IF (T3D(K).LT.269.15.AND.QR3D(K).GE.QSMALL) THEN








            MNUCCR(K) = CONS20*NR3D(K)*(EXP(AIMM*(273.15-T3D(K)))-1.)/LAMR(K)**3 &
                 /LAMR(K)**3

            NNUCCR(K) = PI*NR3D(K)*BIMM*(EXP(AIMM*(273.15-T3D(K)))-1.)/LAMR(K)**3


            NNUCCR(K) = MIN(NNUCCR(K),NR3D(K)/DT)

         END IF






         IF (QR3D(K).GE.1.E-8 .AND. QC3D(K).GE.1.E-8) THEN




           DUM=(QC3D(K)*QR3D(K))
           PRA(K) = 67.*(DUM)**1.15
           NPRA(K) = PRA(K)/(QC3D(K)/NC3D(K))

         END IF






         IF (QR3D(K).GE.1.E-8) THEN

            dum1=300.e-6
            if (1./lamr(k).lt.dum1) then
            dum=1.
            else if (1./lamr(k).ge.dum1) then
            dum=2.-exp(2300.*(1./lamr(k)-dum1))
            end if

            NRAGG(K) = -5.78*dum*NR3D(K)*QR3D(K)*RHO(K)
         END IF







         IF (QI3D(K).GE.1.E-8 .AND.QVQVSI(K).GE.1.) THEN



              NPRCI(K) = CONS21*(QV3D(K)-QVI(K))*RHO(K)                         &
                *N0I(K)*EXP(-LAMI(K)*DCS)*DV(K)/ABI(K)
              PRCI(K) = CONS22*NPRCI(K)
              NPRCI(K) = MIN(NPRCI(K),NI3D(K)/DT)


         END IF






         IF (QNI3D(K).GE.1.E-8 .AND. QI3D(K).GE.QSMALL) THEN
            PRAI(K) = CONS23*ASN(K)*QI3D(K)*RHO(K)*N0S(K)/     &
                     LAMS(K)**(BS+3.)
            NPRAI(K) = CONS23*ASN(K)*NI3D(K)*                                       &
                  RHO(K)*N0S(K)/                                 &
                  LAMS(K)**(BS+3.)
            NPRAI(K)=MIN(NPRAI(K),NI3D(K)/DT)
         END IF






         IF (QR3D(K).GE.1.E-8.AND.QI3D(K).GE.1.E-8.AND.T3D(K).LE.273.15) THEN




            IF (QR3D(K).GE.0.1E-3) THEN
            NIACR(K)=CONS24*NI3D(K)*N0RR(K)*ARN(K) &
                /LAMR(K)**(BR+3.)*RHO(K)
            PIACR(K)=CONS25*NI3D(K)*N0RR(K)*ARN(K) &
                /LAMR(K)**(BR+3.)/LAMR(K)**3*RHO(K)
            PRACI(K)=CONS24*QI3D(K)*N0RR(K)*ARN(K)/ &
                LAMR(K)**(BR+3.)*RHO(K)
            NIACR(K)=MIN(NIACR(K),NR3D(K)/DT)
            NIACR(K)=MIN(NIACR(K),NI3D(K)/DT)
            ELSE 
            NIACRS(K)=CONS24*NI3D(K)*N0RR(K)*ARN(K) &
                /LAMR(K)**(BR+3.)*RHO(K)
            PIACRS(K)=CONS25*NI3D(K)*N0RR(K)*ARN(K) &
                /LAMR(K)**(BR+3.)/LAMR(K)**3*RHO(K)
            PRACIS(K)=CONS24*QI3D(K)*N0RR(K)*ARN(K)/ &
                LAMR(K)**(BR+3.)*RHO(K)
            NIACRS(K)=MIN(NIACRS(K),NR3D(K)/DT)
            NIACRS(K)=MIN(NIACRS(K),NI3D(K)/DT)
            END IF
         END IF




         IF (INUC.EQ.0) THEN



         if ((QVQVS(K).GE.0.999.and.T3D(K).le.265.15).or. &
              QVQVSI(K).ge.1.08) then


      kc2 = 0.005*exp(0.304*(273.15-T3D(K)))*1000. 

      kc2 = min(kc2,500.e3)
      kc2=MAX(kc2/rho(k),0.)  

          IF (KC2.GT.NI3D(K)+NS3D(K)+NG3D(K)) THEN
             NNUCCD(K) = (KC2-NI3D(K)-NS3D(K)-NG3D(K))/DT
             MNUCCD(K) = NNUCCD(K)*MI0
          END IF

          END IF

          ELSE IF (INUC.EQ.1) THEN

          IF (T3D(K).LT.273.15.AND.QVQVSI(K).GT.1.) THEN

             KC2 = 0.16*1000./RHO(K)  
          IF (KC2.GT.NI3D(K)+NS3D(K)+NG3D(K)) THEN
             NNUCCD(K) = (KC2-NI3D(K)-NS3D(K)-NG3D(K))/DT
             MNUCCD(K) = NNUCCD(K)*MI0
          END IF
          END IF

         END IF



 101      CONTINUE






        IF (QI3D(K).GE.QSMALL) THEN

         EPSI = 2.*PI*N0I(K)*RHO(K)*DV(K)/(LAMI(K)*LAMI(K))

      ELSE
         EPSI = 0.
      END IF

      IF (QNI3D(K).GE.QSMALL) THEN
        EPSS = 2.*PI*N0S(K)*RHO(K)*DV(K)*                            &
                   (F1S/(LAMS(K)*LAMS(K))+                       &
                    F2S*(ASN(K)*RHO(K)/MU(K))**0.5*                      &
                    SC(K)**(1./3.)*CONS10/                   &
               (LAMS(K)**CONS35))
      ELSE
      EPSS = 0.
      END IF

      IF (QG3D(K).GE.QSMALL) THEN
        EPSG = 2.*PI*N0G(K)*RHO(K)*DV(K)*                                &
                   (F1S/(LAMG(K)*LAMG(K))+                               &
                    F2S*(AGN(K)*RHO(K)/MU(K))**0.5*                      &
                    SC(K)**(1./3.)*CONS11/                   &
               (LAMG(K)**CONS36))


      ELSE
      EPSG = 0.
      END IF

      IF (QR3D(K).GE.QSMALL) THEN
        EPSR = 2.*PI*N0RR(K)*RHO(K)*DV(K)*                           &
                   (F1R/(LAMR(K)*LAMR(K))+                       &
                    F2R*(ARN(K)*RHO(K)/MU(K))**0.5*                      &
                    SC(K)**(1./3.)*CONS9/                   &
                (LAMR(K)**CONS34))
      ELSE
      EPSR = 0.
      END IF





              IF (QI3D(K).GE.QSMALL) THEN              
              DUM=(1.-EXP(-LAMI(K)*DCS)*(1.+LAMI(K)*DCS))
              PRD(K) = EPSI*(QV3D(K)-QVI(K))/ABI(K)*DUM
              ELSE
              DUM=0.
              END IF

              IF (QNI3D(K).GE.QSMALL) THEN
              PRDS(K) = EPSS*(QV3D(K)-QVI(K))/ABI(K)+ &
                EPSI*(QV3D(K)-QVI(K))/ABI(K)*(1.-DUM)

              ELSE
              PRD(K) = PRD(K)+EPSI*(QV3D(K)-QVI(K))/ABI(K)*(1.-DUM)
              END IF

              PRDG(K) = EPSG*(QV3D(K)-QVI(K))/ABI(K)



           IF (QV3D(K).LT.QVS(K)) THEN
              PRE(K) = EPSR*(QV3D(K)-QVS(K))/AB(K)
              PRE(K) = MIN(PRE(K),0.)
           ELSE
              PRE(K) = 0.
           END IF




           DUM = (QV3D(K)-QVI(K))/DT

           FUDGEF = 0.9999
           SUM_DEP = PRD(K)+PRDS(K)+MNUCCD(K)+PRDG(K)

           IF( (DUM.GT.0. .AND. SUM_DEP.GT.DUM*FUDGEF) .OR.                      &
               (DUM.LT.0. .AND. SUM_DEP.LT.DUM*FUDGEF) ) THEN
               MNUCCD(K) = FUDGEF*MNUCCD(K)*DUM/SUM_DEP
               PRD(K) = FUDGEF*PRD(K)*DUM/SUM_DEP
               PRDS(K) = FUDGEF*PRDS(K)*DUM/SUM_DEP
	       PRDG(K) = FUDGEF*PRDG(K)*DUM/SUM_DEP
           ENDIF



           IF (PRD(K).LT.0.) THEN
              EPRD(K)=PRD(K)
              PRD(K)=0.
           END IF
           IF (PRDS(K).LT.0.) THEN
              EPRDS(K)=PRDS(K)
              PRDS(K)=0.
           END IF
           IF (PRDG(K).LT.0.) THEN
              EPRDG(K)=PRDG(K)
              PRDG(K)=0.
           END IF



















      IF (ILIQ.EQ.1) THEN
      MNUCCC(K)=0.
      NNUCCC(K)=0.
      MNUCCR(K)=0.
      NNUCCR(K)=0.
      MNUCCD(K)=0.
      NNUCCD(K)=0.
      END IF


      IF (IGRAUP.EQ.1) THEN
            PRACG(K) = 0.
            PSACR(K) = 0.
	    PSACWG(K) = 0.
	    PRDG(K) = 0.
	    EPRDG(K) = 0.
            EVPMG(K) = 0.
            PGMLT(K) = 0.
	    NPRACG(K) = 0.
	    NPSACWG(K) = 0.
	    NSCNG(K) = 0.
 	    NGRACS(K) = 0.
	    NSUBG(K) = 0.
	    NGMLTG(K) = 0.
            NGMLTR(K) = 0.

            PIACRS(K)=PIACRS(K)+PIACR(K)
            PIACR(K) = 0.

	    PRACIS(K)=PRACIS(K)+PRACI(K)
	    PRACI(K) = 0.
	    PSACWS(K)=PSACWS(K)+PGSACW(K)
	    PGSACW(K) = 0.
	    PRACS(K)=PRACS(K)+PGRACS(K)
	    PGRACS(K) = 0.
       END IF



      DUM = (PRC(K)+PRA(K)+MNUCCC(K)+PSACWS(K)+PSACWI(K)+QMULTS(K)+PSACWG(K)+PGSACW(K)+QMULTG(K))*DT

      IF (DUM.GT.QC3D(K).AND.QC3D(K).GE.QSMALL) THEN
        RATIO = QC3D(K)/DUM

        PRC(K) = PRC(K)*RATIO
        PRA(K) = PRA(K)*RATIO
        MNUCCC(K) = MNUCCC(K)*RATIO
        PSACWS(K) = PSACWS(K)*RATIO
        PSACWI(K) = PSACWI(K)*RATIO
        QMULTS(K) = QMULTS(K)*RATIO
        QMULTG(K) = QMULTG(K)*RATIO
        PSACWG(K) = PSACWG(K)*RATIO
	PGSACW(K) = PGSACW(K)*RATIO
        END IF
 


      DUM = (-PRD(K)-MNUCCC(K)+PRCI(K)+PRAI(K)-QMULTS(K)-QMULTG(K)-QMULTR(K)-QMULTRG(K) &
                -MNUCCD(K)+PRACI(K)+PRACIS(K)-EPRD(K)-PSACWI(K))*DT

      IF (DUM.GT.QI3D(K).AND.QI3D(K).GE.QSMALL) THEN

        RATIO = (QI3D(K)/DT+PRD(K)+MNUCCC(K)+QMULTS(K)+QMULTG(K)+QMULTR(K)+QMULTRG(K)+ &
                     MNUCCD(K)+PSACWI(K))/ &
                      (PRCI(K)+PRAI(K)+PRACI(K)+PRACIS(K)-EPRD(K))

        PRCI(K) = PRCI(K)*RATIO
        PRAI(K) = PRAI(K)*RATIO
        PRACI(K) = PRACI(K)*RATIO
        PRACIS(K) = PRACIS(K)*RATIO
        EPRD(K) = EPRD(K)*RATIO

        END IF



      DUM=((PRACS(K)-PRE(K))+(QMULTR(K)+QMULTRG(K)-PRC(K))+(MNUCCR(K)-PRA(K))+ &
             PIACR(K)+PIACRS(K)+PGRACS(K)+PRACG(K))*DT

      IF (DUM.GT.QR3D(K).AND.QR3D(K).GE.QSMALL) THEN

        RATIO = (QR3D(K)/DT+PRC(K)+PRA(K))/ &
             (-PRE(K)+QMULTR(K)+QMULTRG(K)+PRACS(K)+MNUCCR(K)+PIACR(K)+PIACRS(K)+PGRACS(K)+PRACG(K))

        PRE(K) = PRE(K)*RATIO
        PRACS(K) = PRACS(K)*RATIO
        QMULTR(K) = QMULTR(K)*RATIO
        QMULTRG(K) = QMULTRG(K)*RATIO
        MNUCCR(K) = MNUCCR(K)*RATIO
        PIACR(K) = PIACR(K)*RATIO
        PIACRS(K) = PIACRS(K)*RATIO
        PGRACS(K) = PGRACS(K)*RATIO
        PRACG(K) = PRACG(K)*RATIO

        END IF




        IF (IGRAUP.EQ.0) THEN

      DUM = (-PRDS(K)-PSACWS(K)-PRAI(K)-PRCI(K)-PRACS(K)-EPRDS(K)+PSACR(K)-PIACRS(K)-PRACIS(K))*DT

      IF (DUM.GT.QNI3D(K).AND.QNI3D(K).GE.QSMALL) THEN

        RATIO = (QNI3D(K)/DT+PRDS(K)+PSACWS(K)+PRAI(K)+PRCI(K)+PRACS(K)+PIACRS(K)+PRACIS(K))/(-EPRDS(K)+PSACR(K))

       EPRDS(K) = EPRDS(K)*RATIO
       PSACR(K) = PSACR(K)*RATIO

       END IF


       ELSE IF (IGRAUP.EQ.1) THEN

      DUM = (-PRDS(K)-PSACWS(K)-PRAI(K)-PRCI(K)-PRACS(K)-EPRDS(K)+PSACR(K)-PIACRS(K)-PRACIS(K)-MNUCCR(K))*DT

      IF (DUM.GT.QNI3D(K).AND.QNI3D(K).GE.QSMALL) THEN

       RATIO = (QNI3D(K)/DT+PRDS(K)+PSACWS(K)+PRAI(K)+PRCI(K)+PRACS(K)+PIACRS(K)+PRACIS(K)+MNUCCR(K))/(-EPRDS(K)+PSACR(K))

       EPRDS(K) = EPRDS(K)*RATIO
       PSACR(K) = PSACR(K)*RATIO

       END IF

       END IF



      DUM = (-PSACWG(K)-PRACG(K)-PGSACW(K)-PGRACS(K)-PRDG(K)-MNUCCR(K)-EPRDG(K)-PIACR(K)-PRACI(K)-PSACR(K))*DT

      IF (DUM.GT.QG3D(K).AND.QG3D(K).GE.QSMALL) THEN

        RATIO = (QG3D(K)/DT+PSACWG(K)+PRACG(K)+PGSACW(K)+PGRACS(K)+PRDG(K)+MNUCCR(K)+PSACR(K)+&
                  PIACR(K)+PRACI(K))/(-EPRDG(K))

       EPRDG(K) = EPRDG(K)*RATIO

      END IF



      QV3DTEN(K) = QV3DTEN(K)+(-PRE(K)-PRD(K)-PRDS(K)-MNUCCD(K)-EPRD(K)-EPRDS(K)-PRDG(K)-EPRDG(K))


      T3DTEN(K) = T3DTEN(K)+(PRE(K)                                 &
               *XXLV(K)+(PRD(K)+PRDS(K)+                            &
                MNUCCD(K)+EPRD(K)+EPRDS(K)+PRDG(K)+EPRDG(K))*XXLS(K)+         &
               (PSACWS(K)+PSACWI(K)+MNUCCC(K)+MNUCCR(K)+                      &
                QMULTS(K)+QMULTG(K)+QMULTR(K)+QMULTRG(K)+PRACS(K) &
                +PSACWG(K)+PRACG(K)+PGSACW(K)+PGRACS(K)+PIACR(K)+PIACRS(K))*XLF(K))/CPM(K)

      QC3DTEN(K) = QC3DTEN(K)+                                      &
                 (-PRA(K)-PRC(K)-MNUCCC(K)+PCC(K)-                  &
                  PSACWS(K)-PSACWI(K)-QMULTS(K)-QMULTG(K)-PSACWG(K)-PGSACW(K))
      QI3DTEN(K) = QI3DTEN(K)+                                      &
         (PRD(K)+EPRD(K)+PSACWI(K)+MNUCCC(K)-PRCI(K)-                                 &
                  PRAI(K)+QMULTS(K)+QMULTG(K)+QMULTR(K)+QMULTRG(K)+MNUCCD(K)-PRACI(K)-PRACIS(K))
      QR3DTEN(K) = QR3DTEN(K)+                                      &
                 (PRE(K)+PRA(K)+PRC(K)-PRACS(K)-MNUCCR(K)-QMULTR(K)-QMULTRG(K) &
             -PIACR(K)-PIACRS(K)-PRACG(K)-PGRACS(K))

      IF (IGRAUP.EQ.0) THEN

      QNI3DTEN(K) = QNI3DTEN(K)+                                    &
           (PRAI(K)+PSACWS(K)+PRDS(K)+PRACS(K)+PRCI(K)+EPRDS(K)-PSACR(K)+PIACRS(K)+PRACIS(K))
      NS3DTEN(K) = NS3DTEN(K)+(NSAGG(K)+NPRCI(K)-NSCNG(K)-NGRACS(K)+NIACRS(K))
      QG3DTEN(K) = QG3DTEN(K)+(PRACG(K)+PSACWG(K)+PGSACW(K)+PGRACS(K)+ &
                    PRDG(K)+EPRDG(K)+MNUCCR(K)+PIACR(K)+PRACI(K)+PSACR(K))
      NG3DTEN(K) = NG3DTEN(K)+(NSCNG(K)+NGRACS(K)+NNUCCR(K)+NIACR(K))


      ELSE IF (IGRAUP.EQ.1) THEN

      QNI3DTEN(K) = QNI3DTEN(K)+                                    &
           (PRAI(K)+PSACWS(K)+PRDS(K)+PRACS(K)+PRCI(K)+EPRDS(K)-PSACR(K)+PIACRS(K)+PRACIS(K)+MNUCCR(K))
      NS3DTEN(K) = NS3DTEN(K)+(NSAGG(K)+NPRCI(K)-NSCNG(K)-NGRACS(K)+NIACRS(K)+NNUCCR(K))

      END IF

      NC3DTEN(K) = NC3DTEN(K)+(-NNUCCC(K)-NPSACWS(K)                &
            -NPRA(K)-NPRC(K)-NPSACWI(K)-NPSACWG(K))

      NI3DTEN(K) = NI3DTEN(K)+                                      &
       (NNUCCC(K)-NPRCI(K)-NPRAI(K)+NMULTS(K)+NMULTG(K)+NMULTR(K)+NMULTRG(K)+ &
               NNUCCD(K)-NIACR(K)-NIACRS(K))

      NR3DTEN(K) = NR3DTEN(K)+(NPRC1(K)-NPRACS(K)-NNUCCR(K)      &
                   +NRAGG(K)-NIACR(K)-NIACRS(K)-NPRACG(K)-NGRACS(K))



	C2PREC(K) = PRA(K)+PRC(K)+PSACWS(K)+QMULTS(K)+QMULTG(K)+PSACWG(K)+ &
       PGSACW(K)+MNUCCC(K)+PSACWI(K)




      DUMT = T3D(K)+DT*T3DTEN(K)
      DUMQV = QV3D(K)+DT*QV3DTEN(K)

      dum=min(0.99*pres(k),POLYSVP(DUMT,0))
      DUMQSS = EP_2*dum/(PRES(K)-dum)
      DUMQC = QC3D(K)+DT*QC3DTEN(K)
      DUMQC = MAX(DUMQC,0.)



      DUMS = DUMQV-DUMQSS
      PCC(K) = DUMS/(1.+XXLV(K)**2*DUMQSS/(CPM(K)*RV*DUMT**2))/DT
      IF (PCC(K)*DT+DUMQC.LT.0.) THEN
           PCC(K) = -DUMQC/DT
      END IF

      QV3DTEN(K) = QV3DTEN(K)-PCC(K)
      T3DTEN(K) = T3DTEN(K)+PCC(K)*XXLV(K)/CPM(K)
      QC3DTEN(K) = QC3DTEN(K)+PCC(K)

















      IF (EPRD(K).LT.0.) THEN
         DUM = EPRD(K)*DT/QI3D(K)
            DUM = MAX(-1.,DUM)
         NSUBI(K) = DUM*NI3D(K)/DT
      END IF
      IF (EPRDS(K).LT.0.) THEN
         DUM = EPRDS(K)*DT/QNI3D(K)
           DUM = MAX(-1.,DUM)
         NSUBS(K) = DUM*NS3D(K)/DT
      END IF
      IF (PRE(K).LT.0.) THEN
         DUM = PRE(K)*DT/QR3D(K)
           DUM = MAX(-1.,DUM)
         NSUBR(K) = DUM*NR3D(K)/DT
      END IF
      IF (EPRDG(K).LT.0.) THEN
         DUM = EPRDG(K)*DT/QG3D(K)
           DUM = MAX(-1.,DUM)
         NSUBG(K) = DUM*NG3D(K)/DT
      END IF








         NI3DTEN(K) = NI3DTEN(K)+NSUBI(K)
         NS3DTEN(K) = NS3DTEN(K)+NSUBS(K)
         NG3DTEN(K) = NG3DTEN(K)+NSUBG(K)
         NR3DTEN(K) = NR3DTEN(K)+NSUBR(K)

         END IF 


         LTRUE = 1

 200     CONTINUE

        END DO


      PRECRT = 0.
      SNOWRT = 0.

      SNOWPRT = 0.
      GRPLPRT = 0.



        IF (LTRUE.EQ.0) GOTO 400










      NSTEP = 1

      DO K = KTE,KTS,-1

        DUMI(K) = QI3D(K)+QI3DTEN(K)*DT
        DUMQS(K) = QNI3D(K)+QNI3DTEN(K)*DT
        DUMR(K) = QR3D(K)+QR3DTEN(K)*DT
        DUMFNI(K) = NI3D(K)+NI3DTEN(K)*DT
        DUMFNS(K) = NS3D(K)+NS3DTEN(K)*DT
        DUMFNR(K) = NR3D(K)+NR3DTEN(K)*DT
        DUMC(K) = QC3D(K)+QC3DTEN(K)*DT
        DUMFNC(K) = NC3D(K)+NC3DTEN(K)*DT
	DUMG(K) = QG3D(K)+QG3DTEN(K)*DT
	DUMFNG(K) = NG3D(K)+NG3DTEN(K)*DT


        IF (iinum.EQ.1) THEN
        DUMFNC(K) = NC3D(K)
        END IF




      DUMFNI(K) = MAX(0.,DUMFNI(K))
      DUMFNS(K) = MAX(0.,DUMFNS(K))
      DUMFNC(K) = MAX(0.,DUMFNC(K))
      DUMFNR(K) = MAX(0.,DUMFNR(K))
      DUMFNG(K) = MAX(0.,DUMFNG(K))




      IF (DUMI(K).GE.QSMALL) THEN
        DLAMI = (CONS12*DUMFNI(K)/DUMI(K))**(1./DI)
        DLAMI=MAX(DLAMI,LAMMINI)
        DLAMI=MIN(DLAMI,LAMMAXI)
      END IF



      IF (DUMR(K).GE.QSMALL) THEN
        DLAMR = (PI*RHOW*DUMFNR(K)/DUMR(K))**(1./3.)
        DLAMR=MAX(DLAMR,LAMMINR)
        DLAMR=MIN(DLAMR,LAMMAXR)
      END IF



      IF (DUMC(K).GE.QSMALL) THEN
         DUM = PRES(K)/(287.15*T3D(K))
         PGAM(K)=0.0005714*(NC3D(K)/1.E6*DUM)+0.2714
         PGAM(K)=1./(PGAM(K)**2)-1.
         PGAM(K)=MAX(PGAM(K),2.)
         PGAM(K)=MIN(PGAM(K),10.)

        DLAMC = (CONS26*DUMFNC(K)*GAMMA(PGAM(K)+4.)/(DUMC(K)*GAMMA(PGAM(K)+1.)))**(1./3.)
        LAMMIN = (PGAM(K)+1.)/60.E-6
        LAMMAX = (PGAM(K)+1.)/1.E-6
        DLAMC=MAX(DLAMC,LAMMIN)
        DLAMC=MIN(DLAMC,LAMMAX)
      END IF



      IF (DUMQS(K).GE.QSMALL) THEN
        DLAMS = (CONS1*DUMFNS(K)/ DUMQS(K))**(1./DS)
        DLAMS=MAX(DLAMS,LAMMINS)
        DLAMS=MIN(DLAMS,LAMMAXS)
      END IF



      IF (DUMG(K).GE.QSMALL) THEN
        DLAMG = (CONS2*DUMFNG(K)/ DUMG(K))**(1./DG)
        DLAMG=MAX(DLAMG,LAMMING)
        DLAMG=MIN(DLAMG,LAMMAXG)
      END IF






      IF (DUMC(K).GE.QSMALL) THEN
      UNC =  ACN(K)*GAMMA(1.+BC+PGAM(K))/ (DLAMC**BC*GAMMA(PGAM(K)+1.))
      UMC = ACN(K)*GAMMA(4.+BC+PGAM(K))/  (DLAMC**BC*GAMMA(PGAM(K)+4.))
      ELSE
      UMC = 0.
      UNC = 0.
      END IF

      IF (DUMI(K).GE.QSMALL) THEN
      UNI =  AIN(K)*CONS27/DLAMI**BI
      UMI = AIN(K)*CONS28/(DLAMI**BI)
      ELSE
      UMI = 0.
      UNI = 0.
      END IF

      IF (DUMR(K).GE.QSMALL) THEN
      UNR = ARN(K)*CONS6/DLAMR**BR
      UMR = ARN(K)*CONS4/(DLAMR**BR)
      ELSE
      UMR = 0.
      UNR = 0.
      END IF

      IF (DUMQS(K).GE.QSMALL) THEN
      UMS = ASN(K)*CONS3/(DLAMS**BS)
      UNS = ASN(K)*CONS5/DLAMS**BS
      ELSE
      UMS = 0.
      UNS = 0.
      END IF

      IF (DUMG(K).GE.QSMALL) THEN
      UMG = AGN(K)*CONS7/(DLAMG**BG)
      UNG = AGN(K)*CONS8/DLAMG**BG
      ELSE
      UMG = 0.
      UNG = 0.
      END IF




        dum=(rhosu/rho(k))**0.54
        UMS=MIN(UMS,1.2*dum)
        UNS=MIN(UNS,1.2*dum)


        UMI=MIN(UMI,1.2*(rhosu/rho(k))**0.35)
        UNI=MIN(UNI,1.2*(rhosu/rho(k))**0.35)
        UMR=MIN(UMR,9.1*dum)
        UNR=MIN(UNR,9.1*dum)
        UMG=MIN(UMG,20.*dum)
        UNG=MIN(UNG,20.*dum)

      FR(K) = UMR
      FI(K) = UMI
      FNI(K) = UNI
      FS(K) = UMS
      FNS(K) = UNS
      FNR(K) = UNR
      FC(K) = UMC
      FNC(K) = UNC
      FG(K) = UMG
      FNG(K) = UNG



	IF (K.LE.KTE-1) THEN
        IF (FR(K).LT.1.E-10) THEN
	FR(K)=FR(K+1)
	END IF
        IF (FI(K).LT.1.E-10) THEN
	FI(K)=FI(K+1)
	END IF
        IF (FNI(K).LT.1.E-10) THEN
	FNI(K)=FNI(K+1)
	END IF
        IF (FS(K).LT.1.E-10) THEN
	FS(K)=FS(K+1)
	END IF
        IF (FNS(K).LT.1.E-10) THEN
	FNS(K)=FNS(K+1)
	END IF
        IF (FNR(K).LT.1.E-10) THEN
	FNR(K)=FNR(K+1)
	END IF
        IF (FC(K).LT.1.E-10) THEN
	FC(K)=FC(K+1)
	END IF
        IF (FNC(K).LT.1.E-10) THEN
	FNC(K)=FNC(K+1)
	END IF
        IF (FG(K).LT.1.E-10) THEN
	FG(K)=FG(K+1)
	END IF
        IF (FNG(K).LT.1.E-10) THEN
	FNG(K)=FNG(K+1)
	END IF
	END IF 



      RGVM = MAX(FR(K),FI(K),FS(K),FC(K),FNI(K),FNR(K),FNS(K),FNC(K),FG(K),FNG(K))

      NSTEP = MAX(INT(RGVM*DT/DZQ(K)+1.),NSTEP)


      DUMR(k) = DUMR(k)*RHO(K)
      DUMI(k) = DUMI(k)*RHO(K)
      DUMFNI(k) = DUMFNI(K)*RHO(K)
      DUMQS(k) = DUMQS(K)*RHO(K)
      DUMFNS(k) = DUMFNS(K)*RHO(K)
      DUMFNR(k) = DUMFNR(K)*RHO(K)
      DUMC(k) = DUMC(K)*RHO(K)
      DUMFNC(k) = DUMFNC(K)*RHO(K)
      DUMG(k) = DUMG(K)*RHO(K)
      DUMFNG(k) = DUMFNG(K)*RHO(K)

      END DO

      DO N = 1,NSTEP

      DO K = KTS,KTE
      FALOUTR(K) = FR(K)*DUMR(K)
      FALOUTI(K) = FI(K)*DUMI(K)
      FALOUTNI(K) = FNI(K)*DUMFNI(K)
      FALOUTS(K) = FS(K)*DUMQS(K)
      FALOUTNS(K) = FNS(K)*DUMFNS(K)
      FALOUTNR(K) = FNR(K)*DUMFNR(K)
      FALOUTC(K) = FC(K)*DUMC(K)
      FALOUTNC(K) = FNC(K)*DUMFNC(K)
      FALOUTG(K) = FG(K)*DUMG(K)
      FALOUTNG(K) = FNG(K)*DUMFNG(K)
      END DO



      K = KTE
      FALTNDR = FALOUTR(K)/DZQ(k)
      FALTNDI = FALOUTI(K)/DZQ(k)
      FALTNDNI = FALOUTNI(K)/DZQ(k)
      FALTNDS = FALOUTS(K)/DZQ(k)
      FALTNDNS = FALOUTNS(K)/DZQ(k)
      FALTNDNR = FALOUTNR(K)/DZQ(k)
      FALTNDC = FALOUTC(K)/DZQ(k)
      FALTNDNC = FALOUTNC(K)/DZQ(k)
      FALTNDG = FALOUTG(K)/DZQ(k)
      FALTNDNG = FALOUTNG(K)/DZQ(k)


      QRSTEN(K) = QRSTEN(K)-FALTNDR/NSTEP/RHO(k)
      QISTEN(K) = QISTEN(K)-FALTNDI/NSTEP/RHO(k)
      NI3DTEN(K) = NI3DTEN(K)-FALTNDNI/NSTEP/RHO(k)
      QNISTEN(K) = QNISTEN(K)-FALTNDS/NSTEP/RHO(k)
      NS3DTEN(K) = NS3DTEN(K)-FALTNDNS/NSTEP/RHO(k)
      NR3DTEN(K) = NR3DTEN(K)-FALTNDNR/NSTEP/RHO(k)
      QCSTEN(K) = QCSTEN(K)-FALTNDC/NSTEP/RHO(k)
      NC3DTEN(K) = NC3DTEN(K)-FALTNDNC/NSTEP/RHO(k)
      QGSTEN(K) = QGSTEN(K)-FALTNDG/NSTEP/RHO(k)
      NG3DTEN(K) = NG3DTEN(K)-FALTNDNG/NSTEP/RHO(k)

      DUMR(K) = DUMR(K)-FALTNDR*DT/NSTEP
      DUMI(K) = DUMI(K)-FALTNDI*DT/NSTEP
      DUMFNI(K) = DUMFNI(K)-FALTNDNI*DT/NSTEP
      DUMQS(K) = DUMQS(K)-FALTNDS*DT/NSTEP
      DUMFNS(K) = DUMFNS(K)-FALTNDNS*DT/NSTEP
      DUMFNR(K) = DUMFNR(K)-FALTNDNR*DT/NSTEP
      DUMC(K) = DUMC(K)-FALTNDC*DT/NSTEP
      DUMFNC(K) = DUMFNC(K)-FALTNDNC*DT/NSTEP
      DUMG(K) = DUMG(K)-FALTNDG*DT/NSTEP
      DUMFNG(K) = DUMFNG(K)-FALTNDNG*DT/NSTEP

      DO K = KTE-1,KTS,-1
      FALTNDR = (FALOUTR(K+1)-FALOUTR(K))/DZQ(K)
      FALTNDI = (FALOUTI(K+1)-FALOUTI(K))/DZQ(K)
      FALTNDNI = (FALOUTNI(K+1)-FALOUTNI(K))/DZQ(K)
      FALTNDS = (FALOUTS(K+1)-FALOUTS(K))/DZQ(K)
      FALTNDNS = (FALOUTNS(K+1)-FALOUTNS(K))/DZQ(K)
      FALTNDNR = (FALOUTNR(K+1)-FALOUTNR(K))/DZQ(K)
      FALTNDC = (FALOUTC(K+1)-FALOUTC(K))/DZQ(K)
      FALTNDNC = (FALOUTNC(K+1)-FALOUTNC(K))/DZQ(K)
      FALTNDG = (FALOUTG(K+1)-FALOUTG(K))/DZQ(K)
      FALTNDNG = (FALOUTNG(K+1)-FALOUTNG(K))/DZQ(K)



      QRSTEN(K) = QRSTEN(K)+FALTNDR/NSTEP/RHO(k)
      QISTEN(K) = QISTEN(K)+FALTNDI/NSTEP/RHO(k)
      NI3DTEN(K) = NI3DTEN(K)+FALTNDNI/NSTEP/RHO(k)
      QNISTEN(K) = QNISTEN(K)+FALTNDS/NSTEP/RHO(k)
      NS3DTEN(K) = NS3DTEN(K)+FALTNDNS/NSTEP/RHO(k)
      NR3DTEN(K) = NR3DTEN(K)+FALTNDNR/NSTEP/RHO(k)
      QCSTEN(K) = QCSTEN(K)+FALTNDC/NSTEP/RHO(k)
      NC3DTEN(K) = NC3DTEN(K)+FALTNDNC/NSTEP/RHO(k)
      QGSTEN(K) = QGSTEN(K)+FALTNDG/NSTEP/RHO(k)
      NG3DTEN(K) = NG3DTEN(K)+FALTNDNG/NSTEP/RHO(k)

      DUMR(K) = DUMR(K)+FALTNDR*DT/NSTEP
      DUMI(K) = DUMI(K)+FALTNDI*DT/NSTEP
      DUMFNI(K) = DUMFNI(K)+FALTNDNI*DT/NSTEP
      DUMQS(K) = DUMQS(K)+FALTNDS*DT/NSTEP
      DUMFNS(K) = DUMFNS(K)+FALTNDNS*DT/NSTEP
      DUMFNR(K) = DUMFNR(K)+FALTNDNR*DT/NSTEP
      DUMC(K) = DUMC(K)+FALTNDC*DT/NSTEP
      DUMFNC(K) = DUMFNC(K)+FALTNDNC*DT/NSTEP
      DUMG(K) = DUMG(K)+FALTNDG*DT/NSTEP
      DUMFNG(K) = DUMFNG(K)+FALTNDNG*DT/NSTEP


	  CSED(K)=CSED(K)+FALOUTC(K)/NSTEP
	  ISED(K)=ISED(K)+FALOUTI(K)/NSTEP
	  SSED(K)=SSED(K)+FALOUTS(K)/NSTEP
	  GSED(K)=GSED(K)+FALOUTG(K)/NSTEP
	  RSED(K)=RSED(K)+FALOUTR(K)/NSTEP
      END DO





        PRECRT = PRECRT+(FALOUTR(KTS)+FALOUTC(KTS)+FALOUTS(KTS)+FALOUTI(KTS)+FALOUTG(KTS))  &
                     *DT/NSTEP
        SNOWRT = SNOWRT+(FALOUTS(KTS)+FALOUTI(KTS)+FALOUTG(KTS))*DT/NSTEP

        SNOWPRT = SNOWPRT+(FALOUTI(KTS)+FALOUTS(KTS))*DT/NSTEP
        GRPLPRT = GRPLPRT+(FALOUTG(KTS))*DT/NSTEP

      END DO

        DO K=KTS,KTE



        QR3DTEN(K)=QR3DTEN(K)+QRSTEN(K)
        QI3DTEN(K)=QI3DTEN(K)+QISTEN(K)
        QC3DTEN(K)=QC3DTEN(K)+QCSTEN(K)
        QG3DTEN(K)=QG3DTEN(K)+QGSTEN(K)
        QNI3DTEN(K)=QNI3DTEN(K)+QNISTEN(K)





        IF (QI3D(K).GE.QSMALL.AND.T3D(K).LT.273.15.AND.LAMI(K).GE.1.E-10) THEN
        IF (1./LAMI(K).GE.2.*DCS) THEN
           QNI3DTEN(K) = QNI3DTEN(K)+QI3D(K)/DT+ QI3DTEN(K)
           NS3DTEN(K) = NS3DTEN(K)+NI3D(K)/DT+   NI3DTEN(K)
           QI3DTEN(K) = -QI3D(K)/DT
           NI3DTEN(K) = -NI3D(K)/DT
        END IF
        END IF




          QC3D(k)        = QC3D(k)+QC3DTEN(k)*DT
          QI3D(k)        = QI3D(k)+QI3DTEN(k)*DT
          QNI3D(k)        = QNI3D(k)+QNI3DTEN(k)*DT
          QR3D(k)        = QR3D(k)+QR3DTEN(k)*DT
          NC3D(k)        = NC3D(k)+NC3DTEN(k)*DT
          NI3D(k)        = NI3D(k)+NI3DTEN(k)*DT
          NS3D(k)        = NS3D(k)+NS3DTEN(k)*DT
          NR3D(k)        = NR3D(k)+NR3DTEN(k)*DT

          IF (IGRAUP.EQ.0) THEN
          QG3D(k)        = QG3D(k)+QG3DTEN(k)*DT
          NG3D(k)        = NG3D(k)+NG3DTEN(k)*DT
          END IF


          T3D(K)         = T3D(K)+T3DTEN(k)*DT
          QV3D(K)        = QV3D(K)+QV3DTEN(k)*DT




            EVS(K) = min(0.99*pres(k),POLYSVP(T3D(K),0))   
            EIS(K) = min(0.99*pres(k),POLYSVP(T3D(K),1))   



            IF (EIS(K).GT.EVS(K)) EIS(K) = EVS(K)

            QVS(K) = EP_2*EVS(K)/(PRES(K)-EVS(K))
            QVI(K) = EP_2*EIS(K)/(PRES(K)-EIS(K))

            QVQVS(K) = QV3D(K)/QVS(K)
            QVQVSI(K) = QV3D(K)/QVI(K)




             IF (QVQVS(K).LT.0.9) THEN
               IF (QR3D(K).LT.1.E-8) THEN
                  QV3D(K)=QV3D(K)+QR3D(K)
                  T3D(K)=T3D(K)-QR3D(K)*XXLV(K)/CPM(K)
                  QR3D(K)=0.
               END IF
               IF (QC3D(K).LT.1.E-8) THEN
                  QV3D(K)=QV3D(K)+QC3D(K)
                  T3D(K)=T3D(K)-QC3D(K)*XXLV(K)/CPM(K)
                  QC3D(K)=0.
               END IF
             END IF

             IF (QVQVSI(K).LT.0.9) THEN
               IF (QI3D(K).LT.1.E-8) THEN
                  QV3D(K)=QV3D(K)+QI3D(K)
                  T3D(K)=T3D(K)-QI3D(K)*XXLS(K)/CPM(K)
                  QI3D(K)=0.
               END IF
               IF (QNI3D(K).LT.1.E-8) THEN
                  QV3D(K)=QV3D(K)+QNI3D(K)
                  T3D(K)=T3D(K)-QNI3D(K)*XXLS(K)/CPM(K)
                  QNI3D(K)=0.
               END IF
               IF (QG3D(K).LT.1.E-8) THEN
                  QV3D(K)=QV3D(K)+QG3D(K)
                  T3D(K)=T3D(K)-QG3D(K)*XXLS(K)/CPM(K)
                  QG3D(K)=0.
               END IF
             END IF




       IF (QC3D(K).LT.QSMALL) THEN
         QC3D(K) = 0.
         NC3D(K) = 0.
         EFFC(K) = 0.
       END IF
       IF (QR3D(K).LT.QSMALL) THEN
         QR3D(K) = 0.
         NR3D(K) = 0.
         EFFR(K) = 0.
       END IF
       IF (QI3D(K).LT.QSMALL) THEN
         QI3D(K) = 0.
         NI3D(K) = 0.
         EFFI(K) = 0.
       END IF
       IF (QNI3D(K).LT.QSMALL) THEN
         QNI3D(K) = 0.
         NS3D(K) = 0.
         EFFS(K) = 0.
       END IF
       IF (QG3D(K).LT.QSMALL) THEN
         QG3D(K) = 0.
         NG3D(K) = 0.
         EFFG(K) = 0.
       END IF




            IF (QC3D(K).LT.QSMALL.AND.QI3D(K).LT.QSMALL.AND.QNI3D(K).LT.QSMALL &
                 .AND.QR3D(K).LT.QSMALL.AND.QG3D(K).LT.QSMALL) GOTO 500






        IF (QI3D(K).GE.QSMALL.AND.T3D(K).GE.273.15) THEN
           QR3D(K) = QR3D(K)+QI3D(K)
           T3D(K) = T3D(K)-QI3D(K)*XLF(K)/CPM(K)
           QI3D(K) = 0.
           NR3D(K) = NR3D(K)+NI3D(K)
           NI3D(K) = 0.
        END IF


        IF (ILIQ.EQ.1) GOTO 778



        IF (T3D(K).LE.233.15.AND.QC3D(K).GE.QSMALL) THEN
           QI3D(K)=QI3D(K)+QC3D(K)
           T3D(K)=T3D(K)+QC3D(K)*XLF(K)/CPM(K)
           QC3D(K)=0.
           NI3D(K)=NI3D(K)+NC3D(K)
           NC3D(K)=0.
        END IF



        IF (IGRAUP.EQ.0) THEN

        IF (T3D(K).LE.233.15.AND.QR3D(K).GE.QSMALL) THEN
           QG3D(K) = QG3D(K)+QR3D(K)
           T3D(K) = T3D(K)+QR3D(K)*XLF(K)/CPM(K)
           QR3D(K) = 0.
           NG3D(K) = NG3D(K)+ NR3D(K)
           NR3D(K) = 0.
        END IF

        ELSE IF (IGRAUP.EQ.1) THEN

        IF (T3D(K).LE.233.15.AND.QR3D(K).GE.QSMALL) THEN
           QNI3D(K) = QNI3D(K)+QR3D(K)
           T3D(K) = T3D(K)+QR3D(K)*XLF(K)/CPM(K)
           QR3D(K) = 0.
           NS3D(K) = NS3D(K)+NR3D(K)
           NR3D(K) = 0.
        END IF

        END IF

 778    CONTINUE



      NI3D(K) = MAX(0.,NI3D(K))
      NS3D(K) = MAX(0.,NS3D(K))
      NC3D(K) = MAX(0.,NC3D(K))
      NR3D(K) = MAX(0.,NR3D(K))
      NG3D(K) = MAX(0.,NG3D(K))




      IF (QI3D(K).GE.QSMALL) THEN
         LAMI(K) = (CONS12*                 &
              NI3D(K)/QI3D(K))**(1./DI)





      IF (LAMI(K).LT.LAMMINI) THEN

      LAMI(K) = LAMMINI

      N0I(K) = LAMI(K)**4*QI3D(K)/CONS12

      NI3D(K) = N0I(K)/LAMI(K)
      ELSE IF (LAMI(K).GT.LAMMAXI) THEN
      LAMI(K) = LAMMAXI
      N0I(K) = LAMI(K)**4*QI3D(K)/CONS12

      NI3D(K) = N0I(K)/LAMI(K)
      END IF
      END IF




      IF (QR3D(K).GE.QSMALL) THEN
      LAMR(K) = (PI*RHOW*NR3D(K)/QR3D(K))**(1./3.)





      IF (LAMR(K).LT.LAMMINR) THEN

      LAMR(K) = LAMMINR

      N0RR(K) = LAMR(K)**4*QR3D(K)/(PI*RHOW)

      NR3D(K) = N0RR(K)/LAMR(K)
      ELSE IF (LAMR(K).GT.LAMMAXR) THEN
      LAMR(K) = LAMMAXR
      N0RR(K) = LAMR(K)**4*QR3D(K)/(PI*RHOW)

      NR3D(K) = N0RR(K)/LAMR(K)
      END IF

      END IF






      IF (QC3D(K).GE.QSMALL) THEN

         DUM = PRES(K)/(287.15*T3D(K))
         PGAM(K)=0.0005714*(NC3D(K)/1.E6*DUM)+0.2714
         PGAM(K)=1./(PGAM(K)**2)-1.
         PGAM(K)=MAX(PGAM(K),2.)
         PGAM(K)=MIN(PGAM(K),10.)



      LAMC(K) = (CONS26*NC3D(K)*GAMMA(PGAM(K)+4.)/   &
                 (QC3D(K)*GAMMA(PGAM(K)+1.)))**(1./3.)




      LAMMIN = (PGAM(K)+1.)/60.E-6
      LAMMAX = (PGAM(K)+1.)/1.E-6

      IF (LAMC(K).LT.LAMMIN) THEN
      LAMC(K) = LAMMIN
      NC3D(K) = EXP(3.*LOG(LAMC(K))+LOG(QC3D(K))+              &
                LOG(GAMMA(PGAM(K)+1.))-LOG(GAMMA(PGAM(K)+4.)))/CONS26

      ELSE IF (LAMC(K).GT.LAMMAX) THEN
      LAMC(K) = LAMMAX
      NC3D(K) = EXP(3.*LOG(LAMC(K))+LOG(QC3D(K))+              &
                LOG(GAMMA(PGAM(K)+1.))-LOG(GAMMA(PGAM(K)+4.)))/CONS26

      END IF

      END IF




      IF (QNI3D(K).GE.QSMALL) THEN
      LAMS(K) = (CONS1*NS3D(K)/QNI3D(K))**(1./DS)





      IF (LAMS(K).LT.LAMMINS) THEN
      LAMS(K) = LAMMINS
      N0S(K) = LAMS(K)**4*QNI3D(K)/CONS1

      NS3D(K) = N0S(K)/LAMS(K)

      ELSE IF (LAMS(K).GT.LAMMAXS) THEN

      LAMS(K) = LAMMAXS
      N0S(K) = LAMS(K)**4*QNI3D(K)/CONS1
      NS3D(K) = N0S(K)/LAMS(K)
      END IF

      END IF




      IF (QG3D(K).GE.QSMALL) THEN
      LAMG(K) = (CONS2*NG3D(K)/QG3D(K))**(1./DG)





      IF (LAMG(K).LT.LAMMING) THEN
      LAMG(K) = LAMMING
      N0G(K) = LAMG(K)**4*QG3D(K)/CONS2

      NG3D(K) = N0G(K)/LAMG(K)

      ELSE IF (LAMG(K).GT.LAMMAXG) THEN

      LAMG(K) = LAMMAXG
      N0G(K) = LAMG(K)**4*QG3D(K)/CONS2

      NG3D(K) = N0G(K)/LAMG(K)
      END IF

      END IF

 500  CONTINUE



      IF (QI3D(K).GE.QSMALL) THEN
         EFFI(K) = 3./LAMI(K)/2.*1.E6
      ELSE
         EFFI(K) = 25.
      END IF

      IF (QNI3D(K).GE.QSMALL) THEN
         EFFS(K) = 3./LAMS(K)/2.*1.E6
      ELSE
         EFFS(K) = 25.
      END IF

      IF (QR3D(K).GE.QSMALL) THEN
         EFFR(K) = 3./LAMR(K)/2.*1.E6
      ELSE
         EFFR(K) = 25.
      END IF

      IF (QC3D(K).GE.QSMALL) THEN
      EFFC(K) = GAMMA(PGAM(K)+4.)/                        &
             GAMMA(PGAM(K)+3.)/LAMC(K)/2.*1.E6
      ELSE
      EFFC(K) = 25.
      END IF

      IF (QG3D(K).GE.QSMALL) THEN
         EFFG(K) = 3./LAMG(K)/2.*1.E6
      ELSE
         EFFG(K) = 25.
      END IF








          NI3D(K) = MIN(NI3D(K),0.3E6/RHO(K))


          IF (iinum.EQ.0.AND.IACT.EQ.2) THEN
          NC3D(K) = MIN(NC3D(K),(NANEW1+NANEW2)/RHO(K))
          END IF

          IF (iinum.EQ.1) THEN 

             NC3D(K) = NDCNST*1.E6/RHO(K)
          END IF

      END DO 

 400         CONTINUE


      RETURN
      END SUBROUTINE MORR_TWO_MOMENT_MICRO



      REAL FUNCTION POLYSVP (T,TYPE)











      IMPLICIT NONE

      REAL DUM
      REAL T
      INTEGER TYPE

      real a0i,a1i,a2i,a3i,a4i,a5i,a6i,a7i,a8i 
      data a0i,a1i,a2i,a3i,a4i,a5i,a6i,a7i,a8i /&
	6.11147274, 0.503160820, 0.188439774e-1, &
        0.420895665e-3, 0.615021634e-5,0.602588177e-7, &
        0.385852041e-9, 0.146898966e-11, 0.252751365e-14/	


      real a0,a1,a2,a3,a4,a5,a6,a7,a8 


      data a0,a1,a2,a3,a4,a5,a6,a7,a8 /&
	6.11239921, 0.443987641, 0.142986287e-1, &
        0.264847430e-3, 0.302950461e-5, 0.206739458e-7, &
        0.640689451e-10,-0.952447341e-13,-0.976195544e-15/
      real dt



      IF (TYPE.EQ.1) THEN






      dt = max(-80.,t-273.16)
      polysvp = a0i + dt*(a1i+dt*(a2i+dt*(a3i+dt*(a4i+dt*(a5i+dt*(a6i+dt*(a7i+a8i*dt))))))) 
      polysvp = polysvp*100.

      END IF



      IF (TYPE.EQ.0) THEN

       dt = max(-80.,t-273.16)
       polysvp = a0 + dt*(a1+dt*(a2+dt*(a3+dt*(a4+dt*(a5+dt*(a6+dt*(a7+a8*dt)))))))
       polysvp = polysvp*100.







         END IF


      END FUNCTION POLYSVP



      REAL FUNCTION GAMMA(X)

























































































      implicit none
      INTEGER I,N
      LOGICAL PARITY
      REAL                                                          &
          CONV,EPS,FACT,HALF,ONE,RES,SUM,TWELVE,                    &
          TWO,X,XBIG,XDEN,XINF,XMININ,XNUM,Y,Y1,YSQ,Z,ZERO
      REAL, DIMENSION(7) :: C
      REAL, DIMENSION(8) :: P
      REAL, DIMENSION(8) :: Q



      DATA ONE,HALF,TWELVE,TWO,ZERO/1.0E0,0.5E0,12.0E0,2.0E0,0.0E0/





      DATA XBIG,XMININ,EPS/35.040E0,1.18E-38,1.19E-7/,XINF/3.4E38/




      DATA P/-1.71618513886549492533811E+0,2.47656508055759199108314E+1,  &
             -3.79804256470945635097577E+2,6.29331155312818442661052E+2,  &
             8.66966202790413211295064E+2,-3.14512729688483675254357E+4,  &
             -3.61444134186911729807069E+4,6.64561438202405440627855E+4/
      DATA Q/-3.08402300119738975254353E+1,3.15350626979604161529144E+2,  &
             -1.01515636749021914166146E+3,-3.10777167157231109440444E+3, &
              2.25381184209801510330112E+4,4.75584627752788110767815E+3,  &
            -1.34659959864969306392456E+5,-1.15132259675553483497211E+5/



      DATA C/-1.910444077728E-03,8.4171387781295E-04,                      &
           -5.952379913043012E-04,7.93650793500350248E-04,				   &
           -2.777777777777681622553E-03,8.333333333333333331554247E-02,	   &
            5.7083835261E-03/



      CONV(I) = REAL(I)
      PARITY=.FALSE.
      FACT=ONE
      N=0
      Y=X
      IF(Y.LE.ZERO)THEN



        Y=-X
        Y1=AINT(Y)
        RES=Y-Y1
        IF(RES.NE.ZERO)THEN
          IF(Y1.NE.AINT(Y1*HALF)*TWO)PARITY=.TRUE.
          FACT=-PI/SIN(PI*RES)
          Y=Y+ONE
        ELSE
          RES=XINF
          GOTO 900
        ENDIF
      ENDIF



      IF(Y.LT.EPS)THEN



        IF(Y.GE.XMININ)THEN
          RES=ONE/Y
        ELSE
          RES=XINF
          GOTO 900
        ENDIF
      ELSEIF(Y.LT.TWELVE)THEN
        Y1=Y
        IF(Y.LT.ONE)THEN



          Z=Y
          Y=Y+ONE
        ELSE



          N=INT(Y)-1
          Y=Y-CONV(N)
          Z=Y-ONE
        ENDIF



        XNUM=ZERO
        XDEN=ONE
        DO I=1,8
          XNUM=(XNUM+P(I))*Z
          XDEN=XDEN*Z+Q(I)
        END DO
        RES=XNUM/XDEN+ONE
        IF(Y1.LT.Y)THEN



          RES=RES/Y1
        ELSEIF(Y1.GT.Y)THEN



          DO I=1,N
            RES=RES*Y
            Y=Y+ONE
          END DO
        ENDIF
      ELSE



        IF(Y.LE.XBIG)THEN
          YSQ=Y*Y
          SUM=C(7)
          DO I=1,6
            SUM=SUM/YSQ+C(I)
          END DO
          SUM=SUM/Y-Y+SQRTPI
          SUM=SUM+(Y-HALF)*LOG(Y)
          RES=EXP(SUM)
        ELSE
          RES=XINF
          GOTO 900
        ENDIF
      ENDIF



      IF(PARITY)RES=-RES
      IF(FACT.NE.ONE)RES=FACT/RES
  900 GAMMA=RES
      RETURN

      END FUNCTION GAMMA


      REAL FUNCTION DERF1(X)
      IMPLICIT NONE
      REAL X
      REAL, DIMENSION(0 : 64) :: A, B
      REAL W,T,Y
      INTEGER K,I
      DATA A/                                                 &
         0.00000000005958930743E0, -0.00000000113739022964E0, &
         0.00000001466005199839E0, -0.00000016350354461960E0, &
         0.00000164610044809620E0, -0.00001492559551950604E0, &
         0.00012055331122299265E0, -0.00085483269811296660E0, &
         0.00522397762482322257E0, -0.02686617064507733420E0, &
         0.11283791670954881569E0, -0.37612638903183748117E0, &
         1.12837916709551257377E0,	                          &
         0.00000000002372510631E0, -0.00000000045493253732E0, &
         0.00000000590362766598E0, -0.00000006642090827576E0, &
         0.00000067595634268133E0, -0.00000621188515924000E0, &
         0.00005103883009709690E0, -0.00037015410692956173E0, &
         0.00233307631218880978E0, -0.01254988477182192210E0, &
         0.05657061146827041994E0, -0.21379664776456006580E0, &
         0.84270079294971486929E0,							  &
         0.00000000000949905026E0, -0.00000000018310229805E0, &
         0.00000000239463074000E0, -0.00000002721444369609E0, &
         0.00000028045522331686E0, -0.00000261830022482897E0, &
         0.00002195455056768781E0, -0.00016358986921372656E0, &
         0.00107052153564110318E0, -0.00608284718113590151E0, &
         0.02986978465246258244E0, -0.13055593046562267625E0, &
         0.67493323603965504676E0, 							  &
         0.00000000000382722073E0, -0.00000000007421598602E0, &
         0.00000000097930574080E0, -0.00000001126008898854E0, &
         0.00000011775134830784E0, -0.00000111992758382650E0, &
         0.00000962023443095201E0, -0.00007404402135070773E0, &
         0.00050689993654144881E0, -0.00307553051439272889E0, &
         0.01668977892553165586E0, -0.08548534594781312114E0, &
         0.56909076642393639985E0,							  &
         0.00000000000155296588E0, -0.00000000003032205868E0, &
         0.00000000040424830707E0, -0.00000000471135111493E0, &
         0.00000005011915876293E0, -0.00000048722516178974E0, &
         0.00000430683284629395E0, -0.00003445026145385764E0, &
         0.00024879276133931664E0, -0.00162940941748079288E0, &
         0.00988786373932350462E0, -0.05962426839442303805E0, &
         0.49766113250947636708E0 /
      DATA (B(I), I = 0, 12) /                                  &
         -0.00000000029734388465E0,  0.00000000269776334046E0, 	&
         -0.00000000640788827665E0, -0.00000001667820132100E0,  &
         -0.00000021854388148686E0,  0.00000266246030457984E0, 	&
          0.00001612722157047886E0, -0.00025616361025506629E0, 	&
          0.00015380842432375365E0,  0.00815533022524927908E0, 	&
         -0.01402283663896319337E0, -0.19746892495383021487E0,  &
          0.71511720328842845913E0 /
      DATA (B(I), I = 13, 25) /                                 &
         -0.00000000001951073787E0, -0.00000000032302692214E0,  &
          0.00000000522461866919E0,  0.00000000342940918551E0, 	&
         -0.00000035772874310272E0,  0.00000019999935792654E0, 	&
          0.00002687044575042908E0, -0.00011843240273775776E0, 	&
         -0.00080991728956032271E0,  0.00661062970502241174E0, 	&
          0.00909530922354827295E0, -0.20160072778491013140E0, 	&
          0.51169696718727644908E0 /
      DATA (B(I), I = 26, 38) /                                 &
         0.00000000003147682272E0, -0.00000000048465972408E0,   &
         0.00000000063675740242E0,  0.00000003377623323271E0, 	&
        -0.00000015451139637086E0, -0.00000203340624738438E0, 	&
         0.00001947204525295057E0,  0.00002854147231653228E0, 	&
        -0.00101565063152200272E0,  0.00271187003520095655E0, 	&
         0.02328095035422810727E0, -0.16725021123116877197E0, 	&
         0.32490054966649436974E0 /
      DATA (B(I), I = 39, 51) /                                 &
         0.00000000002319363370E0, -0.00000000006303206648E0,   &
        -0.00000000264888267434E0,  0.00000002050708040581E0, 	&
         0.00000011371857327578E0, -0.00000211211337219663E0, 	&
         0.00000368797328322935E0,  0.00009823686253424796E0, 	&
        -0.00065860243990455368E0, -0.00075285814895230877E0, 	&
         0.02585434424202960464E0, -0.11637092784486193258E0, 	&
         0.18267336775296612024E0 /
      DATA (B(I), I = 52, 64) /                                 &
        -0.00000000000367789363E0,  0.00000000020876046746E0, 	&
        -0.00000000193319027226E0, -0.00000000435953392472E0, 	&
         0.00000018006992266137E0, -0.00000078441223763969E0, 	&
        -0.00000675407647949153E0,  0.00008428418334440096E0, 	&
        -0.00017604388937031815E0, -0.00239729611435071610E0, 	&
         0.02064129023876022970E0, -0.06905562880005864105E0,   &
         0.09084526782065478489E0 /
      W = ABS(X)
      IF (W .LT. 2.2D0) THEN
          T = W * W
          K = INT(T)
          T = T - K
          K = K * 13
          Y = ((((((((((((A(K) * T + A(K + 1)) * T +              &
              A(K + 2)) * T + A(K + 3)) * T + A(K + 4)) * T +     &
              A(K + 5)) * T + A(K + 6)) * T + A(K + 7)) * T +     &
              A(K + 8)) * T + A(K + 9)) * T + A(K + 10)) * T + 	  &
              A(K + 11)) * T + A(K + 12)) * W
      ELSE IF (W .LT. 6.9D0) THEN
          K = INT(W)
          T = W - K
          K = 13 * (K - 2)
          Y = (((((((((((B(K) * T + B(K + 1)) * T +               &
              B(K + 2)) * T + B(K + 3)) * T + B(K + 4)) * T + 	  &
              B(K + 5)) * T + B(K + 6)) * T + B(K + 7)) * T + 	  &
              B(K + 8)) * T + B(K + 9)) * T + B(K + 10)) * T + 	  &
              B(K + 11)) * T + B(K + 12)
          Y = Y * Y
          Y = Y * Y
          Y = Y * Y
          Y = 1 - Y * Y
      ELSE
          Y = 1
      END IF
      IF (X .LT. 0) Y = -Y
      DERF1 = Y
      END FUNCTION DERF1



      subroutine refl10cm_hm (qv1d, qr1d, nr1d, qs1d, ns1d, qg1d, ng1d, &
                      t1d, p1d, dBZ, kts, kte, ii, jj)

      IMPLICIT NONE


      INTEGER, INTENT(IN):: kts, kte, ii, jj
      REAL, DIMENSION(kts:kte), INTENT(IN)::                            &
                      qv1d, qr1d, nr1d, qs1d, ns1d, qg1d, ng1d, t1d, p1d
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: dBZ


      REAL, DIMENSION(kts:kte):: temp, pres, qv, rho
      REAL, DIMENSION(kts:kte):: rr, nr, rs, ns, rg, ng

      DOUBLE PRECISION, DIMENSION(kts:kte):: ilamr, ilamg, ilams
      DOUBLE PRECISION, DIMENSION(kts:kte):: N0_r, N0_g, N0_s
      DOUBLE PRECISION:: lamr, lamg, lams
      LOGICAL, DIMENSION(kts:kte):: L_qr, L_qs, L_qg

      REAL, DIMENSION(kts:kte):: ze_rain, ze_snow, ze_graupel
      DOUBLE PRECISION:: fmelt_s, fmelt_g
      DOUBLE PRECISION:: cback, x, eta, f_d

      INTEGER:: i, k, k_0, kbot, n
      LOGICAL:: melti



      do k = kts, kte
         dBZ(k) = -35.0
      enddo




      do k = kts, kte
         temp(k) = t1d(k)
         qv(k) = MAX(1.E-10, qv1d(k))
         pres(k) = p1d(k)
         rho(k) = 0.622*pres(k)/(R*temp(k)*(qv(k)+0.622))

         if (qr1d(k) .gt. 1.E-9) then
            rr(k) = qr1d(k)*rho(k)
            nr(k) = nr1d(k)*rho(k)
            lamr = (xam_r*xcrg(3)*xorg2*nr(k)/rr(k))**xobmr
            ilamr(k) = 1./lamr
            N0_r(k) = nr(k)*xorg2*lamr**xcre(2)
            L_qr(k) = .true.
         else
            rr(k) = 1.E-12
            nr(k) = 1.E-12
            L_qr(k) = .false.
         endif

         if (qs1d(k) .gt. 1.E-9) then
            rs(k) = qs1d(k)*rho(k)
            ns(k) = ns1d(k)*rho(k)
            lams = (xam_s*xcsg(3)*xosg2*ns(k)/rs(k))**xobms
            ilams(k) = 1./lams
            N0_s(k) = ns(k)*xosg2*lams**xcse(2)
            L_qs(k) = .true.
         else
            rs(k) = 1.E-12
            ns(k) = 1.E-12
            L_qs(k) = .false.
         endif

         if (qg1d(k) .gt. 1.E-9) then
            rg(k) = qg1d(k)*rho(k)
            ng(k) = ng1d(k)*rho(k)
            lamg = (xam_g*xcgg(3)*xogg2*ng(k)/rg(k))**xobmg
            ilamg(k) = 1./lamg
            N0_g(k) = ng(k)*xogg2*lamg**xcge(2)
            L_qg(k) = .true.
         else
            rg(k) = 1.E-12
            ng(k) = 1.E-12
            L_qg(k) = .false.
         endif
      enddo




      melti = .false.
      k_0 = kts
      do k = kte-1, kts, -1
         if ( (temp(k).gt.273.15) .and. L_qr(k)                         &
                                  .and. (L_qs(k+1).or.L_qg(k+1)) ) then
            k_0 = MAX(k+1, k_0)
            melti=.true.
            goto 195
         endif
      enddo
 195  continue







      do k = kts, kte
         ze_rain(k) = 1.e-22
         ze_snow(k) = 1.e-22
         ze_graupel(k) = 1.e-22
         if (L_qr(k)) ze_rain(k) = N0_r(k)*xcrg(4)*ilamr(k)**xcre(4)
         if (L_qs(k)) ze_snow(k) = (0.176/0.93) * (6.0/PI)*(6.0/PI)     &
                                 * (xam_s/900.0)*(xam_s/900.0)          &
                                 * N0_s(k)*xcsg(4)*ilams(k)**xcse(4)
         if (L_qg(k)) ze_graupel(k) = (0.176/0.93) * (6.0/PI)*(6.0/PI)  &
                                    * (xam_g/900.0)*(xam_g/900.0)       &
                                    * N0_g(k)*xcgg(4)*ilamg(k)**xcge(4)
      enddo









      if (melti .and. k_0.ge.kts+1) then
       do k = k_0-1, kts, -1


          if (L_qs(k) .and. L_qs(k_0) ) then
           fmelt_s = MAX(0.005d0, MIN(1.0d0-rs(k)/rs(k_0), 0.99d0))
           eta = 0.d0
           lams = 1./ilams(k)
           do n = 1, nrbins
              x = xam_s * xxDs(n)**xbm_s
              call rayleigh_soak_wetgraupel (x,DBLE(xocms),DBLE(xobms), &
                    fmelt_s, melt_outside_s, m_w_0, m_i_0, lamda_radar, &
                    CBACK, mixingrulestring_s, matrixstring_s,          &
                    inclusionstring_s, hoststring_s,                    &
                    hostmatrixstring_s, hostinclusionstring_s)
              f_d = N0_s(k)*xxDs(n)**xmu_s * DEXP(-lams*xxDs(n))
              eta = eta + f_d * CBACK * simpson(n) * xdts(n)
           enddo
           ze_snow(k) = SNGL(lamda4 / (pi5 * K_w) * eta)
          endif




          if (L_qg(k) .and. L_qg(k_0) ) then
           fmelt_g = MAX(0.005d0, MIN(1.0d0-rg(k)/rg(k_0), 0.99d0))
           eta = 0.d0
           lamg = 1./ilamg(k)
           do n = 1, nrbins
              x = xam_g * xxDg(n)**xbm_g
              call rayleigh_soak_wetgraupel (x,DBLE(xocmg),DBLE(xobmg), &
                    fmelt_g, melt_outside_g, m_w_0, m_i_0, lamda_radar, &
                    CBACK, mixingrulestring_g, matrixstring_g,          &
                    inclusionstring_g, hoststring_g,                    &
                    hostmatrixstring_g, hostinclusionstring_g)
              f_d = N0_g(k)*xxDg(n)**xmu_g * DEXP(-lamg*xxDg(n))
              eta = eta + f_d * CBACK * simpson(n) * xdtg(n)
           enddo
           ze_graupel(k) = SNGL(lamda4 / (pi5 * K_w) * eta)
          endif

       enddo
      endif

      do k = kte, kts, -1
         dBZ(k) = 10.*log10((ze_rain(k)+ze_snow(k)+ze_graupel(k))*1.d18)
      enddo


      end subroutine refl10cm_hm



END MODULE module_mp_morr_two_moment


