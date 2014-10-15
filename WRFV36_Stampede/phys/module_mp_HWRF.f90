

MODULE module_mp_HWRF






      REAL,PRIVATE,SAVE ::  ABFR, CBFR, CIACW, CIACR, C_N0r0,           &
     &  CN0r0, CN0r_DMRmin, CN0r_DMRmax, CRACW, CRAUT, ESW0,            &
     &  RFmax, RQR_DR1, RQR_DR2, RQR_DR3, RQR_DRmin,                    &
     &  RQR_DRmax, RR_DRmin, RR_DR1, RR_DR2, RR_DR3, RR_DRmax

      INTEGER, PRIVATE,PARAMETER :: MY_T1=1, MY_T2=35
      REAL,PRIVATE,DIMENSION(MY_T1:MY_T2),SAVE :: MY_GROWTH

      REAL, PRIVATE,PARAMETER :: DMImin=.05e-3, DMImax=1.e-3,           &
     &      DelDMI=1.e-6,XMImin=1.e6*DMImin, XMIexp=.0536  
      INTEGER, PUBLIC,PARAMETER :: XMImax=1.e6*DMImax,                  &
     &                             MDImin=XMImin, MDImax=XMImax
      REAL, PRIVATE,DIMENSION(MDImin:MDImax) ::                         &
     &      ACCRI,SDENS,VSNOWI,VENTI1,VENTI2

      REAL, PRIVATE,PARAMETER :: DMRmin=.05e-3, DMRmax=.45e-3,          &
     &      DelDMR=1.e-6,XMRmin=1.e6*DMRmin, XMRmax=1.e6*DMRmax
      INTEGER, PRIVATE,PARAMETER :: MDRmin=XMRmin, MDRmax=XMRmax                   
      REAL, PRIVATE,DIMENSION(MDRmin:MDRmax)::                          &
     &      ACCRR,MASSR,RRATE,VRAIN,VENTR1,VENTR2

      INTEGER, PRIVATE,PARAMETER :: Nrime=40
      REAL, DIMENSION(2:9,0:Nrime),PRIVATE,SAVE :: VEL_RF

      INTEGER,PARAMETER :: NX=7501
      REAL, PARAMETER :: XMIN=180.0,XMAX=330.0
      REAL, DIMENSION(NX),PRIVATE,SAVE :: TBPVS,TBPVS0
      REAL, PRIVATE,SAVE :: C1XPVS0,C2XPVS0,C1XPVS,C2XPVS

      REAL, PRIVATE,PARAMETER ::                                        &

     &   CP=1004.6, EPSQ=1.E-12, GRAV=9.806, RHOL=1000., RD=287.04      &
     &  ,RV=461.5, T0C=273.15, XLS=2.834E6                              &

     &  ,EPS=RD/RV, EPS1=RV/RD-1., EPSQ1=1.001*EPSQ                     &
     &  ,RCP=1./CP, RCPRV=RCP/RV, RGRAV=1./GRAV, RRHOL=1./RHOL          &
     &  ,XLS1=XLS*RCP, XLS2=XLS*XLS*RCPRV, XLS3=XLS*XLS/RV              &


     &  ,CLIMIT=10.*EPSQ, CLIMIT1=-CLIMIT                               &
     &  ,C1=1./3.                                                       &
     &  ,DMR1=.1E-3, DMR2=.2E-3, DMR3=.32E-3                            &
     &  ,XMR1=1.e6*DMR1, XMR2=1.e6*DMR2, XMR3=1.e6*DMR3
      INTEGER, PARAMETER :: MDR1=XMR1, MDR2=XMR2, MDR3=XMR3
        REAL:: WC






















      REAL, PUBLIC,PARAMETER ::                                         &

     &  RHgrd_in=1.                                                     &  
     & ,RHgrd_out=0.975                                                 &  
     & ,P_RHgrd_out=850.E2                                              &  
     & ,T_ICE=-40.                                                      &  

     & ,T_ICEK=T0C+T_ICE                                                &
     & ,T_ICE_init=-5.                                                  &

     & ,NLImax=20.E3                                                     &
     & ,NLImin=1.E3                                                     &
     & ,N0r0=8.E6                                                       &
     & ,N0rmin=1.E4                                                     &


     & ,NCW=250.E6                                                       &  

     & ,FLARGE1=1.                                                      &
     & ,FLARGE2=.2

      LOGICAL, PARAMETER :: PRINT_err=.TRUE.    

      REAL,PUBLIC,SAVE ::  QAUT0
      REAL, PUBLIC,DIMENSION(MDImin:MDImax) :: MASSI


      CONTAINS



      SUBROUTINE ETAMP_NEW_HWRF (itimestep,DT,DX,DY,GID,RAINNC,RAINNCV,    & 
     &                      dz8w,rho_phy,p_phy,pi_phy,th_phy,qv,qt,   & 
     &                      LOWLYR,SR,                                &
     &                      F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,         &
     &                      QC,QR,QI,                                 &
     &                      ids,ide, jds,jde, kds,kde,                &
     &                      ims,ime, jms,jme, kms,kme,                &
     &                      its,ite, jts,jte, kts,kte                 )










      IMPLICIT NONE

      INTEGER, PARAMETER :: ITLO=-60, ITHI=40

      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,ITIMESTEP,GID  

      REAL, INTENT(IN) 	    :: DT,DX,DY
      REAL, INTENT(IN),     DIMENSION(ims:ime, kms:kme, jms:jme)::      &
     &                      dz8w,p_phy,pi_phy,rho_phy
      REAL, INTENT(INOUT),  DIMENSION(ims:ime, kms:kme, jms:jme)::      &
     &                      th_phy,qv,qt,qc,qr,qi
      REAL, INTENT(INOUT),  DIMENSION(ims:ime, kms:kme, jms:jme ) ::    &
     &                      F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
      REAL, INTENT(INOUT),  DIMENSION(ims:ime,jms:jme)           ::     &
     &                                                   RAINNC,RAINNCV
      REAL, INTENT(OUT),    DIMENSION(ims:ime,jms:jme):: SR





      INTEGER, DIMENSION( ims:ime, jms:jme ),INTENT(INOUT) :: LOWLYR







      INTEGER,DIMENSION(ITLO:ITHI,4) :: NSTATS
      REAL,   DIMENSION(ITLO:ITHI,5) :: QMAX
      REAL,   DIMENSION(ITLO:ITHI,22):: QTOT









      REAL,  DIMENSION( ims:ime, kms:kme, jms:jme ) ::                  &
     &       TLATGS_PHY,TRAIN_PHY
      REAL,  DIMENSION(ims:ime,jms:jme):: APREC,PREC,ACPREC
      REAL,  DIMENSION(its:ite, kts:kte, jts:jte):: t_phy

      INTEGER :: I,J,K,KFLIP



















      DO j = jts,jte
      DO k = kts,kte
      DO i = its,ite
        t_phy(i,k,j) = th_phy(i,k,j)*pi_phy(i,k,j)
        qv(i,k,j)=qv(i,k,j)/(1.+qv(i,k,j)) 
      ENDDO
      ENDDO
      ENDDO




      DO k = 1,4
      DO i = ITLO,ITHI
         NSTATS(i,k)=0. 
      ENDDO
      ENDDO

      DO k = 1,5
      DO i = ITLO,ITHI
         QMAX(i,k)=0.
      ENDDO
      ENDDO

      DO k = 1,22
      DO i = ITLO,ITHI
         QTOT(i,k)=0.
      ENDDO
      ENDDO



      DO j = jts,jte
      DO k = kts,kte
      DO i = its,ite
	 TLATGS_PHY (i,k,j)=0.
	 TRAIN_PHY  (i,k,j)=0.
      ENDDO
      ENDDO
      ENDDO

      DO j = jts,jte
      DO i = its,ite
         ACPREC(i,j)=0.
         APREC (i,j)=0.
         PREC  (i,j)=0.
         SR    (i,j)=0.
      ENDDO
      ENDDO


      DO j = jts,jte
      DO k = kts,kte
      DO i = its,ite
         QT(I,K,J)=QC(I,K,J)+QR(I,K,J)+QI(I,K,J)
         IF (QI(I,K,J) <= EPSQ) THEN
            F_ICE_PHY(I,K,J)=0.
            IF (T_PHY(I,K,J) < T_ICEK) F_ICE_PHY(I,K,J)=1.
         ELSE
            F_ICE_PHY(I,K,J)=MAX( 0., MIN(1., QI(I,K,J)/QT(I,K,J) ) )
         ENDIF
         IF (QR(I,K,J) <= EPSQ) THEN
            F_RAIN_PHY(I,K,J)=0.
         ELSE
            F_RAIN_PHY(I,K,J)=QR(I,K,J)/(QR(I,K,J)+QC(I,K,J))
         ENDIF
      ENDDO
      ENDDO
      ENDDO



      CALL EGCP01DRV(GID,DT,LOWLYR,                                     &
     &               APREC,PREC,ACPREC,SR,NSTATS,QMAX,QTOT,	        &
     &               dz8w,rho_phy,qt,t_phy,qv,F_ICE_PHY,P_PHY,          &
     &               F_RAIN_PHY,F_RIMEF_PHY,TLATGS_PHY,TRAIN_PHY,       &
     &               ids,ide, jds,jde, kds,kde,		                &
     &               ims,ime, jms,jme, kms,kme,		                &
     &               its,ite, jts,jte, kts,kte		          )


     DO j = jts,jte
        DO k = kts,kte
	DO i = its,ite
  	   th_phy(i,k,j) = t_phy(i,k,j)/pi_phy(i,k,j)
           qv(i,k,j)=qv(i,k,j)/(1.-qv(i,k,j))  
           WC=qt(I,K,J)
           QI(I,K,J)=0.
           QR(I,K,J)=0.
           QC(I,K,J)=0.
           IF(F_ICE_PHY(I,K,J)>=1.)THEN
             QI(I,K,J)=WC
           ELSEIF(F_ICE_PHY(I,K,J)<=0.)THEN
             QC(I,K,J)=WC
           ELSE
             QI(I,K,J)=F_ICE_PHY(I,K,J)*WC
             QC(I,K,J)=WC-QI(I,K,J)
           ENDIF

           IF(QC(I,K,J)>0..AND.F_RAIN_PHY(I,K,J)>0.)THEN
             IF(F_RAIN_PHY(I,K,J).GE.1.)THEN
               QR(I,K,J)=QC(I,K,J)
               QC(I,K,J)=0.
             ELSE
               QR(I,K,J)=F_RAIN_PHY(I,K,J)*QC(I,K,J)
               QC(I,K,J)=QC(I,K,J)-QR(I,K,J)
             ENDIF
          endif
	ENDDO
        ENDDO
     ENDDO



       DO j=jts,jte
       DO i=its,ite
          RAINNC(i,j)=APREC(i,j)*1000.+RAINNC(i,j)
          RAINNCV(i,j)=APREC(i,j)*1000.
       ENDDO
       ENDDO
















  END SUBROUTINE ETAMP_NEW_HWRF



      SUBROUTINE EGCP01DRV(GID,                            & 
     &  DTPH,LOWLYR,APREC,PREC,ACPREC,SR,                  &
     &  NSTATS,QMAX,QTOT,                                  &
     &  dz8w,RHO_PHY,CWM_PHY,T_PHY,Q_PHY,F_ICE_PHY,P_PHY,  &
     &  F_RAIN_PHY,F_RIMEF_PHY,TLATGS_PHY,TRAIN_PHY,       &
     &  ids,ide, jds,jde, kds,kde,                         &
     &  ims,ime, jms,jme, kms,kme,                         &
     &  its,ite, jts,jte, kts,kte)





















      IMPLICIT NONE


      INTEGER, PARAMETER :: ITLO=-60, ITHI=40

      INTEGER,INTENT(IN ) :: ids,ide, jds,jde, kds,kde                  &
     &                      ,ims,ime, jms,jme, kms,kme                  &
     &                      ,its,ite, jts,jte, kts,kte
      INTEGER,INTENT(IN ) :: GID     
      REAL,INTENT(IN) :: DTPH
      INTEGER, DIMENSION( ims:ime, jms:jme ),INTENT(INOUT) :: LOWLYR
      INTEGER,DIMENSION(ITLO:ITHI,4),INTENT(INOUT) :: NSTATS
      REAL,DIMENSION(ITLO:ITHI,5),INTENT(INOUT) :: QMAX
      REAL,DIMENSION(ITLO:ITHI,22),INTENT(INOUT) :: QTOT
      REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT) ::                  &
     &                         APREC,PREC,ACPREC,SR
      REAL,DIMENSION( its:ite, kts:kte, jts:jte ),INTENT(INOUT) :: t_phy
      REAL,DIMENSION( ims:ime, kms:kme, jms:jme ),INTENT(IN) ::         &
     &                                             dz8w,P_PHY,RHO_PHY
      REAL,DIMENSION( ims:ime, kms:kme, jms:jme ),INTENT(INOUT) ::      &
     &   CWM_PHY, F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,TLATGS_PHY           &
     &   ,Q_PHY,TRAIN_PHY

















      INTEGER :: LSFC,I,J,I_index,J_index,L,K,KFLIP

      REAL,DIMENSION(its:ite,jts:jte,kts:kte) ::                        &
     &   CWM,T,Q,TRAIN,TLATGS,P
      REAL,DIMENSION(kts:kte,its:ite,jts:jte) :: F_ice,F_rain,F_RimeF       
      INTEGER,DIMENSION(its:ite,jts:jte) :: LMH
      REAL :: TC,WC,QI,QR,QW,Fice,Frain,DUM,ASNOW,ARAIN
      REAL,DIMENSION(kts:kte) :: P_col,Q_col,T_col,QV_col,WC_col,       &
         RimeF_col,QI_col,QR_col,QW_col, THICK_col, RHC_col, DPCOL    
      REAL,DIMENSION(2) :: PRECtot,PRECmax


        DO J=JTS,JTE    
        DO I=ITS,ITE  
           LMH(I,J) = KTE-LOWLYR(I,J)+1
        ENDDO
        ENDDO


        DO 98  J=JTS,JTE    
        DO 98  I=ITS,ITE  
           DO L=KTS,KTE
             KFLIP=KTE+1-L
             CWM(I,J,L)=CWM_PHY(I,KFLIP,J)
             T(I,J,L)=T_PHY(I,KFLIP,J)
             Q(I,J,L)=Q_PHY(I,KFLIP,J)
             P(I,J,L)=P_PHY(I,KFLIP,J)
             TLATGS(I,J,L)=TLATGS_PHY(I,KFLIP,J)
             TRAIN(I,J,L)=TRAIN_PHY(I,KFLIP,J)
             F_ice(L,I,J)=F_ice_PHY(I,KFLIP,J)
             F_rain(L,I,J)=F_rain_PHY(I,KFLIP,J)
             F_RimeF(L,I,J)=F_RimeF_PHY(I,KFLIP,J)
           ENDDO
98      CONTINUE
     
       DO 100 J=JTS,JTE    
        DO 100 I=ITS,ITE  
          LSFC=LMH(I,J)                      

          DO K=KTS,KTE
            KFLIP=KTE+1-K
            DPCOL(K)=RHO_PHY(I,KFLIP,J)*GRAV*dz8w(I,KFLIP,J)
          ENDDO

   
   
   
          IF (CWM(I,J,1) .LE. EPSQ) CWM(I,J,1)=EPSQ
          F_ice(1,I,J)=1.
          F_rain(1,I,J)=0.
          F_RimeF(1,I,J)=1.
          DO L=1,LSFC
      
      
      
            P_col(L)=P(I,J,L)
      
      
      
            THICK_col(L)=DPCOL(L)*RGRAV
            T_col(L)=T(I,J,L)
            TC=T_col(L)-T0C
            QV_col(L)=max(EPSQ, Q(I,J,L))
            IF (CWM(I,J,L) .LE. EPSQ1) THEN
              WC_col(L)=0.
              IF (TC .LT. T_ICE) THEN
                F_ice(L,I,J)=1.
              ELSE
                F_ice(L,I,J)=0.
              ENDIF
              F_rain(L,I,J)=0.
              F_RimeF(L,I,J)=1.
            ELSE
              WC_col(L)=CWM(I,J,L)
            ENDIF
      
      
      
      
            WC=WC_col(L)
            QI=0.
            QR=0.
            QW=0.
            Fice=F_ice(L,I,J)
            Frain=F_rain(L,I,J)
            IF (Fice .GE. 1.) THEN
              QI=WC
            ELSE IF (Fice .LE. 0.) THEN
              QW=WC
            ELSE
              QI=Fice*WC
              QW=WC-QI
            ENDIF
            IF (QW.GT.0. .AND. Frain.GT.0.) THEN
              IF (Frain .GE. 1.) THEN
                QR=QW
                QW=0.
              ELSE
                QR=Frain*QW
                QW=QW-QR
              ENDIF
            ENDIF
            RimeF_col(L)=F_RimeF(L,I,J)
            QI_col(L)=QI
            QR_col(L)=QR
            QW_col(L)=QW




            IF(GID .EQ. 1 .AND. P_col(L)<P_RHgrd_out) THEN  
              RHC_col(L)=RHgrd_out        
            ELSE
              RHC_col(L)=RHgrd_in       
            ENDIF

          ENDDO


   
   
   
          I_index=I
          J_index=J
       CALL EGCP01COLUMN ( ARAIN, ASNOW, DTPH, I_index, J_index, LSFC,  &
     & P_col, QI_col, QR_col, QV_col, QW_col, RimeF_col, T_col,         &
     & THICK_col, WC_col, RHC_col, KTS,KTE,NSTATS,QMAX,QTOT )   


   


   
   
   
          DO L=1,LSFC
            TRAIN(I,J,L)=(T_col(L)-T(I,J,L))/DTPH
            TLATGS(I,J,L)=T_col(L)-T(I,J,L)
            T(I,J,L)=T_col(L)
            Q(I,J,L)=QV_col(L)
            CWM(I,J,L)=WC_col(L)
      
      
      
            F_RimeF(L,I,J)=MAX(1., RimeF_col(L))
            IF (QI_col(L) .LE. EPSQ) THEN
              F_ice(L,I,J)=0.
              IF (T_col(L) .LT. T_ICEK) F_ice(L,I,J)=1.
            ELSE
              F_ice(L,I,J)=MAX( 0., MIN(1., QI_col(L)/WC_col(L)) )
            ENDIF
            IF (QR_col(L) .LE. EPSQ) THEN
              DUM=0
            ELSE
              DUM=QR_col(L)/(QR_col(L)+QW_col(L))
            ENDIF
            F_rain(L,I,J)=DUM
      
          ENDDO
   
   
   
   
   
   
        APREC(I,J)=(ARAIN+ASNOW)*RRHOL       
        PREC(I,J)=PREC(I,J)+APREC(I,J)
        ACPREC(I,J)=ACPREC(I,J)+APREC(I,J)
        IF(APREC(I,J) .LT. 1.E-8) THEN
          SR(I,J)=0.
        ELSE
          SR(I,J)=RRHOL*ASNOW/APREC(I,J)
        ENDIF














100   CONTINUE                          
        DO 101 J=JTS,JTE    
        DO 101 I=ITS,ITE  
           DO L=KTS,KTE
              KFLIP=KTE+1-L
             CWM_PHY(I,KFLIP,J)=CWM(I,J,L)
             T_PHY(I,KFLIP,J)=T(I,J,L)
             Q_PHY(I,KFLIP,J)=Q(I,J,L)
             TLATGS_PHY(I,KFLIP,J)=TLATGS(I,J,L)
             TRAIN_PHY(I,KFLIP,J)=TRAIN(I,J,L)
             F_ice_PHY(I,KFLIP,J)=F_ice(L,I,J)
             F_rain_PHY(I,KFLIP,J)=F_rain(L,I,J)
             F_RimeF_PHY(I,KFLIP,J)=F_RimeF(L,I,J)
           ENDDO
101     CONTINUE
      END SUBROUTINE EGCP01DRV





















































      SUBROUTINE EGCP01COLUMN ( ARAIN, ASNOW, DTPH, I_index, J_index,   &
     & LSFC, P_col, QI_col, QR_col, QV_col, QW_col, RimeF_col, T_col,   &
     & THICK_col, WC_col, RHC_col, KTS,KTE,NSTATS,QMAX,QTOT)   


























































































      IMPLICIT NONE

      INTEGER,INTENT(IN) :: KTS,KTE,I_index, J_index, LSFC
      REAL,INTENT(INOUT) ::  ARAIN, ASNOW
      REAL,DIMENSION(KTS:KTE),INTENT(INOUT) ::  P_col, QI_col,QR_col    &
     & ,QV_col ,QW_col, RimeF_col, T_col, THICK_col, WC_col, RHC_col   








      INTEGER, PARAMETER :: ITLO=-60, ITHI=40
      INTEGER,INTENT(INOUT) :: NSTATS(ITLO:ITHI,4)
      REAL,INTENT(INOUT) :: QMAX(ITLO:ITHI,5),QTOT(ITLO:ITHI,22) 












































      REAL, PARAMETER :: TOLER=5.E-7, C2=1./6., RHO0=1.194, Xratio=.025                                           











      REAL, PARAMETER :: BLEND=1.





      REAL EMAIRI, N0r, NLICE, NSmICE, RHgrd   
      LOGICAL CLEAR, ICE_logical, DBG_logical, RAIN_logical
      INTEGER :: IDR,INDEX_MY,INDEXR,INDEXR1,INDEXS,IPASS,ITDX,IXRF,    &
     &           IXS,LBEF,L

      REAL :: ABI,ABW,AIEVP,ARAINnew,ASNOWnew,BLDTRH,BUDGET,            &
     &        CREVP,DELI,DELR,DELT,DELV,DELW,DENOMF,                    &
     &        DENOMI,DENOMW,DENOMWI,DIDEP,                              &
     &        DIEVP,DIFFUS,DLI,DTPH,DTRHO,DUM,DUM1,                     &
     &        DUM2,DWV0,DWVI,DWVR,DYNVIS,ESI,ESW,FIR,FLARGE,FLIMASS,    &
     &        FSMALL,FWR,FWS,GAMMAR,GAMMAS,                             &
     &        PCOND,PIACR,PIACW,PIACWI,PIACWR,PICND,PIDEP,PIDEP_max,    &
     &        PIEVP,PILOSS,PIMLT,PP,PRACW,PRAUT,PREVP,PRLOSS,           &
     &        QI,QInew,QLICE,QR,QRnew,QSI,QSIgrd,QSInew,QSW,QSW0,       &
     &        QSWgrd,QSWnew,QT,QTICE,QTnew,QTRAIN,QV,QW,QW0,QWnew,      &
     &        RFACTOR,RHO,RIMEF,RIMEF1,RQR,RR,RRHO,SFACTOR,             &
     &        TC,TCC,TFACTOR,THERM_COND,THICK,TK,TK2,TNEW,              &
     &        TOT_ICE,TOT_ICEnew,TOT_RAIN,TOT_RAINnew,                  &
     &        VEL_INC,VENTR,VENTIL,VENTIS,VRAIN1,VRAIN2,VRIMEF,VSNOW,   &
     &        WC,WCnew,WSgrd,WS,WSnew,WV,WVnew,WVQW,                    &

     &        XLF,XLF1,XLI,XLV,XLV1,XLV2,XLIMASS,XRF,XSIMASS,           &
     &        VRabove     






      ARAIN=0.                
      ASNOW=0.                

      VRabove=0.              







      DO 10 L=1,LSFC




        IF (QV_col(L).LE.EPSQ .AND. WC_col(L).LE.EPSQ) GO TO 10





          TK=T_col(L)         
          TC=TK-T0C           
          PP=P_col(L)         
          QV=QV_col(L)        
          WV=QV/(1.-QV)       
          WC=WC_col(L)        
          RHgrd=RHC_col(L)    





          CLEAR=.TRUE.



          ESW=1000.*FPVS0(TK)              
          QSW=EPS*ESW/(PP-ESW)             
          QSI = QSW                        
          WS=QSW                           
          IF (TC .LT. 0.) THEN
            ESI=1000.*FPVS(TK)             
            QSI=EPS*ESI/(PP-ESI)           
            WS=QSI                         
          ENDIF



          QSWgrd=RHgrd*QSW
          QSIgrd=RHgrd*QSI
          WSgrd=RHgrd*WS



          IF (WV.GT.WSgrd .OR. WC.GT.EPSQ) CLEAR=.FALSE.



          IF (ARAIN .GT. CLIMIT) THEN
            CLEAR=.FALSE.
          ELSE
            ARAIN=0.
 
            VRabove=0.
 
          ENDIF






          IF (ASNOW .GT. CLIMIT) THEN
            CLEAR=.FALSE.
          ELSE
            ASNOW=0.
          ENDIF





          IF (CLEAR) GO TO 10









          RHO=PP/(RD*TK*(1.+EPS1*QV))   
          RRHO=1./RHO                
          DTRHO=DTPH*RHO             
          BLDTRH=BLEND*DTRHO         
          THICK=THICK_col(L)         

          ARAINnew=0.                
          ASNOWnew=0.                
          QI=QI_col(L)               
          QInew=0.                   
          QR=QR_col(L)               
          QRnew=0.                   
          QW=QW_col(L)               
          QWnew=0.                   

          PCOND=0.                   
          PIDEP=0.                   
          PIACW=0.                   
          PIACWI=0.                  
          PIACWR=0.                  
          PIACR=0.                   
          PICND=0.                   
          PIEVP=0.                   
          PIMLT=0.                   
          PRAUT=0.                   
          PRACW=0.                   
          PREVP=0.                   























          XLV=3.148E6-2370*TK        
          XLF=XLS-XLV                
          XLV1=XLV*RCP               
          XLF1=XLF*RCP               
          TK2=1./(TK*TK)             

          XLV2=XLV*XLV*QSWgrd*TK2/RV    
          DENOMW=1.+XLV2*RCP         






          TFACTOR=TK**1.5/(TK+120.)
          DYNVIS=1.496E-6*TFACTOR
          THERM_COND=2.116E-3*TFACTOR
          DIFFUS=8.794E-5*TK**1.81/PP




          GAMMAS=(1.E5/PP)**C1



          GAMMAR=(RHO0/RHO)**.4







          IF (TC.LT.0. .OR. QI.GT.EPSQ .OR. ASNOW.GT.CLIMIT) THEN
            ICE_logical=.TRUE.
          ELSE
            ICE_logical=.FALSE.
            QLICE=0.
            QTICE=0.
          ENDIF



          RAIN_logical=.FALSE.
          IF (ARAIN.GT.CLIMIT .OR. QR.GT.EPSQ) RAIN_logical=.TRUE.

          IF (ICE_logical) THEN






























            WVQW=WV+QW                



            IF (TC.GE.0. .OR. WVQW.LT.QSIgrd) THEN
   
   
   
              FLARGE=FLARGE1
            ELSE
   
   
   
   
              FLARGE=FLARGE2
   
   
   
              IF (TC.GE.-8. .AND. TC.LE.-3.) FLARGE=.5*FLARGE

            ENDIF            

            FSMALL=(1.-FLARGE)/FLARGE
            XSIMASS=RRHO*MASSI(MDImin)*FSMALL
            IF (QI.LE.EPSQ .AND. ASNOW.LE.CLIMIT) THEN
              INDEXS=MDImin
              TOT_ICE=0.
              PILOSS=0.
              RimeF1=1.
              VrimeF=1.
              VEL_INC=GAMMAS
              VSNOW=0.
              EMAIRI=THICK
              XLIMASS=RRHO*RimeF1*MASSI(INDEXS)
              FLIMASS=XLIMASS/(XLIMASS+XSIMASS)
              QLICE=0.
              QTICE=0.
              NLICE=0.
              NSmICE=0.
            ELSE
   
   
   
   
   
              DUM=XMImax*EXP(.0536*TC)
              INDEXS=MIN(MDImax, MAX(MDImin, INT(DUM) ) )
              TOT_ICE=THICK*QI+BLEND*ASNOW
              PILOSS=-TOT_ICE/THICK
              LBEF=MAX(1,L-1)
              DUM1=RimeF_col(LBEF)
              DUM2=RimeF_col(L)
              RimeF1=(DUM2*THICK*QI+DUM1*BLEND*ASNOW)/TOT_ICE
              RimeF1=MIN(RimeF1, RFmax)
              DO IPASS=0,1
                IF (RimeF1 .LE. 1.) THEN
                  RimeF1=1.
                  VrimeF=1.
                ELSE
                  IXS=MAX(2, MIN(INDEXS/100, 9))
                  XRF=10.492*ALOG(RimeF1)
                  IXRF=MAX(0, MIN(INT(XRF), Nrime))
                  IF (IXRF .GE. Nrime) THEN
                    VrimeF=VEL_RF(IXS,Nrime)
                  ELSE
                    VrimeF=VEL_RF(IXS,IXRF)+(XRF-FLOAT(IXRF))*          &
     &                    (VEL_RF(IXS,IXRF+1)-VEL_RF(IXS,IXRF))
                  ENDIF
                ENDIF            
                VEL_INC=GAMMAS*VrimeF
                VSNOW=VEL_INC*VSNOWI(INDEXS)


                IF (TC>0.) THEN


                   DUM=MAX(VSNOW,VRabove)
                   VEL_INC=DUM/VSNOWI(INDEXS)
                   VSNOW=DUM
                ENDIF

 

                EMAIRI=THICK+BLDTRH*VSNOW
                XLIMASS=RRHO*RimeF1*MASSI(INDEXS)
                FLIMASS=XLIMASS/(XLIMASS+XSIMASS)
                QTICE=TOT_ICE/EMAIRI
                QLICE=FLIMASS*QTICE
                NLICE=QLICE/XLIMASS
                NSmICE=Fsmall*NLICE
   
                IF ( (NLICE.GE.NLImin .AND. NLICE.LE.NLImax)            &
     &                .OR. IPASS.EQ.1) THEN
                  EXIT
                ELSE
        
        
        
        
        
        
                  DUM=MAX(NLImin, MIN(NLImax, NLICE) )
            


                  XLI=RHO*QLICE/(DUM*RimeF1)   
                  IF (XLI .LE. MASSI(MDImin) ) THEN
                    INDEXS=MDImin
                  ELSE IF (XLI .LE. MASSI(450) ) THEN
                    DLI=9.5885E5*XLI**.42066         
                    INDEXS=MIN(MDImax, MAX(MDImin, INT(DLI) ) )
                  ELSE IF (XLI .LE. MASSI(MDImax) ) THEN
                    DLI=3.9751E6*XLI**.49870         
                    INDEXS=MIN(MDImax, MAX(MDImin, INT(DLI) ) )
                  ELSE 
                    INDEXS=MDImax
           
           
           
           
           
                    IF (DUM .GE. NLImax)                                &


     &                   RimeF1=RHO*QLICE/(NLImax*MASSI(INDEXS))    
                  ENDIF             




                ENDIF                  
              ENDDO                    
            ENDIF                      
          ENDIF                        







          IF (QW.GT.EPSQ .AND. TC.GE.T_ICE) THEN
   
   
   
   
   
   
            QW0=QAUT0*RRHO
            PRAUT=MAX(0., QW-QW0)*CRAUT
            IF (QLICE .GT. EPSQ) THEN
      
      
      
      
              FWS=MIN(1., CIACW*VEL_INC*NLICE*ACCRI(INDEXS)/PP**C1)
              PIACW=FWS*QW
              IF (TC .LT. 0.) PIACWI=PIACW    
            ENDIF           
          ENDIF             





          IF (ICE_logical .EQV. .FALSE.) GO TO 20



          IF (TC.LT.T_ICE .AND. (WV.GT.QSIgrd .OR. QW.GT.EPSQ)) THEN
   
   
   
   
   
   
   
            PCOND=-QW
            DUM1=TK+XLV1*PCOND                 
            DUM2=WV+QW                         
            DUM=1000.*FPVS(DUM1)               
            DUM=RHgrd*EPS*DUM/(PP-DUM)         
            IF (DUM2 .GT. DUM) PIDEP=DEPOSIT (PP, DUM1, DUM2, RHgrd)  
            DWVi=0.    
   
          ELSE IF (TC .LT. 0.) THEN
   
   
   
   


            DENOMI=1.+XLS2*QSIgrd*TK2   
            DWVi=MIN(WVQW,QSWgrd)-QSIgrd   
            PIDEP_max=MAX(PILOSS, DWVi/DENOMI)
            IF (QTICE .GT. 0.) THEN
      
      
      
      
      
      
      
      
              SFACTOR=SQRT(VEL_INC)*(RHO/(DIFFUS*DIFFUS*DYNVIS))**C2   
              ABI=1./(RHO*XLS3*QSI*TK2/THERM_COND+1./DIFFUS)
      
      
      
      
      
      
      
              VENTIL=(VENTI1(INDEXS)+SFACTOR*VENTI2(INDEXS))*NLICE
              VENTIS=(VENTI1(MDImin)+SFACTOR*VENTI2(MDImin))*NSmICE
              DIDEP=ABI*(VENTIL+VENTIS)*DTPH
      
      
      
              IF (DIDEP .GE. Xratio)then
                DIDEP=(1.-EXP(-DIDEP*DENOMI))/DENOMI
              endif
              IF (DWVi .GT. 0.) THEN
                PIDEP=MIN(DWVi*DIDEP, PIDEP_max)
              ELSE IF (DWVi .LT. 0.) THEN
                PIDEP=MAX(DWVi*DIDEP, PIDEP_max)
              ENDIF
      

            ELSE IF (WVQW.GT.QSIgrd .AND. TC.LE.T_ICE_init) THEN   
      
      
      
      
      
      
      
              INDEX_MY=MAX(MY_T1, MIN( INT(.5-TC), MY_T2 ) )
      
      
      
      
      
      
      
      
      
              DUM1=QSW/QSI-1.      
              DUM2=1.E3*EXP(12.96*DUM1-.639)
              PIDEP=MIN(PIDEP_max, DUM2*MY_GROWTH(INDEX_MY)*RRHO)
      
            ENDIF       
   
          ENDIF         



20      CONTINUE     







          IF (TC.GE.T_ICE .AND. (QW.GT.EPSQ .OR. WV.GT.QSWgrd)) THEN
            IF (PIACWI.EQ.0. .AND. PIDEP.EQ.0.) THEN
              PCOND=CONDENSE (PP, QW, TK, WV, RHgrd)   
            ELSE
   
   
   
              DUM=XLV*QSWgrd*RCPRV*TK2
              DENOMWI=1.+XLS*DUM
              DENOMF=XLF*DUM
              DUM=MAX(0., PIDEP)
              PCOND=(WV-QSWgrd-DENOMWI*DUM-DENOMF*PIACWI)/DENOMW
              DUM1=-QW
              DUM2=PCOND-PIACW
              IF (DUM2 .LT. DUM1) THEN
      
      
      
                DUM=DUM1/DUM2
                PCOND=DUM*PCOND
                PIACW=DUM*PIACW
                PIACWI=DUM*PIACWI
              ENDIF        
            ENDIF          
          ENDIF            




          TCC=TC+XLV1*PCOND+XLS1*PIDEP+XLF1*PIACWI
          IF (TCC .GT. 0.) THEN
            PIACWI=0.
            TCC=TC+XLV1*PCOND+XLS1*PIDEP
          ENDIF
          IF (TC.GT.0. .AND. TCC.GT.0. .AND. ICE_logical) THEN
   
   
   
   
   
   
            SFACTOR=SQRT(VEL_INC)*(RHO/(DIFFUS*DIFFUS*DYNVIS))**C2   
            VENTIL=NLICE*(VENTI1(INDEXS)+SFACTOR*VENTI2(INDEXS))
            AIEVP=VENTIL*DIFFUS*DTPH
            IF (AIEVP .LT. Xratio) THEN
              DIEVP=AIEVP
            ELSE
              DIEVP=1.-EXP(-AIEVP)
            ENDIF
            QSW0=EPS*ESW0/(PP-ESW0)

            DWV0=MIN(WV,QSWgrd)-QSW0*RHgrd   
            DUM=QW+PCOND

            IF (WV.LT.QSWgrd .AND. DUM.LE.EPSQ) THEN   
   
   
   
   
              DUM=DWV0*DIEVP
              PIEVP=MAX( MIN(0., DUM), PILOSS)
              PICND=MAX(0., DUM)
            ENDIF            
            PIMLT=THERM_COND*TCC*VENTIL*RRHO*DTPH/XLF
   
   
   
            DUM1=MAX( 0., (TCC+XLV1*PIEVP)/XLF1 )
            PIMLT=MIN(PIMLT, DUM1)
   
   
   
            DUM=PIEVP-PIMLT
            IF (DUM .LT. PILOSS) THEN
              DUM1=PILOSS/DUM
              PIMLT=PIMLT*DUM1
              PIEVP=PIEVP*DUM1
            ENDIF           
          ENDIF             














          TOT_RAIN=0.
          VRAIN1=0.
          QTRAIN=0.
          PRLOSS=0.
          RQR=0.
          N0r=0.
          INDEXR=MDRmin
          INDEXR1=INDEXR    
          IF (RAIN_logical) THEN
            IF (ARAIN .LE. 0.) THEN
              INDEXR=MDRmin
              VRAIN1=0.
            ELSE
   
   
   
   
   
   
              RR=ARAIN/(DTPH*GAMMAR)
   
              IF (RR .LE. RR_DRmin) THEN
        
        
        
        
                INDEXR=MDRmin
              ELSE IF (RR .LE. RR_DR1) THEN
        
        
        
        
        
        
        
        
                INDEXR=INT( 1.123E3*RR**.1947 + .5 )
                INDEXR=MAX( MDRmin, MIN(INDEXR, MDR1) )
              ELSE IF (RR .LE. RR_DR2) THEN
        
        
        
        
        
        
        
        
                INDEXR=INT( 1.225E3*RR**.2017 + .5 )
                INDEXR=MAX( MDR1, MIN(INDEXR, MDR2) )
              ELSE IF (RR .LE. RR_DR3) THEN
        
        
        
        
        
        
        
        
                INDEXR=INT( 1.3006E3*RR**.2083 + .5 )
                INDEXR=MAX( MDR2, MIN(INDEXR, MDR3) )
              ELSE IF (RR .LE. RR_DRmax) THEN
        
        
        
        
        
        
        
        
                INDEXR=INT( 1.355E3*RR**.2144 + .5 )
                INDEXR=MAX( MDR3, MIN(INDEXR, MDRmax) )
              ELSE 
        
        
        
        
                INDEXR=MDRmax
              ENDIF              
              VRAIN1=GAMMAR*VRAIN(INDEXR)
            ENDIF              
            INDEXR1=INDEXR     
            TOT_RAIN=THICK*QR+BLEND*ARAIN
            QTRAIN=TOT_RAIN/(THICK+BLDTRH*VRAIN1)
            PRLOSS=-TOT_RAIN/THICK
            RQR=RHO*QTRAIN
   
   
   
            IF (RQR .LE. RQR_DRmin) THEN
              N0r=MAX(N0rmin, CN0r_DMRmin*RQR)
              INDEXR=MDRmin
            ELSE IF (RQR .GE. RQR_DRmax) THEN
              N0r=CN0r_DMRmax*RQR
              INDEXR=MDRmax
            ELSE
              N0r=N0r0
              INDEXR=MAX( XMRmin, MIN(CN0r0*RQR**.25, XMRmax) )
            ENDIF
   
            IF (TC .LT. T_ICE) THEN
              PIACR=-PRLOSS
            ELSE

              DWVr=WV-PCOND-QSWgrd   
              DUM=QW+PCOND
              IF (DWVr.LT.0. .AND. DUM.LE.EPSQ) THEN
      
      
      
      
      
      
      
      
      
      
                RFACTOR=SQRT(GAMMAR)*(RHO/(DIFFUS*DIFFUS*DYNVIS))**C2   
                ABW=1./(RHO*XLV2/THERM_COND+1./DIFFUS)
      
      
      
      
                VENTR=N0r*(VENTR1(INDEXR)+RFACTOR*VENTR2(INDEXR))
                CREVP=ABW*VENTR*DTPH
                IF (CREVP .LT. Xratio) THEN
                  DUM=DWVr*CREVP
                ELSE
                  DUM=DWVr*(1.-EXP(-CREVP*DENOMW))/DENOMW
                ENDIF
                PREVP=MAX(DUM, PRLOSS)
              ELSE IF (QW .GT. EPSQ) THEN
                FWR=CRACW*GAMMAR*N0r*ACCRR(INDEXR)
                PRACW=MIN(1.,FWR)*QW
              ENDIF           
      
              IF (TC.LT.0. .AND. TCC.LT.0.) THEN
         
         
         
         
                DUM=.001*FLOAT(INDEXR)
                DUM=(EXP(ABFR*TC)-1.)*DUM*DUM*DUM*DUM*DUM*DUM*DUM
                PIACR=MIN(CBFR*N0r*RRHO*DUM, QTRAIN)
                IF (QLICE .GT. EPSQ) THEN
            
            
            
                  DUM=GAMMAR*VRAIN(INDEXR)
                  DUM1=DUM-VSNOW
            
            
            
            
            
                  DUM2=SQRT(DUM1*DUM1+.04*DUM*VSNOW)    
                  DUM1=5.E-12*INDEXR*INDEXR+2.E-12*INDEXR*INDEXS        &
     &                 +.5E-12*INDEXS*INDEXS
                  FIR=MIN(1., CIACR*NLICE*DUM1*DUM2)
            
            
            
                  PIACR=MIN(PIACR+FIR*QTRAIN, QTRAIN)
                ENDIF        
                DUM=PREVP-PIACR
                If (DUM .LT. PRLOSS) THEN
                  DUM1=PRLOSS/DUM
                  PREVP=DUM1*PREVP
                  PIACR=DUM1*PIACR
                ENDIF        
              ENDIF          
            ENDIF            
          ENDIF              












          DUM1=PIACW+PRAUT+PRACW-MIN(0.,PCOND)
          IF (DUM1 .GT. QW) THEN
            DUM=QW/DUM1
            PIACW=DUM*PIACW
            PIACWI=DUM*PIACWI
            PRAUT=DUM*PRAUT
            PRACW=DUM*PRACW
            IF (PCOND .LT. 0.) PCOND=DUM*PCOND
          ENDIF
          PIACWR=PIACW-PIACWI          



          DELW=PCOND-PIACW-PRAUT-PRACW
          QWnew=QW+DELW
          IF (QWnew .LE. EPSQ) QWnew=0.
          IF (QW.GT.0. .AND. QWnew.NE.0.) THEN
            DUM=QWnew/QW
            IF (DUM .LT. TOLER) QWnew=0.
          ENDIF



          DELT= XLV1*(PCOND+PIEVP+PICND+PREVP)                          &
     &         +XLS1*PIDEP+XLF1*(PIACWI+PIACR-PIMLT)
          Tnew=TK+DELT

          DELV=-PCOND-PIDEP-PIEVP-PICND-PREVP
          WVnew=WV+DELV
















          DELI=0.
          RimeF=1.
          IF (ICE_logical) THEN
            DELI=PIDEP+PIEVP+PIACWI+PIACR-PIMLT
            TOT_ICEnew=TOT_ICE+THICK*DELI
            IF (TOT_ICE.GT.0. .AND. TOT_ICEnew.NE.0.) THEN
              DUM=TOT_ICEnew/TOT_ICE
              IF (DUM .LT. TOLER) TOT_ICEnew=0.
            ENDIF
            IF (TOT_ICEnew .LE. CLIMIT) THEN
              TOT_ICEnew=0.
              RimeF=1.
              QInew=0.
              ASNOWnew=0.
            ELSE
      
      
      
              DUM=PIACWI+PIACR
              IF (DUM.LE.EPSQ .AND. PIDEP.LE.EPSQ) THEN
                RimeF=RimeF1
              ELSE
         
         
         
         
         
                DUM1=TOT_ICE+THICK*(PIDEP+DUM)
                DUM2=TOT_ICE/RimeF1+THICK*PIDEP
                IF (DUM2 .LE. 0.) THEN
                  RimeF=RFmax
                ELSE
                  RimeF=MIN(RFmax, MAX(1., DUM1/DUM2) )
                ENDIF
              ENDIF       
              QInew=TOT_ICEnew/(THICK+BLDTRH*FLIMASS*VSNOW)
              IF (QInew .LE. EPSQ) QInew=0.
              IF (QI.GT.0. .AND. QInew.NE.0.) THEN
                DUM=QInew/QI
                IF (DUM .LT. TOLER) QInew=0.
              ENDIF
              ASNOWnew=BLDTRH*FLIMASS*VSNOW*QInew
              IF (ASNOW.GT.0. .AND. ASNOWnew.NE.0.) THEN
                DUM=ASNOWnew/ASNOW
                IF (DUM .LT. TOLER) ASNOWnew=0.
              ENDIF
            ENDIF         
          ENDIF           















          DELR=PRAUT+PRACW+PIACWR-PIACR+PIMLT+PREVP+PICND
          TOT_RAINnew=TOT_RAIN+THICK*DELR
          IF (TOT_RAIN.GT.0. .AND. TOT_RAINnew.NE.0.) THEN
            DUM=TOT_RAINnew/TOT_RAIN
            IF (DUM .LT. TOLER) TOT_RAINnew=0.
          ENDIF
          IF (TOT_RAINnew .LE. CLIMIT) THEN
            TOT_RAINnew=0.
            VRAIN2=0.
            QRnew=0.
            ARAINnew=0.
          ELSE
   
   
   
            RR=TOT_RAINnew/(DTPH*GAMMAR)
   
   
   
   
   
   
   
   
            IF (RR .LE. RR_DRmin) THEN
              IDR=MDRmin
            ELSE IF (RR .LE. RR_DR1) THEN
              IDR=INT( 1.123E3*RR**.1947 + .5 )
              IDR=MAX( MDRmin, MIN(IDR, MDR1) )
            ELSE IF (RR .LE. RR_DR2) THEN
              IDR=INT( 1.225E3*RR**.2017 + .5 )
              IDR=MAX( MDR1, MIN(IDR, MDR2) )
            ELSE IF (RR .LE. RR_DR3) THEN
              IDR=INT( 1.3006E3*RR**.2083 + .5 )
              IDR=MAX( MDR2, MIN(IDR, MDR3) )
            ELSE IF (RR .LE. RR_DRmax) THEN
              IDR=INT( 1.355E3*RR**.2144 + .5 )
              IDR=MAX( MDR3, MIN(IDR, MDRmax) )
            ELSE 
              IDR=MDRmax
            ENDIF              
            VRAIN2=GAMMAR*VRAIN(IDR)
            QRnew=TOT_RAINnew/(THICK+BLDTRH*VRAIN2)
            IF (QRnew .LE. EPSQ) QRnew=0.
            IF (QR.GT.0. .AND. QRnew.NE.0.) THEN
              DUM=QRnew/QR
              IF (DUM .LT. TOLER) QRnew=0.
            ENDIF
            ARAINnew=BLDTRH*VRAIN2*QRnew
            IF (ARAIN.GT.0. .AND. ARAINnew.NE.0.) THEN
              DUM=ARAINnew/ARAIN
              IF (DUM .LT. TOLER) ARAINnew=0.
            ENDIF
          ENDIF

          WCnew=QWnew+QRnew+QInew









          QT=THICK*(WV+WC)+ARAIN+ASNOW
          QTnew=THICK*(WVnew+WCnew)+ARAINnew+ASNOWnew
          BUDGET=QT-QTnew



          IF (PRINT_err) THEN
           DBG_logical=.FALSE.
           DUM=ABS(BUDGET)
           IF (DUM .GT. TOLER) THEN
             DUM=DUM/MIN(QT, QTnew)
             IF (DUM .GT. TOLER) DBG_logical=.TRUE.
           ENDIF







           IF (WVnew.LT.EPSQ .OR. DBG_logical) THEN
   
            WRITE(6,"(/2(a,i4),2(a,i2))") '{} i=',I_index,' j=',J_index, &
     &                                    ' L=',L,' LSFC=',LSFC
   
            ESW=1000.*FPVS0(Tnew)
            QSWnew=EPS*ESW/(PP-ESW)
            IF (TC.LT.0. .OR. Tnew .LT. 0.) THEN
              ESI=1000.*FPVS(Tnew)
              QSInew=EPS*ESI/(PP-ESI)
            ELSE
              QSI=QSW
              QSInew=QSWnew
            ENDIF
            WSnew=QSInew
            WRITE(6,"(4(a12,g11.4,1x))")                                   &
     & '{} TCold=',TC,'TCnew=',Tnew-T0C,'P=',.01*PP,'RHO=',RHO,            &
     & '{} THICK=',THICK,'RHold=',WV/WS,'RHnew=',WVnew/WSnew,              &
     &   'RHgrd=',RHgrd,                                                   &
     & '{} RHWold=',WV/QSW,'RHWnew=',WVnew/QSWnew,'RHIold=',WV/QSI,        &
     &   'RHInew=',WVnew/QSInew,                                           &
     & '{} QSWold=',QSW,'QSWnew=',QSWnew,'QSIold=',QSI,'QSInew=',QSInew,   &
     & '{} WSold=',WS,'WSnew=',WSnew,'WVold=',WV,'WVnew=',WVnew,           &
     & '{} WCold=',WC,'WCnew=',WCnew,'QWold=',QW,'QWnew=',QWnew,           &
     & '{} QIold=',QI,'QInew=',QInew,'QRold=',QR,'QRnew=',QRnew,           &
     & '{} ARAINold=',ARAIN,'ARAINnew=',ARAINnew,'ASNOWold=',ASNOW,        &
     &   'ASNOWnew=',ASNOWnew,                                             &
     & '{} TOT_RAIN=',TOT_RAIN,'TOT_RAINnew=',TOT_RAINnew,                 &
     &   'TOT_ICE=',TOT_ICE,'TOT_ICEnew=',TOT_ICEnew,                      &
     & '{} BUDGET=',BUDGET,'QTold=',QT,'QTnew=',QTnew                       
   
            WRITE(6,"(4(a12,g11.4,1x))")                                   &
     & '{} DELT=',DELT,'DELV=',DELV,'DELW=',DELW,'DELI=',DELI,             &
     & '{} DELR=',DELR,'PCOND=',PCOND,'PIDEP=',PIDEP,'PIEVP=',PIEVP,       &
     & '{} PICND=',PICND,'PREVP=',PREVP,'PRAUT=',PRAUT,'PRACW=',PRACW,     &
     & '{} PIACW=',PIACW,'PIACWI=',PIACWI,'PIACWR=',PIACWR,'PIMLT=',       &
     &    PIMLT,                                                           &
     & '{} PIACR=',PIACR                                                    
   
            IF (ICE_logical) WRITE(6,"(4(a12,g11.4,1x))")                  &
     & '{} RimeF1=',RimeF1,'GAMMAS=',GAMMAS,'VrimeF=',VrimeF,              &
     &   'VSNOW=',VSNOW,                                                   &
     & '{} INDEXS=',FLOAT(INDEXS),'FLARGE=',FLARGE,'FSMALL=',FSMALL,       &
     &   'FLIMASS=',FLIMASS,                                               &
     & '{} XSIMASS=',XSIMASS,'XLIMASS=',XLIMASS,'QLICE=',QLICE,            &
     &   'QTICE=',QTICE,                                                   &
     & '{} NLICE=',NLICE,'NSmICE=',NSmICE,'PILOSS=',PILOSS,                &
     &   'EMAIRI=',EMAIRI,                                                 &
     & '{} RimeF=',RimeF                                                    
   
            IF (TOT_RAIN.GT.0. .OR. TOT_RAINnew.GT.0.)                     &
     &        WRITE(6,"(4(a12,g11.4,1x))")                                 &
     & '{} INDEXR1=',FLOAT(INDEXR1),'INDEXR=',FLOAT(INDEXR),               &
     &   'GAMMAR=',GAMMAR,'N0r=',N0r,                                      &
     & '{} VRAIN1=',VRAIN1,'VRAIN2=',VRAIN2,'QTRAIN=',QTRAIN,'RQR=',RQR,   &
     & '{} PRLOSS=',PRLOSS,'VOLR1=',THICK+BLDTRH*VRAIN1,                   &
     &   'VOLR2=',THICK+BLDTRH*VRAIN2
   
            IF (PRAUT .GT. 0.) WRITE(6,"(a12,g11.4,1x)") '{} QW0=',QW0
   
            IF (PRACW .GT. 0.) WRITE(6,"(a12,g11.4,1x)") '{} FWR=',FWR
   
            IF (PIACR .GT. 0.) WRITE(6,"(a12,g11.4,1x)") '{} FIR=',FIR
   
            DUM=PIMLT+PICND-PREVP-PIEVP
            IF (DUM.GT.0. .or. DWVi.NE.0.)                                 &
     &        WRITE(6,"(4(a12,g11.4,1x))")                                 &
     & '{} TFACTOR=',TFACTOR,'DYNVIS=',DYNVIS,                             &
     &   'THERM_CON=',THERM_COND,'DIFFUS=',DIFFUS
   
            IF (PREVP .LT. 0.) WRITE(6,"(4(a12,g11.4,1x))")                &
     & '{} RFACTOR=',RFACTOR,'ABW=',ABW,'VENTR=',VENTR,'CREVP=',CREVP,     &
     & '{} DWVr=',DWVr,'DENOMW=',DENOMW
   
            IF (PIDEP.NE.0. .AND. DWVi.NE.0.)                              &
     &        WRITE(6,"(4(a12,g11.4,1x))")                                 &
     & '{} DWVi=',DWVi,'DENOMI=',DENOMI,'PIDEP_max=',PIDEP_max,            &
     &   'SFACTOR=',SFACTOR,                                               &
     & '{} ABI=',ABI,'VENTIL=',VENTIL,'VENTIL1=',VENTI1(INDEXS),           &
     &   'VENTIL2=',SFACTOR*VENTI2(INDEXS),                                &
     & '{} VENTIS=',VENTIS,'DIDEP=',DIDEP
   




   


   
            DUM=PIMLT+PICND-PIEVP
            IF (DUM.GT. 0.) WRITE(6,"(4(a12,g11.4,1x))")                   &
     & '{} SFACTOR=',SFACTOR,'VENTIL=',VENTIL,'VENTIL1=',VENTI1(INDEXS),   &
     &   'VENTIL2=',SFACTOR*VENTI2(INDEXS),                                &
     & '{} AIEVP=',AIEVP,'DIEVP=',DIEVP,'QSW0=',QSW0,'DWV0=',DWV0       
   
          ENDIF      
          ENDIF      





















































          T_col(L)=Tnew                           

          QV_col(L)=max(EPSQ, WVnew/(1.+WVnew))   
          WC_col(L)=max(EPSQ, WCnew)              
          QI_col(L)=max(EPSQ, QInew)              
          QR_col(L)=max(EPSQ, QRnew)              
          QW_col(L)=max(EPSQ, QWnew)              
          RimeF_col(L)=RimeF                      
          ASNOW=ASNOWnew                          
          ARAIN=ARAINnew                          

 
          VRabove=VRAIN2                          






10      CONTINUE         









        CONTAINS




      REAL FUNCTION CONDENSE (PP, QW, TK, WV, RHgrd)   







      IMPLICIT NONE

      INTEGER, PARAMETER :: HIGH_PRES=Selected_Real_Kind(15)
      REAL (KIND=HIGH_PRES), PARAMETER ::                               &
     & RHLIMIT=.001, RHLIMIT1=-RHLIMIT
      REAL (KIND=HIGH_PRES) :: COND, SSAT, WCdum

      REAL,INTENT(IN) :: QW,PP,WV,TK,RHgrd   
      REAL WVdum,Tdum,XLV2,DWV,WS,ESW,XLV1,XLV
integer nsteps





      XLV=3.148E6-2370.*TK
      XLV1=XLV*RCP
      XLV2=XLV*XLV*RCPRV
      Tdum=TK
      WVdum=WV
      WCdum=QW
      ESW=1000.*FPVS0(Tdum)                     
      WS=RHgrd*EPS*ESW/(PP-ESW)                 
      DWV=WVdum-WS                              
      SSAT=DWV/WS                               
      CONDENSE=0.
nsteps = 0
      DO WHILE ((SSAT.LT.RHLIMIT1 .AND. WCdum.GT.EPSQ)                  &
     &           .OR. SSAT.GT.RHLIMIT)
        nsteps = nsteps + 1
        COND=DWV/(1.+XLV2*WS/(Tdum*Tdum))       
        COND=MAX(COND, -WCdum)                  
        Tdum=Tdum+XLV1*COND                     
        WVdum=WVdum-COND                        
        WCdum=WCdum+COND                        
        CONDENSE=CONDENSE+COND                  
        ESW=1000.*FPVS0(Tdum)                   
        WS=RHgrd*EPS*ESW/(PP-ESW)               
        DWV=WVdum-WS                            
        SSAT=DWV/WS                             
      ENDDO

      END FUNCTION CONDENSE





      REAL FUNCTION DEPOSIT (PP, Tdum, WVdum, RHgrd)   




      IMPLICIT NONE      

      INTEGER, PARAMETER :: HIGH_PRES=Selected_Real_Kind(15)
      REAL (KIND=HIGH_PRES), PARAMETER :: RHLIMIT=.001,                 &
     & RHLIMIT1=-RHLIMIT
      REAL (KIND=HIGH_PRES) :: DEP, SSAT

      real,INTENT(IN) ::  PP,RHgrd   
      real,INTENT(INOUT) ::  WVdum,Tdum
      real ESI,WS,DWV



      ESI=1000.*FPVS(Tdum)                      
      WS=RHgrd*EPS*ESI/(PP-ESI)                 
      DWV=WVdum-WS                              
      SSAT=DWV/WS                               
      DEPOSIT=0.
      DO WHILE (SSAT.GT.RHLIMIT .OR. SSAT.LT.RHLIMIT1)
   
   
   
   
   
        DEP=DWV/(1.+XLS2*WS/(Tdum*Tdum))        
        Tdum=Tdum+XLS1*DEP                      
        WVdum=WVdum-DEP                         
        DEPOSIT=DEPOSIT+DEP                     
        ESI=1000.*FPVS(Tdum)                    
        WS=RHgrd*EPS*ESI/(PP-ESI)               
        DWV=WVdum-WS                            
        SSAT=DWV/WS                             
      ENDDO

      END FUNCTION DEPOSIT

      END SUBROUTINE EGCP01COLUMN 









      SUBROUTINE etanewinit_HWRF (GSMDT,DT,DELX,DELY,LOWLYR,restart,         &
     &   F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,                              &

     &   ALLOWED_TO_READ,                                               &
     &   IDS,IDE,JDS,JDE,KDS,KDE,                                       &
     &   IMS,IME,JMS,JME,KMS,KME,                                       &
     &   ITS,ITE,JTS,JTE,KTS,KTE                                       )

























































      IMPLICIT NONE















      INTEGER, PARAMETER :: MDR1=XMR1, MDR2=XMR2, MDR3=XMR3


      integer,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     & 
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE       

       INTEGER, DIMENSION(ims:ime,jms:jme),INTENT(INOUT) :: LOWLYR

      real, INTENT(IN) ::  DELX,DELY


      real,DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(OUT) ::          &
     &  F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
      INTEGER, PARAMETER :: ITLO=-60, ITHI=40




      real,INTENT(IN) :: DT,GSMDT
      LOGICAL,INTENT(IN) :: allowed_to_read,restart




      REAL :: BBFR,DTPH,PI,DX,Thour_print
      INTEGER :: I,IM,J,L,K,JTF,KTF,ITF
      INTEGER :: etampnew_unit1
      LOGICAL :: opened
      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
      CHARACTER*80 errmess



      JTF=MIN0(JTE,JDE-1)
      KTF=MIN0(KTE,KDE-1)
      ITF=MIN0(ITE,IDE-1)

      DO J=JTS,JTF
      DO I=ITS,ITF
        LOWLYR(I,J)=1
      ENDDO
      ENDDO

      IF(.NOT.RESTART .AND. ALLOWED_TO_READ) THEN    
        CALL wrf_debug(1,'WARNING: F_ICE_PHY,F_RAIN_PHY AND F_RIMEF_PHY IS REINITIALIZED')   
        DO J = jts,jte
        DO K = kts,kte
        DO I= its,ite
          F_ICE_PHY(i,k,j)=0.
          F_RAIN_PHY(i,k,j)=0.
          F_RIMEF_PHY(i,k,j)=1.
        ENDDO
        ENDDO
        ENDDO
      ENDIF


      IF(ALLOWED_TO_READ)THEN


        DX=SQRT((DELX)**2+(DELY)**2)/1000.    
        DX=MIN(100., MAX(5., DX) )






        DTPH=MAX(GSMDT*60.,DT)
        DTPH=NINT(DTPH/DT)*DT



        CALL GPVS



        IF ( wrf_dm_on_monitor() ) THEN
          DO i = 31,99
            INQUIRE ( i , OPENED = opened )
            IF ( .NOT. opened ) THEN
              etampnew_unit1 = i
              GOTO 2061
            ENDIF
          ENDDO
          etampnew_unit1 = -1
 2061     CONTINUE
        ENDIF

        CALL wrf_dm_bcast_bytes ( etampnew_unit1 , 4 )

        IF ( etampnew_unit1 < 0 ) THEN
          CALL wrf_error_fatal3("<stdin>",2282,&
'module_mp_hwrf: etanewinit: Can not find unused fortran unit to read in lookup table.' )
        ENDIF

        IF ( wrf_dm_on_monitor() ) THEN

          OPEN(UNIT=etampnew_unit1,FILE="ETAMPNEW_DATA",                  &
     &        FORM="UNFORMATTED",STATUS="OLD",ERR=9061)

          READ(etampnew_unit1) VENTR1
          READ(etampnew_unit1) VENTR2
          READ(etampnew_unit1) ACCRR
          READ(etampnew_unit1) MASSR
          READ(etampnew_unit1) VRAIN
          READ(etampnew_unit1) RRATE
          READ(etampnew_unit1) VENTI1
          READ(etampnew_unit1) VENTI2
          READ(etampnew_unit1) ACCRI
          READ(etampnew_unit1) MASSI
          READ(etampnew_unit1) VSNOWI
          READ(etampnew_unit1) VEL_RF

          CLOSE (etampnew_unit1)
        ENDIF

        CALL wrf_dm_bcast_bytes ( VENTR1 , size ( VENTR1 ) * 4 )
        CALL wrf_dm_bcast_bytes ( VENTR2 , size ( VENTR2 ) * 4 )
        CALL wrf_dm_bcast_bytes ( ACCRR , size ( ACCRR ) * 4 )
        CALL wrf_dm_bcast_bytes ( MASSR , size ( MASSR ) * 4 )
        CALL wrf_dm_bcast_bytes ( VRAIN , size ( VRAIN ) * 4 )
        CALL wrf_dm_bcast_bytes ( RRATE , size ( RRATE ) * 4 )
        CALL wrf_dm_bcast_bytes ( VENTI1 , size ( VENTI1 ) * 4 )
        CALL wrf_dm_bcast_bytes ( VENTI2 , size ( VENTI2 ) * 4 )
        CALL wrf_dm_bcast_bytes ( ACCRI , size ( ACCRI ) * 4 )
        CALL wrf_dm_bcast_bytes ( MASSI , size ( MASSI ) * 4 )
        CALL wrf_dm_bcast_bytes ( VSNOWI , size ( VSNOWI ) * 4 )
        CALL wrf_dm_bcast_bytes ( VEL_RF , size ( VEL_RF ) * 4 )




        CALL MY_GROWTH_RATES (DTPH)


        PI=ACOS(-1.)




        ABFR=-0.66
        BBFR=100.
        CBFR=20.*PI*PI*BBFR*RHOL*1.E-21





        CIACW=DTPH*0.25*PI*0.5*(1.E5)**C1




        CIACR=PI*DTPH







        RR_DRmin=N0r0*RRATE(MDRmin)     
        RR_DR1=N0r0*RRATE(MDR1)         
        RR_DR2=N0r0*RRATE(MDR2)         
        RR_DR3=N0r0*RRATE(MDR3)         
        RR_DRmax=N0r0*RRATE(MDRmax)     

        RQR_DRmin=N0r0*MASSR(MDRmin)    
        RQR_DR1=N0r0*MASSR(MDR1)        
        RQR_DR2=N0r0*MASSR(MDR2)        
        RQR_DR3=N0r0*MASSR(MDR3)        
        RQR_DRmax=N0r0*MASSR(MDRmax)    
        C_N0r0=PI*RHOL*N0r0
        CN0r0=1.E6/C_N0r0**.25
        CN0r_DMRmin=1./(PI*RHOL*DMRmin**4)
        CN0r_DMRmax=1./(PI*RHOL*DMRmax**4)




        CRACW=DTPH*0.25*PI*1.0

        ESW0=1000.*FPVS0(T0C)     
        RFmax=1.1**Nrime          











        CRAUT=1.-EXP(-1.E-3*DTPH)







        QAUT0=PI*RHOL*NCW*(20.E-6)**3/6.













        DO I=MDImin,MDImax
          SDENS(I)=PI*1.5E-15*FLOAT(I*I*I)/MASSI(I)
        ENDDO

        Thour_print=-DTPH/3600.




























      ENDIF  

      RETURN



9061 CONTINUE
      WRITE( errmess , '(A,I4)' )                                        &
       'module_mp_hwrf: error opening ETAMPNEW_DATA on unit '          &
     &, etampnew_unit1
      CALL wrf_error_fatal3("<stdin>",2451,&
errmess)


      END SUBROUTINE etanewinit_HWRF

      SUBROUTINE MY_GROWTH_RATES (DTPH)













      IMPLICIT NONE

      REAL,INTENT(IN) :: DTPH

      REAL  DT_ICE
      REAL,DIMENSION(35) :: MY_600



      DATA MY_600 /                                                     &
     & 5.5e-8, 1.4E-7, 2.8E-7, 6.E-7, 3.3E-6,                           & 
     & 2.E-6, 9.E-7, 8.8E-7, 8.2E-7, 9.4e-7,                            & 
     & 1.2E-6, 1.85E-6, 5.5E-6, 1.5E-5, 1.7E-5,                         & 
     & 1.5E-5, 1.E-5, 3.4E-6, 1.85E-6, 1.35E-6,                         & 
     & 1.05E-6, 1.E-6, 9.5E-7, 9.0E-7, 9.5E-7,                          & 
     & 9.5E-7, 9.E-7, 9.E-7, 9.E-7, 9.E-7,                              & 
     & 9.E-7, 9.E-7, 9.E-7, 9.E-7, 9.E-7 /        



      DT_ICE=(DTPH/600.)**1.5
      MY_GROWTH=DT_ICE*MY_600



      END SUBROUTINE MY_GROWTH_RATES





      SUBROUTINE GPVS






























      IMPLICIT NONE
      real :: X,XINC,T
      integer :: JX

      XINC=(XMAX-XMIN)/(NX-1)
      C1XPVS=1.-XMIN/XINC
      C2XPVS=1./XINC
      C1XPVS0=1.-XMIN/XINC
      C2XPVS0=1./XINC

      DO JX=1,NX
        X=XMIN+(JX-1)*XINC
        T=X
        TBPVS(JX)=FPVSX(T)
        TBPVS0(JX)=FPVSX0(T)
      ENDDO

      END SUBROUTINE GPVS



                     REAL   FUNCTION FPVS(T)































      IMPLICIT NONE
      real,INTENT(IN) :: T
      real XJ
      integer :: JX

      XJ=MIN(MAX(C1XPVS+C2XPVS*T,1.),FLOAT(NX))
      JX=MIN(XJ,NX-1.)
      FPVS=TBPVS(JX)+(XJ-JX)*(TBPVS(JX+1)-TBPVS(JX))

      END FUNCTION FPVS


                     REAL FUNCTION FPVS0(T)

      IMPLICIT NONE
      real,INTENT(IN) :: T
      real :: XJ1
      integer :: JX1

      XJ1=MIN(MAX(C1XPVS0+C2XPVS0*T,1.),FLOAT(NX))
      JX1=MIN(XJ1,NX-1.)
      FPVS0=TBPVS0(JX1)+(XJ1-JX1)*(TBPVS0(JX1+1)-TBPVS0(JX1))

      END FUNCTION FPVS0



                    REAL FUNCTION FPVSX(T)



































      IMPLICIT NONE

       real, parameter :: TTP=2.7316E+2,HVAP=2.5000E+6,PSAT=6.1078E+2   &
      ,         CLIQ=4.1855E+3,CVAP= 1.8460E+3                          &
      ,         CICE=2.1060E+3,HSUB=2.8340E+6

      real, parameter :: PSATK=PSAT*1.E-3
      real, parameter :: DLDT=CVAP-CLIQ,XA=-DLDT/RV,XB=XA+HVAP/(RV*TTP)
      real, parameter :: DLDTI=CVAP-CICE                                &
      ,                  XAI=-DLDTI/RV,XBI=XAI+HSUB/(RV*TTP)
      real T,TR

      TR=TTP/T

      IF(T.GE.TTP)THEN
        FPVSX=PSATK*(TR**XA)*EXP(XB*(1.-TR))
      ELSE
        FPVSX=PSATK*(TR**XAI)*EXP(XBI*(1.-TR))
      ENDIF

      END FUNCTION FPVSX


                 REAL   FUNCTION FPVSX0(T)

      IMPLICIT NONE
      real,parameter :: TTP=2.7316E+2,HVAP=2.5000E+6,PSAT=6.1078E+2     &
      ,         CLIQ=4.1855E+3,CVAP=1.8460E+3                           &
      ,         CICE=2.1060E+3,HSUB=2.8340E+6
      real,PARAMETER :: PSATK=PSAT*1.E-3
      real,PARAMETER :: DLDT=CVAP-CLIQ,XA=-DLDT/RV,XB=XA+HVAP/(RV*TTP)
      real,PARAMETER :: DLDTI=CVAP-CICE                                 &
      ,                 XAI=-DLDT/RV,XBI=XA+HSUB/(RV*TTP)
      real :: T,TR

      TR=TTP/T
      FPVSX0=PSATK*(TR**XA)*EXP(XB*(1.-TR))

      END FUNCTION FPVSX0

      END MODULE module_mp_HWRF
