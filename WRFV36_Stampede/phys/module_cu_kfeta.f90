MODULE module_cu_kfeta

   USE module_wrf_error














      INTEGER, PARAMETER :: KFNT=250,KFNP=220
      REAL, DIMENSION(KFNT,KFNP),PRIVATE, SAVE :: TTAB,QSTAB
      REAL, DIMENSION(KFNP),PRIVATE, SAVE :: THE0K
      REAL, DIMENSION(200),PRIVATE, SAVE :: ALU
      REAL, PRIVATE, SAVE :: RDPR,RDTHK,PLUTOP




CONTAINS

   SUBROUTINE KF_eta_CPS(                                    &
              ids,ide, jds,jde, kds,kde                      &
             ,ims,ime, jms,jme, kms,kme                      &
             ,its,ite, jts,jte, kts,kte                      &
             ,trigger                                        &
             ,DT,KTAU,DX,CUDT,ADAPT_STEP_FLAG                &
             ,rho,RAINCV,PRATEC,NCA                          &
             ,U,V,TH,T,W,dz8w,Pcps,pi                        &
             ,W0AVG,XLV0,XLV1,XLS0,XLS1,CP,R,G,EP1           &
             ,EP2,SVP1,SVP2,SVP3,SVPT0                       &
             ,STEPCU,CU_ACT_FLAG,warm_rain,CUTOP,CUBOT       &
             ,QV                                             &
            
             ,F_QV    ,F_QC    ,F_QR    ,F_QI    ,F_QS       &
             ,RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN            &
             ,RQICUTEN,RQSCUTEN, RQVFTEN                     &

             ,cldfra_dp_KF,cldfra_sh_KF                      &
             ,qc_KF,qi_KF                                    &
                                                             )


   IMPLICIT NONE

   INTEGER,      INTENT(IN   ) ::                            &
                                  ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte

   INTEGER,      INTENT(IN   ) :: trigger
   INTEGER,      INTENT(IN   ) :: STEPCU
   LOGICAL,      INTENT(IN   ) :: warm_rain

   REAL,         INTENT(IN   ) :: XLV0,XLV1,XLS0,XLS1
   REAL,         INTENT(IN   ) :: CP,R,G,EP1,EP2
   REAL,         INTENT(IN   ) :: SVP1,SVP2,SVP3,SVPT0

   INTEGER,      INTENT(IN   ) :: KTAU           

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         , &
          INTENT(IN   ) ::                                   &
                                                          U, &
                                                          V, &
                                                          W, &
                                                         TH, &
                                                          T, &
                                                         QV, &
                                                       dz8w, &
                                                       Pcps, &
                                                        rho, &
                                                         pi

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         , &
          INTENT(INOUT) ::                                   &
                                                      W0AVG   

   REAL,  INTENT(IN   ) :: DT, DX
   REAL,  INTENT(IN   ) :: CUDT
   LOGICAL,OPTIONAL,INTENT(IN   ) :: ADAPT_STEP_FLAG

   REAL, DIMENSION( ims:ime , jms:jme ),                     &
          INTENT(INOUT) ::                           RAINCV

   REAL,    DIMENSION( ims:ime , jms:jme ),                  &
          INTENT(INOUT) ::                           PRATEC

   REAL,    DIMENSION( ims:ime , jms:jme ),                  &
            INTENT(INOUT) ::                            NCA

   REAL, DIMENSION( ims:ime , jms:jme ),                     &
          INTENT(OUT) ::                              CUBOT, &
                                                      CUTOP    

   LOGICAL, DIMENSION( ims:ime , jms:jme ),                  &
          INTENT(INOUT) :: CU_ACT_FLAG





   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),           &
         OPTIONAL,                                           &
         INTENT(INOUT) ::                                    &
                                                   RTHCUTEN, &
                                                   RQVCUTEN, &
                                                   RQCCUTEN, &
                                                   RQRCUTEN, &
                                                   RQICUTEN, &
                                                   RQSCUTEN, &
                                                   RQVFTEN







   LOGICAL, OPTIONAL ::                                      &
                                                   F_QV      &
                                                  ,F_QC      &
                                                  ,F_QR      &
                                                  ,F_QI      &
                                                  ,F_QS


   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         , &
          INTENT(INOUT) ::                                   &
                                               cldfra_dp_KF, &
                                               cldfra_sh_KF, &
                                                      qc_KF, &
                                                      qi_KF



   LOGICAL :: flag_qr, flag_qi, flag_qs

   REAL, DIMENSION( kts:kte ) ::                             &
                                                        U1D, &
                                                        V1D, &
                                                        T1D, &
                                                       DZ1D, &
                                                       QV1D, &
                                                        P1D, &
                                                      RHO1D, &
                                                  tpart_v1D, &
                                                  tpart_h1D, &
                                                    W0AVG1D

   REAL, DIMENSION( kts:kte )::                              &
                                                       DQDT, &
                                                      DQIDT, &
                                                      DQCDT, &
                                                      DQRDT, &
                                                      DQSDT, &
                                                       DTDT

  REAL, DIMENSION (its-1:ite+1,kts:kte,jts-1:jte+1) ::  aveh_t, aveh_q
  REAL, DIMENSION (its:ite,kts:kte,jts:jte) :: aveh_qmax, aveh_qmin
  REAL, DIMENSION (its:ite,kts:kte,jts:jte) ::  avev_t, avev_q 
  REAL, DIMENSION (its:ite,kts:kte,jts:jte) :: avev_qmax, avev_qmin
  REAL, DIMENSION (its:ite,kts:kte,jts:jte) ::  coef_v, coef_h, tpart_h, tpart_v
  INTEGER :: ii,jj,kk

  REAL :: ttop
  REAL, DIMENSION (kts:kte)  :: z0

   REAL    ::         TST,tv,PRS,RHOE,W0,SCR1,DXSQ,tmp
   integer :: ibegh,iendh,jbegh,jendh
   integer :: istart,iend,jstart,jend
   INTEGER :: i,j,k,NTST
   REAL    :: lastdt = -1.0
   REAL    :: W0AVGfctr, W0fctr, W0den
   

   DXSQ=DX*DX


   NTST=STEPCU
   TST=float(NTST*2)
   flag_qr = .FALSE.
   flag_qi = .FALSE.
   flag_qs = .FALSE.
   IF ( PRESENT(F_QR) ) flag_qr = F_QR
   IF ( PRESENT(F_QI) ) flag_qi = F_QI
   IF ( PRESENT(F_QS) ) flag_qs = F_QS

   if (lastdt < 0) then
      lastdt = dt
   endif
   
   if (ADAPT_STEP_FLAG) then
      W0AVGfctr = 2 * MAX(CUDT*60,dt) - dt
      W0fctr = dt
      W0den = 2 * MAX(CUDT*60,dt)
   else
      W0AVGfctr = (TST-1.)
      W0fctr = 1.
      W0den = TST
   endif

  DO J = jts,jte
      DO K=kts,kte
         DO I= its,ite




            W0=0.5*(w(I,K,J)+w(I,K+1,J))







            W0AVG(I,K,J) = ( W0AVG(I,K,J) * W0AVGfctr + W0 * W0fctr ) / W0den
         ENDDO
      ENDDO
   ENDDO
   lastdt = dt


     IF (trigger.eq.2) THEN         



     aveh_t=-999   
     aveh_q=-999
     avev_t=0   
     avev_q=0
     avev_qmax=0
     avev_qmin=0
     aveh_qmax=0
     aveh_qmin=0
     tpart_h=0
     tpart_v=0
     coef_h=0
     coef_v=0
     ibegh=max(its-1, ids+1)   
     jbegh=max(jts-1, jds+1)
     iendh=min(ite+1, ide-2)   
     jendh=min(jte+1, jde-2)
        DO J = jbegh,jendh
        DO K = kts,kte
        DO I = ibegh,iendh
          aveh_t(i,k,j)=(T(i-1,k,j-1)+T(i-1,k,j)  +T(i-1,k,j+1)+ &
                         T(i,k,j-1)   +T(i,k,j)   +T(i,k,j+1)+         &
                         T(i+1,k,j-1) +T(i+1,k,j) +T(i+1,k,j+1))/9.
          aveh_q(i,k,j)=(rqvften(i-1,k,j-1)+rqvften(i-1,k,j)  +rqvften(i-1,k,j+1)+ &
                         rqvften(i,k,j-1)   +rqvften(i,k,j)   +rqvften(i,k,j+1)+         &
                         rqvften(i+1,k,j-1) +rqvften(i+1,k,j) +rqvften(i+1,k,j+1))/9.
        ENDDO
        ENDDO
        ENDDO

        DO K = kts,kte
           DO J = jts-1,jte+1
            DO I = its-1,ite+1

            if(i.eq.ids) then
            aveh_t(i,k,j)=aveh_t(i+1,k,j)
            aveh_q(i,k,j)=aveh_q(i+1,k,j)
            elseif(i.eq.ide-1) then
            aveh_t(i,k,j)=aveh_t(i-1,k,j)
            aveh_q(i,k,j)=aveh_q(i-1,k,j)
            endif

            if(j.eq.jds) then
             aveh_t(i,k,j)=aveh_t(i,k,j+1)
             aveh_q(i,k,j)=aveh_q(i,k,j+1)
            elseif(j.eq.jde-1) then
            aveh_t(i,k,j)=aveh_t(i,k,j-1)
            aveh_q(i,k,j)=aveh_q(i,k,j-1)
            endif

            if(j.eq.jds.and.i.eq.ids) then
            aveh_q(i,k,j)=aveh_q(i+1,k,j+1) 
            aveh_t(i,k,j)=aveh_t(i+1,k,j+1) 
            endif

            if(j.eq.jde-1.and.i.eq.ids) then
            aveh_q(i,k,j)=aveh_q(i+1,k,j-1) 
            aveh_t(i,k,j)=aveh_t(i+1,k,j-1) 
            endif

            if(j.eq.jde-1.and.i.eq.ide-1) then
            aveh_q(i,k,j)=aveh_q(i-1,k,j-1) 
            aveh_t(i,k,j)=aveh_t(i-1,k,j-1) 
            endif

            if(j.eq.jds.and.i.eq.ide-1) then
            aveh_q(i,k,j)=aveh_q(i-1,k,j+1) 
            aveh_t(i,k,j)=aveh_t(i-1,k,j+1) 
            endif

            ENDDO
           ENDDO
        ENDDO

     istart=max(its, ids+1)   
     jstart=max(jts, jds+1)
     iend=min(ite, ide-2)   
     jend=min(jte, jde-2)
        DO K = kts,kte
        DO J = jstart,jend
        DO I = istart,iend
           aveh_qmax(i,k,j)=aveh_q(i,k,j)
           aveh_qmin(i,k,j)=aveh_q(i,k,j)
          DO ii=-1, 1
           DO jj=-1,1
             if(aveh_q(i+II,k,j+JJ).gt.aveh_qmax(i,k,j)) aveh_qmax(i,k,j)=aveh_q(i+II,k,j+JJ)
             if(aveh_q(i+II,k,j+JJ).lt.aveh_qmin(i,k,j)) aveh_qmin(i,k,j)=aveh_q(i+II,k,j+JJ)
           ENDDO
          ENDDO 
          if(aveh_qmax(i,k,j).gt.aveh_qmin(i,k,j))then
          coef_h(i,k,j)=(aveh_q(i,k,j)-aveh_qmin(i,k,j))/(aveh_qmax(i,k,j)-aveh_qmin(i,k,j))
          else
          coef_h(i,k,j)=0.
          endif
          coef_h(i,k,j)=amin1(coef_h(i,k,j),1.0)
          coef_h(i,k,j)=amax1(coef_h(i,k,j),0.0)
          tpart_h(i,k,j)=coef_h(i,k,j)*(T(i,k,j)-aveh_t(i,k,j))
        ENDDO
        ENDDO
        ENDDO
    89 continue 

        DO J = jts, jte
        DO I = its, ite
          z0(1) = 0.5 * dz8w(i,1,j)
          DO K = 2, kte
            Z0(K) = Z0(K-1) + .5 * (DZ8W(i,K,j) + DZ8W(i,K-1,j))
          ENDDO
        DO K = kts+1,kte-1
	  ttop = t(i,k,j) + ((t(i,k,j) - t(i,k+1,j)) / (z0(k) - z0(k+1))) * (z0(k)-z0(k-1))
          avev_t(i,k,j)=(T(i,k-1,j) + T(i,k,j) + ttop)/3.

          avev_q(i,k,j)=(rqvften(i,k-1,j)+rqvften(i,k,j) + rqvften(i,k+1,j))/3.
        ENDDO
          avev_t(i,kts,j)=avev_t(i,kts+1,j)   
          avev_q(i,kts,j)=avev_q(i,kts+1,j)   
          avev_t(i,kte,j)=avev_t(i,kte-1,j)   
          avev_q(i,kte,j)=avev_q(i,kte-1,j)   
        ENDDO
        ENDDO

        DO J = jts, jte
        DO I = its, ite
        DO K = kts+1,kte-1
          avev_qmax(i,k,j)=avev_q(i,k,j)
          avev_qmin(i,k,j)=avev_q(i,k,j)
         DO kk=-1,1
         if(avev_q(i,k+kk,j).gt.avev_qmax(i,k,j)) avev_qmax(i,k,j)=avev_q(i,k+kk,j) 
         if(avev_q(i,k+kk,j).lt.avev_qmin(i,k,j)) avev_qmin(i,k,j)=avev_q(i,k+kk,j) 
         ENDDO
         if(avev_qmax(i,k,j).gt.avev_qmin(i,k,j)) then
         coef_v(i,k,j)=(avev_q(i,k,j)-avev_qmin(i,k,j))/(avev_qmax(i,k,j)-avev_qmin(i,k,j))
         else
         coef_v(i,k,j)=0
         endif
         tpart_v(i,k,j)=coef_v(i,k,j)*(T(i,k,j)-avev_t(i,k,j))
        ENDDO
         tpart_v(i,kts,j)= tpart_v(i,kts+1,j)    
         tpart_v(i,kte,j)= tpart_v(i,kte-1,j)    
        ENDDO
        ENDDO
     ENDIF       

     DO J = jts,jte
     DO I= its,ite
        CU_ACT_FLAG(i,j) = .true.
     ENDDO
     ENDDO

     DO J = jts,jte
       DO I=its,ite
          

         IF ( NCA(I,J) .ge. 0.5*DT ) then
            CU_ACT_FLAG(i,j) = .false.
         ELSE

            DO k=kts,kte
               DQDT(k)=0.
               DQIDT(k)=0.
               DQCDT(k)=0.
               DQRDT(k)=0.
               DQSDT(k)=0.
               DTDT(k)=0.

               cldfra_dp_KF(I,k,J)=0.
               cldfra_sh_KF(I,k,J)=0.
               qc_KF(I,k,J)=0.
               qi_KF(I,k,J)=0.
            ENDDO
            RAINCV(I,J)=0.
            CUTOP(I,J)=KTS
            CUBOT(I,J)=KTE+1
            PRATEC(I,J)=0.



            DO K=kts,kte
               U1D(K) =U(I,K,J)
               V1D(K) =V(I,K,J)
               T1D(K) =T(I,K,J)
               RHO1D(K) =rho(I,K,J)
               QV1D(K)=QV(I,K,J)
               P1D(K) =Pcps(I,K,J)
               W0AVG1D(K) =W0AVG(I,K,J)
               DZ1D(k)=dz8w(I,K,J)
               IF (trigger.eq.2) THEN
                  tpart_h1D(K) =tpart_h(I,K,J)
                  tpart_v1D(K) =tpart_v(I,K,J)
               ELSE
                  tpart_h1D(K) = 0.
                  tpart_v1D(K) = 0.
               ENDIF
            ENDDO
            CALL KF_eta_PARA(I, J,                  &
                 U1D,V1D,T1D,QV1D,P1D,DZ1D,W0AVG1D, &
                 tpart_h1D,tpart_v1D,               &
                 trigger,                           &
                 DT,DX,DXSQ,RHO1D,                  &
                 XLV0,XLV1,XLS0,XLS1,CP,R,G,        &
                 EP2,SVP1,SVP2,SVP3,SVPT0,          &
                 DQDT,DQIDT,DQCDT,DQRDT,DQSDT,DTDT, &
                 RAINCV,PRATEC,NCA,                 &
                 flag_QI,flag_QS,warm_rain,         &
                 CUTOP,CUBOT,CUDT,                  &
                 ids,ide, jds,jde, kds,kde,         &
                 ims,ime, jms,jme, kms,kme,         &
                 its,ite, jts,jte, kts,kte,         &

                 cldfra_dp_KF,cldfra_sh_KF,         &
                 qc_KF,qi_KF                        )
            IF(PRESENT(rthcuten).AND.PRESENT(rqvcuten)) THEN
              DO K=kts,kte
                 RTHCUTEN(I,K,J)=DTDT(K)/pi(I,K,J)
                 RQVCUTEN(I,K,J)=DQDT(K)
              ENDDO
            ENDIF

            IF(PRESENT(rqrcuten).AND.PRESENT(rqccuten)) THEN
              IF( F_QR )THEN
                DO K=kts,kte
                   RQRCUTEN(I,K,J)=DQRDT(K)
                   RQCCUTEN(I,K,J)=DQCDT(K)
                ENDDO
              ELSE

                DO K=kts,kte
                   RQRCUTEN(I,K,J)=0.
                   RQCCUTEN(I,K,J)=DQRDT(K)+DQCDT(K)
                ENDDO
              ENDIF
            ENDIF




            IF(PRESENT( rqicuten )) THEN
              IF ( F_QI ) THEN
                DO K=kts,kte
                   RQICUTEN(I,K,J)=DQIDT(K)
                ENDDO
              ENDIF
            ENDIF

            IF(PRESENT( rqscuten )) THEN
              IF ( F_QS ) THEN
                DO K=kts,kte
                   RQSCUTEN(I,K,J)=DQSDT(K)
                ENDDO
              ENDIF
            ENDIF

         ENDIF 
       ENDDO     
     ENDDO       

   END SUBROUTINE KF_eta_CPS


   SUBROUTINE KF_eta_PARA (I, J,                           &
                      U0,V0,T0,QV0,P0,DZQ,W0AVG1D,         &
                      TPART_H0,TPART_V0,                   &
                      trigger,                             &
                      DT,DX,DXSQ,rhoe,                     &
                      XLV0,XLV1,XLS0,XLS1,CP,R,G,          &
                      EP2,SVP1,SVP2,SVP3,SVPT0,            &
                      DQDT,DQIDT,DQCDT,DQRDT,DQSDT,DTDT,   &
                      RAINCV,PRATEC,NCA,                   &
                      F_QI,F_QS,warm_rain,                 &
                      CUTOP,CUBOT,CUDT,                    &
                      ids,ide, jds,jde, kds,kde,           &
                      ims,ime, jms,jme, kms,kme,           &
                      its,ite, jts,jte, kts,kte,           &

                      cldfra_dp_KF,cldfra_sh_KF,           &
                      qc_KF,qi_KF                          )




      IMPLICIT NONE

      INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde, &
                                ims,ime, jms,jme, kms,kme, &
                                its,ite, jts,jte, kts,kte, &
                                I,J
          
      INTEGER, INTENT(IN   ) ::  trigger

      LOGICAL, INTENT(IN   ) :: F_QI, F_QS

      LOGICAL, INTENT(IN   ) :: warm_rain

      REAL, DIMENSION( kts:kte ),                          &
            INTENT(IN   ) ::                           U0, &
                                                       V0, &
                                                 TPART_H0, &
                                                 TPART_V0, &
                                                       T0, &
                                                      QV0, &
                                                       P0, &
                                                     rhoe, &
                                                      DZQ, &
                                                  W0AVG1D

      REAL,  INTENT(IN   ) :: DT,DX,DXSQ


      REAL,  INTENT(IN   ) :: XLV0,XLV1,XLS0,XLS1,CP,R,G
      REAL,  INTENT(IN   ) :: EP2,SVP1,SVP2,SVP3,SVPT0


      REAL, DIMENSION( kts:kte ), INTENT(INOUT) ::         &
                                                     DQDT, &
                                                    DQIDT, &
                                                    DQCDT, &
                                                    DQRDT, &
                                                    DQSDT, &
                                                     DTDT

      REAL,    DIMENSION( ims:ime , jms:jme ),             &
            INTENT(INOUT) ::                          NCA


      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),      &
            INTENT(INOUT) ::                 cldfra_dp_KF, &
                                             cldfra_sh_KF, &
                                                    qc_KF, &
                                                    qi_KF

      REAL, DIMENSION( ims:ime , jms:jme ),                &
            INTENT(INOUT) ::                       RAINCV

      REAL, DIMENSION( ims:ime , jms:jme ),                &
            INTENT(INOUT) ::                       PRATEC

      REAL, DIMENSION( ims:ime , jms:jme ),                &
            INTENT(OUT) ::                          CUBOT, &
                                                    CUTOP
      REAL,  INTENT(IN   ) :: CUDT



      REAL, DIMENSION( kts:kte ) ::                        &
            Q0,Z0,TV0,TU,TVU,QU,TZ,TVD,                    &
            QD,QES,THTES,TG,TVG,QG,WU,WD,W0,EMS,EMSD,      &
            UMF,UER,UDR,DMF,DER,DDR,UMF2,UER2,             &
            UDR2,DMF2,DER2,DDR2,DZA,THTA0,THETEE,          &
            THTAU,THETEU,THTAD,THETED,QLIQ,QICE,           &
            QLQOUT,QICOUT,PPTLIQ,PPTICE,DETLQ,DETIC,       &
            DETLQ2,DETIC2,RATIO,RATIO2


      REAL, DIMENSION( kts:kte ) ::                        &
            DOMGDP,EXN,TVQU,DP,RH,EQFRC,WSPD,              &
            QDT,FXM,THTAG,THPA,THFXOUT,                    &
            THFXIN,QPA,QFXOUT,QFXIN,QLPA,QLFXIN,           &
            QLFXOUT,QIPA,QIFXIN,QIFXOUT,QRPA,              &
            QRFXIN,QRFXOUT,QSPA,QSFXIN,QSFXOUT,            &
            QL0,QLG,QI0,QIG,QR0,QRG,QS0,QSG


      REAL, DIMENSION( kts:kte+1 ) :: OMG
      REAL, DIMENSION( kts:kte ) :: RAINFB,SNOWFB
      REAL, DIMENSION( kts:kte ) ::                        &
            CLDHGT,QSD,DILFRC,DDILFRC,TKE,TGU,QGU,THTEEG



      REAL    :: P00,T00,RLF,RHIC,RHBC,PIE,         &
                 TTFRZ,TBFRZ,C5,RATE
      REAL    :: GDRY,ROCP,ALIQ,BLIQ,                      &
                 CLIQ,DLIQ
      REAL    :: FBFRC,P300,DPTHMX,THMIX,QMIX,ZMIX,PMIX,   &
                 ROCPQ,TMIX,EMIX,TLOG,TDPT,TLCL,TVLCL,     &
                 CPORQ,PLCL,ES,DLP,TENV,QENV,TVEN,TVBAR,   &
                 ZLCL,WKL,WABS,TRPPT,WSIGNE,DTLCL,GDT,WLCL,&
                 TVAVG,QESE,WTW,RHOLCL,AU0,VMFLCL,UPOLD,   &
                 UPNEW,ABE,WKLCL,TTEMP,FRC1,   &
                 QNEWIC,RL,R1,QNWFRZ,EFFQ,BE,BOTERM,ENTERM,&
                 DZZ,UDLBE,REI,EE2,UD2,TTMP,F1,F2,         &
                 THTTMP,QTMP,TMPLIQ,TMPICE,TU95,TU10,EE1,  &
                 UD1,DPTT,QNEWLQ,DUMFDP,EE,TSAT,           &
                 THTA,VCONV,TIMEC,SHSIGN,VWS,PEF, &
                 CBH,RCBH,PEFCBH,PEFF,PEFF2,TDER,THTMIN,   &
                 DTMLTD,QS,TADVEC,DPDD,FRC,DPT,RDD,A1,     &
                 DSSDT,DTMP,T1RH,QSRH,PPTFLX,CPR,CNDTNF,   &
                 UPDINC,AINCM2,DEVDMF,PPR,RCED,DPPTDF,     &
                 DMFLFS,DMFLFS2,RCED2,DDINC,AINCMX,AINCM1, &
                 AINC,TDER2,PPTFL2,FABE,STAB,DTT,DTT1,     &
                 DTIME,TMA,TMB,TMM,BCOEFF,ACOEFF,QVDIFF,   &
                 TOPOMG,CPM,DQ,ABEG,DABE,DFDA,FRC2,DR,     &
                 UDFRC,TUC,QGS,RH0,RHG,QINIT,QFNL,ERR2,    &
                 RELERR,RLC,RLS,RNC,FABEOLD,AINCOLD,UEFRC, &
                 DDFRC,TDC,DEFRC,RHBAR,DMFFRC,DPMIN,DILBE
   REAL    ::    ASTRT,TP,VALUE,AINTRP,TKEMAX,QFRZ,&
                 QSS,PPTMLT,DTMELT,RHH,EVAC,BINC

      INTEGER :: INDLU,NU,NUCHM,NNN,KLFS
   REAL    :: CHMIN,PM15,CHMAX,DTRH,RAD,DPPP
   REAL    :: TVDIFF,DTTOT,ABSOMG,ABSOMGTC,FRDP

   REAL    :: xcldfra,UMF_new

      INTEGER :: KX,K,KL

      INTEGER :: NCHECK
      INTEGER, DIMENSION (kts:kte) :: KCHECK

      INTEGER :: ISTOP,ML,L5,KMIX,LOW,                     &
                 LC,MXLAYR,LLFC,NLAYRS,NK,                 &
                 KPBL,KLCL,LCL,LET,IFLAG,                  &
                 NK1,LTOP,NJ,LTOP1,                        &
                 LTOPM1,LVF,KSTART,KMIN,LFS,               &
                 ND,NIC,LDB,LDT,ND1,NDK,                   &
                 NM,LMAX,NCOUNT,NOITR,                     &
                 NSTEP,NTC,NCHM,ISHALL,NSHALL
      LOGICAL :: IPRNT
      REAL :: u00,qslcl,rhlcl,dqssdt    
      CHARACTER*1024 message

      DATA P00,T00/1.E5,273.16/
      DATA RLF/3.339E5/
      DATA RHIC,RHBC/1.,0.90/
      DATA PIE,TTFRZ,TBFRZ,C5/3.141592654,268.16,248.16,1.0723E-3/
      DATA RATE/0.03/   



      IPRNT=.FALSE.
      GDRY=-G/CP
      ROCP=R/CP
      NSHALL = 0
      KL=kte
      KX=kte





      ALIQ = SVP1*1000.
      BLIQ = SVP2
      CLIQ = SVP2*SVPT0
      DLIQ = SVP3








      FBFRC=0.0                                        

      NCHM = 0
      ISHALL = 0
      DPMIN = 5.E3

      P300=P0(1)-30000.








      ML=0 



      DO K=1,KX




         ES=ALIQ*EXP((BLIQ*T0(K)-CLIQ)/(T0(K)-DLIQ))
         QES(K)=0.622*ES/(P0(K)-ES)
         Q0(K)=AMIN1(QES(K),QV0(K))
         Q0(K)=AMAX1(0.000001,Q0(K))
         QL0(K)=0.
         QI0(K)=0.
         QR0(K)=0.
         QS0(K)=0.
         RH(K) = Q0(K)/QES(K)
         DILFRC(K) = 1.
         TV0(K)=T0(K)*(1.+0.608*Q0(K))


         DP(K)=rhoe(k)*g*DZQ(k)



         TKE(K) = 0.
         CLDHGT(K) = 0.

         IF(P0(K).GE.0.5*P0(1))L5=K
         IF(P0(K).GE.P300)LLFC=K
      ENDDO


        Z0(1)=.5*DZQ(1)

        DO K=2,KL
          Z0(K)=Z0(K-1)+.5*(DZQ(K)+DZQ(K-1))
          DZA(K-1)=Z0(K)-Z0(K-1)
        ENDDO   
        DZA(KL)=0.









       NCHECK = 1
       KCHECK(NCHECK)=1
       PM15 = P0(1)-15.E2
       DO K=2,LLFC
         IF(P0(K).LT.PM15)THEN
           NCHECK = NCHECK+1
           KCHECK(NCHECK) = K
           PM15 = PM15-15.E2
         ENDIF
       ENDDO

       NU=0
       NUCHM=0
usl:   DO
           NU = NU+1
           IF(NU.GT.NCHECK)THEN 
             IF(ISHALL.EQ.1)THEN
               CHMAX = 0.
               NCHM = 0
               DO NK = 1,NCHECK
                 NNN=KCHECK(NK)
                 IF(CLDHGT(NNN).GT.CHMAX)THEN
                   NCHM = NNN
                   NUCHM = NK
                   CHMAX = CLDHGT(NNN)
                 ENDIF
               ENDDO
               NU = NUCHM-1
               FBFRC=1.
               CYCLE usl
             ELSE
               RETURN
             ENDIF
           ENDIF      
           KMIX = KCHECK(NU)
           LOW=KMIX

           LC = LOW






           NLAYRS=0
           DPTHMX=0.
           NK=LC-1
           IF ( NK+1 .LT. KTS ) THEN
             WRITE(message,*)'WOULD GO OFF BOTTOM: KF_ETA_PARA I,J,NK',I,J,NK
             CALL wrf_message (TRIM(message)) 
           ELSE
             DO 
               NK=NK+1   
               IF ( NK .GT. KTE ) THEN
                 WRITE(message,*)'WOULD GO OFF TOP: KF_ETA_PARA I,J,DPTHMX,DPMIN',I,J,DPTHMX,DPMIN
                 CALL wrf_message (TRIM(message))
                 EXIT
               ENDIF
               DPTHMX=DPTHMX+DP(NK)
               NLAYRS=NLAYRS+1
               IF(DPTHMX.GT.DPMIN)THEN
                 EXIT 
               ENDIF
             END DO    
           ENDIF
           IF(DPTHMX.LT.DPMIN)THEN 
             RETURN
           ENDIF
           KPBL=LC+NLAYRS-1   






           TMIX=0.
           QMIX=0.
           ZMIX=0.
           PMIX=0.






           DO NK=LC,KPBL
             TMIX=TMIX+DP(NK)*T0(NK)
             QMIX=QMIX+DP(NK)*Q0(NK)
             ZMIX=ZMIX+DP(NK)*Z0(NK)
             PMIX=PMIX+DP(NK)*P0(NK)
           ENDDO   

          TMIX=TMIX/DPTHMX
          QMIX=QMIX/DPTHMX
          ZMIX=ZMIX/DPTHMX
          PMIX=PMIX/DPTHMX
          EMIX=QMIX*PMIX/(0.622+QMIX)






          astrt=1.e-3
          ainc=0.075
          a1=emix/aliq
          tp=(a1-astrt)/ainc
          indlu=int(tp)+1
          value=(indlu-1)*ainc+astrt
          aintrp=(a1-value)/ainc
          tlog=aintrp*alu(indlu+1)+(1-aintrp)*alu(indlu)
          TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)
          TLCL=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(TMIX-T00))*(TMIX-TDPT)
          TLCL=AMIN1(TLCL,TMIX)
          TVLCL=TLCL*(1.+0.608*QMIX)
          ZLCL = ZMIX+(TLCL-TMIX)/GDRY
     
     
     
     
     
     
     
     
     
     
     

       DO NK = LC, KL
         KLCL = NK
         IF ( ZLCL.LE.Z0(NK) )  EXIT
     END DO
     IF ( ZLCL.GT.Z0(KL) )  RETURN

          K=KLCL-1

          DLP=(ZLCL-Z0(K))/(Z0(KLCL)-Z0(K))



          TENV=T0(K)+(T0(KLCL)-T0(K))*DLP
          QENV=Q0(K)+(Q0(KLCL)-Q0(K))*DLP
          TVEN=TENV*(1.+0.608*QENV)











          IF(ZLCL.LT.2.E3)THEN        
            WKLCL=0.02*ZLCL/2.E3
          ELSE
            WKLCL=0.02                
          ENDIF
          WKL=(W0AVG1D(K)+(W0AVG1D(KLCL)-W0AVG1D(K))*DLP)*DX/25.E3-WKLCL
          IF(WKL.LT.0.0001)THEN
            DTLCL=0.
          ELSE 
            DTLCL=4.64*WKL**0.33      
          ENDIF


           IF(trigger.eq.2) then
              DTLCL=amax1(TPART_H0(KLCL)+TPART_V0(KLCL),0.0)
           ENDIF


        dtrh = 0.
	if (trigger  .eq. 3) then






          U00 = 0.75
          IF(U00.lt.1.)THEN
            QSLCL=QES(K)+(QES(KLCL)-QES(K))*DLP
            RHLCL = QENV/QSLCL
            DQSSDT = QMIX*(CLIQ-BLIQ*DLIQ)/((TLCL-DLIQ)*(TLCL-DLIQ))
            IF(RHLCL.ge.0.75 .and. RHLCL.le.0.95)then
              DTRH = 0.25*(RHLCL-0.75)*QMIX/DQSSDT
            ELSEIF(RHLCL.GT.0.95)THEN
              DTRH = (1./RHLCL-1.)*QMIX/DQSSDT
            ELSE
               DTRH = 0.
            ENDIF
          ENDIF   
        endif   



 
trigger2:  IF(TLCL+DTLCL+DTRH.LT.TENV)THEN   



            CYCLE usl

          ELSE                            





            CALL ENVIRTHT(PMIX,TMIX,QMIX,THETEU(K),ALIQ,BLIQ,CLIQ,DLIQ)



            DTTOT = DTLCL+DTRH
            IF(DTTOT.GT.1.E-4)THEN
              GDT=2.*G*DTTOT*500./TVEN     
              WLCL=1.+0.5*SQRT(GDT)
              WLCL = AMIN1(WLCL,3.)
            ELSE
              WLCL=1.
            ENDIF
            PLCL=P0(K)+(P0(KLCL)-P0(K))*DLP
            WTW=WLCL*WLCL

            TVLCL=TLCL*(1.+0.608*QMIX)
            RHOLCL=PLCL/(R*TVLCL)

            LCL=KLCL
            LET=LCL

            IF(WKL.LT.0.)THEN
              RAD = 1000.
            ELSEIF(WKL.GT.0.1)THEN
              RAD = 2000.
            ELSE
              RAD = 1000.+1000*WKL/0.1
            ENDIF











            WU(K)=WLCL
            AU0=0.01*DXSQ
            UMF(K)=RHOLCL*AU0
            VMFLCL=UMF(K)
            UPOLD=VMFLCL
            UPNEW=UPOLD






            RATIO2(K)=0.
            UER(K)=0.
            ABE=0.
            TRPPT=0.
            TU(K)=TLCL
            TVU(K)=TVLCL
            QU(K)=QMIX
            EQFRC(K)=1.
            QLIQ(K)=0.
            QICE(K)=0.
            QLQOUT(K)=0.
            QICOUT(K)=0.
            DETLQ(K)=0.
            DETIC(K)=0.
            PPTLIQ(K)=0.
            PPTICE(K)=0.
            IFLAG=0







            TTEMP=TTFRZ








            EE1=1.
            UD1=0.
            REI = 0.
            DILBE = 0.
updraft:    DO NK=K,KL-1
              NK1=NK+1
              RATIO2(NK1)=RATIO2(NK)
              FRC1=0.
              TU(NK1)=T0(NK1)
              THETEU(NK1)=THETEU(NK)
              QU(NK1)=QU(NK)
              QLIQ(NK1)=QLIQ(NK)
              QICE(NK1)=QICE(NK)
              call tpmix2(p0(nk1),theteu(nk1),tu(nk1),qu(nk1),qliq(nk1),        &
                     qice(nk1),qnewlq,qnewic,XLV1,XLV0)








              IF(TU(NK1).LE.TTFRZ)THEN
                IF(TU(NK1).GT.TBFRZ)THEN
                  IF(TTEMP.GT.TTFRZ)TTEMP=TTFRZ
                  FRC1=(TTEMP-TU(NK1))/(TTEMP-TBFRZ)
                ELSE
                  FRC1=1.
                  IFLAG=1
                ENDIF
                TTEMP=TU(NK1)




                QFRZ = (QLIQ(NK1)+QNEWLQ)*FRC1
                QNEWIC=QNEWIC+QNEWLQ*FRC1
                QNEWLQ=QNEWLQ-QNEWLQ*FRC1
                QICE(NK1) = QICE(NK1)+QLIQ(NK1)*FRC1
                QLIQ(NK1) = QLIQ(NK1)-QLIQ(NK1)*FRC1
                CALL DTFRZNEW(TU(NK1),P0(NK1),THETEU(NK1),QU(NK1),QFRZ,         &
                          QICE(NK1),ALIQ,BLIQ,CLIQ,DLIQ)
              ENDIF
              TVU(NK1)=TU(NK1)*(1.+0.608*QU(NK1))



              IF(NK.EQ.K)THEN
                BE=(TVLCL+TVU(NK1))/(TVEN+TV0(NK1))-1.
                BOTERM=2.*(Z0(NK1)-ZLCL)*G*BE/1.5
                DZZ=Z0(NK1)-ZLCL
              ELSE
                BE=(TVU(NK)+TVU(NK1))/(TV0(NK)+TV0(NK1))-1.
                BOTERM=2.*DZA(NK)*G*BE/1.5
                DZZ=DZA(NK)
              ENDIF
              ENTERM=2.*REI*WTW/UPOLD

              CALL CONDLOAD(QLIQ(NK1),QICE(NK1),WTW,DZZ,BOTERM,ENTERM,      &
                        RATE,QNEWLQ,QNEWIC,QLQOUT(NK1),QICOUT(NK1),G)




              IF(WTW.LT.1.E-3)THEN
                EXIT
              ELSE
                WU(NK1)=SQRT(WTW)
              ENDIF


              CALL ENVIRTHT(P0(NK1),T0(NK1),Q0(NK1),THETEE(NK1),ALIQ,BLIQ,CLIQ,DLIQ)



              REI=VMFLCL*DP(NK1)*0.03/RAD          
              TVQU(NK1)=TU(NK1)*(1.+0.608*QU(NK1)-QLIQ(NK1)-QICE(NK1))
              IF(NK.EQ.K)THEN
                DILBE=((TVLCL+TVQU(NK1))/(TVEN+TV0(NK1))-1.)*DZZ
              ELSE
                DILBE=((TVQU(NK)+TVQU(NK1))/(TV0(NK)+TV0(NK1))-1.)*DZZ
              ENDIF
              IF(DILBE.GT.0.)ABE=ABE+DILBE*G




              IF(TVQU(NK1).LE.TV0(NK1))THEN    
                EE2=0.5       
                UD2=1.
                EQFRC(NK1)=0.
              ELSE
                LET=NK1
                TTMP=TVQU(NK1)



                F1=0.95
                F2=1.-F1
                THTTMP=F1*THETEE(NK1)+F2*THETEU(NK1)
                QTMP=F1*Q0(NK1)+F2*QU(NK1)
                TMPLIQ=F2*QLIQ(NK1)
                TMPICE=F2*QICE(NK1)
                call tpmix2(p0(nk1),thttmp,ttmp,qtmp,tmpliq,tmpice,        &
                           qnewlq,qnewic,XLV1,XLV0)
                TU95=TTMP*(1.+0.608*QTMP-TMPLIQ-TMPICE)
                IF(TU95.GT.TV0(NK1))THEN
                  EE2=1.
                  UD2=0.
                  EQFRC(NK1)=1.0
                ELSE
                  F1=0.10
                  F2=1.-F1
                  THTTMP=F1*THETEE(NK1)+F2*THETEU(NK1)
                  QTMP=F1*Q0(NK1)+F2*QU(NK1)
                  TMPLIQ=F2*QLIQ(NK1)
                  TMPICE=F2*QICE(NK1)
                  call tpmix2(p0(nk1),thttmp,ttmp,qtmp,tmpliq,tmpice,        &
                               qnewlq,qnewic,XLV1,XLV0)
                  TU10=TTMP*(1.+0.608*QTMP-TMPLIQ-TMPICE)
                  TVDIFF = ABS(TU10-TVQU(NK1))
                  IF(TVDIFF.LT.1.e-3)THEN
                    EE2=1.
                    UD2=0.
                    EQFRC(NK1)=1.0
                  ELSE
                    EQFRC(NK1)=(TV0(NK1)-TVQU(NK1))*F1/(TU10-TVQU(NK1))
                    EQFRC(NK1)=AMAX1(0.0,EQFRC(NK1))
                    EQFRC(NK1)=AMIN1(1.0,EQFRC(NK1))
                    IF(EQFRC(NK1).EQ.1)THEN
                      EE2=1.
                      UD2=0.
                    ELSEIF(EQFRC(NK1).EQ.0.)THEN
                      EE2=0.
                      UD2=1.
                    ELSE




                      CALL PROF5(EQFRC(NK1),EE2,UD2)
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF                            





              EE2 = AMAX1(EE2,0.5)
              UD2 = 1.5*UD2
              UER(NK1)=0.5*REI*(EE1+EE2)
              UDR(NK1)=0.5*REI*(UD1+UD2)




              IF(UMF(NK)-UDR(NK1).LT.10.)THEN





                IF(DILBE.GT.0.)THEN
                  ABE=ABE-DILBE*G
                ENDIF
                LET=NK

                EXIT 
              ELSE
                EE1=EE2
                UD1=UD2
                UPOLD=UMF(NK)-UDR(NK1)
                UPNEW=UPOLD+UER(NK1)
                UMF(NK1)=UPNEW
                DILFRC(NK1) = UPNEW/UPOLD




                DETLQ(NK1)=QLIQ(NK1)*UDR(NK1)
                DETIC(NK1)=QICE(NK1)*UDR(NK1)
                QDT(NK1)=QU(NK1)
                QU(NK1)=(UPOLD*QU(NK1)+UER(NK1)*Q0(NK1))/UPNEW
                THETEU(NK1)=(THETEU(NK1)*UPOLD+THETEE(NK1)*UER(NK1))/UPNEW
                QLIQ(NK1)=QLIQ(NK1)*UPOLD/UPNEW
                QICE(NK1)=QICE(NK1)*UPOLD/UPNEW






                PPTLIQ(NK1)=QLQOUT(NK1)*UMF(NK)
                PPTICE(NK1)=QICOUT(NK1)*UMF(NK)

                TRPPT=TRPPT+PPTLIQ(NK1)+PPTICE(NK1)
                IF(NK1.LE.KPBL)UER(NK1)=UER(NK1)+VMFLCL*DP(NK1)/DPTHMX
              ENDIF

            END DO updraft









            LTOP=NK
            CLDHGT(LC)=Z0(LTOP)-ZLCL 






            IF(TLCL.GT.293.)THEN
              CHMIN = 4.E3
            ELSEIF(TLCL.LE.293. .and. TLCL.GE.273)THEN
              CHMIN = 2.E3 + 100.*(TLCL-273.)
            ELSEIF(TLCL.LT.273.)THEN
              CHMIN = 2.E3
            ENDIF

            DO NK=K,LTOP
              qc_KF(I,NK,J)=QLIQ(NK)
              qi_KF(I,NK,J)=QICE(NK)
            END DO

















            IF(LTOP.LE.KLCL .or. LTOP.LE.KPBL .or. LET+1.LE.KPBL)THEN  
              CLDHGT(LC)=0.
              DO NK=K,LTOP
                UMF(NK)=0.
                UDR(NK)=0.
                UER(NK)=0.
                DETLQ(NK)=0.
                DETIC(NK)=0.
                PPTLIQ(NK)=0.
                PPTICE(NK)=0.

                cldfra_dp_KF(I,NK,J)=0.
                cldfra_sh_KF(I,NK,J)=0.
                qc_KF(I,NK,J)=0.
                qi_KF(I,NK,J)=0.
              ENDDO

            ELSEIF(CLDHGT(LC).GT.CHMIN .and. ABE.GT.1)THEN      
              ISHALL=0

              DO NK=K,LTOP
                cldfra_sh_KF(I,NK,J)=0.
              ENDDO
              EXIT usl
            ELSE


              ISHALL = 1

              DO NK=K,LTOP
                cldfra_dp_KF(I,NK,J)=0.
              ENDDO
              IF(NU.EQ.NUCHM)THEN
                EXIT usl               
              ELSE

                DO NK=K,LTOP
                  UMF(NK)=0.
                  UDR(NK)=0.
                  UER(NK)=0.
                  DETLQ(NK)=0.
                  DETIC(NK)=0.
                  PPTLIQ(NK)=0.
                  PPTICE(NK)=0.

                  cldfra_dp_KF(I,NK,J)=0.
                  cldfra_sh_KF(I,NK,J)=0.
                  qc_KF(I,NK,J)=0.
                  qi_KF(I,NK,J)=0.
                ENDDO
              ENDIF
            ENDIF
          ENDIF trigger2
        END DO usl
    IF(ISHALL.EQ.1)THEN
      KSTART=MAX0(KPBL,KLCL)
      LET=KSTART
    endif




    IF(LET.EQ.LTOP)THEN
      UDR(LTOP)=UMF(LTOP)+UDR(LTOP)-UER(LTOP)
      DETLQ(LTOP)=QLIQ(LTOP)*UDR(LTOP)*UPNEW/UPOLD
      DETIC(LTOP)=QICE(LTOP)*UDR(LTOP)*UPNEW/UPOLD
      UER(LTOP)=0.
      UMF(LTOP)=0.
    ELSE 



      DPTT=0.
      DO NJ=LET+1,LTOP
        DPTT=DPTT+DP(NJ)
      ENDDO
      DUMFDP=UMF(LET)/DPTT




      DO NK=LET+1,LTOP







        IF(NK.EQ.LTOP)THEN
          UDR(NK) = UMF(NK-1)
          UER(NK) = 0.
          DETLQ(NK) = UDR(NK)*QLIQ(NK)*DILFRC(NK)
          DETIC(NK) = UDR(NK)*QICE(NK)*DILFRC(NK)
        ELSE
          UMF(NK)=UMF(NK-1)-DP(NK)*DUMFDP
          UER(NK)=UMF(NK)*(1.-1./DILFRC(NK))
          UDR(NK)=UMF(NK-1)-UMF(NK)+UER(NK)
          DETLQ(NK)=UDR(NK)*QLIQ(NK)*DILFRC(NK)
          DETIC(NK)=UDR(NK)*QICE(NK)*DILFRC(NK)
        ENDIF
        IF(NK.GE.LET+2)THEN
          TRPPT=TRPPT-PPTLIQ(NK)-PPTICE(NK)
          PPTLIQ(NK)=UMF(NK-1)*QLQOUT(NK)
          PPTICE(NK)=UMF(NK-1)*QICOUT(NK)
          TRPPT=TRPPT+PPTLIQ(NK)+PPTICE(NK)
        ENDIF
      ENDDO
    ENDIF



    DO NK=1,LTOP
      IF(T0(NK).GT.T00)ML=NK
    ENDDO
    DO NK=1,K
      IF(NK.GE.LC)THEN
        IF(NK.EQ.LC)THEN
          UMF(NK)=VMFLCL*DP(NK)/DPTHMX
          UER(NK)=VMFLCL*DP(NK)/DPTHMX
        ELSEIF(NK.LE.KPBL)THEN
          UER(NK)=VMFLCL*DP(NK)/DPTHMX
          UMF(NK)=UMF(NK-1)+UER(NK)
        ELSE
          UMF(NK)=VMFLCL
          UER(NK)=0.
        ENDIF
        TU(NK)=TMIX+(Z0(NK)-ZMIX)*GDRY
        QU(NK)=QMIX
        WU(NK)=WLCL
      ELSE
        TU(NK)=0.
        QU(NK)=0.
        UMF(NK)=0.
        WU(NK)=0.
        UER(NK)=0.

        cldfra_dp_KF(I,NK,J)=0.
        cldfra_sh_KF(I,NK,J)=0.
        qc_KF(I,NK,J)=0.
        qi_KF(I,NK,J)=0.
      ENDIF
      UDR(NK)=0.
      QDT(NK)=0.
      QLIQ(NK)=0.
      QICE(NK)=0.
      QLQOUT(NK)=0.
      QICOUT(NK)=0.
      PPTLIQ(NK)=0.
      PPTICE(NK)=0.
      DETLQ(NK)=0.
      DETIC(NK)=0.
      RATIO2(NK)=0.
      CALL ENVIRTHT(P0(NK),T0(NK),Q0(NK),THETEE(NK),ALIQ,BLIQ,CLIQ,DLIQ)
      EQFRC(NK)=1.0
    ENDDO

      LTOP1=LTOP+1
      LTOPM1=LTOP-1



      DO NK=LTOP1,KX
        UMF(NK)=0.
        UDR(NK)=0.
        UER(NK)=0.
        QDT(NK)=0.
        QLIQ(NK)=0.
        QICE(NK)=0.
        QLQOUT(NK)=0.
        QICOUT(NK)=0.
        DETLQ(NK)=0.
        DETIC(NK)=0.
        PPTLIQ(NK)=0.
        PPTICE(NK)=0.
        IF(NK.GT.LTOP1)THEN
          TU(NK)=0.
          QU(NK)=0.
          WU(NK)=0.

          cldfra_dp_KF(I,NK,J)=0.
          cldfra_sh_KF(I,NK,J)=0.
          qc_KF(I,NK,J)=0.
          qi_KF(I,NK,J)=0.
        ENDIF
        THTA0(NK)=0.
        THTAU(NK)=0.
        EMS(NK)=0.
        EMSD(NK)=0.
        TG(NK)=T0(NK)
        QG(NK)=Q0(NK)
        QLG(NK)=0.
        QIG(NK)=0.
        QRG(NK)=0.
        QSG(NK)=0.
        OMG(NK)=0.
      ENDDO
        OMG(KX+1)=0.
        DO NK=1,LTOP
          EMS(NK)=DP(NK)*DXSQ/G
          EMSD(NK)=1./EMS(NK)



          EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*QDT(NK)))
          THTAU(NK)=TU(NK)*EXN(NK)
          EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*Q0(NK)))
          THTA0(NK)=T0(NK)*EXN(NK)
          DDILFRC(NK) = 1./DILFRC(NK)
          OMG(NK)=0.
        ENDDO










        WSPD(KLCL)=SQRT(U0(KLCL)*U0(KLCL)+V0(KLCL)*V0(KLCL))
        WSPD(L5)=SQRT(U0(L5)*U0(L5)+V0(L5)*V0(L5))
        WSPD(LTOP)=SQRT(U0(LTOP)*U0(LTOP)+V0(LTOP)*V0(LTOP))
        VCONV=.5*(WSPD(KLCL)+WSPD(L5))


        TIMEC=DX/VCONV
        TADVEC=TIMEC
        TIMEC=AMAX1(1800.,TIMEC)         
        TIMEC=AMIN1(3600.,TIMEC)
        IF(ISHALL.EQ.1)TIMEC=2400.       
        NIC=NINT(TIMEC/DT)
        TIMEC=FLOAT(NIC)*DT



        IF(WSPD(LTOP).GT.WSPD(KLCL))THEN
          SHSIGN=1.
        ELSE
          SHSIGN=-1.
        ENDIF
        VWS=(U0(LTOP)-U0(KLCL))*(U0(LTOP)-U0(KLCL))+(V0(LTOP)-V0(KLCL))*   &
            (V0(LTOP)-V0(KLCL))
        VWS=1.E3*SHSIGN*SQRT(VWS)/(Z0(LTOP)-Z0(LCL))
        PEF=1.591+VWS*(-.639+VWS*(9.53E-2-VWS*4.96E-3))
        PEF=AMAX1(PEF,.2)
        PEF=AMIN1(PEF,.9)



        CBH=(ZLCL-Z0(1))*3.281E-3
        IF(CBH.LT.3.)THEN
          RCBH=.02
        ELSE
          RCBH=.96729352+CBH*(-.70034167+CBH*(.162179896+CBH*(-            &
               1.2569798E-2+CBH*(4.2772E-4-CBH*5.44E-6))))
        ENDIF
        IF(CBH.GT.25)RCBH=2.4
        PEFCBH=1./(1.+RCBH)
        PEFCBH=AMIN1(PEFCBH,.9)



        PEFF=.5*(PEF+PEFCBH)
        PEFF2 = PEFF                                
       IF(IPRNT)THEN  

         WRITE(message,1035)PEF,PEFCBH,LC,LET,WKL,VWS
         CALL wrf_message( message )

       endif     








       TDER=0.
 devap:IF(ISHALL.EQ.1)THEN
         LFS = 1
       ELSE





         KSTART=KPBL+1                                
         KLFS = LET-1
         DO NK = KSTART+1,KL
           DPPP = P0(KSTART)-P0(NK)

           IF(DPPP.GT.150.E2)THEN
             KLFS = NK
             EXIT 
           ENDIF
         ENDDO
         KLFS = MIN0(KLFS,LET-1)
         LFS = KLFS





        IF((P0(KSTART)-P0(LFS)).GT.50.E2)THEN
          THETED(LFS) = THETEE(LFS)
          QD(LFS) = Q0(LFS)



          call tpmix2dd(p0(lfs),theted(lfs),tz(lfs),qss,i,j)
          THTAD(LFS)=TZ(LFS)*(P00/P0(LFS))**(0.2854*(1.-0.28*QSS))



          TVD(LFS)=TZ(LFS)*(1.+0.608*QSS)
          RDD=P0(LFS)/(R*TVD(LFS))
          A1=(1.-PEFF)*AU0
          DMF(LFS)=-A1*RDD
          DER(LFS)=DMF(LFS)
          DDR(LFS)=0.
          RHBAR = RH(LFS)*DP(LFS)
          DPTT = DP(LFS)
          DO ND = LFS-1,KSTART,-1
            ND1 = ND+1
            DER(ND)=DER(LFS)*EMS(ND)/EMS(LFS)
            DDR(ND)=0.
            DMF(ND)=DMF(ND1)+DER(ND)
            THETED(ND)=(THETED(ND1)*DMF(ND1)+THETEE(ND)*DER(ND))/DMF(ND)
            QD(ND)=(QD(ND1)*DMF(ND1)+Q0(ND)*DER(ND))/DMF(ND)    
            DPTT = DPTT+DP(ND)
            RHBAR = RHBAR+RH(ND)*DP(ND)
          ENDDO
          RHBAR = RHBAR/DPTT
          DMFFRC = 2.*(1.-RHBAR)    
          DPDD = 0.



          pptmlt = 0.
          DO NK = KLCL,LTOP
            PPTMLT = PPTMLT+PPTICE(NK)
          ENDDO
          if(lc.lt.ml)then



            DTMELT = RLF*PPTMLT/(CP*UMF(KLCL))
          else
            DTMELT = 0.
          endif
          LDT = MIN0(LFS-1,KSTART-1)

          call tpmix2dd(p0(kstart),theted(kstart),tz(kstart),qss,i,j)

          tz(kstart) = tz(kstart)-dtmelt
          ES=ALIQ*EXP((BLIQ*TZ(KSTART)-CLIQ)/(TZ(KSTART)-DLIQ))
          QSS=0.622*ES/(P0(KSTART)-ES)
          THETED(KSTART)=TZ(KSTART)*(1.E5/P0(KSTART))**(0.2854*(1.-0.28*QSS))*    &
                EXP((3374.6525/TZ(KSTART)-2.5403)*QSS*(1.+0.81*QSS))

          LDT = MIN0(LFS-1,KSTART-1)
          DO ND = LDT,1,-1
            DPDD = DPDD+DP(ND)
            THETED(ND) = THETED(KSTART)
            QD(ND)     = QD(KSTART)       



            call tpmix2dd(p0(nd),theted(nd),tz(nd),qss,i,j)
            qsd(nd) = qss



            RHH = 1.-0.2/1000.*(Z0(KSTART)-Z0(ND))



            IF(RHH.LT.1.)THEN
              DSSDT=(CLIQ-BLIQ*DLIQ)/((TZ(ND)-DLIQ)*(TZ(ND)-DLIQ))
              RL=XLV0-XLV1*TZ(ND)
              DTMP=RL*QSS*(1.-RHH)/(CP+RL*RHH*QSS*DSSDT)
              T1RH=TZ(ND)+DTMP
              ES=RHH*ALIQ*EXP((BLIQ*T1RH-CLIQ)/(T1RH-DLIQ))
              QSRH=0.622*ES/(P0(ND)-ES)




              IF(QSRH.LT.QD(ND))THEN
                QSRH=QD(ND)
                T1RH=TZ(ND)+(QSS-QSRH)*RL/CP
              ENDIF
              TZ(ND)=T1RH
              QSS=QSRH
              QSD(ND) = QSS
            ENDIF         
            TVD(nd) = tz(nd)*(1.+0.608*qsd(nd))
            IF(TVD(ND).GT.TV0(ND).OR.ND.EQ.1)THEN
              LDB=ND
              EXIT
            ENDIF
          ENDDO
          IF((P0(LDB)-P0(LFS)) .gt. 50.E2)THEN   
            DO ND=LDT,LDB,-1
              ND1 = ND+1
              DDR(ND) = -DMF(KSTART)*DP(ND)/DPDD
              DER(ND) = 0.
              DMF(ND) = DMF(ND1)+DDR(ND)
              TDER=TDER+(QSD(nd)-QD(ND))*DDR(ND)
              QD(ND)=QSD(nd)
              THTAD(ND)=TZ(ND)*(P00/P0(ND))**(0.2854*(1.-0.28*QD(ND)))
            ENDDO
          ENDIF
        ENDIF
      ENDIF devap




d_mf:   IF(TDER.LT.1.)THEN


          PPTFLX=TRPPT
          CPR=TRPPT
          TDER=0.
          CNDTNF=0.
          UPDINC=1.
          LDB=LFS
          DO NDK=1,LTOP
            DMF(NDK)=0.
            DER(NDK)=0.
            DDR(NDK)=0.
            THTAD(NDK)=0.
            WD(NDK)=0.
            TZ(NDK)=0.
            QD(NDK)=0.
          ENDDO
          AINCM2=100.
        ELSE 
          DDINC = -DMFFRC*UMF(KLCL)/DMF(KSTART)
          UPDINC=1.
          IF(TDER*DDINC.GT.TRPPT)THEN
            DDINC = TRPPT/TDER
          ENDIF
          TDER = TDER*DDINC
          DO NK=LDB,LFS
            DMF(NK)=DMF(NK)*DDINC
            DER(NK)=DER(NK)*DDINC
            DDR(NK)=DDR(NK)*DDINC
          ENDDO
         CPR=TRPPT
         PPTFLX = TRPPT-TDER
         PEFF=PPTFLX/TRPPT
         IF(IPRNT)THEN

           write(message,*)'PRECIP EFFICIENCY =',PEFF
           CALL wrf_message(message)

         ENDIF



















         IF(LDB.GT.1)THEN
           DO NK=1,LDB-1
             DMF(NK)=0.
             DER(NK)=0.
             DDR(NK)=0.
             WD(NK)=0.
             TZ(NK)=0.
             QD(NK)=0.
             THTAD(NK)=0.
           ENDDO
         ENDIF
         DO NK=LFS+1,KX
           DMF(NK)=0.
           DER(NK)=0.
           DDR(NK)=0.
           WD(NK)=0.
           TZ(NK)=0.
           QD(NK)=0.
           THTAD(NK)=0.
         ENDDO
         DO NK=LDT+1,LFS-1
           TZ(NK)=0.
           QD(NK)=0.
           THTAD(NK)=0.
         ENDDO
       ENDIF d_mf





       AINCMX=1000.
       LMAX=MAX0(KLCL,LFS)
       DO NK=LC,LMAX
         IF((UER(NK)-DER(NK)).GT.1.e-3)THEN
           AINCM1=EMS(NK)/((UER(NK)-DER(NK))*TIMEC)
           AINCMX=AMIN1(AINCMX,AINCM1)
         ENDIF
       ENDDO
       AINC=1.
       IF(AINCMX.LT.AINC)AINC=AINCMX





       TDER2=TDER
       PPTFL2=PPTFLX
       DO NK=1,LTOP
         DETLQ2(NK)=DETLQ(NK)
         DETIC2(NK)=DETIC(NK)
         UDR2(NK)=UDR(NK)
         UER2(NK)=UER(NK)
         DDR2(NK)=DDR(NK)
         DER2(NK)=DER(NK)
         UMF2(NK)=UMF(NK)
         DMF2(NK)=DMF(NK)
       ENDDO
       FABE=1.
       STAB=0.95
       NOITR=0
       ISTOP=0

        IF(ISHALL.EQ.1)THEN                              







          TKEMAX = 5.












          EVAC  = 0.5*TKEMAX*0.1


          AINC = EVAC*DPTHMX*DXSQ/(VMFLCL*G*TIMEC)
          TDER=TDER2*AINC
          PPTFLX=PPTFL2*AINC
          DO NK=1,LTOP
            UMF(NK)=UMF2(NK)*AINC
            DMF(NK)=DMF2(NK)*AINC
            DETLQ(NK)=DETLQ2(NK)*AINC
            DETIC(NK)=DETIC2(NK)*AINC
            UDR(NK)=UDR2(NK)*AINC
            UER(NK)=UER2(NK)*AINC
            DER(NK)=DER2(NK)*AINC
            DDR(NK)=DDR2(NK)*AINC
          ENDDO
        ENDIF                                           

iter:     DO NCOUNT=1,10










            DTT=TIMEC
            DO NK=1,LTOP
              DOMGDP(NK)=-(UER(NK)-DER(NK)-UDR(NK)-DDR(NK))*EMSD(NK)
              IF(NK.GT.1)THEN
                OMG(NK)=OMG(NK-1)-DP(NK-1)*DOMGDP(NK-1)
                ABSOMG = ABS(OMG(NK))
                ABSOMGTC = ABSOMG*TIMEC
                FRDP = 0.75*DP(NK-1)
                IF(ABSOMGTC.GT.FRDP)THEN
                  DTT1 = FRDP/ABSOMG
                  DTT=AMIN1(DTT,DTT1)
                ENDIF
              ENDIF
            ENDDO
            DO NK=1,LTOP
              THPA(NK)=THTA0(NK)
              QPA(NK)=Q0(NK)
              NSTEP=NINT(TIMEC/DTT+1)
              DTIME=TIMEC/FLOAT(NSTEP)
              FXM(NK)=OMG(NK)*DXSQ/G
            ENDDO



        DO NTC=1,NSTEP




            DO  NK=1,LTOP
              THFXIN(NK)=0.
              THFXOUT(NK)=0.
              QFXIN(NK)=0.
              QFXOUT(NK)=0.
            ENDDO
            DO NK=2,LTOP
              IF(OMG(NK).LE.0.)THEN
                THFXIN(NK)=-FXM(NK)*THPA(NK-1)
                QFXIN(NK)=-FXM(NK)*QPA(NK-1)
                THFXOUT(NK-1)=THFXOUT(NK-1)+THFXIN(NK)
                QFXOUT(NK-1)=QFXOUT(NK-1)+QFXIN(NK)
              ELSE
                THFXOUT(NK)=FXM(NK)*THPA(NK)
                QFXOUT(NK)=FXM(NK)*QPA(NK)
                THFXIN(NK-1)=THFXIN(NK-1)+THFXOUT(NK)
                QFXIN(NK-1)=QFXIN(NK-1)+QFXOUT(NK)
              ENDIF
            ENDDO



            DO NK=1,LTOP
              THPA(NK)=THPA(NK)+(THFXIN(NK)+UDR(NK)*THTAU(NK)+DDR(NK)*      &
                       THTAD(NK)-THFXOUT(NK)-(UER(NK)-DER(NK))*THTA0(NK))*  &
                       DTIME*EMSD(NK)
              QPA(NK)=QPA(NK)+(QFXIN(NK)+UDR(NK)*QDT(NK)+DDR(NK)*QD(NK)-    &
                      QFXOUT(NK)-(UER(NK)-DER(NK))*Q0(NK))*DTIME*EMSD(NK)
            ENDDO   
          ENDDO   
          DO NK=1,LTOP
            THTAG(NK)=THPA(NK)
            QG(NK)=QPA(NK)
          ENDDO




        DO NK=1,LTOP
          IF(QG(NK).LT.0.)THEN
            IF(NK.EQ.1)THEN                             


              CALL wrf_error_fatal3("<stdin>",1960,&
'QG, QG(NK).LT.0') 
            ENDIF                                       
            NK1=NK+1
            IF(NK.EQ.LTOP)THEN
              NK1=KLCL
            ENDIF
            TMA=QG(NK1)*EMS(NK1)
            TMB=QG(NK-1)*EMS(NK-1)
            TMM=(QG(NK)-1.E-9)*EMS(NK  )
            BCOEFF=-TMM/((TMA*TMA)/TMB+TMB)
            ACOEFF=BCOEFF*TMA/TMB
            TMB=TMB*(1.-BCOEFF)
            TMA=TMA*(1.-ACOEFF)
            IF(NK.EQ.LTOP)THEN
              QVDIFF=(QG(NK1)-TMA*EMSD(NK1))*100./QG(NK1)






            ENDIF
            QG(NK)=1.E-9
            QG(NK1)=TMA*EMSD(NK1)
            QG(NK-1)=TMB*EMSD(NK-1)
          ENDIF
        ENDDO
        TOPOMG=(UDR(LTOP)-UER(LTOP))*DP(LTOP)*EMSD(LTOP)
        IF(ABS(TOPOMG-OMG(LTOP)).GT.1.E-3)THEN



          ISTOP=1
          IPRNT=.TRUE.
          EXIT iter
        ENDIF



        DO NK=1,LTOP
          EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*QG(NK)))
          TG(NK)=THTAG(NK)/EXN(NK)
          TVG(NK)=TG(NK)*(1.+0.608*QG(NK))
        ENDDO
        IF(ISHALL.EQ.1)THEN
          EXIT iter
        ENDIF










          TMIX=0.
          QMIX=0.





          DO NK=LC,KPBL
            TMIX=TMIX+DP(NK)*TG(NK)
            QMIX=QMIX+DP(NK)*QG(NK)  
          ENDDO
          TMIX=TMIX/DPTHMX
          QMIX=QMIX/DPTHMX
          ES=ALIQ*EXP((TMIX*BLIQ-CLIQ)/(TMIX-DLIQ))
          QSS=0.622*ES/(PMIX-ES)



          IF(QMIX.GT.QSS)THEN
            RL=XLV0-XLV1*TMIX
            CPM=CP*(1.+0.887*QMIX)
            DSSDT=QSS*(CLIQ-BLIQ*DLIQ)/((TMIX-DLIQ)*(TMIX-DLIQ))
            DQ=(QMIX-QSS)/(1.+RL*DSSDT/CPM)
            TMIX=TMIX+RL/CP*DQ
            QMIX=QMIX-DQ
            TLCL=TMIX
          ELSE
            QMIX=AMAX1(QMIX,0.)
            EMIX=QMIX*PMIX/(0.622+QMIX)
            astrt=1.e-3
            binc=0.075
            a1=emix/aliq
            tp=(a1-astrt)/binc
            indlu=int(tp)+1
            value=(indlu-1)*binc+astrt
            aintrp=(a1-value)/binc
            tlog=aintrp*alu(indlu+1)+(1-aintrp)*alu(indlu)
            TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)
            TLCL=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(TMIX-T00))*(TMIX-TDPT)
            TLCL=AMIN1(TLCL,TMIX)
          ENDIF
          TVLCL=TLCL*(1.+0.608*QMIX)
          ZLCL = ZMIX+(TLCL-TMIX)/GDRY
          DO NK = LC,KL
            KLCL=NK
            IF(ZLCL.LE.Z0(NK))THEN
              EXIT 
            ENDIF
          ENDDO
          K=KLCL-1
          DLP=(ZLCL-Z0(K))/(Z0(KLCL)-Z0(K))



          TENV=TG(K)+(TG(KLCL)-TG(K))*DLP
          QENV=QG(K)+(QG(KLCL)-QG(K))*DLP
          TVEN=TENV*(1.+0.608*QENV)
          PLCL=P0(K)+(P0(KLCL)-P0(K))*DLP
          THETEU(K)=TMIX*(1.E5/PMIX)**(0.2854*(1.-0.28*QMIX))*             &
                  EXP((3374.6525/TLCL-2.5403)*QMIX*(1.+0.81*QMIX))



          ABEG=0.
          DO NK=K,LTOPM1
            NK1=NK+1
            THETEU(NK1) = THETEU(NK)

            call tpmix2dd(p0(nk1),theteu(nk1),tgu(nk1),qgu(nk1),i,j)

            TVQU(NK1)=TGU(NK1)*(1.+0.608*QGU(NK1)-QLIQ(NK1)-QICE(NK1))
            IF(NK.EQ.K)THEN
              DZZ=Z0(KLCL)-ZLCL
              DILBE=((TVLCL+TVQU(NK1))/(TVEN+TVG(NK1))-1.)*DZZ
            ELSE
              DZZ=DZA(NK)
              DILBE=((TVQU(NK)+TVQU(NK1))/(TVG(NK)+TVG(NK1))-1.)*DZZ
            ENDIF
            IF(DILBE.GT.0.)ABEG=ABEG+DILBE*G



            CALL ENVIRTHT(P0(NK1),TG(NK1),QG(NK1),THTEEG(NK1),ALIQ,BLIQ,CLIQ,DLIQ)
            THETEU(NK1)=THETEU(NK1)*DDILFRC(NK1)+THTEEG(NK1)*(1.-DDILFRC(NK1))
          ENDDO




          IF(NOITR.EQ.1)THEN




          EXIT iter
          ENDIF
          DABE=AMAX1(ABE-ABEG,0.1*ABE)
          FABE=ABEG/ABE
          IF(FABE.GT.1. .and. ISHALL.EQ.0)THEN


            RETURN  
          ENDIF
          IF(NCOUNT.NE.1)THEN
            IF(ABS(AINC-AINCOLD).LT.0.0001)THEN
              NOITR=1
              AINC=AINCOLD
              CYCLE iter
            ENDIF
            DFDA=(FABE-FABEOLD)/(AINC-AINCOLD)
            IF(DFDA.GT.0.)THEN
              NOITR=1
              AINC=AINCOLD
              CYCLE iter
            ENDIF
          ENDIF
          AINCOLD=AINC
          FABEOLD=FABE
          IF(AINC/AINCMX.GT.0.999.AND.FABE.GT.1.05-STAB)THEN




            EXIT
          ENDIF
          IF((FABE.LE.1.05-STAB.AND.FABE.GE.0.95-STAB) .or. NCOUNT.EQ.10)THEN
            EXIT iter
          ELSE
            IF(NCOUNT.GT.10)THEN




              EXIT
            ENDIF




            IF(FABE.EQ.0.)THEN
              AINC=AINC*0.5
            ELSE
              IF(DABE.LT.1.e-4)THEN
                NOITR=1
                AINC=AINCOLD
                CYCLE iter
              ELSE
                AINC=AINC*STAB*ABE/DABE
              ENDIF
            ENDIF

            AINC=AMIN1(AINCMX,AINC)


            IF(AINC.LT.0.05)then
              RETURN                          
            ENDIF

            TDER=TDER2*AINC
            PPTFLX=PPTFL2*AINC




            DO NK=1,LTOP
              UMF(NK)=UMF2(NK)*AINC
              DMF(NK)=DMF2(NK)*AINC
              DETLQ(NK)=DETLQ2(NK)*AINC
              DETIC(NK)=DETIC2(NK)*AINC
              UDR(NK)=UDR2(NK)*AINC
              UER(NK)=UER2(NK)*AINC
              DER(NK)=DER2(NK)*AINC
              DDR(NK)=DDR2(NK)*AINC
            ENDDO



          ENDIF
        ENDDO iter


        IF(ISHALL.EQ.1) THEN
          DO NK=KLCL-1, LTOP1
            UMF_new = UMF(NK)/DXSQ
            xcldfra = 0.07*alog(1.+(500.*UMF_new))
            xcldfra = amax1(0.01,xcldfra)
            cldfra_sh_KF(I,NK,J) = amin1(0.2,xcldfra)
          ENDDO
        ELSE 
          DO NK=KLCL-1, LTOP1
            UMF_new = UMF(NK)/DXSQ
            xcldfra = 0.14*alog(1.+(500.*UMF_new))
            xcldfra = amax1(0.01,xcldfra)
            cldfra_dp_KF(I,NK,J) = amin1(0.6,xcldfra)
          ENDDO
        ENDIF








        IF(CPR.GT.0.)THEN 
          FRC2=PPTFLX/(CPR*AINC)                    
        ELSE
           FRC2=0.
        ENDIF
        DO NK=1,LTOP
          QLPA(NK)=QL0(NK)
          QIPA(NK)=QI0(NK)
          QRPA(NK)=QR0(NK)
          QSPA(NK)=QS0(NK)
          RAINFB(NK)=PPTLIQ(NK)*AINC*FBFRC*FRC2   
          SNOWFB(NK)=PPTICE(NK)*AINC*FBFRC*FRC2   
        ENDDO
        DO NTC=1,NSTEP




          DO NK=1,LTOP
            QLFXIN(NK)=0.
            QLFXOUT(NK)=0.
            QIFXIN(NK)=0.
            QIFXOUT(NK)=0.
            QRFXIN(NK)=0.
            QRFXOUT(NK)=0.
            QSFXIN(NK)=0.
            QSFXOUT(NK)=0.
          ENDDO   
          DO NK=2,LTOP
            IF(OMG(NK).LE.0.)THEN
              QLFXIN(NK)=-FXM(NK)*QLPA(NK-1)
              QIFXIN(NK)=-FXM(NK)*QIPA(NK-1)
              QRFXIN(NK)=-FXM(NK)*QRPA(NK-1)
              QSFXIN(NK)=-FXM(NK)*QSPA(NK-1)
              QLFXOUT(NK-1)=QLFXOUT(NK-1)+QLFXIN(NK)
              QIFXOUT(NK-1)=QIFXOUT(NK-1)+QIFXIN(NK)
              QRFXOUT(NK-1)=QRFXOUT(NK-1)+QRFXIN(NK)
              QSFXOUT(NK-1)=QSFXOUT(NK-1)+QSFXIN(NK)
            ELSE
              QLFXOUT(NK)=FXM(NK)*QLPA(NK)
              QIFXOUT(NK)=FXM(NK)*QIPA(NK)
              QRFXOUT(NK)=FXM(NK)*QRPA(NK)
              QSFXOUT(NK)=FXM(NK)*QSPA(NK)
              QLFXIN(NK-1)=QLFXIN(NK-1)+QLFXOUT(NK)
              QIFXIN(NK-1)=QIFXIN(NK-1)+QIFXOUT(NK)
              QRFXIN(NK-1)=QRFXIN(NK-1)+QRFXOUT(NK)
              QSFXIN(NK-1)=QSFXIN(NK-1)+QSFXOUT(NK)
            ENDIF
          ENDDO   



          DO NK=1,LTOP
            QLPA(NK)=QLPA(NK)+(QLFXIN(NK)+DETLQ(NK)-QLFXOUT(NK))*DTIME*EMSD(NK)
            QIPA(NK)=QIPA(NK)+(QIFXIN(NK)+DETIC(NK)-QIFXOUT(NK))*DTIME*EMSD(NK)
            QRPA(NK)=QRPA(NK)+(QRFXIN(NK)-QRFXOUT(NK)+RAINFB(NK))*DTIME*EMSD(NK)         
            QSPA(NK)=QSPA(NK)+(QSFXIN(NK)-QSFXOUT(NK)+SNOWFB(NK))*DTIME*EMSD(NK)         
          ENDDO     
        ENDDO
        DO NK=1,LTOP
          QLG(NK)=QLPA(NK)
          QIG(NK)=QIPA(NK)
          QRG(NK)=QRPA(NK)
          QSG(NK)=QSPA(NK)
        ENDDO   







       IF(IPRNT)THEN  

         WRITE(message,1080)LFS,LDB,LDT,TIMEC,TADVEC,NSTEP,NCOUNT,FABE,AINC
         CALL wrf_message(message)

       endif  




       IF(IPRNT)then 


         write(98,*)

         write(message,*)'P(LC), DTP, WKL, WKLCL =',p0(LC)/100.,       &
                     TLCL+DTLCL+dtrh-TENV,WKL,WKLCL
         call wrf_message(message)
         write(message,*)'TLCL, DTLCL, DTRH, TENV =',TLCL,DTLCL,       &
                      DTRH,TENV   
         call wrf_message(message)
         WRITE(message,1025)KLCL,ZLCL,DTLCL,LTOP,P0(LTOP),IFLAG,       &
         TMIX-T00,PMIX,QMIX,ABE
         call wrf_message(message)
         WRITE(message,1030)P0(LET)/100.,P0(LTOP)/100.,VMFLCL,PLCL/100.,  &
         WLCL,CLDHGT(LC)
         call wrf_message(message)
         WRITE(message,1035)PEF,PEFCBH,LC,LET,WKL,VWS 
         call wrf_message(message)
         write(message,*)'PRECIP EFFICIENCY =',PEFF 
         call wrf_message(message)
      WRITE(message,1080)LFS,LDB,LDT,TIMEC,TADVEC,NSTEP,NCOUNT,FABE,AINC
         call wrf_message(message)


           WRITE(message,1070)'  P  ','   DP ',' DT K/D ',' DR K/D ','   OMG  ',        &
          ' DOMGDP ','   UMF  ','   UER  ','   UDR  ','   DMF  ','   DER  '        &
          ,'   DDR  ','   EMS  ','    W0  ','  DETLQ ',' DETIC '
         call wrf_message(message)
           write(message,*)'just before DO 300...'
         call wrf_message(message)

           DO NK=1,LTOP
             K=LTOP-NK+1
             DTT=(TG(K)-T0(K))*86400./TIMEC
             RL=XLV0-XLV1*TG(K)
             DR=-(QG(K)-Q0(K))*RL*86400./(TIMEC*CP)
             UDFRC=UDR(K)*TIMEC*EMSD(K)
             UEFRC=UER(K)*TIMEC*EMSD(K)
             DDFRC=DDR(K)*TIMEC*EMSD(K)
             DEFRC=-DER(K)*TIMEC*EMSD(K)
             WRITE(message,1075)P0(K)/100.,DP(K)/100.,DTT,DR,OMG(K),DOMGDP(K)*1.E4,       &
             UMF(K)/1.E6,UEFRC,UDFRC,DMF(K)/1.E6,DEFRC,DDFRC,EMS(K)/1.E11,           &
             W0AVG1D(K)*1.E2,DETLQ(K)*TIMEC*EMSD(K)*1.E3,DETIC(K)*                   &
             TIMEC*EMSD(K)*1.E3
         call wrf_message(message)
           ENDDO
           WRITE(message,1085)'K','P','Z','T0','TG','DT','TU','TD','Q0','QG',             &
                  'DQ','QU','QD','QLG','QIG','QRG','QSG','RH0','RHG'
         call wrf_message(message)
           DO NK=1,KL
             K=KX-NK+1
             DTT=TG(K)-T0(K)
             TUC=TU(K)-T00
             IF(K.LT.LC.OR.K.GT.LTOP)TUC=0.
             TDC=TZ(K)-T00
             IF((K.LT.LDB.OR.K.GT.LDT).AND.K.NE.LFS)TDC=0.
             IF(T0(K).LT.T00)THEN
               ES=ALIQ*EXP((BLIQ*TG(K)-CLIQ)/(TG(K)-DLIQ))
             ELSE
               ES=ALIQ*EXP((BLIQ*TG(K)-CLIQ)/(TG(K)-DLIQ))
             ENDIF  
             QGS=ES*0.622/(P0(K)-ES)
             RH0=Q0(K)/QES(K)
             RHG=QG(K)/QGS
             WRITE(message,1090)K,P0(K)/100.,Z0(K),T0(K)-T00,TG(K)-T00,DTT,TUC,            &
             TDC,Q0(K)*1000.,QG(K)*1000.,(QG(K)-Q0(K))*1000.,QU(K)*                   &
             1000.,QD(K)*1000.,QLG(K)*1000.,QIG(K)*1000.,QRG(K)*1000.,                &
             QSG(K)*1000.,RH0,RHG
         call wrf_message(message)
           ENDDO












            DO 310 NK = 1,KL
              k = kl - nk + 1
              write(98,4455) p0(k)/100.,t0(k)-273.16,q0(k)*1000.,       &
                       u0(k),v0(k),W0AVG1D(K),dp(k),tke(k)



 310        CONTINUE
            IF(ISTOP.EQ.1)THEN
              CALL wrf_error_fatal3("<stdin>",2396,&
'KAIN-FRITSCH, istop=1, diags' )
            ENDIF

  4455  format(8f11.3) 
       ENDIF
        CNDTNF=(1.-EQFRC(LFS))*(QLIQ(LFS)+QICE(LFS))*DMF(LFS)
        PRATEC(I,J)=PPTFLX*(1.-FBFRC)/DXSQ
        RAINCV(I,J)=DT*PRATEC(I,J)     


        RNC=RAINCV(I,J)*NIC
       IF(ISHALL.EQ.0.AND.IPRNT)write (98,909)I,J,RNC






        QINIT=0.
        QFNL=0.
        DPT=0.
        DO 315 NK=1,LTOP
          DPT=DPT+DP(NK)
          QINIT=QINIT+Q0(NK)*EMS(NK)
          QFNL=QFNL+QG(NK)*EMS(NK)
          QFNL=QFNL+(QLG(NK)+QIG(NK)+QRG(NK)+QSG(NK))*EMS(NK)
  315   CONTINUE
        QFNL=QFNL+PPTFLX*TIMEC*(1.-FBFRC)       

        ERR2=(QFNL-QINIT)*100./QINIT
       IF(IPRNT)WRITE(98,1110)QINIT,QFNL,ERR2
      IF(ABS(ERR2).GT.0.05 .AND. ISTOP.EQ.0)THEN 


        IPRNT=.TRUE.
        ISTOP=1
            write(98,4422)kl
 4422       format(i6)
            DO 311 NK = 1,KL
              k = kl - nk + 1





            WRITE(98,4456)P0(K)/100.,T0(K)-273.16,Q0(K)*1000.,          &
                     U0(K),V0(K),W0AVG1D(K),dp(k)/100.,tke(k)
 311        CONTINUE




      ENDIF
 1115 FORMAT (2X,F7.2,2X,F5.1,2X,F6.3,2(2X,F5.1),2X,F7.2,2X,F7.4)
 4456  format(8f12.3)
        IF(PPTFLX.GT.0.)THEN
          RELERR=ERR2*QINIT/(PPTFLX*TIMEC)
        ELSE
          RELERR=0.
        ENDIF
     IF(IPRNT)THEN
        WRITE(98,1120)RELERR
        WRITE(98,*)'TDER, CPR, TRPPT =',              &
          TDER,CPR*AINC,TRPPT*AINC
     ENDIF






        IF(TADVEC.LT.TIMEC)NIC=NINT(TADVEC/DT)
        NCA(I,J) = REAL(NIC)*DT
        IF(ISHALL.EQ.1)THEN
           TIMEC = 2400.
           NCA(I,J) = CUDT*60.
           NSHALL = NSHALL+1
        ENDIF

        DO K=1,KX





















          IF(.NOT. F_QI .and. warm_rain)THEN

            CPM=CP*(1.+0.887*QG(K))
            TG(K)=TG(K)-(QIG(K)+QSG(K))*RLF/CPM
            DQCDT(K)=(QLG(K)+QIG(K)-QL0(K)-QI0(K))/TIMEC
            DQIDT(K)=0.
            DQRDT(K)=(QRG(K)+QSG(K)-QR0(K)-QS0(K))/TIMEC
            DQSDT(K)=0.
          ELSEIF(.NOT. F_QI .and. .not. warm_rain)THEN




            CPM=CP*(1.+0.887*QG(K))
            IF(K.LE.ML)THEN
              TG(K)=TG(K)-(QIG(K)+QSG(K))*RLF/CPM
            ELSEIF(K.GT.ML)THEN
              TG(K)=TG(K)+(QLG(K)+QRG(K))*RLF/CPM
            ENDIF
            DQCDT(K)=(QLG(K)+QIG(K)-QL0(K)-QI0(K))/TIMEC
            DQIDT(K)=0.
            DQRDT(K)=(QRG(K)+QSG(K)-QR0(K)-QS0(K))/TIMEC
            DQSDT(K)=0.
          ELSEIF(F_QI) THEN




            DQCDT(K)=(QLG(K)-QL0(K))/TIMEC
            DQIDT(K)=(QIG(K)-QI0(K))/TIMEC
            DQRDT(K)=(QRG(K)-QR0(K))/TIMEC
            IF (F_QS) THEN
               DQSDT(K)=(QSG(K)-QS0(K))/TIMEC
            ELSE
               DQIDT(K)=DQIDT(K)+(QSG(K)-QS0(K))/TIMEC
            ENDIF
          ELSE

              CALL wrf_error_fatal3("<stdin>",2536,&
'KAIN-FRITSCH, THIS COMBINATION OF IMOIST, IEXICE, IICE NOT ALLOWED' )
          ENDIF
          DTDT(K)=(TG(K)-T0(K))/TIMEC
          DQDT(K)=(QG(K)-Q0(K))/TIMEC
        ENDDO
        PRATEC(I,J)=PPTFLX*(1.-FBFRC)/DXSQ
        RAINCV(I,J)=DT*PRATEC(I,J)


        RNC=RAINCV(I,J)*NIC
 909     FORMAT('AT I, J =',i3,1x,i3,' CONVECTIVE RAINFALL =',F8.4,' mm')





1000  FORMAT(' ',10A8)
1005  FORMAT(' ',F6.0,2X,F6.4,2X,F7.3,1X,F6.4,2X,4(F6.3,2X),2(F7.3,1X))
1010  FORMAT(' ',' VERTICAL VELOCITY IS NEGATIVE AT ',F4.0,' MB')
1015   FORMAT(' ','ALL REMAINING MASS DETRAINS BELOW ',F4.0,' MB')
1025   FORMAT(5X,' KLCL=',I2,' ZLCL=',F7.1,'M',                         &
        ' DTLCL=',F5.2,' LTOP=',I2,' P0(LTOP)=',-2PF5.1,'MB FRZ LV=',   &
        I2,' TMIX=',0PF4.1,1X,'PMIX=',-2PF6.1,' QMIX=',3PF5.1,          &
        ' CAPE=',0PF7.1)
1030   FORMAT(' ',' P0(LET) = ',F6.1,' P0(LTOP) = ',F6.1,' VMFLCL =',   &
      E12.3,' PLCL =',F6.1,' WLCL =',F6.3,' CLDHGT =',                  &
      F8.1)
1035  FORMAT(1X,'PEF(WS)=',F4.2,'(CB)=',F4.2,'LC,LET=',2I3,'WKL='       &
      ,F6.3,'VWS=',F5.2)




 1070 FORMAT (16A8) 
 1075 FORMAT (F8.2,3(F8.2),2(F8.3),F8.2,2F8.3,F8.2,6F8.3) 
 1080 FORMAT(2X,'LFS,LDB,LDT =',3I3,' TIMEC, TADVEC, NSTEP=',           &
              2(1X,F5.0),I3,'NCOUNT, FABE, AINC=',I2,1X,F5.3,F6.2) 
 1085 FORMAT (A3,16A7,2A8) 
 1090 FORMAT (I3,F7.2,F7.0,10F7.2,4F7.3,2F8.3) 
 1095 FORMAT(' ','  PPT PRODUCTION RATE= ',F10.0,' TOTAL EVAP+PPT= ',F10.0)
1105   FORMAT(' ','NET LATENT HEAT RELEASE =',E12.5,' ACTUAL HEATING =',&
       E12.5,' J/KG-S, DIFFERENCE = ',F9.3,'%')
1110   FORMAT(' ','INITIAL WATER =',E12.5,' FINAL WATER =',E12.5,       &
       ' TOTAL WATER CHANGE =',F8.2,'%')

1120   FORMAT(' ','MOISTURE ERROR AS FUNCTION OF TOTAL PPT =',F9.3,'%')





      CUTOP(I,J)=REAL(LTOP)
      CUBOT(I,J)=REAL(LCL)


   END SUBROUTINE  KF_eta_PARA


   SUBROUTINE TPMIX2(p,thes,tu,qu,qliq,qice,qnewlq,qnewic,XLV1,XLV0)









   IMPLICIT NONE

   REAL,         INTENT(IN   )   :: P,THES,XLV1,XLV0
   REAL,         INTENT(OUT  )   :: QNEWLQ,QNEWIC
   REAL,         INTENT(INOUT)   :: TU,QU,QLIQ,QICE
   REAL    ::    TP,QQ,BTH,TTH,PP,T00,T10,T01,T11,Q00,Q10,Q01,Q11,          &
                 TEMP,QS,QNEW,DQ,QTOT,RLL,CPP
   INTEGER ::    IPTB,ITHTB













      tp=(p-plutop)*rdpr
      qq=tp-aint(tp)
      iptb=int(tp)+1







      bth=(the0k(iptb+1)-the0k(iptb))*qq+the0k(iptb)
      tth=(thes-bth)*rdthk
      pp   =tth-aint(tth)
      ithtb=int(tth)+1
       IF(IPTB.GE.220 .OR. IPTB.LE.1 .OR. ITHTB.GE.250 .OR. ITHTB.LE.1)THEN
         write(98,*)'**** OUT OF BOUNDS *********'

       ENDIF

      t00=ttab(ithtb  ,iptb  )
      t10=ttab(ithtb+1,iptb  )
      t01=ttab(ithtb  ,iptb+1)
      t11=ttab(ithtb+1,iptb+1)

      q00=qstab(ithtb  ,iptb  )
      q10=qstab(ithtb+1,iptb  )
      q01=qstab(ithtb  ,iptb+1)
      q11=qstab(ithtb+1,iptb+1)





      temp=(t00+(t10-t00)*pp+(t01-t00)*qq+(t00-t10-t01+t11)*pp*qq)

      qs=(q00+(q10-q00)*pp+(q01-q00)*qq+(q00-q10-q01+q11)*pp*qq)

      DQ=QS-QU
      IF(DQ.LE.0.)THEN
        QNEW=QU-QS
        QU=QS
      ELSE 




        QNEW=0.
        QTOT=QLIQ+QICE













        IF(QTOT.GE.DQ)THEN
          qliq=qliq-dq*qliq/(qtot+1.e-10)
          qice=qice-dq*qice/(qtot+1.e-10)
          QU=QS
        ELSE
          RLL=XLV0-XLV1*TEMP
          CPP=1004.5*(1.+0.89*QU)
          IF(QTOT.LT.1.E-10)THEN


            TEMP=TEMP+RLL*(DQ/(1.+DQ))/CPP
          ELSE




            TEMP=TEMP+RLL*((DQ-QTOT)/(1+DQ-QTOT))/CPP
            QU=QU+QTOT
            QTOT=0.
            QLIQ=0.
            QICE=0.
          ENDIF
        ENDIF
      ENDIF
      TU=TEMP
      qnewlq=qnew
      qnewic=0.

   END SUBROUTINE TPMIX2

      SUBROUTINE DTFRZNEW(TU,P,THTEU,QU,QFRZ,QICE,ALIQ,BLIQ,CLIQ,DLIQ)

   IMPLICIT NONE

   REAL,         INTENT(IN   )   :: P,QFRZ,ALIQ,BLIQ,CLIQ,DLIQ
   REAL,         INTENT(INOUT)   :: TU,THTEU,QU,QICE
   REAL    ::    RLC,RLS,RLF,CPP,A,DTFRZ,ES,QS,DQEVAP,PII









      RLC=2.5E6-2369.276*(TU-273.16)
      RLS=2833922.-259.532*(TU-273.16)
      RLF=RLS-RLC
      CPP=1004.5*(1.+0.89*QU)




      A=(CLIQ-BLIQ*DLIQ)/((TU-DLIQ)*(TU-DLIQ))
      DTFRZ = RLF*QFRZ/(CPP+RLS*QU*A)
      TU = TU+DTFRZ
      
      ES = ALIQ*EXP((BLIQ*TU-CLIQ)/(TU-DLIQ))
      QS = ES*0.622/(P-ES)







      DQEVAP = QS-QU
      QICE = QICE-DQEVAP
      QU = QU+DQEVAP
      PII=(1.E5/P)**(0.2854*(1.-0.28*QU))
      THTEU=TU*PII*EXP((3374.6525/TU-2.5403)*QU*(1.+0.81*QU))

   END SUBROUTINE DTFRZNEW


      SUBROUTINE CONDLOAD(QLIQ,QICE,WTW,DZ,BOTERM,ENTERM,RATE,QNEWLQ,           &
                          QNEWIC,QLQOUT,QICOUT,G)


   IMPLICIT NONE







      REAL, INTENT(IN   )   :: G
      REAL, INTENT(IN   )   :: DZ,BOTERM,ENTERM,RATE
      REAL, INTENT(INOUT)   :: QLQOUT,QICOUT,WTW,QLIQ,QICE,QNEWLQ,QNEWIC
      REAL :: QTOT,QNEW,QEST,G1,WAVG,CONV,RATIO3,OLDQ,RATIO4,DQ,PPTDRG

      QTOT=QLIQ+QICE                                                    
      QNEW=QNEWLQ+QNEWIC                                                





      QEST=0.5*(QTOT+QNEW)                                              
      G1=WTW+BOTERM-ENTERM-2.*G*DZ*QEST/1.5                             
      IF(G1.LT.0.0)G1=0.                                                
      WAVG=0.5*(SQRT(WTW)+SQRT(G1))                                      
      CONV=RATE*DZ/WAVG               






      RATIO3=QNEWLQ/(QNEW+1.E-8)                                       

      QTOT=QTOT+0.6*QNEW                                                
      OLDQ=QTOT                                                         
      RATIO4=(0.6*QNEWLQ+QLIQ)/(QTOT+1.E-8)                            
      QTOT=QTOT*EXP(-CONV)            




      DQ=OLDQ-QTOT                                                      
      QLQOUT=RATIO4*DQ                                                  
      QICOUT=(1.-RATIO4)*DQ                                             




      PPTDRG=0.5*(OLDQ+QTOT-0.2*QNEW)                                   
      WTW=WTW+BOTERM-ENTERM-2.*G*DZ*PPTDRG/1.5                          
      IF(ABS(WTW).LT.1.E-4)WTW=1.E-4




      QLIQ=RATIO4*QTOT+RATIO3*0.4*QNEW                                  
      QICE=(1.-RATIO4)*QTOT+(1.-RATIO3)*0.4*QNEW                        
      QNEWLQ=0.                                                         
      QNEWIC=0.                                                         

   END SUBROUTINE CONDLOAD


   SUBROUTINE PROF5(EQ,EE,UD)                                        















   IMPLICIT NONE

   REAL,         INTENT(IN   )   :: EQ
   REAL,         INTENT(INOUT)   :: EE,UD
   REAL ::       SQRT2P,A1,A2,A3,P,SIGMA,FE,X,Y,EY,E45,T1,T2,C1,C2

      DATA SQRT2P,A1,A2,A3,P,SIGMA,FE/2.506628,0.4361836,-0.1201676,       &
           0.9372980,0.33267,0.166666667,0.202765151/                        
      X=(EQ-0.5)/SIGMA                                                  
      Y=6.*EQ-3.                                                        
      EY=EXP(Y*Y/(-2))                                                  
      E45=EXP(-4.5)                                                     
      T2=1./(1.+P*ABS(Y))                                               
      T1=0.500498                                                       
      C1=A1*T1+A2*T1*T1+A3*T1*T1*T1                                     
      C2=A1*T2+A2*T2*T2+A3*T2*T2*T2                                     
      IF(Y.GE.0.)THEN                                                   
        EE=SIGMA*(0.5*(SQRT2P-E45*C1-EY*C2)+SIGMA*(E45-EY))-E45*EQ*EQ/2.
        UD=SIGMA*(0.5*(EY*C2-E45*C1)+SIGMA*(E45-EY))-E45*(0.5+EQ*EQ/2.-    &
           EQ)                                                          
      ELSE                                                              
        EE=SIGMA*(0.5*(EY*C2-E45*C1)+SIGMA*(E45-EY))-E45*EQ*EQ/2.       
        UD=SIGMA*(0.5*(SQRT2P-E45*C1-EY*C2)+SIGMA*(E45-EY))-E45*(0.5+EQ*   &
           EQ/2.-EQ)                                                    
      ENDIF                                                             
      EE=EE/FE                                                          
      UD=UD/FE                                                          

   END SUBROUTINE PROF5


   SUBROUTINE TPMIX2DD(p,thes,ts,qs,i,j)









   IMPLICIT NONE

   REAL,         INTENT(IN   )   :: P,THES
   REAL,         INTENT(INOUT)   :: TS,QS
   INTEGER,      INTENT(IN   )   :: i,j     
   REAL    ::    TP,QQ,BTH,TTH,PP,T00,T10,T01,T11,Q00,Q10,Q01,Q11
   INTEGER ::    IPTB,ITHTB
   CHARACTER*256 :: MESS














      tp=(p-plutop)*rdpr
      qq=tp-aint(tp)
      iptb=int(tp)+1






      bth=(the0k(iptb+1)-the0k(iptb))*qq+the0k(iptb)
      tth=(thes-bth)*rdthk
      pp   =tth-aint(tth)
      ithtb=int(tth)+1

      t00=ttab(ithtb  ,iptb  )
      t10=ttab(ithtb+1,iptb  )
      t01=ttab(ithtb  ,iptb+1)
      t11=ttab(ithtb+1,iptb+1)

      q00=qstab(ithtb  ,iptb  )
      q10=qstab(ithtb+1,iptb  )
      q01=qstab(ithtb  ,iptb+1)
      q11=qstab(ithtb+1,iptb+1)





      ts=(t00+(t10-t00)*pp+(t01-t00)*qq+(t00-t10-t01+t11)*pp*qq)

      qs=(q00+(q10-q00)*pp+(q01-q00)*qq+(q00-q10-q01+q11)*pp*qq)

   END SUBROUTINE TPMIX2DD


  SUBROUTINE ENVIRTHT(P1,T1,Q1,THT1,ALIQ,BLIQ,CLIQ,DLIQ)                       


   IMPLICIT NONE

   REAL,         INTENT(IN   )   :: P1,T1,Q1,ALIQ,BLIQ,CLIQ,DLIQ
   REAL,         INTENT(INOUT)   :: THT1
   REAL    ::    EE,TLOG,ASTRT,AINC,A1,TP,VALUE,AINTRP,TDPT,TSAT,THT,      &
                 T00,P00,C1,C2,C3,C4,C5
   INTEGER ::    INDLU

      DATA T00,P00,C1,C2,C3,C4,C5/273.16,1.E5,3374.6525,2.5403,3114.834,   &
           0.278296,1.0723E-3/                                          






      EE=Q1*P1/(0.622+Q1)                                             



      astrt=1.e-3
      ainc=0.075
      a1=ee/aliq
      tp=(a1-astrt)/ainc
      indlu=int(tp)+1
      value=(indlu-1)*ainc+astrt
      aintrp=(a1-value)/ainc
      tlog=aintrp*alu(indlu+1)+(1-aintrp)*alu(indlu)

      TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)                               
      TSAT=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(T1-T00))*(T1-TDPT) 
      THT=T1*(P00/P1)**(0.2854*(1.-0.28*Q1))                          
      THT1=THT*EXP((C1/TSAT-C2)*Q1*(1.+0.81*Q1))                      

  END SUBROUTINE ENVIRTHT                                                              


   SUBROUTINE kf_eta_init(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,      &
                     RQICUTEN,RQSCUTEN,NCA,W0AVG,P_QI,P_QS,         &
                     SVP1,SVP2,SVP3,SVPT0,                          &
                     P_FIRST_SCALAR,restart,allowed_to_read,        &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte                   )

   IMPLICIT NONE

   LOGICAL , INTENT(IN)           ::  restart,allowed_to_read
   INTEGER , INTENT(IN)           ::  ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)           ::  P_QI,P_QS,P_FIRST_SCALAR

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::       &
                                                          RTHCUTEN, &
                                                          RQVCUTEN, &
                                                          RQCCUTEN, &
                                                          RQRCUTEN, &
                                                          RQICUTEN, &
                                                          RQSCUTEN

   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: W0AVG

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: NCA

   INTEGER :: i, j, k, itf, jtf, ktf
   REAL, INTENT(IN)    :: SVP1,SVP2,SVP3,SVPT0

   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)

   IF(.not.restart)THEN

      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         RTHCUTEN(i,k,j)=0.
         RQVCUTEN(i,k,j)=0.
         RQCCUTEN(i,k,j)=0.
         RQRCUTEN(i,k,j)=0.
      ENDDO
      ENDDO
      ENDDO

      IF (P_QI .ge. P_FIRST_SCALAR) THEN
         DO j=jts,jtf
         DO k=kts,ktf
         DO i=its,itf
            RQICUTEN(i,k,j)=0.
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF (P_QS .ge. P_FIRST_SCALAR) THEN
         DO j=jts,jtf
         DO k=kts,ktf
         DO i=its,itf
            RQSCUTEN(i,k,j)=0.
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      DO j=jts,jtf
      DO i=its,itf
         NCA(i,j)=-100.
      ENDDO
      ENDDO

      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         W0AVG(i,k,j)=0.
      ENDDO
      ENDDO
      ENDDO

   endif
 
   CALL KF_LUTAB(SVP1,SVP2,SVP3,SVPT0)

   END SUBROUTINE kf_eta_init



      subroutine kf_lutab(SVP1,SVP2,SVP3,SVPT0)






   IMPLICIT NONE









     INTEGER :: KP,IT,ITCNT,I
     REAL :: DTH,TMIN,TOLER,PBOT,DPR,                               &
             TEMP,P,ES,QS,PI,THES,TGUES,THGUES,F0,T1,T0,THGS,F1,DT, &
             ASTRT,AINC,A1,THTGS

     REAL    :: ALIQ,BLIQ,CLIQ,DLIQ
     REAL, INTENT(IN)    :: SVP1,SVP2,SVP3,SVPT0


      data dth/1./

      data tmin/150./

      data toler/0.001/

      plutop=5000.0

      pbot=110000.0

      ALIQ = SVP1*1000.
      BLIQ = SVP2
      CLIQ = SVP2*SVPT0
      DLIQ = SVP3





      rdthk=1./dth


      DPR=(PBOT-PLUTOP)/REAL(KFNP-1)


      rdpr=1./dpr





      temp=tmin 
      p=plutop-dpr
      do kp=1,kfnp
        p=p+dpr
        es=aliq*exp((bliq*temp-cliq)/(temp-dliq))
        qs=0.622*es/(p-es)
        pi=(1.e5/p)**(0.2854*(1.-0.28*qs))
        the0k(kp)=temp*pi*exp((3374.6525/temp-2.5403)*qs*        &
               (1.+0.81*qs))
      enddo   



      p=plutop-dpr
      do kp=1,kfnp
        thes=the0k(kp)-dth
        p=p+dpr
        do it=1,kfnt

          thes=thes+dth


          if(it.eq.1) then
            tgues=tmin
          else
            tgues=ttab(it-1,kp)
          endif
          es=aliq*exp((bliq*tgues-cliq)/(tgues-dliq))
          qs=0.622*es/(p-es)
          pi=(1.e5/p)**(0.2854*(1.-0.28*qs))
          thgues=tgues*pi*exp((3374.6525/tgues-2.5403)*qs*      &
               (1.+0.81*qs))
          f0=thgues-thes
          t1=tgues-0.5*f0
          t0=tgues
          itcnt=0

          do itcnt=1,11
            es=aliq*exp((bliq*t1-cliq)/(t1-dliq))
            qs=0.622*es/(p-es)
            pi=(1.e5/p)**(0.2854*(1.-0.28*qs))
            thtgs=t1*pi*exp((3374.6525/t1-2.5403)*qs*(1.+0.81*qs))
            f1=thtgs-thes
            if(abs(f1).lt.toler)then
              exit
            endif

            dt=f1*(t1-t0)/(f1-f0)
            t0=t1
            f0=f1
            t1=t1-dt
          enddo 
          ttab(it,kp)=t1 
          qstab(it,kp)=qs
        enddo
      enddo   





       astrt=1.e-3
       ainc=0.075

       a1=astrt-ainc
       do i=1,200
         a1=a1+ainc
         alu(i)=alog(a1)
       enddo   

   END SUBROUTINE KF_LUTAB

END MODULE module_cu_kfeta
