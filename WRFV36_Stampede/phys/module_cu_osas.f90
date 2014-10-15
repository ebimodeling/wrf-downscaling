
MODULE module_cu_osas 

CONTAINS


      SUBROUTINE CU_OSAS(DT,ITIMESTEP,STEPCU,                        &
                 RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,               &
                 RUCUTEN,RVCUTEN,                                   & 
                 RAINCV,PRATEC,HTOP,HBOT,                           &
                 U3D,V3D,W,T3D,QV3D,QC3D,QI3D,PI3D,RHO3D,           &
                 DZ8W,PCPS,P8W,XLAND,CU_ACT_FLAG,                   &
                 P_QC,                                              & 
                 STORE_RAND,MOMMIX, & 
                 P_QI,P_FIRST_SCALAR,                               & 
                 ids,ide, jds,jde, kds,kde,                         &
                 ims,ime, jms,jme, kms,kme,                         &
                 its,ite, jts,jte, kts,kte                          )


      USE MODULE_GFS_MACHINE , ONLY : kind_phys
      USE MODULE_GFS_FUNCPHYS , ONLY : gfuncphys
      USE MODULE_GFS_PHYSCONS, grav => con_g, CP => con_CP, HVAP => con_HVAP  &
     &,             RV => con_RV, FV => con_fvirt, T0C => con_T0C       &
     &,             CVAP => con_CVAP, CLIQ => con_CLIQ                  & 
     &,             EPS => con_eps, EPSM1 => con_epsm1                  &
     &,             ROVCP => con_rocp, RD => con_rd

      IMPLICIT NONE








































































      INTEGER ::                        ICLDCK

      INTEGER, INTENT(IN) ::            ids,ide, jds,jde, kds,kde,      &
                                        ims,ime, jms,jme, kms,kme,      &
                                        its,ite, jts,jte, kts,kte,      &
                                        ITIMESTEP,                      &     
                                        P_FIRST_SCALAR,                 &
                                        P_QC,                           &
                                        P_QI,                           &
                                        STEPCU

      REAL,    INTENT(IN) ::                                            &
                                        DT


      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) ::      &
                                        RQCCUTEN,                       &
                                        RQICUTEN,                       &
                                        RQVCUTEN,                       &
                                        RTHCUTEN
      REAL, DIMENSION(ims:ime, jms:jme, kms:kme), INTENT(INOUT) ::      &
                                        RUCUTEN,                        &  
                                        RVCUTEN                            
      REAL, OPTIONAL,   INTENT(IN) ::    MOMMIX
      REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL,                   &
                         INTENT(IN) :: STORE_RAND

      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(IN) ::               &
                                        XLAND

      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(INOUT) ::            &
                                        RAINCV, PRATEC

      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(OUT) ::              &
                                        HBOT,                           &
                                        HTOP

      LOGICAL, DIMENSION(IMS:IME,JMS:JME), INTENT(INOUT) ::             &
                                        CU_ACT_FLAG


      REAL,    DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) ::      &
                                        DZ8W,                           &
                                        P8w,                            &
                                        Pcps,                           &
                                        PI3D,                           &
                                        QC3D,                           &
                                        QI3D,                           &
                                        QV3D,                           &
                                        RHO3D,                          &
                                        T3D,                            &
                                        U3D,                            &
                                        V3D,                            &
                                        W



      REAL,    DIMENSION(ims:ime, jms:jme) ::                           &
                                        PSFC


      REAL     (kind=kind_phys) ::                                      &
                                        DELT,                           &
                                        DPSHC,                          &
                                        RDELT,                          &
                                        RSEED

      REAL     (kind=kind_phys), DIMENSION(its:ite) ::                  &
                                        CLDWRK,                         &
                                        PS,                             &
                                        RCS,                            &
                                        RN,                             &
                                        SLIMSK,                         &
                                        XKT2

      REAL     (kind=kind_phys), DIMENSION(its:ite, kts:kte+1) ::       &
                                        PRSI                            

      REAL     (kind=kind_phys), DIMENSION(its:ite, kts:kte) ::         &
                                        DEL,                            &
                                        DOT,                            &
                                        PHIL,                           &
                                        PRSL,                           &
                                        PRSLK,                          &
                                        Q1,                             & 
                                        T1,                             & 
                                        U1,                             & 
                                        V1,                             & 
                                        ZI,                             & 
                                        ZL 

      REAL     (kind=kind_phys), DIMENSION(its:ite, kts:kte, 2) ::      &
                                        QL 

      INTEGER, DIMENSION(its:ite) ::                                    &
                                        KBOT,                           &
                                        KTOP,                           &
                                        KUO

      INTEGER ::                                                        &
                                        I,                              &
                                        IGPVS,                          &
                                        IM,                             &
                                        J,                              &
                                        JCAP,                           &
                                        K,                              &
                                        KM,                             &
                                        KP,                             &
                                        KX,                             &
                                        NCLOUD 

      DATA IGPVS/0/




      DO J=JTS,JTE
         DO I=ITS,ITE
            CU_ACT_FLAG(I,J)=.TRUE.
         ENDDO
      ENDDO
 
      IM=ITE-ITS+1
      KX=KTE-KTS+1
      JCAP=126
      DPSHC=30_kind_phys
      DELT=DT*STEPCU
      RDELT=1./DELT
      NCLOUD=1


   DO J=jms,jme
     DO I=ims,ime
       PSFC(i,j)=P8w(i,kms,j)
     ENDDO
   ENDDO

   if(igpvs.eq.0) CALL GFUNCPHYS
   igpvs=1



   DO J=jts,jte


      DO i=its,ite
        ZI(I,KTS)=0.0
      ENDDO

      DO k=kts+1,kte
        KM=K-1
        DO i=its,ite
          ZI(I,K)=ZI(I,KM)+dz8w(i,km,j)
        ENDDO
      ENDDO

      DO k=kts+1,kte
        KM=K-1
        DO i=its,ite
          ZL(I,KM)=(ZI(I,K)+ZI(I,KM))*0.5
        ENDDO
      ENDDO

      DO i=its,ite
        ZL(I,KTE)=2.*ZI(I,KTE)-ZL(I,KTE-1)
      ENDDO













     call init_random_seed()
     call random_number(XKT2)












      DO i=its,ite
        PS(i)=PSFC(i,j)*.001
        RCS(i)=1.
        SLIMSK(i)=ABS(XLAND(i,j)-2.)
      ENDDO

      DO i=its,ite
        PRSI(i,kts)=PS(i)
      ENDDO

      DO k=kts,kte
        kp=k+1
        DO i=its,ite
          PRSL(I,K)=Pcps(i,k,j)*.001
          PHIL(I,K)=ZL(I,K)*GRAV
          DOT(i,k)=-5.0E-4*GRAV*rho3d(i,k,j)*(w(i,k,j)+w(i,kp,j))
        ENDDO
      ENDDO

      DO k=kts,kte
        DO i=its,ite
          DEL(i,k)=PRSL(i,k)*GRAV/RD*dz8w(i,k,j)/T3D(i,k,j)
          U1(i,k)=U3D(i,k,j)
          V1(i,k)=V3D(i,k,j)
          Q1(i,k)=QV3D(i,k,j)/(1.+QV3D(i,k,j))
          T1(i,k)=T3D(i,k,j)
          QL(i,k,1)=QI3D(i,k,j)/(1.+QI3D(i,k,j))
          QL(i,k,2)=QC3D(i,k,j)/(1.+QC3D(i,k,j))
          PRSLK(I,K)=(PRSL(i,k)*.01)**ROVCP
        ENDDO
      ENDDO

      DO k=kts+1,kte+1
        km=k-1
        DO i=its,ite
          PRSI(i,k)=PRSI(i,km)-del(i,km) 
        ENDDO
      ENDDO


      CALL OSASCNV(IM,IM,KX,JCAP,DELT,DEL,PRSL,PS,PHIL,                  &
                  QL,Q1,T1,U1,V1,RCS,CLDWRK,RN,KBOT,                    &
                  KTOP,KUO,SLIMSK,DOT,XKT2,NCLOUD) 




      CALL SHALCV(IM,IM,KX,DELT,DEL,PRSI,PRSL,PRSLK,KUO,Q1,T1,DPSHC)


      DO I=ITS,ITE
        RAINCV(I,J)=RN(I)*1000./STEPCU
        PRATEC(I,J)=RN(I)*1000./(STEPCU * DT)
        HBOT(I,J)=KBOT(I)
        HTOP(I,J)=KTOP(I)
      ENDDO

      DO K=KTS,KTE
        DO I=ITS,ITE
          RTHCUTEN(I,K,J)=(T1(I,K)-T3D(I,K,J))/PI3D(I,K,J)*RDELT
          RQVCUTEN(I,K,J)=(Q1(I,K)/(1.-q1(i,k))-QV3D(I,K,J))*RDELT
        ENDDO
      ENDDO










      IF(P_QC .ge. P_FIRST_SCALAR)THEN
        DO K=KTS,KTE
          DO I=ITS,ITE
            RQCCUTEN(I,K,J)=(QL(I,K,2)/(1.-ql(i,k,2))-QC3D(I,K,J))*RDELT
          ENDDO
        ENDDO
      ENDIF

      IF(P_QI .ge. P_FIRST_SCALAR)THEN
        DO K=KTS,KTE
          DO I=ITS,ITE
            RQICUTEN(I,K,J)=(QL(I,K,1)/(1.-ql(i,k,1))-QI3D(I,K,J))*RDELT
          ENDDO
        ENDDO
      ENDIF

   ENDDO    

   END SUBROUTINE CU_OSAS


   SUBROUTINE osasinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,          &
                      RUCUTEN,RVCUTEN,                              &   
                      RESTART,P_QC,P_QI,P_FIRST_SCALAR,             &
                      allowed_to_read,                              &
                      ids, ide, jds, jde, kds, kde,                 &
                      ims, ime, jms, jme, kms, kme,                 &
                      its, ite, jts, jte, kts, kte                  )

   IMPLICIT NONE

   LOGICAL , INTENT(IN)           ::  allowed_to_read,restart
   INTEGER , INTENT(IN)           ::  ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)           ::  P_FIRST_SCALAR, P_QI, P_QC

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::  &
                                                              RTHCUTEN, &
                                                              RQVCUTEN, &
                                                              RQCCUTEN, &
                                                              RQICUTEN
   REAL,     DIMENSION( ims:ime , jms:jme , kms:kme ) , INTENT(OUT) ::  &
                                                              RUCUTEN,  & 
                                                              RVCUTEN   

   INTEGER :: i, j, k, itf, jtf, ktf

   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)

   IF(.not.restart)THEN
     DO j=jts,jtf
     DO k=kts,ktf
     DO i=its,itf
       RTHCUTEN(i,k,j)=0.
       RQVCUTEN(i,k,j)=0.
       RUCUTEN(i,j,k)=0.   
       RVCUTEN(i,j,k)=0.    
     ENDDO
     ENDDO
     ENDDO

     IF (P_QC .ge. P_FIRST_SCALAR) THEN
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           RQCCUTEN(i,k,j)=0.
        ENDDO
        ENDDO
        ENDDO
     ENDIF

     IF (P_QI .ge. P_FIRST_SCALAR) THEN
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           RQICUTEN(i,k,j)=0.
        ENDDO
        ENDDO
        ENDDO
     ENDIF
   ENDIF

      END SUBROUTINE osasinit



      SUBROUTINE OSASCNV(IM,IX,KM,JCAP,DELT,DEL,PRSL,PS,PHIL,QL,         &
     &       Q1,T1,U1,V1,RCS,CLDWRK,RN,KBOT,KTOP,KUO,SLIMSK,            &
     &       DOT,XKT2,ncloud)






      USE MODULE_GFS_MACHINE , ONLY : kind_phys
      USE MODULE_GFS_FUNCPHYS ,ONLY : fpvs
      USE MODULE_GFS_PHYSCONS, grav => con_g, CP => con_CP, HVAP => con_HVAP &
     &,             RV => con_RV, FV => con_fvirt, T0C => con_T0C       &
     &,             CVAP => con_CVAP, CLIQ => con_CLIQ                  &
     &,             EPS => con_eps, EPSM1 => con_epsm1

      implicit none



      integer            IM, IX,  KM, JCAP, ncloud,                     &
     &                   KBOT(IM), KTOP(IM), KUO(IM), J
      real(kind=kind_phys) DELT
      real(kind=kind_phys) PS(IM),      DEL(IX,KM),  PRSL(IX,KM),       &

     &                     QL(IX,KM,2), Q1(IX,KM),   T1(IX,KM),         &
     &                     U1(IX,KM),   V1(IX,KM),   RCS(IM),           &
     &                     CLDWRK(IM),  RN(IM),      SLIMSK(IM),        &
     &                     DOT(IX,KM),  XKT2(IM),    PHIL(IX,KM)

      integer              I, INDX, jmn, k, knumb, latd, lond, km1

      real(kind=kind_phys) adw,     alpha,   alphal,  alphas,           &
     &                     aup,     beta,    betal,   betas,            &
     &                     c0,      cpoel,   dellat,  delta,            &
     &                     desdt,   deta,    detad,   dg,               &
     &                     dh,      dhh,     dlnsig,  dp,               &
     &                     dq,      dqsdp,   dqsdt,   dt,               &
     &                     dt2,     dtmax,   dtmin,   dv1,              &
     &                     dv1q,    dv2,     dv2q,    dv1u,             &
     &                     dv1v,    dv2u,    dv2v,    dv3u,             &
     &                     dv3v,    dv3,     dv3q,    dvq1,             &
     &                     dz,      dz1,     e1,      edtmax,           &
     &                     edtmaxl, edtmaxs, el2orc,  elocp,            &
     &                     es,      etah,                               &
     &                     evef,    evfact,  evfactl, fact1,            &
     &                     fact2,   factor,  fjcap,   fkm,              &
     &                     fuv,     g,       gamma,   onemf,            &
     &                     onemfu,  pdetrn,  pdpdwn,  pprime,           &
     &                     qc,      qlk,     qrch,    qs,               &
     &                     rain,    rfact,   shear,   tem1,             &
     &                     tem2,    terr,    val,     val1,             &
     &                     val2,    w1,      w1l,     w1s,              &
     &                     w2,      w2l,     w2s,     w3,               &
     &                     w3l,     w3s,     w4,      w4l,              & 
     &                     w4s,     xdby,    xpw,     xpwd,             & 
     &                     xqc,     xqrch,   xlambu,  mbdt,             &
     &                     tem


      integer              JMIN(IM), KB(IM), KBCON(IM), KBDTR(IM),      & 
     &                     KT2(IM),  KTCON(IM), LMIN(IM),               &
     &                     kbm(IM),  kbmax(IM), kmax(IM)

      real(kind=kind_phys) AA1(IM),     ACRT(IM),   ACRTFCT(IM),        & 
     &                     DELHBAR(IM), DELQ(IM),   DELQ2(IM),          &
     &                     DELQBAR(IM), DELQEV(IM), DELTBAR(IM),        &
     &                     DELTV(IM),   DTCONV(IM), EDT(IM),            &
     &                     EDTO(IM),    EDTX(IM),   FLD(IM),            &
     &                     HCDO(IM),    HKBO(IM),   HMAX(IM),           &
     &                     HMIN(IM),    HSBAR(IM),  UCDO(IM),           &
     &                     UKBO(IM),    VCDO(IM),   VKBO(IM),           &
     &                     PBCDIF(IM),  PDOT(IM),   PO(IM,KM),          &
     &                                  PWAVO(IM),  PWEVO(IM),          &

     &                     QCDO(IM),    QCOND(IM),  QEVAP(IM),          &
     &                     QKBO(IM),    RNTOT(IM),  VSHEAR(IM),         &
     &                     XAA0(IM),    XHCD(IM),   XHKB(IM),           & 
     &                     XK(IM),      XLAMB(IM),  XLAMD(IM),          &
     &                     XMB(IM),     XMBMAX(IM), XPWAV(IM),          &
     &                     XPWEV(IM),   XQCD(IM),   XQKB(IM)


      PARAMETER(G=grav)
      PARAMETER(CPOEL=CP/HVAP,ELOCP=HVAP/CP,                            &
     &          EL2ORC=HVAP*HVAP/(RV*CP))
      PARAMETER(TERR=0.,C0=.002,DELTA=fv)
      PARAMETER(FACT1=(CVAP-CLIQ)/RV,FACT2=HVAP/RV-FACT1*T0C)

      real(kind=kind_phys) PFLD(IM,KM),    TO(IM,KM),     QO(IM,KM),    &
     &                     UO(IM,KM),      VO(IM,KM),     QESO(IM,KM)

      real(kind=kind_phys) QLKO_KTCON(IM), DELLAL(IM),    TVO(IM,KM),   &
     &                     DBYO(IM,KM),    ZO(IM,KM),     SUMZ(IM,KM),  &
     &                     SUMH(IM,KM),    HEO(IM,KM),    HESO(IM,KM),  &
     &                     QRCD(IM,KM),    DELLAH(IM,KM), DELLAQ(IM,KM),&
     &                     DELLAU(IM,KM),  DELLAV(IM,KM), HCKO(IM,KM),  &
     &                     UCKO(IM,KM),    VCKO(IM,KM),   QCKO(IM,KM),  &
     &                     ETA(IM,KM),     ETAU(IM,KM),   ETAD(IM,KM),  &
     &                     QRCDO(IM,KM),   PWO(IM,KM),    PWDO(IM,KM),  &
     &                     RHBAR(IM),      TX1(IM)

      LOGICAL TOTFLG, CNVFLG(IM), DWNFLG(IM), DWNFLG2(IM), FLG(IM)

      real(kind=kind_phys) PCRIT(15), ACRITT(15), ACRIT(15)

      DATA PCRIT/850.,800.,750.,700.,650.,600.,550.,500.,450.,400.,     &
     &           350.,300.,250.,200.,150./
      DATA ACRITT/.0633,.0445,.0553,.0664,.075,.1082,.1521,.2216,       &
     &           .3151,.3677,.41,.5255,.7663,1.1686,1.6851/




      real(kind=kind_phys) TF, TCR, TCRF, RZERO, RONE
      parameter (TF=233.16, TCR=263.16, TCRF=1.0/(TCR-TF))
      parameter (RZERO=0.0,RONE=1.0)


      km1 = km - 1


      DO I=1,IM
        RN(I)=0.
        KBOT(I)=KM+1
        KTOP(I)=0
        KUO(I)=0
        CNVFLG(I) = .TRUE.
        DTCONV(I) = 3600.
        CLDWRK(I) = 0.
        PDOT(I) = 0.
        KT2(I) = 0
        QLKO_KTCON(I) = 0.
        DELLAL(I) = 0.
      ENDDO

      DO K = 1, 15
        ACRIT(K) = ACRITT(K) * (975. - PCRIT(K))
      ENDDO
      DT2 = DELT

      val   =         1200.
      dtmin = max(dt2, val )

      val   =         3600.
      dtmax = max(dt2, val )

      MBDT    = 10.
      EDTMAXl = .3
      EDTMAXs = .3
      ALPHAl  = .5
      ALPHAs  = .5
      BETAl   = .15
      betas   = .15
      BETAl   = .05
      betas   = .05

        BETAl = .5
        betas = .5

      evfact  = 0.3
      evfactl = 0.3

         evfact = 0.6
         evfactl = .6

      ALPHAl  = .5
      ALPHAs  = .75
      BETAl   = .05
      betas   = .05
      evfact  = 0.5
      evfactl = 0.5
      PDPDWN  = 0.
      PDETRN  = 200.
      xlambu  = 1.e-4
      fjcap   = (float(jcap) / 126.) ** 2

      val     =           1.
      fjcap   = max(fjcap,val)
      fkm     = (float(km) / 28.) ** 2

      fkm     = max(fkm,val)
      W1l     = -8.E-3 
      W2l     = -4.E-2
      W3l     = -5.E-3 
      W4l     = -5.E-4
      W1s     = -2.E-4
      W2s     = -2.E-3
      W3s     = -1.E-3
      W4s     = -2.E-5

        LATD  = 92
        lond  = 189









      DO I=1,IM
        KBMAX(I) = KM
        KBM(I)   = KM
        KMAX(I)  = KM
        TX1(I)   = 1.0 / PS(I)
      ENDDO

      DO K = 1, KM
        DO I=1,IM
          IF (prSL(I,K)*tx1(I) .GT. 0.45) KBMAX(I) = K + 1
          IF (prSL(I,K)*tx1(I) .GT. 0.70) KBM(I)   = K + 1
          IF (prSL(I,K)*tx1(I) .GT. 0.04) KMAX(I)  = MIN(KM,K + 1)
        ENDDO
      ENDDO
      DO I=1,IM
        KBMAX(I) = MIN(KBMAX(I),KMAX(I))
        KBM(I)   = MIN(KBM(I),KMAX(I))
      ENDDO




      DO K = 1, KM
        DO I=1,IM
          if (K .le. kmax(i)) then
            PFLD(I,k) = PRSL(I,K) * 10.0
            PWO(I,k)  = 0.
            PWDO(I,k) = 0.
            TO(I,k)   = T1(I,k)
            QO(I,k)   = Q1(I,k)
            UO(I,k)   = U1(I,k)
            VO(I,k)   = V1(I,k)
            DBYO(I,k) = 0.
            SUMZ(I,k) = 0.
            SUMH(I,k) = 0.
          endif
        ENDDO
      ENDDO









      DO K = 1, KM
        DO I=1,IM
          if (k .le. kmax(i)) then
         
         
            QESO(I,k) = 0.01 * fpvs(T1(I,K))      
         
            QESO(I,k) = EPS * QESO(I,k) / (PFLD(I,k) + EPSM1*QESO(I,k))
         
            val1      =             1.E-8
            QESO(I,k) = MAX(QESO(I,k), val1)
         
            val2      =           1.e-10
            QO(I,k)   = max(QO(I,k), val2 )
         
            TVO(I,k)  = TO(I,k) + DELTA * TO(I,k) * QO(I,k)
          endif
        ENDDO
      ENDDO




      DO K = 1, KM
        DO I=1,IM
          ZO(I,k) = PHIL(I,k) / G
        ENDDO
      ENDDO

      DO K = 1, KM
        DO I=1,IM
          if (K .le. kmax(i)) then

            tem       = PHIL(I,k) + CP * TO(I,k)
            HEO(I,k)  = tem  + HVAP * QO(I,k)
            HESO(I,k) = tem  + HVAP * QESO(I,k)

          endif
        ENDDO
      ENDDO




      DO I=1,IM
        HMAX(I) = HEO(I,1)
        KB(I) = 1
      ENDDO

      DO K = 2, KM
        DO I=1,IM
          if (k .le. kbm(i)) then
            IF(HEO(I,k).GT.HMAX(I).AND.CNVFLG(I)) THEN
              KB(I)   = K
              HMAX(I) = HEO(I,k)
            ENDIF
          endif
        ENDDO
      ENDDO







      DO K = 1, KM1
        DO I=1,IM
          if (k .le. kmax(i)-1) then
            DZ      = .5 * (ZO(I,k+1) - ZO(I,k))
            DP      = .5 * (PFLD(I,k+1) - PFLD(I,k))


            ES      = 0.01 * fpvs(TO(I,K+1))      

            PPRIME  = PFLD(I,k+1) + EPSM1 * ES
            QS      = EPS * ES / PPRIME
            DQSDP   = - QS / PPRIME
            DESDT   = ES * (FACT1 / TO(I,k+1) + FACT2 / (TO(I,k+1)**2))
            DQSDT   = QS * PFLD(I,k+1) * DESDT / (ES * PPRIME)
            GAMMA   = EL2ORC * QESO(I,k+1) / (TO(I,k+1)**2)
            DT      = (G * DZ + HVAP * DQSDP * DP) / (CP * (1. + GAMMA))
            DQ      = DQSDT * DT + DQSDP * DP
            TO(I,k) = TO(I,k+1) + DT
            QO(I,k) = QO(I,k+1) + DQ
            PO(I,k) = .5 * (PFLD(I,k) + PFLD(I,k+1))
          endif
        ENDDO
      ENDDO

      DO K = 1, KM1
        DO I=1,IM
          if (k .le. kmax(I)-1) then


            QESO(I,k) = 0.01 * fpvs(TO(I,K))      

            QESO(I,k) = EPS * QESO(I,k) / (PO(I,k) + EPSM1*QESO(I,k))

            val1      =             1.E-8
            QESO(I,k) = MAX(QESO(I,k), val1)

            val2      =           1.e-10
            QO(I,k)   = max(QO(I,k), val2 )

            HEO(I,k)  = .5 * G * (ZO(I,k) + ZO(I,k+1)) +                &
     &                  CP * TO(I,k) + HVAP * QO(I,k)
            HESO(I,k) = .5 * G * (ZO(I,k) + ZO(I,k+1)) +                & 
     &                  CP * TO(I,k) + HVAP * QESO(I,k)
            UO(I,k)   = .5 * (UO(I,k) + UO(I,k+1))
            VO(I,k)   = .5 * (VO(I,k) + VO(I,k+1))
          endif
        ENDDO
      ENDDO


















      DO I=1,IM
        IF(CNVFLG(I)) THEN
          INDX    = KB(I)
          HKBO(I) = HEO(I,INDX)
          QKBO(I) = QO(I,INDX)
          UKBO(I) = UO(I,INDX)
          VKBO(I) = VO(I,INDX)
        ENDIF
        FLG(I)    = CNVFLG(I)
        KBCON(I)  = KMAX(I)
      ENDDO

      DO K = 1, KM
        DO I=1,IM
          if (k .le. kbmax(i)) then
            IF(FLG(I).AND.K.GT.KB(I)) THEN
              HSBAR(I)   = HESO(I,k)
              IF(HKBO(I).GT.HSBAR(I)) THEN
                FLG(I)   = .FALSE.
                KBCON(I) = K
              ENDIF
            ENDIF
          endif
        ENDDO
      ENDDO
      DO I=1,IM
        IF(CNVFLG(I)) THEN
          PBCDIF(I) = -PFLD(I,KBCON(I)) + PFLD(I,KB(I))
          PDOT(I)   = 10.* DOT(I,KBCON(I))
          IF(PBCDIF(I).GT.150.)    CNVFLG(I) = .FALSE.
          IF(KBCON(I).EQ.KMAX(I))  CNVFLG(I) = .FALSE.
        ENDIF
      ENDDO

      TOTFLG = .TRUE.
      DO I=1,IM
        TOTFLG = TOTFLG .AND. (.NOT. CNVFLG(I))
      ENDDO
      IF(TOTFLG) RETURN

 6001 FORMAT(2X,-2P10F12.2)
 6002 FORMAT(2X,10F12.2)
 6003 FORMAT(2X,3P10F12.2)




      DO I = 1, IM
        alpha = alphas
        if(SLIMSK(I).eq.1.) alpha = alphal
        IF(CNVFLG(I)) THEN
          IF(KB(I).EQ.1) THEN
            DZ = .5 * (ZO(I,KBCON(I)) + ZO(I,KBCON(I)-1)) - ZO(I,1)
          ELSE
            DZ = .5 * (ZO(I,KBCON(I)) + ZO(I,KBCON(I)-1))               &
     &         - .5 * (ZO(I,KB(I)) + ZO(I,KB(I)-1))
          ENDIF
          IF(KBCON(I).NE.KB(I)) THEN

            XLAMB(I) = - LOG(ALPHA) / DZ
          ELSE
            XLAMB(I) = 0.
          ENDIF
        ENDIF
      ENDDO

      DO K = 1, KM
        DO I = 1, IM
          if (k .le. kmax(i) .and. CNVFLG(I)) then
            ETA(I,k)  = 1.
            ETAU(I,k) = 1.
          ENDIF
        ENDDO
      ENDDO
      DO K = KM1, 2, -1
        DO I = 1, IM
          if (k .le. kbmax(i)) then
            IF(CNVFLG(I).AND.K.LT.KBCON(I).AND.K.GE.KB(I)) THEN
              DZ        = .5 * (ZO(I,k+1) - ZO(I,k-1))
              ETA(I,k)  = ETA(I,k+1) * EXP(-XLAMB(I) * DZ)
              ETAU(I,k) = ETA(I,k)
            ENDIF
          endif
        ENDDO
      ENDDO
      DO I = 1, IM
        IF(CNVFLG(I).AND.KB(I).EQ.1.AND.KBCON(I).GT.1) THEN
          DZ = .5 * (ZO(I,2) - ZO(I,1))
          ETA(I,1) = ETA(I,2) * EXP(-XLAMB(I) * DZ)
          ETAU(I,1) = ETA(I,1)
        ENDIF
      ENDDO



      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          INDX         = KB(I)
          HCKO(I,INDX) = HKBO(I)
          QCKO(I,INDX) = QKBO(I)
          UCKO(I,INDX) = UKBO(I)
          VCKO(I,INDX) = VKBO(I)
          PWAVO(I)     = 0.
        ENDIF
      ENDDO



      DO K = 2, KM1
        DO I = 1, IM
          if (k .le. kmax(i)-1) then
            IF(CNVFLG(I).AND.K.GT.KB(I).AND.K.LE.KBCON(I)) THEN
              FACTOR = ETA(I,k-1) / ETA(I,k)
              ONEMF = 1. - FACTOR
              HCKO(I,k) = FACTOR * HCKO(I,k-1) + ONEMF *                &
     &                    .5 * (HEO(I,k) + HEO(I,k+1))
              UCKO(I,k) = FACTOR * UCKO(I,k-1) + ONEMF *                & 
     &                    .5 * (UO(I,k) + UO(I,k+1))
              VCKO(I,k) = FACTOR * VCKO(I,k-1) + ONEMF *                &
     &                    .5 * (VO(I,k) + VO(I,k+1))
              DBYO(I,k) = HCKO(I,k) - HESO(I,k)
            ENDIF
            IF(CNVFLG(I).AND.K.GT.KBCON(I)) THEN
              HCKO(I,k) = HCKO(I,k-1)
              UCKO(I,k) = UCKO(I,k-1)
              VCKO(I,k) = VCKO(I,k-1)
              DBYO(I,k) = HCKO(I,k) - HESO(I,k)
            ENDIF
          endif
        ENDDO
      ENDDO

      DO I = 1, IM
        FLG(I) = CNVFLG(I)
        KTCON(I) = 1
      ENDDO







      DO K = 2, KM
        DO I = 1, IM
          if (k .le. kmax(i)) then
            IF(DBYO(I,k).LT.0..AND.FLG(I).AND.K.GT.KBCON(I)) THEN
              KTCON(I) = K
              FLG(I) = .FALSE.
            ENDIF
          endif
        ENDDO
      ENDDO
      DO I = 1, IM
        IF(CNVFLG(I).AND.(PFLD(I,KBCON(I)) - PFLD(I,KTCON(I))).LT.150.) &
     &  CNVFLG(I) = .FALSE.
      ENDDO
      TOTFLG = .TRUE.
      DO I = 1, IM
        TOTFLG = TOTFLG .AND. (.NOT. CNVFLG(I))
      ENDDO
      IF(TOTFLG) RETURN



      DO I = 1, IM
        HMIN(I) = HEO(I,KBCON(I))
        LMIN(I) = KBMAX(I)
        JMIN(I) = KBMAX(I)
      ENDDO
      DO I = 1, IM
        DO K = KBCON(I), KBMAX(I)
          IF(HEO(I,k).LT.HMIN(I).AND.CNVFLG(I)) THEN
            LMIN(I) = K + 1
            HMIN(I) = HEO(I,k)
          ENDIF
        ENDDO
      ENDDO



      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          JMIN(I) = MIN(LMIN(I),KTCON(I)-1)
          XMBMAX(I) = .1
          JMIN(I) = MAX(JMIN(I),KBCON(I)+1)
        ENDIF
      ENDDO



      do k = 2, km1
        DO I = 1, IM
          if (k .le. kmax(i)-1) then
            if(CNVFLG(I).and.k.gt.JMIN(I).and.k.le.KTCON(I)) THEN
              SUMZ(I,k) = SUMZ(I,k-1) + .5 * (ZO(I,k+1) - ZO(I,k-1))
              SUMH(I,k) = SUMH(I,k-1) + .5 * (ZO(I,k+1) - ZO(I,k-1))    &
     &                  * HEO(I,k)
            ENDIF
          endif
        enddo
      enddo

      DO I = 1, IM
        IF(CNVFLG(I)) THEN



          KT2(I) = nint(XKT2(I)*float(KTCON(I)-JMIN(I))-.5)+JMIN(I)+1


          tem1 = (HCKO(I,JMIN(I)) - HESO(I,KT2(I)))
          tem2 = (SUMZ(I,KT2(I)) * HESO(I,KT2(I)) - SUMH(I,KT2(I)))
          if (abs(tem2) .gt. 0.000001) THEN
            XLAMB(I) = tem1 / tem2
          else
            CNVFLG(I) = .false.
          ENDIF


          XLAMB(I) = max(XLAMB(I),RZERO)
          XLAMB(I) = min(XLAMB(I),2.3/SUMZ(I,KT2(I)))
        ENDIF
      ENDDO

      DO I = 1, IM
       DWNFLG(I)  = CNVFLG(I)
       DWNFLG2(I) = CNVFLG(I)
       IF(CNVFLG(I)) THEN
        if(KT2(I).ge.KTCON(I)) DWNFLG(I) = .false.
      if(XLAMB(I).le.1.e-30.or.HCKO(I,JMIN(I))-HESO(I,KT2(I)).le.1.e-30)&
     &  DWNFLG(I) = .false.
        do k = JMIN(I), KT2(I)
          if(DWNFLG(I).and.HEO(I,k).gt.HESO(I,KT2(I))) DWNFLG(I)=.false.
        enddo


        IF(CNVFLG(I).AND.(PFLD(I,KBCON(I))-PFLD(I,KTCON(I))).LT.PDPDWN) &
     &     DWNFLG2(I)=.FALSE.
       ENDIF
      ENDDO

      DO K = 2, KM1
        DO I = 1, IM
          if (k .le. kmax(i)-1) then
            IF(DWNFLG(I).AND.K.GT.JMIN(I).AND.K.LE.KT2(I)) THEN
              DZ        = .5 * (ZO(I,k+1) - ZO(I,k-1))



              ETA(I,k)  = ETA(I,k-1) * (1. + XLAMB(I) * dz)
              ETAU(I,k) = ETAU(I,k-1) * (1. + (XLAMB(I)+xlambu) * dz)
            ENDIF
          endif
        ENDDO
      ENDDO

      DO K = 2, KM1
        DO I = 1, IM
          if (k .le. kmax(i)-1) then

            IF(.NOT.DWNFLG(I).AND.K.GT.JMIN(I).AND.K.LE.KTCON(I)) THEN
              DZ        = .5 * (ZO(I,k+1) - ZO(I,k-1))
              ETAU(I,k) = ETAU(I,k-1) * (1. + xlambu * dz)
            ENDIF
          endif
        ENDDO
      ENDDO












      DO I = 1, IM
        if(DWNFLG(I)) THEN
          KTCON(I) = KT2(I)
        ENDIF
      ENDDO



      DO K = 2, KM1
        DO I = 1, IM
          if (k .le. kmax(i)-1) then

            IF(CNVFLG(I).AND.K.GT.KBCON(I).AND.K.LE.KTCON(I)) THEN

              FACTOR    = ETA(I,k-1) / ETA(I,k)
              ONEMF     = 1. - FACTOR
              fuv       = ETAU(I,k-1) / ETAU(I,k)
              onemfu    = 1. - fuv
              HCKO(I,k) = FACTOR * HCKO(I,k-1) + ONEMF *                &
     &                    .5 * (HEO(I,k) + HEO(I,k+1))
              UCKO(I,k) = fuv * UCKO(I,k-1) + ONEMFu *                  &
     &                    .5 * (UO(I,k) + UO(I,k+1))
              VCKO(I,k) = fuv * VCKO(I,k-1) + ONEMFu *                  &
     &                    .5 * (VO(I,k) + VO(I,k+1))
              DBYO(I,k) = HCKO(I,k) - HESO(I,k)
            ENDIF
          endif
        ENDDO
      ENDDO




      DO I = 1, IM
        if(CNVFLG(I).and.DWNFLG2(I).and.JMIN(I).le.KBCON(I))            &
     &     THEN
          CNVFLG(I) = .false.
          DWNFLG(I) = .false.
          DWNFLG2(I) = .false.
        ENDIF
      ENDDO

      TOTFLG = .TRUE.
      DO I = 1, IM
        TOTFLG = TOTFLG .AND. (.NOT. CNVFLG(I))
      ENDDO
      IF(TOTFLG) RETURN




      DO I = 1, IM
          AA1(I) = 0.
          RHBAR(I) = 0.
      ENDDO
      DO K = 1, KM
        DO I = 1, IM
          if (k .le. kmax(i)) then
            IF(CNVFLG(I).AND.K.GT.KB(I).AND.K.LT.KTCON(I)) THEN
              DZ = .5 * (ZO(I,k+1) - ZO(I,k-1))
              DZ1 = (ZO(I,k) - ZO(I,k-1))
              GAMMA = EL2ORC * QESO(I,k) / (TO(I,k)**2)
              QRCH = QESO(I,k)                                          &
     &             + GAMMA * DBYO(I,k) / (HVAP * (1. + GAMMA))
              FACTOR = ETA(I,k-1) / ETA(I,k)
              ONEMF = 1. - FACTOR
              QCKO(I,k) = FACTOR * QCKO(I,k-1) + ONEMF *                &
     &                    .5 * (QO(I,k) + QO(I,k+1))
              DQ = ETA(I,k) * QCKO(I,k) - ETA(I,k) * QRCH
              RHBAR(I) = RHBAR(I) + QO(I,k) / QESO(I,k)



              IF(DQ.GT.0.) THEN
                ETAH = .5 * (ETA(I,k) + ETA(I,k-1))
                QLK = DQ / (ETA(I,k) + ETAH * C0 * DZ)
                AA1(I) = AA1(I) - DZ1 * G * QLK
                QC = QLK + QRCH
                PWO(I,k) = ETAH * C0 * DZ * QLK
                QCKO(I,k) = QC
                PWAVO(I) = PWAVO(I) + PWO(I,k)
              ENDIF
            ENDIF
          endif
        ENDDO
      ENDDO
      DO I = 1, IM
        RHBAR(I) = RHBAR(I) / float(KTCON(I) - KB(I) - 1)
      ENDDO



      if(ncloud.gt.0) THEN



      DO I = 1, IM
        k = KTCON(I)
        IF(CNVFLG(I)) THEN
          GAMMA = EL2ORC * QESO(I,K) / (TO(I,K)**2)
          QRCH = QESO(I,K)                                              &
     &         + GAMMA * DBYO(I,K) / (HVAP * (1. + GAMMA))
          DQ = QCKO(I,K-1) - QRCH



          IF(DQ.GT.0.) THEN
            QLKO_KTCON(I) = dq
            QCKO(I,K-1) = QRCH
          ENDIF
        ENDIF
      ENDDO
      ENDIF



      DO K = 1, KM
        DO I = 1, IM
          if (k .le. kmax(i)) then
            IF(CNVFLG(I).AND.K.GT.KBCON(I).AND.K.LE.KTCON(I)) THEN
              DZ1 = ZO(I,k) - ZO(I,k-1)
              GAMMA = EL2ORC * QESO(I,k-1) / (TO(I,k-1)**2)
              RFACT =  1. + DELTA * CP * GAMMA                          &
     &                 * TO(I,k-1) / HVAP
              AA1(I) = AA1(I) +                                         &
     &                 DZ1 * (G / (CP * TO(I,k-1)))                     &
     &                 * DBYO(I,k-1) / (1. + GAMMA)                     &
     &                 * RFACT
              val = 0.
              AA1(I)=AA1(I)+                                            &
     &                 DZ1 * G * DELTA *                                &

     &                 MAX(val,(QESO(I,k-1) - QO(I,k-1)))
            ENDIF
          endif
        ENDDO
      ENDDO
      DO I = 1, IM
        IF(CNVFLG(I).AND.AA1(I).LE.0.) DWNFLG(I)  = .FALSE.
        IF(CNVFLG(I).AND.AA1(I).LE.0.) DWNFLG2(I) = .FALSE.
        IF(CNVFLG(I).AND.AA1(I).LE.0.) CNVFLG(I)  = .FALSE.
      ENDDO

      TOTFLG = .TRUE.
      DO I = 1, IM
        TOTFLG = TOTFLG .AND. (.NOT. CNVFLG(I))
      ENDDO
      IF(TOTFLG) RETURN










      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          VSHEAR(I) = 0.
        ENDIF
      ENDDO
      DO K = 1, KM
        DO I = 1, IM
          if (k .le. kmax(i)) then
            IF(K.GE.KB(I).AND.K.LE.KTCON(I).AND.CNVFLG(I)) THEN
              shear=rcs(I) * sqrt((UO(I,k+1)-UO(I,k)) ** 2              &
     &                          + (VO(I,k+1)-VO(I,k)) ** 2)
              VSHEAR(I) = VSHEAR(I) + SHEAR
            ENDIF
          endif
        ENDDO
      ENDDO
      DO I = 1, IM
        EDT(I) = 0.
        IF(CNVFLG(I)) THEN
          KNUMB = KTCON(I) - KB(I) + 1
          KNUMB = MAX(KNUMB,1)
          VSHEAR(I) = 1.E3 * VSHEAR(I) / (ZO(I,KTCON(I))-ZO(I,KB(I)))
          E1=1.591-.639*VSHEAR(I)                                       &
     &       +.0953*(VSHEAR(I)**2)-.00496*(VSHEAR(I)**3)
          EDT(I)=1.-E1

          val =         .9
          EDT(I) = MIN(EDT(I),val)

          val =         .0
          EDT(I) = MAX(EDT(I),val)
          EDTO(I)=EDT(I)
          EDTX(I)=EDT(I)
        ENDIF
      ENDDO

      DO I = 1, IM
        KBDTR(I) = KBCON(I)
        beta = betas
        if(SLIMSK(I).eq.1.) beta = betal
        IF(CNVFLG(I)) THEN
          KBDTR(I) = KBCON(I)
          KBDTR(I) = MAX(KBDTR(I),1)
          XLAMD(I) = 0.
          IF(KBDTR(I).GT.1) THEN
            DZ = .5 * ZO(I,KBDTR(I)) + .5 * ZO(I,KBDTR(I)-1)            &
     &         - ZO(I,1)
            XLAMD(I) =  LOG(BETA) / DZ
          ENDIF
        ENDIF
      ENDDO

      DO K = 1, KM
        DO I = 1, IM
          IF(k .le. kmax(i)) then
            IF(CNVFLG(I)) THEN
              ETAD(I,k) = 1.
            ENDIF
            QRCDO(I,k) = 0.
          endif
        ENDDO
      ENDDO
      DO K = KM1, 2, -1
        DO I = 1, IM
          if (k .le. kbmax(i)) then
            IF(CNVFLG(I).AND.K.LT.KBDTR(I)) THEN
              DZ        = .5 * (ZO(I,k+1) - ZO(I,k-1))
              ETAD(I,k) = ETAD(I,k+1) * EXP(XLAMD(I) * DZ)
            ENDIF
          endif
        ENDDO
      ENDDO
      K = 1
      DO I = 1, IM
        IF(CNVFLG(I).AND.KBDTR(I).GT.1) THEN
          DZ = .5 * (ZO(I,2) - ZO(I,1))
          ETAD(I,k) = ETAD(I,k+1) * EXP(XLAMD(I) * DZ)
        ENDIF
      ENDDO



      DO I = 1, IM
        PWEVO(I) = 0.
        FLG(I) = CNVFLG(I)
      ENDDO
      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          JMN = JMIN(I)
          HCDO(I) = HEO(I,JMN)
          QCDO(I) = QO(I,JMN)
          QRCDO(I,JMN) = QESO(I,JMN)
          UCDO(I) = UO(I,JMN)
          VCDO(I) = VO(I,JMN)
        ENDIF
      ENDDO
      DO K = KM1, 1, -1
        DO I = 1, IM
          if (k .le. kmax(i)-1) then
            IF(CNVFLG(I).AND.K.LT.JMIN(I)) THEN
              DQ = QESO(I,k)
              DT = TO(I,k)
              GAMMA      = EL2ORC * DQ / DT**2
              DH         = HCDO(I) - HESO(I,k)
              QRCDO(I,k) = DQ+(1./HVAP)*(GAMMA/(1.+GAMMA))*DH
              DETAD      = ETAD(I,k+1) - ETAD(I,k)
              PWDO(I,k)  = ETAD(I,k+1) * QCDO(I) -                      &
     &                     ETAD(I,k) * QRCDO(I,k)
              PWDO(I,k)  = PWDO(I,k) - DETAD *                          &
     &                    .5 * (QRCDO(I,k) + QRCDO(I,k+1))
              QCDO(I)    = QRCDO(I,k)
              PWEVO(I)   = PWEVO(I) + PWDO(I,k)
            ENDIF
          endif
        ENDDO
      ENDDO








      DO I = 1, IM
        edtmax = edtmaxl
        if(SLIMSK(I).eq.0.) edtmax = edtmaxs
        IF(DWNFLG2(I)) THEN
          IF(PWEVO(I).LT.0.) THEN
            EDTO(I) = -EDTO(I) * PWAVO(I) / PWEVO(I)
            EDTO(I) = MIN(EDTO(I),EDTMAX)
          ELSE
            EDTO(I) = 0.
          ENDIF
        ELSE
          EDTO(I) = 0.
        ENDIF
      ENDDO





      DO K = KM1, 1, -1
        DO I = 1, IM
          if (k .le. kmax(i)-1) then
            IF(DWNFLG2(I).AND.K.LT.JMIN(I)) THEN
              GAMMA = EL2ORC * QESO(I,k+1) / TO(I,k+1)**2
              DHH=HCDO(I)
              DT=TO(I,k+1)
              DG=GAMMA
              DH=HESO(I,k+1)
              DZ=-1.*(ZO(I,k+1)-ZO(I,k))
              AA1(I)=AA1(I)+EDTO(I)*DZ*(G/(CP*DT))*((DHH-DH)/(1.+DG))   &
     &               *(1.+DELTA*CP*DG*DT/HVAP)
              val=0.
              AA1(I)=AA1(I)+EDTO(I)*                                    & 

     &        DZ*G*DELTA*MAX(val,(QESO(I,k+1)-QO(I,k+1)))
            ENDIF
          endif
        ENDDO
      ENDDO



      DO I = 1, IM
        IF(AA1(I).LE.0.) CNVFLG(I)  = .FALSE.
        IF(AA1(I).LE.0.) DWNFLG(I)  = .FALSE.
        IF(AA1(I).LE.0.) DWNFLG2(I) = .FALSE.
      ENDDO

      TOTFLG = .TRUE.
      DO I = 1, IM
        TOTFLG = TOTFLG .AND. (.NOT. CNVFLG(I))
      ENDDO
      IF(TOTFLG) RETURN






      DO K = 1, KM
        DO I = 1, IM
          IF(k .le. kmax(i) .and. CNVFLG(I)) THEN
            DELLAH(I,k) = 0.
            DELLAQ(I,k) = 0.
            DELLAU(I,k) = 0.
            DELLAV(I,k) = 0.
          ENDIF
        ENDDO
      ENDDO
      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          DP = 1000. * DEL(I,1)
          DELLAH(I,1) = EDTO(I) * ETAD(I,1) * (HCDO(I)                  &
     &                - HEO(I,1)) * G / DP
          DELLAQ(I,1) = EDTO(I) * ETAD(I,1) * (QCDO(I)                  &
     &                - QO(I,1)) * G / DP
          DELLAU(I,1) = EDTO(I) * ETAD(I,1) * (UCDO(I)                  &
     &                - UO(I,1)) * G / DP
          DELLAV(I,1) = EDTO(I) * ETAD(I,1) * (VCDO(I)                  &
     &                - VO(I,1)) * G / DP
        ENDIF
      ENDDO



      DO K = 2, KM1
        DO I = 1, IM
          if (k .le. kmax(i)-1) then
            IF(CNVFLG(I).AND.K.LT.KTCON(I)) THEN
              AUP = 1.
              IF(K.LE.KB(I)) AUP = 0.
              ADW = 1.
              IF(K.GT.JMIN(I)) ADW = 0.
              DV1= HEO(I,k)
              DV2 = .5 * (HEO(I,k) + HEO(I,k+1))
              DV3= HEO(I,k-1)
              DV1Q= QO(I,k)
              DV2Q = .5 * (QO(I,k) + QO(I,k+1))
              DV3Q= QO(I,k-1)
              DV1U= UO(I,k)
              DV2U = .5 * (UO(I,k) + UO(I,k+1))
              DV3U= UO(I,k-1)
              DV1V= VO(I,k)
              DV2V = .5 * (VO(I,k) + VO(I,k+1))
              DV3V= VO(I,k-1)
              DP = 1000. * DEL(I,K)
              DZ = .5 * (ZO(I,k+1) - ZO(I,k-1))
              DETA = ETA(I,k) - ETA(I,k-1)
              DETAD = ETAD(I,k) - ETAD(I,k-1)
              DELLAH(I,k) = DELLAH(I,k) +                               &
     &            ((AUP * ETA(I,k) - ADW * EDTO(I) * ETAD(I,k)) * DV1   &
     &        - (AUP * ETA(I,k-1) - ADW * EDTO(I) * ETAD(I,k-1))* DV3   &
     &                    - AUP * DETA * DV2                            &
     &                    + ADW * EDTO(I) * DETAD * HCDO(I)) * G / DP
              DELLAQ(I,k) = DELLAQ(I,k) +                               &
     &            ((AUP * ETA(I,k) - ADW * EDTO(I) * ETAD(I,k)) * DV1Q  &
     &        - (AUP * ETA(I,k-1) - ADW * EDTO(I) * ETAD(I,k-1))* DV3Q  &
     &                    - AUP * DETA * DV2Q                           &
     &       +ADW*EDTO(I)*DETAD*.5*(QRCDO(I,k)+QRCDO(I,k-1))) * G / DP
              DELLAU(I,k) = DELLAU(I,k) +                               &
     &            ((AUP * ETA(I,k) - ADW * EDTO(I) * ETAD(I,k)) * DV1U  &
     &        - (AUP * ETA(I,k-1) - ADW * EDTO(I) * ETAD(I,k-1))* DV3U  &
     &                     - AUP * DETA * DV2U                          &
     &                    + ADW * EDTO(I) * DETAD * UCDO(I)             & 
     &                    ) * G / DP
              DELLAV(I,k) = DELLAV(I,k) +                               &
     &            ((AUP * ETA(I,k) - ADW * EDTO(I) * ETAD(I,k)) * DV1V  &
     &        - (AUP * ETA(I,k-1) - ADW * EDTO(I) * ETAD(I,k-1))* DV3V  &
     &                     - AUP * DETA * DV2V                          &
     &                    + ADW * EDTO(I) * DETAD * VCDO(I)             &
     &                    ) * G / DP
            ENDIF
          endif
        ENDDO
      ENDDO



      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          INDX = KTCON(I)
          DP = 1000. * DEL(I,INDX)
          DV1 = HEO(I,INDX-1)
          DELLAH(I,INDX) = ETA(I,INDX-1) *                              &
     &                     (HCKO(I,INDX-1) - DV1) * G / DP
          DVQ1 = QO(I,INDX-1) 
          DELLAQ(I,INDX) = ETA(I,INDX-1) *                              &
     &                     (QCKO(I,INDX-1) - DVQ1) * G / DP
          DV1U = UO(I,INDX-1)
          DELLAU(I,INDX) = ETA(I,INDX-1) *                              &
     &                     (UCKO(I,INDX-1) - DV1U) * G / DP
          DV1V = VO(I,INDX-1)
          DELLAV(I,INDX) = ETA(I,INDX-1) *                              &
     &                     (VCKO(I,INDX-1) - DV1V) * G / DP



          DELLAL(I) = ETA(I,INDX-1) * QLKO_KTCON(I) * g / dp
        ENDIF
      ENDDO



      DO K = 1, KM
        DO I = 1, IM
          if (k .le. kmax(i)) then
            IF(CNVFLG(I).and.k.gt.KTCON(I)) THEN
              QO(I,k) = Q1(I,k)
              TO(I,k) = T1(I,k)
              UO(I,k) = U1(I,k)
              VO(I,k) = V1(I,k)
            ENDIF
            IF(CNVFLG(I).AND.K.LE.KTCON(I)) THEN
              QO(I,k) = DELLAQ(I,k) * MBDT + Q1(I,k)
              DELLAT = (DELLAH(I,k) - HVAP * DELLAQ(I,k)) / CP
              TO(I,k) = DELLAT * MBDT + T1(I,k)

              val   =           1.e-10
              QO(I,k) = max(QO(I,k), val  )
            ENDIF
          endif
        ENDDO
      ENDDO











      DO K = 1, KM
        DO I = 1, IM
          IF(k .le. kmax(i) .and. CNVFLG(I)) THEN


            QESO(I,k) = 0.01 * fpvs(TO(I,K))      

            QESO(I,k) = EPS * QESO(I,k) / (PFLD(I,k)+EPSM1*QESO(I,k))

            val       =             1.E-8
            QESO(I,k) = MAX(QESO(I,k), val )
            TVO(I,k)  = TO(I,k) + DELTA * TO(I,k) * QO(I,k)
          ENDIF
        ENDDO
      ENDDO
      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          XAA0(I) = 0.
          XPWAV(I) = 0.
        ENDIF
      ENDDO





















      DO K = 1, KM1
        DO I = 1, IM
          IF(k .le. kmax(i)-1 .and. CNVFLG(I)) THEN
            DZ = .5 * (ZO(I,k+1) - ZO(I,k))
            DP = .5 * (PFLD(I,k+1) - PFLD(I,k))


            ES = 0.01 * fpvs(TO(I,K+1))      

            PPRIME = PFLD(I,k+1) + EPSM1 * ES
            QS = EPS * ES / PPRIME
            DQSDP = - QS / PPRIME
            DESDT = ES * (FACT1 / TO(I,k+1) + FACT2 / (TO(I,k+1)**2))
            DQSDT = QS * PFLD(I,k+1) * DESDT / (ES * PPRIME)
            GAMMA = EL2ORC * QESO(I,k+1) / (TO(I,k+1)**2)
            DT = (G * DZ + HVAP * DQSDP * DP) / (CP * (1. + GAMMA))
            DQ = DQSDT * DT + DQSDP * DP
            TO(I,k) = TO(I,k+1) + DT
            QO(I,k) = QO(I,k+1) + DQ
            PO(I,k) = .5 * (PFLD(I,k) + PFLD(I,k+1))
          ENDIF
        ENDDO
      ENDDO
      DO K = 1, KM1
        DO I = 1, IM
          IF(k .le. kmax(i)-1 .and. CNVFLG(I)) THEN


            QESO(I,k) = 0.01 * fpvs(TO(I,K))      

            QESO(I,k) = EPS * QESO(I,k) / (PO(I,k) + EPSM1 * QESO(I,k))

            val1      =             1.E-8
            QESO(I,k) = MAX(QESO(I,k), val1)

            val2      =           1.e-10
            QO(I,k)   = max(QO(I,k), val2 )

            HEO(I,k)   = .5 * G * (ZO(I,k) + ZO(I,k+1)) +               &
     &                    CP * TO(I,k) + HVAP * QO(I,k)
            HESO(I,k) = .5 * G * (ZO(I,k) + ZO(I,k+1)) +                &
     &                  CP * TO(I,k) + HVAP * QESO(I,k)
          ENDIF
        ENDDO
      ENDDO
      DO I = 1, IM
        k = kmax(i)
        IF(CNVFLG(I)) THEN
          HEO(I,k) = G * ZO(I,k) + CP * TO(I,k) + HVAP * QO(I,k)
          HESO(I,k) = G * ZO(I,k) + CP * TO(I,k) + HVAP * QESO(I,k)

        ENDIF
      ENDDO
      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          INDX = KB(I)
          XHKB(I) = HEO(I,INDX)
          XQKB(I) = QO(I,INDX)
          HCKO(I,INDX) = XHKB(I)
          QCKO(I,INDX) = XQKB(I)
        ENDIF
      ENDDO







      DO K = 2, KM1
        DO I = 1, IM
          if (k .le. kmax(i)-1) then

            IF(CNVFLG(I).AND.K.GT.KB(I).AND.K.LE.KTCON(I)) THEN
              FACTOR = ETA(I,k-1) / ETA(I,k)
              ONEMF = 1. - FACTOR
              HCKO(I,k) = FACTOR * HCKO(I,k-1) + ONEMF *                &
     &                    .5 * (HEO(I,k) + HEO(I,k+1))
            ENDIF



          endif
        ENDDO
      ENDDO
      DO K = 2, KM1
        DO I = 1, IM
          if (k .le. kmax(i)-1) then
            IF(CNVFLG(I).AND.K.GT.KB(I).AND.K.LT.KTCON(I)) THEN
              DZ = .5 * (ZO(I,k+1) - ZO(I,k-1))
              GAMMA = EL2ORC * QESO(I,k) / (TO(I,k)**2)
              XDBY = HCKO(I,k) - HESO(I,k)

              val  =          0.
              XDBY = MAX(XDBY,val)
              XQRCH = QESO(I,k)                                         &
     &              + GAMMA * XDBY / (HVAP * (1. + GAMMA))
              FACTOR = ETA(I,k-1) / ETA(I,k)
              ONEMF = 1. - FACTOR
              QCKO(I,k) = FACTOR * QCKO(I,k-1) + ONEMF *                &
     &                    .5 * (QO(I,k) + QO(I,k+1))
              DQ = ETA(I,k) * QCKO(I,k) - ETA(I,k) * XQRCH
              IF(DQ.GT.0.) THEN
                ETAH = .5 * (ETA(I,k) + ETA(I,k-1))
                QLK = DQ / (ETA(I,k) + ETAH * C0 * DZ)
                XAA0(I) = XAA0(I) - (ZO(I,k) - ZO(I,k-1)) * G * QLK
                XQC = QLK + XQRCH
                XPW = ETAH * C0 * DZ * QLK
                QCKO(I,k) = XQC
                XPWAV(I) = XPWAV(I) + XPW
              ENDIF
            ENDIF

            IF(CNVFLG(I).AND.K.GT.KBCON(I).AND.K.LE.KTCON(I)) THEN
              DZ1 = ZO(I,k) - ZO(I,k-1)
              GAMMA = EL2ORC * QESO(I,k-1) / (TO(I,k-1)**2)
              RFACT =  1. + DELTA * CP * GAMMA                          &
     &                 * TO(I,k-1) / HVAP
              XDBY = HCKO(I,k-1) - HESO(I,k-1)
              XAA0(I) = XAA0(I)                                         & 
     &                + DZ1 * (G / (CP * TO(I,k-1)))                    &
     &                * XDBY / (1. + GAMMA)                             &
     &                * RFACT
              val=0.
              XAA0(I)=XAA0(I)+                                          &
     &                 DZ1 * G * DELTA *                                &

     &                 MAX(val,(QESO(I,k-1) - QO(I,k-1)))
            ENDIF
          endif
        ENDDO
      ENDDO









      DO I = 1, IM
        XPWEV(I) = 0.
      ENDDO
      DO I = 1, IM
        IF(DWNFLG2(I)) THEN
          JMN = JMIN(I)
          XHCD(I) = HEO(I,JMN)
          XQCD(I) = QO(I,JMN)
          QRCD(I,JMN) = QESO(I,JMN)
        ENDIF
      ENDDO
      DO K = KM1, 1, -1
        DO I = 1, IM
          if (k .le. kmax(i)-1) then
            IF(DWNFLG2(I).AND.K.LT.JMIN(I)) THEN
              DQ = QESO(I,k)
              DT = TO(I,k)
              GAMMA    = EL2ORC * DQ / DT**2
              DH       = XHCD(I) - HESO(I,k)
              QRCD(I,k)=DQ+(1./HVAP)*(GAMMA/(1.+GAMMA))*DH
              DETAD    = ETAD(I,k+1) - ETAD(I,k)
              XPWD     = ETAD(I,k+1) * QRCD(I,k+1) -                    &
     &                   ETAD(I,k) * QRCD(I,k)
              XPWD     = XPWD - DETAD *                                 & 
     &                 .5 * (QRCD(I,k) + QRCD(I,k+1))
              XPWEV(I) = XPWEV(I) + XPWD
            ENDIF
          endif
        ENDDO
      ENDDO

      DO I = 1, IM
        edtmax = edtmaxl
        if(SLIMSK(I).eq.0.) edtmax = edtmaxs
        IF(DWNFLG2(I)) THEN
          IF(XPWEV(I).GE.0.) THEN
            EDTX(I) = 0.
          ELSE
            EDTX(I) = -EDTX(I) * XPWAV(I) / XPWEV(I)
            EDTX(I) = MIN(EDTX(I),EDTMAX)
          ENDIF
        ELSE
          EDTX(I) = 0.
        ENDIF
      ENDDO






      DO K = KM1, 1, -1
        DO I = 1, IM
          if (k .le. kmax(i)-1) then
            IF(DWNFLG2(I).AND.K.LT.JMIN(I)) THEN
              GAMMA = EL2ORC * QESO(I,k+1) / TO(I,k+1)**2
              DHH=XHCD(I)
              DT= TO(I,k+1)
              DG= GAMMA
              DH= HESO(I,k+1)
              DZ=-1.*(ZO(I,k+1)-ZO(I,k))
              XAA0(I)=XAA0(I)+EDTX(I)*DZ*(G/(CP*DT))*((DHH-DH)/(1.+DG)) &
     &                *(1.+DELTA*CP*DG*DT/HVAP)
              val=0.
              XAA0(I)=XAA0(I)+EDTX(I)*                                  &

     &        DZ*G*DELTA*MAX(val,(QESO(I,k+1)-QO(I,k+1)))
            ENDIF
          endif
        ENDDO
      ENDDO






      DO I = 1, IM
        ACRT(I) = 0.
        IF(CNVFLG(I)) THEN

          IF(PFLD(I,KTCON(I)).LT.PCRIT(15))THEN
            ACRT(I)=ACRIT(15)*(975.-PFLD(I,KTCON(I)))                   &    
     &              /(975.-PCRIT(15))
          ELSE IF(PFLD(I,KTCON(I)).GT.PCRIT(1))THEN
            ACRT(I)=ACRIT(1)
          ELSE

            K =  int((850. - PFLD(I,KTCON(I)))/50.) + 2
            K = MIN(K,15)
            K = MAX(K,2)
            ACRT(I)=ACRIT(K)+(ACRIT(K-1)-ACRIT(K))*                     &
     &           (PFLD(I,KTCON(I))-PCRIT(K))/(PCRIT(K-1)-PCRIT(K))
           ENDIF


         ENDIF
      ENDDO
      DO I = 1, IM
        ACRTFCT(I) = 1.
        IF(CNVFLG(I)) THEN
          if(SLIMSK(I).eq.1.) THEN
            w1 = w1l
            w2 = w2l
            w3 = w3l
            w4 = w4l
          else
            w1 = w1s
            w2 = w2s
            w3 = w3s
            w4 = w4s
          ENDIF





          IF(PDOT(I).LE.W4) THEN
            ACRTFCT(I) = (PDOT(I) - W4) / (W3 - W4)
          ELSEIF(PDOT(I).GE.-W4) THEN
            ACRTFCT(I) = - (PDOT(I) + W4) / (W4 - W3)
          ELSE
            ACRTFCT(I) = 0.
          ENDIF

          val1    =             -1.
          ACRTFCT(I) = MAX(ACRTFCT(I),val1)

          val2    =             1.
          ACRTFCT(I) = MIN(ACRTFCT(I),val2)
          ACRTFCT(I) = 1. - ACRTFCT(I)









          DTCONV(I) = DT2 + max((1800. - DT2),RZERO) *                  &
     &                (PDOT(I) - W2) / (W1 - W2)


          DTCONV(I) = max(DTCONV(I),dtmin)
          DTCONV(I) = min(DTCONV(I),dtmax)

        ENDIF
      ENDDO



      DO I= 1, IM
        FLG(I) = CNVFLG(I)
        IF(CNVFLG(I)) THEN

          FLD(I) = (AA1(I) - ACRT(I) * ACRTFCT(I)) / DTCONV(I)
          IF(FLD(I).LE.0.) FLG(I) = .FALSE.
        ENDIF
        CNVFLG(I) = FLG(I)
        IF(CNVFLG(I)) THEN

          XK(I) = (XAA0(I) - AA1(I)) / MBDT
          IF(XK(I).GE.0.) FLG(I) = .FALSE.
        ENDIF



        CNVFLG(I) = FLG(I)
        IF(CNVFLG(I)) THEN
          XMB(I) = -FLD(I) / XK(I)
          XMB(I) = MIN(XMB(I),XMBMAX(I))
        ENDIF
      ENDDO





      TOTFLG = .TRUE.
      DO I = 1, IM
        TOTFLG = TOTFLG .AND. (.NOT. CNVFLG(I))
      ENDDO
      IF(TOTFLG) RETURN



      do k = 1, km
        DO I = 1, IM
          if (k .le. kmax(i)) then
            TO(I,k) = T1(I,k)
            QO(I,k) = Q1(I,k)


            QESO(I,k) = 0.01 * fpvs(T1(I,K))      

            QESO(I,k) = EPS * QESO(I,k) / (PFLD(I,k) + EPSM1*QESO(I,k))

            val     =             1.E-8
            QESO(I,k) = MAX(QESO(I,k), val )
          endif
        enddo
      enddo






      DO I = 1, IM
        DELHBAR(I) = 0.
        DELQBAR(I) = 0.
        DELTBAR(I) = 0.
        QCOND(I) = 0.
      ENDDO
      DO K = 1, KM
        DO I = 1, IM
          if (k .le. kmax(i)) then
            IF(CNVFLG(I).AND.K.LE.KTCON(I)) THEN
              AUP = 1.
              IF(K.Le.KB(I)) AUP = 0.
              ADW = 1.
              IF(K.GT.JMIN(I)) ADW = 0.
              DELLAT = (DELLAH(I,k) - HVAP * DELLAQ(I,k)) / CP
              T1(I,k) = T1(I,k) + DELLAT * XMB(I) * DT2
              Q1(I,k) = Q1(I,k) + DELLAQ(I,k) * XMB(I) * DT2
              U1(I,k) = U1(I,k) + DELLAU(I,k) * XMB(I) * DT2
              V1(I,k) = V1(I,k) + DELLAV(I,k) * XMB(I) * DT2
              DP = 1000. * DEL(I,K)
              DELHBAR(I) = DELHBAR(I) + DELLAH(I,k)*XMB(I)*DP/G
              DELQBAR(I) = DELQBAR(I) + DELLAQ(I,k)*XMB(I)*DP/G
              DELTBAR(I) = DELTBAR(I) + DELLAT*XMB(I)*DP/G
            ENDIF
          endif
        ENDDO
      ENDDO
      DO K = 1, KM
        DO I = 1, IM
          if (k .le. kmax(i)) then
            IF(CNVFLG(I).AND.K.LE.KTCON(I)) THEN


              QESO(I,k) = 0.01 * fpvs(T1(I,K))      

              QESO(I,k) = EPS * QESO(I,k)/(PFLD(I,k) + EPSM1*QESO(I,k))

              val     =             1.E-8
              QESO(I,k) = MAX(QESO(I,k), val )



              if(ncloud.gt.0.and.CNVFLG(I).and.k.eq.KTCON(I)) THEN
                tem  = DELLAL(I) * XMB(I) * dt2
                tem1 = MAX(RZERO, MIN(RONE, (TCR-t1(I,K))*TCRF))
                if (QL(I,k,2) .gt. -999.0) then
                  QL(I,k,1) = QL(I,k,1) + tem * tem1            
                  QL(I,k,2) = QL(I,k,2) + tem *(1.0-tem1)       
                else
                  tem2      = QL(I,k,1) + tem
                  QL(I,k,1) = tem2 * tem1                       
                  QL(I,k,2) = tem2 - QL(I,k,1)                  
                endif

                dp = 1000. * del(i,k)
                DELLAL(I) = DELLAL(I) * XMB(I) * dp / g
              ENDIF
            ENDIF
          endif
        ENDDO
      ENDDO











      DO I = 1, IM
        RNTOT(I) = 0.
        DELQEV(I) = 0.
        DELQ2(I) = 0.
        FLG(I) = CNVFLG(I)
      ENDDO
      DO K = KM, 1, -1
        DO I = 1, IM
          if (k .le. kmax(i)) then
            IF(CNVFLG(I).AND.K.LE.KTCON(I)) THEN
              AUP = 1.
              IF(K.Le.KB(I)) AUP = 0.
              ADW = 1.
              IF(K.GT.JMIN(I)) ADW = 0.
              rain =  AUP * PWO(I,k) + ADW * EDTO(I) * PWDO(I,k)
              RNTOT(I) = RNTOT(I) + rain * XMB(I) * .001 * dt2
            ENDIF
          endif
        ENDDO
      ENDDO
      DO K = KM, 1, -1
        DO I = 1, IM
          if (k .le. kmax(i)) then
            DELTV(I) = 0.
            DELQ(I) = 0.
            QEVAP(I) = 0.
            IF(CNVFLG(I).AND.K.LE.KTCON(I)) THEN
              AUP = 1.
              IF(K.Le.KB(I)) AUP = 0.
              ADW = 1.
              IF(K.GT.JMIN(I)) ADW = 0.
              rain =  AUP * PWO(I,k) + ADW * EDTO(I) * PWDO(I,k)
              RN(I) = RN(I) + rain * XMB(I) * .001 * dt2
            ENDIF
            IF(FLG(I).AND.K.LE.KTCON(I)) THEN
              evef = EDT(I) * evfact
              if(SLIMSK(I).eq.1.) evef=EDT(I) * evfactl


              QCOND(I) = EVEF * (Q1(I,k) - QESO(I,k))                   &
     &                 / (1. + EL2ORC * QESO(I,k) / T1(I,k)**2)
              DP = 1000. * DEL(I,K)
              IF(RN(I).GT.0..AND.QCOND(I).LT.0.) THEN
                QEVAP(I) = -QCOND(I) * (1.-EXP(-.32*SQRT(DT2*RN(I))))
                QEVAP(I) = MIN(QEVAP(I), RN(I)*1000.*G/DP)
                DELQ2(I) = DELQEV(I) + .001 * QEVAP(I) * dp / g
              ENDIF
              if(RN(I).gt.0..and.QCOND(I).LT.0..and.                    &
     &           DELQ2(I).gt.RNTOT(I)) THEN
                QEVAP(I) = 1000.* g * (RNTOT(I) - DELQEV(I)) / dp
                FLG(I) = .false.
              ENDIF
              IF(RN(I).GT.0..AND.QEVAP(I).gt.0.) THEN
                Q1(I,k) = Q1(I,k) + QEVAP(I)
                T1(I,k) = T1(I,k) - ELOCP * QEVAP(I)
                RN(I) = RN(I) - .001 * QEVAP(I) * DP / G
                DELTV(I) = - ELOCP*QEVAP(I)/DT2
                DELQ(I) =  + QEVAP(I)/DT2
                DELQEV(I) = DELQEV(I) + .001*dp*QEVAP(I)/g
              ENDIF
              DELLAQ(I,k) = DELLAQ(I,k) + DELQ(I) / XMB(I)
              DELQBAR(I) = DELQBAR(I) + DELQ(I)*DP/G
              DELTBAR(I) = DELTBAR(I) + DELTV(I)*DP/G
            ENDIF
          endif
        ENDDO
      ENDDO















      DO I = 1, IM
        IF(CNVFLG(I)) THEN





          if(RN(I).lt.0..and..not.FLG(I)) RN(I) = 0.
          IF(RN(I).LE.0.) THEN
            RN(I) = 0.
          ELSE
            KTOP(I) = KTCON(I)
            KBOT(I) = KBCON(I)
            KUO(I) = 1
            CLDWRK(I) = AA1(I)
          ENDIF
        ENDIF
      ENDDO
      DO K = 1, KM
        DO I = 1, IM
          if (k .le. kmax(i)) then
            IF(CNVFLG(I).AND.RN(I).LE.0.) THEN
              T1(I,k) = TO(I,k)
              Q1(I,k) = QO(I,k)
            ENDIF
          endif
        ENDDO
      ENDDO

      RETURN
   END SUBROUTINE OSASCNV



      SUBROUTINE SHALCV(IM,IX,KM,DT,DEL,PRSI,PRSL,PRSLK,KUO,Q,T,DPSHC)

      USE MODULE_GFS_MACHINE , ONLY : kind_phys
      USE MODULE_GFS_PHYSCONS, grav => con_g, CP => con_CP, HVAP => con_HVAP &
     &,             RD => con_RD

      implicit none



      integer              IM, IX, KM, KUO(IM)
      real(kind=kind_phys) DEL(IX,KM),   PRSI(IX,KM+1), PRSL(IX,KM),    &
     &                     PRSLK(IX,KM),                                &
     &                     Q(IX,KM),     T(IX,KM),      DT, DPSHC



      real(kind=kind_phys) ck,    cpdt,   dmse,   dsdz1, dsdz2,         &
     &                     dsig,  dtodsl, dtodsu, eldq,  g,             &
     &                     gocp,  rtdls

      integer              k,k1,k2,kliftl,kliftu,kt,N2,I,iku,ik1,ik,ii
      integer              INDEX2(IM), KLCL(IM), KBOT(IM), KTOP(IM),kk  &
     &,                    KTOPM(IM)


      PARAMETER(G=GRAV, GOCP=G/CP)

      PARAMETER(KLIFTL=2,KLIFTU=2)
      LOGICAL   LSHC(IM)
      real(kind=kind_phys) Q2(IM*KM),     T2(IM*KM),                    &
     &                     PRSL2(IM*KM),  PRSLK2(IM*KM),                &
     &                     AL(IM*(KM-1)), AD(IM*KM), AU(IM*(KM-1))



      DO I=1,IM
        LSHC(I)=.FALSE.
      ENDDO
      DO K=1,KM-1
        DO I=1,IM
          IF(KUO(I).EQ.0) THEN
            ELDQ    = HVAP*(Q(I,K)-Q(I,K+1))
            CPDT    = CP*(T(I,K)-T(I,K+1))
            RTDLS   = (PRSL(I,K)-PRSL(I,K+1)) /                         &
     &                 PRSI(I,K+1)*RD*0.5*(T(I,K)+T(I,K+1))
            DMSE    = ELDQ+CPDT-RTDLS
            LSHC(I) = LSHC(I).OR.DMSE.GT.0.
          ENDIF
        ENDDO
      ENDDO
      N2 = 0
      DO I=1,IM
        IF(LSHC(I)) THEN
          N2         = N2 + 1
          INDEX2(N2) = I
        ENDIF
      ENDDO
      IF(N2.EQ.0) RETURN
      DO K=1,KM
        KK = (K-1)*N2
        DO I=1,N2
          IK         = KK + I
          ii         = index2(i)
          Q2(IK)     = Q(II,K)
          T2(IK)     = T(II,K)
          PRSL2(IK)  = PRSL(II,K)
          PRSLK2(IK) = PRSLK(II,K)
        ENDDO
      ENDDO
      do i=1,N2
        ktopm(i) = KM
      enddo
      do k=2,KM
        do i=1,N2
          ii = index2(i)
          if (prsi(ii,1)-prsi(ii,k) .le. dpshc) ktopm(i) = k
        enddo
      enddo




      CALL MSTADBT3(N2,KM-1,KLIFTL,KLIFTU,PRSL2,PRSLK2,T2,Q2,           &
     &            KLCL,KBOT,KTOP,AL,AU)
      DO I=1,N2
        KBOT(I) = min(KLCL(I)-1, ktopm(i)-1)
        KTOP(I) = min(KTOP(I)+1, ktopm(i))
        LSHC(I) = .FALSE.
      ENDDO
      DO K=1,KM-1
        KK = (K-1)*N2
        DO I=1,N2
          IF(K.GE.KBOT(I).AND.K.LT.KTOP(I)) THEN
            IK      = KK + I
            IKU     = IK + N2
            ELDQ    = HVAP * (Q2(IK)-Q2(IKU))
            CPDT    = CP   * (T2(IK)-T2(IKU))
            RTDLS   = (PRSL2(IK)-PRSL2(IKU)) /                          &
     &                 PRSI(index2(i),K+1)*RD*0.5*(T2(IK)+T2(IKU))
            DMSE    = ELDQ + CPDT - RTDLS
            LSHC(I) = LSHC(I).OR.DMSE.GT.0.
            AU(IK)  = G/RTDLS
          ENDIF
        ENDDO
      ENDDO
      K1=KM+1
      K2=0
      DO I=1,N2
        IF(.NOT.LSHC(I)) THEN
          KBOT(I) = KM+1
          KTOP(I) = 0
        ENDIF
        K1 = MIN(K1,KBOT(I))
        K2 = MAX(K2,KTOP(I))
      ENDDO
      KT = K2-K1+1
      IF(KT.LT.2) RETURN




      KK = (K1-1) * N2
      DO I=1,N2
        IK     = KK + I
        AD(IK) = 1.
      ENDDO


      DO K=K1,K2-1



        KK = (K-1) * N2
        DO I=1,N2
          ii     = index2(i)
          DTODSL = DT/DEL(II,K)
          DTODSU = DT/DEL(II,K+1)
          DSIG   = PRSL(II,K) - PRSL(II,K+1)
          IK     = KK + I
          IKU    = IK + N2
          IF(K.EQ.KBOT(I)) THEN
            CK=1.5
          ELSEIF(K.EQ.KTOP(I)-1) THEN
            CK=1.
          ELSEIF(K.EQ.KTOP(I)-2) THEN
            CK=3.
          ELSEIF(K.GT.KBOT(I).AND.K.LT.KTOP(I)-2) THEN
            CK=5.
          ELSE
            CK=0.
          ENDIF
          DSDZ1   = CK*DSIG*AU(IK)*GOCP
          DSDZ2   = CK*DSIG*AU(IK)*AU(IK)
          AU(IK)  = -DTODSL*DSDZ2
          AL(IK)  = -DTODSU*DSDZ2
          AD(IK)  = AD(IK)-AU(IK)
          AD(IKU) = 1.-AL(IK)
          T2(IK)  = T2(IK)+DTODSL*DSDZ1
          T2(IKU) = T2(IKU)-DTODSU*DSDZ1
        ENDDO
      ENDDO
      IK1=(K1-1)*N2+1
      CALL TRIDI2T3(N2,KT,AL(IK1),AD(IK1),AU(IK1),Q2(IK1),T2(IK1),      &
     &                                  AU(IK1),Q2(IK1),T2(IK1))
      DO K=K1,K2
        KK = (K-1)*N2
        DO I=1,N2
          IK = KK + I
          Q(INDEX2(I),K) = Q2(IK)
          T(INDEX2(I),K) = T2(IK)
        ENDDO
      ENDDO

      RETURN
      END SUBROUTINE SHALCV

      SUBROUTINE TRIDI2T3(L,N,CL,CM,CU,R1,R2,AU,A1,A2)


      USE MODULE_GFS_MACHINE , ONLY : kind_phys
      implicit none
      integer             k,n,l,i
      real(kind=kind_phys) fk

      real(kind=kind_phys)                                              &
     &          CL(L,2:N),CM(L,N),CU(L,N-1),R1(L,N),R2(L,N),            &
     &          AU(L,N-1),A1(L,N),A2(L,N)

      DO I=1,L
        FK=1./CM(I,1)
        AU(I,1)=FK*CU(I,1)
        A1(I,1)=FK*R1(I,1)
        A2(I,1)=FK*R2(I,1)
      ENDDO
      DO K=2,N-1
        DO I=1,L
          FK=1./(CM(I,K)-CL(I,K)*AU(I,K-1))
          AU(I,K)=FK*CU(I,K)
          A1(I,K)=FK*(R1(I,K)-CL(I,K)*A1(I,K-1))
          A2(I,K)=FK*(R2(I,K)-CL(I,K)*A2(I,K-1))
        ENDDO
      ENDDO
      DO I=1,L
        FK=1./(CM(I,N)-CL(I,N)*AU(I,N-1))
        A1(I,N)=FK*(R1(I,N)-CL(I,N)*A1(I,N-1))
        A2(I,N)=FK*(R2(I,N)-CL(I,N)*A2(I,N-1))
      ENDDO
      DO K=N-1,1,-1
        DO I=1,L
          A1(I,K)=A1(I,K)-AU(I,K)*A1(I,K+1)
          A2(I,K)=A2(I,K)-AU(I,K)*A2(I,K+1)
        ENDDO
      ENDDO

      RETURN
      END SUBROUTINE TRIDI2T3


      SUBROUTINE MSTADBT3(IM,KM,K1,K2,PRSL,PRSLK,TENV,QENV,             &
     &                  KLCL,KBOT,KTOP,TCLD,QCLD)


      USE MODULE_GFS_MACHINE, ONLY : kind_phys
      USE MODULE_GFS_FUNCPHYS, ONLY : FTDP, FTHE, FTLCL, STMA
      USE MODULE_GFS_PHYSCONS, EPS => con_eps, EPSM1 => con_epsm1, FV => con_FVirt

      implicit none



      integer              k,k1,k2,km,i,im
      real(kind=kind_phys) pv,qma,slklcl,tdpd,thelcl,tlcl
      real(kind=kind_phys) tma,tvcld,tvenv

      real(kind=kind_phys) PRSL(IM,KM), PRSLK(IM,KM), TENV(IM,KM),      &
     &                     QENV(IM,KM), TCLD(IM,KM),  QCLD(IM,KM)
      INTEGER              KLCL(IM),    KBOT(IM),      KTOP(IM)

      real(kind=kind_phys) SLKMA(IM), THEMA(IM)




      DO I=1,IM
        SLKMA(I) = 0.
        THEMA(I) = 0.
      ENDDO
      DO K=K1,K2
        DO I=1,IM
          PV   = 1000.0 * PRSL(I,K)*QENV(I,K)/(EPS-EPSM1*QENV(I,K))
          TDPD = TENV(I,K)-FTDP(PV)
          IF(TDPD.GT.0.) THEN
            TLCL   = FTLCL(TENV(I,K),TDPD)
            SLKLCL = PRSLK(I,K)*TLCL/TENV(I,K)
          ELSE
            TLCL   = TENV(I,K)
            SLKLCL = PRSLK(I,K)
          ENDIF
          THELCL=FTHE(TLCL,SLKLCL)
          IF(THELCL.GT.THEMA(I)) THEN
            SLKMA(I) = SLKLCL
            THEMA(I) = THELCL
          ENDIF
        ENDDO
      ENDDO



      DO I=1,IM
        KLCL(I)=KM+1
        KBOT(I)=KM+1
        KTOP(I)=0
      ENDDO
      DO K=1,KM
        DO I=1,IM
          TCLD(I,K)=0.
          QCLD(I,K)=0.
        ENDDO
      ENDDO
      DO K=K1,KM
        DO I=1,IM
          IF(PRSLK(I,K).LE.SLKMA(I)) THEN
            KLCL(I)=MIN(KLCL(I),K)
            CALL STMA(THEMA(I),PRSLK(I,K),TMA,QMA)

            TVCLD=TMA*(1.+FV*QMA)
            TVENV=TENV(I,K)*(1.+FV*QENV(I,K))
            IF(TVCLD.GT.TVENV) THEN
              KBOT(I)=MIN(KBOT(I),K)
              KTOP(I)=MAX(KTOP(I),K)
              TCLD(I,K)=TMA-TENV(I,K)
              QCLD(I,K)=QMA-QENV(I,K)
            ENDIF
          ENDIF
        ENDDO
      ENDDO

      RETURN
      END SUBROUTINE MSTADBT3


      SUBROUTINE init_random_seed()
            INTEGER :: i, n, clock
            INTEGER, DIMENSION(:), ALLOCATABLE :: seed

            CALL RANDOM_SEED(size = n)
            ALLOCATE(seed(n))

            CALL SYSTEM_CLOCK(COUNT=clock)

            seed = clock + 37 * (/ (i - 1, i = 1, n) /)
            CALL RANDOM_SEED(PUT = seed)

            DEALLOCATE(seed)
      END SUBROUTINE 
      END MODULE module_cu_osas
