
      MODULE MODULE_BL_MYJURB



      USE MODULE_MODEL_CONSTANTS

















      INTEGER :: ITRMX=5 



      REAL,PARAMETER :: PI=3.1415926,VKARMAN=0.4

      REAL,PARAMETER :: CAPA=R_D/CP
      REAL,PARAMETER :: RLIVWV=XLS/XLV,ELOCP=2.72E6/CP
      REAL,PARAMETER :: EPS1=1.E-12,EPS2=0.
      REAL,PARAMETER :: EPSL=0.32,EPSRU=1.E-7,EPSRS=1.E-7               &
     &                 ,EPSTRB=1.E-24
      REAL,PARAMETER :: EPSA=1.E-8,EPSIT=1.E-4,EPSU2=1.E-4,EPSUST=0.07  &
     &                 ,FH=1.01 
      REAL,PARAMETER :: ALPH=0.30,BETA=1./273.,EL0MAX=1000.,EL0MIN=1.   &
     &                 ,ELFC=0.23*0.5,GAM1=0.2222222222222222222        &
     &                 ,PRT=1.
      REAL,PARAMETER :: A1=0.659888514560862645                         &
     &                 ,A2x=0.6574209922667784586                       &
     &                 ,B1=11.87799326209552761                         &
     &                 ,B2=7.226971804046074028                         &
     &                 ,C1=0.000830955950095854396
      REAL,PARAMETER :: A2S=17.2693882,A3S=273.16,A4S=35.86
      REAL,PARAMETER :: ELZ0=0.,ESQ=5.0,EXCM=0.001                      &
     &                 ,FHNEU=0.8,GLKBR=10.,GLKBS=30.                   &
     &                 ,QVISC=2.1E-5,RFC=0.191,RIC=0.505,SMALL=0.35     &
     &                 ,SQPR=0.84,SQSC=0.84,SQVISC=258.2,TVISC=2.1E-5   &
     &                 ,USTC=0.7,USTR=0.225,VISC=1.5E-5                 &
     &                 ,WOLD=0.15,WWST=1.2,ZTMAX=1.,ZTFC=1.,ZTMIN=-5.

      REAL,PARAMETER :: SEAFC=0.98,PQ0SEA=PQ0*SEAFC

      REAL,PARAMETER :: BTG=BETA*G,CZIV=SMALL*GLKBS                     &

     &                 ,ESQHF=0.5*5.0,GRRS=GLKBR/GLKBS                  &
     &                 ,RB1=1./B1,RTVISC=1./TVISC,RVISC=1./VISC         &
     &                 ,ZQRZT=SQSC/SQPR

      REAL,PARAMETER :: ADNH= 9.*A1*A2x*A2x*(12.*A1+3.*B2)*BTG*BTG      &                  
     &                 ,ADNM=18.*A1*A1*A2x*(B2-3.*A2x)*BTG              & 
     &                 ,ANMH=-9.*A1*A2x*A2x*BTG*BTG                     &
     &                 ,ANMM=-3.*A1*A2x*(3.*A2x+3.*B2*C1+18.*A1*C1-B2)  &
     &                                *BTG                              &   
     &                 ,BDNH= 3.*A2x*(7.*A1+B2)*BTG                     &
     &                 ,BDNM= 6.*A1*A1                                  &
     &                 ,BEQH= A2x*B1*BTG+3.*A2x*(7.*A1+B2)*BTG          &
     &                 ,BEQM=-A1*B1*(1.-3.*C1)+6.*A1*A1                 &
     &                 ,BNMH=-A2x*BTG                                   &     
     &                 ,BNMM=A1*(1.-3.*C1)                              &
     &                 ,BSHH=9.*A1*A2x*A2x*BTG                          &
     &                 ,BSHM=18.*A1*A1*A2x*C1                           &
     &                 ,BSMH=-3.*A1*A2x*(3.*A2x+3.*B2*C1+12.*A1*C1-B2)  &
     &                                *BTG                              &
     &                 ,CESH=A2x                                        &
     &                 ,CESM=A1*(1.-3.*C1)                              &
     &                 ,CNV=EP_1*G/BTG                                  &
     &                 ,ELFCS=VKARMAN*BTG                               &
     &                 ,FZQ1=RTVISC*QVISC*ZQRZT                         &
     &                 ,FZQ2=RTVISC*QVISC*ZQRZT                         &
     &                 ,FZT1=RVISC *TVISC*SQPR                          &
     &                 ,FZT2=CZIV*GRRS*TVISC*SQPR                       &
     &                 ,FZU1=CZIV*VISC                                  &
     &                 ,PIHF=0.5*PI                                     &
     &                 ,RFAC=RIC/(FHNEU*RFC*RFC)                        &
     &                 ,RQVISC=1./QVISC                                 &
     &                 ,RRIC=1./RIC                                     &
     &                 ,USTFC=0.018/G                                   &
     &                 ,WNEW=1.-WOLD                                    &
     &                 ,WWST2=WWST*WWST





      REAL,PARAMETER :: AEQH=9.*A1*A2x*A2x*B1*BTG*BTG                   &
     &                      +9.*A1*A2x*A2x*(12.*A1+3.*B2)*BTG*BTG       &
     &                 ,AEQM=3.*A1*A2x*B1*(3.*A2x+3.*B2*C1+18.*A1*C1-B2)&
     &                      *BTG+18.*A1*A1*A2x*(B2-3.*A2x)*BTG





      REAL,PARAMETER :: REQU=-AEQH/AEQM                                 &
     &                 ,EPSGH=1.E-9,EPSGM=REQU*EPSGH





      REAL,PARAMETER :: UBRYL=(18.*REQU*A1*A1*A2x*B2*C1*BTG             &
     &                         +9.*A1*A2x*A2x*B2*BTG*BTG)               &
     &                        /(REQU*ADNM+ADNH)                         &
     &                 ,UBRY=(1.+EPSRS)*UBRYL,UBRY3=3.*UBRY

      REAL,PARAMETER :: AUBH=27.*A1*A2x*A2x*B2*BTG*BTG-ADNH*UBRY3       &
     &                 ,AUBM=54.*A1*A1*A2x*B2*C1*BTG -ADNM*UBRY3        &
     &                 ,BUBH=(9.*A1*A2x+3.*A2x*B2)*BTG-BDNH*UBRY3       &
     &                 ,BUBM=18.*A1*A1*C1           -BDNM*UBRY3         &
     &                 ,CUBR=1.                     -     UBRY3         &
     &                 ,RCUBR=1./CUBR



      CONTAINS


      SUBROUTINE MYJURB(IDIFF,FLAG_BEP,DT,STEPBL,HT,DZ                                &
     &                 ,PMID,PINT,TH,T,EXNER,QV,CWM,U,V,RHO            &
     &                 ,TSK,QSFC,CHKLOWQ,THZ0,QZ0,UZ0,VZ0              &
     &                 ,LOWLYR,XLAND,SICE,SNOW                         &
     &                 ,TKE_MYJ,EXCH_H,EXCH_M,USTAR,ZNT,EL_MYJ,PBLH,KPBL,CT   &
     &                 ,AKHS,AKMS,ELFLX                                &
     &                 ,RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN     &

     &                 ,FRC_URB2D                                      & 
     &                 ,A_U_BEP,A_V_BEP,A_T_BEP,A_Q_BEP                &
     &                 ,A_E_BEP,B_U_BEP,B_V_BEP                        &
     &                 ,B_T_BEP,B_Q_BEP,B_E_BEP,DLG_BEP                &
     &                 ,DL_U_BEP,SF_BEP,VL_BEP                         &

     &                 ,IDS,IDE,JDS,JDE,KDS,KDE                        &
     &                 ,IMS,IME,JMS,JME,KMS,KME                        &
     &                 ,ITS,ITE,JTS,JTE,KTS,KTE)


      IMPLICIT NONE


      INTEGER,INTENT(IN) :: IDIFF
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE

      INTEGER,INTENT(IN) :: STEPBL

      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: LOWLYR

      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: KPBL

      REAL,INTENT(IN) :: DT

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: HT,SICE,SNOW       &
     &                                             ,TSK,XLAND

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: CWM,DZ     &
     &                                                     ,EXNER      &
     &                                                     ,PMID,PINT  &
     &                                                     ,QV,RHO     &
     &                                                     ,T,TH,U,V   

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: PBLH

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: AKHS,AKMS

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME)                          &
     &    ,INTENT(OUT) ::                      EL_MYJ                  &
     &                                        ,RQCBLTEN,RQVBLTEN       &
     &                                        ,RTHBLTEN                &
     &                                        ,RUBLTEN,RVBLTEN        

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: A_U_BEP    &
     &                                        ,A_V_BEP,A_T_BEP         &
     &                                        ,A_E_BEP,B_U_BEP         &
     &                                        ,A_Q_BEP,B_Q_BEP         &
     &                                        ,B_V_BEP,B_T_BEP         &
     &                                        ,B_E_BEP,DLG_BEP         &
     &                                        ,DL_U_BEP                &
     &                                        ,VL_BEP,SF_BEP
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: FRC_URB2D
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME)                          &
     &    ,INTENT(INOUT) ::                    EXCH_H,EXCH_M,TKE_MYJ


      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: CT,QSFC,QZ0     &
     &                                                ,THZ0,USTAR      &
     &                                                ,UZ0,VZ0,ZNT

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: CHKLOWQ,ELFLX
      LOGICAL,    INTENT(IN   )    ::     FLAG_BEP





      INTEGER :: I,J,K,KFLIP,LLOW,LMH,LMXL

      REAL,DIMENSION(KTS:KTE) :: A_U,A_V,A_T,A_E,B_U,B_V,B_T,B_E   
      REAL,DIMENSION(KTS:KTE) :: A_Q,B_Q,VLK,DL_U,B_E_FACE,DZ_KFLIP   
      REAL,DIMENSION(KTS:KTE) :: SFK                             
      INTEGER :: IURB
      REAL :: SLOPE,ICEPT



      INTEGER,DIMENSION(ITS:ITE,JTS:JTE) :: LPBL

      REAL :: AKHS_DENS,AKMS_DENS,APEX,DCDT,DELTAZ,DQDT,DTDIF,DTDT     &
     &       ,DTTURBL,DUDT,DVDT,EXNSFC,PSFC,PTOP,QFC1,QLOW,QOLD        &
     &       ,RATIOMX,RDTTURBL,RG,RWMSK,SEAMASK,THNEW,THOLD,TX         &
     &       ,ULOW,VLOW,WMSK

      REAL,DIMENSION(KTS:KTE) :: CWMK,PK,Q2K,QK,THEK,TK,UK,VK

      REAL,DIMENSION(KTS:KTE-1) :: AKHK,AKMK,EL,GH,GM

      REAL,DIMENSION(KTS:KTE+1) :: ZHK

      REAL,DIMENSION(ITS:ITE,JTS:JTE) :: THSK

      REAL,DIMENSION(KTS:KTE,ITS:ITE) :: RHOK

      REAL,DIMENSION(ITS:ITE,KTS:KTE,JTS:JTE) :: APE,THE

      REAL,DIMENSION(ITS:ITE,KTS:KTE-1,JTS:JTE) :: AKH,AKM

      REAL,DIMENSION(ITS:ITE,KTS:KTE+1,JTS:JTE) :: ZINT


      REAL :: ZSL_DIAG
      INTEGER :: IMD,JMD,PRINT_DIAG







      IMD=(IMS+IME)/2
      JMD=(JMS+JME)/2




     

      DTTURBL=DT*STEPBL
      RDTTURBL=1./DTTURBL
      DTDIF=DTTURBL
      RG=1./G

      DO J=JTS,JTE
      DO K=KTS,KTE-1
      DO I=ITS,ITE
        AKM(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO

      DO J=JTS,JTE
      DO K=KTS,KTE+1
      DO I=ITS,ITE
        ZINT(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO

      DO J=JTS,JTE
      DO I=ITS,ITE
        ZINT(I,KTE+1,J)=HT(I,J)     







      ENDDO
      ENDDO

      DO J=JTS,JTE
      DO K=KTE,KTS,-1
        KFLIP=KTE+1-K
        DO I=ITS,ITE
          ZINT(I,K,J)=ZINT(I,K+1,J)+DZ(I,KFLIP,J)
          APEX=1./EXNER(I,K,J)
          APE(I,K,J)=APEX
          TX=T(I,K,J)
          THE(I,K,J)=(CWM(I,K,J)*(-ELOCP/TX)+1.)*TH(I,K,J)
        ENDDO
      ENDDO
      ENDDO
      EL_MYJ(its:ite,:,jts:jte) = 0.


      setup_integration:  DO J=JTS,JTE


        DO I=ITS,ITE



          LMH=KTE-LOWLYR(I,J)+1

          PTOP=PINT(I,KTE+1,J)      
          PSFC=PINT(I,LOWLYR(I,J),J)



          SEAMASK=XLAND(I,J)-1.





          DO K=KTE,KTS,-1
            KFLIP=KTE+1-K
            TK(K)=T(I,KFLIP,J)
            THEK(K)=THE(I,KFLIP,J)
            RATIOMX=QV(I,KFLIP,J)
            QK(K)=RATIOMX/(1.+RATIOMX)
            CWMK(K)=CWM(I,KFLIP,J)
            PK(K)=PMID(I,KFLIP,J)
            UK(K)=U(I,KFLIP,J)
            VK(K)=V(I,KFLIP,J)

            DZ_KFLIP(K)=DZ(I,KFLIP,J)
 





            Q2K(K)=2.*TKE_MYJ(I,KFLIP,J)



            ZHK(K)=ZINT(I,K,J)
          ENDDO

          ZHK(KTE+1)=HT(I,J)          

          if(flag_bep)then
           DO K=KTE,KTS,-1
            KFLIP=KTE+1-K
            B_E(K)=B_E_BEP(I,KFLIP,J)
            A_E(K)=A_E_BEP(I,KFLIP,J)
            SFK(K)=SF_BEP(I,KFLIP,J)
            VLK(K)=VL_BEP(I,KFLIP,J)
            B_E_FACE(K)=0.
            DL_U(K)=DL_U_BEP(I,KFLIP,J)
           enddo
          else
           DO K=KTE,KTS,-1
            B_E(K)=0.
            A_E(K)=0.
            SFK(K)=1.
            VLK(K)=1.
            B_E_FACE(K)=0.
            DL_U(K)=0.
           enddo
          endif



         IF (FLAG_BEP) THEN
          DO K=KTE,KTS,-1
             IF (K==KTE) THEN
                B_E_FACE(K)=0.0
             ELSEIF ( K == KTS ) THEN
                B_E_FACE(K) = 0.0
             ELSE
                SLOPE=(B_E(K-1)-B_E(K))/0.5/(DZ_KFLIP(K-1)+DZ_KFLIP(K))
                ICEPT=B_E(K)-SLOPE*(ZHK(K)-0.5*DZ_KFLIP(K))
                B_E_FACE(K)=2.*(SLOPE*ZHK(K)+ICEPT)                         
             ENDIF
          ENDDO
         ENDIF














          CALL MIXLEN(LMH,UK,VK,TK,THEK,QK,CWMK                    &
     &               ,Q2K,ZHK,GM,GH,EL                                 &
     &               ,PBLH(I,J),LPBL(I,J),LMXL,CT(I,J)                 &

                     ,DL_U                                             &

     &               ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &               ,IMS,IME,JMS,JME,KMS,KME                          &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)







          CALL PRODQ2(LMH,DTTURBL,USTAR(I,J),GM,GH,EL,Q2K              &
     &               ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &               ,IMS,IME,JMS,JME,KMS,KME                          &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)


         IF (FLAG_BEP) THEN
            Q2K(LMH)=Q2K(LMH)*(1-FRC_URB2D(I,J))+(B_E_FACE(LMH)*       &
                 &         0.5*EL(LMH-1)/11.788/2.)**(2./3.)
         ENDIF






          KPBL(I,J)=KTE-LPBL(I,J)+1





          CALL DIFCOF(LMH,LMXL,GM,GH,EL,TK,Q2K,ZHK,AKMK,AKHK      &
     &               ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &               ,IMS,IME,JMS,JME,KMS,KME                          &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE,PRINT_DIAG)   






          DO K=KTS,KTE-1
            KFLIP=KTE-K
            AKH(I,K,J)=AKHK(K)
            AKM(I,K,J)=AKMK(K)
            DELTAZ=0.5*(ZHK(KFLIP)-ZHK(KFLIP+2))

            EXCH_H(I,K+1,J)=AKHK(KFLIP)*DELTAZ
            EXCH_M(I,K+1,J)=AKMK(KFLIP)*DELTAZ

          ENDDO







          CALL VDIFQ(LMH,DTDIF,Q2K,EL,ZHK                             &

     &              ,A_E,B_E_FACE,SFK,VLK                              &

     &              ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &              ,IMS,IME,JMS,JME,KMS,KME                           &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)



          DO K=KTS,KTE
            KFLIP=KTE+1-K
            Q2K(KFLIP)=AMAX1(Q2K(KFLIP),EPSQ2)
            TKE_MYJ(I,K,J)=0.5*Q2K(KFLIP)
            IF(KFLIP/=KTE)EL_MYJ(I,K,J)=EL(KFLIP)   
          ENDDO

        ENDDO


      ENDDO setup_integration


      IF(IDIFF.NE.1)then 




      DO J=JTS,JTE
      DO I=ITS,ITE
        PSFC=PINT(I,LOWLYR(I,J),J)
        THSK(I,J)=TSK(I,J)*(1.E5/PSFC)**CAPA
      ENDDO
      ENDDO




      main_integration:  DO J=JTS,JTE


        DO I=ITS,ITE





          DO K=KTE,KTS,-1
            KFLIP=KTE+1-K
            THEK(K)=THE(I,KFLIP,J)
            RATIOMX=QV(I,KFLIP,J)                            
            QK(K)=RATIOMX/(1.+RATIOMX)                       
            CWMK(K)=CWM(I,KFLIP,J)
            ZHK(K)=ZINT(I,K,J)
            RHOK(K,I)=PMID(I,KFLIP,J)/(R_D*T(I,KFLIP,J)*               &
     &                                (1.+P608*QK(K)-CWMK(K)))

          ENDDO

          if(flag_bep)then
           
           
            DO K=KTE,KTS,-1
             KFLIP=KTE+1-K
             A_Q(K)=A_Q_BEP(I,KFLIP,J)
             B_Q(K)=B_Q_BEP(I,KFLIP,J)
             A_T(K)=A_T_BEP(I,KFLIP,J)
             B_T(K)=B_T_BEP(I,KFLIP,J)
             SFK(K)=SF_BEP(I,KFLIP,J)
             VLK(K)=VL_BEP(I,KFLIP,J)             
            ENDDO

          else
           DO K=KTE,KTS,-1
             A_Q(K)=0.
             B_Q(K)=0.
             A_T(K)=0.
             B_T(K)=0.
             SFK(K)=1.
             VLK(K)=1.
            ENDDO
          endif           





          DO K=KTS,KTE-1
            AKHK(K)=AKH(I,K,J)*0.5*(RHOK(K,I)+RHOK(K+1,I))
          ENDDO

          ZHK(KTE+1)=ZINT(I,KTE+1,J)

          SEAMASK=XLAND(I,J)-1.
          THZ0(I,J)=(1.-SEAMASK)*THSK(I,J)+SEAMASK*THZ0(I,J)

          LLOW=LOWLYR(I,J)
          AKHS_DENS=AKHS(I,J)*RHOK(KTE+1-LLOW,I)

          IF(SEAMASK<0.5)THEN
            QFC1=XLV*CHKLOWQ(I,J)*AKHS_DENS

            IF(SNOW(I,J)>0..OR.SICE(I,J)>0.5)THEN
              QFC1=QFC1*RLIVWV
            ENDIF

            IF(QFC1>0.)THEN
              QLOW=QK(KTE+1-LLOW)
              QSFC(I,J)=QLOW+ELFLX(I,J)/QFC1
            ENDIF

          ELSE
            PSFC=PINT(I,LOWLYR(I,J),J)
            EXNSFC=(1.E5/PSFC)**CAPA
            QSFC(I,J)=PQ0SEA/PSFC                                      &
     &         *EXP(A2*(THSK(I,J)-A3*EXNSFC)/(THSK(I,J)-A4*EXNSFC))
          ENDIF

          QZ0 (I,J)=(1.-SEAMASK)*QSFC(I,J)+SEAMASK*QZ0 (I,J)



          LMH=KTE-LOWLYR(I,J)+1






         
          CALL VDIFH(DTDIF,LMH,THZ0(I,J),QZ0(I,J)                      &
     &              ,AKHS_DENS,CHKLOWQ(I,J),CT(I,J)                    &
     &              ,THEK,QK,CWMK,AKHK,ZHK,RHOK(KTS,I)                 &

     &              ,A_T,B_T,A_Q,B_Q,SFK,VLK                           &

     &              ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &              ,IMS,IME,JMS,JME,KMS,KME                           &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE,I,J)




          DO K=KTS,KTE
            KFLIP=KTE+1-K
            THOLD=TH(I,K,J)
            THNEW=THEK(KFLIP)+CWMK(KFLIP)*ELOCP*APE(I,K,J)
            DTDT=(THNEW-THOLD)*RDTTURBL
            QOLD=QV(I,K,J)/(1.+QV(I,K,J))
            DQDT=(QK(KFLIP)-QOLD)*RDTTURBL
            DCDT=(CWMK(KFLIP)-CWM(I,K,J))*RDTTURBL

            RTHBLTEN(I,K,J)=DTDT
            RQVBLTEN(I,K,J)=DQDT/(1.-QK(KFLIP))**2
            RQCBLTEN(I,K,J)=DCDT
          ENDDO

          









        PSFC=.01*PINT(I,LOWLYR(I,J),J)
        ZSL_DIAG=0.5*DZ(I,1,J)















































        ENDDO

        DO I=ITS,ITE






          DO K=KTS,KTE-1
            AKMK(K)=AKM(I,K,J)
            AKMK(K)=AKMK(K)*(RHOK(K,I)+RHOK(K+1,I))*0.5
          ENDDO

          LLOW=LOWLYR(I,J)
          AKMS_DENS=AKMS(I,J)*RHOK(KTE+1-LLOW,I)


          DO K=KTE,KTS,-1
            KFLIP=KTE+1-K
            UK(K)=U(I,KFLIP,J)
            VK(K)=V(I,KFLIP,J)
            ZHK(K)=ZINT(I,K,J)

          ENDDO            

          if(flag_bep)then

           DO K=KTE,KTS,-1
            KFLIP=KTE+1-K            
            A_U(K)=A_U_BEP(I,KFLIP,J)
            A_V(K)=A_V_BEP(I,KFLIP,J)
            B_U(K)=B_U_BEP(I,KFLIP,J)
            B_V(K)=B_V_BEP(I,KFLIP,J)
            SFK(K)=SF_BEP(I,KFLIP,J)
            VLK(K)=VL_BEP(I,KFLIP,J)
           ENDDO

         else
           DO K=KTE,KTS,-1            
            A_U(K)=0.
            A_V(K)=0.
            B_U(K)=0.
            B_V(K)=0.
            SFK(K)=1.
            VLK(K)=1.
           ENDDO
          endif

          ZHK(KTE+1)=ZINT(I,KTE+1,J)






          CALL VDIFV(LMH,DTDIF,UZ0(I,J),VZ0(I,J)                   &
     &              ,AKMS_DENS,UK,VK,AKMK,ZHK,RHOK(KTS,I)              &

     &              ,A_U,A_V,B_U,B_V,SFK,VLK                           &

     &              ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &              ,IMS,IME,JMS,JME,KMS,KME                           &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE,I,J)





          DO K=KTS,KTE
            KFLIP=KTE+1-K
            DUDT=(UK(KFLIP)-U(I,K,J))*RDTTURBL
            DVDT=(VK(KFLIP)-V(I,K,J))*RDTTURBL
            RUBLTEN(I,K,J)=DUDT
            RVBLTEN(I,K,J)=DVDT
          ENDDO

        ENDDO


      ENDDO main_integration

      ENDIF


      END SUBROUTINE MYJURB




                          SUBROUTINE MIXLEN                           &







     &(LMH,U,V,T,THE,Q,CWM,Q2,Z,GM,GH,EL,PBLH,LPBL,LMXL,CT             &

     &,DL_U                                                            &

     &,IDS,IDE,JDS,JDE,KDS,KDE                                         &
     &,IMS,IME,JMS,JME,KMS,KME                                         &
     &,ITS,ITE,JTS,JTE,KTS,KTE)


      IMPLICIT NONE


      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE

      INTEGER,INTENT(IN) :: LMH

      INTEGER,INTENT(OUT) :: LMXL,LPBL

      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: CWM,Q,Q2,T,THE,U,V

      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: DL_U


      REAL,DIMENSION(KTS:KTE+1),INTENT(IN) :: Z

      REAL,INTENT(OUT) :: PBLH

      REAL,DIMENSION(KTS:KTE-1),INTENT(OUT) :: EL,GH,GM

      REAL,INTENT(INOUT) :: CT




      INTEGER :: K,LPBLM

      REAL :: A,ADEN,B,BDEN,AUBR,BUBR,BLMX,EL0,ELOQ2X,GHL,GML           &
     &       ,QOL2ST,QOL2UN,QDZL,RDZ,SQ,SREL,SZQ,TEM,THM,VKRMZ

      REAL,DIMENSION(KTS:KTE) :: Q1

      REAL,DIMENSION(KTS:KTE-1) :: DTH,ELM,REL




      LPBL=LMH

      DO K=LMH-1,1,-1
        IF(Q2(K)<=EPSQ2*FH)THEN
          LPBL=K
          GO TO 110
        ENDIF
      ENDDO

      LPBL=1



 110  PBLH=Z(LPBL+1)-Z(LMH+1)


      DO K=KTS,LMH
        Q1(K)=0.
      ENDDO

      DO K=1,LMH-1
        DTH(K)=THE(K)-THE(K+1)
      ENDDO

      DO K=LMH-2,1,-1
        IF(DTH(K)>0..AND.DTH(K+1)<=0.)THEN
          DTH(K)=DTH(K)+CT
          EXIT
        ENDIF
      ENDDO

      CT=0.

      DO K=KTS,LMH-1
        RDZ=2./(Z(K)-Z(K+2))
        GML=((U(K)-U(K+1))**2+(V(K)-V(K+1))**2)*RDZ*RDZ
        GM(K)=MAX(GML,EPSGM)

        TEM=(T(K)+T(K+1))*0.5
        THM=(THE(K)+THE(K+1))*0.5

        A=THM*P608
        B=(ELOCP/TEM-1.-P608)*THM

        GHL=(DTH(K)*((Q(K)+Q(K+1)+CWM(K)+CWM(K+1))*(0.5*P608)+1.)      &
     &     +(Q(K)-Q(K+1)+CWM(K)-CWM(K+1))*A                            &
     &     +(CWM(K)-CWM(K+1))*B)*RDZ

        IF(ABS(GHL)<=EPSGH)GHL=EPSGH
        GH(K)=GHL
      ENDDO





      LMXL=LMH

      DO K=KTS,LMH-1
        GML=GM(K)
        GHL=GH(K)

        IF(GHL>=EPSGH)THEN
          IF(GML/GHL<=REQU)THEN
            ELM(K)=EPSL
            LMXL=K
          ELSE
            AUBR=(AUBM*GML+AUBH*GHL)*GHL
            BUBR= BUBM*GML+BUBH*GHL
            QOL2ST=(-0.5*BUBR+SQRT(BUBR*BUBR*0.25-AUBR*CUBR))*RCUBR
            ELOQ2X=1./QOL2ST
            ELM(K)=MAX(SQRT(ELOQ2X*Q2(K)),EPSL)
          ENDIF
        ELSE
          ADEN=(ADNM*GML+ADNH*GHL)*GHL
          BDEN= BDNM*GML+BDNH*GHL
          QOL2UN=-0.5*BDEN+SQRT(BDEN*BDEN*0.25-ADEN)
          ELOQ2X=1./(QOL2UN+EPSRU)       
          ELM(K)=MAX(SQRT(ELOQ2X*Q2(K)),EPSL)
        ENDIF
      ENDDO

      IF(ELM(LMH-1)==EPSL)LMXL=LMH





      BLMX=Z(LMXL)-Z(LMH+1)


      DO K=LPBL,LMH
        Q1(K)=SQRT(Q2(K))
      ENDDO

      SZQ=0.
      SQ =0.

      DO K=KTS,LMH-1
        QDZL=(Q1(K)+Q1(K+1))*(Z(K+1)-Z(K+2))
        SZQ=(Z(K+1)+Z(K+2)-Z(LMH+1)-Z(LMH+1))*QDZL+SZQ
        SQ=QDZL+SQ
      ENDDO





      EL0=MIN(ALPH*SZQ*0.5/SQ,EL0MAX)
      EL0=MAX(EL0            ,EL0MIN)





      LPBLM=MAX(LPBL-1,1)

      DO K=KTS,LPBLM
        EL(K)=MIN((Z(K)-Z(K+2))*ELFC,ELM(K))
        REL(K)=EL(K)/ELM(K)
      ENDDO





      IF(LPBL<LMH)THEN
        DO K=LPBL,LMH-1
          VKRMZ=(Z(K+1)-Z(LMH+1))*VKARMAN

          IF (DL_U(K).GT.0.0) THEN
               EL(K)=MIN(VKRMZ/(VKRMZ/EL0+1.),ELM(K))
               EL(K)=1./(1./EL(K)+1./DL_U(K))
            ELSE
               EL(K)=MIN(VKRMZ/(VKRMZ/EL0+1.),ELM(K))       
          ENDIF

          REL(K)=EL(K)/ELM(K)
        ENDDO
      ENDIF

      DO K=LPBL+1,LMH-2
        SREL=MIN(((REL(K-1)+REL(K+1))*0.5+REL(K))*0.5,REL(K))
        EL(K)=MAX(SREL*ELM(K),EPSL)
      ENDDO


      END SUBROUTINE MIXLEN



                          SUBROUTINE PRODQ2                            &







     &(LMH,DTTURBL,USTAR,GM,GH,EL,Q2                                   &
     &,IDS,IDE,JDS,JDE,KDS,KDE                                         &
     &,IMS,IME,JMS,JME,KMS,KME                                         &
     &,ITS,ITE,JTS,JTE,KTS,KTE)


      IMPLICIT NONE


      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE

      INTEGER,INTENT(IN) :: LMH

      REAL,INTENT(IN) :: DTTURBL,USTAR

      REAL,DIMENSION(KTS:KTE-1),INTENT(IN) :: GH,GM
      REAL,DIMENSION(KTS:KTE-1),INTENT(INOUT) :: EL

      REAL,DIMENSION(KTS:KTE),INTENT(INOUT) :: Q2




      INTEGER :: K

      REAL :: ADEN,AEQU,ANUM,ARHS,BDEN,BEQU,BNUM,BRHS,CDEN,CRHS        &
     &       ,DLOQ1,ELOQ11,ELOQ12,ELOQ13,ELOQ21,ELOQ22,ELOQ31,ELOQ32   &
     &       ,ELOQ41,ELOQ42,ELOQ51,ELOQ52,ELOQN,EQOL2,GHL,GML          &
     &       ,RDEN1,RDEN2,RHS2,RHSP1,RHSP2,RHST2





      main_integration: DO K=1,LMH-1
        GML=GM(K)
        GHL=GH(K)





        AEQU=(AEQM*GML+AEQH*GHL)*GHL
        BEQU= BEQM*GML+BEQH*GHL





        EQOL2=-0.5*BEQU+SQRT(BEQU*BEQU*0.25-AEQU)





        IF((GML+GHL*GHL<=EPSTRB)                                       &
     &   .OR.(GHL>=EPSGH.AND.GML/GHL<=REQU)                            &
     &   .OR.(EQOL2<=EPS2))THEN





          Q2(K)=EPSQ2
          EL(K)=EPSL


        ELSE








          ANUM=(ANMM*GML+ANMH*GHL)*GHL
          BNUM= BNMM*GML+BNMH*GHL





          ADEN=(ADNM*GML+ADNH*GHL)*GHL
          BDEN= BDNM*GML+BDNH*GHL
          CDEN= 1.





          ARHS=-(ANUM*BDEN-BNUM*ADEN)*2.
          BRHS=- ANUM*4.
          CRHS=- BNUM*2.





          DLOQ1=EL(K)/SQRT(Q2(K))





          ELOQ21=1./EQOL2
          ELOQ11=SQRT(ELOQ21)
          ELOQ31=ELOQ21*ELOQ11
          ELOQ41=ELOQ21*ELOQ21
          ELOQ51=ELOQ21*ELOQ31





          RDEN1=1./(ADEN*ELOQ41+BDEN*ELOQ21+CDEN)





          RHSP1=(ARHS*ELOQ51+BRHS*ELOQ31+CRHS*ELOQ11)*RDEN1*RDEN1





          ELOQ12=ELOQ11+(DLOQ1-ELOQ11)*EXP(RHSP1*DTTURBL)
          ELOQ12=MAX(ELOQ12,EPS1)





          ELOQ22=ELOQ12*ELOQ12
          ELOQ32=ELOQ22*ELOQ12
          ELOQ42=ELOQ22*ELOQ22
          ELOQ52=ELOQ22*ELOQ32





          RDEN2=1./(ADEN*ELOQ42+BDEN*ELOQ22+CDEN)
          RHS2 =-(ANUM*ELOQ42+BNUM*ELOQ22)*RDEN2+RB1
          RHSP2= (ARHS*ELOQ52+BRHS*ELOQ32+CRHS*ELOQ12)*RDEN2*RDEN2
          RHST2=RHS2/RHSP2





          ELOQ13=ELOQ12-RHST2+(RHST2+DLOQ1-ELOQ12)*EXP(RHSP2*DTTURBL)
          ELOQ13=AMAX1(ELOQ13,EPS1)





          ELOQN=ELOQ13

          IF(ELOQN>EPS1)THEN
            Q2(K)=EL(K)*EL(K)/(ELOQN*ELOQN)
            Q2(K)=AMAX1(Q2(K),EPSQ2)
            IF(Q2(K)==EPSQ2)THEN
              EL(K)=EPSL
            ENDIF
          ELSE
            Q2(K)=EPSQ2
            EL(K)=EPSL
          ENDIF





        ENDIF




      ENDDO main_integration





      Q2(LMH)=AMAX1(B1**(2./3.)*USTAR*USTAR,EPSQ2)


      END SUBROUTINE PRODQ2




                           SUBROUTINE DIFCOF                           &





     &(LMH,LMXL,GM,GH,EL,T,Q2,Z,AKM,AKH                                &
     &,IDS,IDE,JDS,JDE,KDS,KDE                                         &
     &,IMS,IME,JMS,JME,KMS,KME                                         &
     &,ITS,ITE,JTS,JTE,KTS,KTE,PRINT_DIAG)   


      IMPLICIT NONE


      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE

      INTEGER,INTENT(IN) :: LMH,LMXL

      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: Q2,T
      REAL,DIMENSION(KTS:KTE-1),INTENT(IN) :: EL,GH,GM
      REAL,DIMENSION(KTS:KTE+1),INTENT(IN) :: Z

      REAL,DIMENSION(KTS:KTE-1),INTENT(OUT) :: AKH,AKM




      INTEGER :: K,KINV

      REAL :: ADEN,AKMIN,BDEN,BESH,BESM,CDEN,D2T,ELL,ELOQ2,ELOQ4,ELQDZ &
     &       ,ESH,ESM,GHL,GML,Q1L,RDEN,RDZ


      INTEGER,INTENT(IN) :: PRINT_DIAG







      DO K=1,LMH-1
        ELL=EL(K)

        ELOQ2=ELL*ELL/Q2(K)
        ELOQ4=ELOQ2*ELOQ2

        GML=GM(K)
        GHL=GH(K)





        ADEN=(ADNM*GML+ADNH*GHL)*GHL
        BDEN= BDNM*GML+BDNH*GHL
        CDEN= 1.





        BESM=BSMH*GHL





        BESH=BSHM*GML+BSHH*GHL





        RDEN=1./(ADEN*ELOQ4+BDEN*ELOQ2+CDEN)





        ESM=(BESM*ELOQ2+CESM)*RDEN
        ESH=(BESH*ELOQ2+CESH)*RDEN





        RDZ=2./(Z(K)-Z(K+2))
        Q1L=SQRT(Q2(K))
        ELQDZ=ELL*Q1L*RDZ
        AKM(K)=ELQDZ*ESM
        AKH(K)=ELQDZ*ESH

      ENDDO
























































      END SUBROUTINE DIFCOF




                           SUBROUTINE VDIFQ                            &





     &(LMH,DTDIF,Q2,EL,Z                                               &

     &,A_E,B_E,SFK,VLK                                                 &

     &,IDS,IDE,JDS,JDE,KDS,KDE                                         &
     &,IMS,IME,JMS,JME,KMS,KME                                         &
     &,ITS,ITE,JTS,JTE,KTS,KTE)


      IMPLICIT NONE


      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE

      INTEGER,INTENT(IN) :: LMH

      REAL,INTENT(IN) :: DTDIF

      REAL,DIMENSION(KTS:KTE-1),INTENT(IN) :: EL
      REAL,DIMENSION(KTS:KTE+1),INTENT(IN) :: Z

      REAL,DIMENSION(KTS:KTE),INTENT(INOUT) :: Q2

      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: A_E,B_E,VLK
      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: SFK





      INTEGER :: K

      REAL :: ADEN,AKQS,BDEN,BESH,BESM,CDEN,CF,DTOZS,ELL,ELOQ2,ELOQ4   &
     &       ,ELQDZ,ESH,ESM,ESQHF,GHL,GML,Q1L,RDEN,RDZ

      REAL,DIMENSION(KTS:KTE-2) :: AKQ,CM,CR,DTOZ,RSQ2







      ESQHF=0.5*ESQ

      DO K=KTS,LMH-2
        DTOZ(K)=(DTDIF+DTDIF)/(Z(K)-Z(K+2))/VLK(K)                      
        AKQ(K)=SFK(K)*SQRT((Q2(K)+Q2(K+1))*0.5)*(EL(K)+EL(K+1))*ESQHF &
     &        /(Z(K+1)-Z(K+2))                                          
        CR(K)=-DTOZ(K)*AKQ(K)
      ENDDO

      CM(1)=DTOZ(1)*AKQ(1)+1.
      RSQ2(1)=Q2(1)+DTDIF*B_E(1)                                     

      DO K=KTS+1,LMH-2
        CF=-DTOZ(K)*AKQ(K-1)/CM(K-1)
        CM(K)=-CR(K-1)*CF+(AKQ(K-1)+AKQ(K))*DTOZ(K)+1.
        CM(K)=-CR(K-1)*CF+(AKQ(K-1)+AKQ(K))*DTOZ(K)+1.
        RSQ2(K)=-RSQ2(K-1)*CF+Q2(K)+DTDIF*B_E(K)                     
      ENDDO

      DTOZS=(DTDIF+DTDIF)/(Z(LMH-1)-Z(LMH+1))/VLK(K)
      AKQS=SFK(K)*SQRT((Q2(LMH-1)+Q2(LMH))*0.5)*(EL(LMH-1)+ELZ0)*ESQHF& 
     &    /(Z(LMH)-Z(LMH+1))

      CF=-DTOZS*AKQ(LMH-2)/CM(LMH-2)

      Q2(LMH-1)=(DTOZS*AKQS*Q2(LMH)-RSQ2(LMH-2)*CF+Q2(LMH-1)&
     &              +DTDIF*B_E(LMH-1))          &                    
     &        /((AKQ(LMH-2)+AKQS)*DTOZS-CR(LMH-2)*CF+1.)

      DO K=LMH-2,KTS,-1
        Q2(K)=(-CR(K)*Q2(K+1)+RSQ2(K))/CM(K)
      ENDDO


      END SUBROUTINE VDIFQ




      SUBROUTINE VDIFH(DTDIF,LMH,THZ0,QZ0,RKHS,CHKLOWQ,CT             &
     &                ,THE,Q,CWM,RKH_IN,Z,RHO                         &

     &                ,A_T,B_T,A_Q,B_Q,SFK,VLK                        &

     &                ,IDS,IDE,JDS,JDE,KDS,KDE                        &
     &                ,IMS,IME,JMS,JME,KMS,KME                        &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE,I,J)







      IMPLICIT NONE


      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE,I,J

      INTEGER,INTENT(IN) :: LMH

      REAL,INTENT(IN) :: CHKLOWQ,CT,DTDIF,QZ0,RKHS,THZ0

      REAL,DIMENSION(KTS:KTE-1),INTENT(IN) :: RKH_IN
      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: RHO
      REAL,DIMENSION(KTS:KTE+1),INTENT(IN) :: Z
      REAL,DIMENSION(KTS:KTE),INTENT(INOUT) :: CWM,Q,THE

      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: A_T,B_T,A_Q,B_Q,VLK
      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: SFK






      INTEGER :: K

      REAL :: CF,CMB,CMCB,CMQB,CMTB,CTHF,DTOZL,DTOZS                   &
     &       ,RCML,RKHH,RKQS,RSCB,RSQB,RSTB

      REAL :: CF_T,CF_Q,RCML_T,RCML_Q,CMB_T,CMB_Q


      REAL,DIMENSION(KTS:KTE-1) :: CM,CR,DTOZ,RKCT,RSC,RSQ,RST

      REAL,DIMENSION(KTS:KTE-1) :: CM_T,CM_Q
      REAL,DIMENSION(KTS:KTE-1) :: RKH





      CTHF=0.5*CT

      DO K=KTS,LMH-1
        RKH(K)=RKH_IN(K)*SFK(K)                                    
        DTOZ(K)=DTDIF/(Z(K)-Z(K+1))/VLK(K)                         
        CR(K)=-DTOZ(K)*RKH(K)
        RKCT(K)=RKH(K)*(Z(K)-Z(K+2))*CTHF
      ENDDO

      CM(KTS)=DTOZ(KTS)*RKH(KTS)+RHO(KTS)
      CM_T(KTS)=DTOZ(KTS)*RKH(KTS)+RHO(KTS)*(1.-DTDIF*A_T(KTS))   
      CM_Q(KTS)=DTOZ(KTS)*RKH(KTS)+RHO(KTS)*(1.-DTDIF*A_Q(KTS))   

      RST(KTS) = -RKCT(KTS)*DTOZ(KTS)+&
                &RHO(KTS)*(THE(KTS)+DTDIF*B_T(KTS))                 
      RSQ(KTS) = RHO(KTS)*(  Q(KTS)+DTDIF*B_Q(KTS))                 
      RSC(KTS) = RHO(KTS)* CWM(KTS)

      DO K=KTS+1,LMH-1
        DTOZL=DTOZ(K)
        CF   = -DTOZL*RKH(K-1)/CM(K-1)
        CF_T = -DTOZL*RKH(K-1)/CM_T(K-1)                             
        CF_Q = -DTOZL*RKH(K-1)/CM_Q(K-1)                             
        CM(K)  = -CR(K-1)*CF+  (RKH(K-1)+RKH(K))*DTOZL+RHO(K)
        CM_T(K)= -CR(K-1)*CF_T+(RKH(K-1)+RKH(K))*DTOZL+RHO(K)*&
                   &(1.-DTDIF*A_T(K))
        CM_Q(K)= -CR(K-1)*CF_Q+(RKH(K-1)+RKH(K))*DTOZL+RHO(K)*&
                   &(1.-DTDIF*A_Q(K))                                 
        RST(K)=-RST(K-1)*CF_T+(RKCT(K-1)-RKCT(K))*DTOZL+RHO(K)*&
                    &(THE(K)+DTDIF*B_T(K))                             
        RSQ(K)=-RSQ(K-1)*CF_Q+RHO(K)*(  Q(K)+DTDIF*B_Q(K))             
        RSC(K)=-RSC(K-1)*CF+CWM(K)*RHO(K)
      ENDDO

      DTOZS=DTDIF/(Z(LMH)-Z(LMH+1))/VLK(LMH)                   
      RKHH=RKH(LMH-1)                            

      CF=-DTOZS*RKHH/CM(LMH-1)
      CF_T=-DTOZS*RKHH/CM_T(LMH-1)                  
      CF_Q=-DTOZS*RKHH/CM_Q(LMH-1)                  
      RKQS=RKHS*CHKLOWQ

      CMB=CR(LMH-1)*CF
      CMB_T=CR(LMH-1)*CF_T
      CMB_Q=CR(LMH-1)*CF_Q
      CMTB=-CMB_T+RKHH*DTOZS+RHO(LMH)*(1.-DTDIF*A_T(LMH))  
      CMQB=-CMB_Q+RKHH*DTOZS+RHO(LMH)*(1.-DTDIF*A_Q(LMH))  
      CMCB=-CMB+(RKHH     )*DTOZS+RHO(LMH)

      RSTB=-RST(LMH-1)*CF_T+RHO(LMH)*(THE(LMH)+DTDIF*B_T(LMH))+&
             &RKCT(LMH-1)*DTOZS                                     
      RSQB=-RSQ(LMH-1)*CF_Q+RHO(LMH)*(  Q(LMH)+DTDIF*B_Q(LMH))      
      RSCB=-RSC(LMH-1)*CF+CWM(LMH)*RHO(LMH)

      THE(LMH)= RSTB/CMTB
      Q(LMH)  = RSQB/CMQB
      CWM(LMH)= RSCB/CMCB

      DO K=LMH-1,KTS,-1
        RCML=1./CM(K)
        RCML_T=1./CM_T(K)                                       
        RCML_Q=1./CM_Q(K)                                       
        THE(K)=(-CR(K)*THE(K+1)+RST(K))*RCML_T
        Q(K)  =(-CR(K)*  Q(K+1)+RSQ(K))*RCML_Q
        CWM(K)=(-CR(K)*CWM(K+1)+RSC(K))*RCML
      ENDDO



   
      END SUBROUTINE VDIFH




      SUBROUTINE VDIFX(DTDIF,LMH,RKHS,CT                              &
                      ,DUST1,DUST2,RKH,Z,RHO                          &
                      ,IDS,IDE,JDS,JDE,KDS,KDE                        &
                      ,IMS,IME,JMS,JME,KMS,KME                        &
                      ,ITS,ITE,JTS,JTE,KTS,KTE)







      IMPLICIT NONE


      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
                           ,IMS,IME,JMS,JME,KMS,KME                    &
                           ,ITS,ITE,JTS,JTE,KTS,KTE

      INTEGER,INTENT(IN) :: LMH

      REAL,INTENT(IN) :: CT,DTDIF,RKHS

      REAL,DIMENSION(KTS:KTE-1),INTENT(IN) :: RKH
      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: RHO
      REAL,DIMENSION(KTS:KTE+1),INTENT(IN) :: Z
      REAL,DIMENSION(KTS:KTE),INTENT(INOUT) :: DUST1,DUST2





      INTEGER :: K

      REAL :: CF,CMB,CMDB,CTHF,DTOZL,DTOZS                            &
             ,RCML,RKHH,RSD1B,RSD2B

      REAL,DIMENSION(KTS:KTE-1) :: CM,CR,DTOZ,RKCT,RSD1,RSD2




      CTHF=0.5*CT

      DO K=KTS,LMH-1
        DTOZ(K)=DTDIF/(Z(K)-Z(K+1))
        CR(K)=-DTOZ(K)*RKH(K)
        RKCT(K)=RKH(K)*(Z(K)-Z(K+2))*CTHF
      ENDDO

      CM(KTS)=DTOZ(KTS)*RKH(KTS)+RHO(KTS)

      RSD1(KTS)=DUST1(KTS)*RHO(KTS)
      RSD2(KTS)=DUST2(KTS)*RHO(KTS)

      DO K=KTS+1,LMH-1
        DTOZL=DTOZ(K)
        CF=-DTOZL*RKH(K-1)/CM(K-1)
        CM(K)=-CR(K-1)*CF+(RKH(K-1)+RKH(K))*DTOZL+RHO(K)
        RSD1(K)=-RSD1(K-1)*CF+DUST1(K)*RHO(K)
        RSD2(K)=-RSD2(K-1)*CF+DUST2(K)*RHO(K)
      ENDDO

      DTOZS=DTDIF/(Z(LMH)-Z(LMH+1))
      RKHH=RKH(LMH-1)

      CF=-DTOZS*RKHH/CM(LMH-1)

      CMB=CR(LMH-1)*CF
      CMDB=-CMB+RKHH*DTOZS+RHO(LMH)

      RSD1B=-RSD1(LMH-1)*CF+DUST1(LMH)*RHO(LMH)
      RSD2B=-RSD2(LMH-1)*CF+DUST2(LMH)*RHO(LMH)

      DUST1(LMH)=RSD1B/CMDB
      DUST2(LMH)=RSD2B/CMDB

      DO K=LMH-1,KTS,-1
        RCML=1./CM(K)
        DUST1(K)=(-CR(K)*DUST1(K+1)+RSD1(K))*RCML
        DUST2(K)=(-CR(K)*DUST2(K+1)+RSD2(K))*RCML
      ENDDO


      END SUBROUTINE VDIFX




      SUBROUTINE VDIFV(LMH,DTDIF,UZ0,VZ0,RKMS,U,V,RKM_IN,Z,RHO        &

     &                ,A_UK,A_VK,B_UK,B_VK,SFK,VLK                    &

     &                ,IDS,IDE,JDS,JDE,KDS,KDE                        &
     &                ,IMS,IME,JMS,JME,KMS,KME                        &
                      ,ITS,ITE,JTS,JTE,KTS,KTE,I,J)







      IMPLICIT NONE


      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                   &
     &                     ,IMS,IME,JMS,JME,KMS,KME                   &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE,I,J

      INTEGER,INTENT(IN) :: LMH

      REAL,INTENT(IN) :: RKMS,DTDIF,UZ0,VZ0

      REAL,DIMENSION(KTS:KTE-1),INTENT(IN) :: RKM_IN                    
      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: RHO
      REAL,DIMENSION(KTS:KTE+1),INTENT(IN) :: Z

      REAL,DIMENSION(KTS:KTE),INTENT(INOUT) :: U,V
      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: A_UK,A_VK,B_UK,B_VK,VLK     
      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: SFK                       




      INTEGER :: K

      REAL :: CFU,RCMLU,RCMVBU
      REAL :: CFV,RCMLV,RCMVBV
      REAL :: DTOZAK,DTOZL,DTOZS,RHOK,RKMH

      REAL,DIMENSION(KTS:KTE-1) :: CMU,CMV,CR,DTOZ,RSU,RSV
      REAL,DIMENSION(KTS:KTE-1) :: RKM                                  



      DO K=1,LMH-1
        RKM(K)=RKM_IN(K)*SFK(K)
        DTOZ(K)=DTDIF/(Z(K)-Z(K+1))/VLK(K)
        CR(K)=-DTOZ(K)*RKM(K)                             
      ENDDO

      RHOK=RHO(1)
      CMU(1)=DTOZ(1)*RKM(1)+RHOK*(1.-DTDIF*A_UK(1))        
      CMV(1)=DTOZ(1)*RKM(1)+RHOK*(1.-DTDIF*A_VK(1))        
      RSU(1)=RHOK*(U(1)+DTDIF*B_UK(1))                     
      RSV(1)=RHOK*(V(1)+DTDIF*B_VK(1))                     

      DO K=2,LMH-1
        DTOZL=DTOZ(K)
        CFU=-DTOZL*RKM(K-1)/CMU(K-1)
        CFV=-DTOZL*RKM(K-1)/CMV(K-1)
        RHOK=RHO(K)
        CMU(K)=-CR(K-1)*CFU+(RKM(K-1)+RKM(K))&
                 &*DTOZL+RHOK*(1.-DTDIF*A_UK(K))                      
        CMV(K)=-CR(K-1)*CFV+(RKM(K-1)+RKM(K))&
                 &*DTOZL+RHOK*(1.-DTDIF*A_VK(K))                      
        RSU(K)=-RSU(K-1)*CFU+RHOK*(U(K)+DTDIF*B_UK(K))                
        RSV(K)=-RSV(K-1)*CFV+RHOK*(V(K)+DTDIF*B_VK(K))                
      ENDDO


      DTOZS=DTDIF/(Z(LMH)-Z(LMH+1))/VLK(LMH)           
      RKMH=RKM(LMH-1)                       

      CFU=-DTOZS*RKMH/CMU(LMH-1)
      CFV=-DTOZS*RKMH/CMV(LMH-1)
      RHOK=RHO(LMH)
      RCMVBU=1./(RKMH*DTOZS-CR(LMH-1)*CFU+RHOK*(1.-DTDIF*A_UK(LMH)))   
      RCMVBV=1./(RKMH*DTOZS-CR(LMH-1)*CFV+RHOK*(1.-DTDIF*A_VK(LMH)))   
     

       U(LMH)=(-RSU(LMH-1)*CFU+RHOK*(U(LMH)+DTDIF*B_UK(LMH)))*RCMVBU  
       V(LMH)=(-RSV(LMH-1)*CFV+RHOK*(V(LMH)+DTDIF*B_VK(LMH)))*RCMVBV  

      DO K=LMH-1,1,-1
        RCMLU=1./CMU(K)                                  
        RCMLV=1./CMV(K)                                  
        U(K)=(-CR(K)*U(K+1)+RSU(K))*RCMLU                
        V(K)=(-CR(K)*V(K+1)+RSV(K))*RCMLV                
      ENDDO


      
      END SUBROUTINE VDIFV




      SUBROUTINE MYJURBINIT(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,          &
     &                      TKE_MYJ,EXCH_H,RESTART,ALLOWED_TO_READ,     &
     &                      IDS,IDE,JDS,JDE,KDS,KDE,                    &
     &                      IMS,IME,JMS,JME,KMS,KME,                    &
     &                      ITS,ITE,JTS,JTE,KTS,KTE                 )

      IMPLICIT NONE

      LOGICAL,INTENT(IN) :: ALLOWED_TO_READ,RESTART
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,                    &
     &                      IMS,IME,JMS,JME,KMS,KME,                    &
     &                      ITS,ITE,JTS,JTE,KTS,KTE

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) ::    EXCH_H, &
     &                                                         RUBLTEN, &
     &                                                         RVBLTEN, &
     &                                                        RTHBLTEN, &
     &                                                        RQVBLTEN, &
     &                                                         TKE_MYJ
      INTEGER :: I,J,K,ITF,JTF,KTF



      JTF=MIN0(JTE,JDE-1)
      KTF=MIN0(KTE,KDE-1)
      ITF=MIN0(ITE,IDE-1)

      IF(.NOT.RESTART)THEN
        DO J=JTS,JTF
        DO K=KTS,KTF
        DO I=ITS,ITF
          TKE_MYJ(I,K,J)=EPSQ2
          RUBLTEN(I,K,J)=0.
          RVBLTEN(I,K,J)=0.
          RTHBLTEN(I,K,J)=0.
          RQVBLTEN(I,K,J)=0.
          EXCH_H(I,K,J)=0.
        ENDDO
        ENDDO
        ENDDO
      ENDIF

      END SUBROUTINE MYJURBINIT


      END MODULE MODULE_BL_MYJURB


