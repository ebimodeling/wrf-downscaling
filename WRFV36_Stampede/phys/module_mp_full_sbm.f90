










MODULE module_mp_full_sbm
USE module_mp_radar



      INTEGER,PRIVATE,PARAMETER :: REMSAT = 0

      INTEGER, PRIVATE,PARAMETER :: IBREAKUP=1
      LOGICAL, PRIVATE,PARAMETER :: CONSERV=.TRUE.

      LOGICAL, PRIVATE,PARAMETER :: ORIGINAL_MELT=.FALSE.
      LOGICAL, PRIVATE,PARAMETER :: JIWEN_FAN_MELT=.TRUE.


      INTEGER, PRIVATE,PARAMETER :: p_ff1i01=2, p_ff1i33=34,p_ff5i01=35,p_ff5i33=67,p_ff6i01=68,&
     & p_ff6i33=100,p_ff8i01=101,p_ff8i33=133,p_ff2i01=134,p_ff2i33=166,p_ff3i01=167,p_ff3i33=199,&
     & p_ff4i01=200,p_ff4i33=232,p_ff7i01=233,p_ff7i33=265























      REAL, PRIVATE,PARAMETER :: PI_MORR = 3.1415926535897932384626434
      REAL, PRIVATE,PARAMETER ::  R_MORR = 287.15
      REAL,PRIVATE,PARAMETER :: F_AMOUNT=0.001
      REAL,PRIVATE,PARAMETER :: DX_BOUND=7500
      REAL ACCN,BCCN
      REAL,PRIVATE,PARAMETER :: ACCN_MAR=1.0000E02, BCCN_MAR=0.900E00,ROCCN0=0.1000E01
      REAL,PRIVATE,PARAMETER :: ACCN_CON=2.00000E03, BCCN_CON=0.400E00,ROCCN03=0.1000E01
      REAL,PRIVATE,PARAMETER :: I3POINT=1
      INTEGER,PRIVATE,PARAMETER :: ICCN = 1
       DOUBLE PRECISION, PRIVATE, PARAMETER ::  SCAL=1.d0
       INTEGER, PRIVATE,PARAMETER :: ICEPROCS=1,BULKNUC=0 
       INTEGER, PRIVATE,PARAMETER :: ICETURB=0,LIQTURB=0



       INTEGER, PRIVATE,PARAMETER :: ICEMAX=3,NCD=33,NHYDR=5,NHYDRO=7  &
     &        ,ifreez_down1=0,ifreez_down2=1,ifreez_top=1              &
     &        ,K0_LL=8,KRMIN_LL=1,KRMAX_LL=19,L0_LL=6                  &
     &        , IEPS_400=1,IEPS_800=0,IEPS_1600=0                      &
     &        ,K0L_GL=16,K0G_GL=16                                     &
     &        ,KRMINL_GL=1,KRMAXL_GL=24                                &
     &        ,KRMING_GL=1,KRMAXG_GL=33                                &
     &        ,KRDROP=18,KRBREAK=17,KRICE=18                           &
     &        ,NKR=33,JMAX=33,NRG=2,JBREAK = 18 
       REAL dt_coll

       REAL, PRIVATE,PARAMETER ::C1_MEY=0.00033,C2_MEY=0.              &


     &        ,an0_freez=10.,COL=0.23105                                
       REAL, PRIVATE,PARAMETER :: p1=1000000.0,p2=750000.0,p3=500000.0                     


       INTEGER, PRIVATE :: NCOND
       INTEGER, PRIVATE,PARAMETER :: kr_icempl=9



       REAL, PRIVATE, PARAMETER :: ALCR = 2.25

       REAL, PRIVATE, PARAMETER :: ALCR_G = 3.0

       INTEGER,PRIVATE,PARAMETER :: icempl=1
       REAL, PRIVATE, PARAMETER :: COEFREFLL=1.E6*36.E6*COL/3.1453/3.1453 
       REAL, PRIVATE, PARAMETER :: COEFREFLI=1.E9*36.E3*COL/3.1453/3.1453/5.
       REAL, PRIVATE, PARAMETER :: COEFREF00=1.E9*36.E3*COL/3.1453/3.1453       
       REAL, PRIVATE,DIMENSION(NKR) ::COLREFLL,COLREFLI,COLREFLS,COLREFLG,COLREFLH





       REAL, PRIVATE, SAVE :: &

     &YWLI(NKR,NKR,ICEMAX) &

     &,YWIL(NKR,NKR,ICEMAX),YWII(NKR,NKR,ICEMAX,ICEMAX) &
     &,YWIS(NKR,NKR,ICEMAX),YWIG(NKR,NKR,ICEMAX) &
     &,YWIH(NKR,NKR,ICEMAX),YWSI(NKR,NKR,ICEMAX) &
     &,YWGI(NKR,NKR,ICEMAX),YWHI(NKR,NKR,ICEMAX)

      REAL,PRIVATE,DIMENSION(NKR,NKR),SAVE :: &
     & YWLL_1000MB,YWLL_750MB,YWLL_500MB,YWLL,YWLS,YWLG,YWLH &

     &,YWSL,YWSS,YWSG,YWSH &

     &,YWGL,YWGS,YWGG,YWGH &

     &,YWHL,YWHS,YWHG,YWHH
       REAL, PRIVATE, SAVE :: &
     &  XI(NKR,ICEMAX) &
     & ,RADXX(NKR,NHYDR-1),MASSXX(NKR,NHYDR-1),DENXX(NKR,NHYDR-1) &
     & ,RADXXO(NKR,NHYDRO),MASSXXO(NKR,NHYDRO),DENXXO(NKR,NHYDRO) &
     & ,RIEC(NKR,ICEMAX),COEFIN(NKR),SLIC(NKR,6),TLIC(NKR,2) &
     & ,RO2BL(NKR,ICEMAX)
       REAL, PRIVATE, SAVE :: VR1(NKR),VR2(NKR,ICEMAX),VR3(NKR) &
     & ,VR4(NKR),VR5(NKR),VRX(NKR),VRI(NKR)
      REAL,PRIVATE,DIMENSION(NKR),SAVE ::  &
     &  XL,RLEC,XX,XCCN,XS,RSEC &
     & ,XG,RGEC,XH,RHEC,RO1BL,RO3BL,RO4BL,RO5BL &
     & ,ROCCN,RCCN,DROPRADII
  
      REAL, PRIVATE,SAVE ::  FCCNR_MAR(NKR),FCCNR_CON(NKR)
      REAL, PRIVATE,SAVE ::  FCCNR_MIX(NKR)
      REAL, PRIVATE,SAVE ::  FCCNR(NKR)


        REAL, PRIVATE :: C2,C3,C4
      double precision,private,save ::  cwll(nkr,nkr)
      double precision,private,save::  &
     & xl_mg(0:nkr),xs_mg(0:nkr),xg_mg(0:nkr),xh_mg(0:nkr) &
     &,xi1_mg(0:nkr),xi2_mg(0:nkr),xi3_mg(0:nkr) &
     &,chucm(nkr,nkr),ima(nkr,nkr) &
     &,cwll_1000mb(nkr,nkr),cwll_750mb(nkr,nkr),cwll_500mb(nkr,nkr) &
     &,cwli_1(nkr,nkr),cwli_2(nkr,nkr),cwli_3(nkr,nkr) &
     &,cwls(nkr,nkr),cwlg(nkr,nkr),cwlh(nkr,nkr) &

     &,cwil_1(nkr,nkr),cwil_2(nkr,nkr),cwil_3(nkr,nkr) &

     &,cwii_1_1(nkr,nkr),cwii_1_2(nkr,nkr),cwii_1_3(nkr,nkr) &
     &,cwii_2_1(nkr,nkr),cwii_2_2(nkr,nkr),cwii_2_3(nkr,nkr) &
     &,cwii_3_1(nkr,nkr),cwii_3_2(nkr,nkr),cwii_3_3(nkr,nkr) &

     &,cwis_1(nkr,nkr),cwis_2(nkr,nkr),cwis_3(nkr,nkr) &
     &,cwig_1(nkr,nkr),cwig_2(nkr,nkr),cwig_3(nkr,nkr) &
     &,cwih_1(nkr,nkr),cwih_2(nkr,nkr),cwih_3(nkr,nkr) &

     &,cwsl(nkr,nkr) &
     &,cwsi_1(nkr,nkr),cwsi_2(nkr,nkr),cwsi_3(nkr,nkr)&
     &,cwss(nkr,nkr),cwsg(nkr,nkr),cwsh(nkr,nkr) &
     &,cwgl(nkr,nkr)&
     &,cwgi_1(nkr,nkr),cwgi_2(nkr,nkr),cwgi_3(nkr,nkr)&
     &,cwgs(nkr,nkr),cwgg(nkr,nkr),cwgh(nkr,nkr) &

     &,cwhl(nkr,nkr) &
     &,cwhi_1(nkr,nkr),cwhi_2(nkr,nkr),cwhi_3(nkr,nkr) &
     &,cwhs(nkr,nkr),cwhg(nkr,nkr),cwhh(nkr,nkr) &
     &,dlnr &
     &,CTURBLL(KRMAX_LL,KRMAX_LL)&
     &,CTURB_LL(K0_LL,K0_LL)&
     &,CTURBGL(KRMAXG_GL,KRMAXL_GL)&
     &,CTURB_GL(K0G_GL,K0L_GL)

      DOUBLE PRECISION,private, save :: &
     &   BRKWEIGHT(JBREAK),PKIJ(JBREAK,JBREAK,JBREAK), &
     &   QKJ(JBREAK,JBREAK),ECOALMASSM(NKR,NKR)

 





      CONTAINS



      SUBROUTINE SBM (w,u,v,th_old,                                &
     &                      chem_new,n_chem,                              &
     &                      itimestep,DT,DX,DY,                         &
     &                      dz8w,rho_phy,p_phy,pi_phy,th_phy,           &
     &                      xland,ivgtyp,xlat,xlong,                           &
     &                      QV,QC,QR,QIP,QIC,QID,QS,QG,QH,QV_OLD,                   &
     &                      QNC,QNR,QNIP,QNIC,QNID,QNS,QNG,QNH,QNA,EFFR,ICE_EFFR,TOT_EFFR,       &
     &                      QIC_EFFR,QIP_EFFR,QID_EFFR,       &
     &                      height,tempc,&


     &                      kext_ql,kext_qs,kext_qg,kext_qh,kext_qa,    &
     &                      kext_qic,kext_qip,kext_qid,                 &
     &                      kext_ft_qic,kext_ft_qip,kext_ft_qid,                 &
     &                      kext_ft_qs,kext_ft_qg,                 &
     &                      ids,ide, jds,jde, kds,kde,		        &
     &                      ims,ime, jms,jme, kms,kme,		        &
     &                      its,ite, jts,jte, kts,kte,                  &
     &                      refl_10cm, diagflag, do_radar_ref,      & 
     &                      RAINNC                             )

      IMPLICIT NONE

      INTEGER, PARAMETER :: ITLO=-60, ITHI=40
      INTEGER NKRO,NKRE
      INTEGER KR,IKL,ICE

      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,ITIMESTEP,N_CHEM

      REAL, INTENT(IN) 	    :: DT,DX,DY
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         , &
          INTENT(IN   ) ::                                   &
                                                          U, &
                                                          V, &
                                                          W   

  REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,n_chem),INTENT(INOUT)   :: chem_new
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(INOUT) ::                                          &
                                                              qv, &
                                                          qv_old, &
                                                          th_old, &
                                                              qc, &
                                                              qr, &
                                                              qip, &
                                                              qic, &
                                                              qid, &
                                                              qs, &
                                                              qg, &
                                                              qh, &
                                                              qnc, &
                                                              qnr, &
                                                              qns, &
                                                              qnip, &
                                                              qnic, &
                                                              qnid, &
                                                              qng, &
                                                              qnh, &
                                                              qna, &
                                                              kext_ql, &
                                                              kext_qs, &
                                                              kext_qg, &
                                                              kext_qh, &
                                                              kext_qa, &
                                                              kext_qic, &
                                                              kext_qip, &
                                                              kext_qid, &
                                                              kext_ft_qic, &
                                                              kext_ft_qip, &
                                                              kext_ft_qid, &
                                                              kext_ft_qs, &
                                                              kext_ft_qg, &
                                                              effr, &
                                                              ice_effr,&
                                                              tot_effr,&
                                                              qic_effr,&
                                                              qip_effr,&
                                                              qid_effr,&
                                                              height,  &
                                                              tempc    






      REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN)   :: XLAND
      LOGICAL, OPTIONAL, INTENT(IN) :: diagflag
      INTEGER, OPTIONAL, INTENT(IN) :: do_radar_ref

      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT)::       &  
                          refl_10cm

      INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(IN)::   IVGTYP
      REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )    :: XLAT, XLONG
      REAL, INTENT(IN),     DIMENSION(ims:ime, kms:kme, jms:jme)::      &
     &                      dz8w,p_phy,pi_phy,rho_phy
      REAL, INTENT(INOUT),  DIMENSION(ims:ime, kms:kme, jms:jme)::      &
     &                      th_phy
      REAL, INTENT(INOUT),  DIMENSION(ims:ime,jms:jme), OPTIONAL ::     &
     &                                                   RAINNC











      INTEGER,DIMENSION(ITLO:ITHI,4) :: NSTATS

      REAL,   DIMENSION(ITLO:ITHI,22):: QTOT









      REAL,  DIMENSION( ims:ime, kms:kme, jms:jme ) ::                  &
     &       TLATGS_PHY,TRAIN_PHY
      REAL,  DIMENSION(ims:ime,jms:jme):: APREC,PREC,ACPREC
      REAL,  DIMENSION(its-1:ite+1, kts:kte, jts-1:jte+1):: t_new,t_old,   &
     &                                      zcgs,       rhocgs,pcgs

      INTEGER :: I,J,K,KFLIP

      INTEGER :: KRFREEZ

       REAL Z0IN,ZMIN
       DATA  ZMIN/2.0E5/
       DATA  Z0IN/2.0E5 /


       REAL EPSF2D, &
     &        TAUR1,TAUR2,EPS_R1,EPS_R2,ANC1IN, &
     &        PEPL,PEPI,PERL,PERI,ANC1,ANC2,PARSP, &
     &        AFREEZMY,BFREEZMY,BFREEZMAX, &
     &        TCRIT,TTCOAL, &
     &        EPSF1,EPSF3,EPSF4, &
     &        SUP2_OLD, DSUPICEXZ,TFREEZ_OLD,DTFREEZXZ, &
     &        AA1_MY,BB1_MY,AA2_MY,BB2_MY, &
     &        DTIME,DTCOND, &
     &        A1_MYN, BB1_MYN, A2_MYN, BB2_MYN
        DATA A1_MYN, BB1_MYN, A2_MYN, BB2_MYN  &
     &      /2.53,5.42,3.41E1,6.13/
        DATA AA1_MY,BB1_MY,AA2_MY,BB2_MY/2.53E12,5.42E3,3.41E13,6.13E3/

        DATA KRFREEZ,BFREEZMAX,ANC1,ANC2,PARSP,PEPL,PEPI,PERL,PERI, &
     &  TAUR1,TAUR2,EPS_R1,EPS_R2,TTCOAL,AFREEZMY,&
     &  BFREEZMY,EPSF1,EPSF3,EPSF4,TCRIT/21,&
     &  0.6600E00, &
     &  1.0000E02,1.0000E02,0.9000E02, &
     &  0.6000E00,0.6000E00,1.0000E-03,1.0000E-03, &
     &  0.5000E00,0.8000E00,0.1500E09,0.1500E09, &
     &  2.3315E02,0.3333E-04,0.6600E00, &
     &  0.1000E-02,0.1000E-05,0.1000E-05, &
     &  2.7015E02/


      REAL,DIMENSION (nkr) :: FF1IN,FF3IN,FF4IN,FF5IN,&
     &              FF1R,FF3R,FF4R,FF5R,FCCN
      REAL,DIMENSION (nkr,icemax) :: FF2IN,FF2R

      DOUBLE PRECISION DEL1NR,DEL2NR,DEL12R,DEL12RD,ES1N,ES2N,EW1N,EW1PN
      DOUBLE PRECISION DELSUP1,DELSUP2,DELDIV1,DELDIV2
      DOUBLE PRECISION TT,QQ,TTA,QQA,PP,DPSA,DELTATEMP,DELTAQ
      DOUBLE PRECISION DIV1,DIV2,DIV3,DIV4,DEL1IN,DEL2IN,DEL1AD,DEL2AD
      REAL DEL_BB,DEL_BBN,DEL_BBR
      REAL FACTZ,CONCCCN_XZ,CONCDROP
       REAL SUPICE(KTE),AR1,AR2, &
     & DERIVT_X,DERIVT_Y,DERIVT_Z,DERIVS_X,DERIVS_Y,DERIVS_Z, &
     & ES2NPLSX,ES2NPLSY,EW1NPLSX,EW1NPLSY,UX,VX, &
     & DEL2INPLSX,DEL2INPLSY,DZZ(KTE)
       INTEGER KRR,I_START,I_END,J_START,J_END
   
       REAL DTFREEZ_XYZ(ITE,KTE,JTE),DSUPICE_XYZ(ITE,KTE,JTE)

       REAL DXHUCM,DYHUCM
       REAL FMAX1,FMAX2,FMAX3,FMAX4,FMAX5
       INTEGER ISYM1,ISYM2,ISYM3,ISYM4,ISYM5
       INTEGER DIFFU
       REAL DELTAW
       real zcgs_z(kts:kte),pcgs_z(kts:kte),rhocgs_z(kts:kte),ffx_z(kts:kte,nkr)
       real z_full







       REAL, PARAMETER :: RON=8.E6, GON=5.E7,PI=3.14159265359
       REAL EFF_N,EFF_D
       REAL EFF_NI(its:ite,kts:kte,jts:jte),eff_di(its:ite,kts:kte,jts:jte)
       REAL EFF_NQIC,eff_DQIC
       REAL EFF_NQIP,eff_DQIP
       REAL EFF_NQID,eff_DQID
       real lambda,chi0,xi1,xi2,xi3,xi4,xi5,r_e,chi_3,f1,f2,volume,surface_area,xi6,ft,chi_e
       real ft_bin
      REAL, DIMENSION(kts:kte)::                            &
                      qv1d, qr1d, nr1d, qs1d, ns1d, qg1d, ng1d, t1d, p1d
      REAL, DIMENSION(kts:kte):: dBZ
      

       real nzero,son,nzero_less
       parameter (son=2.E7)
       real raddumb(nkr),massdumb(nkr)
       real hydrosum

       integer imax,kmax,jmax
       real gmax
       real tmax,qmax,divmax,rainmax
       real qnmax,inmax,knmax
       real hydro
       real difmax,tdif,tt_old,w_stag,qq_old
       real teten,es
       integer  print_int
       parameter (print_int=300)
       real ft_liq(nkr)
       data ft_liq/ 6.254894e-01,6.615571e-01,6.922125e-01,7.514451e-01,7.391191e-01,7.592261e-01,7.417122e-01&
     &             ,7.388885e-01,7.430871e-01,7.570534e-01,7.584263e-01,7.735341e-01,7.721352e-01,7.724897e-01&
     &             ,7.744899e-01,7.745646e-01,7.768777e-01,7.776348e-01, 7.788586e-01,7.774171e-01,7.789876e-01 &
     &             ,7.801301e-01,7.806936e-01,7.801274e-01,7.821974e-01,7.815210e-01,7.822269e-01,7.822353e-01 &
     &             ,7.808765e-01,7.824246e-01,7.814153e-01,7.818192e-01, 7.818231e-01/



       real geo_cs
       integer t_print
       t_print=print_int/dt


       difmax = 0


       if (itimestep.eq.1)then
        if (iceprocs.eq.1) call wrf_message(" FULL SBM: ICE PROCESES ACTIVE ")
        if (iceprocs.eq.0) call wrf_message(" FULL SBM: LIQUID PROCESES ONLY")
       end if
       tmax = 0

       NCOND = 0











         NCOND=nint(dx/1000)


       NCOND=max(NCOND,1)
       DTCOND=DT/REAL(NCOND)
       dt_coll=dt
       call kernals(dt)













      DEL_BB=BB2_MY-BB1_MY
      DEL_BBN=BB2_MYN-BB1_MYN
      DEL_BBR=BB1_MYN/DEL_BBN

      if (conserv)then
      DO j = jts,jte
      DO i = its,ite
      DO k = kts,kte
      rhocgs(I,K,J)=rho_phy(I,K,J)*0.001
      KRR=0
      DO KR=p_ff1i01,p_ff1i33
        KRR=KRR+1
        chem_new(I,K,J,KR)=chem_new(I,K,J,KR)*RHOCGS(I,K,J)/COL/XL(KRR)/XL(KRR)/3.0
      END DO
      KRR=0
      DO KR=p_ff5i01,p_ff5i33
        KRR=KRR+1
        chem_new(I,K,J,KR)=chem_new(I,K,J,KR)*RHOCGS(I,K,J)/COL/XS(KRR)/XS(KRR)/3.0
      END DO
      KRR=0
      DO KR=p_ff6i01,p_ff6i33
        KRR=KRR+1
        chem_new(I,K,J,KR)=chem_new(I,K,J,KR)*RHOCGS(I,K,J)/COL/XG(KRR)/XG(KRR)/3.0
      END DO



      KRR=0
      DO KR=p_ff8i01,p_ff8i33
        KRR=KRR+1


        chem_new(I,K,J,KR)=chem_new(I,K,J,KR)*RHOCGS(I,K,J)/1000.   
      END DO

      KRR=0
      DO KR=p_ff2i01,p_ff2i33
        KRR=KRR+1
        chem_new(I,K,J,KR)=chem_new(I,K,J,KR)*RHOCGS(I,K,J)/COL/XI(KRR,1)/XI(KRR,1)/3.0

      END DO

      KRR=0
      DO KR=p_ff3i01,p_ff3i33
        KRR=KRR+1

        chem_new(I,K,J,KR)=chem_new(I,K,J,KR)*RHOCGS(I,K,J)/COL/XI(KRR,2)/XI(KRR,2)/3.0
      END DO

      KRR=0
      DO KR=p_ff4i01,p_ff4i33
        KRR=KRR+1
        chem_new(I,K,J,KR)=chem_new(I,K,J,KR)*RHOCGS(I,K,J)/COL/XI(KRR,3)/XI(KRR,3)/3.0
      END DO
      KRR=0
      DO KR=p_ff7i01,p_ff7i33
        KRR=KRR+1
        chem_new(I,K,J,KR)=chem_new(I,K,J,KR)*RHOCGS(I,K,J)/COL/XH(KRR)/XH(KRR)/3.0
      END DO
      END DO
      END DO
      END DO
      end if

      call kernals(dt)

      DXHUCM=100.*DX
      DYHUCM=100.*DY








      I_START=MAX(1,ITS-1)
      J_START=MAX(1,JTS-1)
      I_END=MIN(IDE-1,ITE+1)
      J_END=MIN(JDE-1,JTE+1)







      DO j = j_start,j_end
      DO i = i_start,i_end
      z_full=0.
      DO k = kts,kte
          pcgs(I,K,J)=P_PHY(I,K,J)*10.
          rhocgs(I,K,J)=rho_phy(I,K,J)*0.001
          zcgs(I,K,J)=z_full+0.5*dz8w(I,K,J)*100
          z_full=z_full+dz8w(i,k,j)*100.
      ENDDO
      ENDDO
      ENDDO

         if (itimestep.eq.1)then
       DO j = jts,jte
       DO i = its,ite
       DO k = kts,kte
         IF (zcgs(I,K,J).LE.ZMIN)THEN
            FACTZ=1.
         ELSE
            FACTZ=EXP(-(zcgs(I,K,J)-ZMIN)/Z0IN)
         END IF

         KRR=0
         DO KR=p_ff8i01,p_ff8i33
          KRR=KRR+1
          if (xland(i,j).eq.1)then
             chem_new(I,K,J,KR)=FCCNR_MAR(KRR)*FACTZ
          else
             chem_new(I,K,J,KR)=FCCNR_MIX(KRR)*FACTZ
          end if








         END DO
       end do
       end do
       end do
       end if
       if (itimestep.ne.1.and.dx.gt.dx_bound)then
       DO j = jts,jte
       DO k = kts,kte
       DO i = its,ite
        if (i.le.5.or.i.ge.IDE-5.OR. &
     &       j.le.5.or.j.ge.JDE-5)THEN
         IF (zcgs(I,K,J).LE.ZMIN)THEN
            FACTZ=1.
         ELSE
            FACTZ=EXP(-(zcgs(I,K,J)-ZMIN)/Z0IN)
         END IF
         KRR=0
         DO kr=p_ff8i01,p_ff8i33
          KRR=KRR+1
          if (xland(i,j).eq.1)then
             chem_new(I,K,J,KR)=FCCNR_MAR(KRR)*FACTZ
          else
             chem_new(I,K,J,KR)=FCCNR_MIX(KRR)*FACTZ
          end if







         End do
        end if
       end do
       end do
       end do
       end if
      if (itimestep.eq.1)then
      DO j = j_start,j_end
      DO k = kts,kte
      DO i = i_start,i_end
         th_old(i,k,j)=th_phy(i,k,j)
         qv_old(i,k,j)=qv(i,k,j)
      END DO
      END DO
      END DO
      end if
      DO j = j_start,j_end
      DO k = kts,kte
      DO i = i_start,i_end
        t_new(i,k,j) = th_phy(i,k,j)*pi_phy(i,k,j)
        tempc(i,k,j)=t_new(i,k,j)-273.16
        t_old(i,k,j) = th_old(i,k,j)*pi_phy(i,k,j)
      ENDDO
      ENDDO
      ENDDO







      DO j = jts,jte
      DO i = its,ite
      DO k = kts,kte
       IF(K.EQ.KTE)THEN
        DZZ(K)=(zcgs(I,K,J)-zcgs(I,K-1,J))
       ELSE IF(K.EQ.1)THEN
        DZZ(K)=(zcgs(I,K+1,J)-zcgs(I,K,J))
       ELSE
        DZZ(K)=(zcgs(I,K+1,J)-zcgs(I,K-1,J))
       END IF
       ES2N=AA2_MY*EXP(-BB2_MY/T_OLD(I,K,J))
       EW1N=QV_OLD(I,K,J)*pcgs(I,K,J)/(0.622+0.378*QV_OLD(I,K,J))
       SUPICE(K)=EW1N/ES2N-1.
       IF(SUPICE(K).GT.0.5) SUPICE(K)=.5
 
      END DO
      DO k = kts,kte
       IF(T_OLD(I,K,J).GE.238.15.AND.T_OLD(I,K,J).LT.274.15) THEN
       if (k.lt.kte)then       
        w_stag=50.*(w(i,k,j)+w(i,k+1,j)) 
       else
        w_stag=100*w(i,k,j)
       end if
             IF (I.LT.IDE-1.AND.J.LT.JDE-1)THEN
              UX=25.*(U(I,K,J)+U(I+1,K,J)+U(I,K,J+1)+U(I+1,K,J+1))
              VX=25.*(V(I,K,J)+V(I+1,K,J)+V(I,K,J+1)+V(I+1,K,J+1))
             ELSE
              UX=U(I,K,J)*100.
              VX=V(I,K,J)*100.
             END IF  
             IF(K.EQ.1) DERIVT_Z=(T_OLD(I,K+1,J)-T_OLD(I,K,J))/DZZ(K)
             IF(K.EQ.KTE) DERIVT_Z=(T_OLD(I,K,J)-T_OLD(I,K-1,J))/DZZ(K)
             IF(K.GT.1.AND.K.LT.KTE) DERIVT_Z= &
     &        (T_OLD(I,K+1,J)-T_OLD(I,K-1,J))/DZZ(K)
             IF (I.EQ.1)THEN
              DERIVT_X=(T_OLD(I+1,K,J)-T_OLD(I,K,J))/(DXHUCM)
             ELSE IF (I.EQ.IDE-1)THEN
              DERIVT_X=(T_OLD(I,K,J)-T_OLD(I-1,K,J))/(DXHUCM)
             ELSE
              DERIVT_X=(T_OLD(I+1,K,J)-T_OLD(I-1,K,J))/(2.*DXHUCM)
             END IF
             IF (J.EQ.1)THEN
              DERIVT_Y=(T_OLD(I,K,J+1)-T_OLD(I,K,J))/(DYHUCM)
             ELSE IF (J.EQ.JDE-1)THEN
              DERIVT_Y=(T_OLD(I,K,J)-T_OLD(I,K,J-1))/(DYHUCM)
             ELSE
              DERIVT_Y=(T_OLD(I,K,J+1)-T_OLD(I,K,J-1))/(2.*DYHUCM)
             END IF
             DTFREEZ_XYZ(I,K,J)=DT*(VX*DERIVT_Y+ &
     &            UX*DERIVT_X+w_stag*DERIVT_Z)
          ELSE
             DTFREEZ_XYZ(I,K,J)=0.
          ENDIF
          IF(SUPICE(K).GE.0.02.AND.T_OLD(I,K,J).LT.268.15) THEN
            IF (I.LT.IDE-1)THEN
             ES2NPLSX=AA2_MY*EXP(-BB2_MY/T_OLD(I+1,K,J))
             EW1NPLSX=QV_OLD(I+1,K,J)*pcgs(I+1,K,J)/ &
     &               (0.622+0.378*QV_OLD(I+1,K,J))
            ELSE
             ES2NPLSX=AA2_MY*EXP(-BB2_MY/T_OLD(I,K,J))
             EW1NPLSX=QV_OLD(I,K,J)*pcgs(I,K,J)/ &
     &               (0.622+0.378*QV_OLD(I,K,J))
            END IF
            IF (ES2NPLSX.EQ.0)THEN
             DEL2INPLSX=0.5
            ELSE
             DEL2INPLSX=EW1NPLSX/ES2NPLSX-1.
            END IF
            IF(DEL2INPLSX.GT.0.5) DEL2INPLSX=.5
            IF (I.GT.1)THEN
             ES2N=AA2_MY*EXP(-BB2_MY/T_OLD(I-1,K,J))
             EW1N=QV_OLD(I-1,K,J)*pcgs(I-1,K,J)/(0.622+0.378*QV_OLD(I-1,K,J))
            ELSE
             ES2N=AA2_MY*EXP(-BB2_MY/T_OLD(I,K,J))
             EW1N=QV_OLD(I,K,J)*pcgs(I,K,J)/(0.622+0.378*QV_OLD(I,K,J))
            END IF
            DEL2IN=EW1N/ES2N-1.
            IF(DEL2IN.GT.0.5) DEL2IN=.5
            IF (I.GT.1.AND.I.LT.IDE-1)THEN
             DERIVS_X=(DEL2INPLSX-DEL2IN)/(2.*DXHUCM)
            ELSE
             DERIVS_X=(DEL2INPLSX-DEL2IN)/(DXHUCM)
            END IF
            IF (J.LT.JDE-1)THEN
             ES2NPLSY=AA2_MY*EXP(-BB2_MY/T_OLD(I,K,J+1))
             EW1NPLSY=QV_OLD(I,K,J+1)*pcgs(I,K,J+1)/(0.622+0.378*QV_OLD(I,K,J+1))
            ELSE
             ES2NPLSY=AA2_MY*EXP(-BB2_MY/T_OLD(I,K,J))
             EW1NPLSY=QV_OLD(I,K,J)*pcgs(I,K,J)/(0.622+0.378*QV_OLD(I,K,J))
            END IF
            DEL2INPLSY=EW1NPLSY/ES2NPLSY-1.
            IF(DEL2INPLSY.GT.0.5) DEL2INPLSY=.5
            IF (J.GT.1)THEN
             ES2N=AA2_MY*EXP(-BB2_MY/T_OLD(I,K,J-1))
             EW1N=QV_OLD(I,K,J-1)*pcgs(I,K,J-1)/(0.622+0.378*QV_OLD(I,K,J-1))
            ELSE
             ES2N=AA2_MY*EXP(-BB2_MY/T_OLD(I,K,J))
             EW1N=QV_OLD(I,K,J)*pcgs(I,K,J)/(0.622+0.378*QV_OLD(I,K,J))
            END IF
             DEL2IN=EW1N/ES2N-1.
            IF(DEL2IN.GT.0.5) DEL2IN=.5
            IF (J.GT.1.AND.J.LT.JDE-1)THEN
             DERIVS_Y=(DEL2INPLSY-DEL2IN)/(2.*DYHUCM)
            ELSE
             DERIVS_Y=(DEL2INPLSY-DEL2IN)/(DYHUCM)
            END IF

            IF (K.EQ.1)DERIVS_Z=(SUPICE(K+1)-SUPICE(K))/DZZ(K)
            IF (K.EQ.KTE)DERIVS_Z=(SUPICE(K)-SUPICE(K-1))/DZZ(K)
            IF(K.GT.1.and.K.LT.KTE) DERIVS_Z=(SUPICE(K+1)-SUPICE(K-1))/DZZ(K)
            IF (I.LT.IDE-1.AND.J.LT.JDE-1)THEN
             UX=25.*(U(I,K,J)+U(I+1,K,J)+U(I,K,J+1)+U(I+1,K,J+1))
             VX=25.*(V(I,K,J)+V(I+1,K,J)+V(I,K,J+1)+V(I+1,K,J+1))
            ELSE
             UX=U(I,K,J)*100.
             VX=V(I,K,J)*100.
            END IF  
            DSUPICE_XYZ(I,K,J)=(UX*DERIVS_X+VX*DERIVS_Y+ &
      &                        w_stag*DERIVS_Z)*DTCOND
          ELSE
            DSUPICE_XYZ(I,K,J)=0.0
          END IF
         END DO
         END DO
         END DO
     

      do j = jts,jte
      do k = kts,kte
      do i = its,ite








          KRR=0
          DO kr=p_ff1i01,p_ff1i33
           KRR=KRR+1
           FF1R(KRR)=chem_new(I,K,J,KR)
           IF (FF1R(KRR).LT.0)FF1R(KRR)=0.
          END DO

        KRR=0
        DO kr=p_ff8i01,p_ff8i33
          KRR=KRR+1
          FCCN(KRR)=chem_new(I,K,J,KR)
          if (fccn(krr).lt.0)fccn(krr)=0.
        END DO
        IF (ICEPROCS.EQ.1)THEN

         KRR=0
         DO kr=p_ff2i01,p_ff2i33
          KRR=KRR+1
          FF2R(KRR,1)=chem_new(I,K,J,KR)
          if (ff2r(krr,1).lt.0)ff2r(krr,1)=0
         END DO

         KRR=0
         DO kr=p_ff3i01,p_ff3i33
          KRR=KRR+1
          FF2R(KRR,2)=chem_new(I,K,J,KR)

          if (ff2r(krr,2).lt.0)ff2r(krr,2)=0
          
         END DO


         KRR=0
         DO KR=p_ff4i01,p_ff4i33
          KRR=KRR+1
          FF2R(KRR,3)=chem_new(I,K,J,KR)
          if (ff2r(krr,3).lt.0)ff2r(krr,3)=0
         END DO

           KRR=0
           DO kr=p_ff5i01,p_ff5i33
            KRR=KRR+1
            FF3R(KRR)=chem_new(I,K,J,KR)
            if (ff3r(krr).lt.0)ff3r(krr)=0.
           END DO


           KRR=0
           DO kr=p_ff6i01,p_ff6i33
            KRR=KRR+1
            FF4R(KRR)=chem_new(I,K,J,KR)
            IF (FF4R(KRR).LT.0)FF4R(KRR)=0.
           END DO


         KRR=0
         DO kr=p_ff7i01,p_ff7i33
          KRR=KRR+1
          FF5R(KRR)=chem_new(I,K,J,KR)
          if (ff5r(krr).lt.0)ff5r(krr)=0.
         END DO
         CALL FREEZ &
     &     (FF1R,XL,FF2R,XI,FF3R,XS,FF4R,XG,FF5R,XH, &
     &      T_NEW(I,K,J),DT,rhocgs(I,K,J), &
     &      COL,AFREEZMY,BFREEZMY,BFREEZMAX, &
     &      KRFREEZ,ICEMAX,NKR)
         IF (ORIGINAL_MELT)THEN
         CALL ORIG_MELT  &
     &    (FF1R,XL,FF2R,XI,FF3R,XS,FF4R,XG,FF5R,XH, &
     &     T_NEW(I,K,J),DT,rhocgs(I,K,J),COL,ICEMAX,NKR)
         END IF
         IF (JIWEN_FAN_MELT) THEN
         CALL J_W_MELT &
     &    (FF1R,XL,FF2R,XI,FF3R,XS,FF4R,XG,FF5R,XH, &
     &     T_NEW(I,K,J),DT,rhocgs(I,K,J),COL,ICEMAX,NKR)
         END IF
        ENDIF

        IF (T_OLD(I,K,J).GT.213)THEN     
         TT=T_OLD(I,K,J)
         QQ=QV_OLD(I,K,J)
 
         IF (QQ.LE.0)QQ=1.D-10
         PP=pcgs(I,K,J)
         TTA=T_NEW(I,K,J)
         QQA=QV(I,K,J)
         IF (QQA.LE.0) call wrf_message("WARNING: FULL SBM, QQA < 0   ")
 
 
 
 
 
 
         ES1N=AA1_MY*DEXP(-BB1_MY/TT)
         ES2N=AA2_MY*DEXP(-BB2_MY/TT)
         EW1N=QQ*PP/(0.622+0.378*QQ)
         DIV1=EW1N/ES1N
         DEL1IN=EW1N/ES1N-1.
         DIV2=EW1N/ES2N
         DEL2IN=EW1N/ES2N-1.
         ES1N=AA1_MY*DEXP(-BB1_MY/TTA)
         ES2N=AA2_MY*DEXP(-BB2_MY/TTA)
         EW1N=QQA*PP/(0.622+0.378*QQA)
         DIV3=EW1N/ES1N
         DEL1AD=EW1N/ES1N-1.
         DIV4=EW1N/ES2N
         DEL2AD=EW1N/ES2N-1.
         SUP2_OLD=DEL2IN
         DELSUP1=(DEL1AD-DEL1IN)/NCOND
         DELSUP2=(DEL2AD-DEL2IN)/NCOND
         DELDIV1=(DIV3-DIV1)/NCOND
         DELDIV2=(DIV4-DIV2)/NCOND
         DELTATEMP=0
         DELTAQ=0
         tt_old = TT
         qq_old = qq
         DIFFU=1
         DO IKL=1,NCOND
          IF (DIFFU.NE.0)THEN
          DEL1IN=DEL1IN+DELSUP1
          DEL2IN=DEL2IN+DELSUP2
          DIV1=DIV1+DELDIV1
          DIV2=DIV2+DELDIV2
          END IF

          IF (DIV1.GT.DIV2.AND.TT.LE.265)THEN











           DIFFU=0
          END IF
          IF (DIFFU.NE.0)THEN
          DEL1NR=A1_MYN*(100.*DIV1)
          DEL2NR=A2_MYN*(100.*DIV2)
     
     
     
     
     
          IF (DEL2NR.EQ.0)call wrf_error_fatal3("<stdin>",944,&
"fatal error in module_mp_full_sbm (DEL2NR.EQ.0) , model stop ")
          DEL12R=DEL1NR/DEL2NR
          DEL12RD=DEL12R**DEL_BBR
          EW1PN=AA1_MY*100.*DIV1*DEL12RD/100.
          TT=-DEL_BB/DLOG(DEL12R)
          QQ=0.622*EW1PN/(PP-0.378*EW1PN)

          DO KR=1,NKR
            FF1IN(KR)=FF1R(KR)
            DO ICE=1,ICEMAX
             FF2IN(KR,ICE)=FF2R(KR,ICE)
            ENDDO
          ENDDO
          IF (BULKNUC.eq.1)THEN
            IF (DEL1IN.GT.0)THEN
              IF (zcgs(I,K,J).LE.500.E2)THEN
                FACTZ=0.
              ELSE
                FACTZ=1

              END IF
             CONCCCN_XZ=FACTZ*ACCN*(100.*DEL1IN)**BCCN

             CONCDROP=0.D0

             DO KR=1,NKR
               CONCDROP=CONCDROP+FF1IN(KR)*XL(KR)
             ENDDO

             CONCDROP=CONCDROP*3.D0*COL
             IF(CONCCCN_XZ.GT.CONCDROP) &
     &       FF1IN(1)=FF1IN(1)+(CONCCCN_XZ-CONCDROP)/(3.D0*COL*XL(1))
            END IF
          ELSE
            IF(DEL1IN.GT.0.OR.DEL2IN.GT.0)THEN
             CALL JERNUCL01(FF1IN,FF2IN,FCCN &
     &       ,XL,XI,TT,QQ &
     &       ,rhocgs(I,K,J),pcgs(I,K,J) &
     &       ,DEL1IN,DEL2IN &
     &       ,COL,AA1_MY, BB1_MY, AA2_MY,BB2_MY &
     &       ,C1_MEY,C2_MEY,SUP2_OLD,DSUPICE_XYZ(I,K,J) &
     &       ,RCCN,DROPRADII,NKR,ICEMAX,ICEPROCS)
             IF (T_OLD(I,K,J).GT.220.AND.T_OLD(I,K,J).LE.233)THEN
              DO KR=1,NKR
               FF2IN(KR,2)=FF2IN(KR,2)+FF1IN(KR)
               FF1IN(KR)=0.
              END DO
             END IF
            END IF
 
          END IF

          DO KR=1,NKR
            FF1R(KR)=FF1IN(KR)
            DO ICE=1,ICEMAX
             FF2R(KR,ICE)=FF2IN(KR,ICE)
            ENDDO
          ENDDO
          FMAX1=0.
          FMAX2=0.
          FMAX3=0.
          FMAX4=0.
          FMAX5=0.
          DO KR=1,NKR
            FF1IN(KR)=FF1R(KR)
            FMAX1=AMAX1(FF1R(KR),FMAX1)
            FF3IN(KR)=FF3R(KR)
            FMAX3=AMAX1(FF3R(KR),FMAX3)
            FF4IN(KR)=FF4R(KR)
            FMAX4=AMAX1(FF4R(KR),FMAX4)
            FF5IN(KR)=FF5R(KR)
            FMAX5=AMAX1(FF5R(KR),FMAX5)
            DO ICE=1,ICEMAX
             FF2IN(KR,ICE)=FF2R(KR,ICE)
             FMAX2=AMAX1(FF2R(KR,ICE),FMAX2)
            END DO
          END DO
          ISYM1=0
          ISYM2=0
          ISYM3=0
          ISYM4=0
          ISYM5=0
          IF(FMAX1.GT.0)ISYM1=1
          IF (ICEPROCS.EQ.1)THEN
           IF(FMAX2.GT.1.E-4)ISYM2=1
           IF(FMAX3.GT.1.E-4)ISYM3=1
           IF(FMAX4.GT.1.E-4)ISYM4=1
           IF(FMAX5.GT.1.E-4)ISYM5=1
          END IF




          IF(ISYM1.EQ.1.AND.((TT-273.15).GT.-0.187.OR. &
     &     (ISYM2.EQ.0.AND. &
     &     ISYM3.EQ.0.AND.ISYM4.EQ.0.AND.ISYM5.EQ.0)))THEN
           IF (T_OLD(I,K,J).GT.233)THEN     
           CALL ONECOND1(TT,QQ,PP,rhocgs(I,K,J) &
     &      ,VR1,pcgs(I,K,J) &
     &      ,DEL1IN,DEL2IN,DIV1,DIV2 &
     &      ,FF1R,FF1IN,XL,RLEC,RO1BL &
     &      ,AA1_MY,BB1_MY,AA2_MY,BB2_MY &
     &      ,C1_MEY,C2_MEY &
     &      ,COL,DTCOND,ICEMAX,NKR)
           END IF
          ELSE IF(ISYM1.EQ.0.AND.(TT-273.15).LE.-0.187.AND. &
     &     (ISYM2.EQ.1.OR.ISYM3.EQ.1.OR.ISYM4.EQ.1.OR.ISYM5.EQ.1))THEN
           IF (T_OLD(I,K,J).GT.233)THEN     
           CALL ONECOND2(TT,QQ,PP,rhocgs(I,K,J) &
     &      ,VR2,VR3,VR4,VR5,pcgs(I,K,J) &
     &      ,DEL1IN,DEL2IN,DIV1,DIV2 &
     &      ,FF2R,FF2IN,XI,RIEC,RO2BL &
     &      ,FF3R,FF3IN,XS,RSEC,RO3BL &
     &      ,FF4R,FF4IN,XG,RGEC,RO4BL &
     &      ,FF5R,FF5IN,XH,RHEC,RO5BL &
     &      ,AA1_MY,BB1_MY,AA2_MY,BB2_MY &
     &      ,C1_MEY,C2_MEY &
     &      ,COL,DTCOND,ICEMAX,NKR &
     &      ,ISYM2,ISYM3,ISYM4,ISYM5)
           END IF
          ELSE IF(ISYM1.EQ.1.AND.(TT-273.15).LE.-0.187.AND. &
     &     (ISYM2.EQ.1.OR.ISYM3.EQ.1.OR.ISYM4.EQ.1 &
     &     .OR.ISYM5.EQ.1))THEN
           CALL ONECOND3(TT,QQ,PP,rhocgs(I,K,J) &
     &      ,VR1,VR2,VR3,VR4,VR5,pcgs(I,K,J) &
     &      ,DEL1IN,DEL2IN,DIV1,DIV2 &
     &      ,FF1R,FF1IN,XL,RLEC,RO1BL &
     &      ,FF2R,FF2IN,XI,RIEC,RO2BL &
     &      ,FF3R,FF3IN,XS,RSEC,RO3BL &
     &      ,FF4R,FF4IN,XG,RGEC,RO4BL &
     &      ,FF5R,FF5IN,XH,RHEC,RO5BL &
     &      ,AA1_MY,BB1_MY,AA2_MY,BB2_MY &
     &      ,C1_MEY,C2_MEY &
     &      ,COL,DTCOND,ICEMAX,NKR &
     &      ,ISYM1,ISYM2,ISYM3,ISYM4,ISYM5)
          END IF
          END IF

             IF (IKL.EQ.NCOND)CALL COAL_BOTT_NEW(FF1R,FF2R,FF3R, &
     &       FF4R,FF5R,TT,QQ,PP,rhocgs(I,K,J),dt_coll,TCRIT,TTCOAL)
         END DO
         IF (DIFFU.EQ.0)THEN
         th_phy(i,k,j) = tt_old/pi_phy(i,k,j)
         qv(i,k,j)=qq_old



         ELSE
         th_phy(i,k,j) = tt/pi_phy(i,k,j)
         qv(i,k,j)=qq
         END IF
        END IF

        IF (REMSAT.EQ.1)THEN
        DO KR=1,NKR
         FF1R(KR)=0.
         FCCN(KR)=0
         IF (ICEPROCS.EQ.1)THEN
          FF2R(KR,1)=0.
          FF2R(KR,2)=0.
          FF2R(KR,3)=0.
          FF3R(KR)=0.
          FF4R(KR)=0.
          FF5R(KR)=0.
         END IF
        END DO
        END IF



        kext_ql(i,k,j)=0.
        krr=0
        DO kr=p_ff1i01,p_ff1i33
          KRR=KRR+1
          chem_new(I,K,J,KR)=FF1R(KRR)
          geo_cs=3.1415*(3.*xl(krr)/(4.*3.1415*1.))**(2./3.)
          ft=0.
          kext_ql(i,k,j)=kext_ql(i,k,j)+(1.-ft_liq(krr))*2.*geo_cs*(100.*col*3.*xl(krr))*ff1r(krr)






        END DO   


        KRR=0
        kext_qa(i,k,j)=0.
        DO kr=p_ff8i01,p_ff8i33
          KRR=KRR+1
          chem_new(I,K,J,KR)=FCCN(KRR)
           geo_cs=3.1415*(3*XCCN(krr)/(4*3.1415*0.4))**(2./3.)
           kext_qa(i,k,j)=kext_qa(i,k,j)+2.*geo_cs*fccn(krr)

        END DO
        IF (ICEPROCS.EQ.1)THEN

         EFF_NI(i,k,j)=0.
         eff_di(i,k,j)=0.
         EFF_NQIC=0
         EFF_DQIC=0
         EFF_NQIP=0
         EFF_DQIP=0
         EFF_NQID=0
         EFF_DQID=0
         KRR=0
         kext_qs(i,k,j)=0.
         kext_ft_qs(i,k,j)=0.
         lambda = 0.55
         chi0 = 0.00000
         xi1 = 0.12534e-2
         xi2 = 0.38929e-2
         xi3 = 0.36593
         xi4 = 0.38827e-1
         xi5 = 0.87616
         DO kr=p_ff5i01,p_ff5i33
          KRR=KRR+1
          chem_new(I,K,J,KR)=FF3R(KRR)
          geo_cs=3.1415*(xs(krr)/(1.2*3.1415*ro3bl(krr)))**(2./3.)
          volume=sqrt(geo_cs/3.1415)**3*chem_new(i,k,j,KR)*XS(KRR)
          surface_area=sqrt(geo_cs/3.1415)**2*chem_new(i,k,j,KR)*XS(KRR)
          if (surface_area.ne.0.and.volume.ne.0)then
          r_e = 3.0/4.0*volume/surface_area
          chi_e = 2.0*pi*(r_e*1.E4)/lambda

          f1 = (1.0 - xi1)* &
     &         (1.0 - (1.0 - exp(-xi2*(chi_e - chi0)))/xi2/(chi_e - chi0))
          f2 = (1.0 - xi3)* &
     &         (1.0 - exp(-xi4*(chi_e - chi0)))
          if(chi_e.le.chi0) then 
             ft = 0
          else 
           ft = (1.0 - xi5)*f1 + xi5*f2
          end if
          else 
          ft=0.
          end if
          ft=0.
          kext_qs(i,k,j)=kext_qs(i,k,j)+(1.-ft)*2.*geo_cs*(100.*col*3.*xs(krr))*ff3r(krr)
         END DO



         KRR=0
         kext_qg(i,k,j)=0.
         kext_ft_qg(i,k,j)=0.
       lambda = 0.55
       chi0 = 0.00000
       xi1 = 0.39026e-1
       xi2 = 0.94264e-5
       xi3 = 0.11281e-2
       xi4 = 0.35218e-1
       xi5 = 0.51453
         DO kr=p_ff6i01,p_ff6i33
          KRR=KRR+1
          chem_new(I,K,J,KR)=FF4R(KRR)
          geo_cs=3.1415*(3.*xg(krr)/(4.*3.1415*0.4))**(2./3.)
          volume=sqrt(geo_cs/3.1415)**3*chem_new(i,k,j,KR)*XG(KRR)
          surface_area=sqrt(geo_cs/3.1415)**2*chem_new(i,k,j,KR)*XG(KRR)
          if (surface_area.ne.0.and.volume.ne.0)then
          r_e = 3.0/4.0*volume/surface_area
          chi_e = 2.0*pi*(r_e*1.E4)/lambda

          f1 = (1.0 - xi1)* &
     &         (1.0 - (1.0 - exp(-xi2*(chi_e - chi0)))/xi2/(chi_e - chi0))
          f2 = (1.0 - xi3)* &
     &         (1.0 - exp(-xi4*(chi_e - chi0)))
          if(chi_e.le.chi0) then 
             ft = 0
          else 
           ft = (1.0 - xi5)*f1 + xi5*f2
          end if
          else 
          ft=0.
          end if
          ft=0.
          kext_qg(i,k,j)=kext_qg(i,k,j)+(1.-ft)*2.*geo_cs*(100.*col*3.*xg(krr))*ff4r(krr)
         END DO


         KRR=0
         kext_qic(i,k,j)=0.
         kext_ft_qic(i,k,j)=0.
       lambda = 0.55
       chi0 = 0.00000
       xi1 = 0.60202
       xi2 = 0.85513e-3
       xi3 = 0.97065e-1
       xi4 = 0.21320e-1
       xi5 = 0.66985
         DO kr=p_ff2i01,p_ff2i33
          KRR=KRR+1
          chem_new(I,K,J,KR)=FF2R(KRR,1)
          geo_cs=0.26*(xi(krr,1)/(ro2bl(krr,1)*0.2))**1.28
          volume=sqrt(geo_cs/3.1415)**3*chem_new(i,k,j,KR)*XI(KRR,1)
          surface_area=sqrt(geo_cs/3.1415)**2.*chem_new(i,k,j,KR)*XI(KRR,1)
          if (surface_area.ne.0.and.volume.ne.0)then
          r_e = 3.0/4.0*volume/surface_area
          chi_e = 2.0*pi*(r_e*1.E4)/lambda

          f1 = (1.0 - xi1)* &
     &         (1.0 - (1.0 - exp(-xi2*(chi_e - chi0)))/xi2/(chi_e - chi0))
          f2 = (1.0 - xi3)* &
     &         (1.0 - exp(-xi4*(chi_e - chi0)))
          if(chi_e.le.chi0) then 
             ft = 0
          else 
           ft = (1.0 - xi5)*f1 + xi5*f2
          end if
          else
          ft=0.
          end if
          ft=0.
          kext_qic(i,k,j)=kext_qic(i,k,j)+(1.-ft)*2.*geo_cs*(100.*col*3.*xi(krr,1))*ff2r(krr,1)
          EFF_NI(i,k,j)=sqrt(geo_cs/3.1415)**3*chem_new(i,k,j,KR)*XI(KRR,1)+EFF_NI(i,k,j)
          eff_di(i,k,j)=sqrt(geo_cs/3.1415)**2*chem_new(i,k,j,KR)*XI(KRR,1)+eff_di(i,k,j)
          EFF_NQIC=sqrt(geo_cs/3.1415)**3*chem_new(i,k,j,KR)*XI(KRR,1)+EFF_NQIC
          eff_dqic=sqrt(geo_cs/3.1415)**2*chem_new(i,k,j,KR)*XI(KRR,1)+eff_dqic
         END DO
         IF (EFF_DQIC.NE.0)THEN
         QIC_EFFR(I,K,J)=EFF_NQIC/EFF_DQIC
         ELSE
         QIC_EFFR(I,K,J)=0.
         END IF
         krr=0

901      format(' ',i3,1x,f12.9,1x,3(f12.9,1x),f12.6,f12.3,1x,10(f12.8,1x))


         KRR=0
         kext_qip(i,k,j)=0.
       lambda = 0.55
       chi0 = 0.00000
       xi1 = 0.23397e-2
       xi2 = 0.19513e-2
       xi3 = 0.51912e-4
       xi4 = 0.15159e-1
       xi5 = 0.81012
         DO kr=p_ff3i01,p_ff3i33
          KRR=KRR+1
          chem_new(I,K,J,KR)=FF2R(KRR,2)
          geo_cs=(3.1415/4)*(xi(krr,2)/(ro2bl(krr,2)*0.108))**0.72
          volume=sqrt(geo_cs/3.1415)**3.*chem_new(i,k,j,KR)*XI(KRR,2)
          surface_area=sqrt(geo_cs/3.1415)**2*chem_new(i,k,j,KR)*XI(KRR,2)
          if (surface_area.ne.0.and.volume.ne.0)then
          r_e = 3.0/4.0*volume/surface_area
          chi_e = 2.0*pi*(r_e*1.E4)/lambda

          f1 = (1.0 - xi1)* &
     &         (1.0 - (1.0 - exp(-xi2*(chi_e - chi0)))/xi2/(chi_e - chi0))
          f2 = (1.0 - xi3)* &
     &         (1.0 - exp(-xi4*(chi_e - chi0)))
          if(chi_e.le.chi0) then 
             ft = 0
          else 
           ft = (1.0 - xi5)*f1 + xi5*f2
          end if
          else 
           ft=0.
          end if
          ft=0.
          kext_qip(i,k,j)=kext_qip(i,k,j)+(1.-ft)*2.*geo_cs*(100.*col*3*xi(krr,2))*ff2r(krr,2)
          EFF_NI(i,k,j)=sqrt(geo_cs/3.1415)**3*chem_new(i,k,j,KR)*XI(KRR,2)+EFF_NI(i,k,j)
          eff_di(i,k,j)=sqrt(geo_cs/3.1415)**2*chem_new(i,k,j,KR)*XI(KRR,2)+eff_di(i,k,j)
          EFF_NQIP=sqrt(geo_cs/3.1415)**3*chem_new(i,k,j,KR)*XI(KRR,2)+EFF_NQIP
          eff_dqiP=sqrt(geo_cs/3.1415)**2*chem_new(i,k,j,KR)*XI(KRR,2)+eff_dqip
         END DO
         IF (EFF_DQIP.NE.0)THEN
         QIP_EFFR(I,K,J)=EFF_NQIP/EFF_DQIP
         ELSE
         QIP_EFFR(I,K,J)=0.
         END IF





         KRR=0
         kext_qid(i,k,j)=0.
         lambda = 0.55
         chi0 = 0.00000
         xi1 = 0.14875
         xi2 = 0.49514e-2
         xi3 = 0.36201
         xi4 = 0.36993e-1
         xi5 = 0.87020
         DO KR=p_ff4i01,p_ff4i33
          KRR=KRR+1
          chem_new(I,K,J,KR)=FF2R(KRR,3)
          geo_cs=(3.1415/4)*(xi(krr,3)/(ro2bl(krr,3)*7.8E-3))**0.828
          volume=sqrt(geo_cs/3.1415)**3*chem_new(i,k,j,KR)*XI(KRR,3)
          surface_area=sqrt(geo_cs/3.1415)**2*chem_new(i,k,j,KR)*XI(KRR,3)
          if (surface_area.ne.0.and.volume.ne.0)then
          r_e = 3.0/4.0*volume/surface_area
          chi_e = 2.0*pi*(r_e*1.E4)/lambda

          f1 = (1.0 - xi1)* &
     &         (1.0 - (1.0 - exp(-xi2*(chi_e - chi0)))/xi2/(chi_e - chi0))
          f2 = (1.0 - xi3)* &
     &         (1.0 - exp(-xi4*(chi_e - chi0)))
          if(chi_e.le.chi0) then 
             ft = 0
          else 
           ft = (1.0 - xi5)*f1 + xi5*f2
          end if
          else
           ft=0.
          end if
          ft=0.
          kext_qid(i,k,j)=kext_qid(i,k,j)+(1.-ft)*2.*geo_cs*(100.*col*3*xi(krr,3))*ff2r(krr,3)
          EFF_NI(i,k,j)=sqrt(geo_cs/3.1415)**3*chem_new(i,k,j,KR)*XI(KRR,3)+EFF_NI(i,k,j)
          eff_di(i,k,j)=sqrt(geo_cs/3.1415)**2*chem_new(i,k,j,KR)*XI(KRR,3)+eff_di(i,k,j)
          EFF_NQID=sqrt(geo_cs/3.1415)**3*chem_new(i,k,j,KR)*XI(KRR,3)+EFF_NQID
          eff_dqiD=sqrt(geo_cs/3.1415)**2*chem_new(i,k,j,KR)*XI(KRR,3)+eff_dqiD
         END DO
         IF (EFF_DQID.NE.0)THEN
         QID_EFFR(I,K,J)=EFF_NQID/EFF_DQID
         ELSE
         QID_EFFR(I,K,J)=0.
         END IF



         KRR=0
         kext_qh(i,k,j)=0.
         DO KR=p_ff7i01,p_ff7i33
          KRR=KRR+1
          chem_new(I,K,J,KR)=FF5R(KRR)
          geo_cs=3.1415*(3*xh(krr)/(4*3.1415*0.9))**(2/3)
          kext_qh(i,k,j)=kext_qh(i,k,j)+2.*geo_cs*(100.*col*3*xh(krr))*ff5r(krr)
          EFF_NI(i,k,j)=sqrt(geo_cs/3.1415)**3*chem_new(i,k,j,KR)*XH(KRR)+EFF_NI(i,k,j)
          eff_di(i,k,j)=sqrt(geo_cs/3.1415)**2*chem_new(i,k,j,KR)*XH(KRR)+eff_di(i,k,j)
         END DO

        END IF
      END DO
      END DO
      END DO

      NKRO=1
      NKRE=NKR
      DO j = jts,jte
      DO i = its,ite
      DO k = kts,kte
      rhocgs_z(k)=rhocgs(i,k,j)
      pcgs_z(k)=pcgs(i,k,j)
      zcgs_z(k)=zcgs(i,k,j)
      krr=0
      do kr=p_ff1i01,p_ff1i33
       krr=krr+1
       ffx_z(k,krr)=chem_new(i,k,j,kr)/rhocgs(i,k,j)
      end do
      end do
      CALL FALFLUXHUCM(ffx_z,VR1,RHOCGS_z,PCGS_z,ZCGS_z,DT,kts,kte,nkr)
      DO k = kts,kte
      krr=0
      do kr=p_ff1i01,p_ff1i33
       krr=krr+1
       chem_new(i,k,j,kr)=ffx_z(k,krr)*rhocgs(i,k,j)
      end do
      end do
      if (iceprocs.eq.1)then
      DO k = kts,kte
      rhocgs_z(k)=rhocgs(i,k,j)
      pcgs_z(k)=pcgs(i,k,j)
      zcgs_z(k)=zcgs(i,k,j)
      krr=0
      do kr=p_ff5i01,p_ff5i33
       krr=krr+1
       ffx_z(k,krr)=chem_new(i,k,j,kr)/rhocgs(i,k,j)
      end do
      end do
      CALL FALFLUXHUCM(ffx_z,VR3,RHOCGS_z,PCGS_z,ZCGS_z,DT,kts,kte,nkr)
      DO k = kts,kte
      krr=0
      do kr=p_ff5i01,p_ff5i33
       krr=krr+1
       chem_new(i,k,j,kr)=ffx_z(k,krr)*rhocgs(i,k,j)
      end do
      end do
      DO k = kts,kte
      rhocgs_z(k)=rhocgs(i,k,j)
      pcgs_z(k)=pcgs(i,k,j)
      zcgs_z(k)=zcgs(i,k,j)
      krr=0
      do kr=p_ff6i01,p_ff6i33
       krr=krr+1
       ffx_z(k,krr)=chem_new(i,k,j,kr)/rhocgs(i,k,j)
      end do
      end do
      CALL FALFLUXHUCM(ffx_z,VR4,RHOCGS_z,PCGS_z,ZCGS_z,DT,kts,kte,nkr)
      DO k = kts,kte
      krr=0
      do kr=p_ff6i01,p_ff6i33
       krr=krr+1
       chem_new(i,k,j,kr)=ffx_z(k,krr)*rhocgs(i,k,j)
      end do
      end do

      DO k = kts,kte
      rhocgs_z(k)=rhocgs(i,k,j)
      pcgs_z(k)=pcgs(i,k,j)
      zcgs_z(k)=zcgs(i,k,j)
      krr=0
      do kr=p_ff2i01,p_ff2i33
       krr=krr+1
       ffx_z(k,krr)=chem_new(i,k,j,kr)/rhocgs(i,k,j)
       vri(krr)=vr2(krr,1)
      end do
      end do
      CALL FALFLUXHUCM(ffx_z,VRI,RHOCGS_z,PCGS_z,ZCGS_z,DT,kts,kte,nkr)
      DO k = kts,kte
      krr=0
      do kr=p_ff2i01,p_ff2i33
       krr=krr+1
       chem_new(i,k,j,kr)=ffx_z(k,krr)*rhocgs(i,k,j)
      end do
      end do
      DO k = kts,kte
      rhocgs_z(k)=rhocgs(i,k,j)
      pcgs_z(k)=pcgs(i,k,j)
      zcgs_z(k)=zcgs(i,k,j)
      krr=0
      do kr=p_ff3i01,p_ff3i33
       krr=krr+1
       ffx_z(k,krr)=chem_new(i,k,j,kr)/rhocgs(i,k,j)
       vri(krr)=vr2(krr,2)
      end do
      end do
      CALL FALFLUXHUCM(ffx_z,VRI,RHOCGS_z,PCGS_z,ZCGS_z,DT,kts,kte,nkr)
      DO k = kts,kte
      krr=0
      do kr=p_ff3i01,p_ff3i33
       krr=krr+1
       chem_new(i,k,j,kr)=ffx_z(k,krr)*rhocgs(i,k,j)
      end do
      end do
      DO k = kts,kte
      rhocgs_z(k)=rhocgs(i,k,j)
      pcgs_z(k)=pcgs(i,k,j)
      zcgs_z(k)=zcgs(i,k,j)
      krr=0
      do kr=p_ff4i01,p_ff4i33
       krr=krr+1
       ffx_z(k,krr)=chem_new(i,k,j,kr)/rhocgs(i,k,j)
       vri(krr)=vr2(krr,3)
      end do
      end do
      CALL FALFLUXHUCM(ffx_z,VRI,RHOCGS_z,PCGS_z,ZCGS_z,DT,kts,kte,nkr)
      DO k = kts,kte
      krr=0
      do kr=p_ff4i01,p_ff4i33
       krr=krr+1
       chem_new(i,k,j,kr)=ffx_z(k,krr)*rhocgs(i,k,j)
      end do
      end do
      DO k = kts,kte
      rhocgs_z(k)=rhocgs(i,k,j)
      pcgs_z(k)=pcgs(i,k,j)
      zcgs_z(k)=zcgs(i,k,j)
      krr=0
      do kr=p_ff7i01,p_ff7i33
       krr=krr+1
       ffx_z(k,krr)=chem_new(i,k,j,kr)/rhocgs(i,k,j)
      end do
      end do
      CALL FALFLUXHUCM(ffx_z,VR5,RHOCGS_z,PCGS_z,ZCGS_z,DT,kts,kte,nkr)
      DO k = kts,kte
      krr=0
      do kr=p_ff7i01,p_ff7i33
       krr=krr+1
       chem_new(i,k,j,kr)=ffx_z(k,krr)*rhocgs(i,k,j)
      end do
      end do
      end if
      end do 
      end do 

      gmax=0
      qmax=0
      imax=0
      kmax=0
      qnmax=0
      inmax=0
      knmax=0
      DO j = jts,jte
      DO k = kts,kte
      DO i = its,ite
      QC(I,K,J)=0
      QR(I,K,J)=0
      QIC(I,K,J)=0
      QIP(I,K,J)=0
      QID(I,K,J)=0
      QS(I,K,J)=0
      QG(I,K,J)=0
      QH(I,K,J)=0
      QNC(I,K,J)=0
      QNR(I,K,J)=0
      QNIP(I,K,J)=0
      QNIC(I,K,J)=0
      QNID(I,K,J)=0
      QNS(I,K,J)=0
      QNG(I,K,J)=0
      QNH(I,K,J)=0
      QNA(I,K,J)=0
      tt= th_phy(i,k,j)*pi_phy(i,k,j)
      DO KR=1,NKR
      COLREFLL(KR)=COEFREFLL
      COLREFLI(KR)=COEFREFLI
        IF(TT.GE.271.15.AND.TT.LE.273.15) THEN
               COLREFLS(KR)=COEFREF00/0.09
               COLREFLG(KR)=COEFREF00/RO4BL(KR)/RO4BL(KR)
               COLREFLH(KR)=COEFREF00/RO5BL(KR)/RO5BL(KR)
        ELSE
               COLREFLS(KR)=COEFREFLI
               COLREFLG(KR)=COEFREFLI
               COLREFLH(KR)=COEFREFLI
        ENDIF
      END DO

      EFF_N=0.
      EFF_D=0.
      KRR=0
      DO KR = p_ff1i01,p_ff1i33
        KRR=KRR+1
        IF (KRR.LT.KRDROP)THEN
          EFF_N=DROPRADII(KRR)**3*chem_new(i,k,j,KR)*XL(KRR)+EFF_N
          EFF_D=DROPRADII(KRR)**2*chem_new(i,k,j,KR)*XL(KRR)+EFF_D
          QC(I,K,J)=QC(I,K,J) &
     &      +(1./RHOCGS(I,K,J))*COL*chem_new(I,K,J,KR)*XL(KRR)*XL(KRR)*3



           QNC(I,K,J)=QNC(I,K,J) &
    &       +COL*chem_new(I,K,J,KR)*XL(KRR)*3/rhocgs(I,K,J)*1000. 
        ELSE
          QR(I,K,J)=QR(I,K,J) &
     &      +(1./RHOCGS(I,K,J))*COL*chem_new(I,K,J,KR)*XL(KRR)*XL(KRR)*3
          QNR(I,K,J)=QNR(I,K,J) &

     &      +COL*chem_new(I,K,J,KR)*XL(KRR)*3/rhocgs(I,K,J)*1000. 
        END IF
      END DO
      IF(QC(I,K,J).GT.1.E-6.and.EFF_D.GT.0)THEN
          EFFR(I,K,J)=EFF_N/EFF_D
      ELSE
          EFFR(I,K,J)=0.
      END IF
      KRR=0
      IF (ICEPROCS.EQ.1)THEN
       KRR=0
       DO  KR=p_ff5i01,p_ff5i33
        KRR=KRR+1




        QS(I,K,J)=QS(I,K,J) &
     &   +(1./RHOCGS(I,K,J))*COL*chem_new(I,K,J,KR)*XS(KRR)*XS(KRR)*3

        QNS(I,K,J)=QNS(I,K,J) &
     &   +COL*chem_new(I,K,J,KR)*XS(KRR)*3/rhocgs(I,K,J)*1000. 
       END DO
       KRR=0
       DO  KR=p_ff6i01,p_ff6i33
        KRR=KRR+1
        QG(I,K,J)=QG(I,K,J) &
     &   +(1./RHOCGS(I,K,J))*COL*chem_new(I,K,J,KR)*XG(KRR)*XG(KRR)*3
        QNG(I,K,J)=QNG(I,K,J) &

     &   +COL*chem_new(I,K,J,KR)*XG(KRR)*3/rhocgs(I,K,J)*1000. 
       END DO
       KRR=0
       DO  KR=p_ff2i01,p_ff2i33
        KRR=KRR+1
        QIC(I,K,J)=QIC(I,K,J) &
     &   +(1./RHOCGS(I,K,J))*COL*chem_new(I,K,J,KR)*XI(KRR,1)*XI(KRR,1)*3
        QNIC(I,K,J)=QNIC(I,K,J) &
     &   +COL*chem_new(I,K,J,KR)*XI(KRR,1)*3/rhocgs(I,K,J)*1000. 
       END DO
       KRR=0
       DO  KR=p_ff3i01,p_ff3i33
        KRR=KRR+1
        QIP(I,K,J)=QIP(I,K,J) &
     &   +(1./RHOCGS(I,K,J))*COL*chem_new(I,K,J,KR)*XI(KRR,2)*XI(KRR,2)*3
        QNIP(I,K,J)=QNIP(I,K,J) &
     &   +COL*chem_new(I,K,J,KR)*XI(KRR,2)*3/rhocgs(I,K,J)*1000. 
       END DO
       KRR=0
       DO  KR=p_ff4i01,p_ff4i33
        KRR=KRR+1
        QID(I,K,J)=QID(I,K,J) &
     &   +(1./RHOCGS(I,K,J))*COL*chem_new(I,K,J,KR)*XI(KRR,3)*XI(KRR,3)*3
        QNID(I,K,J)=QNID(I,K,J) &
     &   +COL*chem_new(I,K,J,KR)*XI(KRR,3)*3/rhocgs(I,K,J)*1000. 
       END DO
         IF((QIP(I,K,J).GT.1.E-6.OR.QIC(I,K,J).GT.1.E-6.OR.QID(I,K,J).GT.1.E-6)&
     &   .and.eff_di(i,k,j).GT.0)THEN
          ICE_EFFR(I,K,J)=EFF_NI(i,k,j)/eff_di(i,k,j)
         ELSE
          ICE_EFFR(I,K,J)=0.
         END IF
      END IF
       KRR=0
       DO  KR=p_ff8i01,p_ff8i33
        KRR=KRR+1
        QNA(I,K,J)=QNA(I,K,J) &


     &   +COL*chem_new(I,K,J,KR)/rhocgs(I,K,J)*1000.   
       END DO



       KRR=0
       DO  KR=p_ff7i01,p_ff7i33
        KRR=KRR+1
        QH(I,K,J)=QH(I,K,J) &
     &   +(1./RHOCGS(I,K,J))*COL*chem_new(I,K,J,KR)*XH(KRR)*XH(KRR)*3
        QNH(I,K,J)=QNH(I,K,J) &
     &   +COL*chem_new(I,K,J,KR)*XH(KRR)*3/rhocgs(I,K,J)*1000. 
       END DO
      END DO
      END DO
      END DO




998   format(' ',10(f10.1,1x))
      DO j = jts,jte
      DO i = its,ite
       krr=0
       DO KR=p_ff1i01,p_ff1i33
        krr=krr+1
        DELTAW=VR1(KRR)
        RAINNC(I,J)=RAINNC(I,J) &
     &  +10*(3./RO1BL(KRR))*COL*DT*DELTAW* &
     &           chem_new(I,1,J,KR)*XL(KRR)*XL(KRR)
       END DO
       KRR=0
       DO KR=p_ff5i01,p_ff5i33
        KRR=KRR+1
        DELTAW=VR3(KRR)
        RAINNC(I,J)=RAINNC(I,J) &
     &  +10*(3./RO1BL(KRR))*COL*DT*DELTAW* &
     &           chem_new(I,1,J,KR)*XS(KRR)*XS(KRR)
       END DO
       KRR=0
       DO KR=p_ff6i01,p_ff6i33
        KRR=KRR+1
        DELTAW=VR4(KRR)
        RAINNC(I,J)=RAINNC(I,J) &
     &  +10*(3./RO1BL(KRR))*COL*DT*DELTAW* &
     &           chem_new(I,1,J,KR)*XG(KRR)*XG(KRR)
       END DO
       KRR=0
       DO KR=p_ff2i01,p_ff2i33
        KRR=KRR+1
        DELTAW=VR2(KRR,1)
        RAINNC(I,J)=RAINNC(I,J) &
     &  +10*(3./RO1BL(KRR))*COL*DT*DELTAW* &
     &           chem_new(I,1,J,KR)*XI(KRR,1)*XI(KRR,1)
       END DO
       KRR=0
       DO KR=p_ff3i01,p_ff3i33
        KRR=KRR+1
        DELTAW=VR2(KRR,2)
        RAINNC(I,J)=RAINNC(I,J) &
     &  +10*(3./RO1BL(KRR))*COL*DT*DELTAW* &
     &           chem_new(I,1,J,KR)*XI(KRR,2)*XI(KRR,2)
       END DO
       KRR=0
       DO KR=p_ff4i01,p_ff4i33
        KRR=KRR+1
        DELTAW=VR2(KRR,3)
        RAINNC(I,J)=RAINNC(I,J) &
     &  +10*(3./RO1BL(KRR))*COL*DT*DELTAW* &
     &           chem_new(I,1,J,KR)*XI(KRR,3)*XI(KRR,3)
       END DO
       KRR=0
       DO KR=p_ff7i01,p_ff7i33
        KRR=KRR+1
        DELTAW=VR5(KRR)
        RAINNC(I,J)=RAINNC(I,J) &
     &  +10*(3./RO1BL(KRR))*COL*DT*DELTAW* &
     &           chem_new(I,1,J,KR)*XH(KRR)*XH(KRR)
       END DO

  
   
      do k=kts,kte


          qv1d(k)=qv(i,k,j)
          qr1d(k)=qr(i,k,j)
          nr1d(k)=qnr(i,k,j)
          qs1d(k)=qs(i,k,j)
          ns1d(k)=qns(i,k,j)
          qg1d(k)=qg(i,k,j)+qh(i,k,j)
          ng1d(k)=qng(i,k,j)+qnh(i,k,j)
          t1d(k)=th_phy(i,k,j)*pi_phy(i,k,j)
          p1d(k)=P_PHY(I,K,J)
       end do



         IF ( PRESENT (diagflag) ) THEN
         if (diagflag .and. do_radar_ref == 1) then
          call refl10cm_hm (qv1d, qr1d, nr1d, qs1d, ns1d, qg1d, ng1d,   &
                      t1d, p1d, dBZ, kts, kte, i, j)
          do k = kts, kte
             refl_10cm(i,k,j) = MAX(-35., dBZ(k))
          enddo
         endif
         ENDIF

      END DO
      END DO



      do j=jts,jte
      do k=kts,kte
      do i=its,ite


         th_old(i,k,j)=th_phy(i,k,j)
         qv_old(i,k,j)=qv(i,k,j)




      end do
      end do
      end do


      if (conserv)then
      DO j = jts,jte
      DO i = its,ite
      DO k = kts,kte
      rhocgs(I,K,J)=rho_phy(I,K,J)*0.001
      krr=0
      DO KR=p_ff1i01,p_ff1i33
        krr=krr+1
       chem_new(I,K,J,KR)=chem_new(I,K,J,KR)/RHOCGS(I,K,J)*COL*XL(KRR)*XL(KRR)*3.0
       if (qc(i,k,j)+qr(i,k,j).lt.1.e-13)chem_new(I,K,J,KR)=0.
      END DO
      KRR=0
      DO KR=p_ff5i01,p_ff5i33
       KRR=KRR+1
       chem_new(I,K,J,KR)=chem_new(I,K,J,KR)/RHOCGS(I,K,J)*COL*XS(KRR)*XS(KRR)*3.0
       if (qs(i,k,j).lt.1.e-13)chem_new(I,K,J,KR)=0.
      END DO
      KRR=0
      DO KR=p_ff6i01,p_ff6i33
       KRR=KRR+1
       chem_new(I,K,J,KR)=chem_new(I,K,J,KR)/RHOCGS(I,K,J)*COL*XG(KRR)*XG(KRR)*3.0
       if (qg(i,k,j).lt.1.e-13)chem_new(I,K,J,KR)=0.
      END DO
      KRR=0



      DO KR=p_ff8i01,p_ff8i33
       KRR=KRR+1


       chem_new(I,K,J,KR)=chem_new(I,K,J,KR)/RHOCGS(I,K,J)*1000.          
      END DO



      KRR=0
      DO KR=p_ff2i01,p_ff2i33
       KRR=KRR+1
       chem_new(I,K,J,KR)=chem_new(I,K,J,KR)/RHOCGS(I,K,J)*COL*XI(KRR,1)*XI(KRR,1)*3.0
       if (qic(i,k,j).lt.1.e-13)chem_new(I,K,J,KR)=0.
      END DO
      KRR=0
      DO KR=p_ff3i01,p_ff3i33
       KRR=KRR+1
       chem_new(I,K,J,KR)=chem_new(I,K,J,KR)/RHOCGS(I,K,J)*COL*XI(KRR,2)*XI(KRR,2)*3.0
       if (qip(i,k,j).lt.1.e-13)chem_new(I,K,J,KR)=0.
      END DO
      KRR=0
      DO KR=p_ff4i01,p_ff4i33
       KRR=KRR+1
       chem_new(I,K,J,KR)=chem_new(I,K,J,KR)/RHOCGS(I,K,J)*COL*XI(KRR,3)*XI(KRR,3)*3.0
       if (qid(i,k,j).lt.1.e-13)chem_new(I,K,J,KR)=0.
      END DO
      KRR=0
      DO KR=p_ff7i01,p_ff7i33
       KRR=KRR+1
       chem_new(I,K,J,KR)=chem_new(I,K,J,KR)/RHOCGS(I,K,J)*COL*XH(KRR)*XH(KRR)*3.0
       if (qh(i,k,j).lt.1.e-13)chem_new(I,K,J,KR)=0.
      END DO
      END DO
      END DO
      END DO
      END IF
     

      RETURN
  END SUBROUTINE SBM
      SUBROUTINE FALFLUXHUCM(chem_new,VR1,RHOCGS,PCGS,ZCGS,DT, &
     &     kts,kte,nkr)
      IMPLICIT NONE
      INTEGER I,J,K,KR
      INTEGER    kts,kte,nkr
      REAL TFALL,DTFALL,VFALL(KTE),DWFLUX(KTE)
      REAL DT
      INTEGER IFALL,N,NSUB
      REAL, DIMENSION( kts:kte,nkr ) :: chem_new 
      REAL,  DIMENSION(kts:kte) :: rhocgs,pcgs,zcgs
      REAL VR1(NKR)
















      DO KR=1,NKR
       IFALL=0
       DO k = kts,kte
          IF(chem_new(K,KR).GE.1.E-10)IFALL=1
       END DO 
       IF (IFALL.EQ.1)THEN
        TFALL=1.E10                
        DO K=kts,kte
         VFALL(K) = VR1(KR)*SQRT(1.E6/PCGS(K))









         TFALL=AMIN1(TFALL,ZCGS(K)/(VFALL(K)+1.E-20))    


        END DO                                                 
        IF(TFALL.GE.1.E10) call wrf_error_fatal3("<stdin>",1901,&
"fatal error in module_mp_full_sbm (TFALL.GE.1.E10), model stop")
        NSUB=(INT(2.0*DT/TFALL)+1)                           
        DTFALL=DT/NSUB                                      

        DO N=1,NSUB                                    
          DO K=KTS,KTE-1                               
           DWFLUX(K)=-(RHOCGS(K)*VFALL(K)*chem_new(k,kr)- &
     &     RHOCGS(K+1)* &
     &     VFALL(K+1)*chem_new(K+1,KR))/(RHOCGS(K)*(ZCGS(K+1)- &
     &      ZCGS(K)))    
          END DO    

          DWFLUX(KTE)=-(RHOCGS(KTE)*VFALL(KTE)* & 
     &       chem_new(kte,kr))/(RHOCGS(KTE)*(ZCGS(KTE)-ZCGS(KTE-1)))         
          DO K=kts,kte                                         
           chem_new(k,kr)=chem_new(k,kr)+DWFLUX(K)*DTFALL
          END DO  
        END DO  
       END IF
      END DO  
      RETURN                                                                  
      END SUBROUTINE FALFLUXHUCM                                                                    
      SUBROUTINE FULL_HUCMINIT(DT)
      IMPLICIT NONE
      INTEGER IKERN_0,IKERN_Z,L0_REAL,L0_INTEGER,INEWMEY,INEST
      INTEGER I,J,K,KR
      REAL DT
      INTEGER :: hujisbm_unit1
      LOGICAL, PARAMETER :: PRINT_diag=.FALSE.
      LOGICAL :: opened 
      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
      CHARACTER*80 errmess
      REAL PI
      double precision ax
      data pi/3.141592654/




        REAL C1(NKR,NKR)

       INTEGER ICE,KGRAN,IPRINT01
       REAL TWSIN,TWCIN,TWNUC,XF5,XF4,XF3,CONCHIN,CONCGIN,CONCSIN, &
     & CONCCLIN,TWHIN,RADH,RADS,RADG,RADL,CONCLIN,A1_MY,A2,A2_MY,XLK, &
     & A1N,A3_MY,A3,A1_MYN,R0CCN,X0DROP,DEG01,CONTCCNIN,CONCCCNIN, &
     & A,B,X0CCN,S_KR,RCCNKR,R0,X0,TWCALLIN,A1,RCCNKR_CM,SUMIIN,TWGIN, &
     & XF1N,XF1,WC1N,RF1N,WNUC,RNUC,WC5,RF5, &
     & WC4,RF4,WC3,RF3,WC1,RF1,SMAX
       REAL TWIIN(ICEMAX)
       REAL RO_SOLUTE      
       REAL A_FALL,B_FALL
       real graupel_fall(nkr)
       data graupel_fall/0.36840E-01,0.57471E-01,0.88417E-01,0.13999E+00,&
     &  0.22841E+00,0.36104E+00,0.56734E+00, 0.88417E+00, 0.13999E+01,&
     &  0.22104E+01, 0.35367E+01, 0.54524E+01, 0.81049E+01,0.12526E+02,&
     &  0.19157E+02, 0.27262E+02, 0.34627E+02, 0.39776E+02,0.45690E+02,& 
     &  0.52485E+02, 0.60289E+02, 0.69254E+02, 0.10000E+03, 0.15429E+03,&
     &  0.18561E+03, 0.22329E+03, 0.26863E+03,  0.32316E+03,0.38877E+03,& 
     &  0.46770E+03, 0.56266E+03, 0.67690E+03,  0.81432E+03/

       INTEGER  KZ_MIN,KZ_MAX
       PARAMETER (RO_SOLUTE=2.16)
       INTEGER KR_MIN,KR_MIN1,KR_MAX
       REAL RADCCN_MIN,RADCCN_MIN1,RADCCN_MAX
       REAL FR_CON,FR_MAR
     REAL  ::      RHOSU       
     REAL ::      RHOW        
     REAL ::      RHOI        
     REAL ::      RHOSN       
     REAL ::      RHOG        
     REAL ::      CI,DI,CS,DS,CG,DG 
       FR_MAR=1.0

       FR_CON=1.0






        call wrf_message(" FULL SBM: INITIALIZING HUCM ")
        call wrf_message(" FULL SBM: ****** HUCM ******* ")




        dlnr=dlog(2.d0)/(3.d0*scal)





        IF ( wrf_dm_on_monitor() ) THEN
          DO i = 31,99
            INQUIRE ( i , OPENED = opened )
            IF ( .NOT. opened ) THEN
              hujisbm_unit1 = i
              GOTO 2061
            ENDIF
          ENDDO
          hujisbm_unit1 = -1
 2061     CONTINUE
        ENDIF


        CALL wrf_dm_bcast_bytes ( hujisbm_unit1 , 4 )


        IF ( hujisbm_unit1 < 0 ) THEN
          CALL wrf_error_fatal3("<stdin>",2011,&
'module_mp_full: etanewinit: Can not find unused fortran unit to read in lookup table.' )
        ENDIF



        IF ( wrf_dm_on_monitor() ) THEN
          OPEN(UNIT=hujisbm_unit1,FILE="capacity.asc",                  &
     &        FORM="FORMATTED",STATUS="OLD",ERR=2070)

  900	FORMAT(6E13.5)
	READ(hujisbm_unit1,900) RLEC,RIEC,RSEC,RGEC,RHEC
	CLOSE(hujisbm_unit1)

        END IF
        CALL wrf_dm_bcast_bytes ( RLEC , size ( RLEC ) * 4 )
        CALL wrf_dm_bcast_bytes ( RIEC , size ( RIEC ) * 4 )
        CALL wrf_dm_bcast_bytes ( RSEC , size ( RSEC ) * 4 )
        CALL wrf_dm_bcast_bytes ( RGEC , size ( RGEC ) * 4 )
        CALL wrf_dm_bcast_bytes ( RHEC , size ( RHEC ) * 4 )

        IF ( wrf_dm_on_monitor() ) THEN
          DO i = 31,99
            INQUIRE ( i , OPENED = opened )
            IF ( .NOT. opened ) THEN
              hujisbm_unit1 = i
              GOTO 2062
            ENDIF
          ENDDO
          hujisbm_unit1 = -1
 2062     CONTINUE
        ENDIF

        CALL wrf_dm_bcast_bytes ( hujisbm_unit1 , 4 )

        IF ( hujisbm_unit1 < 0 ) THEN
          CALL wrf_error_fatal3("<stdin>",2047,&
'module_mp_full: etanewinit: Can not find unused fortran unit to read in lookup table.' )
        ENDIF

        IF ( wrf_dm_on_monitor() ) THEN
          OPEN(UNIT=hujisbm_unit1,FILE="masses.asc",                  &
     &        FORM="FORMATTED",STATUS="OLD",ERR=2070)
	READ(hujisbm_unit1,900) XL,XI,XS,XG,XH          
	CLOSE(hujisbm_unit1)

        call wrf_message(" FULL SBM: ****** file2: succesfull  ******* ")
        ENDIF
        CALL wrf_dm_bcast_bytes ( XL , size ( XL ) * 4 )
        CALL wrf_dm_bcast_bytes ( XI , size ( XI ) * 4 )
        CALL wrf_dm_bcast_bytes ( XS , size ( XS ) * 4 )
        CALL wrf_dm_bcast_bytes ( XG , size ( XG ) * 4 )
        CALL wrf_dm_bcast_bytes ( XH , size ( XH ) * 4 )

        IF ( wrf_dm_on_monitor() ) THEN
          DO i = 31,99
            INQUIRE ( i , OPENED = opened )
            IF ( .NOT. opened ) THEN
              hujisbm_unit1 = i
              GOTO 2063
            ENDIF
          ENDDO
          hujisbm_unit1 = -1
 2063     CONTINUE
        ENDIF

        CALL wrf_dm_bcast_bytes ( hujisbm_unit1 , 4 )

        IF ( hujisbm_unit1 < 0 ) THEN
          CALL wrf_error_fatal3("<stdin>",2080,&
'module_mp_full: etanewinit: Can not find unused fortran unit to read in lookup table.' )
        ENDIF

        IF ( wrf_dm_on_monitor() ) THEN
          OPEN(UNIT=hujisbm_unit1,FILE="termvels.asc",                  &
     &        FORM="FORMATTED",STATUS="OLD",ERR=2070)
	READ(hujisbm_unit1,900) VR1,VR2,VR3,VR4,VR5     
	CLOSE(hujisbm_unit1)

        call wrf_message(" FULL SBM: ****** file3: succesfull  ******* ")
        ENDIF
        CALL wrf_dm_bcast_bytes ( VR1 , size ( VR1 ) * 4 )
        CALL wrf_dm_bcast_bytes ( VR2 , size ( VR2 ) * 4 )
        CALL wrf_dm_bcast_bytes ( VR3 , size ( VR3 ) * 4 )
        CALL wrf_dm_bcast_bytes ( VR4 , size ( VR4 ) * 4 )
        CALL wrf_dm_bcast_bytes ( VR5 , size ( VR5 ) * 4 )

        DO KR=1,NKR


         if (kr.le.17)then
          A_FALL=1
          B_FALL=0
         else
          B_FALL=1
          A_FALL=0
         end if
  



        END DO
 

        IF ( wrf_dm_on_monitor() ) THEN
          DO i = 31,99
            INQUIRE ( i , OPENED = opened )
            IF ( .NOT. opened ) THEN
              hujisbm_unit1 = i
              GOTO 2065
            ENDIF
          ENDDO
          hujisbm_unit1 = -1
 2065     CONTINUE
        ENDIF

        CALL wrf_dm_bcast_bytes ( hujisbm_unit1 , 4 )

        IF ( hujisbm_unit1 < 0 ) THEN
          CALL wrf_error_fatal3("<stdin>",2130,&
'module_mp_full: etanewinit: Can not find unused fortran unit to read in lookup table.' )
        ENDIF

        IF ( wrf_dm_on_monitor() ) THEN
          OPEN(UNIT=hujisbm_unit1,FILE="constants.asc",                  &
     &        FORM="FORMATTED",STATUS="OLD",ERR=2070)
	READ(hujisbm_unit1,900) SLIC,TLIC,COEFIN,C2,C3,C4
	CLOSE(hujisbm_unit1)

        call wrf_message(" FULL SBM: ****** file4: succesfull  ******* ")
        END IF
        CALL wrf_dm_bcast_bytes ( SLIC , size ( SLIC ) * 4 )
        CALL wrf_dm_bcast_bytes ( TLIC , size ( TLIC ) * 4 )
        CALL wrf_dm_bcast_bytes ( COEFIN , size ( COEFIN ) * 4 )





        IF ( wrf_dm_on_monitor() ) THEN
          DO i = 31,99
            INQUIRE ( i , OPENED = opened )
            IF ( .NOT. opened ) THEN
              hujisbm_unit1 = i
              GOTO 2066
            ENDIF
          ENDDO
          hujisbm_unit1 = -1
 2066     CONTINUE
        ENDIF

        CALL wrf_dm_bcast_bytes ( hujisbm_unit1 , 4 )

        IF ( hujisbm_unit1 < 0 ) THEN
          CALL wrf_error_fatal3("<stdin>",2165,&
'module_mp_full: etanewinit: Can not find unused fortran unit to read in lookup table.' )
        ENDIF

        IF ( wrf_dm_on_monitor() ) THEN
          OPEN(UNIT=hujisbm_unit1,FILE="kernels_z.asc",                  &
     &        FORM="FORMATTED",STATUS="OLD",ERR=2070)
        READ(hujisbm_unit1,900)  &
     &  YWLL_1000MB,YWLL_750MB,YWLL_500MB
	CLOSE(hujisbm_unit1)
        END IF
        CALL wrf_dm_bcast_bytes ( YWLL_1000MB , size ( YWLL_1000MB ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWLL_750MB , size ( YWLL_750MB ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWLL_500MB , size ( YWLL_500MB ) * 4 )
        IF ( wrf_dm_on_monitor() ) THEN
          DO i = 31,99
            INQUIRE ( i , OPENED = opened )
            IF ( .NOT. opened ) THEN
              hujisbm_unit1 = i
              GOTO 2067
            ENDIF
          ENDDO
          hujisbm_unit1 = -1
 2067     CONTINUE
        ENDIF

        CALL wrf_dm_bcast_bytes ( hujisbm_unit1 , 4 )

        IF ( hujisbm_unit1 < 0 ) THEN
          CALL wrf_error_fatal3("<stdin>",2194,&
'module_mp_full: etanewinit: Can not find unused fortran unit to read in lookup table.' )
        ENDIF

        IF ( wrf_dm_on_monitor() ) THEN
          OPEN(UNIT=hujisbm_unit1,FILE="kernels.asc_s_0_03_0_9",                  &
     &        FORM="FORMATTED",STATUS="OLD",ERR=2070)

	READ(hujisbm_unit1,900) &
     &  YWLL,YWLI,YWLS,YWLG,YWLH, &
     &  YWIL,YWII,YWIS,YWIG,YWIH, &
     &  YWSL,YWSI,YWSS,YWSG,YWSH, &
     &  YWGL,YWGI,YWGS,YWGG,YWGH, &
     &  YWHL,YWHI,YWHS,YWHG,YWHH
       close (hujisbm_unit1)
        END IF
        CALL wrf_dm_bcast_bytes ( YWLL , size ( YWLL ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWLI , size ( YWLI ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWLS , size ( YWLS ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWLG , size ( YWLG ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWLH , size ( YWLH ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWIL , size ( YWIL ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWII , size ( YWII ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWIS , size ( YWIS ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWIG , size ( YWIG ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWIH , size ( YWIH ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWSL , size ( YWSL ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWSI , size ( YWSI ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWSS , size ( YWSS ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWSG , size ( YWSG ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWSH , size ( YWSH ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWGL , size ( YWGL ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWGI , size ( YWGI ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWGS , size ( YWGS ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWGG , size ( YWGG ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWGH , size ( YWGH ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWHL , size ( YWHL ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWHI , size ( YWHI ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWHS , size ( YWHS ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWHG , size ( YWHG ) * 4 )
        CALL wrf_dm_bcast_bytes ( YWHH , size ( YWHH ) * 4 )

        IF ( wrf_dm_on_monitor() ) THEN
          DO i = 31,99
            INQUIRE ( i , OPENED = opened )
            IF ( .NOT. opened ) THEN
              hujisbm_unit1 = i
              GOTO 2068
            ENDIF
          ENDDO
          hujisbm_unit1 = -1
 2068     CONTINUE
        ENDIF

        CALL wrf_dm_bcast_bytes ( hujisbm_unit1 , 4 )

        IF ( hujisbm_unit1 < 0 ) THEN
          CALL wrf_error_fatal3("<stdin>",2251,&
'module_mp_full: etanewinit: Can not find unused fortran unit to read in lookup table.' )
        ENDIF

        IF ( wrf_dm_on_monitor() ) THEN
          OPEN(UNIT=hujisbm_unit1,FILE="bulkdens.asc_s_0_03_0_9",         & 
     &        FORM="FORMATTED",STATUS="OLD",ERR=2070)
	READ(hujisbm_unit1,900) RO1BL,RO2BL,RO3BL,RO4BL,RO5BL
	CLOSE(hujisbm_unit1)

        call wrf_message(" FULL SBM: ****** file6: succesfull  ******* ")
        END IF
        CALL wrf_dm_bcast_bytes (RO1BL  , size ( RO1BL ) * 4 )
        CALL wrf_dm_bcast_bytes (RO2BL  , size ( RO2BL ) * 4 )
        CALL wrf_dm_bcast_bytes (RO3BL  , size ( RO3BL ) * 4 )
        CALL wrf_dm_bcast_bytes (RO4BL  , size ( RO4BL ) * 4 )
        CALL wrf_dm_bcast_bytes (RO5BL  , size ( RO5BL ) * 4 )

        IF ( wrf_dm_on_monitor() ) THEN
          DO i = 31,99
            INQUIRE ( i , OPENED = opened )
            IF ( .NOT. opened ) THEN
              hujisbm_unit1 = i
              GOTO 2069
            ENDIF
          ENDDO
          hujisbm_unit1 = -1
 2069     CONTINUE
        ENDIF

        CALL wrf_dm_bcast_bytes ( hujisbm_unit1 , 4 )

        IF ( hujisbm_unit1 < 0 ) THEN
          CALL wrf_error_fatal3("<stdin>",2284,&
'module_mp_full: etanewinit: Can not find unused fortran unit to read in lookup table.' )
        ENDIF

        IF ( wrf_dm_on_monitor() ) THEN
          OPEN(UNIT=hujisbm_unit1,FILE="bulkradii.asc_s_0_03_0_9",         & 
     &        FORM="FORMATTED",STATUS="OLD",ERR=2070)
	READ(hujisbm_unit1,*) RADXXO
	CLOSE(hujisbm_unit1)

        call wrf_message(" FULL SBM: ****** file7: succesfull  ******* ")

        call wrf_message(" FULL SBM: Hebrew Univ Cloud model-HUCM ")

        END IF
        CALL wrf_dm_bcast_bytes (RADXXO  , size ( RADXXO ) * 4 )

        ax=2.d0**(1.0/scal)
        xl_mg(1)=0.3351d-7
	do i=2,nkr
           xl_mg(i)=ax*xl_mg(i-1)

        enddo
	do i=1,nkr
           xs_mg(i)=xs(i)*1.e3
           xg_mg(i)=xg(i)*1.e3
           xh_mg(i)=xh(i)*1.e3
           xi1_mg(i)=xi(i,1)*1.e3
           xi2_mg(i)=xi(i,2)*1.e3
           xi3_mg(i)=xi(i,3)*1.e3
        enddo



        call courant_bott

 

	DEG01=1./3.




	X0DROP=XL(ICCN)

	X0CCN =X0DROP/(2.**(NKR-1))
	R0CCN =(3.*X0CCN/4./3.141593/ROCCN0)**DEG01




        A=3.3E-05/288.15
        B=2.*4.3/(22.9+35.5)
        B=B*(4./3.)*3.14*RO_SOLUTE
        A1=2.*(A/3.)**1.5/SQRT(B)
        A2=A1*100.

	CONCCCNIN=0.
	CONTCCNIN=0.
	DO KR=1,NKR
           DROPRADII(KR)=(3.*XL(KR)/4./3.141593/1.)**DEG01
        ENDDO
	DO KR=1,NKR



	   ROCCN(KR)=ROCCN0
	   X0=X0CCN*2.**(KR-1)
	   R0=(3.*X0/4./3.141593/ROCCN(KR))**DEG01
	   XCCN(KR)=X0
	   RCCN(KR)=R0

           RCCNKR_CM=R0


           S_KR=A2/RCCNKR_CM**1.5
           ACCN=ACCN_CON
           BCCN=BCCN_CON


           FCCNR(KR)=1.5*ACCN*BCCN*S_KR**BCCN
           FCCNR_CON(KR)=FCCNR(KR)

           ACCN=ACCN_MAR
           BCCN=BCCN_MAR
           FCCNR(KR)=1.5*ACCN*BCCN*S_KR**BCCN
           FCCNR_MAR(KR)=FCCNR(KR)

	     CONTCCNIN=CONTCCNIN+COL*FCCNR(KR)*R0*R0*R0
             CONCCCNIN=CONCCCNIN+COL*FCCNR(KR)
	ENDDO










        RADCCN_MAX=RCCN(NKR)
        RADCCN_MIN=0.005E-4         
        RADCCN_MIN1=0.02E-4         





        KR_MIN=1.+ 3*(ALOG(RADCCN_MIN)- ALOG(R0CCN))/ALOG(2.)
        KR_MIN1=1.+3*(ALOG(RADCCN_MIN1)- ALOG(R0CCN))/ALOG(2.)

        KR_MAX=1.+3.*(ALOG(RADCCN_MAX)- ALOG(R0CCN))/ALOG(2.)
        KR_MIN=MAX(KR_MIN,1)
        KR_MIN1=MAX(KR_MIN,KR_MIN1)
        KR_MAX=MIN(NKR,KR_MAX)



        DO KR=1,NKR
        IF (kr.ge.kr_min.and.kr.lt.kr_min1)then
           FCCNR_MAR(KR)=FCCNR_MAR(KR_MIN1)* &
     &      (ALOG(RCCN(KR))-ALOG(RCCN(KR_MIN)))/ &
     &      (ALOG(RCCN(KR_MIN1))-ALOG(RCCN(KR_MIN)))

        END IF
        IF (KR.GT.KR_MAX.OR.KR.LT.KR_MIN)FCCNR_MAR(KR)=0

        END DO

        RADCCN_MAX=0.6E-4
        RADCCN_MIN=0.005E-4         
        RADCCN_MIN1=0.02E-4         


        KR_MIN=1.+ 3*(ALOG(RADCCN_MIN)- ALOG(R0CCN))/ALOG(2.)
        KR_MIN1=1.+3*(ALOG(RADCCN_MIN1)- ALOG(R0CCN))/ALOG(2.)

        KR_MAX=1.+3.*(ALOG(RADCCN_MAX)- ALOG(R0CCN))/ALOG(2.)
        KR_MIN=MAX(KR_MIN,1)
        KR_MIN1=MAX(KR_MIN,KR_MIN1)
        KR_MAX=MIN(NKR,KR_MAX)



        DO KR=1,NKR
        IF (kr.ge.kr_min.and.kr.lt.kr_min1)then
           FCCNR_CON(KR)=FCCNR_CON(KR_MIN1)* &
     &      (ALOG(RCCN(KR))-ALOG(RCCN(KR_MIN)))/ &
     &      (ALOG(RCCN(KR_MIN1))-ALOG(RCCN(KR_MIN)))
        END IF
        IF (KR.GT.KR_MAX.OR.KR.LT.KR_MIN)FCCNR_CON(KR)=0

        END DO

        DO KR=1,NKR
         FCCNR_MIX(KR)=FR_CON*FCCNR_CON(KR)+FR_MAR*FCCNR_MAR(KR)

        END DO

         CALL BREAKINIT





  100	FORMAT(10I4)
  101   FORMAT(3X,F7.5,E13.5)
  102	FORMAT(4E12.4)
  105	FORMAT(A48)
  106	FORMAT(A80)
  123	FORMAT(3E12.4,3I4)
  200	FORMAT(6E13.5)
  201   FORMAT(6D13.5)
  300	FORMAT(8E14.6) 
  301   FORMAT(3X,F8.3,3X,E13.5)
  302   FORMAT(5E13.5)




        call kernals(dt)






         RHOW = 997.
         RHOI = 500.
         RHOSN = 100.





         RHOG=450


         CI = RHOI*PI_MORR/6.
         DI = 3.
         CS = RHOSN*PI_MORR/6.
         DS = 3.
         CG = RHOG*PI_MORR/6.
         DG = 3.


         xam_r = PI_MORR*RHOW/6.
         xbm_r = 3.
         xmu_r = 0.
         xam_s = CS
         xbm_s = DS
         xmu_s = 0.
         xam_g = CG
         xbm_g = DG
         xmu_g = 0.

         call radar_init


        return
2070  continue
      WRITE( errmess , '(A,I4)' )                                        &
       'module_mp_full_sbm: error opening hujisbm_DATA on unit '          &
     &, hujisbm_unit1
      CALL wrf_error_fatal3("<stdin>",2509,&
errmess)
        end  subroutine full_hucminit
      SUBROUTINE BREAKINIT
      IMPLICIT NONE
      INTEGER :: hujisbm_unit1
      LOGICAL, PARAMETER :: PRINT_diag=.FALSE.
      LOGICAL :: opened 
      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
      CHARACTER*80 errmess









      INTEGER AP,IE,JE,KE

      PARAMETER (AP = 1)

      INTEGER I,J,K,JDIFF
      REAL  RPKIJ(JBREAK,JBREAK,JBREAK),RQKJ(JBREAK,JBREAK)


      REAL PI,D0,HLP
      DOUBLE PRECISION M(0:JBREAK),ALM
      REAL DBREAK(JBREAK),GAIN,LOSS






      INTEGER IP,KP,JP,KQ,JQ
      REAL XTJ

      CHARACTER*20 FILENAME_P,FILENAME_Q

      FILENAME_P = 'coeff_p.asc'
      FILENAME_Q = 'coeff_q.asc'

      IE = JBREAK
      JE = JBREAK
      KE = JBREAK
      PI    = 3.1415927
      D0    = 0.0101593
      M(1)  = PI/6.0 * D0**3






      JDIFF = JMAX - JBREAK







         ALM  = 2.d0
         M(0)  = M(1)/ALM
         DO K=1,KE-1
            M(K+1) = M(K)*ALM
         ENDDO
         DO K=1,KE
            BRKWEIGHT(K) = 2./(M(K)**2 - M(K-1)**2)



         ENDDO



         WRITE (*,*) 'COLL_BREAKUP_INI: COAGULATION AND BREAKUP GRID'
         WRITE (*,'(2A5,5A15)') 'ICOAG','IBREAK', &
     &        'XCOAG','DCOAG', &
     &        'XBREAK','DBREAK','MWEIGHT'





        IF ( wrf_dm_on_monitor() ) THEN
          DO i = 31,99
            INQUIRE ( i , OPENED = opened )
            IF ( .NOT. opened ) THEN
              hujisbm_unit1 = i
              GOTO 2061
            ENDIF
          ENDDO
          hujisbm_unit1 = -1
 2061     CONTINUE
        ENDIF

        CALL wrf_dm_bcast_bytes ( hujisbm_unit1 , 4 )

        IF ( hujisbm_unit1 < 0 ) THEN
          CALL wrf_error_fatal3("<stdin>",2611,&
'module_mp_full: etanewinit: Can not find unused fortran unit to read in lookup table.' )
        ENDIF

        IF ( wrf_dm_on_monitor() ) THEN
          OPEN(UNIT=hujisbm_unit1,FILE="coeff_p.asc",                  &
     &        FORM="FORMATTED",STATUS="OLD",ERR=2070)


         DO K=1,KE
            DO I=1,IE
               DO J=1,I
                  READ(hujisbm_unit1,'(3I6,1E16.8)') KP,IP,JP,PKIJ(KP,IP,JP)










               ENDDO
            ENDDO

         ENDDO
	CLOSE(hujisbm_unit1)

        END IF
        CALL wrf_dm_bcast_bytes (PKIJ  , size ( PKIJ ) * 8 )
        IF ( wrf_dm_on_monitor() ) THEN
          DO i = 31,99
            INQUIRE ( i , OPENED = opened )
            IF ( .NOT. opened ) THEN
              hujisbm_unit1 = i
              GOTO 2062
            ENDIF
          ENDDO
          hujisbm_unit1 = -1
 2062     CONTINUE
        ENDIF

        CALL wrf_dm_bcast_bytes ( hujisbm_unit1 , 4 )

        IF ( hujisbm_unit1 < 0 ) THEN
          CALL wrf_error_fatal3("<stdin>",2657,&
'module_mp_full: etanewinit: Can not find unused fortran unit to read in lookup table.' )
        ENDIF

        IF ( wrf_dm_on_monitor() ) THEN
          OPEN(UNIT=hujisbm_unit1,FILE="coeff_q.asc",                  &
     &        FORM="FORMATTED",STATUS="OLD",ERR=2070)

         DO K=1,KE
            DO J=1,JE
               READ(hujisbm_unit1,'(2I6,1E16.8)') KQ,JQ,QKJ(KQ,JQ)



            ENDDO
         ENDDO
         CLOSE(hujisbm_unit1)

         WRITE (*,*) 'COLL_BREAKUP READ: ... OK'
         END IF
        CALL wrf_dm_bcast_bytes (QKJ  , size ( QKJ ) * 8 )














      DO I=1,JMAX
         DO J=1,JMAX
              ECOALMASSM(I,J)=1.0D0
         ENDDO
      ENDDO

      DO I=1,JMAX
         DO J=1,JMAX
           ECOALMASSM(I,J)=ECOALMASS(XL(I),XL(J))
         ENDDO
      ENDDO
      RETURN
2070  continue
      WRITE( errmess , '(A,I4)' )                                        &
       'module_mp_full: error opening hujisbm_DATA on unit '          &
     &, hujisbm_unit1
      CALL wrf_error_fatal3("<stdin>",2708,&
errmess)
      END SUBROUTINE BREAKINIT

      REAL FUNCTION ECOALMASS(ETA,KSI)
      IMPLICIT NONE

      REAL PI
      PARAMETER (PI = 3.1415927)

      REAL ETA,KSI
      REAL KPI,RHO
      REAL DETA,DKSI

      PARAMETER (RHO  = 1.0)




      KPI = 6./PI

      DETA = (KPI*ETA/RHO)**(1./3.)
      DKSI = (KPI*KSI/RHO)**(1./3.)

      ECOALMASS = ECOALDIAM(DETA,DKSI)

      RETURN
      END FUNCTION ECOALMASS






      REAL FUNCTION ECOALDIAM(DETA,DKSI)


      INTEGER N
      REAL DETA,DKSI
      REAL DGR,DKL,RGR,RKL,P,Q,E,X,Y,QMIN,QMAX
      REAL ZERO,ONE,EPS,PI

      PARAMETER (ZERO = 0.0)
      PARAMETER (ONE  = 1.0)
      PARAMETER (EPS  = 1.0E-30)
      PARAMETER (PI   = 3.1415927)




      DGR = MAX(DETA,DKSI)
      DKL = MIN(DETA,DKSI)

      RGR = 0.5*DGR
      RKL = 0.5*DKL

      P = (RKL / RGR)
      Q = (RKL * RGR)**0.5
      Q = 0.5 * (RKL + RGR)

      qmin = 250e-4
      qmax = 400e-4        
      if (q.lt.qmin) then
         e = max(ecoalOchs(Dgr,Dkl),ecoalBeard(Dgr,Dkl)) 
      elseif (q.ge.qmin.and.q.lt.qmax) then
         x = (q - qmin) / (qmax - qmin)
         e = sin(pi/2.0*x)**2 * ecoalLowList(Dgr,Dkl) &
     &     + sin(pi/2.0*(1 - x))**2 * ecoalOchs(Dgr,Dkl)
      elseif (q.ge.qmax) then
         e = ecoalLowList(Dgr,Dkl)
      else
         e  = 1.0
      endif

      ECOALDIAM  = MAX(MIN(ONE,E),EPS)

      RETURN
      END FUNCTION  ECOALDIAM





      REAL FUNCTION ECOALLOWLIST(DGR,DKL)
      IMPLICIT NONE

      REAL PI,SIGMA,KA,KB,EPSI
      REAL DGR,DKL,RGR,RKL,X
      REAL ST,SC,ET,DSTSC,CKE,W1,W2,DC,ECL
      REAL QQ0,QQ1,QQ2

      PARAMETER (EPSI=1.E-20)

      PI = 3.1415927
      SIGMA = 72.8
      KA = 0.778
      KB = 2.61E-4

      RGR = 0.5*DGR
      RKL = 0.5*DKL

      CALL COLLENERGY(DGR,DKL,CKE,ST,SC,W1,W2,DC)

      DSTSC = ST-SC
      ET = CKE+DSTSC
      IF (ET .LT. 50.0) THEN
         QQ0=1.0+(DKL/DGR)
         QQ1=KA/QQ0**2
         QQ2=KB*SIGMA*(ET**2)/(SC+EPSI)
         ECL=QQ1*EXP(-QQ2)
      ELSE
         ECL=0.0
      ENDIF

      ECOALLOWLIST = ECL

      RETURN
      END FUNCTION ECOALLOWLIST





      REAL FUNCTION ECOALOCHS(D_L,D_S)
      IMPLICIT NONE

      REAL D_L,D_S
      REAL PI,SIGMA,N_W,R_S,R_L,DV,P,G,X,E

      REAL EPSF,FPMIN


      PARAMETER (EPSF  = 1.E-30)
      PARAMETER (FPMIN = 1.E-30)

      PI = 3.1415927
      SIGMA = 72.8

      R_S = 0.5 * D_S
      R_L = 0.5 * D_L
      P   = R_S / R_L

      DV  = ABS(VTBEARD(D_L) - VTBEARD(D_S))
      IF (DV.LT.FPMIN) DV = FPMIN
      N_W = R_S * DV**2 / SIGMA
      G   = 2**(3./2.)/(6.*PI) * P**4 * (1.+ P) / ((1.+P**2)*(1.+P**3))
      X   = N_W**(0.5) * G
      E   = 0.767 - 10.14 * X

      ECOALOCHS = E

      RETURN
      END FUNCTION ECOALOCHS





      SUBROUTINE COLLENERGY(DGR,DKL,CKE,ST,SC,W1,W2,DC)


      REAL DGR,DKL,DC
      REAL K10,PI,SIGMA,RHO
      REAL CKE,W1,W2,ST,SC
      REAL DGKA3,DGKB3,DGKA2
      REAL V1,V2,DV

      REAL EPSF,FPMIN


      PARAMETER (EPSF  = 1.E-30)
      PARAMETER (FPMIN = 1.E-30)

      PI    = 3.1415927
      RHO   = 1.0
      SIGMA = 72.8

      K10=RHO*PI/12.0D0

      DGR = MAX(DGR,EPSF)
      DKL = MAX(DKL,EPSF)

      DGKA2=(DGR**2)+(DKL**2)

      DGKA3=(DGR**3)+(DKL**3)

      IF (DGR.NE.DKL) THEN
         V1 = VTBEARD(DGR)
         V2 = VTBEARD(DKL)
         DV = (V1-V2)
         IF (DV.LT.FPMIN) DV = FPMIN
         DV = DV**2
         IF (DV.LT.FPMIN) DV = FPMIN
         DGKB3=(DGR**3)*(DKL**3)
         CKE = K10 * DV * DGKB3/DGKA3
      ELSE
         CKE = 0.0D0
      ENDIF
      ST = PI*SIGMA*DGKA2
      SC = PI*SIGMA*DGKA3**(2./3.)

      W1=CKE/(SC+EPSF)
      W2=CKE/(ST+EPSF)

      DC=DGKA3**(1./3.)

      RETURN
      END SUBROUTINE COLLENERGY





      REAL FUNCTION VTBEARD(DIAM)
      IMPLICIT NONE


      REAL DIAM,AA
      REAL ROP,RU,AMT,PP,RL,TT,ETA,DENS,CD,D,A
      REAL ALA,GR,SI,BOND,PART,XX,YY,RE,VT
      REAL B00,B11,B22,B33,B44,B55,B0,B1,B2,B3,B4,B5,B6
      INTEGER ID

      DATA B00,B11,B22,B33,B44,B55,B0,B1,B2,B3,B4,B5,B6/-5.00015, &
     &5.23778,-2.04914,.475294,-.0542819,.00238449,-3.18657,.992696, &
     &-.153193E-2,-.987059E-3,-.578878E-3,.855176E-4,-.327815E-5/

      AA   = DIAM/2.0
      ROP  = 1.0
      RU   = 8.3144E+7
      AMT  = 28.9644
      ID   = 10000
      PP   = FLOAT(ID)*100.
      RL   = RU/AMT
      TT   = 283.15
      ETA  = (1.718+.0049*(TT-273.15))*1.E-4
      DENS = PP/TT/RL
      ALA  = 6.6E-6*1.01325E+6/PP*TT/293.15
      GR   = 979.69
      SI   = 76.1-.155*(TT-273.15)

      IF (AA.GT.500.E-4) THEN
         BOND = GR*(ROP-DENS)*AA*AA/SI
         PART = (SI**3*DENS*DENS/(ETA**4*GR*(ROP-DENS)))**(1./6.)
         XX = LOG(16./3.*BOND*PART)
         YY = B00+B11*XX+B22*XX*XX+B33*XX**3+B44*XX**4+B55*XX**5
         RE = PART*EXP(YY)
         VT = ETA*RE/2./DENS/AA
      ELSEIF (AA.GT.1.E-3) THEN
         CD = 32.*AA*AA*AA*(ROP-DENS)*DENS*GR/3./ETA/ETA
         XX = LOG(CD)
         RE = EXP(B0+B1*XX+B2*XX*XX+B3*XX**3+B4*XX**4+B5*XX**5+B6*XX**6)
         D  = CD/RE/24.-1.
         VT = ETA*RE/2./DENS/AA
      ELSE
         A  = 1.+1.26*ALA/AA
         A  = A*2.*AA*AA*GR*(ROP-DENS)/9./ETA
         CD = 12*ETA/A/AA/DENS
         VT = A
      ENDIF

      VTBEARD = VT

      RETURN
      END FUNCTION VTBEARD


      




 
      REAL FUNCTION ecoalBeard(D_l,D_s) 
       
      IMPLICIT NONE 


      REAL            D_l,D_s
      REAL            R_s,R_l
      REAL            rcoeff
      REAL epsf
      PARAMETER (epsf  = 1.e-30) 

      INTEGER its
      COMPLEX acoeff(4),x

      R_s = 0.5 * D_s
      R_l = 0.5 * D_l      

      rcoeff = 5.07 - log(R_s*1e4) - log(R_l*1e4/200.0)

      acoeff(1) = CMPLX(rcoeff)
      acoeff(2) = CMPLX(-5.94)
      acoeff(3) = CMPLX(+7.27)
      acoeff(4) = CMPLX(-5.29)

      x = (0.50,0)

      CALL LAGUER(acoeff,3,x,its)

      EcoalBeard = REAL(x)

      RETURN 
      END FUNCTION ecoalBeard 



      SUBROUTINE laguer(a,m,x,its)
      INTEGER m,its,MAXIT,MR,MT
      REAL EPSS
      COMPLEX a(m+1),x
      PARAMETER (EPSS=2.e-7,MR=8,MT=10,MAXIT=MT*MR)
      INTEGER iter,j
      REAL abx,abp,abm,err,frac(MR)
      COMPLEX dx,x1,b,d,f,g,h,sq,gp,gm,g2
      SAVE frac
      DATA frac /.5,.25,.75,.13,.38,.62,.88,1./
      do 12 iter=1,MAXIT
        its=iter
        b=a(m+1)
        err=abs(b)
        d=cmplx(0.,0.)
        f=cmplx(0.,0.)
        abx=abs(x)
        do 11 j=m,1,-1
          f=x*f+d
          d=x*d+b
          b=x*b+a(j)
          err=abs(b)+abx*err
11      continue
        err=EPSS*err
        if(abs(b).le.err) then
          return
        else
          g=d/b
          g2=g*g
          h=g2-2.*f/b
          sq=sqrt((m-1)*(m*h-g2))
          gp=g+sq
          gm=g-sq
          abp=abs(gp)
          abm=abs(gm)
          if(abp.lt.abm) gp=gm
          if (max(abp,abm).gt.0.) then
            dx=m/gp
          else
            dx=exp(cmplx(log(1.+abx),float(iter)))
          endif
        endif
        x1=x-dx
        if(x.eq.x1)return
        if (mod(iter,MT).ne.0) then
          x=x1
        else
          x=x-dx*frac(iter/MT)
        endif
12    continue
      pause 'too many iterations in laguer'
      return
      END SUBROUTINE laguer




      subroutine courant_bott
      implicit none
      integer k,kk,j,i
      double precision x0








      xl_mg(0)=xl_mg(1)/2

      do i=1,nkr
         do j=i,nkr
            x0=xl_mg(i)+xl_mg(j)
            do k=j,nkr
               kk=k







               if(xl_mg(k).ge.x0.and.xl_mg(k-1).lt.x0) then
                 chucm(i,j)=dlog(x0/xl_mg(k-1))/(3.d0*dlnr)
 102             continue
                 if(chucm(i,j).gt.1.-1.d-08) then
                   chucm(i,j)=0.
                   kk=kk+1
                 endif
                 ima(i,j)=min(nkr-1,kk-1)

                 goto 2000
               endif
            enddo
 2000       continue

            chucm(j,i)=chucm(i,j)
            ima(j,i)=ima(i,j)
         enddo
      enddo
      return
      end subroutine courant_bott


      SUBROUTINE KERNALS(DTIME)

      IMPLICIT NONE
      INTEGER I,J
      REAL PI

      data pi/3.141592654/





        REAL DTIME






        DO I=1,NKR
           DO J=1,NKR
              CWLL_1000MB(I,J)=DTIME*DLNR*YWLL_1000MB(I,J)
              CWLL_750MB(I,J)=DTIME*DLNR*YWLL_750MB(I,J)
              CWLL_500MB(I,J)=DTIME*DLNR*YWLL_500MB(I,J)

              CWLL(I,J)=DTIME*DLNR*YWLL(I,J)
              CWLG(I,J)=DTIME*DLNR*YWLG(I,J)
              CWLH(I,J)=DTIME*DLNR*YWLH(I,J)


              if (i.le.16.and.j.le.16)then
              CWSL(I,J)=0.d0

              CWSL(i,j)=DTIME*DLNR*YWIL(I,J,2)
              CWLS(I,J)=0.d0

              CWLS(I,J)=DTIME*DLNR*YWLI(I,J,2)
              else
              CWSL(I,J)=DTIME*DLNR*YWSL(I,J)
              CWLS(I,J)=DTIME*DLNR*YWLS(I,J)
              end if
              CWSS(I,J)=DTIME*DLNR*YWSS(I,J)
              CWSG(I,J)=DTIME*DLNR*YWSG(I,J)
              CWSH(I,J)=DTIME*DLNR*YWSH(I,J)

              CWGL(I,J)=0.8*DTIME*DLNR*YWGL(I,J)
              IF(RADXXO(I,6).LT.2.0D-2) THEN
                IF(RADXXO(J,1).LT.1.0D-3) THEN
                  IF(RADXXO(J,1).GE.7.0D-4) THEN
                    CWGL(I,J)=DTIME*DLNR*YWGL(I,J)/1.5D0
                  ELSE
                    CWGL(I,J)=DTIME*DLNR*YWGL(I,J)/3.0D0
                  ENDIF
                ENDIF
              ENDIF
              IF(I.LE.14.AND.J.LE.7) CWGL(I,J)=0.0D0


              CWGS(I,J)=DTIME*DLNR*YWGS(I,J)
              CWGG(I,J)=DTIME*DLNR*YWGG(I,J)
              CWGH(I,J)=DTIME*DLNR*YWGH(I,J)

              CWHL(I,J)=DTIME*DLNR*YWHL(I,J)
              CWHS(I,J)=DTIME*DLNR*YWHS(I,J)
              CWHG(I,J)=DTIME*DLNR*YWHG(I,J)
              CWHH(I,J)=DTIME*DLNR*YWHH(I,J)

              CWLI_1(I,J)=DTIME*DLNR*YWLI(I,J,1)
              CWLI_2(I,J)=DTIME*DLNR*YWLI(I,J,2)
              CWLI_3(I,J)=DTIME*DLNR*YWLI(I,J,3)
              
              CWIL_1(I,J)=DTIME*DLNR*YWIL(I,J,1)
              CWIL_2(I,J)=DTIME*DLNR*YWIL(I,J,2)
              CWIL_3(I,J)=DTIME*DLNR*YWIL(I,J,3)

              CWIS_1(I,J)=DTIME*DLNR*YWIS(I,J,1)
              CWIS_2(I,J)=DTIME*DLNR*YWIS(I,J,2)
              CWIS_3(I,J)=DTIME*DLNR*YWIS(I,J,3)

              CWSI_1(I,J)=DTIME*DLNR*YWSI(I,J,1)
              CWSI_2(I,J)=DTIME*DLNR*YWSI(I,J,2)
              CWSI_3(I,J)=DTIME*DLNR*YWSI(I,J,3)

              CWIG_1(I,J)=DTIME*DLNR*YWIG(I,J,1)
              CWIG_2(I,J)=DTIME*DLNR*YWIG(I,J,2)
              CWIG_3(I,J)=DTIME*DLNR*YWIG(I,J,3)

              CWGI_1(I,J)=DTIME*DLNR*YWGI(I,J,1)
              CWGI_2(I,J)=DTIME*DLNR*YWGI(I,J,2)
              CWGI_3(I,J)=DTIME*DLNR*YWGI(I,J,3)

              CWIH_1(I,J)=DTIME*DLNR*YWIH(I,J,1)
              CWIH_2(I,J)=DTIME*DLNR*YWIH(I,J,2)
              CWIH_3(I,J)=DTIME*DLNR*YWIH(I,J,3)

              CWHI_1(I,J)=DTIME*DLNR*YWHI(I,J,1)
              CWHI_2(I,J)=DTIME*DLNR*YWHI(I,J,2)
              CWHI_3(I,J)=DTIME*DLNR*YWHI(I,J,3)

              if (i.lt.12.and.j.lt.12)then

               CWII_1_1(I,J)=0.D0
               CWII_1_2(I,J)=0.D0
               CWII_1_3(I,J)=0.D0

               CWII_2_1(I,J)=0.D0
               CWII_2_2(I,J)=0.D0
               CWII_2_3(I,J)=0.D0

               CWII_3_1(I,J)=0.D0
               CWII_3_2(I,J)=0.D0
               CWII_3_3(I,J)=0.D0

              else
               CWII_1_1(I,J)=DTIME*DLNR*YWII(I,J,1,1)
               CWII_1_2(I,J)=DTIME*DLNR*YWII(I,J,1,2)
               CWII_1_3(I,J)=DTIME*DLNR*YWII(I,J,1,3)

               CWII_2_1(I,J)=DTIME*DLNR*YWII(I,J,2,1)
               CWII_2_2(I,J)=DTIME*DLNR*YWII(I,J,2,2)
               CWII_2_3(I,J)=DTIME*DLNR*YWII(I,J,2,3)

               CWII_3_1(I,J)=DTIME*DLNR*YWII(I,J,3,1)
               CWII_3_2(I,J)=DTIME*DLNR*YWII(I,J,3,2)
               CWII_3_3(I,J)=DTIME*DLNR*YWII(I,J,3,3)
              end if
           ENDDO
        ENDDO


        CALL TURBCOEF
        DO J=1,7
           DO I=15,24-J
              CWGL(I,J)=0.0D0
           ENDDO
        ENDDO


        DO I=1,NKR
           DO J=1,NKR
              CWLG(J,I)=CWGL(I,J)
           ENDDO
        ENDDO

          DO I=KRMING_GL,KRMAXG_GL
             DO J=KRMINL_GL,KRMAXL_GL
               IF (ICETURB.EQ.1)THEN
                CWGL(I,J)=CTURBGL(I,J)*CWGL(I,J)
               ELSE
                CWGL(I,J)=CWGL(I,J)
               END IF
             ENDDO
          ENDDO
          DO I=KRMING_GL,KRMAXG_GL
             DO J=KRMINL_GL,KRMAXL_GL
                CWLG(J,I)=CWGL(I,J)
             ENDDO
          ENDDO

88     CONTINUE
	RETURN
	END SUBROUTINE KERNALS

      SUBROUTINE KERNALS_IN(DTIME)

      IMPLICIT NONE
      INTEGER I,J
      REAL PI

      data pi/3.141592654/





        REAL DTIME






        DO I=1,NKR
           DO J=1,NKR
              CWLL_1000MB(I,J)=DTIME*DLNR*YWLL_1000MB(I,J)
              CWLL_750MB(I,J)=DTIME*DLNR*YWLL_750MB(I,J)
              CWLL_500MB(I,J)=DTIME*DLNR*YWLL_500MB(I,J)

              CWLL(I,J)=DTIME*DLNR*YWLL(I,J)
              CWLG(I,J)=DTIME*DLNR*YWLG(I,J)



              if (i.le.16.and.j.le.16)then
              CWSL(I,J)=0.d0

              CWSL(i,j)=DTIME*DLNR*YWIL(I,J,2)
              CWLS(I,J)=0.d0

              CWLS(I,J)=DTIME*DLNR*YWLI(I,J,2)
              else
              CWSL(I,J)=DTIME*DLNR*YWSL(I,J)
              CWLS(I,J)=DTIME*DLNR*YWLS(I,J)
              end if
              CWSS(I,J)=DTIME*DLNR*YWSS(I,J)
              CWSG(I,J)=DTIME*DLNR*YWSG(I,J)


              CWGL(I,J)=0.8*DTIME*DLNR*YWGL(I,J)
              IF(RADXXO(I,6).LT.2.0D-2) THEN
                IF(RADXXO(J,1).LT.1.0D-3) THEN
                  IF(RADXXO(J,1).GE.7.0D-4) THEN
                    CWGL(I,J)=DTIME*DLNR*YWGL(I,J)/1.5D0
                  ELSE
                    CWGL(I,J)=DTIME*DLNR*YWGL(I,J)/3.0D0
                  ENDIF
                ENDIF
              ENDIF
              IF(I.LE.14.AND.J.LE.7) CWGL(I,J)=0.0D0


              CWGS(I,J)=DTIME*DLNR*YWGS(I,J)
              CWGG(I,J)=DTIME*DLNR*YWGG(I,J)










              




























              if (i.lt.12.and.j.lt.12)then













              else











              end if
           ENDDO
        ENDDO


        CALL TURBCOEF
        DO J=1,7
           DO I=15,24-J
              CWGL(I,J)=0.0D0
           ENDDO
        ENDDO


        DO I=1,NKR
           DO J=1,NKR
              CWLG(J,I)=CWGL(I,J)
           ENDDO
        ENDDO

          DO I=KRMING_GL,KRMAXG_GL
             DO J=KRMINL_GL,KRMAXL_GL
               IF (ICETURB.EQ.1)THEN
                CWGL(I,J)=CTURBGL(I,J)*CWGL(I,J)
               ELSE
                CWGL(I,J)=CWGL(I,J)
               END IF
             ENDDO
          ENDDO
          DO I=KRMING_GL,KRMAXG_GL
             DO J=KRMINL_GL,KRMAXL_GL
                CWLG(J,I)=CWGL(I,J)
             ENDDO
          ENDDO

88     CONTINUE
	RETURN
	END SUBROUTINE KERNALS_IN
        SUBROUTINE TURBCOEF
        IMPLICIT NONE
        INTEGER I,J

        DOUBLE PRECISION X_KERN,Y_KERN
	DOUBLE PRECISION RL_LL(K0_LL),RL_GL(K0L_GL),RG_GL(K0G_GL)
          RL_LL(1)=RADXXO(KRMIN_LL,1)*1.E4
          RL_LL(2)=10.0D0
          RL_LL(3)=20.0D0
          RL_LL(4)=30.0D0
          RL_LL(5)=40.0D0
          RL_LL(6)=50.0D0
          RL_LL(7)=60.0D0
          RL_LL(8)=RADXXO(KRMAX_LL,1)*1.E4
          DO J=1,K0_LL
             DO I=1,K0_LL
                CTURB_LL(I,J)=1.0D0
             ENDDO
          ENDDO 
	  CTURB_LL(1,1)=4.50D0
	  CTURB_LL(1,2)=4.50D0
	  CTURB_LL(1,3)=3.00D0
	  CTURB_LL(1,4)=2.25D0
	  CTURB_LL(1,5)=1.95D0
	  CTURB_LL(1,6)=1.40D0
	  CTURB_LL(1,7)=1.40D0
	  CTURB_LL(1,8)=1.40D0

	  CTURB_LL(2,1)=4.50D0
	  CTURB_LL(2,2)=4.50D0
	  CTURB_LL(2,3)=3.00D0
	  CTURB_LL(2,4)=2.25D0
	  CTURB_LL(2,5)=1.95D0
	  CTURB_LL(2,6)=1.40D0
	  CTURB_LL(2,7)=1.40D0
	  CTURB_LL(2,8)=1.40D0

	  CTURB_LL(3,1)=3.00D0
	  CTURB_LL(3,2)=3.00D0
	  CTURB_LL(3,3)=2.70D0
	  CTURB_LL(3,4)=2.25D0
	  CTURB_LL(3,5)=1.65D0
	  CTURB_LL(3,6)=1.40D0
	  CTURB_LL(3,7)=1.40D0
	  CTURB_LL(3,8)=1.40D0

	  CTURB_LL(4,1)=2.25D0
	  CTURB_LL(4,2)=2.25D0
	  CTURB_LL(4,3)=2.25D0
	  CTURB_LL(4,4)=1.95D0
	  CTURB_LL(4,5)=1.65D0
	  CTURB_LL(4,6)=1.40D0
	  CTURB_LL(4,7)=1.40D0
	  CTURB_LL(4,8)=1.40D0

	  CTURB_LL(5,1)=1.95D0
	  CTURB_LL(5,2)=1.95D0
	  CTURB_LL(5,3)=1.65D0
	  CTURB_LL(5,4)=1.65D0
	  CTURB_LL(5,5)=1.65D0
	  CTURB_LL(5,6)=1.40D0
	  CTURB_LL(5,7)=1.40D0
	  CTURB_LL(5,8)=1.40D0

	  CTURB_LL(6,1)=1.40D0
	  CTURB_LL(6,2)=1.40D0
	  CTURB_LL(6,3)=1.40D0
	  CTURB_LL(6,4)=1.40D0
	  CTURB_LL(6,5)=1.40D0
	  CTURB_LL(6,6)=1.40D0
	  CTURB_LL(6,7)=1.40D0
	  CTURB_LL(6,8)=1.40D0

	  CTURB_LL(7,1)=1.40D0
	  CTURB_LL(7,2)=1.40D0
	  CTURB_LL(7,3)=1.40D0
	  CTURB_LL(7,4)=1.40D0
	  CTURB_LL(7,5)=1.40D0
	  CTURB_LL(7,6)=1.40D0
	  CTURB_LL(7,7)=1.40D0
	  CTURB_LL(7,8)=1.40D0

	  CTURB_LL(8,1)=1.40D0
	  CTURB_LL(8,2)=1.40D0
	  CTURB_LL(8,3)=1.40D0
	  CTURB_LL(8,4)=1.40D0
	  CTURB_LL(8,5)=1.40D0
	  CTURB_LL(8,6)=1.40D0
	  CTURB_LL(8,7)=1.40D0
	  CTURB_LL(8,8)=1.40D0
          DO J=1,K0_LL
             DO I=1,K0_LL
                CTURB_LL(I,J)=(CTURB_LL(I,J)-1.0D0)/1.5D0+1.0D0
             ENDDO
          ENDDO
	  DO I=KRMIN_LL,KRMAX_LL
             DO J=KRMIN_LL,KRMAX_LL
                CTURBLL(I,J)=1.0D0
             ENDDO
          ENDDO
          DO I=KRMIN_LL,KRMAX_LL
             X_KERN=RADXXO(I,1)*1.0D4
             IF(X_KERN.LT.RL_LL(1)) X_KERN=RL_LL(1)
             IF(X_KERN.GT.RL_LL(K0_LL)) X_KERN=RL_LL(K0_LL) 
             DO J=KRMIN_LL,KRMAX_LL
                Y_KERN=RADXXO(J,1)*1.0D4
                IF(Y_KERN.LT.RL_LL(1)) Y_KERN=RL_LL(1)
                IF(Y_KERN.GT.RL_LL(K0_LL)) Y_KERN=RL_LL(K0_LL)
                CTURBLL(I,J)=F(X_KERN,Y_KERN,RL_LL,RL_LL,CTURB_LL &
     &                      ,K0_LL,K0_LL)	                         
             ENDDO
          ENDDO
          RL_GL(1) = RADXXO(1,1)*1.E4 
          RL_GL(2) = 8.0D0
          RL_GL(3) = 10.0D0
	  RL_GL(4) = 16.0D0
          RL_GL(5) = 20.0D0
          RL_GL(6) = 30.0D0
          RL_GL(7) = 40.0D0
          RL_GL(8) = 50.0D0
          RL_GL(9) = 60.0D0
          RL_GL(10)= 70.0D0
          RL_GL(11)= 80.0D0
	  RL_GL(12)= 90.0D0
	  RL_GL(13)=100.0D0
	  RL_GL(14)=200.0D0
	  RL_GL(15)=300.0D0
	  RL_GL(16)=RADXXO(24,1)*1.0D4

          RG_GL(1) = RADXXO(1,6)*1.0D4 
          RG_GL(2) = 30.0D0  
          RG_GL(3) = 60.0D0 
          RG_GL(4) = 100.0D0 
          RG_GL(5) = 200.0D0 
	  RG_GL(6) = 300.0D0
	  RG_GL(7) = 400.0D0
	  RG_GL(8) = 500.0D0
	  RG_GL(9) = 600.0D0
	  RG_GL(10)= 700.0D0
	  RG_GL(11)= 800.0D0
	  RG_GL(12)= 900.0D0
	  RG_GL(13)=1000.0D0
	  RG_GL(14)=2000.0D0
	  RG_GL(15)=3000.0D0
	  RG_GL(16)=RADXXO(33,6)*1.0D4
	  DO I=KRMING_GL,KRMAXG_GL
             DO J=KRMINL_GL,KRMAXL_GL
                CTURBGL(I,J)=1.0D0
             ENDDO
          ENDDO
          DO I=1,K0G_GL
             DO J=1,K0L_GL
                CTURB_GL(I,J)=1.0D0
             ENDDO
          ENDDO 
          IF(IEPS_400.EQ.1) THEN
	    CTURB_GL(1,1)=0.0D0
	    CTURB_GL(1,2)=0.0D0
	    CTURB_GL(1,3)=1.2D0
	    CTURB_GL(1,4)=1.3D0
	    CTURB_GL(1,5)=1.4D0
	    CTURB_GL(1,6)=1.5D0
	    CTURB_GL(1,7)=1.5D0
	    CTURB_GL(1,8)=1.5D0
	    CTURB_GL(1,9)=1.5D0
	    CTURB_GL(1,10)=1.5D0
	    CTURB_GL(1,11)=1.5D0
	    CTURB_GL(1,12)=1.0D0
	    CTURB_GL(1,13)=1.0D0
	    CTURB_GL(1,14)=1.0D0
	    CTURB_GL(1,15)=1.0D0
	
	    CTURB_GL(2,1)=1.0D0
	    CTURB_GL(2,2)=1.4D0
	    CTURB_GL(2,3)=1.8D0
	    CTURB_GL(2,4)=2.2D0
	    CTURB_GL(2,5)=2.6D0
	    CTURB_GL(2,6)=3.0D0
	    CTURB_GL(2,7)=2.85D0
	    CTURB_GL(2,8)=2.7D0
	    CTURB_GL(2,9)=2.55D0
	    CTURB_GL(2,10)=2.4D0
	    CTURB_GL(2,11)=2.25D0
	    CTURB_GL(2,12)=1.0D0
	    CTURB_GL(2,13)=1.0D0
	    CTURB_GL(2,14)=1.0D0

	    CTURB_GL(3,1)=7.5D0
	    CTURB_GL(3,2)=7.5D0
	    CTURB_GL(3,3)=4.5D0	
	    CTURB_GL(3,4)=4.5D0	
	    CTURB_GL(3,5)=4.65D0	
	    CTURB_GL(3,6)=4.65D0	
	    CTURB_GL(3,7)=4.5D0	
	    CTURB_GL(3,8)=4.5D0	
	    CTURB_GL(3,9)=4.0D0	
	    CTURB_GL(3,10)=3.0D0	
	    CTURB_GL(3,11)=2.0D0	
	    CTURB_GL(3,12)=1.5D0	
	    CTURB_GL(3,13)=1.3D0	
	    CTURB_GL(3,14)=1.0D0	
    
	    CTURB_GL(4,1)=5.5D0
	    CTURB_GL(4,2)=5.5D0
	    CTURB_GL(4,3)=4.5D0
	    CTURB_GL(4,4)=4.5D0
	    CTURB_GL(4,5)=4.65D0
	    CTURB_GL(4,6)=4.65D0
	    CTURB_GL(4,7)=4.5D0
	    CTURB_GL(4,8)=4.5D0
	    CTURB_GL(4,9)=4.0D0
	    CTURB_GL(4,10)=3.0D0
	    CTURB_GL(4,11)=2.0D0
	    CTURB_GL(4,12)=1.5D0
	    CTURB_GL(4,13)=1.35D0
	    CTURB_GL(4,14)=1.0D0
	 
	    CTURB_GL(5,1)=4.5D0
	    CTURB_GL(5,2)=4.5D0
	    CTURB_GL(5,3)=3.3D0	
	    CTURB_GL(5,4)=3.3D0	
	    CTURB_GL(5,5)=3.3D0	
	    CTURB_GL(5,6)=3.4D0	
	    CTURB_GL(5,7)=3.8D0	
	    CTURB_GL(5,8)=3.8D0	
	    CTURB_GL(5,9)=3.8D0	
	    CTURB_GL(5,10)=3.6D0
	    CTURB_GL(5,11)=2.5D0	
	    CTURB_GL(5,12)=2.0D0	
	    CTURB_GL(5,13)=1.4D0	
	    CTURB_GL(5,14)=1.0D0	
			 		
	    CTURB_GL(6,1)=4.0D0
	    CTURB_GL(6,2)=4.0D0
	    CTURB_GL(6,3)=2.8D0
	    CTURB_GL(6,4)=2.8D0
	    CTURB_GL(6,5)=2.85D0
	    CTURB_GL(6,6)=2.9D0
	    CTURB_GL(6,7)=3.0D0
	    CTURB_GL(6,8)=3.1D0
	    CTURB_GL(6,9)=2.9D0
	    CTURB_GL(6,10)=2.6D0
	    CTURB_GL(6,11)=2.5D0
	    CTURB_GL(6,12)=2.0D0
	    CTURB_GL(6,13)=1.3D0
	    CTURB_GL(6,14)=1.1D0

	    CTURB_GL(7,1)=3.5D0
	    CTURB_GL(7,2)=3.5D0
	    CTURB_GL(7,3)=2.5D0
	    CTURB_GL(7,4)=2.5D0
	    CTURB_GL(7,5)=2.6D0
	    CTURB_GL(7,6)=2.7D0
	    CTURB_GL(7,7)=2.8D0
	    CTURB_GL(7,8)=2.8D0
	    CTURB_GL(7,9)=2.8D0
	    CTURB_GL(7,10)=2.6D0
	    CTURB_GL(7,11)=2.3D0
	    CTURB_GL(7,12)=2.0D0
	    CTURB_GL(7,13)=1.3D0
	    CTURB_GL(7,14)=1.1D0

	    CTURB_GL(8,1)=3.25D0
	    CTURB_GL(8,2)=3.25D0
	    CTURB_GL(8,3)=2.3D0
	    CTURB_GL(8,4)=2.3D0
	    CTURB_GL(8,5)=2.35D0
	    CTURB_GL(8,6)=2.37D0
	    CTURB_GL(8,7)=2.55D0
	    CTURB_GL(8,8)=2.55D0
	    CTURB_GL(8,9)=2.55D0
	    CTURB_GL(8,10)=2.3D0
	    CTURB_GL(8,11)=2.1D0
	    CTURB_GL(8,12)=1.9D0
	    CTURB_GL(8,13)=1.3D0
	    CTURB_GL(8,14)=1.1D0

	    CTURB_GL(9,1)=3.0D0
	    CTURB_GL(9,2)=3.0D0
	    CTURB_GL(9,3)=3.1D0
	    CTURB_GL(9,4)=2.2D0
	    CTURB_GL(9,5)=2.2D0
	    CTURB_GL(9,6)=2.2D0
	    CTURB_GL(9,7)=2.3D0
	    CTURB_GL(9,8)=2.3D0
	    CTURB_GL(9,9)=2.5D0
	    CTURB_GL(9,10)=2.5D0
	    CTURB_GL(9,11)=2.2D0
	    CTURB_GL(9,12)=1.8D0
	    CTURB_GL(9,13)=1.25D0
	    CTURB_GL(9,14)=1.1D0

	    CTURB_GL(10,1)=2.75D0
	    CTURB_GL(10,2)=2.75D0
	    CTURB_GL(10,3)=2.0D0
	    CTURB_GL(10,4)=2.0D0
	    CTURB_GL(10,5)=2.0D0
	    CTURB_GL(10,6)=2.1D0
	    CTURB_GL(10,7)=2.2D0
	    CTURB_GL(10,8)=2.2D0
	    CTURB_GL(10,9)=2.3D0
	    CTURB_GL(10,10)=2.3D0
	    CTURB_GL(10,11)=2.3D0
	    CTURB_GL(10,12)=1.8D0
	    CTURB_GL(10,13)=1.2D0
	    CTURB_GL(10,14)=1.1D0

	    CTURB_GL(11,1)=2.6D0
	    CTURB_GL(11,2)=2.6D0
	    CTURB_GL(11,3)=1.95D0
	    CTURB_GL(11,4)=1.95D0
	    CTURB_GL(11,5)=1.95D0
	    CTURB_GL(11,6)=2.05D0
	    CTURB_GL(11,7)=2.15D0
	    CTURB_GL(11,8)=2.15D0
	    CTURB_GL(11,9)=2.25D0
	    CTURB_GL(11,10)=2.25D0
	    CTURB_GL(11,11)=1.9D0
	    CTURB_GL(11,12)=1.8D0
	    CTURB_GL(11,13)=1.2D0
	    CTURB_GL(11,14)=1.1D0

	    CTURB_GL(12,1)=2.4D0
	    CTURB_GL(12,2)=2.4D0
	    CTURB_GL(12,3)=1.85D0
	    CTURB_GL(12,4)=1.85D0
	    CTURB_GL(12,5)=1.85D0
	    CTURB_GL(12,6)=1.75D0
	    CTURB_GL(12,7)=1.85D0
	    CTURB_GL(12,8)=1.85D0
	    CTURB_GL(12,9)=2.1D0
	    CTURB_GL(12,10)=2.1D0
	    CTURB_GL(12,11)=1.9D0
	    CTURB_GL(12,12)=1.8D0 
	    CTURB_GL(12,13)=1.3D0
	    CTURB_GL(12,14)=1.1D0

	    CTURB_GL(13,1)=1.67D0
	    CTURB_GL(13,2)=1.67D0
	    CTURB_GL(13,3)=1.75D0
	    CTURB_GL(13,4)=1.83D0
	    CTURB_GL(13,5)=1.87D0
	    CTURB_GL(13,6)=2.0D0
	    CTURB_GL(13,7)=2.1D0
	    CTURB_GL(13,8)=2.12D0
	    CTURB_GL(13,9)=2.15D0
	    CTURB_GL(13,10)=2.18D0
	    CTURB_GL(13,11)=2.19D0
	    CTURB_GL(13,12)=1.67D0
	    CTURB_GL(13,13)=1.28D0
	    CTURB_GL(13,14)=1.0D0

	    CTURB_GL(14,1)=1.3D0
	    CTURB_GL(14,2)=1.3D0
	    CTURB_GL(14,3)=1.35D0
	    CTURB_GL(14,4)=1.4D0
	    CTURB_GL(14,5)=1.6D0
	    CTURB_GL(14,6)=1.7D0
	    CTURB_GL(14,7)=1.7D0
	    CTURB_GL(14,8)=1.7D0
	    CTURB_GL(14,9)=1.7D0
	    CTURB_GL(14,10)=1.7D0
	    CTURB_GL(14,11)=1.7D0
	    CTURB_GL(14,12)=1.4D0
	    CTURB_GL(14,13)=1.25D0
	    CTURB_GL(14,14)=1.0D0

	    CTURB_GL(15,1)=1.17D0
	    CTURB_GL(15,2)=1.17D0
	    CTURB_GL(15,3)=1.17D0
	    CTURB_GL(15,4)=1.25D0
	    CTURB_GL(15,5)=1.3D0
	    CTURB_GL(15,6)=1.35D0
	    CTURB_GL(15,7)=1.4D0
	    CTURB_GL(15,8)=1.4D0
	    CTURB_GL(15,9)=1.45D0
	    CTURB_GL(15,10)=1.47D0
	    CTURB_GL(15,11)=1.44D0
	    CTURB_GL(15,12)=1.3D0
	    CTURB_GL(15,13)=1.12D0
	    CTURB_GL(15,14)=1.0D0

	    CTURB_GL(16,1)=1.17D0
	    CTURB_GL(16,2)=1.17D0
	    CTURB_GL(16,3)=1.17D0
	    CTURB_GL(16,4)=1.25D0
	    CTURB_GL(16,5)=1.3D0
	    CTURB_GL(16,6)=1.35D0
	    CTURB_GL(16,7)=1.4D0
	    CTURB_GL(16,8)=1.45D0
	    CTURB_GL(16,9)=1.45D0
	    CTURB_GL(16,10)=1.47D0
	    CTURB_GL(16,11)=1.44D0
	    CTURB_GL(16,12)=1.3D0
	    CTURB_GL(16,13)=1.12D0
	    CTURB_GL(16,14)=1.0D0
          ENDIF
          IF(IEPS_800.EQ.1) THEN
	    CTURB_GL(1,1) =0.00D0
	    CTURB_GL(1,2) =0.00D0
	    CTURB_GL(1,3) =1.00D0
            CTURB_GL(1,4) =1.50D0
	    CTURB_GL(1,5) =1.40D0
	    CTURB_GL(1,6) =1.30D0
	    CTURB_GL(1,7) =1.20D0
	    CTURB_GL(1,8) =1.10D0
	    CTURB_GL(1,9) =1.00D0
	    CTURB_GL(1,10)=1.00D0
	    CTURB_GL(1,11)=1.00D0
	    CTURB_GL(1,12)=1.00D0
	    CTURB_GL(1,13)=1.00D0
	    CTURB_GL(1,14)=1.00D0
	    CTURB_GL(1,15)=1.00D0
	    CTURB_GL(1,16)=1.00D0

	    CTURB_GL(2,1) =0.00D0
	    CTURB_GL(2,2) =0.00D0
	    CTURB_GL(2,3) =1.00D0
	    CTURB_GL(2,4) =2.00D0
	    CTURB_GL(2,5) =1.80D0
	    CTURB_GL(2,6) =1.70D0
	    CTURB_GL(2,7) =1.60D0
	    CTURB_GL(2,8) =1.50D0
	    CTURB_GL(2,9) =1.50D0
	    CTURB_GL(2,10)=1.50D0
	    CTURB_GL(2,11)=1.50D0
	    CTURB_GL(2,12)=1.50D0
	    CTURB_GL(2,13)=1.50D0
	    CTURB_GL(2,14)=1.00D0
	    CTURB_GL(2,15)=1.00D0
	    CTURB_GL(2,16)=1.00D0

	    CTURB_GL(3,1) =0.00D0
	    CTURB_GL(3,2) =0.00D0
	    CTURB_GL(3,3) =4.00D0
	    CTURB_GL(3,4) =7.65D0
	    CTURB_GL(3,5) =7.65D0
	    CTURB_GL(3,6) =8.00D0
	    CTURB_GL(3,7) =8.00D0
	    CTURB_GL(3,8) =7.50D0
	    CTURB_GL(3,9) =6.50D0
	    CTURB_GL(3,10)=6.00D0
	    CTURB_GL(3,11)=5.00D0
	    CTURB_GL(3,12)=4.50D0
	    CTURB_GL(3,13)=4.00D0
	    CTURB_GL(3,14)=2.00D0
	    CTURB_GL(3,15)=1.30D0
	    CTURB_GL(3,16)=1.00D0

	    CTURB_GL(4,1) =7.50D0
	    CTURB_GL(4,2) =7.50D0
	    CTURB_GL(4,3) =7.50D0
	    CTURB_GL(4,4) =7.65D0	
	    CTURB_GL(4,5) =7.65D0	
	    CTURB_GL(4,6) =8.00D0	
	    CTURB_GL(4,7) =8.00D0	
	    CTURB_GL(4,8) =7.50D0	
	    CTURB_GL(4,9) =6.50D0	
	    CTURB_GL(4,10)=6.00D0	
	    CTURB_GL(4,11)=5.00D0	
	    CTURB_GL(4,12)=4.50D0	
	    CTURB_GL(4,13)=4.00D0	
	    CTURB_GL(4,14)=2.00D0	
	    CTURB_GL(4,15)=1.30D0	
	    CTURB_GL(4,16)=1.00D0	
    
	    CTURB_GL(5,1) =5.50D0
	    CTURB_GL(5,2) =5.50D0
	    CTURB_GL(5,3) =5.50D0
	    CTURB_GL(5,4) =5.75D0
	    CTURB_GL(5,5) =5.75D0
	    CTURB_GL(5,6) =6.00D0
	    CTURB_GL(5,7) =6.25D0
	    CTURB_GL(5,8) =6.17D0
	    CTURB_GL(5,9) =5.75D0
	    CTURB_GL(5,10)=5.25D0
	    CTURB_GL(5,11)=4.75D0
	    CTURB_GL(5,12)=4.25D0
	    CTURB_GL(5,13)=4.00D0
	    CTURB_GL(5,14)=2.00D0
	    CTURB_GL(5,15)=1.35D0
	    CTURB_GL(5,16)=1.00D0
	 
	    CTURB_GL(6,1) =4.50D0
	    CTURB_GL(6,2) =4.50D0
	    CTURB_GL(6,3) =4.50D0
	    CTURB_GL(6,4) =4.75D0	
	    CTURB_GL(6,5) =4.75D0	
	    CTURB_GL(6,6) =5.00D0	
	    CTURB_GL(6,7) =5.25D0	
	    CTURB_GL(6,8) =5.25D0	
	    CTURB_GL(6,9) =5.00D0	
	    CTURB_GL(6,10)=4.75D0	
	    CTURB_GL(6,11)=4.50D0	
	    CTURB_GL(6,12)=4.00D0	
	    CTURB_GL(6,13)=3.75D0	
	    CTURB_GL(6,14)=2.00D0	
	    CTURB_GL(6,15)=1.40D0	
	    CTURB_GL(6,16)=1.00D0	
			 		
	    CTURB_GL(7,1) =4.00D0
	    CTURB_GL(7,2) =4.00D0
	    CTURB_GL(7,3) =4.00D0
	    CTURB_GL(7,4) =4.00D0
	    CTURB_GL(7,5) =4.00D0
	    CTURB_GL(7,6) =4.25D0
	    CTURB_GL(7,7) =4.50D0
	    CTURB_GL(7,8) =4.67D0
	    CTURB_GL(7,9) =4.50D0
	    CTURB_GL(7,10)=4.30D0
	    CTURB_GL(7,11)=4.10D0
	    CTURB_GL(7,12)=3.80D0
	    CTURB_GL(7,13)=3.50D0
	    CTURB_GL(7,14)=2.00D0
	    CTURB_GL(7,15)=1.30D0
	    CTURB_GL(7,16)=1.10D0

	    CTURB_GL(8,1) =3.50D0
	    CTURB_GL(8,2) =3.50D0
	    CTURB_GL(8,3) =3.50D0
	    CTURB_GL(8,4) =3.65D0
	    CTURB_GL(8,5) =3.65D0
	    CTURB_GL(8,6) =3.80D0
	    CTURB_GL(8,7) =4.1D02
	    CTURB_GL(8,8) =4.17D0
	    CTURB_GL(8,9) =4.17D0
	    CTURB_GL(8,10)=4.00D0
	    CTURB_GL(8,11)=3.80D0
	    CTURB_GL(8,12)=3.67D0
	    CTURB_GL(8,13)=3.40D0
	    CTURB_GL(8,14)=2.00D0
	    CTURB_GL(8,15)=1.30D0
	    CTURB_GL(8,16)=1.10D0

	    CTURB_GL(9,1) =3.25D0
	    CTURB_GL(9,2) =3.25D0
	    CTURB_GL(9,3) =3.25D0
	    CTURB_GL(9,4) =3.25D0
	    CTURB_GL(9,5) =3.25D0
	    CTURB_GL(9,6) =3.50D0
	    CTURB_GL(9,7) =3.75D0
	    CTURB_GL(9,8) =3.75D0
	    CTURB_GL(9,9) =3.75D0
	    CTURB_GL(9,10)=3.75D0
	    CTURB_GL(9,11)=3.60D0
	    CTURB_GL(9,12)=3.40D0
	    CTURB_GL(9,13)=3.25D0
	    CTURB_GL(9,14)=2.00D0
	    CTURB_GL(9,15)=1.30D0
	    CTURB_GL(9,16)=1.10D0
	    
	    CTURB_GL(10,1) =3.00D0
	    CTURB_GL(10,2) =3.00D0
	    CTURB_GL(10,3) =3.00D0
	    CTURB_GL(10,4) =3.10D0
	    CTURB_GL(10,5) =3.10D0
	    CTURB_GL(10,6) =3.25D0
	    CTURB_GL(10,7) =3.40D0
	    CTURB_GL(10,8) =3.50D0
	    CTURB_GL(10,9) =3.50D0
	    CTURB_GL(10,10)=3.50D0
	    CTURB_GL(10,11)=3.40D0
	    CTURB_GL(10,12)=3.25D0
	    CTURB_GL(10,13)=3.15D0
	    CTURB_GL(10,14)=1.90D0
	    CTURB_GL(10,15)=1.30D0
	    CTURB_GL(10,16)=1.10D0

	    CTURB_GL(11,1) =2.75D0
	    CTURB_GL(11,2) =2.75D0
	    CTURB_GL(11,3) =2.75D0
	    CTURB_GL(11,4) =2.75D0
	    CTURB_GL(11,5) =2.75D0
	    CTURB_GL(11,6) =3.00D0
	    CTURB_GL(11,7) =3.25D0
	    CTURB_GL(11,8) =3.25D0
	    CTURB_GL(11,9) =3.25D0
	    CTURB_GL(11,10)=3.25D0
	    CTURB_GL(11,11)=3.25D0
	    CTURB_GL(11,12)=3.15D0
	    CTURB_GL(11,13)=3.00D0
	    CTURB_GL(11,14)=1.80D0
	    CTURB_GL(11,15)=1.30D0
	    CTURB_GL(11,16)=1.10D0

	    CTURB_GL(12,1) =2.60D0
	    CTURB_GL(12,2) =2.60D0
	    CTURB_GL(12,3) =2.60D0
	    CTURB_GL(12,4) =2.67D0
	    CTURB_GL(12,5) =2.67D0
	    CTURB_GL(12,6) =2.75D0
	    CTURB_GL(12,7) =3.00D0
	    CTURB_GL(12,8) =3.17D0
	    CTURB_GL(12,9) =3.17D0
	    CTURB_GL(12,10)=3.17D0
	    CTURB_GL(12,11)=3.10D0
	    CTURB_GL(12,12)=2.90D0
	    CTURB_GL(12,13)=2.80D0
	    CTURB_GL(12,14)=1.87D0
	    CTURB_GL(12,15)=1.37D0
	    CTURB_GL(12,16)=1.10D0

	    CTURB_GL(13,1) =2.40D0
	    CTURB_GL(13,2) =2.40D0
	    CTURB_GL(13,3) =2.40D0
	    CTURB_GL(13,4) =2.50D0
	    CTURB_GL(13,5) =2.50D0
	    CTURB_GL(13,6) =2.67D0
	    CTURB_GL(13,7) =2.83D0
	    CTURB_GL(13,8) =2.90D0
	    CTURB_GL(13,9) =3.00D0
	    CTURB_GL(13,10)=2.90D0
	    CTURB_GL(13,11)=2.85D0
	    CTURB_GL(13,12)=2.80D0
	    CTURB_GL(13,13)=2.75D0
	    CTURB_GL(13,14)=1.83D0
	    CTURB_GL(13,15)=1.30D0
	    CTURB_GL(13,16)=1.10D0

	    CTURB_GL(14,1) =1.67D0
	    CTURB_GL(14,2) =1.67D0
	    CTURB_GL(14,3) =1.67D0
	    CTURB_GL(14,4) =1.75D0
	    CTURB_GL(14,5) =1.75D0
	    CTURB_GL(14,6) =1.83D0
	    CTURB_GL(14,7) =1.87D0
	    CTURB_GL(14,8) =2.00D0
	    CTURB_GL(14,9) =2.10D0
	    CTURB_GL(14,10)=2.12D0
	    CTURB_GL(14,11)=2.15D0
	    CTURB_GL(14,12)=2.18D0
	    CTURB_GL(14,13)=2.19D0
	    CTURB_GL(14,14)=1.67D0
	    CTURB_GL(14,15)=1.28D0
	    CTURB_GL(14,16)=1.00D0

	    CTURB_GL(15,1) =1.30D0
	    CTURB_GL(15,2) =1.30D0
	    CTURB_GL(15,3) =1.30D0
	    CTURB_GL(15,4) =1.35D0
	    CTURB_GL(15,5) =1.35D0
	    CTURB_GL(15,6) =1.40D0
	    CTURB_GL(15,7) =1.60D0
	    CTURB_GL(15,8) =1.70D0
	    CTURB_GL(15,9) =1.70D0
	    CTURB_GL(15,10)=1.70D0
	    CTURB_GL(15,11)=1.70D0
	    CTURB_GL(15,12)=1.70D0
	    CTURB_GL(15,13)=1.70D0
	    CTURB_GL(15,14)=1.40D0
	    CTURB_GL(15,15)=1.25D0
	    CTURB_GL(15,16)=1.00D0

	    CTURB_GL(16,1) =1.17D0
	    CTURB_GL(16,2) =1.17D0
	    CTURB_GL(16,3) =1.17D0
	    CTURB_GL(16,4) =1.17D0
	    CTURB_GL(16,5) =1.17D0
	    CTURB_GL(16,6) =1.25D0
	    CTURB_GL(16,7) =1.30D0
	    CTURB_GL(16,8) =1.35D0
	    CTURB_GL(16,9) =1.40D0
	    CTURB_GL(16,10)=1.45D0
	    CTURB_GL(16,11)=1.45D0
	    CTURB_GL(16,12)=1.47D0
	    CTURB_GL(16,13)=1.44D0
	    CTURB_GL(16,14)=1.30D0
	    CTURB_GL(16,15)=1.12D0
	    CTURB_GL(16,16)=1.00D0
          ENDIF
          IF(IEPS_800.EQ.1.AND.IEPS_1600.EQ.1) THEN
            DO I=1,K0G_GL
               DO J=1,K0L_GL
                  CTURB_GL(I,J)=CTURB_GL(I,J)*1.7D0
               ENDDO
            ENDDO 
          ENDIF
          DO J=1,K0L_GL
             DO I=1,K0G_GL
                CTURB_GL(I,J)=(CTURB_GL(I,J)-1.0D0)/1.5D0+1.0D0
             ENDDO
          ENDDO
	  DO I=KRMING_GL,KRMAXG_GL
             DO J=KRMINL_GL,KRMAXL_GL
                CTURBGL(I,J)=1.
             ENDDO
          ENDDO
          DO I=KRMING_GL,KRMAXG_GL                   
             X_KERN=RADXXO(I,6)*1.0D4
             IF(X_KERN.LT.RG_GL(1)) X_KERN=RG_GL(1)
             IF(X_KERN.GT.RG_GL(K0G_GL)) X_KERN=RG_GL(K0G_GL) 
             DO J=KRMINL_GL,KRMAXL_GL
                Y_KERN=RADXXO(J,1)*1.0D4
                IF(Y_KERN.LT.RL_GL(1)) Y_KERN=RL_GL(1)
                IF(Y_KERN.GT.RL_GL(K0L_GL)) Y_KERN=RL_GL(K0L_GL)
                CTURBGL(I,J)=F(X_KERN,Y_KERN,RG_GL,RL_GL,CTURB_GL &
     &                      ,K0G_GL,K0L_GL)	      
             ENDDO
          ENDDO
          IF(IEPS_800.EQ.1) THEN
            DO I=KRMING_GL,15
               DO J=KRMINL_GL,13
                  IF(CTURBGL(I,J).LT.3.0D0) CTURBGL(I,J)=3.0D0
               ENDDO
            ENDDO
          ENDIF
          IF(IEPS_1600.EQ.1) THEN
            DO I=KRMING_GL,15
               DO J=KRMINL_GL,13
                  IF(CTURBGL(I,J).LT.5.1D0) CTURBGL(I,J)=5.1D0
               ENDDO
            ENDDO
          ENDIF
	  DO I=1,33
             DO J=1,24
                IF(I.LE.14.AND.J.EQ.8) CTURBGL(I,J)=1.0D0
                IF(I.GT.14.AND.J.LE.8) CTURBGL(I,J)=1.2D0
	     ENDDO
          ENDDO                       
	RETURN
	END SUBROUTINE TURBCOEF


        real * 8 function f(x,y,x0,y0,table,k0,kk0)



       implicit none
       integer k0,kk0,k,ir,kk,iq
       double precision x,y,p,q,ec,ek

       double precision x0(k0),y0(kk0),table(k0,kk0)


        do k=2,k0
           if(x.le.x0(k).and.x.ge.x0(k-1)) then
             ir=k     
           elseif(x.gt.x0(k0)) then
             ir=k0+1
           elseif(x.lt.x0(1)) then
             ir=1
           endif
        enddo
        do kk=2,kk0
           if(y.le.y0(kk).and.y.ge.y0(kk-1)) iq=kk
        enddo
        if(ir.lt.k0+1) then
          if(ir.ge.2) then
            p =(x-x0(ir-1))/(x0(ir)-x0(ir-1))
            q =(y-y0(iq-1))/(y0(iq)-y0(iq-1))
            ec=(1.d0-p)*(1.d0-q)*table(ir-1,iq-1)+ &
     &              p*(1.d0-q)*table(ir,iq-1)+ &
     &              q*(1.d0-p)*table(ir-1,iq)+ &
     &                   p*q*table(ir,iq)    
          else
            q =(y-y0(iq-1))/(y0(iq)-y0(iq-1))
            ec=(1.d0-q)*table(1,iq-1)+q*table(1,iq)    
          endif
        else
          q =(y-y0(iq-1))/(y0(iq)-y0(iq-1))
          ek=(1.d0-q)*table(k0,iq-1)+q*table(k0,iq)
          ec=min(ek,1.d0) 
        endif
        f=ec
        return
        end function f

                                                                            

                                                                            


        SUBROUTINE FREEZ(FF1,XL,FF2,XI,FF3,XS,FF4,XG,FF5,XH &
     &,TIN,DT,RO,COL,AFREEZMY,BFREEZMY,BFREEZMAX,KRFREEZ,ICEMAX,NKR)       
      IMPLICIT NONE 
      INTEGER KR,ICE,ICE_TYPE
      REAL COL,AFREEZMY,BFREEZMY,BFREEZMAX
      INTEGER KRFREEZ,ICEMAX,NKR
      REAL DT,RO,YKK,PF,PF_1,DEL_T,TT_DROP,ARG_1,YK2,DF1,BF,ARG_M, & 
     & TT_DROP_AFTER_FREEZ,CFREEZ,SUM_ICE,TIN,TTIN,AF,FF_MAX,F1_MAX, &
     & F2_MAX,F3_MAX,F4_MAX,F5_MAX


	REAL FF1(NKR),XL(NKR),FF2(NKR,ICEMAX) &
     &           ,XI(NKR,ICEMAX),FF3(NKR),XS(NKR),FF4(NKR) &
     &           ,XG(NKR),FF5(NKR),XH(NKR)



	TTIN=TIN
        DEL_T	=TTIN-273.15
	ICE_TYPE=2
	F1_MAX=0.
	F2_MAX=0.
	F3_MAX=0.
	F4_MAX=0.
	F5_MAX=0.
	DO 1 KR=1,NKR
	F1_MAX=AMAX1(F1_MAX,FF1(KR))
	F3_MAX=AMAX1(F3_MAX,FF3(KR))
	F4_MAX=AMAX1(F4_MAX,FF4(KR))
	F5_MAX=AMAX1(F5_MAX,FF5(KR))
	DO 1 ICE=1,ICEMAX
     	F2_MAX=AMAX1(F2_MAX,FF2(KR,ICE))
    1   CONTINUE
    	FF_MAX=AMAX1(F2_MAX,F3_MAX,F4_MAX,F5_MAX)



        IF(DEL_T.LT.0.AND.F1_MAX.NE.0) THEN
	SUM_ICE=0.
	AF	=AFREEZMY
	CFREEZ	=(BFREEZMAX-BFREEZMY)/XL(NKR)



         DO  KR	=1,NKR
	 ARG_M	=XL(KR)
	 BF	=BFREEZMY+CFREEZ*ARG_M
         PF_1	=AF*EXP(-BF*DEL_T)
         PF	=ARG_M*PF_1
	 YKK	=EXP(-PF*DT)
         DF1	=FF1(KR)*(1.-YKK)
	 YK2	=DF1
         FF1(KR)=FF1(KR)*YKK
	 IF(KR.LE.KRFREEZ)  THEN
	 FF2(KR,ICE_TYPE)=FF2(KR,ICE_TYPE)+YK2
			    ELSE
	  FF5(KR)	=FF5(KR)+YK2
	 ENDIF
         SUM_ICE=SUM_ICE+YK2*3.*XL(KR)*XL(KR)*COL



	 ENDDO



	ARG_1	=333.*SUM_ICE/RO
      	TT_DROP_AFTER_FREEZ=TTIN+ARG_1
	TIN	=TT_DROP_AFTER_FREEZ



	ENDIF

   	RETURN                                                           
      	END SUBROUTINE FREEZ                                                             

        SUBROUTINE ORIG_MELT(FF1,XL,FF2,XI,FF3,XS,FF4,XG,FF5,XH &
     &                           ,TIN,DT,RO,COL,ICEMAX,NKR)
      IMPLICIT NONE
      INTEGER KR,ICE,ICE_TYPE
      INTEGER ICEMAX,NKR
      REAL COL
      REAL ARG_M,TT_DROP,ARG_1,TT_DROP_AFTER_FREEZ,DT,DF1,DN,DN0, &
     & RO,A,B,DTFREEZ,SUM_ICE,FF_MAX,F1_MAX,F2_MAX,F3_MAX,F4_MAX,F5_MAX, &
     & DEL_T,TIN
        REAL FF1(NKR),XL(NKR),FF2(NKR,ICEMAX),XI(NKR,ICEMAX) &
     &           ,FF3(NKR),XS(NKR),FF4(NKR) &
     &           ,XG(NKR),FF5(NKR),XH(NKR)



        DEL_T	=TIN-273.15
	ICE_TYPE=2
	F1_MAX=0.
	F2_MAX=0.
	F3_MAX=0.
	F4_MAX=0.
	F5_MAX=0.
	DO 1 KR=1,NKR
	F1_MAX=AMAX1(F1_MAX,FF1(KR))
	F3_MAX=AMAX1(F3_MAX,FF3(KR))
	F4_MAX=AMAX1(F4_MAX,FF4(KR))
	F5_MAX=AMAX1(F5_MAX,FF5(KR))
	DO 1 ICE=1,ICEMAX
     	F2_MAX=AMAX1(F2_MAX,FF2(KR,ICE))
    1	CONTINUE
    	FF_MAX=AMAX1(F2_MAX,F3_MAX,F4_MAX,F5_MAX)

	IF(DEL_T.GE.0.AND.FF_MAX.NE.0) THEN
	  SUM_ICE=0.

  	  DO KR=1,NKR
	     ARG_M=FF3(KR)+FF4(KR)+FF5(KR)
	     DO ICE=1,ICEMAX
	        ARG_M=ARG_M+FF2(KR,ICE)
      	        FF2(KR,ICE)=0.
 	     ENDDO
      	     FF1(KR)=FF1(KR)+ARG_M
      	     FF3(KR)=0.
             FF4(KR)=0.
      	     FF5(KR)=0.
	     SUM_ICE=SUM_ICE+ARG_M*3.*XL(KR)*XL(KR)*COL

	  ENDDO


	  ARG_1=333.*SUM_ICE/RO	
	  TIN=TIN-ARG_1


	ENDIF
   	RETURN                                                           
      	END SUBROUTINE ORIG_MELT                                                             
       SUBROUTINE J_W_MELT(FF1,XL,FF2,XI,FF3,XS,FF4,XG,FF5,XH &
     &                           ,TIN,DT,RO,COL,ICEMAX,NKR)
      IMPLICIT NONE
      INTEGER KR,ICE,ICE_TYPE
      INTEGER ICEMAX,NKR
      REAL COL
      REAL ARG_M,TT_DROP,ARG_1,TT_DROP_AFTER_FREEZ,DT,DF1,DN,DN0, &
     & RO,A,B,DTFREEZ,SUM_ICE,FF_MAX,F1_MAX,F2_MAX,F3_MAX,F4_MAX,F5_MAX, &
     & DEL_T,TIN,meltrate
        REAL FF1(NKR),XL(NKR),FF2(NKR,ICEMAX),XI(NKR,ICEMAX) &
     &           ,FF3(NKR),XS(NKR),FF4(NKR) &
     &           ,XG(NKR),FF5(NKR),XH(NKR)



        DEL_T	=TIN-273.15
	F1_MAX=0.
	F2_MAX=0.
	F3_MAX=0.
	F4_MAX=0.
	F5_MAX=0.
	DO 1 KR=1,NKR
	F1_MAX=AMAX1(F1_MAX,FF1(KR))
	F3_MAX=AMAX1(F3_MAX,FF3(KR))
	F4_MAX=AMAX1(F4_MAX,FF4(KR))
	F5_MAX=AMAX1(F5_MAX,FF5(KR))
	DO 1 ICE=1,ICEMAX
     	F2_MAX=AMAX1(F2_MAX,FF2(KR,ICE))
    1	CONTINUE
    	FF_MAX=AMAX1(F2_MAX,F3_MAX,F4_MAX,F5_MAX)

	SUM_ICE=0.
	IF(DEL_T.GE.0.AND.FF_MAX.NE.0) THEN

          DO KR = 1,NKR
             ARG_M = 0.
            DO ICE = 1,ICEMAX
             IF (ICE ==1) THEN
                 IF (KR .le. 10) THEN
                     FF2(KR,ICE)=0.
                     ARG_M = ARG_M+FF2(KR,ICE)
                 ELSEIF (KR .gt. 10 .and. KR .lt. 18) THEN
                     meltrate = 0.5/50.
                     FF2(KR,ICE)=FF2(KR,ICE)-FF2(KR,ICE)*(meltrate*dt)
                     ARG_M=ARG_M+FF2(KR,ICE)*(meltrate*dt)
                 ELSE
                     meltrate = 0.683/120.
                     FF2(KR,ICE)=FF2(KR,ICE)-FF2(KR,ICE)*(meltrate*dt)
                     ARG_M=ARG_M+FF2(KR,ICE)*(meltrate*dt)
                 ENDIF
             ENDIF
             IF (ICE ==2 .or. ICE ==3) THEN
                IF (kr .le. 12) THEN
                    FF2(KR,ICE)=0.
                    ARG_M = ARG_M+FF2(KR,ICE)
                ELSEIF (kr .gt. 12 .and. kr .lt. 20) THEN
                    meltrate = 0.5/50.
                    FF2(KR,ICE)=FF2(KR,ICE)-FF2(KR,ICE)*(meltrate*dt)
                    ARG_M=ARG_M+FF2(KR,ICE)*(meltrate*dt)
                 ELSE
                     meltrate = 0.683/120.
                    FF2(KR,ICE)=FF2(KR,ICE)-FF2(KR,ICE)*(meltrate*dt)
                    ARG_M=ARG_M+FF2(KR,ICE)*(meltrate*dt)
                 ENDIF
             ENDIF
            ENDDO  

                 IF (kr .le. 14) THEN
                    FF3(KR)=0.
                    ARG_M = ARG_M+FF3(KR)
                 ELSEIF (kr .gt. 14 .and. kr .lt. 22) THEN
                    meltrate = 0.5/50.
                    FF3(KR)=FF3(KR)-FF3(KR)*(meltrate*dt)
                    ARG_M=ARG_M+FF3(KR)*(meltrate*dt)
                 ELSE
                    meltrate = 0.683/120.
                    FF3(KR)=FF3(KR)-FF3(KR)*(meltrate*dt)
                    ARG_M=ARG_M+FF3(KR)*(meltrate*dt)
                 ENDIF

                 IF (kr .le. 13) then
                     FF4(KR)=0.
                     FF5(KR)=0.
                     ARG_M = ARG_M+FF4(KR)+FF5(KR)
                 ELSEIF (kr .gt. 13 .and. kr .lt. 23) THEN
                     meltrate = 0.5/50.
                     FF4(KR)=FF4(KR)-FF4(KR)*(meltrate*dt)
                     FF5(KR)=FF5(KR)-FF5(KR)*(meltrate*dt)
                     ARG_M=ARG_M+(FF4(KR)+FF5(KR))*(meltrate*dt)
                 ELSE
                     meltrate = 0.683/120.
                    FF4(KR)=FF4(KR)-FF4(KR)*(meltrate*dt)
                    FF5(KR)=FF5(KR)-FF5(KR)*(meltrate*dt)
                    ARG_M=ARG_M+(FF4(KR)+FF5(KR))*(meltrate*dt)
                 ENDIF

                   FF1(KR)=FF1(KR)+ARG_M

                   SUM_ICE=SUM_ICE+ARG_M*3.*XL(KR)*XL(KR)*COL

       ENDDO


        ARG_1=333.*SUM_ICE/RO
        TIN=TIN-ARG_1


	ENDIF
   	RETURN                                                           
      	END SUBROUTINE J_W_MELT                                                             

      SUBROUTINE JERNUCL01(PSI1,PSI2,FCCNR &
     &                    ,X1,X2,DTT,DQQ,ROR,PP,DSUP1,DSUP2 &
     &  ,COL,AA1_MY, BB1_MY, AA2_MY, BB2_MY &
     &  ,C1_MEY,C2_MEY,SUP2_OLD,DSUPICEXZ &
     &  ,RCCN,DROPRADII,NKR,ICEMAX,ICEPROCS)
      IMPLICIT NONE 

      INTEGER ICEMAX,NKR
      INTEGER ICEPROCS
      REAL COL,AA1_MY, BB1_MY, AA2_MY, BB2_MY, &
     &  C1_MEY,C2_MEY,SUP2_OLD,DSUPICEXZ, &
     &  RCCN(NKR),DROPRADII(NKR),FCCNR(NKR)

      INTEGER KR,ICE,ITYPE,NRGI,ICORR,II,JJ,KK,NKRDROP,NCRITI
       DOUBLE PRECISION DTT,DQQ,DSUP1,DSUP2
       REAL TT,QQ,              &
     & DX,BMASS,CONCD,C2,CONCDF,DELTACD,CONCDIN,ROR, &
     & DELTAF,DELMASSL,FMASS,HELEK1,DEL2NN,FF1BN, &
     & HELEK2,TPCC,PP,ADDF,DSUP2N,FACT,EW1N,ES2N,ES1N,FNEW, &
     & C1,SUP1N,SUP2N,QPN,TPN,TPC,SUP1,SUP2,DEL1N,DEL2N,AL1,AL2, &
     & TEMP1,TEMP2,TEMP3,A1,B1,A2,B2 












      REAL PSI1(NKR),X1(NKR),DROPCONCN(NKR) &
     &     ,PSI2(NKR,ICEMAX),X2(NKR,ICEMAX)
      

      DATA A1,B1,A2,B2/-0.639,0.1296,-2.8,0.262/
      DATA TEMP1,TEMP2,TEMP3/-5.,-2.,-20./
      DATA AL1/2500./,AL2/2834./
      SUP1=DSUP1
      SUP2=DSUP2


      TT=DTT
      QQ=DQQ


        TPN=TT
        QPN=QQ

        DEL1N=100.*SUP1
        TPC=TT-273.15

        IF(DEL1N.GT.0.AND.TPC.GT.-73.16) THEN
         CALL WATER_NUCL (PSI1,FCCNR,X1,TT,SUP1  &
     &        ,COL,RCCN,DROPRADII,NKR,ICEMAX)
        ENDIF




       IF (ICEPROCS.EQ.1)THEN
        DEL2N=100.*SUP2
        IF(TPC.LT.0..AND.TPC.GE.-73.16.AND.DEL2N.GT.0.) THEN

              CALL ICE_NUCL (PSI2,X2,TT,ROR,SUP2,SUP2_OLD &
     &                      ,C1_MEY,C2_MEY,COL,DSUPICEXZ &
     &                      ,NKR,ICEMAX)
        ENDIF
       ENDIF



      RETURN
      END SUBROUTINE JERNUCL01



      SUBROUTINE WATER_NUCL (PSI1,FCCNR,X1,TT,SUP1 &
     &,COL,RCCN,DROPRADII,NKR,ICEMAX)
      IMPLICIT NONE
      INTEGER NDROPMAX,KR,ICEMAX,NKR
      REAL PSI1(NKR),FCCNR(NKR),X1(NKR)
      REAL DROPCONCN(NKR)
      REAL RCCN(NKR),DROPRADII(NKR)
      REAL TT,SUP1,DX,COL


      CALL NUCLEATION (SUP1,TT,FCCNR,DROPCONCN  &
     &,NDROPMAX,COL,RCCN,DROPRADII,NKR,ICEMAX)


        DO KR=1,NDROPMAX
           DX=3.*COL*X1(KR)

           PSI1(KR)=PSI1(KR)+DROPCONCN(KR)/DX

        ENDDO

      RETURN
      END SUBROUTINE WATER_NUCL
      SUBROUTINE ICE_NUCL (PSI2,X2,TT,ROR,SUP2,SUP2_OLD &
     &                      ,C1_MEY,C2_MEY,COL,DSUPICEXZ &
     &                      ,NKR,ICEMAX)
        IMPLICIT NONE
        INTEGER ITYPE,KR,ICE,NRGI,ICEMAX,NKR
        REAL DEL2N,SUP2,C1,C2,C1_MEY,C2_MEY,TPC,TT,ROR
        REAL DX,COL,BMASS,BFMASS,FMASS
        REAL HELEK1,HELEK2,TPCC,DEL2NN,FF1BN,DSUPICEXZ
        REAL FACT,DSUP2N,SUP2_OLD,DELTACD,DELTAF,ADDF,FNEW
        REAL X2(NKR,ICEMAX),PSI2(NKR,ICEMAX)

        REAL A1,B1,A2,B2
        DATA A1,B1,A2,B2/-0.639,0.1296,-2.8,0.262/

        REAL TEMP1,TEMP2,TEMP3
        DATA TEMP1,TEMP2,TEMP3/-5.,-2.,-20./
        REAL ICE_CON

        C1=C1_MEY
        C2=C2_MEY


        TPC=TT-273.15
        ITYPE=0

        IF((TPC.GT.-4.0).OR.(TPC.LE.-8.1.AND.TPC.GT.-12.7).OR.&
     &  (TPC.LE.-17.8.AND.TPC.GT.-22.4)) THEN
          ITYPE=2
        ELSE
          IF((TPC.LE.-4.0.AND.TPC.GT.-8.1).OR.(TPC.LE.-22.4)) THEN
            ITYPE=1
          ELSE
            ITYPE=3
          ENDIF
        ENDIF





        ICE=ITYPE

        NRGI=2
        IF(TPC.LT.TEMP1) THEN
          DEL2N=100.*SUP2
          DEL2NN=DEL2N
          IF(DEL2N.GT.50.0) DEL2NN=50.
          HELEK1=C1*EXP(A1+B1*DEL2NN)
        ELSE
          HELEK1=0.
        ENDIF

        IF(TPC.LT.TEMP2) THEN
          TPCC=TPC
          IF(TPCC.LT.TEMP3) TPCC=TEMP3
          HELEK2=C2*EXP(A2-B2*TPCC)
        ELSE
          HELEK2=0.
        ENDIF

        FF1BN=HELEK1+HELEK2

        FACT=1.
        DSUP2N=(SUP2-SUP2_OLD+DSUPICEXZ)*100.

        SUP2_OLD=SUP2

        IF(DSUP2N.GT.50.) DSUP2N=50.

        DELTACD=FF1BN*B1*DSUP2N

        IF(DELTACD.GE.FF1BN) DELTACD=FF1BN

        IF(DELTACD.GT.0.) THEN
          ICE_CON=0.
          DO KR=1,NRGI-1
             DX=3.*X2(KR,ICE)*COL
             ICE_CON=ICE_CON+DX*PSI2(KR,ICE)
          ENDDO
          IF(ICE_CON.GT.HELEK1)THEN

          ELSE 
           DELTAF=DELTACD*FACT
           DO KR=1,NRGI-1
             DX=3.*X2(KR,ICE)*COL
             ADDF=DELTAF/DX
             PSI2(KR,ICE)=PSI2(KR,ICE)+ADDF
           ENDDO
          END IF
        ENDIF

       RETURN
       END SUBROUTINE ICE_NUCL





      SUBROUTINE NUCLEATION (SUP1,TT,FCCNR,DROPCONCN  &
     &,NDROPMAX,COL,RCCN,DROPRADII,NKR,ICEMAX)




      IMPLICIT NONE
      INTEGER NDROPMAX,IDROP,ICCN,INEXT,ISMALL,KR,NCRITI
      INTEGER ICEMAX,IMIN,IMAX,NKR,I,II,I0,I1
      REAL &
     &  SUP1,TT,RACTMAX,XKOE,R03,SUPCRITI,AKOE23,RCRITI,BKOE, &
     &  AKOE,CONCCCNIN,DEG01,ALN_IP
      REAL CCNCONC(NKR)
      REAL CCNCONC_BFNUCL


      REAL COL
      REAL RCCN(NKR),DROPRADII(NKR),FCCNR(NKR)
      REAL RACT(NKR),DROPCONC(NKR),DROPCONCN(NKR)
      REAL DLN1,DLN2,FOLD_IP



        DEG01=1./3.







        NDROPMAX=0

        DO KR=1,NKR

           RACT(KR)=0.

           CCNCONC(KR)=0.

           DROPCONCN(KR)=0.
        ENDDO





        CCNCONC_BFNUCL=0.
        DO I=1,NKR
           CCNCONC_BFNUCL=CCNCONC_BFNUCL+FCCNR(I)
        ENDDO

        CCNCONC_BFNUCL=CCNCONC_BFNUCL*COL

        IF(CCNCONC_BFNUCL.EQ.0.) THEN
           RETURN    
        ELSE
           CALL BOUNDARY(IMIN,IMAX,FCCNR,NKR)
           CALL CRITICAL (AKOE,BKOE,TT,RCRITI,SUP1,DEG01)
           IF(RCRITI.GE.RCCN(IMAX))  RETURN
        END IF




        IF (IMIN.EQ.1)THEN
         CALL CCNIMIN(IMIN,IMAX,RCRITI,NCRITI,RCCN,CCNCONC,COL, &
     &       FCCNR,NKR)
         CALL CCNLOOP(IMIN,IMAX,RCRITI,NCRITI,RCCN,CCNCONC,COL, &
     &       FCCNR,NKR)
        ELSE
         CALL CCNLOOP(IMIN,IMAX,RCRITI,NCRITI,RCCN,CCNCONC,COL, &
     &       FCCNR,NKR)
        END IF















        CALL ACTIVATE(IMIN,IMAX,AKOE,BKOE,RCCN,RACT,RACTMAX,NKR)



        CALL DROPMAX(DROPRADII,RACTMAX,NDROPMAX,NKR)


        ISMALL=NCRITI

        INEXT=ISMALL




        DO IDROP=1,NDROPMAX
           DROPCONCN(IDROP)=0.
           DO I=ISMALL,IMAX
              IF(RACT(I).LE.DROPRADII(IDROP)) THEN
                DROPCONCN(IDROP)=DROPCONCN(IDROP)+CCNCONC(I)
                INEXT=I+1
              ENDIF
           ENDDO
           ISMALL=INEXT
        ENDDO




        RETURN
        END SUBROUTINE NUCLEATION



        SUBROUTINE BOUNDARY(IMIN,IMAX,FCCNR,NKR)

        IMPLICIT NONE
        INTEGER I,IMIN,IMAX,NKR
        REAL FCCNR(NKR)

        IMIN=0

        DO I=1,NKR
           IF(FCCNR(I).NE.0.) THEN
             IMIN=I
             GOTO 40
           ENDIF
        ENDDO

 40     CONTINUE



        IMAX=0

        DO I=NKR,1,-1
           IF(FCCNR(I).NE.0.) THEN
             IMAX=I
             GOTO 41
           ENDIF
        ENDDO

 41     CONTINUE
        RETURN
        END  SUBROUTINE BOUNDARY

        SUBROUTINE CRITICAL (AKOE,BKOE,TT,RCRITI,SUP1,DEG01)

        IMPLICIT NONE
        REAL AKOE,BKOE,TT,RCRITI,SUP1,DEG01
        REAL RO_SOLUTE
        PARAMETER (RO_SOLUTE=2.16)

         

        AKOE=3.3E-05/TT
        BKOE=2.*4.3/(22.9+35.5)

        BKOE=BKOE*(4./3.)*3.141593*RO_SOLUTE                  

        















        RCRITI=(AKOE/3.)*(4./BKOE/SUP1/SUP1)**DEG01
        RETURN
        END  SUBROUTINE CRITICAL
            
        SUBROUTINE CCNIMIN(IMIN,IMAX,RCRITI,NCRITI,RCCN,CCNCONC,COL, &
     &       FCCNR,NKR)

        IMPLICIT NONE
        INTEGER IMIN,II,IMAX,NCRITI,NKR
        REAL RCRITI,COL
        REAL RCCN(NKR),FCCNR(NKR),CCNCONC(NKR)
        REAL RCCN_MIN
        REAL DLN1,DLN2,FOLD_IP

        RCCN_MIN=RCCN(1)/10000.











          IF(RCRITI.LE.RCCN_MIN) THEN
            NCRITI=1
            DO II=NCRITI+1,IMAX
               CCNCONC(II)=COL*FCCNR(II)     
               FCCNR(II)=0.                  
            ENDDO
            GOTO 42
          ENDIF
          IF(RCRITI.GT.RCCN_MIN.AND.RCRITI.LT.RCCN(IMIN)) THEN
            NCRITI=1
            DO II=NCRITI+1,IMAX
               CCNCONC(II)=COL*FCCNR(II)
               FCCNR(II)=0.
            ENDDO
            DLN1=ALOG(RCRITI)-ALOG(RCCN_MIN)
            DLN2=ALOG(RCCN(1))-ALOG(RCRITI)
            CCNCONC(NCRITI)=DLN2*FCCNR(NCRITI)
            FCCNR(NCRITI)=FCCNR(NCRITI)*DLN1/(DLN1+DLN2)
            GOTO 42

          ENDIF

42       CONTINUE
     
         RETURN
         END SUBROUTINE CCNIMIN
        SUBROUTINE CCNLOOP(IMIN,IMAX,RCRITI,NCRITI,RCCN,CCNCONC,COL, &
     &       FCCNR,NKR)
        IMPLICIT NONE
         INTEGER I,IMIN,IMAX,NKR,II,NCRITI
         REAL COL
         REAL RCRITI,RCCN(NKR),CCNCONC(NKR),FCCNR(NKR)
         REAL DLN1,DLN2,FOLD_IP
        IF(IMIN.GT.1) THEN
          IF(RCRITI.LE.RCCN(IMIN-1)) THEN
            NCRITI=IMIN
            DO II=NCRITI,IMAX
               CCNCONC(II)=COL*FCCNR(II)
               FCCNR(II)=0.
            ENDDO
            GOTO 42
          ENDIF
          IF(RCRITI.LT.RCCN(IMIN).AND.RCRITI.GT.RCCN(IMIN-1)) &
     &    THEN

            NCRITI=IMIN
            
            DO II=NCRITI+1,IMAX
               CCNCONC(II)=COL*FCCNR(II)
               FCCNR(II)=0.
            ENDDO
            DLN1=ALOG(RCRITI)-ALOG(RCCN(IMIN-1))
            DLN2=COL-DLN1
            CCNCONC(NCRITI)=DLN2*FCCNR(NCRITI)
            FCCNR(NCRITI)=FCCNR(NCRITI)*DLN1/COL
            GOTO 42

          ENDIF

        ENDIF
        




      

         DO I=IMIN,IMAX-1
           IF(RCRITI.EQ.RCCN(I)) THEN
             NCRITI=I+1
             DO II=I+1,IMAX
                CCNCONC(II)=COL*FCCNR(II)
                FCCNR(II)=0.
             ENDDO
             GOTO 42
           ENDIF
           IF(RCRITI.GT.RCCN(I).AND.RCRITI.LT.RCCN(I+1)) THEN
             NCRITI=I+1
             IF(I.NE.IMAX-1) THEN
               DO II=NCRITI+1,IMAX
                  CCNCONC(II)=COL*FCCNR(II)
                  FCCNR(II)=0.
               ENDDO
             ENDIF
             DLN1=ALOG(RCRITI)-ALOG(RCCN(I))
             DLN2=COL-DLN1
             CCNCONC(NCRITI)=DLN2*FCCNR(NCRITI)
             FCCNR(NCRITI)=FCCNR(NCRITI)*DLN1/COL
             GOTO 42

           END IF
      

         ENDDO


  42    CONTINUE
        RETURN
        END  SUBROUTINE CCNLOOP
       SUBROUTINE ACTIVATE(IMIN,IMAX,AKOE,BKOE,RCCN,RACT,RACTMAX,NKR)
       IMPLICIT NONE

       INTEGER IMIN,IMAX,NKR
       INTEGER I,I0,I1
       REAL RCCN(NKR)
        REAL  R03,SUPCRITI,RACT(NKR),XKOE
        REAL AKOE,BKOE,AKOE23,RACTMAX

        DO I=IMIN,IMAX



           XKOE=(4./27.)*(AKOE**3/BKOE)
           AKOE23=AKOE*2./3.
           R03=RCCN(I)**3
           SUPCRITI=SQRT(XKOE/R03)



           IF(RCCN(I).LE.(0.3E-5)) &
     &     RACT(I)=AKOE23/SUPCRITI
           IF(RCCN(I).GT.(0.3E-5))&
     &     RACT(I)=5.*RCCN(I)
        ENDDO




        I0=IMIN

        DO I=IMIN,IMAX-1
           IF(RACT(I+1).LT.RACT(I)) THEN
             I0=I+1
             GOTO 45
           ENDIF
        ENDDO

 45     CONTINUE

        I1=I0-1


        IF(I0.EQ.IMIN) GOTO 47



        IF(I0.EQ.IMAX) THEN
          RACT(IMAX)=RACT(IMAX-1)
          GOTO 47
        ENDIF

        IF(RACT(IMAX).LE.RACT(I0-1)) THEN
          DO I=I0,IMAX
             RACT(I)=RACT(I0-1)
          ENDDO
          GOTO 47
        ENDIF







        DO I=I0+1,IMAX
           IF(RACT(I).GE.RACT(I0-1)) THEN
             I1=I
             GOTO 46
           ENDIF
        ENDDO
 46     CONTINUE






        DO I=I0,I1
           RACT(I)=RACT(I0-1)+(I-I0+1)*(RACT(I1)-RACT(I0-1)) &
     &                       /(I1-I0+1)
        ENDDO


  47    CONTINUE



        RACTMAX=0.

        DO I=IMIN,IMAX
           RACTMAX=AMAX1(RACTMAX,RACT(I))
	ENDDO
        RETURN

        END SUBROUTINE ACTIVATE
        SUBROUTINE DROPMAX(DROPRADII,RACTMAX,NDROPMAX,NKR)
        IMPLICIT NONE
        INTEGER IDROP,NKR,NDROPMAX
        REAL RACTMAX,DROPRADII(NKR)



        NDROPMAX=1

        DO IDROP=1,NKR
           IF(RACTMAX.LE.DROPRADII(IDROP)) THEN
             NDROPMAX=IDROP
             GOTO 44
           ENDIF
        ENDDO
 44     CONTINUE
        RETURN
        END  SUBROUTINE DROPMAX


        SUBROUTINE ONECOND1 &
     & (TT,QQ,PP,ROR &
     & ,VR1,PSINGLE &
     & ,DEL1N,DEL2N,DIV1,DIV2 &
     & ,FF1,PSI1,R1,RLEC,RO1BL &
     & ,AA1_MY,BB1_MY,AA2_MY,BB2_MY &
     & ,C1_MEY,C2_MEY &
     & ,COL,DTCOND,ICEMAX,NKR)

       IMPLICIT NONE


      INTEGER NKR,ICEMAX
      REAL    COL,VR1(NKR),PSINGLE &
     &       ,AA1_MY,BB1_MY,AA2_MY,BB2_MY &
     &       ,DTCOND

      REAL C1_MEY,C2_MEY
      INTEGER I_ABERGERON,I_BERGERON, &
     & KR,ICE,ITIME,KCOND,NR,NRM, &
     & KLIMIT, &
     & KM,KLIMITL  
      REAL AL1,AL2,D,GAM,POD, &
     & RV_MY,CF_MY,D_MYIN,AL1_MY,AL2_MY,ALC,DT0LREF,DTLREF, &
     & A1_MYN, BB1_MYN, A2_MYN, BB2_MYN,DT,DTT,XRAD, &
     & TPC1, TPC2, TPC3, TPC4, TPC5, &
     & EPSDEL, EPSDEL2,DT0L, DT0I,&
     & ROR, &
     & CWHUCM,B6,B8L,B8I, &
     & DEL1,DEL2,DEL1S,DEL2S, &
     & TIMENEW,TIMEREV,SFN11,SFN12, &
     & SFNL,SFNI,B5L,B5I,B7L,B7I,DOPL,DOPI,RW,RI,QW,PW, &
     & PI,QI,DEL1N0,DEL2N0,D1N0,D2N0,DTNEWL,DTNEWL1,D1N,D2N, &
     & DEL_R1,DT0L0,DT0I0, &
     & DTNEWL0, &
     & DTNEWL2 
       REAL DT_WATER_COND,DT_WATER_EVAP

       INTEGER K


      REAL  FF1_OLD(NKR),SUPINTW(NKR)
      DOUBLE PRECISION DSUPINTW(NKR),DD1N,DB11_MY,DAL1,DAL2
      DOUBLE PRECISION COL3,RORI,TPN,TPS,QPN,QPS,TOLD,QOLD &
     &                  ,FI1_K,FI2_K,FI3_K,FI4_K,FI5_K &
     &                  ,R1_K,R2_K,R3_K,R4_K,R5_K &
     &                  ,FI1R1,FI2R2,FI3R3,FI4R4,FI5R5 &
     &                  ,RMASSLAA,RMASSLBB,RMASSIAA,RMASSIBB &
     &                  ,ES1N,ES2N,EW1N,ARGEXP &
     &                  ,TT,QQ,PP &
     &                  ,DEL1N,DEL2N,DIV1,DIV2 &
     &                  ,OPER2,OPER3,AR1,AR2

       DOUBLE PRECISION DELMASSL1


                                                                       
        REAL R1(NKR) &
     &           ,RLEC(NKR),RO1BL(NKR) &
     &           ,FI1(NKR),FF1(NKR),PSI1(NKR) &
     &           ,B11_MY(NKR),B12_MY(NKR)





       
	REAL DTIMEO(NKR),DTIMEL(NKR) &
     &           ,TIMESTEPD(NKR)





	OPER2(AR1)=0.622/(0.622+0.378*AR1)/AR1
	OPER3(AR1,AR2)=AR1*AR2/(0.622+0.378*AR1)

        DATA AL1 /2500./, AL2 /2834./, D /0.211/ &
     &      ,GAM /1.E-4/, POD /10./ 
           
	DATA RV_MY,CF_MY,D_MYIN,AL1_MY,AL2_MY &
     &      /461.5,0.24E-1,0.211E-4,2.5E6,2.834E6/

	DATA A1_MYN, BB1_MYN, A2_MYN, BB2_MYN &
     &      /2.53,5.42,3.41E1,6.13/

	DATA TPC1, TPC2, TPC3, TPC4, TPC5 &
     &      /-4.0,-8.1,-12.7,-17.8,-22.4/ 


        DATA EPSDEL, EPSDEL2 /0.1E-03,0.1E-03/  
    
	DATA DT0L, DT0I /1.E20,1.E20/





        
        I_ABERGERON=0
        I_BERGERON=0
        COL3=3.0*COL
        ITIME=0
        KCOND=0
        DT_WATER_COND=0.4
        DT_WATER_EVAP=0.4
	ITIME=0
	KCOND=0
        DT0LREF=0.2
        DTLREF=0.4

	NR=NKR
	NRM=NKR-1
	DT=DTCOND
	DTT=DTCOND
	XRAD=0.


	CWHUCM=0.
	XRAD=0.
	B6=CWHUCM*GAM-XRAD
	B8L=1./ROR
	B8I=1./ROR
        RORI=1./ROR





        DO KR=1,NKR
           FF1_OLD(KR)=FF1(KR)
           SUPINTW(KR)=0.
           DSUPINTW(KR)=0.
        ENDDO





        TPN=TT
        QPN=QQ
        DO 19 KR=1,NKR
              FI1(KR)=FF1(KR)
19     CONTINUE


              TIMENEW=0.
              ITIME=0

              TOLD=TPN
              QOLD=QPN

   56         ITIME=ITIME+1
              TIMEREV=DT-TIMENEW
              TIMEREV=DT-TIMENEW
              DEL1=DEL1N
              DEL2=DEL2N
              DEL1S=DEL1N
              DEL2S=DEL2N
              TPS=TPN
              QPS=QPN

              CALL JERRATE(R1,TPS,PP,ROR,VR1,PSINGLE &
     &                    ,RLEC,RO1BL,B11_MY,B12_MY,1,1,ICEMAX,NKR)








              CALL JERTIMESC(FI1,R1,SFN11,SFN12 &
     &                      ,B11_MY,B12_MY,RLEC,B8L,1,COL,NKR)        


	      SFNL=SFN11+SFN12
	      SFNI=0.       


	      B5L=BB1_MY/TPS/TPS
	      B5I=BB2_MY/TPS/TPS
              B7L=B5L*B6                                                     
              B7I=B5I*B6
	      DOPL=1.+DEL1S                                                     
	      DOPI=1.+DEL2S                                                     
              RW=(OPER2(QPS)+B5L*AL1)*DOPL*SFNL                                                 
              RI=(OPER2(QPS)+B5L*AL2)*DOPL*SFNI
	      QW=B7L*DOPL
	      PW=(OPER2(QPS)+B5I*AL1)*DOPI*SFNL
              PI=(OPER2(QPS)+B5I*AL2)*DOPI*SFNI
              QI=B7I*DOPI





	      KCOND=10

	      IF(DEL1.GT.0) KCOND=11



	      IF(KCOND.EQ.11) THEN

                IF (DEL1N.EQ.0)THEN
	           DTNEWL=DT
                ELSE
                 DTNEWL=ABS(R1(ITIME)/(B11_MY(ITIME)*DEL1N &
     &                               -B12_MY(ITIME)))
	         IF(DTNEWL.GT.DT) DTNEWL=DT
                END IF
                IF(ITIME.GE.NKR) THEN
                call wrf_error_fatal3("<stdin>",5300,&
"fatal error in module_mp_full_sbm (ITIME.GE.NKR), model stop")
                ENDIF
                TIMESTEPD(ITIME)=DTNEWL




	        IF((TIMENEW+DTNEWL).GT.DT.AND.ITIME.LT.(NKR-1))  & 
     &          DTNEWL=DT-TIMENEW
                IF(ITIME.EQ.(NKR-1)) DTNEWL=DT-TIMENEW

	        TIMESTEPD(ITIME)=DTNEWL

	        TIMENEW=TIMENEW+DTNEWL

	        DTT=DTNEWL






	        CALL JERSUPSAT(DEL1,DEL2,DEL1N,DEL2N &
     &                        ,RW,PW,RI,PI,QW,QI &
     &                        ,DTT,D1N,D2N,DT0L,DT0I)






                                                         

	          CALL JERDFUN(R1,B11_MY,B12_MY &
     &                        ,FI1,PSI1,D1N &
     &                        ,1,1,COL,NKR,TPN)

	        IF((DEL1.GT.0.AND.DEL1N.LT.0) &
     &         .AND.ABS(DEL1N).GT.EPSDEL) THEN
                call wrf_error_fatal3("<stdin>",5340,&
"fatal error in module_mp_full_sbm (DEL1.GT.0.AND.DEL1N.LT.0), model stop")
	        ENDIF



	      ELSE




               IF (DEL1N.EQ.0)THEN
                DTIMEO(1)=DT
	        DO KR=2,NKR
	           DTIMEO(KR)=DT
	        ENDDO
               ELSE
	        DTIMEO(1)=-R1(1)/(B11_MY(1)*DEL1N-B12_MY(1))

	        DO KR=2,NKR
	           KM=KR-1
	           DTIMEO(KR)=(R1(KM)-R1(KR))/(B11_MY(KR)*DEL1N &
     &                                       -B12_MY(KR))
	        ENDDO
               END IF

	        KLIMIT=1

	        DO KR=1,NKR
	           IF(DTIMEO(KR).GT.TIMEREV) GOTO 55
	           KLIMIT=KR
	        ENDDO

   55           KLIMIT=KLIMIT-1

	        IF(KLIMIT.LT.1) KLIMIT=1



  	        DTNEWL1=AMIN1(DTIMEO(3),TIMEREV)
                IF(DTNEWL1.LT.DTLREF) DTNEWL1=AMIN1(DTLREF,TIMEREV)
	        DTNEWL=DTNEWL1
	        IF(ITIME.GE.NKR) THEN
                call wrf_error_fatal3("<stdin>",5383,&
"fatal error in module_mp_full_sbm (ITIME.GE.NKR), model stop")
	        ENDIF

	        TIMESTEPD(ITIME)=DTNEWL



	        IF(DTNEWL.GT.DT) DTNEWL=DT
                IF((TIMENEW+DTNEWL).GT.DT.AND.ITIME.LT.(NKR-1))  &
     &          DTNEWL=DT-TIMENEW
                IF(ITIME.EQ.(NKR-1)) DTNEWL=DT-TIMENEW

	        TIMESTEPD(ITIME)=DTNEWL

	        TIMENEW=TIMENEW+DTNEWL

	        DTT=DTNEWL






	        CALL JERSUPSAT(DEL1,DEL2,DEL1N,DEL2N &
     &                        ,RW,PW,RI,PI,QW,QI &
     &                        ,DTT,D1N,D2N,DT0L0,DT0I0)







                                                         

             
 	          CALL JERDFUN(R1,B11_MY,B12_MY &
     &                        ,FI1,PSI1,D1N &
     &                        ,1,1,COL,NKR,TPN)






	        IF((DEL1.LT.0.AND.DEL1N.GT.0) &
     &         .AND.ABS(DEL1N).GT.EPSDEL) THEN
                call wrf_error_fatal3("<stdin>",5431,&
"fatal error in module_mp_full_sbm (DEL1.LT.0.AND.DEL1N.GT.0), model stop")
	        ENDIF





              ENDIF






      RMASSLBB=0.
      RMASSLAA=0.



              DO K=1,NKR
                 FI1_K=FI1(K)
                 R1_K=R1(K)
                 FI1R1=FI1_K*R1_K*R1_K
                 RMASSLBB=RMASSLBB+FI1R1
              ENDDO
              RMASSLBB=RMASSLBB*COL3*RORI

              IF(RMASSLBB.LE.0.) RMASSLBB=0.
              DO K=1,NKR
                 FI1_K=PSI1(K)
                 R1_K=R1(K)
                 FI1R1=FI1_K*R1_K*R1_K
                 RMASSLAA=RMASSLAA+FI1R1
              ENDDO
              RMASSLAA=RMASSLAA*COL3*RORI
              IF(RMASSLAA.LE.0.) RMASSLAA=0.

              DELMASSL1=RMASSLAA-RMASSLBB
              QPN=QPS-DELMASSL1
              DAL1=AL1
              TPN=TPS+DAL1*DELMASSL1

              ARGEXP=-BB1_MY/TPN
              ES1N=AA1_MY*DEXP(ARGEXP)
              ARGEXP=-BB2_MY/TPN
              ES2N=AA2_MY*DEXP(ARGEXP)
              EW1N=OPER3(QPN,PP)
              IF(ES1N.EQ.0)THEN
               DEL1N=0.5
               DIV1=1.5
              ELSE
               DIV1=EW1N/ES1N
               DEL1N=EW1N/ES1N-1.
              END IF
              IF(ES2N.EQ.0)THEN
               DEL2N=0.5
               DIV2=1.5
              ELSE
               DEL2N=EW1N/ES2N-1.
               DIV2=EW1N/ES2N
              END IF
              DO KR=1,NKR
                SUPINTW(KR)=SUPINTW(KR)+B11_MY(KR)*D1N
                DD1N=D1N
                DB11_MY=B11_MY(KR)
                DSUPINTW(KR)=DSUPINTW(KR)+DB11_MY*DD1N
              ENDDO

	      IF(TIMENEW.LT.DT) GOTO 56
57            CONTINUE
              CALL JERDFUN_NEW(R1,DSUPINTW &
     &                        ,FF1_OLD,PSI1,D1N &
     &                        ,1,1,COL,NKR,TPN)
              RMASSLAA=0.0
              RMASSLBB=0.0

              DO K=1,NKR
                 FI1_K=FF1_OLD(K)
                 R1_K=R1(K)
                 FI1R1=FI1_K*R1_K*R1_K
                 RMASSLBB=RMASSLBB+FI1R1
              ENDDO
              RMASSLBB=RMASSLBB*COL3*RORI

              IF(RMASSLBB.LT.0.0) RMASSLBB=0.0

              DO K=1,NKR
                 FI1_K=PSI1(K)
                 R1_K=R1(K)
                 FI1R1=FI1_K*R1_K*R1_K
                 RMASSLAA=RMASSLAA+FI1R1
              ENDDO
              RMASSLAA=RMASSLAA*COL3*RORI

              IF(RMASSLAA.LT.0.0) RMASSLAA=0.0
              IF(RMASSLAA.LT.0.0) RMASSLAA=0.0

              DELMASSL1=RMASSLAA-RMASSLBB

              QPN=QOLD-DELMASSL1
              DAL1 = AL1
              TPN=TOLD+DAL1*DELMASSL1


              ARGEXP=-BB1_MY/TPN
              ES1N=AA1_MY*DEXP(ARGEXP)
              ARGEXP=-BB2_MY/TPN
              ES2N=AA2_MY*DEXP(ARGEXP)
              EW1N=OPER3(QPN,PP)
              IF(ES1N.EQ.0)THEN
               DEL1N=0.5
               DIV1=1.5
              call wrf_error_fatal3("<stdin>",5544,&
"fatal error in module_mp_full_sbm (ES1N.EQ.0), model stop")
              ELSE
               DIV1=EW1N/ES1N
               DEL1N=EW1N/ES1N-1.
              END IF
              IF(ES2N.EQ.0)THEN
               DEL2N=0.5
               DIV2=1.5
              call wrf_error_fatal3("<stdin>",5553,&
"fatal error in module_mp_full_sbm (ES2N.EQ.0), model stop")
              ELSE
               DEL2N=EW1N/ES2N-1.
               DIV2=EW1N/ES2N
              END IF
        TT=TPN
        QQ=QPN
	DO KR=1,NKR
	   FF1(KR)=PSI1(KR)
	ENDDO




       RETURN


  END SUBROUTINE ONECOND1





        SUBROUTINE JERDFUN(R2,B21_MY,B22_MY &
     &                    ,FI2,PSI2,DEL2N &
     &                    ,IND,ITYPE,COL,NKR,TPN)
       IMPLICIT NONE


       REAL COL,DEL2N
                                                                       
      INTEGER IND,ITYPE,KR,ICE,ITYP,NRM,NR,NKR,IDROP
       REAL &
     &       R2(NKR,IND),R2N(NKR,IND) &
     &      ,FI2(NKR,IND),PSI2(NKR,IND) &
     &      ,B21_MY(NKR,IND),B22_MY(NKR,IND) &
     &      ,DEL_R2M(NKR,IND)
        DOUBLE PRECISION R2R(NKR),R2NR(NKR),FI2R(NKR),PSI2R(NKR)
        DOUBLE PRECISION DR2(NKR,IND),DR2N(NKR,IND),DDEL2N, &
     &     DB21_MY(NKR,IND)
       DOUBLE PRECISION CHECK,TPN
          CHECK=0.D0
           DO KR=1,NKR
             CHECK=B21_MY(1,1)*B21_MY(KR,1)
             IF (CHECK.LT.0) call wrf_error_fatal3("<stdin>",5598,&
"fatal error in module_mp_full_sbm (CHECK.LT.0), model stop")
           END DO

	IF(IND.NE.1) THEN
	  ITYP=ITYPE
        ELSE
	  ITYP=1
	ENDIF

           DDEL2N=DEL2N
	DO KR=1,NKR
	   PSI2R(KR)=FI2(KR,ITYP)
	   FI2R(KR)=FI2(KR,ITYP)
           DR2(KR,ITYP)=R2(KR,ITYP)
           DB21_MY(KR,ITYP)=B21_MY(KR,ITYP)
	ENDDO


	NR=NKR
	NRM=NKR-1



	  DO 8 ICE=1,IND
	       IF(ITYP.EQ.ICE) THEN
	          DO KR=1,NKR
                    DR2N(KR,ICE)=DR2(KR,ICE)+DDEL2N*DB21_MY(KR,ICE)
                    R2N(KR,ICE)=DR2N(KR,ICE)







	          ENDDO
	        ENDIF
    8	  CONTINUE

                                                          
	  DO ICE=1,IND


	     IF(ITYP.EQ.ICE) THEN

               DO 5 KR=1,NKR
	            R2R(KR)=DR2(KR,ICE)
	            R2NR(KR)=DR2N(KR,ICE)               
    5         continue



             IF(IND.EQ.1.AND.ITYPE.EQ.1) IDROP=1

             CALL JERNEWF(NR,NRM,R2R,FI2R,PSI2R,R2NR,COL,NKR &

     &                   ,IDROP,TPN)





	       DO KR=1,NKR                              
	          PSI2(KR,ICE)=PSI2R(KR)
	       ENDDO




	     ENDIF


                                                          
	  ENDDO




	RETURN
	END SUBROUTINE JERDFUN

        SUBROUTINE JERDFUN_NEW(R2,B21_MY &
     &                    ,FI2,PSI2,DEL2N &
     &                    ,IND,ITYPE,COL,NKR,TPN)
       IMPLICIT NONE


       REAL COL,DEL2N
                                                                       
      INTEGER IND,ITYPE,KR,ICE,ITYP,NRM,NR,KK,NKR,IDROP
       REAL &
     &       R2(NKR,IND),R2N(NKR,IND) &
     &      ,FI2(NKR,IND),PSI2(NKR,IND)
       DOUBLE PRECISION TPN
       DOUBLE PRECISION  B21_MY(NKR,IND)
        DOUBLE PRECISION R2R(NKR),R2NR(NKR),FI2R(NKR),PSI2R(NKR)
        DOUBLE PRECISION DR2(NKR,IND),DR2N(NKR,IND),DDEL2N, &
     &     DB21_MY(NKR,IND)
	IF(IND.NE.1) THEN
	  ITYP=ITYPE
        ELSE
	  ITYP=1
	ENDIF

           DDEL2N=DEL2N
	DO KR=1,NKR
	   PSI2R(KR)=FI2(KR,ITYP)
	   FI2R(KR)=FI2(KR,ITYP)
           DR2(KR,ITYP)=R2(KR,ITYP)
	ENDDO


	NR=NKR
	NRM=NKR-1




	  DO ICE=1,IND

	     IF(ITYP.EQ.ICE) THEN
               DO 5 KR=1,NKR
	            R2R(KR)=DR2(KR,ICE)
	            R2NR(KR)=DR2(KR,ICE)+B21_MY(KR,ICE)
                    R2N(KR,ICE)=R2NR(KR)






    5         continue

             IDROP=1

             CALL JERNEWF(NR,NRM,R2R,FI2R,PSI2R,R2NR,COL,NKR &
     &                   ,IDROP,TPN)




	       DO KR=1,NKR                              
	          PSI2(KR,ICE)=PSI2R(KR)
	       ENDDO



	     ENDIF


                                                          
	  ENDDO




	RETURN
	END SUBROUTINE JERDFUN_NEW





        SUBROUTINE JERNEWF &
       (NRX,NRM,RR,FI_OLD,PSI,RN,COL,NKR, &

        IDROP,TPN)

 
        IMPLICIT NONE
	


        INTEGER &
	KRDROP_REMAP_MIN,KRDROP_REMAP_MAX,IDROP,KMAX
        INTEGER NRX
	
        DOUBLE PRECISION &
	COEFF_REMAP,TPN
	
        DOUBLE PRECISION & 
        CDROP(NRX),DELTA_CDROP(NRX)
		


        INTEGER  & 
        I,K,KM,NRXP,IM,IP,IFIN,IIN,ISYM,NKR
 
        REAL & 
        COL

        DOUBLE PRECISION &
	AOLDCON,ANEWCON,AOLDMASS,ANEWMASS

        DOUBLE PRECISION &
        RNTMP,RRTMP,RRP,RRM,RNTMP2,RRTMP2,RRP2,RRM2, &
        GN1,GN1P,GN2,GN3,GMAT2

        DOUBLE PRECISION &
        DRP,FNEW,FIK,PSINEW,DRM,GMAT,R1,R2,R3,DMASS,CONCL,RRI,RNK

        INTEGER NRM

        DOUBLE PRECISION & 
        RR(NRX),FI(NRX),PSI(NRX),RN(NRX) &
       ,RRS(NKR+1),RNS(NKR+1),PSIN(NKR+1),FIN(NKR+1)

        DOUBLE PRECISION & 
        FI_OLD(NRX)


        DOUBLE PRECISION & 
        PSI_IM,PSI_I,PSI_IP





       IF(TPN.LT.273.15-7.0D0) IDROP=0


 


        KRDROP_REMAP_MIN=8
	KRDROP_REMAP_MAX=13 
	
        COEFF_REMAP=1.0D0/150.0D0 
	       	

	


	NRXP=NRX+1

	DO K=1,NRX
	   FI(K)=FI_OLD(K)
        ENDDO
 
	DO K=1,NRX
	   PSI(K)=0.0D0
        ENDDO



	IF(RN(NRX).NE.RR(NRX)) THEN






	  ISYM=1

	  IF(RN(1).LT.RR(1)) ISYM=-1



	  IF(ISYM.GT.0) THEN
	


	    RNS(NRXP)=1024.0D0*RR(NRX)
	    RRS(NRXP)=1024.0D0*RR(NRX)

  	    PSIN(NRXP)=0.0D0
	    FIN(NRXP)=0.0D0

	    DO K=1,NRX
	       RNS(K)=RN(K)
	       RRS(K)=RR(K)
	       PSIN(K)=0.0D0

	       FIN(K)=3.0D0*FI(K)*RR(K)*COL
	    ENDDO





	    RNK=RNS(1)

	    DO I=1,NRX
	       RRI=RRS(I)
	       IF(RRI.GT.RNK) GOTO 3
            ENDDO

    3	    IIN=I-1

	    IFIN=NRX

	    CONCL=0.0D0
            DMASS=0.0D0
                        
            DO 6 I=IIN,IFIN

                 IP=I+1
                                                                                
                 IM=MAX(1,I-1)

	         R1=RRS(IM)
	         R2=RRS(I)
	         R3=RRS(IP)

	         DRM=R2-R1
	         DRP=R3-R2

	         FNEW=0.0D0

	         DO 7 K=1,I
                 
	              FIK=FIN(K)

	              IF(FIK.NE.0.0D0) THEN

	                KM=K-1



	                RNK=RNS(K)

	                IF(RNK.NE.R2) THEN
	                  GMAT=0.0D0
	                  IF(RNK.GT.R1.AND.RNK.LT.R3) THEN
	                    IF(RNK.LT.R2) THEN
	                      GMAT=(RNK-R1)/DRM
		            ELSE
	                      GMAT=(R3-RNK)/DRP
	                    ENDIF
	                  ENDIF
	                ELSE
	                  GMAT=1.0D0
	                ENDIF

                        FNEW=FNEW+FIK*GMAT

	              ENDIF
                 
   7	         CONTINUE

	         CONCL=CONCL+FNEW

	         DMASS=DMASS+FNEW*R2



    	         PSIN(I)=FNEW
                        	
   6        CONTINUE


 



	    DO K=1,NRX
	       PSI(K)=PSIN(K)/3./RR(K)/COL
	    ENDDO


	
          ELSE



	    RNS(1)=0.0D0
	    RRS(1)=0.0D0
	    FIN(1)=0.0D0
	    PSIN(1)=0.0D0



	    DO K=2,NRXP
	       KM=K-1
	       RNS(K)=RN(KM)
	       RRS(K)=RR(KM)
	       PSIN(K)=0.0D0
	       FIN(K)=3.0D0*FI(KM)*RR(KM)*COL
	    ENDDO

	    DO I=1,NRXP

               IM=MAX(I-1,1)
               IP=MIN(I+1,NRXP)

   	       R1=RRS(IP)
	       R2=RRS(I)
	       R3=RRS(IM)

               DRM=R1-R2
               DRP=R2-R3

	       FNEW=0.0D0

	       DO K=I,NRXP
	          RNK=RNS(K)
                  IF(RNK.GE.R1) GOTO 4321
                  IF(RNK.GT.R3)THEN
                    IF(RNK.GT.R2) THEN
                      FNEW=FNEW+FIN(K)*(R1-RNK)/DRM
                    ELSE
                      FNEW=FNEW+FIN(K)*(RNK-R3)/DRP
	            ENDIF
	          ENDIF
               ENDDO

 4321          CONTINUE



    	       PSIN(I)=FNEW
	
            ENDDO






	    DO K=2,NRXP
	       KM=K-1
	       R1=PSIN(K)*RR(KM)
	       PSINEW=PSIN(K)/3.0D0/RR(KM)/COL
	       IF(R1.LT.1.0D-20) PSINEW=0.0D0
	       PSI(KM)=PSINEW
	    ENDDO





	  ENDIF
	

          IF(I3POINT.NE.0.AND.ISYM.GT.0) THEN




	    DO K=1,NKR
	       RRS(K)=RR(K)
	    ENDDO

            RRS(NKR+1)=RRS(NKR)*1024.0D0

	    DO I=1,NKR
 
               PSI(I)=PSI(I)*RR(I)




               IF(RN(I).LT.0.0D0) THEN 
                 RN(I)=1.0D-50
	         FI(I)=0.0D0
               ENDIF

            ENDDO
 
	    DO K=1,NKR

               IF(FI(K).NE.0.0D0) THEN

                 IF(RRS(2).LT.RN(K)) THEN
 
                   I=2

                   DO  WHILE &
                     (.NOT.(RRS(I).LT.RN(K).AND.RRS(I+1).GT.RN(K)) &
                      .AND.I.LT.NKR)
                       I=I+1
	           ENDDO


                   IF(I.LT.NKR-2) THEN


                     RNTMP=RN(K)

                     RRTMP=RRS(I)
                     RRP=RRS(I+1)
                     RRM=RRS(I-1)
 
                     RNTMP2=RN(K+1)

                     RRTMP2=RRS(I+1)
                     RRP2=RRS(I+2)
                     RRM2=RRS(I)
 
                     GN1=(RRP-RNTMP)*(RRTMP-RNTMP)/(RRP-RRM)/ &
                       (RRTMP-RRM)

                     GN1P=(RRP2-RNTMP2)*(RRTMP2-RNTMP2)/ &
                        (RRP2-RRM2)/(RRTMP2-RRM2)

                     GN2=(RRP-RNTMP)*(RNTMP-RRM)/(RRP-RRTMP)/ &
                       (RRTMP-RRM)
 
	             GMAT=(RRP-RNTMP)/(RRP-RRTMP)


                     GN3=(RRTMP-RNTMP)*(RRM-RNTMP)/(RRP-RRM)/ &
                                                 (RRP-RRTMP)
	             GMAT2=(RNTMP-RRTMP)/(RRP-RRTMP)

                     PSI_IM=PSI(I-1)+GN1*FI(K)*RR(K)



                     PSI_I=PSI(I)+GN1P*FI(K+1)*RR(K+1)+&
	                         (GN2-GMAT)*FI(K)*RR(K)

                     PSI_IP=PSI(I+1)+(GN3-GMAT2)*FI(K)*RR(K)
                    
                     IF(PSI_IM.GT.0.0D0) THEN

                       IF(PSI_IP.GT.0.0D0) THEN

                         IF(I.GT.2) THEN

                           IF(PSI_IM.GT.PSI(I-2).AND.PSI_IM.LT.PSI_I &
                          .AND.PSI(I-2).LT.PSI(I).OR.PSI(I-2) &
                          .GE.PSI(I)) THEN

                             PSI(I-1)=PSI_IM

                             PSI(I)=PSI(I)+FI(K)*RR(K)*(GN2-GMAT)

                             PSI(I+1)=PSI_IP



                           ENDIF 

                         ENDIF



	               ENDIF



	             ENDIF



                   ENDIF




                 ENDIF
 


               ENDIF

 1000          CONTINUE

	    ENDDO

	    AOLDCON=0.0D0
	    ANEWCON=0.0D0
	    AOLDMASS=0.0D0
	    ANEWMASS=0.0D0

	    DO K=1,NKR
	       AOLDCON=AOLDCON+FI(K)*RR(K)
	       ANEWCON=ANEWCON+PSI(K)
	       AOLDMASS=AOLDMASS+FI(K)*RR(K)*RN(K)
	       ANEWMASS=ANEWMASS+PSI(K)*RR(K)
	    ENDDO






	    DO K=1,NKR
	       PSI(K)=PSI(K)/RR(K)
            ENDDO
	  




								     	       

								     		    
	  ENDIF



          IF(IDROP.NE.0.AND.ISYM.GT.0) THEN
	  
	    DO K=KRDROP_REMAP_MIN,KRDROP_REMAP_MAX
	       CDROP(K)=3.0D0*COL*PSI(K)*RR(K)
	    ENDDO
								     		 



            DO K=KRDROP_REMAP_MAX,KRDROP_REMAP_MIN,-1
               KMAX=K
               IF(PSI(K).GT.0.0D0) GOTO 2011
            ENDDO

 2011       CONTINUE
 




            DO K=KMAX-1,KRDROP_REMAP_MIN,-1

	       IF(CDROP(K).GT.1.d-20) THEN
                 DELTA_CDROP(K)=CDROP(K+1)/CDROP(K)
	         IF(DELTA_CDROP(K).LT.COEFF_REMAP) THEN
	           CDROP(K)=CDROP(K)+CDROP(K+1)
		   CDROP(K+1)=0.0D0
	         ENDIF
	       ENDIF
            ENDDO
	    
	    DO K=KRDROP_REMAP_MIN,KMAX
	       PSI(K)=CDROP(K)/(3.0D0*COL*RR(K))
	    ENDDO
	    

		      
	  ENDIF
	    	  






        ELSE



	  DO K=1,NKR
	     PSI(K)=FI(K)
	  ENDDO

        ENDIF




        RETURN 


        END SUBROUTINE JERNEWF


        SUBROUTINE JERRATEOLD(R1S,TP,PP,ROR,VR1,PSINGLE,RIEC,RO1BL &
     &                    ,B11_MY,B12_MY,ID,IN,ICEMAX,NKR)
       IMPLICIT NONE
       INTEGER ID,IN,KR,ICE,NRM,ICEMAX,NKR
      DOUBLE PRECISION TP,PP
      REAL DETL,FACTPL,VENTPL,VR1K,CONSTL,RO1,RVT,D_MY, &
     & CONST
       REAL VR1(NKR,ID),PSINGLE,ROR
        REAL       &
     & R1S(NKR,ID),B11_MY(NKR,ID),B12_MY(NKR,ID) &
     &,RO1BL(NKR,ID),RIEC(NKR,ID) &
     &,VR1KL(NKR,ICEMAX),VENTRL(NKR,ICEMAX) &
     &,FD1(NKR,ICEMAX),FK1(NKR,ICEMAX),FACTRL(NKR,ICEMAX) &
     &,R11_MY(NKR,ICEMAX),R12_MY(NKR,ICEMAX) &
     &,R1_MY1(NKR,ICEMAX),R1_MY2(NKR,ICEMAX),R1_MY3(NKR,ICEMAX) &
     &,AL1(2),AL1_MY(2),A1_MY(2),BB1_MY(2),ESAT1(2),CONSTLI(ICEMAX)
      DOUBLE PRECISION TZERO
      REAL PZERO,CF_MY,D_MYIN,RV_MY
      PARAMETER (TZERO=273.150,PZERO=1.013E6)
      DATA AL1/2500.,2833./
	CONST=12.566372
        AL1_MY(1)=2.5E10
        AL1_MY(2)=2.834E10
        A1_MY(1)=2.53E12
        A1_MY(2)=3.41E13
        BB1_MY(1)=5.42E3
        BB1_MY(2)=6.13E3
        CF_MY=2.4E3
        D_MYIN=0.221
        RV_MY=461.5E4
	NRM=NKR-1



	D_MY=D_MYIN*(PZERO/PP)*(TP/TZERO)**1.94
	RVT=RV_MY*TP
	ESAT1(IN)=A1_MY(IN)*EXP(-BB1_MY(IN)/TP)

	DO 1 ICE=1,ID
	     DO 1 KR=1,NKR
	     RO1=RO1BL(KR,ICE)
	     CONSTL=CONST*RIEC(KR,ICE)
	     CONSTLI(ICE)=CONSTL
	     VR1K=0.
	     VR1KL(KR,ICE)=VR1K
	     VENTPL=1.
	     VENTRL(KR,ICE)=VENTPL
	     FACTPL=1.
	     FACTRL(KR,ICE)=FACTPL
	     FD1(KR,ICE)=RVT/D_MY/ESAT1(IN)/FACTPL
	     FK1(KR,ICE)=(AL1_MY(IN)/RVT-1.)*AL1_MY(IN)/CF_MY/TP
	     R1_MY1(KR,ICE)=VENTPL*CONSTL
	     R11_MY(KR,ICE)=R1_MY1(KR,ICE)







	     DETL=FK1(KR,ICE)+FD1(KR,ICE)
	     B11_MY(KR,ICE)=R11_MY(KR,ICE)/DETL

           B12_MY(KR,ICE)=0                       
    1	CONTINUE

	RETURN
	END SUBROUTINE JERRATEOLD








        SUBROUTINE JERRATE(R1S,TP,PP,ROR,VR1,PSINGLE,RIEC,RO1BL &
     &                    ,B11_MY,B12_MY,ID,IN,ICEMAX,NKR)
       IMPLICIT NONE
       INTEGER ID,IN,KR,ICE,NRM,ICEMAX,NKR
      DOUBLE PRECISION TP,PP
      REAL DETL,FACTPL,VENTPL,VR1K,CONSTL,RO1,RVT,D_MY, &
     & CONST
        REAL VR1(NKR,ID),PSINGLE &
     &,R1S(NKR,ID),B11_MY(NKR,ID),B12_MY(NKR,ID) &
     &,RO1BL(NKR,ID),RIEC(NKR,ID) &
     &,VR1KL(NKR,ICEMAX),VENTRL(NKR,ICEMAX) &
     &,FD1(NKR,ICEMAX),FK1(NKR,ICEMAX),FACTRL(NKR,ICEMAX) &
     &,R11_MY(NKR,ICEMAX),R12_MY(NKR,ICEMAX) &
     &,R1_MY1(NKR,ICEMAX),R1_MY2(NKR,ICEMAX),R1_MY3(NKR,ICEMAX) &
     &,AL1(2),AL1_MY(2),A1_MY(2),BB1_MY(2),ESAT1(2),CONSTLI(ICEMAX)
      DOUBLE PRECISION TZERO
      REAL PZERO,CF_MY,D_MYIN,RV_MY,DEG01,DEG03
      REAL COEFF_VISCOUS,SHMIDT_NUMBER,A,B
      REAL REINOLDS_NUMBER,RESHM,ROR
      PARAMETER (TZERO=273.150,PZERO=1.013E6)
      DATA AL1/2500.,2833./
        DEG01=1./3.     
        DEG03=1./3.     
	CONST=12.566372
        AL1_MY(1)=2.5E10
        AL1_MY(2)=2.834E10
        A1_MY(1)=2.53E12
        A1_MY(2)=3.41E13
        BB1_MY(1)=5.42E3
        BB1_MY(2)=6.13E3
        CF_MY=2.4E3
        D_MYIN=0.221
        RV_MY=461.5E4
	NRM=NKR-1


        D_MY=D_MYIN*(PZERO/PP)*(TP/TZERO)**1.94


        COEFF_VISCOUS=1.72E-2*SQRT(TP/273.)*393./(TP-120.)/ROR

        SHMIDT_NUMBER=COEFF_VISCOUS/D_MY

        A=2.*(3./4./3.141593)**DEG01
        B=A/COEFF_VISCOUS
        
        RVT=RV_MY*TP
        ESAT1(IN)=A1_MY(IN)*EXP(-BB1_MY(IN)/TP)
        DO ICE=1,ID
           DO KR=1,NKR

              REINOLDS_NUMBER= &
     &        B*VR1(KR,ICE)*SQRT(1.E6/PSINGLE)* &
     &        (R1S(KR,ICE)/RO1BL(KR,ICE))**DEG03
              RESHM=SQRT(REINOLDS_NUMBER)*SHMIDT_NUMBER**DEG03
              IF(REINOLDS_NUMBER.LT.2.5) THEN
                VENTPL=1.+0.108*RESHM*RESHM
              ELSE
                VENTPL=0.78+0.308*RESHM
              ENDIF

              CONSTL=CONST*RIEC(KR,ICE)                         
              CONSTLI(ICE)=CONSTL






              FACTPL=1.                                         
              FACTRL(KR,ICE)=FACTPL                             
              FD1(KR,ICE)=RVT/D_MY/ESAT1(IN)/FACTPL             
              FK1(KR,ICE)=(AL1_MY(IN)/RVT-1.)*AL1_MY(IN)/CF_MY/TP
              R1_MY1(KR,ICE)=VENTPL*CONSTL


              R11_MY(KR,ICE)=R1_MY1(KR,ICE)


              DETL=FK1(KR,ICE)+FD1(KR,ICE)
              B11_MY(KR,ICE)=R11_MY(KR,ICE)/DETL

              B12_MY(KR,ICE)=0.
           ENDDO
        ENDDO


	RETURN
	END SUBROUTINE JERRATE






	SUBROUTINE JERSUPSAT(DEL1,DEL2,DEL1N,DEL2N &
     &                      ,RW,PW,RI,PI,QW,QI &
     &                      ,DT,DEL1INT,DEL2INT,DT0L,DT0I)
      IMPLICIT NONE
   
      INTEGER ITYPE
      REAL DEL1,DEL2,RW,PW,RI,PI,QW,QI, &
     &  DT,DEL1INT,DEL2INT,DT0L,DT0I,DTLIN,DTIIN
      REAL DETER,DBLRW,DBLPW,DBLPI,DBLRI, &
     &  DBLDEL1,DBLDEL2,DBLDEL1INT,DBLDTLIN,DBLDTIIN, &
     &  EXPM,EXPP,ALFAMX,ALFAPX,X,ALFA,DELX,DBLDEL2INT, &
     &  R1RES,R2RES,R1,R2,R3,R4,R21,R11,R10,R41,R31,R30,DBLDT, &
     &  DBLDEL1N,DBLDEL2N
      DOUBLE PRECISION DEL1N,DEL2N

        DOUBLE PRECISION DEL1N_2P,DEL1INT_2P,DEL2N_2P,DEL2INT_2P 
        DOUBLE PRECISION EXPP_2P,EXPM_2P,ARGEXP     

      DOUBLE PRECISION RW_DP,PW_DP,PI_DP,RI_DP,X_DP,ALFA_DP


      DOUBLE PRECISION  EXPM1
      EXPM1(x_dp)= &
     &x_dp+x_dp*x_dp/2.0D0+x_dp*x_dp*x_dp/6.0D0+x_dp*x_dp*x_dp*x_dp/24.0D0+x_dp*x_dp*x_dp*x_dp*x_dp/120.0D0
      DOUBLE PRECISION  DETER_MIN


      DOUBLE PRECISION EXP1, EXP2


	DTLIN=1000.E17
	DTIIN=1000.E17

      DETER=RW*PI-PW*RI




       IF(RW.EQ.0.AND.RI.EQ.0) THEN

	    DEL1N_2P=DEL1
	    DEL2N_2P=DEL2
	    DEL1INT_2P=DEL1*DT
	    DEL2INT_2P=DEL2*DT

       ELSE IF(RW.NE.0.AND.RI*1.E5.LT.RW) THEN

              ARGEXP=-RW*DT

	      DEL1N_2P=DEL1*DEXP(ARGEXP)+QW*(1.-DEXP(ARGEXP))
	      DEL1INT_2P=(DEL1-DEL1N_2P)/RW
	      DEL2N_2P=DEL2-PW*DEL1INT_2P
	      DEL2INT_2P= &
     &       (DEL2N_2P-PW*DEL1N_2P/RW)*DT+PW*DEL1INT_2P/RW
	ELSE IF(RI.NE.0.AND.RW*1.E5.LT.RI) THEN


              ARGEXP=-PI*DT

	      DEL2N_2P=DEL2*DEXP(ARGEXP)+QI*(1.-DEXP(ARGEXP))
	      DEL2INT_2P=(DEL2-DEL2N_2P)/PI
	      DEL1N_2P=DEL1-RI*DEL2INT_2P
	      DEL1INT_2P= &
     &       (DEL1N_2P-RI*DEL2N_2P/PI)*DT+RI*DEL2INT_2P/PI



        ELSE






          RW_DP=RW
          RI_DP=RI
          PI_DP=PI
          PW_DP=PW
          IF (RW.LE.0)PRINT*,'RW = ',RW
          IF (PW.LE.0)PRINT*,'PW = ',PW
          IF (RI.LE.0)PRINT*,'RI = ',RI
          IF (PI.LE.0)PRINT*,'PI = ',PI
          IF (RW.LE.0) call wrf_error_fatal3("<stdin>",6509,&
"fatal error in module_mp_full_sbm (RW.LE.0), model stop")
          IF (PW.LE.0) call wrf_error_fatal3("<stdin>",6511,&
"fatal error in module_mp_full_sbm (PW.LE.0), model stop")
          IF (RI.LE.0) call wrf_error_fatal3("<stdin>",6513,&
"fatal error in module_mp_full_sbm (RI.LE.0), model stop")
          IF (PI.LE.0) call wrf_error_fatal3("<stdin>",6515,&
"fatal error in module_mp_full_sbm (PI.LE.0), model stop")
          ALFA_DP=SQRT((RW_DP-PI_DP)*(RW_DP-PI_DP)+4.*PW_DP*RI_DP) 
	  X_DP=RW_DP+PI_DP
	  ALFAPX=.5*(ALFA_DP+X_DP)
          IF (ALFAPX.LE.0) call wrf_error_fatal3("<stdin>",6520,&
"fatal error in module_mp_full_sbm (ALFAPX.LE.0), model stop") 
	  ALFAMX=.5*(ALFA_DP-X_DP)


          ARGEXP=-ALFAPX*DT

	  EXPP_2P=DEXP(ARGEXP)
          IF(DABS(ARGEXP).LE.1.0E-6) THEN
               EXP1=EXPM1(ARGEXP)
          ELSE
               EXP1=EXPP_2P-1.0D0
          ENDIF

          ARGEXP=ALFAMX*DT

	  EXPM_2P=DEXP(ARGEXP)
              IF(DABS(ARGEXP).LE.1.0E-6) THEN
                EXP2=EXPM1(ARGEXP)
              ELSE
                EXP2=EXPM_2P-1.0D0
              ENDIF


	  R10=RW*DEL1+RI*DEL2
	  R11=R10-ALFAPX*DEL1
	  R21=R10+ALFAMX*DEL1
	  DEL1N_2P=(R21*EXPP_2P-R11*EXPM_2P)/ALFA_DP

	  IF(ALFAMX.NE.0) THEN
	    R1=-R11/ALFAMX
	    R2=R21/ALFAPX

            DEL1INT_2P=(R1*EXP2-R2*EXP1)/ALFA_DP
	  ELSE
            DEL1INT_2P = 0.
	  ENDIF

	  R1RES=0.
	  IF(R11.NE.0) R1RES=R21/R11
	  IF(R1RES.GT.0) DTLIN=ALOG(R1RES)/ALFA_DP

	  R30=PW*DEL1+PI*DEL2
	  R31=R30-ALFAPX*DEL2
	  R41=R30+ALFAMX*DEL2

	  DEL2N_2P=(R41*EXPP_2P-R31*EXPM_2P)/ALFA_DP
	  IF(ALFAMX.NE.0.AND.ALFAPX.NE.0) THEN
	    R3=-R31/ALFAMX
	    R4=R41/ALFAPX

            DEL2INT_2P=(R3*EXP2-R4*EXP1)/ALFA_DP
          ELSE
	    DEL2INT_2P=0.
	  ENDIF
	  R2RES=0.
	  IF(R31.NE.0) R2RES=R41/R31
	  IF(R2RES.GT.0) DTIIN=ALOG(R2RES)/ALFA_DP


	ENDIF

 100    CONTINUE
        DEL1N=DEL1N_2P
        DEL2N=DEL2N_2P
       

        DEL1INT=DEL1INT_2P
        DEL2INT=DEL2INT_2P
	DT0L=DTLIN
	IF(DT0L.LT.0) DT0L=1.E20
	DT0I=DTIIN
	IF(DT0I.LT.0) DT0I=1.E20
	RETURN
	END SUBROUTINE JERSUPSAT

        SUBROUTINE JERTIMESC(FI1,X1,SFN11,SFN12 &
     &                      ,B11_MY,B12_MY,RIEC,CF,ID,COL,NKR)        
      IMPLICIT NONE
       INTEGER NRM,KR,ICE,ID,NKR
      REAL B12,B11,FUN,DELM,FK,CF,SFN12S,SFN11S
	REAL  COL, &
     & X1(NKR,ID),FI1(NKR,ID),B11_MY(NKR,ID),B12_MY(NKR,ID) &
     &,RIEC(NKR,ID),SFN11,SFN12

	NRM=NKR-1
	DO 1 ICE=1,ID  
             SFN11S=0.                              
             SFN12S=0.
	     SFN11=CF*SFN11S	
	     SFN12=CF*SFN12S
             DO KR=1,NRM

	        FK=FI1(KR,ICE)

	        DELM=X1(KR,ICE)*3.*COL

	        FUN=FK*DELM

	        B11=B11_MY(KR,ICE)
        	B12=B12_MY(KR,ICE)
                SFN11S=SFN11S+FUN*B11                               
                SFN12S=SFN12S+FUN*B12
	     ENDDO

	     SFN11=CF*SFN11S
             SFN12=CF*SFN12S
    1   CONTINUE

	RETURN
	END SUBROUTINE JERTIMESC

        SUBROUTINE JERTIMESC_ICE(FI1,X1,SFN11,SFN12 &
     &                      ,B11_MY,B12_MY,RIEC,CF,ID,COL,NKR)        
      IMPLICIT NONE
       INTEGER NRM,KR,ICE,ID,NKR
      REAL B12,B11,FUN,DELM,FK,CF,SFN12S,SFN11S
	REAL  COL, &
     & X1(NKR,ID),FI1(NKR,ID),B11_MY(NKR,ID),B12_MY(NKR,ID) &
     &,RIEC(NKR,ID),SFN11(ID),SFN12(ID)

	NRM=NKR-1
	DO 1 ICE=1,ID  
             SFN11S=0.                              
             SFN12S=0.
	     SFN11(ICE)=CF*SFN11S	
	     SFN12(ICE)=CF*SFN12S
             DO KR=1,NRM

	        FK=FI1(KR,ICE)

	        DELM=X1(KR,ICE)*3.*COL

	        FUN=FK*DELM

	        B11=B11_MY(KR,ICE)
        	B12=B12_MY(KR,ICE)
                SFN11S=SFN11S+FUN*B11                               
                SFN12S=SFN12S+FUN*B12
	     ENDDO

	     SFN11(ICE)=CF*SFN11S
             SFN12(ICE)=CF*SFN12S
    1   CONTINUE

	RETURN
	END SUBROUTINE JERTIMESC_ICE


        SUBROUTINE ONECOND2 &
     & (TT,QQ,PP,ROR  &
     & ,VR2,VR3,VR4,VR5,PSINGLE &
     & ,DEL1N,DEL2N,DIV1,DIV2 &
     & ,FF2,PSI2,R2,RIEC,RO2BL &
     & ,FF3,PSI3,R3,RSEC,RO3BL &
     & ,FF4,PSI4,R4,RGEC,RO4BL &
     & ,FF5,PSI5,R5,RHEC,RO5BL &
     & ,AA1_MY,BB1_MY,AA2_MY,BB2_MY &
     & ,C1_MEY,C2_MEY &
     & ,COL,DTCOND,ICEMAX,NKR &
     & ,ISYM2,ISYM3,ISYM4,ISYM5)

       IMPLICIT NONE

      INTEGER NKR,ICEMAX
      REAL    COL,VR2(NKR,ICEMAX),VR3(NKR),VR4(NKR) &
     &           ,VR5(NKR),PSINGLE &
     &       ,AA1_MY,BB1_MY,AA2_MY,BB2_MY &
     &       ,DTCOND

      REAL C1_MEY,C2_MEY
      INTEGER I_MIXCOND,I_MIXEVAP,I_ABERGERON,I_BERGERON, &
     & KR,ICE,ITIME,ICM,KCOND,NR,NRM,INUC, &
     & ISYM2,ISYM3,ISYM4,ISYM5,KP,KLIMIT, &
     & KM,ITER,KLIMITL,KLIMITG,KLIMITH,KLIMITI_1,KLIMITI_2,KLIMITI_3, &
     & NCRITI
      REAL AL1,AL2,D,GAM,POD, &
     & RV_MY,CF_MY,D_MYIN,AL1_MY,AL2_MY,ALC,DT0LREF,DTLREF, &
     & A1_MYN, BB1_MYN, A2_MYN, BB2_MYN,DT,DTT,XRAD, &
     & TPC1, TPC2, TPC3, TPC4, TPC5, &
     & EPSDEL, DT0L, DT0I, &
     & ROR, &
     & DEL1NUC,DEL2NUC, &
     & CWHUCM,B6,B8L,B8I,RMASSGL,RMASSGI, &
     & DEL1,DEL2,DEL1S,DEL2S, &
     & TIMENEW,TIMEREV,SFN11,SFN12, &
     & SFNL,SFNI,B5L,B5I,B7L,B7I,DOPL,DOPI,OPERQ,RW,RI,QW,PW, &
     & PI,QI,D1N0,D2N0,DTNEWL,DTNEWL1,D1N,D2N, &
     & DEL_R1,DT0L0,DT0I0,SFN31,SFN32,SFN52, &
     & SFNII1,SFN21,SFN22,DTNEWI3,DTNEWI4,DTNEWI5,DTNEWI2_1, &
     & DTNEWI2_2,DTNEWI1,DEL_R2,DEL_R4,DEL_R5,SFN41,SFN42, &
     & SNF51,DTNEWI2_3,DTNEWI2,DTNEWI_1,DTNEWI_2, &
     & DTNEWL0,DTNEWG1,DTNEWH1,DTNEWI_3, &
     & DTNEWL2,SFN51,SFNII2,DEL_R3,DTNEWI  
       REAL DT_WATER_COND,DT_WATER_EVAP,DT_ICE_COND,DT_ICE_EVAP, &
     &  DT_MIX_COND,DT_MIX_EVAP,DT_MIX_BERGERON,DT_MIX_ANTIBERGERON

       INTEGER K



      DOUBLE PRECISION DD1N,DB11_MY,DAL1,DAL2
      DOUBLE PRECISION COL3,RORI,TPN,TPS,QPN,QPS,TOLD,QOLD &
     &                  ,FI1_K,FI2_K,FI3_K,FI4_K,FI5_K &
     &                  ,R1_K,R2_K,R3_K,R4_K,R5_K &
     &                  ,FI1R1,FI2R2,FI3R3,FI4R4,FI5R5 &
     &                  ,RMASSLAA,RMASSLBB,RMASSIAA,RMASSIBB &
     &                  ,ES1N,ES2N,EW1N,ARGEXP &
     &                  ,TT,QQ,PP &
     &                  ,DEL1N,DEL2N,DIV1,DIV2 &
     &                  ,OPER2,OPER3,AR1,AR2  

       DOUBLE PRECISION DELTAQ1,DELMASSI1,DELMASSL1



        CHARACTER*70 CPRINT








                                                                       
	REAL R2(NKR,ICEMAX) &
     &           ,RIEC(NKR,ICEMAX) &
     &           ,RO2BL(NKR,ICEMAX) &
     &           ,FI2(NKR,ICEMAX),PSI2(NKR,ICEMAX) &
     &           ,FF2(NKR,ICEMAX) &
     &           ,B21_MY(NKR,ICEMAX),B22_MY(NKR,ICEMAX)


        REAL R3(NKR) &
     &           ,RSEC(NKR),RO3BL(NKR) &
     &           ,FI3(NKR),FF3(NKR),PSI3(NKR) &
     &           ,B31_MY(NKR),B32_MY(NKR)


                                                                       
        REAL R4(NKR) &
     &           ,RGEC(NKR),RO4BL(NKR) &
     &           ,FI4(NKR),FF4(NKR),PSI4(NKR) &
     &           ,B41_MY(NKR),B42_MY(NKR)  


        REAL R5(NKR) &
     &           ,RHEC(NKR),RO5BL(NKR) &
     &           ,FI5(NKR),FF5(NKR),PSI5(NKR) &
     &           ,B51_MY(NKR),B52_MY(NKR)  







	REAL DTIMEG(NKR),DTIMEH(NKR) 
       
	REAL DEL2D(ICEMAX),DTIMEO(NKR),DTIMEL(NKR) &



     &           ,DTIMEI_1(NKR),DTIMEI_2(NKR),DTIMEI_3(NKR) &
     &           ,SFNI1(ICEMAX),SFNI2(ICEMAX) &
     &           ,TIMESTEPD(NKR) &
     &           ,FI1REF(NKR),PSI1REF(NKR) &
     &           ,FI2REF(NKR,ICEMAX),PSI2REF(NKR,ICEMAX)&
     &           ,FCCNRREF(NKR)


	OPER2(AR1)=0.622/(0.622+0.378*AR1)/AR1
	OPER3(AR1,AR2)=AR1*AR2/(0.622+0.378*AR1)

        DATA AL1 /2500./, AL2 /2834./, D /0.211/ &
     &      ,GAM /1.E-4/, POD /10./ 
           
	DATA RV_MY,CF_MY,D_MYIN,AL1_MY,AL2_MY &
     &      /461.5,0.24E-1,0.211E-4,2.5E6,2.834E6/

	DATA A1_MYN, BB1_MYN, A2_MYN, BB2_MYN &
     &      /2.53,5.42,3.41E1,6.13/

	DATA TPC1, TPC2, TPC3, TPC4, TPC5 &
     &      /-4.0,-8.1,-12.7,-17.8,-22.4/ 


        DATA EPSDEL/0.1E-03/
    
	DATA DT0L, DT0I /1.E20,1.E20/





        
        I_MIXCOND=0
        I_MIXEVAP=0
        I_ABERGERON=0
        I_BERGERON=0

        COL3=3.0*COL
        ICM=ICEMAX
        ITIME=0
        KCOND=0
        DT_WATER_COND=0.4
        DT_WATER_EVAP=0.4
        DT_ICE_COND=0.4
        DT_ICE_EVAP=0.4
        DT_MIX_COND=0.4
        DT_MIX_EVAP=0.4
        DT_MIX_BERGERON=0.4
        DT_MIX_ANTIBERGERON=0.4
	ICM=ICEMAX
	ITIME=0
	KCOND=0
        DT0LREF=0.2
        DTLREF=0.4

	NR=NKR
	NRM=NKR-1
	DT=DTCOND
	DTT=DTCOND
	XRAD=0.


	CWHUCM=0.
	XRAD=0.
	B6=CWHUCM*GAM-XRAD
	B8L=1./ROR
	B8I=1./ROR
        RORI=1./ROR




        TPN=TT
        QPN=QQ




	      DO ICE=1,ICEMAX
	         SFNI1(ICE)=0.
	         SFNI2(ICE)=0.
	         DEL2D(ICE)=0.
	      ENDDO



	      TIMENEW=0.
	      ITIME=0



   46         ITIME=ITIME+1

	      TIMEREV=DT-TIMENEW

	      DEL1=DEL1N
	      DEL2=DEL2N
	      DEL1S=DEL1N
	      DEL2S=DEL2N
	      DEL2D(1)=DEL2N
	      DEL2D(2)=DEL2N
	      DEL2D(3)=DEL2N
	      TPS=TPN
	      QPS=QPN
              DO KR=1,NKR
                 FI3(KR)=PSI3(KR)
                 FI4(KR)=PSI4(KR)
                 FI5(KR)=PSI5(KR)
                 DO ICE=1,ICEMAX
                    FI2(KR,ICE)=PSI2(KR,ICE)
                 ENDDO
              ENDDO


              CALL JERRATE(R2,TPS,PP,ROR,VR2,PSINGLE &
     &                    ,RIEC,RO2BL,B21_MY,B22_MY,3,2,ICEMAX,NKR)   
              CALL JERRATE(R3,TPS,PP,ROR,VR3,PSINGLE &
     &                    ,RSEC,RO3BL,B31_MY,B32_MY,1,2,ICEMAX,NKR)
              CALL JERRATE(R4,TPS,PP,ROR,VR4,PSINGLE &
     &                    ,RGEC,RO4BL,B41_MY,B42_MY,1,2,ICEMAX,NKR)
              CALL JERRATE(R5,TPS,PP,ROR,VR5,PSINGLE &
     &                    ,RHEC,RO5BL,B51_MY,B52_MY,1,2,ICEMAX,NKR)





              CALL JERTIMESC_ICE  &
     &       (FI2,R2,SFNI1,SFNI2,B21_MY,B22_MY,RIEC,B8I,ICM,COL,NKR) 
              CALL JERTIMESC &
     &       (FI3,R3,SFN31,SFN32,B31_MY,B32_MY,RSEC,B8I,1,COL,NKR)  
              CALL JERTIMESC &
     &       (FI4,R4,SFN41,SFN42,B41_MY,B42_MY,RGEC,B8I,1,COL,NKR) 
              CALL JERTIMESC &
     &       (FI5,R5,SFN51,SFN52,B51_MY,B52_MY,RHEC,B8I,1,COL,NKR)
	      SFNII1=SFNI1(1)+SFNI1(2)+SFNI1(3)
	      SFNII2=SFNI2(1)+SFNI2(2)+SFNI2(3)
	      SFN21=SFNII1+SFN31+SFN41+SFN51        
	      SFN22=SFNII2+SFN32+SFN42+SFN52 
	      SFNL=0.
	      SFNI=SFN21+SFN22       

	      B5L=BB1_MY/TPS/TPS
	      B5I=BB2_MY/TPS/TPS
              B7L=B5L*B6                                                     
              B7I=B5I*B6
	      DOPL=1.+DEL1S                                                     
	      DOPI=1.+DEL2S                                                     
	      OPERQ=OPER2(QPS)  
              RW=(OPERQ+B5L*AL1)*DOPL*SFNL                                      
              QW=B7L*DOPL
              PW=(OPERQ+B5I*AL1)*DOPI*SFNL
              RI=(OPERQ+B5L*AL2)*DOPL*SFNI
              PI=(OPERQ+B5I*AL2)*DOPI*SFNI
              QI=B7I*DOPI
	      KCOND=20
	      IF(DEL2.GT.0) KCOND=21



	      IF(KCOND.EQ.21)  THEN



	      
                DT0I=1.E20
	        DTNEWI1=DTCOND
	        DTNEWL=DTNEWI1
	        IF(ITIME.GE.NKR) THEN
                call wrf_error_fatal3("<stdin>",6954,&
"fatal error in module_mp_full_sbm (ITIME.GE.NKR), model stop")
	        ENDIF
	        TIMESTEPD(ITIME)=DTNEWL

	        IF(DTNEWL.GT.DT) DTNEWL=DT
	        IF((TIMENEW+DTNEWL).GT.DT.AND.ITIME.LT.(NKR-1))  &
     &          DTNEWL=DT-TIMENEW
                IF(ITIME.EQ.(NKR-1)) DTNEWL=DT-TIMENEW
	        TIMESTEPD(ITIME)=DTNEWL
	        TIMENEW=TIMENEW+DTNEWL
	        DTT=DTNEWL




	        CALL JERSUPSAT(DEL1,DEL2,DEL1N,DEL2N &
     &                        ,RW,PW,RI,PI,QW,QI &
     &                        ,DTT,D1N,D2N,DT0L0,DT0I0)






	        IF(ISYM2.NE.0) THEN


 



	          CALL JERDFUN(R2,B21_MY,B22_MY &
     &                        ,FI2,PSI2,D2N &
     &                        ,ICM,1,COL,NKR,TPN)

	          CALL JERDFUN(R2,B21_MY,B22_MY &
     &                        ,FI2,PSI2,D2N &
     &                        ,ICM,2,COL,NKR,TPN)

	          CALL JERDFUN(R2,B21_MY,B22_MY &
     &                        ,FI2,PSI2,D2N &
     &                        ,ICM,3,COL,NKR,TPN)


	        ENDIF

	        IF(ISYM3.NE.0) THEN


                                                         


                  CALL JERDFUN(R3,B31_MY,B32_MY &
     &                        ,FI3,PSI3,D2N &
     &                        ,1,3,COL,NKR,TPN)

	        ENDIF



                IF(ISYM4.NE.0) THEN



                  CALL JERDFUN(R4,B41_MY,B42_MY &
     &                        ,FI4,PSI4,D2N &
     &                        ,1,4,COL,NKR,TPN)


                ENDIF





	        IF(ISYM5.NE.0) THEN


                                                         

	          CALL JERDFUN(R5,B51_MY,B52_MY &
     &                        ,FI5,PSI5,D2N &
     &                        ,1,5,COL,NKR,TPN)


	        ENDIF

	        IF((DEL2.GT.0.AND.DEL2N.LT.0) &
     &         .AND.ABS(DEL2N).GT.EPSDEL) THEN
                call wrf_error_fatal3("<stdin>",7044,&
"fatal error in module_mp_full_sbm (DEL2.GT.0.AND.DEL2N.LT.0), model stop")
                ENDIF

	      ELSE







	        DT0I=1.E20
                IF (DEL2N.EQ.0)THEN
	          DTNEWL=DT
                ELSE
	         DTNEWI3=-R3(3)/(B31_MY(3)*DEL2N-B32_MY(3))
	         DTNEWI4=-R4(3)/(B41_MY(3)*DEL2N-B42_MY(3))
	         DTNEWI5=-R5(3)/(B51_MY(3)*DEL2N-B52_MY(3))

	         DTNEWI2_1=-R2(3,1)/(B21_MY(1,1)*DEL2N-B22_MY(1,1))
	         DTNEWI2_2=-R2(3,2)/(B21_MY(1,2)*DEL2N-B22_MY(1,2))
	         DTNEWI2_3=-R2(3,3)/(B21_MY(1,3)*DEL2N-B22_MY(1,3))
                 DTNEWI2=AMIN1(DTNEWI2_1,DTNEWI2_2,DTNEWI2_3)
	         DTNEWI1=AMIN1(DTNEWI2,DTNEWI3,DTNEWI4 &
     &                       ,DTNEWI5,DT0I,TIMEREV)
	         DTNEWI1=AMIN1(DTNEWI2,DTNEWI4,DTNEWI5,DT0I,TIMEREV)
	         DTNEWL=DTNEWI1
	         IF(DTNEWL.LT.DTLREF) DTNEWL=AMIN1(DTLREF,TIMEREV)
                END IF
	        IF(ITIME.GE.NKR) THEN
                call wrf_error_fatal3("<stdin>",7075,&
"fatal error in module_mp_full_sbm (ITIME.GE.NKR), model stop")
                ENDIF
	        TIMESTEPD(ITIME)=DTNEWL



	        IF(DTNEWL.GT.DT) DTNEWL=DT
	        IF((TIMENEW+DTNEWL).GT.DT.AND.ITIME.LT.(NKR-1))  &
     &          DTNEWL=DT-TIMENEW
                IF(ITIME.EQ.(NKR-1)) DTNEWL=DT-TIMENEW
	        TIMENEW=TIMENEW+DTNEWL
	        TIMESTEPD(ITIME)=DTNEWL
	        DTT=DTNEWL

	        CALL JERSUPSAT(DEL1,DEL2,DEL1N,DEL2N &
     &                        ,RW,PW,RI,PI,QW,QI &
     &                        ,DTT,D1N,D2N,DT0L0,DT0I0)



	        IF(ISYM2.NE.0) THEN





	          CALL JERDFUN(R2,B21_MY,B22_MY &
     &                         ,FI2,PSI2,D2N &
     &                         ,ICM,1,COL,NKR,TPN)

	          CALL JERDFUN(R2,B21_MY,B22_MY &
     &                         ,FI2,PSI2,D2N &
     &                         ,ICM,2,COL,NKR,TPN)

	          CALL JERDFUN(R2,B21_MY,B22_MY &
     &                         ,FI2,PSI2,D2N &
     &                         ,ICM,3,COL,NKR,TPN)
	        ENDIF

	        IF(ISYM3.NE.0) THEN


                                                         



	          CALL JERDFUN(R3,B31_MY,B32_MY &
     &                        ,FI3,PSI3,D2N &
     &                        ,1,3,COL,NKR,TPN)







	        ENDIF



	        IF(ISYM4.NE.0) THEN


                                                         
	          CALL JERDFUN(R4,B41_MY,B42_MY &
     &                        ,FI4,PSI4,D2N &
     &                        ,1,4,COL,NKR,TPN)


	        ENDIF



	        IF(ISYM5.NE.0) THEN


                                                         
	          CALL JERDFUN(R5,B51_MY,B52_MY &
     &                        ,FI5,PSI5,D2N &
     &                        ,1,5,COL,NKR,TPN)


	        ENDIF

	        IF((DEL2.LT.0.AND.DEL2N.GT.0) &
     &         .AND.ABS(DEL2N).GT.EPSDEL) THEN
                call wrf_error_fatal3("<stdin>",7162,&
"fatal error in module_mp_full_sbm (DEL2.LT.0.AND.DEL2N.GT.0), model stop")
	        ENDIF


 
	      ENDIF






              RMASSIBB=0.0
              RMASSIAA=0.0

              DO K=1,NKR
                 DO ICE =1,ICEMAX
                    FI2_K=FI2(K,ICE)
                    R2_K=R2(K,ICE)
                    FI2R2=FI2_K*R2_K*R2_K
                    RMASSIBB=RMASSIBB+FI2R2
                 ENDDO
                 FI3_K=FI3(K)
                 FI4_K=FI4(K)
                 FI5_K=FI5(K)
                 R3_K=R3(K)
                 R4_K=R4(K)
                 R5_K=R5(K)
                 FI3R3=FI3_K*R3_K*R3_K
                 FI4R4=FI4_K*R4_K*R4_K
                 FI5R5=FI5_K*R5_K*R5_K
                 RMASSIBB=RMASSIBB+FI3R3
                 RMASSIBB=RMASSIBB+FI4R4
                 RMASSIBB=RMASSIBB+FI5R5
              ENDDO
              RMASSIBB=RMASSIBB*COL3*RORI

              IF(RMASSIBB.LT.0.0) RMASSIBB=0.0

              DO K=1,NKR
                 DO ICE =1,ICEMAX
                    FI2_K=PSI2(K,ICE)
                    R2_K=R2(K,ICE)
                    FI2R2=FI2_K*R2_K*R2_K
                    RMASSIAA=RMASSIAA+FI2R2
                 ENDDO
                 FI3_K=PSI3(K)
                 FI4_K=PSI4(K)
                 FI5_K=PSI5(K)
                 R3_K=R3(K)
                 R4_K=R4(K)
                 R5_K=R5(K)
                 FI3R3=FI3_K*R3_K*R3_K
                 FI4R4=FI4_K*R4_K*R4_K
                 FI5R5=FI5_K*R5_K*R5_K
                 RMASSIAA=RMASSIAA+FI3R3
                 RMASSIAA=RMASSIAA+FI4R4
                 RMASSIAA=RMASSIAA+FI5R5
              ENDDO
              RMASSIAA=RMASSIAA*COL3*RORI

              IF(RMASSIAA.LT.0.0) RMASSIAA=0.0

              DELMASSI1=RMASSIAA-RMASSIBB
              QPN=QPS-DELMASSI1
              DAL2=AL2
              TPN=TPS+DAL2*DELMASSI1

              ARGEXP=-BB1_MY/TPN
              ES1N=AA1_MY*DEXP(ARGEXP)
              ARGEXP=-BB2_MY/TPN
              ES2N=AA2_MY*DEXP(ARGEXP)
              EW1N=OPER3(QPN,PP)
              IF(ES1N.EQ.0)THEN
               DEL1N=0.5
               DIV1=1.5
               call wrf_error_fatal3("<stdin>",7239,&
"fatal error in module_mp_full_sbm (ES1N.EQ.0), model stop")
              ELSE
               DIV1=EW1N/ES1N
               DEL1N=EW1N/ES1N-1.
              END IF
              IF(ES2N.EQ.0)THEN
               DEL2N=0.5
               DIV2=1.5
               call wrf_error_fatal3("<stdin>",7248,&
"fatal error in module_mp_full_sbm (ES2N.EQ.0), model stop")
              ELSE
               DEL2N=EW1N/ES2N-1.
               DIV2=EW1N/ES2N
              END IF



	      IF(TIMENEW.LT.DT) GOTO 46
        TT=TPN
        QQ=QPN
	DO KR=1,NKR
	   DO ICE=1,ICEMAX
	      FF2(KR,ICE)=PSI2(KR,ICE)
	   ENDDO
	   FF3(KR)=PSI3(KR)
	   FF4(KR)=PSI4(KR)
	   FF5(KR)=PSI5(KR)
	ENDDO





        RETURN                                          
        END SUBROUTINE ONECOND2


        SUBROUTINE ONECOND3 &
     & (TT,QQ,PP,ROR &
     & ,VR1,VR2,VR3,VR4,VR5,PSINGLE &
     & ,DEL1N,DEL2N,DIV1,DIV2 &
     & ,FF1,PSI1,R1,RLEC,RO1BL &
     & ,FF2,PSI2,R2,RIEC,RO2BL &
     & ,FF3,PSI3,R3,RSEC,RO3BL &
     & ,FF4,PSI4,R4,RGEC,RO4BL &
     & ,FF5,PSI5,R5,RHEC,RO5BL &
     & ,AA1_MY,BB1_MY,AA2_MY,BB2_MY &
     & ,C1_MEY,C2_MEY &
     & ,COL,DTCOND,ICEMAX,NKR &
     & ,ISYM1,ISYM2,ISYM3,ISYM4,ISYM5)
       IMPLICIT NONE
       INTEGER ICEMAX,NKR,KR,ITIME,ICE,KCOND,K &
     &           ,ISYM1,ISYM2,ISYM3,ISYM4,ISYM5
       INTEGER KLIMITL,KLIMITG,KLIMITH,KLIMITI_1, &
     &  KLIMITI_2,KLIMITI_3
       INTEGER I_MIXCOND,I_MIXEVAP,I_ABERGERON,I_BERGERON  
       REAL ROR,VR1(NKR),VR2(NKR,ICEMAX),VR3(NKR),VR4(NKR) &
     &           ,VR5(NKR),PSINGLE &
     &           ,AA1_MY,BB1_MY,AA2_MY,BB2_MY &
     &           ,C1_MEY,C2_MEY &
     &           ,COL,DTCOND


                                                                       
        REAL R1(NKR)&
     &           ,RLEC(NKR),RO1BL(NKR) &
     &           ,FI1(NKR),FF1(NKR),PSI1(NKR) &
     &           ,B11_MY(NKR),B12_MY(NKR)


                                                                       
	REAL R2(NKR,ICEMAX) &
     &           ,RIEC(NKR,ICEMAX) &
     &           ,RO2BL(NKR,ICEMAX) &
     &           ,FI2(NKR,ICEMAX),PSI2(NKR,ICEMAX) &
     &           ,FF2(NKR,ICEMAX) &
     &           ,B21_MY(NKR,ICEMAX),B22_MY(NKR,ICEMAX) &
     &           ,RATE2(NKR,ICEMAX),DEL_R2M(NKR,ICEMAX)


        REAL R3(NKR) &
     &           ,RSEC(NKR),RO3BL(NKR) &
     &           ,FI3(NKR),FF3(NKR),PSI3(NKR) &
     &           ,B31_MY(NKR),B32_MY(NKR) &
     &           ,DEL_R3M(NKR)  


                                                                       
        REAL R4(NKR),R4N(NKR) &
     &           ,RGEC(NKR),RO4BL(NKR) &
     &           ,FI4(NKR),FF4(NKR),PSI4(NKR) &
     &           ,B41_MY(NKR),B42_MY(NKR) &
     &           ,DEL_R4M(NKR)


        REAL R5(NKR),R5N(NKR) &
     &           ,RHEC(NKR),RO5BL(NKR) &
     &           ,FI5(NKR),FF5(NKR),PSI5(NKR) &
     &           ,B51_MY(NKR),B52_MY(NKR) &
     &           ,DEL_R5M(NKR)

      DOUBLE PRECISION DD1N,DB11_MY,DAL1,DAL2
      DOUBLE PRECISION COL3,RORI,TPN,TPS,QPN,QPS,TOLD,QOLD &
     &                  ,FI1_K,FI2_K,FI3_K,FI4_K,FI5_K &
     &                  ,R1_K,R2_K,R3_K,R4_K,R5_K &
     &                  ,FI1R1,FI2R2,FI3R3,FI4R4,FI5R5 &
     &                  ,RMASSLAA,RMASSLBB,RMASSIAA,RMASSIBB &
     &                  ,ES1N,ES2N,EW1N,ARGEXP &
     &                  ,TT,QQ,PP,DEL1N0,DEL2N0 &
     &                  ,DEL1N,DEL2N,DIV1,DIV2 &
     &                  ,OPER2,OPER3,AR1,AR2

       DOUBLE PRECISION DELTAQ1,DELMASSI1,DELMASSL1

       REAL A1_MYN, BB1_MYN, A2_MYN, BB2_MYN
        DATA A1_MYN, BB1_MYN, A2_MYN, BB2_MYN &
     &      /2.53,5.42,3.41E1,6.13/
       REAL B8L,B8I,SFN11,SFN12,SFNL,SFNI
       REAL B5L,B5I,B7L,B7I,B6,DOPL,DEL1S,DEL2S,DOPI,RW,QW,PW, &
     &  RI,PI,QI,SFNI1(ICEMAX),SFNI2(ICEMAX),AL1,AL2
       REAL D1N,D2N,DT0L, DT0I,D1N0,D2N0
       REAL SFN21,SFN22,SFNII1,SFNII2,SFN31,SFN32,SFN41,SFN42,SFN51, &
     &  SFN52
       REAL DEL1,DEL2
       REAL  TIMEREV,DT,DTT,TIMENEW
       REAL DTIMEG(NKR),DTIMEH(NKR)

       REAL DEL2D(ICEMAX),DTIMEO(NKR),DTIMEL(NKR) &
     &           ,DTIMEI_1(NKR),DTIMEI_2(NKR),DTIMEI_3(NKR)
       REAL DT_WATER_COND,DT_WATER_EVAP,DT_ICE_COND,DT_ICE_EVAP, &
     &  DT_MIX_COND,DT_MIX_EVAP,DT_MIX_BERGERON,DT_MIX_ANTIBERGERON
       REAL DTNEWL0,DTNEWL1,DTNEWI1,DTNEWI2_1,DTNEWI2_2,DTNEWI2_3, &
     & DTNEWI2,DTNEWI_1,DTNEWI_2,DTNEWI3,DTNEWI4,DTNEWI5, &
     & DTNEWL,DTNEWL2,DTNEWG1,DTNEWH1
       REAL TIMESTEPD(NKR)

       DATA AL1 /2500./, AL2 /2834./
       REAL EPSDEL,EPSDEL2
       DATA EPSDEL, EPSDEL2 /0.1E-03,0.1E-03/
       OPER2(AR1)=0.622/(0.622+0.378*AR1)/AR1
       OPER3(AR1,AR2)=AR1*AR2/(0.622+0.378*AR1)
      


        DT_WATER_COND=0.4
        DT_WATER_EVAP=0.4
        DT_ICE_COND=0.4
        DT_ICE_EVAP=0.4
        DT_MIX_COND=0.4
        DT_MIX_EVAP=0.4
        DT_MIX_BERGERON=0.4
        DT_MIX_ANTIBERGERON=0.4

        I_MIXCOND=0
        I_MIXEVAP=0
        I_ABERGERON=0
        I_BERGERON=0

       ITIME = 0
       TIMENEW=0.
       DT=DTCOND
       DTT=DTCOND

       B6=0.
       B8L=1./ROR
       B8I=1./ROR

        RORI=1.D0/ROR


        COL3=3.D0*COL





        TPN=TT
        QPN=QQ

   16         ITIME=ITIME+1


              IF((TPN-273.15).GE.-0.187) GO TO 17
              TIMEREV=DT-TIMENEW
              DEL1=DEL1N
              DEL2=DEL2N
              DEL1S=DEL1N
              DEL2S=DEL2N

              DEL2D(1)=DEL2N
              DEL2D(2)=DEL2N
              DEL2D(3)=DEL2N
              TPS=TPN
              QPS=QPN
              DO KR=1,NKR
                 FI1(KR)=PSI1(KR)
                 FI3(KR)=PSI3(KR)
                 FI4(KR)=PSI4(KR)
                 FI5(KR)=PSI5(KR)
                 DO ICE=1,ICEMAX
                    FI2(KR,ICE)=PSI2(KR,ICE)
                 ENDDO
              ENDDO


              CALL JERRATE(R1,TPS,PP,ROR,VR1,PSINGLE &
     &                    ,RLEC,RO1BL,B11_MY,B12_MY,1,1,ICEMAX,NKR)
              CALL JERRATE(R2,TPS,PP,ROR,VR2,PSINGLE &
     &                    ,RIEC,RO2BL,B21_MY,B22_MY,3,2,ICEMAX,NKR)
              CALL JERRATE(R3,TPS,PP,ROR,VR3,PSINGLE &
     &                    ,RSEC,RO3BL,B31_MY,B32_MY,1,2,ICEMAX,NKR)
              CALL JERRATE(R4,TPS,PP,ROR,VR4,PSINGLE &
     &                    ,RGEC,RO4BL,B41_MY,B42_MY,1,2,ICEMAX,NKR)
              CALL JERRATE(R5,TPS,PP,ROR,VR5,PSINGLE &
     &                    ,RHEC,RO5BL,B51_MY,B52_MY,1,2,ICEMAX,NKR)
              CALL JERTIMESC(FI1,R1,SFN11,SFN12 &
     &                      ,B11_MY,B12_MY,RLEC,B8L,1,COL,NKR)
              CALL JERTIMESC_ICE(FI2,R2,SFNI1,SFNI2 &
     &                      ,B21_MY,B22_MY,RIEC,B8I,ICEMAX,COL,NKR)
              CALL JERTIMESC(FI3,R3,SFN31,SFN32 &
     &                      ,B31_MY,B32_MY,RSEC,B8I,1,COL,NKR)
              CALL JERTIMESC(FI4,R4,SFN41,SFN42 &
     &                      ,B41_MY,B42_MY,RGEC,B8I,1,COL,NKR)
              CALL JERTIMESC(FI5,R5,SFN51,SFN52 &
     &                      ,B51_MY,B52_MY,RHEC,B8I,1,COL,NKR)

              SFNII1=SFNI1(1)+SFNI1(2)+SFNI1(3)
              SFNII2=SFNI2(1)+SFNI2(2)+SFNI2(3)
              SFN21=SFNII1+SFN31+SFN41+SFN51
              SFN22=SFNII2+SFN32+SFN42+SFN52
              SFNL=SFN11+SFN12
              SFNI=SFN21+SFN22

              B5L=BB1_MY/TPS/TPS
              B5I=BB2_MY/TPS/TPS
              B7L=B5L*B6
              B7I=B5I*B6
              DOPL=1.+DEL1S
              DOPI=1.+DEL2S
              RW=(OPER2(QPS)+B5L*AL1)*DOPL*SFNL
              QW=B7L*DOPL
              PW=(OPER2(QPS)+B5I*AL1)*DOPI*SFNL
              RI=(OPER2(QPS)+B5L*AL2)*DOPL*SFNI
              PI=(OPER2(QPS)+B5I*AL2)*DOPI*SFNI
              QI=B7I*DOPI

              CALL JERSUPSAT(DEL1,DEL2,DEL1N0,DEL2N0 &
     &                      ,RW,PW,RI,PI,QW,QI &
     &                      ,DTT,D1N0,D2N0,DT0L,DT0I)




              KCOND=50

              IF(DEL1.LT.0.AND.DEL2.LT.0) KCOND=30
              IF(DEL1.GT.0.AND.DEL2.GT.0) KCOND=31
              IF(DEL1.LT.0.AND.DEL2.GT.0) KCOND=32
              IF(KCOND.EQ.50) THEN 
                I_ABERGERON=I_ABERGERON+1
                IF(DT0L.EQ.0) THEN
                  DTNEWL=DT
                ELSE
                  DTNEWL=AMIN1(DT,DT0L)
                ENDIF

                IF(DTNEWL.GT.DT) DTNEWL=DT
                IF((TIMENEW+DTNEWL).GT.DT.AND.ITIME.LT.(NKR-1)) &
     &          DTNEWL=DT-TIMENEW
                IF(ITIME.EQ.(NKR-1)) DTNEWL=DT-TIMENEW
                TIMENEW=TIMENEW+DTNEWL
                DTT=DTNEWL
                IF(ITIME.GE.NKR) THEN
                call wrf_error_fatal3("<stdin>",7513,&
"fatal error in module_mp_full_sbm (ITIME.GE.NKR), model stop")
                ENDIF
                TIMESTEPD(ITIME)=DTNEWL


              ENDIF
              IF(KCOND.EQ.31) THEN


                I_MIXCOND=I_MIXCOND+1
               IF (DEL1N.EQ.0)THEN
                DTNEWL0=DT
               ELSE
                DTNEWL0=ABS(R1(ITIME)/(B11_MY(ITIME)*DEL1N- &
     &                                 B12_MY(ITIME)))
               END IF


               IF (DEL2N.EQ.0)THEN
                DTNEWI2_1=DT
                DTNEWI2_2=DT
                DTNEWI2_3=DT
                DTNEWI3=DT
                DTNEWI4=DT
                DTNEWI5=DT
               ELSE
                DTNEWI2_1=ABS(R2(ITIME,1)/ &
     &         (B21_MY(ITIME,1)*DEL2N-B22_MY(ITIME,1)))
                DTNEWI2_2=ABS(R2(ITIME,2)/ &
     &         (B21_MY(ITIME,2)*DEL2N-B22_MY(ITIME,2))) 
                DTNEWI2_3=ABS(R2(ITIME,3)/ &
     &         (B21_MY(ITIME,3)*DEL2N-B22_MY(ITIME,3)))  
                DTNEWI2=AMIN1(DTNEWI2_1,DTNEWI2_2,DTNEWI2_3)

                DTNEWI3=ABS(R3(ITIME)/(B31_MY(ITIME)*DEL2N- &
     &                                 B32_MY(ITIME)))
                DTNEWI4=ABS(R4(ITIME)/(B41_MY(ITIME)*DEL2N- &
     &                                 B42_MY(ITIME)))
                DTNEWI5=ABS(R5(ITIME)/(B51_MY(ITIME)*DEL2N- &
     &                                 B52_MY(ITIME)))
               END IF
                DTNEWI1=AMIN1(DTNEWI2,DTNEWI4,DTNEWI5,DT0I)
                IF(DT0L.NE.0) THEN
                  IF(ABS(DT0L).LT.DT_MIX_COND) THEN
                    DTNEWL1=AMIN1(DT_MIX_COND,DTNEWL0)
                  ELSE
                    DTNEWL1=AMIN1(DT0L,DTNEWL0)
                  ENDIF
                ELSE
                  DTNEWL1=DTNEWL0
                ENDIF
                DTNEWL=AMIN1(DTNEWL1,DTNEWI1)
                IF(ITIME.GE.NKR) THEN
                call wrf_error_fatal3("<stdin>",7567,&
"fatal error in module_mp_full_sbm (ITIME.GE.NKR), model stop")
                ENDIF
                TIMESTEPD(ITIME)=DTNEWL

                IF(DTNEWL.GT.DT) DTNEWL=DT
                IF((TIMENEW+DTNEWL).GT.DT.AND.ITIME.LT.(NKR-1)) &
     &          DTNEWL=DT-TIMENEW
                IF(ITIME.EQ.(NKR-1)) DTNEWL=DT-TIMENEW
                TIMENEW=TIMENEW+DTNEWL
                TIMESTEPD(ITIME)=DTNEWL
                DTT=DTNEWL


              ENDIF
              IF(KCOND.EQ.30) THEN


                I_MIXEVAP=I_MIXEVAP+1
                DO KR=1,NKR
                   DTIMEL(KR)=0.
                   DTIMEG(KR)=0.
                   DTIMEH(KR)=0.

                   DTIMEI_1(KR)=0.
                   DTIMEI_2(KR)=0.
                   DTIMEI_3(KR)=0.
                ENDDO
                DO KR=1,NKR
                 IF (DEL1N.EQ.0) THEN
                   DTIMEL(KR)=DT
                   DTIMEG(KR)=DT
                   DTIMEH(KR)=DT
                 ELSE
                   DTIMEL(KR)=-R1(KR)/(B11_MY(KR)*DEL1N- &
     &                                 B12_MY(KR))
                   DTIMEG(KR)=-R4(KR)/(B41_MY(KR)*DEL1N- &
     &                                 B42_MY(KR))
                   DTIMEH(KR)=-R5(KR)/(B51_MY(KR)*DEL1N- &
     &                             B52_MY(KR))

                 END IF
                 IF (DEL2N.EQ.0) THEN
                   DTIMEI_1(KR)=DT
                   DTIMEI_2(KR)=DT
                   DTIMEI_3(KR)=DT
                 ELSE
                   DTIMEI_1(KR)=-R2(KR,1)/ &
     &               (B21_MY(KR,1)*DEL2N-B22_MY(KR,1))
                   DTIMEI_2(KR)=-R2(KR,2)/ &
     &               (B21_MY(KR,2)*DEL2N-B22_MY(KR,2))
                   DTIMEI_3(KR)=-R2(KR,3)/ &
     &               (B21_MY(KR,3)*DEL2N-B22_MY(KR,3))
                 END IF
                ENDDO

                KLIMITL=1
                DO KR=1,NKR
                   IF(DTIMEL(KR).GT.TIMEREV) GOTO 355
                   KLIMITL=KR
                ENDDO
  355           KLIMITL=KLIMITL-1
                IF(KLIMITL.LT.1) KLIMITL=1
                DTNEWL1=AMIN1(DTIMEL(KLIMITL),DT0L,TIMEREV)

                KLIMITG=1
                DO KR=1,NKR
                   IF(DTIMEG(KR).GT.TIMEREV) GOTO 455
                   KLIMITG=KR
                ENDDO
  455           KLIMITG=KLIMITG-1
                IF(KLIMITG.LT.1) KLIMITG=1
                DTNEWG1=AMIN1(DTIMEG(KLIMITG),TIMEREV)

                KLIMITH=1
                DO KR=1,NKR
                   IF(DTIMEH(KR).GT.TIMEREV) GOTO 555
                   KLIMITH=KR
                ENDDO
  555           KLIMITH=KLIMITH-1
                IF(KLIMITH.LT.1) KLIMITH=1
                DTNEWH1=AMIN1(DTIMEH(KLIMITH),TIMEREV)


                KLIMITI_1=1
                KLIMITI_2=1
                KLIMITI_3=1
                DO KR=1,NKR
                   IF(DTIMEI_1(KR).GT.TIMEREV) GOTO 655
                   KLIMITI_1=KR
                ENDDO
  655           CONTINUE
                DO KR=1,NKR
                   IF(DTIMEI_2(KR).GT.TIMEREV) GOTO 656
                   KLIMITI_2=KR
                ENDDO
  656           CONTINUE
                DO KR=1,NKR
                   IF(DTIMEI_3(KR).GT.TIMEREV) GOTO 657
                   KLIMITI_3=KR
                ENDDO
  657           CONTINUE
                KLIMITI_1=KLIMITI_1-1
                IF(KLIMITI_1.LT.1) KLIMITI_1=1
                DTNEWI2_1=AMIN1(DTIMEI_1(KLIMITI_1),TIMEREV)
                KLIMITI_2=KLIMITI_2-1
                IF(KLIMITI_2.LT.1) KLIMITI_2=1
                DTNEWI2_2=AMIN1(DTIMEI_2(KLIMITI_2),TIMEREV)
                KLIMITI_3=KLIMITI_3-1
                IF(KLIMITI_3.LT.1) KLIMITI_3=1
                DTNEWI2_3=AMIN1(DTIMEI_3(KLIMITI_3),TIMEREV)
                DTNEWI2=AMIN1(DTNEWI2_1,DTNEWI2_2,DTNEWI2_3)

                DTNEWI1=AMIN1(DTNEWI2,DTNEWG1,DTNEWH1,DT0I)
                IF(ABS(DEL2N).LT.EPSDEL2) &
     &          DTNEWI1=AMIN1(DTNEWI2,DTNEWG1,DTNEWH1)
                DTNEWL2=AMIN1(DTNEWL1,DTNEWI1)
                DTNEWL=DTNEWL2
                IF(DTNEWL.LT.DT_MIX_EVAP) &
     &          DTNEWL=AMIN1(DT_MIX_EVAP,TIMEREV)  
                IF(ITIME.GE.NKR) THEN
                call wrf_error_fatal3("<stdin>",7688,&
"fatal error in module_mp_full_sbm (ITIME.GE.NKR), model stop")
                ENDIF
                TIMESTEPD(ITIME)=DTNEWL

                IF(DTNEWL.GT.DT) DTNEWL=DT
                IF((TIMENEW+DTNEWL).GT.DT &
     &         .AND.ITIME.LT.(NKR-1)) &
     &          DTNEWL=DT-TIMENEW
                IF(ITIME.EQ.(NKR-1)) DTNEWL=DT-TIMENEW
                TIMESTEPD(ITIME)=DTNEWL
                TIMENEW=TIMENEW+DTNEWL
                DTT=DTNEWL


              ENDIF
              IF(KCOND.EQ.32) THEN


                I_BERGERON=I_BERGERON+1

               IF (DEL1N.EQ.0)THEN
                DTNEWL0=DT
               ELSE
                DTNEWL0=-R1(1)/(B11_MY(1)*DEL1N-B12_MY(1))
               END IF

               IF (DEL2N.EQ.0)THEN
                DTNEWI2_1=DT
                DTNEWI2_2=DT
                DTNEWI2_3=DT
               ELSE
                DTNEWI2_1=R2(1,1)/(B21_MY(1,1)*DEL2N-B22_MY(1,1))
                DTNEWI2_2=R2(1,2)/(B21_MY(1,2)*DEL2N-B22_MY(1,2))
                DTNEWI2_3=R2(1,3)/(B21_MY(1,3)*DEL2N-B22_MY(1,3))
               END IF
               DTNEWI2=AMIN1(DTNEWI2_1,DTNEWI2_2,DTNEWI2_3)
               IF (DEL2N.EQ.0)THEN
                DTNEWI3=DT
                DTNEWI4=DT
                DTNEWI5=DT
               ELSE
                DTNEWI3=R3(1)/(B31_MY(1)*DEL2N-B32_MY(1))
                DTNEWI4=R4(1)/(B41_MY(1)*DEL2N-B42_MY(1))
                DTNEWI5=R5(1)/(B51_MY(1)*DEL2N-B52_MY(1))
               END IF
                DTNEWL1=AMIN1(DTNEWL0,DT0L,TIMEREV)
                DTNEWI1=AMIN1(DTNEWI2,DTNEWI3,DTNEWI4 &
     &                       ,DTNEWI5,DT0I,TIMEREV)
                DTNEWI1=AMIN1(DTNEWI2,DTNEWI4,DTNEWI5,DT0I,TIMEREV)
                DTNEWL=AMIN1(DTNEWL1,DTNEWI1)

                IF(DTNEWL.LT.DT_MIX_BERGERON) &
     &          DTNEWL=AMIN1(DT_MIX_BERGERON,TIMEREV)
                TIMESTEPD(ITIME)=DTNEWL

                IF(DTNEWL.GT.DT) DTNEWL=DT
                IF((TIMENEW+DTNEWL).GT.DT.AND.ITIME.LT.(NKR-1)) &
     &          DTNEWL=DT-TIMENEW
                IF(ITIME.EQ.(NKR-1)) DTNEWL=DT-TIMENEW
                TIMESTEPD(ITIME)=DTNEWL
                TIMENEW=TIMENEW+DTNEWL
                DTT=DTNEWL


              ENDIF


         
	      CALL JERSUPSAT(DEL1,DEL2,DEL1N,DEL2N &
     &                      ,RW,PW,RI,PI,QW,QI &
     &                      ,DTT,D1N,D2N,DT0L,DT0I)



	      IF(ISYM1.NE.0) THEN



                                                         

	        CALL JERDFUN(R1,B11_MY,B12_MY &
     &                      ,FI1,PSI1,D1N &
     &                      ,1,1,COL,NKR,TPN)

 


 	      ENDIF                     

	      IF(ISYM2.NE.0) THEN


 
	        CALL JERDFUN(R2,B21_MY,B22_MY &
     &                      ,FI2,PSI2,D2N &
     &                      ,ICEMAX,1,COL,NKR,TPN)

	        CALL JERDFUN(R2,B21_MY,B22_MY &
     &                      ,FI2,PSI2,D2N &
     &                      ,ICEMAX,2,COL,NKR,TPN)

	        CALL JERDFUN(R2,B21_MY,B22_MY &
     &                      ,FI2,PSI2,D2N &
     &                      ,ICEMAX,3,COL,NKR,TPN)


	      ENDIF

	      IF(ISYM3.NE.0) THEN


                                                         



 	        CALL JERDFUN(R3,B31_MY,B32_MY &
     &                      ,FI3,PSI3,D2N &
     &                      ,1,3,COL,NKR,TPN)




  	      ENDIF



	      IF(ISYM4.NE.0) THEN


                                                         
	        CALL JERDFUN(R4,B41_MY,B42_MY &
     &                      ,FI4,PSI4,D2N &
     &                      ,1,4,COL,NKR,TPN)


	      ENDIF

	      IF(ISYM5.NE.0) THEN


                                                         
	        CALL JERDFUN(R5,B51_MY,B52_MY &
     &                      ,FI5,PSI5,D2N &
     &                      ,1,5,COL,NKR,TPN)


	      ENDIF

              RMASSLBB=0.D0
              RMASSIBB=0.D0
              RMASSLAA=0.D0
              RMASSIAA=0.D0

              DO K=1,NKR
                 FI1_K=FI1(K)
                 R1_K=R1(K)
                 FI1R1=FI1_K*R1_K*R1_K
                 RMASSLBB=RMASSLBB+FI1R1
                 DO ICE =1,ICEMAX
                    FI2_K=FI2(K,ICE)
                    R2_K=R2(K,ICE)
                    FI2R2=FI2_K*R2_K*R2_K
                    RMASSIBB=RMASSIBB+FI2R2
                 ENDDO
                 FI3_K=FI3(K)
                 FI4_K=FI4(K)
                 FI5_K=FI5(K)
                 R3_K=R3(K)
                 R4_K=R4(K)
                 R5_K=R5(K)
                 FI3R3=FI3_K*R3_K*R3_K
                 FI4R4=FI4_K*R4_K*R4_K
                 FI5R5=FI5_K*R5_K*R5_K
                 RMASSIBB=RMASSIBB+FI3R3
                 RMASSIBB=RMASSIBB+FI4R4
                 RMASSIBB=RMASSIBB+FI5R5
              ENDDO
              RMASSIBB=RMASSIBB*COL3*RORI

              IF(RMASSIBB.LT.0.0) RMASSIBB=0.0
              RMASSLBB=RMASSLBB*COL3*RORI

              IF(RMASSLBB.LT.0.0) RMASSLBB=0.0

              DO K=1,NKR
                 FI1_K=PSI1(K)
                 R1_K=R1(K)
                 FI1R1=FI1_K*R1_K*R1_K
                 RMASSLAA=RMASSLAA+FI1R1
                 DO ICE =1,ICEMAX
                    FI2(K,ICE)=PSI2(K,ICE)
                    FI2_K=FI2(K,ICE)
                    R2_K=R2(K,ICE)
                    FI2R2=FI2_K*R2_K*R2_K
                    RMASSIAA=RMASSIAA+FI2R2
                 ENDDO
                 FI3_K=PSI3(K)
                 FI4_K=PSI4(K)
                 FI5_K=PSI5(K)
                 R3_K=R3(K)
                 R4_K=R4(K)
                 R5_K=R5(K)
                 FI3R3=FI3_K*R3_K*R3_K
                 FI4R4=FI4_K*R4_K*R4_K
                 FI5R5=FI5_K*R5_K*R5_K
                 RMASSIAA=RMASSIAA+FI3R3
                 RMASSIAA=RMASSIAA+FI4R4
                 RMASSIAA=RMASSIAA+FI5R5
              ENDDO
              RMASSIAA=RMASSIAA*COL3*RORI

              IF(RMASSIAA.LE.0.0) RMASSIAA=0.0
              RMASSLAA=RMASSLAA*COL3*RORI

              IF(RMASSLAA.LT.0.0) RMASSLAA=0.0

              DELMASSL1=RMASSLAA-RMASSLBB
              DELMASSI1=RMASSIAA-RMASSIBB
              DELTAQ1=DELMASSL1+DELMASSI1

              QPN=QPS-DELTAQ1
              DAL1=AL1
              DAL2=AL2

              TPN=TPS+DAL1*DELMASSL1+DAL2*DELMASSI1

              ARGEXP=-BB1_MY/TPN
              ES1N=AA1_MY*DEXP(ARGEXP)
              ARGEXP=-BB2_MY/TPN
              ES2N=AA2_MY*DEXP(ARGEXP)
              EW1N=OPER3(QPN,PP)
              IF(ES1N.EQ.0)THEN
               DEL1N=0.5
               DIV1=1.5
               print*,'es1n onecond3 = 0'

              ELSE
               DIV1=EW1N/ES1N
               DEL1N=EW1N/ES1N-1.
              END IF
              IF(ES2N.EQ.0)THEN
               DEL2N=0.5
               DIV2=1.5
               print*,'es2n onecond3 = 0'

              ELSE
               DEL2N=EW1N/ES2N-1.
               DIV2=EW1N/ES2N
              END IF




        IF(TIMENEW.LT.DT) GOTO 16
17      CONTINUE

        TT=TPN
        QQ=QPN
        DO KR=1,NKR
           FF1(KR)=PSI1(KR)
           DO ICE=1,ICEMAX
              FF2(KR,ICE)=PSI2(KR,ICE)
           ENDDO
           FF3(KR)=PSI3(KR)
           FF4(KR)=PSI4(KR)
           FF5(KR)=PSI5(KR)
        ENDDO


        RETURN                                          
        END SUBROUTINE ONECOND3

        SUBROUTINE COAL_BOTT_NEW(FF1R,FF2R,FF3R, &
     &   FF4R,FF5R,TT,QQ,PP,RHO,dt_coll,TCRIT,TTCOAL)
       implicit none
       INTEGER KR,ICE
       INTEGER icol_drop,icol_snow,icol_graupel,icol_hail, &
     & icol_column,icol_plate,icol_dendrite,icol_drop_brk
       double precision  g1(nkr),g2(nkr,icemax),g3(nkr),g4(nkr),g5(nkr)
       double precision gdumb(JMAX),xl_dumb(0:nkr),g_orig(nkr)
       double precision g2_1(nkr),g2_2(nkr),g2_3(nkr)
       real cont_fin_drop,dconc,conc_icempl,deldrop,t_new, &
     & delt_new,cont_fin_ice,conc_old,conc_new,cont_init_ice, &
     & cont_init_drop,ALWC
       REAL    FF1R(NKR),FF2R(NKR,ICEMAX),FF3R(NKR),FF4R(NKR),FF5R(NKR)
       REAL dt_coll
       REAL TCRIT,TTCOAL
       real tt_no_coll
       parameter (tt_no_coll=273.16)


       
   

       INTEGER I,J,IT,NDIV
       REAL RHO
       DOUBLE PRECISION break_drop_bef,break_drop_aft,dtbreakup
       DOUBLE PRECISION break_drop_per
       DOUBLE PRECISION TT,QQ,PP,prdkrn,prdkrn1
       parameter (prdkrn1=1.d0)






      icol_drop_brk=0
      icol_drop=0
      icol_snow=0
      icol_graupel=0
      icol_hail=0
      icol_column=0
      icol_plate=0
      icol_dendrite=0


       t_new=tt
         CALL MISC1(PP,cwll_1000mb,cwll_750mb,cwll_500mb, &
     &    cwll,nkr)

         DO I=1,NKR
            DO J=1,NKR
               CWLL(I,J)=ECOALMASSM(I,J)*CWLL(I,J)
            ENDDO
         ENDDO


        IF (LIQTURB.EQ.1)THEN
         DO I=1,KRMAX_LL
           DO J=1,KRMAX_LL
               CWLL(I,J)=CTURBLL(I,J)*CWLL(I,J)
           END DO
         END DO
        END IF
         CALL MODKRN(TT,QQ,PP,PRDKRN,TTCOAL)
        DO 13 KR=1,NKR
         G1(KR)=FF1R(KR)*3.*XL(KR)*XL(KR)*1.E3
         G2(KR,1)=FF2R(KR,1)*3*xi(KR,1)*XI(KR,1)*1.e3
         G2(KR,2)=FF2R(KR,2)*3.*xi(KR,2)*XI(KR,2)*1.e3
         G2(KR,3)=FF2R(KR,3)*3.*xi(KR,3)*XI(KR,3)*1.e3
         G3(KR)=FF3R(KR)*3.*xs(kr)*xs(kr)*1.e3
         G4(KR)=FF4R(KR)*3.*xg(kr)*xg(kr)*1.e3
         G5(KR)=FF5R(KR)*3.*xh(kr)*xh(kr)*1.e3
         g2_1(kr)=g2(KR,1)
         g2_2(KR)=g2(KR,2)
         g2_3(KR)=g2(KR,3)
         if(kr.gt.(nkr-jbreak).and.g1(kr).gt.1.e-17)icol_drop_brk=1

         IF (IBREAKUP.NE.1)icol_drop_brk=0 
         if(g1(kr).gt.1.e-10)icol_drop=1
         if (tt.le.tt_no_coll)then
         if(g2_1(kr).gt.1.e-10)icol_column=1
         if(g2_2(kr).gt.1.e-10)icol_plate=1
         if(g2_3(kr).gt.1.e-10)icol_dendrite=1
         if(g3(kr).gt.1.e-10)icol_snow=1
         if(g4(kr).gt.1.e-10)icol_graupel=1
         if(g5(kr).gt.1.e-10)icol_hail=1
         end if
13     CONTINUE 

      cont_init_drop=0.
      cont_init_ice=0.
      do kr=1,nkr
         cont_init_drop=cont_init_drop+g1(kr)
         cont_init_ice=cont_init_ice+g3(kr)+g4(kr)+g5(kr)
         do ice=1,icemax
            cont_init_ice=cont_init_ice+g2(kr,ice)
         enddo
      enddo
      cont_init_drop=col*cont_init_drop*1.e-3
      cont_init_ice=col*cont_init_ice*1.e-3

      alwc=cont_init_drop*1.e6




      if (icol_drop.eq.1)then 


       call coll_xxx (G1,CWLL,XL_MG,CHUCM,IMA,NKR)

       if(icol_drop_brk.eq.1)then
       ndiv=1
10     continue
       do it = 1,ndiv
         if (ndiv.gt.1024)print*,'ndiv in coal_bott_new = ',ndiv
         if (ndiv.gt.10000) call wrf_error_fatal3("<stdin>",8076,&
"fatal error in module_mp_full_sbm (ndiv.gt.10000), model stop")
         dtbreakup = dt_coll/ndiv
         if (it.eq.1)then

          do kr=1,JMAX
           gdumb(kr)= g1(kr)*1.D-3
           xl_dumb(kr)=xl_mg(KR)*1.D-3
          end do
          break_drop_bef=0.d0

          do kr=1,JMAX
            break_drop_bef=break_drop_bef+g1(kr)*1.D-3
          enddo
         end if
         call breakup(gdumb,xl_dumb,dtbreakup,brkweight, &
     &        pkij,qkj,JMAX,jbreak)
       end do
       break_drop_aft=0.0d0
       do kr=1,JMAX
           break_drop_aft=break_drop_aft+gdumb(kr)
       enddo
       break_drop_per=break_drop_aft/break_drop_bef
       if (break_drop_per.gt.1.001)then
           ndiv=ndiv*2
           GO TO 10
       else
           do kr=1,JMAX
            g1(kr)=gdumb(kr)*1.D3
           end do
       end if
       end if
      end if
       if (icol_snow.eq.1)then 
         call coll_xyz (g1,g3,g4,cwls,xl_mg,xs_mg, &
     &                chucm,ima,prdkrn1,nkr,0)
         if(alwc.lt.alcr) then
         call coll_xyx (g3,g1,cwsl,xs_mg,xl_mg, &
     &                chucm,ima,prdkrn1,nkr,1)
         endif
         if(alwc.ge.alcr) then


            call coll_xyxz_h (g3,g1,g4,cwsl,xs_mg,xl_mg, &
     &                chucm,ima,prdkrn1,nkr,1)
         endif

       end if





       if (icol_graupel.eq.1)then 


         call coll_xyyz_h (g1,g4,g5,cwlg,xl_mg,xg_mg, &
       &                chucm,ima,prdkrn1,nkr,1)

          conc_old=0.
          conc_new=0.
          do kr=kr_icempl,nkr
               conc_old=conc_old+col*g1(kr)/xl_mg(kr)
          enddo

           if(alwc.lt.alcr_g) then


            call coll_xyy (g1,g4,cwlg,xl_mg,xg_mg, &
     &               chucm,ima,prdkrn1,nkr,0)
            call coll_xyx (g4,g1,cwgl,xg_mg,xl_mg, &
     &          chucm,ima,prdkrn1,nkr,1)

           else
            call coll_xyxz_h (g4,g1,g5,cwgl,xg_mg,xl_mg, &
     &                chucm,ima,prdkrn1,nkr,1)
           end if


         if(icempl.eq.1) then
          if(tt.ge.265.15.and.tt.le.tcrit) then

            do kr=kr_icempl,nkr
               conc_new=conc_new+col*g1(kr)/xl_mg(kr)
            enddo
            dconc=conc_old-conc_new
            if(tt.le.268.15) then
              conc_icempl=dconc*4.e-3*(265.15-tt)/(265.15-268.15)
            endif
            if(tt.gt.268.15) then
             conc_icempl=dconc*4.e-3*(tcrit-tt)/(tcrit-268.15)
            endif

            g2_2(1)=g2_2(1)+conc_icempl*xi2_mg(1)/col


          endif

         endif


       endif


       if(icol_hail.eq.1) then
        call coll_xyy (g1,g5,cwlh,xl_mg,xh_mg, &
     &               chucm,ima,prdkrn1,nkr,0)
        call coll_xyx (g5,g1,cwhl,xh_mg,xl_mg, &
     &               chucm,ima,prdkrn1,nkr,1)

       endif







       if(icol_column.eq.1) then
        if(tt.lt.tcrit) then
         call coll_xyz (g1,g2_1,g4,cwli_1,xl_mg,xi1_mg, &
     &                 chucm,ima,prdkrn,nkr,0)
        endif
        if(tt.ge.tcrit) then
         call coll_xyz (g1,g2_1,g5,cwli_1,xl_mg,xi1_mg, &
     &                 chucm,ima,prdkrn,nkr,0)
        endif
        call coll_xyxz (g2_1,g1,g4,cwil_1,xi1_mg,xl_mg, &
     &                 chucm,ima,prdkrn,nkr,1)
        call coll_xyx (g2_1,g1,cwil_1,xi1_mg,xl_mg, &
     &                 chucm,ima,prdkrn,nkr,1)

       endif







       if(icol_plate.eq.1) then
        if(tt.lt.tcrit) then
         call coll_xyz (g1,g2_2,g4,cwli_2,xl_mg,xi2_mg, &
     &                 chucm,ima,prdkrn,nkr,0)
        endif
        if(tt.ge.tcrit) then
         call coll_xyz (g1,g2_2,g5,cwli_2,xl_mg,xi2_mg, &
     &                 chucm,ima,prdkrn,nkr,0)
        endif
        call coll_xyxz (g2_2,g1,g4,cwil_2,xi2_mg,xl_mg, &
     &                 chucm,ima,prdkrn,nkr,1)
        call coll_xyx (g2_2,g1,cwil_2,xi2_mg,xl_mg, &
     &                 chucm,ima,prdkrn,nkr,1)

       endif






       if(icol_dendrite.eq.1) then
        if(tt.lt.tcrit) then
         call coll_xyz (g1,g2_3,g4,cwli_3,xl_mg,xi3_mg, &
     &                 chucm,ima,prdkrn,nkr,0)
        endif
        if(tt.ge.tcrit) then
         call coll_xyz (g1,g2_3,g5,cwli_3,xl_mg,xi3_mg, &
     &                 chucm,ima,prdkrn,nkr,0)
        endif
        call coll_xyxz (g2_3,g1,g4,cwil_3,xi3_mg,xl_mg, &
     &                 chucm,ima,prdkrn,nkr,1)
        call coll_xyx (g2_3,g1,cwil_3,xi3_mg,xl_mg, &
     &                 chucm,ima,prdkrn,nkr,1)

       endif







      if(tt.gt.TTCOAL) then

       if(icol_column.eq.1) then

        call coll_xxy (g2_1,g3,cwii_1_1,xi1_mg, &
     &                 chucm,ima,prdkrn,nkr)



        if(icol_plate.eq.1) then     
         call coll_xyz (g2_1,g2_2,g3,cwii_1_2,xi1_mg,xi2_mg, &
     &                 chucm,ima,prdkrn,nkr,0)
         call coll_xyz (g2_2,g2_1,g3,cwii_2_1,xi2_mg,xi1_mg, &
     &                 chucm,ima,prdkrn,nkr,1)
        end if



        if(icol_dendrite.eq.1) then
           call coll_xyz (g2_1,g2_3,g3,cwii_1_3,xi1_mg,xi3_mg, &
     &                 chucm,ima,prdkrn,nkr,0)
           call coll_xyz (g2_3,g2_1,g3,cwii_3_1,xi3_mg,xi1_mg, &
     &                 chucm,ima,prdkrn,nkr,1)
        end if




        if(icol_snow.eq.1) then
         call coll_xyy (g2_1,g3,cwis_1,xi1_mg,xs_mg, &
     &                 chucm,ima,prdkrn,nkr,0)
         call coll_xyx (g3,g2_1,cwsi_1,xs_mg,xi1_mg, &
     &                 chucm,ima,prdkrn,nkr,1)
        endif          

       endif



       if(icol_plate.eq.1) then
        call coll_xxy (g2_2,g3,cwii_2_2,xi2_mg, &
     &                 chucm,ima,prdkrn,nkr)



        if(icol_dendrite.eq.1) then
         call coll_xyz (g2_2,g2_3,g3,cwii_2_3,xi2_mg,xi3_mg, &
     &                 chucm,ima,prdkrn,nkr,0)
         call coll_xyz (g2_3,g2_2,g3,cwii_3_2,xi3_mg,xi2_mg, &
     &                 chucm,ima,prdkrn,nkr,1)
        end if



        if(icol_snow.eq.1) then

         call coll_xyy (g2_2,g3,cwis_2,xi2_mg,xs_mg, &
     &                 chucm,ima,prdkrn,nkr,0)
          call coll_xyx (g3,g2_2,cwsi_2,xs_mg,xi2_mg, &
     &                 chucm,ima,prdkrn,nkr,1)
         end if

       endif



       if(icol_dendrite.eq.1) then
         call coll_xxy (g2_3,g3,cwii_3_3,xi3_mg, &
      &                  chucm,ima,prdkrn,nkr)



        if(icol_snow.eq.1) then

         call coll_xyy (g2_3,g3,cwis_3,xi3_mg,xs_mg,  &
     &                 chucm,ima,prdkrn,nkr,0)
          call coll_xyx (g3,g2_3,cwsi_3,xs_mg,xi3_mg, &
     &                 chucm,ima,prdkrn,nkr,1)
        end if

       endif


        if(icol_snow.ne.0) then


         call coll_xxx_prd (g3,cwss,xs_mg,chucm,ima,prdkrn,nkr)



         if(icol_graupel.eq.1) then
           call coll_xyx (g3,g4,cwsg,xs_mg,xg_mg, &
     &                chucm,ima,prdkrn,nkr,1)

         endif

        endif


      endif


      cont_fin_drop=0.
      cont_fin_ice=0.
      do kr=1,nkr
         g2(kr,1)=g2_1(kr)
         g2(kr,2)=g2_2(kr)
         g2(kr,3)=g2_3(kr)
         cont_fin_drop=cont_fin_drop+g1(kr)
         cont_fin_ice=cont_fin_ice+g3(kr)+g4(kr)+g5(kr)

         do ice=1,icemax
            cont_fin_ice=cont_fin_ice+g2(kr,ice)
         enddo
      enddo
      cont_fin_drop=col*cont_fin_drop*1.e-3
      cont_fin_ice=col*cont_fin_ice*1.e-3
      deldrop=cont_init_drop-cont_fin_drop


      if(t_new.le.273.15) then
        if(deldrop.ge.0.) then
          t_new=t_new+320.*deldrop/rho
        else

          if(abs(deldrop).gt.cont_init_drop*0.05) then
            call wrf_error_fatal3("<stdin>",8385,&
"fatal error in module_mp_full_sbm (abs(deldrop).gt.cont_init_drop), model stop")
          endif
        endif
       endif

61    continue

        DO 15 KR=1,NKR
         FF1R(KR)=G1(KR)/(3.*XL(KR)*XL(KR)*1.E3)
         FF2R(KR,1)=G2(KR,1)/(3*xi(KR,1)*XI(KR,1)*1.e3)
         FF2R(KR,2)=G2(KR,2)/(3.*xi(KR,2)*XI(KR,2)*1.e3)
         FF2R(KR,3)=G2(KR,3)/(3.*xi(KR,3)*XI(KR,3)*1.e3)
         FF3R(KR)=G3(KR)/(3.*xs(kr)*xs(kr)*1.e3)
         FF4R(KR)=G4(KR)/(3.*xg(kr)*xg(kr)*1.e3)
         FF5R(KR)=G5(KR)/(3.*xh(kr)*xh(kr)*1.e3)
15     CONTINUE 
      tt=t_new
      RETURN
      END SUBROUTINE COAL_BOTT_NEW
      SUBROUTINE MISC1(PP,cwll_1000mb,cwll_750mb,cwll_500mb, &
     &      cwll,nkr)
      IMPLICIT NONE
      INTEGER kr1,kr2,NKR
      DOUBLE PRECISION PP
      REAL P_Z
      double precision cwll(nkr,nkr),cwll_1,cwll_2,cwll_3 &
     &,cwll_1000mb(nkr,nkr),cwll_750mb(nkr,nkr),cwll_500mb(nkr,nkr)
      P_Z=PP
              do 12 kr1=1,nkr
              do 12 kr2=1,nkr
               cwll_1=cwll_1000mb(kr1,kr2)
               cwll_2=cwll_750mb(kr1,kr2)
               cwll_3=cwll_500mb(kr1,kr2)
               if(p_z.ge.p1) cwll(kr1,kr2)=cwll_1
               if(p_z.eq.p2) cwll(kr1,kr2)=cwll_2
               if(p_z.eq.p3) cwll(kr1,kr2)=cwll_3
               if(p_z.lt.p1.and.p_z.gt.p2) &
     &         cwll(kr1,kr2)=cwll_2+ &
     &         (cwll_1-cwll_2)*(p_z-p2)/(p1-p2) 
               if(p_z.lt.p2.and.p_z.gt.p3) &
     &         cwll(kr1,kr2)=cwll_3+ &
     &         (cwll_2-cwll_3)*(p_z-p3)/(p2-p3)
               if(p_z.lt.p3) cwll(kr1,kr2)=cwll_3
12            CONTINUE 
      RETURN
      END SUBROUTINE  MISC1

        subroutine coll_xxx (g,ckxx,x,chucm,ima,nkr)
        implicit double precision (a-h,o-z)
        dimension g(nkr),ckxx(nkr,nkr),x(0:nkr)
        dimension chucm(nkr,nkr)
        double precision ima(nkr,nkr)
        gmin=1.d-60


        do i=1,nkr-1
           ix0=i
           if(g(i).gt.gmin) goto 2000
        enddo
 2000   continue
        if(ix0.eq.nkr-1) goto 2020
        do i=nkr-1,1,-1
           ix1=i
           if(g(i).gt.gmin) goto 2010
        enddo
 2010   continue





        do i=ix0,ix1-1
           do j=i+1,ix1

              k=ima(i,j)
              kp=k+1
              x0=ckxx(i,j)*g(i)*g(j)
              x0=min(x0,g(i)*x(j))
              if(j.ne.k) then
                x0=min(x0,g(j)*x(i))
              endif
              gsi=x0/x(j)
              gsj=x0/x(i)
              gsk=gsi+gsj
              g(i)=g(i)-gsi
              if(g(i).lt.0.d0) g(i)=0.d0
              g(j)=g(j)-gsj
              gk=g(k)+gsk
              if(g(j).lt.0.d0.and.gk.lt.gmin) then
                g(j)=0.d0
                g(k)=g(k)+gsi
              endif
              flux=0.d0


              if(gk.gt.gmin) then
                x1=dlog(g(kp)/gk+1.d-15)
               if (x1.eq.0)then
                flux=0  
               else
                flux=gsk/x1*(dexp(0.5*x1)-dexp(x1*(0.5-chucm(i,j))))
                flux=min(flux,gsk)
               end if


                g(k)=gk-flux
                if(g(k).lt.0.d0) g(k)=0.d0
                g(kp)=g(kp)+flux

              endif
            end do
        end do
 2020   continue
        return
        end subroutine coll_xxx
        subroutine coll_xxx_prd (g,ckxx,x,chucm,ima,prdkrn,nkr)
        implicit double precision (a-h,o-z)
        dimension g(nkr),ckxx(nkr,nkr),x(0:nkr)
        dimension chucm(nkr,nkr)
        double precision ima(nkr,nkr)

        gmin=1.d-60


        do i=1,nkr-1
           ix0=i
           if(g(i).gt.gmin) goto 2000
        enddo
 2000   continue
        if(ix0.eq.nkr-1) goto 2020
        do i=nkr-1,1,-1
           ix1=i
           if(g(i).gt.gmin) goto 2010
        enddo
 2010   continue





        do i=ix0,ix1-1
           do j=i+1,ix1

              k=ima(i,j)
              kp=k+1
              x0=ckxx(i,j)*g(i)*g(j)*prdkrn
              x0=min(x0,g(i)*x(j))
              if(j.ne.k) then
                x0=min(x0,g(j)*x(i))
              endif
              gsi=x0/x(j)
              gsj=x0/x(i)
              gsk=gsi+gsj
              g(i)=g(i)-gsi
              if(g(i).lt.0.d0) g(i)=0.d0
              g(j)=g(j)-gsj
              gk=g(k)+gsk
              if(g(j).lt.0.d0.and.gk.lt.gmin) then
                g(j)=0.d0
                g(k)=g(k)+gsi
              endif
              flux=0.d0


              if(gk.gt.gmin) then
                x1=dlog(g(kp)/gk+1.d-15)
               if (x1.eq.0)then
                flux=0  
               else
                flux=gsk/x1*(dexp(0.5*x1)-dexp(x1*(0.5-chucm(i,j))))
                flux=min(flux,gsk)
               end if


                g(k)=gk-flux
                if(g(k).lt.0.d0) g(k)=0.d0
                g(kp)=g(kp)+flux

              endif
            end do
        end do
 2020   continue
        return
        end subroutine coll_xxx_prd 
      subroutine modkrn(TT,QQ,PP,PRDKRN,TTCOAL)
      implicit none
      real epsf,tc,ttt1,ttt,factor,qs2,qq1,dele,f,factor_t
      double precision TT,QQ,PP,satq2,t,p
      double precision prdkrn
      REAL at,bt,ct,dt,temp,a,b,c,d,tc_min,tc_max
       real factor_max,factor_min
      REAL TTCOAL
	data at,bt,ct,dt/0.88333,0.0931878,0.0034793,4.5185186e-05/
        satq2(t,p)=3.80e3*(10**(9.76421-2667.1/t))/p
        temp(a,b,c,d,tc)=d*tc*tc*tc+c*tc*tc+b*tc+a
        IF (QQ.LE.0)QQ=1.E-12
        epsf    =.5
        tc      =tt-273.15
        factor=1  
        if(tc.le.0) then

          ttt1  =temp(at,bt,ct,dt,tc)
          ttt   =ttt1
          qs2   =satq2(tt,pp)
          qq1   =qq*(0.622+0.378*qs2)/(0.622+0.378*qq)/qs2
          dele  =ttt*qq1

          if(tc.ge.-6.) then
            factor = dele
            if(factor.lt.epsf) factor=epsf
            if(factor.gt.1.) factor=1.

          endif                        
          factor_t=factor
          if(tc.ge.-12.5.and.tc.lt.-6.) factor_t=0.5
          if(tc.ge.-17.0.and.tc.lt.-12.5) factor_t=1.
          if(tc.ge.-20.0.and.tc.lt.-17.) factor_t=0.4
          if(tc.lt.-20.) then
            tc_min=ttcoal-273.15
            tc_max=-20.
            factor_max=0.25
            factor_min=0.
            f=factor_min+(tc-tc_min)*(factor_max-factor_min)/  &
     &                               (tc_max-tc_min)
            factor_t=f
          endif

          if (factor_t.lt.0)factor_t=0.01
          prdkrn=factor_t
      else
          prdkrn=1.d0
      end if
      RETURN
      END SUBROUTINE modkrn 
           


        subroutine coll_xxy(gx,gy,ckxx,x,chucm,ima,prdkrn,nkr)
        implicit double precision (a-h,o-z)
        dimension chucm(nkr,nkr)
        double precision ima(nkr,nkr)
        dimension  &
     &  gx(nkr),gy(nkr),ckxx(nkr,nkr),x(0:nkr)
        gmin=1.d-60

        do i=1,nkr-1
           ix0=i
           if(gx(i).gt.gmin) goto 2000
        enddo
        if(ix0.eq.nkr-1) goto 2020
 2000   continue
        do i=nkr-1,1,-1
           ix1=i
           if(gx(i).gt.gmin) goto 2010
        enddo
 2010   continue

        do i=ix0,ix1
           do j=i,ix1
              k=ima(i,j)
              kp=k+1
              x0=ckxx(i,j)*gx(i)*gx(j)*prdkrn
              x0=min(x0,gx(i)*x(j))
              x0=min(x0,gx(j)*x(i))
              gsi=x0/x(j)
              gsj=x0/x(i)
              gsk=gsi+gsj
              gx(i)=gx(i)-gsi
              if(gx(i).lt.0.d0) gx(i)=0.d0
              gx(j)=gx(j)-gsj
              if(gx(j).lt.0.d0) gx(j)=0.d0
              gk=gy(k)+gsk
              flux=0.d0

              if(gk.gt.gmin) then

                x1=dlog(gy(kp)/gk+1.d-15)








               if (x1.eq.0)then
                flux=0  
               else
                flux=gsk/x1*(dexp(0.5*x1)-dexp(x1*(0.5-chucm(i,j))))
                flux=min(flux,gsk)
               end if
                gy(k)=gk-flux
                if(gy(k).lt.0.d0) gy(k)=0.d0
                gy(kp)=gy(kp)+flux

              endif
           enddo
        enddo
 2020   continue
        return
        end subroutine coll_xxy

        subroutine coll_xyy(gx,gy,ckxy,x,y,chucm,ima, &
     &     prdkrn,nkr,indc)
        implicit double precision (a-h,o-z)
        dimension  &
     &  gy(nkr),gx(nkr),ckxy(nkr,nkr),x(0:nkr),y(0:nkr)
        dimension chucm(nkr,nkr)
        double precision ima(nkr,nkr)
        gmin=1.d-60

        do i=1,nkr-1
           ix0=i
           if(gx(i).gt.gmin) go to 2000
        enddo
 2000   continue
        if(ix0.eq.nkr-1) goto 2020
        do i=nkr-1,1,-1
           ix1=i
           if(gx(i).gt.gmin) go to 2010
        enddo
 2010   continue

        do i=1,nkr-1
           iy0=i
           if(gy(i).gt.gmin) go to 2001
        enddo
 2001   continue
        if(iy0.eq.nkr-1) goto 2020
        do i=nkr-1,1,-1
           iy1=i
           if(gy(i).gt.gmin) go to 2011
        enddo
 2011   continue

        do i=iy0,iy1
           jmin=i
           if(jmin.eq.(nkr-1)) goto 2020
           if(i.lt.ix0) jmin=ix0-indc
	   do j=jmin+indc,ix1         
              k=ima(i,j)
              kp=k+1
              x0=ckxy(j,i)*gy(i)*gx(j)*prdkrn
              x0=min(x0,gy(i)*x(j))
              x0=min(x0,gx(j)*y(i))
              gsi=x0/x(j)
              gsj=x0/y(i)
              gsk=gsi+gsj
              gy(i)=gy(i)-gsi
              if(gy(i).lt.0.d0) gy(i)=0.d0
              gx(j)=gx(j)-gsj
              if(gx(j).lt.0.d0) gx(j)=0.d0
              gk=gy(k)+gsk
              flux=0.d0

              if(gk.gt.gmin) then
                x1=dlog(gy(kp)/gk+1.d-15)






               if (x1.eq.0)then
                flux=0  
               else
                flux=gsk/x1*(dexp(0.5*x1)-dexp(x1*(0.5-chucm(i,j))))
                flux=min(flux,gsk)
               end if

                gy(k)=gk-flux
                if(gy(k).lt.0.d0) gy(k)=0.d0
                gy(kp)=gy(kp)+flux

              endif

           enddo
        enddo
 2020   continue
        return
        end subroutine coll_xyy

        subroutine coll_xyx(gx,gy,ckxy,x,y,chucm,ima, &
     &    prdkrn,nkr,indc)
        implicit double precision (a-h,o-z)
        dimension gy(nkr),gx(nkr),ckxy(nkr,nkr),x(0:nkr),y(0:nkr)
        dimension chucm(nkr,nkr)
        double precision ima(nkr,nkr)
        gmin=1.d-60

        do i=1,nkr-1
           ix0=i
           if(gx(i).gt.gmin) go to 2000
        enddo
 2000   continue
        if(ix0.eq.nkr-1) goto 2020
        do i=nkr-1,1,-1
           ix1=i
           if(gx(i).gt.gmin) go to 2010
        enddo
 2010   continue

        do i=1,nkr-1
           iy0=i
           if(gy(i).gt.gmin) go to 2001
        enddo
 2001   continue
        if(iy0.eq.nkr-1) goto 2020
        do i=nkr-1,1,-1
           iy1=i
           if(gy(i).gt.gmin) go to 2011
        enddo
 2011   continue

        do i=iy0,iy1
           jmin=i
           if(jmin.eq.(nkr-1)) goto 2020
           if(i.lt.ix0) jmin=ix0-indc
	   do j=jmin+indc,ix1
              k=ima(i,j)
              kp=k+1
              x0=ckxy(j,i)*gy(i)*gx(j)*prdkrn
              x0=min(x0,gy(i)*x(j))
              if(j.ne.k) then
                x0=min(x0,gx(j)*y(i))
              endif
              gsi=x0/x(j)
              gsj=x0/y(i)
              gsk=gsi+gsj
              gy(i)=gy(i)-gsi
              if(gy(i).lt.0.d0) gy(i)=0.d0
              gx(j)=gx(j)-gsj
              gk=gx(k)+gsk






              if(gx(j).lt.0.d0.and.gk.lt.gmin) then
                gx(j)=0.d0
                gx(k)=gx(k)+gsi
              endif
              flux=0.d0            

              if(gk.gt.gmin) then
                x1=dlog(gx(kp)/gk+1.d-15)






               if (x1.eq.0)then
                flux=0  
               else
                flux=gsk/x1*(dexp(0.5*x1)-dexp(x1*(0.5-chucm(i,j))))
                flux=min(flux,gsk)
               end if

                gx(k)=gk-flux
                if(gx(k).lt.0.d0) gx(k)=0.d0
                gx(kp)=gx(kp)+flux

              endif


10         continue
           enddo
        enddo
 2020   continue
        return
        end subroutine coll_xyx

        subroutine coll_xyxz(gx,gy,gz,ckxy,x,y,chucm,ima, &
     &    prdkrn,nkr,indc)
        implicit double precision (a-h,o-z)
      dimension gy(nkr),gx(nkr),gz(nkr),ckxy(nkr,nkr),x(0:nkr),y(0:nkr)
        dimension chucm(nkr,nkr)
        double precision ima(nkr,nkr)
        gmin=1.d-60

        do i=1,nkr-1
           ix0=i
           if(gx(i).gt.gmin) go to 2000
        enddo
 2000   continue
        if(ix0.eq.nkr-1) goto 2020
        do i=nkr-1,1,-1
           ix1=i
           if(gx(i).gt.gmin) go to 2010
        enddo
 2010   continue

        do i=1,nkr-1
           iy0=i
           if(gy(i).gt.gmin) go to 2001
        enddo
 2001   continue
        if(iy0.eq.nkr-1) goto 2020
        do i=nkr-1,1,-1
           iy1=i
           if(gy(i).gt.gmin) go to 2011
        enddo
 2011   continue

        do i=iy0,iy1
           jmin=i
           if(jmin.eq.(nkr-1)) goto 2020
           if(i.lt.ix0) jmin=ix0-indc
	   do j=jmin+indc,ix1
              k=ima(i,j)
              kp=k+1
              x0=ckxy(j,i)*gy(i)*gx(j)*prdkrn
              x0=min(x0,gy(i)*x(j))
              if(j.ne.k) then
                x0=min(x0,gx(j)*y(i))
              endif
              gsi=x0/x(j)
              gsj=x0/y(i)
              gsk=gsi+gsj
              gy(i)=gy(i)-gsi
              if(gy(i).lt.0.d0) gy(i)=0.d0
              gx(j)=gx(j)-gsj
              gk=gx(k)+gsk
              if(gx(j).lt.0.d0.and.gk.lt.gmin) then
                gx(j)=0.d0
                gx(k)=gx(k)+gsi
              endif
              flux=0.d0

              if(kp.lt.17) gkp=gx(kp)
              if(kp.ge.17) gkp=gz(kp)
              if(gk.gt.gmin) then
                x1=dlog(gkp/gk+1.d-15)






               if (x1.eq.0)then
                flux=0  
               else
                flux=gsk/x1*(dexp(0.5*x1)-dexp(x1*(0.5-chucm(i,j))))
                flux=min(flux,gsk)
               end if

                gx(k)=gk-flux
                if(gx(k).lt.0.d0) gx(k)=0.d0
                if(kp.lt.17) gx(kp)=gkp+flux
                if(kp.ge.17) gz(kp)=gkp+flux



              endif

           enddo
        enddo
 2020   continue
        return
        end subroutine coll_xyxz

        subroutine coll_xyxz_h(gx,gy,gz,ckxy,x,y,chucm,ima, &
     &    prdkrn,nkr,indc)
        implicit double precision (a-h,o-z)
      dimension gy(nkr),gx(nkr),gz(nkr),ckxy(nkr,nkr),x(0:nkr),y(0:nkr)
        dimension chucm(nkr,nkr)
        double precision ima(nkr,nkr)
        gmin=1.d-60

        do i=1,nkr-1
           ix0=i
           if(gx(i).gt.gmin) go to 2000
        enddo
 2000   continue
        if(ix0.eq.nkr-1) goto 2020
        do i=nkr-1,1,-1
           ix1=i
           if(gx(i).gt.gmin) go to 2010
        enddo
 2010   continue

        do i=1,nkr-1
           iy0=i
           if(gy(i).gt.gmin) go to 2001
        enddo
 2001   continue
        if(iy0.eq.nkr-1) goto 2020
        do i=nkr-1,1,-1
           iy1=i
           if(gy(i).gt.gmin) go to 2011
        enddo
 2011   continue

        do i=iy0,iy1
           jmin=i
           if(jmin.eq.(nkr-1)) goto 2020
           if(i.lt.ix0) jmin=ix0-indc
	   do j=jmin+indc,ix1
              k=ima(i,j)
              kp=k+1
              x0=ckxy(j,i)*gy(i)*gx(j)*prdkrn
              x0=min(x0,gy(i)*x(j))
              if(j.ne.k) then
                x0=min(x0,gx(j)*y(i))
              endif
              gsi=x0/x(j)
              gsj=x0/y(i)
              gsk=gsi+gsj
              gy(i)=gy(i)-gsi
              if(gy(i).lt.0.d0) gy(i)=0.d0
              gx(j)=gx(j)-gsj
              gk=gx(k)+gsk
              if(gx(j).lt.0.d0.and.gk.lt.gmin) then
                gx(j)=0.d0
                gx(k)=gx(k)+gsi
              endif
              flux=0.d0

              if(kp.lt.22) gkp=gx(kp)
              if(kp.ge.22) gkp=gz(kp)
              if(gk.gt.gmin) then
                x1=dlog(gkp/gk+1.d-15)






               if (x1.eq.0)then
                flux=0  
               else
                flux=gsk/x1*(dexp(0.5*x1)-dexp(x1*(0.5-chucm(i,j))))
                flux=min(flux,gsk)
               end if

                gx(k)=gk-flux
                if(gx(k).lt.0.d0) gx(k)=0.d0
                if(kp.lt.22) gx(kp)=gkp+flux
                if(kp.ge.22) gz(kp)=gkp+flux



              endif

           enddo
        enddo
 2020   continue
        return
        end subroutine coll_xyxz_h

        subroutine coll_xyz(gx,gy,gz,ckxy,x,y,chucm,ima, &
     &                      prdkrn,nkr,indc)
        implicit double precision (a-h,o-z)
      dimension gx(nkr),gy(nkr),gz(nkr),ckxy(nkr,nkr),x(0:nkr),y(0:nkr)
        dimension chucm(nkr,nkr)
        double precision ima(nkr,nkr)
        gmin=1.d-60

        do i=1,nkr-1
           ix0=i
           if(gx(i).gt.gmin) go to 2000
        enddo
 2000   continue
        if(ix0.eq.nkr-1) goto 2020
        do i=nkr-1,1,-1
           ix1=i
           if(gx(i).gt.gmin) go to 2010
        enddo
 2010   continue

        do i=1,nkr-1
           iy0=i
           if(gy(i).gt.gmin) go to 2001
        enddo
 2001   continue
        if(iy0.eq.nkr-1) goto 2020
        do i=nkr-1,1,-1
           iy1=i
           if(gy(i).gt.gmin) go to 2011
        enddo
 2011   continue

        do i=iy0,iy1
           jmin=i
           if(jmin.eq.(nkr-1)) goto 2020
           if(i.lt.ix0) jmin=ix0-indc
	   do j=jmin+indc,ix1         
              k=ima(i,j)
              kp=k+1
              x0=ckxy(j,i)*gy(i)*gx(j)*prdkrn
              x0=min(x0,gy(i)*x(j))
              x0=min(x0,gx(j)*y(i))
              gsi=x0/x(j)
              gsj=x0/y(i)
              gsk=gsi+gsj
              gy(i)=gy(i)-gsi
              if(gy(i).lt.0.d0) gy(i)=0.d0
              gx(j)=gx(j)-gsj
              if(gx(j).lt.0.d0) gx(j)=0.d0
              gk=gz(k)+gsk
              flux=0.d0

              if(gk.gt.gmin) then
                x1=dlog(gz(kp)/gk+1.d-15)

               if (x1.eq.0)then
                flux=0  
               else
                flux=gsk/x1*(dexp(0.5*x1)-dexp(x1*(0.5-chucm(i,j))))
                flux=min(flux,gsk)
               end if

                gz(k)=gk-flux
                if(gz(k).lt.0.d0) gz(k)=0.d0
                gz(kp)=gz(kp)+flux

              endif
           enddo
        enddo
 2020   continue
        return
        end subroutine coll_xyz

        subroutine coll_xyyz_h(gx,gy,gz,ckxy,x,y,chucm,ima, &
     &    prdkrn,nkr,indc)
        implicit double precision (a-h,o-z)
      dimension gy(nkr),gx(nkr),gz(nkr),ckxy(nkr,nkr),x(0:nkr),y(0:nkr)
        dimension chucm(nkr,nkr)
        double precision ima(nkr,nkr)
        gmin=1.d-60

        do i=1,nkr-1
           ix0=i
           if(gx(i).gt.gmin) go to 2000
        enddo
 2000   continue
        if(ix0.eq.nkr-1) goto 2020
        do i=nkr-1,1,-1
           ix1=i
           if(gx(i).gt.gmin) go to 2010
        enddo
 2010   continue

        do i=1,nkr-1
           iy0=i
           if(gy(i).gt.gmin) go to 2001
        enddo
 2001   continue
        if(iy0.eq.nkr-1) goto 2020
        do i=nkr-1,1,-1
           iy1=i
           if(gy(i).gt.gmin) go to 2011
        enddo
 2011   continue

        do i=iy0,iy1
           jmin=i
           if(jmin.eq.(nkr-1)) goto 2020
           if(i.lt.ix0) jmin=ix0-indc
           do j=jmin+indc,ix1
              k=ima(i,j)
              kp=k+1
              x0=ckxy(j,i)*gy(i)*gx(j)*prdkrn
              x0=min(x0,gy(i)*x(j))
              if(j.ne.k) then
                x0=min(x0,gx(j)*y(i))
              endif
              gsi=x0/x(j)
              gsj=x0/y(i)
              gsk=gsi+gsj
              gy(i)=gy(i)-gsi
              if(gy(i).lt.0.d0) gy(i)=0.d0
              gx(j)=gx(j)-gsj
              gk=gx(k)+gsk
              if(gx(j).lt.0.d0.and.gk.lt.gmin) then
                gx(j)=0.d0
                gx(k)=gx(k)+gsi
              endif
              flux=0.d0

              if(kp.lt.25) gkp=gy(kp)
              if(kp.ge.25) gkp=gz(kp)
              if(gk.gt.gmin) then
                x1=dlog(gkp/gk+1.d-15)






               if (x1.eq.0)then
                flux=0
               else
                flux=gsk/x1*(dexp(0.5*x1)-dexp(x1*(0.5-chucm(i,j))))
                flux=min(flux,gsk)
               end if

                gx(k)=gk-flux
                if(gx(k).lt.0.d0) gx(k)=0.d0
                if(kp.lt.25) gy(kp)=gkp+flux
                if(kp.ge.25) gz(kp)=gkp+flux



              endif

           enddo
        enddo
 2020   continue
        return
        end subroutine coll_xyyz_h




      SUBROUTINE BREAKUP(GT_MG,XT_MG,DT,BRKWEIGHT, &
     &           PKIJ,QKJ,JMAX,JBREAK)










      INTEGER JMAX



      LOGICAL LTHAN
      INTEGER JBREAK,AP,IA,JA,KA,IE,JE,KE
      DOUBLE PRECISION EPS,NEGSUM

      PARAMETER (AP = 1)
      PARAMETER (IA = 1)
      PARAMETER (JA = 1)
      PARAMETER (KA = 1)
      PARAMETER (EPS = 1.D-20)

      INTEGER I,J,K,JJ,JDIFF
      DOUBLE PRECISION GT_MG(JMAX),XT_MG(0:JMAX),DT

      DOUBLE PRECISION BRKWEIGHT(JBREAK),PKIJ(JBREAK,JBREAK,JBREAK), &
     &    QKJ(JBREAK,JBREAK)
      DOUBLE PRECISION D0,ALM,HLP(JMAX)
      DOUBLE PRECISION FT(JMAX),FA(JMAX)
      DOUBLE PRECISION DG(JMAX),DF(JMAX),DBREAK(JBREAK),GAIN,LOSS
      REAL PI
      PARAMETER (PI = 3.1415927)
      INTEGER IP,KP,JP,KQ,JQ
      IE = JBREAK
      JE = JBREAK
      KE = JBREAK
















      JDIFF = JMAX - JBREAK





      DO J=1,JMAX
         FT(J) = GT_MG(J) / XT_MG(J)**2
      ENDDO



      DO K=1,KE
         FA(K) = FT(K+JDIFF)
      ENDDO







      DO K=1,KE
         GAIN = 0.0
         DO I=1,IE
            DO J=1,I
               GAIN = GAIN + FA(I)*FA(J)*PKIJ(K,I,J)
            ENDDO
         ENDDO
         LOSS = 0.0
         DO J=1,JE
            LOSS = LOSS + FA(J)*QKJ(K,J)
         ENDDO
         DBREAK(K) = BRKWEIGHT(K) * (GAIN - FA(K)*LOSS)
      ENDDO



      DO J=1,JDIFF
         DF(J) = 0.0
      ENDDO
      DO J=1,KE
         DF(J+JDIFF) = DBREAK(J)
      ENDDO


      DO J=1,JMAX
         DG(J) = DF(J) * XT_MG(J)**2
      ENDDO



      DO J=1,JMAX
      HLP(J) = 0.0
      NEGSUM = 0.0
         GT_MG(J) = GT_MG(J) + DG(J) * DT
         IF (GT_MG(J).LT.0) THEN
            HLP(J) = MIN(GT_MG(J),HLP(J))
            GT_MG(J) = EPS


         ENDIF
      ENDDO
























      RETURN

      END SUBROUTINE BREAKUP

      SUBROUTINE BOUNDNUM(MASSMM5,FCONC,RHOX,COL,NZERO, &
     &       RADXX,MASSXX,HYDROSUM, &
     &       NKR)
      IMPLICIT NONE
     
      INTEGER NKR,NKRI,KRBEG,KREND,IP,IPCNT
      REAL NZERO,LAMBDAHYD,MASSMM5,RHOX,HYDROMASS,COL
      REAL RADXX(NKR),MASSXX(NKR)
      REAL TERM1,TERM2A,TERM2B,TERM2C
      REAL FCONC(NKR),HYDROSUM 
      DOUBLE PRECISION D1,D2,D3,D4,D5,D6,D7A,D7B 
      DOUBLE PRECISION VAR1,VAR2,VAR3,VAR4,VAR5,VAR6













      LAMBDAHYD=(6.*NZERO/RHOX)*MASSXX(1)/(8.*RADXX(1)**3) &
     &     *(1./MASSMM5)
      LAMBDAHYD=SQRT(SQRT(LAMBDAHYD))
      HYDROSUM  =0
      TERM1=(NZERO/RHOX)*(MASSXX(1)/(8.*RADXX(1)**3))
      DO NKRI=1,NKR
       IF(NKRI.EQ.1)THEN
        D1=LAMBDAHYD*2.*RADXX(NKRI)
        D2=0
       ELSE
        D1=LAMBDAHYD*2.*RADXX(NKRI)
        D2=LAMBDAHYD*2.*RADXX(NKRI-1)
       END IF
       D3=DEXP(-D1)
       D4=DEXP(-D2)
       D5 = (1./LAMBDAHYD**4)
       D6=TERM1
       IF (NKRI.EQ.1)THEN
        D7A= -D5*D3*(D1**3+3.*D1**2+6.*D1+6)
        D7B=-6.*D5
       ELSE
        D7A= -D5*D3*(D1**3+3.*D1**2+6.*D1+6)
        D7B= -D5*D4*(D2**3+3.*D2**2+6.*D2+6)
       END IF
       HYDROMASS= D6*(D7A-D7B)
       HYDROSUM=HYDROSUM+HYDROMASS   
       FCONC(NKRI)=HYDROMASS*RHOX/(COL  &
     &          *MASSXX(NKRI)*MASSXX(NKRI)*3)
        IF (HYDROMASS .LT.0)THEN
        call wrf_error_fatal3("<stdin>",9404,&
"fatal error in module_mp_full_sbm (HYDROMASS.LT.0), model stop")
        END IF
      END DO

      IF (HYDROSUM.LT.MASSMM5)THEN
       D1=LAMBDAHYD*2.*RADXX(NKR)
       D2=LAMBDAHYD*2.*RADXX(NKR-1)
       D3=DEXP(-D1)
       D4=DEXP(-D2)
       D5 = (1./LAMBDAHYD**4)
       D6=TERM1
       D7A= -D5*D3*(D1**3+3.*D1**2+6.*D1+6)
       D7B= -D5*D4*(D2**3+3.*D2**2+6.*D2+6)
       HYDROMASS= D6*(D7A-D7B)+(MASSMM5-HYDROSUM)
       FCONC(NKR)=HYDROMASS*RHOX/(COL &
     &          *MASSXX(NKR)*MASSXX(NKR)*3)
       HYDROSUM=HYDROSUM+(MASSMM5-HYDROSUM)
      END IF

      RETURN
      END SUBROUTINE BOUNDNUM




SUBROUTINE MELTING &

(ihucm_flag &

,FF1,XL,VTL &
,FF2,XI,V2,VTC,FLIQFR_I,RHO_I &
,FF3,XS,V3,VTS,FLIQFR_S,RHO_S &
,FF4,XG,V4,VTG,FLIQFR_G,RHO_G &
,FF5,XH,V5,VTH,FLIQFR_H,RHO_H &
,XI_MELT,XS_MELT,XG_MELT,XH_MELT &
,TIN,rhoa,pres,DT,QQV)








implicit double precision (a-h,o-z)









PARAMETER(CP=1004.7D0, RV=461.51D0, RD=287.039D0, &
          EPS=RD/RV, FJOULES_IN_A_CAL=4.187D0, PI=3.141592654D0, &
          AR_LIM=2.D0, GRAV=9.8D0, RHO_ICE=920.D0, &
          RHO_WATER=1000.D0, FLIQFRAC_LIM=0.9D0, &
          PETIT_PARAMETRE=1.D-10)
PARAMETER (ivt_G_H_interpol=0)





PARAMETER(ISHEDDING_ON=1, IVT_ADJUST=1, IPRINTING=0, &
          ITEMP_ADJUST=1, IEVAP_ADJUST=1, ISUBLIME_ADJUST=1)














DIMENSION FF1(NKR), XL(NKR), VTL(NKR)

DIMENSION FF2(NKR,ICEMAX),XI(NKR,ICEMAX),V2(NKR,ICEMAX), &
          VTC(NKR,ICEMAX),FLIQFR_I(NKR,ICEMAX),RHO_I(NKR,ICEMAX)

DIMENSION FF3(NKR),XS(NKR),V3(NKR), &
          VTS(NKR),FLIQFR_S(NKR),RHO_S(NKR)

DIMENSION FF4(NKR),XG(NKR),V4(NKR), &
          VTG(NKR),FLIQFR_G(NKR),RHO_G(NKR)

DIMENSION FF5(NKR),XH(NKR),V5(NKR), &
          VTH(NKR),FLIQFR_H(NKR),RHO_H(NKR)

DIMENSION FF1_SI(NKR), XL_SI(NKR), VTL_SI(NKR)

DIMENSION FF2_SI(NKR,ICEMAX),XI_SI(NKR,ICEMAX),V2_SI(NKR,ICEMAX), &
          VTC_SI(NKR,ICEMAX),RHO_I_SI(NKR,ICEMAX)

DIMENSION FF3_SI(NKR),XS_SI(NKR),V3_SI(NKR), &
          VTS_SI(NKR), RHO_S_SI(NKR)
	  
DIMENSION FF4_SI(NKR),XG_SI(NKR),V4_SI(NKR), &
          VTG_SI(NKR), RHO_G_SI(NKR)

DIMENSION FF5_SI(NKR),XH_SI(NKR),V5_SI(NKR), &
          VTH_SI(NKR), RHO_H_SI(NKR)
DIMENSION &
XI_MELT(NKR,ICEMAX),XS_MELT(NKR),XG_MELT(NKR),XH_MELT(NKR)

DIMENSION &
XI_MELT_SI(NKR,ICEMAX),XS_MELT_SI(NKR),XG_MELT_SI(NKR),XH_MELT_SI(NKR)

INTRINSIC SUM

If(TIN <= 273.15D0) then
  RETURN
ENDIF

if(SUM(FF2) <= 0.D0.and.SUM(FF3) <= 0.D0.and.SUM(FF4) <= 0.D0.and. &
SUM(FF5) <= 0.D0) then
  return
endif





if(ihucm_flag == 1) then

RHO_I_SI = RHO_I*1000.D0
RHO_S_SI = RHO_S*1000.D0
RHO_G_SI = RHO_G*1000.D0
RHO_H_SI = RHO_H*1000.D0

XL_SI = XL/1000.D0
XI_SI = XI/1000.D0
XS_SI = XS/1000.D0
XG_SI = XG/1000.D0
XH_SI = XH/1000.D0

XI_MELT_SI = XI_SI
XS_MELT_SI = XS_SI
XG_MELT_SI = XG_SI
XH_MELT_SI = XH_SI

VTL_SI = VTL/100.D0
VTC_SI = VTC/100.D0
VTS_SI = VTS/100.D0



VTG_SI = VTG/100.D0
VTH_SI = VTH/100.D0

V2_SI = V2/100.D0
V3_SI = V3/100.D0
V4_SI = V4/100.D0
V5_SI = V5/100.D0

FF1_SI = 1.E9*FF1
FF2_SI = 1.E9*FF2
FF3_SI = 1.E9*FF3
FF4_SI = 1.E9*FF4
FF5_SI = 1.E9*FF5

pres_SI = pres/10.D0
rhoa_SI = rhoa*1000.D0



else



RHO_I_SI = RHO_I
RHO_S_SI = RHO_S
RHO_G_SI = RHO_G
RHO_H_SI = RHO_H

XL_SI = XL
XI_SI = XI
XS_SI = XS
XG_SI = XG
XH_SI = XH

XI_MELT_SI = XI_SI
XS_MELT_SI = XS_SI
XG_MELT_SI = XG_SI
XH_MELT_SI = XH_SI

VTL_SI = VTL
VTC_SI = VTC
VTS_SI = VTS
VTG_SI = VTG
VTH_SI = VTH

V2_SI = V2
V3_SI = V3
V4_SI = V4
V5_SI = V5

FF1_SI = FF1
FF2_SI = FF2
FF3_SI = FF3
FF4_SI = FF4
FF5_SI = FF5

pres_SI = pres
rhoa_SI = rhoa


endif






V2_SI(:,:) = VTC_SI(:,:)
V3_SI(:) = VTS_SI(:)
V4_SI(:) = VTG_SI(:)
V5_SI(:) = VTH_SI(:)

ee = QQV*pres_SI/(EPS + QQV)

es_zero = 611.21D0

if(pres_SI > 200000.D0.or.pres_SI < 10000.D0) call wrf_error_fatal3("<stdin>",9634,&
"fatal error in module_mp_full_sbm (PSI is wrong), model stop")

D_V=0.211D0*((TIN/273.15D0)**1.94D0)*(101325.D0/pres_SI)/1.D4




FK_a =(5.69D0+0.017D0*(TIN-273.15D0))*1.0D-3*4.187D0











XLV = 597.3D0
XLV = XLV*FJOULES_IN_A_CAL*1000.D0
XLS = 2.83D6



XLF = 79.7D0
XLF = XLF*FJOULES_IN_A_CAL*1000.D0



etaa = (1.718D0 + 0.0049D0*(TIN-273.15D0) - &
        1.2D-5*(TIN-273.15D0)*(TIN-273.15D0))*1.D-5



FNSC = etaa/(rhoa_SI*D_V)



ALPHA_H = FK_a/(CP*rhoa_SI)
FNPR = etaa/(rhoa_SI*ALPHA_H)
RHO_CRIT = 910.D0




if(rhoa_SI > 2.D0) call wrf_error_fatal3("<stdin>",9681,&
"fatal error in module_mp_full_sbm (rhoa_SI>2), model stop 111")

if(rhoa_SI < 0.1D0) then
  call wrf_error_fatal3("<stdin>",9685,&
"fatal error in module_mp_full_sbm (rhoa_SI<0.1), model stop 112")
endif

if(RHO_H_SI(1) < 1.D0) then
  call wrf_error_fatal3("<stdin>",9690,&
"fatal error in module_mp_full_sbm (RHO_H_SI(1) < 1.D0kg/m3), model stop 113")
endif




TS = SURFACE_TEMP(ee, TIN, XLS*D_V/(FK_a*RV), 1.D0, XLS, RV)
if(TS > 273.15D0) TS = 273.15D0


      





DO I = 1, ICEMAX

   I_MELT=I

DO IK = 1, NKR

   IK_MELT=IK

if(TIN > 273.15D0) then

IF(FLIQFR_I(IK,I).GE.1.D0.OR.FF2_SI(IK,I).LE.PETIT_PARAMETRE.OR. &
TIN <= 273.15D0) THEN
  IF(FLIQFR_I(IK,I) > 1.D0) FLIQFR_I(IK,I) = 1.D0
  CYCLE
ENDIF

rho_p=RHO_I_SI(IK,I)+FLIQFR_I(IK,I)*(RHO_WATER-RHO_I_SI(IK,I))
fm_i = XI_SI(IK,I)*(1.D0 - FLIQFR_I(IK,I))
fm_w = XI_SI(IK,I)*FLIQFR_I(IK,I)
V_p = (fm_i+fm_w)/rho_p
V_i = V_p
rhoi = fm_i/V_i



IF(I.eq.1) then

  AR_izero = column_AR(XI_SI(IK,I), RHO_I_SI(IK,I))
  AR_i = AR_izero + FLIQFR_I(IK,I)*(1.D0 - AR_izero)
  if(AR_i < AR_LIM) AR_i = AR_LIM
  CAP_izero = COLUMN_CAP_ZERO(fm_i, AR_i, rhoi, FL_star)

  vt_R = VTL_SI(IK)
  vt_start = VTC_SI(IK,I)
  vt = vt_start + (vt_R - vt_start) * chi_fra(fm_w/(fm_i+fm_w))

  fnre = vt *FL_star*rhoa_SI/etaa
  fv = COLUMN_VENTILATION_COEF(fnre, FNSC)


endif



IF(I.eq.2) then

  AR_izero = PLATE_AR(XI_SI(IK,I))
  AR_i = AR_izero + FLIQFR_I(IK,I)*(1.D0 - AR_izero)
  if(AR_i > 1.D0/AR_LIM) AR_i = 1.D0/AR_LIM

  CAP_izero = PLANAR_CAP_ZERO(fm_i, AR_i, rhoi, FL_star)

  vt_R = VTL_SI(IK)
  vt_start = VTC_SI(IK,I)
  vt = vt_start + (vt_R - vt_start) * chi_fra(fm_w/(fm_i+fm_w))

  fnre = vt * FL_star*rhoa_SI/etaa
  fv = PLATE_VENTILATION_COEF(fnre, FNSC)


endif



IF(I.eq.3) then

  AR_izero = DENDRITE_AR(XI_SI(IK,I))
  AR_i = AR_izero + FLIQFR_I(IK,I)*(1.D0 - AR_izero)
  if(AR_i > 1./AR_LIM) AR_i = 1.D0/AR_LIM

  CAP_izero = PLANAR_CAP_ZERO(fm_i, AR_i, rhoi, FL_star)

        vt_R = VTL_SI(IK)
        vt_start = VTC_SI(IK,I)
        vt = vt_start + (vt_R - vt_start) * chi_fra(fm_w/(fm_i+fm_w))

        fnre = vt * FL_star*rhoa_SI/etaa
        fv = DENDRITE_VENTILATION_COEF(fnre, FNSC)


endif



V2_SI(IK,I) = vt
CAP = CAP_izero*(0.8D0 + FLIQFR_I(IK,I)*0.2D0)

FICEMASS = XI_SI(IK,I) * (1.D0 - FLIQFR_I(IK,I))
DMELT = DT * ( 4.D0*PI*CAP*fv/XLF) * &
(FK_a*(TIN - 273.15D0) + (D_V*XLV/RV) * &
(ee/TIN - es_zero/273.15D0))



if(TS < 273.15D0 .and. FLIQFR_I(IK,I) <= 0.D0) DMELT = 0.D0



call fmass_limits(DMELT, FICEMASS, fm_w, XI_SI(IK,I))

if(ITEMP_ADJUST == 1) then

  call thermodynamical_limits &
 (FF2_SI(IK,I), XI_SI(IK,I), rhoa_SI, XLF/CP, TIN, DMELT)




endif

FICEMASS = FICEMASS - DMELT



FLIQFR_I(IK,I) = (XI_SI(IK,I) - FICEMASS)/XI_SI(IK,I)

if(FLIQFR_I(IK,I) < 0.D0) FLIQFR_I(IK,I) = 0.D0

if(FLIQFR_I(IK,I) > 0.D0) then

  if(IEVAP_ADJUST == 1 ) then

    if(FLIQFR_I(IK,I) > 1.D0) call wrf_error_fatal3("<stdin>",9829,&
"fatal error in module_mp_full_sbm (FLIQFR_I > 1), model stop 114")






    HEAT_EVAP=-DMELT*XLF+DT*(4.D0*PI*CAP*fv)*FK_a*(TIN-273.15D0)



    IF(HEAT_EVAP.LT.0.D0) THEN







      








    ENDIF
    


    FMASS_EVAP = HEAT_EVAP/XLV

    if(FMASS_EVAP > FLIQFR_I(IK,I) * XI_SI(IK,I)) then
      FMASS_EVAP = FLIQFR_I(IK,I) * XI_SI(IK,I)
    endif

    Q_EVAP=FMASS_EVAP*FF2_SI(IK,I)*XI_SI(IK,I)*3.D0*COL/rhoa_SI


    CALL EVAP_MELTWATER &
   (XI_SI(IK,I),rhoa_SI,Q_EVAP,FLIQFR_I(IK,I),FF2_SI(IK,I))

   XI_MELT_SI(IK,I)=XX_MELT







    TIN=TIN-XLV/CP*Q_EVAP

    QQV=QQV+Q_EVAP



    ee = QQV*pres_SI/(EPS + QQV)



    if(QQV < 0.D0) call wrf_error_fatal3("<stdin>",9892,&
"fatal error in module_mp_full_sbm , model stop 135")
    if(TIN < 150.D0) call wrf_error_fatal3("<stdin>",9894,&
"fatal error in module_mp_full_sbm , model stop 138")


  endif



else



  if(ISUBLIME_ADJUST == 1 ) then



    if(TS > 273.16) call wrf_error_fatal3("<stdin>",9910,&
"fatal error in module_mp_full_sbm , model stop 13655")
    sub_fac = (XLS/(RV*TIN) - 1.D0)*XLS/(FK_a * TIN)
    sub_fac = sub_fac + RV*TIN/((100.D0*GGESI(TS))*D_V)
    DSUB = -DT*4.D0*PI*CAP*fv*(ee/(100.D0*GGESI(TS)) - 1.D0)/sub_fac



    if(DSUB >  XI_SI(IK,I)) then
      DSUB = XI_SI(IK,I)
    endif

    Q_SUBL = DSUB*FF2_SI(IK,I)*XI_SI(IK,I)*3.D0*COL/rhoa_SI


    CALL SUBLIME_ICE &
   (XI_SI(IK,I),rhoa_SI,Q_SUBL,FF2_SI(IK,I))

    XI_MELT_SI(IK,I)=XX_MELT

    TIN=TIN-XLS/CP*Q_SUBL
    QQV=QQV+Q_SUBL



    ee = QQV*pres_SI/(EPS + QQV)



    if(QQV < 0.D0) call wrf_error_fatal3("<stdin>",9939,&
"fatal error in module_mp_full_sbm (QQV < 0), model stop ")
    if(TIN < 150.D0) call wrf_error_fatal3("<stdin>",9941,&
"fatal error in module_mp_full_sbm ( TIN < 150) , model stop ")



  endif


endif

if(FLIQFR_I(IK,I) < 0.D0) then
  FLIQFR_I(IK,I) = 0.D0
endif

IF(FLIQFR_I(IK,I) > FLIQFRAC_LIM) then

  if(XL_SI(IK).NE.XI_SI(IK,I)) call wrf_error_fatal3("<stdin>",9957,&
"fatal error in module_mp_full_sbm (QQV < 0), model stop 7011")

  if(ITEMP_ADJUST == 1) then


    Q_ICE_MELTED= &
    FICEMASS*FF2_SI(IK,I)*XI_SI(IK,I)*3.D0*COL/rhoa_SI

    TIN=TIN-XLF/CP*Q_ICE_MELTED



  endif

  FF1_SI(IK) = FF1_SI(IK) + FF2_SI(IK,I)



  FLIQFR_I(IK,I) = 1.D0

  FF2_SI(IK,I) = 0.D0



ENDIF



endif

ENDDO


ENDDO







DO IK = 1, NKR

   IK_MELT=IK
   I_MELT=0

if(TIN > 273.15D0) then

IF(FLIQFR_S(IK).GE.1.D0.OR.FF3_SI(IK).LE.PETIT_PARAMETRE.OR. &
TIN <= 273.15D0) THEN
  IF(FLIQFR_S(IK) > 1.D0) FLIQFR_S(IK) = 1.D0
CYCLE
ENDIF

rho_p = RHO_S_SI(IK) + FLIQFR_S(IK)* &
       (RHO_WATER - RHO_S_SI(IK))

fm_i = XS_SI(IK)*(1.D0 - FLIQFR_S(IK))
fm_w = XS_SI(IK)*FLIQFR_S(IK)
V_p = (fm_i + fm_w)/rho_p
V_i = V_p
rhoi = fm_i/V_i














AR_p = 0.3D0 + 0.7D0 * FLIQFR_S(IK)









AR_i = AR_p

CAP_izero = PLANAR_CAP_ZERO(fm_i, AR_i, rhoi, FL_star)
CAP = CAP_izero*(0.8D0 + FLIQFR_S(IK)*0.2D0)

vt_R = VTL_SI(IK)
vt_start = VTS_SI(IK)
vt = vt_start + (vt_R - vt_start) * chi_fra(fm_w/(fm_i+fm_w))
fnre = FL_star * vt*rhoa_SI/etaa



fv = SNOW_VENTILATION_COEF(fnre, FNSC, AR_i)



V3_SI(IK) = vt

FICEMASS = XS_SI(IK) * (1.D0 - FLIQFR_S(IK))

DMELT = DT * ( 4.D0*PI*CAP*fv/XLF) * &
(FK_a*(TIN - 273.15D0) + (D_V*XLV/RV) * &
(ee/TIN - es_zero/273.15D0))



if(TS < 273.15D0 .and. FLIQFR_S(IK) <= 0.D0) DMELT = 0.D0



call fmass_limits(DMELT, FICEMASS, fm_w, XS_SI(IK))

if(ITEMP_ADJUST == 1) then


  call thermodynamical_limits &
 (FF3_SI(IK), XS_SI(IK), rhoa_SI, XLF/CP, TIN, DMELT)



endif

FICEMASS = FICEMASS - DMELT

FLIQFR_S(IK) = (XS_SI(IK) - FICEMASS)/XS_SI(IK)

if(FLIQFR_S(IK) < 0.D0) then
  FLIQFR_S(IK) = 0.D0
endif

if(FLIQFR_S(IK) > 0.D0) then

  if(IEVAP_ADJUST == 1) then

    if(FLIQFR_S(IK) > 1.D0) call wrf_error_fatal3("<stdin>",10098,&
"fatal error in module_mp_full_sbm , model stop 905")





    HEAT_EVAP=-DMELT*XLF+ DT*(4.D0*PI*CAP*fv)*FK_a*(TIN-273.15D0)



    IF(HEAT_EVAP.LT.0.D0) THEN
      






 



    ENDIF
    


    FMASS_EVAP = HEAT_EVAP/XLV

    if(FMASS_EVAP > FLIQFR_S(IK) * XS_SI(IK)) then
      FMASS_EVAP = FLIQFR_S(IK) * XS_SI(IK)
    endif

    Q_EVAP= FMASS_EVAP*FF3_SI(IK)*XS_SI(IK)*3.D0*COL/rhoa_SI


    CALL EVAP_MELTWATER &
   (XS_SI(IK),rhoa_SI,Q_EVAP,FLIQFR_S(IK),FF3_SI(IK))

    XS_MELT_SI(IK)=XX_MELT

    TIN=TIN-XLV/CP*Q_EVAP
    QQV=QQV+Q_EVAP



    ee = QQV*pres_SI/(EPS + QQV)



    if(QQV < 0.D0) call wrf_error_fatal3("<stdin>",10148,&
"fatal error in module_mp_full_sbm , model stop 915")
    if(TIN < 150.D0) call wrf_error_fatal3("<stdin>",10150,&
"fatal error in module_mp_full_sbm , model stop 916")



  endif



else



  if(ISUBLIME_ADJUST == 1) then

    sub_fac = (XLS/(RV*TIN) - 1.D0)*XLS/(FK_a * TIN)



    sub_fac = sub_fac + RV* TIN/((100.D0*GGESI(TS)) * D_V)

    DSUB = -DT*4.D0*PI*CAP*fv*(ee/(100.D0*GGESI(TS)) - 1.D0)/sub_fac



    if(DSUB >  XS_SI(IK)) then
      DSUB = XS_SI(IK)
    endif

    Q_SUBL = DSUB*FF3_SI(IK)*XS_SI(IK)*3.D0*COL/rhoa_SI


    CALL SUBLIME_ICE(XS_SI(IK),rhoa_SI,Q_SUBL,FF3_SI(IK))

    XS_MELT_SI(IK)=XX_MELT

    TIN=TIN-XLS/CP*Q_SUBL
    QQV=QQV+Q_SUBL



    ee = QQV*pres_SI/(EPS + QQV)



    if(QQV < 0.) call wrf_error_fatal3("<stdin>",10195,&
"fatal error in module_mp_full_sbm , model stop 135")
    if(TIN < 150.) call wrf_error_fatal3("<stdin>",10197,&
"fatal error in module_mp_full_sbm , model stop 138")



  endif


endif

if(FLIQFR_S(IK) < 0.D0) then
        FLIQFR_S(IK) = 0.D0
endif

IF(FLIQFR_S(IK) > FLIQFRAC_LIM) then

  if(XL_SI(IK).NE.XS_SI(IK)) call wrf_error_fatal3("<stdin>",10213,&
"fatal error in module_mp_full_sbm , model stop 7012")

  if(ITEMP_ADJUST == 1) then


    Q_ICE_MELTED=FICEMASS*FF3_SI(IK)*XS_SI(IK)*3.D0*COL/rhoa_SI

    TIN=TIN-XLF/CP*Q_ICE_MELTED





  endif

  FF1_SI(IK) = FF1_SI(IK) + FF3_SI(IK)



  FLIQFR_S(IK) = 1.D0

  FF3_SI(IK) = 0.D0


ENDIF


endif

ENDDO







DO IK = 1, NKR

   IK_MELT=IK
   I_MELT=0

if(TIN > 273.15D0) then

IF(FLIQFR_G(IK).GE.1.D0.OR.FF4_SI(IK).LE.PETIT_PARAMETRE.OR. &
TIN <= 273.15D0) THEN
  IF(FLIQFR_G(IK) > 1.D0) FLIQFR_G(IK) = 1.D0
CYCLE
ENDIF

vt_start = 0.D0
vt_end = 0.D0

rhoi = RHO_G_SI(IK)
fm_i = XG_SI(IK)*(1.D0 - FLIQFR_G(IK))
V_i = fm_i/rhoi
fm_w = XG_SI(IK)*FLIQFR_G(IK)
V_w = fm_w/RHO_WATER

if(rhoi < RHO_CRIT) then
  V_soakable = V_i - fm_i/RHO_ICE
else
  V_soakable = 0.D0
endif

a_i = rad_sphere(V_i)
a_izero = rad_sphere(XG_SI(IK)/rhoi)
fnre_dry = VTG_SI(IK) * 2.D0*rhoa_SI*a_izero/etaa




X_Best = 8.D0 * XG_SI(IK) * rhoa_SI * GRAV / (PI * etaa * etaa)
fnre_smooth = fnre_sphere(X_Best)

if(V_w < V_soakable) then

  a_d = a_i
  vt=VT_LOW_DENSITY_SOAKING &
    (fnre_dry,fnre_smooth,VTG_SI(IK),a_i,a_izero,etaa,rhoa_SI)



else



  a_d = rad_sphere(V_i + (V_w - V_soakable))
  fm_w_soaked = RHO_WATER* V_soakable
  fm_w_crit = (0.268D0 + (fm_i + fm_w_soaked) * 1.D3 * 0.1389D0)
  fm_w_crit = fm_w_crit* 1.D-3
  a_crit = rad_sphere(V_i + fm_w_crit/RHO_WATER)

  if(rhoi < RHO_CRIT) then
    vt_start = VT_LOW_DENSITY_TRANS &
              (fnre_dry, fnre_smooth, &
               VTG_SI(IK),a_izero,etaa,rhoa_SI,rhoi,XG_SI(IK))
  else
    vt_start=VT_HIGH_DENSITY_TRANS &
            (fnre_dry,fnre_smooth,VTG_SI(IK),a_izero,etaa,rhoa_SI)
  endif

  vt_end=equilibrium_fallspeed &
        (fm_i+fm_w_soaked,fm_w_crit, &
         XG(:),VTL_SI(:),rhoa_SI,etaa,a_crit)

  frac_eqm=(fm_w-fm_w_soaked)/fm_w_crit

  if(frac_eqm < 0.D0) frac_eqm = 0.D0
  if(frac_eqm > 1.D0) frac_eqm = 1.D0

  vt = vt_start + (vt_end - vt_start) * frac_eqm

  if(vt < 0.D0) vt = 0.D0



endif





if(ivt_G_H_interpol.ne.0) then

  vt=VTG_SI(IK)+FLIQFR_G(IK)*(VTL_SI(IK) - VTG_SI(IK))

endif





V4_SI(IK) = vt

fnre = vt * (2.D0 * a_d * rhoa_SI)/etaa



fv = HAIL_VENTILATION_COEF(fnre,FNSC,IK)
fh = HAIL_VENTILATION_COEF(fnre,FNPR,IK)





if(FLIQFR_G(IK) <= 0.D0) then
  TS = SURFACE_TEMP(ee, TIN, XLS*D_V/(FK_a*RV), fv/fh, XLS, RV)
else
  TS = 273.15D0
endif

if(TS > 273.15D0) TS = 273.15D0



if(fnre < 6000.D0) then
  CAP = a_d
else
  CAP = a_i
endif

FICEMASS = XG_SI(IK) * (1.D0 - FLIQFR_G(IK))

DMELT = DT*(4.D0*PI*CAP/XLF) * &
(FK_a*(TIN-273.15D0)*fh+(D_V*XLV/RV)*fv*(ee/TIN - es_zero/273.15D0))



if(TS < 273.15D0 .and. FLIQFR_G(IK) <= 0.) DMELT = 0.D0




call fmass_limits(DMELT, FICEMASS, fm_w, XG_SI(IK))

if(ITEMP_ADJUST == 1) then


  call thermodynamical_limits &
 (FF4_SI(IK), XG_SI(IK), rhoa_SI, XLF/CP, TIN, DMELT)




endif

FICEMASS = FICEMASS - DMELT

FLIQFR_G(IK) = (XG_SI(IK) - FICEMASS)/XG_SI(IK)

if(FLIQFR_G(IK) < 0.D0) then
  FLIQFR_G(IK) = 0.D0
endif

if(FLIQFR_G(IK) > 0.D0) then

  if(IEVAP_ADJUST == 1) then

    if(FLIQFR_G(IK) > 1.D0) call wrf_error_fatal3("<stdin>",10413,&
"fatal error in module_mp_full_sbm , model stop 901")





    HEAT_EVAP=-DMELT*XLF+ DT*(4.D0*PI*CAP)*FK_a*(TIN-273.15D0)*fh



    IF(HEAT_EVAP.LT.0.D0) THEN
      






 



    ENDIF



    FMASS_EVAP=HEAT_EVAP/XLV

    if(FMASS_EVAP > FLIQFR_G(IK)*XG_SI(IK)) then
      FMASS_EVAP = FLIQFR_G(IK)*XG_SI(IK)
    endif

    Q_EVAP =  FMASS_EVAP * FF4_SI(IK)*XG_SI(IK)*3.D0*COL/rhoa_SI



  endif



else



  if(ISUBLIME_ADJUST == 1)then

    sub_fac = (XLS/(RV*TIN) - 1.D0)*XLS/(FK_a * TIN)



    sub_fac = sub_fac + RV* TIN/((100.D0*GGESI(TS)) * D_V)

    DSUB = -DT*4.D0*PI*CAP*fv*(ee/(100.D0*GGESI(TS)) - 1.D0)/sub_fac
    


    if(DSUB >  XG_SI(IK)) then
      DSUB = XG_SI(IK)
    endif

    Q_SUBL = DSUB*FF4_SI(IK)*XG_SI(IK)*3.D0*COL/rhoa_SI


    CALL SUBLIME_ICE( XG_SI(IK), rhoa_SI, Q_SUBL, FF4_SI(IK))

    XG_MELT_SI(IK)=XX_MELT

    TIN = TIN - XLS/CP*Q_SUBL
    QQV = QQV + Q_SUBL



    ee = QQV*pres_SI/(EPS + QQV)



    if(QQV < 0.D0)  call wrf_error_fatal3("<stdin>",10490,&
"fatal error in module_mp_full_sbm , model stop 135")
    if(TIN < 150.D0)   call wrf_error_fatal3("<stdin>",10492,&
"fatal error in module_mp_full_sbm , model stop 138")




  endif



endif

IF(FLIQFR_G(IK) > FLIQFRAC_LIM) then

  if(XL_SI(IK).NE.XG_SI(IK)) call wrf_error_fatal3("<stdin>",10506,&
"fatal error in module_mp_full_sbm , model stop 7013")

  if(ITEMP_ADJUST == 1) then


    Q_ICE_MELTED = FICEMASS *FF4_SI(IK)*XG_SI(IK)*3.D0*COL/rhoa_SI

    TIN = TIN - XLF/CP*Q_ICE_MELTED




  endif

  FF1_SI(IK) = FF1_SI(IK) + FF4_SI(IK)



  FLIQFR_G(IK) = 1.D0

  FF4_SI(IK) = 0.D0



ENDIF



endif

ENDDO







DO IK = 1, NKR

   IK_MELT=IK
   I_MELT=0

if(TIN > 273.15D0) then

IF(FLIQFR_H(IK).GE.1.D0.OR.FF5_SI(IK).LE.PETIT_PARAMETRE.OR. &
TIN <= 273.15D0) THEN
  IF(FLIQFR_H(IK) > 1.D0) FLIQFR_H(IK) = 1.D0
CYCLE
ENDIF

vt_start = 0.D0
vt_end = 0.D0

rhoi  = RHO_H_SI(IK)
fm_i = XH_SI(IK)*(1.D0 - FLIQFR_H(IK))
V_i = fm_i/rhoi
fm_w = XH_SI(IK)*FLIQFR_H(IK)
V_w = fm_w/RHO_WATER

if(rhoi < RHO_CRIT) then
  V_soakable = V_i - fm_i/RHO_ICE
else
  V_soakable = 0.D0
endif

a_i = rad_sphere(V_i)
a_izero = rad_sphere(XH_SI(IK)/rhoi)




if(IPRINTING == 1) print *, 'fnre_dry = ', fnre_dry

fnre_dry=VTH_SI(IK)*2.D0*rhoa_SI*a_izero/etaa
X_Best=8.D0*XH_SI(IK)*rhoa_SI*GRAV/(PI * etaa * etaa)
fnre_smooth=fnre_sphere(X_Best)

vt_justwet = 0.D0
vt_justsoaked = 0.D0

if(V_w < V_soakable) then



  a_d = a_i
  vt=VT_LOW_DENSITY_SOAKING &
    (fnre_dry,fnre_smooth,VTH_SI(IK),a_i,a_izero,etaa,rhoa_SI)



else



  a_d = rad_sphere(V_i + (V_w - V_soakable))
  fm_w_soaked = RHO_WATER* V_soakable
  fm_w_crit=(0.268D0+(fm_i+fm_w_soaked)*1.D3*0.1389D0)
  fm_w_crit = fm_w_crit* 1.D-3
  a_crit = rad_sphere(V_i + fm_w_crit/RHO_WATER)






  if(rhoi < RHO_CRIT) then

    vt_start = VT_LOW_DENSITY_TRANS &
              (fnre_dry,fnre_smooth, &
               VTH_SI(IK),a_izero,etaa,rhoa_SI,rhoi,XH_SI(IK))
  else

    vt_start = VT_HIGH_DENSITY_TRANS(fnre_dry, fnre_smooth, &
    VTH_SI(IK), a_izero, etaa, rhoa_SI)

  endif

    vt_end=equilibrium_fallspeed &
          (fm_i + fm_w_soaked, fm_w_crit, XH(:), &
           VTL_SI(:), rhoa_SI, etaa, a_crit)




    frac_eqm = (fm_w - fm_w_soaked)/fm_w_crit
    if(frac_eqm < 0.D0) frac_eqm = 0.D0
    if(frac_eqm > 1.D0) frac_eqm = 1.D0

    vt = vt_start + (vt_end - vt_start) * frac_eqm

    if(vt < 0.D0) then
      if(IPRINTING == 1) print *, 'WARNING: vt < 0', vt
      vt = 0.D0
    endif

    if(IPRINTING == 1) print *, &
   'HERE 2:: vt_start,vt_end,a_izero/a_i= ', &
             vt_start,vt_end,a_izero/a_i

    if(IPRINTING == 1) print *, &
   'HERE 2:: fnre_dry,fnre_smooth,vt_justsoaked,vt_justwet', &
             fnre_dry,fnre_smooth,vt_justsoaked,vt_justwet



endif





if(ivt_G_H_interpol.ne.0) then

  vt=VTH_SI(IK)+FLIQFR_H(IK)*(VTL_SI(IK) - VTH_SI(IK))
  
endif


  


V5_SI(IK) = vt

if(IPRINTING == 1) print *, 'HERE 2: VT,LIQUID FRACTION,IK', &
                                     V5_SI(IK),FLIQFR_H(IK),IK
fnre = vt * (2.D0 * a_d * rhoa_SI)/etaa



fv = HAIL_VENTILATION_COEF(fnre,FNSC,IK)
fh = HAIL_VENTILATION_COEF(fnre,FNPR,IK)





if(FLIQFR_H(IK) <= 0.D0) then
  TS = SURFACE_TEMP(ee, TIN, XLS*D_V/(FK_a*RV), fv/fh, XLS, RV)
else
  TS = 273.15D0
endif

if(TS > 273.15D0) TS = 273.15D0




if(fnre < 6000.D0) then
  CAP = a_d
else
  CAP = a_i
endif


FICEMASS = XH_SI(IK) * (1.D0 - FLIQFR_H(IK))

DMELT = DT*4.D0*PI*CAP/XLF* &
(FK_a*(TIN-273.15D0)*fh+D_V*XLV/RV*fv*(ee/TIN-es_zero/273.15D0))



if(TS < 273.15D0 .and. FLIQFR_H(IK) <= 0.) DMELT = 0.D0





call fmass_limits (DMELT,FICEMASS,fm_w,XH_SI(IK))


if(ITEMP_ADJUST == 1) then


  call thermodynamical_limits &
 (FF5_SI(IK),XH_SI(IK),rhoa_SI,XLF/CP,TIN,DMELT)




endif

FICEMASS = FICEMASS - DMELT

FLIQFR_H(IK) = (XH_SI(IK) - FICEMASS)/XH_SI(IK)



if(FLIQFR_H(IK) < 0.D0) then
  FLIQFR_H(IK) = 0.D0
endif

if(FLIQFR_H(IK) > 0.D0) then

  if(IEVAP_ADJUST == 1) then

    if( FLIQFR_H(IK) > 1.D0) call wrf_error_fatal3("<stdin>",10743,&
"fatal error in module_mp_full_sbm , model stop 906")






    HEAT_EVAP=-DMELT*XLF+DT*(4.D0*PI*CAP)*FK_a*(TIN-273.15D0)*fh



    IF(HEAT_EVAP.LT.0.D0) THEN
      






 



    ENDIF



    FMASS_EVAP = HEAT_EVAP/XLV

    if(FMASS_EVAP > FLIQFR_H(IK) * XH_SI(IK)) then
      FMASS_EVAP = FLIQFR_H(IK) * XH_SI(IK)
    endif

    Q_EVAP=FMASS_EVAP*FF5_SI(IK)*XH_SI(IK)*3.D0*COL/rhoa_SI


    CALL EVAP_MELTWATER &
   (XH_SI(IK),rhoa_SI,Q_EVAP,FLIQFR_H(IK),FF5_SI(IK))

    XH_MELT_SI(IK)=XX_MELT

    TIN = TIN - XLV/CP*Q_EVAP
    QQV = QQV + Q_EVAP



    ee = QQV*pres_SI/(EPS + QQV)



    if(QQV < 0.D0) call wrf_error_fatal3("<stdin>",10794,&
"fatal error in module_mp_full_sbm , model stop 135")
    if(TIN < 150.D0) call wrf_error_fatal3("<stdin>",10796,&
"fatal error in module_mp_full_sbm , model stop 138")




  endif



else



  if(ISUBLIME_ADJUST == 1) then

    sub_fac = (XLS/(RV*TIN) - 1.D0)*XLS/(FK_a * TIN)



    sub_fac = sub_fac + RV* TIN/((100.D0*GGESI(TS)) * D_V)

    DSUB = -DT*4.D0*PI*CAP*fv*(ee/(100.D0*GGESI(TS)) - 1.D0)/sub_fac



    if(DSUB > XH_SI(IK)) then
      DSUB = XH_SI(IK)
    endif

    Q_SUBL = DSUB*FF5_SI(IK)*XH_SI(IK)*3.D0*COL/rhoa_SI


    CALL SUBLIME_ICE(XH_SI(IK),rhoa_SI,Q_SUBL,FF5_SI(IK))

    XH_MELT_SI(IK)=XX_MELT

    TIN = TIN - XLS/CP*Q_SUBL
    QQV = QQV + Q_SUBL



    ee = QQV*pres_SI/(EPS + QQV)



    if(QQV < 0.D0) call wrf_error_fatal3("<stdin>",10842,&
"fatal error in module_mp_full_sbm , model stop 135")
    if(TIN < 150.D0) call wrf_error_fatal3("<stdin>",10844,&
"fatal error in module_mp_full_sbm , model stop 138")



  endif



endif

if(FLIQFR_H(IK) < 0.D0) then
  FLIQFR_H(IK) = 0.D0
endif

IF(FLIQFR_H(IK) > FLIQFRAC_LIM) then

  if(XL_SI(IK).NE.XH_SI(IK)) call wrf_error_fatal3("<stdin>",10861,&
"fatal error in module_mp_full_sbm , model stop 7014")

  if(ITEMP_ADJUST == 1) then


    Q_ICE_MELTED = FICEMASS *FF5_SI(IK)*XH_SI(IK)*3.D0*COL/rhoa_SI

    TIN = TIN - XLF/CP*Q_ICE_MELTED
    



  endif

  FF1_SI(IK) = FF1_SI(IK) + FF5_SI(IK)



  FLIQFR_H(IK) = 1.D0

  FF5_SI(IK) = 0.D0



ENDIF



endif

ENDDO






if(ihucm_flag == 1) then

  if(IVT_ADJUST == 1) then
    V2 = 100.D0 * V2_SI
    V3 = 100.D0 * V3_SI
    V4 = 100.D0 * V4_SI
    V5 = 100.D0 * V5_SI
  endif

  FF1 = 1.D-9*FF1_SI
  FF2 = 1.D-9*FF2_SI
  FF3 = 1.D-9*FF3_SI
  FF4 = 1.D-9*FF4_SI
  FF5 = 1.D-9*FF5_SI

  XI_MELT = XI_MELT_SI*1000.D0
  XS_MELT = XS_MELT_SI*1000.D0
  XG_MELT = XG_MELT_SI*1000.D0
  XH_MELT = XH_MELT_SI*1000.D0



else



  if(IVT_ADJUST == 1) then
    V2 = V2_SI
    V3 = V3_SI
    V4 = V4_SI
    V5 = V5_SI
  endif
  
  FF1 = FF1_SI
  FF2 = FF2_SI
  FF3 = FF3_SI
  FF4 = FF4_SI
  FF5 = FF5_SI

  XI_MELT = XI_MELT_SI
  XS_MELT = XS_MELT_SI
  XG_MELT = XG_MELT_SI
  XH_MELT = XH_MELT_SI



endif

 101 FORMAT(1X,D13.5)
 102 FORMAT(1X,2D13.5)
 103 FORMAT(1X,3D13.5)
 104 FORMAT(1X,4D13.5)
 105 FORMAT(1X,5D13.5)
 106 FORMAT(1X,6D13.5)
 107 FORMAT(1X,7D13.5)
 201 FORMAT(1X,I2,D13.5)
 202 FORMAT(1X,I2,2D13.5)
 203 FORMAT(1X,I2,3D13.5)
 204 FORMAT(1X,I2,4D13.5)

END SUBROUTINE MELTING


SUBROUTINE EVAP_MELTWATER(XX,rhoax,Q_EVAPX,FLIQFRX,FFX)

implicit double precision (a-h,o-z)

PARAMETER(COL=0.23105D0)











total_mass= XX*FFX*XX*3.D0*COL/rhoax
total_mass_ice=(1.D0-FLIQFRX)*total_mass
total_mass_liq=FLIQFRX*total_mass


if(Q_EVAPX > total_mass_liq) Q_EVAPX = total_mass_liq
if(Q_EVAPX > total_mass) Q_EVAPX = total_mass

total_mass_liq = total_mass_liq - Q_EVAPX
total_mass = total_mass - Q_EVAPX

XX_MELT=total_mass*rhoax/(3.D0*XX*FFX*COL)

FFX = total_mass/(XX*XX*3.D0*COL/rhoax)

if(FFX < 0.D0) call wrf_error_fatal3("<stdin>",10993,&
"fatal error in module_mp_full_sbm , model stop 136")

if(total_mass_liq < 0.D0) call wrf_error_fatal3("<stdin>",10996,&
"fatal error in module_mp_full_sbm , model stop 137")
if(total_mass_ice < 0.D0) call wrf_error_fatal3("<stdin>",10998,&
"fatal error in module_mp_full_sbm , model stop 140")
if(total_mass < 0.D0) call wrf_error_fatal3("<stdin>",11000,&
"fatal error in module_mp_full_sbm , model stop 141")

IF(total_mass.EQ.0.D0) THEN
  FLIQFRX =1.D0
ELSE
  FLIQFRX = (total_mass - total_mass_ice)/total_mass
ENDIF

if(FLIQFRX < 0.D0) FLIQFRX = 0.D0
if(FLIQFRX > 1.D0) FLIQFRX = 1.D0

101 FORMAT(1X,D13.5)
102 FORMAT(1X,2D13.5)
103 FORMAT(1X,3D13.5)
104 FORMAT(1X,4D13.5)
105 FORMAT(1X,5D13.5)
106 FORMAT(1X,6D13.5)

END SUBROUTINE evap_meltwater



SUBROUTINE SUBLIME_ICE (XX,rhoax,Q_SUBLX,FFX)

implicit double precision (a-h,o-z)

PARAMETER(COL = 0.23105D0)






total_mass =  XX*FFX*XX*3.D0*COL/rhoax

if(Q_SUBLX > total_mass) Q_SUBLX = total_mass

total_mass = total_mass - Q_SUBLX



XX_MELT=total_mass*rhoax/(3.D0*FFX*XX*COL)



FFX = total_mass/(XX*XX*3.D0*COL/rhoax)

if(FFX < 0.D0) call wrf_error_fatal3("<stdin>",11048,&
"fatal error in module_mp_full_sbm , model stop 140")

if(total_mass < 0.D0) call wrf_error_fatal3("<stdin>",11051,&
"fatal error in module_mp_full_sbm , model stop 141")

END SUBROUTINE sublime_ice



FUNCTION VT_LOW_DENSITY_SOAKING &
(fnre_dryx,fnre_smoothx,vt_dryx,a_ix,a_izerox,etaax,rhoax)

implicit double precision (a-h,o-z)







if(fnre_dryx > 4000.D0) then

  vtx = vt_dryx * a_izerox/a_ix




else



  vtx = fnre_smoothx * etaax/(2.D0 * a_ix * rhoax)



endif

VT_LOW_DENSITY_SOAKING = vtx

RETURN
END FUNCTION VT_LOW_DENSITY_SOAKING 



FUNCTION VT_LOW_DENSITY_TRANS &
(fnre_dryx,fnre_smoothx,vt_dryx,a_izerox,etaax,rhoax,rhoix,fm_tot)

implicit double precision (a-h,o-z)

PARAMETER(RHO_WATER=1000.D0, RHO_ICE=920.D0, PI = 3.141592654D0)

fm_ijustsoaked=fm_tot/(1.D0+RHO_WATER/rhoix-RHO_WATER/RHO_ICE)
a_ijustsoaked=rad_sphere(fm_ijustsoaked/rhoix)

if(fnre_dryx <=  4000.D0.or.rhoix < 800.D0) then
  vt_justsoaked=fnre_smoothx*etaax/(2.D0*a_ijustsoaked*rhoax)
else
  vt_justsoaked=vt_dryx*a_izerox/a_ijustsoaked
endif

vtx = vt_justsoaked

VT_LOW_DENSITY_TRANS = vtx

RETURN
END FUNCTION VT_LOW_DENSITY_TRANS



FUNCTION VT_HIGH_DENSITY_TRANS &
(fnre_dryx,fnre_smoothx,vt_dryx,a_izerox,etaax,rhoax)

implicit double precision (a-h,o-z)

PARAMETER(RHO_WATER=1000.D0, RHO_ICE=920.D0, PI = 3.141592654D0)



if(fnre_dryx > 4000.D0) then
  vt_justwet=vt_dryx
else
  vt_justwet=fnre_smoothx*etaax/(2.D0*a_izerox*rhoax)
endif

vtx = vt_justwet

VT_HIGH_DENSITY_TRANS = vtx

RETURN
END FUNCTION VT_HIGH_DENSITY_TRANS





FUNCTION HAIL_VENTILATION_COEF (fnrex, fnumber, KR)



implicit double precision (a-h,o-z)






if(fnrex < 6000.D0) then

  X_F = (fnrex**0.5D0)*(fnumber**(1.D0/3.D0))

  IF(X_F < 1.4D0) then
    fx = 1.D0 + 0.108D0*X_F*X_F
  ELSE
    fx = 0.78D0 + 0.308D0*X_F
  ENDIF

  if(fnrex < 250.D0) then
    fx = fx*2.D0
  endif



else



  if(fnrex < 20000.D0) then
    chi_fr = 0.76D0
  else
    chi_fr = 0.57 + fnrex*9.D-6
  endif

  fx = chi_fr*(fnrex**0.5D0)*(fnumber**(1.D0/3.D0))/2.D0



endif

if(fx < 1.D0) then
  fx = 1.D0
endif




if(fx > 100.D0) then






  fx=100.D0
endif



HAIL_VENTILATION_COEF = fx

  201   FORMAT(E13.5)
  202   FORMAT(2E13.5)
  203   FORMAT(3E13.5)
  204   FORMAT(4E13.5)
  205   FORMAT(5E13.5)
  206   FORMAT(6E13.5)
  207   FORMAT(7E13.5)

return
end function HAIL_VENTILATION_COEF





      FUNCTION GGESI(T)

implicit double precision (a-h,o-z)

intrinsic DLOG10







      DATA C1_MELT/-9.09718D0/C2_MELT/-3.56654D0/C3_MELT/0.876793D0/C4_MELT/0.78583503D0/

      A = 273.16D0/T
      B = C1_MELT*(A-1.0D0)+C2_MELT*DLOG10(A)+C3_MELT*(1.0D0-1.0D0/A)+C4_MELT
      GGESI = 10.0D0**B

      RETURN
      END FUNCTION GGESI







FUNCTION SNOW_VENTILATION_COEF(fnrex,fnumber, ARx)



implicit double precision (a-h,o-z)

X_F = (fnrex**0.5D0) * (fnumber**(1.D0/3.D0))



if(ARx == 1.D0) then



  IF(X_F < 1.4D0) then
    fx = 1.D0 + 0.108D0*X_F*X_F
  ELSE
    fx = 0.78D0 + 0.308D0*X_F
  ENDIF

else





if(X_F.le.1.D0) then
  fx=1.D0 + 0.14D0*X_F*X_F
else
  fx = 0.86D0 + 0.28D0*X_F
endif

endif

if(fx < 1.D0) then
  fx = 1.D0
endif

if(fx > 100.D0) then

print *,'99992 stop:',fx,X_F,fnrex,fnumber, ARx
fx = 100.D0

endif

SNOW_VENTILATION_COEF = fx

return
end function SNOW_VENTILATION_COEF







FUNCTION SURFACE_TEMP(eex, tempK, factor_vap, fvofh, XLS, RV)


 
implicit double precision (a-h,o-z)

intrinsic  DEXP, DABS

tsxold = 269.D0

tsx = 270.D0

tdiff = 1.D0

ilj = 0

alpha_ts = factor_vap*fvofh

beta_ts = alpha_ts*eex/tempK

do while(tdiff > 1.D-6)



   esix = 100.D0*GGESI(tsx)



  f_tsx = tempK - tsx - alpha_ts*esix/tsx + beta_ts

  f_tsxold= &
  tempK-tsxold-alpha_ts*100.D0*GGESI(tsxold)/tsxold+beta_ts

  tsxnew = tsx - f_tsx*(tsx - tsxold)/(f_tsx - f_tsxold)

  tsxold = tsx
  tsx = tsxnew

  tdiff = DABS(tsx - tsxold)

  ilj = ilj + 1

  if(ilj > 1e6) then
    print *, &
   'SURFACE_TEMP not converging', tsx,tempK,tdiff,fvofh,eex,esix
    tsx = tempK
    exit
  endif

enddo

SURFACE_TEMP = tsx

return
END FUNCTION SURFACE_TEMP





FUNCTION COLUMN_VENTILATION_COEF(fnrex, fnumber)

implicit double precision (a-h,o-z)

if(fnrex < 50.D0) then
  X_F = (fnrex**0.5D0) * (fnumber**(1.D0/3.D0))
else
  X_F = (50.D0**0.5D0) * (fnumber**(1.D0/3.D0))
endif

fx=1.D0-0.00668D0*X_F/4.D0+2.39402D0*((X_F/4.D0)**2.D0)+ &
   0.73409D0*((X_F/4.D0)**3.D0)-0.73911D0*((X_F/4.D0)**4.D0)

if(fx < 1.D0) then
  fx = 1.D0
endif

if(fx > 100.D0) call wrf_error_fatal3("<stdin>",11383,&
"fatal error in module_mp_full_sbm , model stop 99993")

COLUMN_VENTILATION_COEF = fx

return
end function COLUMN_VENTILATION_COEF



FUNCTION PLATE_VENTILATION_COEF(fnrex, fnumber)

implicit double precision (a-h,o-z)

if(fnrex < 150.D0) then
  X_F = fnrex**0.5D0 * fnumber**(1.D0/3.D0)
else
  X_F = 150.D0**0.5D0 * fnumber**(1.D0/3.D0)
endif

fx=1.D0-0.06042D0*X_F/10.D0+2.79820D0*((X_F/10.D0)**2.D0) - &
   0.31933D0*((X_F/10.D0)**3.D0)-0.06247D0*((X_F/10.D0)**4.D0)

if(fx < 1.D0) then
  fx = 1.D0
endif

if(fx > 100.D0) call wrf_error_fatal3("<stdin>",11410,&
"fatal error in module_mp_full_sbm , model stop 99994")

PLATE_VENTILATION_COEF = fx

return
end function PLATE_VENTILATION_COEF



FUNCTION DENDRITE_VENTILATION_COEF(fnrex, fnumber)

implicit double precision (a-h,o-z)

if(fnrex < 150.D0) then
  X_F = (fnrex**0.5D0) * (fnumber**(1.D0/3.D0))
else
  X_F = (150.D0**0.5D0) * (fnumber**(1.D0/3.D0))
endif

fx=1.D0+0.35463D0*X_F/10.D0+3.55338D0*((X_F/10.D0)**2.D0)

if(fx < 1.D0) then
  fx = 1.D0
endif

if(fx > 100.D0) call wrf_error_fatal3("<stdin>",11436,&
"fatal error in module_mp_full_sbm , model stop 99995")

DENDRITE_VENTILATION_COEF = fx

return
end function DENDRITE_VENTILATION_COEF



FUNCTION chi_fra(fra)

implicit double precision (a-h,o-z)

DIMENSION xxa(14), yya(14)

pc = 100.D0 * fra

if(pc.le.0.D0) then
  chi_fra = 0.D0
  return
endif

if(pc.ge.100.D0) then
  chi_fra = 1.D0
  return
endif

xxa(1) = 0.D0
yya(1) = 0.D0
xxa(2) = 10.D0
yya(2) = 1.25D0
xxa(3) = 20.D0
yya(3) = 3.12D0
xxa(4) = 30.D0
yya(4) = 5.D0
xxa(5) = 40.D0
yya(5) = 8.12D0
xxa(6) = 50.D0
yya(6) = 11.87D0
xxa(7) = 60.D0
yya(7) = 17.49D0
xxa(8) = 70.D0
yya(8) = 24.36D0
xxa(9) = 75.D0
yya(9) = 28.73D0
xxa(10) = 80.D0
yya(10) = 34.98D0
xxa(11) = 85.D0
yya(11) = 43.72D0
xxa(12) = 90.D0
yya(12) = 56.84D0
xxa(13) = 95.D0
yya(13) = 73.08D0
xxa(14) = 100.D0
yya(14) = 100.D0

ix_max = 14

ix = 0

pc_hi = 0.D0

DO WHILE(pc_hi < pc)

ix = ix + 1

if(ix > ix_max) then
ix = ix - 1
exit
endif

pc_hi = xxa(ix)

ENDDO



if(ix -1 < 1) call wrf_error_fatal3("<stdin>",11514,&
"fatal error in module_mp_full_sbm , model stop 42567")
if(ix  > ix_max) call wrf_error_fatal3("<stdin>",11516,&
"fatal error in module_mp_full_sbm , model stop 42568")



chi_fra=yya(ix-1)+ &
(pc-xxa(ix-1))*(yya(ix)-yya(ix-1))/(xxa(ix)-xxa(ix-1))

chi_fra = chi_fra/100.D0

if(chi_fra  < 0.D0) chi_fra  = 0.D0
if(chi_fra  > 1.D0) chi_fra  = 1.D0



if(chi_fra > 0.3D0 .and. pc < 75.D0) call wrf_error_fatal3("<stdin>",11531,&
"fatal error in module_mp_full_sbm , model stop 1478")
if(chi_fra > 0.6D0 .and. pc < 90.D0) call wrf_error_fatal3("<stdin>",11533,&
"fatal error in module_mp_full_sbm , model stop 1477")



RETURN
END FUNCTION chi_fra



function fnre_sphere(xd)

implicit double precision (a-h,o-z)

INTRINSIC DLOG10

ww1 = dlog10(xd)

ww2 = ww1 * ww1
ww3 = ww1 * ww1* ww1

fnre_sphere = 0.d0

if(xd < 73.D0) then
  fnre_sphere = xd/24.D0
endif

if(xd < 562.D0.and.xd >= 73.D0) then
  fnre_sphere = - 1.7095D0 + 1.33438D0*ww1 - 0.11591D0*ww2
  fnre_sphere = 10.D0**fnre_sphere
endif

if(xd < 1.83D3.and.xd >= 562.D0) then
  fnre_sphere= &
  -1.81391D0 + 1.34671D0*ww1 - 0.12427D0*ww2 + 0.0063D0*ww3
  fnre_sphere = 10.D0**fnre_sphere
endif

if(xd < 5.4D10.and.xd >= 1.83D3) then
  fnre_sphere= &
  0.003567D0*ww3 - 0.089620D0*ww2 + 1.225713D0*ww1 - 1.706026D0
  fnre_sphere = 10.D0**fnre_sphere
endif

if(xd >= 5.4D10) then
  fnre_sphere = (xd/0.1D0)**0.5D0
endif

end function fnre_sphere



function equilibrium_fallspeed (fm_s, fm_w_critx, XXL, vt_rain, &
                                rhoax, etaax, a_eqm)
implicit double precision (a-h,o-z)


PARAMETER(PI = 3.141592654D0, GRAV = 9.8D0)

DIMENSION XXL(NKR), vt_rain(NKR)

fnre_shed = 4800.D0 + 4831.5D0*1000.D0*fm_s

if(fnre_shed >= 5000.D0.and.fnre_shed <= 2.5D4) then





  vt_eqm = 1.5D-5* fnre_shed/(2.D0*a_eqm)
  


  vt_eqm = vt_eqm* ((1.20D0/rhoax)**0.5D0)

  if(vt_eqm > 100.D0) then
  
  
  
  call wrf_error_fatal3("<stdin>",11612,&
"fatal error in module_mp_full_sbm , model stop 9999")
  endif



else



  if(fnre_shed > 2.5D4) then

    X_Best_crit=8.D0*(fm_s+fm_w_critx)*rhoax*GRAV/(PI*etaax*etaax)
    fnre_fast=(X_Best_crit/0.6D0)**0.5D0
    vt_eqm=fnre_fast*etaax/(2.D0*a_eqm*rhoax)



  else



    ILIQ = IFIND_IK (fm_s + fm_w_critx, XXL, finter_frac)

    if(ILIQ < NKR ) then
      vt_eqm = &
      vt_rain(ILIQ)+finter_frac*(vt_rain(ILIQ+1)-vt_rain(ILIQ))
    else
      vt_eqm = vt_rain(NKR)
    endif


  endif


endif

equilibrium_fallspeed = vt_eqm

end function equilibrium_fallspeed



FUNCTION IFIND_IK (fmass_target, fmass_array, fraction)

implicit double precision (a-h,o-z)



DIMENSION fmass_array(NKR)

IKX = 2

DO WHILE(fmass_array(IKX) < fmass_target)
   if(IKX > NKR - 1) exit
   IKX = IKX + 1
ENDDO

IKX = IKX - 1

fraction= &
(fmass_target-fmass_array(IKX))/(fmass_array(IKX+1)-fmass_array(IKX))
if(fraction < 0.D0) fraction = 0.D0
if(fraction > 1.D0) fraction = 1.D0

if(IKX > NKR.or.IKX < 1) call wrf_error_fatal3("<stdin>",11677,&
"fatal error in module_mp_full_sbm , model stop 99999")

IFIND_IK = IKX

END FUNCTION IFIND_IK



FUNCTION COLUMN_AR (fmassx, rhoix)

implicit double precision (a-h,o-z)

parameter (PI = 3.141592654D0)



d_equiv = (fmassx/rhoix)/(4.D0*PI/3.D0)
d_equiv = d_equiv**(1.D0/3.D0)
d_equiv = 2.D0*d_equiv*1000.D0



if(d_equiv < 0.3D0) then
  shape = 2.D0
else
  shape = d_equiv/(0.1973D0*(d_equiv**0.414D0))
endif



FL_i = 4.D0*shape*shape*(fmassx/rhoix)/PI
FL_i = FL_i**(1.D0/3.D0)
FL_i = FL_i* 1000.D0

if(FL_i < 0.3D0) then
  COLUMN_AR = 2.D0
else
  COLUMN_AR = FL_i/(0.1973D0*(FL_i**0.414D0))
endif

if(COLUMN_AR > 5.D0) COLUMN_AR = 5.D0

return
end function COLUMN_AR



FUNCTION PLATE_AR (fmassx)

implicit double precision (a-h,o-z)

d_i = (fmassx/1.d-3)/0.03760d0
d_i = d_i**(1.d0/3.31d0)
d_i = d_i/100.d0
h_i = 0.0141d0*( (d_i*100.d0)**0.474d0)
h_i = h_i/100.d0

PLATE_AR = h_i/d_i

return
end function  PLATE_AR



FUNCTION DENDRITE_AR(fmassx)

implicit double precision (a-h,o-z)

d_i = (fmassx/1.d-3)/0.00376D0
d_i = d_i**(1.D0/2.79D0)
d_i = d_i/100.D0
h_i = 0.00996D0*((d_i*100.D0)** 0.415D0)
h_i = h_i/100.D0

DENDRITE_AR = h_i/d_i

return
end function DENDRITE_AR



FUNCTION COLUMN_CAP_ZERO (fm_ice, AR_ice, rho_ice, FLstar)

implicit double precision (a-h,o-z)

PARAMETER(PI = 3.141592654D0)

INTRINSIC DLOG

a_ix = (fm_ice/rho_ice)/(4.D0*PI*AR_ice/3.D0)
a_ix = a_ix**(1.D0/3.D0)
b_i = AR_ice*a_ix

if(AR_ice < 0.D0.or.AR_ice < 1.D0) then
call wrf_error_fatal3("<stdin>",11772,&
"fatal error in module_mp_full_sbm , model stop 9011")
endif

epsil_i = b_i*b_i - a_ix*a_ix

if(epsil_i.le.0.D0) then

call wrf_error_fatal3("<stdin>",11780,&
"fatal error in module_mp_full_sbm , model stop 9044")
endif

epsil_i = epsil_i**0.5D0

COLUMN_CAP_ZERO= (b_i+epsil_i)/a_ix
COLUMN_CAP_ZERO = epsil_i /(DLOG(COLUMN_CAP_ZERO))

omega_i = 2.D0*(PI*a_ix*a_ix) + 4.D0*b_i*a_ix

P_i = 2.D0*PI*a_ix

FLstar = omega_i/P_i

return
end function COLUMN_CAP_ZERO



FUNCTION PLANAR_CAP_ZERO (fm_ice, AR_ice, rho_ice, FLstar)

implicit double precision (a-h,o-z)

PARAMETER(PI = 3.141592654D0)




INTRINSIC DLOG, DASIN



a_ix = (fm_ice/rho_ice)/(4.D0*PI*AR_ice/3.D0)
a_ix = a_ix**(1.D0/3.D0)

if(AR_ice < 0.D0.or.AR_ice > 1.D0) then
 call wrf_error_fatal3("<stdin>",11817,&
"fatal error in module_mp_full_sbm , model stop 9022")
endif

epsil_i = 1.D0 - AR_ice*AR_ice

if( epsil_i < 0.D0) call wrf_error_fatal3("<stdin>",11823,&
"fatal error in module_mp_full_sbm , model stop 9086")

epsil_i = epsil_i**0.5D0

if(epsil_i > 0.D0) then




  PLANAR_CAP_ZERO = a_ix*epsil_i/DASIN(epsil_i)



  if((1.D0+epsil_i)/(1.D0-epsil_i).le.0.D0) call wrf_error_fatal3("<stdin>",11837,&
"fatal error in module_mp_full_sbm , model stop 9054")

  omega_i = DLOG((1.D0+ epsil_i)/( 1.D0- epsil_i))
  omega_i = 2.D0 + PI*AR_ice*(1.D0/epsil_i)*omega_i
  omega_i = PI*a_ix*a_ix*omega_i

  P_i = 2.D0*PI*a_ix
  FLstar = omega_i/P_i

else

  PLANAR_CAP_ZERO = a_ix
  FLstar = 2.D0*a_ix

endif

return
end function PLANAR_CAP_ZERO



FUNCTION rad_sphere (volume)

implicit double precision (a-h,o-z)

PARAMETER(PI = 3.141592654D0)

rad_sphere = volume/(4.D0*PI/3.D0)
rad_sphere = rad_sphere**(1.D0/3.D0)

return
end FUNCTION rad_sphere



SUBROUTINE thermodynamical_limits &
(FFX, fm_tot, rhoax, XLFOCP, tempx, dmeltx)

implicit double precision (a-h,o-z)

PARAMETER (COL=0.23105D0)















fnumber_MR = 3.D0*FFX*fm_tot*COL/rhoax

Q_ICE_MELTED = dmeltx*fnumber_MR

temp_star = tempx - XLFOCP*Q_ICE_MELTED

if(temp_star < 273.15D0) then

  Q_ICE_MELTED = (tempx - 273.15D0)/XLFOCP
  dmeltx = Q_ICE_MELTED / fnumber_MR
  tempx = 273.15D0



else



  tempx = temp_star


endif

101 FORMAT(1X,D13.5)
102 FORMAT(1X,2D13.5)
103 FORMAT(1X,3D13.5)
104 FORMAT(1X,4D13.5)
105 FORMAT(1X,5D13.5)
106 FORMAT(1X,6D13.5)

END SUBROUTINE thermodynamical_limits



SUBROUTINE fmass_limits (dmeltx, ficemassx, fm_water, fm_tot)

implicit double precision (a-h,o-z)

INTRINSIC DABS






if(dmeltx > ficemassx) then
  dmeltx = ficemassx
endif

if(dmeltx < 0.D0.and.DABS(dmeltx) > fm_water) then
        dmeltx = - fm_water
endif

if(ficemassx - dmeltx > fm_tot) then
  dmeltx = ficemassx - fm_tot
endif


101 FORMAT(1X,D13.5)
102 FORMAT(1X,2D13.5)
103 FORMAT(1X,3D13.5)
104 FORMAT(1X,4D13.5)
105 FORMAT(1X,5D13.5)
106 FORMAT(1X,6D13.5)

end subroutine fmass_limits






	SUBROUTINE JERDFUN_MELT &

     & (R2,R2N&
     & ,FI2,PSI2&
     & ,FL2_OLD,FL2_NEW&
     & ,IND,ITYPE)

       implicit none     

       REAL DEL2N

      INTEGER IND,ITYPE,KR,ICE,ITYP,NRM,NR,IDROP
      INTEGER NRX,I_3POINT,ICE_TYPE
     





 REAL &
     &  R2(NKR,IND),R2N(NKR,IND) &
     & ,FI2(NKR,IND),PSI2(NKR,IND) &
     & ,FL2_OLD(NKR,IND),FL2_NEW(NKR,IND)




        DOUBLE PRECISION R2R(NKR),R2NR(NKR),FI2R(NKR),PSI2R(NKR)
        DOUBLE PRECISION DR2(NKR,IND),DR2N(NKR,IND)

	DOUBLE PRECISION FL2R_OLD(NKR),FL2R_NEW(NKR)

        NRX=NKR

	IF(IND.NE.1) THEN
	  ITYP=ITYPE
        ELSE
	  ITYP=1
	ENDIF



	DO ICE_TYPE=1,IND

	   IF(ITYP.EQ.ICE_TYPE) THEN

             DO KR=1,NKR

	        R2R(KR)=R2(KR,ICE_TYPE)
	        R2NR(KR)=R2N(KR,ICE_TYPE)               
                FI2R(KR)=FI2(KR,ICE_TYPE)
                PSI2R(KR)=FI2R(KR)
                FL2R_OLD(KR)=FL2_OLD(KR,ICE_TYPE)
                FL2R_NEW(KR)=FL2R_OLD(KR)
                
             ENDDO





             I_3POINT=0

             CALL JERNEWF_MELT(NRX,R2R,R2NR,FI2R,PSI2R,FL2R_OLD,FL2R_NEW,I_3POINT)

	     DO KR=1,NKR                              
	        PSI2(KR,ICE_TYPE)=PSI2R(KR)
                FL2_NEW(KR,ICE_TYPE)=FL2R_NEW(KR)
	     ENDDO




	   ENDIF

        ENDDO







 128    FORMAT(1X,I2,2D13.5) 

	RETURN
	END SUBROUTINE JERDFUN_MELT


        SUBROUTINE JERNEWF_MELT &

       (NRX,RR,RN,FI,PSI,FL_OLD,FL_NEW,I3POINT)
 
        IMPLICIT NONE

        INTEGER & 
        KR


        INTEGER & 
        I,K,NRXP,I3POINT


        INTEGER & 
        ISIGN_DIFFUSIONAL_GROWTH

 
        DOUBLE PRECISION &
	AOLDCON,ANEWCON,AOLDMASS,ANEWMASS

        DOUBLE PRECISION &
        RNTMP,RRTMP,RRP,RRM,RNTMP2,RRTMP2,RRP2,RRM2, &
        GN1,GN2,GN3,GN1P,GMAT,GMAT2

        INTEGER & 
        NRX

        DOUBLE PRECISION & 
        RR(NRX),FI(NRX),PSI(NRX),RN(NRX) &

       ,RRS(NRX+1),PSINEW(NRX+1)


        DOUBLE PRECISION & 
        FL_OLD(NRX),FL_NEW(NRX)

        DOUBLE PRECISION & 

        DROPMASS(NRX+1)


        DOUBLE PRECISION & 
        PSI_IM,PSI_I,PSI_IP


 
	NRXP=NRX+1

        DO I=1,NRX



           IF(RN(I).LT.0.0D0) THEN 
             RN(I)=1.0D-50
	     FI(I)=0.0D0
           ENDIF

        ENDDO

	DO K=1,NRX
	   PSI(K)=0.0D0

	   PSINEW(K)=0.0D0

	   RRS(K)=RR(K)
           DROPMASS(K)=0.0D0
	ENDDO
	
        RRS(NRXP)=RRS(NRX)*1024.0D0

        PSINEW(NRXP)=0.0D0



        DROPMASS(NRXP)=0.0D0

 


        ISIGN_DIFFUSIONAL_GROWTH=0

	DO K=1,NRX
           IF(RN(K).NE.RR(K)) THEN
              ISIGN_DIFFUSIONAL_GROWTH=1
              GOTO 2000
           ENDIF
        ENDDO

 2000   CONTINUE
       
        IF(ISIGN_DIFFUSIONAL_GROWTH.NE.0) THEN





	  DO K=1,NRX

             IF(FI(K).NE.0.0D0) THEN

               I=1

               DO WHILE &

                (.NOT.(RRS(I).LE.RN(K).AND.RRS(I+1).GT.RN(K)) &

                 .AND.I.LT.NRX)
                  I=I+1
	       ENDDO

	       IF(RN(K).LT.RRS(1)) THEN

                 RNTMP=RN(K)
                 RRTMP=0.0D0
                 RRP=RRS(1)
    	         GMAT2=(RNTMP-RRTMP)/(RRP-RRTMP)

                 PSINEW(1)=PSINEW(1)+FI(K)*RR(K)*GMAT2
                 DROPMASS(1)= &
                 DROPMASS(1)+FL_OLD(K)*RR(1)*FI(K)*RR(K)*GMAT2


               ELSE

                 RNTMP=RN(K)
                 RRTMP=RRS(I)
                 RRP=RRS(I+1)
	         GMAT2=(RNTMP-RRTMP)/(RRP-RRTMP)
	         GMAT=(RRP-RNTMP)/(RRP-RRTMP)

                 PSINEW(I)=PSINEW(I)+FI(K)*RR(K)*GMAT
                 PSINEW(I+1)=PSINEW(I+1)+FI(K)*RR(K)*GMAT2

                 DROPMASS(I)= &
                 DROPMASS(I)+FL_OLD(K)*RR(I)*FI(K)*RR(K)*GMAT



                 DROPMASS(I+1)= &
                 DROPMASS(I+1)+FL_OLD(K)*RRS(I+1)*FI(K)*RR(K)*GMAT2



               ENDIF



             ENDIF

	  ENDDO



          DO I=1,NRX

             PSI(I)=PSINEW(I)

             IF(PSI(I).NE.0.D0) THEN
               FL_NEW(I)=DROPMASS(I)/RR(I)/PSI(I)
             ELSE


               FL_NEW(I)=0.0D0

             ENDIF
          ENDDO
               





	  AOLDCON=0.0D0
	  ANEWCON=0.0D0
	  AOLDMASS=0.0D0
	  ANEWMASS=0.0D0
	
	  DO K=1,NRX
	     AOLDCON=AOLDCON+FI(K)*RR(K)
	     ANEWCON=ANEWCON+PSI(K)
	     AOLDMASS=AOLDMASS+FI(K)*RR(K)*RN(K)
	     ANEWMASS=ANEWMASS+PSI(K)*RR(K)
	  ENDDO
	  


          IF(I3POINT.NE.0) GOTO 2001

 
	    DO K=1,NRX

               IF(FI(K).NE.0.0D0) THEN

                 IF(RRS(2).LT.RN(K)) THEN
 
                   I=2

                   DO  WHILE &
                     (.NOT.(RRS(I).LT.RN(K).AND.RRS(I+1).GT.RN(K)) &
                      .AND.I.LT.NRX)
                       I=I+1
	           ENDDO

                   IF(I.LT.NRX-2) THEN

                     RNTMP=RN(K)

                     RRTMP=RRS(I)
                     RRP=RRS(I+1)
                     RRM=RRS(I-1)
 
                     RNTMP2=RN(K+1)

                     RRTMP2=RRS(I+1)
                     RRP2=RRS(I+2)
                     RRM2=RRS(I)
 
                     GN1=(RRP-RNTMP)*(RRTMP-RNTMP)/(RRP-RRM)/ &
                         (RRTMP-RRM)

                     GN1P=(RRP2-RNTMP2)*(RRTMP2-RNTMP2)/ &
                          (RRP2-RRM2)/(RRTMP2-RRM2)

                     GN2=(RRP-RNTMP)*(RNTMP-RRM)/(RRP-RRTMP)/ &
                         (RRTMP-RRM)
 
	             GMAT=(RRP-RNTMP)/(RRP-RRTMP)

                     GN3=(RRTMP-RNTMP)*(RRM-RNTMP)/(RRP-RRM)/ &
                                                 (RRP-RRTMP)
	             GMAT2=(RNTMP-RRTMP)/(RRP-RRTMP)

                     PSI_IM=PSI(I-1)+GN1*FI(K)*RR(K)
                     PSI_I=PSI(I)+(GN1P+GN2-GMAT)*FI(K+1)*RR(K+1)
                     PSI_IP=PSI(I+1)+(GN3-GMAT2)*FI(K)*RR(K)
                    
                     IF(PSI_IM.GT.0.0D0) THEN

                       IF(PSI_IP.GT.0.0D0) THEN

                         IF(I.GT.2) THEN

                           IF(PSI_IM.GT.PSI(I-2) &
                          .AND.PSI_IM.LT.PSI_I &
                          .AND.PSI(I-2).LT.PSI(I) &
                          .OR.PSI(I-2).GE.PSI(I)) THEN

                             PSI(I-1)=PSI_IM

                             PSI(I)=PSI(I)+FI(K)*RR(K)*(GN2-GMAT)

                             PSI(I+1)=PSI_IP

                           ENDIF 

                         ENDIF



	               ENDIF



	             ENDIF

                   ENDIF



                 ENDIF
 


               ENDIF

 1000          CONTINUE

	    ENDDO

	    AOLDCON=0.0D0
	    ANEWCON=0.0D0
	    AOLDMASS=0.0D0
	    ANEWMASS=0.0D0

	    DO K=1,NRX
	       AOLDCON=AOLDCON+FI(K)*RR(K)
	       ANEWCON=ANEWCON+PSI(K)
	       AOLDMASS=AOLDMASS+FI(K)*RR(K)*RN(K)
	       ANEWMASS=ANEWMASS+PSI(K)*RR(K)
	    ENDDO









 2001     CONTINUE





	  DO K=1,NRX
	     PSI(K)=PSI(K)/RR(K)
	  ENDDO





        ELSE





	  DO K=1,NRX
	     PSI(K)=FI(K)
	  ENDDO

        ENDIF


  201	FORMAT(1X,D13.5)
  202	FORMAT(1X,2D13.5)
  203	FORMAT(1X,3D13.5)
  204	FORMAT(1X,4D13.5)
  205	FORMAT(1X,5D13.5)
  206	FORMAT(1X,6D13.5)
  301   FORMAT(1X,I2,2X,D13.5)
  302   FORMAT(1X,I2,2X,2D13.5)
  303   FORMAT(1X,I2,2X,3D13.5)
  304   FORMAT(1X,I2,2X,4D13.5)
  305   FORMAT(1X,I2,2X,5D13.5)
  306   FORMAT(1X,I2,2X,6D13.5)

        RETURN 
        END SUBROUTINE JERNEWF_MELT






SUBROUTINE SHEDDING &

(ihucm_flag&

,FF1,XL,VTL &
,FF4,XG,V4,VTG,FLIQFR_G,RHO_G &
,FF5,XH,V5,VTH,FLIQFR_H,RHO_H &
,TIN,rhoa,pres,DT,QQV)











implicit double precision (a-h,o-z)





PARAMETER(COL=0.23105D0, CP=1004.7D0, RV=461.51D0, RD=287.039D0, &
          EPS=RD/RV, FJOULES_IN_A_CAL=4.187D0, PI=3.141592654D0, &
          AR_LIM=2.D0, GRAV=9.8D0, RHO_ICE=920.D0, &
          RHO_WATER=1000.D0, FLIQFRAC_LIM=0.9D0, &
          PETIT_PARAMETRE=1.D-10)
	  

PARAMETER(ISHEDDING_ON=1, IVT_ADJUST=1, IPRINTING=0, &
          ITEMP_ADJUST=1, IEVAP_ADJUST=1, ISUBLIME_ADJUST=1)








DIMENSION FF1(NKR), XL(NKR), VTL(NKR)



DIMENSION FF4(NKR),XG(NKR),V4(NKR), &
          VTG(NKR),FLIQFR_G(NKR),RHO_G(NKR)

DIMENSION FF5(NKR),XH(NKR),V5(NKR), &
          VTH(NKR),FLIQFR_H(NKR),RHO_H(NKR)

DIMENSION FF1_SI(NKR), XL_SI(NKR), & 
          VTL_SI(NKR)

DIMENSION FF4_SI(NKR),XG_SI(NKR),V4_SI(NKR), &
          VTG_SI(NKR), RHO_G_SI(NKR)

DIMENSION FF5_SI(NKR),XH_SI(NKR),V5_SI(NKR), &
          VTH_SI(NKR), RHO_H_SI(NKR)
	  
INTRINSIC SUM


If(TIN <= 273.15D0) then
  RETURN
ENDIF

if(SUM(FF4) <= 0.D0.and.SUM(FF5) <= 0.D0) then

  return
  
endif





if(ihucm_flag == 1) then

RHO_G_SI = RHO_G*1000.D0
RHO_H_SI = RHO_H*1000.D0

XL_SI = XL/1000.D0
XG_SI = XG/1000.D0
XH_SI = XH/1000.D0


VTL_SI = VTL/100.D0
VTG_SI = VTG/100.D0
VTH_SI = VTH/100.D0

V4_SI = V4/100.D0
V5_SI = V5/100.D0

FF1_SI = 1.E9*FF1
FF4_SI = 1.E9*FF4
FF5_SI = 1.E9*FF5

pres_SI = pres/10.D0
rhoa_SI = rhoa*1000.D0



else



RHO_G_SI = RHO_G
RHO_H_SI = RHO_H


XL_SI = XL
XG_SI = XG
XH_SI = XH

VTL_SI = VTL
VTG_SI = VTG
VTH_SI = VTH

V4_SI = V4
V5_SI = V5

FF1_SI = FF1
FF4_SI = FF4
FF5_SI = FF5

pres_SI = pres
rhoa_SI = rhoa



endif





V4_SI(:) = VTG_SI(:)
V5_SI(:) = VTH_SI(:)

ee = QQV*pres_SI/(EPS + QQV)

es_zero = 611.21D0

if(pres_SI > 200000.D0.or.pres_SI < 10000.D0) call wrf_error_fatal3("<stdin>",12548,&
"fatal error in module_mp_full_sbm , model stop 9071")

D_V=0.211D0*((TIN/273.15D0)**1.94D0)*(101325.D0/pres_SI)/1.D4




FK_a =(5.69D0+0.017D0*(TIN-273.15D0))*1.0D-3*4.187D0











XLV = 597.3D0
XLV = XLV*FJOULES_IN_A_CAL*1000.D0
XLS = 2.83D6



XLF = 79.7D0
XLF = XLF*FJOULES_IN_A_CAL*1000.D0



etaa = (1.718D0 + 0.0049D0*(TIN-273.15D0) - &
        1.2D-5*(TIN-273.15D0)*(TIN-273.15D0))*1.D-5



FNSC = etaa/(rhoa_SI*D_V)



ALPHA_H = FK_a/(CP*rhoa_SI)
FNPR = etaa/(rhoa_SI*ALPHA_H)
RHO_CRIT = 910.D0

if(rhoa_SI > 2.D0) call wrf_error_fatal3("<stdin>",12592,&
"fatal error in module_mp_full_sbm , model stop 111")

if(rhoa_SI < 0.1D0) then



  call wrf_error_fatal3("<stdin>",12599,&
"fatal error in module_mp_full_sbm , model stop 112")
endif

if(RHO_H_SI(1) < 1.D0) then
 
  call wrf_error_fatal3("<stdin>",12605,&
"fatal error in module_mp_full_sbm , model stop 113")
endif

TS = SURFACE_TEMP(ee, TIN, XLS*D_V/(FK_a*RV), 1.D0, XLS, RV)
if(TS > 273.15D0) TS = 273.15D0





ISIGN_GRAUPEL=1
ISIGN_HAIL=0

DO IK = 1, NKR

   IK_MELT=IK
   I_MELT=0

if(TIN > 273.15D0) then

IF(FLIQFR_G(IK).GE.1.D0.OR.FF4_SI(IK).LE.PETIT_PARAMETRE.OR. &
TIN <= 273.15D0) THEN
  IF(FLIQFR_G(IK) > 1.D0) FLIQFR_G(IK) = 1.D0
CYCLE
ENDIF

vt_start = 0.D0
vt_end = 0.D0

rhoi = RHO_G_SI(IK)
fm_i = XG_SI(IK)*(1.D0 - FLIQFR_G(IK))
V_i = fm_i/rhoi

fm_w = XG_SI(IK)*FLIQFR_G(IK)
V_w = fm_w/RHO_WATER

if(rhoi < RHO_CRIT) then
  V_soakable = V_i - fm_i/RHO_ICE
else
  V_soakable = 0.D0
endif

a_i = rad_sphere(V_i)
a_izero = rad_sphere(XG_SI(IK)/rhoi)
fnre_dry = VTG_SI(IK) * 2.D0*rhoa_SI*a_izero/etaa




X_Best = 8.D0 * XG_SI(IK) * rhoa_SI * GRAV / (PI * etaa * etaa)
fnre_smooth = fnre_sphere(X_Best)

if(V_w < V_soakable) then

  a_d = a_i
  vt=VT_LOW_DENSITY_SOAKING &
    (fnre_dry,fnre_smooth,VTG_SI(IK),a_i,a_izero,etaa,rhoa_SI)



else



  a_d = rad_sphere(V_i + (V_w - V_soakable))
  fm_w_soaked = RHO_WATER* V_soakable
  fm_w_crit = (0.268D0 + (fm_i + fm_w_soaked) * 1.D3 * 0.1389D0)
  fm_w_crit = fm_w_crit* 1.D-3
  a_crit = rad_sphere(V_i + fm_w_crit/RHO_WATER)

  if(rhoi < RHO_CRIT) then
    vt_start = VT_LOW_DENSITY_TRANS &
              (fnre_dry, fnre_smooth, &
               VTG_SI(IK),a_izero,etaa,rhoa_SI,rhoi,XG_SI(IK))
  else
    vt_start=VT_HIGH_DENSITY_TRANS &
            (fnre_dry,fnre_smooth,VTG_SI(IK),a_izero,etaa,rhoa_SI)
  endif

  vt_end=equilibrium_fallspeed &
        (fm_i+fm_w_soaked,fm_w_crit, &
         XG(:),VTL_SI(:),rhoa_SI,etaa,a_crit)

  frac_eqm=(fm_w-fm_w_soaked)/fm_w_crit

  if(frac_eqm < 0.D0) frac_eqm = 0.D0
  if(frac_eqm > 1.D0) frac_eqm = 1.D0

  vt = vt_start + (vt_end - vt_start) * frac_eqm

  if(vt < 0.D0) vt = 0.D0



endif



if(ivt_G_H_interpol.ne.0) then

  vt=VTG_SI(IK)+FLIQFR_G(IK)*(VTL_SI(IK) - VTG_SI(IK))
  
endif



V4_SI(IK) = vt

fnre = vt * (2.D0 * a_d * rhoa_SI)/etaa

fv = HAIL_VENTILATION_COEF(fnre,FNSC,IK)
fh = HAIL_VENTILATION_COEF(fnre,FNPR,IK)



if(FLIQFR_G(IK) <= 0.D0) then
  TS = SURFACE_TEMP(ee, TIN, XLS*D_V/(FK_a*RV), fv/fh, XLS, RV)
else
  TS = 273.15D0
endif



if(TS > 273.15D0) TS = 273.15D0

if(fnre < 6000.D0) then
  CAP = a_d
else
  CAP = a_i
endif

if(FLIQFR_G(IK) <= FLIQFRAC_LIM) then

  if(ISHEDDING_ON.eq.1) then

    if(IPRINTING == 1) print *,' SHEDDING CODE(GRAUPEL)  '

    CALL SHED_MELTWATER &
   (fnre,rhoi,RHO_CRIT,XG_SI,FF4_SI,FLIQFR_G,XL_SI,FF1_SI,IK)

  endif
  


endif



endif

ENDDO






ISIGN_GRAUPEL=0
ISIGN_HAIL=1

DO IK = 1, NKR

   IK_MELT=IK
   I_MELT=0

if(TIN > 273.15D0) then

IF(FLIQFR_H(IK).GE.1.D0.OR.FF5_SI(IK).LE.PETIT_PARAMETRE.OR. &
TIN <= 273.15D0) THEN
  IF(FLIQFR_H(IK) > 1.D0) FLIQFR_H(IK) = 1.D0
CYCLE
ENDIF

vt_start = 0.D0
vt_end = 0.D0

rhoi  = RHO_H_SI(IK)
fm_i = XH_SI(IK)*(1.D0 - FLIQFR_H(IK))
V_i = fm_i/rhoi

fm_w = XH_SI(IK)*FLIQFR_H(IK)
V_w = fm_w/RHO_WATER

if(rhoi < RHO_CRIT) then
  V_soakable = V_i - fm_i/RHO_ICE
else
  V_soakable = 0.D0
endif

a_i = rad_sphere(V_i)
a_izero = rad_sphere(XH_SI(IK)/rhoi)




if(IPRINTING == 1) print *, 'fnre_dry = ', fnre_dry

fnre_dry=VTH_SI(IK)*2.D0*rhoa_SI*a_izero/etaa
X_Best=8.D0*XH_SI(IK)*rhoa_SI*GRAV/(PI * etaa * etaa)
fnre_smooth=fnre_sphere(X_Best)

vt_justwet = 0.D0
vt_justsoaked = 0.D0

if(V_w < V_soakable) then



  a_d = a_i
  vt=VT_LOW_DENSITY_SOAKING &
    (fnre_dry,fnre_smooth,VTH_SI(IK),a_i,a_izero,etaa,rhoa_SI)



else



  a_d = rad_sphere(V_i + (V_w - V_soakable))
  fm_w_soaked = RHO_WATER* V_soakable
  fm_w_crit=(0.268D0+(fm_i+fm_w_soaked)*1.D3*0.1389D0)
  fm_w_crit = fm_w_crit* 1.D-3
  a_crit = rad_sphere(V_i + fm_w_crit/RHO_WATER)






  if(rhoi < RHO_CRIT) then

    vt_start = VT_LOW_DENSITY_TRANS &
              (fnre_dry,fnre_smooth, &
               VTH_SI(IK),a_izero,etaa,rhoa_SI,rhoi,XH_SI(IK))
  else

    vt_start = VT_HIGH_DENSITY_TRANS(fnre_dry, fnre_smooth, &
    VTH_SI(IK), a_izero, etaa, rhoa_SI)

  endif

    vt_end=equilibrium_fallspeed &
          (fm_i + fm_w_soaked, fm_w_crit, XH(:), &
           VTL_SI(:), rhoa_SI, etaa, a_crit)




    frac_eqm = (fm_w - fm_w_soaked)/fm_w_crit
    if(frac_eqm < 0.D0) frac_eqm = 0.D0
    if(frac_eqm > 1.D0) frac_eqm = 1.D0

    vt = vt_start + (vt_end - vt_start) * frac_eqm

    if(vt < 0.D0) then
      if(IPRINTING == 1) print *, 'WARNING: vt < 0', vt
      vt = 0.D0
    endif

    if(IPRINTING == 1) print *, &
   'HERE 2:: vt_start,vt_end,a_izero/a_i= ', &
             vt_start,vt_end,a_izero/a_i

    if(IPRINTING == 1) print *, &
   'HERE 2:: fnre_dry,fnre_smooth,vt_justsoaked,vt_justwet', &
             fnre_dry,fnre_smooth,vt_justsoaked,vt_justwet



endif



if(ivt_G_H_interpol.ne.0) then

  vt=VTH_SI(IK)+FLIQFR_H(IK)*(VTL_SI(IK) - VTH_SI(IK))
  
endif
  


V5_SI(IK) = vt

fnre = vt * (2.D0 * a_d * rhoa_SI)/etaa



fv = HAIL_VENTILATION_COEF(fnre,FNSC,IK)
fh = HAIL_VENTILATION_COEF(fnre,FNPR,IK)



if(FLIQFR_H(IK) <= 0.D0) then
  TS = SURFACE_TEMP(ee, TIN, XLS*D_V/(FK_a*RV), fv/fh, XLS, RV)
else
  TS = 273.15D0
endif



if(TS > 273.15D0) TS = 273.15D0

if(fnre < 6000.D0) then
  CAP = a_d
else
  CAP = a_i
endif

if(FLIQFR_H(IK) <= FLIQFRAC_LIM) then

  if(ISHEDDING_ON.eq.1) then

    CALL SHED_MELTWATER &
   (fnre,rhoi,RHO_CRIT,XH_SI,FF5_SI,FLIQFR_H,XL_SI,FF1_SI,IK)



  endif
  


endif



endif

ENDDO






if(ihucm_flag == 1) then

  if(IVT_ADJUST == 1) then
    V4 = 100.D0 * V4_SI
    V5 = 100.D0 * V5_SI
  endif

  FF1 = 1.D-9*FF1_SI
  FF4 = 1.D-9*FF4_SI
  FF5 = 1.D-9*FF5_SI
  


else



  if(IVT_ADJUST == 1) then
    V4 = V4_SI
    V5 = V5_SI
  endif

  FF1 = FF1_SI
  FF4 = FF4_SI
  FF5 = FF5_SI
  


endif

 101 FORMAT(1X,D13.5)
 102 FORMAT(1X,2D13.5)
 103 FORMAT(1X,3D13.5)
 104 FORMAT(1X,4D13.5)
 105 FORMAT(1X,5D13.5)
 106 FORMAT(1X,6D13.5)
 107 FORMAT(1X,7D13.5)
 201 FORMAT(1X,I2,D13.5)
 202 FORMAT(1X,I2,2D13.5)
 203 FORMAT(1X,I2,3D13.5)
 204 FORMAT(1X,I2,4D13.5)

END SUBROUTINE



SUBROUTINE SHED_MELTWATER &
(fnrex,rhoix,RHO_CRITX,XX,FFX,FLIQFR_X,XL_SI,FF1_SI,INK)

implicit double precision (a-h,o-z)




PARAMETER(PI=3.141592654D0, RHO_ICE=920D0, RHO_WATER=1000.D0, &





IPRINTING=0, & 


COL=0.23105D0, FMAX_DROP_MASS_FRACTION=0.5D0)






DIMENSION XX(NKR), XL_SI(NKR)
DIMENSION FFX(NKR), FF1_SI(NKR), FLIQFR_X(NKR)

DIMENSION fmass_ice(NKR), fmass_X(NKR)

INTRINSIC DABS, SUM

fm_i = XX(INK)*(1.D0 - FLIQFR_X(INK))

V_i = fm_i/rhoix

fm_w = XX(INK)*FLIQFR_X(INK)

V_w = fm_w/RHO_WATER

if(rhoix < RHO_CRITX) then
  V_soakable = V_i - fm_i/RHO_ICE
else
  V_soakable = 0.D0
endif

if(V_w > V_soakable) then





  fm_w_soaked = RHO_WATER*V_soakable
  fm_w_crit=(0.268D0+(fm_i+fm_w_soaked)*1.D3*0.1389D0)
  fm_w_crit = fm_w_crit*1.D-3





  if(fm_w - fm_w_soaked > fm_w_crit) then





    if(fnrex > 1.5D4) then
      melting_mode = 2
    else
      if(fnrex > 1.D4 ) then
        melting_mode = 3
      else
        melting_mode = 4
      endif
    endif

    select case (melting_mode)

       case(2)
       d_w_shed = 1.5D-3

       case(3)
       d_w_shed = 3.D-3

       case(4)
       d_w_shed = 4.5E-3

       case default

 call wrf_error_fatal3("<stdin>",13074,&
"fatal error in module_mp_full_sbm , model stop 9999")

    end select

    drop_mass = RHO_WATER*(PI/6.D0)*(d_w_shed**3.D0)

    if(drop_mass > fm_w_crit*FMAX_DROP_MASS_FRACTION) &
    drop_mass = fm_w_crit*FMAX_DROP_MASS_FRACTION

    fm_w_save=fm_w

    if(melting_mode == 2) then

      if(fnrex > 2.5D4) then



        fm_w = fm_w_soaked

      else



        fm_w = fm_w_crit + fm_w_soaked

      endif



    else






      fm_w =  fm_w - drop_mass



    endif

    if(fm_w - fm_w_soaked > fm_w_crit) fm_w = fm_w_crit + fm_w_soaked

    if(fm_w < fm_w_soaked) call wrf_error_fatal3("<stdin>",13119,&
"fatal error in module_mp_full_sbm , model stop 9065")

    fm_w_shed = fm_w_save - fm_w

    ILIQ = IFIND_IK(drop_mass, XL_SI, frac_liq)

    INEW = IFIND_IK(fm_w + fm_i, XX, frac)

    if(INEW < INK) then





      fmass_X(:)=FFX(:)*XX(:)*XX(:)*3.D0*COL
      fmass_ice(:)=FFX(:)*XX(:)*(1.D0-FLIQFR_X(:))*XX(:)*3.D0*COL
      fm_X_before = SUM(fmass_X)
      fm_ice_before = SUM(fmass_ice)




      res_mass_shed =  FFX(INK) * fm_w_shed * XX(INK)*3.D0*COL
      fmass_X(INK) = fmass_X(INK) - res_mass_shed

      if(fmass_X(INK) < 0.D0) call wrf_error_fatal3("<stdin>",13145,&
"fatal error in module_mp_full_sbm , model stop 8020")
      if(res_mass_shed < 0.D0) call wrf_error_fatal3("<stdin>",13147,&
"fatal error in module_mp_full_sbm , model stop 8021")




      res_mass_X = fmass_X(INK)
      fmass_X(INK) = 0.D0
      res_mass_ice = fmass_ice(INK)
      fmass_ice(INK) = 0.D0




      fmass_X(INEW)=fmass_X(INEW )+(1.D0-frac)*res_mass_X
      fmass_X(INEW+1)=fmass_X(INEW+1)+frac*res_mass_X
      res_mass_X = 0.D0




      fmass_ice(INEW)=fmass_ice(INEW)+(1.D0-frac)*res_mass_ice
      fmass_ice(INEW+1)=fmass_ice(INEW+1)+frac*res_mass_ice
      res_mass_ice=0.D0



      FF1_SI(ILIQ)=FF1_SI(ILIQ)+ &
      res_mass_shed/(XL_SI(ILIQ)*XL_SI(ILIQ)*3.D0*COL)

      FFX(INEW)=fmass_X (INEW)/(XX(INEW)*XX(INEW)*3.D0*COL)
      FFX(INEW+1)=fmass_X (INEW+1)/(XX(INEW+1)*XX(INEW+1)*3.D0*COL)
      FFX(INK)=fmass_X (INK)/(XX(INK)*XX(INK)*3.D0*COL)

      if(FFX(INEW) > 0.D0) then
      
        FLIQFR_X(INEW)= &
        1.D0-fmass_ice (INEW)/(XX(INEW)*FFX(INEW)*XX(INEW)*3.D0*COL)
	

        if(DABS(FLIQFR_X(INEW)) < 1.0D-3) FLIQFR_X(INEW)= 0.0D0 


      else

        FLIQFR_X(INEW) = 1.D0

      endif

      if(FFX(INEW+1) > 0.D0) then

        FLIQFR_X(INEW+1)=1.D0 - &
                         fmass_ice(INEW+1)/ &
                        (XX(INEW+1)*FFX(INEW+1)*XX(INEW+1)*3.D0*COL)

        if(DABS(FLIQFR_X(INEW+1)) < 1.0D-3) FLIQFR_X(INEW+1)= 0.0D0 


      else

        FLIQFR_X(INEW+1) = 1.D0

      endif

      if(FFX(INK) > 0.D0) then

        FLIQFR_X(INK)=1.D0 - fmass_ice(INK)/ &
                            (XX(INK)*FFX(INK)*XX(INK)*3.D0*COL)

        if(DABS(FLIQFR_X(INK)) < 1.0D-3) FLIQFR_X(INK)= 0.0D0 


      else

        FLIQFR_X(INK) = 1.D0

      endif









      if(FLIQFR_X(INEW) < 0.D0.or.FLIQFR_X(INEW) > 1.D0) THEN
      
      
      









       
          call wrf_error_fatal3("<stdin>",13247,&
"fatal error in module_mp_full_sbm , model stop 8003")
	
      endif
	

 
      if(FLIQFR_X(INEW+1) < 0.D0.or.FLIQFR_X(INEW+1) > 1.D0) call wrf_error_fatal3("<stdin>",13254,&
"fatal error in module_mp_full_sbm , model stop 8004")
      if(FLIQFR_X(INK) < 0.D0.or.FLIQFR_X(INK) > 1.D0) call wrf_error_fatal3("<stdin>",13256,&
"fatal error in module_mp_full_sbm , model stop 8005")

      fmass_X(:)=FFX(:)*XX(:)*XX(:)*3.D0*COL
      fmass_ice(:)=FFX(:)*XX(:)*(1.D0-FLIQFR_X(:))*XX(:)*3.D0*COL

      fm_X_after = SUM(fmass_X)
      fm_ice_after = SUM(fmass_ice)

      if(fm_ice_before > 0.D0) then

        fjunk = (fm_ice_after/fm_ice_before-1.D0)*100.D0









      endif

      if(fm_X_before > 0.D0) then

        fjunk=((fm_X_after+res_mass_shed)/fm_X_before-1.D0)*100.D0








      endif







    else





   

 

 

 
 

 
 

       call wrf_error_fatal3("<stdin>",13316,&
"fatal error in module_mp_full_sbm , model stop 9089")
      




    endif



  endif



endif



106 FORMAT(1X,6D13.5)



END SUBROUTINE




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
         rho(k) = 0.622*pres(k)/(R_MORR*temp(k)*(qv(k)+0.622))

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
         if (L_qs(k)) ze_snow(k) = (0.176/0.93) * (6.0/PI_MORR)*(6.0/PI_MORR)     &
                                 * (xam_s/900.0)*(xam_s/900.0)          &
                                 * N0_s(k)*xcsg(4)*ilams(k)**xcse(4)
         if (L_qg(k)) ze_graupel(k) = (0.176/0.93) * (6.0/PI_MORR)*(6.0/PI_MORR)  &
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

      END MODULE module_mp_full_sbm
