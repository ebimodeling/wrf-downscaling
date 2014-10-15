


MODULE module_mp_lin

   USE     module_wrf_error
   USE module_utility, ONLY: WRFU_Clock, WRFU_Alarm
   USE module_domain, ONLY : HISTORY_ALARM, Is_alarm_tstep
   USE module_mp_radar

   REAL    , PARAMETER, PRIVATE ::       RH = 1.0

   REAL    , PARAMETER, PRIVATE ::     xnor = 8.0e6
   REAL    , PARAMETER, PRIVATE ::     xnos = 3.0e6






  REAL     , PARAMETER, PRIVATE ::     xnog = 4.0e6
  REAL     , PARAMETER, PRIVATE ::     rhograul = 400.


  REAL     , PARAMETER, PRIVATE ::                              &
             qi0 = 1.0e-3, ql0 = 7.0e-4, qs0 = 6.0E-4,          &
             xmi50 = 4.8e-10, xmi40 = 2.46e-10,                 &
             constb = 0.8, constd = 0.25,                       &
             o6 = 1./6.,  cdrag = 0.6,                          &
             avisc = 1.49628e-6, adiffwv = 8.7602e-5,           &
             axka = 1.4132e3, di50 = 1.0e-4, xmi = 4.19e-13,    &
             cw = 4.187e3, vf1s = 0.78, vf2s = 0.31,            &
             xni0 = 1.0e-2, xmnin = 1.05e-18, bni = 0.5,        &
             ci = 2.093e3
CONTAINS






  SUBROUTINE lin_et_al(th                                          &
                      ,qv, ql, qr                                  &
                      ,qi, qs                                      &
                      ,rho, pii, p                                 &
                      ,dt_in                                       &
                      ,z,ht, dz8w                                  &
                      ,grav, cp, Rair, rvapor                      &
                      ,XLS, XLV, XLF, rhowater, rhosnow            &
                      ,EP2,SVP1,SVP2,SVP3,SVPT0                    &
                      , RAINNC, RAINNCV                            &
                      , SNOWNC, SNOWNCV                            &
                      , GRAUPELNC, GRAUPELNCV, SR                  &
                      ,refl_10cm, diagflag, do_radar_ref           &
                      ,ids,ide, jds,jde, kds,kde                   &
                      ,ims,ime, jms,jme, kms,kme                   &
                      ,its,ite, jts,jte, kts,kte                   &
                  
                      ,qlsink, precr, preci, precs, precg          &
                      , F_QG,F_QNDROP                              &
                      , qg, qndrop                                 &
                                                                   )

  IMPLICIT NONE




  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(INOUT) ::                                          &
                                                              th, &
                                                              qv, &
                                                              ql, &
                                                              qr


  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(IN   ) ::                                          &
                                                             rho, &
                                                             pii, &
                                                               p, &
                                                            dz8w

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(IN   ) ::                                       z



  REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN) ::       ht

  REAL, INTENT(IN   ) ::                                   dt_in, &
                                                            grav, &
                                                            Rair, &
                                                          rvapor, &
                                                              cp, &
                                                             XLS, &
                                                             XLV, &
                                                             XLF, &
                                                        rhowater, &
                                                         rhosnow

  REAL, INTENT(IN   ) :: EP2,SVP1,SVP2,SVP3,SVPT0

  REAL, DIMENSION( ims:ime , jms:jme ),                           &
        INTENT(INOUT) ::                                  RAINNC, &
                                                         RAINNCV, &
                                                              SR


  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT)::     &  
                                                       refl_10cm



  REAL, DIMENSION( ims:ime , jms:jme ),                           &
        OPTIONAL,                                                 &
        INTENT(INOUT) ::                                  SNOWNC, &
                                                         SNOWNCV, &
                                                       GRAUPELNC, &
                                                      GRAUPELNCV

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        OPTIONAL,                                                 &
        INTENT(INOUT) ::                                          &
                                                              qi, &
                                                              qs, &
                                                              qg, &
                                                          qndrop

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        OPTIONAL, INTENT(OUT   ) ::                               &
        qlsink, & 
        precr,  & 
        preci,  & 
        precs,  & 
        precg     

  LOGICAL, INTENT(IN), OPTIONAL ::                F_QG, F_QNDROP



  INTEGER             ::                            min_q, max_q

  REAL, DIMENSION( its:ite , jts:jte )                            &
                               ::        rain, snow, graupel,ice

  REAL, DIMENSION( kts:kte )   ::                  qvz, qlz, qrz, &
                                                   qiz, qsz, qgz, &
                                                             thz, &
                                                     tothz, rhoz, &
                                                   orhoz, sqrhoz, &
                                                        prez, zz, &
                                  precrz, preciz, precsz, precgz, &
                                                         qndropz, &
                                                     dzw, preclw

  LOGICAL :: flag_qg, flag_qndrop

  REAL    ::         dt, pptrain, pptsnow, pptgraul, rhoe_s,      &
                     gindex, pptice
  real :: qndropconst

  INTEGER ::               i,j,k


      REAL, DIMENSION(kts:kte):: qv1d, t1d, p1d, qr1d, qs1d, qg1d, dBZ
      LOGICAL, OPTIONAL, INTENT(IN) :: diagflag
      INTEGER, OPTIONAL, INTENT(IN) :: do_radar_ref


   flag_qg     = .false.
   flag_qndrop = .false.
   IF ( PRESENT ( f_qg     ) ) flag_qg     = f_qg
   IF ( PRESENT ( f_qndrop ) ) flag_qndrop = f_qndrop

   dt=dt_in
   rhoe_s=1.29
   qndropconst=100.e6  
   gindex=1.0

   IF (.not.flag_qg) gindex=0.

   j_loop:  DO j = jts, jte
   i_loop:  DO i = its, ite



   DO k = kts, kte
      qvz(k)=qv(i,k,j)
      qlz(k)=ql(i,k,j)
      qrz(k)=qr(i,k,j)
      thz(k)=th(i,k,j)

      rhoz(k)=rho(i,k,j)
      orhoz(k)=1./rhoz(k)
      prez(k)=p(i,k,j)
      sqrhoz(k)=sqrt(rhoe_s*orhoz(k))
      tothz(k)=pii(i,k,j)
      zz(k)=z(i,k,j)
      dzw(k)=dz8w(i,k,j)
   END DO

   IF (flag_qndrop .AND. PRESENT( qndrop )) THEN
     DO k = kts, kte
         qndropz(k)=qndrop(i,k,j)
      ENDDO
   ELSE
      DO k = kts, kte
         qndropz(k)=qndropconst
      ENDDO
   ENDIF

   DO k = kts, kte
      qiz(k)=qi(i,k,j)
      qsz(k)=qs(i,k,j)
   ENDDO

   IF ( flag_qg .AND. PRESENT( qg ) ) THEN
      DO k = kts, kte
         qgz(k)=qg(i,k,j)
      ENDDO
   ELSE
      DO k = kts, kte
         qgz(k)=0.
      ENDDO
   ENDIF

   pptrain=0.
   pptsnow=0.
   pptgraul=0.
   pptice=0.
   CALL clphy1d(    dt, qvz, qlz, qrz, qiz, qsz, qgz,         &
                    qndropz,flag_qndrop,                      &
                    thz, tothz, rhoz, orhoz, sqrhoz,          &
                    prez, zz, dzw, ht(I,J), preclw,           &
                    precrz, preciz, precsz, precgz,           &
                    pptrain, pptsnow, pptgraul, pptice,       &
                    grav, cp, Rair, rvapor, gindex,           &
                    XLS, XLV, XLF, rhowater, rhosnow,         &
                    EP2,SVP1,SVP2,SVP3,SVPT0,                 &
                    kts, kte, i, j                            )







   rain(i,j)=pptrain
   snow(i,j)=pptsnow
   graupel(i,j)=pptgraul
   ice(i,j)=pptice
   sr(i,j)=(pptice+pptsnow+pptgraul)/(pptice+pptsnow+pptgraul+pptrain+1.e-12)

   RAINNCV(i,j)= pptrain + pptsnow + pptgraul + pptice
   RAINNC(i,j)=RAINNC(i,j) + pptrain + pptsnow + pptgraul + pptice
   IF(PRESENT(SNOWNCV))SNOWNCV(i,j)= pptsnow + pptice
   IF(PRESENT(SNOWNC))SNOWNC(i,j)=SNOWNC(i,j) + pptsnow + pptice
   IF(PRESENT(GRAUPELNCV))GRAUPELNCV(i,j)= pptgraul
   IF(PRESENT(GRAUPELNC))GRAUPELNC(i,j)=GRAUPELNC(i,j) + pptgraul





   IF ( present(qlsink) .and. present(precr) ) THEN 
      DO k = kts, kte
         if(ql(i,k,j)>1.e-20) then
            qlsink(i,k,j)=-preclw(k)/ql(i,k,j)
         else
            qlsink(i,k,j)=0.
         endif
         precr(i,k,j)=precrz(k)
      END DO
   END IF                                          

   DO k = kts, kte
      qv(i,k,j)=qvz(k)
      ql(i,k,j)=qlz(k)
      qr(i,k,j)=qrz(k)
      th(i,k,j)=thz(k)
   END DO

   IF ( flag_qndrop .AND. PRESENT( qndrop ) ) THEN 
      DO k = kts, kte
         qndrop(i,k,j)=qndropz(k)
      ENDDO
   END IF                                          

   DO k = kts, kte
      qi(i,k,j)=qiz(k)
      qs(i,k,j)=qsz(k)
   ENDDO

   IF ( present(preci) ) THEN     
      DO k = kts, kte
         preci(i,k,j)=preciz(k)
      ENDDO
   END IF
      
   IF ( present(precs) ) THEN
      DO k = kts, kte
         precs(i,k,j)=precsz(k)
      ENDDO
   END IF                         
      
   IF ( flag_qg .AND. PRESENT( qg ) ) THEN
      DO k = kts, kte
         qg(i,k,j)=qgz(k)
      ENDDO

      IF ( present(precg) ) THEN  
         DO k = kts, kte
            precg(i,k,j)=precgz(k)
         ENDDO                    
      END IF
   ELSE                           
      IF ( present(precg) ) precg(i,:,j)=0.  
   ENDIF


         IF ( PRESENT (diagflag) ) THEN
         if (diagflag .and. do_radar_ref == 1) then
               DO K=kts,kte
                  t1d(k)=th(i,k,j)*pii(i,k,j)
                  p1d(k)=p(i,k,j)
                  qv1d(k)=qv(i,k,j)
                  qr1d(k)=qr(i,k,j)
                  qs1d(k)=qs(i,k,j)
                  qg1d(k)=qg(i,k,j)
               ENDDO
               call refl10cm_lin (qv1d, qr1d, qs1d, qg1d,              &
                       t1d, p1d, dBZ, kts, kte, i, j)
               do k = kts, kte
                  refl_10cm(i,k,j) = MAX(-35., dBZ(k))
               enddo
         endif
         ENDIF



   ENDDO i_loop
   ENDDO j_loop

   END SUBROUTINE lin_et_al



   SUBROUTINE clphy1d(dt, qvz, qlz, qrz, qiz, qsz, qgz,                &
                      qndropz,flag_qndrop,                             &
                      thz, tothz, rho, orho, sqrho,                    &
                      prez, zz, dzw, zsfc, preclw,                     &
                      precrz, preciz, precsz, precgz,                  &
                      pptrain, pptsnow, pptgraul,                      &
                      pptice, grav, cp, Rair, rvapor, gindex,          &
                      XLS, XLV, XLF, rhowater, rhosnow,                &
                      EP2,SVP1,SVP2,SVP3,SVPT0,                        &
                      kts, kte, i, j                                   )

    IMPLICIT NONE























































  INTEGER, INTENT(IN   )               :: kts, kte, i, j

  REAL,    DIMENSION( kts:kte ),                                      &
           INTENT(INOUT)               :: qvz, qlz, qrz, qiz, qsz,    &
                                          qndropz,                    &
                                          qgz, thz

  REAL,    DIMENSION( kts:kte ),                                      &
           INTENT(IN   )               :: tothz, rho, orho, sqrho,    &
                                          prez, zz, dzw

  REAL,    INTENT(IN   )               :: dt, grav, cp, Rair, rvapor, &
                                          XLS, XLV, XLF, rhowater,    &
                                          rhosnow,EP2,SVP1,SVP2,SVP3,SVPT0

  REAL,    DIMENSION( kts:kte ), INTENT(OUT)               :: preclw, &
  		    precrz, preciz, precsz, precgz

  REAL,    INTENT(INOUT)               :: pptrain, pptsnow, pptgraul, pptice

  REAL,    INTENT(IN   )               :: zsfc
  logical, intent(in)                  :: flag_qndrop 



   REAL                                :: obp4, bp3, bp5, bp6, odp4,  &
                                          dp3, dp5, dp5o2




   REAL                                :: tmp, tmp0, tmp1, tmp2,tmp3,  &
                                          tmp4,delta2,delta3, delta4,  &
                                          tmpa,tmpb,tmpc,tmpd,alpha1,  &
                                          qic, abi,abr, abg, odtberg,  &
                                          vti50,eiw,eri,esi,esr, esw,  &
                                          erw,delrs,term0,term1,araut, &
                                          constg2, vf1r, vf2r,alpha2,  &
                                          Ap, Bp, egw, egi, egs, egr,  &
                                          constg, gdelta4, g1sdelt4,   &
                                          factor, tmp_r, tmp_s,tmp_g,  &
                                          qlpqi, rsat, a1, a2, xnin

  INTEGER                              :: k

  REAL, DIMENSION( kts:kte )    ::  oprez, tem, temcc, theiz, qswz,    &
                                    qsiz, qvoqswz, qvoqsiz, qvzodt,    &
                                    qlzodt, qizodt, qszodt, qrzodt,    &
                                    qgzodt

  REAL, DIMENSION( kts:kte )    :: psnow, psaut, psfw,  psfi,  praci,  &
                                   piacr, psaci, psacw, psdep, pssub,  &
                                   pracs, psacr, psmlt, psmltevp,      &
                                   prain, praut, pracw, prevp, pvapor, &
                                   pclw,  pladj, pcli,  pimlt, pihom,  &
                                   pidw,  piadj, pgraupel, pgaut,      &
                                   pgfr,  pgacw, pgaci, pgacr, pgacs,  &
                                   pgacip,pgacrp,pgacsp,pgwet, pdry,   &
                                   pgsub, pgdep, pgmlt, pgmltevp,      &
                                   qschg, qgchg


  REAL, DIMENSION( kts:kte )    :: qvsbar, rs0, viscmu, visc, diffwv,  &
                                   schmidt, xka

  REAL, DIMENSION( kts:kte )    :: vtr, vts, vtg,                      &
                                   vtrold, vtsold, vtgold, vtiold,     &
                                   xlambdar, xlambdas, xlambdag,       &
                                   olambdar, olambdas, olambdag

  REAL                          :: episp0k, dtb, odtb, pi, pio4,       &
                                   pio6, oxLf, xLvocp, xLfocp, consta, &
                                   constc, ocdrag, gambp4, gamdp4,     &
                                   gam4pt5, Cpor, oxmi, gambp3, gamdp3,&
                                   gambp6, gam3pt5, gam2pt75, gambp5o2,&
                                   gamdp5o2, cwoxlf, ocp, xni50, es

  REAL                          :: qvmin=1.e-20
  REAL                          :: gindex
  REAL                          :: temc1,save1,save2,xni50mx



  INTEGER                       :: min_q, max_q
  REAL                          :: t_del_tv, del_tv, flux, fluxin, fluxout ,tmpqrz
  LOGICAL                       :: notlast

  REAL                          :: tmp_tem, tmp_temcc 

  real :: liqconc, dis, beta, kappa, p0, xc, capn,rhocgs


      INTEGER:: NCALL=0

      IF (NCALL .EQ. 0) THEN



         xam_r = 3.14159*rhowater/6.
         xbm_r = 3.
         xmu_r = 0.
         xam_s = 3.14159*rhosnow/6.
         xbm_s = 3.
         xmu_s = 0.
         xam_g = 3.14159*rhograul/6.
         xbm_g = 3.
         xmu_g = 0.

         call radar_init
         NCALL = 1
      ENDIF













  if(flag_qndrop)then
     dis = 0.5 

     kappa=1.1d10

     beta = (1.0d0+3.0d0*dis**2)*(1.0d0+4.0d0*dis**2)*    &
         (1.0d0+5.0d0*dis**2)/((1.0d0+dis**2)*(1.0d0+2.0d0*dis**2))
  endif


   dtb=dt
   odtb=1./dtb
   pi=acos(-1.)
   pio4=acos(-1.)/4.
   pio6=acos(-1.)/6.
   ocp=1./cp
   oxLf=1./xLf
   xLvocp=xLv/cp
   xLfocp=xLf/cp
   consta=2115.0*0.01**(1-constb)
   constc=152.93*0.01**(1-constd)
   ocdrag=1./Cdrag

   episp0k=RH*ep2*1000.*svp1

   gambp4=ggamma(constb+4.)
   gamdp4=ggamma(constd+4.)
   gam4pt5=ggamma(4.5)
   Cpor=cp/Rair
   oxmi=1.0/xmi
   gambp3=ggamma(constb+3.)
   gamdp3=ggamma(constd+3.)
   gambp6=ggamma(constb+6)
   gam3pt5=ggamma(3.5)
   gam2pt75=ggamma(2.75)
   gambp5o2=ggamma((constb+5.)/2.)
   gamdp5o2=ggamma((constd+5.)/2.)
   cwoxlf=cw/xlf
   delta2=0.
   delta3=0.
   delta4=0.


















      obp4=1.0/(constb+4.0)
      bp3=constb+3.0
      bp5=constb+5.0
      bp6=constb+6.0
      odp4=1.0/(constd+4.0)
      dp3=constd+3.0
      dp5=constd+5.0
      dp5o2=0.5*(constd+5.0)

      do k=kts,kte
         oprez(k)=1./prez(k)
      enddo

      do k=kts,kte
         qlz(k)=amax1( 0.0,qlz(k) )
         qiz(k)=amax1( 0.0,qiz(k) )
         qvz(k)=amax1( qvmin,qvz(k) )
         qsz(k)=amax1( 0.0,qsz(k) )
         qrz(k)=amax1( 0.0,qrz(k) )
         qgz(k)=amax1( 0.0,qgz(k) )
         qndropz(k)=amax1( 0.0,qndropz(k) )     

         tem(k)=thz(k)*tothz(k)
         temcc(k)=tem(k)-273.15



         es=1000.*svp1*exp( svp2*temcc(k)/(tem(k)-svp3) )
         qswz(k)=ep2*es/(prez(k)-es)
         if (tem(k) .lt. 273.15 ) then


            es=1000.*svp1*exp( 21.8745584*(tem(k)-273.16)/(tem(k)-7.66) )
            qsiz(k)=ep2*es/(prez(k)-es)
            if (temcc(k) .lt. -40.0) qswz(k)=qsiz(k)
         else
            qsiz(k)=qswz(k)
         endif

         qvoqswz(k)=qvz(k)/qswz(k)
         qvoqsiz(k)=qvz(k)/qsiz(k)

         theiz(k)=thz(k)+(xlvocp*qvz(k)-xlfocp*qiz(k))/tothz(k)
      enddo
































































      do k=kts,kte
         psnow(k)=0.0
         psaut(k)=0.0
         psfw(k)=0.0
         psfi(k)=0.0
         praci(k)=0.0
         piacr(k)=0.0
         psaci(k)=0.0
         psacw(k)=0.0
         psdep(k)=0.0
         pssub(k)=0.0
         pracs(k)=0.0
         psacr(k)=0.0
         psmlt(k)=0.0
         psmltevp(k)=0.0

         prain(k)=0.0
         praut(k)=0.0
         pracw(k)=0.0
         prevp(k)=0.0

         pvapor(k)=0.0

         pclw(k)=0.0
         preclw(k)=0.0       
         pladj(k)=0.0

         pcli(k)=0.0
         pimlt(k)=0.0
         pihom(k)=0.0
         pidw(k)=0.0
         piadj(k)=0.0
      enddo




      do k=kts,kte
         pgraupel(k)=0.0
         pgaut(k)=0.0
         pgfr(k)=0.0
         pgacw(k)=0.0
         pgaci(k)=0.0
         pgacr(k)=0.0
         pgacs(k)=0.0
         pgacip(k)=0.0
         pgacrP(k)=0.0
         pgacsp(k)=0.0
         pgwet(k)=0.0
         pdry(k)=0.0
         pgsub(k)=0.0
         pgdep(k)=0.0
         pgmlt(k)=0.0
         pgmltevp(k)=0.0
         qschg(k)=0.
         qgchg(k)=0.
      enddo






      DO k=kts,kte
         rs0(k)=ep2*1000.*svp1/(prez(k)-1000.*svp1)
      END DO










    t_del_tv=0.
    del_tv=dtb
    notlast=.true.
    DO while (notlast)

      min_q=kte
      max_q=kts-1

      do k=kts,kte-1
         if (qrz(k) .gt. 1.0e-8) then
            min_q=min0(min_q,k)
            max_q=max0(max_q,k)
            tmp1=sqrt(pi*rhowater*xnor/rho(k)/qrz(k))
            tmp1=sqrt(tmp1)
            vtrold(k)=o6*consta*gambp4*sqrho(k)/tmp1**constb
            if (k .eq. 1) then
               del_tv=amin1(del_tv,0.9*(zz(k)-zsfc)/vtrold(k))
            else
               del_tv=amin1(del_tv,0.9*(zz(k)-zz(k-1))/vtrold(k))
            endif
         else
            vtrold(k)=0.
         endif
      enddo

      if (max_q .ge. min_q) then




         t_del_tv=t_del_tv+del_tv

         if ( t_del_tv .ge. dtb ) then
              notlast=.false.
              del_tv=dtb+del_tv-t_del_tv
         endif

         fluxin=0.
         do k=max_q,min_q,-1
            fluxout=rho(k)*vtrold(k)*qrz(k)
            flux=(fluxin-fluxout)/rho(k)/dzw(k)
            tmpqrz=qrz(k)
            qrz(k)=qrz(k)+del_tv*flux
            fluxin=fluxout
         enddo
         if (min_q .eq. 1) then
            pptrain=pptrain+fluxin*del_tv
         else
            qrz(min_q-1)=qrz(min_q-1)+del_tv*  &
                          fluxin/rho(min_q-1)/dzw(min_q-1)
         endif

      else
         notlast=.false.
      endif
    ENDDO




    t_del_tv=0.
    del_tv=dtb
    notlast=.true.

    DO while (notlast)

      min_q=kte
      max_q=kts-1

      do k=kts,kte-1
         if (qsz(k) .gt. 1.0e-8) then
            min_q=min0(min_q,k)
            max_q=max0(max_q,k)
            tmp1=sqrt(pi*rhosnow*xnos/rho(k)/qsz(k))
            tmp1=sqrt(tmp1)
            vtsold(k)=o6*constc*gamdp4*sqrho(k)/tmp1**constd
            if (k .eq. 1) then
               del_tv=amin1(del_tv,0.9*(zz(k)-zsfc)/vtsold(k))
            else
               del_tv=amin1(del_tv,0.9*(zz(k)-zz(k-1))/vtsold(k))
            endif
         else
            vtsold(k)=0.
         endif
      enddo

      if (max_q .ge. min_q) then





         t_del_tv=t_del_tv+del_tv

         if ( t_del_tv .ge. dtb ) then
              notlast=.false.
              del_tv=dtb+del_tv-t_del_tv
         endif

         fluxin=0.
         do k=max_q,min_q,-1
            fluxout=rho(k)*vtsold(k)*qsz(k)
            flux=(fluxin-fluxout)/rho(k)/dzw(k)
            qsz(k)=qsz(k)+del_tv*flux
            qsz(k)=amax1(0.,qsz(k))
            fluxin=fluxout
         enddo
         if (min_q .eq. 1) then
            pptsnow=pptsnow+fluxin*del_tv
         else
            qsz(min_q-1)=qsz(min_q-1)+del_tv*  &
                         fluxin/rho(min_q-1)/dzw(min_q-1)
         endif

      else
         notlast=.false.
      endif

    ENDDO



    t_del_tv=0.
    del_tv=dtb
    notlast=.true.

    DO while (notlast)

      min_q=kte
      max_q=kts-1

      do k=kts,kte-1
         if (qgz(k) .gt. 1.0e-8) then
            min_q=min0(min_q,k)
            max_q=max0(max_q,k)
            tmp1=sqrt(pi*rhograul*xnog/rho(k)/qgz(k))
            tmp1=sqrt(tmp1)
            term0=sqrt(4.*grav*rhograul*0.33334/rho(k)/cdrag)
            vtgold(k)=o6*gam4pt5*term0*sqrt(1./tmp1)
            if (k .eq. 1) then
               del_tv=amin1(del_tv,0.9*(zz(k)-zsfc)/vtgold(k))
            else
               del_tv=amin1(del_tv,0.9*(zz(k)-zz(k-1))/vtgold(k))
            endif
         else
            vtgold(k)=0.
         endif
      enddo

      if (max_q .ge. min_q) then





         t_del_tv=t_del_tv+del_tv

         if ( t_del_tv .ge. dtb ) then
              notlast=.false.
              del_tv=dtb+del_tv-t_del_tv
         endif


         fluxin=0.
         do k=max_q,min_q,-1
            fluxout=rho(k)*vtgold(k)*qgz(k)
            flux=(fluxin-fluxout)/rho(k)/dzw(k)
            qgz(k)=qgz(k)+del_tv*flux
            qgz(k)=amax1(0.,qgz(k))
            fluxin=fluxout
         enddo
         if (min_q .eq. 1) then
            pptgraul=pptgraul+fluxin*del_tv
         else
            qgz(min_q-1)=qgz(min_q-1)+del_tv*  &
                         fluxin/rho(min_q-1)/dzw(min_q-1)
         endif

      else
         notlast=.false.
      endif

   ENDDO




    t_del_tv=0.
    del_tv=dtb
    notlast=.true.

    DO while (notlast)

      min_q=kte
      max_q=kts-1

      do k=kts,kte-1
         if (qiz(k) .gt. 1.0e-8) then
            min_q=min0(min_q,k)
            max_q=max0(max_q,k)
            vtiold(k)= 3.29 * (rho(k)* qiz(k))** 0.16  
            if (k .eq. 1) then
               del_tv=amin1(del_tv,0.9*(zz(k)-zsfc)/vtiold(k))
            else
               del_tv=amin1(del_tv,0.9*(zz(k)-zz(k-1))/vtiold(k))
            endif
         else
            vtiold(k)=0.
         endif
      enddo

      if (max_q .ge. min_q) then





         t_del_tv=t_del_tv+del_tv

         if ( t_del_tv .ge. dtb ) then
              notlast=.false.
              del_tv=dtb+del_tv-t_del_tv
         endif

         fluxin=0.
         do k=max_q,min_q,-1
            fluxout=rho(k)*vtiold(k)*qiz(k)
            flux=(fluxin-fluxout)/rho(k)/dzw(k)
            qiz(k)=qiz(k)+del_tv*flux
            qiz(k)=amax1(0.,qiz(k))
            fluxin=fluxout
         enddo
         if (min_q .eq. 1) then
            pptice=pptice+fluxin*del_tv
         else
            qiz(min_q-1)=qiz(min_q-1)+del_tv*  &
                         fluxin/rho(min_q-1)/dzw(min_q-1)
         endif

      else
         notlast=.false.
      endif

   ENDDO
   do k=kts,kte-1                         
      precrz(k)=rho(k)*vtrold(k)*qrz(k)
      preciz(k)=rho(k)*vtiold(k)*qiz(k)
      precsz(k)=rho(k)*vtsold(k)*qsz(k)
      precgz(k)=rho(k)*vtgold(k)*qgz(k)
   enddo                                  
   precrz(kte)=0. 
   preciz(kte)=0. 
   precsz(kte)=0. 
   precgz(kte)=0. 
   



      DO 2000 k=kts,kte

         qvzodt(k)=amax1( 0.0,odtb*qvz(k) )
         qlzodt(k)=amax1( 0.0,odtb*qlz(k) )
         qizodt(k)=amax1( 0.0,odtb*qiz(k) )
         qszodt(k)=amax1( 0.0,odtb*qsz(k) )
         qrzodt(k)=amax1( 0.0,odtb*qrz(k) )
         qgzodt(k)=amax1( 0.0,odtb*qgz(k) )

















        tmp=qiz(k)+qlz(k)+qsz(k)+qrz(k)+qgz(k)*gindex
        if( qvz(k)+qlz(k)+qiz(k) .lt. qsiz(k)  &
            .and. tmp .eq. 0.0 ) go to 2000



        if (qrz(k) .gt. 1.0e-8) then
            tmp1=sqrt(pi*rhowater*xnor*orho(k)/qrz(k))
            xlambdar(k)=sqrt(tmp1)
            olambdar(k)=1.0/xlambdar(k)
            vtrold(k)=o6*consta*gambp4*sqrho(k)*olambdar(k)**constb
        else
            vtrold(k)=0.
            olambdar(k)=0.
        endif


        if (qrz(k) .gt. 1.0e-8) then
            tmp1=sqrt(pi*rhowater*xnor*orho(k)/qrz(k))
            xlambdar(k)=sqrt(tmp1)
            olambdar(k)=1.0/xlambdar(k)
            vtr(k)=o6*consta*gambp4*sqrho(k)*olambdar(k)**constb
        else
            vtr(k)=0.
            olambdar(k)=0.
        endif



        if (qsz(k) .gt. 1.0e-8) then
            tmp1=sqrt(pi*rhosnow*xnos*orho(k)/qsz(k))
            xlambdas(k)=sqrt(tmp1)
            olambdas(k)=1.0/xlambdas(k)
            vtsold(k)=o6*constc*gamdp4*sqrho(k)*olambdas(k)**constd
        else
            vtsold(k)=0.
            olambdas(k)=0.
        endif


        if (qsz(k) .gt. 1.0e-8) then
            tmp1=sqrt(pi*rhosnow*xnos*orho(k)/qsz(k))
            xlambdas(k)=sqrt(tmp1)
            olambdas(k)=1.0/xlambdas(k)
            vts(k)=o6*constc*gamdp4*sqrho(k)*olambdas(k)**constd
        else
            vts(k)=0.
            olambdas(k)=0.
        endif



        if (qgz(k) .gt. 1.0e-8) then
            tmp1=sqrt( pi*rhograul*xnog*orho(k)/qgz(k))
            xlambdag(k)=sqrt(tmp1)
            olambdag(k)=1.0/xlambdag(k)
            term0=sqrt(4.*grav*rhograul*0.33334*orho(k)*ocdrag)
            vtgold(k)=o6*gam4pt5*term0*sqrt(olambdag(k))
        else
            vtgold(k)=0.
            olambdag(k)=0.
        endif


        if (qgz(k) .gt. 1.0e-8) then
            tmp1=sqrt( pi*rhograul*xnog*orho(k)/qgz(k))
            xlambdag(k)=sqrt(tmp1)
            olambdag(k)=1.0/xlambdag(k)
            term0=sqrt(4.*grav*rhograul*0.33334*orho(k)*ocdrag)
            vtg(k)=o6*gam4pt5*term0*sqrt(olambdag(k))
        else
            vtg(k)=0.
            olambdag(k)=0.
        endif




















        viscmu(k)=avisc*tem(k)**1.5/(tem(k)+120.0)
        visc(k)=viscmu(k)*orho(k)
        diffwv(k)=adiffwv*tem(k)**1.81*oprez(k)
        schmidt(k)=visc(k)/diffwv(k)
        xka(k)=axka*viscmu(k)

        if (tem(k) .lt. 273.15) then












           alpha1=1.0e-3*exp( 0.025*temcc(k) )

           if(temcc(k) .lt. -20.0) then
             tmp1=-7.6+4.0*exp( -0.2443e-3*(abs(temcc(k))-20)**2.455 )
             qic=1.0e-3*exp(tmp1)*orho(k)
           else
             qic=qi0
           end if




           tmp1=odtb*(qiz(k)-qic)*(1.0-exp(-alpha1*dtb))
           psaut(k)=amax1( 0.0,tmp1 )











          if( qlz(k) .gt. 1.0e-10 ) then
            temc1=amax1(-30.99,temcc(k))

            a1=parama1( temc1 )
            a2=parama2( temc1 )
            tmp1=1.0-a2

            a1=a1*0.001**tmp1


            odtberg=(a1*tmp1)/(xmi50**tmp1-xmi40**tmp1)



            vti50=constc*di50**constd*sqrho(k)

            eiw=1.0
            save1=a1*xmi50**a2
            save2=0.25*pi*eiw*rho(k)*di50*di50*vti50

            tmp2=( save1 + save2*qlz(k) )




            xni50mx=qlzodt(k)/tmp2




            xni50=qiz(k)*( 1.0-exp(-dtb*odtberg) )/xmi50
            xni50=amin1(xni50,xni50mx)

            tmp3=odtb*tmp2/save2*( 1.0-exp(-save2*xni50*dtb) )
            psfw(k)=amin1( tmp3,qlzodt(k) )





























            tmp1=xni50*xmi50-psfw(k)
            psfi(k)=amin1(tmp1,qizodt(k))


          end if








          if(qrz(k) .le. 0.0) go to 1000







          eri=1.0




          save1=pio4*eri*xnor*consta*sqrho(k)
          tmp1=save1*gambp3*olambdar(k)**bp3
          praci(k)=qizodt(k)*( 1.0-exp(-tmp1*dtb) )








          tmp2=qiz(k)*save1*rho(k)*pio6*rhowater*gambp6*oxmi* &
                   olambdar(k)**bp6
          piacr(k)=amin1( tmp2,qrzodt(k) )


1000      continue

          if(qsz(k) .le. 0.0) go to 1200






          esi=exp( 0.025*temcc(k) )
          save1=pio4*xnos*constc*gamdp3*sqrho(k)* &
               olambdas(k)**dp3
          tmp1=esi*save1
          psaci(k)=qizodt(k)*( 1.0-exp(-tmp1*dtb) )








          esw=1.0
          tmp1=esw*save1
          psacw(k)=qlzodt(K)*( 1.0-exp(-tmp1*dtb) )









          tmpa=rvapor*xka(k)*tem(k)*tem(k)
          tmpb=xls*xls*rho(k)*qsiz(k)*diffwv(k)
          tmpc=tmpa*qsiz(k)*diffwv(k)
          abi=2.0*pi*(qvoqsiz(k)-1.0)*tmpc/(tmpa+tmpb)




          tmp1=constc*sqrho(k)*olambdas(k)**dp5/visc(k)
          tmp2=abi*xnos*( vf1s*olambdas(k)*olambdas(k)+ &
                    vf2s*schmidt(k)**0.33334*gamdp5o2*sqrt(tmp1) )
          tmp3=odtb*( qvz(k)-qsiz(k) )





          if( tmp3 .le. 0.0) then
            tmp2=amax1( tmp2,tmp3)
            pssub(k)=amin1(0.,amax1( tmp2,-qszodt(k) ))

            psdep(k)=0.0
          else
            psdep(k)=amin1( tmp2,tmp3 )
            pssub(k)=0.0
          end if





          if(qrz(k) .le. 0.0) go to 1200






          esr=1.0
          tmpa=olambdar(k)*olambdar(k)
          tmpb=olambdas(k)*olambdas(k)
          tmpc=olambdar(k)*olambdas(k)
          tmp1=pi*pi*esr*xnor*xnos*abs( vtr(k)-vts(k) )*orho(k)
          tmp2=tmpb*tmpb*olambdar(k)*(5.0*tmpb+2.0*tmpc+0.5*tmpa)
          tmp3=tmp1*rhosnow*tmp2
          pracs(k)=amin1( tmp3,qszodt(k) )



          tmp3=tmpa*tmpa*olambdas(k)*(5.0*tmpa+2.0*tmpc+0.5*tmpb)
          tmp4=tmp1*rhowater*tmp3
          psacr(k)=amin1( tmp4,qrzodt(k) )

1200      continue

        else





         if (qsz(k) .le. 0.0) go to 1400



            esw=1.0

            tmp1=esw*pio4*xnos*constc*gamdp3*sqrho(k)* &
                 olambdas(k)**dp3
            psacw(k)=qlzodt(k)*( 1.0-exp(-tmp1*dtb) )








            esr=1.0
            tmpa=olambdar(k)*olambdar(k)
            tmpb=olambdas(k)*olambdas(k)
            tmpc=olambdar(k)*olambdas(k)
            tmp1=pi*pi*esr*xnor*xnos*abs( vtr(k)-vts(k) )*orho(k)
            tmp2=tmpa*tmpa*olambdas(k)*(5.0*tmpa+2.0*tmpc+0.5*tmpb)
            tmp3=tmp1*rhowater*tmp2
            psacr(k)=amin1( tmp3,qrzodt(k) )




            delrs=rs0(k)-qvz(k)
            term1=2.0*pi*orho(k)*( xlv*diffwv(k)*rho(k)*delrs- &
                  xka(k)*temcc(k) )
            tmp1=constc*sqrho(k)*olambdas(k)**dp5/visc(k)
            tmp2=xnos*( vf1s*olambdas(k)*olambdas(k)+  &
                 vf2s*schmidt(k)**0.33334*gamdp5o2*sqrt(tmp1) )
            tmp3=term1*oxlf*tmp2-cwoxlf*temcc(k)*( psacw(k)+psacr(k) )
            tmp4=amin1(0.0,tmp3)
            psmlt(k)=amax1( tmp4,-qszodt(k) )





            tmpa=rvapor*xka(k)*tem(k)*tem(k)
            tmpb=xlv*xlv*rho(k)*qswz(k)*diffwv(k)
            tmpc=tmpa*qswz(k)*diffwv(k)
            tmpd=amin1( 0.0,(qvoqswz(k)-0.90)*qswz(k)*odtb )



            abr=2.0*pi*(qvoqswz(k)-0.90)*tmpc/(tmpa+tmpb)










            tmp1=constc*sqrho(k)*olambdas(k)**dp5/visc(k)
            tmp2=abr*xnos*( vf1s*olambdas(k)*olambdas(k)+  &
                 vf2s*schmidt(k)**0.33334*gamdp5o2*sqrt(tmp1) )
            tmp3=amin1(0.0,tmp2)
            tmp3=amax1( tmp3,tmpd )
            psmltevp(k)=amax1( tmp3,-qszodt(k) )
1400     continue

        end if








        if(flag_qndrop)then
           if( qndropz(k) >= 1. ) then

              rhocgs=rho(k)*1.e-3
              liqconc=rhocgs*qlz(k)   
              capn=1.0e-3*rhocgs*qndropz(k)   

              if(liqconc.gt.1.e-10)then
                 p0=(kappa*beta/capn)*(liqconc*liqconc*liqconc)
                 xc=9.7d-17*capn*sqrt(capn)/(liqconc*liqconc)

                 if(xc.lt.10.)then
                    praut(k)=(p0/rhocgs) * ( 0.5d0*(xc*xc+2*xc+2.0d0)* &
                         (1.0d0+xc)*exp(-2.0d0*xc) )
                 endif
              endif
           endif
        else






            araut=0.001



            tmp1=odtb*(qlz(k)-ql0)*( 1.0-exp(-araut*dtb) )
            praut(k)=amax1( 0.0,tmp1 )
        endif 




         erw=1.0




        tmp1=pio4*erw*xnor*consta*sqrho(k)* &
             gambp3*olambdar(k)**bp3
        pracw(k)=qlzodt(k)*( 1.0-exp(-tmp1*dtb) )







         tmpa=rvapor*xka(k)*tem(k)*tem(k)
         tmpb=xlv*xlv*rho(k)*qswz(k)*diffwv(k)
         tmpc=tmpa*qswz(k)*diffwv(k)





         tmpd = qswz(k)*xlv/(rvapor*tem(k)**2) 
         tmpd = min( 0., 0.9*odtb*(qvz(k) + qlz(k) - qswz(k)) &
                          / (1. + xlvocp * tmpd) )

         abr=2.0*pi*(qvoqswz(k)-1.0)*tmpc/(tmpa+tmpb)







         vf1r=0.78
         vf2r=0.31
         tmp1=consta*sqrho(k)*olambdar(k)**bp5/visc(k)
         tmp2=abr*xnor*( vf1r*olambdar(k)*olambdar(k)+  &
              vf2r*schmidt(k)**0.33334*gambp5o2*sqrt(tmp1) )
         tmp3=amin1( 0.0,tmp2 )
         tmp3=amax1( tmp3,tmpd )
         prevp(k)=amax1( tmp3,-qrzodt(k) )










      if (tem(k) .lt. 273.15) then












            alpha2=1.0e-3*exp(0.09*temcc(k))







            tmp1=odtb*(qsz(k)-qs0)*(1.0-exp(-alpha2*dtb))
            pgaut(k)=amax1( 0.0,tmp1 )








            if (qrz(k) .gt. 1.e-8 ) then
               Bp=100.
               Ap=0.66
               tmp1=olambdar(k)*olambdar(k)*olambdar(k)
               tmp2=20.*pi*pi*Bp*xnor*rhowater*orho(k)*  &
                    (exp(-Ap*temcc(k))-1.0)*tmp1*tmp1*olambdar(k)
               Pgfr(k)=amin1( tmp2,qrzodt(k) )
            else
               Pgfr(k)=0
            endif




         if (qgz(k) .eq. 0.0) goto 4000















         egw=1.0
         constg=sqrt(4.*grav*rhograul*0.33334*orho(k)*oCdrag)
         tmp1=pio4*xnog*gam3pt5*constg*olambdag(k)**3.5
         tmp2=qlz(k)*egw*tmp1
         Pgacw(k)=amin1( tmp2,qlzodt(k) )





         egi=0.1
         tmp2=qiz(k)*egi*tmp1
         pgaci(k)=amin1( tmp2,qizodt(k) )






         egs=exp(0.09*temcc(k))
         tmpa=olambdas(k)*olambdas(k)
         tmpb=olambdag(k)*olambdag(k)
         tmpc=olambdas(k)*olambdag(k)
         tmp1=pi*pi*xnos*xnog*abs( vts(k)-vtg(k) )*orho(k)
         tmp2=tmpa*tmpa*olambdag(k)*(5.0*tmpa+2.0*tmpc+0.5*tmpb)
         tmp3=tmp1*egs*rhosnow*tmp2
         Pgacs(k)=amin1( tmp3,qszodt(k) )







         egr=1.
         tmpa=olambdar(k)*olambdar(k)
         tmpb=olambdag(k)*olambdag(k)
         tmpc=olambdar(k)*olambdag(k)
         tmp1=pi*pi*xnor*xnog*abs( vtr(k)-vtg(k) )*orho(k)
         tmp2=tmpa*tmpa*olambdag(k)*(5.0*tmpa+2.0*tmpc+0.5*tmpb)
         tmp3=tmp1*egr*rhowater*tmp2
         pgacr(k)=amin1( tmp3,qrzodt(k) )




         Pdry(k)=Pgacw(k)+pgaci(k)+Pgacs(k)+pgacr(k)









         tmp2=10.*pgaci(k)
         pgacip(k)=amin1( tmp2,qizodt(k) )






         tmp3=Pgacs(k)*1.0/egs
         Pgacsp(k)=amin1( tmp3,qszodt(k) )







         IF(temcc(k).gt.-40.)THEN

             term0=constg*olambdag(k)**5.5/visc(k)








             delrs=rs0(k)-qvz(k)
             tmp0=1./(xlf+cw*temcc(k))
             tmp1=2.*pi*xnog*(rho(k)*xlv*diffwv(k)*delrs-xka(k)*  &
                  temcc(k))*orho(k)*tmp0
             constg2=vf1s*olambdag(k)*olambdag(k)+  &
                     vf2s*schmidt(k)**0.33334*gam2pt75*sqrt(term0)
             tmp3=tmp1*constg2+(Pgacip(k)+Pgacsp(k))*  &
                  (1-Ci*temcc(k)*tmp0)
             tmp3=amax1(0.0,tmp3)
             Pgwet(k)=amin1(tmp3,qlzodt(k)+qszodt(k)+qizodt(k) ) 







         if ( Pdry(k) .lt. Pgwet(k) ) then
            delta4=1.0
         else
            delta4=0.0
         endif
         ELSE
            delta4=1.0
         ENDIF









            Pgacrp(k)=Pgwet(k)-Pgacw(k)-Pgacip(k)-Pgacsp(k)










            tmpa=rvapor*xka(k)*tem(k)*tem(k)
            tmpb=xls*xls*rho(k)*qsiz(k)*diffwv(k)
            tmpc=tmpa*qsiz(k)*diffwv(k)
            abg=2.0*pi*(qvoqsiz(k)-1.0)*tmpc/(tmpa+tmpb)





            term0=constg*olambdag(k)**5.5/visc(k)
            constg2=vf1s*olambdag(k)*olambdag(k)+  &
                    vf2s*schmidt(k)**0.33334*gam2pt75*sqrt(term0)
            tmp2=abg*xnog*constg2
            pgdep(k)=amax1(0.0,tmp2)
            pgsub(k)=amin1(0.0,tmp2)
            pgsub(k)=amax1( pgsub(k),-qgzodt(k) )

 4000    continue
        else











            egw=1.0
            constg=sqrt(4.*grav*rhograul*0.33334*orho(k)*oCdrag)
            tmp1=pio4*xnog*gam3pt5*constg*olambdag(k)**3.5
            tmp2=qlz(k)*egw*tmp1
            Pgacw(k)=amin1( tmp2,qlzodt(k) )






            egr=1.
            tmpa=olambdar(k)*olambdar(k)
            tmpb=olambdag(k)*olambdag(k)
            tmpc=olambdar(k)*olambdag(k)
            tmp1=pi*pi*xnor*xnog*abs( vtr(k)-vtg(k) )*orho(k)
            tmp2=tmpa*tmpa*olambdag(k)*(5.0*tmpa+2.0*tmpc+0.5*tmpb)
            tmp3=tmp1*egr*rhowater*tmp2
            pgacr(k)=amin1( tmp3,qrzodt(k) )










            delrs=rs0(k)-qvz(k)
            term1=2.0*pi*orho(k)*( xlv*diffwv(k)*rho(k)*delrs- &
                  xka(k)*temcc(k) )
            term0=sqrt(4.*grav*rhograul*0.33334*orho(k)*ocdrag) &
                  *olambdag(k)**5.5/visc(k)

            constg2=vf1s*olambdag(k)*olambdag(k)+ &
                    vf2s*schmidt(k)**0.33334*gam2pt75*sqrt(term0)
            tmp2=xnog*constg2
            tmp3=term1*oxlf*tmp2-cwoxlf*temcc(k)*( pgacw(k)+pgacr(k) )
            tmp4=amin1(0.0,tmp3)
            pgmlt(k)=amax1( tmp4,-qgzodt(k) )








            tmpa=rvapor*xka(k)*tem(k)*tem(k)
            tmpb=xlv*xlv*rho(k)*qswz(k)*diffwv(k)
            tmpc=tmpa*qswz(k)*diffwv(k)
            tmpd=amin1( 0.0,(qvoqswz(k)-0.90)*qswz(k)*odtb )




            abg=2.0*pi*(qvoqswz(k)-0.90)*tmpc/(tmpa+tmpb)













            tmp2=abg*xnog*constg2
            tmp3=amin1(0.0,tmp2)
            tmp3=amax1( tmp3,tmpd )
            pgmltevp(k)=amax1( tmp3,-qgzodt(k) )






           egs=1.
           tmpa=olambdas(k)*olambdas(k)
           tmpb=olambdag(k)*olambdag(k)
           tmpc=olambdas(k)*olambdag(k)
           tmp1=pi*pi*xnos*xnog*abs( vts(k)-vtg(k) )*orho(k)
           tmp2=tmpa*tmpa*olambdag(k)*(5.0*tmpa+2.0*tmpc+0.5*tmpb)
           tmp3=tmp1*egs*rhosnow*tmp2
           Pgacs(k)=amin1( tmp3,qszodt(k) )

        endif



  900   continue








      if ( temcc(k) .lt. 0.0) then





           gdelta4=gindex*delta4
           g1sdelt4=gindex*(1.-delta4)




           tmp=psdep(k)+pgdep(k)*gindex
           if ( tmp .gt. qvzodt(k) ) then
              factor=qvzodt(k)/tmp
              psdep(k)=psdep(k)*factor
              pgdep(k)=pgdep(k)*factor*gindex
           end if



           tmp=praut(k)+psacw(k)+psfw(k)+pracw(k)+gindex*Pgacw(k)
           if ( tmp .gt. qlzodt(k) ) then
              factor=qlzodt(k)/tmp
              praut(k)=praut(k)*factor
              psacw(k)=psacw(k)*factor
              psfw(k)=psfw(k)*factor
              pracw(k)=pracw(k)*factor

              Pgacw(k)=Pgacw(k)*factor*gindex
           end if



           tmp=psaut(k)+psaci(k)+praci(k)+psfi(k)+Pgaci(k)*gdelta4 &
               +Pgacip(k)*g1sdelt4
           if (tmp .gt. qizodt(k) ) then
              factor=qizodt(k)/tmp
              psaut(k)=psaut(k)*factor
              psaci(k)=psaci(k)*factor
              praci(k)=praci(k)*factor
              psfi(k)=psfi(k)*factor

              Pgaci(k)=Pgaci(k)*factor*gdelta4
              Pgacip(k)=Pgacip(k)*factor*g1sdelt4
           endif



          tmp_r=piacr(k)+psacr(k)-prevp(k)-praut(k)-pracw(k) &
                +Pgfr(k)*gindex+Pgacr(k)*gdelta4 &
                +Pgacrp(k)*g1sdelt4
          if (tmp_r .gt. qrzodt(k) ) then
             factor=qrzodt(k)/tmp_r
             piacr(k)=piacr(k)*factor
             psacr(k)=psacr(k)*factor
             prevp(k)=prevp(k)*factor

             Pgfr(k)=Pgfr(k)*factor*gindex
             Pgacr(k)=Pgacr(k)*factor*gdelta4
             Pgacrp(k)=Pgacrp(k)*factor*g1sdelt4
          endif







          if (qrz(k) .lt. 1.0E-4 .and. qsz(k) .lt. 1.0E-4) then
             delta2=1.0
          else
             delta2=0.0
          endif









          if (qrz(k) .lt. 1.0e-4) then
             delta3=1.0
          else
             delta3=0.0
          endif





          if (gindex .eq. 0.) then
              delta2=1.0
              delta3=1.0
         endif




          tmp_s=-pssub(k)-(psaut(k)+psaci(k)+psacw(k)+psfw(k)+ &
                 psfi(k)+praci(k)*delta3+piacr(k)*delta3+ &
                 psdep(k))+Pgaut(k)*gindex+Pgacs(k)*gdelta4+ &
                 Pgacsp(k)*g1sdelt4+Pracs(k)*(1.-delta2)- &
                 Psacr(k)*delta2
          if ( tmp_s .gt. qszodt(k) ) then
             factor=qszodt(k)/tmp_s
             pssub(k)=pssub(k)*factor
             Pracs(k)=Pracs(k)*factor

             Pgaut(k)=Pgaut(k)*factor*gindex
             Pgacs(k)=Pgacs(k)*factor*gdelta4
             Pgacsp(k)=Pgacsp(k)*factor*g1sdelt4
          endif








           if(delta4.lt.0.5) then
             
             
             pgwet(k) = pgacrp(k) + pgacw(k) + pgacip(k) + pgacsp(k)
           end if
           tmp_g=-pgaut(k)-pgfr(k)-Pgacw(k)*delta4-Pgaci(k)*delta4  &
                 -Pgacr(k)*delta4-Pgacs(k)*delta4  &
                 -pgwet(k)*(1.-delta4)-pgsub(k)-pgdep(k)  &
                 -psacr(k)*(1-delta2)-Pracs(k)*(1-delta2)  &
                 -praci(k)*(1-delta3)-piacr(k)*(1-delta3)
           if (tmp_g .gt. qgzodt(k)) then
               factor=qgzodt(k)/tmp_g
               pgsub(k)=pgsub(k)*factor
           endif

  998      continue





         pvapor(k)=-pssub(k)-psdep(k)-prevp(k)-pgsub(k)*gindex &
                   -pgdep(k)*gindex
         qvz(k)=amax1( qvmin,qvz(k)+dtb*pvapor(k) )
         pclw(k)=-praut(k)-pracw(k)-psacw(k)-psfw(k)-pgacw(k)*gindex
	 if(flag_qndrop)then
           if( qlz(k) > 1e-20 ) &
              qndropz(k)=amax1( 0.0,qndropz(k)+dtb*pclw(k)*qndropz(k)/qlz(k) )  
	 endif
         qlz(k)=amax1( 0.0,qlz(k)+dtb*pclw(k) )
         pcli(k)=-psaut(k)-psfi(k)-psaci(k)-praci(k)-pgaci(k)*gdelta4 &
                 -Pgacip(k)*g1sdelt4
         qiz(k)=amax1( 0.0,qiz(k)+dtb*pcli(k) )
         tmp_r=piacr(k)+psacr(k)-prevp(k)-praut(k)-pracw(k) &
                +Pgfr(k)*gindex+Pgacr(k)*gdelta4 &
                +Pgacrp(k)*g1sdelt4
 232     format(i2,1x,6(f9.3,1x))
         prain(k)=-tmp_r
         qrz(k)=amax1( 0.0,qrz(k)+dtb*prain(k) )
         tmp_s=-pssub(k)-(psaut(k)+psaci(k)+psacw(k)+psfw(k)+  &
                psfi(k)+praci(k)*delta3+piacr(k)*delta3+  &
                psdep(k))+Pgaut(k)*gindex+Pgacs(k)*gdelta4+  &
                Pgacsp(k)*g1sdelt4+Pracs(k)*(1.-delta2)-  &
                Psacr(k)*delta2
         psnow(k)=-tmp_s
         qsz(k)=amax1( 0.0,qsz(k)+dtb*psnow(k) )
         qschg(k)=qschg(k)+psnow(k)
         qschg(k)=psnow(k)

         tmp_g=-pgaut(k)-pgfr(k)-Pgacw(k)*delta4-Pgaci(k)*delta4 &
               -Pgacr(k)*delta4-Pgacs(k)*delta4 &
               -pgwet(k)*(1.-delta4)-pgsub(k)-pgdep(k) &
               -psacr(k)*(1-delta2)-Pracs(k)*(1-delta2) &
               -praci(k)*(1-delta3)-piacr(k)*(1-delta3)
 252     format(i2,1x,6(f12.9,1x))
 262     format(i2,1x,7(f12.9,1x))
         pgraupel(k)=-tmp_g
         pgraupel(k)=pgraupel(k)*gindex
         qgz(k)=amax1( 0.0,qgz(k)+dtb*pgraupel(k))

         qgchg(k)=pgraupel(k)
         qgz(k)=qgz(k)*gindex

         tmp=ocp/tothz(k)*xLf*(qschg(k)+qgchg(k))
         theiz(k)=theiz(k)+dtb*tmp
         thz(k)=theiz(k)-(xLvocp*qvz(k)-xLfocp*qiz(k))/tothz(k)
         tem(k)=thz(k)*tothz(k)

         temcc(k)=tem(k)-273.15












      else



          tmp=praut(k)+psacw(k)+pracw(k)+pgacw(k)*gindex
          if ( tmp .gt. qlzodt(k) ) then
             factor=qlzodt(k)/tmp
             praut(k)=praut(k)*factor
             psacw(k)=psacw(k)*factor
             pracw(k)=pracw(k)*factor

             pgacw(k)=pgacw(k)*factor*gindex
          end if



          tmp_s=-(psmlt(k)+psmltevp(k))+Pgacs(k)*gindex
          if (tmp_s .gt. qszodt(k) ) then
             factor=qszodt(k)/tmp_s
             psmlt(k)=psmlt(k)*factor
             psmltevp(k)=psmltevp(k)*factor

             Pgacs(k)=Pgacs(k)*factor*gindex
          endif










            tmp_g=-pgmlt(k)-pgacs(k)-pgmltevp(k)
            if (tmp_g .gt. qgzodt(k)) then
              factor=qgzodt(k)/tmp_g
              pgmltevp(k)=pgmltevp(k)*factor
              pgmlt(k)=pgmlt(k)*factor
            endif

  997     continue




          tmp_r=-prevp(k)-(praut(k)+pracw(k)+psacw(k)-psmlt(k)) &
                +pgmlt(k)*gindex-pgacw(k)*gindex
          if (tmp_r .gt. qrzodt(k) ) then
             factor=qrzodt(k)/tmp_r
             prevp(k)=prevp(k)*factor
          endif






          pvapor(k)=-psmltevp(k)-prevp(k)-pgmltevp(k)*gindex
          qvz(k)=amax1( qvmin,qvz(k)+dtb*pvapor(k))
          pclw(k)=-praut(k)-pracw(k)-psacw(k)-pgacw(k)*gindex
          if(flag_qndrop)then
	     if( qlz(k) > 1e-20 ) &
               qndropz(k)=amax1( 0.0,qndropz(k)+dtb*pclw(k)*qndropz(k)/qlz(k) )  
	  endif
          qlz(k)=amax1( 0.0,qlz(k)+dtb*pclw(k) )
          pcli(k)=0.0
          qiz(k)=amax1( 0.0,qiz(k)+dtb*pcli(k) )
          tmp_r=-prevp(k)-(praut(k)+pracw(k)+psacw(k)-psmlt(k)) &
                +pgmlt(k)*gindex-pgacw(k)*gindex
 242      format(i2,1x,7(f9.6,1x))
          prain(k)=-tmp_r
          tmpqrz=qrz(k)
          qrz(k)=amax1( 0.0,qrz(k)+dtb*prain(k) )
          tmp_s=-(psmlt(k)+psmltevp(k))+Pgacs(k)*gindex
          psnow(k)=-tmp_s
          qsz(k)=amax1( 0.0,qsz(k)+dtb*psnow(k) )

          qschg(k)=psnow(k)


          tmp_g=-pgmlt(k)-pgacs(k)-pgmltevp(k)

 272      format(i2,1x,3(f12.9,1x))
          pgraupel(k)=-tmp_g*gindex
          qgz(k)=amax1( 0.0,qgz(k)+dtb*pgraupel(k))

          qgchg(k)=pgraupel(k)
          qgz(k)=qgz(k)*gindex

          tmp=ocp/tothz(k)*xLf*(qschg(k)+qgchg(k))
          theiz(k)=theiz(k)+dtb*tmp
          thz(k)=theiz(k)-(xLvocp*qvz(k)-xLfocp*qiz(k))/tothz(k)

          tem(k)=thz(k)*tothz(k)
          temcc(k)=tem(k)-273.15










      end if
      preclw(k)=pclw(k)        















      tmp_tem = tem(k)          

      if(qlz(k)+qiz(k).gt. 0.) then
         
         tmp_tem = tem(k) - xlvocp*qlz(k) - (xlvocp+xlfocp)*qiz(k)
      end if

     
      tmp_temcc = tmp_tem - 273.15

      
      
      if (tmp_temcc .lt. 0.) then
         es=1000.*svp1*exp( 21.8745584*(tmp_tem-273.16)/(tmp_tem-7.66) ) 
      else
         es=1000.*svp1*exp( svp2*tmp_temcc/(tmp_tem-svp3) ) 
      end if

      
      qvsbar(k)=ep2*es/(prez(k)-es)







         rsat=1.0+0.5*(50000.0-prez(k))*5.0e-5
         rsat=amax1(1.0,rsat)
         rsat=amin1(1.5,rsat)
         rsat=1.0
         if( qvz(k)+qlz(k)+qiz(k) .lt. rsat*qvsbar(k) ) then




          qvz(k)=qvz(k)+qlz(k)+qiz(k)
          qlz(k)=0.0
          qiz(k)=0.0

          thz(k)=theiz(k)-(xLvocp*qvz(k)-xLfocp*qiz(k))/tothz(k)
          tem(k)=thz(k)*tothz(k)
          temcc(k)=tem(k)-273.15

          go to 1800

        else




          pladj(k)=qlz(k)
          piadj(k)=qiz(k)


          CALL satadj(qvz, qlz, qiz, prez, theiz, thz, tothz, kts, kte, &
                      k, xLvocp, xLfocp, episp0k, EP2,SVP1,SVP2,SVP3,SVPT0 )


          pladj(k)=odtb*(qlz(k)-pladj(k))
          piadj(k)=odtb*(qiz(k)-piadj(k))

          pclw(k)=pclw(k)+pladj(k)
          pcli(k)=pcli(k)+piadj(k)
          pvapor(k)=pvapor(k)-( pladj(k)+piadj(k) )

          thz(k)=theiz(k)-(xLvocp*qvz(k)-xLfocp*qiz(k))/tothz(k)
          tem(k)=thz(k)*tothz(k)

          temcc(k)=tem(k)-273.15



          es=1000.*svp1*exp( svp2*temcc(k)/(tem(k)-svp3) )
          qswz(k)=ep2*es/(prez(k)-es)
          if (tem(k) .lt. 273.15 ) then


             es=1000.*svp1*exp( 21.8745584*(tem(k)-273.16)/(tem(k)-7.66) )
             qsiz(k)=ep2*es/(prez(k)-es)
             if (temcc(k) .lt. -40.0) qswz(k)=qsiz(k)
          else
             qsiz(k)=qswz(k)
          endif
          qlpqi=qlz(k)+qiz(k)
          if ( qlpqi .eq. 0.0 ) then
             qvsbar(k)=qsiz(k)
          else
             qvsbar(k)=( qiz(k)*qsiz(k)+qlz(k)*qswz(k) )/qlpqi
          endif

        end if





        qlpqi=qlz(k)+qiz(k)
        if(qlpqi .le. 0.0) go to 1800




        if(temcc(k) .lt. -40.0) pihom(k)=qlz(k)*odtb



        if(temcc(k) .gt. 0.0) pimlt(k)=qiz(k)*odtb




        if(temcc(k) .lt. 0.0 .and. temcc(k) .gt. -31.0) then



          a1=parama1( temcc(k) )
          a2=parama2( temcc(k) )

          a1=a1*0.001**(1.0-a2)
          xnin=xni0*exp(-bni*temcc(k))
          pidw(k)=xnin*orho(k)*(a1*xmnin**a2)
        end if

        pcli(k)=pcli(k)+pihom(k)-pimlt(k)+pidw(k)
        pclw(k)=pclw(k)-pihom(k)+pimlt(k)-pidw(k)
        qlz(k)=amax1( 0.0,qlz(k)+dtb*(-pihom(k)+pimlt(k)-pidw(k)) )
        qiz(k)=amax1( 0.0,qiz(k)+dtb*(pihom(k)-pimlt(k)+pidw(k)) )


        CALL satadj(qvz, qlz, qiz, prez, theiz, thz, tothz, kts, kte, &
                    k, xLvocp, xLfocp, episp0k ,EP2,SVP1,SVP2,SVP3,SVPT0)

        thz(k)=theiz(k)-(xLvocp*qvz(k)-xLfocp*qiz(k))/tothz(k)
        tem(k)=thz(k)*tothz(k)

        temcc(k)=tem(k)-273.15



        es=1000.*svp1*exp( svp2*temcc(k)/(tem(k)-svp3) )
        qswz(k)=ep2*es/(prez(k)-es)

        if (tem(k) .lt. 273.15 ) then


           es=1000.*svp1*exp( 21.8745584*(tem(k)-273.16)/(tem(k)-7.66) )
           qsiz(k)=ep2*es/(prez(k)-es)
           if (temcc(k) .lt. -40.0) qswz(k)=qsiz(k)
        else
           qsiz(k)=qswz(k)
        endif
        qlpqi=qlz(k)+qiz(k)
        if ( qlpqi .eq. 0.0 ) then
           qvsbar(k)=qsiz(k)
        else
           qvsbar(k)=( qiz(k)*qsiz(k)+qlz(k)*qswz(k) )/qlpqi
        endif

1800  continue






2000  continue



























































































































      do k=kts+1,kte
         if ( qvz(k) .lt. qvmin ) then
            qlz(k)=0.0
            qiz(k)=0.0
            qvz(k)=amax1( qvmin,qvz(k)+qlz(k)+qiz(k) )
         end if
      enddo

  END SUBROUTINE clphy1d





      SUBROUTINE satadj(qvz, qlz, qiz, prez, theiz, thz, tothz,      &
                        kts, kte, k, xLvocp, xLfocp, episp0k, EP2,SVP1,SVP2,SVP3,SVPT0)

   IMPLICIT NONE














  INTEGER, INTENT(IN   )             :: kts, kte, k

  REAL,      DIMENSION( kts:kte ),                                   &
                       INTENT(INOUT) :: qvz, qlz, qiz

  REAL,      DIMENSION( kts:kte ),                                   &
                       INTENT(IN   ) :: prez, theiz, tothz

  REAL,     INTENT(IN   )            :: xLvocp, xLfocp, episp0k
  REAL,     INTENT(IN   )            :: EP2,SVP1,SVP2,SVP3,SVPT0



  INTEGER                            :: n

  REAL, DIMENSION( kts:kte )         :: thz, tem, temcc, qsiz,       &
                                        qswz, qvsbar

  REAL ::   qsat, qlpqi, ratql, t0, t1, tmp1, ratqi, tsat, absft,    &
            denom1, denom2, dqvsbar, ftsat, dftsat, qpz,             &
            gindex, es

  REAL ::   tem_noliqice, qsat_noliqice 



      thz(k)=theiz(k)-(xLvocp*qvz(k)-xLfocp*qiz(k))/tothz(k)

      tem(k)=tothz(k)*thz(k)


      tem_noliqice = tem(k) - xlvocp*qlz(k) - (xLvocp+xLfocp)*qiz(k)

      if (tem_noliqice .gt. 273.15) then
         es=1000.*svp1*exp( svp2*(tem_noliqice-svpt0)/(tem_noliqice-svp3) )
         qsat_noliqice=ep2*es/(prez(k)-es)
      else
        qsat_noliqice=episp0k/prez(k)*  &
             exp( 21.8745584*(tem_noliqice-273.15)/(tem_noliqice-7.66) )
      end if

      qpz=qvz(k)+qlz(k)+qiz(k)
      if (qpz .lt. qsat_noliqice) then
         qvz(k)=qpz
         qiz(k)=0.0
         qlz(k)=0.0
         go to 400
      end if
      qlpqi=qlz(k)+qiz(k)
      if( qlpqi .ge. 1.0e-5) then
        ratql=qlz(k)/qlpqi
        ratqi=qiz(k)/qlpqi
      else
        t0=273.15

        t1=248.15
        tmp1=( t0-tem(k) )/(t0-t1)
        tmp1=amin1(1.0,tmp1)
        tmp1=amax1(0.0,tmp1)
        ratqi=tmp1
        ratql=1.0-tmp1
      end if












      tsat=tem(k)
      absft=1.0

      do 200 n=1,20
         denom1=1.0/(tsat-svp3)
         denom2=1.0/(tsat-7.66)


         es=1000.*svp1*exp( svp2*denom1*(tsat-svpt0) )
         qswz(k)=ep2*es/(prez(k)-es)
         if (tem(k) .lt. 273.15) then


            es=1000.*svp1*exp( 21.8745584*denom2*(tsat-273.15) )
            qsiz(k)=ep2*es/(prez(k)-es)
            if (tem(k) .lt. 233.15) qswz(k)=qsiz(k)
         else
            qsiz(k)=qswz(k)
         endif
         qvsbar(k)=ratql*qswz(k)+ratqi*qsiz(k)


         if( absft .lt. 0.01 ) go to 300

         dqvsbar=ratql*qswz(k)*svp2*243.5*denom1*denom1+  &
                 ratqi*qsiz(k)*21.8745584*265.5*denom2*denom2
         ftsat=tsat+(xlvocp+ratqi*xlfocp)*qvsbar(k)-  &
               tothz(k)*theiz(k)-xlfocp*ratqi*(qvz(k)+qlz(k)+qiz(k))
         dftsat=1.0+(xlvocp+ratqi*xlfocp)*dqvsbar
         tsat=tsat-ftsat/dftsat
         absft=abs(ftsat)

200   continue
9020  format(1x,'point can not converge, absft,n=',e12.5,i5)

300   continue
      if( qpz .gt. qvsbar(k) ) then
        qvz(k)=qvsbar(k)
        qiz(k)=ratqi*( qpz-qvz(k) )
        qlz(k)=ratql*( qpz-qvz(k) )
      else
        qvz(k)=qpz
        qiz(k)=0.0
        qlz(k)=0.0
      end if
 400  continue

   END SUBROUTINE satadj



   REAL FUNCTION parama1(temp)

   IMPLICIT NONE





      REAL, INTENT (IN   )   :: temp
      REAL, DIMENSION(32)    :: a1
      INTEGER                :: i1, i1p1
      REAL                   :: ratio

      data a1/0.100e-10,0.7939e-7,0.7841e-6,0.3369e-5,0.4336e-5, &
              0.5285e-5,0.3728e-5,0.1852e-5,0.2991e-6,0.4248e-6, &
              0.7434e-6,0.1812e-5,0.4394e-5,0.9145e-5,0.1725e-4, &
              0.3348e-4,0.1725e-4,0.9175e-5,0.4412e-5,0.2252e-5, &
              0.9115e-6,0.4876e-6,0.3473e-6,0.4758e-6,0.6306e-6, &
              0.8573e-6,0.7868e-6,0.7192e-6,0.6513e-6,0.5956e-6, &
              0.5333e-6,0.4834e-6/

      i1=int(-temp)+1
      i1p1=i1+1
      ratio=-(temp)-float(i1-1)
      parama1=a1(i1)+ratio*( a1(i1p1)-a1(i1) )

   END FUNCTION parama1


   REAL FUNCTION parama2(temp)

   IMPLICIT NONE





      REAL, INTENT (IN   )   :: temp
      REAL, DIMENSION(32)    :: a2
      INTEGER                :: i1, i1p1
      REAL                   :: ratio

      data a2/0.0100,0.4006,0.4831,0.5320,0.5307,0.5319,0.5249, &
              0.4888,0.3849,0.4047,0.4318,0.4771,0.5183,0.5463, &
              0.5651,0.5813,0.5655,0.5478,0.5203,0.4906,0.4447, &
              0.4126,0.3960,0.4149,0.4320,0.4506,0.4483,0.4460, &
              0.4433,0.4413,0.4382,0.4361/
      i1=int(-temp)+1
      i1p1=i1+1
      ratio=-(temp)-float(i1-1)
      parama2=a2(i1)+ratio*( a2(i1p1)-a2(i1) )

   END FUNCTION parama2


   REAL FUNCTION ggamma(X)

   IMPLICIT NONE

      REAL, INTENT(IN   ) :: x
      REAL, DIMENSION(8)  :: B
      INTEGER             ::j, K1
      REAL                ::PF, G1TO2 ,TEMP

      DATA B/-.577191652,.988205891,-.897056937,.918206857,  &
             -.756704078,.482199394,-.193527818,.035868343/

      PF=1.
      TEMP=X
      DO 10 J=1,200
      IF (TEMP .LE. 2) GO TO 20
      TEMP=TEMP-1.
   10 PF=PF*TEMP
  100 FORMAT(//,5X,'module_mp_lin: INPUT TO GAMMA FUNCTION TOO LARGE, X=',E12.5)
      WRITE(wrf_err_message,100)X
      CALL wrf_error_fatal3("<stdin>",2747,&
wrf_err_message)
   20 G1TO2=1.
      TEMP=TEMP - 1.
      DO 30 K1=1,8
   30 G1TO2=G1TO2 + B(K1)*TEMP**K1
      ggamma=PF*G1TO2

      END FUNCTION ggamma




      subroutine refl10cm_lin (qv1d, qr1d, qs1d, qg1d,                  &
                       t1d, p1d, dBZ, kts, kte, ii, jj)

      IMPLICIT NONE


      INTEGER, INTENT(IN):: kts, kte, ii, jj
      REAL, DIMENSION(kts:kte), INTENT(IN)::                            &
                      qv1d, qr1d, qs1d, qg1d, t1d, p1d
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: dBZ


      REAL, DIMENSION(kts:kte):: temp, pres, qv, rho
      REAL, DIMENSION(kts:kte):: rr, rs, rg

      DOUBLE PRECISION, DIMENSION(kts:kte):: ilamr, ilams, ilamg
      DOUBLE PRECISION, DIMENSION(kts:kte):: N0_r, N0_s, N0_g
      DOUBLE PRECISION:: lamr, lams, lamg
      LOGICAL, DIMENSION(kts:kte):: L_qr, L_qs, L_qg

      REAL, DIMENSION(kts:kte):: ze_rain, ze_snow, ze_graupel
      DOUBLE PRECISION:: fmelt_s, fmelt_g

      INTEGER:: i, k, k_0, kbot, n
      LOGICAL:: melti

      DOUBLE PRECISION:: cback, x, eta, f_d
      REAL, PARAMETER:: R=287.
      REAL, PARAMETER:: PIx=3.1415926536



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
            N0_r(k) = xnor
            lamr = (xam_r*xcrg(3)*N0_r(k)/rr(k))**(1./xcre(1))
            ilamr(k) = 1./lamr
            L_qr(k) = .true.
         else
            rr(k) = 1.E-12
            L_qr(k) = .false.
         endif

         if (qs1d(k) .gt. 1.E-9) then
            rs(k) = qs1d(k)*rho(k)
            N0_s(k) = xnos
            lams = (xam_s*xcsg(3)*N0_s(k)/rs(k))**(1./xcse(1))
            ilams(k) = 1./lams
            L_qs(k) = .true.
         else
            rs(k) = 1.E-12
            L_qs(k) = .false.
         endif

         if (qg1d(k) .gt. 1.E-9) then
            rg(k) = qg1d(k)*rho(k)
            N0_g(k) = xnog
            lamg = (xam_g*xcgg(3)*N0_g(k)/rg(k))**(1./xcge(1))
            ilamg(k) = 1./lamg
            L_qg(k) = .true.
         else
            rg(k) = 1.E-12
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
         if (L_qs(k)) ze_snow(k) = (0.176/0.93) * (6.0/PIx)*(6.0/PIx)     &
                                 * (xam_s/900.0)*(xam_s/900.0)          &
                                 * N0_s(k)*xcsg(4)*ilams(k)**xcse(4)
         if (L_qg(k)) ze_graupel(k) = (0.176/0.93) * (6.0/PIx)*(6.0/PIx)  &
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

      end subroutine refl10cm_lin


END MODULE module_mp_lin
