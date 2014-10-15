
















      MODULE module_mp_sbu_ylin
      USE    module_wrf_error


      REAL, PARAMETER, PRIVATE :: RH = 1.0
      REAL, PARAMETER, PRIVATE :: xnor = 8.0e6
      REAL, PARAMETER, PRIVATE :: Nt_c = 10.E6

      REAL, PARAMETER, PRIVATE :: Rvapor = 461.5
      REAL, PARAMETER, PRIVATE :: oRv    = 1./Rvapor
      REAL, PARAMETER, PRIVATE :: Rair   = 287.04
      REAL, PARAMETER, PRIVATE :: Cp     = 1004.0
      REAL, PARAMETER, PRIVATE :: grav   = 9.81
      REAL, PARAMETER, PRIVATE :: rhowater = 1000.0
      REAL, PARAMETER, PRIVATE :: rhosnow  = 100.0

      REAL, PARAMETER, PRIVATE :: SVP1=0.6112
      REAL, PARAMETER, PRIVATE :: SVP2=17.67
      REAL, PARAMETER, PRIVATE :: SVP3=29.65
      REAL, PARAMETER, PRIVATE :: SVPT0=273.15
      REAL, PARAMETER, PRIVATE :: EP1=Rvapor/Rair-1.
      REAL, PARAMETER, PRIVATE :: EP2=Rair/Rvapor

      REAL, PARAMETER, PRIVATE :: XLS = 2.834E6
      REAL, PARAMETER, PRIVATE :: XLV = 2.5E6
      REAL, PARAMETER, PRIVATE :: XLF = XLS - XLV


      REAL, PARAMETER, PRIVATE ::                           &
             qi0 = 1.0e-3,                                  &   
             xmi50 = 4.8e-10, xmi40 = 2.46e-10,             &
             xni0 = 1.0e-2, xmnin = 1.05e-18, bni = 0.5,    &
             di50 = 1.0e-4, xmi = 4.19e-13,                 &   
             bv_r = 0.8, bv_i = 0.25,                       &
             o6 = 1./6.,  cdrag = 0.6,                      &
             avisc = 1.49628e-6, adiffwv = 8.7602e-5,       &
             axka = 1.4132e3, cw = 4.187e3,  ci = 2.093e3
CONTAINS





      SUBROUTINE sbu_ylin(th                                       &
                      ,qv, ql, qr                                  &
                      ,qi, qs, Ri3D                                &
                      ,rho, pii, p                                 &
                      ,dt_in                                       &
                      ,z,ht, dz8w                                  &
                      ,RAINNC, RAINNCV                             &
                      ,ids,ide, jds,jde, kds,kde                   &
                      ,ims,ime, jms,jme, kms,kme                   &
                      ,its,ite, jts,jte, kts,kte                   &
                                                                   )

      IMPLICIT NONE



     INTEGER,   INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte

     REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
        INTENT(INOUT) ::                                          &
                                                              th, &
                                                              qv, &
                                                           qi,ql, &
                                                           qs,qr



     REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                &
        INTENT(INOUT) :: Ri3D



     REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
        INTENT(IN   ) ::                                          &
                                                             rho, &
                                                             pii, &
                                                             z,p, &
                                                            dz8w


     REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN) ::       ht
     REAL, INTENT(IN   ) ::                                   dt_in  

     REAL, DIMENSION( ims:ime , jms:jme ),                           &
        INTENT(INOUT) ::                                  RAINNC, &
                                                          RAINNCV



     INTEGER             ::                            min_q, max_q

     REAL, DIMENSION( its:ite , jts:jte )                            &
                               ::        rain, snow,ice

     REAL, DIMENSION( kts:kte )   ::                  qvz, qlz, qrz, &
                                                      qiz, qsz, qgz, &
                                                                thz, &
                                                        tothz, rhoz, &
                                                      orhoz, sqrhoz, &
                                                           prez, zz, &
                                                        dzw


     REAL, DIMENSION( kts:kte ) :: riz


     REAL    ::         dt, pptice, pptrain, pptsnow, pptgraul, rhoe_s

     INTEGER ::               i,j,k

      dt=dt_in
      rhoe_s=1.29
  
      j_loop:  DO j = jts, jte
      i_loop:  DO i = its, ite



      DO k = kts, kte
       qvz(k)=qv(i,k,j)
       qlz(k)=ql(i,k,j)
       qrz(k)=qr(i,k,j)
       qiz(k)=qi(i,k,j)
       qsz(k)=qs(i,k,j)
       thz(k)=th(i,k,j)
       rhoz(k)=rho(i,k,j)
       orhoz(k)=1./rhoz(k)
       prez(k)=p(i,k,j)


       sqrhoz(k)=1.0
       tothz(k)=pii(i,k,j)
       zz(k)=z(i,k,j)
       dzw(k)=dz8w(i,k,j)
      END DO

      pptrain=0.
      pptsnow=0.
      pptice =0.



     CALL clphy1d_ylin(  dt, qvz, qlz, qrz, qiz, qsz,              &
                         thz, tothz, rhoz, orhoz, sqrhoz,          &
                         prez, zz, dzw, ht(I,J),                   &
                         pptrain, pptsnow, pptice,                 &
                         kts, kte, i, j, riz                       )






      rain(i,j)= pptrain
      snow(i,j)= pptsnow
      ice(i,j) = pptice

      RAINNCV(i,j)= pptrain + pptsnow + pptice
      RAINNC(i,j) = RAINNC(i,j) + pptrain + pptsnow + pptice




      DO k = kts, kte
       qv(i,k,j)=qvz(k)
       ql(i,k,j)=qlz(k)
       qr(i,k,j)=qrz(k)
       th(i,k,j)=thz(k)
       qi(i,k,j)=qiz(k)
       qs(i,k,j)=qsz(k)
       ri3d(i,k,j)=riz(k)
      END DO

      ENDDO i_loop
      ENDDO j_loop

      END SUBROUTINE sbu_ylin



      SUBROUTINE clphy1d_ylin(dt, qvz, qlz, qrz, qiz, qsz,             &
                      thz, tothz, rho, orho, sqrho,                    &
                      prez, zz, dzw, zsfc, pptrain, pptsnow,pptice,    &
                      kts, kte, i, j,riz                               )

      IMPLICIT NONE
















































     INTEGER, INTENT(IN   )               :: kts, kte, i, j

     REAL,    DIMENSION( kts:kte ),                                   &
           INTENT(INOUT)               :: qvz, qlz, qrz, qiz, qsz,    &
                                          thz

     REAL,    DIMENSION( kts:kte ),                                   &
           INTENT(IN   )               :: tothz, rho, orho, sqrho,    &
                                          prez, zz, dzw


     REAL,    INTENT(INOUT)               :: pptrain, pptsnow, pptice

     REAL,    INTENT(IN   )               :: dt, zsfc



     REAL                                :: obp4, bp3, bp5, bp6, odp4,  &
                                            dp3, dp5, dp5o2



     REAL                              :: tmp, tmp0, tmp1, tmp2,tmp3,  &
                                          tmp4, tmpa,tmpb,tmpc,tmpd,alpha1,  &
                                          qic, abi,abr, abg, odtberg,  &
                                          vti50,eiw,eri,esi,esr, esw,  &
                                          erw,delrs,term0,term1,       &
                                          Ap, Bp,                      &
                                          factor, tmp_r, tmp_s,tmp_g,  &
                                          qlpqi, rsat, a1, a2, xnin


     REAL, DIMENSION( kts:kte )    ::  oprez, tem, temcc, theiz, qswz,    &
                                       qsiz, qvoqswz, qvoqsiz, qvzodt,    &
                                       qlzodt, qizodt, qszodt, qrzodt



     REAL, DIMENSION( kts:kte )    :: psnow, psaut, psfw,  psfi,  praci,  &
                                      piacr, psaci, psacw, psdep, pssub,  &
                                      pracs, psacr, psmlt, psmltevp,      &
                                      prain, praut, pracw, prevp, pvapor, &
                                      pclw,  pladj, pcli,  pimlt, pihom,  &
                                      pidw,  piadj, pgfr,                 &
                                      qschg


     REAL, DIMENSION( kts:kte )    :: qvsbar, rs0, viscmu, visc, diffwv,  &
                                      schmidt, xka


     REAL, DIMENSION( kts:kte ):: ab_s,ab_r,ab_riming,lamc 
     REAL, DIMENSION( kts:kte ):: cap_s       
 
     REAL, PARAMETER :: vf1s = 0.65, vf2s = 0.44, vf1r =0.78 , vf2r = 0.31 

     REAL, PARAMETER :: am_c1=0.004, am_c2= 6e-5,    am_c3=0.15
     REAL, PARAMETER :: bm_c1=1.85,  bm_c2= 0.003,   bm_c3=1.25
     REAL, PARAMETER :: aa_c1=1.28,  aa_c2= -0.012,  aa_c3=-0.6
     REAL, PARAMETER :: ba_c1=1.5,   ba_c2= 0.0075,  ba_c3=0.5

     REAL, PARAMETER :: best_a=1.08 ,  best_b = 0.499
     REAL, DIMENSION(kts:kte):: am_s,bm_s,av_s,bv_s,Ri,N0_s,tmp_ss,lams  
     REAL, DIMENSION(kts:kte):: aa_s,ba_s,tmp_sa  
     REAL, PARAMETER :: mu_s=0.,mu_i=0.,mu_r=0.

     REAL :: tc0, disp, Dc_liu, eta, mu_c, R6c      



     REAL, DIMENSION(kts:kte) :: Riz

     REAL, DIMENSION( kts:kte )    :: vtr, vts,                   &
                                      vtrold, vtsold, vtiold,     &
                                      xlambdar, xlambdas,            &
                                      olambdar, olambdas

     REAL                          :: episp0k, dtb, odtb, pi, pio4,       &
                                      pio6, oxLf, xLvocp, xLfocp, av_r,   &
                                      av_i, ocdrag, gambp4, gamdp4,       &
                                      gam4pt5, Cpor, oxmi, gambp3, gamdp3,&
                                      gambp6, gam3pt5, gam2pt75, gambp5o2,&
                                      gamdp5o2, cwoxlf, ocp, xni50, es

     REAL                          :: qvmin=1.e-20
     REAL                          :: temc1,save1,save2,xni50mx



     INTEGER                       :: min_q, max_q, max_ri_k, k
     REAL                          :: max_ri
     REAL                          :: t_del_tv, del_tv, flux, fluxin, fluxout ,tmpqrz
     LOGICAL                       :: notlast

      mu_c = AMIN1(15., (1000.E6/Nt_c + 2.))
      R6c  = 10.0E-6      
      dtb=dt
      odtb=1./dtb
      pi  =acos(-1.)
      pio4=acos(-1.)/4.
      pio6=acos(-1.)/6.
      ocp=1./cp
      oxLf=1./xLf
      xLvocp=xLv/cp
      xLfocp=xLf/cp
      Cpor=cp/Rair
      oxmi=1.0/xmi
      cwoxlf=cw/xlf 
      av_r=2115.0*0.01**(1-bv_r)
      av_i=152.93*0.01**(1-bv_i)
      ocdrag=1./Cdrag
      episp0k=RH*ep2*1000.*svp1

      gambp4=ggamma(bv_r+4.)
      gamdp4=ggamma(bv_i+4.)
      gambp3=ggamma(bv_r+3.)
      gambp6=ggamma(bv_r+6)
      gambp5o2=ggamma((bv_r+5.)/2.)
      gamdp5o2=ggamma((bv_i+5.)/2.)















      obp4=1.0/(bv_r+4.0)
      bp3=bv_r+3.0
      bp5=bv_r+5.0
      bp6=bv_r+6.0
      odp4=1.0/(bv_i+4.0)
      dp3=bv_i+3.0
      dp5=bv_i+5.0
      dp5o2=0.5*(bv_i+5.0)

      do k=kts,kte
         oprez(k)=1./prez(k)
         qlz(k)=amax1( 0.0,qlz(k) )
         qiz(k)=amax1( 0.0,qiz(k) )
         qvz(k)=amax1( qvmin,qvz(k) )
         qsz(k)=amax1( 0.0,qsz(k) )
         qrz(k)=amax1( 0.0,qrz(k) )
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
         qvzodt(k)=amax1( 0.0,odtb*qvz(k) )
         qlzodt(k)=amax1( 0.0,odtb*qlz(k) )
         qizodt(k)=amax1( 0.0,odtb*qiz(k) )
         qszodt(k)=amax1( 0.0,odtb*qsz(k) )
         qrzodt(k)=amax1( 0.0,odtb*qrz(k) )
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
         pgfr(k)=0.0

         pvapor(k)=0.0

         pclw(k)=0.0
         pladj(k)=0.0

         pcli(k)=0.0
         pimlt(k)=0.0
         pihom(k)=0.0
         pidw(k)=0.0
         piadj(k)=0.0

         qschg(k)=0.

      enddo



















      DO k=kts,kte
        viscmu(k)=avisc*tem(k)**1.5/(tem(k)+120.0)
        visc(k)=viscmu(k)*orho(k)
        diffwv(k)=adiffwv*tem(k)**1.81*oprez(k)
        schmidt(k)=visc(k)/diffwv(k)
        xka(k)=axka*viscmu(k)
        rs0(k)=ep2*1000.*svp1/(prez(k)-1000.*svp1)
      END DO







       do k = kts, kte
         tc0   = tem(k)-273.15       
        if (rho(k)*qlz(k) .gt. 1e-5 .AND. rho(k)*qsz(k) .gt. 1e-5) then 
         Ri(k) = 1.0/(1.0+6e-5/(rho(k)**1.170*qlz(k)*qsz(k)**0.170))
        else
         Ri(k) = 0
        endif
       enddo



       max_ri_k = MAXLOC(Ri,dim=1)
       max_ri   = MAXVAL(Ri)

       do k = kts, max_ri_k
         Ri(k) = max_ri
       enddo


      do k = kts, kte
         Ri(k) = AMAX1(0.,AMIN1(Ri(k),1.0))      

         Riz(k) = Ri(k)

         cap_s(k)= 0.25*(1+Ri(k))
         tc0     = AMIN1(-0.1, tem(k)-273.15)          
         N0_s(k) = amin1(2.0E8, 2.0E6*exp(-0.12*tc0))          
         am_s(k) = am_c1+am_c2*tc0+am_c3*Ri(k)*Ri(k)   
         am_s(k) = AMAX1(0.000023,am_s(k))             
         bm_s(k) = bm_c1+bm_c2*tc0+bm_c3*Ri(k)
         bm_s(k) = AMIN1(bm_s(k),3.0)                  

         am_s(k) =  10**(2*bm_s(k)-3.0)*am_s(k)
         aa_s(k) = aa_c1 + aa_c2*tc0 + aa_c3*Ri(k)
         ba_s(k) = ba_c1 + ba_c2*tc0 + ba_c3*Ri(k)

         aa_s(k) = (1e-2)**(2.0-ba_s(k))*aa_s(k)

         av_s(k) = best_a*viscmu(k)*(2*grav*am_s(k)/rho(k)/aa_s(k)/(viscmu(k)**2))**best_b
         bv_s(k) = best_b*(bm_s(k)-ba_s(k)+2)-1
        
         tmp_ss(k)= bm_s(k)+mu_s+1
         tmp_sa(k)= ba_s(k)+mu_s+1

      enddo 













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
            vtrold(k)=o6*av_r*gambp4*sqrho(k)/tmp1**bv_r
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

            tmp1= (am_s(k)*N0_s(k)*ggamma(tmp_ss(k))*orho(k)/qsz(k))&
                   **(1./tmp_ss(k))

            vtsold(k)= sqrho(k)*av_s(k)*ggamma(bv_s(k)+tmp_ss(k))/ &
                      ggamma(tmp_ss(k))/(tmp1**bv_s(k))

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





      DO 2000 k=kts,kte

         qvzodt(k)=amax1( 0.0,odtb*qvz(k) )
         qlzodt(k)=amax1( 0.0,odtb*qlz(k) )
         qizodt(k)=amax1( 0.0,odtb*qiz(k) )
         qszodt(k)=amax1( 0.0,odtb*qsz(k) )
         qrzodt(k)=amax1( 0.0,odtb*qrz(k) )


















        tmp=qiz(k)+qlz(k)+qsz(k)+qrz(k)
        if( qvz(k)+qlz(k)+qiz(k) .lt. qsiz(k)  &
            .and. tmp .eq. 0.0 ) go to 2000



        if (qrz(k) .gt. 1.0e-8) then
            tmp1=sqrt(pi*rhowater*xnor*orho(k)/qrz(k))
            xlambdar(k)=sqrt(tmp1)
            olambdar(k)=1.0/xlambdar(k)
            vtrold(k)=o6*av_r*gambp4*sqrho(k)*olambdar(k)**bv_r
        else
            vtrold(k)=0.
            olambdar(k)=0.
        endif

        if (qrz(k) .gt. 1.0e-8) then
            tmp1=sqrt(pi*rhowater*xnor*orho(k)/qrz(k))
            xlambdar(k)=sqrt(tmp1)
            olambdar(k)=1.0/xlambdar(k)
            vtr(k)=o6*av_r*gambp4*sqrho(k)*olambdar(k)**bv_r
        else
            vtr(k)=0.
            olambdar(k)=0.
        endif



        if (qsz(k) .gt. 1.0e-8) then
            tmp1= (am_s(k)*N0_s(k)*ggamma(tmp_ss(k))*orho(k)/qsz(k))&
                   **(1./tmp_ss(k))
            olambdas(k)=1.0/tmp1
            vtsold(k)= sqrho(k)*av_s(k)*ggamma(bv_s(k)+tmp_ss(k))/ &
                      ggamma(tmp_ss(k))/(tmp1**bv_s(k))

        else
            vtsold(k)=0.
            olambdas(k)=0.
        endif

        if (qsz(k) .gt. 1.0e-8) then
             tmp1= (am_s(k)*N0_s(k)*ggamma(tmp_ss(k))*orho(k)/qsz(k))&
                   **(1./tmp_ss(k))
             olambdas(k)=1.0/tmp1
             vts(k)= sqrho(k)*av_s(k)*ggamma(bv_s(k)+tmp_ss(k))/ &
                      ggamma(tmp_ss(k))/(tmp1**bv_s(k))

        else
            vts(k)=0.
            olambdas(k)=0.
        endif



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



            vti50=av_i*di50**bv_i*sqrho(k)

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
          save1=pio4*eri*xnor*av_r*sqrho(k)
          tmp1=save1*gambp3*olambdar(k)**bp3
          praci(k)=qizodt(k)*( 1.0-exp(-tmp1*dtb) )




          tmp2=qiz(k)*save1*rho(k)*pio6*rhowater*gambp6*oxmi* &
                   olambdar(k)**bp6
          piacr(k)=amin1( tmp2,qrzodt(k) )


1000      continue

          if(qsz(k) .le. 0.0) go to 1200






          esi=exp( 0.025*temcc(k) )
          save1 = aa_s(k)*sqrho(k)*N0_s(k)* &
                  ggamma(bv_s(k)+tmp_sa(k))*olambdas(k)**(bv_s(k)+tmp_sa(k))

          tmp1=esi*save1
          psaci(k)=qizodt(k)*( 1.0-exp(-tmp1*dtb) )




          esw=1.0
          tmp1=esw*save1
          psacw(k)=qlzodt(K)*( 1.0-exp(-tmp1*dtb) )





          tmpa=rvapor*xka(k)*tem(k)*tem(k)
          tmpb=xls*xls*rho(k)*qsiz(k)*diffwv(k)
          tmpc=tmpa*qsiz(k)*diffwv(k)
          abi=4.0*pi*cap_s(k)*(qvoqsiz(k)-1.0)*tmpc/(tmpa+tmpb)
          tmp1=av_s(k)*sqrho(k)*olambdas(k)**(5+bv_s(k)+2*mu_s)/visc(k)



          tmp2= abi*N0_s(k)*( vf1s*olambdas(k)*olambdas(k)+ &
                vf2s*schmidt(k)**0.33334* &
                ggamma(2.5+0.5*bv_s(k)+mu_s)*sqrt(tmp1) )

          tmp3=odtb*( qvz(k)-qsiz(k) )

          if( tmp2 .le. 0.0) then
            tmp2=amax1( tmp2,tmp3)
            pssub(k)=amax1( tmp2,-qszodt(k) )
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
          tmp1=pi*pi*esr*xnor*N0_s(k)*abs( vtr(k)-vts(k) )*orho(k)
          tmp2=tmpb*tmpb*olambdar(k)*(5.0*tmpb+2.0*tmpc+0.5*tmpa)
          tmp3=tmp1*rhosnow*tmp2
          pracs(k)=amin1( tmp3,qszodt(k) )
          pracs(k)=0.0



          tmp3=tmpa*tmpa*olambdas(k)*(5.0*tmpa+2.0*tmpc+0.5*tmpb)
          tmp4=tmp1*rhowater*tmp3
          psacr(k)=amin1( tmp4,qrzodt(k) )








            if (qrz(k) .gt. 1.e-8 ) then
               Bp=100.
               Ap=0.66
               tmp1=olambdar(k)*olambdar(k)*olambdar(k)
               tmp2=20.*pi*pi*Bp*xnor*rhowater*orho(k)*  &
                    (exp(-Ap*temcc(k))-1.0)*tmp1*tmp1*olambdar(k)
               pgfr(k)=amin1( tmp2,qrzodt(k) )
            else
               pgfr(k)=0
            endif

1200      continue


        else                        






         if (qsz(k) .le. 0.0) go to 1400



            esw=1.0

            save1 =aa_s(k)*sqrho(k)*N0_s(k)* &
                   ggamma(bv_s(k)+tmp_sa(k))*olambdas(k)**(bv_s(k)+tmp_sa(k))

            tmp1=esw*save1
            psacw(k)=qlzodt(k)*( 1.0-exp(-tmp1*dtb) )




            esr=1.0
            tmpa=olambdar(k)*olambdar(k)
            tmpb=olambdas(k)*olambdas(k)
            tmpc=olambdar(k)*olambdas(k)
            tmp1=pi*pi*esr*xnor*N0_s(k)*abs( vtr(k)-vts(k) )*orho(k)
            tmp2=tmpa*tmpa*olambdas(k)*(5.0*tmpa+2.0*tmpc+0.5*tmpb)
            tmp3=tmp1*rhowater*tmp2
            psacr(k)=amin1( tmp3,qrzodt(k) )




            delrs=rs0(k)-qvz(k)
            term1=2.0*pi*orho(k)*( xlv*diffwv(k)*rho(k)*delrs- &
                  xka(k)*temcc(k) )
            tmp1= av_s(k)*sqrho(k)*olambdas(k)**(5+bv_s(k)+2*mu_s)/visc(k)
            tmp2= N0_s(k)*( vf1s*olambdas(k)*olambdas(k)+ &
                  vf2s*schmidt(k)**0.33334* &
                  ggamma(2.5+0.5*bv_s(k)+mu_s)*sqrt(tmp1) )
            tmp3=term1*oxlf*tmp2-cwoxlf*temcc(k)*( psacw(k)+psacr(k) )
            tmp4=amin1(0.0,tmp3)
            psmlt(k)=amax1( tmp4,-qszodt(k) )





            tmpa=rvapor*xka(k)*tem(k)*tem(k)
            tmpb=xlv*xlv*rho(k)*qswz(k)*diffwv(k)
            tmpc=tmpa*qswz(k)*diffwv(k)
            tmpd=amin1( 0.0,(qvoqswz(k)-0.90)*qswz(k)*odtb )

            abr=2.0*pi*(qvoqswz(k)-0.90)*tmpc/(tmpa+tmpb)







            tmp1=av_s(k)*sqrho(k)*olambdas(k)**(5+bv_s(k)+2*mu_s)/visc(k)
            tmp2= N0_s(k)*( vf1s*olambdas(k)*olambdas(k)+ &
                  vf2s*schmidt(k)**0.33334* &
                  ggamma(2.5+0.5*bv_s(k)+mu_s)*sqrt(tmp1) )
            tmp3=amin1(0.0,tmp2)
            tmp3=amax1( tmp3,tmpd )
            psmltevp(k)=amax1( tmp3,-qszodt(k) )
1400     continue

        end if      














       if (qlz(k) .gt. 1e-6) then
           lamc(k) = (Nt_c*rhowater*pi*ggamma(4.+mu_c)/(6.*rho(k)*qlz(k))/ &        
                    ggamma(1+mu_c))**0.3333
           Dc_liu  = (ggamma(6+1+mu_c)/ggamma(1+mu_c))**(1./6.)/lamc(k)             

           if (Dc_liu .gt. R6c) then
             disp = 1./(mu_c+1.)      
             eta  = (0.75/pi/(1e-3*rhowater))**2*1.9e11*((1+3*disp)*(1+4*disp)*&
                   (1+5*disp)/(1+disp)/(1+2*disp))
             praut(k) = eta*(1e-3*rho(k)*qlz(k))**3/(1e-6*Nt_c)                      
             praut(k) = praut(k)/(1e-3*rho(k))                                       
           else
            praut(k) = 0.0
           endif
       else
         praut(k) = 0.0
       endif 




        erw=1.0

        tmp1=pio4*erw*xnor*av_r*sqrho(k)* &
             gambp3*olambdar(k)**bp3
        pracw(k)=qlzodt(k)*( 1.0-exp(-tmp1*dtb) )







         tmpa=rvapor*xka(k)*tem(k)*tem(k)
         tmpb=xlv*xlv*rho(k)*qswz(k)*diffwv(k)
         tmpc=tmpa*qswz(k)*diffwv(k)
         tmpd=amin1(0.0,(qvoqswz(k)-0.90)*qswz(k)*odtb)

         abr=2.0*pi*(qvoqswz(k)-0.90)*tmpc/(tmpa+tmpb)
         tmp1=av_r*sqrho(k)*olambdar(k)**bp5/visc(k)
         tmp2=abr*xnor*( vf1r*olambdar(k)*olambdar(k)+  &
              vf2r*schmidt(k)**0.33334*gambp5o2*sqrt(tmp1) )
         tmp3=amin1( 0.0,tmp2 )
         tmp3=amax1( tmp3,tmpd )
         prevp(k)=amax1( tmp3,-qrzodt(k) )









      if ( temcc(k) .lt. 0.0) then



           tmp=psdep(k)
           if ( tmp .gt. qvzodt(k) ) then
              factor=qvzodt(k)/tmp
              psdep(k)=psdep(k)*factor
           end if



           tmp=praut(k)+psacw(k)+psfw(k)+pracw(k)
           if ( tmp .gt. qlzodt(k) ) then
              factor=qlzodt(k)/tmp
              praut(k)=praut(k)*factor
              psacw(k)=psacw(k)*factor
              psfw(k)=psfw(k)*factor
              pracw(k)=pracw(k)*factor
           end if



           tmp=psaut(k)+psaci(k)+praci(k)+psfi(k)
           if (tmp .gt. qizodt(k) ) then
              factor=qizodt(k)/tmp
              psaut(k)=psaut(k)*factor
              psaci(k)=psaci(k)*factor
              praci(k)=praci(k)*factor
              psfi(k)=psfi(k)*factor
           endif



          tmp_r=piacr(k)+psacr(k)-prevp(k)-praut(k)-pracw(k)+pgfr(k) 
          if (tmp_r .gt. qrzodt(k) ) then
             factor=qrzodt(k)/tmp_r
             piacr(k)=piacr(k)*factor
             psacr(k)=psacr(k)*factor
             prevp(k)=prevp(k)*factor
             pgfr(k)=pgfr(k)*factor
          endif



          tmp_s=-pssub(k)-(psaut(k)+psaci(k)+psacw(k)+psfw(k)+pgfr(k)+ &
                 psfi(k)+praci(k)+piacr(k)+ &
                 psdep(k)+psacr(k)-pracs(k))
          if ( tmp_s .gt. qszodt(k) ) then
             factor=qszodt(k)/tmp_s
             pssub(k)=pssub(k)*factor
             Pracs(k)=Pracs(k)*factor
          endif





         pvapor(k)=-pssub(k)-psdep(k)-prevp(k)
         qvz(k)=amax1( qvmin,qvz(k)+dtb*pvapor(k) )
         pclw(k)=-praut(k)-pracw(k)-psacw(k)-psfw(k)
         qlz(k)=amax1( 0.0,qlz(k)+dtb*pclw(k) )
         pcli(k)=-psaut(k)-psfi(k)-psaci(k)-praci(k)
         qiz(k)=amax1( 0.0,qiz(k)+dtb*pcli(k) )
         tmp_r=piacr(k)+psacr(k)-prevp(k)-praut(k)-pracw(k)+pgfr(k)-pracs(k) 
         prain(k)=-tmp_r
         qrz(k)=amax1( 0.0,qrz(k)+dtb*prain(k) )
         tmp_s=-pssub(k)-(psaut(k)+psaci(k)+psacw(k)+psfw(k)+pgfr(k)+  &
                psfi(k)+praci(k)+piacr(k)+  &
                psdep(k)+psacr(k)-pracs(k))
         psnow(k)=-tmp_s
         qsz(k)=amax1( 0.0,qsz(k)+dtb*psnow(k) )

         qschg(k)=qschg(k)+psnow(k)
         qschg(k)=psnow(k)

         tmp=ocp/tothz(k)*xLf*qschg(k)
         theiz(k)=theiz(k)+dtb*tmp
         thz(k)=theiz(k)-(xLvocp*qvz(k)-xLfocp*qiz(k))/tothz(k)
         tem(k)=thz(k)*tothz(k)

         temcc(k)=tem(k)-273.15

         if( temcc(k) .lt. -40.0 ) qswz(k)=qsiz(k)
         qlpqi=qlz(k)+qiz(k)
         if ( qlpqi .eq. 0.0 ) then
            qvsbar(k)=qsiz(k)
         else
            qvsbar(k)=( qiz(k)*qsiz(k)+qlz(k)*qswz(k) )/qlpqi
         endif


      else                  



          tmp=praut(k)+psacw(k)+pracw(k)
          if ( tmp .gt. qlzodt(k) ) then
             factor=qlzodt(k)/tmp
             praut(k)=praut(k)*factor
             psacw(k)=psacw(k)*factor
             pracw(k)=pracw(k)*factor
          end if



          tmp_s=-(psmlt(k)+psmltevp(k))
          if (tmp_s .gt. qszodt(k) ) then
             factor=qszodt(k)/tmp_s
             psmlt(k)=psmlt(k)*factor
             psmltevp(k)=psmltevp(k)*factor
          endif



          tmp_r=-prevp(k)-(praut(k)+pracw(k)+psacw(k)-psmlt(k)) 
          if (tmp_r .gt. qrzodt(k) ) then
             factor=qrzodt(k)/tmp_r
             prevp(k)=prevp(k)*factor
          endif



          pvapor(k)=-psmltevp(k)-prevp(k)
          qvz(k)=amax1( qvmin,qvz(k)+dtb*pvapor(k))
          pclw(k)=-praut(k)-pracw(k)-psacw(k)
          qlz(k)=amax1( 0.0,qlz(k)+dtb*pclw(k) )
          pcli(k)=0.0
          qiz(k)=amax1( 0.0,qiz(k)+dtb*pcli(k) )
          tmp_r=-prevp(k)-(praut(k)+pracw(k)+psacw(k)-psmlt(k)) 
          prain(k)=-tmp_r
          tmpqrz=qrz(k)
          qrz(k)=amax1( 0.0,qrz(k)+dtb*prain(k) )
          tmp_s=-(psmlt(k)+psmltevp(k))
          psnow(k)=-tmp_s
          qsz(k)=amax1( 0.0,qsz(k)+dtb*psnow(k) )
          qschg(k)=psnow(k)

          tmp=ocp/tothz(k)*xLf*qschg(k)
          theiz(k)=theiz(k)+dtb*tmp
          thz(k)=theiz(k)-(xLvocp*qvz(k)-xLfocp*qiz(k))/tothz(k)

          tem(k)=thz(k)*tothz(k)
          temcc(k)=tem(k)-273.15
          es=1000.*svp1*exp( svp2*temcc(k)/(tem(k)-svp3) )
          qswz(k)=ep2*es/(prez(k)-es)
          qsiz(k)=qswz(k)
          qvsbar(k)=qswz(k)

      end if











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
                    k, xLvocp, xLfocp, episp0k, EP2,SVP1,SVP2,SVP3,SVPT0)


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




      END SUBROUTINE clphy1d_ylin







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

     REAL :: qsat, qlpqi, ratql, t0, t1, tmp1, ratqi, tsat, absft,    &
             denom1, denom2, dqvsbar, ftsat, dftsat, qpz,es             



      thz(k)=theiz(k)-(xLvocp*qvz(k)-xLfocp*qiz(k))/tothz(k)

      tem(k)=tothz(k)*thz(k)
      if (tem(k) .gt. 273.15) then


         es=1000.*svp1*exp( svp2*(tem(k)-svpt0)/(tem(k)-svp3) )
         qsat=ep2*es/(prez(k)-es)
      else
        qsat=episp0k/prez(k)*  &
             exp( 21.8745584*(tem(k)-273.15)/(tem(k)-7.66) )
      end if
      qpz=qvz(k)+qlz(k)+qiz(k)
      if (qpz .lt. qsat) then
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





      REAL FUNCTION RSLF(P,T)

      IMPLICIT NONE
      REAL, INTENT(IN):: P, T
      REAL:: ESL,X
      REAL, PARAMETER:: C0= .611583699E03
      REAL, PARAMETER:: C1= .444606896E02
      REAL, PARAMETER:: C2= .143177157E01
      REAL, PARAMETER:: C3= .264224321E-1
      REAL, PARAMETER:: C4= .299291081E-3
      REAL, PARAMETER:: C5= .203154182E-5
      REAL, PARAMETER:: C6= .702620698E-8
      REAL, PARAMETER:: C7= .379534310E-11
      REAL, PARAMETER:: C8=-.321582393E-13

      X=MAX(-80.,T-273.16)


      ESL=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))
      RSLF=.622*ESL/(P-ESL)

      END FUNCTION RSLF





      REAL FUNCTION RSIF(P,T)

      IMPLICIT NONE
      REAL, INTENT(IN):: P, T
      REAL:: ESI,X
      REAL, PARAMETER:: C0= .609868993E03
      REAL, PARAMETER:: C1= .499320233E02
      REAL, PARAMETER:: C2= .184672631E01
      REAL, PARAMETER:: C3= .402737184E-1
      REAL, PARAMETER:: C4= .565392987E-3
      REAL, PARAMETER:: C5= .521693933E-5
      REAL, PARAMETER:: C6= .307839583E-7
      REAL, PARAMETER:: C7= .105785160E-9
      REAL, PARAMETER:: C8= .161444444E-12

      X=MAX(-80.,T-273.16)
      ESI=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))
      RSIF=.622*ESI/(P-ESI)

      END FUNCTION RSIF



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



   20 G1TO2=1.
      TEMP=TEMP - 1.
      DO 30 K1=1,8
   30 G1TO2=G1TO2 + B(K1)*TEMP**K1
      ggamma=PF*G1TO2

      END FUNCTION ggamma



      END MODULE module_mp_sbu_ylin

