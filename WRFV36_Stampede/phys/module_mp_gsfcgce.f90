


MODULE module_mp_gsfcgce

   USE     module_wrf_error
   USE module_utility, ONLY: WRFU_Clock, WRFU_Alarm
   USE module_domain, ONLY : HISTORY_ALARM, Is_alarm_tstep
   USE module_mp_radar




   REAL,    PRIVATE ::          rd1,  rd2,   al,   cp


   REAL,    PRIVATE ::          c38, c358, c610, c149, &
                               c879, c172, c409,  c76, &
                               c218, c580, c141

   REAL,    PRIVATE ::           ag,   bg,   as,   bs, &
                                 aw,   bw,  bgh,  bgq, &
                                bsh,  bsq,  bwh,  bwq


   REAL,    PRIVATE ::          tnw,  tns,  tng,       &
                               roqs, roqg, roqr


   REAL,    PRIVATE ::          zrc,  zgc,  zsc,       &
                                vrc,  vgc,  vsc


   REAL,    PRIVATE ::          alv,  alf,  als,   t0,   t00,     &
                                avc,  afc,  asc,  rn1,  bnd1,     &
                                rn2, bnd2,  rn3,  rn4,   rn5,     &
                                rn6,  rn7,  rn8,  rn9,  rn10,     &
                              rn101,rn10a, rn11,rn11a,  rn12

   REAL,    PRIVATE ::         rn14, rn15,rn15a, rn16,  rn17,     &
                              rn17a,rn17b,rn17c, rn18, rn18a,     &
                               rn19,rn19a,rn19b, rn20, rn20a,     &
                              rn20b, bnd3, rn21, rn22,  rn23,     &
                              rn23a,rn23b, rn25,rn30a, rn30b,     &
                              rn30c, rn31, beta, rn32

   REAL,    PRIVATE, DIMENSION( 31 ) ::    rn12a, rn12b, rn13, rn25a


   REAL,    PRIVATE ::         rn10b, rn10c, rnn191, rnn192,  rn30,     &
                             rnn30a,  rn33,  rn331,  rn332


   REAL,    PRIVATE, DIMENSION( 31 )  ::      aa1,  aa2
   DATA aa1/.7939e-7, .7841e-6, .3369e-5, .4336e-5, .5285e-5,     &
           .3728e-5, .1852e-5, .2991e-6, .4248e-6, .7434e-6,     &
           .1812e-5, .4394e-5, .9145e-5, .1725e-4, .3348e-4,     &
           .1725e-4, .9175e-5, .4412e-5, .2252e-5, .9115e-6,     &
           .4876e-6, .3473e-6, .4758e-6, .6306e-6, .8573e-6,     &
           .7868e-6, .7192e-6, .6513e-6, .5956e-6, .5333e-6,     &
           .4834e-6/
   DATA aa2/.4006, .4831, .5320, .5307, .5319,      &
           .5249, .4888, .3894, .4047, .4318,      &
           .4771, .5183, .5463, .5651, .5813,      &
           .5655, .5478, .5203, .4906, .4447,      &
           .4126, .3960, .4149, .4320, .4506,      &
           .4483, .4460, .4433, .4413, .4382,      &
           .4361/






      REAL    , PARAMETER ::     xnor = 8.0e6
      REAL    , PARAMETER ::     xnos = 1.6e7
      REAL    , PARAMETER ::     xnoh = 2.0e5
      REAL    , PARAMETER ::     xnog = 4.0e6
      REAL    , PARAMETER ::     rhohail = 917.
      REAL    , PARAMETER ::     rhograul = 400.




CONTAINS






  SUBROUTINE gsfcgce(  th,                                         &
                       qv, ql,                                     &
                       qr, qi,                                     &
                       qs,                                         &



                       rho, pii, p, dt_in, z,                      &
                       ht, dz8w, grav,                             &
                       rhowater, rhosnow,                          &
                       itimestep,                                  &
                       ids,ide, jds,jde, kds,kde,                  & 
                       ims,ime, jms,jme, kms,kme,                  & 
                       its,ite, jts,jte, kts,kte,                  & 
                       rainnc, rainncv,                            &
                       snownc, snowncv, sr,                        &
                       graupelnc, graupelncv,                      &
                       refl_10cm, diagflag, do_radar_ref,          &

                       f_qg, qg,                                   &
                       ihail, ice2                                 &
                                                                   )


  IMPLICIT NONE




  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte 
  INTEGER,      INTENT(IN   )    ::   itimestep, ihail, ice2 

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(INOUT) ::                                          &
                                                              th, &
                                                              qv, &
                                                              ql, &
                                                              qr, &
                                                              qi, &
                                                              qs, &
                                                              qg

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(IN   ) ::                                          &







                                                             rho, &
                                                             pii, &
                                                               p, &
                                                            dz8w, &
                                                               z

  REAL, DIMENSION( ims:ime , jms:jme ),                           &
        INTENT(INOUT) ::                               rainnc,    &
                                                       rainncv,   &
                                                       snownc,    &   
                                                       snowncv,   &
                                                       sr,        &
                                                       graupelnc, &
                                                       graupelncv 


  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT)::           &  
                                                       refl_10cm
  LOGICAL, OPTIONAL, INTENT(IN) :: diagflag
  INTEGER, OPTIONAL, INTENT(IN) :: do_radar_ref


  REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN) ::       ht

  REAL, INTENT(IN   ) ::                                   dt_in, &
                                                            grav, &
                                                        rhowater, &
                                                         rhosnow

  LOGICAL, INTENT(IN), OPTIONAL :: F_QG











  INTEGER ::  itaobraun, istatmin, new_ice_sat, id

  INTEGER :: i, j, k
  INTEGER :: iskip, ih, icount, ibud, i24h 
  REAL    :: hour
  REAL    , PARAMETER :: cmin=1.e-20
  REAL    :: dth, dqv, dqrest, dqall, dqall1, rhotot, a1, a2 


 
  LOGICAL :: flag_qg



      INTEGER:: NCALL=0

      IF (NCALL .EQ. 0) THEN



         xam_r = 3.14159*rhowater/6.
         xbm_r = 3.
         xmu_r = 0.
         xam_s = 3.14159*rhosnow/6.
         xbm_s = 3.
         xmu_s = 0.
         if (ihail .eq. 1) then
            xam_g = 3.14159*rhohail/6.
         else
            xam_g = 3.14159*rhograul/6.
         endif
         xbm_g = 3.
         xmu_g = 0.

         call radar_init
         NCALL = 1
      ENDIF









   itaobraun = 1








  i24h=nint(86400./dt_in)
  if (mod(itimestep,i24h).eq.1) then
     write(6,*) 'ihail=',ihail,'  ice2=',ice2
     if (ice2.eq.0) then
        write(6,*) 'Running 3-ice scheme in GSFCGCE with'
        if (ihail.eq.0) then 
           write(6,*) '     ice, snow and graupel'
        else if (ihail.eq.1) then
                write(6,*) '     ice, snow and hail'
        else
             write(6,*) 'ihail has to be either 1 or 0'
             stop
        endif 
     else if (ice2.eq.1) then
             write(6,*) 'Running 2-ice scheme in GSFCGCE with'
             write(6,*) '     ice and snow'
     else if (ice2.eq.2) then
             write(6,*) 'Running 2-ice scheme in GSFCGCE with'
             write(6,*) '     ice and graupel'
     else if (ice2.eq.3) then
             write(6,*) 'Running warm rain only scheme in GSFCGCE without any ice'
     else
             write(6,*) 'gsfcgce_2ice in namelist.input has to be 0, 1, 2, or 3'
             stop
     endif 
  endif 


    new_ice_sat = 2 


    istatmin = 180



    id = 0



    ibud = 0










   call fall_flux(dt_in, qr, qi, qs, qg, p,                   &
                      rho, z, dz8w, ht, rainnc,               &
                      rainncv, grav,itimestep,                &
                      rhowater, rhosnow,                      &
                      snownc, snowncv, sr,                    &
                      graupelnc, graupelncv,                  &
                      ihail, ice2,                            &
                      ims,ime, jms,jme, kms,kme,              & 
                      its,ite, jts,jte, kts,kte               ) 




   call consat_s (ihail, itaobraun)




   iskip = 1
 
   if (iskip.eq.0) then
      call negcor(qv,rho,dz8w,ims,ime,jms,jme,kms,kme, &
                           itimestep,1,             &
                           its,ite,jts,jte,kts,kte)
      call negcor(ql,rho,dz8w,ims,ime,jms,jme,kms,kme, &
                           itimestep,2,             &
                           its,ite,jts,jte,kts,kte)
      call negcor(qr,rho,dz8w,ims,ime,jms,jme,kms,kme, &
                           itimestep,3,             &
                           its,ite,jts,jte,kts,kte)
      call negcor(qi,rho,dz8w,ims,ime,jms,jme,kms,kme, &
                           itimestep,4,             &
                           its,ite,jts,jte,kts,kte)
      call negcor(qs,rho,dz8w,ims,ime,jms,jme,kms,kme, &
                           itimestep,5,             &
                           its,ite,jts,jte,kts,kte)
      call negcor(qg,rho,dz8w,ims,ime,jms,jme,kms,kme, &
                           itimestep,6,             &
                           its,ite,jts,jte,kts,kte)


   endif 



   call SATICEL_S( dt_in, IHAIL, itaobraun, ICE2, istatmin,     &
                   new_ice_sat, id,                             &

                   th, qv, ql, qr,                      &
                   qi, qs, qg,                                  &


                   rho, pii, p, itimestep,                      & 
                   refl_10cm, diagflag, do_radar_ref,           & 

                   ids,ide, jds,jde, kds,kde,                   & 
                   ims,ime, jms,jme, kms,kme,                   & 
                   its,ite, jts,jte, kts,kte                    & 
                                                                ) 


   END SUBROUTINE gsfcgce


   SUBROUTINE fall_flux ( dt, qr, qi, qs, qg, p,              &
                      rho, z, dz8w, topo, rainnc,             &
                      rainncv, grav, itimestep,               &
                      rhowater, rhosnow,                      &
                      snownc, snowncv, sr,                    &
                      graupelnc, graupelncv,                  &
                      ihail, ice2,                            &
                      ims,ime, jms,jme, kms,kme,              & 
                      its,ite, jts,jte, kts,kte               ) 





  IMPLICIT NONE
  INTEGER, INTENT(IN   )               :: ihail, ice2,                &
                                          ims,ime, jms,jme, kms,kme,  &
                                          its,ite, jts,jte, kts,kte 
  INTEGER, INTENT(IN   )               :: itimestep
  REAL,    DIMENSION( ims:ime , kms:kme , jms:jme ),                  &
           INTENT(INOUT)               :: qr, qi, qs, qg       
  REAL,    DIMENSION( ims:ime , jms:jme ),                            &
           INTENT(INOUT)               :: rainnc, rainncv,            &
                                          snownc, snowncv, sr,        &
                                          graupelnc, graupelncv
  REAL,    DIMENSION( ims:ime , kms:kme , jms:jme ),                  &
           INTENT(IN   )               :: rho, z, dz8w, p

  REAL,    INTENT(IN   )               :: dt, grav, rhowater, rhosnow


  REAL,    DIMENSION( ims:ime , jms:jme ),                            &
           INTENT(IN   )               :: topo   



  REAL,    DIMENSION( kts:kte )           :: sqrhoz
  REAL                                    :: tmp1, term0
  REAL                                :: pptrain, pptsnow,        &
                                         pptgraul, pptice
  REAL,    DIMENSION( kts:kte )       :: qrz, qiz, qsz, qgz,      &
                                         zz, dzw, prez, rhoz,     &
                                         orhoz


   INTEGER                    :: k, i, j


  REAL, DIMENSION( kts:kte )    :: vtr, vts, vtg, vti

  REAL                          :: dtb, pi, consta, constc, gambp4,    &
                                   gamdp4, gam4pt5, gam4bbar





   REAL    , PARAMETER ::                              &

             constb = 0.8, constd = 0.11, o6 = 1./6.,           &
             cdrag = 0.6








  REAL    , PARAMETER ::     abar = 19.3, bbar = 0.37,      &
                                      p0 = 1.0e5

  REAL    , PARAMETER ::     rhoe_s = 1.29


  INTEGER                       :: min_q, max_q
  REAL                          :: t_del_tv, del_tv, flux, fluxin, fluxout ,tmpqrz
  LOGICAL                       :: notlast



















   dtb=dt
   pi=acos(-1.)
   consta=2115.0*0.01**(1-constb)

   constc=78.63*0.01**(1-constd)


   gambp4=ggamma(constb+4.)
   gamdp4=ggamma(constd+4.)
   gam4pt5=ggamma(4.5)
   gam4bbar=ggamma(4.+bbar)








 j_loop:  do j = jts, jte
 i_loop:  do i = its, ite

   pptrain = 0.
   pptsnow = 0.
   pptgraul = 0.
   pptice  = 0.

   do k = kts, kte
      qrz(k)=qr(i,k,j)
      rhoz(k)=rho(i,k,j)
      orhoz(k)=1./rhoz(k)
      prez(k)=p(i,k,j)
      sqrhoz(k)=sqrt(rhoe_s/rhoz(k))
      zz(k)=z(i,k,j)
      dzw(k)=dz8w(i,k,j)
   enddo 

      DO k = kts, kte
         qiz(k)=qi(i,k,j)
      ENDDO

      DO k = kts, kte
         qsz(k)=qs(i,k,j)
      ENDDO

   IF (ice2 .eq. 0) THEN
      DO k = kts, kte
         qgz(k)=qg(i,k,j)
      ENDDO
   ELSE
      DO k = kts, kte
         qgz(k)=0.
      ENDDO
   ENDIF





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
            tmp1=sqrt(pi*rhowater*xnor/rhoz(k)/qrz(k))
            tmp1=sqrt(tmp1)
            vtr(k)=consta*gambp4*sqrhoz(k)/tmp1**constb
            vtr(k)=vtr(k)/6.
            if (k .eq. 1) then
               del_tv=amin1(del_tv,0.9*(zz(k)-topo(i,j))/vtr(k))
            else
               del_tv=amin1(del_tv,0.9*(zz(k)-zz(k-1))/vtr(k))
            endif
         else
            vtr(k)=0.
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
            fluxout=rhoz(k)*vtr(k)*qrz(k)
            flux=(fluxin-fluxout)/rhoz(k)/dzw(k)

            qrz(k)=qrz(k)+del_tv*flux
            qrz(k)=amax1(0.,qrz(k))
            qr(i,k,j)=qrz(k)
            fluxin=fluxout
         enddo
         if (min_q .eq. 1) then
            pptrain=pptrain+fluxin*del_tv
         else
            qrz(min_q-1)=qrz(min_q-1)+del_tv*  &
                          fluxin/rhoz(min_q-1)/dzw(min_q-1)
            qr(i,min_q-1,j)=qrz(min_q-1)
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
            tmp1=sqrt(pi*rhosnow*xnos/rhoz(k)/qsz(k))
            tmp1=sqrt(tmp1)
            vts(k)=constc*gamdp4*sqrhoz(k)/tmp1**constd
            vts(k)=vts(k)/6.
            if (k .eq. 1) then
               del_tv=amin1(del_tv,0.9*(zz(k)-topo(i,j))/vts(k))
            else
               del_tv=amin1(del_tv,0.9*(zz(k)-zz(k-1))/vts(k))
            endif
         else
            vts(k)=0.
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
            fluxout=rhoz(k)*vts(k)*qsz(k)
            flux=(fluxin-fluxout)/rhoz(k)/dzw(k)
            qsz(k)=qsz(k)+del_tv*flux
            qsz(k)=amax1(0.,qsz(k))
            qs(i,k,j)=qsz(k)
            fluxin=fluxout
         enddo
         if (min_q .eq. 1) then
            pptsnow=pptsnow+fluxin*del_tv
         else
            qsz(min_q-1)=qsz(min_q-1)+del_tv*  &
                         fluxin/rhoz(min_q-1)/dzw(min_q-1)
            qs(i,min_q-1,j)=qsz(min_q-1)
         endif

      else
         notlast=.false.
      endif

    ENDDO





  if (ice2.eq.0) then 









    t_del_tv=0.
    del_tv=dtb
    notlast=.true.

    DO while (notlast)

      min_q=kte
      max_q=kts-1

      do k=kts,kte-1
         if (qgz(k) .gt. 1.0e-8) then
            if (ihail .eq. 1) then

              min_q=min0(min_q,k)
              max_q=max0(max_q,k)
              tmp1=sqrt(pi*rhohail*xnoh/rhoz(k)/qgz(k))
              tmp1=sqrt(tmp1)
              term0=sqrt(4.*grav*rhohail/3./rhoz(k)/cdrag)
              vtg(k)=gam4pt5*term0*sqrt(1./tmp1)
              vtg(k)=vtg(k)/6.
              if (k .eq. 1) then
                 del_tv=amin1(del_tv,0.9*(zz(k)-topo(i,j))/vtg(k))
              else
                 del_tv=amin1(del_tv,0.9*(zz(k)-zz(k-1))/vtg(k))
              endif 
            else


              min_q=min0(min_q,k)
              max_q=max0(max_q,k)
              tmp1=sqrt(pi*rhograul*xnog/rhoz(k)/qgz(k))
              tmp1=sqrt(tmp1)
              tmp1=tmp1**bbar
              tmp1=1./tmp1
              term0=abar*gam4bbar/6.
              vtg(k)=term0*tmp1*(p0/prez(k))**0.4
              if (k .eq. 1) then
                 del_tv=amin1(del_tv,0.9*(zz(k)-topo(i,j))/vtg(k))
              else
                 del_tv=amin1(del_tv,0.9*(zz(k)-zz(k-1))/vtg(k))
              endif 
            endif 
         else
            vtg(k)=0.
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
            fluxout=rhoz(k)*vtg(k)*qgz(k)
            flux=(fluxin-fluxout)/rhoz(k)/dzw(k)
            qgz(k)=qgz(k)+del_tv*flux
            qgz(k)=amax1(0.,qgz(k))
            qg(i,k,j)=qgz(k)
            fluxin=fluxout
         enddo
         if (min_q .eq. 1) then
            pptgraul=pptgraul+fluxin*del_tv
         else
            qgz(min_q-1)=qgz(min_q-1)+del_tv*  &
                         fluxin/rhoz(min_q-1)/dzw(min_q-1)
            qg(i,min_q-1,j)=qgz(min_q-1)
         endif

      else
         notlast=.false.
      endif

    ENDDO
 ENDIF 




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
            vti(k)= 3.29 * (rhoz(k)* qiz(k))** 0.16  
            if (k .eq. 1) then
               del_tv=amin1(del_tv,0.9*(zz(k)-topo(i,j))/vti(k))
            else
               del_tv=amin1(del_tv,0.9*(zz(k)-zz(k-1))/vti(k))
            endif
         else
            vti(k)=0.
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
            fluxout=rhoz(k)*vti(k)*qiz(k)
            flux=(fluxin-fluxout)/rhoz(k)/dzw(k)
            qiz(k)=qiz(k)+del_tv*flux
            qiz(k)=amax1(0.,qiz(k))
            qi(i,k,j)=qiz(k)
            fluxin=fluxout
         enddo
         if (min_q .eq. 1) then
            pptice=pptice+fluxin*del_tv
         else
            qiz(min_q-1)=qiz(min_q-1)+del_tv*  &
                         fluxin/rhoz(min_q-1)/dzw(min_q-1)
            qi(i,min_q-1,j)=qiz(min_q-1)
         endif

      else
         notlast=.false.
      endif

   ENDDO 










   snowncv(i,j) = pptsnow
   snownc(i,j) = snownc(i,j) + pptsnow
   graupelncv(i,j) = pptgraul
   graupelnc(i,j) = graupelnc(i,j) + pptgraul 
   RAINNCV(i,j) = pptrain + pptsnow + pptgraul + pptice                 
   RAINNC(i,j)  = RAINNC(i,j) + pptrain + pptsnow + pptgraul + pptice
   sr(i,j) = 0.
   if (RAINNCV(i,j) .gt. 0.) sr(i,j) = (pptsnow + pptgraul + pptice) / RAINNCV(i,j) 

  ENDDO i_loop
  ENDDO j_loop












  END SUBROUTINE fall_flux


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
  100 FORMAT(//,5X,'module_gsfcgce: INPUT TO GAMMA FUNCTION TOO LARGE, X=',E12.5)
      WRITE(wrf_err_message,100)X
      CALL wrf_error_fatal3("<stdin>",871,&
wrf_err_message)
   20 G1TO2=1.
      TEMP=TEMP - 1.
      DO 30 K1=1,8
   30 G1TO2=G1TO2 + B(K1)*TEMP**K1
      ggamma=PF*G1TO2

      END FUNCTION ggamma



   SUBROUTINE negcor ( X, rho, dz8w,                         &
                      ims,ime, jms,jme, kms,kme,              & 
                      itimestep, ics,                         &
                      its,ite, jts,jte, kts,kte               ) 

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(INOUT) ::                                     X   
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(IN   ) ::                              rho, dz8w  
  integer, INTENT(IN   ) ::                           itimestep, ics 



  REAL   ::   A0, A1, A2

  A1=0.
  A2=0.
  do k=kts,kte
     do j=jts,jte
        do i=its,ite
        A1=A1+max(X(i,k,j), 0.)*rho(i,k,j)*dz8w(i,k,j)
        A2=A2+max(-X(i,k,j), 0.)*rho(i,k,j)*dz8w(i,k,j)
        enddo
     enddo
  enddo








  A0=0.0

  if (A1.NE.0.0.and.A1.GT.A2) then 
     A0=(A1-A2)/A1

  if (mod(itimestep,540).eq.0) then
     if (ics.eq.1) then
        write(61,*) 'kms=',kms,'  kme=',kme,'  kts=',kts,'  kte=',kte
        write(61,*) 'jms=',jms,'  jme=',jme,'  jts=',jts,'  jte=',jte 
        write(61,*) 'ims=',ims,'  ime=',ime,'  its=',its,'  ite=',ite 
     endif 
     if (ics.eq.1) then
         write(61,*) 'qv timestep=',itimestep
         write(61,*) '  A1=',A1,'   A2=',A2,'   A0=',A0
     else if (ics.eq.2) then
             write(61,*) 'ql timestep=',itimestep
             write(61,*) '  A1=',A1,'   A2=',A2,'   A0=',A0
     else if (ics.eq.3) then
             write(61,*) 'qr timestep=',itimestep
             write(61,*) '  A1=',A1,'   A2=',A2,'   A0=',A0
     else if (ics.eq.4) then
             write(61,*) 'qi timestep=',itimestep
             write(61,*) '  A1=',A1,'   A2=',A2,'   A0=',A0
     else if (ics.eq.5) then
             write(61,*) 'qs timestep=',itimestep
             write(61,*) '  A1=',A1,'   A2=',A2,'   A0=',A0
     else if (ics.eq.6) then
             write(61,*) 'qg timestep=',itimestep
             write(61,*) '  A1=',A1,'   A2=',A2,'   A0=',A0
     else
             write(61,*) 'wrong cloud specieis number'
     endif 
  endif 

     do k=kts,kte
        do j=jts,jte
           do i=its,ite
           X(i,k,j)=A0*AMAX1(X(i,k,j), 0.0)
           enddo
        enddo
     enddo
  endif

  END SUBROUTINE negcor

 SUBROUTINE consat_s (ihail,itaobraun)

































 integer :: itaobraun
 real    :: cn0




















      al = 2.5e10
      cp = 1.004e7
      rd1 = 1.e-3
      rd2 = 2.2

      cpi=4.*atan(1.)
      cpi2=cpi*cpi
      grvt=980.
      cd1=6.e-1
      cd2=4.*grvt/(3.*cd1)
      tca=2.43e3
      dwv=.226
      dva=1.718e-4
      amw=18.016
      ars=8.314e7
      scv=2.2904487
      t0=273.16
      t00=238.16
      alv=2.5e10
      alf=3.336e9
      als=2.8336e10
      avc=alv/cp
      afc=alf/cp
      asc=als/cp
      rw=4.615e6
      cw=4.187e7
      ci=2.093e7
      c76=7.66
      c358=35.86
      c172=17.26939
      c409=4098.026
      c218=21.87456
      c580=5807.695
      c610=6.1078e3
      c149=1.496286e-5
      c879=8.794142
      c141=1.4144354e7



      if(ihail .eq. 1) then
         roqg=.9
         ag=sqrt(cd2*roqg)
         bg=.5
         tng=.002
      else
         roqg=.4
         ag=351.2

         bg=.37
         tng=.04
      endif




      tns=.16 
      roqs=.1

      as=78.63

      bs=.11

      aw=2115.
      bw=.8
      roqr=1.
      tnw=.08

      bgh=.5*bg
      bsh=.5*bs
      bwh=.5*bw
      bgq=.25*bg
      bsq=.25*bs
      bwq=.25*bw

      ga3b  = gammagce(3.+bw)
      ga4b  = gammagce(4.+bw)
      ga6b  = gammagce(6.+bw)
      ga5bh = gammagce((5.+bw)/2.)
      ga3g  = gammagce(3.+bg)
      ga4g  = gammagce(4.+bg)
      ga5gh = gammagce((5.+bg)/2.)
      ga3d  = gammagce(3.+bs)
      ga4d  = gammagce(4.+bs)
      ga5dh = gammagce((5.+bs)/2.)

      ac1=aw

      ac2=ag
      ac3=as

      bc1=bw
      cc1=as
      dc1=bs
      zrc=(cpi*roqr*tnw)**0.25
      zsc=(cpi*roqs*tns)**0.25
      zgc=(cpi*roqg*tng)**0.25
      vrc=aw*ga4b/(6.*zrc**bw)
      vsc=as*ga4d/(6.*zsc**bs)
      vgc=ag*ga4g/(6.*zgc**bg)


      rn1=9.4e-15 
      bnd1=6.e-4
      rn2=1.e-3


      bnd2=2.0e-3 
      rn3=.25*cpi*tns*cc1*ga3d
      esw=1.
      rn4=.25*cpi*esw*tns*cc1*ga3d

      eri=.1  
      rn5=.25*cpi*eri*tnw*ac1*ga3b

      ami=1./(24.*6.e-9) 
      rn6=cpi2*eri*tnw*ac1*roqr*ga6b*ami

      esr=.5 
      rn7=cpi2*esr*tnw*tns*roqs
      esr=1.
      rn8=cpi2*esr*tnw*tns*roqr
      rn9=cpi2*tns*tng*roqs
      rn10=2.*cpi*tns
      rn101=.31*ga5dh*sqrt(cc1)
      rn10a=als*als/rw

       rn10b=alv/tca
       rn10c=ars/(dwv*amw)

      rn11=2.*cpi*tns/alf
      rn11a=cw/alf

      ami50=3.84e-6 

      ami40=3.08e-8 
      eiw=1.

      ui50=100. 
      ri50=2.*5.e-3
      cmn=1.05e-15
      rn12=cpi*eiw*ui50*ri50**2

      do 10 k=1,31
         y1=1.-aa2(k)
         rn13(k)=aa1(k)*y1/(ami50**y1-ami40**y1)
         rn12a(k)=rn13(k)/ami50
         rn12b(k)=aa1(k)*ami50**aa2(k)
         rn25a(k)=aa1(k)*cmn**aa2(k)
   10 continue

      egw=1.
      rn14=.25*cpi*egw*tng*ga3g*ag
      egi=.1
      rn15=.25*cpi*egi*tng*ga3g*ag
      egi=1.
      rn15a=.25*cpi*egi*tng*ga3g*ag
      egr=1.
      rn16=cpi2*egr*tng*tnw*roqr
      rn17=2.*cpi*tng
      rn17a=.31*ga5gh*sqrt(ag)
      rn17b=cw-ci
      rn17c=cw
      apri=.66
      bpri=1.e-4
      bpri=0.5*bpri 
      rn18=20.*cpi2*bpri*tnw*roqr
      rn18a=apri
      rn19=2.*cpi*tng/alf
      rn19a=.31*ga5gh*sqrt(ag)
      rn19b=cw/alf

       rnn191=.78
       rnn192=.31*ga5gh*sqrt(ac2/dva)

      rn20=2.*cpi*tng
      rn20a=als*als/rw
      rn20b=.31*ga5gh*sqrt(ag)
      bnd3=2.e-3
      rn21=1.e3*1.569e-12/0.15
      erw=1.
      rn22=.25*cpi*erw*ac1*tnw*ga3b
      rn23=2.*cpi*tnw
      rn23a=.31*ga5bh*sqrt(ac1)
      rn23b=alv*alv/rw








       if (itaobraun .eq. 0) then
         cn0=1.e-8
         beta=-.6
       elseif (itaobraun .eq. 1) then
         cn0=1.e-6
         beta=-.46
       endif






      rn25=cn0
      rn30a=alv*als*amw/(tca*ars)
      rn30b=alv/tca
      rn30c=ars/(dwv*amw)
      rn31=1.e-17

      rn32=4.*51.545e-4

      rn30=2.*cpi*tng
      rnn30a=alv*alv*amw/(tca*ars)

      rn33=4.*tns
       rn331=.65
       rn332=.44*sqrt(ac3/dva)*ga5dh


    return
 END SUBROUTINE consat_s

 SUBROUTINE saticel_s (dt, ihail, itaobraun, ice2, istatmin, &
                       new_ice_sat, id, &
                       ptwrf, qvwrf, qlwrf, qrwrf, &
                       qiwrf, qswrf, qgwrf, &
                       rho_mks, pi_mks, p0_mks,itimestep, &
                       refl_10cm, diagflag, do_radar_ref,           & 

                       ids,ide, jds,jde, kds,kde, &
                       ims,ime, jms,jme, kms,kme, &
                       its,ite, jts,jte, kts,kte  &
                           )
    IMPLICIT NONE




































  integer,    parameter ::  nt=2880, nt2=2*nt 


  integer  ::   itaobraun,ice2,ihail,new_ice_sat,id,istatmin
  integer  ::   itimestep
  real     ::   tairccri, cn0, dt































  integer ids,ide,jds,jde,kds,kde
  integer ims,ime,jms,jme,kms,kme
  integer its,ite,jts,jte,kts,kte
  integer i,j,k, kp

  real :: a0 ,a1 ,a2 ,afcp ,alvr ,ami100 ,ami40 ,ami50 ,ascp ,avcp ,betah &
   ,bg3 ,bgh5 ,bs3 ,bs6 ,bsh5 ,bw3 ,bw6 ,bwh5 ,cmin ,cmin1 ,cmin2 ,cp409 &
   ,cp580 ,cs580 ,cv409 ,d2t ,del ,dwvp ,ee1 ,ee2 ,f00 ,f2 ,f3 ,ft ,fv0 ,fvs &
   ,pi0 ,pir ,pr0 ,qb0 ,r00 ,r0s ,r101f ,r10ar ,r10t ,r11at ,r11rt ,r12r ,r14f &
   ,r14r ,r15af ,r15ar ,r15f ,r15r ,r16r ,r17aq ,r17as ,r17r ,r18r ,r19aq ,r19as &
   ,r19bt ,r19rt ,r20bq ,r20bs ,r20t ,r22f ,r23af ,r23br ,r23t ,r25a ,r25rt ,r2ice &
   ,r31r ,r32rt ,r3f ,r4f ,r5f ,r6f ,r7r ,r8r ,r9r ,r_nci ,rft ,rijl2 ,rp0 ,rr0 &
   ,rrq ,rrs ,rt0 ,scc ,sccc ,sddd ,see ,seee ,sfff ,smmm ,ssss ,tb0 ,temp ,ucog &
   ,ucor ,ucos ,uwet ,vgcf ,vgcr ,vrcf ,vscf ,zgr ,zrr ,zsr


  real, dimension (its:ite,jts:jte,kts:kte) ::  fv
  real, dimension (its:ite,jts:jte,kts:kte) ::  dpt, dqv
  real, dimension (its:ite,jts:jte,kts:kte) ::  qcl, qrn,      &
                                                qci, qcs, qcg












  real, dimension (ims:ime, kms:kme, jms:jme) ::  ptwrf, qvwrf 
  real, dimension (ims:ime, kms:kme, jms:jme) ::  qlwrf, qrwrf,        &
                                                  qiwrf, qswrf, qgwrf











  real, dimension (ims:ime, kms:kme, jms:jme) ::  rho_mks
  real, dimension (ims:ime, kms:kme, jms:jme) ::  pi_mks
  real, dimension (ims:ime, kms:kme, jms:jme) ::  p0_mks






  real, dimension (its:ite,jts:jte) ::        &
           vg,      zg,       &
           ps,      pg,       &
          prn,     psn,       &
        pwacs,   wgacr,       &
        pidep,    pint,       &
          qsi,     ssi,       &
          esi,     esw,       &
          qsw,      pr,       &
          ssw,   pihom,       &
         pidw,   pimlt,       &
        psaut,   qracs,       &
        psaci,   psacw,       &
        qsacw,   praci,       &
        pmlts,   pmltg,       &
        asss,       y1,    y2



  real, dimension (its:ite,jts:jte) ::        &
        praut,   pracw,       &
         psfw,    psfi,       &
        dgacs,   dgacw,       &
        dgaci,   dgacr,       &
        pgacs,   wgacs,       &
        qgacw,   wgaci,       &
        qgacr,   pgwet,       &
        pgaut,   pracs,       &
        psacr,   qsacr,       &
         pgfr,   psmlt,       &
        pgmlt,   psdep,       &
        pgdep,   piacr,       &
           y5,     scv,       &
          tca,     dwv,       &
          egs,      y3,       &
           y4,     ddb


  real, dimension (its:ite,jts:jte) ::        &
           pt,      qv,       &
           qc,      qr,       &
           qi,      qs,       &
           qg,    tair,       &
        tairc,   rtair,       &
          dep,      dd,       &
          dd1,     qvs,       &
           dm,      rq,       &
        rsub1,     col,       &
          cnd,     ern,       &
         dlt1,    dlt2,       &
         dlt3,    dlt4,       &
           zr,      vr,       &
           zs,      vs,       &
                 pssub,       &
        pgsub,     dda


  real, dimension (its:ite,jts:jte,kts:kte) ::  rho
  real, dimension (kts:kte) ::                 & 
           tb,      qb,    rho1,              &
           ta,      qa,     ta1,     qa1,     &
         coef,      z1,      z2,      z3,     &
           am,     am1,      ub,      vb,     &
           wb,     ub1,     vb1,    rrho,     &
        rrho1,     wbx


  real, dimension (its:ite,jts:jte,kts:kte) ::  p0, pi, f0
  real, dimension (kts:kte) ::    & 
           fd,      fe,        &
           st,      sv,        &
           sq,      sc,        &
           se,     sqa


  real, dimension (kts:kte) ::    & 
         srro,    qrro,    sqc,    sqr,    &
          sqi,     sqs,    sqg,   stqc,    &
         stqr,    stqi,   stqs,   stqg
  real, dimension (nt) ::    & 
          tqc,     tqr,    tqi,    tqs,    tqg




  real, dimension (ims:ime,jms:jme) ::     &
           y0,     ts0,   qss0


  integer, dimension (its:ite,jts:jte) ::        it  
  integer, dimension (its:ite,jts:jte, 4) ::    ics 

  integer :: i24h
  integer :: iwarm
  real :: r2is, r2ig
  






























  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT):: refl_10cm  




      REAL, DIMENSION(kts:kte):: qv1d, t1d, p1d, qr1d, qs1d, qg1d, dBZ
      LOGICAL, OPTIONAL, INTENT(IN) :: diagflag
      INTEGER, OPTIONAL, INTENT(IN) :: do_radar_ref






















      do k=kts,kte
         do j=jts,jte
         do i=its,ite
         rho(i,j,k)=rho_mks(i,k,j)*0.001
         p0(i,j,k)=p0_mks(i,k,j)*10.0
         pi(i,j,k)=pi_mks(i,k,j)
         dpt(i,j,k)=ptwrf(i,k,j)
         dqv(i,j,k)=qvwrf(i,k,j)
         qcl(i,j,k)=qlwrf(i,k,j)
         qrn(i,j,k)=qrwrf(i,k,j)
         qci(i,j,k)=qiwrf(i,k,j)
         qcs(i,j,k)=qswrf(i,k,j)
         qcg(i,j,k)=qgwrf(i,k,j)









         enddo 
         enddo 
      enddo 

      do k=kts,kte
         do j=jts,jte
         do i=its,ite
         fv(i,j,k)=sqrt(rho(i,j,2)/rho(i,j,k))
         enddo 
         enddo 
      enddo 









         d2t=dt



        itaobraun=1 

       if ( itaobraun.eq.0 ) then
          cn0=1.e-8

       elseif ( itaobraun.eq.1 ) then
          cn0=1.e-6


       endif










         r2ig=1.
         r2is=1.
          if (ice2 .eq. 1) then


              r2ig=0.
              r2is=1.
          endif
          if (ice2 .eq. 2) then


              r2ig=1.
              r2is=0.
          endif

     


    iwarm = 0
    if (ice2 .eq. 3 ) iwarm = 1



      cmin=1.e-19
      cmin1=1.e-20
      cmin2=1.e-12
      ucor=3071.29/tnw**0.75
      ucos=687.97*roqs**0.25/tns**0.75
      ucog=687.97*roqg**0.25/tng**0.75
      uwet=4.464**0.95

      rijl2 = 1. / (ide-ids) / (jde-jds)





       do j=jts,jte
          do i=its,ite
          it(i,j)=1
          enddo
       enddo

      f2=rd1*d2t
      f3=rd2*d2t

      ft=dt/d2t
      rft=rijl2*ft
      a0=.5*istatmin*rijl2
      rt0=1./(t0-t00)
      bw3=bw+3.
      bs3=bs+3.
      bg3=bg+3.
      bsh5=2.5+bsh
      bgh5=2.5+bgh
      bwh5=2.5+bwh
      bw6=bw+6.
      bs6=bs+6.
      betah=.5*beta
      r10t=rn10*d2t
      r11at=rn11a*d2t
      r19bt=rn19b*d2t
      r20t=-rn20*d2t
      r23t=-rn23*d2t
      r25a=rn25


       ami50=3.76e-8
       ami100=1.51e-7
       ami40=2.41e-8




      do 1000 k=kts,kte
         kp=k+1


         tb0=0.
         qb0=0.

      do 2000 j=jts,jte
         do 2000 i=its,ite

         rp0=3.799052e3/p0(i,j,k)
         pi0=pi(i,j,k)
         pir=1./(pi(i,j,k))
         pr0=1./p0(i,j,k)
         r00=rho(i,j,k)
         r0s=sqrt(rho(i,j,k))

         rr0=1./rho(i,j,k)

         rrs=sqrt(rr0)

         rrq=sqrt(rrs)
         f0(i,j,k)=al/cp/pi(i,j,k)
         f00=f0(i,j,k)
         fv0=fv(i,j,k)
         fvs=sqrt(fv(i,j,k))
         zrr=1.e5*zrc*rrq
         zsr=1.e5*zsc*rrq
         zgr=1.e5*zgc*rrq
         cp409=c409*pi0
         cv409=c409*avc
         cp580=c580*pi0
         cs580=c580*asc
         alvr=r00*alv
         afcp=afc*pir
         avcp=avc*pir
         ascp=asc*pir
         vrcf=vrc*fv0
         vscf=vsc*fv0
         vgcf=vgc*fv0
         vgcr=vgc*rrs
         dwvp=c879*pr0
         r3f=rn3*fv0
         r4f=rn4*fv0
         r5f=rn5*fv0
         r6f=rn6*fv0
         r7r=rn7*rr0
         r8r=rn8*rr0
         r9r=rn9*rr0
         r101f=rn101*fvs
         r10ar=rn10a*r00
         r11rt=rn11*rr0*d2t
         r12r=rn12*r00
         r14r=rn14*rrs
         r14f=rn14*fv0
         r15r=rn15*rrs
         r15ar=rn15a*rrs
         r15f=rn15*fv0
         r15af=rn15a*fv0
         r16r=rn16*rr0
         r17r=rn17*rr0
         r17aq=rn17a*rrq
         r17as=rn17a*fvs
         r18r=rn18*rr0
         r19rt=rn19*rr0*d2t
         r19aq=rn19a*rrq
         r19as=rn19a*fvs
         r20bq=rn20b*rrq
         r20bs=rn20b*fvs
         r22f=rn22*fv0
         r23af=rn23a*fvs
         r23br=rn23b*r00
         r25rt=rn25*rr0*d2t
         r31r=rn31*rr0
         r32rt=rn32*d2t*rrs



        pt(i,j)=dpt(i,j,k)
        qv(i,j)=dqv(i,j,k)
        qc(i,j)=qcl(i,j,k)
        qr(i,j)=qrn(i,j,k)
        qi(i,j)=qci(i,j,k)
        qs(i,j)=qcs(i,j,k)
        qg(i,j)=qcg(i,j,k)

         if (qc(i,j) .le.  cmin1) qc(i,j)=0.0
         if (qr(i,j) .le.  cmin1) qr(i,j)=0.0
         if (qi(i,j) .le.  cmin1) qi(i,j)=0.0
         if (qs(i,j) .le.  cmin1) qs(i,j)=0.0
         if (qg(i,j) .le.  cmin1) qg(i,j)=0.0
        tair(i,j)=(pt(i,j)+tb0)*pi0
        tairc(i,j)=tair(i,j)-t0
         zr(i,j)=zrr
         zs(i,j)=zsr
         zg(i,j)=zgr
         vr(i,j)=0.0
         vs(i,j)=0.0
         vg(i,j)=0.0


    IF (IWARM .EQ. 1) THEN

                qi(i,j)=0.0
                qs(i,j)=0.0
                qg(i,j)=0.0
                dep(i,j)=0.
                pint(i,j)=0.
                psdep(i,j)=0.
                pgdep(i,j)=0.
                dd1(i,j)=0.
                pgsub(i,j)=0.
                psmlt(i,j)=0.
                pgmlt(i,j)=0.
                pimlt(i,j)=0.
                psacw(i,j)=0.
                piacr(i,j)=0.
                psfw(i,j)=0.
                pgfr(i,j)=0.
                dgacw(i,j)=0.
                dgacr(i,j)=0.
                psacr(i,j)=0.
                wgacr(i,j)=0.
                pihom(i,j)=0.
                pidw(i,j)=0.

                if (qr(i,j) .gt. cmin1) then
                   dd(i,j)=r00*qr(i,j)
                   y1(i,j)=dd(i,j)**.25
                   zr(i,j)=zrc/y1(i,j)
                   vr(i,j)=max(vrcf*dd(i,j)**bwq, 0.)
                endif



                pracw(i,j)=0.
                praut(i,j)=0.0
                pracw(i,j)=r22f*qc(i,j)/zr(i,j)**bw3
                y1(i,j)=qc(i,j)-bnd3
                if (y1(i,j).gt.0.0) then
                    praut(i,j)=r00*y1(i,j)*y1(i,j)/(1.2e-4+rn21/y1(i,j))
                 endif


                 Y1(I,J)=QC(I,J)/D2T
                 PRAUT(I,J)=MIN(Y1(I,J), PRAUT(I,J))
                 PRACW(I,J)=MIN(Y1(I,J), PRACW(I,J))
                 Y1(I,J)=(PRAUT(I,J)+PRACW(I,J))*D2T
               
               if (qc(i,j) .lt. y1(i,j) .and. y1(i,j) .ge. cmin2) then
                   y2(i,j)=qc(i,j)/(y1(i,j)+cmin2)
                   praut(i,j)=praut(i,j)*y2(i,j)
                   pracw(i,j)=pracw(i,j)*y2(i,j)
                   qc(i,j)=0.0
               else
                  qc(i,j)=qc(i,j)-y1(i,j)
               endif
               
               PR(I,J)=(PRAUT(I,J)+PRACW(I,J))*D2T
               QR(I,J)=QR(I,J)+PR(I,J)
                        

           
           cnd(i,j)=0.0
           tair(i,j)=(pt(i,j)+tb0)*pi0
              y1(i,j)=1./(tair(i,j)-c358)
              qsw(i,j)=rp0*exp(c172-c409*y1(i,j))
              dd(i,j)=cp409*y1(i,j)*y1(i,j)
              dm(i,j)=qv(i,j)+qb0-qsw(i,j)
              cnd(i,j)=dm(i,j)/(1.+avcp*dd(i,j)*qsw(i,j))

              cnd(i,j)=max(-qc(i,j), cnd(i,j))
                         pt(i,j)=pt(i,j)+avcp*cnd(i,j)
             qv(i,j)=qv(i,j)-cnd(i,j)
                         qc(i,j)=qc(i,j)+cnd(i,j)



            ern(i,j)=0.0

            if(qr(i,j).gt.0.0) then
               tair(i,j)=(pt(i,j)+tb0)*pi0
               rtair(i,j)=1./(tair(i,j)-c358)
               qsw(i,j)=rp0*exp(c172-c409*rtair(i,j))
               ssw(i,j)=(qv(i,j)+qb0)/qsw(i,j)-1.0
               dm(i,j)=qv(i,j)+qb0-qsw(i,j)
               rsub1(i,j)=cv409*qsw(i,j)*rtair(i,j)*rtair(i,j)
               dd1(i,j)=max(-dm(i,j)/(1.+rsub1(i,j)), 0.0)
               y1(i,j)=.78/zr(i,j)**2+r23af*scv(i,j)/zr(i,j)**bwh5
               y2(i,j)=r23br/(tca(i,j)*tair(i,j)**2)+1./(dwv(i,j) &
                       *qsw(i,j))

               ern(i,j)=r23t*ssw(i,j)*y1(i,j)/y2(i,j)
               ern(i,j)=min(dd1(i,j),qr(i,j),max(ern(i,j),0.))
               pt(i,j)=pt(i,j)-avcp*ern(i,j)
               qv(i,j)=qv(i,j)+ern(i,j)
               qr(i,j)=qr(i,j)-ern(i,j)
            endif

       ELSE       






            if (qr(i,j) .gt. cmin1) then
               dd(i,j)=r00*qr(i,j)
               y1(i,j)=dd(i,j)**.25
               zr(i,j)=zrc/y1(i,j)
               vr(i,j)=max(vrcf*dd(i,j)**bwq, 0.)
            endif

            if (qs(i,j) .gt. cmin1) then
               dd(i,j)=r00*qs(i,j)
               y1(i,j)=dd(i,j)**.25
               zs(i,j)=zsc/y1(i,j)
               vs(i,j)=max(vscf*dd(i,j)**bsq, 0.)
            endif

            if (qg(i,j) .gt. cmin1) then
               dd(i,j)=r00*qg(i,j)
               y1(i,j)=dd(i,j)**.25
               zg(i,j)=zgc/y1(i,j)
               if(ihail .eq. 1) then
                  vg(i,j)=max(vgcr*dd(i,j)**bgq, 0.)
               else
                  vg(i,j)=max(vgcf*dd(i,j)**bgq, 0.)
               endif
            endif

            if (qr(i,j) .le. cmin2) vr(i,j)=0.0
            if (qs(i,j) .le. cmin2) vs(i,j)=0.0
            if (qg(i,j) .le. cmin2) vg(i,j)=0.0







            y1(i,j)=c149*tair(i,j)**1.5/(tair(i,j)+120.)
            dwv(i,j)=dwvp*tair(i,j)**1.81
            tca(i,j)=c141*y1(i,j)
            scv(i,j)=1./((rr0*y1(i,j))**.1666667*dwv(i,j)**.3333333)










            psaut(i,j)=0.0
            psaci(i,j)=0.0
            praci(i,j)=0.0
            piacr(i,j)=0.0
            psacw(i,j)=0.0
            qsacw(i,j)=0.0
            dd(i,j)=1./zs(i,j)**bs3

            if (tair(i,j).lt.t0) then
               esi(i,j)=exp(.025*tairc(i,j))
               psaut(i,j)=r2is*max(rn1*esi(i,j)*(qi(i,j)-bnd1) ,0.0)
               psaci(i,j)=r2is*r3f*esi(i,j)*qi(i,j)*dd(i,j)



               psacw(i,j)=r2is*0.5*r4f*qc(i,j)*dd(i,j)

               praci(i,j)=r2is*r5f*qi(i,j)/zr(i,j)**bw3
               piacr(i,j)=r2is*r6f*qi(i,j)*(zr(i,j)**(-bw6))

            else
               qsacw(i,j)=r2is*r4f*qc(i,j)*dd(i,j)
            endif




            pracw(i,j)=r22f*qc(i,j)/zr(i,j)**bw3
            praut(i,j)=0.0
            y1(i,j)=qc(i,j)-bnd3
            if (y1(i,j).gt.0.0) then
               praut(i,j)=r00*y1(i,j)*y1(i,j)/(1.2e-4+rn21/y1(i,j))
            endif




            psfw(i,j)=0.0
            psfi(i,j)=0.0
            pidep(i,j)=0.0

            if(tair(i,j).lt.t0.and.qi(i,j).gt.cmin) then
               y1(i,j)=max( min(tairc(i,j), -1.), -31.)
               it(i,j)=int(abs(y1(i,j)))
               y1(i,j)=rn12a(it(i,j))
               y2(i,j)=rn12b(it(i,j))

          psfw(i,j)=r2is* &
                    max(d2t*y1(i,j)*(y2(i,j)+r12r*qc(i,j))*qi(i,j),0.0)
               rtair(i,j)=1./(tair(i,j)-c76)
               y2(i,j)=exp(c218-c580*rtair(i,j))
               qsi(i,j)=rp0*y2(i,j)
               esi(i,j)=c610*y2(i,j)
               ssi(i,j)=(qv(i,j)+qb0)/qsi(i,j)-1.
               r_nci=min(1.e-6*exp(-.46*tairc(i,j)),1.)

               dm(i,j)=max( (qv(i,j)+qb0-qsi(i,j)), 0.)
               rsub1(i,j)=cs580*qsi(i,j)*rtair(i,j)*rtair(i,j)
               y3(i,j)=1./tair(i,j)
          dd(i,j)=y3(i,j)*(rn30a*y3(i,j)-rn30b)+rn30c*tair(i,j)/esi(i,j)
               y1(i,j)=206.18*ssi(i,j)/dd(i,j)
               pidep(i,j)=y1(i,j)*sqrt(r_nci*qi(i,j)/r00)
               dep(i,j)=dm(i,j)/(1.+rsub1(i,j))/d2t
               if(dm(i,j).gt.cmin2) then
                  a2=1.
                if(pidep(i,j).gt.dep(i,j).and.pidep(i,j).gt.cmin2) then
                     a2=dep(i,j)/pidep(i,j)
                     pidep(i,j)=dep(i,j)
                endif
                  psfi(i,j)=r2is*a2*.5*qi(i,j)*y1(i,j)/(sqrt(ami100) &
                          -sqrt(ami40))
                  elseif(dm(i,j).lt.-cmin2) then




                  pidep(i,j)=0.
                  psfi(i,j)=0.
               else
                  pidep(i,j)=0.
                  psfi(i,j)=0.
               endif
            endif






            if(qc(i,j)+qr(i,j).lt.1.e-4) then
               ee1=.01
              else
                 ee1=1.
              endif
            ee2=0.09
            egs(i,j)=ee1*exp(ee2*tairc(i,j))

            if (tair(i,j).ge.t0) egs(i,j)=1.0
            y1(i,j)=abs(vg(i,j)-vs(i,j))
            y2(i,j)=zs(i,j)*zg(i,j)
            y3(i,j)=5./y2(i,j)
            y4(i,j)=.08*y3(i,j)*y3(i,j)
            y5(i,j)=.05*y3(i,j)*y4(i,j)
            dd(i,j)=y1(i,j)*(y3(i,j)/zs(i,j)**5+y4(i,j)/zs(i,j)**3 &
                    +y5(i,j)/zs(i,j))
            pgacs(i,j)=r2ig*r2is*r9r*egs(i,j)*dd(i,j)

            if (ihail.eq.1) then
               dgacs(i,j)=pgacs(i,j)
            else
               dgacs(i,j)=0.
            endif

            wgacs(i,j)=r2ig*r2is*r9r*dd(i,j)

            y1(i,j)=1./zg(i,j)**bg3

            if(ihail .eq. 1) then
               dgacw(i,j)=r2ig*max(r14r*qc(i,j)*y1(i,j), 0.0)
            else
               dgacw(i,j)=r2ig*max(r14f*qc(i,j)*y1(i,j), 0.0)
            endif

            qgacw(i,j)=dgacw(i,j)
            y1(i,j)=abs(vg(i,j)-vr(i,j))
            y2(i,j)=zr(i,j)*zg(i,j)
            y3(i,j)=5./y2(i,j)
            y4(i,j)=.08*y3(i,j)*y3(i,j)
            y5(i,j)=.05*y3(i,j)*y4(i,j)
            dd(i,j)=r16r*y1(i,j)*(y3(i,j)/zr(i,j)**5+y4(i,j)/zr(i,j)**3 &
                    +y5(i,j)/zr(i,j))
            dgacr(i,j)=r2ig*max(dd(i,j), 0.0)
            qgacr(i,j)=dgacr(i,j)

            if (tair(i,j).ge.t0) then
               dgacs(i,j)=0.0
               wgacs(i,j)=0.0
               dgacw(i,j)=0.0
               dgacr(i,j)=0.0
            else
               pgacs(i,j)=0.0
               qgacw(i,j)=0.0
               qgacr(i,j)=0.0
            endif





            dgaci(i,j)=0.0
            wgaci(i,j)=0.0
            pgwet(i,j)=0.0

            if (tair(i,j).lt.t0) then
               y1(i,j)=qi(i,j)/zg(i,j)**bg3
               if (ihail.eq.1) then
                  dgaci(i,j)=r2ig*r15r*y1(i,j)
                  wgaci(i,j)=r2ig*r15ar*y1(i,j)

               else


                   dgaci(i,j)=0.
                  wgaci(i,j)=r2ig*r15af*y1(i,j)

               endif

               if (tairc(i,j).ge.-50.) then
                if (alf+rn17c*tairc(i,j) .eq. 0.) then
                   write(91,*) itimestep, i,j,k, alf, rn17c, tairc(i,j)
                endif
                y1(i,j)=1./(alf+rn17c*tairc(i,j))
                if (ihail.eq.1) then
                   y3(i,j)=.78/zg(i,j)**2+r17aq*scv(i,j)/zg(i,j)**bgh5
                else
                   y3(i,j)=.78/zg(i,j)**2+r17as*scv(i,j)/zg(i,j)**bgh5
                endif
                y4(i,j)=alvr*dwv(i,j)*(rp0-(qv(i,j)+qb0)) &
                        -tca(i,j)*tairc(i,j)
                dd(i,j)=y1(i,j)*(r17r*y4(i,j)*y3(i,j) &
                       +(wgaci(i,j)+wgacs(i,j))*(alf+rn17b*tairc(i,j)))
                pgwet(i,j)=r2ig*max(dd(i,j), 0.0)
               endif
            endif








            y1(i,j)=qc(i,j)/d2t
            psacw(i,j)=min(y1(i,j), psacw(i,j))
            praut(i,j)=min(y1(i,j), praut(i,j))
            pracw(i,j)=min(y1(i,j), pracw(i,j))
            psfw(i,j)= min(y1(i,j), psfw(i,j))
            dgacw(i,j)=min(y1(i,j), dgacw(i,j))
            qsacw(i,j)=min(y1(i,j), qsacw(i,j))
            qgacw(i,j)=min(y1(i,j), qgacw(i,j))

            y1(i,j)=(psacw(i,j)+praut(i,j)+pracw(i,j)+psfw(i,j) &
                    +dgacw(i,j)+qsacw(i,j)+qgacw(i,j))*d2t
            qc(i,j)=qc(i,j)-y1(i,j)

            if (qc(i,j) .lt. 0.0) then
               a1=1.
               if (y1(i,j) .ne. 0.0) a1=qc(i,j)/y1(i,j)+1.
               psacw(i,j)=psacw(i,j)*a1
               praut(i,j)=praut(i,j)*a1
               pracw(i,j)=pracw(i,j)*a1
               psfw(i,j)=psfw(i,j)*a1
               dgacw(i,j)=dgacw(i,j)*a1
               qsacw(i,j)=qsacw(i,j)*a1
               qgacw(i,j)=qgacw(i,j)*a1
               qc(i,j)=0.0
            endif




            wgacr(i,j)=pgwet(i,j)-dgacw(i,j)-wgaci(i,j)-wgacs(i,j)
            y2(i,j)=dgacw(i,j)+dgaci(i,j)+dgacr(i,j)+dgacs(i,j)
            if (pgwet(i,j).ge.y2(i,j)) then
               wgacr(i,j)=0.0
               wgaci(i,j)=0.0
               wgacs(i,j)=0.0
            else
               dgacr(i,j)=0.0
               dgaci(i,j)=0.0
               dgacs(i,j)=0.0
            endif


            y1(i,j)=qi(i,j)/d2t
            psaut(i,j)=min(y1(i,j), psaut(i,j))
            psaci(i,j)=min(y1(i,j), psaci(i,j))
            praci(i,j)=min(y1(i,j), praci(i,j))
            psfi(i,j)= min(y1(i,j), psfi(i,j))
            dgaci(i,j)=min(y1(i,j), dgaci(i,j))
            wgaci(i,j)=min(y1(i,j), wgaci(i,j))

            y2(i,j)=(psaut(i,j)+psaci(i,j)+praci(i,j)+psfi(i,j) &
                   +dgaci(i,j)+wgaci(i,j))*d2t
            qi(i,j)=qi(i,j)-y2(i,j)+pidep(i,j)*d2t

            if (qi(i,j).lt.0.0) then
               a2=1.
               if (y2(i,j) .ne. 0.0) a2=qi(i,j)/y2(i,j)+1.
               psaut(i,j)=psaut(i,j)*a2
               psaci(i,j)=psaci(i,j)*a2
               praci(i,j)=praci(i,j)*a2
               psfi(i,j)=psfi(i,j)*a2
               dgaci(i,j)=dgaci(i,j)*a2
               wgaci(i,j)=wgaci(i,j)*a2
               qi(i,j)=0.0
            endif

            dlt3(i,j)=0.0
            dlt2(i,j)=0.0










            if (tair(i,j).lt.t0) then
               if (qr(i,j).lt.1.e-4) then
                  dlt3(i,j)=1.0
                  dlt2(i,j)=1.0
               endif
               if (qs(i,j).ge.1.e-4) then
                  dlt2(i,j)=0.0
               endif
            endif

            if (ice2 .eq. 1) then
                  dlt3(i,j)=1.0
                  dlt2(i,j)=1.0
            endif

            pr(i,j)=(qsacw(i,j)+praut(i,j)+pracw(i,j)+qgacw(i,j))*d2t
            ps(i,j)=(psaut(i,j)+psaci(i,j)+psacw(i,j)+psfw(i,j) &
                    +psfi(i,j)+dlt3(i,j)*praci(i,j))*d2t


            pg(i,j)=((1.-dlt3(i,j))*praci(i,j)+dgaci(i,j)+wgaci(i,j) &
                    +dgacw(i,j))*d2t











            y1(i,j)=abs(vr(i,j)-vs(i,j))
            y2(i,j)=zr(i,j)*zs(i,j)
            y3(i,j)=5./y2(i,j)
            y4(i,j)=.08*y3(i,j)*y3(i,j)
            y5(i,j)=.05*y3(i,j)*y4(i,j)
            pracs(i,j)=r2ig*r2is*r7r*y1(i,j)*(y3(i,j)/zs(i,j)**5 &
                      +y4(i,j)/zs(i,j)**3+y5(i,j)/zs(i,j))
            psacr(i,j)=r2is*r8r*y1(i,j)*(y3(i,j)/zr(i,j)**5 &
                      +y4(i,j)/zr(i,j)**3+y5(i,j)/zr(i,j))
            qsacr(i,j)=psacr(i,j)

            if (tair(i,j).ge.t0) then
               pracs(i,j)=0.0
               psacr(i,j)=0.0
            else
               qsacr(i,j)=0.0
            endif




            pgaut(i,j)=0.0
            pgfr(i,j)=0.0

            if (tair(i,j) .lt. t0) then



               y2(i,j)=exp(rn18a*(t0-tair(i,j)))




               temp = 1./zr(i,j)
               temp = temp*temp*temp*temp*temp*temp*temp
               pgfr(i,j)=r2ig*max(r18r*(y2(i,j)-1.)* &
                                    temp, 0.0)
            endif









            y1(i,j)=qr(i,j)/d2t
            y2(i,j)=-qg(i,j)/d2t
            piacr(i,j)=min(y1(i,j), piacr(i,j))
            dgacr(i,j)=min(y1(i,j), dgacr(i,j))
            wgacr(i,j)=min(y1(i,j), wgacr(i,j))
            wgacr(i,j)=max(y2(i,j), wgacr(i,j))
            psacr(i,j)=min(y1(i,j), psacr(i,j))
            pgfr(i,j)= min(y1(i,j), pgfr(i,j))
            del=0.
            if(wgacr(i,j) .lt. 0.) del=1.
            y1(i,j)=(piacr(i,j)+dgacr(i,j)+(1.-del)*wgacr(i,j) &
                    +psacr(i,j)+pgfr(i,j))*d2t
            qr(i,j)=qr(i,j)+pr(i,j)-y1(i,j)-del*wgacr(i,j)*d2t
            if (qr(i,j) .lt. 0.0) then
               a1=1.
               if(y1(i,j) .ne. 0.) a1=qr(i,j)/y1(i,j)+1.
               piacr(i,j)=piacr(i,j)*a1
               dgacr(i,j)=dgacr(i,j)*a1
               if (wgacr(i,j).gt.0.) wgacr(i,j)=wgacr(i,j)*a1
               pgfr(i,j)=pgfr(i,j)*a1
               psacr(i,j)=psacr(i,j)*a1
               qr(i,j)=0.0
            endif

            prn(i,j)=d2t*((1.-dlt3(i,j))*piacr(i,j)+dgacr(i,j) &
                     +wgacr(i,j)+(1.-dlt2(i,j))*psacr(i,j)+pgfr(i,j))
            ps(i,j)=ps(i,j)+d2t*(dlt3(i,j)*piacr(i,j) &
                    +dlt2(i,j)*psacr(i,j))
            pracs(i,j)=(1.-dlt2(i,j))*pracs(i,j)
            y1(i,j)=qs(i,j)/d2t
            pgacs(i,j)=min(y1(i,j), pgacs(i,j))
            dgacs(i,j)=min(y1(i,j), dgacs(i,j))
            wgacs(i,j)=min(y1(i,j), wgacs(i,j))
            pgaut(i,j)=min(y1(i,j), pgaut(i,j))
            pracs(i,j)=min(y1(i,j), pracs(i,j))
            psn(i,j)=d2t*(pgacs(i,j)+dgacs(i,j)+wgacs(i,j) &
                     +pgaut(i,j)+pracs(i,j))
            qs(i,j)=qs(i,j)+ps(i,j)-psn(i,j)

            if (qs(i,j).lt.0.0) then
               a2=1.
               if (psn(i,j) .ne. 0.0) a2=qs(i,j)/psn(i,j)+1.
               pgacs(i,j)=pgacs(i,j)*a2
               dgacs(i,j)=dgacs(i,j)*a2
               wgacs(i,j)=wgacs(i,j)*a2
               pgaut(i,j)=pgaut(i,j)*a2
               pracs(i,j)=pracs(i,j)*a2
               psn(i,j)=psn(i,j)*a2
               qs(i,j)=0.0
            endif



            y2(i,j)=d2t*(psacw(i,j)+psfw(i,j)+dgacw(i,j)+piacr(i,j) &
                    +dgacr(i,j)+wgacr(i,j)+psacr(i,j)+pgfr(i,j))
            pt(i,j)=pt(i,j)+afcp*y2(i,j)
            qg(i,j)=qg(i,j)+pg(i,j)+prn(i,j)+psn(i,j)









            psmlt(i,j)=0.0
            pgmlt(i,j)=0.0
            tair(i,j)=(pt(i,j)+tb0)*pi0

            if (tair(i,j).ge.t0) then
               tairc(i,j)=tair(i,j)-t0
               y1(i,j)=tca(i,j)*tairc(i,j)-alvr*dwv(i,j) &
                               *(rp0-(qv(i,j)+qb0))
               y2(i,j)=.78/zs(i,j)**2+r101f*scv(i,j)/zs(i,j)**bsh5
               dd(i,j)=r11rt*y1(i,j)*y2(i,j)+r11at*tairc(i,j) &
                       *(qsacw(i,j)+qsacr(i,j))
               psmlt(i,j)=r2is*max(0.0, min(dd(i,j), qs(i,j)))

               if(ihail.eq.1) then
                  y3(i,j)=.78/zg(i,j)**2+r19aq*scv(i,j)/zg(i,j)**bgh5
               else
                  y3(i,j)=.78/zg(i,j)**2+r19as*scv(i,j)/zg(i,j)**bgh5
               endif

               dd1(i,j)=r19rt*y1(i,j)*y3(i,j)+r19bt*tairc(i,j) &
                        *(qgacw(i,j)+qgacr(i,j))
               pgmlt(i,j)=r2ig*max(0.0, min(dd1(i,j), qg(i,j)))
               pt(i,j)=pt(i,j)-afcp*(psmlt(i,j)+pgmlt(i,j))
               qr(i,j)=qr(i,j)+psmlt(i,j)+pgmlt(i,j)
               qs(i,j)=qs(i,j)-psmlt(i,j)
               qg(i,j)=qg(i,j)-pgmlt(i,j)
            endif





            if (qc(i,j).le.cmin1) qc(i,j)=0.0
            if (qi(i,j).le.cmin1) qi(i,j)=0.0
            tair(i,j)=(pt(i,j)+tb0)*pi0

            if(tair(i,j).le.t00) then
               pihom(i,j)=qc(i,j)
            else
               pihom(i,j)=0.0
            endif
            if(tair(i,j).ge.t0) then
               pimlt(i,j)=qi(i,j)
            else
               pimlt(i,j)=0.0
            endif
            pidw(i,j)=0.0

            if (tair(i,j).lt.t0 .and. tair(i,j).gt.t00) then
               tairc(i,j)=tair(i,j)-t0
               y1(i,j)=max( min(tairc(i,j), -1.), -31.)
               it(i,j)=int(abs(y1(i,j)))
               y2(i,j)=aa1(it(i,j))
               y3(i,j)=aa2(it(i,j))
               y4(i,j)=exp(abs(beta*tairc(i,j)))
               y5(i,j)=(r00*qi(i,j)/(r25a*y4(i,j)))**y3(i,j)
               pidw(i,j)=min(r25rt*y2(i,j)*y4(i,j)*y5(i,j), qc(i,j))
            endif

            y1(i,j)=pihom(i,j)-pimlt(i,j)+pidw(i,j)
            pt(i,j)=pt(i,j)+afcp*y1(i,j)+ascp*(pidep(i,j))*d2t
            qv(i,j)=qv(i,j)-(pidep(i,j))*d2t
            qc(i,j)=qc(i,j)-y1(i,j)
            qi(i,j)=qi(i,j)+y1(i,j)









           pint(i,j)=0.0

        if ( itaobraun.eq.1 ) then
            tair(i,j)=(pt(i,j)+tb0)*pi0
            if (tair(i,j) .lt. t0) then

              if (qi(i,j) .le. cmin2) qi(i,j)=0.
               tairc(i,j)=tair(i,j)-t0
               rtair(i,j)=1./(tair(i,j)-c76)
               y2(i,j)=exp(c218-c580*rtair(i,j))
              qsi(i,j)=rp0*y2(i,j)
               esi(i,j)=c610*y2(i,j)
              ssi(i,j)=(qv(i,j)+qb0)/qsi(i,j)-1.
                        ami50=3.76e-8




             y1(i,j)=1./tair(i,j)




             tairccri=tairc(i,j)          
             if(tairccri.le.-30.) tairccri=-30.


             y2(i,j)=exp(betah*tairccri)
             y3(i,j)=sqrt(qi(i,j))
             dd(i,j)=y1(i,j)*(rn10a*y1(i,j)-rn10b)+rn10c*tair(i,j) &
                                                /esi(i,j)
          pidep(i,j)=max(r32rt*ssi(i,j)*y2(i,j)*y3(i,j)/dd(i,j), 0.e0)

           r_nci=min(cn0*exp(beta*tairc(i,j)),1.)


           dd(i,j)=max(1.e-9*r_nci/r00-qi(i,j)*1.e-9/ami50, 0.)
                dm(i,j)=max( (qv(i,j)+qb0-qsi(i,j)), 0.0)
                rsub1(i,j)=cs580*qsi(i,j)*rtair(i,j)*rtair(i,j)
              dep(i,j)=dm(i,j)/(1.+rsub1(i,j))
              pint(i,j)=max(min(dd(i,j), dm(i,j)), 0.)


              pint(i,j)=min(pint(i,j)+pidep(i,j), dep(i,j))


               if (pint(i,j) .le. cmin2) pint(i,j)=0.
              pt(i,j)=pt(i,j)+ascp*pint(i,j)
              qv(i,j)=qv(i,j)-pint(i,j)
              qi(i,j)=qi(i,j)+pint(i,j)
           endif
        endif  



        if ( itaobraun.eq.0 ) then
             tair(i,j)=(pt(i,j)+tb0)*pi0
             if (tair(i,j) .lt. t0) then
               if (qi(i,j) .le. cmin1) qi(i,j)=0.
               tairc(i,j)=tair(i,j)-t0
               dd(i,j)=r31r*exp(beta*tairc(i,j))
               rtair(i,j)=1./(tair(i,j)-c76)
               y2(i,j)=exp(c218-c580*rtair(i,j))
               qsi(i,j)=rp0*y2(i,j)
               esi(i,j)=c610*y2(i,j)
               ssi(i,j)=(qv(i,j)+qb0)/qsi(i,j)-1.
               dm(i,j)=max( (qv(i,j)+qb0-qsi(i,j)), 0.)
               rsub1(i,j)=cs580*qsi(i,j)*rtair(i,j)*rtair(i,j)
               dep(i,j)=dm(i,j)/(1.+rsub1(i,j))
              pint(i,j)=max(min(dd(i,j), dm(i,j)), 0.)
               y1(i,j)=1./tair(i,j)
               y2(i,j)=exp(betah*tairc(i,j))
               y3(i,j)=sqrt(qi(i,j))
               dd(i,j)=y1(i,j)*(rn10a*y1(i,j)-rn10b) &
                     +rn10c*tair(i,j)/esi(i,j)
             pidep(i,j)=max(r32rt*ssi(i,j)*y2(i,j)*y3(i,j)/dd(i,j), 0.)
              pint(i,j)=pint(i,j)+pidep(i,j)
              pint(i,j)=min(pint(i,j),dep(i,j))

             pt(i,j)=pt(i,j)+ascp*pint(i,j)
             qv(i,j)=qv(i,j)-pint(i,j)
             qi(i,j)=qi(i,j)+pint(i,j)
            endif
        endif  






         if (new_ice_sat .eq. 0) then



               tair(i,j)=(pt(i,j)+tb0)*pi0
               cnd(i,j)=rt0*(tair(i,j)-t00)
               dep(i,j)=rt0*(t0-tair(i,j))
               y1(i,j)=1./(tair(i,j)-c358)
               y2(i,j)=1./(tair(i,j)-c76)
               qsw(i,j)=rp0*exp(c172-c409*y1(i,j))
               qsi(i,j)=rp0*exp(c218-c580*y2(i,j))
               dd(i,j)=cp409*y1(i,j)*y1(i,j)
               dd1(i,j)=cp580*y2(i,j)*y2(i,j)
               if (qc(i,j).le.cmin) qc(i,j)=cmin
               if (qi(i,j).le.cmin) qi(i,j)=cmin
               if (tair(i,j).ge.t0) then
                  dep(i,j)=0.0
                  cnd(i,j)=1.
                  qi(i,j)=0.0
               endif

               if (tair(i,j).lt.t00) then
                  cnd(i,j)=0.0
                  dep(i,j)=1.
                  qc(i,j)=0.0
               endif

               y5(i,j)=avcp*cnd(i,j)+ascp*dep(i,j)

               y1(i,j)=qc(i,j)*qsw(i,j)/(qc(i,j)+qi(i,j))
               y2(i,j)=qi(i,j)*qsi(i,j)/(qc(i,j)+qi(i,j))
               y4(i,j)=dd(i,j)*y1(i,j)+dd1(i,j)*y2(i,j)
               qvs(i,j)=y1(i,j)+y2(i,j)
               rsub1(i,j)=(qv(i,j)+qb0-qvs(i,j))/(1.+y4(i,j)*y5(i,j))
               cnd(i,j)=cnd(i,j)*rsub1(i,j)
               dep(i,j)=dep(i,j)*rsub1(i,j)
               if (qc(i,j).le.cmin) qc(i,j)=0.
               if (qi(i,j).le.cmin) qi(i,j)=0.



               cnd(i,j)=max(-qc(i,j),cnd(i,j))



               dep(i,j)=max(-qi(i,j),dep(i,j))

               pt(i,j)=pt(i,j)+avcp*cnd(i,j)+ascp*dep(i,j)
               qv(i,j)=qv(i,j)-cnd(i,j)-dep(i,j)
               qc(i,j)=qc(i,j)+cnd(i,j)
               qi(i,j)=qi(i,j)+dep(i,j)

         endif

         if (new_ice_sat .eq. 1) then




               tair(i,j)=(pt(i,j)+tb0)*pi0
               cnd(i,j)=rt0*(tair(i,j)-t00)
               dep(i,j)=rt0*(t0-tair(i,j))
               y1(i,j)=1./(tair(i,j)-c358)
               y2(i,j)=1./(tair(i,j)-c76)
               qsw(i,j)=rp0*exp(c172-c409*y1(i,j))
               qsi(i,j)=rp0*exp(c218-c580*y2(i,j))
               dd(i,j)=cp409*y1(i,j)*y1(i,j)
               dd1(i,j)=cp580*y2(i,j)*y2(i,j)
               y5(i,j)=avcp*cnd(i,j)+ascp*dep(i,j)
               y1(i,j)=rt0*(tair(i,j)-t00)*qsw(i,j)
               y2(i,j)=rt0*(t0-tair(i,j))*qsi(i,j)



               if (tair(i,j).ge.t0) then

                  dep(i,j)=0.0
                  cnd(i,j)=1.
                  y2(i,j)=0.
                  y1(i,j)=qsw(i,j)
               endif
               if (tair(i,j).lt.t00) then
                  cnd(i,j)=0.0
                  dep(i,j)=1.
                  y2(i,j)=qsi(i,j)
                  y1(i,j)=0.

               endif




               y4(i,j)=dd(i,j)*y1(i,j)+dd1(i,j)*y2(i,j)
               qvs(i,j)=y1(i,j)+y2(i,j)
               rsub1(i,j)=(qv(i,j)+qb0-qvs(i,j))/(1.+y4(i,j)*y5(i,j))
               cnd(i,j)=cnd(i,j)*rsub1(i,j)
               dep(i,j)=dep(i,j)*rsub1(i,j)





               cnd(i,j)=max(-qc(i,j),cnd(i,j))



               dep(i,j)=max(-qi(i,j),dep(i,j))

               pt(i,j)=pt(i,j)+avcp*cnd(i,j)+ascp*dep(i,j)
               qv(i,j)=qv(i,j)-cnd(i,j)-dep(i,j)
               qc(i,j)=qc(i,j)+cnd(i,j)
               qi(i,j)=qi(i,j)+dep(i,j)


         endif



          if (new_ice_sat .eq. 2) then


          dep(i,j)=0.0
          cnd(i,j)=0.0
          tair(i,j)=(pt(i,j)+tb0)*pi0
          if (tair(i,j) .ge. 253.16) then
              y1(i,j)=1./(tair(i,j)-c358)
              qsw(i,j)=rp0*exp(c172-c409*y1(i,j))
              dd(i,j)=cp409*y1(i,j)*y1(i,j)
              dm(i,j)=qv(i,j)+qb0-qsw(i,j)
              cnd(i,j)=dm(i,j)/(1.+avcp*dd(i,j)*qsw(i,j))

              cnd(i,j)=max(-qc(i,j), cnd(i,j))
             pt(i,j)=pt(i,j)+avcp*cnd(i,j)
             qv(i,j)=qv(i,j)-cnd(i,j)
             qc(i,j)=qc(i,j)+cnd(i,j)
         endif
          if (tair(i,j) .le. 258.16) then

           y2(i,j)=1./(tair(i,j)-c76)
           qsi(i,j)=rp0*exp(c218-c580*y2(i,j))
          dd1(i,j)=cp580*y2(i,j)*y2(i,j)
         dep(i,j)=(qv(i,j)+qb0-qsi(i,j))/(1.+ascp*dd1(i,j)*qsi(i,j))

             dep(i,j)=max(-qi(i,j),dep(i,j))
             pt(i,j)=pt(i,j)+ascp*dep(i,j)
             qv(i,j)=qv(i,j)-dep(i,j)
             qi(i,j)=qi(i,j)+dep(i,j)
         endif


      endif









            psdep(i,j)=0.0
            pgdep(i,j)=0.0
            pssub(i,j)=0.0
            pgsub(i,j)=0.0
            tair(i,j)=(pt(i,j)+tb0)*pi0

            if(tair(i,j).lt.t0) then
               if(qs(i,j).lt.cmin1) qs(i,j)=0.0
               if(qg(i,j).lt.cmin1) qg(i,j)=0.0
               rtair(i,j)=1./(tair(i,j)-c76)
               qsi(i,j)=rp0*exp(c218-c580*rtair(i,j))
               ssi(i,j)=(qv(i,j)+qb0)/qsi(i,j)-1.

               y1(i,j)=r10ar/(tca(i,j)*tair(i,j)**2)+1./(dwv(i,j) &
                      *qsi(i,j))
               y2(i,j)=.78/zs(i,j)**2+r101f*scv(i,j)/zs(i,j)**bsh5
               psdep(i,j)=r10t*ssi(i,j)*y2(i,j)/y1(i,j)
               pssub(i,j)=psdep(i,j)
               psdep(i,j)=r2is*max(psdep(i,j), 0.)
               pssub(i,j)=r2is*max(-qs(i,j), min(pssub(i,j), 0.))

               if(ihail.eq.1) then
                  y2(i,j)=.78/zg(i,j)**2+r20bq*scv(i,j)/zg(i,j)**bgh5
               else
                  y2(i,j)=.78/zg(i,j)**2+r20bs*scv(i,j)/zg(i,j)**bgh5
               endif

               pgsub(i,j)=r2ig*r20t*ssi(i,j)*y2(i,j)/y1(i,j)
               dm(i,j)=qv(i,j)+qb0-qsi(i,j)
               rsub1(i,j)=cs580*qsi(i,j)*rtair(i,j)*rtair(i,j)



               y1(i,j)=dm(i,j)/(1.+rsub1(i,j))
               psdep(i,j)=r2is*min(psdep(i,j),max(y1(i,j),0.))
               y2(i,j)=min(y1(i,j),0.)
               pssub(i,j)=r2is*max(pssub(i,j),y2(i,j))



               dd(i,j)=max((-y2(i,j)-qs(i,j)), 0.)
              pgsub(i,j)=r2ig*min(dd(i,j), qg(i,j), max(pgsub(i,j),0.))

               if(qc(i,j)+qi(i,j).gt.1.e-5) then
                  dlt1(i,j)=1.
               else
                  dlt1(i,j)=0.
               endif

               psdep(i,j)=dlt1(i,j)*psdep(i,j)
               pssub(i,j)=(1.-dlt1(i,j))*pssub(i,j)
               pgsub(i,j)=(1.-dlt1(i,j))*pgsub(i,j)

               pt(i,j)=pt(i,j)+ascp*(psdep(i,j)+pssub(i,j)-pgsub(i,j))
               qv(i,j)=qv(i,j)+pgsub(i,j)-pssub(i,j)-psdep(i,j)
               qs(i,j)=qs(i,j)+psdep(i,j)+pssub(i,j)
               qg(i,j)=qg(i,j)-pgsub(i,j)
            endif



            ern(i,j)=0.0

            if(qr(i,j).gt.0.0) then
               tair(i,j)=(pt(i,j)+tb0)*pi0
               rtair(i,j)=1./(tair(i,j)-c358)
               qsw(i,j)=rp0*exp(c172-c409*rtair(i,j))
               ssw(i,j)=(qv(i,j)+qb0)/qsw(i,j)-1.0
               dm(i,j)=qv(i,j)+qb0-qsw(i,j)
               rsub1(i,j)=cv409*qsw(i,j)*rtair(i,j)*rtair(i,j)
               dd1(i,j)=max(-dm(i,j)/(1.+rsub1(i,j)), 0.0)
               y1(i,j)=.78/zr(i,j)**2+r23af*scv(i,j)/zr(i,j)**bwh5
               y2(i,j)=r23br/(tca(i,j)*tair(i,j)**2)+1./(dwv(i,j) &
                       *qsw(i,j))

               ern(i,j)=r23t*ssw(i,j)*y1(i,j)/y2(i,j)
               ern(i,j)=min(dd1(i,j),qr(i,j),max(ern(i,j),0.))
               pt(i,j)=pt(i,j)-avcp*ern(i,j)
               qv(i,j)=qv(i,j)+ern(i,j)
               qr(i,j)=qr(i,j)-ern(i,j)
            endif


    ENDIF    



            if (qc(i,j) .le. cmin1) qc(i,j)=0.
            if (qr(i,j) .le. cmin1) qr(i,j)=0.
            if (qi(i,j) .le. cmin1) qi(i,j)=0.
            if (qs(i,j) .le. cmin1) qs(i,j)=0.
            if (qg(i,j) .le. cmin1) qg(i,j)=0.
            dpt(i,j,k)=pt(i,j)
            dqv(i,j,k)=qv(i,j)
            qcl(i,j,k)=qc(i,j)
            qrn(i,j,k)=qr(i,j)
            qci(i,j,k)=qi(i,j)
            qcs(i,j,k)=qs(i,j)
            qcg(i,j,k)=qg(i,j)



         scc=0.
         see=0.



























            dd(i,j)=max(-cnd(i,j), 0.)
            cnd(i,j)=max(cnd(i,j), 0.)
            dd1(i,j)=max(-dep(i,j), 0.)+pidep(i,j)*d2t
            dep(i,j)=max(dep(i,j), 0.)



















              sccc=cnd(i,j)
              seee=dd(i,j)+ern(i,j)
              sddd=dep(i,j)+pint(i,j)+psdep(i,j)+pgdep(i,j)
              ssss=dd1(i,j) + pgsub(i,j)
              smmm=psmlt(i,j)+pgmlt(i,j)+pimlt(i,j)
              sfff=d2t*(psacw(i,j)+piacr(i,j)+psfw(i,j) &
               +pgfr(i,j)+dgacw(i,j)+dgacr(i,j)+psacr(i,j) &
               +wgacr(i,j))+pihom(i,j)+pidw(i,j)
















 2000 continue

 1000 continue



      do k=kts,kte
         do j=jts,jte
         do i=its,ite
         ptwrf(i,k,j) = dpt(i,j,k)
         qvwrf(i,k,j) = dqv(i,j,k)
         qlwrf(i,k,j) = qcl(i,j,k)
         qrwrf(i,k,j) = qrn(i,j,k)
         qiwrf(i,k,j) = qci(i,j,k)
         qswrf(i,k,j) = qcs(i,j,k)
         qgwrf(i,k,j) = qcg(i,j,k)
         enddo 
         enddo 
      enddo 




         IF ( PRESENT (diagflag) ) THEN
         if (diagflag .and. do_radar_ref == 1) then
            do j=jts,jte
            do i=its,ite
               DO K=kts,kte
                  t1d(k)=ptwrf(i,k,j)*pi_mks(i,k,j)
                  p1d(k)=p0_mks(i,k,j)
                  qv1d(k)=qvwrf(i,k,j)
                  qr1d(k)=qrwrf(i,k,j)
               ENDDO
               if (ice2.eq.0) then
                  DO K=kts,kte
                     qs1d(k)=qswrf(i,k,j)
                     qg1d(k)=qgwrf(i,k,j)
                  ENDDO
               elseif (ice2.eq.1) then
                  DO K=kts,kte
                     qs1d(k)=qswrf(i,k,j)
                  ENDDO
               elseif (ice2.eq.2) then
                  DO K=kts,kte
                     qs1d(k)=0.
                     qg1d(k)=qgwrf(i,k,j)
                  ENDDO
               elseif (ice2.eq.3) then
                  DO K=kts,kte
                     qs1d(k)=0.
                     qg1d(k)=0.
                  ENDDO
               endif
               call refl10cm_gsfc (qv1d, qr1d, qs1d, qg1d,             &
                       t1d, p1d, dBZ, kts, kte, i, j, ihail)
               do k = kts, kte
                  refl_10cm(i,k,j) = MAX(-35., dBZ(k))
               enddo
            enddo
            enddo
         endif
         ENDIF


      return
 END SUBROUTINE saticel_s









  real function gammagce (xx)

  real*8 cof(6),stp,half,one,fpf,x,tmp,ser
  data cof,stp /  76.18009173,-86.50532033,24.01409822, &
     -1.231739516,.120858003e-2,-.536382e-5, 2.50662827465 /
  data half,one,fpf / .5, 1., 5.5 /

      x=xx-one
      tmp=x+fpf
      tmp=(x+half)*log(tmp)-tmp
      ser=one
      do  j=1,6
         x=x+one
        ser=ser+cof(j)/x
      enddo 
      gammln=tmp+log(stp*ser)

      gammagce=exp(gammln)

      return
 END FUNCTION gammagce



      subroutine refl10cm_gsfc (qv1d, qr1d, qs1d, qg1d,                 &
                       t1d, p1d, dBZ, kts, kte, ii, jj, ihail)

      IMPLICIT NONE


      INTEGER, INTENT(IN):: kts, kte, ii, jj, ihail
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
            if (ihail.eq.1) then
               N0_g(k) = xnoh
            else
               N0_g(k) = xnog
            endif
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

      end subroutine refl10cm_gsfc



END MODULE  module_mp_gsfcgce
