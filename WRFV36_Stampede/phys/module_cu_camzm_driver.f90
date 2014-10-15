MODULE module_cu_camzm_driver

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


  USE module_cam_support, only: pcnst =>pcnst_runtime, pcols, pver, pverp



  USE shr_kind_mod,    only: r8 => shr_kind_r8
  USE module_cu_camzm, only: convtran, momtran, zm_conv_evap, zm_convr

  IMPLICIT NONE

  PRIVATE                  
  PUBLIC :: &              
       camzm_driver  , &



       zm_conv_init

  
  integer :: ixcldliq, ixcldice, ixnumliq, ixnumice
  
CONTAINS


SUBROUTINE camzm_driver(                                      &
              ids,ide, jds,jde, kds,kde                       &
             ,ims,ime, jms,jme, kms,kme                       &
             ,its,ite, jts,jte, kts,kte                       &
             ,itimestep, bl_pbl_physics, sf_sfclay_physics    &
             ,th, t_phy, tsk, tke_pbl, ust, qv, qc, qi        &
             ,mavail, kpbl, pblh, xland, z                    &
             ,z_at_w, dz8w, ht, p, p8w, pi_phy, psfc          &
             ,u_phy, v_phy, hfx, qfx, cldfra, cldfra_mp_all   &
             ,is_CAMMGMP_used, tpert_camuwpbl                 &
             ,dx, dt, stepcu, cudt, curr_secs, adapt_step_flag&
             ,cudtacttime                                     & 
             ,cape_out, mu_out, md_out, zmdt, zmdq            &
             ,rliq_out, dlf_out                               &
             ,pconvb, pconvt, cubot, cutop, raincv, pratec    &
             ,rucuten, rvcuten                                &
             ,rthcuten, rqvcuten, rqccuten, rqicuten          &
             ,rqcncuten, rqincuten                            &
             ,evaptzm, fzsntzm, evsntzm, evapqzm, zmflxprc    &
             ,zmflxsnw, zmntprpd, zmntsnpd, zmeiheat          &
             ,cmfmc, cmfmcdzm, preccdzm, precz                &
             ,zmmtu, zmmtv, zmupgu, zmupgd, zmvpgu, zmvpgd    &
             ,zmicuu, zmicud, zmicvu, zmicvd                  &
             ,zmdice, zmdliq                                  &
             
             ,evapcdp3d, icwmrdp3d, rprddp3d                  &
             
             ,dp3d, du3d, ed3d, eu3d, md3d, mu3d, dsubcld2d   &
             ,ideep2d, jt2d, maxg2d, lengath2d                )








  USE physconst,     only: latice,cpair, gravit
  USE wv_saturation, only: fqsatd,epsqs, polysvp
  USE wv_saturation,            only: fqsatd,epsqs, polysvp


  logical, intent(in)    ::    is_CAMMGMP_used
  INTEGER, INTENT(IN   ) ::    ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte,  &
                     bl_pbl_physics, & 
                          itimestep, & 
                  sf_sfclay_physics, & 
                             stepcu    

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: &
                             cldfra, & 
                      cldfra_mp_all, & 
                               dz8w, & 
                                  p, & 
                                p8w, & 
                             pi_phy, & 
                                 qv, & 
                                 th, & 
                            tke_pbl, & 
                              t_phy, & 
                              u_phy, & 
                              v_phy, & 
                                  z, & 
                             z_at_w    

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN), OPTIONAL :: &
                                 qc, & 
                                 qi    

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: &
                            dlf_out, & 
                            evaptzm, & 
                            fzsntzm, & 
                            evsntzm, & 
                            evapqzm, & 
                           zmflxprc, & 
                           zmflxsnw, & 
                           zmntprpd, & 
                           zmntsnpd, & 
                           zmeiheat, & 
                              cmfmc, & 
                           cmfmcdzm, & 
                             md_out, & 
                             mu_out, & 
                            rucuten, & 
                            rvcuten, & 
                           rthcuten, & 
                           rqvcuten, & 
                               zmdt, & 
                               zmdq, & 
                              zmmtu, & 
                              zmmtv, & 
                             zmupgu, & 
                             zmupgd, & 
                             zmvpgu, & 
                             zmvpgd, & 
                             zmicuu, & 
                             zmicud, & 
                             zmicvu, & 
                             zmicvd, & 
                             zmdice, & 
                             zmdliq    

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT), OPTIONAL :: &
                           rqccuten, & 
                           rqicuten, & 
                          rqcncuten, & 
                          rqincuten    


  
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT) :: & 
                          evapcdp3d, & 
                          icwmrdp3d, & 
                           rprddp3d    

  
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT) :: &
                               dp3d, &
                               du3d, &
                               ed3d, &
                               eu3d, &
                               md3d, &
                               mu3d


  REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: &
                                hfx, & 
                                 ht, & 
                              xland, & 
                             mavail, & 
                               pblh, & 
                               psfc, & 
                                qfx, & 
                     tpert_camuwpbl, & 
                                tsk, & 
                                ust    

  REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: &
                           cape_out, & 
                              cubot, & 
                              cutop, & 
                             pconvb, & 
                             pconvt, & 
                             pratec, & 
                           preccdzm, & 
                              precz, & 
                             raincv, & 
                           rliq_out    
  REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: &
                           dsubcld2d

  REAL, INTENT(IN) :: &
                               cudt, & 
                          curr_secs, & 
                                 dt, & 
                                 dx    

  REAL, INTENT (INOUT) :: &
                        cudtacttime    

  INTEGER, DIMENSION( ims:ime, jms:jme), INTENT(IN) :: &
                               kpbl    

  INTEGER, DIMENSION( ims:ime, jms:jme), INTENT(OUT) :: &
                           ideep2d, &
                           jt2d,    &
                           maxg2d,  &
                        lengath2d 

  LOGICAL, INTENT(IN), OPTIONAL :: &
                    adapt_step_flag    


  
  REAL(r8), DIMENSION(pcols, kte+1) ::  &
                               mcon, & 
                               pflx, & 
                              pint8, & 
                                zi8, & 
                            flxprec, & 
                            flxsnow    


  REAL(r8), DIMENSION(pcols, kte, pcnst) ::  &
                                qh8    

  REAL(r8), DIMENSION(pcols, kte, 3) ::  &
                              cloud, & 
                           cloudtnd    






 REAL(r8), DIMENSION(pcols, kte, 5) ::  &

                             fracis    


  REAL(r8), DIMENSION(pcols, kte, 2) ::  &
                               icwu, & 
                               icwd, & 
                             pguall, & 
                             pgdall, & 
                              winds, & 
                         wind_tends    

  REAL(r8), DIMENSION(pcols, kte) ::  &
                               cld8, & 
                                cme, & 
                                dlf, & 
                         fake_dpdry, & 
                            ntprprd, & 
                            ntsnprd, & 
                              pdel8, & 
                              pmid8, & 
                                ql8, & 
                                qi8, & 
                                 t8, & 
                                zm8, & 
                               qtnd, & 
                               rprd, & 
                               stnd, & 
                      tend_s_snwprd, & 
                    tend_s_snwevmlt, & 
                                zdu, & 
                            evapcdp    

  REAL(r8), DIMENSION(pcols, kte) ::  &
                                esl, & 
                                qvs, & 
                          qcten_det, &
                          qiten_det, &
                         qcnten_det, &
                         qinten_det, &
                          qsten_det
  REAL(r8), DIMENSION(pcols) ::  &
                               cape, & 
                              jctop, & 
                              jcbot, & 
                           landfrac, & 
                              pblh8, & 
                               phis, & 
                               prec, & 
                               rliq, & 
                               snow, & 
                              tpert    

  REAL(r8), DIMENSION(pcols, kte, (ime-ims+1)*(jme-jms+1)) :: &
                                 dp, & 
                                 du, &
                                 ed, &
                                 eu, &
                                 md, &
                                 mu

  REAL(r8), DIMENSION(pcols, (ime-ims+1)*(jme-jms+1)) :: &
                            dsubcld    

  INTEGER, DIMENSION(pcols, (ime-ims+1)*(jme-jms+1)) :: &
                              ideep, & 
                                 jt, & 
                               maxg    
                               
  INTEGER, DIMENSION((ime-ims+1)*(jme-jms+1)) :: &
                            lengath    

  
  REAL(r8):: dum1,cudts,hcudts
  INTEGER :: i, j, k, kflip, n, ncnst
  INTEGER :: lchnk         
  INTEGER :: ncol          
  LOGICAL, DIMENSION(3) :: l_qt    
  LOGICAL, DIMENSION(2) :: l_windt 
  LOGICAL :: run_param , & 
             doing_adapt_dt , decided






   if (cudt .eq. 0.) then
    cudts = dt
    hcudts=cudts*0.5_r8
   else 
    cudts=cudt*60._r8
    hcudts=cudts*0.5_r8
   end if

   doing_adapt_dt = .FALSE.
   IF ( PRESENT(adapt_step_flag) ) THEN
      IF ( adapt_step_flag ) THEN
         doing_adapt_dt = .TRUE.
         IF ( cudtacttime .EQ. 0. ) THEN
            cudtacttime = curr_secs + cudt*60.
         END IF
      END IF
   END IF



















   decided = .FALSE.
   run_param = .FALSE.
   IF ( ( .NOT. decided ) .AND. &
        ( itimestep .EQ. 1 ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( ( cudt .EQ. 0. ) .OR. ( stepcu .EQ. 1 ) ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( .NOT. doing_adapt_dt ) .AND. &
        ( MOD(itimestep,stepcu) .EQ. 0 ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( doing_adapt_dt ) .AND. &
        ( curr_secs .GE. cudtacttime ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
      cudtacttime = curr_secs + cudt*60
   END IF

  
  if( .not. run_param ) return



  ncol  = 1          

  cape_out(its:ite, jts:jte)        = 0.
  mu_out(its:ite, kts:kte, jts:jte) = 0.
  md_out(its:ite, kts:kte, jts:jte) = 0.
  zmdt(its:ite, kts:kte, jts:jte)   = 0.




  do j = jts,jte
     do i = its,ite
        lchnk = (j-jts)*(ite-its+1) + (i-its+1) 

        
        do k = kts,kte
           kflip = kte-k+1
           if(is_CAMMGMP_used) then
              cld8(1,kflip)  = cldfra_mp_all(i,k,j)
           else
              cld8(1,kflip)  = cldfra(i,k,j)
           endif
		   if (itimestep .eq. 1) cld8(1,kflip) =0.
           pdel8(1,kflip) = p8w(i,k,j) - p8w(i,k+1,j)
           pmid8(1,kflip) = p(i,k,j)
           qh8(1,kflip,1) = max( qv(i,k,j)/(1.+qv(i,k,j)), 1e-30 ) 
           if( present(qc) ) then
              ql8(1,kflip) = qc(i,k,j)/(1.+qv(i,k,j)) 
           else
              ql8(1,kflip) = 0.
           end if
           if( present(qi) ) then
              qi8(1,kflip) = qi(i,k,j)/(1.+qv(i,k,j)) 
           else
              qi8(1,kflip) = 0.
           end if
           t8(1,kflip)    = t_phy(i,k,j)
           zm8(1,kflip)   = z(i,k,j) - ht(i,j) 
           
           dp(1,kflip,lchnk) = 0.0_r8
           du(1,kflip,lchnk) = 0.0_r8
           ed(1,kflip,lchnk) = 0.0_r8
           eu(1,kflip,lchnk) = 0.0_r8
           md(1,kflip,lchnk) = 0.0_r8
           mu(1,kflip,lchnk) = 0.0_r8
        end do

        
        do k = kts,kte+1
           kflip = kte-k+2

           pint8(1,kflip) = p8w(i,k,j)
           zi8(1,kflip)   = z_at_w(i,k,j) -ht(i,j) 
        end do

        
        if( xland(i,j)==2 ) then
           landfrac(1) = 1. 
        else
           landfrac(1) = 0. 
        end if
        pblh8(1) = pblh(i,j)
        phis(1)  = ht(i,j)*gravit

        call get_tpert(bl_pbl_physics, sf_sfclay_physics, dx, &
             mavail(i,j), kpbl(i,j), pblh(i,j), &
             dz8w(i,1,j), psfc(i,j), qv(i,1,j), t_phy(i,1,j), &
             th(i,1,j), tsk(i,j), tke_pbl(i,:,j), ust(i,j),   &
             u_phy(i,1,j), v_phy(i,1,j), hfx(i,j), qfx(i,j), &
             tpert_camuwpbl(i,j), kte, &
             tpert(1))

        
        
        

        
        call zm_convr( lchnk, ncol, &
             t8, qh8, prec, jctop, jcbot, &
             pblh8, zm8, phis, zi8, qtnd, &
             stnd, pmid8, pint8, pdel8, &
             hcudts, mcon, cme, cape, &
             tpert, dlf, pflx, zdu, rprd, &
             mu(:,:,lchnk), md(:,:,lchnk),du(:,:,lchnk),eu(:,:,lchnk),ed(:,:,lchnk), &
             dp(:,:,lchnk), dsubcld(:,lchnk), jt(:,lchnk), maxg(:,lchnk), ideep(:,lchnk), &
             lengath(lchnk), ql8, rliq, landfrac )

        
        
        do k=kts,kte
           kflip = kte-k+1
           dlf(1,kflip)    = max(min(1.e-6_r8,dlf(1,kflip)),0._r8)
           dlf_out(i,k,j)  = dlf(1,kflip)
        end do
        cape_out(i,j) = cape(1)
        rliq_out(i,j) = rliq(1)

        
        mcon(:ncol,:pverp) = mcon(:ncol,:pverp) * 100._r8/gravit

        
        
        do n=1,lengath(lchnk)
           do k=kts,kte
              kflip = kte-k+1

              mu_out(i,k,j) = mu(n,kflip,lchnk) * 100._r8/gravit
              md_out(i,k,j) = md(n,kflip,lchnk) * 100._r8/gravit
           end do
        end do

        do k=kts,kte
           kflip = kte-k+1
           zmdt(i,k,j) = stnd(1,kflip)/cpair
           zmdq(i,k,j) = qtnd(1,kflip)
        end do

        
        pconvt(i,j) = p8w(i,1,j)
        pconvb(i,j) = p8w(i,1,j)
        do n = 1,lengath(lchnk)
           if (maxg(n,lchnk).gt.jt(n,lchnk)) then
              pconvt(i,j) = pmid8(ideep(n,lchnk),jt(n,lchnk))  
              pconvb(i,j) = pmid8(ideep(n,lchnk),maxg(n,lchnk))
           endif
        end do
        cutop(i,j) = jctop(1)
        cubot(i,j) = jcbot(1)

        
        
        
        
        
        
        
        do k=kts,kte
           kflip = kte-k+1

           
           
           rthcuten(i,k,j) = zmdt(i,k,j)/pi_phy(i,k,j)
           rqvcuten(i,k,j) = zmdq(i,k,j)/(1._r8 - zmdq(i,k,j))

           t8(1,kflip)    = t8(1,kflip) + zmdt(i,k,j)*cudts   
           qh8(1,kflip,1) = qh8(1,kflip,1) + zmdq(i,k,j)*cudts   
        end do

        
        
        
        
        qtnd = 0._r8 
        stnd = 0._r8
        call zm_conv_evap(ncol, lchnk, &
             t8, pmid8, pdel8, qh8(:,:,1), &
             stnd, tend_s_snwprd, tend_s_snwevmlt, qtnd, &
             rprd, cld8, cudts, &                            
             prec, snow, ntprprd, ntsnprd , flxprec, flxsnow)
        evapcdp(:ncol,:pver) = qtnd(:ncol,:pver) 

        
        do k=kts,kte
           kflip = kte-k+1

           evaptzm(i,k,j)  = stnd(1,kflip)/cpair
           fzsntzm(i,k,j)  = tend_s_snwprd(1,kflip)/cpair
           evsntzm(i,k,j)  = tend_s_snwevmlt(1,kflip)/cpair
           evapqzm(i,k,j)  = qtnd(1,kflip)
           zmflxprc(i,k,j) = flxprec(1,kflip)
           zmflxsnw(i,k,j) = flxsnow(1,kflip)
           zmntprpd(i,k,j) = ntprprd(1,kflip)
           zmntsnpd(i,k,j) = ntsnprd(1,kflip)
           zmeiheat(i,k,j) = stnd(1,kflip) 
           
           
           
           preccdzm(i,j)   = prec(1)       
           precz(i,j)      = prec(1)       
           pratec(i,j)     = prec(1)*1e3   
           raincv(i,j)     = pratec(i,j)*cudts 
        end do

        
        do k = kts,kte+1
           kflip             = kte - k + 2
           cmfmc(i,k,j)    = mcon(1,kflip) 
           cmfmcdzm(i,k,j) = mcon(1,kflip)
        end do
        
        
        
        
        
        
        do k=kts,kte
           kflip = kte-k+1

           
           
           rthcuten(i,k,j) = rthcuten(i,k,j) + &
                             evaptzm(i,k,j)/pi_phy(i,k,j)
           rqvcuten(i,k,j) = rqvcuten(i,k,j) + &
                             evapqzm(i,k,j)/(1. - qv(i,k,j))

           t8(1,kflip)    = t8(1,kflip) + evaptzm(i,k,j)*cudts    
           qh8(1,kflip,1) = qh8(1,kflip,1) + evapqzm(i,k,j)*cudts 
        end do

        
        stnd       = 0._r8     
        wind_tends = 0._r8
        do k=kts,kte
           kflip = kte-k+1
           winds(1,k,1) = u_phy(i,kflip,j)
           winds(1,k,2) = v_phy(i,kflip,j)
        end do
        l_windt(1:2) = .true.

        call momtran (lchnk, ncol, &
             l_windt, winds, 2, mu(:,:,lchnk), md(:,:,lchnk), &
             du(:,:,lchnk), eu(:,:,lchnk), ed(:,:,lchnk), dp(:,:,lchnk), dsubcld(:,lchnk), &
             jt(:,lchnk),maxg(:,lchnk), ideep(:,lchnk), 1, lengath(lchnk), &
             itimestep, wind_tends, pguall, pgdall, icwu, icwd, hcudts, stnd )  
        
        
        
        
        
        
        
        do k=kts,kte
           kflip = kte-k+1

           
           
           rucuten(i,k,j)  = wind_tends(1,kflip,1)
           rvcuten(i,k,j)  = wind_tends(1,kflip,2)
           rthcuten(i,k,j) = rthcuten(i,k,j) + &
                                stnd(1,kflip)/cpair/pi_phy(i,k,j)

           t8(1,kflip) = t8(1,kflip) + stnd(1,kflip)/cpair*cudts   
           
        end do

        
        do k=kts,kte
           kflip = kte-k+1

           zmmtu(i,k,j) = wind_tends(1,kflip,1) 
           zmmtv(i,k,j) = wind_tends(1,kflip,2)

           zmupgu(i,k,j) = pguall(1,kflip,1)   
           zmupgd(i,k,j) = pgdall(1,kflip,1)
           zmvpgu(i,k,j) = pguall(1,kflip,2)
           zmvpgd(i,k,j) = pgdall(1,kflip,2)

           zmicuu(i,k,j) = icwu(1,kflip,1)     
           zmicud(i,k,j) = icwd(1,kflip,1)
           zmicvu(i,k,j) = icwu(1,kflip,2)
           zmicvd(i,k,j) = icwd(1,kflip,2)
        end do

        
        
        
        
        l_qt(1)   = .false.     
        l_qt(2:3) = .true.      
        cloudtnd = 0._r8
        fracis(1,:,1:3) = 1._r8 

        
        
        
        
        fracis(1,:,4:5) = 1._r8

        ncnst = 3               
        fake_dpdry = 0._r8      
        do k=kts,kte
           kflip = kte-k+1

           cloud(1,kflip,1) = qh8(1,kflip,1)  
           cloud(1,kflip,2) = ql8(1,kflip)    
           cloud(1,kflip,3) = qi8(1,kflip)    
                                              
        end do

        call convtran (lchnk, &
             l_qt, cloud, ncnst,  mu(:,:,lchnk), md(:,:,lchnk), &
             du(:,:,lchnk), eu(:,:,lchnk), ed(:,:,lchnk), dp(:,:,lchnk), dsubcld(:,lchnk), &
             jt(:,lchnk), maxg(:,lchnk), ideep(:,lchnk), 1, lengath(lchnk), &
             itimestep, fracis, cloudtnd, fake_dpdry)

        

        do k=kts,kte
           kflip = kte-k+1
           esl(1,kflip)     = polysvp(t8(1,kflip),0)
           qvs(1,kflip)     = epsqs*esl(1,kflip)/(pmid8(1,kflip)-(1._r8-epsqs)*esl(1,kflip))
            if( t8(1,kflip) > 268.15_r8 ) then
              dum1 = 0.0_r8
            elseif( t8(1,kflip) < 238.15_r8 ) then
              dum1 = 1.0_r8
            else
              dum1 = ( 268.15_r8 - t8(1,kflip) ) / 30._r8
            endif
           qcten_det(1,kflip) = dlf(1,kflip) * ( 1._r8 - dum1 )
           qiten_det(1,kflip) = dlf(1,kflip) * dum1
           qcnten_det(1,kflip) = 3._r8 * (dlf(1,kflip)    * ( 1._r8 - dum1 ) ) / (4._r8*3.14159_r8*(8.e-6_r8**3)*997._r8)
           qinten_det(1,kflip) = 3._r8 * (dlf(1,kflip)    *  dum1 ) / (4._r8*3.14159_r8*(25.e-6_r8**3)*500._r8)
           qsten_det(1,kflip)  =  dlf(1,kflip) * dum1 * latice                    
        end do
        do k=kts,kte
           kflip = kte-k+1

           
           evapcdp3d(i,k,j) = evapcdp(1,kflip) 
           icwmrdp3d(i,k,j) = ql8(1,kflip)     
           rprddp3d(i,k,j)  = rprd(1,kflip)    

           zmdice(i,k,j) = cloudtnd(1,kflip,3)+qiten_det(1,kflip)
           zmdliq(i,k,j) = cloudtnd(1,kflip,2)+qcten_det(1,kflip)
           rthcuten(i,k,j) = rthcuten(i,k,j) + &
                            qsten_det(1,kflip)/cpair/pi_phy(i,k,j)

           
           if( present(rqccuten) ) then
              rqccuten(i,k,j) = (cloudtnd(1,kflip,2)+qcten_det(1,kflip))/(1. - qv(i,k,j))
           end if
           if( present(rqicuten) ) then
              rqicuten(i,k,j) = (cloudtnd(1,kflip,3)+qiten_det(1,kflip))/(1. - qv(i,k,j))
           end if
           if( present(rqcncuten) ) then
              rqcncuten(i,k,j) = qcnten_det(1,kflip)/(1. - qv(i,k,j)) 
           end if
           if( present(rqincuten) ) then
              rqincuten(i,k,j) = qinten_det(1,kflip)/(1. - qv(i,k,j)) 
           end if
           
           dp3d(i,k,j) = dp(1,kflip,lchnk)
           du3d(i,k,j) = du(1,kflip,lchnk)
           ed3d(i,k,j) = ed(1,kflip,lchnk)
           eu3d(i,k,j) = eu(1,kflip,lchnk)
           md3d(i,k,j) = md(1,kflip,lchnk)
           mu3d(i,k,j) = mu(1,kflip,lchnk)

           dsubcld2d(i,j) = dsubcld(1,lchnk)
           ideep2d(i,j)   = ideep(1,lchnk)
           jt2d(i,j)      = jt(1,lchnk)
           maxg2d(i,j)    = maxg(1,lchnk)
           lengath2d(i,j) = lengath(lchnk)
        end do
        
     end do 
  end do    
END SUBROUTINE camzm_driver



SUBROUTINE zm_conv_init(DT, DX, rucuten, rvcuten, rthcuten, rqvcuten,   &
                     rqccuten, rqicuten, rqcncuten, rqincuten,          &
                     p_qc, p_qi, p_qnc, p_qni, param_first_scalar,      &
                     restart,                                           &
                     ids, ide, jds, jde, kds, kde,                      &
                     ims, ime, jms, jme, kms, kme,                      &
                     its, ite, jts, jte, kts, kte                    )






  USE physconst, only: epsilo, latvap, latice, rh2o, cpair, tmelt
  USE module_cu_camzm, only: zm_convi, zmconv_readnl
  USE constituents, only: cnst_get_ind

  LOGICAL , INTENT(IN)           ::   restart
  INTEGER , INTENT(IN)           ::   ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte, &
                                      p_qc, p_qi, p_qnc, p_qni,     &
                                      param_first_scalar

   REAL ,    INTENT(IN)        :: DT, DX


  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: &
                                                           rucuten, &
                                                           rvcuten, &
                                                          rthcuten, &
                                                          rqvcuten, &
                                                          rqccuten, &
                                                          rqicuten, &
                                                          rqcncuten,&
                                                          rqincuten

  integer :: i, itf, j, jtf, k, ktf
  integer :: limcnv

  jtf = min(jte,jde-1)
  ktf = min(kte,kde-1)
  itf = min(ite,ide-1)

  call cnst_get_ind('CLDLIQ', ixcldliq)
  call cnst_get_ind('CLDICE', ixcldice)       
  call cnst_get_ind('NUMLIQ', ixnumliq)
  call cnst_get_ind('NUMICE', ixnumice)
  







  pver  = ktf-kts+1
  pverp = pver+1

















  limcnv = 2



  call zmconv_readnl("hard-wired")



  call zm_convi(DT, DX, limcnv, NO_DEEP_PBL_IN=.true.)




  if(.not.restart)then
     do j=jts,jtf
        do k=kts,ktf
           do i=its,itf
              rucuten(i,k,j)  = 0.
              rvcuten(i,k,j)  = 0.
              rthcuten(i,k,j) = 0.
              rqvcuten(i,k,j) = 0.
              if( p_qc  > param_first_scalar ) rqccuten(i,k,j)  = 0.
              if( p_qi  > param_first_scalar ) rqicuten(i,k,j)  = 0.
              if( p_qnc > param_first_scalar ) rqcncuten(i,k,j) = 0.
              if( p_qni > param_first_scalar ) rqincuten(i,k,j) = 0.
           enddo
        enddo
     enddo
  end if

END SUBROUTINE zm_conv_init



SUBROUTINE get_tpert(bl_pbl_physics, sf_sfclay_physics, dx, &
     mavail, kpbl, pblh, dzlowest, &
     psfc, qvlowest, t_phylowest, thlowest, tsk, tke_pbl, ust,   &
     u_phylowest, v_phylowest, hfx, qfx, tpert_camuwpbl, kte, tpert)












  USE module_model_constants, only: cp, ep_1, ep_2, g, r_d, rcp, &
                                    svp1, svp2, svp3, svpt0, xlv
  USE module_state_description, ONLY : SFCLAYSCHEME              &
                                      ,MYJSFCSCHEME              &
                                      ,GFSSFCSCHEME              &
                                      ,SLABSCHEME                &
                                      ,LSMSCHEME                 &
                                      ,RUCLSMSCHEME              &
                                      ,MYJPBLSCHEME              &
                                      ,CAMUWPBLSCHEME



  real, dimension(:), intent(in) :: tke_pbl
  real, intent(in) :: dx, dzlowest, hfx, mavail, pblh, psfc, qvlowest, &
       tpert_camuwpbl, tsk, t_phylowest, thlowest, ust, u_phylowest, &
       v_phylowest
  integer, intent(in) :: bl_pbl_physics, kpbl, kte, sf_sfclay_physics
  real(r8),intent(inout) :: tpert



  real, parameter :: fak      = 8.5   
  real, parameter :: tfac     = 1.    
  real, parameter :: wfac     = 1.    
  real, parameter :: wpertmin = 1.e-6 
  real :: ebrk                        
                                      
                                      
  real :: br2, dthvdz, e1, flux, govrth, psfccmb, qfx, qsfc, rhox, thgb, &
       thv, tskv, tvcon, vconv, vsgd, wpert, wspd, za
  integer :: k
  character(len=250) :: msg
  logical :: UnstableOrNeutral




  if( bl_pbl_physics==CAMUWPBLSCHEME ) then
     tpert = tpert_camuwpbl










  elseif( bl_pbl_physics==MYJPBLSCHEME ) then

     UnstableOrNeutral = .false.
     sfclay_case: SELECT CASE (sf_sfclay_physics)
     CASE (MYJSFCSCHEME)
        
        
        

        if( pblh <= 0. ) call wrf_error_fatal3("<stdin>",947,&
             "CAMZMSCHEME needs a PBL height from a PBL scheme.")

        za     = 0.5*dzlowest

        e1     = svp1*exp(svp2*(tsk-svpt0)/(tsk-svp3))
        psfccmb=psfc/1000.  
        qsfc   = ep_2*e1/(psfccmb-e1)
        thgb   = tsk*(100./psfccmb)**rcp
        tskv   = thgb*(1.+ep_1*qsfc*mavail)
        tvcon  = 1.+ep_1*qvlowest
        thv    = thlowest*tvcon
        dthvdz = (thv-tskv)

        govrth = g/thlowest

        rhox   = psfc/(r_d*t_phylowest*tvcon)
        flux   = max(hfx/rhox/cp + ep_1*tskv*qfx/rhox,0.)
        vconv  = (g/tsk*pblh*flux)**.33
        vsgd   = 0.32 * (max(dx/5000.-1.,0.))**.33
        wspd   = sqrt(u_phylowest*u_phylowest+v_phylowest*v_phylowest)
        wspd   = sqrt(wspd*wspd+vconv*vconv+vsgd*vsgd)
        wspd   = max(wspd,0.1)

        
        br2   = govrth*za*dthvdz/(wspd*wspd)

        if( br2 <= 0. ) UnstableOrNeutral = .true.

     CASE DEFAULT
        call wrf_error_fatal3("<stdin>",977,&
"CAMZMSCHEME requires MYJSFCSCHEME or else CAMUWPBLSCHEME.")

     END SELECT sfclay_case





     
     
     
     
     if( bl_pbl_physics /= MYJPBLSCHEME ) &
          call wrf_error_fatal3("<stdin>",991,&
"CAMZMSCHEME requires MYJPBLSCHEME or CAMUWPBLSCHEME")
   

     
     
     
     tvcon = 1.+ep_1*qvlowest
     rhox  = psfc/(r_d*t_phylowest*tvcon)

     if( UnstableOrNeutral ) then
        
        ebrk = 0.
        do k=1,min(kpbl,kte+1) 
           ebrk = ebrk + tke_pbl(k)
        end do
        ebrk = ebrk/real(kpbl)

        wpert = max( wfac*sqrt(ebrk), wpertmin )
        tpert = max( abs(hfx/rhox/cp)*tfac/wpert, 0. )



     else 
        tpert = max( hfx/rhox/cp*fak/ust, 0. )
     end if

  else
     call wrf_error_fatal3("<stdin>",1019,&
"CAMZMSCHEME requires MYJPBLSCHEME or CAMUWPBLSCHEME")

  end if 

END SUBROUTINE get_tpert




END MODULE module_cu_camzm_driver
