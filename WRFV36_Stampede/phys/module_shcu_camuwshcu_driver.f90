MODULE module_shcu_camuwshcu_driver
  USE shr_kind_mod,    only: r8 => shr_kind_r8




  
  
  
  
  
  
  
  
  
  




  IMPLICIT NONE

  PRIVATE                  
  PUBLIC :: &              
       camuwshcu_driver

CONTAINS


SUBROUTINE camuwshcu_driver(                                  &
              ids,ide, jds,jde, kds,kde                       &
             ,ims,ime, jms,jme, kms,kme                       &
             ,its,ite, jts,jte, kts,kte                       &
             ,num_moist, dt                                   &
             ,p, p8w, pi_phy, z, z_at_w, dz8w                 &
             ,t_phy, u_phy, v_phy                             &
             ,moist, qv, qc, qi, qnc, qni                     &



             ,pblh_in, tke_pbl, cldfra, cldfra_old            &
             ,cldfra_old_mp,cldfra_conv, is_CAMMGMP_used      &
             ,cldfrash                                        &
             ,cush_inout, pratesh, snowsh, icwmrsh    &
             ,cmfmc, cmfmc2_inout, rprdsh_inout, cbmf_inout   &
             ,cmfsl, cmflq, dlf, dlf2, evapcsh_inout          &
             ,rliq, rliq2_inout, cubot, cutop                 &
             ,rushten, rvshten, rthshten                      &
             ,rqvshten, rqcshten, rqrshten                    &
             ,rqishten, rqsshten, rqgshten                    &
             ,rqcnshten,rqinshten                             &
             ,ht, shfrc3d,itimestep                           &
                                                              )






  USE module_state_description, only: param_first_scalar, &
                                      p_qc, p_qr, p_qi, p_qs, p_qg, p_qnc, p_qni
  USE module_cam_support,       only: pcols, pver, pcnst =>pcnst_runtime



  USE constituents,             only: cnst_get_ind
  USE physconst,                only: latice,cpair, gravit, latvap
  USE uwshcu,                   only: compute_uwshcu_inv
  USE wv_saturation,            only: fqsatd








  LOGICAL, INTENT(IN) :: is_CAMMGMP_used
  INTEGER, INTENT(IN   ) ::    ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte,  &
                               num_moist,itimestep




  REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ), INTENT(IN) :: &
                              moist    





  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: &
                             cldfra, & 
                         cldfra_old, & 
                      cldfra_old_mp, &
                        cldfra_conv, &
                               dz8w, & 
                                  p, & 
                                p8w, & 
                             pi_phy, & 
                                 qv, & 
                              t_phy, & 
                            tke_pbl, & 
                              u_phy, & 
                              v_phy, & 
                                  z, & 
                             z_at_w    

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN), OPTIONAL :: &
                                 qc, & 
                                 qi, & 
                                qnc, & 
                                qni    

  REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: &
                            pblh_in, & 
                            ht         

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: &
                           cldfrash, & 
                              cmfmc, & 
                       cmfmc2_inout, & 
                              cmflq, & 
                              cmfsl, & 
                                dlf, & 
                      evapcsh_inout, & 
                            icwmrsh, & 
                       rprdsh_inout, & 
                            rushten, & 
                            rvshten, & 
                           rthshten, & 
                           rqvshten, & 
                           rqcshten, & 
                           rqrshten, & 
                           rqishten, & 
                           rqsshten, & 
                           rqgshten, & 
                          rqcnshten, & 
                          rqinshten


  REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: &
                         cbmf_inout, & 
                              cubot, & 
                              cutop, & 
                         cush_inout, & 
                            pratesh, & 
                               rliq, & 
                        rliq2_inout, & 
                             snowsh    

  REAL, INTENT(IN) :: &
                                 dt    

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT) ::   &
                        dlf2           
 REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT) ::   &
                           shfrc3d     


  
  REAL(r8), DIMENSION(pcols, kte, pcnst) ::  &
                             moist8, & 
                         tnd_tracer    

  REAL(r8), DIMENSION(pcols, kte+1) ::  &
                              pint8, & 
                                zi8, & 
                               tke8, & 
                              slflx, & 
                              qtflx, & 
                            flxprec, & 
                            flxsnow, & 
                            cmfmc2     

                                                            


  REAL(r8), DIMENSION(pcols, kte) ::  &
                               cld8, & 
                            cldold8, & 
                             cmfdqs, & 
                            evapcsh, & 
                           iccmr_uw, & 
                           icwmr_uw, & 
                           icimr_uw, & 
                              pdel8, & 
                           pdeldry8, & 
                              pmid8, & 
                                qc2, & 
                                qh8, & 
                                qc8, & 
                                qi8, & 
                              qhtnd, & 
                              qctnd, & 
                              qitnd, & 
                             rprdsh, & 
                                 s8, & 
                              shfrc, & 
                               stnd, & 
                                 t8, & 
                                 u8, & 
                               utnd, & 
                                 v8, & 
                               vtnd, & 
                                zm8    

  REAL(r8), DIMENSION(pcols, kte) ::  &
                           qcten_det, &
                           qiten_det, &
                          qcnten_det, &
                          qinten_det, &
                           qsten_det

  REAL(r8), DIMENSION(pcols) ::  &
                               cbmf, & 
                               cnb2, & 
                               cnt2, & 
                               cush, & 
                               pblh, & 
                              precc, & 
                              rliq2, & 
                               snow    

  
  REAL(r8) :: ztodt,dum1
  INTEGER :: i, j, k, kflip, m, mp1
  INTEGER :: cnb, cnt      
  INTEGER :: lchnk         
  INTEGER :: ncnst         
  INTEGER :: ncol          
  CHARACTER(LEN=1000) :: msg

  character*24 :: ptend_name            
  logical      :: ptend_ls              
  logical      :: ptend_lq(pcnst)       

  integer :: l, l2
  real(r8) :: state_s(pcols,kte)
  real(r8) :: ptend_s(pcols,kte)                   






  ncol  = 1     
  ncnst = pcnst 
  ztodt = dt




  ij_loops : do j = jts,jte
     do i = its,ite
        lchnk = (j-jts)*(ite-its+1) + (i-its+1) 

        
        do k = kts,kte+1
           kflip = kte-k+2

           pint8(1,kflip) = p8w(i,k,j)
           zi8(1,kflip)   = z_at_w(i,k,j) - ht(i,j) 
        end do

        
        do k = kts,kte
           kflip = kte-k+1
           if(is_CAMMGMP_used) then
              cld8(1,kflip)    = cldfra_old_mp(i,k,j)
              cldold8(1,kflip) = cldfra_conv(i,k,j)
           else
              cld8(1,kflip)    = cldfra(i,k,j)
              cldold8(1,kflip) = cldfra_old(i,k,j)
           endif
           if (itimestep .eq. 1) then
             cld8(1,kflip)    = 0._r8
             cldold8(1,kflip) = 0._r8
           end if
           cld8(1,kflip) = min(max((cld8(1,kflip) + cldold8(1,kflip)),0._r8),1._r8)

           pdel8(1,kflip)   = p8w(i,k,j) - p8w(i,k+1,j)
           pmid8(1,kflip)   = p(i,k,j)
           qh8(1,kflip)     = max( qv(i,k,j)/(1. + qv(i,k,j)), 1e-30 ) 
           if( present(qc) ) then
              qc8(1,kflip)  = qc(i,k,j)/(1. + qv(i,k,j)) 
           else
              qc8(1,kflip)  = 0.
           end if
           if( present(qi) ) then
              qi8(1,kflip)  = qi(i,k,j)/(1. + qv(i,k,j)) 
           else
              qi8(1,kflip)  = 0.
           end if
           pdeldry8(1,kflip)= pdel8(1,kflip)*(1._r8 - qh8(1,kflip))
           t8(1,kflip)      = t_phy(i,k,j)
           s8(1,kflip)      = cpair*t8(1,kflip) + gravit*(z(i,k,j)-ht(i,j))
           u8(1,kflip)      = u_phy(i,k,j)
           v8(1,kflip)      = v_phy(i,k,j)
           zm8(1,kflip)     = z(i,k,j)-ht(i,j)
        end do

        
        do k = kts, kte+1
           kflip = kte - k + 2
           
           tke8(1,kflip) = tke_pbl(i,k,j)  
        end do

        
        
        
        do k = kts,kte
           kflip = kte-k+1

           moist8(1,kflip,1:ncnst) = 0.

           moist8(1,kflip,1) = max(0.0,qv(i,k,j)/(1. + qv(i,k,j)))

           call cnst_get_ind( 'CLDLIQ', m )
           moist8(1,kflip,m) = max(0.0,qc(i,k,j)/(1. + qv(i,k,j)))

           call cnst_get_ind( 'CLDICE', m )
           moist8(1,kflip,m) = max(0.0,qi(i,k,j)/(1. + qv(i,k,j)))

           call cnst_get_ind( 'NUMLIQ', m )
           moist8(1,kflip,m) = max(0.0,qnc(i,k,j)/(1. + qv(i,k,j)))

           call cnst_get_ind( 'NUMICE', m )
           moist8(1,kflip,m) = max(0.0,qni(i,k,j)/(1. + qv(i,k,j)))


        end do

        
        pblh(1) = pblh_in(i,j)
        cush(1) = cush_inout(i,j)






        call compute_uwshcu_inv(                        &
             pcols, pver, ncol, ncnst, ztodt,           &
             pint8, zi8, pmid8, zm8, pdel8,             &
             u8, v8, qh8, qc8, qi8,                     &
             t8, s8, moist8,                            &
             tke8, cld8, cldold8, pblh, cush,           &
             cmfmc2, slflx, qtflx,                      &
             flxprec, flxsnow,                          &
             qhtnd, qctnd, qitnd,                       &
             stnd, utnd, vtnd, tnd_tracer,              &
             rprdsh, cmfdqs, precc, snow,               &
             evapcsh, shfrc, iccmr_UW, icwmr_UW,        &
             icimr_UW, cbmf, qc2, rliq2,                &
             cnt2, cnb2, fqsatd, lchnk, pdeldry8        )



        cush_inout(i,j) = cush(1)

       do k = kts,kte
           kflip = kte-k+1
           qc2(1,kflip)=max(0._r8,min(1.e-6_r8,qc2(1,kflip)))
            if( t8(1,kflip) > 268.15_r8 ) then
              dum1 = 0.0_r8
            elseif( t8(1,kflip) < 238.15_r8 ) then
              dum1 = 1.0_r8
            else
              dum1 = ( 268.15_r8 - t8(1,kflip) ) / 30._r8
            endif
           qcten_det(1,kflip) = qc2(1,kflip) * ( 1._r8 - dum1 )
           qiten_det(1,kflip) = qc2(1,kflip) * dum1 
           qcnten_det(1,kflip) = 3._r8 * (qc2(1,kflip)    * ( 1._r8 - dum1 ) ) / (4._r8*3.14159_r8*(10.e-6_r8**3)*997._r8) 
           qinten_det(1,kflip) = 3._r8 * (qc2(1,kflip)    *  dum1 ) / (4._r8*3.14159_r8*(50.e-6_r8**3)*500._r8)  
           qsten_det(1,kflip)      =  qc2(1,kflip) * dum1 * latice                    
        end do
        do k = kts,kte
           kflip = kte-k+1
           dlf2(i,k,j)         = qc2(1,kflip)
           shfrc3d(i,k,j)      = shfrc(1,kflip)   

           
           
           dlf(i,k,j)          = dlf(i,k,j) + qc2(1,kflip)

           evapcsh_inout(i,k,j)= evapcsh(1,kflip)
           icwmrsh(i,k,j)      = icwmr_uw(1,kflip)

           rprdsh(1,kflip)     = rprdsh(1,kflip) + cmfdqs(1,kflip)
           rprdsh_inout(i,k,j) = rprdsh(1,kflip)
           

           
           
           rushten(i,k,j)  = utnd(1,kflip)
           rvshten(i,k,j)  = vtnd(1,kflip)
           rthshten(i,k,j) = (stnd(1,kflip)+qsten_det(1,kflip))/cpair/pi_phy(i,k,j)
           rqvshten(i,k,j) = qhtnd(1,kflip)/(1. - qv(i,k,j))
           if( p_qc >= param_first_scalar ) &
                rqcshten(i,k,j) = (qctnd(1,kflip)+qcten_det(1,kflip))/(1. - qv(i,k,j))
           if( p_qi >= param_first_scalar ) &
                rqishten(i,k,j) = (qitnd(1,kflip)+qiten_det(1,kflip))/(1. - qv(i,k,j))

           if( p_qnc >= param_first_scalar ) then
              call cnst_get_ind( 'NUMLIQ', m )
              rqcnshten(i,k,j) = (tnd_tracer(1,kflip,m)+qcnten_det(1,kflip))/(1. - qv(i,k,j))
           endif
           if( p_qni >= param_first_scalar ) then
              call cnst_get_ind( 'NUMICE', m )
              rqinshten(i,k,j) = (tnd_tracer(1,kflip,m)+qinten_det(1,kflip))/(1. - qv(i,k,j))
           endif
        end do 

           


        do k = kts,kte+1
           kflip = kte-k+2

           
           cmfsl(i,k,j) = slflx(1,kflip)
           cmflq(i,k,j) = qtflx(1,kflip)*latvap
           
           cmfmc2_inout(i,k,j) = cmfmc2(1,kflip)
           cmfmc(i,k,j)        = cmfmc(i,k,j) + cmfmc2(1,kflip)
        end do 

        
        

        
        pratesh(i,j) = precc(1)*1e3/dt 

        
        
        
        kflip = kte - cutop(i,j) + 1
        cnt = kflip
        if( cnt2(1) < kflip ) cnt = cnt2(1)
        cutop(i,j) = kte - cnt + 1

        kflip = kte - cubot(i,j) + 1
        cnb = kflip
        if( cnb2(1) > kflip ) cnb = cnb2(1)
        cubot(i,j) = kte - cnb + 1

        
        
        rliq2_inout(i,j) = rliq2(1)
        rliq(i,j)        = rliq(i,j) + rliq2(1)

     end do
  end do ij_loops
END SUBROUTINE camuwshcu_driver

END MODULE module_shcu_camuwshcu_driver
