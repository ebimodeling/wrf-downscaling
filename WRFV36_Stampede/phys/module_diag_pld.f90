









MODULE module_diag_pld
CONTAINS

   SUBROUTINE pld ( u,v,w,t,qv,zp,zb,pp,pb,p,pw,                    &
                    msfux,msfuy,msfvx,msfvy,msftx,msfty,            &
                    f,e,                                            &
                    use_tot_or_hyd_p,missing,                       &
                    num_press_levels,max_press_levels,press_levels, &
                    p_pl,u_pl,v_pl,t_pl,rh_pl,ght_pl,s_pl,td_pl,    &
                    ids,ide, jds,jde, kds,kde,                      &
                    ims,ime, jms,jme, kms,kme,                      &
                    its,ite, jts,jte, kts,kte                       )
   
      USE module_model_constants
   
      IMPLICIT NONE
   
   
      
   
      INTEGER, INTENT(IN   )                                          :: ids,ide, jds,jde, kds,kde, &
                                                                         ims,ime, jms,jme, kms,kme, &
                                                                         its,ite, jts,jte, kts,kte
      REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: msfux,msfuy,msfvx,msfvy,msftx,msfty, &
                                                                         f,e
      INTEGER, INTENT(IN   )                                          :: use_tot_or_hyd_p
      REAL   , INTENT(IN   )                                          :: missing
      REAL   , INTENT(IN   ) , DIMENSION(ims:ime , kms:kme , jms:jme) :: u,v,w,t,qv,zp,zb,pp,pb,p,pw
      INTEGER, INTENT(IN   )                                          :: num_press_levels, max_press_levels
      REAL   , INTENT(IN   ) , DIMENSION(max_press_levels)            :: press_levels
   
      
   
      REAL   , INTENT(  OUT) ,  DIMENSION(num_press_levels)                     :: p_pl
      REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , num_press_levels , jms:jme) :: u_pl,v_pl,t_pl,rh_pl,ght_pl,s_pl,td_pl
   
      
   
      REAL, PARAMETER :: eps = 0.622, t_kelvin = svpt0 , s1 = 243.5, s2 = svp2 , s3 = svp1*10., s4 = 611.0, s5 = 5418.12
   
      INTEGER :: i, j, ke, kp, ke_h, ke_f
      REAL    :: pu, pd, pm , &
                 tu, td     , &
                 su, sd     , &
                 uu, ud     , &
                 vu, vd     , &
                 zu, zd     , &
                 qu, qd, qm , &
                 eu, ed, em , &
                 du, dd
      REAL    :: es, qs
   
      
   
      DO kp = 1 , num_press_levels
         p_pl(kp) = press_levels(kp)
      END DO
   
      
   
      DO j = jts , jte
         DO kp = 1 , num_press_levels
            DO i = its , ite
               u_pl  (i,kp,j) = missing
               v_pl  (i,kp,j) = missing
               t_pl  (i,kp,j) = missing
               rh_pl (i,kp,j) = missing
               ght_pl(i,kp,j) = missing
               s_pl  (i,kp,j) = missing
               td_pl (i,kp,j) = missing
            END DO
         END DO
      END DO
   
      
   
      j_loop : DO j = jts , MIN(jte,jde-1)
         i_loop : DO i = its , MIN(ite,ide-1)
   
            
   
            ke_h = kts
            ke_f = kts
            kp_loop : DO kp = 1 , num_press_levels
   
               
               
   
               ke_loop_half : DO ke = ke_h , kte-2
   
                  IF      ( use_tot_or_hyd_p .EQ. 1 ) THEN     
                     pu = pp(i,ke+1,j)+pb(i,ke+1,j)
                     pd = pp(i,ke  ,j)+pb(i,ke  ,j)
                  ELSE IF ( use_tot_or_hyd_p .EQ. 2 ) THEN     
                     pu = p(i,ke+1,j)
                     pd = p(i,ke  ,j)
                  END IF
                  pm = p_pl(kp)
   
                  IF ( ( pd .GE. pm ) .AND. &
                       ( pu .LT. pm ) ) THEN
   
                     
                     
   
                     
   
                     tu = (t(i,ke+1,j)+t0)*(pu/p1000mb)**rcp
                     td = (t(i,ke  ,j)+t0)*(pd/p1000mb)**rcp
                     t_pl(i,kp,j) = ( tu * (pm-pd) + td * (pu-pm) ) / (pu-pd)
   
                     
   
                     su = 0.5 * SQRT ( ( u(i,ke+1,j)+u(i+1,ke+1,j) )**2 + ( v(i,ke+1,j)+v(i,ke+1,j+1) )**2 ) 
                     sd = 0.5 * SQRT ( ( u(i,ke  ,j)+u(i+1,ke  ,j) )**2 + ( v(i,ke  ,j)+v(i,ke  ,j+1) )**2 ) 
                     s_pl(i,kp,j) = ( su * (pm-pd) + sd * (pu-pm) ) / (pu-pd)
   
                     
   
                     uu = 0.5 *        ( u(i,ke+1,j)+u(i+1,ke+1,j) )
                     ud = 0.5 *        ( u(i,ke  ,j)+u(i+1,ke  ,j) )
                     u_pl(i,kp,j) = ( uu * (pm-pd) + ud * (pu-pm) ) / (pu-pd)
   
                     vu = 0.5 *                                           ( v(i,ke+1,j)+v(i,ke+1,j+1) )
                     vd = 0.5 *                                           ( v(i,ke  ,j)+v(i,ke  ,j+1) )
                     v_pl(i,kp,j) = ( vu * (pm-pd) + vd * (pu-pm) ) / (pu-pd)
   
                     
   
                     qu = MAX(qv(i,ke+1,j),0.)
                     qd = MAX(qv(i,ke  ,j),0.)
                     eu = qu * pu * 0.01 / ( eps + qu )       
                     ed = qd * pd * 0.01 / ( eps + qd )       
                     eu = max(eu, 0.001)
                     ed = max(ed, 0.001)
   
                     du = t_kelvin + ( s1 / ((s2 / log(eu/s3)) - 1.0) )
                     dd = t_kelvin + ( s1 / ((s2 / log(ed/s3)) - 1.0) )
                     td_pl(i,kp,j) = ( du * (pm-pd) + dd * (pu-pm) ) / (pu-pd)
   
                     
   
                     qm = ( qu * (pm-pd) + qd * (pu-pm) ) / (pu-pd)                           
                     es = s4 * exp(s5 * (1.0 / 273.0 - 1.0 / t_pl(i,kp,j)))
                     qs = eps * es / (pm - es)
                     rh_pl(i,kp,j)   = qm / qs * 100.
   
                     
                     
                     
   
                     ke_h = ke
                     EXIT ke_loop_half
                  END IF
               END DO ke_loop_half
   
               ke_loop_full : DO ke = ke_f , kte-1
                  IF ( ( pw(i,ke  ,j) .GE. p_pl(kp) ) .AND. &
                       ( pw(i,ke+1,j) .LT. p_pl(kp) ) ) THEN
   
                     
   
                     pu = LOG(pw(i,ke+1,j))
                     pm = LOG(p_pl(kp))
                     pd = LOG(pw(i,ke  ,j))
   
                     
   
                     
   
                     zu = ( zp(i,ke+1,j)+zb(i,ke+1,j) ) / g
                     zd = ( zp(i,ke  ,j)+zb(i,ke  ,j) ) / g
                     ght_pl(i,kp,j) = ( zu * (pm-pd) + zd * (pu-pm) ) / (pu-pd)
   
                     ke_f = ke
                     EXIT ke_loop_full
                  END IF
               END DO ke_loop_full
   
            END DO kp_loop
         END DO i_loop
      END DO j_loop

   END SUBROUTINE pld

END MODULE module_diag_pld
