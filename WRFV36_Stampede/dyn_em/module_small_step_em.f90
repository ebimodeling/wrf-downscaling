






MODULE module_small_step_em

   USE module_configure
   USE module_model_constants

CONTAINS



SUBROUTINE small_step_prep( u_1, u_2, v_1, v_2, w_1, w_2, &
                            t_1, t_2, ph_1, ph_2,         &
                            mub, mu_1, mu_2,              &
                            muu, muus, muv, muvs,         &
                            mut, muts, mudf,              &
                            u_save, v_save, w_save,       &
                            t_save, ph_save, mu_save,     &
                            ww, ww_save,                  &
                            dnw, c2a, pb, p, alt,         &
                            msfux, msfuy, msfvx,          &
                            msfvx_inv,                    &
                            msfvy, msftx, msfty,          &
                            rdx, rdy,                     &
                            rk_step,                      &
                            ids,ide, jds,jde, kds,kde,    & 
                            ims,ime, jms,jme, kms,kme,    &
                            its,ite, jts,jte, kts,kte    )

  IMPLICIT NONE  



  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  INTEGER,      INTENT(IN   )    :: rk_step

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(INOUT) :: u_1,   &
                                                              v_1,   &
                                                              w_1,   &
                                                              t_1,   &
                                                              ph_1

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(  OUT) :: u_save,   &
                                                              v_save,   &
                                                              w_save,   &
                                                              t_save,   &
                                                              ph_save

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(INOUT) :: u_2,   &
                                                              v_2,   &
                                                              w_2,   &
                                                              t_2,   &
                                                              ph_2

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(  OUT) :: c2a, &
                                                               ww_save

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN   ) ::  pb,  &
                                                                p,   &
                                                                alt, &
                                                                ww

  REAL, DIMENSION(ims:ime, jms:jme)         , INTENT(INOUT) :: mu_1,mu_2

  REAL, DIMENSION(ims:ime, jms:jme)         , INTENT(IN   ) :: mub,  &
                                                               muu,  &
                                                               muv,  &
                                                               mut,  &
                                                               msfux,&
                                                               msfuy,&
                                                               msfvx,&
                                                               msfvx_inv,&
                                                               msfvy,&
                                                               msftx,&
                                                               msfty

  REAL, DIMENSION(ims:ime, jms:jme)         , INTENT(  OUT) :: muus, &
                                                               muvs, &
                                                               muts, &
                                                               mudf

  REAL, DIMENSION(ims:ime, jms:jme)         , INTENT(  OUT) :: mu_save

  REAL, DIMENSION(kms:kme, jms:jme)         , INTENT(IN   ) :: dnw

  REAL, INTENT(IN) :: rdx,rdy



  INTEGER :: i, j, k
  INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end
  INTEGER :: i_endu, j_endv












    i_start = its
    i_end   = min(ite,ide-1)
    j_start = jts
    j_end   = min(jte,jde-1)
    k_start = kts
    k_end = min(kte,kde-1)

    i_endu = ite
    j_endv = jte

    
    

    IF ((rk_step == 1) ) THEN

      DO j=j_start, j_end
      DO i=i_start, i_end
        mu_1(i,j)=mu_2(i,j)
        ww_save(i,kde,j) = 0.
        ww_save(i,1,j) = 0.
        mudf(i,j) = 0.  
      ENDDO
      ENDDO

      DO j=j_start, j_end
      DO k=k_start, k_end
      DO i=i_start, i_endu
        u_1(i,k,j) = u_2(i,k,j)
      ENDDO
      ENDDO
      ENDDO

      DO j=j_start, j_endv
      DO k=k_start, k_end
      DO i=i_start, i_end
        v_1(i,k,j) = v_2(i,k,j)
      ENDDO
      ENDDO
      ENDDO

      DO j=j_start, j_end
      DO k=k_start, k_end
      DO i=i_start, i_end
        t_1(i,k,j) = t_2(i,k,j)
      ENDDO
      ENDDO
      ENDDO

      DO j=j_start, j_end
      DO k=k_start, min(kde,kte)
      DO i=i_start, i_end
        w_1(i,k,j)  = w_2(i,k,j)
        ph_1(i,k,j) = ph_2(i,k,j)
      ENDDO
      ENDDO
      ENDDO

    DO j=j_start, j_end
      DO i=i_start, i_end
        muts(i,j)=mub(i,j)+mu_2(i,j)
      ENDDO
      DO i=i_start, i_endu


        muus(i,j) = muu(i,j)
      ENDDO
    ENDDO

    DO j=j_start, j_endv
    DO i=i_start, i_end


        muvs(i,j) = muv(i,j)
    ENDDO
    ENDDO

    DO j=j_start, j_end
      DO i=i_start, i_end
        mu_save(i,j)=mu_2(i,j)
        mu_2(i,j)=0.

      ENDDO
    ENDDO

    ELSE

    DO j=j_start, j_end
      DO i=i_start, i_end
        muts(i,j)=mub(i,j)+mu_1(i,j)
      ENDDO
      DO i=i_start, i_endu
        muus(i,j)=0.5*(mub(i,j)+mu_1(i,j)+mub(i-1,j)+mu_1(i-1,j))
      ENDDO
    ENDDO

    DO j=j_start, j_endv
    DO i=i_start, i_end
      muvs(i,j)=0.5*(mub(i,j)+mu_1(i,j)+mub(i,j-1)+mu_1(i,j-1))
    ENDDO
    ENDDO

    DO j=j_start, j_end
      DO i=i_start, i_end
        mu_save(i,j)=mu_2(i,j)
        mu_2(i,j)=mu_1(i,j)-mu_2(i,j)
      ENDDO
    ENDDO


    END IF

    

      DO j=j_start, j_end
      DO i=i_start, i_end
        ww_save(i,kde,j) = 0.
        ww_save(i,1,j) = 0.
      ENDDO
      ENDDO

    DO j=j_start, j_end
    DO k=k_start, k_end
    DO i=i_start, i_end
      c2a(i,k,j) = cpovcv*(pb(i,k,j)+p(i,k,j))/alt(i,k,j)
    ENDDO
    ENDDO
    ENDDO

    DO j=j_start, j_end
    DO k=k_start, k_end
    DO i=i_start, i_endu
      u_save(i,k,j) = u_2(i,k,j)
      
      u_2(i,k,j) = (muus(i,j)*u_1(i,k,j)-muu(i,j)*u_2(i,k,j))/msfuy(i,j)
    ENDDO
    ENDDO
    ENDDO

    DO j=j_start, j_endv
    DO k=k_start, k_end
    DO i=i_start, i_end
      v_save(i,k,j) = v_2(i,k,j)
      

      v_2(i,k,j) = (muvs(i,j)*v_1(i,k,j)-muv(i,j)*v_2(i,k,j))*msfvx_inv(i,j)
    ENDDO
    ENDDO
    ENDDO

    DO j=j_start, j_end
    DO k=k_start, k_end
    DO i=i_start, i_end
      t_save(i,k,j) = t_2(i,k,j)
      t_2(i,k,j) = muts(i,j)*t_1(i,k,j)-mut(i,j)*t_2(i,k,j)
    ENDDO
    ENDDO
    ENDDO

    DO j=j_start, j_end

    DO k=k_start, kde
    DO i=i_start, i_end
      w_save(i,k,j) = w_2(i,k,j)
      
      w_2(i,k,j)  = (muts(i,j)* w_1(i,k,j)-mut(i,j)* w_2(i,k,j))/msfty(i,j)
      ph_save(i,k,j) = ph_2(i,k,j)
      ph_2(i,k,j) = ph_1(i,k,j)-ph_2(i,k,j)
    ENDDO
    ENDDO
    ENDDO

      DO j=j_start, j_end

      DO k=k_start, kde
      DO i=i_start, i_end
        ww_save(i,k,j) = ww(i,k,j)
      ENDDO
      ENDDO
      ENDDO

END SUBROUTINE small_step_prep




SUBROUTINE small_step_finish( u_2, u_1, v_2, v_1, w_2, w_1,    &
                              t_2, t_1, ph_2, ph_1, ww, ww1,   &
                              mu_2, mu_1,                      &
                              mut, muts, muu, muus, muv, muvs, &
                              u_save, v_save, w_save,          &
                              t_save, ph_save, mu_save,        &
                              msfux, msfuy, msfvx, msfvy,      &
                              msftx, msfty,                    &
                              h_diabatic,                      &
                              number_of_small_timesteps,dts,   &
                              rk_step, rk_order,               &
                              ids,ide, jds,jde, kds,kde,       &
                              ims,ime, jms,jme, kms,kme,       &
                              its,ite, jts,jte, kts,kte       )


  IMPLICIT NONE  



  INTEGER,                  INTENT(IN   ) :: ids,ide, jds,jde, kds,kde
  INTEGER,                  INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER,                  INTENT(IN   ) :: its,ite, jts,jte, kts,kte
  INTEGER,                  INTENT(IN   ) :: number_of_small_timesteps
  INTEGER,                  INTENT(IN   ) :: rk_step, rk_order
  REAL,                     INTENT(IN   ) :: dts

  REAL,   DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN   ) :: u_1, &
                                                                 v_1, &
                                                                 w_1, &
                                                                 t_1, &
                                                                 ww1, &
                                                                 ph_1

  REAL,   DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: u_2, &
                                                                 v_2, &
                                                                 w_2, &
                                                                 t_2, &
                                                                 ww,  &
                                                                 ph_2

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(IN   ) :: u_save,   &
                                                              v_save,   &
                                                              w_save,   &
                                                              t_save,   &
                                                              ph_save,  &
                                                             h_diabatic

  REAL,   DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: muus, muvs
  REAL,   DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: mu_2, mu_1
  REAL,   DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: mut, muts, &
                                                        muu, muv, mu_save
  REAL,   DIMENSION(ims:ime, jms:jme), INTENT(IN   ) :: msfux, msfuy, &
                                                        msfvx, msfvy, &
                                                        msftx, msfty




  INTEGER         :: i,j,k
  INTEGER :: i_start, i_end, j_start, j_end, i_endu, j_endv








  i_start = its
  i_end   = min(ite,ide-1)
  j_start = jts
  j_end   = min(jte,jde-1)

  i_endu = ite
  j_endv = jte



  DO j = j_start, j_endv
  DO k = kds, kde-1
  DO i = i_start, i_end
    
    v_2(i,k,j) = (msfvx(i,j)*v_2(i,k,j) + v_save(i,k,j)*muv(i,j))/muvs(i,j)
  ENDDO
  ENDDO
  ENDDO

  DO j = j_start, j_end
  DO k = kds, kde-1
  DO i = i_start, i_endu
    
    u_2(i,k,j) = (msfuy(i,j)*u_2(i,k,j) + u_save(i,k,j)*muu(i,j))/muus(i,j)
  ENDDO
  ENDDO
  ENDDO

  DO j = j_start, j_end
  DO k = kds, kde
  DO i = i_start, i_end
    
    w_2(i,k,j) = (msfty(i,j)*w_2(i,k,j) + w_save(i,k,j)*mut(i,j))/muts(i,j)
    ph_2(i,k,j) = ph_2(i,k,j) + ph_save(i,k,j)
    ww(i,k,j) = ww(i,k,j) + ww1(i,k,j)
  ENDDO
  ENDDO
  ENDDO

  IF ( rk_step < rk_order ) THEN
    DO j = j_start, j_end
    DO k = kds, kde-1
    DO i = i_start, i_end
      t_2(i,k,j) = (t_2(i,k,j) + t_save(i,k,j)*mut(i,j))/muts(i,j)
    ENDDO
    ENDDO
    ENDDO
  ELSE

    DO j = j_start, j_end
    DO k = kds, kde-1
    DO i = i_start, i_end
      t_2(i,k,j) = (t_2(i,k,j) - dts*number_of_small_timesteps*mut(i,j)*h_diabatic(i,k,j) &
                    + t_save(i,k,j)*mut(i,j))/muts(i,j)
    ENDDO
    ENDDO
    ENDDO
  ENDIF

  DO j = j_start, j_end
  DO i = i_start, i_end
    mu_2(i,j) = mu_2(i,j) + mu_save(i,j)
  ENDDO
  ENDDO

END SUBROUTINE small_step_finish



SUBROUTINE calc_p_rho( al, p, ph,                    &
                       alt, t_2, t_1, c2a, pm1,      &
                       mu, muts, znu, t0,            &
                       rdnw, dnw, smdiv,             &
                       non_hydrostatic, step,        &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its,ite, jts,jte, kts,kte    )

  IMPLICIT NONE  



  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  INTEGER,      INTENT(IN   )    :: step

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(  OUT) :: al,   &
                                                               p

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(IN   ) :: alt,   &
                                                              t_2,   &
                                                              t_1,   &
                                                              c2a

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(INOUT) :: ph, pm1

  REAL, DIMENSION(ims:ime, jms:jme)         , INTENT(IN   ) :: mu,   &
                                                               muts

  REAL, DIMENSION(kms:kme)         , INTENT(IN   ) :: dnw,  &
                                                      rdnw, &
                                                      znu

  REAL,                                       INTENT(IN   ) :: t0, smdiv

  LOGICAL, INTENT(IN   )  :: non_hydrostatic



  INTEGER :: i, j, k
  INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end
  REAL    :: ptmp



















   i_start = its
   i_end   = min(ite,ide-1)
   j_start = jts
   j_end   = min(jte,jde-1)
   k_start = kts
   k_end = min(kte,kde-1)

   IF (non_hydrostatic) THEN
     DO j=j_start, j_end
     DO k=k_start, k_end
     DO i=i_start, i_end



      al(i,k,j)=-1./muts(i,j)*(alt(i,k,j)*mu(i,j)  &
             +rdnw(k)*(ph(i,k+1,j)-ph(i,k,j)))



      p(i,k,j)=c2a(i,k,j)*(alt(i,k,j)*(t_2(i,k,j)-mu(i,j)*t_1(i,k,j))  &
                       /(muts(i,j)*(t0+t_1(i,k,j)))-al (i,k,j))

     ENDDO
     ENDDO
     ENDDO

   ELSE  

       DO j=j_start, j_end
       DO k=k_start, k_end
       DO i=i_start, i_end
         p(i,k,j)=mu(i,j)*znu(k)
         al(i,k,j)=alt(i,k,j)*(t_2(i,k,j)-mu(i,j)*t_1(i,k,j))            &
                      /(muts(i,j)*(t0+t_1(i,k,j)))-p(i,k,j)/c2a(i,k,j)
         ph(i,k+1,j)=ph(i,k,j)-dnw(k)*(muts(i,j)*al (i,k,j)              &
                          +mu(i,j)*alt(i,k,j))
       ENDDO
       ENDDO
       ENDDO

   END IF


 
     IF (step == 0) then   
       DO j=j_start, j_end
       DO k=k_start, k_end
       DO i=i_start, i_end
         pm1(i,k,j)=p(i,k,j)
       ENDDO
       ENDDO
       ENDDO 
     ELSE                     
       DO j=j_start, j_end    
       DO k=k_start, k_end
       DO i=i_start, i_end
         ptmp = p(i,k,j)
         p(i,k,j) = p(i,k,j) + smdiv*(p(i,k,j)-pm1(i,k,j))
         pm1(i,k,j) = ptmp
       ENDDO
       ENDDO
       ENDDO
     END IF

END SUBROUTINE calc_p_rho



SUBROUTINE calc_coef_w( a,alpha,gamma,              &
                        mut, cqw,                   &
                        rdn, rdnw, c2a,             &
                        dts, g, epssm, top_lid,     &
                        ids,ide, jds,jde, kds,kde,  & 
                        ims,ime, jms,jme, kms,kme,  & 
                        its,ite, jts,jte, kts,kte  )  
                                                   
      IMPLICIT NONE  



  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  LOGICAL,      INTENT(IN   )    :: top_lid

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN   ) :: c2a,  &
                                                               cqw

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: alpha, &
                                                               gamma, &
                                                               a

  REAL, DIMENSION(ims:ime, jms:jme),          INTENT(IN   ) :: mut

  REAL, DIMENSION(kms:kme),                   INTENT(IN   ) :: rdn,   &
                                                               rdnw

  REAL,                                       INTENT(IN   ) :: epssm, &
                                                               dts,   &
                                                               g



  REAL, DIMENSION(ims:ime)                         :: cof
  REAL  :: b, c

  INTEGER :: i, j, k, i_start, i_end, j_start, j_end, k_start, k_end
  INTEGER :: ij, ijp, ijm, lid_flag









      i_start = its
      i_end   = min(ite,ide-1)
      j_start = jts
      j_end   = min(jte,jde-1)
      k_start = kts
      k_end   = kte-1

      lid_flag=1
      IF(top_lid)lid_flag=0

     outer_j_loop:  DO j = j_start, j_end

        DO i = i_start, i_end
          cof(i)  = (.5*dts*g*(1.+epssm)/mut(i,j))**2
          a(i, 2 ,j) = 0.
          a(i,kde,j) =-2.*cof(i)*rdnw(kde-1)**2*c2a(i,kde-1,j)*lid_flag
          gamma(i,1 ,j) = 0.
        ENDDO

        DO k=3,kde-1
        DO i=i_start, i_end
          a(i,k,j) =   -cqw(i,k,j)*cof(i)*rdn(k)* rdnw(k-1)*c2a(i,k-1,j)    
        ENDDO
        ENDDO


        DO k=2,kde-1
        DO i=i_start, i_end
          b = 1.+cqw(i,k,j)*cof(i)*rdn(k)*(rdnw(k  )*c2a(i,k,j  )  &
                                       +rdnw(k-1)*c2a(i,k-1,j))
          c =   -cqw(i,k,j)*cof(i)*rdn(k)*rdnw(k  )*c2a(i,k,j  )
          alpha(i,k,j) = 1./(b-a(i,k,j)*gamma(i,k-1,j))
          gamma(i,k,j) = c*alpha(i,k,j)
        ENDDO
        ENDDO

        DO i=i_start, i_end
          b = 1.+2.*cof(i)*rdnw(kde-1)**2*c2a(i,kde-1,j)
          c = 0.
          alpha(i,kde,j) = 1./(b-a(i,kde,j)*gamma(i,kde-1,j))
          gamma(i,kde,j) = c*alpha(i,kde,j)
        ENDDO

      ENDDO outer_j_loop

END SUBROUTINE calc_coef_w



SUBROUTINE advance_uv ( u, ru_tend, v, rv_tend,        &
                        p, pb,                         &
                        ph, php, alt, al, mu,          &
                        muu, cqu, muv, cqv, mudf,      &
                        msfux, msfuy, msfvx,           &
                        msfvx_inv, msfvy,              &
                        rdx, rdy, dts,                 &
                        cf1, cf2, cf3, fnm, fnp,       &
                        emdiv,                         &
                        rdnw, config_flags, spec_zone, &
                        non_hydrostatic, top_lid,      &
                        ids, ide, jds, jde, kds, kde,  &
                        ims, ime, jms, jme, kms, kme,  &
                        its, ite, jts, jte, kts, kte  )



      IMPLICIT NONE  



      TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

      LOGICAL, INTENT(IN   ) :: non_hydrostatic, top_lid

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_zone

      REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),  &
            INTENT(INOUT) ::                          &
                                                  u,  &
                                                  v

      REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), &
            INTENT(IN   ) ::                          &
                                             ru_tend, &
                                             rv_tend, &
                                             ph,      &
                                             php,     &
                                             p,       &
                                             pb,      &
                                             alt,     &
                                             al,      &
                                             cqu,     &
                                             cqv


      REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(IN   ) :: muu,  &
                                                                muv,  &
                                                                mu,   &
                                                                mudf


      REAL, DIMENSION( kms:kme ),              INTENT(IN   ) :: fnm,    &
                                                                fnp ,   &
                                                                rdnw

      REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(IN   ) :: msfux,  &
                                                                msfuy,  &
                                                                msfvx,  &
                                                                msfvy,  &
                                                                msfvx_inv

      REAL,                                    INTENT(IN   ) :: rdx,    &
                                                                rdy,    &
                                                                dts,    &
                                                                cf1,    &
                                                                cf2,    &
                                                                cf3,    &
                                                                emdiv
    



      REAL, DIMENSION (its:ite, kts:kte) :: dpn, dpxy
      REAL, DIMENSION (its:ite) :: mudf_xy
      REAL                      :: dx, dy

      INTEGER :: i,j,k, i_start, i_end, j_start, j_end, k_start, k_end
      INTEGER :: i_endu, j_endv, k_endw
      INTEGER :: i_start_up, i_end_up, j_start_up, j_end_up
      INTEGER :: i_start_vp, i_end_vp, j_start_vp, j_end_vp

      INTEGER :: i_start_u_tend, i_end_u_tend, j_start_v_tend, j_end_v_tend












    IF( config_flags%nested .or. config_flags%specified ) THEN
      i_start = max( its,ids+spec_zone )
      i_end   = min( ite,ide-spec_zone-1 )
      j_start = max( jts,jds+spec_zone )
      j_end   = min( jte,jde-spec_zone-1 )
      k_start = kts
      k_end   = min( kte, kde-1 )

      i_endu = min( ite,ide-spec_zone )
      j_endv = min( jte,jde-spec_zone )
      k_endw = k_end

      IF( config_flags%periodic_x) THEN
        i_start = its
        i_end   = min(ite,ide-1)
        i_endu = ite
      ENDIF
    ELSE
      i_start = its
      i_end   = min(ite,ide-1)
      j_start = jts
      j_end   = min(jte,jde-1)
      k_start = kts
      k_end   = kte-1

      i_endu = ite
      j_endv = jte
      k_endw = k_end
    ENDIF

      i_start_up = i_start
      i_end_up   = i_endu
      j_start_up = j_start
      j_end_up   = j_end

      i_start_vp = i_start
      i_end_vp   = i_end
      j_start_vp = j_start
      j_end_vp   = j_endv

      IF ( (config_flags%open_xs   .or.     &
            config_flags%symmetric_xs   )   &
            .and. (its == ids) )            &
                 i_start_up = i_start_up + 1

      IF ( (config_flags%open_xe    .or.  &
            config_flags%symmetric_xe   ) &
             .and. (ite == ide) )         &
                 i_end_up   = i_end_up - 1

      IF ( (config_flags%open_ys    .or.   &
            config_flags%symmetric_ys  .or.   &
            config_flags%polar   )  &
                     .and. (jts == jds) )  &
                 j_start_vp = j_start_vp + 1

      IF ( (config_flags%open_ye     .or. &
            config_flags%symmetric_ye  .or.   &
            config_flags%polar   )  &
            .and. (jte == jde) )          &
                 j_end_vp   = j_end_vp - 1

      i_start_u_tend = i_start
      i_end_u_tend   = i_endu
      j_start_v_tend = j_start
      j_end_v_tend   = j_endv

      IF ( config_flags%symmetric_xs .and. (its == ids) ) &
                     i_start_u_tend = i_start_u_tend+1
      IF ( config_flags%symmetric_xe .and. (ite == ide) ) &
                     i_end_u_tend = i_end_u_tend-1
      IF ( config_flags%symmetric_ys .and. (jts == jds) ) &
                     j_start_v_tend = j_start_v_tend+1
      IF ( config_flags%symmetric_ye .and. (jte == jde) ) &
                     j_end_v_tend = j_end_v_tend-1

   dx = 1./rdx
   dy = 1./rdy




  u_outer_j_loop: DO j = j_start, j_end

   DO k = k_start, k_end
   DO i = i_start_u_tend, i_end_u_tend
     u(i,k,j) = u(i,k,j) + dts*ru_tend(i,k,j)
   ENDDO
   ENDDO

   DO i = i_start_up, i_end_up
     mudf_xy(i)= -emdiv*dx*(mudf(i,j)-mudf(i-1,j))/msfuy(i,j)
   ENDDO

   DO k = k_start, k_end
   DO i = i_start_up, i_end_up
















     dpxy(i,k)= (msfux(i,j)/msfuy(i,j))*.5*rdx*muu(i,j)*(         &
       ((ph (i,k+1,j)-ph (i-1,k+1,j))+(ph (i,k,j)-ph (i-1,k,j)))  &
      +(alt(i,k  ,j)+alt(i-1,k  ,j))*(p  (i,k,j)-p  (i-1,k,j))  &
      +(al (i,k  ,j)+al (i-1,k  ,j))*(pb (i,k,j)-pb (i-1,k,j)) ) 

   ENDDO
   ENDDO

   IF (non_hydrostatic) THEN

     DO i = i_start_up, i_end_up
       dpn(i,1) = .5*( cf1*(p(i,1,j)+p(i-1,1,j))  &
                      +cf2*(p(i,2,j)+p(i-1,2,j))  &
                      +cf3*(p(i,3,j)+p(i-1,3,j)) )
       dpn(i,kde) = 0.
     ENDDO
     IF (top_lid) THEN
       DO i = i_start_up, i_end_up
         dpn(i,kde) =.5*( cf1*(p(i-1,kde-1,j)+p(i,kde-1,j))   &
                         +cf2*(p(i-1,kde-2,j)+p(i,kde-2,j))   &
                         +cf3*(p(i-1,kde-3,j)+p(i,kde-3,j))  )
       ENDDO
     ENDIF

     DO k = k_start+1, k_end
       DO i = i_start_up, i_end_up
         dpn(i,k) = .5*( fnm(k)*(p(i,k  ,j)+p(i-1,k  ,j))   &
                        +fnp(k)*(p(i,k-1,j)+p(i-1,k-1,j)) )
       ENDDO
     ENDDO







     DO k = k_start, k_end
       DO i = i_start_up, i_end_up
         dpxy(i,k)=dpxy(i,k) + (msfux(i,j)/msfuy(i,j))*rdx*(php(i,k,j)-php(i-1,k,j))*        &
           (rdnw(k)*(dpn(i,k+1)-dpn(i,k))-.5*(mu(i-1,j)+mu(i,j)))
       ENDDO
     ENDDO

 
   END IF


   DO k = k_start, k_end
     DO i = i_start_up, i_end_up
       u(i,k,j)=u(i,k,j)-dts*cqu(i,k,j)*dpxy(i,k)+mudf_xy(i) 
     ENDDO
   ENDDO

   ENDDO u_outer_j_loop



  v_outer_j_loop: DO j = j_start_v_tend, j_end_v_tend


   DO k = k_start, k_end
   DO i = i_start, i_end
     v(i,k,j) = v(i,k,j) + dts*rv_tend(i,k,j)
   ENDDO
   ENDDO

   DO i = i_start, i_end
     mudf_xy(i)= -emdiv*dy*(mudf(i,j)-mudf(i,j-1))*msfvx_inv(i,j)
   ENDDO

     IF (     ( j >= j_start_vp)  &
       .and.( j <= j_end_vp  ) )  THEN

         DO k = k_start, k_end
	 DO i = i_start, i_end


















       dpxy(i,k)= (msfvy(i,j)/msfvx(i,j))*.5*rdy*muv(i,j)*(       &
        ((ph(i,k+1,j)-ph(i,k+1,j-1))+(ph (i,k,j)-ph (i,k,j-1)))   &
        +(alt(i,k  ,j)+alt(i,k  ,j-1))*(p  (i,k,j)-p  (i,k,j-1))  &
        +(al (i,k  ,j)+al (i,k  ,j-1))*(pb (i,k,j)-pb (i,k,j-1)) ) 
	    ENDDO
	    ENDDO


     IF (non_hydrostatic) THEN

       DO i = i_start, i_end     
         dpn(i,1) = .5*( cf1*(p(i,1,j)+p(i,1,j-1))  &
                        +cf2*(p(i,2,j)+p(i,2,j-1))  &
                        +cf3*(p(i,3,j)+p(i,3,j-1)) )
         dpn(i,kde) = 0.
       ENDDO
     IF (top_lid) THEN
       DO i = i_start, i_end
         dpn(i,kde) =.5*( cf1*(p(i,kde-1,j-1)+p(i,kde-1,j))   &
                         +cf2*(p(i,kde-2,j-1)+p(i,kde-2,j))   &
                         +cf3*(p(i,kde-3,j-1)+p(i,kde-3,j))  )
       ENDDO
     ENDIF

       DO k = k_start+1, k_end
         DO i = i_start, i_end
           dpn(i,k) = .5*( fnm(k)*(p(i,k  ,j)+p(i,k  ,j-1))  &
                          +fnp(k)*(p(i,k-1,j)+p(i,k-1,j-1)) )
         ENDDO
       ENDDO







       DO k = k_start, k_end
         DO i = i_start, i_end
           dpxy(i,k)=dpxy(i,k) + (msfvy(i,j)/msfvx(i,j))*rdy*(php(i,k,j)-php(i,k,j-1))*    &
             (rdnw(k)*(dpn(i,k+1)-dpn(i,k))-.5*(mu(i,j-1)+mu(i,j)))
         ENDDO
       ENDDO


     END IF


     DO k = k_start, k_end
       DO i = i_start, i_end
         v(i,k,j)=v(i,k,j)-dts*cqv(i,k,j)*dpxy(i,k)+mudf_xy(i) 
       ENDDO
     ENDDO
   END IF

  ENDDO  v_outer_j_loop

  
  
  
  
  IF (config_flags%polar) THEN
     IF (jts == jds) THEN
        DO k = k_start, k_end
        DO i = i_start, i_end
           v(i,k,jds) = 0.
        ENDDO
        ENDDO
     END IF
     IF (jte == jde) THEN
        DO k = k_start, k_end
        DO i = i_start, i_end
           v(i,k,jde) = 0.
        ENDDO
        ENDDO
     END IF
  END IF


END SUBROUTINE advance_uv



SUBROUTINE advance_mu_t( ww, ww_1, u, u_1, v, v_1,            &
                         mu, mut, muave, muts, muu, muv,      &
                         mudf, uam, vam, wwam, t, t_1,        &
                         t_ave, ft, mu_tend,                  &
                         rdx, rdy, dts, epssm,                &
                         dnw, fnm, fnp, rdnw,                 &
                         msfux, msfuy, msfvx, msfvx_inv,      &
                         msfvy, msftx, msfty,                 &
                         step, config_flags,                  &
                         ids, ide, jds, jde, kds, kde,        &
                         ims, ime, jms, jme, kms, kme,        &
                         its, ite, jts, jte, kts, kte        )

  IMPLICIT NONE  



  TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  INTEGER,      INTENT(IN   )    :: step

  REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),   &
            INTENT(IN   ) ::                       &
                                              u,   &
                                              v,   &
                                              u_1, &
                                              v_1, &
                                              t_1, &
                                              ft

  REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),      &
            INTENT(INOUT) ::                          &
                                              ww,     &
                                              ww_1,   &
                                              t,      &
                                              t_ave,  &
                                              uam,    &
                                              vam,    &
                                              wwam
                                              
  REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(IN   ) :: muu,  &
                                                            muv,  &
                                                            mut,  &
                                                            msfux,&
                                                            msfuy,&
                                                            msfvx,&
                                                            msfvx_inv,&
                                                            msfvy,&
                                                            msftx,&
                                                            msfty,&
                                                            mu_tend

  REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(  OUT) :: muave, &
                                                            muts,  &
                                                            mudf

  REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(INOUT) :: mu

  REAL, DIMENSION( kms:kme ),              INTENT(IN   ) :: fnm,    &
                                                            fnp,    &
                                                            dnw,    &
                                                            rdnw


  REAL,                                    INTENT(IN   ) :: rdx,    &
                                                            rdy,    &
                                                            dts,    &
                                                            epssm



  REAL, DIMENSION (its:ite, kts:kte) :: wdtn, dvdxi
  REAL, DIMENSION (its:ite) :: dmdt

  INTEGER :: i,j,k, i_start, i_end, j_start, j_end, k_start, k_end
  INTEGER :: i_endu, j_endv
  REAL    :: acc












  i_start = its
  i_end   = min(ite,ide-1)
  j_start = jts
  j_end   = min(jte,jde-1)
  k_start = kts
  k_end   = kte-1
  IF ( .NOT. config_flags%periodic_x )THEN
     IF ( config_flags%specified .or. config_flags%nested ) then
       i_start = max(its,ids+1)
       i_end   = min(ite,ide-2)
     ENDIF
  ENDIF
  IF ( config_flags%specified .or. config_flags%nested ) then
     j_start = max(jts,jds+1)
     j_end   = min(jte,jde-2)
  ENDIF

  i_endu = ite
  j_endv = jte


   DO j = j_start, j_end

     DO i=i_start, i_end
            dmdt(i) = 0.
     ENDDO























     DO k=k_start, k_end
     DO i=i_start, i_end
         dvdxi(i,k) = msftx(i,j)*msfty(i,j)*(                                      &
                     rdy*( (v(i,k,j+1)+muv(i,j+1)*v_1(i,k,j+1)*msfvx_inv(i,j+1))   &
                          -(v(i,k,j  )+muv(i,j  )*v_1(i,k,j  )*msfvx_inv(i,j  )) ) &
                    +rdx*( (u(i+1,k,j)+muu(i+1,j)*u_1(i+1,k,j)/msfuy(i+1,j))       &
                          -(u(i,k,j  )+muu(i  ,j)*u_1(i,k,j  )/msfuy(i  ,j)) ))
        dmdt(i)    = dmdt(i) + dnw(k)*dvdxi(i,k)
     ENDDO
     ENDDO
     DO i=i_start, i_end
       muave(i,j) = mu(i,j)
       mu(i,j) = mu(i,j)+dts*(dmdt(i)+mu_tend(i,j))
       mudf(i,j) = (dmdt(i)+mu_tend(i,j)) 
       muts(i,j) = mut(i,j)+mu(i,j)
       muave(i,j) =.5*((1.+epssm)*mu(i,j)+(1.-epssm)*muave(i,j))
     ENDDO

     DO k=2,k_end
     DO i=i_start, i_end
       ww(i,k,j)=ww(i,k-1,j)-dnw(k-1)*(dmdt(i)+dvdxi(i,k-1)+mu_tend(i,j))/msfty(i,j)
     ENDDO
     END DO




     DO k=1,k_end
     DO i=i_start, i_end
       ww(i,k,j)=ww(i,k,j)-ww_1(i,k,j)
     END DO
     END DO

   ENDDO



















   DO j=j_start, j_end
     DO k=1,k_end
     DO i=i_start, i_end
       t_ave(i,k,j) = t(i,k,j)
       t   (i,k,j) = t(i,k,j) + msfty(i,j)*dts*ft(i,k,j)
     END DO
     END DO
   ENDDO   

   DO j=j_start, j_end

     DO i=i_start, i_end
       wdtn(i,1  )=0.
       wdtn(i,kde)=0.
     ENDDO

     DO k=2,k_end
     DO i=i_start, i_end
        
        wdtn(i,k)= ww(i,k,j)*(fnm(k)*t_1(i,k  ,j)+fnp(k)*t_1(i,k-1,j))
     ENDDO
     ENDDO




     DO k=1,k_end
     DO i=i_start, i_end
       
       t(i,k,j) = t(i,k,j) - dts*msfty(i,j)*(              &
                          
                          msftx(i,j)*(                     &
               .5*rdy*                                     &
              ( v(i,k,j+1)*(t_1(i,k,j+1)+t_1(i,k, j ))     &
               -v(i,k,j  )*(t_1(i,k, j )+t_1(i,k,j-1)) )   &
             + .5*rdx*                                     &
              ( u(i+1,k,j)*(t_1(i+1,k,j)+t_1(i  ,k,j))     &
               -u(i  ,k,j)*(t_1(i  ,k,j)+t_1(i-1,k,j)) ) ) &
             + rdnw(k)*( wdtn(i,k+1)-wdtn(i,k) ) )       
     ENDDO
     ENDDO

   ENDDO

END SUBROUTINE advance_mu_t
          




SUBROUTINE advance_w( w, rw_tend, ww, w_save, u, v,  &
                      mu1, mut, muave, muts,      &
                      t_2ave, t_2, t_1,           &
                      ph, ph_1, phb, ph_tend,     &
                      ht, c2a, cqw, alt, alb,     &
                      a, alpha, gamma,            &
                      rdx, rdy, dts, t0, epssm,   &
                      dnw, fnm, fnp, rdnw, rdn,   &
                      cf1, cf2, cf3, msftx, msfty,&
                      config_flags, top_lid,      &
                      ids,ide, jds,jde, kds,kde,  & 
                      ims,ime, jms,jme, kms,kme,  & 
                      its,ite, jts,jte, kts,kte  )  





  IMPLICIT NONE 
      



  TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  LOGICAL,      INTENT(IN   )    :: top_lid

      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), &
            INTENT(INOUT) ::                          &
                                             t_2ave,  &
                                             w,       &
                                             ph

      REAL, DIMENSION(  ims:ime , kms:kme, jms:jme ), &
            INTENT(IN   ) ::                          &
                                             rw_tend, &
                                             ww,      &
                                             w_save,  &
                                             u,       &
                                             v,       &
                                             t_2,     &
                                             t_1,     &
                                             ph_1,    &
                                             phb,     &
                                             ph_tend, &
                                             alpha,   &
                                             gamma,   &
                                             a,       &
                                             c2a,     &
                                             cqw,     &
                                             alb,     &
                                             alt

      REAL, DIMENSION( ims:ime , jms:jme ), &
            INTENT(IN   )  ::               &
                                   mu1,     &
                                   mut,     &
                                   muave,   &
                                   muts,    &
                                   ht,      &
                                   msftx,   &
                                   msfty

      REAL, DIMENSION( kms:kme ),  INTENT(IN   )  :: fnp,     &
                                                     fnm,     &
                                                     rdnw,    &
                                                     rdn,     &
                                                     dnw

      REAL,   INTENT(IN   )  :: rdx,     &
                                rdy,     &
                                dts,     &
                                cf1,     &
                                cf2,     &
                                cf3,     &
                                t0,      &
                                epssm



      REAL, DIMENSION( its:ite ) :: mut_inv, msft_inv
      REAL, DIMENSION( its:ite, kts:kte ) :: rhs, wdwn
      INTEGER :: i,j,k, i_start, i_end, j_start, j_end, k_start, k_end
      REAL, DIMENSION (kts:kte) :: dampwt
      real :: htop,hbot,hdepth,hk
      real    :: pi,dampmag










      i_start = its
      i_end   = min(ite,ide-1)
      j_start = jts
      j_end   = min(jte,jde-1)
      k_start = kts
      k_end   = kte-1
      IF ( .NOT. config_flags%periodic_x )THEN
         IF ( config_flags%specified .or. config_flags%nested ) then
           i_start = max(its,ids+1)
           i_end   = min(ite,ide-2)
         ENDIF
      ENDIF
      IF ( config_flags%specified .or. config_flags%nested ) then
         j_start = max(jts,jds+1)
         j_end   = min(jte,jde-2)
      ENDIF

   pi = 4.*atan(1.)
   dampmag = dts*config_flags%dampcoef
   hdepth=config_flags%zdamp























    DO i=i_start, i_end
      rhs(i,1) = 0.
    ENDDO

  j_loop_w:  DO j = j_start, j_end
    DO i=i_start, i_end
       mut_inv(i) = 1./mut(i,j)
       msft_inv(i) = 1./msfty(i,j)
    ENDDO

    DO k=1, k_end
    DO i=i_start, i_end
      t_2ave(i,k,j)=.5*((1.+epssm)*t_2(i,k,j)       &
                    +(1.-epssm)*t_2ave(i,k,j))
      t_2ave(i,k,j)=(t_2ave(i,k,j) + muave(i,j)*t0) &
                    /(muts(i,j)*(t0+t_1(i,k,j)))
      wdwn(i,k+1)=.5*(ww(i,k+1,j)+ww(i,k,j))*rdnw(k)    &
           *(ph_1(i,k+1,j)-ph_1(i,k,j)+phb(i,k+1,j)-phb(i,k,j))
      rhs(i,k+1) = dts*(ph_tend(i,k+1,j) + .5*g*(1.-epssm)*w(i,k+1,j))

    ENDDO
    ENDDO




    DO k=2,k_end
    DO i=i_start, i_end
       rhs(i,k) = rhs(i,k)-dts*( fnm(k)*wdwn(i,k+1)  &
                                +fnp(k)*wdwn(i,k  ) )
    ENDDO
    ENDDO











    DO k=2,k_end+1
    DO i=i_start, i_end
      rhs(i,k) = ph(i,k,j) + msfty(i,j)*rhs(i,k)*mut_inv(i)
      if(top_lid .and. k.eq.k_end+1)rhs(i,k)=0.
    ENDDO
    ENDDO














    DO i=i_start, i_end
       w(i,1,j)=                                           &

                msfty(i,j)*.5*rdy*(                        &
                         (ht(i,j+1)-ht(i,j  ))             &
        *(cf1*v(i,1,j+1)+cf2*v(i,2,j+1)+cf3*v(i,3,j+1))    &
                        +(ht(i,j  )-ht(i,j-1))             &
        *(cf1*v(i,1,j  )+cf2*v(i,2,j  )+cf3*v(i,3,j  ))  ) &

               +msftx(i,j)*.5*rdx*(                        &
                         (ht(i+1,j)-ht(i,j  ))             &
        *(cf1*u(i+1,1,j)+cf2*u(i+1,2,j)+cf3*u(i+1,3,j))    &
                        +(ht(i,j  )-ht(i-1,j))             &
        *(cf1*u(i  ,1,j)+cf2*u(i  ,2,j)+cf3*u(i  ,3,j))  ) 

     ENDDO






    DO k=2,k_end
      DO i=i_start, i_end
        w(i,k,j)=w(i,k,j)+dts*rw_tend(i,k,j)                       &
                 + msft_inv(i)*cqw(i,k,j)*(                        &
            +.5*dts*g*mut_inv(i)*rdn(k)*                           &
             (c2a(i,k  ,j)*rdnw(k  )                               &
        *((1.+epssm)*(rhs(i,k+1  )-rhs(i,k    ))                   &
         +(1.-epssm)*(ph(i,k+1,j)-ph(i,k  ,j)))                    &
             -c2a(i,k-1,j)*rdnw(k-1)                               &
        *((1.+epssm)*(rhs(i,k    )-rhs(i,k-1  ))                   &
         +(1.-epssm)*(ph(i,k  ,j)-ph(i,k-1,j)))))                  &

                +dts*g*msft_inv(i)*(rdn(k)*                        &
             (c2a(i,k  ,j)*alt(i,k  ,j)*t_2ave(i,k  ,j)            &
             -c2a(i,k-1,j)*alt(i,k-1,j)*t_2ave(i,k-1,j))           &
              - muave(i,j))
      ENDDO
    ENDDO

    K=k_end+1

    DO i=i_start, i_end
       w(i,k,j)=w(i,k,j)+dts*rw_tend(i,k,j)                         &
           +msft_inv(i)*(                                        &
         -.5*dts*g*mut_inv(i)*rdnw(k-1)**2*2.*c2a(i,k-1,j)            &
             *((1.+epssm)*(rhs(i,k  )-rhs(i,k-1  ))                 &
              +(1.-epssm)*(ph(i,k,j)-ph(i,k-1,j)))                  &
         -dts*g*(2.*rdnw(k-1)*                                      &
                   c2a(i,k-1,j)*alt(i,k-1,j)*t_2ave(i,k-1,j)        &
              + muave(i,j)) )
       if(top_lid)w(i,k,j) = 0.
    ENDDO

    DO k=2,k_end+1
    DO i=i_start, i_end
       w(i,k,j)=(w(i,k,j)-a(i,k,j)*w(i,k-1,j))*alpha(i,k,j)
    ENDDO
    ENDDO

    DO k=k_end,2,-1
    DO i=i_start, i_end
       w (i,k,j)=w (i,k,j)-gamma(i,k,j)*w(i,k+1,j)
    ENDDO
    ENDDO

    IF (config_flags%damp_opt .eq. 3) THEN
      DO k=k_end+1,2,-1
      DO i=i_start, i_end
          htop=(ph_1(i,k_end+1,j)+phb(i,k_end+1,j))/g
          hk=(ph_1(i,k,j)+phb(i,k,j))/g
          hbot=htop-hdepth
          dampwt(k) = 0.
          if(hk .ge. hbot)then
            dampwt(k) = dampmag*sin(0.5*pi*(hk-hbot)/hdepth)*sin(0.5*pi*(hk-hbot)/hdepth)
          endif
          w(i,k,j) = (w(i,k,j) - dampwt(k)*mut(i,j)*w_save(i,k,j))/(1.+dampwt(k))
      ENDDO
      ENDDO
    ENDIF

    DO k=k_end+1,2,-1
    DO i=i_start, i_end
       ph(i,k,j) = rhs(i,k)+msfty(i,j)*.5*dts*g*(1.+epssm) &
                      *w(i,k,j)/muts(i,j)
    ENDDO
    ENDDO

  ENDDO j_loop_w

END SUBROUTINE advance_w



SUBROUTINE sumflux ( ru, rv, ww,                             &
                     u_lin, v_lin, ww_lin,                   &
                     muu, muv,                               &
                     ru_m, rv_m, ww_m, epssm,                &
                     msfux, msfuy, msfvx, msfvx_inv, msfvy,  &
                     iteration , number_of_small_timesteps,  &
                     ids,ide, jds,jde, kds,kde,              &
                     ims,ime, jms,jme, kms,kme,              &
                     its,ite, jts,jte, kts,kte              )


  IMPLICIT NONE  



  INTEGER,      INTENT(IN   )    :: number_of_small_timesteps
  INTEGER,      INTENT(IN   )    :: iteration
  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  INTENT(IN   ) :: ru, &
                                                                rv, &
                                                                ww, &
                                                                u_lin,  &
                                                                v_lin,  &
                                                                ww_lin


  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) , INTENT(INOUT) :: ru_m, &
                                                                rv_m, &
                                                                ww_m
  REAL, DIMENSION(ims:ime, jms:jme) , INTENT(IN   ) :: muu, muv,      &
                                                       msfux, msfuy,  &
                                                       msfvx, msfvy, msfvx_inv

  INTEGER :: mini, minj, mink


  REAL, INTENT(IN   )  ::  epssm
  INTEGER   :: i,j,k









    IF (iteration == 1 )THEN
      DO  j = jts, jte
      DO  k = kts, kte
      DO  i = its, ite
        ru_m(i,k,j)  = 0.
        rv_m(i,k,j)  = 0.
        ww_m(i,k,j)  = 0.
      ENDDO
      ENDDO
      ENDDO
    ENDIF

  mini = min(ide-1,ite)
  minj = min(jde-1,jte)
  mink = min(kde-1,kte)


    DO  j = jts, minj
    DO  k = kts, mink
    DO  i = its, mini
      ru_m(i,k,j)  = ru_m(i,k,j) + ru(i,k,j)
      rv_m(i,k,j)  = rv_m(i,k,j) + rv(i,k,j)
      ww_m(i,k,j)  = ww_m(i,k,j) + ww(i,k,j)
    ENDDO
    ENDDO
    ENDDO
 
    IF (ite .GT. mini) THEN
      DO  j = jts, minj
      DO  k = kts, mink
      DO  i = mini+1, ite
        ru_m(i,k,j)  = ru_m(i,k,j) + ru(i,k,j)
      ENDDO
      ENDDO
      ENDDO
    END IF
    IF (jte .GT. minj) THEN
      DO  j = minj+1, jte
      DO  k = kts, mink
      DO  i = its, mini
	rv_m(i,k,j)  = rv_m(i,k,j) + rv(i,k,j)
      ENDDO
      ENDDO
      ENDDO
    END IF
    IF ( kte .GT. mink) THEN
      DO  j = jts, minj
      DO  k = mink+1, kte
      DO  i = its, mini
        ww_m(i,k,j)  = ww_m(i,k,j) + ww(i,k,j)
      ENDDO
      ENDDO
      ENDDO
    END IF

  IF (iteration == number_of_small_timesteps) THEN

    DO  j = jts, minj
    DO  k = kts, mink
    DO  i = its, mini
      ru_m(i,k,j)  = ru_m(i,k,j) / number_of_small_timesteps   &
                     + muu(i,j)*u_lin(i,k,j)/msfuy(i,j)
      rv_m(i,k,j)  = rv_m(i,k,j) / number_of_small_timesteps   &
                     + muv(i,j)*v_lin(i,k,j)*msfvx_inv(i,j) 
      ww_m(i,k,j)  = ww_m(i,k,j) / number_of_small_timesteps   &
                     + ww_lin(i,k,j) 
    ENDDO
    ENDDO
    ENDDO


    IF (ite .GT. mini) THEN
      DO  j = jts, minj
      DO  k = kts, mink
      DO  i = mini+1, ite
        ru_m(i,k,j)  = ru_m(i,k,j) / number_of_small_timesteps   &
                     + muu(i,j)*u_lin(i,k,j)/msfuy(i,j)
      ENDDO
      ENDDO
      ENDDO
    END IF
    IF (jte .GT. minj) THEN
      DO  j = minj+1, jte
      DO  k = kts, mink
      DO  i = its, mini
	rv_m(i,k,j)  = rv_m(i,k,j) / number_of_small_timesteps   &
                     + muv(i,j)*v_lin(i,k,j)*msfvx_inv(i,j)
      ENDDO
      ENDDO
      ENDDO
    END IF
    IF ( kte .GT. mink) THEN
      DO  j = jts, minj
      DO  k = mink+1, kte
      DO  i = its, mini
        ww_m(i,k,j)  = ww_m(i,k,j) / number_of_small_timesteps   &
                     + ww_lin(i,k,j)
      ENDDO
      ENDDO
      ENDDO
    END IF

  ENDIF


END SUBROUTINE sumflux 



  SUBROUTINE init_module_small_step
  END SUBROUTINE init_module_small_step

END MODULE module_small_step_em
