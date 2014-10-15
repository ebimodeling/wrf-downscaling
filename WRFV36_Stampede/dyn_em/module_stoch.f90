module module_stoch

































































      implicit none
      public :: SETUP_STOCH_SKEBS, SETUP_STOCH_SPPT, UPDATE_STOCH,& 
                          do_fftback_along_x,do_fftback_along_y, SP2GP_prep

      INTEGER :: LMINFORC, LMAXFORC, KMINFORC, KMAXFORC, & 
      &          LMINFORCT, LMAXFORCT, KMINFORCT, KMAXFORCT
      REAL    :: ALPH, ALPH_PSI, ALPH_T, TOT_BACKSCAT_PSI, TOT_BACKSCAT_T,  REXPONENT_PSI,REXPONENT_T



      INTEGER :: LENSAV
      INTEGER,ALLOCATABLE:: wavenumber_k(:), wavenumber_l(:),ISEED(:)
      REAL, ALLOCATABLE :: WSAVE1(:),WSAVE2(:)


      REAL, PARAMETER:: RPI= 3.141592653589793 
      REAL, PARAMETER:: CP= 1006.0 
      REAL, PARAMETER:: T0= 300.0 

      save



contains






      subroutine SETUP_STOCH_SKEBS( &
                       VERTSTRUCC,VERTSTRUCS,                        & 
                       SPT_AMP,SPSTREAM_AMP,                         & 
                       VERTAMPT,VERTAMPUV,                           &
                       stoch_vertstruc_opt,                          &
                       ISEED1,ISEED2,itime_step,DX,DY,               &
                       TOT_BACKSCAT_PSI,TOT_BACKSCAT_T,              &
                       ZTAU_PSI,ZTAU_T,REXPONENT_PSI,REXPONENT_T,               &    
                       KMINFORC,KMAXFORCH,LMINFORC,LMAXFORCH,          & 
                       KMINFORCT,KMAXFORCTH,LMINFORCT,LMAXFORCTH,      & 
                       KMAXFORC,LMAXFORC,KMAXFORCT,LMAXFORCT, &
                       ZSIGMA2_EPSH,ZSIGMA2_ETAH,                      & 
                       ids, ide, jds, jde, kds, kde,                 &
                       ims, ime, jms, jme, kms, kme,                 &
                       its, ite, jts, jte, kts, kte                  )

      IMPLICIT NONE
      INTEGER :: IER,IK,IL,iseed1,iseed2,I,J
      INTEGER :: itime_step,stoch_vertstruc_opt
      INTEGER :: KMAX,LMAX,LENSAV,ILEV
      INTEGER , INTENT(IN)     ::  ids, ide, jds, jde, kds, kde,   &
                                   ims, ime, jms, jme, kms, kme,   &
                                   its, ite, jts, jte, kts, kte
      INTEGER :: KMINFORC,LMINFORC,KMINFORCT,LMINFORCT 
      INTEGER :: KMAXFORC,LMAXFORC,KMAXFORCT,LMAXFORCT 
      INTEGER :: KMAXFORCH,LMAXFORCH,KMAXFORCTH,LMAXFORCTH
      REAL    :: ZSIGMA2_EPSH,ZSIGMA2_ETAH 
      REAL    :: DX,DY,RY,RX,RATIO_BACKSCAT,TOT_BACKSCAT_PSI,TOT_BACKSCAT_T
      REAL    :: ZGAMMAN,ZGAMMAT,ZTAU_PSI,ZTAU_T,ZCONSTF0,ZCONSTF0T,ZSIGMA2_EPS,ZSIGMA2_ETA,RHOKLMAX,ZREF,RHOKL,EPS
      REAL    :: REXPONENT_PSI,REXPONENT_T
      REAL    :: ZNORM1,ZNORM2
      REAL, DIMENSION (ims:ime,kms:kme,jms:jme) :: VERTSTRUCC,VERTSTRUCS
      REAL, DIMENSION (ims:ime,jms:jme)         :: SPSTREAM_AMP,SPT_AMP
      REAL, DIMENSION (ids:ide,jds:jde)         :: ZCHI,ZCHIT
      REAL, DIMENSION (kms:kme          )       :: VERTAMPT,VERTAMPUV
      LOGICAL :: is_print = .true.
      INTEGER , ALLOCATABLE , DIMENSION(:) :: iseed
      INTEGER :: how_many

      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor

    

      KMAX=(jde-jds)+1 
      LMAX=(ide-ids)+1 
      RY=  KMAX*DY
      RX=  LMAX*DX
      LENSAV= 4*(KMAX+LMAX)+INT(LOG(REAL(KMAX))) + INT(LOG(REAL(LMAX))) + 8


      IF ( ALLOCATED(WSAVE1)      ) DEALLOCATE(WSAVE1)
      IF ( ALLOCATED(WSAVE2)      ) DEALLOCATE(WSAVE2)
      ALLOCATE(WSAVE1(LENSAV),WSAVE2(LENSAV))

      IF ( ALLOCATED(WAVENUMBER_K)) DEALLOCATE(WAVENUMBER_K)
      IF ( ALLOCATED(WAVENUMBER_L)) DEALLOCATE(WAVENUMBER_L)
      ALLOCATE (wavenumber_k(jds:jde),wavenumber_l(ids:ide))


      call CFFT1I (LMAX, WSAVE1, LENSAV, IER)
      if(ier.ne. 0) write(*,95) ier

      call CFFT1I (KMAX, WSAVE2, LENSAV, IER)
      if(ier.ne. 0) write(*,95) ier
      95 format('error in cFFT2I=  'i5)

      call findindex( wavenumber_k, wavenumber_l,             &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )




     KMAXFORC =min0(((ide-ids)+1)/2,((jde-jds)+1 )/2)-5
     LMAXFORC =KMAXFORC
     KMAXFORCT=min0(((ide-ids)+1)/2,((jde-jds)+1 )/2)-5
     LMAXFORCT=KMAXFORCT
     if (KMAXFORC > KMAXFORCH) then
        KMAXFORC=KMAXFORCH
     endif
     if (LMAXFORC > LMAXFORCH) then
        LMAXFORC=LMAXFORCH
     endif
     if (KMAXFORCT > KMAXFORCTH) then
        KMAXFORCT=KMAXFORCTH
     endif
     if (LMAXFORCT > LMAXFORCTH) then
        LMAXFORCT=LMAXFORCTH
     endif


      ALPH_PSI   =  float(itime_step)/ZTAU_PSI 
      ALPH_T   =  float(itime_step)/ZTAU_PSI 
      ZSIGMA2_EPS=1./(12.0*ALPH_PSI)
      ZSIGMA2_ETA=1./(12.0*ALPH_T)


      if (is_print) then
      WRITE(*,'(''                                               '')')
      WRITE(*,'('' =============================================='')')
      WRITE(*,'('' >> Initializing stochastic kinetic-energy backscatter scheme << '')')
      WRITE(*,'('' Total backscattered energy, TOT_BACKSCAT_PSI '',E12.5)') TOT_BACKSCAT_PSI
      WRITE(*,'('' Total backscattered temperature, TOT_BACKSCAT_T '',E12.5)') TOT_BACKSCAT_T
      WRITE(*,'('' Exponent for energy spectra, REXPONENT_PSI ='',E12.5)') REXPONENT_PSI
      WRITE(*,'('' Exponent for temperature spectra, REXPONENT_T ='',E12.5)') REXPONENT_T
      WRITE(*,'('' Minimal wavenumber of streamfunction forcing, LMINFORC ='',I10)') LMINFORC
      WRITE(*,'('' Maximal wavenumber of streamfunction forcing, LMAXFORC ='',I10)') LMAXFORC
      WRITE(*,'('' Minimal wavenumber of streamfunction forcing, KMINFORC ='',I10)') KMINFORC
      WRITE(*,'('' Maximal wavenumber of streamfunction forcing, KMAXFORC ='',I10)') KMAXFORC
      WRITE(*,'('' Minimal wavenumber of temperature forcing, LMINFORCT ='',I10)') LMINFORCT
      WRITE(*,'('' Maximal wavenumber of temperature forcing, LMAXFORCT ='',I10)') LMAXFORCT
      WRITE(*,'('' Minimal wavenumber of temperature forcing, KMINFORCT ='',I10)') KMINFORCT
      WRITE(*,'('' Maximal wavenumber of temperature forcing, KMAXFORCT ='',I10)') KMAXFORCT
      WRITE(*,'('' stoch_vertstruc_opt                             '',I10)') stoch_vertstruc_opt
      WRITE(*,'('' Time step: itime_step='',I10)') itime_step 
      WRITE(*,'('' Decorrelation time of noise, ZTAU_PSI ='',E12.5)') ZTAU_PSI
      WRITE(*,'('' Decorrelation time of noise, ZTAU_T ='',E12.5)') ZTAU_T
      WRITE(*,'('' Variance of noise, ZSIGMA2_EPS  ='',E12.5)') ZSIGMA2_EPS
      WRITE(*,'('' Variance of noise, ZSIGMA2_ETA  ='',E12.5)') ZSIGMA2_ETA
      WRITE(*,'('' Autoregressive parameter 1-ALPH_PSI ='',E12.5)') 1.-ALPH_PSI
      WRITE(*,'('' Autoregressive parameter 1-ALPH_T ='',E12.5)') 1.-ALPH_T
      WRITE(*,'('' =============================================='')')
      endif 






      ZCHI    =  0.0
      ZGAMMAN  =  0.0
      
       DO IK=jds-1,jde   
       DO IL=ids-1,ide  
       if (((sqrt((IK/RY*IK/RY)+(IL/RX*IL/RX)).lt.((KMAXFORC+0.5)/RX)).and.&
            (sqrt((IK/RY*IK/RY)+(IL/RX*IL/RX)).ge.((KMINFORC-0.5)/RX))) .or. & 
           ((sqrt((IK/RY*IK/RY)+(IL/RX*IL/RX)).lt.((LMAXFORC+0.5)/RX)).and.&
            (sqrt((IK/RY*IK/RY)+(IL/RX*IL/RX)).ge.((LMINFORC-0.5)/RX))))then 
         if ((IK>0).or.(IL>0)) then 
           ZCHI(IL+1,IK+1)=((IK/RY*IK/RY)+(IL/RX*IL/RX))**(REXPONENT_PSI/2.)
           ZGAMMAN= ZGAMMAN + ((IK/RY*IK/RY)+(IL/RX*IL/RX))**(REXPONENT_PSI+1)
         endif
       endif
       enddo
       enddo
       ZGAMMAN=4.0*ZGAMMAN 
       ZCONSTF0=SQRT(ALPH_PSI*TOT_BACKSCAT_PSI/(float(itime_step)*ZSIGMA2_EPS*ZGAMMAN))/(2*RPI)

      ZCHIT    =  0.0
      ZGAMMAT  =  0.0
      
       DO IK=jds-1,jde   
       DO IL=ids-1,ide  
       if (((sqrt((IK/RY*IK/RY)+(IL/RX*IL/RX)).lt.((KMAXFORCT+0.5)/RX)).and.&
            (sqrt((IK/RY*IK/RY)+(IL/RX*IL/RX)).ge.((KMINFORCT-0.5)/RX))) .or. & 
           ((sqrt((IK/RY*IK/RY)+(IL/RX*IL/RX)).lt.((LMAXFORCT+0.5)/RX)).and.&
            (sqrt((IK/RY*IK/RY)+(IL/RX*IL/RX)).ge.((LMINFORCT-0.5)/RX))))then 
         if ((IK>0).or.(IL>0)) then 
           ZCHIT(IL+1,IK+1)=((IK/RY*IK/RY)+(IL/RX*IL/RX))**(REXPONENT_T/2.)
           ZGAMMAT= ZGAMMAT + ((IK/RY*IK/RY)+(IL/RX*IL/RX))**(REXPONENT_T)
         endif
       endif
       enddo
       enddo
       ZGAMMAT=4.0*ZGAMMAT 






      ZCONSTF0T=SQRT(T0*ALPH_T*TOT_BACKSCAT_T/(float(itime_step)*cp*ZSIGMA2_ETA*ZGAMMAT))
 



      SPSTREAM_AMP=0.0
      SPT_AMP=0.0
      DO IK=jts,jte
      DO IL=its,ite
      if ((IL .le. (LMAX/2+1)) .and. (IK .le. (KMAX/2+1)) ) then
        SPSTREAM_AMP(IL,IK) = ZCONSTF0 *ZCHI(IL,IK) 
        SPT_AMP(IL,IK)      = ZCONSTF0T*ZCHIT(IL,IK)
      endif
      ENDDO
      ENDDO

      
      
      DO IK=jts,jte
      DO IL=its,ite
      if ( (IL .gt. (LMAX/2+1)) .and. (IK .le. (KMAX/2+1)) ) then
        SPSTREAM_AMP(IL,IK) =  ZCONSTF0 *ZCHI(LMAX-IL+2,IK)
        SPT_AMP(IL,IK)      =  ZCONSTF0T*ZCHIT(LMAX-IL+2,IK)
      endif
      ENDDO
      ENDDO


      DO IK=jts,jte
      DO IL=its,ite
      
      if ((IK .gt. (KMAX/2+1)) .and. (IL.le.(LMAX/2+1)) ) then
        SPSTREAM_AMP(IL,IK) = ZCONSTF0 *ZCHI(IL,KMAX-IK+2)
        SPT_AMP(IL,IK)      = ZCONSTF0T*ZCHIT(IL,KMAX-IK+2)
      endif
      ENDDO
      ENDDO


      DO IK=jts,jte
      DO IL=its,ite
      
       if ((IK .gt. (KMAX/2+1)) .and. (IL.gt.(LMAX/2+1)) ) then
        SPSTREAM_AMP(IL,IK) = ZCONSTF0 *ZCHI(LMAX-IL+2,KMAX-IK+2)
        SPT_AMP(IL,IK)      = ZCONSTF0T*ZCHIT(LMAX-IL+2,KMAX-IK+2)
      endif
      ENDDO
      ENDDO




      IF (stoch_vertstruc_opt==1) then
        VERTSTRUCC=0.0
        VERTSTRUCS=0.0
        RHOKLMAX= sqrt(KMAX**2/DY**2 + LMAX**2/DX**2)
        ZREF=32.0
        DO ILEV=kts,kte
          DO IK=jts,jte
            DO IL=its,ite
            if (IL.le.(LMAX/2)) then
              RHOKL   = sqrt((IK+1)**2/DY**2 + (IL+1)**2/DX**2)
              EPS     = ((RHOKLMAX - RHOKL)/ RHOKLMAX)  * (ILEV/ZREF) * RPI
              VERTSTRUCC(IL,ILEV,IK) =  cos ( eps* (IL+1) )
              VERTSTRUCS(IL,ILEV,IK) =  sin ( eps* (IL+1) )
             else
              RHOKL   = sqrt((IK+1)**2/DY**2 + (LMAX-IL+2)**2/DX**2)
              EPS     = ((RHOKLMAX - RHOKL)/ RHOKLMAX)  * (ILEV/ZREF) * RPI
              VERTSTRUCC (IL,ILEV,IK) =   cos ( eps* (LMAX-IL+2) )
              VERTSTRUCS (IL,ILEV,IK) = - sin ( eps* (LMAX-IL+2) )
            endif
            ENDDO
          ENDDO
        ENDDO
      ELSEIF (stoch_vertstruc_opt==2) then
        VERTAMPT=1.0 
        VERTAMPUV=1.0 
      ENDIF
             


       CALL random_seed(size=how_many)
      IF ( ALLOCATED(iseed)) DEALLOCATE(iseed)
       ALLOCATE(iseed(how_many))
       IF ( wrf_dm_on_monitor() ) THEN
          iseed=0
          iseed(1) = iseed1
          iseed(2) = iseed2
          call random_seed(put=iseed(1:how_many))   
          call random_seed(get=iseed(1:how_many))
       END IF

       CALL wrf_dm_bcast_integer ( iseed , how_many )
       CALL random_seed(put=iseed(1:how_many))   


       END subroutine SETUP_STOCH_SKEBS





      subroutine SETUP_STOCH_SPPT( &
                       VERTSTRUCC,VERTSTRUCS,                        & 
                       SPT_AMP,                                      & 
                       SPTFORCC,SPTFORCS,                            & 
                       VERTAMPT,VERTAMPUV,                           &
                       stoch_vertstruc_opt,                          &
                       ISEED1,ISEED2,itime_step,DX,DY,               &
                       gridpointvariance, l_sppt, tau_sppt,          &
                       KMINFORCT,KMAXFORCTH,LMINFORCT,LMAXFORCTH,    & 
                       KMAXFORCT,LMAXFORCT,                          &
                       ids, ide, jds, jde, kds, kde,                 &
                       ims, ime, jms, jme, kms, kme,                 &
                       its, ite, jts, jte, kts, kte                  )


      IMPLICIT NONE
      INTEGER :: IER,IK,IL,iseed1,iseed2,I,J
      INTEGER :: itime_step,stoch_vertstruc_opt
      INTEGER :: KMAX,LMAX,LENSAV,ILEV
      INTEGER , INTENT(IN)     ::  ids, ide, jds, jde, kds, kde,   &
                                   ims, ime, jms, jme, kms, kme,   &
                                   its, ite, jts, jte, kts, kte
      REAL    :: DX,DY,RY,RX,RATIO_BACKSCAT,TOT_BACKSCAT_PSI,TOT_BACKSCAT_T
      REAL    :: ZGAMMAN,ZCONSTF0,ZCONSTF0T,ZSIGMA2_EPS, RHOKLMAX,ZREF,RHOKL,EPS
      REAL    :: z,phi,gridpointvariance,kappat,tau_sppt,l_sppt,sum
      INTEGER :: KMINFORCT,LMINFORCT,KMAXFORCT,LMAXFORCT,KMAXFORCTH,LMAXFORCTH
      REAL, DIMENSION (kms:kme          )       :: VERTAMPT,VERTAMPUV
      REAL, DIMENSION (ims:ime,kms:kme,jms:jme) :: VERTSTRUCC,VERTSTRUCS
      REAL, DIMENSION (ims:ime,jms:jme)         :: SPSTREAM_AMP,SPT_AMP
      REAL, DIMENSION (ids:ide,jds:jde)         :: ZCHI,ZCHIT
      REAL, DIMENSION (ims:ime,jms:jme)         :: SPTFORCS,SPTFORCC
      REAL, DIMENSION (ims:ime,jms:jme) ::   var_sigma1
      LOGICAL :: is_print = .true.
      INTEGER , ALLOCATABLE , DIMENSION(:) :: iseed
      INTEGER :: how_many

      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor

    

      KMAX=(jde-jds)+1 
      LMAX=(ide-ids)+1 
      RY=  KMAX*DY
      RX=  LMAX*DY
      LENSAV= 4*(KMAX+LMAX)+INT(LOG(REAL(KMAX))) + INT(LOG(REAL(LMAX))) + 8



      IF ( ALLOCATED(WSAVE1)      ) DEALLOCATE(WSAVE1)
      IF ( ALLOCATED(WSAVE2)      ) DEALLOCATE(WSAVE2)
      ALLOCATE(WSAVE1(LENSAV),WSAVE2(LENSAV))

      IF ( ALLOCATED(WAVENUMBER_K)) DEALLOCATE(WAVENUMBER_K)
      IF ( ALLOCATED(WAVENUMBER_L)) DEALLOCATE(WAVENUMBER_L)
      ALLOCATE (wavenumber_k(jds:jde),wavenumber_l(ids:ide))


      call CFFT1I (LMAX, WSAVE1, LENSAV, IER)
      if(ier.ne. 0) write(*,95) ier

      call CFFT1I (KMAX, WSAVE2, LENSAV, IER)
      if(ier.ne. 0) write(*,95) ier

      95 format('error in cFFT2I=  'i5)

      call findindex( wavenumber_k, wavenumber_l,             &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )



      kappat= L_sppt**2 
      phi = exp (-float(itime_step)/tau_sppt)
      alph = 1.-phi

     KMAXFORCT=min0(((ide-ids)+1)/2,((jde-jds)+1 )/2)-5
     LMAXFORCT=KMAXFORCT
     if (KMAXFORCT > KMAXFORCTH) then
        KMAXFORCT=KMAXFORCTH
     endif
     if (LMAXFORCT > LMAXFORCTH) then
        LMAXFORCT=LMAXFORCTH
     endif


      if (is_print) then
      WRITE(*,'(''                                               '')')
      WRITE(*,'('' =============================================='')')
      WRITE(*,'('' >> Initializing stochastically perturbed physical tendencies (SPPT) scheme << '')')
      WRITE(*,'('' Minimal wavenumber of temperature forcing, KMINFORCT ='',I10)') KMINFORCT
      WRITE(*,'('' Maximal wavenumber of temperature forcing, KMAXFORCT ='',I10)') KMAXFORCT
      WRITE(*,'('' stoch_vertstruc_opt                             '',I10)') stoch_vertstruc_opt
      WRITE(*,'('' Time step: itime_step='',I10)') itime_step
      WRITE(*,'('' Decorrelation time of noise, tau_sppt ='',E12.5)') TAU_sppt
      WRITE(*,'('' Autoregressive parameter phi ='',E12.5)') phi
      WRITE(*,'('' Length Scale l_sppt'',E12.5)') l_sppt
      WRITE(*,'('' Variance in gridpoint space'',E12.5)') gridpointvariance
      WRITE(*,'('' =============================================='')')
      endif

















     ZCHIT    =  0.0
     ZGAMMAN  =  0.0
      
      DO IK=jds-1,jde   
      DO IL=ids-1,ide
      if (((sqrt((IK/RY*IK/RY)+(IL/RX*IL/RX)).lt.((KMAXFORCT+0.5)/RX)).and.&
           (sqrt((IK/RY*IK/RY)+(IL/RX*IL/RX)).ge.((KMINFORCT-0.5)/RX))) .or. &
          ((sqrt((IK/RY*IK/RY)+(IL/RX*IL/RX)).lt.((LMAXFORCT+0.5)/RX)).and.&
           (sqrt((IK/RY*IK/RY)+(IL/RX*IL/RX)).ge.((LMINFORCT-0.5)/RX))))then
        if ((IK>0).or.(IL>0)) then
          ZCHIT(IL+1,IK+1)=exp(  -2*RPI**2*kappat*((IK/RY*IK/RY)+(IL/RX*IL/RX)) ) 
          ZGAMMAN= ZGAMMAN + exp(  -4*RPI**2*kappat*((IK/RY*IK/RY)+(IL/RX*IL/RX)) ) 
        endif
      endif
      enddo
      enddo
      ZGAMMAN=4.0*ZGAMMAN 
      ZCONSTF0T= sqrt(gridpointvariance*(1.-phi**2)/(2.*ZGAMMAN))





      SPT_AMP=0.0
      DO IK=jts,jte
      DO IL=its,ite
      if ((IL .le. (LMAX/2+1)) .and. (IK .le. (KMAX/2+1)) ) then
        SPT_AMP(IL,IK)      = ZCONSTF0T*ZCHIT(IL,IK)
      endif
      ENDDO
      ENDDO

      
      
      DO IK=jts,jte
      DO IL=its,ite
      if ( (IL .gt. (LMAX/2+1)) .and. (IK .le. (KMAX/2+1)) ) then
        SPT_AMP(IL,IK)      =  ZCONSTF0T*ZCHIT(LMAX-IL+2,IK)
      endif
      ENDDO
      ENDDO


      DO IK=jts,jte
      DO IL=its,ite
      if ((IK .gt. (KMAX/2+1)) .and. (IL.le.LMAX/2) ) then
        SPT_AMP(IL,IK)      = ZCONSTF0T*ZCHIT(IL,KMAX-IK+2)
      endif
      ENDDO
      ENDDO


      DO IK=jts,jte
      DO IL=its,ite
      if ((IK .gt. (KMAX/2+1)) .and. (IL.gt.LMAX/2) ) then
        SPT_AMP(IL,IK)      = ZCONSTF0T*ZCHIT(LMAX-IL+2,KMAX-IK+2)
      endif
      ENDDO
      ENDDO

      IF (stoch_vertstruc_opt>0) then
      VERTSTRUCC=0.0
      VERTSTRUCS=0.0
      RHOKLMAX= sqrt(KMAX**2/DY**2 + LMAX**2/DX**2)
      ZREF=32.0
      DO ILEV=kds,kde
        DO IK=jts,jte
          DO IL=its,ite
          if (IL.le.(LMAX/2)) then
            RHOKL   = sqrt((IK+1)**2/DY**2 + (IL+1)**2/DX**2)
            EPS     = ((RHOKLMAX - RHOKL)/ RHOKLMAX)  * (ILEV/ZREF) * RPI
            VERTSTRUCC(IL,ILEV,IK) =  cos ( eps* (IL+1) )
            VERTSTRUCS(IL,ILEV,IK) =  sin ( eps* (IL+1) )
            VERTSTRUCC (LMAX-IL+1,ILEV,IK) =   cos ( eps* (IL+1) )
            VERTSTRUCS (LMAX-IL+1,ILEV,IK) = - sin ( eps* (IL+1) )
          endif
          ENDDO
        ENDDO
      ENDDO
      ENDIF

      IF (stoch_vertstruc_opt>1) then
      
      
      
      
        VERTAMPT=0.0
        VERTAMPUV=0.0
        DO ILEV=1,kde-3
           VERTAMPT(ILEV+3)=tanh(float(kde-ilev-3))+tanh(float(ilev)/2.5)-1.0
        ENDDO
      ENDIF



       CALL random_seed(size=how_many)
      IF ( ALLOCATED(iseed)) DEALLOCATE(iseed)
       ALLOCATE(iseed(how_many))
       IF ( wrf_dm_on_monitor() ) THEN
          iseed=0
          iseed(1) = iseed1
          iseed(2) = iseed2
          call random_seed(put=iseed(1:how_many))   
          call random_seed(get=iseed(1:how_many))
       END IF

       CALL wrf_dm_bcast_integer ( iseed , how_many )
       CALL random_seed(put=iseed(1:how_many))   



       DO IK=jts,jte
         DO IL=its,ite
           call gauss_noise(z)
           SPTFORCC(IL,IK) = (1.-phi**2)**(0.5)*SPT_AMP(IL,IK)*z
           call gauss_noise(z)
           SPTFORCS(IL,IK) = (1.-phi**2)**(0.5)*SPT_AMP(IL,IK)*z
         ENDDO
       ENDDO


       END subroutine SETUP_STOCH_SPPT





      subroutine UPDATE_STOCH(                                         & 
                       SPSTREAMFORCS,SPSTREAMFORCC,SPTFORCS,SPTFORCC,  &
                       SPT_AMP,SPSTREAM_AMP,                           &
                       ids, ide, jds, jde, kds, kde,                   &
                       ims, ime, jms, jme, kms, kme,                   &
                       its, ite, jts, jte, kts, kte                    )

      IMPLICIT NONE

      REAL, DIMENSION( ids:ide,jds:jde)      :: ZRANDNOSS1,ZRANDNOSC1,ZRANDNOSS2,ZRANDNOSC2
      REAL, DIMENSION (ims:ime,jms:jme)      :: SPSTREAMFORCS,SPSTREAMFORCC,SPTFORCS,SPTFORCc,SPSTREAM_AMP,SPT_AMP
      INTEGER , INTENT(IN)     ::               ids, ide, jds, jde, kds, kde,   &
                                                ims, ime, jms, jme, kms, kme,   &
                                                its, ite, jts, jte, kts, kte
    
      REAL :: Z
      REAL, PARAMETER :: thresh = 3.0 
      INTEGER ::IL, IK,LMAX,KMAX
      LOGICAL :: LGAUSS

      KMAX=(jde-jds)+1 
      LMAX=(ide-ids)+1 




      LGAUSS=.true.
      IF (LGAUSS) then
        DO IK=jds,jde
          DO IL=ids,ide
           do
            call gauss_noise(z)
            if (abs(z)<thresh) exit
           ENDDO
           ZRANDNOSS1(IL,IK)=z
           do
            call gauss_noise(z)
            if (abs(z)<thresh) exit
           ENDDO
           ZRANDNOSC1(IL,IK)=z
           do
            call gauss_noise(z)
            if (abs(z)<thresh) exit
           ENDDO
           ZRANDNOSS2(IL,IK)=z
           do
            call gauss_noise(z)
            if (abs(z)<thresh) exit
           ENDDO
           ZRANDNOSC2(IL,IK)=z
          ENDDO
        ENDDO
      ELSE
        DO IK=jds,jde
          DO IL=ids,ide
            CALL RANDOM_NUMBER(z)
            ZRANDNOSS1(IL,IK)=z-0.5
            CALL RANDOM_NUMBER(z)
            ZRANDNOSC1(IL,IK)=z-0.5
            CALL RANDOM_NUMBER(z)
            ZRANDNOSS2(IL,IK)=z-0.5
            CALL RANDOM_NUMBER(z)
            ZRANDNOSC2(IL,IK)=z-0.5
          ENDDO
        ENDDO
      ENDIF




      DO IK=jts,jte
      if ((IK.le.(KMAX/2+1)) .and. (IK>1)) then 
        DO IL=its,ite
          SPSTREAMFORCC(IL,IK)  = (1.-ALPH_PSI)*SPSTREAMFORCC(IL,IK) + SPSTREAM_AMP(IL,IK)* ZRANDNOSC1(IL,IK)
          SPSTREAMFORCS(IL,IK)  = (1.-ALPH_PSI)*SPSTREAMFORCS(IL,IK) + SPSTREAM_AMP(IL,IK)* ZRANDNOSS1(IL,IK) 
          SPTFORCC(IL,IK)       = (1.-ALPH_T)*SPTFORCC(IL,IK)      + SPT_AMP(IL,IK)     * ZRANDNOSC2(IL,IK)  
          SPTFORCS(IL,IK)       = (1.-ALPH_T)*SPTFORCS(IL,IK)      + SPT_AMP(IL,IK)     * ZRANDNOSS2(IL,IK)  
        ENDDO
      ELSEIF (IK==1) then
        DO IL=its,ite
        if ((IL.le.(LMAX/2+1))) then
          SPSTREAMFORCC(IL,IK)  = (1.-ALPH_PSI)*SPSTREAMFORCC(IL,IK) + SPSTREAM_AMP(IL,IK)* ZRANDNOSC1(IL,IK)
          SPSTREAMFORCS(IL,IK)  = (1.-ALPH_PSI)*SPSTREAMFORCS(IL,IK) + SPSTREAM_AMP(IL,IK)* ZRANDNOSS1(IL,IK) 
          SPTFORCC(IL,IK)       = (1.-ALPH_T)*SPTFORCC(IL,IK)      + SPT_AMP(IL,IK)     * ZRANDNOSC2(IL,IK)  
          SPTFORCS(IL,IK)       = (1.-ALPH_T)*SPTFORCS(IL,IK)      + SPT_AMP(IL,IK)     * ZRANDNOSS2(IL,IK)  
        elseif ((IL.gt.(LMAX/2+1))) then
          SPSTREAMFORCC(IL,IK)  = (1.-ALPH_PSI)*SPSTREAMFORCC(IL,IK) + SPSTREAM_AMP(IL,IK)* ZRANDNOSC1(LMAX-IL+2,IK)
          SPSTREAMFORCS(IL,IK)  = (1.-ALPH_PSI)*SPSTREAMFORCS(IL,IK) - SPSTREAM_AMP(IL,IK)* ZRANDNOSS1(LMAX-IL+2,IK) 
          SPTFORCC(IL,IK)       = (1.-ALPH_T)*SPTFORCC(IL,IK)      + SPT_AMP(IL,IK)     * ZRANDNOSC2(LMAX-IL+2,IK)  
          SPTFORCS(IL,IK)       = (1.-ALPH_T)*SPTFORCS(IL,IK)      - SPT_AMP(IL,IK)     * ZRANDNOSS2(LMAX-IL+2,IK)  
        endif
        ENDDO
      ENDIF
      ENDDO

      DO IK=jts,jte
      if (IK.gt.(KMAX/2+1)) then 
        DO IL=its,ite
          if (IL.le.(LMAX/2+1).and.(IL.gt.1)) then 
           SPSTREAMFORCC(IL,IK) = (1.-ALPH_PSI)* SPSTREAMFORCC(IL,IK) + SPSTREAM_AMP(IL,IK) * ZRANDNOSC1(LMAX-IL+2,KMAX-IK+2)
           SPSTREAMFORCS(IL,IK) = (1.-ALPH_PSI)* SPSTREAMFORCS(IL,IK) - SPSTREAM_AMP(IL,IK) * ZRANDNOSS1(LMAX-IL+2,KMAX-IK+2)
           SPTFORCC(IL,IK)      = (1.-ALPH_T)* SPTFORCC(IL,IK)      + SPT_AMP(IL,IK)      * ZRANDNOSC2(LMAX-IL+2,KMAX-IK+2)
           SPTFORCS(IL,IK)      = (1.-ALPH_T)* SPTFORCS(IL,IK)      - SPT_AMP(IL,IK)      * ZRANDNOSS2(LMAX-IL+2,KMAX-IK+2)
          elseif (IL.eq.1) then 
           SPSTREAMFORCC(IL,IK) = (1.-ALPH_PSI)* SPSTREAMFORCC(IL,IK) + SPSTREAM_AMP(IL,IK) * ZRANDNOSC1(        1,KMAX-IK+2)
           SPSTREAMFORCS(IL,IK) = (1.-ALPH_PSI)* SPSTREAMFORCS(IL,IK) - SPSTREAM_AMP(IL,IK) * ZRANDNOSS1(        1,KMAX-IK+2)
           SPTFORCC(IL,IK)      = (1.-ALPH_T)* SPTFORCC(IL,IK)      + SPT_AMP(IL,IK)      * ZRANDNOSC2(        1,KMAX-IK+2)
           SPTFORCS(IL,IK)      = (1.-ALPH_T)* SPTFORCS(IL,IK)      - SPT_AMP(IL,IK)      * ZRANDNOSS2(        1,KMAX-IK+2)
          elseif (IL.gt.(LMAX/2+1)) then 
           SPSTREAMFORCC(IL,IK) = (1.-ALPH_PSI)* SPSTREAMFORCC(IL,IK) + SPSTREAM_AMP(IL,IK) * ZRANDNOSC1(LMAX-IL+2,KMAX-IK+2)
           SPSTREAMFORCS(IL,IK) = (1.-ALPH_PSI)* SPSTREAMFORCS(IL,IK) - SPSTREAM_AMP(IL,IK) * ZRANDNOSS1(LMAX-IL+2,KMAX-IK+2)
           SPTFORCC(IL,IK)      = (1.-ALPH_T)* SPTFORCC(IL,IK)      + SPT_AMP(IL,IK)      * ZRANDNOSC2(LMAX-IL+2,KMAX-IK+2)
           SPTFORCS(IL,IK)      = (1.-ALPH_T)* SPTFORCS(IL,IK)      - SPT_AMP(IL,IK)      * ZRANDNOSS2(LMAX-IL+2,KMAX-IK+2)
          endif
        ENDDO
      endif
      ENDDO


     END subroutine UPDATE_STOCH



      SUBROUTINE CALCULATE_STOCH_TEN(                                  &
                       ru_tendf,rv_tendf,t_tendf,                   &
                       GPUFORC,GPVFORC,GPTFORC,                     &
                       ru_real,rv_real,rt_real,                     &
                       mu,mub,                                      & 
                       ids, ide, jds, jde, kds, kde,                &
                       ims, ime, jms, jme, kms, kme,                &
                       its, ite, jts, jte, kts, kte,                &
                       dt)

       IMPLICIT NONE
       INTEGER , INTENT(IN)        ::  ids, ide, jds, jde, kds, kde,   &
                                       ims, ime, jms, jme, kms, kme,   &
                                       its, ite, jts, jte, kts, kte

       REAL , DIMENSION(ims:ime , kms:kme, jms:jme),INTENT(INOUT) ::   &
                                        ru_tendf, rv_tendf, t_tendf,   & 
                                        GPUFORC,GPVFORC,GPTFORC

       REAL , DIMENSION(ims:ime , kms:kme, jms:jme),INTENT(IN) ::   &
                                        ru_real,rv_real,rt_real


       REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: mu,mub

       INTEGER :: I,J,K
       REAL    :: dt

       DO j = jts,MIN(jde-1,jte)
         DO k = kts,kte-1
           DO i = its,ite 
             GPUFORC(i,k,j)= ru_real(i,k,j)
           ENDDO
         ENDDO
       ENDDO

       DO j = jts,jte
         DO k = kts,kte-1
           DO i = its,MIN(ide-1,ite)
             GPVFORC(i,k,j)= rv_real(i,k,j)
           ENDDO
         ENDDO
       ENDDO
 
       DO j = jts,MIN(jde-1,jte)
         DO k = kts,kte-1
           DO i = its,MIN(ide-1,ite)
             GPTFORC(i,k,j)= rt_real(i,k,j)
           ENDDO
         ENDDO
       ENDDO

       END SUBROUTINE CALCULATE_STOCH_TEN


      SUBROUTINE UPDATE_STOCH_TEN(ru_tendf,rv_tendf,t_tendf, &
                       GPUFORC,GPVFORC,GPTFORC,                    &
                       mu,mub,                                      & 
                       ids, ide, jds, jde, kds, kde,                &
                       ims, ime, jms, jme, kms, kme,                &
                       its, ite, jts, jte, kts, kte,                &
                       dt                                  )

       IMPLICIT NONE
       INTEGER , INTENT(IN)        ::  ids, ide, jds, jde, kds, kde,   &
                                       ims, ime, jms, jme, kms, kme,   &
                                       its, ite, jts, jte, kts, kte

       REAL , DIMENSION(ims:ime , kms:kme, jms:jme),INTENT(INOUT) :: &
                                       ru_tendf, rv_tendf, t_tendf

       
       REAL , DIMENSION(ims:ime , kms:kme, jms:jme)           :: &
                                       GPUFORC,GPVFORC,GPTFORC 

       REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: mu,mub

       INTEGER :: I,J,K
       REAL  :: dt,xm

       DO j = jts,MIN(jde-1,jte)
         DO k = kts,kte-1
           DO i = its,ite 
             ru_tendf(i,k,j) = ru_tendf(i,k,j) +  GPUFORC(i,k,j)  * (mu(i,j)+mub(i,j)) 
           ENDDO
         ENDDO
       ENDDO

       DO j = jts,jte
         DO i = its,MIN(ide-1,ite)
           DO k = kts,kte-1
             rv_tendf(i,k,j) = rv_tendf(i,k,j) +  GPVFORC(i,k,j) *  (mu(i,j)+mub(i,j)) 
           ENDDO
         ENDDO
       ENDDO

       DO j = jts,MIN(jde-1,jte)
         DO k = kts,kte-1
           DO i = its,MIN(ide-1,ite)
             t_tendf(i,k,j) = t_tendf(i,k,j) + GPTFORC(i,k,j) * (mu(i,j)+mub(i,j))
           ENDDO
         ENDDO
       ENDDO

       END SUBROUTINE UPDATE_STOCH_TEN



      subroutine perturb_physics_tend(gridpointvariance, & 
                       sppt_thresh_fact,rstoch,                 & 
                       ru_tendf,rv_tendf,t_tendf,moist_tend,            &
                       ids, ide, jds, jde, kds, kde,                    &
                       ims, ime, jms, jme, kms, kme,                    &
                       its, ite, jts, jte, kts, kte                )










       IMPLICIT NONE
       INTEGER , INTENT(IN)        ::  ids, ide, jds, jde, kds, kde,   &
                                       ims, ime, jms, jme, kms, kme,   &
                                       its, ite, jts, jte, kts, kte

       REAL , DIMENSION(ims:ime , kms:kme, jms:jme),INTENT(INOUT) ::   &
                                        ru_tendf, rv_tendf, t_tendf,moist_tend,   & 
                                        rstoch
       REAL :: gridpointvariance ,thresh,sppt_thresh_fact

       INTEGER :: I,J,K



       thresh=sppt_thresh_fact*sqrt(gridpointvariance)
       DO j = jts,jte
         DO k = kts,kte-1
           DO i = its,ite 
            if (rstoch(i,k,j).lt.-thresh) then
                rstoch(i,k,j)=-thresh
            endif
            if (rstoch(i,k,j).gt.thresh) then
                rstoch(i,k,j)=thresh
            endif
           ENDDO
         ENDDO
       ENDDO


       DO j = jts,MIN(jde-1,jte)
         DO k = kts,kte-1
           DO i = its,ite 
              ru_tendf(i,k,j)      = ru_tendf(i,k,j)*(1.0 + rstoch(i,k,j))
           ENDDO
         ENDDO
       ENDDO

       DO j = jts,jte
         DO k = kts,kte-1
            DO i = its,MIN(ide-1,ite)
              rv_tendf(i,k,j)      = rv_tendf(i,k,j)*(1.0 + rstoch(i,k,j))
           ENDDO
         ENDDO
       ENDDO

       DO j = jts,MIN(jde-1,jte)
         DO k = kts,kte-1
           DO i = its,MIN(ide-1,ite)
              moist_tend(i,k,j)    = moist_tend(i,k,j)*(1.0 + rstoch(i,k,j))
              t_tendf   (i,k,j)    =    t_tendf(i,k,j)*(1.0 + rstoch(i,k,j))
           ENDDO
         ENDDO
       ENDDO

      end subroutine perturb_physics_tend




       subroutine SP2GP_prep(                                               & 
                       SPSTREAMFORCS,SPSTREAMFORCC,SPTFORCS,SPTFORCC,  &             
                       VERTSTRUCC,VERTSTRUCS,                          &
                       VERTAMPT,VERTAMPUV,                       &
                       RU_REAL,RV_REAL,RT_REAL,                        & 
                       RU_IMAG,RV_IMAG,RT_IMAG,                        & 
                       dx,dy,stoch_vertstruc_opt,                      & 
                       ids, ide, jds, jde, kds, kde,                   & 
                       ims, ime, jms, jme, kms, kme,                   & 
                       its, ite, jts, jte, kts, kte                    ) 

      IMPLICIT NONE 
      INTEGER , INTENT(IN)     ::  ids, ide, jds, jde, kds, kde,   &
                                   ims, ime, jms, jme, kms, kme,   &
                                   its, ite, jts, jte, kts, kte
      REAL, DIMENSION    (ims:ime , jms:jme)          :: SPSTREAMFORCS,SPSTREAMFORCC,SPTFORCS,SPTFORCC
      REAL, DIMENSION    (ims:ime , kms:kme, jms:jme) :: RU_REAL,RV_REAL,RT_REAL,RU_IMAG,RV_IMAG,RT_IMAG, & 
                                                         VERTSTRUCC,VERTSTRUCS
      REAL, DIMENSION (kms:kme          )       :: VERTAMPT,VERTAMPUV
      INTEGER :: IK,IL,ILEV,NLAT,NLON,stoch_vertstruc_opt
      REAL    :: dx,dy,RY,RX

      NLAT=(jde-jds)+1 
      NLON=(ide-ids)+1 
      RY=  NLAT*DY
      RX=  NLON*DX


      DO ILEV=kts,kte

      if (stoch_vertstruc_opt==0) then
        DO IL=its,ite
        DO IK=jts,jte
          rt_real(IL,ILEV,IK)  = SPTFORCC(IL,IK)
          rt_imag(IL,ILEV,IK)  = SPTFORCS(IL,IK)
          ru_real(IL,ILEV,IK)  = 2*RPI/RY*  wavenumber_k(IK) * SPSTREAMFORCS(IL,IK)
          ru_imag(IL,ILEV,IK)  =-2*RPI/RY*  wavenumber_k(IK) * SPSTREAMFORCC(IL,IK)
          rv_real(IL,ILEV,IK)  =-2*RPI/RX*  wavenumber_l(IL) * SPSTREAMFORCS(IL,IK)
          rv_imag(IL,ILEV,IK)  = 2*RPI/RX*  wavenumber_l(IL) * SPSTREAMFORCC(IL,IK)
        ENDDO
        ENDDO

       elseif (stoch_vertstruc_opt==1) then
 
        DO IL=its,ite
        DO IK=jts,jte
          rt_real(IL,ILEV,IK)  = SPTFORCC(IL,IK)*VERTSTRUCC(IL,ILEV,IK)      - SPTFORCS(IL,IK)*VERTSTRUCS(IL,ILEV,IK)
          rt_imag(IL,ILEV,IK)  = SPTFORCC(IL,IK)*VERTSTRUCS(IL,ILEV,IK)      + SPTFORCS(IL,IK)*VERTSTRUCC(IL,ILEV,IK)
          ru_real(IL,ILEV,IK)  = 2*RPI/RY*  wavenumber_k(IK) *&
                            (+SPSTREAMFORCC(IL,IK)*VERTSTRUCS(IL,ILEV,IK) + SPSTREAMFORCS(IL,IK)*VERTSTRUCC(IL,ILEV,IK))
          ru_imag(IL,ILEV,IK)  = 2*RPI/RY*  wavenumber_k(IK) *&
                            (-SPSTREAMFORCC(IL,IK)*VERTSTRUCC(IL,ILEV,IK) + SPSTREAMFORCS(IL,IK)*VERTSTRUCS(IL,ILEV,IK))
          rv_real(IL,ILEV,IK)  = 2*RPI/RX* wavenumber_l(IL) *&
                             (-SPSTREAMFORCC(IL,IK)*VERTSTRUCS(IL,ILEV,IK) - SPSTREAMFORCS(IL,IK)*VERTSTRUCC(IL,ILEV,IK))
          rv_imag(IL,ILEV,IK)  = 2*RPI/RX* wavenumber_l(IL) *&
                             (+SPSTREAMFORCC(IL,IK)*VERTSTRUCC(IL,ILEV,IK) - SPSTREAMFORCS(IL,IK)*VERTSTRUCS(IL,ILEV,IK))
      
        ENDDO
        ENDDO


       elseif (stoch_vertstruc_opt==3) then

        DO IL=its,ite
        DO IK=jts,jte
          rt_real(IL,ILEV,IK)  = SPTFORCC(IL,IK)*VERTSTRUCC(IL,ILEV,IK)      - SPTFORCS(IL,IK)*VERTSTRUCS(IL,ILEV,IK)
          rt_real(IL,ILEV,IK)  = rt_real(IL,ILEV,IK)  * VERTAMPT(ILEV)
          rt_imag(IL,ILEV,IK)  = SPTFORCC(IL,IK)*VERTSTRUCS(IL,ILEV,IK)      + SPTFORCS(IL,IK)*VERTSTRUCC(IL,ILEV,IK)
          rt_imag(IL,ILEV,IK)  = rt_imag(IL,ILEV,IK)  * VERTAMPT(ILEV)
          ru_real(IL,ILEV,IK)  = 2*RPI/RY*  wavenumber_k(IK) *&
                            (+SPSTREAMFORCC(IL,IK)*VERTSTRUCS(IL,ILEV,IK) + SPSTREAMFORCS(IL,IK)*VERTSTRUCC(IL,ILEV,IK))
          ru_real(IL,ILEV,IK)  = ru_real(IL,ILEV,IK)  * VERTAMPUV(ILEV)
          ru_imag(IL,ILEV,IK)  = 2*RPI/RY*  wavenumber_k(IK) *&
                            (-SPSTREAMFORCC(IL,IK)*VERTSTRUCC(IL,ILEV,IK) + SPSTREAMFORCS(IL,IK)*VERTSTRUCS(IL,ILEV,IK))
          ru_imag(IL,ILEV,IK)  = ru_imag(IL,ILEV,IK)  * VERTAMPUV(ILEV)
          rv_real(IL,ILEV,IK)  = 2*RPI/RX* wavenumber_l(IL) *&
                             (-SPSTREAMFORCC(IL,IK)*VERTSTRUCS(IL,ILEV,IK) - SPSTREAMFORCS(IL,IK)*VERTSTRUCC(IL,ILEV,IK))
          rv_real(IL,ILEV,IK)  = rv_real(IL,ILEV,IK)  * VERTAMPUV(ILEV)
          rv_imag(IL,ILEV,IK)  = 2*RPI/RX* wavenumber_l(IL) *&
                             (+SPSTREAMFORCC(IL,IK)*VERTSTRUCC(IL,ILEV,IK) - SPSTREAMFORCS(IL,IK)*VERTSTRUCS(IL,ILEV,IK))
          rv_imag(IL,ILEV,IK)  = rv_imag(IL,ILEV,IK)  * VERTAMPUV(ILEV)
        ENDDO
        ENDDO

      endif 
      ENDDO 

     
      END subroutine SP2GP_prep




       subroutine do_fftback_along_x(fieldc_U_xxx,fields_U_xxx,     &
                                     fieldc_V_xxx,fields_V_xxx,     &
                                     fieldc_T_xxx,fields_T_xxx,     &
                               ids, ide, jds, jde, kds, kde,     &
                               ims, ime, jms, jme, kms, kme,     &
                               ips, ipe, jps, jpe, kps, kpe,     &
                               imsx,imex,jmsx,jmex,kmsx,kmex,    &
                               ipsx,ipex,jpsx,jpex,kpsx,kpex,    &
                               imsy,imey,jmsy,jmey,kmsy,kmey,    &
                               ipsy,ipey,jpsy,jpey,kpsy,kpey,    &
                               k_start , k_end                   &
                              )
       IMPLICIT NONE
 
       INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             ips, ipe, jps, jpe, kps, kpe,     &
                             imsx,imex,jmsx,jmex,kmsx,kmex,    &
                             ipsx,ipex,jpsx,jpex,kpsx,kpex,    &
                             imsy,imey,jmsy,jmey,kmsy,kmey,    &
                             ipsy,ipey,jpsy,jpey,kpsy,kpey,    & 
                             k_start , k_end                   
  
       REAL, DIMENSION    (imsx:imex, kmsx:kmex, jmsx:jmex) :: fieldc_U_xxx,fields_U_xxx, & 
                                                               fieldc_V_xxx,fields_V_xxx, & 
                                                               fieldc_T_xxx,fields_T_xxx

       COMPLEX, DIMENSION (ipsx:ipex)            :: dummy_complex
       INTEGER                                   :: IER,LENWRK,KMAX,LMAX,I,J,K
       REAL, ALLOCATABLE                         :: WORK(:)

       CHARACTER (LEN=160) :: mess


       KMAX=(jde-jds)+1
       LMAX=(ide-ids)+1

       LENWRK=2*KMAX*LMAX
       ALLOCATE(WORK(LENWRK))
       LENSAV= 4*(KMAX+LMAX)+INT(LOG(REAL(KMAX))) + INT(LOG(REAL(LMAX))) + 8

       DO k=kpsx,kpex 
         DO j = jpsx, jpex
           DO i = ipsx, ipex
             dummy_complex(i)=cmplx(fieldc_U_xxx(i,k,j),fields_U_xxx(i,k,j))
           ENDDO
           CALL cFFT1B (LMAX, 1 ,dummy_complex,LMAX, WSAVE1, LENSAV, WORK, LENWRK, IER)
           if (ier.ne.0) then 
              WRITE(mess,FMT='(A)') 'error in cFFT1B in do_fftback_along_x, field U'
              CALL wrf_debug(0,mess)
           end if
           DO i = ipsx, ipex
             fieldc_U_xxx(i,k,j)=real(dummy_complex(i))
             fields_U_xxx(i,k,j)=imag(dummy_complex(i))
           END DO
         END DO
       END DO

       DO k=kpsx,kpex 
         DO j = jpsx, jpex
           DO i = ipsx, ipex
             dummy_complex(i)=cmplx(fieldc_V_xxx(i,k,j),fields_V_xxx(i,k,j))
           ENDDO
           CALL cFFT1B (LMAX, 1 ,dummy_complex,LMAX, WSAVE1, LENSAV, WORK, LENWRK, IER)
           if (ier.ne.0) then 
              WRITE(mess,FMT='(A)') 'error in cFFT1B in do_fftback_along_x, field V'
              CALL wrf_debug(0,mess)
           end if
           DO i = ipsx,ipex
             fieldc_V_xxx(i,k,j)=real(dummy_complex(i))
             fields_V_xxx(i,k,j)=imag(dummy_complex(i))
           END DO
         END DO
       END DO 

       DO k=kpsx,kpex 
         DO j = jpsx, jpex
           DO i = ipsx, ipex
             dummy_complex(i)=cmplx(fieldc_T_xxx(i,k,j),fields_T_xxx(i,k,j))
           ENDDO
           CALL cFFT1B (LMAX, 1 ,dummy_complex,LMAX, WSAVE1, LENSAV, WORK, LENWRK, IER)
           if (ier.ne.0) then 
              WRITE(mess,FMT='(A)') 'error in cFFT1B in do_fftback_along_x, field T'
              CALL wrf_debug(0,mess)
           end if
           DO i = ipsx, ipex
            fieldc_T_xxx(i,k,j)=real(dummy_complex(i))
            fields_T_xxx(i,k,j)=imag(dummy_complex(i))
           END DO
         END DO
       END DO 

       DEALLOCATE(WORK)
       end subroutine do_fftback_along_x




       subroutine do_fftback_along_y(fieldc_U_yyy,fields_U_yyy,     &
                                     fieldc_V_yyy,fields_V_yyy,     &
                                     fieldc_T_yyy,fields_T_yyy,     &
                               ids, ide, jds, jde, kds, kde,     &
                               ims, ime, jms, jme, kms, kme,     &
                               ips, ipe, jps, jpe, kps, kpe,     &
                               imsx,imex,jmsx,jmex,kmsx,kmex,    &
                               ipsx,ipex,jpsx,jpex,kpsx,kpex,    &
                               imsy,imey,jmsy,jmey,kmsy,kmey,    &
                               ipsy,ipey,jpsy,jpey,kpsy,kpey,    &
                               k_start , k_end                   &
                              )
       IMPLICIT NONE
 
       INTEGER :: IER,LENWRK,KMAX,LMAX,I,J,K
 
       INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             ips, ipe, jps, jpe, kps, kpe,     &
                             imsx,imex,jmsx,jmex,kmsx,kmex,    &
                             ipsx,ipex,jpsx,jpex,kpsx,kpex,    &
                             imsy,imey,jmsy,jmey,kmsy,kmey,    &
                             ipsy,ipey,jpsy,jpey,kpsy,kpey,    & 
                             k_start , k_end                   
  
       REAL, DIMENSION    (imsy:imey, kmsy:kmey, jmsy:jmey) :: fieldc_U_yyy,fields_U_yyy, & 
                                                               fieldc_V_yyy,fields_V_yyy, & 
                                                               fieldc_T_yyy,fields_T_yyy

       COMPLEX, DIMENSION (jpsy:jpey)            :: dummy_complex
       REAL, ALLOCATABLE :: WORK(:)

       CHARACTER (LEN=160) :: mess

       KMAX=(jde-jds)+1
       LMAX=(ide-ids)+1
       LENWRK=2*KMAX*LMAX
       ALLOCATE(WORK(LENWRK))
       LENSAV= 4*(KMAX+LMAX)+INT(LOG(REAL(KMAX))) + INT(LOG(REAL(LMAX))) + 8

        DO k=kpsy,kpey
          DO i = ipsy, ipey
            DO j = jpsy,jpey
            dummy_complex(j)=cmplx(fieldc_U_yyy(i,k,j),fields_U_yyy(i,k,j))
            ENDDO
            CALL cFFT1B (KMAX, 1 ,dummy_complex,KMAX, WSAVE2, LENSAV, WORK, LENWRK, IER)
            if (ier.ne.0) then 
               WRITE(mess,FMT='(A)') 'error in cFFT1B in do_fftback_along_y, field U'
               CALL wrf_debug(0,mess)
            end if
            DO j = jpsy, jpey
            fieldc_U_yyy(i,k,j)=real(dummy_complex(j))
            fields_U_yyy(i,k,j)=imag(dummy_complex(j))
            END DO
          END DO
        END DO 

        DO k=kpsy,kpey 
          DO i = ipsy, ipey
            DO j = jpsy, jpey
            dummy_complex(j)=cmplx(fieldc_V_yyy(i,k,j),fields_V_yyy(i,k,j))
            ENDDO
            CALL cFFT1B (KMAX, 1 ,dummy_complex,KMAX, WSAVE2, LENSAV, WORK, LENWRK, IER)
            if (ier.ne.0) then 
               WRITE(mess,FMT='(A)') 'error in cFFT1B in do_fftback_along_y, field V'
               CALL wrf_debug(0,mess)
            end if
            DO j = jpsy, jpey
            fieldc_V_yyy(i,k,j)=real(dummy_complex(j))
            fields_V_yyy(i,k,j)=imag(dummy_complex(j))
            END DO
          END DO
        END DO 

        DO k=kpsy,kpey 
          DO i = ipsy, ipey
            DO j = jpsy,jpey
            dummy_complex(j)=cmplx(fieldc_T_yyy(i,k,j),fields_T_yyy(i,k,j))
            ENDDO
            CALL cFFT1B (KMAX, 1 ,dummy_complex,KMAX, WSAVE2, LENSAV, WORK, LENWRK, IER)
            if (ier.ne.0) then 
               WRITE(mess,FMT='(A)') 'error in cFFT1B in do_fftback_along_y, field T'
               CALL wrf_debug(0,mess)
            end if
            DO j = jpsy,jpey
            fieldc_T_yyy(i,k,j)=real(dummy_complex(j))
            fields_T_yyy(i,k,j)=imag(dummy_complex(j))
            END DO
          END DO
        END DO 

       DEALLOCATE(WORK)
       end subroutine do_fftback_along_y



      subroutine findindex( wavenumber_k, wavenumber_L,             & 
                       ids, ide, jds, jde, kds, kde,                &
                       ims, ime, jms, jme, kms, kme,                &
                       its, ite, jts, jte, kts, kte                 )

      IMPLICIT NONE
      INTEGER :: IK,IL,KMAX,LMAX
      INTEGER, DIMENSION (jds:jde)::  wavenumber_k
      INTEGER, DIMENSION (ids:ide)::  wavenumber_l
      INTEGER , INTENT(IN)     ::  ids, ide, jds, jde, kds, kde,   &
                                   ims, ime, jms, jme, kms, kme,   &
                                   its, ite, jts, jte, kts, kte
      KMAX=(jde-jds)+1 
      LMAX=(ide-ids)+1 

      
      DO IK=1,KMAX/2+1 
        wavenumber_k(IK)=IK-1 
      ENDDO
      DO IK=KMAX,KMAX/2+2,-1
        wavenumber_k(IK)=IK-KMAX-1 
      ENDDO
      DO IL=1,LMAX/2+1 
        wavenumber_l(IL)=IL-1
      ENDDO
      DO IL=LMAX,LMAX/2+2,-1
        wavenumber_l(IL)=IL-LMAX-1 
      ENDDO

      END subroutine findindex
       

     subroutine gauss_noise(z)
      real :: z                    
      real :: x,y,r, coeff         



      do

      call random_number( x )
      call random_number( y )



      x = 2.0 * x - 1.0
      y = 2.0 * y - 1.0
      r = x * x + y * y

      if ( r > 0.0 .and. r < 1.0 ) exit

      end do



      coeff = sqrt( -2.0 * log(r) / r )
      z = coeff * x

     end subroutine gauss_noise

     SUBROUTINE rand_seed (config_flags, seed1, seed2,nens )
      USE module_configure
      IMPLICIT NONE


      TYPE (grid_config_rec_type)                       :: config_flags


     INTEGER, INTENT(OUT) :: seed1, seed2
     INTEGER, INTENT(IN ) :: nens


      integer            :: date_time(8)
      integer*8          :: yyyy,mmdd,newtime
      integer*8          :: ihr,isc,idiv
      character (len=10) :: real_clock(3), time

      LOGICAL :: is_print = .false.

        newtime = config_flags%start_year * ( config_flags%start_month*100+config_flags%start_day) + config_flags%start_hour

        idiv=2;
        seed1 = newtime+nens*1000000
        seed2 = mod(newtime+nens*1000000,idiv)
        if(is_print) print *,'Rand_seed (newtime/idiv):',newtime,idiv,nens

        end SUBROUTINE rand_seed

        end module module_stoch
