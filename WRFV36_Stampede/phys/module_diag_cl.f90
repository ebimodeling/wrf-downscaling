









MODULE module_diag_cl
CONTAINS

   SUBROUTINE clwrf_output_calc(                                      &
                      ids,ide, jds,jde, kds,kde,                      &
                      ims,ime, jms,jme, kms,kme,                      &
                      ips,ipe, jps,jpe, kps,kpe,                      & 
                      i_start,i_end,j_start,j_end,kts,kte,num_tiles   &
                     ,is_restart                                      & 
                     ,clwrfH,t2,q2,u10,v10, skintemp                  & 
                     ,t2clmin,t2clmax,tt2clmin,tt2clmax               & 
                     ,t2clmean,t2clstd                                & 
                     ,q2clmin,q2clmax,tq2clmin,tq2clmax               & 
                     ,q2clmean,q2clstd                                & 
                     ,u10clmax,v10clmax,spduv10clmax,tspduv10clmax    & 
                     ,u10clmean,v10clmean,spduv10clmean               & 
                     ,u10clstd,v10clstd,spduv10clstd                  & 
                     ,raincclmax,rainncclmax,traincclmax,trainncclmax & 
                     ,raincclmean,rainncclmean,raincclstd,rainncclstd & 
                     ,skintempclmin,skintempclmax                     & 
                     ,tskintempclmin,tskintempclmax                   & 
                     ,skintempclmean,skintempclstd                    & 
                     ,raincv,rainncv                                  &
                     ,dt,xtime,curr_secs2                             &
                                                                      )


  USE module_dm, ONLY: wrf_dm_sum_real, wrf_dm_maxval
  USE module_configure 

   IMPLICIT NONE
















































   INTEGER,      INTENT(IN   )                     ::            &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                      ips,ipe, jps,jpe, kps,kpe, &
                                                        kts,kte, &
                                                      num_tiles

   INTEGER, DIMENSION(num_tiles), INTENT(IN)       :: i_start,   &
                                      i_end,j_start,j_end

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) ::           & 
                                      RAINNCV, RAINCV, SKINTEMP 

   REAL,  INTENT(IN   )                            :: DT, XTIME
   REAL,  INTENT(IN   )                            :: curr_secs2



   INTEGER                                   :: i,j,k,its,ite,jts,jte,ij
   INTEGER                                   :: idp,jdp,irc,jrc,irnc,jrnc,isnh,jsnh
   INTEGER                                   :: prfreq

   REAL                                      :: xtimep
   LOGICAL, EXTERNAL                         :: wrf_dm_on_monitor
   CHARACTER*256                             :: outstring
   CHARACTER*6                               :: grid_str




   CHARACTER (LEN=80)                        :: timestr

   REAL, DIMENSION( ims:ime , jms:jme ),                                          & 
                          INTENT(IN)         :: t2, q2, u10, v10 
   REAL, DIMENSION( ims:ime , jms:jme ),                                          &
                          INTENT(OUT)        :: t2clmin, t2clmax, tt2clmin,       &
                          tt2clmax, t2clmean, t2clstd,                            & 
                          q2clmin, q2clmax, tq2clmin, tq2clmax, q2clmean, q2clstd,&
                          u10clmax, v10clmax, spduv10clmax, tspduv10clmax,        &
                          u10clmean, v10clmean, spduv10clmean,                    &
                          u10clstd, v10clstd, spduv10clstd, skintempclmin,        &
                          skintempclmax, tskintempclmin, tskintempclmax,          &
                          skintempclmean, skintempclstd
   REAL, DIMENSION( ims:ime , jms:jme ),                                          &
                          INTENT(OUT)        :: raincclmax, rainncclmax,          &
                          traincclmax, trainncclmax, raincclmean, rainncclmean,   & 
                          raincclstd, rainncclstd 
   REAL, PARAMETER                           :: minimum0= 1000000.,               &
                          maximum0= -1000000. 
   REAL                                      :: value
   INTEGER, INTENT(IN)                       :: clwrfH
   CHARACTER (LEN=1024)                      :: message
   INTEGER, SAVE                             :: nsteps
   LOGICAL                                   :: is_restart






!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )


  IF (( MOD(NINT(curr_secs2/dt),NINT(clwrfH*60./dt)) == 0) .AND. (.NOT.is_restart)) THEN
    DO ij = 1 , num_tiles
      IF  ( wrf_dm_on_monitor() ) THEN
        WRITE(message, *)'CLWRFdiag - T2; tile: ',ij,' T2clmin:',           & 
          t2clmin(i_start(ij)+(i_end(ij)-i_start(ij))/2,                    &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' T2clmax:',               &
          t2clmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,                    &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' TT2clmin:',              &
          tt2clmin(i_start(ij)+(i_end(ij)-i_start(ij))/2,                   &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' TT2clmax:',              &
          tt2clmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,                   &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' T2clmean:',              &
          t2clmean(i_start(ij)+(i_end(ij)-i_start(ij))/2,                   &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' T2clstd:',               &
          t2clstd(i_start(ij)+(i_end(ij)-i_start(ij))/2,                    &
          j_start(ij)+(j_end(ij)-j_start(ij))/2)
        CALL wrf_debug(75, message)
        WRITE(message, *)'CLWRFdiag - Q2; tile: ',ij,' Q2clmin:',           &
          q2clmin(i_start(ij)+(i_end(ij)-i_start(ij))/2,                    &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' Q2clmax:',               &
          q2clmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,                    &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' TQ2clmin:',              &
          tq2clmin(i_start(ij)+(i_end(ij)-i_start(ij))/2,                   &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' TQ2clmax:',              &
          tq2clmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,                   &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' Q2clmean:',              &
          q2clmean(i_start(ij)+(i_end(ij)-i_start(ij))/2,                   &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' Q2clstd:',               &
          q2clstd(i_start(ij)+(i_end(ij)-i_start(ij))/2,                    &
          j_start(ij)+(j_end(ij)-j_start(ij))/2)
        CALL wrf_debug(75, message)
        WRITE(message, *)'CLWRFdiag - WINDSPEED; tile: ',ij,' U10clmax:',   &
          u10clmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,                   &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' V10clmax:',              &
          v10clmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,                   &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' SPDUV10clmax:',          &
          spduv10clmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,               &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' TSPDUV10clmax:',         &
          tspduv10clmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,              &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' U10clmean:',             &
          u10clmean(i_start(ij)+(i_end(ij)-i_start(ij))/2,                  &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' V10clmean:',             &
          v10clmean(i_start(ij)+(i_end(ij)-i_start(ij))/2,                  &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' SPDUV10clmean:',         &
          spduv10clmean(i_start(ij)+(i_end(ij)-i_start(ij))/2,              &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' U10clstd:',              &
          u10clstd(i_start(ij)+(i_end(ij)-i_start(ij))/2,                   &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' V10clstd:',              &
          v10clstd(i_start(ij)+(i_end(ij)-i_start(ij))/2,                   &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' SPDUV10clstd:',          &
          spduv10clstd(i_start(ij)+(i_end(ij)-i_start(ij))/2,               &
          j_start(ij)+(j_end(ij)-j_start(ij))/2)
        CALL wrf_debug(75, message)
        WRITE(message, *)'CLWRFdiag - RAIN; tile: ',ij,' RAINCclmax:',      &
          raincclmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,                 &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' RAINNCclmax:',           &
          rainncclmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,                &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' TRAINCclmax:',           &
          traincclmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,                &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' TRAINNCclmax:',          &
          trainncclmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,               &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' RAINCclmean:',           &
          raincclmean(i_start(ij)+(i_end(ij)-i_start(ij))/2,                &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' RAINNCclmean:',          &
          rainncclmean(i_start(ij)+(i_end(ij)-i_start(ij))/2,               &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' RAINCclstd:',            &
          raincclstd(i_start(ij)+(i_end(ij)-i_start(ij))/2,                 &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' RAINNCclstd:',           &
          rainncclstd(i_start(ij)+(i_end(ij)-i_start(ij))/2,                &
          j_start(ij)+(j_end(ij)-j_start(ij))/2)
        CALL wrf_debug(75, message)
        WRITE(message,*)'CLWRFdiag - SKINTEMP; tile: ',ij,' SKINTEMPclmin:',&
          skintempclmin(i_start(ij)+(i_end(ij)-i_start(ij))/2,              &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' SKINTEMPclmax:',         &
          skintempclmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,              &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' TSKINTEMPclmin:',        &
          tskintempclmin(i_start(ij)+(i_end(ij)-i_start(ij))/2,             &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' TSKINTEMPclmax:',        &
          tskintempclmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,             &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' SKINTEMPclmean:',        &
          skintempclmean(i_start(ij)+(i_end(ij)-i_start(ij))/2,             &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' SKINTEMPclstd:',         &
          skintempclstd(i_start(ij)+(i_end(ij)-i_start(ij))/2,              &
          j_start(ij)+(j_end(ij)-j_start(ij))/2)
        CALL wrf_debug(75, message)
      ENDIF
      DO j = j_start(ij), j_end(ij)
        DO i = i_start(ij), i_end(ij)
          t2clmin(i,j)=t2(i,j)
          t2clmax(i,j)=t2(i,j)
          t2clmean(i,j)=t2(i,j)
          t2clstd(i,j)=t2(i,j)*t2(i,j)
          q2clmin(i,j)=q2(i,j)
          q2clmax(i,j)=q2(i,j)
          q2clmean(i,j)=q2(i,j)
          q2clstd(i,j)=q2(i,j)*q2(i,j)
          spduv10clmax(i,j)=sqrt(u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j))
          u10clmean(i,j)=u10(i,j)
          v10clmean(i,j)=v10(i,j)
          spduv10clmean(i,j)=sqrt(u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j))
          u10clstd(i,j)=u10(i,j)*u10(i,j)
          v10clstd(i,j)=v10(i,j)*v10(i,j)
          spduv10clstd(i,j)=u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j)
          raincclmax(i,j)=raincv(i,j)/dt
          rainncclmax(i,j)=rainncv(i,j)/dt
          raincclmean(i,j)=raincv(i,j)/dt
          rainncclmean(i,j)=rainncv(i,j)/dt
          raincclstd(i,j)=(raincv(i,j)/dt)*(raincv(i,j)/dt)
          rainncclstd(i,j)=(rainncv(i,j)/dt)*(rainncv(i,j)/dt)
          skintempclmin(i,j)=skintemp(i,j)
          skintempclmax(i,j)=skintemp(i,j)
          skintempclmean(i,j)=skintemp(i,j)
          skintempclstd(i,j)=skintemp(i,j)*skintemp(i,j)
          nsteps=0
        ENDDO
      ENDDO
  ENDDO

  ELSE
    xtimep = xtime + dt/60.   
    nsteps=nsteps+1






          CALL varstatistics(t2,xtimep,ime-ims+1,jme-jms+1,t2clmin,t2clmax,   &
            tt2clmin,tt2clmax,t2clmean,t2clstd)

          CALL varstatistics(q2,xtimep,ime-ims+1,jme-jms+1,q2clmin,q2clmax,   &
            tq2clmin,tq2clmax,q2clmean,q2clstd)

          CALL varstatisticsWIND(u10,v10,xtimep,ime-ims+1,jme-jms+1,u10clmax, &
            v10clmax,spduv10clmax,tspduv10clmax,u10clmean,v10clmean,         &
            spduv10clmean,u10clstd,v10clstd,spduv10clstd)

          CALL varstatisticsMAX(raincv/dt,xtimep,ime-ims+1,jme-jms+1,         &
            raincclmax,traincclmax,raincclmean,raincclstd) 
          CALL varstatisticsMAX(rainncv/dt,xtimep,ime-ims+1,jme-jms+1,        &
            rainncclmax,trainncclmax,rainncclmean,rainncclstd)

          CALL varstatistics(skintemp,xtimep,ime-ims+1,jme-jms+1,skintempclmin,&
            skintempclmax, tskintempclmin,tskintempclmax,skintempclmean,      &
            skintempclstd)



           IF (MOD(NINT((curr_secs2+dt)/dt),NINT(clwrfH*60./dt)) == 0) THEN
             IF  ( wrf_dm_on_monitor() ) PRINT *,'nsteps=',nsteps,' xtime:',  &
               xtime,' clwrfH:',clwrfH 
               t2clmean=t2clmean/nsteps
               t2clstd=SQRT(t2clstd/nsteps-t2clmean**2.)
               q2clmean=q2clmean/nsteps
               q2clstd=SQRT(q2clstd/nsteps-q2clmean**2.)
               u10clmean=u10clmean/nsteps
               v10clmean=v10clmean/nsteps
               spduv10clmean=spduv10clmean/nsteps
               u10clstd=SQRT(u10clstd/nsteps-u10clmean**2.)
               v10clstd=SQRT(v10clstd/nsteps-v10clmean**2.)
               spduv10clstd=SQRT(spduv10clstd/nsteps-                        &
                 spduv10clmean**2)
               raincclmean=raincclmean/nsteps
               rainncclmean=rainncclmean/nsteps
               raincclstd=SQRT(raincclstd/nsteps-raincclmean**2.)
               rainncclstd=SQRT(rainncclstd/nsteps-rainncclmean**2.)
               skintempclmean=skintempclmean/nsteps
              skintempclstd=SQRT(skintempclstd/nsteps-skintempclmean**2.)
            END IF


  ENDIF
!  !$OMP END PARALLEL DO

   END SUBROUTINE clwrf_output_calc


SUBROUTINE varstatisticsWIND(varu, varv, tt, dx, dy, varumax, varvmax,       &
  varuvmax, tvaruvmax, varumean, varvmean, varuvmean, varustd, varvstd,     & 
  varuvstd) 


IMPLICIT NONE

INTEGER                                                        :: i, j
INTEGER, INTENT(IN)                                            :: dx, dy
REAL, DIMENSION(dx,dy), INTENT(IN)                             :: varu, varv
REAL, INTENT(IN)                                               :: tt
REAL, DIMENSION(dx,dy), INTENT(INOUT)                          :: varumax,   &
  varvmax, varuvmax, tvaruvmax, varumean, varvmean, varuvmean, varustd,      & 
  varvstd, varuvstd
REAL                                                           :: varuv

DO i=1,dx
  DO j=1,dy
    varuv=sqrt(varu(i,j)*varu(i,j)+varv(i,j)*varv(i,j))
      IF (varuv > varuvmax(i,j)) THEN
        varumax(i,j)=varu(i,j)
        varvmax(i,j)=varv(i,j)
        varuvmax(i,j)=varuv
        tvaruvmax(i,j)=tt
      END IF
    varuvmean(i,j)=varuvmean(i,j)+varuv
    varuvstd(i,j)=varuvstd(i,j)+varuv**2
  END DO
END DO
varumean=varumean+varu
varvmean=varvmean+varv
varustd=varustd+varu**2
varvstd=varvstd+varv**2

END SUBROUTINE varstatisticsWIND

SUBROUTINE varstatisticsMAX(var, tt, dx, dy, varmax, tvarmax, varmean,       &
   varstd)


IMPLICIT NONE

INTEGER                                                        :: i,j
INTEGER, INTENT(IN)                                            :: dx, dy
REAL, DIMENSION(dx,dy), INTENT(IN)                             :: var
REAL, INTENT(IN)                                               :: tt
REAL, DIMENSION(dx,dy), INTENT(INOUT)                          :: varmax,    &
  tvarmax, varmean, varstd

DO i=1,dx
  DO j=1,dy
    IF (var(i,j) > varmax(i,j)) THEN
      varmax(i,j)=var(i,j)
      tvarmax(i,j)=tt
    END IF
  END DO
END DO
varmean=varmean+var
varstd=varstd+var**2

END SUBROUTINE varstatisticsMAX 

SUBROUTINE varstatistics(var, tt, dx, dy, varmin, varmax, tvarmin, tvarmax,  & 
  varmean, varstd) 


IMPLICIT NONE

INTEGER                                                        :: i,j
INTEGER, INTENT(IN)                                            :: dx, dy
REAL, DIMENSION(dx,dy), INTENT(IN)                             :: var
REAL, INTENT(IN)                                               :: tt
REAL, DIMENSION(dx,dy), INTENT(INOUT)                          :: varmin,    &
  varmax, tvarmin, tvarmax, varmean, varstd

DO i=1,dx
  DO j=1,dy
    IF (var(i,j) < varmin(i,j)) THEN
      varmin(i,j)=var(i,j)
      tvarmin(i,j)=tt
    END IF
    IF (var(i,j) > varmax(i,j)) THEN
      varmax(i,j)=var(i,j)
      tvarmax(i,j)=tt
    END IF
  END DO
END DO
varmean=varmean+var
varstd=varstd+var**2

END SUBROUTINE varstatistics

END MODULE module_diag_cl
