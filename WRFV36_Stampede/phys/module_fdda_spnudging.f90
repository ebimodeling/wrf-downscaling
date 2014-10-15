



MODULE module_fdda_spnudging


  USE module_dm , ONLY : ntasks_x, ntasks_y, local_communicator_x, local_communicator_y, data_order_xzy

  USE module_wrf_error , ONLY : wrf_err_message

CONTAINS



   SUBROUTINE spectral_nudging(grid,itimestep,dt,xtime,id,analysis_interval, end_fdda_hour, &
               if_no_pbl_nudging_uv, if_no_pbl_nudging_t, if_no_pbl_nudging_ph,&
               if_zfac_uv, k_zfac_uv, dk_zfac_uv, & 
               if_zfac_t, k_zfac_t, dk_zfac_t, &
               if_zfac_ph, k_zfac_ph, dk_zfac_ph, &
               guv, gt, gph, if_ramping, dtramp_min, xwavenum, ywavenum, &
               u3d,v3d,th3d,ph3d,                 &
               u_ndg_old,v_ndg_old,t_ndg_old,ph_ndg_old,       &
               u_ndg_new,v_ndg_new,t_ndg_new,ph_ndg_new,       &
               RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,RPHNDGDTEN,&
               pblh, ht, z, z_at_w,                             &
               ids,ide, jds,jde, kds,kde,                           &
               ims,ime, jms,jme, kms,kme,                           &
               i_start,i_end, j_start,j_end, kts,kte, num_tiles, &
               ips,ipe,jps,jpe,kps,kpe,                   &
               imsx,imex,jmsx,jmex,kmsx,kmex,                    &
               ipsx,ipex,jpsx,jpex,kpsx,kpex,                   &
               imsy,imey,jmsy,jmey,kmsy,kmey,                    &
               ipsy,ipey,jpsy,jpey,kpsy,kpey                   )



   USE module_state_description
   USE module_domain, ONLY : domain


   implicit none

































   TYPE(domain) , TARGET          :: grid

   INTEGER,  INTENT(IN)   ::      itimestep, analysis_interval, end_fdda_hour

   INTEGER,  INTENT(IN)   ::      if_no_pbl_nudging_uv, if_no_pbl_nudging_t, &
                                  if_no_pbl_nudging_ph
   INTEGER,  INTENT(IN)   ::      if_zfac_uv, if_zfac_t, if_zfac_ph
   INTEGER,  INTENT(IN)   ::      k_zfac_uv,  k_zfac_t, k_zfac_ph
   INTEGER,  INTENT(IN)   ::      dk_zfac_uv,  dk_zfac_t, dk_zfac_ph
   INTEGER,  INTENT(IN)   ::      if_ramping
   INTEGER,  INTENT(IN)   ::      xwavenum,ywavenum

   INTEGER , INTENT(IN)   ::      id
   REAL,     INTENT(IN)   ::      DT, xtime, dtramp_min

   INTEGER,  INTENT(IN)   ::      ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  kts,kte, num_tiles,        &
                                  ips,ipe,jps,jpe,kps,kpe,   &
                                  imsx,imex,jmsx,jmex,kmsx,kmex,   &
                                  ipsx,ipex,jpsx,jpex,kpsx,kpex,   &
                                  imsy,imey,jmsy,jmey,kmsy,kmey,   &
                                  ipsy,ipey,jpsy,jpey,kpsy,kpey 

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                   &
  &                                    i_start,i_end,j_start,j_end
 
   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(IN)   ::                  ph3d, &
                                              th3d, &
                                                 z, &
                                            z_at_w

   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(INOUT)   ::           rundgdten, &
                                          rvndgdten, &
                                         rthndgdten, &
                                         rphndgdten


   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(INOUT)   ::           u_ndg_old, &
                                          v_ndg_old, &
                                          t_ndg_old, &
                                          ph_ndg_old, &
                                          u_ndg_new, &
                                          v_ndg_new, &
                                          t_ndg_new, &
                                          ph_ndg_new

   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(IN)   ::                    u3d, &
                                                v3d

   REAL,  DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: pblh, &
                                                         ht

   REAL, INTENT(IN)    :: guv, gt ,gph

   INTEGER             :: its,ite, jts,jte,ij
   INTEGER             :: i, j, k, itsu, jtsv, itf, jtf, ktf, i0, k0, j0
   REAL                :: xtime_old, xtime_new, coef, val_analysis
   INTEGER             :: kpbl, dbg_level

   REAL                :: zpbl, zagl, zagl_bot, zagl_top, tfac, actual_end_fdda_min
   REAL, DIMENSION( ips:ipe, kps:kpe, jps:jpe, 4 ) :: wpbl  
   REAL, DIMENSION( kps:kpe, 4 )                   :: wzfac 

   LOGICAL , EXTERNAL  :: wrf_dm_on_monitor

   CHARACTER (LEN=256) :: wrf_err_message



      DO ij = 1 , num_tiles
          its=i_start(ij)
          ite=i_end(ij)
          jts=j_start(ij)
          jte=j_end(ij)
      ENDDO

   wpbl(:,:,:,:) = 1.0
   wzfac(:,:) = 1.0

 !$OMP PARALLEL DO   &
 !$OMP PRIVATE ( ij, i,j,k ) ! only for part of the calculation.
 DO ij = 1 , num_tiles
     DO j = jts, jte
     DO k = kts, kte
     DO i = its, ite
       RUNDGDTEN(i,k,j) = 0.0
       RVNDGDTEN(i,k,j) = 0.0
       RTHNDGDTEN(i,k,j) = 0.0
       RPHNDGDTEN(i,k,j) = 0.0
     ENDDO
     ENDDO
     ENDDO
 ENDDO
 !$OMP END PARALLEL DO

 actual_end_fdda_min = end_fdda_hour*60.0
 IF( if_ramping == 1 .AND. dtramp_min > 0.0 ) &
       actual_end_fdda_min = end_fdda_hour*60.0 + ABS(dtramp_min)
 IF( xtime > actual_end_fdda_min ) RETURN

 !$OMP PARALLEL DO   &
 !$OMP PRIVATE ( ij, i,j,k ) ! only for part of the calculation. 
 
 
 DO ij = 1 , num_tiles





   xtime_old = FLOOR(xtime/analysis_interval) * analysis_interval * 1.0
   xtime_new = xtime_old + analysis_interval
   coef = (xtime-xtime_old)/(xtime_new-xtime_old)

   IF ( wrf_dm_on_monitor()) THEN

     CALL get_wrf_debug_level( dbg_level )

     IF( xtime-xtime_old < 0.5*dt/60.0 ) THEN

       IF( xtime < end_fdda_hour*60.0 ) THEN
         WRITE(wrf_err_message,FMT='(a,i2.2,a,f15.3,a)') &
          'D',id,' Spectral nudging read in new data at time = ', xtime, ' min.'
         CALL wrf_message( TRIM(wrf_err_message) )
         WRITE(wrf_err_message,FMT='(a,i2.2,a,2(f15.3,1x),a)') &
          'D',id,' Spectral nudging bracketing times = ', xtime_old, xtime_new, ' min.'
         CALL wrf_message( TRIM(wrf_err_message) )
       ENDIF

       actual_end_fdda_min = end_fdda_hour*60.0
       IF( if_ramping == 1 .AND. dtramp_min > 0.0 ) &
           actual_end_fdda_min = end_fdda_hour*60.0 + ABS(dtramp_min)

       IF( dbg_level .GE. 10 .AND. xtime <= actual_end_fdda_min ) THEN

         i0 = (ite-its)/2+its
         j0 = (jte-jts)/2+jts 

         IF( guv > 0.0 ) THEN
           DO k = kts, kte
             WRITE(wrf_err_message,'(a,i1,a,3i4,a,f10.4,a,f10.4)') &
               '    D0',id,' sample analysis values at i,k,j=', i0, k, j0, &
               ' u_ndg_old=', u_ndg_old(i0,k,j0), ' u_ndg_new=', u_ndg_new(i0,k,j0)
             CALL wrf_message( TRIM(wrf_err_message) )
           ENDDO
           DO k = kts, kte
             WRITE(wrf_err_message,'(a,i1,a,3i4,a,f10.4,a,f10.4)') &
               '    D0',id,' sample analysis values at i,k,j=', i0, k, j0, &
               ' v_ndg_old=', v_ndg_old(i0,k,j0), ' v_ndg_new=', v_ndg_new(i0,k,j0)
             CALL wrf_message( TRIM(wrf_err_message) )
           ENDDO
         ENDIF

         IF( gt > 0.0 ) THEN
           DO k = kts, kte
             WRITE(wrf_err_message,'(a,i1,a,3i4,a,f10.4,a,f10.4)') &
               '    D0',id,' sample analysis values at i,k,j=', i0, k, j0, &
               ' t_ndg_old=', t_ndg_old(i0,k,j0), ' t_ndg_new=', t_ndg_new(i0,k,j0)
             CALL wrf_message( TRIM(wrf_err_message) )
           ENDDO
         ENDIF

         IF( gph > 0.0 ) THEN
           DO k = kts, kte
             WRITE(wrf_err_message,'(a,i1,a,3i4,a,f10.4,a,f10.4)') &
               '    D0',id,' sample analysis values at i,k,j=', i0, k, j0, &
               ' ph_ndg_old=', ph_ndg_old(i0,k,j0), ' ph_ndg_new=', ph_ndg_new(i0,k,j0)
             CALL wrf_message( TRIM(wrf_err_message) )
           ENDDO
         ENDIF

       ENDIF
     ENDIF
   ENDIF

   jtsv=MAX0(jts,jds+1)
   itsu=MAX0(its,ids+1)

   jtf=MIN0(jte,jde-1)
   ktf=MIN0(kte,kde-1)
   itf=MIN0(ite,ide-1)















   IF( if_no_pbl_nudging_uv == 1 ) THEN

     DO j=jts,jtf 
     DO i=itsu,itf

       kpbl = 1
       zpbl = 0.5 * ( pblh(i-1,j) + pblh(i,j) )

       loop_ku: DO k=kts,ktf 
         zagl_bot = 0.5 * ( z_at_w(i-1,k,  j)-ht(i-1,j) + z_at_w(i,k,  j)-ht(i,j) )
         zagl_top = 0.5 * ( z_at_w(i-1,k+1,j)-ht(i-1,j) + z_at_w(i,k+1,j)-ht(i,j) )
         IF( zpbl >= zagl_bot .AND. zpbl < zagl_top ) THEN
           kpbl = k
           EXIT loop_ku
         ENDIF
       ENDDO loop_ku

       DO k=kts,ktf
          wpbl(i, k, j, 1) = max ( min ( float(k-kpbl) / float(dk_zfac_uv) , 1. ) , 0.)
       ENDDO

     ENDDO
     ENDDO

     DO i=its,itf
     DO j=jtsv,jtf

       kpbl = 1
       zpbl = 0.5 * ( pblh(i,j-1) + pblh(i,j) )

       loop_kv: DO k=kts,ktf
         zagl_bot = 0.5 * ( z_at_w(i,k,  j-1)-ht(i,j-1) + z_at_w(i,k,  j)-ht(i,j) )
         zagl_top = 0.5 * ( z_at_w(i,k+1,j-1)-ht(i,j-1) + z_at_w(i,k+1,j)-ht(i,j) )
         IF( zpbl >= zagl_bot .AND. zpbl < zagl_top ) THEN
           kpbl = k
           EXIT loop_kv
         ENDIF
       ENDDO loop_kv

       DO k=kts,ktf
          wpbl(i, k, j, 2) = max ( min ( float(k-kpbl) / float(dk_zfac_uv) , 1. ) , 0.)
       ENDDO

     ENDDO
     ENDDO

   ELSEIF( if_zfac_uv == 1 ) THEN

     DO j=jts,jte
     DO k=kts,ktf
     DO i=its,ite
          wpbl(i, k, j, 1) = max ( min ( float(k-k_zfac_uv) / float(dk_zfac_uv) , 1. ) , 0.)
          wpbl(i, k, j, 2) = wpbl(i, k, j, 1)
     ENDDO
     ENDDO
     ENDDO

   ENDIF


   IF( if_no_pbl_nudging_t == 1 ) THEN
   
     DO j=jts,jtf
     DO i=its,itf

       kpbl = 1
       zpbl = pblh(i,j)
        
       loop_kt: DO k=kts,ktf
         zagl_bot = z_at_w(i,k,  j)-ht(i,j)
         zagl_top = z_at_w(i,k+1,j)-ht(i,j)
         IF( zpbl >= zagl_bot .AND. zpbl < zagl_top ) THEN
           kpbl = k
           EXIT loop_kt
         ENDIF
       ENDDO loop_kt

       DO k=kts,ktf
          wpbl(i, k, j, 3) = max ( min ( float(k-kpbl) / float(dk_zfac_t) , 1. ) , 0.)
       ENDDO 
        
     ENDDO
     ENDDO

   ELSEIF( if_zfac_t == 1 ) THEN

     DO j=jts,jtf
     DO k=kts,ktf
     DO i=its,itf
          wpbl(i, k, j, 3) = max ( min ( float(k-k_zfac_t) / float(dk_zfac_t) , 1. ) , 0.)
     ENDDO
     ENDDO
     ENDDO

   ENDIF


   IF( if_no_pbl_nudging_ph == 1 ) THEN
   
     DO j=jts,jtf
     DO i=its,itf

       kpbl = 1
       zpbl = pblh(i,j)
          
       loop_kph: DO k=kts,kte
         zagl = z_at_w(i,k,  j)-ht(i,j)
         IF( zpbl >= zagl) THEN
           kpbl = k
           EXIT loop_kph
         ENDIF
       ENDDO loop_kph

       DO k=kts,kte
          wpbl(i, k, j, 4) = max ( min ( float(k-kpbl) / float(dk_zfac_ph) , 1. ) , 0.)
       ENDDO 
            
     ENDDO  
     ENDDO
        
   ELSEIF( if_zfac_ph == 1 ) THEN

     DO j=jts,jtf
     DO k=kts,kte
     DO i=its,itf
          wpbl(i, k, j, 4) = max ( min ( float(k-k_zfac_ph) / float(dk_zfac_ph) , 1. ) , 0.)
     ENDDO
     ENDDO
     ENDDO

   ENDIF














   tfac = 1.0

   IF( if_ramping == 1 .AND. ABS(dtramp_min) > 0.0 ) THEN
 
     IF( dtramp_min <= 0.0 ) THEN
       actual_end_fdda_min = end_fdda_hour*60.0
     ELSE
       actual_end_fdda_min = end_fdda_hour*60.0 + dtramp_min
     ENDIF

     IF( xtime < actual_end_fdda_min-ABS(dtramp_min) )THEN 
       tfac = 1.0
     ELSEIF( xtime >= actual_end_fdda_min-ABS(dtramp_min) .AND. xtime <= actual_end_fdda_min )THEN
       tfac = ( actual_end_fdda_min - xtime ) / ABS(dtramp_min)
       IF( dtramp_min > 0.0 ) coef = (xtime-xtime_old+analysis_interval)/analysis_interval
     ELSE                                                     
       tfac = 0.0
     ENDIF

   ENDIF                                                  

   ENDDO
   !$OMP END PARALLEL DO












IF(guv > 0. ) then

 !$OMP PARALLEL DO   &
 !$OMP PRIVATE ( ij, i,j,k )
 DO ij = 1 , num_tiles

   DO j=jts,jte
   DO k=kts,ktf
   DO i=its,ite
     val_analysis = u_ndg_old(i,k,j) *( 1.0 - coef ) + u_ndg_new(i,k,j) * coef
     
     grid%dif_analysis(i,k,j) = val_analysis - u3d(i,k,j)

   ENDDO
   ENDDO
   ENDDO
 
 ENDDO
 !$OMP END PARALLEL DO









  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%dif_analysis, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%dif_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 


     CALL spectral_nudging_filter_3dx( grid%dif_xxx, xwavenum,    &
                                ids, ide, jds, jde, kds, kde,         &
                                imsx, imex, jmsx, jmex, kmsx, kmex,     &
                                ipsx, ipex, jpsx, jpex, kpsx, MIN(kde-1,kpex) ) 







  call trans_x2y ( ntasks_y, local_communicator_y, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%dif_xxx, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x, &
                   grid%dif_yyy, &  
                   grid%sp31y, grid%ep31y, grid%sp32y, grid%ep32y, grid%sp33y, grid%ep33y, &
                   grid%sm31y, grid%em31y, grid%sm32y, grid%em32y, grid%sm33y, grid%em33y ) 


     CALL spectral_nudging_filter_3dy( grid%dif_yyy, ywavenum,    &
                                ids, ide, jds, jde, kds, kde,         &
                                imsy, imey, jmsy, jmey, kmsy, kmey,     &
                                ipsy, ipey, jpsy, jpey, kpsy, MIN(kde-1,kpey) )







  call trans_x2y ( ntasks_y, local_communicator_y, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%dif_xxx, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x, &
                   grid%dif_yyy, &  
                   grid%sp31y, grid%ep31y, grid%sp32y, grid%ep32y, grid%sp33y, grid%ep33y, &
                   grid%sm31y, grid%em31y, grid%sm32y, grid%em32y, grid%sm33y, grid%em33y ) 
  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%dif_analysis, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%dif_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x)






 !$OMP PARALLEL DO   &
 !$OMP PRIVATE ( ij, i,j,k )
 DO ij = 1 , num_tiles

   DO j=jts,jte
   DO k=kts,ktf
   DO i=its,ite
     RUNDGDTEN(i,k,j) = guv * wpbl(i,k,j,1) * wzfac(k,1) * tfac * &
                        grid%dif_analysis(i,k,j)
   ENDDO
   ENDDO
   ENDDO






   DO j=jts,jte
   DO k=kts,ktf
   DO i=its,ite
     val_analysis = v_ndg_old(i,k,j) *( 1.0 - coef ) + v_ndg_new(i,k,j) * coef

     grid%dif_analysis(i,k,j) = val_analysis - v3d(i,k,j)

   ENDDO
   ENDDO
   ENDDO

 ENDDO
 !$OMP END PARALLEL DO










  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%dif_analysis, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%dif_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 

     CALL spectral_nudging_filter_3dx( grid%dif_xxx, xwavenum,     &
                                ids, ide, jds, jde, kds, kde,         &
                                imsx, imex, jmsx, jmex, kmsx, kmex,     &
                                ipsx, ipex, jpsx, jpex, kpsx, MIN(kde-1,kpex) )







  call trans_x2y ( ntasks_y, local_communicator_y, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%dif_xxx, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x, &
                   grid%dif_yyy, &  
                   grid%sp31y, grid%ep31y, grid%sp32y, grid%ep32y, grid%sp33y, grid%ep33y, &
                   grid%sm31y, grid%em31y, grid%sm32y, grid%em32y, grid%sm33y, grid%em33y ) 

     CALL spectral_nudging_filter_3dy( grid%dif_yyy, ywavenum,   &
                                ids, ide, jds, jde, kds, kde-1,         &
                                imsy, imey, jmsy, jmey, kmsy, kmey,     &
                                ipsy, ipey, jpsy, jpey, kpsy, MIN(kde-1,kpey) )







  call trans_x2y ( ntasks_y, local_communicator_y, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%dif_xxx, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x, &
                   grid%dif_yyy, &  
                   grid%sp31y, grid%ep31y, grid%sp32y, grid%ep32y, grid%sp33y, grid%ep33y, &
                   grid%sm31y, grid%em31y, grid%sm32y, grid%em32y, grid%sm33y, grid%em33y ) 
  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%dif_analysis, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%dif_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x)






 !$OMP PARALLEL DO   &
 !$OMP PRIVATE ( ij, i,j,k )
 DO ij = 1 , num_tiles

   DO j=jts,jte
   DO k=kts,ktf
   DO i=its,ite
     RVNDGDTEN(i,k,j) = guv * wpbl(i,k,j,2) * wzfac(k,2) * tfac * &
                        grid%dif_analysis(i,k,j)
   ENDDO
   ENDDO
   ENDDO

 ENDDO
 !$OMP END PARALLEL DO

ENDIF

IF(gt > 0. ) then

 !$OMP PARALLEL DO   &
 !$OMP PRIVATE ( ij, i,j,k )
 DO ij = 1 , num_tiles

   DO j=jts,jte
   DO k=kts,ktf
   DO i=its,ite
     val_analysis = t_ndg_old(i,k,j) *( 1.0 - coef ) + t_ndg_new(i,k,j) * coef

     grid%dif_analysis(i,k,j) = val_analysis - th3d(i,k,j) + 300.

   ENDDO
   ENDDO
   ENDDO

 ENDDO
 !$OMP END PARALLEL DO









  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%dif_analysis, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%dif_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 


     CALL spectral_nudging_filter_3dx( grid%dif_xxx, xwavenum,    &
                                ids, ide, jds, jde, kds, kde,         &
                                imsx, imex, jmsx, jmex, kmsx, kmex,     &
                                ipsx, ipex, jpsx, jpex, kpsx, MIN(kde-1,kpex) )







  call trans_x2y ( ntasks_y, local_communicator_y, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%dif_xxx, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x, &
                   grid%dif_yyy, &  
                   grid%sp31y, grid%ep31y, grid%sp32y, grid%ep32y, grid%sp33y, grid%ep33y, &
                   grid%sm31y, grid%em31y, grid%sm32y, grid%em32y, grid%sm33y, grid%em33y ) 


     CALL spectral_nudging_filter_3dy( grid%dif_yyy, ywavenum,    &
                                ids, ide, jds, jde, kds, kde,         &
                                imsy, imey, jmsy, jmey, kmsy, kmey,     &
                                ipsy, ipey, jpsy, jpey, kpsy, MIN(kde-1,kpey) )







  call trans_x2y ( ntasks_y, local_communicator_y, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%dif_xxx, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x, &
                   grid%dif_yyy, &  
                   grid%sp31y, grid%ep31y, grid%sp32y, grid%ep32y, grid%sp33y, grid%ep33y, &
                   grid%sm31y, grid%em31y, grid%sm32y, grid%em32y, grid%sm33y, grid%em33y ) 
  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%dif_analysis, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%dif_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x)






 !$OMP PARALLEL DO   &
 !$OMP PRIVATE ( ij, i,j,k )
 DO ij = 1 , num_tiles

   DO j=jts,jte
   DO k=kts,ktf
   DO i=its,ite
     RTHNDGDTEN(i,k,j) = gt * wpbl(i,k,j,3) * wzfac(k,3) * tfac * &
                        grid%dif_analysis(i,k,j)
   ENDDO
   ENDDO
   ENDDO

 ENDDO
 !$OMP END PARALLEL DO

ENDIF

IF(gph > 0. ) then

 !$OMP PARALLEL DO   &
 !$OMP PRIVATE ( ij, i,j,k )
 DO ij = 1 , num_tiles

   DO j=jts,jte
   DO k=kts,kte
   DO i=its,ite
     val_analysis = ph_ndg_old(i,k,j) *( 1.0 - coef ) + ph_ndg_new(i,k,j) * coef

     grid%dif_analysis(i,k,j) = val_analysis - ph3d(i,k,j)

   ENDDO
   ENDDO
   ENDDO

 ENDDO
 !$OMP END PARALLEL DO









  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%dif_analysis, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%dif_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 


     CALL spectral_nudging_filter_3dx( grid%dif_xxx, xwavenum,    &
                                ids, ide, jds, jde, kds, kde,         &
                                imsx, imex, jmsx, jmex, kmsx, kmex,     &
                                ipsx, ipex, jpsx, jpex, kpsx, kpex  )







  call trans_x2y ( ntasks_y, local_communicator_y, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%dif_xxx, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x, &
                   grid%dif_yyy, &  
                   grid%sp31y, grid%ep31y, grid%sp32y, grid%ep32y, grid%sp33y, grid%ep33y, &
                   grid%sm31y, grid%em31y, grid%sm32y, grid%em32y, grid%sm33y, grid%em33y ) 


     CALL spectral_nudging_filter_3dy( grid%dif_yyy, ywavenum,    &
                                ids, ide, jds, jde, kds, kde,         &
                                imsy, imey, jmsy, jmey, kmsy, kmey,     &
                                ipsy, ipey, jpsy, jpey, kpsy, kpey  )







  call trans_x2y ( ntasks_y, local_communicator_y, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%dif_xxx, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x, &
                   grid%dif_yyy, &  
                   grid%sp31y, grid%ep31y, grid%sp32y, grid%ep32y, grid%sp33y, grid%ep33y, &
                   grid%sm31y, grid%em31y, grid%sm32y, grid%em32y, grid%sm33y, grid%em33y ) 
  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%dif_analysis, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%dif_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x)






 !$OMP PARALLEL DO   &
 !$OMP PRIVATE ( ij, i,j,k )
 DO ij = 1 , num_tiles

   DO j=jts,jte
   DO k=kts,kte
   DO i=its,ite
     RPHNDGDTEN(i,k,j) = gph * wpbl(i,k,j,4) * wzfac(k,4) * tfac * &
                        grid%dif_analysis(i,k,j)
   ENDDO
   ENDDO
   ENDDO

 ENDDO
 !$OMP END PARALLEL DO

ENDIF


   END SUBROUTINE spectral_nudging



SUBROUTINE spectral_nudging_filter_3dx( f, nwave,            &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            its, ite, jts, jte, kts, kte )

  IMPLICIT NONE

  INTEGER ,       INTENT(IN   ) :: nwave
  INTEGER ,       INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte

  REAL , DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(INOUT) ::  f

  REAL , DIMENSION(1:ide-ids+1,1:kte-kts+1) :: sheet
  INTEGER ::  i, j, j_end, k, nx, ny

  
  
  
  

  
  IF ((its /= ids) .OR. (ite /= ide)) THEN
     WRITE ( wrf_err_message , * ) 'module_spectral_nudging: 3d: (its /= ids) or (ite /= ide)',its,ids,ite,ide
     CALL wrf_error_fatal3("<stdin>",920,&
TRIM( wrf_err_message ) )
  END IF


  nx = ide-ids+1 
  ny = kte-kts+1 
  j_end = MIN(jte, jde-1)
  IF (j_end == jde-1) j_end = jde
  DO j = jts, j_end

        DO k=kts,kte
        DO i=ids,ide-1
           sheet(i-ids+1,k-kts+1) = f(i,k,j)
        END DO
           sheet(ide,k-kts+1) = 0.
        END DO

        CALL spectralnudgingfilterfft2dncar(nx,ny,nwave,sheet)

        DO k=kts,kte
           DO i=ids,ide
              f(i,k,j) = sheet(i-ids+1,k-kts+1)
           END DO
        END DO
  END DO 

END SUBROUTINE spectral_nudging_filter_3dx


SUBROUTINE spectral_nudging_filter_3dy( f, nwave,   &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            its, ite, jts, jte, kts, kte )

  IMPLICIT NONE

  INTEGER ,       INTENT(IN   ) :: nwave
  INTEGER ,       INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte

  REAL , DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(INOUT) ::  f

  REAL , DIMENSION(1:jde-jds+1,1:kte-kts+1) :: sheet
  INTEGER ::  i, j, i_end, k, nx, ny

  
  
  
  

  
  IF ((jts /= jds) .OR. (jte /= jde)) THEN
     WRITE ( wrf_err_message , * ) 'module_spectral_nudging: 3d: (jts /= jds) or (jte /= jde)',jts,ids,ite,ide
     CALL wrf_error_fatal3("<stdin>",975,&
TRIM( wrf_err_message ) )
  END IF


  nx = jde-jds+1
  ny = kte-kts+1 
  i_end = MIN(ite, ide-1)
  IF (i_end == ide-1) i_end = ide
  DO i = its, i_end

        DO k=kts,kte
        DO j=jds,jde
           sheet(j-jds+1,k-kts+1) = f(i,k,j)
        END DO
           sheet(jde,k-kts+1) = 0.
        END DO

        CALL spectralnudgingfilterfft2dncar(nx,ny,nwave,sheet)

        DO k=kts,kte
           DO j=jds,jde
              f(i,k,j) = sheet(j-jds+1,k-kts+1)
           END DO
        END DO
  END DO 

END SUBROUTINE spectral_nudging_filter_3dy



SUBROUTINE spectralnudgingfilterfft2dncar(nx,ny,nwave,fin)
  IMPLICIT NONE
  INTEGER , INTENT(IN) :: nx, ny, nwave
  REAL , DIMENSION(nx,ny), INTENT(INOUT) :: fin

  INTEGER :: i, j
  REAL, dimension(nx,ny) :: fp

  INTEGER :: lensave, ier, nh, n1
  INTEGER :: lot, jump, n, inc, lenr, lensav, lenwrk
  REAL, DIMENSION(nx+15) :: wsave
  REAL, DIMENSION(nx,ny) :: work
  





  n = nx
  lot = ny
  lensav = n+15
  inc = 1
  lenr = nx*ny
  jump = nx
  lenwrk = lenr





  call rfftmi(n,wsave,lensav,ier)
  IF(ier /= 0) THEN
    write(wrf_err_message,*) ' error in rfftmi ',ier
    CALL wrf_message(TRIM(wrf_err_message))
  END IF



  call rfftmf( lot, jump, n, inc, fin, lenr, wsave, lensav, work, lenwrk, ier )
  IF(ier /= 0) THEN
    write(wrf_err_message,*) ' error in rfftmf ',ier
    CALL wrf_message(TRIM(wrf_err_message))
  END IF

  if(MOD(n,2) == 0) then
    nh = n/2 - 1
  else
    nh = (n-1)/2
  end if




  fp = 1.

  DO j=1,ny
     DO i=nwave+1,nh
         fp(2*i-1,j) = 0.
         fp(2*i,j) = 0.
     ENDDO
  ENDDO

  DO j=1,ny
    DO i=1,nx
      fin(i,j) = fp(i,j)*fin(i,j)
    ENDDO
  ENDDO



  call rfftmb( lot, jump, n, inc, fin, lenr, wsave, lensav, work, lenwrk, ier )
  IF(ier /= 0) THEN
    write(wrf_err_message,*) ' error in rfftmb ',ier
    CALL wrf_message(TRIM(wrf_err_message))
  END IF

END SUBROUTINE spectralnudgingfilterfft2dncar



   SUBROUTINE fddaspnudginginit(id,rundgdten,rvndgdten,rthndgdten,rphndgdten, &
               run_hours,  &
               if_no_pbl_nudging_uv, if_no_pbl_nudging_t, if_no_pbl_nudging_ph, &
               if_zfac_uv, k_zfac_uv, dk_zfac_uv, &
               if_zfac_t, k_zfac_t, dk_zfac_t, &
               if_zfac_ph, k_zfac_ph, dk_zfac_ph, &               
               guv, gt, gph, if_ramping, dtramp_min, end_fdda_hour, &
               xwavenum,ywavenum,                          &
                      restart, allowed_to_read,                    &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )

   IMPLICIT NONE


   INTEGER , INTENT(IN)         ::  id
   LOGICAL, INTENT(IN)          ::  restart, allowed_to_read
   INTEGER, INTENT(IN)          ::  ids, ide, jds, jde, kds, kde, &
                                    ims, ime, jms, jme, kms, kme, &
                                    its, ite, jts, jte, kts, kte
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(OUT) :: &
                                                       rundgdten, &
                                                       rvndgdten, &
                                                      rthndgdten, &
                                                      rphndgdten
   INTEGER,  INTENT(IN)   ::      run_hours
   INTEGER,  INTENT(IN)   ::      if_no_pbl_nudging_uv, if_no_pbl_nudging_t, &
                                  if_no_pbl_nudging_ph, end_fdda_hour
   INTEGER,  INTENT(IN)   ::      if_zfac_uv, if_zfac_t, if_zfac_ph
   INTEGER,  INTENT(IN)   ::      k_zfac_uv,  k_zfac_t,  k_zfac_ph
   INTEGER,  INTENT(IN)   ::      dk_zfac_uv,  dk_zfac_t,  dk_zfac_ph
   INTEGER,  INTENT(IN)   ::      if_ramping
   INTEGER,  INTENT(IN)   ::      xwavenum,ywavenum
   REAL,     INTENT(IN)   ::      dtramp_min
   REAL, INTENT(IN)       ::      guv, gt, gph
   REAL                   ::      actual_end_fdda_min

   INTEGER :: i, j, k

   LOGICAL , EXTERNAL     ::      wrf_dm_on_monitor

   CHARACTER (LEN=256) :: wrf_err_message

   IF ( wrf_dm_on_monitor() ) THEN

     IF( guv > 0.0) THEN
       WRITE(wrf_err_message,'(a,i1,a,e12.4,a,i4,a,i4)') &
           'D0',id,' Spectral nudging for wind is turned on and Guv= ', guv,' xwave= ',xwavenum,' ywavenum= ',ywavenum
       CALL wrf_message(TRIM(wrf_err_message))
     ELSE IF( guv < 0.0 ) THEN
       CALL wrf_error_fatal3("<stdin>",1137,&
'In grid FDDA, Guv must be positive.')
     ELSE
       WRITE(wrf_err_message,'(a,i1,a,e12.4)') &
           'D0',id,' Spectral nudging for wind is turned off and Guv= ', guv
       CALL wrf_message(TRIM(wrf_err_message))
     ENDIF

     IF( gt > 0.0) THEN
       WRITE(wrf_err_message,'(a,i1,a,e12.4,a,i4,a,i4)') &
           'D0',id,' Spectral nudging for temperature is turned on and Gt= ', gt,' xwave= ',xwavenum,' ywavenum= ',ywavenum
       CALL wrf_message(TRIM(wrf_err_message))
     ELSE IF( gt < 0.0 ) THEN
       CALL wrf_error_fatal3("<stdin>",1150,&
'In grid FDDA, Gt must be positive.')
     ELSE
       WRITE(wrf_err_message,'(a,i1,a,e12.4)') &
           'D0',id,' Spectral nudging for temperature is turned off and Gt= ', gt
       CALL wrf_message(TRIM(wrf_err_message))
     ENDIF

     IF( gph > 0.0) THEN
       WRITE(wrf_err_message,'(a,i1,a,e12.4,a,i4,a,i4)') &
         'D0',id,' Spectral nudging for geopotential is turned on and Gph= ', gph,' xwave= ',xwavenum,' ywavenum= ',ywavenum
       CALL wrf_message(TRIM(wrf_err_message))
     ELSE IF( gph < 0.0 ) THEN
       CALL wrf_error_fatal3("<stdin>",1163,&
'In grid FDDA, Gph must be positive.')
     ELSE
       WRITE(wrf_err_message,'(a,i1,a,e12.4)') &
         'D0',id,' Spectral nudging for geopotential is turned off and Gph= ', gph
       CALL wrf_message(TRIM(wrf_err_message))
     ENDIF

     IF( guv > 0.0 .AND. if_no_pbl_nudging_uv == 1 ) THEN
        WRITE(wrf_err_message,'(a,i1,a)') &
           'D0',id,' Spectral nudging for wind is turned off within the PBL.'
        CALL wrf_message(TRIM(wrf_err_message))
             IF( dk_zfac_uv < 1 ) CALL wrf_error_fatal3("<stdin>",1175,&
'In spectral nudging, dk_zfac_uv must be greater or equal than 1.')
     ELSEIF( guv > 0.0 .AND. if_zfac_uv == 1 ) THEN
        WRITE(wrf_err_message,'(a,i1,a,i3)') &
           'D0',id,' Spectral nudging for wind is turned off below layer', k_zfac_uv
        CALL wrf_message(TRIM(wrf_err_message))
             IF( dk_zfac_uv < 1 ) CALL wrf_error_fatal3("<stdin>",1181,&
'In spectral nudging, dk_zfac_uv must  be greater or equal than 1.')       
     ENDIF


     IF( gt > 0.0 .AND. if_no_pbl_nudging_t == 1 ) THEN
        WRITE(wrf_err_message,'(a,i1,a)') &
           'D0',id,' Spectral nudging for temperature is turned off within the PBL.'
        CALL wrf_message(TRIM(wrf_err_message))
             IF( dk_zfac_t < 1 ) CALL wrf_error_fatal3("<stdin>",1190,&
'In spectral nudging, dk_zfac_t must be greater or equal than 1.')
     ELSEIF( gt > 0.0 .AND. if_zfac_t == 1 ) THEN
        WRITE(wrf_err_message,'(a,i1,a,i3)') &
           'D0',id,' Spectral nudging for temperature is turned off below layer', k_zfac_t
        CALL wrf_message(TRIM(wrf_err_message))
            IF( dk_zfac_t < 1 ) CALL wrf_error_fatal3("<stdin>",1196,&
'In spectral nudging, dk_zfac_t must be greater or equal than 1.')
     ENDIF


     IF( gph > 0.0 .AND. if_no_pbl_nudging_ph == 1 ) THEN
        WRITE(wrf_err_message,'(a,i1,a)') &
         'D0',id,' Spectral nudging for geopotential is turned off within the PBL.'
        CALL wrf_message(TRIM(wrf_err_message))
            IF( dk_zfac_ph < 1 ) CALL wrf_error_fatal3("<stdin>",1205,&
'In spectral nudging, dk_zfac_ph must be greater or equal than 1.')
     ELSEIF( gph > 0.0 .AND. if_zfac_ph == 1 ) THEN
        WRITE(wrf_err_message,'(a,i1,a,i3)') &
          'D0',id,' Spectral nudging for geopotential is turned off below layer', &
           k_zfac_ph
        CALL wrf_message(TRIM(wrf_err_message))
            IF( dk_zfac_ph < 1 ) CALL wrf_error_fatal3("<stdin>",1212,&
'In spectral nudging, dk_zfac_ph must be greater or equal than 1.')
     ENDIF

     IF( if_ramping == 1 .AND. ABS(dtramp_min) > 0.0 ) THEN
       IF( dtramp_min <= 0.0 ) THEN
         actual_end_fdda_min = end_fdda_hour*60.0
       ELSE
         actual_end_fdda_min = end_fdda_hour*60.0 + ABS(dtramp_min)
       ENDIF

       IF( actual_end_fdda_min <= run_hours*60. ) THEN
          WRITE(wrf_err_message,'(a,i1,a)') &
            'D0',id,' Spectral nudging is ramped down near the end of the nudging period,'
          CALL wrf_message(TRIM(wrf_err_message))

          WRITE(wrf_err_message,'(a,f6.2,a,f6.2,a)') &
             '      starting at ', (actual_end_fdda_min - ABS(dtramp_min))/60.0,&
             'h, ending at ', actual_end_fdda_min/60.0,'h.'
          CALL wrf_message(TRIM(wrf_err_message))
       ENDIF
     ENDIF

   ENDIF

   IF(.not.restart) THEN
     DO j = jts,jte
     DO k = kts,kte
     DO i = its,ite
        rundgdten(i,k,j) = 0.
        rvndgdten(i,k,j) = 0.
        rthndgdten(i,k,j) = 0.
        rphndgdten(i,k,j) = 0.
     ENDDO
     ENDDO
     ENDDO
   ENDIF

   END SUBROUTINE fddaspnudginginit


END MODULE module_fdda_spnudging
