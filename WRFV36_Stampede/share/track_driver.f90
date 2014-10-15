





SUBROUTINE track_driver( grid )

   USE module_domain
   USE module_configure
   USE module_state_description
   USE module_scalar_tables
   USE module_model_constants
   USE module_date_time

   IMPLICIT NONE

   
   TYPE (domain), INTENT(INOUT) :: grid

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   

   INTEGER :: level
   INTEGER :: level_stag
   INTEGER :: level_zref
   INTEGER :: num_tuv
   INTEGER :: num_rad
   INTEGER :: m, n, i
   INTEGER :: ix, iy
   TYPE(WRFU_Time) :: xcurrent_time, track_time_test 
   CHARACTER (LEN=132) :: message
   CHARACTER (LEN=19) :: xcurrent_timestr
   CHARACTER (LEN=19) :: chem_name
   

   
   IF ( grid%track_loc_domain <= 0 ) RETURN     


   IF ( grid%dfi_opt /= DFI_NODFI .AND. grid%dfi_stage /= DFI_FST ) RETURN




   n = grid%track_next_time

   IF ( grid%track_next_time > grid%track_loc_domain ) RETURN   



   CALL wrf_atotime( grid%track_time_domain(n), track_time_test )



   CALL domain_clock_get( grid, current_time=xcurrent_time, current_timestr=xcurrent_timestr )

   IF ( track_time_test .NE. xcurrent_time ) RETURN  



   write(message,*) 'track_driver: current_time = ',xcurrent_timestr 
   call wrf_message( trim(message) )

   level      = grid%em32-grid%sm32
   level_stag = grid%em32-grid%sm32+1






   ix = grid%track_i(n)
   iy = grid%track_j(n)
  
      IF (grid%sp31 <= ix .AND. ix <= grid%ep31 .AND. &
          grid%sp33 <= iy .AND. iy <= grid%ep33) THEN






         grid%track_z(n,grid%sm32:grid%em32-1)      = grid%z(ix,grid%sm32:grid%em32-1,iy)
         grid%track_p(n,grid%sm32:grid%em32-1)      = grid%p(ix,grid%sm32:grid%em32-1,iy) + &
                                                    grid%pb(ix,grid%sm32:grid%em32-1,iy)
         grid%track_t(n,grid%sm32:grid%em32-1)      = (grid%t_2(ix,grid%sm32:grid%em32-1,iy) + t0 ) * &
                                                    (grid%track_p(n,grid%sm32:grid%em32-1)/p1000mb)**rcp
         grid%track_u(n,grid%sm32:grid%em32-1)      = (grid%u_2(ix,grid%sm32:grid%em32-1,iy) + &
                                                     grid%u_2(ix+1,grid%sm32:grid%em32-1,iy) )*0.5
         grid%track_v(n,grid%sm32:grid%em32-1)      = (grid%v_2(ix,grid%sm32:grid%em32-1,iy) + &
                                                     grid%v_2(ix,grid%sm32:grid%em32-1,iy+1) )*0.5
         grid%track_w(n,grid%sm32:grid%em32)      = grid%w_2(ix,grid%sm32:grid%em32,iy)
         grid%track_rh(n,grid%sm32:grid%em32-1)     = MIN( 1.00,grid%moist(ix,grid%sm32:grid%em32-1,iy,P_QV) /      &
                                                    (3.80*exp(17.27*(grid%track_t(n,grid%sm32:grid%em32-1)-273.)/ &
                                                    (grid%track_t(n,grid%sm32:grid%em32-1)-36.))/                 &
                                                    (.01*grid%track_p(n,grid%sm32:grid%em32-1)))   )
         grid%track_alt(n,grid%sm32:grid%em32-1)    = grid%alt(ix,grid%sm32:grid%em32-1,iy)
         grid%track_qcloud(n,grid%sm32:grid%em32-1) = grid%moist(ix,grid%sm32:grid%em32-1,iy,P_QC)
         grid%track_qrain (n,grid%sm32:grid%em32-1) = grid%moist(ix,grid%sm32:grid%em32-1,iy,P_QR)
         grid%track_qice  (n,grid%sm32:grid%em32-1) = grid%moist(ix,grid%sm32:grid%em32-1,iy,P_QI)
         grid%track_qsnow (n,grid%sm32:grid%em32-1) = grid%moist(ix,grid%sm32:grid%em32-1,iy,P_QS)
         grid%track_qgraup(n,grid%sm32:grid%em32-1) = grid%moist(ix,grid%sm32:grid%em32-1,iy,P_QG)
         grid%track_qvapor(n,grid%sm32:grid%em32-1) = grid%moist(ix,grid%sm32:grid%em32-1,iy,P_QV)


       
      ELSE








         grid%track_z     (n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_p     (n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_t     (n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_u     (n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_v     (n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_w     (n,grid%sm32:grid%em32) = 1.E30
         grid%track_rh    (n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_alt   (n,grid%sm32:grid%em32-1) = 1.E30

         grid%track_qcloud(n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_qrain (n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_qice  (n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_qsnow (n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_qgraup(n,grid%sm32:grid%em32-1) = 1.E30
         grid%track_qvapor(n,grid%sm32:grid%em32-1) = 1.E30
 
      END IF
 




   if ( grid%track_next_time == grid%track_loc_domain ) then



      call write_track(grid)

      write (*,*) 'track_driver: DONE write_track'
   end if
 
   grid%track_next_time = grid%track_next_time + 1


END SUBROUTINE track_driver


SUBROUTINE write_track( grid )

   USE module_dm
   USE module_domain
   USE module_configure

   IMPLICIT NONE

   

   TYPE (domain), INTENT(INOUT) :: grid
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   INTEGER, EXTERNAL :: get_unused_unit

   

   INTEGER :: level
   INTEGER :: level_stag
   INTEGER :: level_zref
   INTEGER :: num_tuv
   INTEGER :: num_rad
   INTEGER :: m,n
   INTEGER :: i
   INTEGER :: ncid
   INTEGER :: astat
   CHARACTER (LEN=19) :: track_output
   CHARACTER (LEN=19) :: chem_name

   character (len=40) :: description
   character (len=40) :: units

   integer, parameter :: DateStrLen = 19
   integer            :: time_dim
   integer            :: level_dim
   integer            :: level_stag_dim
   integer            :: level_zref_dim
   integer            :: rad_dim
   integer            :: tuv_dim
   integer            :: Times_dim
   integer            :: var_dim(3)
   integer            :: var_id
   integer            :: start(3)
   integer            :: count(3) 

   REAL, ALLOCATABLE, DIMENSION(:,:)   :: track_buf2


include 'netcdf.inc'


   IF ( grid%track_loc_domain .LE. 0 ) RETURN

   IF ( grid%dfi_opt /= DFI_NODFI .AND. grid%dfi_stage /= DFI_FST ) RETURN

   level      = grid%em32 - grid%sm32
   level_stag = grid%em32 - grid%sm32 + 1  


   ALLOCATE(track_buf2(grid%track_loc_in, level))



   track_buf2(:,:) = grid%track_z(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_z(:,:),grid%track_loc_in*level)

   track_buf2(:,:) = grid%track_p(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_p(:,:),grid%track_loc_in*level)

   track_buf2(:,:) = grid%track_t(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_t(:,:),grid%track_loc_in*level)

   track_buf2(:,:) = grid%track_u(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_u(:,:),grid%track_loc_in*level)

   track_buf2(:,:) = grid%track_v(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_v(:,:),grid%track_loc_in*level)




   track_buf2(:,:) = grid%track_rh(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_rh(:,:),grid%track_loc_in*level)

   track_buf2(:,:) = grid%track_alt(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_alt(:,:),grid%track_loc_in*level)

   track_buf2(:,:) = grid%track_qcloud(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_qcloud(:,:),grid%track_loc_in*level)

   track_buf2(:,:) = grid%track_qrain(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_qrain(:,:),grid%track_loc_in*level)

   track_buf2(:,:) = grid%track_qice(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_qice(:,:),grid%track_loc_in*level)

   track_buf2(:,:) = grid%track_qsnow(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_qsnow(:,:),grid%track_loc_in*level)

   track_buf2(:,:) = grid%track_qgraup(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_qgraup(:,:),grid%track_loc_in*level)

   track_buf2(:,:) = grid%track_qvapor(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_qvapor(:,:),grid%track_loc_in*level)


   DEALLOCATE(track_buf2)

   ALLOCATE(track_buf2(grid%track_loc_in, level_stag))

   track_buf2(:,:) = grid%track_w(:,:)
   CALL wrf_dm_min_reals(track_buf2(:,:),grid%track_w(:,:),grid%track_loc_in*level_stag)

   DEALLOCATE(track_buf2)



   IF ( wrf_dm_on_monitor() ) THEN










      write (track_output,'(A)') trim('wrfout_track_d00')
      i = len_trim(track_output)
      write ( track_output(i-1:i), '(I2.2)') grid%id



      astat = NF_CREATE(track_output, NF_CLOBBER, ncid)
      if (astat .ne. NF_NOERR) then
         call wrf_abort
      end if



      astat = NF_DEF_DIM(ncid, 'time'       , NF_UNLIMITED , time_dim )
      astat = NF_DEF_DIM(ncid, 'level'      , level        , level_dim)
      astat = NF_DEF_DIM(ncid, 'DateStrLen' , DateStrLen   , Times_dim)
      astat = NF_DEF_DIM(ncid, 'level_stag' , level_stag   , level_stag_dim)




      var_dim(1) = Times_dim
      var_dim(2) = time_dim

      astat = NF_DEF_VAR(ncid,'Times', NF_CHAR, 2, var_dim(1:2), var_id)





      description = 'LATITUDE, SOUTH IS NEGATIVE'
      units       = 'degree_north'
      astat = NF_DEF_VAR(ncid, 'lat'   , NF_REAL, 1, time_dim, var_id  )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

      description = 'LONGITUDE, WEST IS NEGATIVE'
      units       = 'degree_east'
      astat = NF_DEF_VAR(ncid, 'lon'   , NF_REAL, 1, time_dim, var_id  )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

      description = 'grid_i, longitude direction '
      units       = ''
      astat = NF_DEF_VAR(ncid, 'grid_i'   , NF_INT, 1, time_dim, var_id  )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

      description = 'grid_j, latitude direction '
      units       = ''
      astat = NF_DEF_VAR(ncid, 'grid_j'   , NF_INT, 1, time_dim, var_id  )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

      description = 'elevation'
      units       = 'm'
      astat = NF_DEF_VAR(ncid, 'ele'   , NF_REAL, 1, time_dim, var_id  )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )



      var_dim(1) = level_dim
      var_dim(2) = time_dim

      description = 'height'
      units       = 'm'
      astat = NF_DEF_VAR(ncid, 'z', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

      description = 'pressure'
      units       = 'Pa'
      astat = NF_DEF_VAR(ncid, 'p', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

      description = 'temperature'
      units       = 'K'
      astat = NF_DEF_VAR(ncid, 't', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

      description = 'x-wind component'
      units       = 'm s-1'
      astat = NF_DEF_VAR(ncid, 'u', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

      description = 'y-wind component'
      units       = 'm s-1'
      astat = NF_DEF_VAR(ncid, 'v', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )







      description = 'relative humidity'
      units       = 'fraction'
      astat = NF_DEF_VAR(ncid, 'rh', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

      description = 'inverse density'
      units       = 'm3 Kg-1'
      astat = NF_DEF_VAR(ncid, 'alt', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

      description = 'Cloud water mixing ratio'
      units       = 'kg kg-1'
      astat = NF_DEF_VAR(ncid, 'qcloud', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

      description = 'Rain water mixing ratio'
      units       = 'kg kg-1'
      astat = NF_DEF_VAR(ncid, 'qrain', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

      description = 'Ice mixing ratio'
      units       = 'kg kg-1'
      astat = NF_DEF_VAR(ncid, 'qice', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

      description = 'Snow mixing ratio'
      units       = 'kg kg-1'
      astat = NF_DEF_VAR(ncid, 'qsnow', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

      description = 'Graupel mixing ratio'
      units       = 'kg kg-1'
      astat = NF_DEF_VAR(ncid, 'qgraup', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )

      description = 'Water vapor mixing ratio'
      units       = 'kg kg-1'
      astat = NF_DEF_VAR(ncid, 'qvapor', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )



      var_dim(1) = level_stag_dim
      var_dim(2) = time_dim
      description = 'z-wind component'
      units       = 'm s-1'
      astat = NF_DEF_VAR(ncid, 'w', NF_REAL, 2 , var_dim(1:2), var_id )
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'description', len_trim(description),description)
      astat = NF_PUT_ATT_TEXT(ncid,var_id,'units',       len_trim(units),      units      )



      astat = NF_ENDDEF(ncid)



      start(1) = 1
      start(2) = 1
      count(1) = DateStrLen
      count(2) = 1

      astat = NF_INQ_VARID(ncid,'Times',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_TEXT(ncid,var_id,start,count,grid%track_time_domain(m))
      end do
      write (*,*) 'var_id,grid%track_time_domain = ', var_id,grid%track_time_domain





      start(2) = 1
      count(2) = grid%track_loc_domain

      astat = NF_INQ_VARID(ncid,'lat',var_id)
      astat = NF_PUT_VARA_REAL(ncid,var_id,start(2),count(2),grid%track_lat_domain)
      write (*,*) 'var_id,grid%track_lat_domain = ', var_id,grid%track_lat_domain

      astat = NF_INQ_VARID(ncid,'lon',var_id)
      astat = NF_PUT_VARA_REAL(ncid,var_id,start(2),count(2),grid%track_lon_domain)
      write (*,*) 'var_id,grid%track_lon_domain = ', var_id,grid%track_lon_domain

      astat = NF_INQ_VARID(ncid,'grid_i',var_id)
      astat = NF_PUT_VARA_INT(ncid,var_id,start(2),count(2),grid%track_i)

      astat = NF_INQ_VARID(ncid,'grid_j',var_id)
      astat = NF_PUT_VARA_INT(ncid,var_id,start(2),count(2),grid%track_j)

      astat = NF_INQ_VARID(ncid,'ele',var_id)
      astat = NF_PUT_VARA_REAL(ncid,var_id,start(2),count(2),grid%track_ele)
      write (*,*) 'var_id,grid%track_ele = ', var_id,grid%track_ele



      start(1) = 1
      start(2) = 1
      count(1) = level
      count(2) = 1

      astat = NF_INQ_VARID(ncid,'z',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_z(m,:))
      end do

      astat = NF_INQ_VARID(ncid,'p',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_p(m,:))
      end do

      astat = NF_INQ_VARID(ncid,'t',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_t(m,:))
      end do

      astat = NF_INQ_VARID(ncid,'u',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_u(m,:))
      end do

      astat = NF_INQ_VARID(ncid,'v',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_v(m,:))
      end do







      astat = NF_INQ_VARID(ncid,'rh',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_rh(m,:))
      end do

      astat = NF_INQ_VARID(ncid,'alt',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_alt(m,:))
      end do

      astat = NF_INQ_VARID(ncid,'qcloud',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_qcloud(m,:))
      end do

      astat = NF_INQ_VARID(ncid,'qrain',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_qrain(m,:))
      end do

      astat = NF_INQ_VARID(ncid,'qice',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_qice(m,:))
      end do

      astat = NF_INQ_VARID(ncid,'qsnow',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_qsnow(m,:))
      end do

      astat = NF_INQ_VARID(ncid,'qgraup',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_qgraup(m,:))
      end do

      astat = NF_INQ_VARID(ncid,'qvapor',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_qvapor(m,:))
      end do



      start(1) = 1
      start(2) = 1
      count(1) = level_stag
      count(2) = 1

      astat = NF_INQ_VARID(ncid,'w',var_id)
      do m= 1,grid%track_loc_domain
         start(2) = m
         astat = NF_PUT_VARA_REAL(ncid,var_id,start(1:2),count(1:2),grid%track_w(m,:))
      end do



      astat = NF_CLOSE(ncid)

   END IF

   grid%track_next_time = 1

END SUBROUTINE write_track

SUBROUTINE calc_track_locations( grid )

   USE module_domain
   USE module_configure
   USE module_dm
   USE module_llxy

   IMPLICIT NONE

   
   TYPE (domain), INTENT(INOUT) :: grid
   
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   
   INTEGER :: track_loc_temp
   INTEGER :: i, k, iunit
   REAL :: track_rx, track_ry
   REAL :: known_lat, known_lon
   CHARACTER (LEN=132) :: message
   TYPE (PROJ_INFO) :: track_proj
   TYPE (grid_config_rec_type) :: config_flags

   INTEGER :: ids, ide, jds, jde, kds, kde,        &
              ims, ime, jms, jme, kms, kme,        &
              ips, ipe, jps, jpe, kps, kpe,        &
              imsx, imex, jmsx, jmex, kmsx, kmex,  &
              ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
              imsy, imey, jmsy, jmey, kmsy, kmey,  &
              ipsy, ipey, jpsy, jpey, kpsy, kpey

   IF ( grid%track_loc <= 0 ) then
     RETURN
   ENDIF

   IF ( grid%dfi_stage == DFI_FST ) THEN
     CALL get_ijk_from_grid ( grid ,                               &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              imsx, imex, jmsx, jmex, kmsx, kmex,  &
                              ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
                              imsy, imey, jmsy, jmey, kmsy, kmey,  &
                              ipsy, ipey, jpsy, jpey, kpsy, kpey )
   
     CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   

     CALL map_init(track_proj)
   
     IF (ips <= 1 .AND. 1 <= ipe .AND. &
         jps <= 1 .AND. 1 <= jpe) THEN
        known_lat = grid%xlat(1,1)
        known_lon = grid%xlong(1,1)
     ELSE
        known_lat = 9999.
        known_lon = 9999.
     ENDIF
     known_lat = wrf_dm_min_real(known_lat)
     known_lon = wrf_dm_min_real(known_lon)
   

   IF (config_flags%map_proj == PROJ_MERC) THEN
     CALL map_set(PROJ_MERC, track_proj,                &
                      truelat1 = config_flags%truelat1, &
                      lat1     = known_lat,             &
                      lon1     = known_lon,             &
                      knowni   = 1.,                    &
                      knownj   = 1.,                    &
                      dx       = config_flags%dx)
   

   ELSE IF (config_flags%map_proj == PROJ_LC) THEN
     CALL map_set(PROJ_LC, track_proj,                   &
                      truelat1 = config_flags%truelat1,  &
                      truelat2 = config_flags%truelat2,  &
                      stdlon   = config_flags%stand_lon, &
                      lat1     = known_lat,              &
                      lon1     = known_lon,              &
                      knowni   = 1.,                     &
                      knownj   = 1.,                     &
                      dx       = config_flags%dx)
   

   ELSE IF (config_flags%map_proj == PROJ_PS) THEN
     CALL map_set(PROJ_PS, track_proj,                   &
                      truelat1 = config_flags%truelat1,  &
                      stdlon   = config_flags%stand_lon, &
                      lat1     = known_lat,              &
                      lon1     = known_lon,              &
                      knowni   = 1.,                     &
                      knownj   = 1.,                     &
                      dx       = config_flags%dx)
   

   ELSE IF (config_flags%map_proj == PROJ_CASSINI) THEN
     CALL map_set(PROJ_CASSINI, track_proj,                             &
                      latinc   = grid%dy*360.0/(2.0*EARTH_RADIUS_M*PI), &
                      loninc   = grid%dx*360.0/(2.0*EARTH_RADIUS_M*PI), & 
                      lat1     = known_lat,                             &
                      lon1     = known_lon,                             &


                      lat0     = 90.0,                                  &
                      lon0     = 0.0,                                   &
                      knowni   = 1.,                                    &
                      knownj   = 1.,                                    &
                      stdlon   = config_flags%stand_lon)


   ELSE IF (config_flags%map_proj == PROJ_ROTLL) THEN
     CALL map_set(PROJ_ROTLL, track_proj,                       &

                      ixdim    = grid%e_we-1,                   &
                      jydim    = grid%e_sn-1,                   &
                      phi      = real(grid%e_sn-2)*grid%dy/2.0, &
                      lambda   = real(grid%e_we-2)*grid%dx,     &
                      lat1     = config_flags%cen_lat,          &
                      lon1     = config_flags%cen_lon,          &
                      latinc   = grid%dy,                       &
                      loninc   = grid%dx,                       &
                      stagger  = HH)
   
   ENDIF

   IF (.NOT. grid%track_have_calculated) THEN
     grid%track_have_calculated = .TRUE.
     WRITE(message, '(A46,I3)') 'Computing track locations inside model domain ', grid%id
     CALL wrf_message(message)





     grid%track_next_time = 1



     track_loc_temp = 0
     DO k = 1,grid%track_loc
       CALL latlon_to_ij(track_proj, grid%track_lat_in(k), grid%track_lon_in(k), track_rx, track_ry)
       track_loc_temp = track_loc_temp + 1



       grid%track_i(track_loc_temp) = NINT(track_rx)
       grid%track_j(track_loc_temp) = NINT(track_ry)



       grid%track_time_domain(track_loc_temp) = grid%track_time_in(k)



       IF (grid%track_i(track_loc_temp) < ids .OR. grid%track_i(track_loc_temp) > ide .OR. &
           grid%track_j(track_loc_temp) < jds .OR. grid%track_j(track_loc_temp) > jde) THEN
         track_loc_temp = track_loc_temp - 1   
       ENDIF
     ENDDO




     grid%track_loc_domain = track_loc_temp



     DO k = 1,grid%track_loc_domain



       IF (grid%track_i(k) < ips .OR. grid%track_i(k) > ipe .OR. &
           grid%track_j(k) < jps .OR. grid%track_j(k) > jpe) THEN
         grid%track_lat_domain(k) = 1.E30
         grid%track_lon_domain(k) = 1.E30
         grid%track_ele(k) = 1.E30
       ELSE
         grid%track_lat_domain(k) = grid%xlat(grid%track_i(k),grid%track_j(k))
         grid%track_lon_domain(k) = grid%xlong(grid%track_i(k),grid%track_j(k))
         grid%track_ele(k) = grid%ht(grid%track_i(k),grid%track_j(k))
       ENDIF

       grid%track_ele(k)         = wrf_dm_min_real(grid%track_ele(k))
       grid%track_lat_domain(k)  = wrf_dm_min_real(grid%track_lat_domain(k))
       grid%track_lon_domain(k)  = wrf_dm_min_real(grid%track_lon_domain(k))

       call wrf_dm_bcast_string(grid%track_time_domain(k), 19)
     END DO

     write(message,*) 'calc_track_locations: valid track locations in the model domain ', grid%track_loc_domain
     call wrf_message( trim(message) )
   
    ENDIF
   ENDIF

END SUBROUTINE calc_track_locations
