module input_module

   use gridinfo_module
   use misc_definitions_module
   use module_debug

   use module_internal_header_util

   use parallel_module
   use queue_module
 
   type (queue) :: unit_desc
 
   ! WRF I/O API related variables
   integer :: handle
 
   integer :: num_calls
 
   character (len=1) :: internal_gridtype
 
   contains
 
 
   subroutine input_init(nest_number, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: nest_number
      integer, intent(out) :: istatus
  
      integer, parameter  :: WRF_FILE_NOT_OPENED                  = 100
      integer, parameter  :: WRF_FILE_OPENED_NOT_COMMITTED        = 101
      integer, parameter  :: WRF_FILE_OPENED_FOR_WRITE            = 102
      integer, parameter  :: WRF_FILE_OPENED_FOR_READ             = 103
      integer, parameter  :: WRF_REAL                             = 104
      integer, parameter  :: WRF_DOUBLE                           = 105
      integer, parameter  :: WRF_FLOAT=WRF_REAL
      integer, parameter  :: WRF_INTEGER                          = 106
      integer, parameter  :: WRF_LOGICAL                          = 107
      integer, parameter  :: WRF_COMPLEX                          = 108
      integer, parameter  :: WRF_DOUBLE_COMPLEX                   = 109
      integer, parameter  :: WRF_FILE_OPENED_FOR_UPDATE           = 110
  
!WRF Error and Warning messages (1-999)
!All i/o package-specific status codes you may want to add must be handled by your package (see below)
! WRF handles these and netCDF messages only
  integer, parameter  :: WRF_NO_ERR                  =  0       !no error
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       !file not found, or incomplete
  integer, parameter  :: WRF_WARN_MD_NF              = -2       !metadata not found
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       !timestamp not found
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       !no more timestamps
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       !variable not found
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       !no more variables for the current time
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       !too many open files
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       !data type mismatch
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       !attempt to write readonly file
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      !attempt to read writeonly file
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      !attempt to access unopened file
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      !attempt to do 2 trainings for 1 variable
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      !attempt to read past EOF
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      !bad data handle
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      !write length not equal to training length
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      !more dimensions requested than training
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      !attempt to read more data than exists
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      !input dimension inconsistent
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      !input MemoryOrder not recognized
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      !a dimension name with 2 different lengths
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      !string longer than provided storage
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      !function not supportable
  integer, parameter  :: WRF_WARN_NOOP               = -23      !package implements this routine as NOOP

!Fatal errors 
  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 !allocation error
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 !dealloc error
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 !bad file status


!Package specific errors (1000+)        
!Netcdf status codes
!WRF will accept status codes of 1000+, but it is up to the package to handle
! and return the status to the user.

  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    !this code not in either spec?
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    !this code not in either spec?
  integer, parameter  :: WRF_WARN_FILE_OPEN_FOR_READ = -1009
  integer, parameter  :: WRF_IO_NOT_INITIALIZED      = -1010
  integer, parameter  :: WRF_WARN_MD_AFTER_OPEN      = -1011
  integer, parameter  :: WRF_WARN_TOO_MANY_VARIABLES = -1012
  integer, parameter  :: WRF_WARN_DRYRUN_CLOSE       = -1013
  integer, parameter  :: WRF_WARN_DATESTR_BAD_LENGTH = -1014
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_READ   = -1015
  integer, parameter  :: WRF_WARN_DATA_TYPE_NOT_FOUND = -1016
  integer, parameter  :: WRF_WARN_DATESTR_ERROR      = -1017
  integer, parameter  :: WRF_WARN_DRYRUN_READ        = -1018
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_GET    = -1019
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_PUT    = -1020
  integer, parameter  :: WRF_WARN_NETCDF             = -1021    
  integer, parameter  :: WRF_WARN_LENGTH_LESS_THAN_1 = -1022    
  integer, parameter  :: WRF_WARN_MORE_DATA_IN_FILE  = -1023    
  integer, parameter  :: WRF_WARN_DATE_LT_LAST_DATE  = -1024

! For HDF5 only
  integer, parameter  :: WRF_HDF5_ERR_FILE                 = -200
  integer, parameter  :: WRF_HDF5_ERR_MD                   = -201
  integer, parameter  :: WRF_HDF5_ERR_TIME                 = -202
  integer, parameter  :: WRF_HDF5_ERR_TIME_EOF             = -203
  integer, parameter  :: WRF_HDF5_ERR_MORE_DATA_IN_FILE    = -204
  integer, parameter  :: WRF_HDF5_ERR_DATE_LT_LAST_DATE    = -205
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_FILES       = -206
  integer, parameter  :: WRF_HDF5_ERR_TYPE_MISMATCH        = -207
  integer, parameter  :: WRF_HDF5_ERR_LENGTH_LESS_THAN_1   = -208
  integer, parameter  :: WRF_HDF5_ERR_WRITE_RONLY_FILE     = -209
  integer, parameter  :: WRF_HDF5_ERR_READ_WONLY_FILE      = -210
  integer, parameter  :: WRF_HDF5_ERR_FILE_NOT_OPENED      = -211
  integer, parameter  :: WRF_HDF5_ERR_DATESTR_ERROR        = -212
  integer, parameter  :: WRF_HDF5_ERR_DRYRUN_READ          = -213
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_GET      = -214
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_PUT      = -215
  integer, parameter  :: WRF_HDF5_ERR_2DRYRUNS_1VARIABLE   = -216
  integer, parameter  :: WRF_HDF5_ERR_DATA_TYPE_NOTFOUND   = -217
  integer, parameter  :: WRF_HDF5_ERR_READ_PAST_EOF        = -218
  integer, parameter  :: WRF_HDF5_ERR_BAD_DATA_HANDLE      = -219
  integer, parameter  :: WRF_HDF5_ERR_WRTLEN_NE_DRRUNLEN   = -220
  integer, parameter  :: WRF_HDF5_ERR_DRYRUN_CLOSE         = -221
  integer, parameter  :: WRF_HDF5_ERR_DATESTR_BAD_LENGTH   = -222
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_READ     = -223
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_DIMS        = -224
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_VARIABLES   = -225
  integer, parameter  :: WRF_HDF5_ERR_COUNT_TOO_LONG       = -226
  integer, parameter  :: WRF_HDF5_ERR_DIMENSION_ERROR      = -227
  integer, parameter  :: WRF_HDF5_ERR_BAD_MEMORYORDER      = -228
  integer, parameter  :: WRF_HDF5_ERR_DIMNAME_REDEFINED    = -229
  integer, parameter  :: WRF_HDF5_ERR_MD_AFTER_OPEN        = -230
  integer, parameter  :: WRF_HDF5_ERR_CHARSTR_GT_LENDATA   = -231
  integer, parameter  :: WRF_HDF5_ERR_BAD_DATA_TYPE        = -232
  integer, parameter  :: WRF_HDF5_ERR_FILE_NOT_COMMITTED   = -233

  integer, parameter  :: WRF_HDF5_ERR_ALLOCATION        = -2001
  integer, parameter  :: WRF_HDF5_ERR_DEALLOCATION      = -2002
  integer, parameter  :: WRF_HDF5_ERR_BAD_FILE_STATUS   = -2003
  integer, parameter  :: WRF_HDF5_ERR_BAD_VARIABLE_DIM  = -2004
  integer, parameter  :: WRF_HDF5_ERR_MDVAR_DIM_NOT_1D  = -2005
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_TIMES    = -2006
  integer, parameter ::  WRF_HDF5_ERR_DATA_ID_NOTFOUND  = -2007

  integer, parameter ::  WRF_HDF5_ERR_DATASPACE         = -300
  integer, parameter ::  WRF_HDF5_ERR_DATATYPE          = -301
  integer, parameter :: WRF_HDF5_ERR_PROPERTY_LIST      = -302

  integer, parameter :: WRF_HDF5_ERR_DATASET_CREATE     = -303
  integer, parameter :: WRF_HDF5_ERR_DATASET_READ       = -304
  integer, parameter :: WRF_HDF5_ERR_DATASET_WRITE      = -305
  integer, parameter :: WRF_HDF5_ERR_DATASET_OPEN       = -306
  integer, parameter :: WRF_HDF5_ERR_DATASET_GENERAL    = -307
  integer, parameter :: WRF_HDF5_ERR_GROUP              = -308

  integer, parameter :: WRF_HDF5_ERR_FILE_OPEN          = -309
  integer, parameter :: WRF_HDF5_ERR_FILE_CREATE        = -310
  integer, parameter :: WRF_HDF5_ERR_DATASET_CLOSE      = -311
  integer, parameter :: WRF_HDF5_ERR_FILE_CLOSE         = -312
  integer, parameter :: WRF_HDF5_ERR_CLOSE_GENERAL      = -313

  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_CREATE   = -314
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_READ     = -315
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_WRITE    = -316
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_OPEN     = -317
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_GENERAL  = -318
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_CLOSE    = -319

  integer, parameter :: WRF_HDF5_ERR_OTHERS             = -320
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_OTHERS   = -321

  integer, parameter :: WRF_GRIB2_ERR_GRIBCREATE        = -401
  integer, parameter :: WRF_GRIB2_ERR_ADDLOCAL          = -402
  integer, parameter :: WRF_GRIB2_ERR_ADDGRIB           = -403
  integer, parameter :: WRF_GRIB2_ERR_ADDFIELD          = -404
  integer, parameter :: WRF_GRIB2_ERR_GRIBEND           = -405
  integer, parameter :: WRF_GRIB2_ERR_WRITE             = -406
  integer, parameter :: WRF_GRIB2_ERR_GRIB2MAP          = -407
  integer, parameter :: WRF_GRIB2_ERR_GETGB2            = -408
  integer, parameter :: WRF_GRIB2_ERR_READ              = -409
  
      ! Local variables
      integer :: i
      integer :: comm_1, comm_2
      character (len=MAX_FILENAME_LEN) :: input_fname
  
      istatus = 0
  
      if (my_proc_id == IO_NODE .or. do_tiled_input) then
  
         if (io_form_input == BINARY) call ext_int_ioinit('sysdep info', istatus)
         if (io_form_input == NETCDF) call ext_ncd_ioinit('sysdep info', istatus)
         if (io_form_input == GRIB1) call ext_gr1_ioinit('sysdep info', istatus)
         call mprintf((istatus /= 0),ERROR,'Error in ext_pkg_ioinit')
     
         comm_1 = 1
         comm_2 = 1
         input_fname = ' '
         if (gridtype == 'C') then
            if (io_form_input == BINARY) input_fname = trim(opt_output_from_geogrid_path)//'geo_em.d  .int'
            if (io_form_input == NETCDF) input_fname = trim(opt_output_from_geogrid_path)//'geo_em.d  .nc'
            if (io_form_input == GRIB1) input_fname = trim(opt_output_from_geogrid_path)//'geo_em.d  .grib'
            i = len_trim(opt_output_from_geogrid_path)
            write(input_fname(i+9:i+10),'(i2.2)') nest_number
         else if (gridtype == 'E') then
            if (io_form_input == BINARY) input_fname = trim(opt_output_from_geogrid_path)//'geo_nmm.d  .int'
            if (io_form_input == NETCDF) input_fname = trim(opt_output_from_geogrid_path)//'geo_nmm.d  .nc'
            if (io_form_input == GRIB1) input_fname = trim(opt_output_from_geogrid_path)//'geo_nmm.d  .grib'
            i = len_trim(opt_output_from_geogrid_path)
            write(input_fname(i+10:i+11),'(i2.2)') nest_number
         end if

         if (nprocs > 1 .and. do_tiled_input) then
            write(input_fname(len_trim(input_fname)+1:len_trim(input_fname)+5), '(a1,i4.4)') &
                            '_', my_proc_id
         end if
     
         istatus = 0
         if (io_form_input == BINARY) &
            call ext_int_open_for_read(trim(input_fname), comm_1, comm_2, 'sysdep info', handle, istatus)
         if (io_form_input == NETCDF) &
            call ext_ncd_open_for_read(trim(input_fname), comm_1, comm_2, 'sysdep info', handle, istatus)
         if (io_form_input == GRIB1) &
            call ext_gr1_open_for_read(trim(input_fname), comm_1, comm_2, 'sysdep info', handle, istatus)
         call mprintf((istatus /= 0),ERROR,'Couldn''t open file %s for input.',s1=input_fname)
     
         call q_init(unit_desc)
  
      end if ! (my_proc_id == IO_NODE .or. do_tiled_input)
  
      num_calls = 0
 
   end subroutine input_init
 
 
   subroutine read_next_field(start_patch_i, end_patch_i, &
                              start_patch_j, end_patch_j, &
                              start_patch_k, end_patch_k, &
                              cname, cunits, cdesc, memorder, stagger, &
                              dimnames, sr_x, sr_y, real_array, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: start_patch_i, end_patch_i, &
                              start_patch_j, end_patch_j, &
                              start_patch_k, end_patch_k, &
                              sr_x, sr_y
      real, pointer, dimension(:,:,:) :: real_array
      character (len=*), intent(out) :: cname, memorder, stagger, cunits, cdesc
      character (len=128), dimension(3), intent(inout) :: dimnames
      integer, intent(inout) :: istatus
  
      integer, parameter  :: WRF_FILE_NOT_OPENED                  = 100
      integer, parameter  :: WRF_FILE_OPENED_NOT_COMMITTED        = 101
      integer, parameter  :: WRF_FILE_OPENED_FOR_WRITE            = 102
      integer, parameter  :: WRF_FILE_OPENED_FOR_READ             = 103
      integer, parameter  :: WRF_REAL                             = 104
      integer, parameter  :: WRF_DOUBLE                           = 105
      integer, parameter  :: WRF_FLOAT=WRF_REAL
      integer, parameter  :: WRF_INTEGER                          = 106
      integer, parameter  :: WRF_LOGICAL                          = 107
      integer, parameter  :: WRF_COMPLEX                          = 108
      integer, parameter  :: WRF_DOUBLE_COMPLEX                   = 109
      integer, parameter  :: WRF_FILE_OPENED_FOR_UPDATE           = 110
  
!WRF Error and Warning messages (1-999)
!All i/o package-specific status codes you may want to add must be handled by your package (see below)
! WRF handles these and netCDF messages only
  integer, parameter  :: WRF_NO_ERR                  =  0       !no error
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       !file not found, or incomplete
  integer, parameter  :: WRF_WARN_MD_NF              = -2       !metadata not found
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       !timestamp not found
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       !no more timestamps
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       !variable not found
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       !no more variables for the current time
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       !too many open files
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       !data type mismatch
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       !attempt to write readonly file
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      !attempt to read writeonly file
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      !attempt to access unopened file
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      !attempt to do 2 trainings for 1 variable
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      !attempt to read past EOF
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      !bad data handle
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      !write length not equal to training length
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      !more dimensions requested than training
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      !attempt to read more data than exists
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      !input dimension inconsistent
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      !input MemoryOrder not recognized
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      !a dimension name with 2 different lengths
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      !string longer than provided storage
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      !function not supportable
  integer, parameter  :: WRF_WARN_NOOP               = -23      !package implements this routine as NOOP

!Fatal errors 
  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 !allocation error
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 !dealloc error
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 !bad file status


!Package specific errors (1000+)        
!Netcdf status codes
!WRF will accept status codes of 1000+, but it is up to the package to handle
! and return the status to the user.

  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    !this code not in either spec?
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    !this code not in either spec?
  integer, parameter  :: WRF_WARN_FILE_OPEN_FOR_READ = -1009
  integer, parameter  :: WRF_IO_NOT_INITIALIZED      = -1010
  integer, parameter  :: WRF_WARN_MD_AFTER_OPEN      = -1011
  integer, parameter  :: WRF_WARN_TOO_MANY_VARIABLES = -1012
  integer, parameter  :: WRF_WARN_DRYRUN_CLOSE       = -1013
  integer, parameter  :: WRF_WARN_DATESTR_BAD_LENGTH = -1014
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_READ   = -1015
  integer, parameter  :: WRF_WARN_DATA_TYPE_NOT_FOUND = -1016
  integer, parameter  :: WRF_WARN_DATESTR_ERROR      = -1017
  integer, parameter  :: WRF_WARN_DRYRUN_READ        = -1018
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_GET    = -1019
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_PUT    = -1020
  integer, parameter  :: WRF_WARN_NETCDF             = -1021    
  integer, parameter  :: WRF_WARN_LENGTH_LESS_THAN_1 = -1022    
  integer, parameter  :: WRF_WARN_MORE_DATA_IN_FILE  = -1023    
  integer, parameter  :: WRF_WARN_DATE_LT_LAST_DATE  = -1024

! For HDF5 only
  integer, parameter  :: WRF_HDF5_ERR_FILE                 = -200
  integer, parameter  :: WRF_HDF5_ERR_MD                   = -201
  integer, parameter  :: WRF_HDF5_ERR_TIME                 = -202
  integer, parameter  :: WRF_HDF5_ERR_TIME_EOF             = -203
  integer, parameter  :: WRF_HDF5_ERR_MORE_DATA_IN_FILE    = -204
  integer, parameter  :: WRF_HDF5_ERR_DATE_LT_LAST_DATE    = -205
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_FILES       = -206
  integer, parameter  :: WRF_HDF5_ERR_TYPE_MISMATCH        = -207
  integer, parameter  :: WRF_HDF5_ERR_LENGTH_LESS_THAN_1   = -208
  integer, parameter  :: WRF_HDF5_ERR_WRITE_RONLY_FILE     = -209
  integer, parameter  :: WRF_HDF5_ERR_READ_WONLY_FILE      = -210
  integer, parameter  :: WRF_HDF5_ERR_FILE_NOT_OPENED      = -211
  integer, parameter  :: WRF_HDF5_ERR_DATESTR_ERROR        = -212
  integer, parameter  :: WRF_HDF5_ERR_DRYRUN_READ          = -213
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_GET      = -214
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_PUT      = -215
  integer, parameter  :: WRF_HDF5_ERR_2DRYRUNS_1VARIABLE   = -216
  integer, parameter  :: WRF_HDF5_ERR_DATA_TYPE_NOTFOUND   = -217
  integer, parameter  :: WRF_HDF5_ERR_READ_PAST_EOF        = -218
  integer, parameter  :: WRF_HDF5_ERR_BAD_DATA_HANDLE      = -219
  integer, parameter  :: WRF_HDF5_ERR_WRTLEN_NE_DRRUNLEN   = -220
  integer, parameter  :: WRF_HDF5_ERR_DRYRUN_CLOSE         = -221
  integer, parameter  :: WRF_HDF5_ERR_DATESTR_BAD_LENGTH   = -222
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_READ     = -223
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_DIMS        = -224
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_VARIABLES   = -225
  integer, parameter  :: WRF_HDF5_ERR_COUNT_TOO_LONG       = -226
  integer, parameter  :: WRF_HDF5_ERR_DIMENSION_ERROR      = -227
  integer, parameter  :: WRF_HDF5_ERR_BAD_MEMORYORDER      = -228
  integer, parameter  :: WRF_HDF5_ERR_DIMNAME_REDEFINED    = -229
  integer, parameter  :: WRF_HDF5_ERR_MD_AFTER_OPEN        = -230
  integer, parameter  :: WRF_HDF5_ERR_CHARSTR_GT_LENDATA   = -231
  integer, parameter  :: WRF_HDF5_ERR_BAD_DATA_TYPE        = -232
  integer, parameter  :: WRF_HDF5_ERR_FILE_NOT_COMMITTED   = -233

  integer, parameter  :: WRF_HDF5_ERR_ALLOCATION        = -2001
  integer, parameter  :: WRF_HDF5_ERR_DEALLOCATION      = -2002
  integer, parameter  :: WRF_HDF5_ERR_BAD_FILE_STATUS   = -2003
  integer, parameter  :: WRF_HDF5_ERR_BAD_VARIABLE_DIM  = -2004
  integer, parameter  :: WRF_HDF5_ERR_MDVAR_DIM_NOT_1D  = -2005
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_TIMES    = -2006
  integer, parameter ::  WRF_HDF5_ERR_DATA_ID_NOTFOUND  = -2007

  integer, parameter ::  WRF_HDF5_ERR_DATASPACE         = -300
  integer, parameter ::  WRF_HDF5_ERR_DATATYPE          = -301
  integer, parameter :: WRF_HDF5_ERR_PROPERTY_LIST      = -302

  integer, parameter :: WRF_HDF5_ERR_DATASET_CREATE     = -303
  integer, parameter :: WRF_HDF5_ERR_DATASET_READ       = -304
  integer, parameter :: WRF_HDF5_ERR_DATASET_WRITE      = -305
  integer, parameter :: WRF_HDF5_ERR_DATASET_OPEN       = -306
  integer, parameter :: WRF_HDF5_ERR_DATASET_GENERAL    = -307
  integer, parameter :: WRF_HDF5_ERR_GROUP              = -308

  integer, parameter :: WRF_HDF5_ERR_FILE_OPEN          = -309
  integer, parameter :: WRF_HDF5_ERR_FILE_CREATE        = -310
  integer, parameter :: WRF_HDF5_ERR_DATASET_CLOSE      = -311
  integer, parameter :: WRF_HDF5_ERR_FILE_CLOSE         = -312
  integer, parameter :: WRF_HDF5_ERR_CLOSE_GENERAL      = -313

  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_CREATE   = -314
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_READ     = -315
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_WRITE    = -316
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_OPEN     = -317
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_GENERAL  = -318
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_CLOSE    = -319

  integer, parameter :: WRF_HDF5_ERR_OTHERS             = -320
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_OTHERS   = -321

  integer, parameter :: WRF_GRIB2_ERR_GRIBCREATE        = -401
  integer, parameter :: WRF_GRIB2_ERR_ADDLOCAL          = -402
  integer, parameter :: WRF_GRIB2_ERR_ADDGRIB           = -403
  integer, parameter :: WRF_GRIB2_ERR_ADDFIELD          = -404
  integer, parameter :: WRF_GRIB2_ERR_GRIBEND           = -405
  integer, parameter :: WRF_GRIB2_ERR_WRITE             = -406
  integer, parameter :: WRF_GRIB2_ERR_GRIB2MAP          = -407
  integer, parameter :: WRF_GRIB2_ERR_GETGB2            = -408
  integer, parameter :: WRF_GRIB2_ERR_READ              = -409
  
      ! Local variables
      integer :: ndim, wrftype
      integer :: sm1, em1, sm2, em2, sm3, em3, sp1, ep1, sp2, ep2, sp3, ep3
      integer, dimension(3) :: domain_start, domain_end, temp
      real, pointer, dimension(:,:,:) :: real_domain
      character (len=20) :: datestr
      type (q_data) :: qd
  
      if (my_proc_id == IO_NODE .or. do_tiled_input) then
  
         if (num_calls == 0) then
            if (io_form_input == BINARY) call ext_int_get_next_time(handle, datestr, istatus)
            if (io_form_input == NETCDF) call ext_ncd_get_next_time(handle, datestr, istatus)
            if (io_form_input == GRIB1) call ext_gr1_get_next_time(handle, datestr, istatus)
         end if
     
         num_calls = num_calls + 1
   
         if (io_form_input == BINARY) call ext_int_get_next_var(handle, cname, istatus) 
         if (io_form_input == NETCDF) call ext_ncd_get_next_var(handle, cname, istatus) 
         if (io_form_input == GRIB1) call ext_gr1_get_next_var(handle, cname, istatus) 
      end if
  
      if (nprocs > 1 .and. .not. do_tiled_input) call parallel_bcast_int(istatus)
      if (istatus /= 0) return
  
      if (my_proc_id == IO_NODE .or. do_tiled_input) then
  
         istatus = 0
         if (io_form_input == BINARY) then
            call ext_int_get_var_info(handle, cname, ndim, memorder, stagger, domain_start, domain_end, wrftype, istatus)
         end if
         if (io_form_input == NETCDF) then
            call ext_ncd_get_var_info(handle, cname, ndim, memorder, stagger, domain_start, domain_end, wrftype, istatus)
            call ext_ncd_get_var_ti_integer(handle, 'sr_x', &
                                            trim(cname), temp(1), 1, temp(3), istatus)
            call ext_ncd_get_var_ti_integer(handle, 'sr_y', &
                                            trim(cname), temp(2), 1, temp(3), istatus)
         end if
         if (io_form_input == GRIB1) then
            call ext_gr1_get_var_info(handle, cname, ndim, memorder, stagger, domain_start, domain_end, wrftype, istatus)
            call ext_gr1_get_var_ti_integer(handle, 'sr_x', &
                                            trim(cname), temp(1), 1, temp(3), istatus)
            call ext_gr1_get_var_ti_integer(handle, 'sr_y', &
                                            trim(cname), temp(2), 1, temp(3), istatus)
         end if
     
         call mprintf((istatus /= 0),ERROR,'In read_next_field(), problems with ext_pkg_get_var_info()')

         start_patch_i = domain_start(1) 
         start_patch_j = domain_start(2) 
         end_patch_i = domain_end(1)
         end_patch_j = domain_end(2)
         if (ndim == 3) then
            start_patch_k = domain_start(3) 
            end_patch_k = domain_end(3) 
         else
            domain_start(3) = 1
            domain_end(3) = 1
            start_patch_k = 1
            end_patch_k = 1
         end if
     
         nullify(real_domain)
     
         allocate(real_domain(start_patch_i:end_patch_i, start_patch_j:end_patch_j, start_patch_k:end_patch_k))
         if (io_form_input == BINARY) then
            call ext_int_read_field(handle, '0000-00-00_00:00:00', cname, real_domain, WRF_REAL, &
                          1, 1, 0, memorder, stagger, &
                          dimnames, domain_start, domain_end, domain_start, domain_end, &
                          domain_start, domain_end, istatus)
         end if
         if (io_form_input == NETCDF) then
            call ext_ncd_read_field(handle, '0000-00-00_00:00:00', cname, real_domain, WRF_REAL, &
                          1, 1, 0, memorder, stagger, &
                          dimnames, domain_start, domain_end, domain_start, domain_end, &
                          domain_start, domain_end, istatus)
         end if
         if (io_form_input == GRIB1) then
            call ext_gr1_read_field(handle, '0000-00-00_00:00:00', cname, real_domain, WRF_REAL, &
                          1, 1, 0, memorder, stagger, &
                          dimnames, domain_start, domain_end, domain_start, domain_end, &
                          domain_start, domain_end, istatus)
         end if
     
         call mprintf((istatus /= 0),ERROR,'In read_next_field(), got error code %i.', i1=istatus)

         if (io_form_input == BINARY) then
            qd = q_remove(unit_desc)
            cunits = qd%units
            cdesc = qd%description
            stagger = qd%stagger
            sr_x = qd%sr_x
            sr_y = qd%sr_y
         else
            cunits = ' '
            cdesc = ' '
            stagger = ' '
            sr_x = temp(1)
            sr_y = temp(2)
        
            if (io_form_input == NETCDF) then
               call ext_ncd_get_var_ti_char(handle, 'units', cname, cunits, istatus)
               call ext_ncd_get_var_ti_char(handle, 'description', cname, cdesc, istatus)
               call ext_ncd_get_var_ti_char(handle, 'stagger', cname, stagger, istatus)
            end if
            if (io_form_input == GRIB1) then
               call ext_gr1_get_var_ti_char(handle, 'units', cname, cunits, istatus)
               call ext_gr1_get_var_ti_char(handle, 'description', cname, cdesc, istatus)
               call ext_gr1_get_var_ti_char(handle, 'stagger', cname, stagger, istatus)
            end if
         end if
  
      end if ! (my_proc_id == IO_NODE .or. do_tiled_input)

      if (nprocs > 1 .and. .not. do_tiled_input) then
         call parallel_bcast_char(cname, len(cname))
         call parallel_bcast_char(cunits, len(cunits))
         call parallel_bcast_char(cdesc, len(cdesc))
         call parallel_bcast_char(memorder, len(memorder))
         call parallel_bcast_char(stagger, len(stagger))
         call parallel_bcast_char(dimnames(1), 128)
         call parallel_bcast_char(dimnames(2), 128)
         call parallel_bcast_char(dimnames(3), 128)
         call parallel_bcast_int(domain_start(3))
         call parallel_bcast_int(domain_end(3))
         call parallel_bcast_int(sr_x)
         call parallel_bcast_int(sr_y)
   
         sp1 = my_minx
         ep1 = my_maxx - 1
         sp2 = my_miny
         ep2 = my_maxy - 1
         sp3 = domain_start(3)
         ep3 = domain_end(3)
   
         if (internal_gridtype == 'C') then
            if (my_x /= nproc_x - 1 .or. stagger == 'U' .or. sr_x > 1) then
               ep1 = ep1 + 1
            end if
            if (my_y /= nproc_y - 1 .or. stagger == 'V' .or. sr_y > 1) then
               ep2 = ep2 + 1
            end if
         else if (internal_gridtype == 'E') then
            ep1 = ep1 + 1
            ep2 = ep2 + 1
         end if
   
         if (sr_x > 1) then
            sp1 = (sp1-1)*sr_x+1
            ep1 =  ep1   *sr_x
         end if
         if (sr_y > 1) then
            sp2 = (sp2-1)*sr_y+1
            ep2 =  ep2   *sr_y
         end if

         sm1 = sp1
         em1 = ep1
         sm2 = sp2
         em2 = ep2
         sm3 = sp3
         em3 = ep3
   
         start_patch_i = sp1
         end_patch_i   = ep1
         start_patch_j = sp2
         end_patch_j   = ep2
         start_patch_k = sp3
         end_patch_k   = ep3
   
         allocate(real_array(sm1:em1,sm2:em2,sm3:em3))
         if (my_proc_id /= IO_NODE) then
            allocate(real_domain(1,1,1))
            domain_start(1) = 1
            domain_start(2) = 1
            domain_start(3) = 1
            domain_end(1) = 1
            domain_end(2) = 1
            domain_end(3) = 1
         end if
         call scatter_whole_field_r(real_array, &
                                   sm1, em1, sm2, em2, sm3, em3, &
                                   sp1, ep1, sp2, ep2, sp3, ep3, &
                                   real_domain, &
                                   domain_start(1), domain_end(1), &
                                   domain_start(2), domain_end(2), &
                                   domain_start(3), domain_end(3))
         deallocate(real_domain)

      else
  
         real_array => real_domain

      end if
 
   end subroutine read_next_field
 
   subroutine read_global_attrs(title, start_date, grid_type, dyn_opt,                                &
                                west_east_dim, south_north_dim, bottom_top_dim,                       &
                                we_patch_s, we_patch_e, we_patch_s_stag, we_patch_e_stag,             &
                                sn_patch_s, sn_patch_e, sn_patch_s_stag, sn_patch_e_stag,             &
                                map_proj, mminlu, num_land_cat, is_water, is_lake, is_ice, is_urban,  &
                                isoilwater, grid_id, parent_id, i_parent_start, j_parent_start,       &
                                i_parent_end, j_parent_end, dx, dy, cen_lat, moad_cen_lat, cen_lon,   &
                                stand_lon, truelat1, truelat2, pole_lat, pole_lon, parent_grid_ratio, &
                                corner_lats, corner_lons, sr_x, sr_y)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: dyn_opt, west_east_dim, south_north_dim, bottom_top_dim, map_proj,   &
                 is_water, is_lake, we_patch_s, we_patch_e, we_patch_s_stag, we_patch_e_stag,      &
                 sn_patch_s, sn_patch_e, sn_patch_s_stag, sn_patch_e_stag,                         &
                 is_ice, is_urban, isoilwater, grid_id, parent_id, i_parent_start, j_parent_start, &
                 i_parent_end, j_parent_end, parent_grid_ratio, sr_x, sr_y, num_land_cat
      real, intent(out) :: dx, dy, cen_lat, moad_cen_lat, cen_lon, stand_lon, truelat1, truelat2,  &
                 pole_lat, pole_lon
      real, dimension(16), intent(out) :: corner_lats, corner_lons
      character (len=128), intent(out) :: title, start_date, grid_type, mminlu
  
      ! Local variables
      integer :: istatus, i
      real :: wps_version
      character (len=128) :: cunits, cdesc, cstagger
      integer, dimension(3) :: sr
      type (q_data) :: qd
  
      if (my_proc_id == IO_NODE .or. do_tiled_input) then
  
         if (io_form_input == BINARY) then
            istatus = 0
            do while (istatus == 0) 
               cunits = ' '
               cdesc = ' '
               cstagger = ' '
               sr = 0
               call ext_int_get_var_ti_char(handle, 'units', 'VAR', cunits, istatus)
         
               if (istatus == 0) then
                  call ext_int_get_var_ti_char(handle, 'description', 'VAR', cdesc, istatus)
          
                  if (istatus == 0) then
                     call ext_int_get_var_ti_char(handle, 'stagger', 'VAR', cstagger, istatus)

                    if (istatus == 0) then
                    call ext_int_get_var_ti_integer(handle, 'sr_x', 'VAR', sr(1), 1, sr(3), istatus)

                        if (istatus == 0) then
                        call ext_int_get_var_ti_integer(handle, 'sr_y', 'VAR', sr(2), 1, sr(3), istatus)
         
                             qd%units = cunits
                             qd%description = cdesc
                             qd%stagger = cstagger
                             qd%sr_x = sr(1)
                             qd%sr_y = sr(2)
                             call q_insert(unit_desc, qd)
 
                        end if
                    end if
                  end if
               end if
            end do
         end if
     
         call ext_get_dom_ti_char          ('TITLE', title)
         if (index(title,'GEOGRID V3.6') /= 0) then
            wps_version = 3.6
         else if (index(title,'GEOGRID V3.5.1') /= 0) then
            wps_version = 3.51
         else if (index(title,'GEOGRID V3.5') /= 0) then
            wps_version = 3.5
         else if (index(title,'GEOGRID V3.4.1') /= 0) then
            wps_version = 3.41
         else if (index(title,'GEOGRID V3.4') /= 0) then
            wps_version = 3.4
         else if (index(title,'GEOGRID V3.3.1') /= 0) then
            wps_version = 3.31
         else if (index(title,'GEOGRID V3.3') /= 0) then
            wps_version = 3.3
         else if (index(title,'GEOGRID V3.2.1') /= 0) then
            wps_version = 3.21
         else if (index(title,'GEOGRID V3.2') /= 0) then
            wps_version = 3.2
         else if (index(title,'GEOGRID V3.1.1') /= 0) then
            wps_version = 3.11
         else if (index(title,'GEOGRID V3.1') /= 0) then
            wps_version = 3.1
         else if (index(title,'GEOGRID V3.0.1') /= 0) then
            wps_version = 3.01
         else
            wps_version = 3.0
         end if
         call mprintf(.true.,DEBUG,'Reading static data from WPS version %f', f1=wps_version)
         call ext_get_dom_ti_char          ('SIMULATION_START_DATE', start_date)
         call ext_get_dom_ti_integer_scalar('WEST-EAST_GRID_DIMENSION', west_east_dim)
         call ext_get_dom_ti_integer_scalar('SOUTH-NORTH_GRID_DIMENSION', south_north_dim)
         call ext_get_dom_ti_integer_scalar('BOTTOM-TOP_GRID_DIMENSION', bottom_top_dim)
         call ext_get_dom_ti_integer_scalar('WEST-EAST_PATCH_START_UNSTAG', we_patch_s)
         call ext_get_dom_ti_integer_scalar('WEST-EAST_PATCH_END_UNSTAG', we_patch_e)
         call ext_get_dom_ti_integer_scalar('WEST-EAST_PATCH_START_STAG', we_patch_s_stag)
         call ext_get_dom_ti_integer_scalar('WEST-EAST_PATCH_END_STAG', we_patch_e_stag)
         call ext_get_dom_ti_integer_scalar('SOUTH-NORTH_PATCH_START_UNSTAG', sn_patch_s)
         call ext_get_dom_ti_integer_scalar('SOUTH-NORTH_PATCH_END_UNSTAG', sn_patch_e)
         call ext_get_dom_ti_integer_scalar('SOUTH-NORTH_PATCH_START_STAG', sn_patch_s_stag)
         call ext_get_dom_ti_integer_scalar('SOUTH-NORTH_PATCH_END_STAG', sn_patch_e_stag)
         call ext_get_dom_ti_char          ('GRIDTYPE', grid_type)
         call ext_get_dom_ti_real_scalar   ('DX', dx)
         call ext_get_dom_ti_real_scalar   ('DY', dy)
         call ext_get_dom_ti_integer_scalar('DYN_OPT', dyn_opt)
         call ext_get_dom_ti_real_scalar   ('CEN_LAT', cen_lat)
         call ext_get_dom_ti_real_scalar   ('CEN_LON', cen_lon)
         call ext_get_dom_ti_real_scalar   ('TRUELAT1', truelat1)
         call ext_get_dom_ti_real_scalar   ('TRUELAT2', truelat2)
         call ext_get_dom_ti_real_scalar   ('MOAD_CEN_LAT', moad_cen_lat)
         call ext_get_dom_ti_real_scalar   ('STAND_LON', stand_lon)
         call ext_get_dom_ti_real_scalar   ('POLE_LAT', pole_lat)
         call ext_get_dom_ti_real_scalar   ('POLE_LON', pole_lon)
         call ext_get_dom_ti_real_vector   ('corner_lats', corner_lats, 16)
         call ext_get_dom_ti_real_vector   ('corner_lons', corner_lons, 16)
         call ext_get_dom_ti_integer_scalar('MAP_PROJ', map_proj)
         call ext_get_dom_ti_char          ('MMINLU', mminlu)
         if ( wps_version >= 3.01 ) then
            call ext_get_dom_ti_integer_scalar('NUM_LAND_CAT', num_land_cat)
         else
            num_land_cat = 24
         end if
         call ext_get_dom_ti_integer_scalar('ISWATER', is_water)
         if ( wps_version >= 3.01 ) then
            call ext_get_dom_ti_integer_scalar('ISLAKE', is_lake)
         else
            is_lake = -1
         end if
         call ext_get_dom_ti_integer_scalar('ISICE', is_ice)
         call ext_get_dom_ti_integer_scalar('ISURBAN', is_urban)
         call ext_get_dom_ti_integer_scalar('ISOILWATER', isoilwater)
         call ext_get_dom_ti_integer_scalar('grid_id', grid_id)
         call ext_get_dom_ti_integer_scalar('parent_id', parent_id)
         call ext_get_dom_ti_integer_scalar('i_parent_start', i_parent_start)
         call ext_get_dom_ti_integer_scalar('j_parent_start', j_parent_start)
         call ext_get_dom_ti_integer_scalar('i_parent_end', i_parent_end)
         call ext_get_dom_ti_integer_scalar('j_parent_end', j_parent_end)
         call ext_get_dom_ti_integer_scalar('parent_grid_ratio', parent_grid_ratio)
         call ext_get_dom_ti_integer_scalar('sr_x', sr_x)
         call ext_get_dom_ti_integer_scalar('sr_y', sr_y)
   
      end if

  
      if (nprocs > 1 .and. .not. do_tiled_input) then
  
         call parallel_bcast_char(title, len(title))
         call parallel_bcast_char(start_date, len(start_date))
         call parallel_bcast_char(grid_type, len(grid_type))
         call parallel_bcast_int(west_east_dim)
         call parallel_bcast_int(south_north_dim)
         call parallel_bcast_int(bottom_top_dim)
         call parallel_bcast_int(we_patch_s)
         call parallel_bcast_int(we_patch_e)
         call parallel_bcast_int(we_patch_s_stag)
         call parallel_bcast_int(we_patch_e_stag)
         call parallel_bcast_int(sn_patch_s)
         call parallel_bcast_int(sn_patch_e)
         call parallel_bcast_int(sn_patch_s_stag)
         call parallel_bcast_int(sn_patch_e_stag)
         call parallel_bcast_int(sr_x)
         call parallel_bcast_int(sr_y)

         ! Must figure out patch dimensions from info in parallel module
!         we_patch_s      = my_minx
!         we_patch_s_stag = my_minx
!         we_patch_e      = my_maxx - 1
!         sn_patch_s      = my_miny
!         sn_patch_s_stag = my_miny
!         sn_patch_e      = my_maxy - 1
!
!         if (trim(grid_type) == 'C') then
!            if (my_x /= nproc_x - 1) then
!               we_patch_e_stag = we_patch_e + 1
!            end if
!            if (my_y /= nproc_y - 1) then
!               sn_patch_e_stag = sn_patch_e + 1
!            end if
!         else if (trim(grid_type) == 'E') then
!            we_patch_e = we_patch_e + 1
!            sn_patch_e = sn_patch_e + 1
!            we_patch_e_stag = we_patch_e
!            sn_patch_e_stag = sn_patch_e
!         end if

         call parallel_bcast_real(dx)
         call parallel_bcast_real(dy)
         call parallel_bcast_int(dyn_opt)
         call parallel_bcast_real(cen_lat)
         call parallel_bcast_real(cen_lon)
         call parallel_bcast_real(truelat1)
         call parallel_bcast_real(truelat2)
         call parallel_bcast_real(pole_lat)
         call parallel_bcast_real(pole_lon)
         call parallel_bcast_real(moad_cen_lat)
         call parallel_bcast_real(stand_lon)
         do i=1,16
            call parallel_bcast_real(corner_lats(i))
            call parallel_bcast_real(corner_lons(i))
         end do
         call parallel_bcast_int(map_proj)
         call parallel_bcast_char(mminlu, len(mminlu))
         call parallel_bcast_int(is_water)
         call parallel_bcast_int(is_lake)
         call parallel_bcast_int(is_ice)
         call parallel_bcast_int(is_urban)
         call parallel_bcast_int(isoilwater)
         call parallel_bcast_int(grid_id)
         call parallel_bcast_int(parent_id)
         call parallel_bcast_int(i_parent_start)
         call parallel_bcast_int(i_parent_end)
         call parallel_bcast_int(j_parent_start)
         call parallel_bcast_int(j_parent_end)
         call parallel_bcast_int(parent_grid_ratio)
      end if
  
      internal_gridtype = grid_type
 
   end subroutine read_global_attrs


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: ext_get_dom_ti_integer
   !
   ! Purpose: Read a domain time-independent integer attribute from input.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine ext_get_dom_ti_integer_scalar(var_name, var_value, suppress_errors)

      implicit none

      ! Arguments
      integer, intent(out) :: var_value
      character (len=*), intent(in) :: var_name
      logical, intent(in), optional :: suppress_errors

      ! Local variables
      integer :: istatus, outcount

      if (io_form_input == BINARY) then
         call ext_int_get_dom_ti_integer(handle, trim(var_name), &
                                         var_value, &
                                         1, outcount, istatus)
      end if
      if (io_form_input == NETCDF) then
         call ext_ncd_get_dom_ti_integer(handle, trim(var_name), &
                                         var_value, &
                                         1, outcount, istatus)
      end if
      if (io_form_input == GRIB1) then
         call ext_gr1_get_dom_ti_integer(handle, trim(var_name), &
                                         var_value, &
                                         1, outcount, istatus)
      end if

      if (present(suppress_errors)) then
         call mprintf((istatus /= 0 .and. .not.suppress_errors),ERROR,'Error while reading domain time-independent attribute.')
      else
         call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      end if

   end subroutine ext_get_dom_ti_integer_scalar


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: ext_get_dom_ti_integer
   !
   ! Purpose: Read a domain time-independent integer attribute from input.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine ext_get_dom_ti_integer_vector(var_name, var_value, n)

      implicit none

      ! Arguments
      integer, intent(in) :: n
      integer, dimension(n), intent(out) :: var_value
      character (len=*), intent(in) :: var_name

      ! Local variables
      integer :: istatus, outcount

      if (io_form_input == BINARY) then
         call ext_int_get_dom_ti_integer(handle, trim(var_name), &
                                         var_value, &
                                         n, outcount, istatus)
      end if
      if (io_form_input == NETCDF) then
         call ext_ncd_get_dom_ti_integer(handle, trim(var_name), &
                                         var_value, &
                                         n, outcount, istatus)
      end if
      if (io_form_input == GRIB1) then
         call ext_gr1_get_dom_ti_integer(handle, trim(var_name), &
                                         var_value, &
                                         n, outcount, istatus)
      end if

      call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')

   end subroutine ext_get_dom_ti_integer_vector


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: ext_get_dom_ti_real
   !
   ! Purpose: Read a domain time-independent real attribute from input.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine ext_get_dom_ti_real_scalar(var_name, var_value)

      implicit none

      ! Arguments
      real, intent(out) :: var_value
      character (len=*), intent(in) :: var_name

      ! Local variables
      integer :: istatus, outcount

      if (io_form_input == BINARY) then
         call ext_int_get_dom_ti_real(handle, trim(var_name), &
                                         var_value, &
                                         1, outcount, istatus)
      end if
      if (io_form_input == NETCDF) then
         call ext_ncd_get_dom_ti_real(handle, trim(var_name), &
                                         var_value, &
                                         1, outcount, istatus)
      end if
      if (io_form_input == GRIB1) then
         call ext_gr1_get_dom_ti_real(handle, trim(var_name), &
                                         var_value, &
                                         1, outcount, istatus)
      end if

      call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')

   end subroutine ext_get_dom_ti_real_scalar


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: ext_get_dom_ti_real
   !
   ! Purpose: Read a domain time-independent real attribute from input.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine ext_get_dom_ti_real_vector(var_name, var_value, n)

      implicit none

      ! Arguments
      integer, intent(in) :: n
      real, dimension(n), intent(out) :: var_value
      character (len=*), intent(in) :: var_name

      ! Local variables
      integer :: istatus, outcount

      if (io_form_input == BINARY) then
         call ext_int_get_dom_ti_real(handle, trim(var_name), &
                                         var_value, &
                                         n, outcount, istatus)
      end if
      if (io_form_input == NETCDF) then
         call ext_ncd_get_dom_ti_real(handle, trim(var_name), &
                                         var_value, &
                                         n, outcount, istatus)
      end if
      if (io_form_input == GRIB1) then
         call ext_gr1_get_dom_ti_real(handle, trim(var_name), &
                                         var_value, &
                                         n, outcount, istatus)
      end if

      call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')

   end subroutine ext_get_dom_ti_real_vector


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: ext_get_dom_ti_char
   !
   ! Purpose: Read a domain time-independent character attribute from input.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine ext_get_dom_ti_char(var_name, var_value)

      implicit none

      ! Arguments
      character (len=*), intent(in) :: var_name
      character (len=128), intent(out) :: var_value

      ! Local variables
      integer :: istatus

      if (io_form_input == BINARY) then
         call ext_int_get_dom_ti_char(handle, trim(var_name), &
                                         var_value, &
                                         istatus)
      end if
      if (io_form_input == NETCDF) then
         call ext_ncd_get_dom_ti_char(handle, trim(var_name), &
                                         var_value, &
                                         istatus)
      end if
      if (io_form_input == GRIB1) then
         call ext_gr1_get_dom_ti_char(handle, trim(var_name), &
                                         var_value, &
                                         istatus)
      end if

      call mprintf((istatus /= 0),ERROR,'Error in reading domain time-independent attribute')

   end subroutine ext_get_dom_ti_char

 
   subroutine input_close()
 
      implicit none
  
      ! Local variables
      integer :: istatus
  
      istatus = 0
      if (my_proc_id == IO_NODE .or. do_tiled_input) then
         if (io_form_input == BINARY) then
            call ext_int_ioclose(handle, istatus)
            call ext_int_ioexit(istatus)
         end if
         if (io_form_input == NETCDF) then
            call ext_ncd_ioclose(handle, istatus)
            call ext_ncd_ioexit(istatus)
         end if
         if (io_form_input == GRIB1) then
            call ext_gr1_ioclose(handle, istatus)
            call ext_gr1_ioexit(istatus)
         end if
      end if
 
      call q_destroy(unit_desc)
 
   end subroutine input_close
 
end module input_module
