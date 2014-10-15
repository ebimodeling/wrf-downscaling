



MODULE module_io























  USE module_configure

  LOGICAL :: is_inited = .FALSE.
  INTEGER, PARAMETER, PRIVATE :: MAX_WRF_IO_HANDLE = 1000
  INTEGER :: wrf_io_handles(MAX_WRF_IO_HANDLE), how_opened(MAX_WRF_IO_HANDLE) 
  LOGICAL :: for_output(MAX_WRF_IO_HANDLE), first_operation(MAX_WRF_IO_HANDLE)
  INTEGER :: filtno = 0
  LOGICAL, PRIVATE :: bdy_dist_flag = .TRUE.   
                                                
                                                
                                                
  CHARACTER*256 extradims


























INTERFACE wrf_get_dom_ti_real
  MODULE PROCEDURE wrf_get_dom_ti_real_arr, wrf_get_dom_ti_real_sca
END INTERFACE


INTERFACE wrf_put_dom_ti_real
  MODULE PROCEDURE wrf_put_dom_ti_real_arr, wrf_put_dom_ti_real_sca
END INTERFACE


INTERFACE wrf_get_dom_ti_double
  MODULE PROCEDURE wrf_get_dom_ti_double_arr, wrf_get_dom_ti_double_sca
END INTERFACE


INTERFACE wrf_put_dom_ti_double
  MODULE PROCEDURE wrf_put_dom_ti_double_arr, wrf_put_dom_ti_double_sca
END INTERFACE


INTERFACE wrf_get_dom_ti_integer
  MODULE PROCEDURE wrf_get_dom_ti_integer_arr, wrf_get_dom_ti_integer_sca
END INTERFACE


INTERFACE wrf_put_dom_ti_integer
  MODULE PROCEDURE wrf_put_dom_ti_integer_arr, wrf_put_dom_ti_integer_sca
END INTERFACE


INTERFACE wrf_get_dom_ti_logical
  MODULE PROCEDURE wrf_get_dom_ti_logical_arr, wrf_get_dom_ti_logical_sca
END INTERFACE


INTERFACE wrf_put_dom_ti_logical
  MODULE PROCEDURE wrf_put_dom_ti_logical_arr, wrf_put_dom_ti_logical_sca
END INTERFACE


INTERFACE wrf_get_dom_ti_char
  MODULE PROCEDURE wrf_get_dom_ti_char_arr
END INTERFACE


INTERFACE wrf_put_dom_ti_char
  MODULE PROCEDURE wrf_put_dom_ti_char_arr
END INTERFACE



INTERFACE wrf_get_dom_td_real
  MODULE PROCEDURE wrf_get_dom_td_real_arr, wrf_get_dom_td_real_sca
END INTERFACE


INTERFACE wrf_put_dom_td_real
  MODULE PROCEDURE wrf_put_dom_td_real_arr, wrf_put_dom_td_real_sca
END INTERFACE


INTERFACE wrf_get_dom_td_double
  MODULE PROCEDURE wrf_get_dom_td_double_arr, wrf_get_dom_td_double_sca
END INTERFACE


INTERFACE wrf_put_dom_td_double
  MODULE PROCEDURE wrf_put_dom_td_double_arr, wrf_put_dom_td_double_sca
END INTERFACE


INTERFACE wrf_get_dom_td_integer
  MODULE PROCEDURE wrf_get_dom_td_integer_arr, wrf_get_dom_td_integer_sca
END INTERFACE


INTERFACE wrf_put_dom_td_integer
  MODULE PROCEDURE wrf_put_dom_td_integer_arr, wrf_put_dom_td_integer_sca
END INTERFACE


INTERFACE wrf_get_dom_td_logical
  MODULE PROCEDURE wrf_get_dom_td_logical_arr, wrf_get_dom_td_logical_sca
END INTERFACE


INTERFACE wrf_put_dom_td_logical
  MODULE PROCEDURE wrf_put_dom_td_logical_arr, wrf_put_dom_td_logical_sca
END INTERFACE


INTERFACE wrf_get_dom_td_char
  MODULE PROCEDURE wrf_get_dom_td_char_arr
END INTERFACE


INTERFACE wrf_put_dom_td_char
  MODULE PROCEDURE wrf_put_dom_td_char_arr
END INTERFACE



INTERFACE wrf_get_var_ti_real
  MODULE PROCEDURE wrf_get_var_ti_real_arr, wrf_get_var_ti_real_sca
END INTERFACE


INTERFACE wrf_put_var_ti_real
  MODULE PROCEDURE wrf_put_var_ti_real_arr, wrf_put_var_ti_real_sca
END INTERFACE


INTERFACE wrf_get_var_ti_double
  MODULE PROCEDURE wrf_get_var_ti_double_arr, wrf_get_var_ti_double_sca
END INTERFACE


INTERFACE wrf_put_var_ti_double
  MODULE PROCEDURE wrf_put_var_ti_double_arr, wrf_put_var_ti_double_sca
END INTERFACE


INTERFACE wrf_get_var_ti_integer
  MODULE PROCEDURE wrf_get_var_ti_integer_arr, wrf_get_var_ti_integer_sca
END INTERFACE


INTERFACE wrf_put_var_ti_integer
  MODULE PROCEDURE wrf_put_var_ti_integer_arr, wrf_put_var_ti_integer_sca
END INTERFACE


INTERFACE wrf_get_var_ti_logical
  MODULE PROCEDURE wrf_get_var_ti_logical_arr, wrf_get_var_ti_logical_sca
END INTERFACE


INTERFACE wrf_put_var_ti_logical
  MODULE PROCEDURE wrf_put_var_ti_logical_arr, wrf_put_var_ti_logical_sca
END INTERFACE


INTERFACE wrf_get_var_ti_char
  MODULE PROCEDURE wrf_get_var_ti_char_arr
END INTERFACE


INTERFACE wrf_put_var_ti_char
  MODULE PROCEDURE wrf_put_var_ti_char_arr
END INTERFACE



INTERFACE wrf_get_var_td_real
  MODULE PROCEDURE wrf_get_var_td_real_arr, wrf_get_var_td_real_sca
END INTERFACE


INTERFACE wrf_put_var_td_real
  MODULE PROCEDURE wrf_put_var_td_real_arr, wrf_put_var_td_real_sca
END INTERFACE


INTERFACE wrf_get_var_td_double
  MODULE PROCEDURE wrf_get_var_td_double_arr, wrf_get_var_td_double_sca
END INTERFACE


INTERFACE wrf_put_var_td_double
  MODULE PROCEDURE wrf_put_var_td_double_arr, wrf_put_var_td_double_sca
END INTERFACE


INTERFACE wrf_get_var_td_integer
  MODULE PROCEDURE wrf_get_var_td_integer_arr, wrf_get_var_td_integer_sca
END INTERFACE


INTERFACE wrf_put_var_td_integer
  MODULE PROCEDURE wrf_put_var_td_integer_arr, wrf_put_var_td_integer_sca
END INTERFACE


INTERFACE wrf_get_var_td_logical
  MODULE PROCEDURE wrf_get_var_td_logical_arr, wrf_get_var_td_logical_sca
END INTERFACE


INTERFACE wrf_put_var_td_logical
  MODULE PROCEDURE wrf_put_var_td_logical_arr, wrf_put_var_td_logical_sca
END INTERFACE


INTERFACE wrf_get_var_td_char
  MODULE PROCEDURE wrf_get_var_td_char_arr
END INTERFACE


INTERFACE wrf_put_var_td_char
  MODULE PROCEDURE wrf_put_var_td_char_arr
END INTERFACE



CONTAINS



SUBROUTINE wrf_get_dom_ti_real_arr ( DataHandle,Element,   Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

 

 real  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_dom_ti_real_arr " )


locCount = Count


Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_dom_ti_real ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_dom_ti_real ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_dom_ti_real ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_dom_ti_real ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_dom_ti_real ( Hndl, Element,   Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_dom_ti_real_arr 


SUBROUTINE wrf_get_dom_ti_real_sca ( DataHandle,Element,   Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

 

 real  :: Data 

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_dom_ti_real_sca " )


locCount = Count


Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_dom_ti_real ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_dom_ti_real ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_dom_ti_real ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_dom_ti_real ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_dom_ti_real ( Hndl, Element,   Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_dom_ti_real_sca 




SUBROUTINE wrf_put_dom_ti_real_arr ( DataHandle,Element,   Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

 

 real  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_dom_ti_real_arr " )


locCount = Count


Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_dom_ti_real ( Hndl, Element,   Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_dom_ti_real ( Hndl, Element,   Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_dom_ti_real ( Hndl, Element,   Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_dom_ti_real ( Hndl, Element,   Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_dom_ti_real ( Hndl, Element,   Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_dom_ti_real_arr 


SUBROUTINE wrf_put_dom_ti_real_sca ( DataHandle,Element,   Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

 

 real  :: Data 

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_dom_ti_real_sca " )


locCount = Count


Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_dom_ti_real ( Hndl, Element,   Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_dom_ti_real ( Hndl, Element,   Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_dom_ti_real ( Hndl, Element,   Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_dom_ti_real ( Hndl, Element,   Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_dom_ti_real ( Hndl, Element,   Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_dom_ti_real_sca 




SUBROUTINE wrf_get_dom_ti_double_arr ( DataHandle,Element,   Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

 

 real*8  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_dom_ti_double_arr " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_dom_ti_double ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_dom_ti_double ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_dom_ti_double ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_dom_ti_double ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_dom_ti_double ( Hndl, Element,   Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_dom_ti_double_arr 


SUBROUTINE wrf_get_dom_ti_double_sca ( DataHandle,Element,   Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

 

 real*8  :: Data 

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_dom_ti_double_sca " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_dom_ti_double ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_dom_ti_double ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_dom_ti_double ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_dom_ti_double ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_dom_ti_double ( Hndl, Element,   Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_dom_ti_double_sca 




SUBROUTINE wrf_put_dom_ti_double_arr ( DataHandle,Element,   Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

 

 real*8  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_dom_ti_double_arr " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_dom_ti_double ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_dom_ti_double ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_dom_ti_double ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_dom_ti_double ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_dom_ti_double ( Hndl, Element,   Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_dom_ti_double_arr 


SUBROUTINE wrf_put_dom_ti_double_sca ( DataHandle,Element,   Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

 

 real*8  :: Data 

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_dom_ti_double_sca " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_dom_ti_double ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_dom_ti_double ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_dom_ti_double ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_dom_ti_double ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_dom_ti_double ( Hndl, Element,   Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_dom_ti_double_sca 




SUBROUTINE wrf_get_dom_ti_integer_arr ( DataHandle,Element,   Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

 

 integer  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_dom_ti_integer_arr " )

locCount = Count



Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_dom_ti_integer ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_dom_ti_integer ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_dom_ti_integer ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_dom_ti_integer ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_dom_ti_integer ( Hndl, Element,   Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_dom_ti_integer_arr 


SUBROUTINE wrf_get_dom_ti_integer_sca ( DataHandle,Element,   Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

 

 integer  :: Data 

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_dom_ti_integer_sca " )

locCount = Count



Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_dom_ti_integer ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_dom_ti_integer ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_dom_ti_integer ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_dom_ti_integer ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_dom_ti_integer ( Hndl, Element,   Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_dom_ti_integer_sca 




SUBROUTINE wrf_put_dom_ti_integer_arr ( DataHandle,Element,   Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

 

 integer  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_dom_ti_integer_arr " )

locCount = Count



Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_dom_ti_integer ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_dom_ti_integer ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_dom_ti_integer ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_dom_ti_integer ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_dom_ti_integer ( Hndl, Element,   Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_dom_ti_integer_arr 


SUBROUTINE wrf_put_dom_ti_integer_sca ( DataHandle,Element,   Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

 

 integer  :: Data 

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_dom_ti_integer_sca " )

locCount = Count



Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_dom_ti_integer ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_dom_ti_integer ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_dom_ti_integer ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_dom_ti_integer ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_dom_ti_integer ( Hndl, Element,   Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_dom_ti_integer_sca 




SUBROUTINE wrf_get_dom_ti_logical_arr ( DataHandle,Element,   Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

 

 logical  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_dom_ti_logical_arr " )



locCount = Count

Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_dom_ti_logical ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_dom_ti_logical ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_dom_ti_logical ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_dom_ti_logical ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_dom_ti_logical ( Hndl, Element,   Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_dom_ti_logical_arr 


SUBROUTINE wrf_get_dom_ti_logical_sca ( DataHandle,Element,   Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

 

 logical  :: Data 

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_dom_ti_logical_sca " )



locCount = Count

Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_dom_ti_logical ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_dom_ti_logical ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_dom_ti_logical ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_dom_ti_logical ( Hndl, Element,   Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_dom_ti_logical ( Hndl, Element,   Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_dom_ti_logical_sca 




SUBROUTINE wrf_put_dom_ti_logical_arr ( DataHandle,Element,   Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

 

 logical  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_dom_ti_logical_arr " )



locCount = Count

Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_dom_ti_logical ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_dom_ti_logical ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_dom_ti_logical ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_dom_ti_logical ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_dom_ti_logical ( Hndl, Element,   Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_dom_ti_logical_arr 


SUBROUTINE wrf_put_dom_ti_logical_sca ( DataHandle,Element,   Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

 

 logical  :: Data 

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_dom_ti_logical_sca " )



locCount = Count

Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_dom_ti_logical ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_dom_ti_logical ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_dom_ti_logical ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_dom_ti_logical ( Hndl, Element,   Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_dom_ti_logical ( Hndl, Element,   Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_dom_ti_logical_sca 




SUBROUTINE wrf_get_dom_ti_char_arr ( DataHandle,Element,   Data,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

 

 CHARACTER*(*)  :: Data



INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_dom_ti_char_arr " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_dom_ti_char ( Hndl, Element,   Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          len_of_str = LEN(Data)
          CALL wrf_dm_bcast_string( Data, len_of_str )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_dom_ti_char ( Hndl, Element,   Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          len_of_str = LEN(Data)
          CALL wrf_dm_bcast_string( Data, len_of_str )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_dom_ti_char ( Hndl, Element,   Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          len_of_str = LEN(Data)
          CALL wrf_dm_bcast_string( Data, len_of_str )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_dom_ti_char ( Hndl, Element,   Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           len_of_str = LEN(Data)
           CALL wrf_dm_bcast_string( Data, len_of_str )
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_dom_ti_char ( Hndl, Element,   Data, &
                           Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_dom_ti_char_arr 




SUBROUTINE wrf_put_dom_ti_char_arr ( DataHandle,Element,   Data,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

 

 CHARACTER*(*)  :: Data



INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_dom_ti_char_arr " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_dom_ti_char ( Hndl, Element,   Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_dom_ti_char ( Hndl, Element,   Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_dom_ti_char ( Hndl, Element,   Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_dom_ti_char ( Hndl, Element,   Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_dom_ti_char ( Hndl, Element,   Data, &
                           Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_dom_ti_char_arr 





SUBROUTINE wrf_get_dom_td_real_arr ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
 

 real  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_dom_td_real_arr " )


locCount = Count


Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_dom_td_real ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_dom_td_real ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_dom_td_real ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_dom_td_real ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_dom_td_real ( Hndl, Element, DateStr,  Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_dom_td_real_arr 


SUBROUTINE wrf_get_dom_td_real_sca ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
 

 real  :: Data 

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_dom_td_real_sca " )


locCount = Count


Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_dom_td_real ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_dom_td_real ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_dom_td_real ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_dom_td_real ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_dom_td_real ( Hndl, Element, DateStr,  Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_dom_td_real_sca 




SUBROUTINE wrf_put_dom_td_real_arr ( DataHandle,Element, DateStr,  Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
 

 real  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_dom_td_real_arr " )


locCount = Count


Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_dom_td_real ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_dom_td_real ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_dom_td_real ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_dom_td_real ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_dom_td_real ( Hndl, Element, DateStr,  Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_dom_td_real_arr 


SUBROUTINE wrf_put_dom_td_real_sca ( DataHandle,Element, DateStr,  Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
 

 real  :: Data 

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_dom_td_real_sca " )


locCount = Count


Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_dom_td_real ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_dom_td_real ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_dom_td_real ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_dom_td_real ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_dom_td_real ( Hndl, Element, DateStr,  Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_dom_td_real_sca 




SUBROUTINE wrf_get_dom_td_double_arr ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
 

 real*8  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_dom_td_double_arr " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_dom_td_double ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_dom_td_double ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_dom_td_double ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_dom_td_double ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_dom_td_double ( Hndl, Element, DateStr,  Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_dom_td_double_arr 


SUBROUTINE wrf_get_dom_td_double_sca ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
 

 real*8  :: Data 

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_dom_td_double_sca " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_dom_td_double ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_dom_td_double ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_dom_td_double ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_dom_td_double ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_dom_td_double ( Hndl, Element, DateStr,  Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_dom_td_double_sca 




SUBROUTINE wrf_put_dom_td_double_arr ( DataHandle,Element, DateStr,  Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
 

 real*8  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_dom_td_double_arr " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_dom_td_double ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_dom_td_double ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_dom_td_double ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_dom_td_double ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_dom_td_double ( Hndl, Element, DateStr,  Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_dom_td_double_arr 


SUBROUTINE wrf_put_dom_td_double_sca ( DataHandle,Element, DateStr,  Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
 

 real*8  :: Data 

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_dom_td_double_sca " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_dom_td_double ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_dom_td_double ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_dom_td_double ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_dom_td_double ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_dom_td_double ( Hndl, Element, DateStr,  Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_dom_td_double_sca 




SUBROUTINE wrf_get_dom_td_integer_arr ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
 

 integer  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_dom_td_integer_arr " )

locCount = Count



Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_dom_td_integer ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_dom_td_integer ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_dom_td_integer ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_dom_td_integer ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_dom_td_integer ( Hndl, Element, DateStr,  Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_dom_td_integer_arr 


SUBROUTINE wrf_get_dom_td_integer_sca ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
 

 integer  :: Data 

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_dom_td_integer_sca " )

locCount = Count



Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_dom_td_integer ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_dom_td_integer ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_dom_td_integer ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_dom_td_integer ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_dom_td_integer ( Hndl, Element, DateStr,  Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_dom_td_integer_sca 




SUBROUTINE wrf_put_dom_td_integer_arr ( DataHandle,Element, DateStr,  Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
 

 integer  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_dom_td_integer_arr " )

locCount = Count



Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_dom_td_integer ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_dom_td_integer ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_dom_td_integer ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_dom_td_integer ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_dom_td_integer ( Hndl, Element, DateStr,  Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_dom_td_integer_arr 


SUBROUTINE wrf_put_dom_td_integer_sca ( DataHandle,Element, DateStr,  Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
 

 integer  :: Data 

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_dom_td_integer_sca " )

locCount = Count



Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_dom_td_integer ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_dom_td_integer ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_dom_td_integer ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_dom_td_integer ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_dom_td_integer ( Hndl, Element, DateStr,  Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_dom_td_integer_sca 




SUBROUTINE wrf_get_dom_td_logical_arr ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
 

 logical  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_dom_td_logical_arr " )



locCount = Count

Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_dom_td_logical ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_dom_td_logical ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_dom_td_logical ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_dom_td_logical ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_dom_td_logical ( Hndl, Element, DateStr,  Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_dom_td_logical_arr 


SUBROUTINE wrf_get_dom_td_logical_sca ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
 

 logical  :: Data 

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_dom_td_logical_sca " )



locCount = Count

Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_dom_td_logical ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_dom_td_logical ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_dom_td_logical ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_dom_td_logical ( Hndl, Element, DateStr,  Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_dom_td_logical ( Hndl, Element, DateStr,  Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_dom_td_logical_sca 




SUBROUTINE wrf_put_dom_td_logical_arr ( DataHandle,Element, DateStr,  Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
 

 logical  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_dom_td_logical_arr " )



locCount = Count

Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_dom_td_logical ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_dom_td_logical ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_dom_td_logical ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_dom_td_logical ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_dom_td_logical ( Hndl, Element, DateStr,  Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_dom_td_logical_arr 


SUBROUTINE wrf_put_dom_td_logical_sca ( DataHandle,Element, DateStr,  Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
 

 logical  :: Data 

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_dom_td_logical_sca " )



locCount = Count

Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_dom_td_logical ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_dom_td_logical ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_dom_td_logical ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_dom_td_logical ( Hndl, Element, DateStr,  Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_dom_td_logical ( Hndl, Element, DateStr,  Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_dom_td_logical_sca 




SUBROUTINE wrf_get_dom_td_char_arr ( DataHandle,Element, DateStr,  Data,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
 

 CHARACTER*(*)  :: Data



INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_dom_td_char_arr " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_dom_td_char ( Hndl, Element, DateStr,  Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          len_of_str = LEN(Data)
          CALL wrf_dm_bcast_string( Data, len_of_str )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_dom_td_char ( Hndl, Element, DateStr,  Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          len_of_str = LEN(Data)
          CALL wrf_dm_bcast_string( Data, len_of_str )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_dom_td_char ( Hndl, Element, DateStr,  Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          len_of_str = LEN(Data)
          CALL wrf_dm_bcast_string( Data, len_of_str )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_dom_td_char ( Hndl, Element, DateStr,  Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           len_of_str = LEN(Data)
           CALL wrf_dm_bcast_string( Data, len_of_str )
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_dom_td_char ( Hndl, Element, DateStr,  Data, &
                           Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_dom_td_char_arr 




SUBROUTINE wrf_put_dom_td_char_arr ( DataHandle,Element, DateStr,  Data,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
 

 CHARACTER*(*)  :: Data



INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_dom_td_char_arr " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_dom_td_char ( Hndl, Element, DateStr,  Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_dom_td_char ( Hndl, Element, DateStr,  Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_dom_td_char ( Hndl, Element, DateStr,  Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_dom_td_char ( Hndl, Element, DateStr,  Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_dom_td_char ( Hndl, Element, DateStr,  Data, &
                           Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_dom_td_char_arr 





SUBROUTINE wrf_get_var_ti_real_arr ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

CHARACTER*(*) , INTENT(IN)  :: VarName 

 real  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_var_ti_real_arr " )


locCount = Count


Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_var_ti_real ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_var_ti_real ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_var_ti_real ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_var_ti_real ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_var_ti_real ( Hndl, Element,  Varname, Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_var_ti_real_arr 


SUBROUTINE wrf_get_var_ti_real_sca ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

CHARACTER*(*) , INTENT(IN)  :: VarName 

 real  :: Data 

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_var_ti_real_sca " )


locCount = Count


Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_var_ti_real ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_var_ti_real ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_var_ti_real ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_var_ti_real ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_var_ti_real ( Hndl, Element,  Varname, Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_var_ti_real_sca 




SUBROUTINE wrf_put_var_ti_real_arr ( DataHandle,Element,  Varname, Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

CHARACTER*(*) , INTENT(IN)  :: VarName 

 real  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_var_ti_real_arr " )


locCount = Count


Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_var_ti_real ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_var_ti_real ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_var_ti_real ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_var_ti_real ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_var_ti_real ( Hndl, Element,  Varname, Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_var_ti_real_arr 


SUBROUTINE wrf_put_var_ti_real_sca ( DataHandle,Element,  Varname, Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

CHARACTER*(*) , INTENT(IN)  :: VarName 

 real  :: Data 

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_var_ti_real_sca " )


locCount = Count


Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_var_ti_real ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_var_ti_real ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_var_ti_real ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_var_ti_real ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_var_ti_real ( Hndl, Element,  Varname, Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_var_ti_real_sca 




SUBROUTINE wrf_get_var_ti_double_arr ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

CHARACTER*(*) , INTENT(IN)  :: VarName 

 real*8  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_var_ti_double_arr " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_var_ti_double ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_var_ti_double ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_var_ti_double ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_var_ti_double ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_var_ti_double ( Hndl, Element,  Varname, Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_var_ti_double_arr 


SUBROUTINE wrf_get_var_ti_double_sca ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

CHARACTER*(*) , INTENT(IN)  :: VarName 

 real*8  :: Data 

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_var_ti_double_sca " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_var_ti_double ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_var_ti_double ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_var_ti_double ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_var_ti_double ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_var_ti_double ( Hndl, Element,  Varname, Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_var_ti_double_sca 




SUBROUTINE wrf_put_var_ti_double_arr ( DataHandle,Element,  Varname, Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

CHARACTER*(*) , INTENT(IN)  :: VarName 

 real*8  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_var_ti_double_arr " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_var_ti_double ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_var_ti_double ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_var_ti_double ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_var_ti_double ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_var_ti_double ( Hndl, Element,  Varname, Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_var_ti_double_arr 


SUBROUTINE wrf_put_var_ti_double_sca ( DataHandle,Element,  Varname, Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

CHARACTER*(*) , INTENT(IN)  :: VarName 

 real*8  :: Data 

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_var_ti_double_sca " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_var_ti_double ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_var_ti_double ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_var_ti_double ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_var_ti_double ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_var_ti_double ( Hndl, Element,  Varname, Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_var_ti_double_sca 




SUBROUTINE wrf_get_var_ti_integer_arr ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

CHARACTER*(*) , INTENT(IN)  :: VarName 

 integer  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_var_ti_integer_arr " )

locCount = Count



Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_var_ti_integer ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_var_ti_integer ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_var_ti_integer ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_var_ti_integer ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_var_ti_integer ( Hndl, Element,  Varname, Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_var_ti_integer_arr 


SUBROUTINE wrf_get_var_ti_integer_sca ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

CHARACTER*(*) , INTENT(IN)  :: VarName 

 integer  :: Data 

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_var_ti_integer_sca " )

locCount = Count



Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_var_ti_integer ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_var_ti_integer ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_var_ti_integer ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_var_ti_integer ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_var_ti_integer ( Hndl, Element,  Varname, Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_var_ti_integer_sca 




SUBROUTINE wrf_put_var_ti_integer_arr ( DataHandle,Element,  Varname, Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

CHARACTER*(*) , INTENT(IN)  :: VarName 

 integer  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_var_ti_integer_arr " )

locCount = Count



Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_var_ti_integer ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_var_ti_integer ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_var_ti_integer ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_var_ti_integer ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_var_ti_integer ( Hndl, Element,  Varname, Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_var_ti_integer_arr 


SUBROUTINE wrf_put_var_ti_integer_sca ( DataHandle,Element,  Varname, Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

CHARACTER*(*) , INTENT(IN)  :: VarName 

 integer  :: Data 

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_var_ti_integer_sca " )

locCount = Count



Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_var_ti_integer ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_var_ti_integer ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_var_ti_integer ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_var_ti_integer ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_var_ti_integer ( Hndl, Element,  Varname, Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_var_ti_integer_sca 




SUBROUTINE wrf_get_var_ti_logical_arr ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

CHARACTER*(*) , INTENT(IN)  :: VarName 

 logical  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_var_ti_logical_arr " )



locCount = Count

Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_var_ti_logical ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_var_ti_logical ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_var_ti_logical ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_var_ti_logical ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_var_ti_logical ( Hndl, Element,  Varname, Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_var_ti_logical_arr 


SUBROUTINE wrf_get_var_ti_logical_sca ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

CHARACTER*(*) , INTENT(IN)  :: VarName 

 logical  :: Data 

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_var_ti_logical_sca " )



locCount = Count

Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_var_ti_logical ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_var_ti_logical ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_var_ti_logical ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_var_ti_logical ( Hndl, Element,  Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_var_ti_logical ( Hndl, Element,  Varname, Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_var_ti_logical_sca 




SUBROUTINE wrf_put_var_ti_logical_arr ( DataHandle,Element,  Varname, Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

CHARACTER*(*) , INTENT(IN)  :: VarName 

 logical  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_var_ti_logical_arr " )



locCount = Count

Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_var_ti_logical ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_var_ti_logical ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_var_ti_logical ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_var_ti_logical ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_var_ti_logical ( Hndl, Element,  Varname, Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_var_ti_logical_arr 


SUBROUTINE wrf_put_var_ti_logical_sca ( DataHandle,Element,  Varname, Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

CHARACTER*(*) , INTENT(IN)  :: VarName 

 logical  :: Data 

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_var_ti_logical_sca " )



locCount = Count

Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_var_ti_logical ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_var_ti_logical ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_var_ti_logical ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_var_ti_logical ( Hndl, Element,  Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_var_ti_logical ( Hndl, Element,  Varname, Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_var_ti_logical_sca 




SUBROUTINE wrf_get_var_ti_char_arr ( DataHandle,Element,  Varname, Data,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

CHARACTER*(*) , INTENT(IN)  :: VarName 

 CHARACTER*(*)  :: Data



INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_var_ti_char_arr " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_var_ti_char ( Hndl, Element,  Varname, Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          len_of_str = LEN(Data)
          CALL wrf_dm_bcast_string( Data, len_of_str )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_var_ti_char ( Hndl, Element,  Varname, Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          len_of_str = LEN(Data)
          CALL wrf_dm_bcast_string( Data, len_of_str )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_var_ti_char ( Hndl, Element,  Varname, Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          len_of_str = LEN(Data)
          CALL wrf_dm_bcast_string( Data, len_of_str )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_var_ti_char ( Hndl, Element,  Varname, Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           len_of_str = LEN(Data)
           CALL wrf_dm_bcast_string( Data, len_of_str )
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_var_ti_char ( Hndl, Element,  Varname, Data, &
                           Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_var_ti_char_arr 




SUBROUTINE wrf_put_var_ti_char_arr ( DataHandle,Element,  Varname, Data,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element

CHARACTER*(*) , INTENT(IN)  :: VarName 

 CHARACTER*(*)  :: Data



INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_var_ti_char_arr " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_var_ti_char ( Hndl, Element,  Varname, Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_var_ti_char ( Hndl, Element,  Varname, Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_var_ti_char ( Hndl, Element,  Varname, Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_var_ti_char ( Hndl, Element,  Varname, Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_var_ti_char ( Hndl, Element,  Varname, Data, &
                           Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_var_ti_char_arr 





SUBROUTINE wrf_get_var_td_real_arr ( DataHandle,Element, DateStr, Varname, Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
CHARACTER*(*) , INTENT(IN)  :: VarName 

 real  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_var_td_real_arr " )


locCount = Count


Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_var_td_real ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_var_td_real ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_var_td_real ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_var_td_real ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_var_td_real ( Hndl, Element, DateStr, Varname, Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_var_td_real_arr 


SUBROUTINE wrf_get_var_td_real_sca ( DataHandle,Element, DateStr, Varname, Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
CHARACTER*(*) , INTENT(IN)  :: VarName 

 real  :: Data 

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_var_td_real_sca " )


locCount = Count


Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_var_td_real ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_var_td_real ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_var_td_real ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_var_td_real ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_var_td_real ( Hndl, Element, DateStr, Varname, Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_var_td_real_sca 




SUBROUTINE wrf_put_var_td_real_arr ( DataHandle,Element, DateStr, Varname, Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
CHARACTER*(*) , INTENT(IN)  :: VarName 

 real  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_var_td_real_arr " )


locCount = Count


Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_var_td_real ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_var_td_real ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_var_td_real ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_var_td_real ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_var_td_real ( Hndl, Element, DateStr, Varname, Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_var_td_real_arr 


SUBROUTINE wrf_put_var_td_real_sca ( DataHandle,Element, DateStr, Varname, Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
CHARACTER*(*) , INTENT(IN)  :: VarName 

 real  :: Data 

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_var_td_real_sca " )


locCount = Count


Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_var_td_real ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_var_td_real ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_var_td_real ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_var_td_real ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_var_td_real ( Hndl, Element, DateStr, Varname, Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_var_td_real_sca 




SUBROUTINE wrf_get_var_td_double_arr ( DataHandle,Element, DateStr, Varname, Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
CHARACTER*(*) , INTENT(IN)  :: VarName 

 real*8  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_var_td_double_arr " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_var_td_double ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_var_td_double ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_var_td_double ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_var_td_double ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_var_td_double ( Hndl, Element, DateStr, Varname, Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_var_td_double_arr 


SUBROUTINE wrf_get_var_td_double_sca ( DataHandle,Element, DateStr, Varname, Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
CHARACTER*(*) , INTENT(IN)  :: VarName 

 real*8  :: Data 

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_var_td_double_sca " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_var_td_double ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_var_td_double ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_var_td_double ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_var_td_double ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_var_td_double ( Hndl, Element, DateStr, Varname, Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_var_td_double_sca 




SUBROUTINE wrf_put_var_td_double_arr ( DataHandle,Element, DateStr, Varname, Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
CHARACTER*(*) , INTENT(IN)  :: VarName 

 real*8  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_var_td_double_arr " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_var_td_double ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_var_td_double ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_var_td_double ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_var_td_double ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_var_td_double ( Hndl, Element, DateStr, Varname, Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_var_td_double_arr 


SUBROUTINE wrf_put_var_td_double_sca ( DataHandle,Element, DateStr, Varname, Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
CHARACTER*(*) , INTENT(IN)  :: VarName 

 real*8  :: Data 

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_var_td_double_sca " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_var_td_double ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_var_td_double ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_var_td_double ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_var_td_double ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_var_td_double ( Hndl, Element, DateStr, Varname, Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_var_td_double_sca 




SUBROUTINE wrf_get_var_td_integer_arr ( DataHandle,Element, DateStr, Varname, Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
CHARACTER*(*) , INTENT(IN)  :: VarName 

 integer  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_var_td_integer_arr " )

locCount = Count



Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_var_td_integer ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_var_td_integer ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_var_td_integer ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_var_td_integer ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_var_td_integer ( Hndl, Element, DateStr, Varname, Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_var_td_integer_arr 


SUBROUTINE wrf_get_var_td_integer_sca ( DataHandle,Element, DateStr, Varname, Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
CHARACTER*(*) , INTENT(IN)  :: VarName 

 integer  :: Data 

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_var_td_integer_sca " )

locCount = Count



Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_var_td_integer ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_var_td_integer ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_var_td_integer ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_var_td_integer ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_var_td_integer ( Hndl, Element, DateStr, Varname, Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_var_td_integer_sca 




SUBROUTINE wrf_put_var_td_integer_arr ( DataHandle,Element, DateStr, Varname, Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
CHARACTER*(*) , INTENT(IN)  :: VarName 

 integer  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_var_td_integer_arr " )

locCount = Count



Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_var_td_integer ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_var_td_integer ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_var_td_integer ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_var_td_integer ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_var_td_integer ( Hndl, Element, DateStr, Varname, Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_var_td_integer_arr 


SUBROUTINE wrf_put_var_td_integer_sca ( DataHandle,Element, DateStr, Varname, Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
CHARACTER*(*) , INTENT(IN)  :: VarName 

 integer  :: Data 

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_var_td_integer_sca " )

locCount = Count



Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_var_td_integer ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_var_td_integer ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_var_td_integer ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_var_td_integer ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_var_td_integer ( Hndl, Element, DateStr, Varname, Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_var_td_integer_sca 




SUBROUTINE wrf_get_var_td_logical_arr ( DataHandle,Element, DateStr, Varname, Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
CHARACTER*(*) , INTENT(IN)  :: VarName 

 logical  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_var_td_logical_arr " )



locCount = Count

Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_var_td_logical ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_var_td_logical ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_var_td_logical ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_var_td_logical ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_var_td_logical ( Hndl, Element, DateStr, Varname, Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_var_td_logical_arr 


SUBROUTINE wrf_get_var_td_logical_sca ( DataHandle,Element, DateStr, Varname, Data, Count, Outcount, Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
CHARACTER*(*) , INTENT(IN)  :: VarName 

 logical  :: Data 

INTEGER ,       INTENT(IN)  :: Count
INTEGER ,       INTENT(OUT)  :: OutCount
INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_var_td_logical_sca " )



locCount = Count

Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_var_td_logical ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_var_td_logical ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_var_td_logical ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          CALL wrf_dm_bcast_bytes( locCount, 4 )
          CALL wrf_dm_bcast_bytes( Data, 4*locCount )
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_var_td_logical ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount, Outcount, Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           CALL wrf_dm_bcast_bytes( locCount, 4 )
           CALL wrf_dm_bcast_bytes( Data, 4*locCount )
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_var_td_logical ( Hndl, Element, DateStr, Varname, Data, &
                          locCount, Outcount, Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_var_td_logical_sca 




SUBROUTINE wrf_put_var_td_logical_arr ( DataHandle,Element, DateStr, Varname, Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
CHARACTER*(*) , INTENT(IN)  :: VarName 

 logical  :: Data (*)

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_var_td_logical_arr " )



locCount = Count

Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_var_td_logical ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_var_td_logical ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_var_td_logical ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_var_td_logical ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_var_td_logical ( Hndl, Element, DateStr, Varname, Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_var_td_logical_arr 


SUBROUTINE wrf_put_var_td_logical_sca ( DataHandle,Element, DateStr, Varname, Data, Count,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
CHARACTER*(*) , INTENT(IN)  :: VarName 

 logical  :: Data 

INTEGER ,       INTENT(IN)  :: Count

INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_var_td_logical_sca " )



locCount = Count

Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_var_td_logical ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_var_td_logical ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_var_td_logical ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_var_td_logical ( Hndl, Element, DateStr, Varname, Data, &
                                 locCount,  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_var_td_logical ( Hndl, Element, DateStr, Varname, Data, &
                          locCount,  Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_var_td_logical_sca 




SUBROUTINE wrf_get_var_td_char_arr ( DataHandle,Element, DateStr, Varname, Data,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
CHARACTER*(*) , INTENT(IN)  :: VarName 

 CHARACTER*(*)  :: Data



INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_get_var_td_char_arr " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_get_var_td_char ( Hndl, Element, DateStr, Varname, Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          len_of_str = LEN(Data)
          CALL wrf_dm_bcast_string( Data, len_of_str )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_get_var_td_char ( Hndl, Element, DateStr, Varname, Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          len_of_str = LEN(Data)
          CALL wrf_dm_bcast_string( Data, len_of_str )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_get_var_td_char ( Hndl, Element, DateStr, Varname, Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          len_of_str = LEN(Data)
          CALL wrf_dm_bcast_string( Data, len_of_str )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_get_var_td_char ( Hndl, Element, DateStr, Varname, Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           len_of_str = LEN(Data)
           CALL wrf_dm_bcast_string( Data, len_of_str )
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_get_var_td_char ( Hndl, Element, DateStr, Varname, Data, &
                           Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_get_var_td_char_arr 




SUBROUTINE wrf_put_var_td_char_arr ( DataHandle,Element, DateStr, Varname, Data,  Status )












USE module_state_description
IMPLICIT NONE
INTEGER ,       INTENT(IN)  :: DataHandle
CHARACTER*(*) , INTENT(IN)  :: Element
CHARACTER*(*) , INTENT(IN)  :: DateStr
CHARACTER*(*) , INTENT(IN)  :: VarName 

 CHARACTER*(*)  :: Data



INTEGER ,       INTENT(OUT) :: Status

  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

INTEGER                     :: len_of_str
LOGICAL                     :: for_out
INTEGER, EXTERNAL           :: use_package
LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
INTEGER                     :: locCount

INTEGER io_form , Hndl

CALL wrf_debug( 500, "module_io.F (md_calls.m4) : in wrf_put_var_td_char_arr " )





Status = 0
CALL get_handle ( Hndl, io_form , for_out, DataHandle )
IF ( Hndl .GT. -1 ) THEN
  IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
    SELECT CASE ( use_package( io_form ) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_ncd_put_var_td_char ( Hndl, Element, DateStr, Varname, Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr1_put_var_td_char ( Hndl, Element, DateStr, Varname, Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_gr2_put_var_td_char ( Hndl, Element, DateStr, Varname, Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          
          
          
          
          
          
          
          
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
           CALL ext_int_put_var_td_char ( Hndl, Element, DateStr, Varname, Data, &
                                  Status ) 
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
           
           
           
           
           
           
           
           
           CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
    END SELECT
  ELSE IF ( for_out .AND. use_output_servers() ) THEN
    CALL wrf_quilt_put_var_td_char ( Hndl, Element, DateStr, Varname, Data, &
                           Status )
  ELSE
    Status = 0
ENDIF
ELSE
  Status = WRF_ERR_FATAL_BAD_FILE_STATUS
ENDIF
RETURN
END SUBROUTINE wrf_put_var_td_char_arr 






  INTEGER FUNCTION io_form_for_dataset ( DataSet )
    IMPLICIT NONE
    CHARACTER*(*), INTENT(IN)  :: DataSet
    INTEGER                    :: io_form 






    IF      ( DataSet .eq. 'RESTART' ) THEN
      CALL nl_get_io_form_restart( 1, io_form )
    ELSE IF ( DataSet .eq. 'INPUT' ) THEN
      CALL nl_get_io_form_input( 1, io_form )
    ELSE IF ( DataSet .eq. 'HISTORY' ) THEN
      CALL nl_get_io_form_history( 1, io_form )
    ELSE IF ( DataSet .eq. 'BOUNDARY' ) THEN
      CALL nl_get_io_form_boundary( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT1' ) THEN
      CALL nl_get_io_form_auxinput1( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST1' ) THEN
      CALL nl_get_io_form_auxhist1( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT2' ) THEN
      CALL nl_get_io_form_auxinput2( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST2' ) THEN
      CALL nl_get_io_form_auxhist2( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT3' ) THEN
      CALL nl_get_io_form_auxinput3( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST3' ) THEN
      CALL nl_get_io_form_auxhist3( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT4' ) THEN
      CALL nl_get_io_form_auxinput4( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST4' ) THEN
      CALL nl_get_io_form_auxhist4( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT5' ) THEN
      CALL nl_get_io_form_auxinput5( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST5' ) THEN
      CALL nl_get_io_form_auxhist5( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT6' ) THEN
      CALL nl_get_io_form_auxinput6( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST6' ) THEN
      CALL nl_get_io_form_auxhist6( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT7' ) THEN
      CALL nl_get_io_form_auxinput7( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST7' ) THEN
      CALL nl_get_io_form_auxhist7( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT8' ) THEN
      CALL nl_get_io_form_auxinput8( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST8' ) THEN
      CALL nl_get_io_form_auxhist8( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT9' ) THEN
      CALL nl_get_io_form_auxinput9( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST9' ) THEN
      CALL nl_get_io_form_auxhist9( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT10' ) THEN
      CALL nl_get_io_form_auxinput10( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST10' ) THEN
      CALL nl_get_io_form_auxhist10( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT11' ) THEN
      CALL nl_get_io_form_auxinput11( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST11' ) THEN
      CALL nl_get_io_form_auxhist11( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT12' ) THEN
      CALL nl_get_io_form_auxinput12( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST12' ) THEN
      CALL nl_get_io_form_auxhist12( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT13' ) THEN
      CALL nl_get_io_form_auxinput13( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST13' ) THEN
      CALL nl_get_io_form_auxhist13( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT14' ) THEN
      CALL nl_get_io_form_auxinput14( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST14' ) THEN
      CALL nl_get_io_form_auxhist14( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT15' ) THEN
      CALL nl_get_io_form_auxinput15( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST15' ) THEN
      CALL nl_get_io_form_auxhist15( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT16' ) THEN
      CALL nl_get_io_form_auxinput16( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST16' ) THEN
      CALL nl_get_io_form_auxhist16( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT17' ) THEN
      CALL nl_get_io_form_auxinput17( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST17' ) THEN
      CALL nl_get_io_form_auxhist17( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT18' ) THEN
      CALL nl_get_io_form_auxinput18( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST18' ) THEN
      CALL nl_get_io_form_auxhist18( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT19' ) THEN
      CALL nl_get_io_form_auxinput19( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST19' ) THEN
      CALL nl_get_io_form_auxhist19( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT20' ) THEN
      CALL nl_get_io_form_auxinput20( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST20' ) THEN
      CALL nl_get_io_form_auxhist20( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT21' ) THEN
      CALL nl_get_io_form_auxinput21( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST21' ) THEN
      CALL nl_get_io_form_auxhist21( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT22' ) THEN
      CALL nl_get_io_form_auxinput22( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST22' ) THEN
      CALL nl_get_io_form_auxhist22( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT23' ) THEN
      CALL nl_get_io_form_auxinput23( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST23' ) THEN
      CALL nl_get_io_form_auxhist23( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT24' ) THEN
      CALL nl_get_io_form_auxinput24( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST24' ) THEN
      CALL nl_get_io_form_auxhist24( 1, io_form )
    ELSE  
      CALL nl_get_io_form_history( 1, io_form )
    ENDIF

    io_form_for_dataset = io_form
    RETURN
  END FUNCTION io_form_for_dataset

  INTEGER FUNCTION io_form_for_stream ( stream )
    USE module_streams
    IMPLICIT NONE
    INTEGER,       INTENT(IN)  :: stream
    INTEGER                    :: io_form 






    IF      ( stream .eq. restart_only ) THEN
      CALL nl_get_io_form_restart( 1, io_form )
    ELSE IF ( stream .eq. input_only ) THEN
      CALL nl_get_io_form_input( 1, io_form )
    ELSE IF ( stream .eq. history_only ) THEN
      CALL nl_get_io_form_history( 1, io_form )
    ELSE IF ( stream .eq. boundary_only ) THEN
      CALL nl_get_io_form_boundary( 1, io_form )
    ELSE IF ( stream .eq. auxinput1_only ) THEN
      CALL nl_get_io_form_auxinput1( 1, io_form )
    ELSE IF ( stream .eq. auxhist1_only ) THEN
      CALL nl_get_io_form_auxhist1( 1, io_form )
    ELSE IF ( stream .eq. auxinput2_only ) THEN
      CALL nl_get_io_form_auxinput2( 1, io_form )
    ELSE IF ( stream .eq. auxhist2_only ) THEN
      CALL nl_get_io_form_auxhist2( 1, io_form )
    ELSE IF ( stream .eq. auxinput3_only ) THEN
      CALL nl_get_io_form_auxinput3( 1, io_form )
    ELSE IF ( stream .eq. auxhist3_only ) THEN
      CALL nl_get_io_form_auxhist3( 1, io_form )
    ELSE IF ( stream .eq. auxinput4_only ) THEN
      CALL nl_get_io_form_auxinput4( 1, io_form )
    ELSE IF ( stream .eq. auxhist4_only ) THEN
      CALL nl_get_io_form_auxhist4( 1, io_form )
    ELSE IF ( stream .eq. auxinput5_only ) THEN
      CALL nl_get_io_form_auxinput5( 1, io_form )
    ELSE IF ( stream .eq. auxhist5_only ) THEN
      CALL nl_get_io_form_auxhist5( 1, io_form )
    ELSE IF ( stream .eq. auxinput6_only ) THEN
      CALL nl_get_io_form_auxinput6( 1, io_form )
    ELSE IF ( stream .eq. auxhist6_only ) THEN
      CALL nl_get_io_form_auxhist6( 1, io_form )
    ELSE IF ( stream .eq. auxinput7_only ) THEN
      CALL nl_get_io_form_auxinput7( 1, io_form )
    ELSE IF ( stream .eq. auxhist7_only ) THEN
      CALL nl_get_io_form_auxhist7( 1, io_form )
    ELSE IF ( stream .eq. auxinput8_only ) THEN
      CALL nl_get_io_form_auxinput8( 1, io_form )
    ELSE IF ( stream .eq. auxhist8_only ) THEN
      CALL nl_get_io_form_auxhist8( 1, io_form )
    ELSE IF ( stream .eq. auxinput9_only ) THEN
      CALL nl_get_io_form_auxinput9( 1, io_form )
    ELSE IF ( stream .eq. auxhist9_only ) THEN
      CALL nl_get_io_form_auxhist9( 1, io_form )
    ELSE IF ( stream .eq. auxinput10_only ) THEN
      CALL nl_get_io_form_auxinput10( 1, io_form )
    ELSE IF ( stream .eq. auxhist10_only ) THEN
      CALL nl_get_io_form_auxhist10( 1, io_form )
    ELSE IF ( stream .eq. auxinput11_only ) THEN
      CALL nl_get_io_form_auxinput11( 1, io_form )
    ELSE IF ( stream .eq. auxhist11_only ) THEN
      CALL nl_get_io_form_auxhist11( 1, io_form )
    ELSE IF ( stream .eq. auxinput12_only ) THEN
      CALL nl_get_io_form_auxinput12( 1, io_form )
    ELSE IF ( stream .eq. auxhist12_only ) THEN
      CALL nl_get_io_form_auxhist12( 1, io_form )
    ELSE IF ( stream .eq. auxinput13_only ) THEN
      CALL nl_get_io_form_auxinput13( 1, io_form )
    ELSE IF ( stream .eq. auxhist13_only ) THEN
      CALL nl_get_io_form_auxhist13( 1, io_form )
    ELSE IF ( stream .eq. auxinput14_only ) THEN
      CALL nl_get_io_form_auxinput14( 1, io_form )
    ELSE IF ( stream .eq. auxhist14_only ) THEN
      CALL nl_get_io_form_auxhist14( 1, io_form )
    ELSE IF ( stream .eq. auxinput15_only ) THEN
      CALL nl_get_io_form_auxinput15( 1, io_form )
    ELSE IF ( stream .eq. auxhist15_only ) THEN
      CALL nl_get_io_form_auxhist15( 1, io_form )
    ELSE IF ( stream .eq. auxinput16_only ) THEN
      CALL nl_get_io_form_auxinput16( 1, io_form )
    ELSE IF ( stream .eq. auxhist16_only ) THEN
      CALL nl_get_io_form_auxhist16( 1, io_form )
    ELSE IF ( stream .eq. auxinput17_only ) THEN
      CALL nl_get_io_form_auxinput17( 1, io_form )
    ELSE IF ( stream .eq. auxhist17_only ) THEN
      CALL nl_get_io_form_auxhist17( 1, io_form )
    ELSE IF ( stream .eq. auxinput18_only ) THEN
      CALL nl_get_io_form_auxinput18( 1, io_form )
    ELSE IF ( stream .eq. auxhist18_only ) THEN
      CALL nl_get_io_form_auxhist18( 1, io_form )
    ELSE IF ( stream .eq. auxinput19_only ) THEN
      CALL nl_get_io_form_auxinput19( 1, io_form )
    ELSE IF ( stream .eq. auxhist19_only ) THEN
      CALL nl_get_io_form_auxhist19( 1, io_form )
    ELSE IF ( stream .eq. auxinput20_only ) THEN
      CALL nl_get_io_form_auxinput20( 1, io_form )
    ELSE IF ( stream .eq. auxhist20_only ) THEN
      CALL nl_get_io_form_auxhist20( 1, io_form )
    ELSE IF ( stream .eq. auxinput21_only ) THEN
      CALL nl_get_io_form_auxinput21( 1, io_form )
    ELSE IF ( stream .eq. auxhist21_only ) THEN
      CALL nl_get_io_form_auxhist21( 1, io_form )
    ELSE IF ( stream .eq. auxinput22_only ) THEN
      CALL nl_get_io_form_auxinput22( 1, io_form )
    ELSE IF ( stream .eq. auxhist22_only ) THEN
      CALL nl_get_io_form_auxhist22( 1, io_form )
    ELSE IF ( stream .eq. auxinput23_only ) THEN
      CALL nl_get_io_form_auxinput23( 1, io_form )
    ELSE IF ( stream .eq. auxhist23_only ) THEN
      CALL nl_get_io_form_auxhist23( 1, io_form )
    ELSE IF ( stream .eq. auxinput24_only ) THEN
      CALL nl_get_io_form_auxinput24( 1, io_form )
    ELSE IF ( stream .eq. auxhist24_only ) THEN
      CALL nl_get_io_form_auxhist24( 1, io_form )
    ELSE  
      CALL wrf_error_fatal3("<stdin>",19102,&
'internal error: please contact wrfhelp@ucar.edu: io_form_for_stream.inc -- invalid stream number')
    ENDIF

    io_form_for_stream = io_form
    RETURN
  END FUNCTION io_form_for_stream



SUBROUTINE wrf_ioinit( Status )





  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: Status

  CHARACTER(len=80) :: SysDepInfo
  INTEGER :: ierr(10), minerr, maxerr

  Status = 0
  ierr = 0
  SysDepInfo = " "
  CALL wrf_debug( 500, 'module_io.F: in wrf_ioinit' )
  CALL init_io_handles    
  if ( model_config_rec%use_netcdf_classic ) SysDepInfo="use_netcdf_classic"
  CALL ext_ncd_ioinit(   SysDepInfo, ierr(1) )
  SysDepInfo = " "
  CALL ext_int_ioinit(   SysDepInfo, ierr(2) )
  CALL ext_gr1_ioinit(   SysDepInfo, ierr(9) )
  CALL ext_gr2_ioinit(   SysDepInfo, ierr(10) )
  minerr = MINVAL(ierr)
  maxerr = MAXVAL(ierr)
  IF ( minerr < 0 ) THEN
    Status = minerr
  ELSE IF ( maxerr > 0 ) THEN
    Status = maxerr
  ELSE
    Status = 0
  ENDIF
END SUBROUTINE wrf_ioinit



SUBROUTINE wrf_ioexit( Status )





  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: Status

  LOGICAL, EXTERNAL :: use_output_servers
  INTEGER :: ierr(11), minerr, maxerr

  Status = 0
  ierr = 0
  CALL wrf_debug( 500, 'module_io.F: in wrf_ioexit' )
  CALL ext_ncd_ioexit(  ierr(1) )
  CALL ext_int_ioexit(  ierr(2) )
  CALL ext_gr1_ioexit(  ierr(9) )
  CALL ext_gr2_ioexit(  ierr(10) )
 
  IF ( use_output_servers() ) CALL wrf_quilt_ioexit( ierr(11) )
  minerr = MINVAL(ierr)
  maxerr = MAXVAL(ierr)
  IF ( minerr < 0 ) THEN
    Status = minerr
  ELSE IF ( maxerr > 0 ) THEN
    Status = maxerr
  ELSE
    Status = 0
  ENDIF
END SUBROUTINE wrf_ioexit



SUBROUTINE wrf_open_for_write_begin( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                     DataHandle , Status )






  USE module_state_description
  USE module_dm, ONLY :  ntasks_x, mytask_x, local_communicator_x
  IMPLICIT NONE
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


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER*(*), INTENT(INOUT):: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
 
  CHARACTER*128               :: DataSet
  INTEGER                     :: io_form
  INTEGER                     :: Hndl
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
  CHARACTER*128     :: LocFilename   
  INTEGER           :: myproc
  CHARACTER*128     :: mess
  CHARACTER*1028    :: tstr, t1
  INTEGER i,j
  LOGICAL ncd_nofill

  WRITE(mess,*) 'module_io.F: in wrf_open_for_write_begin, FileName = ',TRIM(FileName)
  CALL wrf_debug( 500, mess )

  CALL get_value_from_pairs ( "DATASET" , SysDepInfo , DataSet )

  CALL nl_get_ncd_nofill( 1 , ncd_nofill )

  io_form = io_form_for_dataset( DataSet )

  Status = 0
  Hndl = -1
  IF ( multi_files( io_form ) .OR. .NOT. use_output_servers() ) THEN
    SELECT CASE ( use_package(io_form) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) THEN
          IF ( multi_files(io_form) ) THEN
            CALL wrf_get_myproc ( myproc )
            CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
          ELSE
            LocFilename = FileName
          ENDIF
          IF ( ncd_nofill ) THEN
            CALL ext_ncd_open_for_write_begin ( LocFileName , Comm_compute, Comm_io, SysDepInfo // ",NOFILL=.TRUE.", &
                                                Hndl , Status )
          ELSE
            CALL ext_ncd_open_for_write_begin ( LocFileName , Comm_compute, Comm_io, SysDepInfo, &
                                                Hndl , Status )
          ENDIF
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( Hndl, 4 )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) THEN
          IF ( multi_files(io_form) ) THEN
            CALL wrf_get_myproc ( myproc )
            CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
          ELSE
            LocFilename = FileName
          ENDIF
          CALL ext_gr1_open_for_write_begin ( LocFileName , Comm_compute, Comm_io, SysDepInfo, &
                                              Hndl , Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( Hndl, 4 )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) THEN
          IF ( multi_files(io_form) ) THEN
            CALL wrf_get_myproc ( myproc )
            CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
          ELSE
            LocFilename = FileName
          ENDIF
          CALL ext_gr2_open_for_write_begin ( LocFileName , Comm_compute, Comm_io, SysDepInfo, &
                                              Hndl , Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( Hndl, 4 )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) THEN
          IF ( multi_files(io_form) ) THEN
            CALL wrf_get_myproc ( myproc )
            CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
          ELSE
            LocFilename = FileName
          ENDIF
          CALL ext_int_open_for_write_begin ( LocFileName , Comm_compute, Comm_io, SysDepInfo, &
                                              Hndl , Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( Hndl, 4 )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        ENDIF
      CASE DEFAULT
        IF ( io_form .NE. 0 ) THEN
          WRITE(mess,*)'Tried to open ',FileName,' writing: no valid io_form (',io_form,')'
          CALL wrf_debug(1, mess)
          Status = WRF_FILE_NOT_OPENED
        ENDIF
    END SELECT
  ELSE IF ( use_output_servers() ) THEN
    IF ( io_form .GT. 0 ) THEN
      IF ( ncd_nofill ) THEN
        CALL wrf_quilt_open_for_write_begin ( FileName , Comm_compute, Comm_io, TRIM(SysDepInfo) // ",NOFILL=.TRUE.", &
                                              Hndl , io_form, Status )
      ELSE
        CALL wrf_quilt_open_for_write_begin ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                              Hndl , io_form, Status )
      ENDIF
    ENDIF
  ELSE
    Status = 0
  ENDIF
  CALL add_new_handle( Hndl, io_form, .TRUE., DataHandle )
END SUBROUTINE wrf_open_for_write_begin



SUBROUTINE wrf_open_for_write_commit( DataHandle , Status )







  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
 
  CHARACTER (128)             :: DataSet
  INTEGER                     :: io_form
  INTEGER                     :: Hndl
  LOGICAL                     :: for_out
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
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


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102

  CALL wrf_debug( 500, 'module_io.F: in wrf_open_for_write_commit' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  CALL set_first_operation( DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
      SELECT CASE ( use_package(io_form) )
        CASE ( IO_NETCDF   )
          IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) THEN
            CALL ext_ncd_open_for_write_commit ( Hndl , Status )
          ENDIF
          IF ( .NOT. multi_files(io_form) ) CALL wrf_dm_bcast_bytes( Status, 4 )
      CASE ( IO_GRIB1   )
         IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) THEN
            CALL ext_gr1_open_for_write_commit ( Hndl , Status )
         ENDIF
         IF ( .NOT. multi_files(io_form) ) CALL wrf_dm_bcast_bytes( Status, 4 )
      CASE ( IO_GRIB2   )
         IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) THEN
            CALL ext_gr2_open_for_write_commit ( Hndl , Status )
         ENDIF
         IF ( .NOT. multi_files(io_form) ) CALL wrf_dm_bcast_bytes( Status, 4 )
      CASE ( IO_INTIO   )
        CALL ext_int_open_for_write_commit ( Hndl , Status )
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers() ) THEN
      CALL wrf_quilt_open_for_write_commit ( Hndl , Status )
    ELSE
      Status = 0
    ENDIF
  ELSE
    Status = 0
  ENDIF
  RETURN
END SUBROUTINE wrf_open_for_write_commit



SUBROUTINE wrf_open_for_read_begin( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                     DataHandle , Status )






  USE module_state_description
  IMPLICIT NONE
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


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER*(*) :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  
  CHARACTER*128               :: DataSet
  INTEGER                     :: io_form
  INTEGER                     :: Hndl
  LOGICAL                     :: also_for_out
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers

  CHARACTER*128     :: LocFilename   
  INTEGER     myproc
  CHARACTER*128     :: mess, fhand
  CHARACTER*1028    :: tstr

  CALL wrf_debug( 500, 'module_io.F: in wrf_open_for_read_begin' )

  CALL get_value_from_pairs ( "DATASET" , SysDepInfo , DataSet )

  io_form = io_form_for_dataset( DataSet )

  Status = 0
  Hndl = -1
  also_for_out = .FALSE.

    SELECT CASE ( use_package(io_form) )
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
          IF ( multi_files(io_form) ) THEN
              CALL wrf_get_myproc ( myproc )
              CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
          ELSE
              LocFilename = FileName
          ENDIF
          CALL ext_ncd_open_for_read_begin ( LocFilename , Comm_compute, Comm_io, SysDepInfo, &
                                       Hndl , Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( Status, 4 )
          CALL wrf_dm_bcast_bytes( Hndl, 4 )
        ENDIF
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
          IF ( multi_files(io_form) ) THEN
              CALL wrf_get_myproc ( myproc )
              CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
          ELSE
              LocFilename = FileName
          ENDIF
          CALL ext_gr1_open_for_read_begin ( LocFileName , Comm_compute, Comm_io, SysDepInfo, &
               Hndl , Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( Status, 4 )
          CALL wrf_dm_bcast_bytes( Hndl, 4 )
        ENDIF
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
          IF ( multi_files(io_form) ) THEN
              CALL wrf_get_myproc ( myproc )
              CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
          ELSE
              LocFilename = FileName
          ENDIF
          CALL ext_gr2_open_for_read_begin ( LocFileName , Comm_compute, Comm_io, SysDepInfo, &
               Hndl , Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( Status, 4 )
          CALL wrf_dm_bcast_bytes( Hndl, 4 )
        ENDIF
      CASE ( IO_INTIO   )
      CASE DEFAULT
        IF ( io_form .NE. 0 ) THEN
          WRITE(mess,*)'Tried to open ',FileName,' reading: no valid io_form (',io_form,')'
          CALL wrf_message(mess)
        ENDIF
        Status = WRF_FILE_NOT_OPENED
    END SELECT



  CALL add_new_handle( Hndl, io_form, also_for_out, DataHandle )
END SUBROUTINE wrf_open_for_read_begin



SUBROUTINE wrf_open_for_read_commit( DataHandle , Status )







  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
 
  CHARACTER (128)             :: DataSet
  INTEGER                     :: io_form
  INTEGER                     :: Hndl
  LOGICAL                     :: for_out
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
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


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102

  CALL wrf_debug( 500, 'module_io.F: in wrf_open_for_read_commit' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  CALL set_first_operation( DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( .NOT. (for_out .AND. use_output_servers()) ) THEN
      SELECT CASE ( use_package(io_form) )
        CASE ( IO_NETCDF   )
          IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) THEN
            CALL ext_ncd_open_for_read_commit ( Hndl , Status )
          ENDIF
          IF ( .NOT. multi_files(io_form) ) CALL wrf_dm_bcast_bytes( Status, 4 )
      CASE ( IO_GRIB1   )
        CALL ext_gr1_open_for_read_commit ( Hndl , Status )
      CASE ( IO_GRIB2   )
        CALL ext_gr2_open_for_read_commit ( Hndl , Status )
      CASE ( IO_INTIO   )
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE
      Status = 0
    ENDIF
  ELSE
    Status = WRF_FILE_NOT_OPENED
  ENDIF
  RETURN
END SUBROUTINE wrf_open_for_read_commit



SUBROUTINE wrf_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               DataHandle , Status )





  USE module_state_description
  IMPLICIT NONE
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER*(*) :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  CHARACTER (128)             :: DataSet, LocFileName
  INTEGER                     :: io_form, myproc
  INTEGER                     :: Hndl
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers

  CALL wrf_debug( 500, 'module_io.F: in wrf_open_for_read' )

  CALL get_value_from_pairs ( "DATASET" , SysDepInfo , DataSet )

  io_form = io_form_for_dataset( DataSet )

  Hndl = -1
  Status = 0
  SELECT CASE ( use_package(io_form) )
    CASE ( IO_NETCDF   )
      IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
        IF ( multi_files(io_form) ) THEN
            CALL wrf_get_myproc ( myproc )
            CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
        ELSE
            LocFilename = FileName
        ENDIF

        CALL ext_ncd_open_for_read ( LocFilename , Comm_compute, Comm_io, SysDepInfo, &
                                     Hndl , Status )
      ENDIF
      IF ( .NOT. multi_files(io_form) ) THEN
        CALL wrf_dm_bcast_bytes( Status, 4 )
        CALL wrf_dm_bcast_bytes( Hndl, 4 )
      ENDIF
    CASE ( IO_GRIB1   )
      IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
        IF ( multi_files(io_form) ) THEN
            CALL wrf_get_myproc ( myproc )
            CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
        ELSE
            LocFilename = FileName
        ENDIF

        CALL ext_gr1_open_for_read ( LocFilename , Comm_compute, Comm_io, SysDepInfo, &
                                     Hndl , Status )
      ENDIF
      IF ( .NOT. multi_files(io_form) ) THEN
        CALL wrf_dm_bcast_bytes( Status, 4 )
        CALL wrf_dm_bcast_bytes( Hndl, 4 )
      ENDIF
    CASE ( IO_GRIB2   )
      IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
        IF ( multi_files(io_form) ) THEN
            CALL wrf_get_myproc ( myproc )
            CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
        ELSE
            LocFilename = FileName
        ENDIF

        CALL ext_gr2_open_for_read ( LocFilename , Comm_compute, Comm_io, SysDepInfo, &
                                     Hndl , Status )
      ENDIF
      IF ( .NOT. multi_files(io_form) ) THEN
        CALL wrf_dm_bcast_bytes( Status, 4 )
        CALL wrf_dm_bcast_bytes( Hndl, 4 )
      ENDIF
    CASE ( IO_INTIO   )
      IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
        IF ( multi_files(io_form) ) THEN
            CALL wrf_get_myproc ( myproc )
            CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
        ELSE
            LocFilename = FileName
        ENDIF
        CALL ext_int_open_for_read ( LocFileName , Comm_compute, Comm_io, SysDepInfo, &
                                     Hndl , Status )
      ENDIF
      IF ( .NOT. multi_files(io_form) ) THEN
        CALL wrf_dm_bcast_bytes( Status, 4 )
        CALL wrf_dm_bcast_bytes( Hndl, 4 )
      ENDIF
    CASE DEFAULT
        Status = 0
  END SELECT
  CALL add_new_handle( Hndl, io_form, .FALSE., DataHandle )
  RETURN  
END SUBROUTINE wrf_open_for_read



SUBROUTINE wrf_inquire_opened ( DataHandle, FileName , FileStatus, Status )





  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(OUT) :: FileStatus
  INTEGER ,       INTENT(OUT) :: Status
  LOGICAL                     :: for_out
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
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


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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


  INTEGER io_form , Hndl

  CALL wrf_debug( 500, 'module_io.F: in wrf_inquire_opened' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
      SELECT CASE ( use_package(io_form) )
        CASE ( IO_NETCDF   )
          IF (wrf_dm_on_monitor()) CALL ext_ncd_inquire_opened ( Hndl, FileName , FileStatus, Status )
          CALL wrf_dm_bcast_bytes( FileStatus, 4 )
          CALL wrf_dm_bcast_bytes( Status    , 4 )
      CASE ( IO_GRIB1   )
          IF (wrf_dm_on_monitor()) CALL ext_gr1_inquire_opened ( Hndl, FileName , FileStatus, Status )
          CALL wrf_dm_bcast_bytes( FileStatus, 4 )
          CALL wrf_dm_bcast_bytes( Status    , 4 )
      CASE ( IO_GRIB2   )
          IF (wrf_dm_on_monitor()) CALL ext_gr2_inquire_opened ( Hndl, FileName , FileStatus, Status )
          CALL wrf_dm_bcast_bytes( FileStatus, 4 )
          CALL wrf_dm_bcast_bytes( Status    , 4 )
      CASE ( IO_INTIO   )
          IF (wrf_dm_on_monitor()) CALL ext_int_inquire_opened ( Hndl, FileName , FileStatus, Status )
          CALL wrf_dm_bcast_bytes( FileStatus, 4 )
          CALL wrf_dm_bcast_bytes( Status    , 4 )
        CASE DEFAULT
          FileStatus = WRF_FILE_NOT_OPENED
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers() ) THEN
      CALL wrf_quilt_inquire_opened ( Hndl, FileName , FileStatus, Status )
    ENDIF
  ELSE
    FileStatus = WRF_FILE_NOT_OPENED
    Status = 0
  ENDIF
  RETURN
END SUBROUTINE wrf_inquire_opened




SUBROUTINE wrf_inquire_filename ( DataHandle, FileName , FileStatus, Status )





  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(OUT) :: FileStatus
  INTEGER ,       INTENT(OUT) :: Status
  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
  LOGICAL                     :: for_out

  INTEGER io_form , Hndl
  INTEGER                     :: str_length , str_count

  CALL wrf_debug( 500, 'module_io.F: in wrf_inquire_filename' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
      SELECT CASE ( use_package( io_form ) )
        CASE ( IO_NETCDF   )
          str_length = LEN ( FileName )
          DO str_count = 1 , str_length
            FileName(str_count:str_count) = ' '
          END DO
          IF (wrf_dm_on_monitor()) CALL ext_ncd_inquire_filename ( Hndl, FileName , FileStatus, Status )
          CALL wrf_dm_bcast_bytes( FileStatus, 4 )
          CALL wrf_dm_bcast_bytes( Status    , 4 )
        CASE ( IO_GRIB1   )
          IF (wrf_dm_on_monitor()) CALL ext_gr1_inquire_filename ( Hndl, FileName , FileStatus, Status )
          CALL wrf_dm_bcast_bytes( FileStatus, 4 )
          CALL wrf_dm_bcast_bytes( Status    , 4 )
        CASE ( IO_GRIB2   )
          IF (wrf_dm_on_monitor()) CALL ext_gr2_inquire_filename ( Hndl, FileName , FileStatus, Status )
          CALL wrf_dm_bcast_bytes( FileStatus, 4 )
          CALL wrf_dm_bcast_bytes( Status    , 4 )
        CASE ( IO_INTIO   )
          IF (wrf_dm_on_monitor()) CALL ext_int_inquire_filename ( Hndl, FileName , FileStatus, Status )
          CALL wrf_dm_bcast_bytes( FileStatus, 4 )
          CALL wrf_dm_bcast_bytes( Status    , 4 )
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers() ) THEN
      CALL wrf_quilt_inquire_filename ( Hndl, FileName , FileStatus, Status )
    ENDIF
  ELSE
    FileName = ""
    Status = 0
  ENDIF
  RETURN
END SUBROUTINE wrf_inquire_filename



SUBROUTINE wrf_iosync ( DataHandle, Status )





  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
  LOGICAL                     :: for_out

  INTEGER io_form , Hndl

  CALL wrf_debug( 500, 'module_io.F: in wrf_iosync' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
      SELECT CASE ( use_package(io_form) )
        CASE ( IO_NETCDF   )
          IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) CALL ext_ncd_iosync( Hndl, Status )
          CALL wrf_dm_bcast_bytes( Status    , 4 )
        CASE ( IO_GRIB1   )
          IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) CALL ext_gr1_iosync( Hndl, Status )
          CALL wrf_dm_bcast_bytes( Status    , 4 )
        CASE ( IO_GRIB2   )
          IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) CALL ext_gr2_iosync( Hndl, Status )
          CALL wrf_dm_bcast_bytes( Status    , 4 )
        CASE ( IO_INTIO   )
          IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) CALL ext_int_iosync( Hndl, Status )
          CALL wrf_dm_bcast_bytes( Status    , 4 )
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers() ) THEN
      CALL wrf_quilt_iosync( Hndl, Status )
    ELSE
      Status = 0
    ENDIF
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN
END SUBROUTINE wrf_iosync



SUBROUTINE wrf_ioclose ( DataHandle, Status )





  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
  INTEGER io_form , Hndl
  LOGICAL                     :: for_out

  CALL wrf_debug( 500, 'module_io.F: in wrf_ioclose' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  CALL free_handle( DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
      SELECT CASE ( use_package(io_form) )
        CASE ( IO_NETCDF   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_ncd_ioclose( Hndl, Status )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        CASE ( IO_GRIB1   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_gr1_ioclose( Hndl, Status )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        CASE ( IO_GRIB2   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_gr2_ioclose( Hndl, Status )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        CASE ( IO_INTIO   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_int_ioclose( Hndl, Status )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers() ) THEN
      CALL wrf_quilt_ioclose( Hndl, Status )
    ELSE
      Status = 0
    ENDIF
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN
END SUBROUTINE wrf_ioclose



SUBROUTINE wrf_get_next_time ( DataHandle, DateStr, Status )





  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status
  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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


  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
  INTEGER io_form , Hndl, len_of_str
  LOGICAL                     :: for_out

  CALL wrf_debug( 500, 'module_io.F: in wrf_get_next_time' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
      SELECT CASE ( use_package(io_form) )
        CASE ( IO_NETCDF   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_ncd_get_next_time( Hndl, DateStr, Status )
          IF ( .NOT. multi_files(io_form) ) THEN
            CALL wrf_dm_bcast_bytes( Status, 4 )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
          ENDIF
        CASE ( IO_GRIB1   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_gr1_get_next_time( Hndl, DateStr, Status )
          IF ( .NOT. multi_files(io_form) ) THEN
            CALL wrf_dm_bcast_bytes( Status, 4 )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
          ENDIF
        CASE ( IO_GRIB2   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_gr2_get_next_time( Hndl, DateStr, Status )
          IF ( .NOT. multi_files(io_form) ) THEN
            CALL wrf_dm_bcast_bytes( Status, 4 )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
          ENDIF
        CASE ( IO_INTIO   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_int_get_next_time( Hndl, DateStr, Status )
          IF ( .NOT. multi_files(io_form) ) THEN
            CALL wrf_dm_bcast_bytes( Status, 4 )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
          ENDIF
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers() ) THEN
      CALL wrf_quilt_get_next_time( Hndl, DateStr, Status )
    ELSE
      Status = 0
    ENDIF
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN
END SUBROUTINE wrf_get_next_time



SUBROUTINE wrf_get_previous_time ( DataHandle, DateStr, Status )





  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status
  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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


  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
  INTEGER io_form , Hndl, len_of_str
  LOGICAL                     :: for_out

  CALL wrf_debug( 500, 'module_io.F: in wrf_get_previous_time' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
      SELECT CASE ( use_package(io_form) )
        CASE ( IO_NETCDF   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_ncd_get_previous_time( Hndl, DateStr, Status )
          IF ( .NOT. multi_files(io_form) ) THEN
            CALL wrf_dm_bcast_bytes( Status, 4 )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
          ENDIF
        CASE ( IO_GRIB1   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_gr1_get_previous_time( Hndl, DateStr, Status )
          IF ( .NOT. multi_files(io_form) ) THEN
            CALL wrf_dm_bcast_bytes( Status, 4 )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
         ENDIF
        CASE ( IO_GRIB2   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_gr2_get_previous_time( Hndl, DateStr, Status )
          IF ( .NOT. multi_files(io_form) ) THEN
            CALL wrf_dm_bcast_bytes( Status, 4 )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
         ENDIF
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers() ) THEN
      CALL wrf_quilt_get_previous_time( Hndl, DateStr, Status )
    ELSE
      Status = 0
    ENDIF
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN
END SUBROUTINE wrf_get_previous_time



SUBROUTINE wrf_set_time ( DataHandle, DateStr, Status )





  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status
  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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


  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
  INTEGER io_form , Hndl
  LOGICAL                     :: for_out

  CALL wrf_debug( 500, 'module_io.F: in wrf_set_time' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
      SELECT CASE ( use_package( io_form ) )
        CASE ( IO_NETCDF   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_ncd_set_time( Hndl, DateStr, Status )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        CASE ( IO_GRIB1   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_gr1_set_time( Hndl, DateStr, Status )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        CASE ( IO_GRIB2   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_gr2_set_time( Hndl, DateStr, Status )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        CASE ( IO_INTIO   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_int_set_time( Hndl, DateStr, Status )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers() ) THEN
      CALL wrf_quilt_set_time( Hndl, DateStr, Status )
    ELSE
      Status = 0
    ENDIF
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN
END SUBROUTINE wrf_set_time



SUBROUTINE wrf_get_next_var ( DataHandle, VarName, Status )






  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: VarName
  INTEGER ,       INTENT(OUT) :: Status
  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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


  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
  INTEGER io_form , Hndl
  LOGICAL                     :: for_out

  CALL wrf_debug( 500, 'module_io.F: in wrf_get_next_var' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
      SELECT CASE ( use_package( io_form ) )
        CASE ( IO_NETCDF   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_ncd_get_next_var( Hndl, VarName, Status )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        CASE ( IO_GRIB1   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_gr1_get_next_var( Hndl, VarName, Status )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        CASE ( IO_GRIB2   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_gr2_get_next_var( Hndl, VarName, Status )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        CASE ( IO_INTIO   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_int_get_next_var( Hndl, VarName, Status )
          CALL wrf_dm_bcast_bytes( Status, 4 )
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers() ) THEN
      CALL wrf_quilt_get_next_var( Hndl, VarName, Status )
    ELSE
      Status = 0
    ENDIF
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN
END SUBROUTINE wrf_get_next_var




SUBROUTINE wrf_get_var_info ( DataHandle , VarName , NDim , MemoryOrder , Stagger , &
                              DomainStart , DomainEnd , Status )






  USE module_state_description
  IMPLICIT NONE
  INTEGER               ,INTENT(IN)     :: DataHandle
  CHARACTER*(*)         ,INTENT(IN)     :: VarName
  INTEGER               ,INTENT(OUT)    :: NDim
  CHARACTER*(*)         ,INTENT(OUT)    :: MemoryOrder
  CHARACTER*(*)         ,INTENT(OUT)    :: Stagger
  INTEGER ,dimension(*) ,INTENT(OUT)    :: DomainStart, DomainEnd
  INTEGER               ,INTENT(OUT)    :: Status
  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

  INTEGER io_form , Hndl
  LOGICAL                     :: for_out
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers

  CALL wrf_debug( 500, 'module_io.F: in wrf_get_var_info' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF (( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) .AND. .NOT. (for_out .AND. use_output_servers()) ) THEN
      SELECT CASE ( use_package( io_form ) )
        CASE ( IO_NETCDF   )
          CALL ext_ncd_get_var_info ( Hndl , VarName , NDim ,            &
                                      MemoryOrder , Stagger ,                  &
                                      DomainStart , DomainEnd ,                &
                                      Status )
        CASE ( IO_GRIB1 )
          CALL ext_gr1_get_var_info ( Hndl , VarName , NDim ,            &
                                      MemoryOrder , Stagger ,                  &
                                      DomainStart , DomainEnd ,                &
                                      Status )
        CASE ( IO_GRIB2 )
          CALL ext_gr2_get_var_info ( Hndl , VarName , NDim ,            &
                                      MemoryOrder , Stagger ,                  &
                                      DomainStart , DomainEnd ,                &
                                      Status )
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers() ) THEN
      CALL wrf_quilt_get_var_info ( Hndl , VarName , NDim ,            &
                                    MemoryOrder , Stagger ,                  &
                                    DomainStart , DomainEnd ,                &
                                    Status )
    ELSE
      Status = 0
    ENDIF
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN

END SUBROUTINE wrf_get_var_info






SUBROUTINE init_io_handles()





  IMPLICIT NONE
  INTEGER i
  IF ( .NOT. is_inited ) THEN
    DO i = 1, MAX_WRF_IO_HANDLE
      wrf_io_handles(i) = -999319
    ENDDO
    is_inited = .TRUE.
  ENDIF
  RETURN
END SUBROUTINE init_io_handles

SUBROUTINE add_new_handle( Hndl, Hopened, for_out, DataHandle )









  IMPLICIT NONE
  INTEGER, INTENT(IN)     :: Hndl
  INTEGER, INTENT(IN)     :: Hopened
  LOGICAL, INTENT(IN)     :: for_out
  INTEGER, INTENT(OUT)    :: DataHandle
  INTEGER i
  INTEGER, EXTERNAL       :: use_package
  LOGICAL, EXTERNAL       :: multi_files
  IF ( .NOT. is_inited ) THEN
    CALL wrf_error_fatal3("<stdin>",21460,&
'add_new_handle: not initialized' )
  ENDIF
  IF ( multi_files( Hopened ) ) THEN
    SELECT CASE ( use_package( Hopened ) )
      CASE ( IO_PHDF5  )
        CALL wrf_error_fatal3("<stdin>",21466,&
'add_new_handle:  multiple output files not supported for PHDF5' )
      CASE ( IO_PNETCDF  )
        CALL wrf_error_fatal3("<stdin>",21469,&
'add_new_handle:  multiple output files not supported for PNETCDF' )
    END SELECT
  ENDIF
  DataHandle = -1
  DO i = 1, MAX_WRF_IO_HANDLE
    IF ( wrf_io_handles(i) .EQ. -999319 ) THEN
      DataHandle = i 
      wrf_io_handles(i) = Hndl
      how_opened(i)     = Hopened
      for_output(DataHandle) = for_out
      first_operation(DataHandle) = .TRUE.
      EXIT
    ENDIF
  ENDDO
  IF ( DataHandle .EQ. -1 ) THEN
    CALL wrf_error_fatal3("<stdin>",21485,&
'add_new_handle: no handles left' )
  ENDIF
  RETURN
END SUBROUTINE add_new_handle

SUBROUTINE get_handle ( Hndl, Hopened, for_out, DataHandle )










  IMPLICIT NONE
  INTEGER, INTENT(OUT)     :: Hndl
  INTEGER, INTENT(OUT)     :: Hopened
  LOGICAL, INTENT(OUT)     :: for_out
  INTEGER, INTENT(IN)    :: DataHandle
  CHARACTER*128 mess
  INTEGER i
  IF ( .NOT. is_inited ) THEN
    CALL wrf_error_fatal3("<stdin>",21510,&
'module_io.F: get_handle: not initialized' )
  ENDIF
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. MAX_WRF_IO_HANDLE ) THEN
    Hndl = wrf_io_handles(DataHandle)
    Hopened = how_opened(DataHandle)
    for_out = for_output(DataHandle)
  ELSE
    Hndl = -1
  ENDIF
  RETURN
END SUBROUTINE get_handle

SUBROUTINE set_first_operation( DataHandle )






  IMPLICIT NONE
  INTEGER, INTENT(IN)    :: DataHandle
  IF ( .NOT. is_inited ) THEN
    CALL wrf_error_fatal3("<stdin>",21533,&
'module_io.F: get_handle: not initialized' )
  ENDIF
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. MAX_WRF_IO_HANDLE ) THEN
    first_operation(DataHandle) = .TRUE.
  ENDIF
  RETURN
END SUBROUTINE set_first_operation

SUBROUTINE reset_first_operation( DataHandle )






  IMPLICIT NONE
  INTEGER, INTENT(IN)    :: DataHandle
  IF ( .NOT. is_inited ) THEN
    CALL wrf_error_fatal3("<stdin>",21552,&
'module_io.F: get_handle: not initialized' )
  ENDIF
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. MAX_WRF_IO_HANDLE ) THEN
    first_operation(DataHandle) = .FALSE.
  ENDIF
  RETURN
END SUBROUTINE reset_first_operation

LOGICAL FUNCTION is_first_operation( DataHandle )






  IMPLICIT NONE
  INTEGER, INTENT(IN)    :: DataHandle
  IF ( .NOT. is_inited ) THEN
    CALL wrf_error_fatal3("<stdin>",21571,&
'module_io.F: get_handle: not initialized' )
  ENDIF
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. MAX_WRF_IO_HANDLE ) THEN
    is_first_operation = first_operation(DataHandle)
  ENDIF
  RETURN
END FUNCTION is_first_operation

SUBROUTINE free_handle ( DataHandle )





  IMPLICIT NONE
  INTEGER, INTENT(IN)    :: DataHandle
  INTEGER i
  IF ( .NOT. is_inited ) THEN
    CALL wrf_error_fatal3("<stdin>",21590,&
'free_handle: not initialized' )
  ENDIF
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. MAX_WRF_IO_HANDLE ) THEN
    wrf_io_handles(DataHandle) = -999319
  ENDIF
  RETURN
END SUBROUTINE free_handle



SUBROUTINE init_module_io






  CALL init_io_handles
END SUBROUTINE init_module_io

SUBROUTINE are_bdys_distributed( res )
  IMPLICIT NONE
  LOGICAL, INTENT(OUT) :: res
  res = bdy_dist_flag
END SUBROUTINE are_bdys_distributed

SUBROUTINE bdys_not_distributed
  IMPLICIT NONE
  bdy_dist_flag = .FALSE.
END SUBROUTINE bdys_not_distributed

SUBROUTINE bdys_are_distributed
  IMPLICIT NONE
  bdy_dist_flag = .TRUE.
END SUBROUTINE bdys_are_distributed

LOGICAL FUNCTION on_stream ( mask , switch )
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: mask(*), switch
  INTEGER             :: result


  CALL get_mask( mask, switch-1, result )
  on_stream = ( result .NE. 0 )
END FUNCTION on_stream

END MODULE module_io








SUBROUTINE wrf_read_field ( DataHandle , DateStr , VarName , Field , FieldType ,         &
                            Comm       , IOComm  ,                                       &
                            DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )







  USE module_state_description
  USE module_configure
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)    :: DataHandle
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  LOGICAL ,       INTENT(INOUT) :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(OUT)   :: Status
  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
  INTEGER, ALLOCATABLE        :: ICAST(:)
  LOGICAL perturb_input
  IF ( FieldType .EQ. WRF_LOGICAL ) THEN
    ALLOCATE(ICAST((MemoryEnd(1)-MemoryStart(1)+1)*(MemoryEnd(2)-MemoryStart(2)+1)*(MemoryEnd(3)-MemoryStart(3)+1)))

    CALL wrf_read_field1 ( DataHandle , DateStr , VarName , ICAST , WRF_INTEGER ,         &
                           Comm       , IOComm  ,                                       &
                           DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                           DomainStart , DomainEnd ,                                    &
                           MemoryStart , MemoryEnd ,                                    &
                           PatchStart , PatchEnd ,                                      &
                           Status )
    Field(1:(MemoryEnd(1)-MemoryStart(1)+1)*(MemoryEnd(2)-MemoryStart(2)+1)*(MemoryEnd(3)-MemoryStart(3)+1)) = ICAST == 1
    DEALLOCATE(ICAST)
  ELSE
    CALL wrf_read_field1 ( DataHandle , DateStr , VarName , Field , FieldType ,         &
                           Comm       , IOComm  ,                                       &
                           DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                           DomainStart , DomainEnd ,                                    &
                           MemoryStart , MemoryEnd ,                                    &
                           PatchStart , PatchEnd ,                                      &
                           Status )
    CALL nl_get_perturb_input( 1, perturb_input )
    IF ( perturb_input .AND. FieldType .EQ. WRF_FLOAT .AND. TRIM(MemoryOrder) .EQ. 'XZY' ) THEN
       CALL perturb_real ( Field, DomainStart, DomainEnd,        &
                                  MemoryStart, MemoryEnd,        &
                                  PatchStart, PatchEnd )
    ENDIF
  ENDIF
END SUBROUTINE wrf_read_field

SUBROUTINE wrf_read_field1 ( DataHandle , DateStr , VarName , Field , FieldType ,         &
                            Comm       , IOComm  ,                                       &
                            DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )






  USE module_state_description
  USE module_configure
  USE module_io
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  INTEGER ,       INTENT(INOUT) :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm 
  INTEGER                       ,INTENT(INOUT) :: IOComm 
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(OUT)   :: Status
  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

  INTEGER io_form , Hndl
  LOGICAL                     :: for_out
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers, use_input_servers
  EXTERNAL     ext_ncd_read_field
  EXTERNAL     ext_int_read_field
  EXTERNAL ext_gr1_read_field
  EXTERNAL ext_gr2_read_field

  CALL wrf_debug( 500, 'module_io.F: in wrf_read_field' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  CALL reset_first_operation( DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( .NOT. io_form .GT. 0 ) THEN
      Status = 0 
    ELSE IF ( .NOT. use_input_servers() ) THEN
      SELECT CASE ( use_package( io_form ) )
        CASE ( IO_NETCDF   )

          CALL call_pkg_and_dist   ( ext_ncd_read_field, multi_files(io_form), .false. ,        &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )

        CASE ( IO_INTIO )
          CALL call_pkg_and_dist   ( ext_int_read_field, multi_files(io_form), .false.,         &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
        CASE ( IO_GRIB1 )
          CALL call_pkg_and_dist   ( ext_gr1_read_field, multi_files(io_form), .false.,         &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
        CASE ( IO_GRIB2 )
          CALL call_pkg_and_dist   ( ext_gr2_read_field, multi_files(io_form), .false.,         &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE
      CALL wrf_error_fatal3("<stdin>",22081,&
'module_io.F: wrf_read_field: input_servers not implemented yet')
    ENDIF
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN
END SUBROUTINE wrf_read_field1

SUBROUTINE wrf_write_field ( DataHandle , DateStr , VarName , Field , FieldType ,         &
                             Comm       , IOComm  ,                                       &
                             DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                             DomainStart , DomainEnd ,                                    &
                             MemoryStart , MemoryEnd ,                                    &
                             PatchStart , PatchEnd ,                                      &
                             Status )







  USE module_state_description
  USE module_configure
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)    :: DataHandle
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  LOGICAL ,       INTENT(IN)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)         ,INTENT(IN)    :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(OUT)   :: Status
  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
  INTEGER, ALLOCATABLE        :: ICAST(:)
  IF ( FieldType .EQ. WRF_LOGICAL ) THEN
      ALLOCATE(ICAST((MemoryEnd(1)-MemoryStart(1)+1)*(MemoryEnd(2)-MemoryStart(2)+1)*(MemoryEnd(3)-MemoryStart(3)+1)))
      ICAST = 0
      WHERE ( Field(1:(MemoryEnd(1)-MemoryStart(1)+1)*(MemoryEnd(2)-MemoryStart(2)+1)*(MemoryEnd(3)-MemoryStart(3)+1)) )
        ICAST = 1
      END WHERE
    CALL wrf_write_field1 ( DataHandle , DateStr , VarName , ICAST , WRF_INTEGER ,         &
                            Comm       , IOComm  ,                                       &
                            DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )
      DEALLOCATE(ICAST)
  ELSE
    CALL wrf_write_field1 ( DataHandle , DateStr , VarName , Field , FieldType ,         &
                            Comm       , IOComm  ,                                       &
                            DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )
  ENDIF
END SUBROUTINE wrf_write_field

SUBROUTINE wrf_write_field1 ( DataHandle , DateStr , VarName , Field , FieldType ,         &
                             Comm       , IOComm  ,                                       &
                             DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                             DomainStart , DomainEnd ,                                    &
                             MemoryStart , MemoryEnd ,                                    &
                             PatchStart , PatchEnd ,                                      &
                             Status )







  USE module_state_description
  USE module_configure
  USE module_io
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  INTEGER ,       INTENT(IN)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)         ,INTENT(IN)    :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(OUT)   :: Status
  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
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

  INTEGER, DIMENSION(3) :: starts, ends
  INTEGER io_form , Hndl
  CHARACTER*3 MemOrd
  LOGICAL                     :: for_out, okay_to_call
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
  EXTERNAL     ext_ncd_write_field
  EXTERNAL     ext_int_write_field
  EXTERNAL ext_gr1_write_field
  EXTERNAL ext_gr2_write_field

  CALL wrf_debug( 500, 'module_io.F: in wrf_write_field' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  CALL reset_first_operation ( DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. use_output_servers() ) THEN
      SELECT CASE ( use_package( io_form ) )
        CASE ( IO_NETCDF   )
          CALL collect_fld_and_call_pkg ( ext_ncd_write_field, multi_files(io_form),                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
        CASE ( IO_GRIB1 )
          CALL collect_fld_and_call_pkg ( ext_gr1_write_field, multi_files(io_form),                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
        CASE ( IO_GRIB2 )
          CALL collect_fld_and_call_pkg ( ext_gr2_write_field, multi_files(io_form),                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
        CASE ( IO_INTIO )
          CALL collect_fld_and_call_pkg ( ext_int_write_field, multi_files(io_form),                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( use_output_servers() ) THEN
      IF ( io_form .GT. 0 ) THEN
      CALL wrf_quilt_write_field ( Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                   DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                   DomainStart , DomainEnd ,                                    &
                                   MemoryStart , MemoryEnd ,                                    &
                                   PatchStart , PatchEnd ,                                      &
                                   Status )
      ENDIF
    ENDIF
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN
END SUBROUTINE wrf_write_field1

SUBROUTINE get_value_from_pairs ( varname , str , retval )







  IMPLICIT NONE
  CHARACTER*(*) ::    varname
  CHARACTER*(*) ::    str
  CHARACTER*(*) ::    retval

  CHARACTER (128) varstr, tstr
  INTEGER i,j,n,varstrn
  LOGICAL nobreak, nobreakouter

  varstr = TRIM(varname)//"="
  varstrn = len(TRIM(varstr))
  n = len(str)
  retval = ""
  i = 1
  nobreakouter = .TRUE.
  DO WHILE ( nobreakouter )
    j = 1
    nobreak = .TRUE.
    tstr = ""











    DO WHILE ( nobreak )
      nobreak = .FALSE.
      IF ( i .LE. n ) THEN
        IF (str(i:i) .NE. ',' ) THEN
           tstr(j:j) = str(i:i)
           nobreak = .TRUE.
        ENDIF
      ENDIF
      j = j + 1
      i = i + 1
    ENDDO
    IF ( i .GT. n ) nobreakouter = .FALSE.
    IF ( varstr(1:varstrn) .EQ. tstr(1:varstrn) ) THEN
      retval(1:) = TRIM(tstr(varstrn+1:))
      nobreakouter = .FALSE.
    ENDIF
  ENDDO
  RETURN
END SUBROUTINE get_value_from_pairs

LOGICAL FUNCTION multi_files ( io_form )










  IMPLICIT NONE
  INTEGER, INTENT(IN) :: io_form
  multi_files = io_form > 99
END FUNCTION multi_files

INTEGER FUNCTION use_package ( io_form )





  IMPLICIT NONE
  INTEGER, INTENT(IN) :: io_form
  use_package = MOD( io_form, 100 )
END FUNCTION use_package


SUBROUTINE collect_fld_and_call_pkg (    fcn, donotcollect_arg,                                       &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )









  IMPLICIT NONE
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


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
  EXTERNAL fcn
  LOGICAL,        INTENT(IN)    :: donotcollect_arg
  INTEGER ,       INTENT(IN)    :: Hndl
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  INTEGER ,       INTENT(IN)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(OUT)   :: Status
  LOGICAL donotcollect
  INTEGER ndims, nproc

  CALL dim_from_memorder( MemoryOrder , ndims)
  CALL wrf_get_nproc( nproc )
  donotcollect = donotcollect_arg .OR. (nproc .EQ. 1)

  IF ( donotcollect ) THEN

    CALL fcn ( Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , MemoryOrder , Stagger , DimNames ,                &
               DomainStart , DomainEnd ,                                      &
               MemoryStart , MemoryEnd ,                                      &
               PatchStart , PatchEnd ,                                        &
               Status )

  ELSE IF ( FieldType .EQ. WRF_DOUBLE  ) THEN

     CALL collect_double_and_call_pkg ( fcn,                                        &
               Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
               DomainStart , DomainEnd ,                                    &
               MemoryStart , MemoryEnd ,                                    &
               PatchStart , PatchEnd ,                                      &
               Status )

  ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN

     CALL collect_real_and_call_pkg ( fcn,                                        &
               Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
               DomainStart , DomainEnd ,                                    &
               MemoryStart , MemoryEnd ,                                    &
               PatchStart , PatchEnd ,                                      &
               Status )

  ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

     CALL collect_int_and_call_pkg ( fcn,                                        &
               Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
               DomainStart , DomainEnd ,                                    &
               MemoryStart , MemoryEnd ,                                    &
               PatchStart , PatchEnd ,                                      &
               Status )

  ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN

     CALL collect_logical_and_call_pkg ( fcn,                                        &
               Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
               DomainStart , DomainEnd ,                                    &
               MemoryStart , MemoryEnd ,                                    &
               PatchStart , PatchEnd ,                                      &
               Status )

  ENDIF
  RETURN
END SUBROUTINE collect_fld_and_call_pkg

SUBROUTINE collect_real_and_call_pkg (   fcn,                                                     &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )









  USE module_state_description
  USE module_driver_constants
  IMPLICIT NONE
  EXTERNAL fcn
  INTEGER ,       INTENT(IN)    :: Hndl
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  REAL    ,       INTENT(IN)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(INOUT)   :: Status
  REAL, ALLOCATABLE :: globbuf (:)
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor

  IF ( wrf_dm_on_monitor() ) THEN
    ALLOCATE( globbuf( (DomainEnd(1)-DomainStart(1)+3)*(DomainEnd(2)-DomainStart(2)+3)*(DomainEnd(3)-DomainStart(3)+3) ) )
  ELSE
    ALLOCATE( globbuf( 1 ) )
  ENDIF

  
  CALL collect_generic_and_call_pkg (   fcn, globbuf ,                                    &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  DEALLOCATE ( globbuf )
  RETURN

END SUBROUTINE collect_real_and_call_pkg

SUBROUTINE collect_int_and_call_pkg (   fcn,                                                     &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )









  USE module_state_description
  USE module_driver_constants
  IMPLICIT NONE
  EXTERNAL fcn
  INTEGER ,       INTENT(IN)    :: Hndl
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  INTEGER    ,       INTENT(IN)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(INOUT)   :: Status
  INTEGER, ALLOCATABLE :: globbuf (:)
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor

  IF ( wrf_dm_on_monitor() ) THEN
    ALLOCATE( globbuf( (DomainEnd(1)-DomainStart(1)+3)*(DomainEnd(2)-DomainStart(2)+3)*(DomainEnd(3)-DomainStart(3)+3) ) )
  ELSE
    ALLOCATE( globbuf( 1 ) )
  ENDIF

  CALL collect_generic_and_call_pkg (   fcn, globbuf  ,                                   &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  DEALLOCATE ( globbuf )
  RETURN

END SUBROUTINE collect_int_and_call_pkg

SUBROUTINE collect_double_and_call_pkg (   fcn,                                                     &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )










  USE module_state_description
  USE module_driver_constants
  IMPLICIT NONE
  EXTERNAL fcn
  INTEGER ,       INTENT(IN)    :: Hndl
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  DOUBLE PRECISION    ,       INTENT(IN)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(INOUT)   :: Status
  DOUBLE PRECISION, ALLOCATABLE :: globbuf (:)
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor

  IF ( wrf_dm_on_monitor() ) THEN
    ALLOCATE( globbuf( (DomainEnd(1)-DomainStart(1)+3)*(DomainEnd(2)-DomainStart(2)+3)*(DomainEnd(3)-DomainStart(3)+3) ) )
  ELSE
    ALLOCATE( globbuf( 1 ) )
  ENDIF

  CALL collect_generic_and_call_pkg (   fcn, globbuf  ,                                   &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  DEALLOCATE ( globbuf )
  RETURN

END SUBROUTINE collect_double_and_call_pkg

SUBROUTINE collect_logical_and_call_pkg (   fcn,                                                     &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )









  USE module_state_description
  USE module_driver_constants
  IMPLICIT NONE
  EXTERNAL fcn
  INTEGER ,       INTENT(IN)    :: Hndl
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  LOGICAL    ,       INTENT(IN)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(INOUT)   :: Status
  LOGICAL, ALLOCATABLE :: globbuf (:)
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor

  IF ( wrf_dm_on_monitor() ) THEN
    ALLOCATE( globbuf( (DomainEnd(1)-DomainStart(1)+3)*(DomainEnd(2)-DomainStart(2)+3)*(DomainEnd(3)-DomainStart(3)+3) ) )
  ELSE
    ALLOCATE( globbuf( 1 ) )
  ENDIF

  CALL collect_generic_and_call_pkg (   fcn, globbuf  ,                                   &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  DEALLOCATE ( globbuf )
  RETURN

END SUBROUTINE collect_logical_and_call_pkg


SUBROUTINE collect_generic_and_call_pkg ( fcn, globbuf,                                           &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )









  USE module_state_description
  USE module_driver_constants
  IMPLICIT NONE
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


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
include "mpif.h"
  EXTERNAL fcn
  REAL , DIMENSION(*) , INTENT(INOUT) :: globbuf
  INTEGER ,       INTENT(IN)    :: Hndl
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  REAL    ,       INTENT(IN)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(OUT)   :: Status
  CHARACTER*3 MemOrd
  LOGICAL, EXTERNAL :: has_char
  INTEGER ids, ide, jds, jde, kds, kde
  INTEGER ims, ime, jms, jme, kms, kme
  INTEGER ips, ipe, jps, jpe, kps, kpe
  INTEGER, ALLOCATABLE :: counts(:), displs(:)
  INTEGER nproc, communicator, mpi_bdyslice_type, ierr, my_displ
  INTEGER my_count
  INTEGER , dimension(3)                       :: dom_end_rev
  LOGICAL, EXTERNAL         :: wrf_dm_on_monitor
  INTEGER, EXTERNAL         :: wrf_dm_monitor_rank
  LOGICAL     distributed_field
  INTEGER i,j,k,idx,lx,idx2,lx2
  INTEGER collective_root

  CALL wrf_get_nproc( nproc )
  CALL wrf_get_dm_communicator ( communicator )

  ALLOCATE( counts( nproc ) )
  ALLOCATE( displs( nproc ) )
  CALL lower_case( MemoryOrder, MemOrd )

  collective_root = wrf_dm_monitor_rank()

  dom_end_rev(1) = DomainEnd(1)
  dom_end_rev(2) = DomainEnd(2)
  dom_end_rev(3) = DomainEnd(3)

  SELECT CASE (TRIM(MemOrd))
    CASE (  'xzy' )
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'z' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(3) = dom_end_rev(3) + 1
    CASE (  'zxy' )
      IF ( .NOT. has_char( Stagger, 'z' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(3) = dom_end_rev(3) + 1
    CASE (  'xyz' )
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
      IF ( .NOT. has_char( Stagger, 'z' ) ) dom_end_rev(3) = dom_end_rev(3) + 1
    CASE (  'xy' )
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
    CASE (  'yxz' )
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
      IF ( .NOT. has_char( Stagger, 'z' ) ) dom_end_rev(3) = dom_end_rev(3) + 1
    CASE (  'yx' )
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
    CASE DEFAULT
      
  END SELECT

  SELECT CASE (TRIM(MemOrd))
    CASE (  'xzy','zxy','xyz','yxz','xy','yx' )

      distributed_field = .TRUE.
      IF ( FieldType .EQ. WRF_DOUBLE ) THEN
        CALL wrf_patch_to_global_double ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
           DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
           MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
           PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
      ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN
        CALL wrf_patch_to_global_real ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
           DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
           MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
           PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
      ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
        CALL wrf_patch_to_global_integer ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
           DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
           MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
           PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
      ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN
        CALL wrf_patch_to_global_logical ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
           DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
           MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
           PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
      ENDIF

    CASE ( 'xsz', 'xez' )
      distributed_field = .FALSE.
      IF ( nproc .GT. 1 ) THEN
        jds = DomainStart(1) ; jde = DomainEnd(1) ; IF ( .NOT. has_char( Stagger, 'y' ) ) jde = jde+1  
        kds = DomainStart(2) ; kde = DomainEnd(2) ; IF ( .NOT. has_char( Stagger, 'z' ) ) kde = kde+1  
        ids = DomainStart(3) ; ide = DomainEnd(3) ; 
        dom_end_rev(1) = jde
        dom_end_rev(2) = kde
        dom_end_rev(3) = ide
        distributed_field = .TRUE.
        IF ( (MemOrd .eq. 'xsz' .AND. bdy_mask( P_XSB )) .OR.     &
             (MemOrd .eq. 'xez' .AND. bdy_mask( P_XEB ))       ) THEN
          my_displ = PatchStart(1)-1
          my_count = PatchEnd(1)-PatchStart(1)+1
        ELSE
          my_displ = 0
          my_count = 0
        ENDIF
        CALL mpi_gather( my_displ, 1, MPI_INTEGER, displs, 1, MPI_INTEGER, collective_root, communicator, ierr )
        CALL mpi_gather( my_count, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, collective_root, communicator, ierr )
        do i = DomainStart(3),DomainEnd(3)    
        do k = DomainStart(2),DomainEnd(2)    
           lx   = MemoryEnd(1)-MemoryStart(1)+1
           lx2  = dom_end_rev(1)-DomainStart(1)+1
           idx  = lx*((k-1)+(i-1)*(MemoryEnd(2)-MemoryStart(2)+1))
           idx2 = lx2*((k-1)+(i-1)*(MemoryEnd(2)-MemoryStart(2)+1))
           IF ( FieldType .EQ. WRF_DOUBLE  ) THEN

             CALL wrf_gatherv_double ( Field, PatchStart(1)-MemoryStart(1)+1+idx , &
                             my_count ,                       &    
                             globbuf, 1+idx2 ,                &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN

             CALL wrf_gatherv_real ( Field, PatchStart(1)-MemoryStart(1)+1+idx , &
                             my_count ,                       &    
                             globbuf, 1+idx2 ,                &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

             CALL wrf_gatherv_integer ( Field, PatchStart(1)-MemoryStart(1)+1+idx , &
                             my_count ,                       &    
                             globbuf, 1+idx2 ,                &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )
           ENDIF

        enddo
        enddo
      ENDIF
    CASE ( 'xs', 'xe' )
      distributed_field = .FALSE.
      IF ( nproc .GT. 1 ) THEN
        jds = DomainStart(1) ; jde = DomainEnd(1) ; IF ( .NOT. has_char( Stagger, 'y' ) ) jde = jde+1  
        ids = DomainStart(2) ; ide = DomainEnd(2) ; 
        dom_end_rev(1) = jde
        dom_end_rev(2) = ide
        distributed_field = .TRUE.
        IF ( (MemOrd .eq. 'xs' .AND. bdy_mask( P_XSB )) .OR.     &
             (MemOrd .eq. 'xe' .AND. bdy_mask( P_XEB ))       ) THEN
          my_displ = PatchStart(1)-1
          my_count = PatchEnd(1)-PatchStart(1)+1
        ELSE
          my_displ = 0
          my_count = 0
        ENDIF
        CALL mpi_gather( my_displ, 1, MPI_INTEGER, displs, 1, MPI_INTEGER, collective_root, communicator, ierr )
        CALL mpi_gather( my_count, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, collective_root, communicator, ierr )
        do i = DomainStart(2),DomainEnd(2)    
           lx   = MemoryEnd(1)-MemoryStart(1)+1
           idx  = lx*(i-1)
           lx2  = dom_end_rev(1)-DomainStart(1)+1
           idx2 = lx2*(i-1)
           IF ( FieldType .EQ. WRF_DOUBLE ) THEN

             CALL wrf_gatherv_double ( Field, PatchStart(1)-MemoryStart(1)+1+idx , &
                             my_count ,                       &    
                             globbuf, 1+idx2 ,                &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN

             CALL wrf_gatherv_real ( Field, PatchStart(1)-MemoryStart(1)+1+idx , &
                             my_count ,                       &    
                             globbuf, 1+idx2 ,                &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

             CALL wrf_gatherv_integer ( Field, PatchStart(1)-MemoryStart(1)+1+idx , &
                             my_count ,                       &    
                             globbuf, 1+idx2 ,                &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )
           ENDIF

        enddo
      ENDIF
    CASE ( 'ysz', 'yez' )
      distributed_field = .FALSE.
      IF ( nproc .GT. 1 ) THEN
        ids = DomainStart(1) ; ide = DomainEnd(1) ; IF ( .NOT. has_char( Stagger, 'y' ) ) ide = ide+1  
        kds = DomainStart(2) ; kde = DomainEnd(2) ; IF ( .NOT. has_char( Stagger, 'z' ) ) kde = kde+1  
        jds = DomainStart(3) ; jde = DomainEnd(3) ; 
        dom_end_rev(1) = ide
        dom_end_rev(2) = kde
        dom_end_rev(3) = jde
        distributed_field = .TRUE.
        IF ( (MemOrd .eq. 'ysz' .AND. bdy_mask( P_YSB )) .OR.     &
             (MemOrd .eq. 'yez' .AND. bdy_mask( P_YEB ))       ) THEN
          my_displ = PatchStart(1)-1
          my_count = PatchEnd(1)-PatchStart(1)+1
        ELSE
          my_displ = 0
          my_count = 0
        ENDIF
        CALL mpi_gather( my_displ, 1, MPI_INTEGER, displs, 1, MPI_INTEGER, collective_root, communicator, ierr )
        CALL mpi_gather( my_count, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, collective_root, communicator, ierr )
        do j = DomainStart(3),DomainEnd(3)    
        do k = DomainStart(2),DomainEnd(2)    
           lx   = MemoryEnd(1)-MemoryStart(1)+1
           lx2  = dom_end_rev(1)-DomainStart(1)+1
           idx  = lx*((k-1)+(j-1)*(MemoryEnd(2)-MemoryStart(2)+1))
           idx2 = lx2*((k-1)+(j-1)*(MemoryEnd(2)-MemoryStart(2)+1))

           IF ( FieldType .EQ. WRF_DOUBLE ) THEN 

             CALL wrf_gatherv_double ( Field, PatchStart(1)-MemoryStart(1)+1+idx ,      &    
                             my_count                       , &    
                             globbuf, 1+idx2                , &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN

             CALL wrf_gatherv_real( Field, PatchStart(1)-MemoryStart(1)+1+idx ,      &    
                             my_count                       , &    
                             globbuf, 1+idx2                , &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

             CALL wrf_gatherv_integer( Field, PatchStart(1)-MemoryStart(1)+1+idx ,      &    
                             my_count                       , &    
                             globbuf, 1+idx2                , &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )
           ENDIF

        enddo
        enddo
      ENDIF
    CASE ( 'ys', 'ye' )
      distributed_field = .FALSE.
      IF ( nproc .GT. 1 ) THEN
        ids = DomainStart(1) ; ide = DomainEnd(1) ; IF ( .NOT. has_char( Stagger, 'y' ) ) ide = ide+1  
        jds = DomainStart(2) ; jde = DomainEnd(2) ; 
        dom_end_rev(1) = ide
        dom_end_rev(2) = jde
        distributed_field = .TRUE.
        IF ( (MemOrd .eq. 'ys' .AND. bdy_mask( P_YSB )) .OR.     &
             (MemOrd .eq. 'ye' .AND. bdy_mask( P_YEB ))       ) THEN
          my_displ = PatchStart(1)-1
          my_count = PatchEnd(1)-PatchStart(1)+1
        ELSE
          my_displ = 0
          my_count = 0
        ENDIF
        CALL mpi_gather( my_displ, 1, MPI_INTEGER, displs, 1, MPI_INTEGER, collective_root, communicator, ierr )
        CALL mpi_gather( my_count, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, collective_root, communicator, ierr )
        do j = DomainStart(2),DomainEnd(2)    
           lx   = MemoryEnd(1)-MemoryStart(1)+1
           idx  = lx*(j-1)
           lx2  = dom_end_rev(1)-DomainStart(1)+1
           idx2 = lx2*(j-1)

           IF ( FieldType .EQ. WRF_DOUBLE ) THEN 

             CALL wrf_gatherv_double( Field, PatchStart(1)-MemoryStart(1)+1+idx ,      &    
                             my_count                       , &    
                             globbuf, 1+idx2                , &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN

             CALL wrf_gatherv_real( Field, PatchStart(1)-MemoryStart(1)+1+idx ,      &    
                             my_count                       , &    
                             globbuf, 1+idx2                , &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

             CALL wrf_gatherv_integer( Field, PatchStart(1)-MemoryStart(1)+1+idx ,      &    
                             my_count                       , &    
                             globbuf, 1+idx2                , &    
                             counts                         , &    
                             displs                         , &    
                             collective_root                , &    
                             communicator                   , &    
                             ierr )
           ENDIF

        enddo
      ENDIF
    CASE DEFAULT
      distributed_field = .FALSE.
  END SELECT
  IF ( wrf_dm_on_monitor() ) THEN
    IF ( distributed_field ) THEN
      CALL fcn ( Hndl , DateStr , VarName , globbuf , FieldType , Comm , IOComm , &
                 DomainDesc , MemoryOrder , Stagger , DimNames ,                  &
                 DomainStart , DomainEnd ,                                        &
                 DomainStart , dom_end_rev ,                                      &  
                 DomainStart , DomainEnd ,                                        &
                 Status )
    ELSE
      CALL fcn ( Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                 DomainDesc , MemoryOrder , Stagger , DimNames ,                  &
                 DomainStart , DomainEnd ,                                        &
                 MemoryStart , MemoryEnd ,                                        &
                 PatchStart  , PatchEnd  ,                                        &
                 Status )
    ENDIF
  ENDIF
  CALL wrf_dm_bcast_bytes( Status , 4 )
  DEALLOCATE( counts )
  DEALLOCATE( displs )
  RETURN
END SUBROUTINE collect_generic_and_call_pkg


SUBROUTINE call_pkg_and_dist (       fcn, donotdist_arg, update_arg,                           &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )








  IMPLICIT NONE
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


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
  EXTERNAL fcn
  LOGICAL,        INTENT(IN)    :: donotdist_arg, update_arg  
  INTEGER ,       INTENT(IN)    :: Hndl
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  INTEGER                          :: Field(*)
  INTEGER                                      :: FieldType
  INTEGER                                      :: Comm
  INTEGER                                      :: IOComm
  INTEGER                                      :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                                :: MemoryOrder
  CHARACTER*(*)                                :: Stagger
  CHARACTER*(*) , dimension (*)                :: DimNames
  INTEGER ,dimension(*)                        :: DomainStart, DomainEnd
  INTEGER ,dimension(*)                        :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)                        :: PatchStart,  PatchEnd
  INTEGER                                      :: Status
  LOGICAL donotdist
  INTEGER ndims, nproc

  CALL dim_from_memorder( MemoryOrder , ndims)
  CALL wrf_get_nproc( nproc )
  donotdist = donotdist_arg .OR. (nproc .EQ. 1)

  IF ( donotdist ) THEN
    CALL fcn ( Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , MemoryOrder , Stagger , DimNames ,                &
               DomainStart , DomainEnd ,                                      &
               MemoryStart , MemoryEnd ,                                      &
               PatchStart , PatchEnd ,                                        &
               Status )

  ELSE IF (FieldType .EQ. WRF_DOUBLE) THEN

     CALL call_pkg_and_dist_double ( fcn, update_arg,                            &
               Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
               DomainStart , DomainEnd ,                                    &
               MemoryStart , MemoryEnd ,                                    &
               PatchStart , PatchEnd ,                                      &
               Status )

  ELSE IF (FieldType .EQ. WRF_FLOAT) THEN

     CALL call_pkg_and_dist_real ( fcn, update_arg,                            &
               Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
               DomainStart , DomainEnd ,                                    &
               MemoryStart , MemoryEnd ,                                    &
               PatchStart , PatchEnd ,                                      &
               Status )

  ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

     CALL call_pkg_and_dist_int ( fcn, update_arg,                            &
               Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
               DomainStart , DomainEnd ,                                    &
               MemoryStart , MemoryEnd ,                                    &
               PatchStart , PatchEnd ,                                      &
               Status )

  ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN

     CALL call_pkg_and_dist_logical ( fcn, update_arg,                            &
               Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
               DomainStart , DomainEnd ,                                    &
               MemoryStart , MemoryEnd ,                                    &
               PatchStart , PatchEnd ,                                      &
               Status )

  ENDIF
  RETURN
END SUBROUTINE call_pkg_and_dist

SUBROUTINE call_pkg_and_dist_real (  fcn, update_arg,                                             &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )








  IMPLICIT NONE
  EXTERNAL fcn
  INTEGER ,       INTENT(IN)    :: Hndl
  LOGICAL ,       INTENT(IN)    :: update_arg
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  REAL    ,       INTENT(INOUT)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(INOUT)   :: Status
  REAL, ALLOCATABLE :: globbuf (:)
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
  INTEGER test
  CHARACTER*128 mess

  IF ( wrf_dm_on_monitor() ) THEN
    ALLOCATE( globbuf( (DomainEnd(1)-DomainStart(1)+3)*(DomainEnd(2)-DomainStart(2)+3)*(DomainEnd(3)-DomainStart(3)+3) ), &
              STAT=test )
    IF ( test .NE. 0 ) THEN
      write(mess,*)"module_io.b",'allocating globbuf ',&
           (DomainEnd(1)-DomainStart(1)+3)*(DomainEnd(2)-DomainStart(2)+3)*(DomainEnd(3)-DomainStart(3)+3)
      CALL wrf_error_fatal3("<stdin>",23521,&
mess)
    ENDIF
  ELSE
    ALLOCATE( globbuf( 1 ), STAT=test )
    IF ( test .NE. 0 ) THEN
      write(mess,*)"module_io.b",'allocating globbuf ',1
      CALL wrf_error_fatal3("<stdin>",23528,&
mess)
    ENDIF
  ENDIF

  globbuf = 0.

  CALL call_pkg_and_dist_generic (   fcn, globbuf  , update_arg,                          &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  DEALLOCATE ( globbuf )
  RETURN
END SUBROUTINE call_pkg_and_dist_real


SUBROUTINE call_pkg_and_dist_double  (  fcn, update_arg ,                                            &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )








  IMPLICIT NONE
  EXTERNAL fcn
  INTEGER ,       INTENT(IN)    :: Hndl
  LOGICAL ,       INTENT(IN)    :: update_arg
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  DOUBLE PRECISION   ,       INTENT(INOUT)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(INOUT)   :: Status
  DOUBLE PRECISION , ALLOCATABLE :: globbuf (:)
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor

  IF ( wrf_dm_on_monitor() ) THEN
    ALLOCATE( globbuf( (DomainEnd(1)-DomainStart(1)+3)*(DomainEnd(2)-DomainStart(2)+3)*(DomainEnd(3)-DomainStart(3)+3) ) )
  ELSE
    ALLOCATE( globbuf( 1 ) )
  ENDIF

  globbuf = 0

  CALL call_pkg_and_dist_generic (   fcn, globbuf  , update_arg ,                         &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  DEALLOCATE ( globbuf )
  RETURN
END SUBROUTINE call_pkg_and_dist_double


SUBROUTINE call_pkg_and_dist_int  (  fcn, update_arg ,                                            &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )








  IMPLICIT NONE
  EXTERNAL fcn
  INTEGER ,       INTENT(IN)    :: Hndl
  LOGICAL ,       INTENT(IN)    :: update_arg
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  INTEGER    ,       INTENT(INOUT)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(INOUT)   :: Status
  INTEGER , ALLOCATABLE :: globbuf (:)
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor

  IF ( wrf_dm_on_monitor() ) THEN
    ALLOCATE( globbuf( (DomainEnd(1)-DomainStart(1)+3)*(DomainEnd(2)-DomainStart(2)+3)*(DomainEnd(3)-DomainStart(3)+3) ) )
  ELSE
    ALLOCATE( globbuf( 1 ) )
  ENDIF

  globbuf = 0

  CALL call_pkg_and_dist_generic (   fcn, globbuf  , update_arg ,                                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  DEALLOCATE ( globbuf )
  RETURN
END SUBROUTINE call_pkg_and_dist_int


SUBROUTINE call_pkg_and_dist_logical  (  fcn, update_arg ,                                            &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )








  IMPLICIT NONE
  EXTERNAL fcn
  INTEGER ,       INTENT(IN)    :: Hndl
  LOGICAL ,       INTENT(IN)    :: update_arg
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  logical    ,       INTENT(INOUT)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(INOUT)   :: Status
  LOGICAL , ALLOCATABLE :: globbuf (:)
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor

  IF ( wrf_dm_on_monitor() ) THEN
    ALLOCATE( globbuf( (DomainEnd(1)-DomainStart(1)+3)*(DomainEnd(2)-DomainStart(2)+3)*(DomainEnd(3)-DomainStart(3)+3) ) )
  ELSE
    ALLOCATE( globbuf( 1 ) )
  ENDIF

  globbuf = .false.

  CALL call_pkg_and_dist_generic (   fcn, globbuf  , update_arg ,                         &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  DEALLOCATE ( globbuf )
  RETURN
END SUBROUTINE call_pkg_and_dist_logical

SUBROUTINE call_pkg_and_dist_generic (   fcn, globbuf , update_arg ,                                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )










  USE module_state_description
  USE module_driver_constants
  USE module_io
  IMPLICIT NONE
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


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
include "mpif.h"

  EXTERNAL fcn
  REAL, DIMENSION(*) ::  globbuf
  INTEGER ,       INTENT(IN)    :: Hndl
  LOGICAL ,       INTENT(IN)    :: update_arg
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  REAL                           :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(OUT)   :: Status
  CHARACTER*3 MemOrd
  LOGICAL, EXTERNAL :: has_char
  INTEGER ids, ide, jds, jde, kds, kde
  INTEGER ims, ime, jms, jme, kms, kme
  INTEGER ips, ipe, jps, jpe, kps, kpe
  INTEGER , dimension(3)                       :: dom_end_rev
  INTEGER memsize
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
  INTEGER, EXTERNAL :: wrf_dm_monitor_rank

  INTEGER lx, lx2, i,j,k ,idx,idx2
  INTEGER my_count, nproc, communicator, ierr, my_displ

  INTEGER, ALLOCATABLE :: counts(:), displs(:)

  LOGICAL distributed_field
  INTEGER collective_root

  CALL lower_case( MemoryOrder, MemOrd )

  collective_root = wrf_dm_monitor_rank()

  CALL wrf_get_nproc( nproc )
  CALL wrf_get_dm_communicator ( communicator )

  ALLOCATE(displs( nproc ))
  ALLOCATE(counts( nproc ))

  dom_end_rev(1) = DomainEnd(1)
  dom_end_rev(2) = DomainEnd(2)
  dom_end_rev(3) = DomainEnd(3)

  SELECT CASE (TRIM(MemOrd))
    CASE (  'xzy' )
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'z' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(3) = dom_end_rev(3) + 1
    CASE (  'zxy' )
      IF ( .NOT. has_char( Stagger, 'z' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(3) = dom_end_rev(3) + 1
    CASE (  'xyz' )
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
      IF ( .NOT. has_char( Stagger, 'z' ) ) dom_end_rev(3) = dom_end_rev(3) + 1
    CASE (  'xy' )
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
    CASE (  'yxz' )
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
      IF ( .NOT. has_char( Stagger, 'z' ) ) dom_end_rev(3) = dom_end_rev(3) + 1
    CASE (  'yx' )
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
    CASE DEFAULT
      
  END SELECT

  data_ordering : SELECT CASE ( model_data_order )
    CASE  ( DATA_ORDER_XYZ )
       ids=DomainStart(1); ide=dom_end_rev(1); jds=DomainStart(2); jde=dom_end_rev(2); kds=DomainStart(3); kde=dom_end_rev(3);
       ims=MemoryStart(1); ime=  MemoryEnd(1); jms=MemoryStart(2); jme=  MemoryEnd(2); kms=MemoryStart(3); kme=  MemoryEnd(3);
       ips= PatchStart(1); ipe=   PatchEnd(1); jps= PatchStart(2); jpe=   PatchEnd(2); kps= PatchStart(3); kpe=   PatchEnd(3);
    CASE  ( DATA_ORDER_YXZ )
       ids=DomainStart(2); ide=dom_end_rev(2); jds=DomainStart(1); jde=dom_end_rev(1); kds=DomainStart(3); kde=dom_end_rev(3);
       ims=MemoryStart(2); ime=  MemoryEnd(2); jms=MemoryStart(1); jme=  MemoryEnd(1); kms=MemoryStart(3); kme=  MemoryEnd(3);
       ips= PatchStart(2); ipe=   PatchEnd(2); jps= PatchStart(1); jpe=   PatchEnd(1); kps= PatchStart(3); kpe=   PatchEnd(3);
    CASE  ( DATA_ORDER_ZXY )
       ids=DomainStart(2); ide=dom_end_rev(2); jds=DomainStart(3); jde=dom_end_rev(3); kds=DomainStart(1); kde=dom_end_rev(1);
       ims=MemoryStart(2); ime=  MemoryEnd(2); jms=MemoryStart(3); jme=  MemoryEnd(3); kms=MemoryStart(1); kme=  MemoryEnd(1);
       ips= PatchStart(2); ipe=   PatchEnd(2); jps= PatchStart(3); jpe=   PatchEnd(3); kps= PatchStart(1); kpe=   PatchEnd(1);
    CASE  ( DATA_ORDER_ZYX )
       ids=DomainStart(3); ide=dom_end_rev(3); jds=DomainStart(2); jde=dom_end_rev(2); kds=DomainStart(1); kde=dom_end_rev(1);
       ims=MemoryStart(3); ime=  MemoryEnd(3); jms=MemoryStart(2); jme=  MemoryEnd(2); kms=MemoryStart(1); kme=  MemoryEnd(1);
       ips= PatchStart(3); ipe=   PatchEnd(3); jps= PatchStart(2); jpe=   PatchEnd(2); kps= PatchStart(1); kpe=   PatchEnd(1);
    CASE  ( DATA_ORDER_XZY )
       ids=DomainStart(1); ide=dom_end_rev(1); jds=DomainStart(3); jde=dom_end_rev(3); kds=DomainStart(2); kde=dom_end_rev(2);
       ims=MemoryStart(1); ime=  MemoryEnd(1); jms=MemoryStart(3); jme=  MemoryEnd(3); kms=MemoryStart(2); kme=  MemoryEnd(2);
       ips= PatchStart(1); ipe=   PatchEnd(1); jps= PatchStart(3); jpe=   PatchEnd(3); kps= PatchStart(2); kpe=   PatchEnd(2);
    CASE  ( DATA_ORDER_YZX )
       ids=DomainStart(3); ide=dom_end_rev(3); jds=DomainStart(1); jde=dom_end_rev(1); kds=DomainStart(2); kde=dom_end_rev(2);
       ims=MemoryStart(3); ime=  MemoryEnd(3); jms=MemoryStart(1); jme=  MemoryEnd(1); kms=MemoryStart(2); kme=  MemoryEnd(2);
       ips= PatchStart(3); ipe=   PatchEnd(3); jps= PatchStart(1); jpe=   PatchEnd(1); kps= PatchStart(2); kpe=   PatchEnd(2);
  END SELECT data_ordering


  SELECT CASE (MemOrd)
    CASE ( 'xzy', 'yzx', 'xyz', 'yxz', 'zxy', 'zyx', 'xy', 'yx' )
      distributed_field = .TRUE.
    CASE ( 'xsz', 'xez', 'xs', 'xe' )
      CALL are_bdys_distributed( distributed_field )
    CASE ( 'ysz', 'yez', 'ys', 'ye' )
      CALL are_bdys_distributed( distributed_field )
    CASE DEFAULT
      
      distributed_field = .FALSE.
  END SELECT

  IF ( distributed_field ) THEN


    IF ( update_arg ) THEN
      SELECT CASE (TRIM(MemOrd))
        CASE (  'xzy','zxy','xyz','yxz','xy','yx' )
          IF ( FieldType .EQ. WRF_DOUBLE ) THEN
            CALL wrf_patch_to_global_double ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
               DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
               MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
               PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
          ELSE IF (  FieldType .EQ. WRF_FLOAT ) THEN
            CALL wrf_patch_to_global_real ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
               DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
               MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
               PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
          ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
            CALL wrf_patch_to_global_integer ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
               DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
               MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
               PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
          ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN
            CALL wrf_patch_to_global_logical ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
               DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
               MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
               PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
          ENDIF
        CASE DEFAULT
      END SELECT
    ENDIF

    IF ( wrf_dm_on_monitor()) THEN
      CALL fcn ( Hndl , DateStr , VarName , globbuf , FieldType , Comm , IOComm , &
                 DomainDesc , MemoryOrder , Stagger , DimNames ,                  &
                 DomainStart , DomainEnd ,                                        &
                 DomainStart , dom_end_rev ,                                        &
                 DomainStart , DomainEnd ,                                          &
                 Status )
    ENDIF

    CALL wrf_dm_bcast_bytes( Status, 4 )

    CALL lower_case( MemoryOrder, MemOrd )


    IF ( TRIM(MemOrd) .EQ. 'xsz' .OR. TRIM(MemOrd) .EQ. 'xez' .OR. &
         TRIM(MemOrd) .EQ. 'xs'  .OR. TRIM(MemOrd) .EQ. 'xe'  .OR. &
         TRIM(MemOrd) .EQ. 'ysz' .OR. TRIM(MemOrd) .EQ. 'yez' .OR. &
         TRIM(MemOrd) .EQ. 'ys'  .OR. TRIM(MemOrd) .EQ. 'ye'    ) THEN

      IF ( TRIM(MemOrd) .EQ. 'xsz' .OR. TRIM(MemOrd) .EQ. 'xez' .OR. &
           TRIM(MemOrd) .EQ. 'xs'  .OR. TRIM(MemOrd) .EQ. 'xe'    ) THEN

       jds=DomainStart(1); jde=dom_end_rev(1); ids=DomainStart(3); ide=dom_end_rev(3); kds=DomainStart(2); kde=dom_end_rev(2);
       jms=MemoryStart(1); jme=  MemoryEnd(1); ims=MemoryStart(3); ime=  MemoryEnd(3); kms=MemoryStart(2); kme=  MemoryEnd(2);
       jps= PatchStart(1); jpe=   PatchEnd(1); ips= PatchStart(3); ipe=   PatchEnd(3); kps= PatchStart(2); kpe=   PatchEnd(2);

        IF ( nproc .GT. 1 ) THEN











          IF ( (MemOrd(1:2) .EQ. 'xs' .AND. bdy_mask( P_XSB )) .OR.     &
               (MemOrd(1:2) .EQ. 'xe' .AND. bdy_mask( P_XEB ))       ) THEN
            my_displ = jps-1         
            my_count = jpe-jps+1
          ELSE
            my_displ = 0
            my_count = 0
          ENDIF

          CALL mpi_gather( my_displ, 1, MPI_INTEGER, displs, 1, MPI_INTEGER, collective_root, communicator, ierr )
          CALL mpi_gather( my_count, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, collective_root, communicator, ierr )

          do i = ips,ipe    
          do k = kds,kde    
             lx   = jme-jms+1
             lx2  = jde-jds+1
             idx  = lx*((k-1)+(i-1)*(kme-kms+1))
             idx2 = lx2*((k-1)+(i-1)*(kde-kds+1))
             IF ( FieldType .EQ. WRF_DOUBLE  ) THEN
               CALL wrf_scatterv_double (                        &
                               globbuf, 1+idx2 ,                &    
                               counts                         , &    
                               Field, jps-jms+1+idx ,       &
                               my_count ,                       &    
                               displs                         , &    
                               collective_root                , &    
                               communicator                   , &    
                               ierr )
             ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN

               CALL wrf_scatterv_real (                          &
                               globbuf, 1+idx2 ,                &    
                               counts                         , &    
                               Field, jps-jms+1+idx ,       &
                               my_count ,                       &    
                               displs                         , &    
                               collective_root                , &    
                               communicator                   , &    
                               ierr )

             ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
               CALL wrf_scatterv_integer (                       &
                               globbuf, 1+idx2 ,                &    
                               counts                         , &    
                               Field, jps-jms+1+idx ,       &
                               my_count ,                       &    
                               displs                         , &    
                               collective_root                , &    
                               communicator                   , &    
                               ierr )
             ENDIF
          enddo
          enddo
        ENDIF
      ENDIF

      IF ( TRIM(MemOrd) .EQ. 'ysz' .OR. TRIM(MemOrd) .EQ. 'yez' .OR. &
           TRIM(MemOrd) .EQ. 'ys'  .OR. TRIM(MemOrd) .EQ. 'ye'    ) THEN


       ids=DomainStart(1); ide=dom_end_rev(1); jds=DomainStart(3); jde=dom_end_rev(3); kds=DomainStart(2); kde=dom_end_rev(2);
       ims=MemoryStart(1); ime=  MemoryEnd(1); jms=MemoryStart(3); jme=  MemoryEnd(3); kms=MemoryStart(2); kme=  MemoryEnd(2);
       ips= PatchStart(1); ipe=   PatchEnd(1); jps= PatchStart(3); jpe=   PatchEnd(3); kps= PatchStart(2); kpe=   PatchEnd(2);

        IF ( nproc .GT. 1 ) THEN

          IF ( (MemOrd(1:2) .EQ. 'ys' .AND. bdy_mask( P_YSB )) .OR.     &
               (MemOrd(1:2) .EQ. 'ye' .AND. bdy_mask( P_YEB ))       ) THEN
            my_displ = ips-1
            my_count = ipe-ips+1
           ELSE
             my_displ = 0
             my_count = 0
          ENDIF

          CALL mpi_gather( my_displ, 1, MPI_INTEGER, displs, 1, MPI_INTEGER, collective_root, communicator, ierr )
          CALL mpi_gather( my_count, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, collective_root, communicator, ierr )

          do j = jds,jde    
          do k = kds,kde    
             lx   = ime-ims+1
             lx2  = ide-ids+1
             idx  = lx*((k-1)+(j-1)*(kme-kms+1))
             idx2 = lx2*((k-1)+(j-1)*(kde-kds+1))

             IF ( FieldType .EQ. WRF_DOUBLE  ) THEN
               CALL wrf_scatterv_double (                        &
                               globbuf, 1+idx2 ,                &    
                               counts                         , &    
                               Field, ips-ims+1+idx ,       &
                               my_count ,                       &    
                               displs                         , &    
                               collective_root                , &    
                               communicator                   , &    
                               ierr )
             ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN
               CALL wrf_scatterv_real (                          &
                               globbuf, 1+idx2 ,                &    
                               counts                         , &    
                               Field, ips-ims+1+idx ,       &
                               my_count ,                       &    
                               displs                         , &    
                               collective_root                , &    
                               communicator                   , &    
                               ierr )
             ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
               CALL wrf_scatterv_integer (                       &
                               globbuf, 1+idx2 ,                &    
                               counts                         , &    
                               Field, ips-ims+1+idx ,       &
                               my_count ,                       &    
                               displs                         , &    
                               collective_root                , &    
                               communicator                   , &    
                               ierr )
             ENDIF
          enddo
          enddo
        ENDIF
      ENDIF

    ELSE  
  
      IF ( FieldType .EQ. WRF_DOUBLE ) THEN

        SELECT CASE (MemOrd)
        CASE ( 'xzy','xyz','yxz','zxy' )
          CALL wrf_global_to_patch_double (  globbuf,  Field  , DomainDesc, Stagger, MemOrd ,    &
             DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
             MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
             PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3)  )
        CASE ( 'xy','yx' )
          CALL wrf_global_to_patch_double (  globbuf, Field ,  DomainDesc, Stagger, MemOrd ,  &
             DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), 1            , 1 , &
             MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), 1            , 1 , &
             PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , 1            , 1   )
        END SELECT

      ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN

        SELECT CASE (MemOrd)
        CASE ( 'xzy','xyz','yxz','zxy' )
          CALL wrf_global_to_patch_real (  globbuf,  Field  , DomainDesc, Stagger, MemOrd ,    &
             DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
             MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
             PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3)  )
        CASE ( 'xy','yx' )
          CALL wrf_global_to_patch_real (  globbuf, Field ,  DomainDesc, Stagger, MemOrd ,  &
             DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), 1            , 1 , &
             MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), 1            , 1 , &
             PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , 1            , 1   )
        END SELECT

      ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

        SELECT CASE (MemOrd)
        CASE ( 'xzy','xyz','yxz','zxy' )
          CALL wrf_global_to_patch_integer (  globbuf,  Field  , DomainDesc, Stagger, MemOrd ,    &
             DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
             MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
             PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3)  )
        CASE ( 'xy','yx' )
          CALL wrf_global_to_patch_integer (  globbuf, Field ,  DomainDesc, Stagger, MemOrd ,  &
             DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), 1            , 1 , &
             MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), 1            , 1 , &
             PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , 1            , 1   )
        END SELECT

      ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN

        SELECT CASE (MemOrd)
        CASE ( 'xzy','xyz','yxz','zxy' )
          CALL wrf_global_to_patch_logical (  globbuf,  Field  , DomainDesc, Stagger, MemOrd ,    &
             DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
             MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
             PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3)  )
        CASE ( 'xy','yx' )
          CALL wrf_global_to_patch_logical (  globbuf, Field ,  DomainDesc, Stagger, MemOrd ,  &
             DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), 1            , 1 , &
             MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), 1            , 1 , &
             PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , 1            , 1   )
        END SELECT

      ENDIF
    ENDIF

  ELSE 

    IF ( wrf_dm_on_monitor()) THEN
      CALL fcn ( Hndl , DateStr , VarName , Field   , FieldType , Comm , IOComm , &
                 DomainDesc , MemoryOrder , Stagger , DimNames ,                  &
                 DomainStart , DomainEnd ,                                        &
                 MemoryStart , MemoryEnd ,                                        &
                 PatchStart  , PatchEnd  ,                                        &
                 Status )
    ENDIF
    CALL wrf_dm_bcast_bytes( Status, 4 )
    memsize = (MemoryEnd(1)-MemoryStart(1)+1)*(MemoryEnd(2)-MemoryStart(2)+1)*(MemoryEnd(3)-MemoryStart(3)+1)
    IF ( FieldType .EQ. WRF_DOUBLE ) THEN 
      CALL wrf_dm_bcast_bytes( Field , 8*memsize )
    ELSE IF ( FieldType .EQ. WRF_FLOAT) THEN
      CALL wrf_dm_bcast_bytes( Field , 4*memsize )
    ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
      CALL wrf_dm_bcast_bytes( Field , 4*memsize )
    ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN
      CALL wrf_dm_bcast_bytes( Field , 4*memsize )
    ENDIF

  ENDIF

  DEALLOCATE(displs)
  DEALLOCATE(counts)
  RETURN
END SUBROUTINE call_pkg_and_dist_generic




SUBROUTINE dim_from_memorder(MemoryOrder,NDim)





  CHARACTER*(*) ,INTENT(IN)  :: MemoryOrder
  INTEGER       ,INTENT(OUT) :: NDim

  CHARACTER*3                :: MemOrd

  CALL Lower_Case(MemoryOrder,MemOrd)
  SELECT CASE (MemOrd)
    CASE ('xyz','xzy','yxz','yzx','zxy','zyx')
      NDim = 3
    CASE ('xy','yx')
      NDim = 2
    CASE ('z','c','0')
      NDim = 1
    CASE DEFAULT
      NDim = 0
      RETURN
  END SELECT
  RETURN
END SUBROUTINE dim_from_memorder

SUBROUTINE lower_case(MemoryOrder,MemOrd)





  CHARACTER*(*) ,INTENT(IN)  :: MemoryOrder
  CHARACTER*(*) ,INTENT(OUT) :: MemOrd

  CHARACTER*1                :: c
  INTEGER       ,PARAMETER   :: upper_to_lower =IACHAR('a')-IACHAR('A')
  INTEGER                    :: i,n,n1

  MemOrd = ' '
  N = len(MemoryOrder)
  N1 = len(MemOrd)
  N = MIN(N,N1)
  MemOrd(1:N) = MemoryOrder(1:N)
  DO i=1,N
    c = MemoryOrder(i:i)
    if('A'<=c .and. c <='Z') MemOrd(i:i)=achar(iachar(c)+upper_to_lower)
  ENDDO
  RETURN
END SUBROUTINE Lower_Case

LOGICAL FUNCTION has_char( str, c )





  IMPLICIT NONE
  CHARACTER*(*) str
  CHARACTER c, d
  CHARACTER*80 str1, str2, str3
  INTEGER i

  CALL lower_case( TRIM(str), str1 )
  str2 = ""
  str2(1:1) = c
  CALL lower_case( str2, str3 )
  d = str3(1:1)
  DO i = 1, LEN(TRIM(str1))
    IF ( str1(i:i) .EQ. d ) THEN
      has_char = .TRUE.
      RETURN
    ENDIF
  ENDDO
  has_char = .FALSE.
  RETURN
END FUNCTION has_char

