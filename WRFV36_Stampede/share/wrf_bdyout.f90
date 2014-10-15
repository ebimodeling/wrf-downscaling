  SUBROUTINE wrf_bdyout ( fid , grid , config_flags, switch , &
                           dryrun, ierr )
    USE module_io
    USE module_wrf_error
    USE module_io_wrf
    USE module_domain
    USE module_state_description
    USE module_configure
    USE module_scalar_tables
    USE module_utility
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

    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(INOUT)    :: config_flags
    INTEGER, INTENT(IN) :: fid, switch
    INTEGER, INTENT(INOUT) :: ierr
    LOGICAL, INTENT(IN) :: dryrun

    
    INTEGER ids , ide , jds , jde , kds , kde , &
            ims , ime , jms , jme , kms , kme , &
            ips , ipe , jps , jpe , kps , kpe

    INTEGER       itrace
    INTEGER , DIMENSION(3) :: domain_start , domain_end
    INTEGER , DIMENSION(3) :: memory_start , memory_end
    INTEGER , DIMENSION(3) :: patch_start , patch_end
    INTEGER i,j
    INTEGER julyr, julday, idt, iswater , map_proj
    REAL    gmt, cen_lat, cen_lon, bdyfrq , truelat1 , truelat2, &
            mp_physics, ra_lw_physics, ra_sw_physics, sf_sfclay_physics, &
            sf_surface_physics, bl_pbl_physics, cu_physics
    REAL    khdif, kvdif
    INTEGER rc

    CHARACTER*256 message
    CHARACTER*80  char_junk
    INTEGER    ibuf(1)
    REAL       rbuf(1)
    CHARACTER*40            :: next_datestr

    CALL get_ijk_from_grid (  grid ,                        &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              ips, ipe, jps, jpe, kps, kpe    )


    
    








CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'U_BXS'               , &  
                       grid%u_bxs(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XSZ'               , &  
                       'X'               , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy x-wind component'               , &  
                       'm s-1'               , &  
'inc/wrf_bdyout.inc ext_write_field U_BXS memorder XSZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'U_BXE'               , &  
                       grid%u_bxe(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XEZ'               , &  
                       'X'               , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy x-wind component'               , &  
                       'm s-1'               , &  
'inc/wrf_bdyout.inc ext_write_field U_BXE memorder XEZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'U_BYS'               , &  
                       grid%u_bys(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YSZ'               , &  
                       'X'               , &  
                       'west_east_stag'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy x-wind component'               , &  
                       'm s-1'               , &  
'inc/wrf_bdyout.inc ext_write_field U_BYS memorder YSZ' , & 
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( ide, ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'U_BYE'               , &  
                       grid%u_bye(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YEZ'               , &  
                       'X'               , &  
                       'west_east_stag'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy x-wind component'               , &  
                       'm s-1'               , &  
'inc/wrf_bdyout.inc ext_write_field U_BYE memorder YEZ' , & 
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( ide, ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'U_BTXS'               , &  
                       grid%u_btxs(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XSZ'               , &  
                       'X'               , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy tend x-wind component'               , &  
                       '(m s-1)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field U_BTXS memorder XSZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'U_BTXE'               , &  
                       grid%u_btxe(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XEZ'               , &  
                       'X'               , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy tend x-wind component'               , &  
                       '(m s-1)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field U_BTXE memorder XEZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'U_BTYS'               , &  
                       grid%u_btys(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YSZ'               , &  
                       'X'               , &  
                       'west_east_stag'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy tend x-wind component'               , &  
                       '(m s-1)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field U_BTYS memorder YSZ' , & 
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( ide, ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'U_BTYE'               , &  
                       grid%u_btye(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YEZ'               , &  
                       'X'               , &  
                       'west_east_stag'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy tend x-wind component'               , &  
                       '(m s-1)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field U_BTYE memorder YEZ' , & 
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( ide, ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'V_BXS'               , &  
                       grid%v_bxs(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XSZ'               , &  
                       'Y'               , &  
                       'south_north_stag'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy y-wind component'               , &  
                       'm s-1'               , &  
'inc/wrf_bdyout.inc ext_write_field V_BXS memorder XSZ' , & 
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( jde, jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'V_BXE'               , &  
                       grid%v_bxe(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XEZ'               , &  
                       'Y'               , &  
                       'south_north_stag'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy y-wind component'               , &  
                       'm s-1'               , &  
'inc/wrf_bdyout.inc ext_write_field V_BXE memorder XEZ' , & 
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( jde, jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'V_BYS'               , &  
                       grid%v_bys(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YSZ'               , &  
                       'Y'               , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy y-wind component'               , &  
                       'm s-1'               , &  
'inc/wrf_bdyout.inc ext_write_field V_BYS memorder YSZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'V_BYE'               , &  
                       grid%v_bye(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YEZ'               , &  
                       'Y'               , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy y-wind component'               , &  
                       'm s-1'               , &  
'inc/wrf_bdyout.inc ext_write_field V_BYE memorder YEZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'V_BTXS'               , &  
                       grid%v_btxs(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XSZ'               , &  
                       'Y'               , &  
                       'south_north_stag'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy tend y-wind component'               , &  
                       '(m s-1)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field V_BTXS memorder XSZ' , & 
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( jde, jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'V_BTXE'               , &  
                       grid%v_btxe(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XEZ'               , &  
                       'Y'               , &  
                       'south_north_stag'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy tend y-wind component'               , &  
                       '(m s-1)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field V_BTXE memorder XEZ' , & 
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( jde, jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'V_BTYS'               , &  
                       grid%v_btys(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YSZ'               , &  
                       'Y'               , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy tend y-wind component'               , &  
                       '(m s-1)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field V_BTYS memorder YSZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'V_BTYE'               , &  
                       grid%v_btye(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YEZ'               , &  
                       'Y'               , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy tend y-wind component'               , &  
                       '(m s-1)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field V_BTYE memorder YEZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'W_BXS'               , &  
                       grid%w_bxs(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XSZ'               , &  
                       'Z'               , &  
                       'south_north'               , &  
                       'bottom_top_stag'               , &  
                       'bdy_width'               , &  
                       'bdy z-wind component'               , &  
                       'm s-1'               , &  
'inc/wrf_bdyout.inc ext_write_field W_BXS memorder XSZ' , & 
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'W_BXE'               , &  
                       grid%w_bxe(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XEZ'               , &  
                       'Z'               , &  
                       'south_north'               , &  
                       'bottom_top_stag'               , &  
                       'bdy_width'               , &  
                       'bdy z-wind component'               , &  
                       'm s-1'               , &  
'inc/wrf_bdyout.inc ext_write_field W_BXE memorder XEZ' , & 
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'W_BYS'               , &  
                       grid%w_bys(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YSZ'               , &  
                       'Z'               , &  
                       'west_east'               , &  
                       'bottom_top_stag'               , &  
                       'bdy_width'               , &  
                       'bdy z-wind component'               , &  
                       'm s-1'               , &  
'inc/wrf_bdyout.inc ext_write_field W_BYS memorder YSZ' , & 
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'W_BYE'               , &  
                       grid%w_bye(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YEZ'               , &  
                       'Z'               , &  
                       'west_east'               , &  
                       'bottom_top_stag'               , &  
                       'bdy_width'               , &  
                       'bdy z-wind component'               , &  
                       'm s-1'               , &  
'inc/wrf_bdyout.inc ext_write_field W_BYE memorder YEZ' , & 
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'W_BTXS'               , &  
                       grid%w_btxs(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XSZ'               , &  
                       'Z'               , &  
                       'south_north'               , &  
                       'bottom_top_stag'               , &  
                       'bdy_width'               , &  
                       'bdy tend z-wind component'               , &  
                       '(m s-1)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field W_BTXS memorder XSZ' , & 
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'W_BTXE'               , &  
                       grid%w_btxe(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XEZ'               , &  
                       'Z'               , &  
                       'south_north'               , &  
                       'bottom_top_stag'               , &  
                       'bdy_width'               , &  
                       'bdy tend z-wind component'               , &  
                       '(m s-1)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field W_BTXE memorder XEZ' , & 
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'W_BTYS'               , &  
                       grid%w_btys(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YSZ'               , &  
                       'Z'               , &  
                       'west_east'               , &  
                       'bottom_top_stag'               , &  
                       'bdy_width'               , &  
                       'bdy tend z-wind component'               , &  
                       '(m s-1)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field W_BTYS memorder YSZ' , & 
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'W_BTYE'               , &  
                       grid%w_btye(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YEZ'               , &  
                       'Z'               , &  
                       'west_east'               , &  
                       'bottom_top_stag'               , &  
                       'bdy_width'               , &  
                       'bdy tend z-wind component'               , &  
                       '(m s-1)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field W_BTYE memorder YEZ' , & 
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PH_BXS'               , &  
                       grid%ph_bxs(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XSZ'               , &  
                       'Z'               , &  
                       'south_north'               , &  
                       'bottom_top_stag'               , &  
                       'bdy_width'               , &  
                       'bdy perturbation geopotential'               , &  
                       'm2 s-2'               , &  
'inc/wrf_bdyout.inc ext_write_field PH_BXS memorder XSZ' , & 
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PH_BXE'               , &  
                       grid%ph_bxe(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XEZ'               , &  
                       'Z'               , &  
                       'south_north'               , &  
                       'bottom_top_stag'               , &  
                       'bdy_width'               , &  
                       'bdy perturbation geopotential'               , &  
                       'm2 s-2'               , &  
'inc/wrf_bdyout.inc ext_write_field PH_BXE memorder XEZ' , & 
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PH_BYS'               , &  
                       grid%ph_bys(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YSZ'               , &  
                       'Z'               , &  
                       'west_east'               , &  
                       'bottom_top_stag'               , &  
                       'bdy_width'               , &  
                       'bdy perturbation geopotential'               , &  
                       'm2 s-2'               , &  
'inc/wrf_bdyout.inc ext_write_field PH_BYS memorder YSZ' , & 
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PH_BYE'               , &  
                       grid%ph_bye(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YEZ'               , &  
                       'Z'               , &  
                       'west_east'               , &  
                       'bottom_top_stag'               , &  
                       'bdy_width'               , &  
                       'bdy perturbation geopotential'               , &  
                       'm2 s-2'               , &  
'inc/wrf_bdyout.inc ext_write_field PH_BYE memorder YEZ' , & 
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PH_BTXS'               , &  
                       grid%ph_btxs(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XSZ'               , &  
                       'Z'               , &  
                       'south_north'               , &  
                       'bottom_top_stag'               , &  
                       'bdy_width'               , &  
                       'bdy tend perturbation geopotential'               , &  
                       '(m2 s-2)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field PH_BTXS memorder XSZ' , & 
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PH_BTXE'               , &  
                       grid%ph_btxe(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XEZ'               , &  
                       'Z'               , &  
                       'south_north'               , &  
                       'bottom_top_stag'               , &  
                       'bdy_width'               , &  
                       'bdy tend perturbation geopotential'               , &  
                       '(m2 s-2)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field PH_BTXE memorder XEZ' , & 
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PH_BTYS'               , &  
                       grid%ph_btys(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YSZ'               , &  
                       'Z'               , &  
                       'west_east'               , &  
                       'bottom_top_stag'               , &  
                       'bdy_width'               , &  
                       'bdy tend perturbation geopotential'               , &  
                       '(m2 s-2)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field PH_BTYS memorder YSZ' , & 
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PH_BTYE'               , &  
                       grid%ph_btye(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YEZ'               , &  
                       'Z'               , &  
                       'west_east'               , &  
                       'bottom_top_stag'               , &  
                       'bdy_width'               , &  
                       'bdy tend perturbation geopotential'               , &  
                       '(m2 s-2)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field PH_BTYE memorder YEZ' , & 
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'T_BXS'               , &  
                       grid%t_bxs(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XSZ'               , &  
                       ''               , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy perturbation potential temperature (theta-t0)'               , &  
                       'K'               , &  
'inc/wrf_bdyout.inc ext_write_field T_BXS memorder XSZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'T_BXE'               , &  
                       grid%t_bxe(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XEZ'               , &  
                       ''               , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy perturbation potential temperature (theta-t0)'               , &  
                       'K'               , &  
'inc/wrf_bdyout.inc ext_write_field T_BXE memorder XEZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'T_BYS'               , &  
                       grid%t_bys(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YSZ'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy perturbation potential temperature (theta-t0)'               , &  
                       'K'               , &  
'inc/wrf_bdyout.inc ext_write_field T_BYS memorder YSZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'T_BYE'               , &  
                       grid%t_bye(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YEZ'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy perturbation potential temperature (theta-t0)'               , &  
                       'K'               , &  
'inc/wrf_bdyout.inc ext_write_field T_BYE memorder YEZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'T_BTXS'               , &  
                       grid%t_btxs(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XSZ'               , &  
                       ''               , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy tend perturbation potential temperature (theta-t0)'               , &  
                       '(K)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field T_BTXS memorder XSZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'T_BTXE'               , &  
                       grid%t_btxe(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XEZ'               , &  
                       ''               , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy tend perturbation potential temperature (theta-t0)'               , &  
                       '(K)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field T_BTXE memorder XEZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'T_BTYS'               , &  
                       grid%t_btys(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YSZ'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy tend perturbation potential temperature (theta-t0)'               , &  
                       '(K)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field T_BTYS memorder YSZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'T_BTYE'               , &  
                       grid%t_btye(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YEZ'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
                       'bdy tend perturbation potential temperature (theta-t0)'               , &  
                       '(K)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field T_BTYE memorder YEZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MU_BXS'               , &  
                       grid%mu_bxs(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XS'               , &  
                       ''               , &  
                       'south_north'               , &  
                       'bdy_width'               , &  
                       'one_element'               , &  
                       'bdy perturbation dry air mass in column'               , &  
                       'Pa'               , &  
'inc/wrf_bdyout.inc ext_write_field MU_BXS memorder XS' , & 
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
jps, MIN( (jde-1), jpe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MU_BXE'               , &  
                       grid%mu_bxe(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XE'               , &  
                       ''               , &  
                       'south_north'               , &  
                       'bdy_width'               , &  
                       'one_element'               , &  
                       'bdy perturbation dry air mass in column'               , &  
                       'Pa'               , &  
'inc/wrf_bdyout.inc ext_write_field MU_BXE memorder XE' , & 
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
jps, MIN( (jde-1), jpe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MU_BYS'               , &  
                       grid%mu_bys(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YS'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'bdy_width'               , &  
                       'one_element'               , &  
                       'bdy perturbation dry air mass in column'               , &  
                       'Pa'               , &  
'inc/wrf_bdyout.inc ext_write_field MU_BYS memorder YS' , & 
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
ips, MIN( (ide-1), ipe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MU_BYE'               , &  
                       grid%mu_bye(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YE'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'bdy_width'               , &  
                       'one_element'               , &  
                       'bdy perturbation dry air mass in column'               , &  
                       'Pa'               , &  
'inc/wrf_bdyout.inc ext_write_field MU_BYE memorder YE' , & 
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
ips, MIN( (ide-1), ipe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MU_BTXS'               , &  
                       grid%mu_btxs(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XS'               , &  
                       ''               , &  
                       'south_north'               , &  
                       'bdy_width'               , &  
                       'one_element'               , &  
                       'bdy tend perturbation dry air mass in column'               , &  
                       '(Pa)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field MU_BTXS memorder XS' , & 
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
jps, MIN( (jde-1), jpe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MU_BTXE'               , &  
                       grid%mu_btxe(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XE'               , &  
                       ''               , &  
                       'south_north'               , &  
                       'bdy_width'               , &  
                       'one_element'               , &  
                       'bdy tend perturbation dry air mass in column'               , &  
                       '(Pa)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field MU_BTXE memorder XE' , & 
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
jps, MIN( (jde-1), jpe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MU_BTYS'               , &  
                       grid%mu_btys(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YS'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'bdy_width'               , &  
                       'one_element'               , &  
                       'bdy tend perturbation dry air mass in column'               , &  
                       '(Pa)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field MU_BTYS memorder YS' , & 
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
ips, MIN( (ide-1), ipe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MU_BTYE'               , &  
                       grid%mu_btye(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YE'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'bdy_width'               , &  
                       'one_element'               , &  
                       'bdy tend perturbation dry air mass in column'               , &  
                       '(Pa)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field MU_BTYE memorder YE' , & 
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
ips, MIN( (ide-1), ipe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
DO itrace = PARAM_FIRST_SCALAR , num_moist
  IF ( moist_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(moist_dname_table( grid%id, itrace )) // '_BXS', & 
          grid%moist_BXS(jms,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XSZ'               , &  
          ''                , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          moist_desc_table( grid%id, itrace  ), & 
          moist_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XSZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(moist_dname_table( grid%id, itrace )) // '_BXE', & 
          grid%moist_BXE(jms,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XEZ'               , &  
          ''                , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          moist_desc_table( grid%id, itrace  ), & 
          moist_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XEZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(moist_dname_table( grid%id, itrace )) // '_BYS', & 
          grid%moist_BYS(ims,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'YSZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          moist_desc_table( grid%id, itrace  ), & 
          moist_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder YSZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(moist_dname_table( grid%id, itrace )) // '_BYE', & 
          grid%moist_BYE(ims,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'YEZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          moist_desc_table( grid%id, itrace  ), & 
          moist_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder YEZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_moist
  IF ( moist_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(moist_dname_table( grid%id, itrace )) // '_BTXS', & 
          grid%moist_BTXS(jms,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XSZ'               , &  
          ''                , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          moist_desc_table( grid%id, itrace  ), & 
          moist_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XSZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(moist_dname_table( grid%id, itrace )) // '_BTXE', & 
          grid%moist_BTXE(jms,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XEZ'               , &  
          ''                , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          moist_desc_table( grid%id, itrace  ), & 
          moist_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XEZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(moist_dname_table( grid%id, itrace )) // '_BTYS', & 
          grid%moist_BTYS(ims,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'YSZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          moist_desc_table( grid%id, itrace  ), & 
          moist_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder YSZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(moist_dname_table( grid%id, itrace )) // '_BTYE', & 
          grid%moist_BTYE(ims,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'YEZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          moist_desc_table( grid%id, itrace  ), & 
          moist_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder YEZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_moist
  IF ( dfi_moist_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BXS', & 
          grid%dfi_moist_BXS(jms,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XSZ'               , &  
          ''                , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          dfi_moist_desc_table( grid%id, itrace  ), & 
          dfi_moist_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder XSZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BXE', & 
          grid%dfi_moist_BXE(jms,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XEZ'               , &  
          ''                , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          dfi_moist_desc_table( grid%id, itrace  ), & 
          dfi_moist_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder XEZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BYS', & 
          grid%dfi_moist_BYS(ims,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'YSZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          dfi_moist_desc_table( grid%id, itrace  ), & 
          dfi_moist_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder YSZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BYE', & 
          grid%dfi_moist_BYE(ims,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'YEZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          dfi_moist_desc_table( grid%id, itrace  ), & 
          dfi_moist_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder YEZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_moist
  IF ( dfi_moist_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BTXS', & 
          grid%dfi_moist_BTXS(jms,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XSZ'               , &  
          ''                , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          dfi_moist_desc_table( grid%id, itrace  ), & 
          dfi_moist_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder XSZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BTXE', & 
          grid%dfi_moist_BTXE(jms,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XEZ'               , &  
          ''                , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          dfi_moist_desc_table( grid%id, itrace  ), & 
          dfi_moist_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder XEZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BTYS', & 
          grid%dfi_moist_BTYS(ims,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'YSZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          dfi_moist_desc_table( grid%id, itrace  ), & 
          dfi_moist_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder YSZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BTYE', & 
          grid%dfi_moist_BTYE(ims,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'YEZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          dfi_moist_desc_table( grid%id, itrace  ), & 
          dfi_moist_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder YEZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_scalar
  IF ( scalar_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BXS', & 
          grid%scalar_BXS(jms,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XSZ'               , &  
          ''                , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          scalar_desc_table( grid%id, itrace  ), & 
          scalar_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XSZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BXE', & 
          grid%scalar_BXE(jms,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XEZ'               , &  
          ''                , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          scalar_desc_table( grid%id, itrace  ), & 
          scalar_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XEZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BYS', & 
          grid%scalar_BYS(ims,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'YSZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          scalar_desc_table( grid%id, itrace  ), & 
          scalar_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder YSZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BYE', & 
          grid%scalar_BYE(ims,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'YEZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          scalar_desc_table( grid%id, itrace  ), & 
          scalar_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder YEZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_scalar
  IF ( scalar_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BTXS', & 
          grid%scalar_BTXS(jms,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XSZ'               , &  
          ''                , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          scalar_desc_table( grid%id, itrace  ), & 
          scalar_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XSZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BTXE', & 
          grid%scalar_BTXE(jms,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XEZ'               , &  
          ''                , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          scalar_desc_table( grid%id, itrace  ), & 
          scalar_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XEZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BTYS', & 
          grid%scalar_BTYS(ims,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'YSZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          scalar_desc_table( grid%id, itrace  ), & 
          scalar_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder YSZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BTYE', & 
          grid%scalar_BTYE(ims,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'YEZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          scalar_desc_table( grid%id, itrace  ), & 
          scalar_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder YEZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_scalar
  IF ( dfi_scalar_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BXS', & 
          grid%dfi_scalar_BXS(jms,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XSZ'               , &  
          ''                , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          dfi_scalar_desc_table( grid%id, itrace  ), & 
          dfi_scalar_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XSZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BXE', & 
          grid%dfi_scalar_BXE(jms,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XEZ'               , &  
          ''                , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          dfi_scalar_desc_table( grid%id, itrace  ), & 
          dfi_scalar_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XEZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BYS', & 
          grid%dfi_scalar_BYS(ims,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'YSZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          dfi_scalar_desc_table( grid%id, itrace  ), & 
          dfi_scalar_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder YSZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BYE', & 
          grid%dfi_scalar_BYE(ims,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'YEZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          dfi_scalar_desc_table( grid%id, itrace  ), & 
          dfi_scalar_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder YEZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_scalar
  IF ( dfi_scalar_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BTXS', & 
          grid%dfi_scalar_BTXS(jms,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XSZ'               , &  
          ''                , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          dfi_scalar_desc_table( grid%id, itrace  ), & 
          dfi_scalar_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XSZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BTXE', & 
          grid%dfi_scalar_BTXE(jms,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XEZ'               , &  
          ''                , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          dfi_scalar_desc_table( grid%id, itrace  ), & 
          dfi_scalar_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XEZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BTYS', & 
          grid%dfi_scalar_BTYS(ims,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'YSZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          dfi_scalar_desc_table( grid%id, itrace  ), & 
          dfi_scalar_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder YSZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BTYE', & 
          grid%dfi_scalar_BTYE(ims,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'YEZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          dfi_scalar_desc_table( grid%id, itrace  ), & 
          dfi_scalar_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder YEZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HT_SHAD_BXS'               , &  
                       grid%ht_shad_bxs(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XS'               , &  
                       ''               , &  
                       'south_north'               , &  
                       'bdy_width'               , &  
                       'one_element'               , &  
                       'bdy Height of orographic shadow'               , &  
                       'm'               , &  
'inc/wrf_bdyout.inc ext_write_field HT_SHAD_BXS memorder XS' , & 
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
jps, MIN( (jde-1), jpe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HT_SHAD_BXE'               , &  
                       grid%ht_shad_bxe(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XE'               , &  
                       ''               , &  
                       'south_north'               , &  
                       'bdy_width'               , &  
                       'one_element'               , &  
                       'bdy Height of orographic shadow'               , &  
                       'm'               , &  
'inc/wrf_bdyout.inc ext_write_field HT_SHAD_BXE memorder XE' , & 
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
jps, MIN( (jde-1), jpe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HT_SHAD_BYS'               , &  
                       grid%ht_shad_bys(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YS'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'bdy_width'               , &  
                       'one_element'               , &  
                       'bdy Height of orographic shadow'               , &  
                       'm'               , &  
'inc/wrf_bdyout.inc ext_write_field HT_SHAD_BYS memorder YS' , & 
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
ips, MIN( (ide-1), ipe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HT_SHAD_BYE'               , &  
                       grid%ht_shad_bye(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YE'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'bdy_width'               , &  
                       'one_element'               , &  
                       'bdy Height of orographic shadow'               , &  
                       'm'               , &  
'inc/wrf_bdyout.inc ext_write_field HT_SHAD_BYE memorder YE' , & 
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
ips, MIN( (ide-1), ipe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HT_SHAD_BTXS'               , &  
                       grid%ht_shad_btxs(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XS'               , &  
                       ''               , &  
                       'south_north'               , &  
                       'bdy_width'               , &  
                       'one_element'               , &  
                       'bdy tend Height of orographic shadow'               , &  
                       '(m)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field HT_SHAD_BTXS memorder XS' , & 
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
jps, MIN( (jde-1), jpe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HT_SHAD_BTXE'               , &  
                       grid%ht_shad_btxe(jms,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'XE'               , &  
                       ''               , &  
                       'south_north'               , &  
                       'bdy_width'               , &  
                       'one_element'               , &  
                       'bdy tend Height of orographic shadow'               , &  
                       '(m)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field HT_SHAD_BTXE memorder XE' , & 
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
jps, MIN( (jde-1), jpe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HT_SHAD_BTYS'               , &  
                       grid%ht_shad_btys(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YS'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'bdy_width'               , &  
                       'one_element'               , &  
                       'bdy tend Height of orographic shadow'               , &  
                       '(m)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field HT_SHAD_BTYS memorder YS' , & 
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
ips, MIN( (ide-1), ipe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HT_SHAD_BTYE'               , &  
                       grid%ht_shad_btye(ims,kds,1)     , &  
                       WRF_FLOAT          , &  
                       grid%communicator , &  
                       grid%iocommunicator , &  
                       grid%domdesc      , &  
                       grid%bdy_mask     , &  
                       dryrun             , &  
                       'YE'               , &  
                       ''               , &  
                       'west_east'               , &  
                       'bdy_width'               , &  
                       'one_element'               , &  
                       'bdy tend Height of orographic shadow'               , &  
                       '(m)/dt'               , &  
'inc/wrf_bdyout.inc ext_write_field HT_SHAD_BTYE memorder YE' , & 
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
ips, MIN( (ide-1), ipe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
DO itrace = PARAM_FIRST_SCALAR , num_tracer
  IF ( tracer_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BXS', & 
          grid%tracer_BXS(jms,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XSZ'               , &  
          ''                , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          tracer_desc_table( grid%id, itrace  ), & 
          tracer_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder XSZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BXE', & 
          grid%tracer_BXE(jms,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XEZ'               , &  
          ''                , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          tracer_desc_table( grid%id, itrace  ), & 
          tracer_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder XEZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BYS', & 
          grid%tracer_BYS(ims,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'YSZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          tracer_desc_table( grid%id, itrace  ), & 
          tracer_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder YSZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BYE', & 
          grid%tracer_BYE(ims,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'YEZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          tracer_desc_table( grid%id, itrace  ), & 
          tracer_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder YEZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_tracer
  IF ( tracer_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BTXS', & 
          grid%tracer_BTXS(jms,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XSZ'               , &  
          ''                , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          tracer_desc_table( grid%id, itrace  ), & 
          tracer_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder XSZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BTXE', & 
          grid%tracer_BTXE(jms,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'XEZ'               , &  
          ''                , &  
                       'south_north'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          tracer_desc_table( grid%id, itrace  ), & 
          tracer_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder XEZ' , & 
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BTYS', & 
          grid%tracer_BTYS(ims,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'YSZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          tracer_desc_table( grid%id, itrace  ), & 
          tracer_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder YSZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BTYE', & 
          grid%tracer_BTYE(ims,kds,1,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          dryrun             , &  
          'YEZ'               , &  
          ''                , &  
                       'west_east'               , &  
                       'bottom_top'               , &  
                       'bdy_width'               , &  
          tracer_desc_table( grid%id, itrace  ), & 
          tracer_units_table( grid%id, itrace  ), & 
'inc/wrf_bdyout.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder YEZ' , & 
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO


    RETURN
    END
