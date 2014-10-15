module read_util_module

contains

   subroutine arguments(v2file, lmore)
     implicit none
     character(len=*) :: v2file
     character(len=120) :: harg
     logical :: lmore
   
     integer :: ierr, i, numarg
   
     numarg = command_argument_count()
   
     i = 1
     lmore = .false.
   
     do while ( i < numarg) 
        call get_command_argument(number=i, value=harg)
        print*, 'harg = ', trim(harg)
   
        if (harg == "-v") then
           i = i + 1
           lmore = .true.
        elseif (harg == "-h") then
           call help
        endif
   
     enddo
   
     call get_command_argument(number=i, value=harg)
     v2file = harg
   end subroutine arguments
   
   subroutine help
     implicit none
     character(len=120) :: cmd
     call get_command_argument(number=0, value=cmd)
   
     write(*,'(/,"Usage: ", A, " [-v] v2file ")') trim(cmd)
     write(*,'(8x, "-v     : Print extra info")')
     write(*,'(8x, "v3file : MM5v3 file name to read.")')
     write(*,'(8x, "-h     : print this help message and exit.",/)')
     stop
   end subroutine help
end module read_util_module



 program readv3
  use wrf_data
  use read_util_module
  implicit none
  
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

!     NetCDF-3.
!
! netcdf version 3 fortran interface:
!

!
! external netcdf data types:
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double

      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)

!
! default fill values:
!
      integer           nf_fill_byte
      integer           nf_fill_int1
      integer           nf_fill_char
      integer           nf_fill_short
      integer           nf_fill_int2
      integer           nf_fill_int
      real              nf_fill_float
      real              nf_fill_real
      doubleprecision   nf_fill_double

      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690d+36)

!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit

      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)

!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)

!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)

!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims

      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)

!
! error codes:
!
      integer nf_noerr
      integer nf_ebadid
      integer nf_eexist
      integer nf_einval
      integer nf_eperm
      integer nf_enotindefine
      integer nf_eindefine
      integer nf_einvalcoords
      integer nf_emaxdims
      integer nf_enameinuse
      integer nf_enotatt
      integer nf_emaxatts
      integer nf_ebadtype
      integer nf_ebaddim
      integer nf_eunlimpos
      integer nf_emaxvars
      integer nf_enotvar
      integer nf_eglobal
      integer nf_enotnc
      integer nf_ests
      integer nf_emaxname
      integer nf_eunlimit
      integer nf_enorecvars
      integer nf_echar
      integer nf_eedge
      integer nf_estride
      integer nf_ebadname
      integer nf_erange
      integer nf_enomem
      integer nf_evarsize
      integer nf_edimsize
      integer nf_etrunc

      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
!
! error handling modes:
!
      integer  nf_fatal
      integer nf_verbose

      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)

!
! miscellaneous routines:
!
      character*80   nf_inq_libvers
      external       nf_inq_libvers

      character*80   nf_strerror
!                         (integer             ncerr)
      external       nf_strerror

      logical        nf_issyserr
!                         (integer             ncerr)
      external       nf_issyserr

!
! control routines:
!
      integer         nf_inq_base_pe
!                         (integer             ncid,
!                          integer             pe)
      external        nf_inq_base_pe

      integer         nf_set_base_pe
!                         (integer             ncid,
!                          integer             pe)
      external        nf_set_base_pe

      integer         nf_create
!                         (character*(*)       path,
!                          integer             cmode,
!                          integer             ncid)
      external        nf_create

      integer         nf__create
!                         (character*(*)       path,
!                          integer             cmode,
!                          integer             initialsz,
!                          integer             chunksizehint,
!                          integer             ncid)
      external        nf__create

      integer         nf__create_mp
!                         (character*(*)       path,
!                          integer             cmode,
!                          integer             initialsz,
!                          integer             basepe,
!                          integer             chunksizehint,
!                          integer             ncid)
      external        nf__create_mp

      integer         nf_open
!                         (character*(*)       path,
!                          integer             mode,
!                          integer             ncid)
      external        nf_open

      integer         nf__open
!                         (character*(*)       path,
!                          integer             mode,
!                          integer             chunksizehint,
!                          integer             ncid)
      external        nf__open

      integer         nf__open_mp
!                         (character*(*)       path,
!                          integer             mode,
!                          integer             basepe,
!                          integer             chunksizehint,
!                          integer             ncid)
      external        nf__open_mp

      integer         nf_set_fill
!                         (integer             ncid,
!                          integer             fillmode,
!                          integer             old_mode)
      external        nf_set_fill

      integer         nf_set_default_format
!                          (integer             format,
!                          integer             old_format)
      external        nf_set_default_format

      integer         nf_redef
!                         (integer             ncid)
      external        nf_redef

      integer         nf_enddef
!                         (integer             ncid)
      external        nf_enddef

      integer         nf__enddef
!                         (integer             ncid,
!                          integer             h_minfree,
!                          integer             v_align,
!                          integer             v_minfree,
!                          integer             r_align)
      external        nf__enddef

      integer         nf_sync
!                         (integer             ncid)
      external        nf_sync

      integer         nf_abort
!                         (integer             ncid)
      external        nf_abort

      integer         nf_close
!                         (integer             ncid)
      external        nf_close

      integer         nf_delete
!                         (character*(*)       ncid)
      external        nf_delete

!
! general inquiry routines:
!

      integer         nf_inq
!                         (integer             ncid,
!                          integer             ndims,
!                          integer             nvars,
!                          integer             ngatts,
!                          integer             unlimdimid)
      external        nf_inq

      integer         nf_inq_ndims
!                         (integer             ncid,
!                          integer             ndims)
      external        nf_inq_ndims

      integer         nf_inq_nvars
!                         (integer             ncid,
!                          integer             nvars)
      external        nf_inq_nvars

      integer         nf_inq_natts
!                         (integer             ncid,
!                          integer             ngatts)
      external        nf_inq_natts

      integer         nf_inq_unlimdim
!                         (integer             ncid,
!                          integer             unlimdimid)
      external        nf_inq_unlimdim

      integer         nf_inq_format
!                         (integer             ncid,
!                          integer             format)
      external        nf_inq_format

!
! dimension routines:
!

      integer         nf_def_dim
!                         (integer             ncid,
!                          character(*)        name,
!                          integer             len,
!                          integer             dimid)
      external        nf_def_dim

      integer         nf_inq_dimid
!                         (integer             ncid,
!                          character(*)        name,
!                          integer             dimid)
      external        nf_inq_dimid

      integer         nf_inq_dim
!                         (integer             ncid,
!                          integer             dimid,
!                          character(*)        name,
!                          integer             len)
      external        nf_inq_dim

      integer         nf_inq_dimname
!                         (integer             ncid,
!                          integer             dimid,
!                          character(*)        name)
      external        nf_inq_dimname

      integer         nf_inq_dimlen
!                         (integer             ncid,
!                          integer             dimid,
!                          integer             len)
      external        nf_inq_dimlen

      integer         nf_rename_dim
!                         (integer             ncid,
!                          integer             dimid,
!                          character(*)        name)
      external        nf_rename_dim

!
! general attribute routines:
!

      integer         nf_inq_att
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len)
      external        nf_inq_att

      integer         nf_inq_attid
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             attnum)
      external        nf_inq_attid

      integer         nf_inq_atttype
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype)
      external        nf_inq_atttype

      integer         nf_inq_attlen
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             len)
      external        nf_inq_attlen

      integer         nf_inq_attname
!                         (integer             ncid,
!                          integer             varid,
!                          integer             attnum,
!                          character(*)        name)
      external        nf_inq_attname

      integer         nf_copy_att
!                         (integer             ncid_in,
!                          integer             varid_in,
!                          character(*)        name,
!                          integer             ncid_out,
!                          integer             varid_out)
      external        nf_copy_att

      integer         nf_rename_att
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        curname,
!                          character(*)        newname)
      external        nf_rename_att

      integer         nf_del_att
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name)
      external        nf_del_att

!
! attribute put/get routines:
!

      integer         nf_put_att_text
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             len,
!                          character(*)        text)
      external        nf_put_att_text

      integer         nf_get_att_text
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          character(*)        text)
      external        nf_get_att_text

      integer         nf_put_att_int1
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          nf_int1_t           i1vals(1))
      external        nf_put_att_int1

      integer         nf_get_att_int1
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          nf_int1_t           i1vals(1))
      external        nf_get_att_int1

      integer         nf_put_att_int2
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          nf_int2_t           i2vals(1))
      external        nf_put_att_int2

      integer         nf_get_att_int2
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          nf_int2_t           i2vals(1))
      external        nf_get_att_int2

      integer         nf_put_att_int
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          integer             ivals(1))
      external        nf_put_att_int

      integer         nf_get_att_int
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             ivals(1))
      external        nf_get_att_int

      integer         nf_put_att_real
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          real                rvals(1))
      external        nf_put_att_real

      integer         nf_get_att_real
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          real                rvals(1))
      external        nf_get_att_real

      integer         nf_put_att_double
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          double              dvals(1))
      external        nf_put_att_double

      integer         nf_get_att_double
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          double              dvals(1))
      external        nf_get_att_double

!
! general variable routines:
!

      integer         nf_def_var
!                         (integer             ncid,
!                          character(*)        name,
!                          integer             datatype,
!                          integer             ndims,
!                          integer             dimids(1),
!                          integer             varid)
      external        nf_def_var

      integer         nf_inq_var
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             datatype,
!                          integer             ndims,
!                          integer             dimids(1),
!                          integer             natts)
      external        nf_inq_var

      integer         nf_inq_varid
!                         (integer             ncid,
!                          character(*)        name,
!                          integer             varid)
      external        nf_inq_varid

      integer         nf_inq_varname
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name)
      external        nf_inq_varname

      integer         nf_inq_vartype
!                         (integer             ncid,
!                          integer             varid,
!                          integer             xtype)
      external        nf_inq_vartype

      integer         nf_inq_varndims
!                         (integer             ncid,
!                          integer             varid,
!                          integer             ndims)
      external        nf_inq_varndims

      integer         nf_inq_vardimid
!                         (integer             ncid,
!                          integer             varid,
!                          integer             dimids(1))
      external        nf_inq_vardimid

      integer         nf_inq_varnatts
!                         (integer             ncid,
!                          integer             varid,
!                          integer             natts)
      external        nf_inq_varnatts

      integer         nf_rename_var
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name)
      external        nf_rename_var

      integer         nf_copy_var
!                         (integer             ncid_in,
!                          integer             varid,
!                          integer             ncid_out)
      external        nf_copy_var

!
! entire variable put/get routines:
!

      integer         nf_put_var_text
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        text)
      external        nf_put_var_text

      integer         nf_get_var_text
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        text)
      external        nf_get_var_text

      integer         nf_put_var_int1
!                         (integer             ncid,
!                          integer             varid,
!                          nf_int1_t           i1vals(1))
      external        nf_put_var_int1

      integer         nf_get_var_int1
!                         (integer             ncid,
!                          integer             varid,
!                          nf_int1_t           i1vals(1))
      external        nf_get_var_int1

      integer         nf_put_var_int2
!                         (integer             ncid,
!                          integer             varid,
!                          nf_int2_t           i2vals(1))
      external        nf_put_var_int2

      integer         nf_get_var_int2
!                         (integer             ncid,
!                          integer             varid,
!                          nf_int2_t           i2vals(1))
      external        nf_get_var_int2

      integer         nf_put_var_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             ivals(1))
      external        nf_put_var_int

      integer         nf_get_var_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             ivals(1))
      external        nf_get_var_int

      integer         nf_put_var_real
!                         (integer             ncid,
!                          integer             varid,
!                          real                rvals(1))
      external        nf_put_var_real

      integer         nf_get_var_real
!                         (integer             ncid,
!                          integer             varid,
!                          real                rvals(1))
      external        nf_get_var_real

      integer         nf_put_var_double
!                         (integer             ncid,
!                          integer             varid,
!                          doubleprecision     dvals(1))
      external        nf_put_var_double

      integer         nf_get_var_double
!                         (integer             ncid,
!                          integer             varid,
!                          doubleprecision     dvals(1))
      external        nf_get_var_double

!
! single variable put/get routines:
!

      integer         nf_put_var1_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          character*1         text)
      external        nf_put_var1_text

      integer         nf_get_var1_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          character*1         text)
      external        nf_get_var1_text

      integer         nf_put_var1_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          nf_int1_t           i1val)
      external        nf_put_var1_int1

      integer         nf_get_var1_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          nf_int1_t           i1val)
      external        nf_get_var1_int1

      integer         nf_put_var1_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          nf_int2_t           i2val)
      external        nf_put_var1_int2

      integer         nf_get_var1_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          nf_int2_t           i2val)
      external        nf_get_var1_int2

      integer         nf_put_var1_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          integer             ival)
      external        nf_put_var1_int

      integer         nf_get_var1_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          integer             ival)
      external        nf_get_var1_int

      integer         nf_put_var1_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          real                rval)
      external        nf_put_var1_real

      integer         nf_get_var1_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          real                rval)
      external        nf_get_var1_real

      integer         nf_put_var1_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          doubleprecision     dval)
      external        nf_put_var1_double

      integer         nf_get_var1_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          doubleprecision     dval)
      external        nf_get_var1_double

!
! variable array put/get routines:
!

      integer         nf_put_vara_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          character(*)        text)
      external        nf_put_vara_text

      integer         nf_get_vara_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          character(*)        text)
      external        nf_get_vara_text

      integer         nf_put_vara_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          nf_int1_t           i1vals(1))
      external        nf_put_vara_int1

      integer         nf_get_vara_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          nf_int1_t           i1vals(1))
      external        nf_get_vara_int1

      integer         nf_put_vara_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          nf_int2_t           i2vals(1))
      external        nf_put_vara_int2

      integer         nf_get_vara_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          nf_int2_t           i2vals(1))
      external        nf_get_vara_int2

      integer         nf_put_vara_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             ivals(1))
      external        nf_put_vara_int

      integer         nf_get_vara_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             ivals(1))
      external        nf_get_vara_int

      integer         nf_put_vara_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          real                rvals(1))
      external        nf_put_vara_real

      integer         nf_get_vara_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          real                rvals(1))
      external        nf_get_vara_real

      integer         nf_put_vara_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          doubleprecision     dvals(1))
      external        nf_put_vara_double

      integer         nf_get_vara_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          doubleprecision     dvals(1))
      external        nf_get_vara_double

!
! strided variable put/get routines:
!

      integer         nf_put_vars_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          character(*)        text)
      external        nf_put_vars_text

      integer         nf_get_vars_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          character(*)        text)
      external        nf_get_vars_text

      integer         nf_put_vars_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          nf_int1_t           i1vals(1))
      external        nf_put_vars_int1

      integer         nf_get_vars_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          nf_int1_t           i1vals(1))
      external        nf_get_vars_int1

      integer         nf_put_vars_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          nf_int2_t           i2vals(1))
      external        nf_put_vars_int2

      integer         nf_get_vars_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          nf_int2_t           i2vals(1))
      external        nf_get_vars_int2

      integer         nf_put_vars_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             ivals(1))
      external        nf_put_vars_int

      integer         nf_get_vars_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             ivals(1))
      external        nf_get_vars_int

      integer         nf_put_vars_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          real                rvals(1))
      external        nf_put_vars_real

      integer         nf_get_vars_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          real                rvals(1))
      external        nf_get_vars_real

      integer         nf_put_vars_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          doubleprecision     dvals(1))
      external        nf_put_vars_double

      integer         nf_get_vars_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          doubleprecision     dvals(1))
      external        nf_get_vars_double

!
! mapped variable put/get routines:
!

      integer         nf_put_varm_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          character(*)        text)
      external        nf_put_varm_text

      integer         nf_get_varm_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          character(*)        text)
      external        nf_get_varm_text

      integer         nf_put_varm_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          nf_int1_t           i1vals(1))
      external        nf_put_varm_int1

      integer         nf_get_varm_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          nf_int1_t           i1vals(1))
      external        nf_get_varm_int1

      integer         nf_put_varm_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          nf_int2_t           i2vals(1))
      external        nf_put_varm_int2

      integer         nf_get_varm_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          nf_int2_t           i2vals(1))
      external        nf_get_varm_int2

      integer         nf_put_varm_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          integer             ivals(1))
      external        nf_put_varm_int

      integer         nf_get_varm_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          integer             ivals(1))
      external        nf_get_varm_int

      integer         nf_put_varm_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          real                rvals(1))
      external        nf_put_varm_real

      integer         nf_get_varm_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          real                rvals(1))
      external        nf_get_varm_real

      integer         nf_put_varm_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          doubleprecision     dvals(1))
      external        nf_put_varm_double

      integer         nf_get_varm_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          doubleprecision     dvals(1))
      external        nf_get_varm_double


!     NetCDF-2.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!

!      
! functions in the fortran interface
!
      integer nccre
      integer ncopn
      integer ncddef
      integer ncdid
      integer ncvdef
      integer ncvid
      integer nctlen
      integer ncsfil

      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil


      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool


!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble

      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)

!     
!     masks for the struct nc flag field; passed in as 'mode' arg to
!     nccreate and ncopen.
!     

!     read/write, 0 => readonly 
      parameter(ncrdwr = 1)
!     in create phase, cleared by ncendef 
      parameter(nccreat = 2)
!     on create destroy existing file 
      parameter(ncexcl = 4)
!     in define mode, cleared by ncendef 
      parameter(ncindef = 8)
!     synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
!     synchronise whole header on change (x'20')
      parameter(nchsync = 32)
!     numrecs has changed (x'40')
      parameter(ncndirty = 64)  
!     header info has changed (x'80')
      parameter(nchdirty = 128)
!     prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
!     do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
!     isa link (x'8000')
      parameter(nclink = 32768)

!     
!     'mode' arguments for nccreate and ncopen
!     
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)

!     
!     'size' argument to ncdimdef for an unlimited dimension
!     
      integer ncunlim
      parameter(ncunlim = 0)

!     
!     attribute id to put/get a global attribute
!     
      parameter(ncglobal  = 0)

!     
!     advisory maximums:
!     
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
!     not enforced 
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)

!     
!     global netcdf error status variable
!     initialized in error.c
!     

!     no error 
      parameter(ncnoerr = nf_noerr)
!     not a netcdf id 
      parameter(ncebadid = nf_ebadid)
!     too many netcdfs open 
      parameter(ncenfile = -31)   ! nc_syserr
!     netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
!     invalid argument 
      parameter(nceinval = nf_einval)
!     write to read only 
      parameter(nceperm = nf_eperm)
!     operation not allowed in data mode 
      parameter(ncenotin = nf_enotindefine )   
!     operation not allowed in define mode 
      parameter(nceindef = nf_eindefine)   
!     coordinates out of domain 
      parameter(ncecoord = nf_einvalcoords)
!     maxncdims exceeded 
      parameter(ncemaxds = nf_emaxdims)
!     string match to name in use 
      parameter(ncename = nf_enameinuse)   
!     attribute not found 
      parameter(ncenoatt = nf_enotatt)
!     maxncattrs exceeded 
      parameter(ncemaxat = nf_emaxatts)
!     not a netcdf data type 
      parameter(ncebadty = nf_ebadtype)
!     invalid dimension id 
      parameter(ncebadd = nf_ebaddim)
!     ncunlimited in the wrong index 
      parameter(nceunlim = nf_eunlimpos)
!     maxncvars exceeded 
      parameter(ncemaxvs = nf_emaxvars)
!     variable not found 
      parameter(ncenotvr = nf_enotvar)
!     action prohibited on ncglobal varid 
      parameter(nceglob = nf_eglobal)
!     not a netcdf file 
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname) 
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)

!     
!     global options variable. used to determine behavior of error handler.
!     initialized in lerror.c
!     
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)

!
!     default fill values.  these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub

      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690d+36)

!     NetCDF-4.
!     This is part of netCDF-4. Copyright 2006, UCAR, See COPYRIGHT
!     file for distribution information.

!     Netcdf version 4 fortran interface.

!     $Id: netcdf4.inc,v 1.28 2010/05/25 13:53:02 ed Exp $

!     New netCDF-4 types.
      integer nf_ubyte
      integer nf_ushort
      integer nf_uint
      integer nf_int64
      integer nf_uint64
      integer nf_string
      integer nf_vlen
      integer nf_opaque
      integer nf_enum
      integer nf_compound

      parameter (nf_ubyte = 7)
      parameter (nf_ushort = 8)
      parameter (nf_uint = 9)
      parameter (nf_int64 = 10)
      parameter (nf_uint64 = 11)
      parameter (nf_string = 12)
      parameter (nf_vlen = 13)
      parameter (nf_opaque = 14)
      parameter (nf_enum = 15)
      parameter (nf_compound = 16)

!     New netCDF-4 fill values.
      integer           nf_fill_ubyte
      integer           nf_fill_ushort
!      real              nf_fill_uint
!      real              nf_fill_int64
!      real              nf_fill_uint64
      parameter (nf_fill_ubyte = 255)
      parameter (nf_fill_ushort = 65535)

!     New constants.
      integer nf_format_netcdf4
      parameter (nf_format_netcdf4 = 3)

      integer nf_format_netcdf4_classic
      parameter (nf_format_netcdf4_classic = 4)

      integer nf_netcdf4
      parameter (nf_netcdf4 = 4096)

      integer nf_classic_model
      parameter (nf_classic_model = 256)

      integer nf_chunk_seq
      parameter (nf_chunk_seq = 0)
      integer nf_chunk_sub
      parameter (nf_chunk_sub = 1)
      integer nf_chunk_sizes
      parameter (nf_chunk_sizes = 2)

      integer nf_endian_native
      parameter (nf_endian_native = 0)
      integer nf_endian_little
      parameter (nf_endian_little = 1)
      integer nf_endian_big
      parameter (nf_endian_big = 2)

!     For NF_DEF_VAR_CHUNKING
      integer nf_chunked
      parameter (nf_chunked = 0)
      integer nf_contiguous
      parameter (nf_contiguous = 1)

!     For NF_DEF_VAR_FLETCHER32
      integer nf_nochecksum
      parameter (nf_nochecksum = 0)
      integer nf_fletcher32
      parameter (nf_fletcher32 = 1)

!     For NF_DEF_VAR_DEFLATE
      integer nf_noshuffle
      parameter (nf_noshuffle = 0)
      integer nf_shuffle
      parameter (nf_shuffle = 1)

!     For NF_DEF_VAR_SZIP
      integer nf_szip_ec_option_mask
      parameter (nf_szip_ec_option_mask = 4)
      integer nf_szip_nn_option_mask
      parameter (nf_szip_nn_option_mask = 32)

!     For parallel I/O.
      integer nf_mpiio      
      parameter (nf_mpiio = 8192)
      integer nf_mpiposix
      parameter (nf_mpiposix = 16384)
      integer nf_pnetcdf
      parameter (nf_pnetcdf = 32768)

!     For NF_VAR_PAR_ACCESS.
      integer nf_independent
      parameter (nf_independent = 0)
      integer nf_collective
      parameter (nf_collective = 1)

!     New error codes.
      integer nf_ehdferr        ! Error at HDF5 layer. 
      parameter (nf_ehdferr = -101)
      integer nf_ecantread      ! Can't read. 
      parameter (nf_ecantread = -102)
      integer nf_ecantwrite     ! Can't write. 
      parameter (nf_ecantwrite = -103)
      integer nf_ecantcreate    ! Can't create. 
      parameter (nf_ecantcreate = -104)
      integer nf_efilemeta      ! Problem with file metadata. 
      parameter (nf_efilemeta = -105)
      integer nf_edimmeta       ! Problem with dimension metadata. 
      parameter (nf_edimmeta = -106)
      integer nf_eattmeta       ! Problem with attribute metadata. 
      parameter (nf_eattmeta = -107)
      integer nf_evarmeta       ! Problem with variable metadata. 
      parameter (nf_evarmeta = -108)
      integer nf_enocompound    ! Not a compound type. 
      parameter (nf_enocompound = -109)
      integer nf_eattexists     ! Attribute already exists. 
      parameter (nf_eattexists = -110)
      integer nf_enotnc4        ! Attempting netcdf-4 operation on netcdf-3 file.   
      parameter (nf_enotnc4 = -111)
      integer nf_estrictnc3     ! Attempting netcdf-4 operation on strict nc3 netcdf-4 file.   
      parameter (nf_estrictnc3 = -112)
      integer nf_enotnc3        ! Attempting netcdf-3 operation on netcdf-4 file.   
      parameter (nf_enotnc3 = -113)
      integer nf_enopar         ! Parallel operation on file opened for non-parallel access.   
      parameter (nf_enopar = -114)
      integer nf_eparinit       ! Error initializing for parallel access.   
      parameter (nf_eparinit = -115)
      integer nf_ebadgrpid      ! Bad group ID.   
      parameter (nf_ebadgrpid = -116)
      integer nf_ebadtypid      ! Bad type ID.   
      parameter (nf_ebadtypid = -117)
      integer nf_etypdefined    ! Type has already been defined and may not be edited. 
      parameter (nf_etypdefined = -118)
      integer nf_ebadfield      ! Bad field ID.   
      parameter (nf_ebadfield = -119)
      integer nf_ebadclass      ! Bad class.   
      parameter (nf_ebadclass = -120)
      integer nf_emaptype       ! Mapped access for atomic types only.   
      parameter (nf_emaptype = -121)
      integer nf_elatefill      ! Attempt to define fill value when data already exists. 
      parameter (nf_elatefill = -122)
      integer nf_elatedef       ! Attempt to define var properties, like deflate, after enddef. 
      parameter (nf_elatedef = -123)
      integer nf_edimscale      ! Probem with HDF5 dimscales. 
      parameter (nf_edimscale = -124)
      integer nf_enogrp       ! No group found.
      parameter (nf_enogrp = -125)


!     New functions.

!     Parallel I/O.
      integer nf_create_par
      external nf_create_par

      integer nf_open_par
      external nf_open_par

      integer nf_var_par_access
      external nf_var_par_access

!     Functions to handle groups.
      integer nf_inq_ncid
      external nf_inq_ncid

      integer nf_inq_grps
      external nf_inq_grps

      integer nf_inq_grpname
      external nf_inq_grpname

      integer nf_inq_grpname_full
      external nf_inq_grpname_full

      integer nf_inq_grpname_len
      external nf_inq_grpname_len

      integer nf_inq_grp_parent
      external nf_inq_grp_parent

      integer nf_inq_grp_ncid
      external nf_inq_grp_ncid

      integer nf_inq_grp_full_ncid
      external nf_inq_grp_full_ncid

      integer nf_inq_varids
      external nf_inq_varids

      integer nf_inq_dimids
      external nf_inq_dimids

      integer nf_def_grp
      external nf_def_grp

!     New options for netCDF variables.
      integer nf_def_var_deflate
      external nf_def_var_deflate

      integer nf_inq_var_deflate
      external nf_inq_var_deflate

      integer nf_def_var_fletcher32
      external nf_def_var_fletcher32

      integer nf_inq_var_fletcher32
      external nf_inq_var_fletcher32

      integer nf_def_var_chunking
      external nf_def_var_chunking

      integer nf_inq_var_chunking
      external nf_inq_var_chunking

      integer nf_def_var_fill
      external nf_def_var_fill

      integer nf_inq_var_fill
      external nf_inq_var_fill

      integer nf_def_var_endian
      external nf_def_var_endian

      integer nf_inq_var_endian
      external nf_inq_var_endian

!     User defined types.
      integer nf_inq_typeids
      external nf_inq_typeids

      integer nf_inq_typeid
      external nf_inq_typeid

      integer nf_inq_type
      external nf_inq_type

      integer nf_inq_user_type
      external nf_inq_user_type

!     User defined types - compound types.
      integer nf_def_compound
      external nf_def_compound

      integer nf_insert_compound
      external nf_insert_compound

      integer nf_insert_array_compound
      external nf_insert_array_compound

      integer nf_inq_compound
      external nf_inq_compound

      integer nf_inq_compound_name
      external nf_inq_compound_name

      integer nf_inq_compound_size
      external nf_inq_compound_size

      integer nf_inq_compound_nfields
      external nf_inq_compound_nfields

      integer nf_inq_compound_field
      external nf_inq_compound_field

      integer nf_inq_compound_fieldname
      external nf_inq_compound_fieldname

      integer nf_inq_compound_fieldindex
      external nf_inq_compound_fieldindex

      integer nf_inq_compound_fieldoffset
      external nf_inq_compound_fieldoffset

      integer nf_inq_compound_fieldtype
      external nf_inq_compound_fieldtype

      integer nf_inq_compound_fieldndims
      external nf_inq_compound_fieldndims

      integer nf_inq_compound_fielddim_sizes
      external nf_inq_compound_fielddim_sizes

!     User defined types - variable length arrays.
      integer nf_def_vlen
      external nf_def_vlen

      integer nf_inq_vlen
      external nf_inq_vlen

      integer nf_free_vlen
      external nf_free_vlen

!     User defined types - enums.
      integer nf_def_enum
      external nf_def_enum

      integer nf_insert_enum
      external nf_insert_enum

      integer nf_inq_enum
      external nf_inq_enum

      integer nf_inq_enum_member
      external nf_inq_enum_member

      integer nf_inq_enum_ident
      external nf_inq_enum_ident

!     User defined types - opaque.
      integer nf_def_opaque
      external nf_def_opaque

      integer nf_inq_opaque
      external nf_inq_opaque

!     Write and read attributes of any type, including user defined
!     types.
      integer nf_put_att
      external nf_put_att
      integer nf_get_att
      external nf_get_att

!     Write and read variables of any type, including user defined
!     types.
      integer nf_put_var
      external nf_put_var
      integer nf_put_var1
      external nf_put_var1
      integer nf_put_vara
      external nf_put_vara
      integer nf_put_vars
      external nf_put_vars
      integer nf_get_var
      external nf_get_var
      integer nf_get_var1
      external nf_get_var1
      integer nf_get_vara
      external nf_get_vara
      integer nf_get_vars
      external nf_get_vars

!     64-bit int functions.
      integer nf_put_var1_int64
      external nf_put_var1_int64
      integer nf_put_vara_int64
      external nf_put_vara_int64
      integer nf_put_vars_int64
      external nf_put_vars_int64
      integer nf_put_varm_int64
      external nf_put_varm_int64
      integer nf_put_var_int64
      external nf_put_var_int64
      integer nf_get_var1_int64
      external nf_get_var1_int64
      integer nf_get_vara_int64
      external nf_get_vara_int64
      integer nf_get_vars_int64
      external nf_get_vars_int64
      integer nf_get_varm_int64
      external nf_get_varm_int64
      integer nf_get_var_int64
      external nf_get_var_int64

!     For helping F77 users with VLENs.
      integer nf_get_vlen_element
      external nf_get_vlen_element
      integer nf_put_vlen_element
      external nf_put_vlen_element

!     For dealing with file level chunk cache.
      integer nf_set_chunk_cache
      external nf_set_chunk_cache
      integer nf_get_chunk_cache
      external nf_get_chunk_cache

!     For dealing with per variable chunk cache.
      integer nf_set_var_chunk_cache
      external nf_set_var_chunk_cache
      integer nf_get_var_chunk_cache
      external nf_get_var_chunk_cache
  character(len=255) :: flnm
  character(len=255) :: flnm2
  character(len=120) :: arg3
  character(len=19) :: DateStr
  character(len=19) :: DateStr2
  character(len=31) :: VarName
  character(len=31) :: VarName2
  integer dh1, dh2

  integer :: flag, flag2
  integer :: iunit, iunit2

  integer :: i,j,k
  integer :: levlim
  integer :: cross
  integer :: ndim, ndim2
  integer :: WrfType, WrfType2
  real :: time, time2
  real*8 :: a, b
  real*8 :: sumE, sum1, sum2, diff1, diff2, serr, perr, rmse, rms1, rms2, tmp1, tmp2
  integer digits,d1, d2
  integer, dimension(4) :: start_index, end_index, start_index2, end_index2
  integer , Dimension(3) :: MemS,MemE,PatS,PatE
  character (len= 4) :: staggering,   staggering2
  character (len= 3) :: ordering,     ordering2, ord
  character (len=24) :: start_date,   start_date2
  character (len=24) :: current_date, current_date2
  character (len=31) :: name,         name2,  tmpname
  character (len=25) :: units,        units2
  character (len=46) :: description,  description2

  character (len=80), dimension(3)  ::  dimnames
  character (len=80) :: SysDepInfo

  logical :: first, searchcoords
  integer :: l, n, ntimes
  integer :: ikdiffs, ifdiffs
  integer :: icenter, prev_icenter, jcenter, prev_jcenter,ntries
  real :: searchlat, searchlong

  real, allocatable, dimension(:,:,:,:) :: data,data2
  real, allocatable, dimension(:,:)     :: xlat,xlong

  integer :: ierr, ierr2, ier, ier2, Status, Status_next_time, Status_next_time2, Status_next_var, Status_next_var_2
  integer :: nargs

  logical :: newtime = .TRUE.
  logical :: justplot, efound

  logical, external :: iveceq

  levlim = -1

  call ext_ncd_ioinit(SysDepInfo,Status)
  call set_wrf_debug_level ( 1 )

  nargs = command_argument_count()

  Justplot = .false.
  searchcoords = .false.
! get arguments
  if ( nargs .ge. 2 ) then
    call get_command_argument(number=1, value=flnm)
    call get_command_argument(number=2, value=flnm2)
    IF ( flnm2(1:4) .EQ. '-lat' ) THEN
print*,'reading ',TRIM(flnm2(5:))
      read(flnm2(5:),*)searchlat
      call get_command_argument(number=3, value=flnm2)
      IF ( flnm2(1:5) .EQ. '-long' ) THEN
print*,'reading ',TRIM(flnm2(6:))
        read(flnm2(6:),*)searchlong
      ELSE
        write(*,*)'missing -long argument (no spaces after -lat or -long, either)'
        STOP
      ENDIF
      nargs = 0
      Justplot = .true.
      searchcoords = .true.
      call ext_ncd_open_for_read( trim(flnm), 0, 0, "", dh1, Status)
      goto 924
    ENDIF
    ierr = 0
    call ext_ncd_open_for_read( trim(flnm), 0, 0, "", dh1, Status)
    if ( Status /= 0 ) then 
      print*,'error opening ',flnm, ' Status = ', Status ; stop 
    endif
    call ext_ncd_open_for_read( trim(flnm2), 0, 0, "", dh2, Status)
    if ( Status /= 0 ) go to 923
    goto 924
923    continue

! bounce here if second name is not openable -- this would mean that
! it is a field name instead.

    print*,'could not open ',flnm2
    name = flnm2
    Justplot = .true.
924    continue
  if ( nargs .eq. 3 ) then
    call get_command_argument(number=3, value=arg3)
    read(arg3,*)levlim
    print*,'LEVLIM = ',LEVLIM
  endif
  else
     print*,'Usage: command file1 file2'
     stop
  endif

print*,'Just plot ',Justplot

if ( Justplot ) then
  print*, 'flnm = ', trim(flnm)
  first = .TRUE.

  call ext_ncd_get_next_time(dh1, DateStr, Status_next_time)

  ntimes = 0
  DO WHILE ( Status_next_time .eq. 0 )
    write(*,*)'Next Time ',TRIM(Datestr)
    ntimes = ntimes + 1
    call ext_ncd_get_next_var (dh1, VarName, Status_next_var)
    DO WHILE ( Status_next_var .eq. 0 )
!    write(*,*)'Next Var |',TRIM(VarName),'|'

      start_index = 1
      end_index = 1
      call ext_ncd_get_var_info (dh1,VarName,ndim,ordering,staggering,start_index,end_index, WrfType, ierr )
      if(WrfType /= WRF_REAL .AND. WrfType /= WRF_DOUBLE) then 
        call ext_ncd_get_next_var (dh1, VarName, Status_next_var) 
        cycle 
      endif 
      IF ( .NOT. searchcoords ) THEN
        write(*,'(A9,1x,I1,3(1x,I5),1x,A,1x,A)')&
                 VarName, ndim, end_index(1), end_index(2), end_index(3), &
                 trim(ordering), trim(DateStr)
      ENDIF

      if ( VarName .eq. name .OR. TRIM(VarName) .EQ. 'XLAT' .OR. TRIM(VarName) .EQ. 'XLONG' ) then
        write(*,*)'Writing fort.88 file for ', trim(name)

        allocate(data(end_index(1), end_index(2), end_index(3), 1))

        if ( ndim .eq. 3 ) then
          ord = 'XYZ'
        else if ( ndim .eq. 2 ) then
          ord = 'XY'
        else if ( ndim .eq. 1 ) then
          ord = 'Z'
        else if ( ndim .eq. 0 ) then
          ord = '0'
        endif

        call ext_ncd_read_field(dh1,DateStr,TRIM(VarName),data,WRF_REAL,0,0,0,ord, &
                            staggering, dimnames ,                      &
                            start_index,end_index,                      & !dom
                            start_index,end_index,                      & !mem
                            start_index,end_index,                      & !pat
                            ierr)

        if ( ierr/=0 ) then
             write(*,*)'error reading data record'
             write(*,*)'  ndim = ', ndim
             write(*,*)'  end_index(1) ',end_index(1)
             write(*,*)'  end_index(2) ',end_index(2)
             write(*,*)'  end_index(3) ',end_index(3)
        endif

write(*,*)'name: ',TRIM(VarName)
        IF ( TRIM(VarName) .EQ. 'XLAT' .AND. .NOT. ALLOCATED(xlat)) THEN
write(*,*)'allocating xlat'
           ALLOCATE(xlat(end_index(1), end_index(2)))
           xlat = data(:,:,1,1)
        ENDIF
        IF ( TRIM(VarName) .EQ. 'XLONG' .AND. .NOT. ALLOCATED(xlong)) THEN
write(*,*)'allocating xlong'
           ALLOCATE(xlong(end_index(1), end_index(2)))
           xlong = data(:,:,1,1)
        ENDIF


        if ( VarName .eq. name ) then
! give k-slices 
        do k = start_index(3), end_index(3)
          if ( levlim .eq. -1 .or. k .eq. levlim ) then
            write(88,*)end_index(1),end_index(2),' ',trim(name),' ',k,' time ',TRIM(Datestr)
            do j = 1, end_index(2)
              do i = 1, end_index(1)
                write(88,*) data(i,j,k,1)
              enddo
            enddo
          endif
        enddo
        endif

        deallocate(data)
      endif
      call ext_ncd_get_next_var (dh1, VarName, Status_next_var)
      IF ( ntimes .EQ. 1 .AND. ALLOCATED(xlong) .AND. ALLOCATED(xlat) .AND. first ) THEN
        first = .FALSE.
        icenter = 1 
        jcenter = 1
        ntries = 0
        prev_icenter = 0 
        prev_jcenter = 0  
        DO WHILE ( ntries .LT. 10 .AND. (icenter .NE. prev_icenter .OR. jcenter .NE. prev_jcenter ))
          prev_icenter = icenter
          prev_jcenter = jcenter
          DO j = start_index(2), end_index(2)-1
            IF ( xlat(icenter,j) .LE. searchlat .AND. searchlat .LT. xlat(icenter,j+1) ) THEN
              jcenter = j
!write(*,*)'xlat ',ntries,icenter,jcenter,xlat(icenter,j),searchlat
              exit
            ENDIF
          ENDDO
          DO i = start_index(1), end_index(1)-1
            IF ( xlong(i,jcenter) .LE. searchlong .AND. searchlong .LT. xlong(i+1,jcenter)) THEN
              icenter = i
!write(*,*)'xlon ',ntries,icenter,jcenter,xlong(i,jcenter),searchlong
              exit
            ENDIF
          ENDDO
          ntries = ntries + 1
        ENDDO
        write(*,*)'Lon ',searchlong,' Lat ',searchlat,' : ',icenter,jcenter
        write(*,*)'Coordinates at that point ',xlong(icenter,jcenter),xlat(icenter,jcenter)
        write(*,*)'Coordinates at next point ',xlong(icenter+1,jcenter+1),xlat(icenter+1,jcenter+1)
        write(*,*)'Ntries : ',ntries
        if ( ntries .GE. 10 ) write(*,*)'max tries exceeded. Probably did not find'
      ENDIF
    enddo
    call ext_ncd_get_next_time(dh1, DateStr, Status_next_time)
  enddo
else
  write (6,FMT='(4A)') 'Diffing ',trim(flnm),' ',trim(flnm2)

  call ext_ncd_get_next_time(dh1, DateStr, Status_next_time)
  call ext_ncd_get_next_time(dh2, DateStr2, Status_next_time2)

  IF ( DateStr .NE. DateStr2 ) THEN
    print*,'They differ big time.  Dates do not match'
    print*,'   ',flnm,' ',DateStr
    print*,'   ',flnm2,' ',DateStr2
    Status_next_time = 1
  ENDIF

  DO WHILE ( Status_next_time .eq. 0 .AND. Status_next_time2 .eq. 0 )
    write(*,*)'Next Time ',TRIM(Datestr)
    print 76
    call ext_ncd_get_next_var (dh1, VarName, Status_next_var)
    DO WHILE ( Status_next_var .eq. 0 )
!    write(*,*)'Next Var |',TRIM(VarName),'|'

      start_index = 1
      end_index = 1
      start_index2 = 1
      end_index2 = 1

      call ext_ncd_get_var_info (dh1,VarName,ndim,ordering,staggering,start_index,end_index, WrfType, ierr )
      call ext_ncd_get_var_info (dh2,VarName,ndim2,ordering2,staggering2,start_index2,end_index2, WrfType2, ierr )
      IF ( ierr /= 0 ) THEN
        write(*,*)'Big difference: ',VarName,' not found in ',flnm2
        GOTO 1234
      ENDIF
      IF ( ndim /= ndim2 ) THEN
        write(*,*)'Big difference: Number of dimensions for ',Varname,' differs in ',flnm2,'(',ndim,') /= (',ndim2
        GOTO 1234
      ENDIF
      IF ( WrfType /= WrfType2 ) THEN
        write(*,*)'Big difference: The types do not match'
        GOTO 1234
      ENDIF
      if( WrfType == WRF_REAL) then
        DO i = 1, ndim
          IF ( end_index(i) /= end_index2(i) ) THEN
            write(*,*)'Big difference: dim ',i,' lengths differ for ',Varname,' differ in ',flnm2
            GOTO 1234
          ENDIF
        ENDDO
        DO i = ndim+1,3
          start_index(i) = 1
          end_index(i) = 1
          start_index2(i) = 1
          end_index2(i) = 1
        ENDDO

!        write(*,'(A9,1x,I1,3(1x,I3),1x,A,1x,A)')&
!                 VarName, ndim, end_index(1), end_index(2), end_index(3), &
!                 trim(ordering), trim(DateStr)

        allocate(data (end_index(1), end_index(2), end_index(3), 1))
        allocate(data2(end_index(1), end_index(2), end_index(3), 1))

        if ( ndim .eq. 3 ) then
          ord = 'XYZ'
        else if ( ndim .eq. 2 ) then
          ord = 'XY'
        else if ( ndim .eq. 1 ) then
          ord = 'Z'
        else if ( ndim .eq. 0 ) then
          ord = '0'
        endif

        call ext_ncd_read_field(dh1,DateStr,TRIM(VarName),data,WRF_REAL,0,0,0,ord,&
                            staggering, dimnames ,                      &
                            start_index,end_index,                      & !dom 
                            start_index,end_index,                      & !mem
                            start_index,end_index,                      & !pat
                            ierr)

        IF ( ierr /= 0 ) THEN
          write(*,*)'Error reading ',Varname,' from ',flnm
          write(*,*)'  ndim = ', ndim
          write(*,*)'  end_index(1) ',end_index(1)
          write(*,*)'  end_index(2) ',end_index(2)
          write(*,*)'  end_index(3) ',end_index(3)
        ENDIF
        call ext_ncd_read_field(dh2,DateStr,TRIM(VarName),data2,WRF_REAL,0,0,0,ord,&
                            staggering, dimnames ,                      &
                            start_index,end_index,                      & !dom 
                            start_index,end_index,                      & !mem
                            start_index,end_index,                      & !pat
                            ierr)
        IF ( ierr /= 0 ) THEN
          write(*,*)'Error reading ',Varname,' from ',flnm2
          write(*,*)'  ndim = ', ndim
          write(*,*)'  end_index(1) ',end_index(1)
          write(*,*)'  end_index(2) ',end_index(2)
          write(*,*)'  end_index(3) ',end_index(3)
        ENDIF

        IFDIFFS=0
        sumE = 0.0
        sum1 = 0.0
        sum2 = 0.0
        diff1 = 0.0
        diff2 = 0.0
        n = 0 
        DO K = 1,end_index(3)-start_index(3)+1
         IF (LEVLIM.EQ.-1.OR.K.EQ.LEVLIM.OR.NDIM.eq.2) THEN
          cross = 0 
          IKDIFFS = 0
          do i = 1, end_index(1)-cross
            do j = 1, end_index(2)-cross
              a = data(I,J,K,1)
              b = data2(I,J,K,1)
              ! borrowed from  Thomas Oppe's comp program
              sumE = sumE + ( a - b ) * ( a - b )
              sum1 = sum1 + a * a
              sum2 = sum2 + b * b
              diff1 = max ( diff1 , abs ( a - b ) )
              diff2 = max ( diff2 , abs ( b ) )
              n = n + 1
              IF (a .ne. b) then
                IKDIFFS = IKDIFFS + 1
                IFDIFFS = IFDIFFS + 1
              ENDIF
            ENDDO
          ENDDO
         ENDIF
        enddo
        rmsE = sqrt ( sumE / dble( n ) )
        rms1 = sqrt ( sum1 / dble( n ) )
        rms2 = sqrt ( sum2 / dble( n ) )
        serr = 0.0
        IF ( sum2 .GT. 0.0d0 ) THEN
          serr = sqrt ( sumE / sum2 )
        ELSE
          IF ( sumE .GT. 0.0d0 ) serr = 1.0
        ENDIF
        perr = 0.0
        IF ( diff2 .GT. 0.0d0 ) THEN
          perr = diff1/diff2
        ELSE
          IF ( diff1 .GT. 0.0d0 ) perr = 1.0
        ENDIF

        IF ( rms1 - rms2 .EQ. 0.0d0 ) THEN
          digits = 15
        ELSE
          IF ( rms2 .NE. 0 ) THEN
            tmp1 = 1.0d0/( ( abs( rms1 - rms2 ) ) / rms2 )
            IF ( tmp1 .NE. 0 ) THEN
              digits = log10(tmp1)
            ENDIF
          ENDIF
        ENDIF

        IF (IFDIFFS .NE. 0 ) THEN
           ! create the fort.88 and fort.98 files because regression scripts will
           ! look for these to see if there were differences.
           write(88,*)trim(varname)
           write(98,*)trim(varname)
           PRINT 77,trim(varname), IFDIFFS, ndim, rms1, rms2, digits, rmsE, perr
 76 FORMAT (5x,'Field ',2x,'Ndifs',4x,'Dims ',6x,'RMS (1)',12x,'RMS (2)',5x,'DIGITS',4x,'RMSE',5x,'pntwise max')
 77 FORMAT ( A10,1x,I9,2x,I3,1x,e18.10,1x,e18.10,1x,i3,1x,e12.4,1x,e12.4 )
        ENDIF
        deallocate(data)
        deallocate(data2)

      endif
 1234 CONTINUE
      call ext_ncd_get_next_var (dh1, VarName, Status_next_var)
    enddo
    call ext_ncd_get_next_time(dh1, DateStr, Status_next_time)
    call ext_ncd_get_next_time(dh2, DateStr2, Status_next_time2)
    IF ( DateStr .NE. DateStr2 ) THEN
      print*,'They differ big time.  Dates do not match'
      print*,'They differ big time.  Dates do not match'
      print*,'   ',flnm,' ',DateStr
      print*,'   ',flnm2,' ',DateStr2
      Status_next_time = 1
    ENDIF
  enddo

endif

end program readv3

logical function wrf_dm_on_monitor()
  wrf_dm_on_monitor=.true.
end function wrf_dm_on_monitor

logical function iveceq( a, b, n )
  implicit none
  integer n
  integer a(n), b(n)
  integer i
  iveceq = .true.
  do i = 1,n
    if ( a(i) .ne. b(i) ) iveceq = .false.
  enddo
  return
end function iveceq

! stubs for routines called by module_wrf_error (used by netcdf implementation of IO api)
SUBROUTINE wrf_abort
  STOP
END SUBROUTINE wrf_abort

SUBROUTINE get_current_time_string( time_str )
  CHARACTER(LEN=*), INTENT(OUT) :: time_str
  time_str = ''
END SUBROUTINE get_current_time_string

SUBROUTINE get_current_grid_name( grid_str )
  CHARACTER(LEN=*), INTENT(OUT) :: grid_str
  grid_str = ''
END SUBROUTINE get_current_grid_name

