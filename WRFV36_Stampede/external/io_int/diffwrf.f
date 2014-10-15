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
  use read_util_module
  use module_ext_internal
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
! This bit is for backwards compatibility with old variants of these flags 
! that are still being used in io_grib1 and io_phdf5.  It should be removed!  
      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
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
  integer :: WrfType, WrfType2

  integer :: i,j,k
  integer :: levlim
  integer :: cross
  integer :: ndim, ndim2
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

  integer :: l, n
  integer :: ikdiffs, ifdiffs

  real, allocatable, dimension(:,:,:,:) :: data,data2

  integer :: ierr, ierr2, ier, ier2, Status, Status_next_time, Status_next_time2, Status_next_var, Status_next_var_2
  integer :: nargs

  logical :: newtime = .TRUE.
  logical :: justplot, efound

  logical, external :: iveceq

  levlim = -1

  call ext_int_ioinit(' ', Status)
 
  nargs = command_argument_count()

  Justplot = .false.
! get arguments
  if ( nargs .ge. 2 ) then
    call get_command_argument(number=1, value=flnm)
    call get_command_argument(number=2, value=flnm2)
    ierr = 0
    call ext_int_open_for_read( trim(flnm), 0, 0, "", dh1, Status)
    if ( Status /= 0 ) then 
      print*,'error opening ',flnm, ' Status = ', Status ; stop
    endif
    call ext_int_open_for_read( trim(flnm2), 0, 0, "", dh2, Status)
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

   call ext_int_get_next_time(dh1, DateStr, Status_next_time)

   DO WHILE ( Status_next_time .eq. 0 )
    write(*,*)'Next Time ',TRIM(Datestr)
    call ext_int_get_next_var (dh1, VarName, Status_next_var)
    DO WHILE ( Status_next_var .eq. 0 )
!    write(*,*)'Next Var |',TRIM(VarName),'|'

      start_index = 1
      end_index = 1
      call ext_int_get_var_info (dh1,VarName,ndim,ordering,staggering,start_index,end_index, WrfType, ierr )
      if(WrfType /= WRF_REAL .AND. WrfType /= WRF_DOUBLE) then
        call ext_int_get_next_var (dh1, VarName, Status_next_var)
        cycle
      endif

      write(*,'(A9,1x,I1,3(1x,I5),1x,A,1x,A)')&
               VarName, ndim, end_index(1), end_index(2), end_index(3), &
               trim(ordering), trim(DateStr)

      if ( VarName .eq. name ) then
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

        call ext_int_read_field(dh1,DateStr,TRIM(name),data,WRF_REAL,0,0,0,ord,   &
                            staggering,          &
                            dimnames,         &
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


        IF ( ndim .eq. 3 ) THEN
        do k = start_index(2), end_index(2)
          if ( levlim .eq. -1 .or. k .eq. levlim ) then
            write(88,*)end_index(1),end_index(3),' ',trim(name),' ',k,' time ',n
            do j = 1, end_index(3)
              do i = 1, end_index(1)
                write(88,*) data(i,k,j,1)
              enddo
            enddo
          endif
        enddo
        ELSE IF ( ndim .eq. 2 ) THEN
            k = 1
            write(88,*)end_index(1),end_index(2),' ',trim(name),' ',k,' time ',n
            do j = 1, end_index(2)
              do i = 1, end_index(1)
                write(88,*) data(i,j,1,1)
              enddo
            enddo
        ENDIF
        deallocate(data)
      endif
      call ext_int_get_next_var (dh1, VarName, Status_next_var)
    enddo
    call ext_int_get_next_time(dh1, DateStr, Status_next_time)
  enddo
else
  write (6,FMT='(4A)') 'Diffing ',trim(flnm),' ',trim(flnm2)

  call ext_int_get_next_time(dh1, DateStr, Status_next_time)
  call ext_int_get_next_time(dh2, DateStr2, Status_next_time2)

  IF ( DateStr .NE. DateStr2 ) THEN
    print*,'They differ big time.  Dates do not match'
    print*,'   ',flnm,' ',DateStr
    print*,'   ',flnm2,' ',DateStr2
    Status_next_time = 1
  ENDIF

  DO WHILE ( Status_next_time .eq. 0 .AND. Status_next_time2 .eq. 0 )
!write(*,*)'Next Time ',TRIM(Datestr)
    print 76
    call ext_int_get_next_var (dh1, VarName, Status_next_var)
    call ext_int_get_next_var (dh2, VarName, Status_next_var)
    DO WHILE ( Status_next_var .eq. 0 )

!write(*,*)'Next Var |',TRIM(VarName),'|'

      start_index = 1
      end_index = 1
      call ext_int_get_var_info (dh1,VarName,ndim,ordering,staggering,start_index,end_index, WrfType, ierr )
      call ext_int_get_var_info (dh2,VarName,ndim2,ordering2,staggering2,start_index2,end_index2, WrfType2, ierr2 )

!write(*,*)'ierr , err2 ',TRIM(VarName), ierr , ierr2, ' ',ordering, ' ', ordering2

      IF ( ierr /= 0 ) THEN
        write(*,*)'Big difference: ',VarName,' not found in ',flnm2
        GOTO 1234
      ENDIF
      IF ( ndim /= ndim2 ) THEN
        write(*,*)'Big difference: Number of dimensions for ',Varname,' differs in ',flnm2,'(',ndim,') /= (',ndim2
        GOTO 1234
      ENDIF

      IF ( WrfType /= WrfType2 ) THEN
        write(*,*)'Big difference: The types do not match', WrfType, WrfType2
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

!      write(*,'(A9,1x,I1,3(1x,I3),1x,A,1x,A)')&
!               VarName, ndim, end_index(1), end_index(2), end_index(3), &
!               trim(ordering), trim(DateStr)

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

        call ext_int_read_field(dh1,DateStr,TRIM(VarName),data,WRF_REAL,0,0,0,ord,   &
                            staggering,          &
                            dimnames,         &
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

        call ext_int_read_field(dh2,DateStr,TRIM(VarName),data2,WRF_REAL,0,0,0,ord,   &
                            staggering,          &
                            dimnames,         &
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
if(n.eq.0) n=1
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

        digits = 0
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
1234  CONTINUE
      call ext_int_get_next_var (dh1, VarName, Status_next_var)
      call ext_int_get_next_var (dh2, VarName, Status_next_var)
    enddo
    call ext_int_get_next_time(dh1, DateStr, Status_next_time)
    call ext_int_get_next_time(dh2, DateStr2, Status_next_time2)
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


! stubs for routines called by module_wrf_error (used by this implementation of IO api)
SUBROUTINE wrf_abort
  STOP
END SUBROUTINE wrf_abort

logical function wrf_dm_on_monitor()
  wrf_dm_on_monitor=.true.
end function wrf_dm_on_monitor

SUBROUTINE get_current_time_string( time_str )
  CHARACTER(LEN=*), INTENT(OUT) :: time_str
  time_str = ''
END SUBROUTINE get_current_time_string

SUBROUTINE get_current_grid_name( grid_str )
  CHARACTER(LEN=*), INTENT(OUT) :: grid_str
  grid_str = ''
END SUBROUTINE get_current_grid_name

