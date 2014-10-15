


MODULE module_wrf_error
  INTEGER           :: wrf_debug_level = 0
  CHARACTER*256     :: wrf_err_message

  
  
  
  
  
  

  integer, save :: silence=0




  
  
  
  
  
  
  
  
  




  integer, PARAMETER :: buffered=0 


  
  
  

  
  


  integer :: stderrlog=1 




  INTEGER, PARAMETER :: wrf_log_flush=0, wrf_log_set_buffer_size=1, &
                        wrf_log_write=2

  
  








  integer, parameter :: min_allowed_buffer_size=200

!$OMP THREADPRIVATE (wrf_err_message)
CONTAINS



  LOGICAL FUNCTION wrf_at_debug_level ( level )
    IMPLICIT NONE
    INTEGER , INTENT(IN) :: level
    wrf_at_debug_level = ( level .LE. wrf_debug_level )
    RETURN
  END FUNCTION wrf_at_debug_level



  SUBROUTINE init_module_wrf_error(on_io_server)
    IMPLICIT NONE
    LOGICAL,OPTIONAL,INTENT(IN) :: on_io_server

    LOGICAL, EXTERNAL :: wrf_dm_on_monitor

    LOGICAL :: compute_slaves_silent
    LOGICAL :: io_servers_silent
    INTEGER :: buffer_size,iostat,stderr_logging
    namelist /logging/ buffer_size,compute_slaves_silent, &
                       io_servers_silent,stderr_logging

    
    

    
    compute_slaves_silent=.false.
    io_servers_silent=.false.
    buffer_size=0

    
    
    

    stderr_logging=1




    
    
    OPEN(unit=27, file="namelist.input", form="formatted", status="old")
    READ(27,nml=logging,iostat=iostat)
    if(iostat /= 0) then

       CALL wrf_debug ( 1 , 'Namelist logging not found in namelist.input. ' )
       CALL wrf_debug ( 1 , ' --> Using registry defaults for variables in logging.' )







       close(27)
       return
    endif
    CLOSE(27)

    if(buffer_size>=min_allowed_buffer_size) then
       write(0,*) 'Forcing disabling of buffering due to compile-time configuration.'
       write(6,*) 'Forcing disabling of buffering due to compile-time configuration.'
    endif

    stderrlog=stderr_logging
    if(buffered/=0 .and. stderrlog/=0) then
       write(0,*) 'Disabling stderr logging since buffering is enabled.'
       write(6,*) 'Disabling stderr logging since buffering is enabled.'
       stderrlog=0
    endif

    silence=0
    if(present(on_io_server)) then
       if(on_io_server) then
          if(io_servers_silent) &
               silence=1
          return
       endif
    endif
    if(compute_slaves_silent) then
       if(wrf_dm_on_monitor()) then
          silence=0
       else
          silence=1
       endif
    endif
  END SUBROUTINE init_module_wrf_error

END MODULE module_wrf_error









SUBROUTINE wrf_message( str )
  use module_wrf_error, only: silence, buffered, stderrlog, wrf_log_write
  IMPLICIT NONE

  CHARACTER*(*) str
  if(silence/=0) return
  if(buffered/=0) then
  else
!$OMP MASTER
     if(stderrlog/=0) then
        write(0,*) trim(str)
     endif
     print *,trim(str)
!$OMP END MASTER
  endif

END SUBROUTINE wrf_message








SUBROUTINE wrf_message2( str )
  IMPLICIT NONE
  CHARACTER*(*) str
!$OMP MASTER
  write(0,*) str
!$OMP END MASTER
END SUBROUTINE wrf_message2



SUBROUTINE wrf_error_fatal3( file_str, line, str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) file_str
  INTEGER , INTENT (IN) :: line  
  CHARACTER*(*) str
  CHARACTER*256 :: line_str

  write(line_str,'(i6)') line

  
  

  CALL wrf_message( '-------------- FATAL CALLED ---------------' )
  
  IF ( line > 0 ) THEN
    CALL wrf_message( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
  CALL wrf_message( str )
  CALL wrf_message( '-------------------------------------------' )

  force_stderr: if(stderrlog==0) then
  CALL wrf_message2( '-------------- FATAL CALLED ---------------' )
  
  IF ( line > 0 ) THEN
        CALL wrf_message2( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
     CALL wrf_message2( trim(str) )
  CALL wrf_message2( '-------------------------------------------' )
  endif force_stderr

  
  flush(6)
  flush(0)


  CALL wrf_abort
END SUBROUTINE wrf_error_fatal3



SUBROUTINE wrf_error_fatal( str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) str
  CALL wrf_error_fatal3 ( ' ', 0, str )
END SUBROUTINE wrf_error_fatal





SUBROUTINE wrf_check_error( expected, actual, str, file_str, line )
  USE module_wrf_error
  IMPLICIT NONE
  INTEGER , INTENT (IN) :: expected
  INTEGER , INTENT (IN) :: actual
  CHARACTER*(*) str
  CHARACTER*(*) file_str
  INTEGER , INTENT (IN) :: line
  CHARACTER (LEN=512)   :: rc_str
  CHARACTER (LEN=512)   :: str_with_rc

  IF ( expected .ne. actual ) THEN
    WRITE (rc_str,*) '  Routine returned error code = ',actual
    str_with_rc = TRIM(str // rc_str)
    CALL wrf_error_fatal3 ( file_str, line, str_with_rc )
  ENDIF
END SUBROUTINE wrf_check_error


