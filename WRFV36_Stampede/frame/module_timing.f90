


MODULE module_timing

   INTEGER, PARAMETER, PRIVATE :: cnmax = 30
   INTEGER, PRIVATE :: cn = 0 
   REAL, PRIVATE    :: elapsed_seconds , elapsed_seconds_total = 0






   REAL(kind=8) :: epoch_seconds_hires(cnmax)


CONTAINS

   SUBROUTINE init_module_timing



      
      
      call init_hires_timer()

      cn = 0
   END SUBROUTINE init_module_timing


   SUBROUTINE start_timing
     use module_wrf_error, only: silence

      IMPLICIT NONE

      if(silence/=0) return
      cn = cn + 1
      IF ( cn .gt. cnmax ) THEN
        CALL wrf_error_fatal3("<stdin>",40,&
'module_timing: clock nesting error (too many nests)' )
        RETURN
      ENDIF





      call hires_timer(epoch_seconds_hires(cn))


   END SUBROUTINE start_timing


   SUBROUTINE end_timing ( string )
     use module_wrf_error, only: silence, stderrlog, buffered
   
      IMPLICIT NONE
      REAL(kind=8) :: now_hires
      CHARACTER *(*) :: string
      character*512 :: buf

      if(silence/=0) return

      IF ( cn .lt. 1 ) THEN
        CALL wrf_error_fatal3("<stdin>",66,&
'module_timing: clock nesting error, cn<1' ) 
      ELSE IF ( cn .gt. cnmax ) THEN
        CALL wrf_error_fatal3("<stdin>",69,&
'module_timing: clock nesting error, cn>cnmax' ) 
      ENDIF

      call hires_timer(now_hires)
      
      elapsed_seconds = REAL(now_hires-epoch_seconds_hires(cn))
      elapsed_seconds_total = elapsed_seconds_total + elapsed_seconds

3031 format("Timing for ",A,": ",F10.5," elapsed seconds")
      if(buffered/=0) then
         write(buf,3031) TRIM(string),elapsed_seconds
         call wrf_message(buf)
      else
         if(stderrlog/=0) &
              write(0,3031) TRIM(string),elapsed_seconds
         write(6,3031) TRIM(string),elapsed_seconds
      endif





      cn = cn - 1

   END SUBROUTINE end_timing

END MODULE module_timing

