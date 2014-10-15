


PROGRAM wrf

   USE module_wrf_top, only : wrf_init, wrf_dfi, wrf_run, wrf_finalize











   IMPLICIT NONE





  
  CALL wrf_init

  
  CALL wrf_dfi

  
  CALL wrf_run

  
  CALL wrf_finalize

END PROGRAM wrf


