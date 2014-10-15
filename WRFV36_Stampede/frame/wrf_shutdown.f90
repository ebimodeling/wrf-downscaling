

SUBROUTINE wrf_shutdown

   USE , INTRINSIC :: IEEE_EXCEPTIONS



    LOGICAL wrf_dm_on_monitor
    EXTERNAL wrf_dm_on_monitor
    EXTERNAL wrf_dm_shutdown


    CALL wrf_dm_shutdown
END SUBROUTINE wrf_shutdown

