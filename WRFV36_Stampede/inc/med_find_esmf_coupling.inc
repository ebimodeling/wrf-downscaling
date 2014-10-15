!STARTOFREGISTRYGENERATEDINCLUDE 'inc/med_find_esmf_coupling.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput1( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT1_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT1_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist1( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST1_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST1_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput2( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT2_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT2_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist2( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST2_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST2_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput3( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT3_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT3_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist3( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST3_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST3_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput4( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT4_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT4_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist4( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST4_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST4_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput5( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT5_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT5_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist5( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST5_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST5_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput6( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT6_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT6_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist6( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST6_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST6_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput7( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT7_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT7_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist7( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST7_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST7_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput8( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT8_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT8_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist8( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST8_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST8_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput9( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT9_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT9_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist9( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST9_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST9_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput10( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT10_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT10_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist10( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST10_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST10_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput11( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT11_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT11_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist11( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST11_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST11_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput12( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT12_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT12_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist12( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST12_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST12_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput13( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT13_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT13_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist13( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST13_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST13_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput14( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT14_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT14_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist14( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST14_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST14_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput15( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT15_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT15_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist15( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST15_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST15_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput16( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT16_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT16_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist16( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST16_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST16_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput17( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT17_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT17_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist17( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST17_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST17_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput18( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT18_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT18_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist18( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST18_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST18_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput19( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT19_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT19_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist19( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST19_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST19_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput20( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT20_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT20_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist20( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST20_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST20_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput21( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT21_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT21_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist21( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST21_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST21_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput22( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT22_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT22_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist22( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST22_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST22_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput23( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT23_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT23_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist23( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST23_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST23_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxinput24( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT24_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT24_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
IF ( .NOT. foundcoupling ) THEN
  CALL nl_get_io_form_auxhist24( 1, io_form )
  IF ( use_package( io_form ) == IO_ESMF ) THEN
    CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST24_ALARM ), &
                        RingInterval=couplingInterval, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST24_ALARM) failed' )
    ENDIF
    foundcoupling = .TRUE.
  ENDIF
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
