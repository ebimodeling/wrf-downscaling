!STARTOFREGISTRYGENERATEDINCLUDE 'inc/check_auxstream_alarms.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
! - AUX HISTORY OUTPUT
#ifndef  DISABLE_ALARM_AUXHIST1
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST1_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 1, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST1_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST2
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST2_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 2, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST2_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST3
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST3_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 3, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST3_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST4
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST4_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 4, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST4_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST5
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST5_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 5, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST5_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST6
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST6_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 6, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST6_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST7
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST7_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 7, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST7_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST8
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST8_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 8, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST8_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST9
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST9_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 9, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST9_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST10
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST10_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 10, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST10_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST11
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST11_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 11, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST11_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST12
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST12_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 12, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST12_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST13
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST13_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 13, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST13_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST14
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST14_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 14, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST14_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST15
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST15_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 15, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST15_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST16
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST16_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 16, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST16_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST17
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST17_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 17, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST17_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST18
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST18_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 18, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST18_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST19
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST19_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 19, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST19_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST20
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST20_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 20, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST20_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST21
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST21_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 21, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST21_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST22
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST22_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 22, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST22_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST23
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST23_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 23, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST23_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXHIST24
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST24_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 24, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST24_ALARM ), rc=rc )
   ENDIF
#endif
! - AUX INPUT INPUT
#ifndef  DISABLE_ALARM_AUXINPUT1
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT1_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput1_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput1_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT1_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT2
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT2_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput2_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput2_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT2_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT3
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT3_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput3_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput3_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT3_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT4
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT4_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput4_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput4_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT4_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT5
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT5_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput5_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput5_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT5_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT6
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT6_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput6_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput6_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT6_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT7
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT7_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput7_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput7_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT7_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT8
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT8_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput8_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput8_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT8_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT9
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT9_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput9_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput9_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT9_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT10
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT10_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput10_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput10_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT10_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT11
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT11_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput11_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput11_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT11_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT12
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT12_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput12_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput12_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT12_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT13
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT13_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput13_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput13_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT13_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT14
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT14_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput14_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput14_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT14_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT15
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT15_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput15_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput15_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT15_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT16
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT16_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput16_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput16_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT16_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT17
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT17_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput17_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput17_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT17_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT18
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT18_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput18_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput18_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT18_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT19
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT19_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput19_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput19_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT19_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT20
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT20_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput20_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput20_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT20_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT21
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT21_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput21_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput21_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT21_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT22
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT22_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput22_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput22_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT22_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT23
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT23_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput23_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput23_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT23_ALARM ), rc=rc )
   ENDIF
#endif
#ifndef  DISABLE_ALARM_AUXINPUT24
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT24_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput24_in ( grid , config_flags )
     WRITE ( message , FMT='(A,A,A,i3)' )  'Input data processed for ' , &
        TRIM(config_flags%auxinput24_inname) , ' for domain ',grid%id
     CALL wrf_debug ( 0 , message )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT24_ALARM ), rc=rc )
   ENDIF
#endif
!ENDOFREGISTRYGENERATEDINCLUDE
