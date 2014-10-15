!STARTOFREGISTRYGENERATEDINCLUDE 'inc/set_timekeeping_alarms.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
! auxinput1 INTERVAL
   CALL nl_get_auxinput1_interval( grid%id, auxinput1_interval )   ! same as minutes
   CALL nl_get_auxinput1_interval_d( grid%id, auxinput1_interval_d )
   CALL nl_get_auxinput1_interval_h( grid%id, auxinput1_interval_h )
   CALL nl_get_auxinput1_interval_m( grid%id, auxinput1_interval_m )
   CALL nl_get_auxinput1_interval_s( grid%id, auxinput1_interval_s )
   IF ( auxinput1_interval_m .EQ. 0 ) auxinput1_interval_m = auxinput1_interval
   IF ( MAX( auxinput1_interval_d,   &
             auxinput1_interval_h, auxinput1_interval_m , auxinput1_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput1_interval_d, &
                                        H=auxinput1_interval_h, M=auxinput1_interval_m, S=auxinput1_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput1_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput1_begin  ( grid%id, auxinput1_begin   )
   CALL nl_get_auxinput1_begin_y( grid%id, auxinput1_begin_y )
   CALL nl_get_auxinput1_begin_d( grid%id, auxinput1_begin_d )
   CALL nl_get_auxinput1_begin_h( grid%id, auxinput1_begin_h )
   CALL nl_get_auxinput1_begin_m( grid%id, auxinput1_begin_m )
   CALL nl_get_auxinput1_begin_s( grid%id, auxinput1_begin_s )
   IF ( auxinput1_begin_m .EQ. 0 ) auxinput1_begin_m = auxinput1_begin
   IF ( MAX( auxinput1_begin_y, auxinput1_begin_d,   &
             auxinput1_begin_h, auxinput1_begin_m , auxinput1_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput1_begin_d, &
                                      H=auxinput1_begin_h, M=auxinput1_begin_m, S=auxinput1_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput1_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput1_end( grid%id, auxinput1_end )
   CALL nl_get_auxinput1_end_y( grid%id, auxinput1_end_y )
   CALL nl_get_auxinput1_end_d( grid%id, auxinput1_end_d )
   CALL nl_get_auxinput1_end_h( grid%id, auxinput1_end_h )
   CALL nl_get_auxinput1_end_m( grid%id, auxinput1_end_m )
   CALL nl_get_auxinput1_end_s( grid%id, auxinput1_end_s )
   IF ( auxinput1_end_m .EQ. 0 ) auxinput1_end_m = auxinput1_end
   IF ( MAX( auxinput1_end_y, auxinput1_end_d,   &
             auxinput1_end_h, auxinput1_end_m , auxinput1_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput1_end_d, &
                                     H=auxinput1_end_h, M=auxinput1_end_m, S=auxinput1_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput1_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput1_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput1_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput1_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput2 INTERVAL
   CALL nl_get_auxinput2_interval( grid%id, auxinput2_interval )   ! same as minutes
   CALL nl_get_auxinput2_interval_d( grid%id, auxinput2_interval_d )
   CALL nl_get_auxinput2_interval_h( grid%id, auxinput2_interval_h )
   CALL nl_get_auxinput2_interval_m( grid%id, auxinput2_interval_m )
   CALL nl_get_auxinput2_interval_s( grid%id, auxinput2_interval_s )
   IF ( auxinput2_interval_m .EQ. 0 ) auxinput2_interval_m = auxinput2_interval
   IF ( MAX( auxinput2_interval_d,   &
             auxinput2_interval_h, auxinput2_interval_m , auxinput2_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput2_interval_d, &
                                        H=auxinput2_interval_h, M=auxinput2_interval_m, S=auxinput2_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput2_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput2_begin  ( grid%id, auxinput2_begin   )
   CALL nl_get_auxinput2_begin_y( grid%id, auxinput2_begin_y )
   CALL nl_get_auxinput2_begin_d( grid%id, auxinput2_begin_d )
   CALL nl_get_auxinput2_begin_h( grid%id, auxinput2_begin_h )
   CALL nl_get_auxinput2_begin_m( grid%id, auxinput2_begin_m )
   CALL nl_get_auxinput2_begin_s( grid%id, auxinput2_begin_s )
   IF ( auxinput2_begin_m .EQ. 0 ) auxinput2_begin_m = auxinput2_begin
   IF ( MAX( auxinput2_begin_y, auxinput2_begin_d,   &
             auxinput2_begin_h, auxinput2_begin_m , auxinput2_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput2_begin_d, &
                                      H=auxinput2_begin_h, M=auxinput2_begin_m, S=auxinput2_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput2_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput2_end( grid%id, auxinput2_end )
   CALL nl_get_auxinput2_end_y( grid%id, auxinput2_end_y )
   CALL nl_get_auxinput2_end_d( grid%id, auxinput2_end_d )
   CALL nl_get_auxinput2_end_h( grid%id, auxinput2_end_h )
   CALL nl_get_auxinput2_end_m( grid%id, auxinput2_end_m )
   CALL nl_get_auxinput2_end_s( grid%id, auxinput2_end_s )
   IF ( auxinput2_end_m .EQ. 0 ) auxinput2_end_m = auxinput2_end
   IF ( MAX( auxinput2_end_y, auxinput2_end_d,   &
             auxinput2_end_h, auxinput2_end_m , auxinput2_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput2_end_d, &
                                     H=auxinput2_end_h, M=auxinput2_end_m, S=auxinput2_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput2_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput2_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput2_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput2_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput3 INTERVAL
   CALL nl_get_auxinput3_interval( grid%id, auxinput3_interval )   ! same as minutes
   CALL nl_get_auxinput3_interval_d( grid%id, auxinput3_interval_d )
   CALL nl_get_auxinput3_interval_h( grid%id, auxinput3_interval_h )
   CALL nl_get_auxinput3_interval_m( grid%id, auxinput3_interval_m )
   CALL nl_get_auxinput3_interval_s( grid%id, auxinput3_interval_s )
   IF ( auxinput3_interval_m .EQ. 0 ) auxinput3_interval_m = auxinput3_interval
   IF ( MAX( auxinput3_interval_d,   &
             auxinput3_interval_h, auxinput3_interval_m , auxinput3_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput3_interval_d, &
                                        H=auxinput3_interval_h, M=auxinput3_interval_m, S=auxinput3_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput3_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput3_begin  ( grid%id, auxinput3_begin   )
   CALL nl_get_auxinput3_begin_y( grid%id, auxinput3_begin_y )
   CALL nl_get_auxinput3_begin_d( grid%id, auxinput3_begin_d )
   CALL nl_get_auxinput3_begin_h( grid%id, auxinput3_begin_h )
   CALL nl_get_auxinput3_begin_m( grid%id, auxinput3_begin_m )
   CALL nl_get_auxinput3_begin_s( grid%id, auxinput3_begin_s )
   IF ( auxinput3_begin_m .EQ. 0 ) auxinput3_begin_m = auxinput3_begin
   IF ( MAX( auxinput3_begin_y, auxinput3_begin_d,   &
             auxinput3_begin_h, auxinput3_begin_m , auxinput3_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput3_begin_d, &
                                      H=auxinput3_begin_h, M=auxinput3_begin_m, S=auxinput3_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput3_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput3_end( grid%id, auxinput3_end )
   CALL nl_get_auxinput3_end_y( grid%id, auxinput3_end_y )
   CALL nl_get_auxinput3_end_d( grid%id, auxinput3_end_d )
   CALL nl_get_auxinput3_end_h( grid%id, auxinput3_end_h )
   CALL nl_get_auxinput3_end_m( grid%id, auxinput3_end_m )
   CALL nl_get_auxinput3_end_s( grid%id, auxinput3_end_s )
   IF ( auxinput3_end_m .EQ. 0 ) auxinput3_end_m = auxinput3_end
   IF ( MAX( auxinput3_end_y, auxinput3_end_d,   &
             auxinput3_end_h, auxinput3_end_m , auxinput3_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput3_end_d, &
                                     H=auxinput3_end_h, M=auxinput3_end_m, S=auxinput3_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput3_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput3_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput3_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput3_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput4 INTERVAL
   CALL nl_get_auxinput4_interval( grid%id, auxinput4_interval )   ! same as minutes
   CALL nl_get_auxinput4_interval_d( grid%id, auxinput4_interval_d )
   CALL nl_get_auxinput4_interval_h( grid%id, auxinput4_interval_h )
   CALL nl_get_auxinput4_interval_m( grid%id, auxinput4_interval_m )
   CALL nl_get_auxinput4_interval_s( grid%id, auxinput4_interval_s )
   IF ( auxinput4_interval_m .EQ. 0 ) auxinput4_interval_m = auxinput4_interval
   IF ( MAX( auxinput4_interval_d,   &
             auxinput4_interval_h, auxinput4_interval_m , auxinput4_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput4_interval_d, &
                                        H=auxinput4_interval_h, M=auxinput4_interval_m, S=auxinput4_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput4_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput4_begin  ( grid%id, auxinput4_begin   )
   CALL nl_get_auxinput4_begin_y( grid%id, auxinput4_begin_y )
   CALL nl_get_auxinput4_begin_d( grid%id, auxinput4_begin_d )
   CALL nl_get_auxinput4_begin_h( grid%id, auxinput4_begin_h )
   CALL nl_get_auxinput4_begin_m( grid%id, auxinput4_begin_m )
   CALL nl_get_auxinput4_begin_s( grid%id, auxinput4_begin_s )
   IF ( auxinput4_begin_m .EQ. 0 ) auxinput4_begin_m = auxinput4_begin
   IF ( MAX( auxinput4_begin_y, auxinput4_begin_d,   &
             auxinput4_begin_h, auxinput4_begin_m , auxinput4_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput4_begin_d, &
                                      H=auxinput4_begin_h, M=auxinput4_begin_m, S=auxinput4_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput4_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput4_end( grid%id, auxinput4_end )
   CALL nl_get_auxinput4_end_y( grid%id, auxinput4_end_y )
   CALL nl_get_auxinput4_end_d( grid%id, auxinput4_end_d )
   CALL nl_get_auxinput4_end_h( grid%id, auxinput4_end_h )
   CALL nl_get_auxinput4_end_m( grid%id, auxinput4_end_m )
   CALL nl_get_auxinput4_end_s( grid%id, auxinput4_end_s )
   IF ( auxinput4_end_m .EQ. 0 ) auxinput4_end_m = auxinput4_end
   IF ( MAX( auxinput4_end_y, auxinput4_end_d,   &
             auxinput4_end_h, auxinput4_end_m , auxinput4_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput4_end_d, &
                                     H=auxinput4_end_h, M=auxinput4_end_m, S=auxinput4_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput4_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput4_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput4_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput4_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput5 INTERVAL
   CALL nl_get_auxinput5_interval( grid%id, auxinput5_interval )   ! same as minutes
   CALL nl_get_auxinput5_interval_d( grid%id, auxinput5_interval_d )
   CALL nl_get_auxinput5_interval_h( grid%id, auxinput5_interval_h )
   CALL nl_get_auxinput5_interval_m( grid%id, auxinput5_interval_m )
   CALL nl_get_auxinput5_interval_s( grid%id, auxinput5_interval_s )
   IF ( auxinput5_interval_m .EQ. 0 ) auxinput5_interval_m = auxinput5_interval
   IF ( MAX( auxinput5_interval_d,   &
             auxinput5_interval_h, auxinput5_interval_m , auxinput5_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput5_interval_d, &
                                        H=auxinput5_interval_h, M=auxinput5_interval_m, S=auxinput5_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput5_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput5_begin  ( grid%id, auxinput5_begin   )
   CALL nl_get_auxinput5_begin_y( grid%id, auxinput5_begin_y )
   CALL nl_get_auxinput5_begin_d( grid%id, auxinput5_begin_d )
   CALL nl_get_auxinput5_begin_h( grid%id, auxinput5_begin_h )
   CALL nl_get_auxinput5_begin_m( grid%id, auxinput5_begin_m )
   CALL nl_get_auxinput5_begin_s( grid%id, auxinput5_begin_s )
   IF ( auxinput5_begin_m .EQ. 0 ) auxinput5_begin_m = auxinput5_begin
   IF ( MAX( auxinput5_begin_y, auxinput5_begin_d,   &
             auxinput5_begin_h, auxinput5_begin_m , auxinput5_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput5_begin_d, &
                                      H=auxinput5_begin_h, M=auxinput5_begin_m, S=auxinput5_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput5_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput5_end( grid%id, auxinput5_end )
   CALL nl_get_auxinput5_end_y( grid%id, auxinput5_end_y )
   CALL nl_get_auxinput5_end_d( grid%id, auxinput5_end_d )
   CALL nl_get_auxinput5_end_h( grid%id, auxinput5_end_h )
   CALL nl_get_auxinput5_end_m( grid%id, auxinput5_end_m )
   CALL nl_get_auxinput5_end_s( grid%id, auxinput5_end_s )
   IF ( auxinput5_end_m .EQ. 0 ) auxinput5_end_m = auxinput5_end
   IF ( MAX( auxinput5_end_y, auxinput5_end_d,   &
             auxinput5_end_h, auxinput5_end_m , auxinput5_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput5_end_d, &
                                     H=auxinput5_end_h, M=auxinput5_end_m, S=auxinput5_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput5_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput5_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput5_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput5_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput6 INTERVAL
   CALL nl_get_auxinput6_interval( grid%id, auxinput6_interval )   ! same as minutes
   CALL nl_get_auxinput6_interval_d( grid%id, auxinput6_interval_d )
   CALL nl_get_auxinput6_interval_h( grid%id, auxinput6_interval_h )
   CALL nl_get_auxinput6_interval_m( grid%id, auxinput6_interval_m )
   CALL nl_get_auxinput6_interval_s( grid%id, auxinput6_interval_s )
   IF ( auxinput6_interval_m .EQ. 0 ) auxinput6_interval_m = auxinput6_interval
   IF ( MAX( auxinput6_interval_d,   &
             auxinput6_interval_h, auxinput6_interval_m , auxinput6_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput6_interval_d, &
                                        H=auxinput6_interval_h, M=auxinput6_interval_m, S=auxinput6_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput6_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput6_begin  ( grid%id, auxinput6_begin   )
   CALL nl_get_auxinput6_begin_y( grid%id, auxinput6_begin_y )
   CALL nl_get_auxinput6_begin_d( grid%id, auxinput6_begin_d )
   CALL nl_get_auxinput6_begin_h( grid%id, auxinput6_begin_h )
   CALL nl_get_auxinput6_begin_m( grid%id, auxinput6_begin_m )
   CALL nl_get_auxinput6_begin_s( grid%id, auxinput6_begin_s )
   IF ( auxinput6_begin_m .EQ. 0 ) auxinput6_begin_m = auxinput6_begin
   IF ( MAX( auxinput6_begin_y, auxinput6_begin_d,   &
             auxinput6_begin_h, auxinput6_begin_m , auxinput6_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput6_begin_d, &
                                      H=auxinput6_begin_h, M=auxinput6_begin_m, S=auxinput6_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput6_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput6_end( grid%id, auxinput6_end )
   CALL nl_get_auxinput6_end_y( grid%id, auxinput6_end_y )
   CALL nl_get_auxinput6_end_d( grid%id, auxinput6_end_d )
   CALL nl_get_auxinput6_end_h( grid%id, auxinput6_end_h )
   CALL nl_get_auxinput6_end_m( grid%id, auxinput6_end_m )
   CALL nl_get_auxinput6_end_s( grid%id, auxinput6_end_s )
   IF ( auxinput6_end_m .EQ. 0 ) auxinput6_end_m = auxinput6_end
   IF ( MAX( auxinput6_end_y, auxinput6_end_d,   &
             auxinput6_end_h, auxinput6_end_m , auxinput6_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput6_end_d, &
                                     H=auxinput6_end_h, M=auxinput6_end_m, S=auxinput6_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput6_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput6_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput6_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput6_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput7 INTERVAL
   CALL nl_get_auxinput7_interval( grid%id, auxinput7_interval )   ! same as minutes
   CALL nl_get_auxinput7_interval_d( grid%id, auxinput7_interval_d )
   CALL nl_get_auxinput7_interval_h( grid%id, auxinput7_interval_h )
   CALL nl_get_auxinput7_interval_m( grid%id, auxinput7_interval_m )
   CALL nl_get_auxinput7_interval_s( grid%id, auxinput7_interval_s )
   IF ( auxinput7_interval_m .EQ. 0 ) auxinput7_interval_m = auxinput7_interval
   IF ( MAX( auxinput7_interval_d,   &
             auxinput7_interval_h, auxinput7_interval_m , auxinput7_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput7_interval_d, &
                                        H=auxinput7_interval_h, M=auxinput7_interval_m, S=auxinput7_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput7_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput7_begin  ( grid%id, auxinput7_begin   )
   CALL nl_get_auxinput7_begin_y( grid%id, auxinput7_begin_y )
   CALL nl_get_auxinput7_begin_d( grid%id, auxinput7_begin_d )
   CALL nl_get_auxinput7_begin_h( grid%id, auxinput7_begin_h )
   CALL nl_get_auxinput7_begin_m( grid%id, auxinput7_begin_m )
   CALL nl_get_auxinput7_begin_s( grid%id, auxinput7_begin_s )
   IF ( auxinput7_begin_m .EQ. 0 ) auxinput7_begin_m = auxinput7_begin
   IF ( MAX( auxinput7_begin_y, auxinput7_begin_d,   &
             auxinput7_begin_h, auxinput7_begin_m , auxinput7_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput7_begin_d, &
                                      H=auxinput7_begin_h, M=auxinput7_begin_m, S=auxinput7_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput7_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput7_end( grid%id, auxinput7_end )
   CALL nl_get_auxinput7_end_y( grid%id, auxinput7_end_y )
   CALL nl_get_auxinput7_end_d( grid%id, auxinput7_end_d )
   CALL nl_get_auxinput7_end_h( grid%id, auxinput7_end_h )
   CALL nl_get_auxinput7_end_m( grid%id, auxinput7_end_m )
   CALL nl_get_auxinput7_end_s( grid%id, auxinput7_end_s )
   IF ( auxinput7_end_m .EQ. 0 ) auxinput7_end_m = auxinput7_end
   IF ( MAX( auxinput7_end_y, auxinput7_end_d,   &
             auxinput7_end_h, auxinput7_end_m , auxinput7_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput7_end_d, &
                                     H=auxinput7_end_h, M=auxinput7_end_m, S=auxinput7_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput7_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput7_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput7_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput7_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput8 INTERVAL
   CALL nl_get_auxinput8_interval( grid%id, auxinput8_interval )   ! same as minutes
   CALL nl_get_auxinput8_interval_d( grid%id, auxinput8_interval_d )
   CALL nl_get_auxinput8_interval_h( grid%id, auxinput8_interval_h )
   CALL nl_get_auxinput8_interval_m( grid%id, auxinput8_interval_m )
   CALL nl_get_auxinput8_interval_s( grid%id, auxinput8_interval_s )
   IF ( auxinput8_interval_m .EQ. 0 ) auxinput8_interval_m = auxinput8_interval
   IF ( MAX( auxinput8_interval_d,   &
             auxinput8_interval_h, auxinput8_interval_m , auxinput8_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput8_interval_d, &
                                        H=auxinput8_interval_h, M=auxinput8_interval_m, S=auxinput8_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput8_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput8_begin  ( grid%id, auxinput8_begin   )
   CALL nl_get_auxinput8_begin_y( grid%id, auxinput8_begin_y )
   CALL nl_get_auxinput8_begin_d( grid%id, auxinput8_begin_d )
   CALL nl_get_auxinput8_begin_h( grid%id, auxinput8_begin_h )
   CALL nl_get_auxinput8_begin_m( grid%id, auxinput8_begin_m )
   CALL nl_get_auxinput8_begin_s( grid%id, auxinput8_begin_s )
   IF ( auxinput8_begin_m .EQ. 0 ) auxinput8_begin_m = auxinput8_begin
   IF ( MAX( auxinput8_begin_y, auxinput8_begin_d,   &
             auxinput8_begin_h, auxinput8_begin_m , auxinput8_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput8_begin_d, &
                                      H=auxinput8_begin_h, M=auxinput8_begin_m, S=auxinput8_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput8_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput8_end( grid%id, auxinput8_end )
   CALL nl_get_auxinput8_end_y( grid%id, auxinput8_end_y )
   CALL nl_get_auxinput8_end_d( grid%id, auxinput8_end_d )
   CALL nl_get_auxinput8_end_h( grid%id, auxinput8_end_h )
   CALL nl_get_auxinput8_end_m( grid%id, auxinput8_end_m )
   CALL nl_get_auxinput8_end_s( grid%id, auxinput8_end_s )
   IF ( auxinput8_end_m .EQ. 0 ) auxinput8_end_m = auxinput8_end
   IF ( MAX( auxinput8_end_y, auxinput8_end_d,   &
             auxinput8_end_h, auxinput8_end_m , auxinput8_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput8_end_d, &
                                     H=auxinput8_end_h, M=auxinput8_end_m, S=auxinput8_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput8_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput8_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput8_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput8_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput9 INTERVAL
   CALL nl_get_auxinput9_interval( grid%id, auxinput9_interval )   ! same as minutes
   CALL nl_get_auxinput9_interval_d( grid%id, auxinput9_interval_d )
   CALL nl_get_auxinput9_interval_h( grid%id, auxinput9_interval_h )
   CALL nl_get_auxinput9_interval_m( grid%id, auxinput9_interval_m )
   CALL nl_get_auxinput9_interval_s( grid%id, auxinput9_interval_s )
   IF ( auxinput9_interval_m .EQ. 0 ) auxinput9_interval_m = auxinput9_interval
   IF ( MAX( auxinput9_interval_d,   &
             auxinput9_interval_h, auxinput9_interval_m , auxinput9_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput9_interval_d, &
                                        H=auxinput9_interval_h, M=auxinput9_interval_m, S=auxinput9_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput9_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput9_begin  ( grid%id, auxinput9_begin   )
   CALL nl_get_auxinput9_begin_y( grid%id, auxinput9_begin_y )
   CALL nl_get_auxinput9_begin_d( grid%id, auxinput9_begin_d )
   CALL nl_get_auxinput9_begin_h( grid%id, auxinput9_begin_h )
   CALL nl_get_auxinput9_begin_m( grid%id, auxinput9_begin_m )
   CALL nl_get_auxinput9_begin_s( grid%id, auxinput9_begin_s )
   IF ( auxinput9_begin_m .EQ. 0 ) auxinput9_begin_m = auxinput9_begin
   IF ( MAX( auxinput9_begin_y, auxinput9_begin_d,   &
             auxinput9_begin_h, auxinput9_begin_m , auxinput9_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput9_begin_d, &
                                      H=auxinput9_begin_h, M=auxinput9_begin_m, S=auxinput9_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput9_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput9_end( grid%id, auxinput9_end )
   CALL nl_get_auxinput9_end_y( grid%id, auxinput9_end_y )
   CALL nl_get_auxinput9_end_d( grid%id, auxinput9_end_d )
   CALL nl_get_auxinput9_end_h( grid%id, auxinput9_end_h )
   CALL nl_get_auxinput9_end_m( grid%id, auxinput9_end_m )
   CALL nl_get_auxinput9_end_s( grid%id, auxinput9_end_s )
   IF ( auxinput9_end_m .EQ. 0 ) auxinput9_end_m = auxinput9_end
   IF ( MAX( auxinput9_end_y, auxinput9_end_d,   &
             auxinput9_end_h, auxinput9_end_m , auxinput9_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput9_end_d, &
                                     H=auxinput9_end_h, M=auxinput9_end_m, S=auxinput9_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput9_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput9_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput9_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput9_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput10 INTERVAL
   CALL nl_get_auxinput10_interval( grid%id, auxinput10_interval )   ! same as minutes
   CALL nl_get_auxinput10_interval_d( grid%id, auxinput10_interval_d )
   CALL nl_get_auxinput10_interval_h( grid%id, auxinput10_interval_h )
   CALL nl_get_auxinput10_interval_m( grid%id, auxinput10_interval_m )
   CALL nl_get_auxinput10_interval_s( grid%id, auxinput10_interval_s )
   IF ( auxinput10_interval_m .EQ. 0 ) auxinput10_interval_m = auxinput10_interval
   IF ( MAX( auxinput10_interval_d,   &
             auxinput10_interval_h, auxinput10_interval_m , auxinput10_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput10_interval_d, &
                                        H=auxinput10_interval_h, M=auxinput10_interval_m, S=auxinput10_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput10_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput10_begin  ( grid%id, auxinput10_begin   )
   CALL nl_get_auxinput10_begin_y( grid%id, auxinput10_begin_y )
   CALL nl_get_auxinput10_begin_d( grid%id, auxinput10_begin_d )
   CALL nl_get_auxinput10_begin_h( grid%id, auxinput10_begin_h )
   CALL nl_get_auxinput10_begin_m( grid%id, auxinput10_begin_m )
   CALL nl_get_auxinput10_begin_s( grid%id, auxinput10_begin_s )
   IF ( auxinput10_begin_m .EQ. 0 ) auxinput10_begin_m = auxinput10_begin
   IF ( MAX( auxinput10_begin_y, auxinput10_begin_d,   &
             auxinput10_begin_h, auxinput10_begin_m , auxinput10_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput10_begin_d, &
                                      H=auxinput10_begin_h, M=auxinput10_begin_m, S=auxinput10_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput10_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput10_end( grid%id, auxinput10_end )
   CALL nl_get_auxinput10_end_y( grid%id, auxinput10_end_y )
   CALL nl_get_auxinput10_end_d( grid%id, auxinput10_end_d )
   CALL nl_get_auxinput10_end_h( grid%id, auxinput10_end_h )
   CALL nl_get_auxinput10_end_m( grid%id, auxinput10_end_m )
   CALL nl_get_auxinput10_end_s( grid%id, auxinput10_end_s )
   IF ( auxinput10_end_m .EQ. 0 ) auxinput10_end_m = auxinput10_end
   IF ( MAX( auxinput10_end_y, auxinput10_end_d,   &
             auxinput10_end_h, auxinput10_end_m , auxinput10_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput10_end_d, &
                                     H=auxinput10_end_h, M=auxinput10_end_m, S=auxinput10_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput10_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput10_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput10_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput10_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput11 INTERVAL
   CALL nl_get_auxinput11_interval( grid%id, auxinput11_interval )   ! same as minutes
   CALL nl_get_auxinput11_interval_d( grid%id, auxinput11_interval_d )
   CALL nl_get_auxinput11_interval_h( grid%id, auxinput11_interval_h )
   CALL nl_get_auxinput11_interval_m( grid%id, auxinput11_interval_m )
   CALL nl_get_auxinput11_interval_s( grid%id, auxinput11_interval_s )
   IF ( auxinput11_interval_m .EQ. 0 ) auxinput11_interval_m = auxinput11_interval
   IF ( MAX( auxinput11_interval_d,   &
             auxinput11_interval_h, auxinput11_interval_m , auxinput11_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput11_interval_d, &
                                        H=auxinput11_interval_h, M=auxinput11_interval_m, S=auxinput11_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput11_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput11_begin  ( grid%id, auxinput11_begin   )
   CALL nl_get_auxinput11_begin_y( grid%id, auxinput11_begin_y )
   CALL nl_get_auxinput11_begin_d( grid%id, auxinput11_begin_d )
   CALL nl_get_auxinput11_begin_h( grid%id, auxinput11_begin_h )
   CALL nl_get_auxinput11_begin_m( grid%id, auxinput11_begin_m )
   CALL nl_get_auxinput11_begin_s( grid%id, auxinput11_begin_s )
   IF ( auxinput11_begin_m .EQ. 0 ) auxinput11_begin_m = auxinput11_begin
   IF ( MAX( auxinput11_begin_y, auxinput11_begin_d,   &
             auxinput11_begin_h, auxinput11_begin_m , auxinput11_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput11_begin_d, &
                                      H=auxinput11_begin_h, M=auxinput11_begin_m, S=auxinput11_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput11_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput11_end( grid%id, auxinput11_end )
   CALL nl_get_auxinput11_end_y( grid%id, auxinput11_end_y )
   CALL nl_get_auxinput11_end_d( grid%id, auxinput11_end_d )
   CALL nl_get_auxinput11_end_h( grid%id, auxinput11_end_h )
   CALL nl_get_auxinput11_end_m( grid%id, auxinput11_end_m )
   CALL nl_get_auxinput11_end_s( grid%id, auxinput11_end_s )
   IF ( auxinput11_end_m .EQ. 0 ) auxinput11_end_m = auxinput11_end
   IF ( MAX( auxinput11_end_y, auxinput11_end_d,   &
             auxinput11_end_h, auxinput11_end_m , auxinput11_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput11_end_d, &
                                     H=auxinput11_end_h, M=auxinput11_end_m, S=auxinput11_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput11_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput11_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput11_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput11_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput12 INTERVAL
   CALL nl_get_auxinput12_interval( grid%id, auxinput12_interval )   ! same as minutes
   CALL nl_get_auxinput12_interval_d( grid%id, auxinput12_interval_d )
   CALL nl_get_auxinput12_interval_h( grid%id, auxinput12_interval_h )
   CALL nl_get_auxinput12_interval_m( grid%id, auxinput12_interval_m )
   CALL nl_get_auxinput12_interval_s( grid%id, auxinput12_interval_s )
   IF ( auxinput12_interval_m .EQ. 0 ) auxinput12_interval_m = auxinput12_interval
   IF ( MAX( auxinput12_interval_d,   &
             auxinput12_interval_h, auxinput12_interval_m , auxinput12_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput12_interval_d, &
                                        H=auxinput12_interval_h, M=auxinput12_interval_m, S=auxinput12_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput12_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput12_begin  ( grid%id, auxinput12_begin   )
   CALL nl_get_auxinput12_begin_y( grid%id, auxinput12_begin_y )
   CALL nl_get_auxinput12_begin_d( grid%id, auxinput12_begin_d )
   CALL nl_get_auxinput12_begin_h( grid%id, auxinput12_begin_h )
   CALL nl_get_auxinput12_begin_m( grid%id, auxinput12_begin_m )
   CALL nl_get_auxinput12_begin_s( grid%id, auxinput12_begin_s )
   IF ( auxinput12_begin_m .EQ. 0 ) auxinput12_begin_m = auxinput12_begin
   IF ( MAX( auxinput12_begin_y, auxinput12_begin_d,   &
             auxinput12_begin_h, auxinput12_begin_m , auxinput12_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput12_begin_d, &
                                      H=auxinput12_begin_h, M=auxinput12_begin_m, S=auxinput12_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput12_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput12_end( grid%id, auxinput12_end )
   CALL nl_get_auxinput12_end_y( grid%id, auxinput12_end_y )
   CALL nl_get_auxinput12_end_d( grid%id, auxinput12_end_d )
   CALL nl_get_auxinput12_end_h( grid%id, auxinput12_end_h )
   CALL nl_get_auxinput12_end_m( grid%id, auxinput12_end_m )
   CALL nl_get_auxinput12_end_s( grid%id, auxinput12_end_s )
   IF ( auxinput12_end_m .EQ. 0 ) auxinput12_end_m = auxinput12_end
   IF ( MAX( auxinput12_end_y, auxinput12_end_d,   &
             auxinput12_end_h, auxinput12_end_m , auxinput12_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput12_end_d, &
                                     H=auxinput12_end_h, M=auxinput12_end_m, S=auxinput12_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput12_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput12_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput12_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput12_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput13 INTERVAL
   CALL nl_get_auxinput13_interval( grid%id, auxinput13_interval )   ! same as minutes
   CALL nl_get_auxinput13_interval_d( grid%id, auxinput13_interval_d )
   CALL nl_get_auxinput13_interval_h( grid%id, auxinput13_interval_h )
   CALL nl_get_auxinput13_interval_m( grid%id, auxinput13_interval_m )
   CALL nl_get_auxinput13_interval_s( grid%id, auxinput13_interval_s )
   IF ( auxinput13_interval_m .EQ. 0 ) auxinput13_interval_m = auxinput13_interval
   IF ( MAX( auxinput13_interval_d,   &
             auxinput13_interval_h, auxinput13_interval_m , auxinput13_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput13_interval_d, &
                                        H=auxinput13_interval_h, M=auxinput13_interval_m, S=auxinput13_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput13_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput13_begin  ( grid%id, auxinput13_begin   )
   CALL nl_get_auxinput13_begin_y( grid%id, auxinput13_begin_y )
   CALL nl_get_auxinput13_begin_d( grid%id, auxinput13_begin_d )
   CALL nl_get_auxinput13_begin_h( grid%id, auxinput13_begin_h )
   CALL nl_get_auxinput13_begin_m( grid%id, auxinput13_begin_m )
   CALL nl_get_auxinput13_begin_s( grid%id, auxinput13_begin_s )
   IF ( auxinput13_begin_m .EQ. 0 ) auxinput13_begin_m = auxinput13_begin
   IF ( MAX( auxinput13_begin_y, auxinput13_begin_d,   &
             auxinput13_begin_h, auxinput13_begin_m , auxinput13_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput13_begin_d, &
                                      H=auxinput13_begin_h, M=auxinput13_begin_m, S=auxinput13_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput13_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput13_end( grid%id, auxinput13_end )
   CALL nl_get_auxinput13_end_y( grid%id, auxinput13_end_y )
   CALL nl_get_auxinput13_end_d( grid%id, auxinput13_end_d )
   CALL nl_get_auxinput13_end_h( grid%id, auxinput13_end_h )
   CALL nl_get_auxinput13_end_m( grid%id, auxinput13_end_m )
   CALL nl_get_auxinput13_end_s( grid%id, auxinput13_end_s )
   IF ( auxinput13_end_m .EQ. 0 ) auxinput13_end_m = auxinput13_end
   IF ( MAX( auxinput13_end_y, auxinput13_end_d,   &
             auxinput13_end_h, auxinput13_end_m , auxinput13_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput13_end_d, &
                                     H=auxinput13_end_h, M=auxinput13_end_m, S=auxinput13_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput13_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput13_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput13_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput13_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput14 INTERVAL
   CALL nl_get_auxinput14_interval( grid%id, auxinput14_interval )   ! same as minutes
   CALL nl_get_auxinput14_interval_d( grid%id, auxinput14_interval_d )
   CALL nl_get_auxinput14_interval_h( grid%id, auxinput14_interval_h )
   CALL nl_get_auxinput14_interval_m( grid%id, auxinput14_interval_m )
   CALL nl_get_auxinput14_interval_s( grid%id, auxinput14_interval_s )
   IF ( auxinput14_interval_m .EQ. 0 ) auxinput14_interval_m = auxinput14_interval
   IF ( MAX( auxinput14_interval_d,   &
             auxinput14_interval_h, auxinput14_interval_m , auxinput14_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput14_interval_d, &
                                        H=auxinput14_interval_h, M=auxinput14_interval_m, S=auxinput14_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput14_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput14_begin  ( grid%id, auxinput14_begin   )
   CALL nl_get_auxinput14_begin_y( grid%id, auxinput14_begin_y )
   CALL nl_get_auxinput14_begin_d( grid%id, auxinput14_begin_d )
   CALL nl_get_auxinput14_begin_h( grid%id, auxinput14_begin_h )
   CALL nl_get_auxinput14_begin_m( grid%id, auxinput14_begin_m )
   CALL nl_get_auxinput14_begin_s( grid%id, auxinput14_begin_s )
   IF ( auxinput14_begin_m .EQ. 0 ) auxinput14_begin_m = auxinput14_begin
   IF ( MAX( auxinput14_begin_y, auxinput14_begin_d,   &
             auxinput14_begin_h, auxinput14_begin_m , auxinput14_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput14_begin_d, &
                                      H=auxinput14_begin_h, M=auxinput14_begin_m, S=auxinput14_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput14_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput14_end( grid%id, auxinput14_end )
   CALL nl_get_auxinput14_end_y( grid%id, auxinput14_end_y )
   CALL nl_get_auxinput14_end_d( grid%id, auxinput14_end_d )
   CALL nl_get_auxinput14_end_h( grid%id, auxinput14_end_h )
   CALL nl_get_auxinput14_end_m( grid%id, auxinput14_end_m )
   CALL nl_get_auxinput14_end_s( grid%id, auxinput14_end_s )
   IF ( auxinput14_end_m .EQ. 0 ) auxinput14_end_m = auxinput14_end
   IF ( MAX( auxinput14_end_y, auxinput14_end_d,   &
             auxinput14_end_h, auxinput14_end_m , auxinput14_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput14_end_d, &
                                     H=auxinput14_end_h, M=auxinput14_end_m, S=auxinput14_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput14_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput14_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput14_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput14_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput15 INTERVAL
   CALL nl_get_auxinput15_interval( grid%id, auxinput15_interval )   ! same as minutes
   CALL nl_get_auxinput15_interval_d( grid%id, auxinput15_interval_d )
   CALL nl_get_auxinput15_interval_h( grid%id, auxinput15_interval_h )
   CALL nl_get_auxinput15_interval_m( grid%id, auxinput15_interval_m )
   CALL nl_get_auxinput15_interval_s( grid%id, auxinput15_interval_s )
   IF ( auxinput15_interval_m .EQ. 0 ) auxinput15_interval_m = auxinput15_interval
   IF ( MAX( auxinput15_interval_d,   &
             auxinput15_interval_h, auxinput15_interval_m , auxinput15_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput15_interval_d, &
                                        H=auxinput15_interval_h, M=auxinput15_interval_m, S=auxinput15_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput15_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput15_begin  ( grid%id, auxinput15_begin   )
   CALL nl_get_auxinput15_begin_y( grid%id, auxinput15_begin_y )
   CALL nl_get_auxinput15_begin_d( grid%id, auxinput15_begin_d )
   CALL nl_get_auxinput15_begin_h( grid%id, auxinput15_begin_h )
   CALL nl_get_auxinput15_begin_m( grid%id, auxinput15_begin_m )
   CALL nl_get_auxinput15_begin_s( grid%id, auxinput15_begin_s )
   IF ( auxinput15_begin_m .EQ. 0 ) auxinput15_begin_m = auxinput15_begin
   IF ( MAX( auxinput15_begin_y, auxinput15_begin_d,   &
             auxinput15_begin_h, auxinput15_begin_m , auxinput15_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput15_begin_d, &
                                      H=auxinput15_begin_h, M=auxinput15_begin_m, S=auxinput15_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput15_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput15_end( grid%id, auxinput15_end )
   CALL nl_get_auxinput15_end_y( grid%id, auxinput15_end_y )
   CALL nl_get_auxinput15_end_d( grid%id, auxinput15_end_d )
   CALL nl_get_auxinput15_end_h( grid%id, auxinput15_end_h )
   CALL nl_get_auxinput15_end_m( grid%id, auxinput15_end_m )
   CALL nl_get_auxinput15_end_s( grid%id, auxinput15_end_s )
   IF ( auxinput15_end_m .EQ. 0 ) auxinput15_end_m = auxinput15_end
   IF ( MAX( auxinput15_end_y, auxinput15_end_d,   &
             auxinput15_end_h, auxinput15_end_m , auxinput15_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput15_end_d, &
                                     H=auxinput15_end_h, M=auxinput15_end_m, S=auxinput15_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput15_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput15_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput15_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput15_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput16 INTERVAL
   CALL nl_get_auxinput16_interval( grid%id, auxinput16_interval )   ! same as minutes
   CALL nl_get_auxinput16_interval_d( grid%id, auxinput16_interval_d )
   CALL nl_get_auxinput16_interval_h( grid%id, auxinput16_interval_h )
   CALL nl_get_auxinput16_interval_m( grid%id, auxinput16_interval_m )
   CALL nl_get_auxinput16_interval_s( grid%id, auxinput16_interval_s )
   IF ( auxinput16_interval_m .EQ. 0 ) auxinput16_interval_m = auxinput16_interval
   IF ( MAX( auxinput16_interval_d,   &
             auxinput16_interval_h, auxinput16_interval_m , auxinput16_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput16_interval_d, &
                                        H=auxinput16_interval_h, M=auxinput16_interval_m, S=auxinput16_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput16_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput16_begin  ( grid%id, auxinput16_begin   )
   CALL nl_get_auxinput16_begin_y( grid%id, auxinput16_begin_y )
   CALL nl_get_auxinput16_begin_d( grid%id, auxinput16_begin_d )
   CALL nl_get_auxinput16_begin_h( grid%id, auxinput16_begin_h )
   CALL nl_get_auxinput16_begin_m( grid%id, auxinput16_begin_m )
   CALL nl_get_auxinput16_begin_s( grid%id, auxinput16_begin_s )
   IF ( auxinput16_begin_m .EQ. 0 ) auxinput16_begin_m = auxinput16_begin
   IF ( MAX( auxinput16_begin_y, auxinput16_begin_d,   &
             auxinput16_begin_h, auxinput16_begin_m , auxinput16_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput16_begin_d, &
                                      H=auxinput16_begin_h, M=auxinput16_begin_m, S=auxinput16_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput16_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput16_end( grid%id, auxinput16_end )
   CALL nl_get_auxinput16_end_y( grid%id, auxinput16_end_y )
   CALL nl_get_auxinput16_end_d( grid%id, auxinput16_end_d )
   CALL nl_get_auxinput16_end_h( grid%id, auxinput16_end_h )
   CALL nl_get_auxinput16_end_m( grid%id, auxinput16_end_m )
   CALL nl_get_auxinput16_end_s( grid%id, auxinput16_end_s )
   IF ( auxinput16_end_m .EQ. 0 ) auxinput16_end_m = auxinput16_end
   IF ( MAX( auxinput16_end_y, auxinput16_end_d,   &
             auxinput16_end_h, auxinput16_end_m , auxinput16_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput16_end_d, &
                                     H=auxinput16_end_h, M=auxinput16_end_m, S=auxinput16_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput16_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput16_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput16_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput16_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput17 INTERVAL
   CALL nl_get_auxinput17_interval( grid%id, auxinput17_interval )   ! same as minutes
   CALL nl_get_auxinput17_interval_d( grid%id, auxinput17_interval_d )
   CALL nl_get_auxinput17_interval_h( grid%id, auxinput17_interval_h )
   CALL nl_get_auxinput17_interval_m( grid%id, auxinput17_interval_m )
   CALL nl_get_auxinput17_interval_s( grid%id, auxinput17_interval_s )
   IF ( auxinput17_interval_m .EQ. 0 ) auxinput17_interval_m = auxinput17_interval
   IF ( MAX( auxinput17_interval_d,   &
             auxinput17_interval_h, auxinput17_interval_m , auxinput17_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput17_interval_d, &
                                        H=auxinput17_interval_h, M=auxinput17_interval_m, S=auxinput17_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput17_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput17_begin  ( grid%id, auxinput17_begin   )
   CALL nl_get_auxinput17_begin_y( grid%id, auxinput17_begin_y )
   CALL nl_get_auxinput17_begin_d( grid%id, auxinput17_begin_d )
   CALL nl_get_auxinput17_begin_h( grid%id, auxinput17_begin_h )
   CALL nl_get_auxinput17_begin_m( grid%id, auxinput17_begin_m )
   CALL nl_get_auxinput17_begin_s( grid%id, auxinput17_begin_s )
   IF ( auxinput17_begin_m .EQ. 0 ) auxinput17_begin_m = auxinput17_begin
   IF ( MAX( auxinput17_begin_y, auxinput17_begin_d,   &
             auxinput17_begin_h, auxinput17_begin_m , auxinput17_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput17_begin_d, &
                                      H=auxinput17_begin_h, M=auxinput17_begin_m, S=auxinput17_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput17_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput17_end( grid%id, auxinput17_end )
   CALL nl_get_auxinput17_end_y( grid%id, auxinput17_end_y )
   CALL nl_get_auxinput17_end_d( grid%id, auxinput17_end_d )
   CALL nl_get_auxinput17_end_h( grid%id, auxinput17_end_h )
   CALL nl_get_auxinput17_end_m( grid%id, auxinput17_end_m )
   CALL nl_get_auxinput17_end_s( grid%id, auxinput17_end_s )
   IF ( auxinput17_end_m .EQ. 0 ) auxinput17_end_m = auxinput17_end
   IF ( MAX( auxinput17_end_y, auxinput17_end_d,   &
             auxinput17_end_h, auxinput17_end_m , auxinput17_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput17_end_d, &
                                     H=auxinput17_end_h, M=auxinput17_end_m, S=auxinput17_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput17_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput17_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput17_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput17_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput18 INTERVAL
   CALL nl_get_auxinput18_interval( grid%id, auxinput18_interval )   ! same as minutes
   CALL nl_get_auxinput18_interval_d( grid%id, auxinput18_interval_d )
   CALL nl_get_auxinput18_interval_h( grid%id, auxinput18_interval_h )
   CALL nl_get_auxinput18_interval_m( grid%id, auxinput18_interval_m )
   CALL nl_get_auxinput18_interval_s( grid%id, auxinput18_interval_s )
   IF ( auxinput18_interval_m .EQ. 0 ) auxinput18_interval_m = auxinput18_interval
   IF ( MAX( auxinput18_interval_d,   &
             auxinput18_interval_h, auxinput18_interval_m , auxinput18_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput18_interval_d, &
                                        H=auxinput18_interval_h, M=auxinput18_interval_m, S=auxinput18_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput18_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput18_begin  ( grid%id, auxinput18_begin   )
   CALL nl_get_auxinput18_begin_y( grid%id, auxinput18_begin_y )
   CALL nl_get_auxinput18_begin_d( grid%id, auxinput18_begin_d )
   CALL nl_get_auxinput18_begin_h( grid%id, auxinput18_begin_h )
   CALL nl_get_auxinput18_begin_m( grid%id, auxinput18_begin_m )
   CALL nl_get_auxinput18_begin_s( grid%id, auxinput18_begin_s )
   IF ( auxinput18_begin_m .EQ. 0 ) auxinput18_begin_m = auxinput18_begin
   IF ( MAX( auxinput18_begin_y, auxinput18_begin_d,   &
             auxinput18_begin_h, auxinput18_begin_m , auxinput18_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput18_begin_d, &
                                      H=auxinput18_begin_h, M=auxinput18_begin_m, S=auxinput18_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput18_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput18_end( grid%id, auxinput18_end )
   CALL nl_get_auxinput18_end_y( grid%id, auxinput18_end_y )
   CALL nl_get_auxinput18_end_d( grid%id, auxinput18_end_d )
   CALL nl_get_auxinput18_end_h( grid%id, auxinput18_end_h )
   CALL nl_get_auxinput18_end_m( grid%id, auxinput18_end_m )
   CALL nl_get_auxinput18_end_s( grid%id, auxinput18_end_s )
   IF ( auxinput18_end_m .EQ. 0 ) auxinput18_end_m = auxinput18_end
   IF ( MAX( auxinput18_end_y, auxinput18_end_d,   &
             auxinput18_end_h, auxinput18_end_m , auxinput18_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput18_end_d, &
                                     H=auxinput18_end_h, M=auxinput18_end_m, S=auxinput18_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput18_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput18_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput18_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput18_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput19 INTERVAL
   CALL nl_get_auxinput19_interval( grid%id, auxinput19_interval )   ! same as minutes
   CALL nl_get_auxinput19_interval_d( grid%id, auxinput19_interval_d )
   CALL nl_get_auxinput19_interval_h( grid%id, auxinput19_interval_h )
   CALL nl_get_auxinput19_interval_m( grid%id, auxinput19_interval_m )
   CALL nl_get_auxinput19_interval_s( grid%id, auxinput19_interval_s )
   IF ( auxinput19_interval_m .EQ. 0 ) auxinput19_interval_m = auxinput19_interval
   IF ( MAX( auxinput19_interval_d,   &
             auxinput19_interval_h, auxinput19_interval_m , auxinput19_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput19_interval_d, &
                                        H=auxinput19_interval_h, M=auxinput19_interval_m, S=auxinput19_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput19_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput19_begin  ( grid%id, auxinput19_begin   )
   CALL nl_get_auxinput19_begin_y( grid%id, auxinput19_begin_y )
   CALL nl_get_auxinput19_begin_d( grid%id, auxinput19_begin_d )
   CALL nl_get_auxinput19_begin_h( grid%id, auxinput19_begin_h )
   CALL nl_get_auxinput19_begin_m( grid%id, auxinput19_begin_m )
   CALL nl_get_auxinput19_begin_s( grid%id, auxinput19_begin_s )
   IF ( auxinput19_begin_m .EQ. 0 ) auxinput19_begin_m = auxinput19_begin
   IF ( MAX( auxinput19_begin_y, auxinput19_begin_d,   &
             auxinput19_begin_h, auxinput19_begin_m , auxinput19_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput19_begin_d, &
                                      H=auxinput19_begin_h, M=auxinput19_begin_m, S=auxinput19_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput19_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput19_end( grid%id, auxinput19_end )
   CALL nl_get_auxinput19_end_y( grid%id, auxinput19_end_y )
   CALL nl_get_auxinput19_end_d( grid%id, auxinput19_end_d )
   CALL nl_get_auxinput19_end_h( grid%id, auxinput19_end_h )
   CALL nl_get_auxinput19_end_m( grid%id, auxinput19_end_m )
   CALL nl_get_auxinput19_end_s( grid%id, auxinput19_end_s )
   IF ( auxinput19_end_m .EQ. 0 ) auxinput19_end_m = auxinput19_end
   IF ( MAX( auxinput19_end_y, auxinput19_end_d,   &
             auxinput19_end_h, auxinput19_end_m , auxinput19_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput19_end_d, &
                                     H=auxinput19_end_h, M=auxinput19_end_m, S=auxinput19_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput19_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput19_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput19_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput19_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput20 INTERVAL
   CALL nl_get_auxinput20_interval( grid%id, auxinput20_interval )   ! same as minutes
   CALL nl_get_auxinput20_interval_d( grid%id, auxinput20_interval_d )
   CALL nl_get_auxinput20_interval_h( grid%id, auxinput20_interval_h )
   CALL nl_get_auxinput20_interval_m( grid%id, auxinput20_interval_m )
   CALL nl_get_auxinput20_interval_s( grid%id, auxinput20_interval_s )
   IF ( auxinput20_interval_m .EQ. 0 ) auxinput20_interval_m = auxinput20_interval
   IF ( MAX( auxinput20_interval_d,   &
             auxinput20_interval_h, auxinput20_interval_m , auxinput20_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput20_interval_d, &
                                        H=auxinput20_interval_h, M=auxinput20_interval_m, S=auxinput20_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput20_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput20_begin  ( grid%id, auxinput20_begin   )
   CALL nl_get_auxinput20_begin_y( grid%id, auxinput20_begin_y )
   CALL nl_get_auxinput20_begin_d( grid%id, auxinput20_begin_d )
   CALL nl_get_auxinput20_begin_h( grid%id, auxinput20_begin_h )
   CALL nl_get_auxinput20_begin_m( grid%id, auxinput20_begin_m )
   CALL nl_get_auxinput20_begin_s( grid%id, auxinput20_begin_s )
   IF ( auxinput20_begin_m .EQ. 0 ) auxinput20_begin_m = auxinput20_begin
   IF ( MAX( auxinput20_begin_y, auxinput20_begin_d,   &
             auxinput20_begin_h, auxinput20_begin_m , auxinput20_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput20_begin_d, &
                                      H=auxinput20_begin_h, M=auxinput20_begin_m, S=auxinput20_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput20_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput20_end( grid%id, auxinput20_end )
   CALL nl_get_auxinput20_end_y( grid%id, auxinput20_end_y )
   CALL nl_get_auxinput20_end_d( grid%id, auxinput20_end_d )
   CALL nl_get_auxinput20_end_h( grid%id, auxinput20_end_h )
   CALL nl_get_auxinput20_end_m( grid%id, auxinput20_end_m )
   CALL nl_get_auxinput20_end_s( grid%id, auxinput20_end_s )
   IF ( auxinput20_end_m .EQ. 0 ) auxinput20_end_m = auxinput20_end
   IF ( MAX( auxinput20_end_y, auxinput20_end_d,   &
             auxinput20_end_h, auxinput20_end_m , auxinput20_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput20_end_d, &
                                     H=auxinput20_end_h, M=auxinput20_end_m, S=auxinput20_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput20_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput20_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput20_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput20_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput21 INTERVAL
   CALL nl_get_auxinput21_interval( grid%id, auxinput21_interval )   ! same as minutes
   CALL nl_get_auxinput21_interval_d( grid%id, auxinput21_interval_d )
   CALL nl_get_auxinput21_interval_h( grid%id, auxinput21_interval_h )
   CALL nl_get_auxinput21_interval_m( grid%id, auxinput21_interval_m )
   CALL nl_get_auxinput21_interval_s( grid%id, auxinput21_interval_s )
   IF ( auxinput21_interval_m .EQ. 0 ) auxinput21_interval_m = auxinput21_interval
   IF ( MAX( auxinput21_interval_d,   &
             auxinput21_interval_h, auxinput21_interval_m , auxinput21_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput21_interval_d, &
                                        H=auxinput21_interval_h, M=auxinput21_interval_m, S=auxinput21_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput21_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput21_begin  ( grid%id, auxinput21_begin   )
   CALL nl_get_auxinput21_begin_y( grid%id, auxinput21_begin_y )
   CALL nl_get_auxinput21_begin_d( grid%id, auxinput21_begin_d )
   CALL nl_get_auxinput21_begin_h( grid%id, auxinput21_begin_h )
   CALL nl_get_auxinput21_begin_m( grid%id, auxinput21_begin_m )
   CALL nl_get_auxinput21_begin_s( grid%id, auxinput21_begin_s )
   IF ( auxinput21_begin_m .EQ. 0 ) auxinput21_begin_m = auxinput21_begin
   IF ( MAX( auxinput21_begin_y, auxinput21_begin_d,   &
             auxinput21_begin_h, auxinput21_begin_m , auxinput21_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput21_begin_d, &
                                      H=auxinput21_begin_h, M=auxinput21_begin_m, S=auxinput21_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput21_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput21_end( grid%id, auxinput21_end )
   CALL nl_get_auxinput21_end_y( grid%id, auxinput21_end_y )
   CALL nl_get_auxinput21_end_d( grid%id, auxinput21_end_d )
   CALL nl_get_auxinput21_end_h( grid%id, auxinput21_end_h )
   CALL nl_get_auxinput21_end_m( grid%id, auxinput21_end_m )
   CALL nl_get_auxinput21_end_s( grid%id, auxinput21_end_s )
   IF ( auxinput21_end_m .EQ. 0 ) auxinput21_end_m = auxinput21_end
   IF ( MAX( auxinput21_end_y, auxinput21_end_d,   &
             auxinput21_end_h, auxinput21_end_m , auxinput21_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput21_end_d, &
                                     H=auxinput21_end_h, M=auxinput21_end_m, S=auxinput21_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput21_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput21_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput21_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput21_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput22 INTERVAL
   CALL nl_get_auxinput22_interval( grid%id, auxinput22_interval )   ! same as minutes
   CALL nl_get_auxinput22_interval_d( grid%id, auxinput22_interval_d )
   CALL nl_get_auxinput22_interval_h( grid%id, auxinput22_interval_h )
   CALL nl_get_auxinput22_interval_m( grid%id, auxinput22_interval_m )
   CALL nl_get_auxinput22_interval_s( grid%id, auxinput22_interval_s )
   IF ( auxinput22_interval_m .EQ. 0 ) auxinput22_interval_m = auxinput22_interval
   IF ( MAX( auxinput22_interval_d,   &
             auxinput22_interval_h, auxinput22_interval_m , auxinput22_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput22_interval_d, &
                                        H=auxinput22_interval_h, M=auxinput22_interval_m, S=auxinput22_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput22_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput22_begin  ( grid%id, auxinput22_begin   )
   CALL nl_get_auxinput22_begin_y( grid%id, auxinput22_begin_y )
   CALL nl_get_auxinput22_begin_d( grid%id, auxinput22_begin_d )
   CALL nl_get_auxinput22_begin_h( grid%id, auxinput22_begin_h )
   CALL nl_get_auxinput22_begin_m( grid%id, auxinput22_begin_m )
   CALL nl_get_auxinput22_begin_s( grid%id, auxinput22_begin_s )
   IF ( auxinput22_begin_m .EQ. 0 ) auxinput22_begin_m = auxinput22_begin
   IF ( MAX( auxinput22_begin_y, auxinput22_begin_d,   &
             auxinput22_begin_h, auxinput22_begin_m , auxinput22_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput22_begin_d, &
                                      H=auxinput22_begin_h, M=auxinput22_begin_m, S=auxinput22_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput22_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput22_end( grid%id, auxinput22_end )
   CALL nl_get_auxinput22_end_y( grid%id, auxinput22_end_y )
   CALL nl_get_auxinput22_end_d( grid%id, auxinput22_end_d )
   CALL nl_get_auxinput22_end_h( grid%id, auxinput22_end_h )
   CALL nl_get_auxinput22_end_m( grid%id, auxinput22_end_m )
   CALL nl_get_auxinput22_end_s( grid%id, auxinput22_end_s )
   IF ( auxinput22_end_m .EQ. 0 ) auxinput22_end_m = auxinput22_end
   IF ( MAX( auxinput22_end_y, auxinput22_end_d,   &
             auxinput22_end_h, auxinput22_end_m , auxinput22_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput22_end_d, &
                                     H=auxinput22_end_h, M=auxinput22_end_m, S=auxinput22_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput22_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput22_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput22_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput22_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput23 INTERVAL
   CALL nl_get_auxinput23_interval( grid%id, auxinput23_interval )   ! same as minutes
   CALL nl_get_auxinput23_interval_d( grid%id, auxinput23_interval_d )
   CALL nl_get_auxinput23_interval_h( grid%id, auxinput23_interval_h )
   CALL nl_get_auxinput23_interval_m( grid%id, auxinput23_interval_m )
   CALL nl_get_auxinput23_interval_s( grid%id, auxinput23_interval_s )
   IF ( auxinput23_interval_m .EQ. 0 ) auxinput23_interval_m = auxinput23_interval
   IF ( MAX( auxinput23_interval_d,   &
             auxinput23_interval_h, auxinput23_interval_m , auxinput23_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput23_interval_d, &
                                        H=auxinput23_interval_h, M=auxinput23_interval_m, S=auxinput23_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput23_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput23_begin  ( grid%id, auxinput23_begin   )
   CALL nl_get_auxinput23_begin_y( grid%id, auxinput23_begin_y )
   CALL nl_get_auxinput23_begin_d( grid%id, auxinput23_begin_d )
   CALL nl_get_auxinput23_begin_h( grid%id, auxinput23_begin_h )
   CALL nl_get_auxinput23_begin_m( grid%id, auxinput23_begin_m )
   CALL nl_get_auxinput23_begin_s( grid%id, auxinput23_begin_s )
   IF ( auxinput23_begin_m .EQ. 0 ) auxinput23_begin_m = auxinput23_begin
   IF ( MAX( auxinput23_begin_y, auxinput23_begin_d,   &
             auxinput23_begin_h, auxinput23_begin_m , auxinput23_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput23_begin_d, &
                                      H=auxinput23_begin_h, M=auxinput23_begin_m, S=auxinput23_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput23_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput23_end( grid%id, auxinput23_end )
   CALL nl_get_auxinput23_end_y( grid%id, auxinput23_end_y )
   CALL nl_get_auxinput23_end_d( grid%id, auxinput23_end_d )
   CALL nl_get_auxinput23_end_h( grid%id, auxinput23_end_h )
   CALL nl_get_auxinput23_end_m( grid%id, auxinput23_end_m )
   CALL nl_get_auxinput23_end_s( grid%id, auxinput23_end_s )
   IF ( auxinput23_end_m .EQ. 0 ) auxinput23_end_m = auxinput23_end
   IF ( MAX( auxinput23_end_y, auxinput23_end_d,   &
             auxinput23_end_h, auxinput23_end_m , auxinput23_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput23_end_d, &
                                     H=auxinput23_end_h, M=auxinput23_end_m, S=auxinput23_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput23_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput23_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput23_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput23_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxinput24 INTERVAL
   CALL nl_get_auxinput24_interval( grid%id, auxinput24_interval )   ! same as minutes
   CALL nl_get_auxinput24_interval_d( grid%id, auxinput24_interval_d )
   CALL nl_get_auxinput24_interval_h( grid%id, auxinput24_interval_h )
   CALL nl_get_auxinput24_interval_m( grid%id, auxinput24_interval_m )
   CALL nl_get_auxinput24_interval_s( grid%id, auxinput24_interval_s )
   IF ( auxinput24_interval_m .EQ. 0 ) auxinput24_interval_m = auxinput24_interval
   IF ( MAX( auxinput24_interval_d,   &
             auxinput24_interval_h, auxinput24_interval_m , auxinput24_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxinput24_interval_d, &
                                        H=auxinput24_interval_h, M=auxinput24_interval_m, S=auxinput24_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxinput24_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxinput24_begin  ( grid%id, auxinput24_begin   )
   CALL nl_get_auxinput24_begin_y( grid%id, auxinput24_begin_y )
   CALL nl_get_auxinput24_begin_d( grid%id, auxinput24_begin_d )
   CALL nl_get_auxinput24_begin_h( grid%id, auxinput24_begin_h )
   CALL nl_get_auxinput24_begin_m( grid%id, auxinput24_begin_m )
   CALL nl_get_auxinput24_begin_s( grid%id, auxinput24_begin_s )
   IF ( auxinput24_begin_m .EQ. 0 ) auxinput24_begin_m = auxinput24_begin
   IF ( MAX( auxinput24_begin_y, auxinput24_begin_d,   &
             auxinput24_begin_h, auxinput24_begin_m , auxinput24_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxinput24_begin_d, &
                                      H=auxinput24_begin_h, M=auxinput24_begin_m, S=auxinput24_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput24_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxinput24_end( grid%id, auxinput24_end )
   CALL nl_get_auxinput24_end_y( grid%id, auxinput24_end_y )
   CALL nl_get_auxinput24_end_d( grid%id, auxinput24_end_d )
   CALL nl_get_auxinput24_end_h( grid%id, auxinput24_end_h )
   CALL nl_get_auxinput24_end_m( grid%id, auxinput24_end_m )
   CALL nl_get_auxinput24_end_s( grid%id, auxinput24_end_s )
   IF ( auxinput24_end_m .EQ. 0 ) auxinput24_end_m = auxinput24_end
   IF ( MAX( auxinput24_end_y, auxinput24_end_d,   &
             auxinput24_end_h, auxinput24_end_m , auxinput24_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxinput24_end_d, &
                                     H=auxinput24_end_h, M=auxinput24_end_m, S=auxinput24_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxinput24_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxinput24_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxinput24_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxinput24_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! history INTERVAL
   CALL nl_get_history_interval( grid%id, history_interval )   ! same as minutes
   CALL nl_get_history_interval_d( grid%id, history_interval_d )
   CALL nl_get_history_interval_h( grid%id, history_interval_h )
   CALL nl_get_history_interval_m( grid%id, history_interval_m )
   CALL nl_get_history_interval_s( grid%id, history_interval_s )
   IF ( history_interval_m .EQ. 0 ) history_interval_m = history_interval
   IF ( MAX( history_interval_d,   &
             history_interval_h, history_interval_m , history_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=history_interval_d, &
                                        H=history_interval_h, M=history_interval_m, S=history_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(history_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_history_begin  ( grid%id, history_begin   )
   CALL nl_get_history_begin_y( grid%id, history_begin_y )
   CALL nl_get_history_begin_d( grid%id, history_begin_d )
   CALL nl_get_history_begin_h( grid%id, history_begin_h )
   CALL nl_get_history_begin_m( grid%id, history_begin_m )
   CALL nl_get_history_begin_s( grid%id, history_begin_s )
   IF ( history_begin_m .EQ. 0 ) history_begin_m = history_begin
   IF ( MAX( history_begin_y, history_begin_d,   &
             history_begin_h, history_begin_m , history_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=history_begin_d, &
                                      H=history_begin_h, M=history_begin_m, S=history_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(history_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_history_end( grid%id, history_end )
   CALL nl_get_history_end_y( grid%id, history_end_y )
   CALL nl_get_history_end_d( grid%id, history_end_d )
   CALL nl_get_history_end_h( grid%id, history_end_h )
   CALL nl_get_history_end_m( grid%id, history_end_m )
   CALL nl_get_history_end_s( grid%id, history_end_s )
   IF ( history_end_m .EQ. 0 ) history_end_m = history_end
   IF ( MAX( history_end_y, history_end_d,   &
             history_end_h, history_end_m , history_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=history_end_d, &
                                     H=history_end_h, M=history_end_m, S=history_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(history_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, history_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( history_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(history_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist1 INTERVAL
   CALL nl_get_auxhist1_interval( grid%id, auxhist1_interval )   ! same as minutes
   CALL nl_get_auxhist1_interval_d( grid%id, auxhist1_interval_d )
   CALL nl_get_auxhist1_interval_h( grid%id, auxhist1_interval_h )
   CALL nl_get_auxhist1_interval_m( grid%id, auxhist1_interval_m )
   CALL nl_get_auxhist1_interval_s( grid%id, auxhist1_interval_s )
   IF ( auxhist1_interval_m .EQ. 0 ) auxhist1_interval_m = auxhist1_interval
   IF ( MAX( auxhist1_interval_d,   &
             auxhist1_interval_h, auxhist1_interval_m , auxhist1_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist1_interval_d, &
                                        H=auxhist1_interval_h, M=auxhist1_interval_m, S=auxhist1_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist1_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist1_begin  ( grid%id, auxhist1_begin   )
   CALL nl_get_auxhist1_begin_y( grid%id, auxhist1_begin_y )
   CALL nl_get_auxhist1_begin_d( grid%id, auxhist1_begin_d )
   CALL nl_get_auxhist1_begin_h( grid%id, auxhist1_begin_h )
   CALL nl_get_auxhist1_begin_m( grid%id, auxhist1_begin_m )
   CALL nl_get_auxhist1_begin_s( grid%id, auxhist1_begin_s )
   IF ( auxhist1_begin_m .EQ. 0 ) auxhist1_begin_m = auxhist1_begin
   IF ( MAX( auxhist1_begin_y, auxhist1_begin_d,   &
             auxhist1_begin_h, auxhist1_begin_m , auxhist1_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist1_begin_d, &
                                      H=auxhist1_begin_h, M=auxhist1_begin_m, S=auxhist1_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist1_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist1_end( grid%id, auxhist1_end )
   CALL nl_get_auxhist1_end_y( grid%id, auxhist1_end_y )
   CALL nl_get_auxhist1_end_d( grid%id, auxhist1_end_d )
   CALL nl_get_auxhist1_end_h( grid%id, auxhist1_end_h )
   CALL nl_get_auxhist1_end_m( grid%id, auxhist1_end_m )
   CALL nl_get_auxhist1_end_s( grid%id, auxhist1_end_s )
   IF ( auxhist1_end_m .EQ. 0 ) auxhist1_end_m = auxhist1_end
   IF ( MAX( auxhist1_end_y, auxhist1_end_d,   &
             auxhist1_end_h, auxhist1_end_m , auxhist1_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist1_end_d, &
                                     H=auxhist1_end_h, M=auxhist1_end_m, S=auxhist1_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist1_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist1_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist1_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist1_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist2 INTERVAL
   CALL nl_get_auxhist2_interval( grid%id, auxhist2_interval )   ! same as minutes
   CALL nl_get_auxhist2_interval_d( grid%id, auxhist2_interval_d )
   CALL nl_get_auxhist2_interval_h( grid%id, auxhist2_interval_h )
   CALL nl_get_auxhist2_interval_m( grid%id, auxhist2_interval_m )
   CALL nl_get_auxhist2_interval_s( grid%id, auxhist2_interval_s )
   IF ( auxhist2_interval_m .EQ. 0 ) auxhist2_interval_m = auxhist2_interval
   IF ( MAX( auxhist2_interval_d,   &
             auxhist2_interval_h, auxhist2_interval_m , auxhist2_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist2_interval_d, &
                                        H=auxhist2_interval_h, M=auxhist2_interval_m, S=auxhist2_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist2_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist2_begin  ( grid%id, auxhist2_begin   )
   CALL nl_get_auxhist2_begin_y( grid%id, auxhist2_begin_y )
   CALL nl_get_auxhist2_begin_d( grid%id, auxhist2_begin_d )
   CALL nl_get_auxhist2_begin_h( grid%id, auxhist2_begin_h )
   CALL nl_get_auxhist2_begin_m( grid%id, auxhist2_begin_m )
   CALL nl_get_auxhist2_begin_s( grid%id, auxhist2_begin_s )
   IF ( auxhist2_begin_m .EQ. 0 ) auxhist2_begin_m = auxhist2_begin
   IF ( MAX( auxhist2_begin_y, auxhist2_begin_d,   &
             auxhist2_begin_h, auxhist2_begin_m , auxhist2_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist2_begin_d, &
                                      H=auxhist2_begin_h, M=auxhist2_begin_m, S=auxhist2_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist2_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist2_end( grid%id, auxhist2_end )
   CALL nl_get_auxhist2_end_y( grid%id, auxhist2_end_y )
   CALL nl_get_auxhist2_end_d( grid%id, auxhist2_end_d )
   CALL nl_get_auxhist2_end_h( grid%id, auxhist2_end_h )
   CALL nl_get_auxhist2_end_m( grid%id, auxhist2_end_m )
   CALL nl_get_auxhist2_end_s( grid%id, auxhist2_end_s )
   IF ( auxhist2_end_m .EQ. 0 ) auxhist2_end_m = auxhist2_end
   IF ( MAX( auxhist2_end_y, auxhist2_end_d,   &
             auxhist2_end_h, auxhist2_end_m , auxhist2_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist2_end_d, &
                                     H=auxhist2_end_h, M=auxhist2_end_m, S=auxhist2_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist2_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist2_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist2_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist2_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist3 INTERVAL
   CALL nl_get_auxhist3_interval( grid%id, auxhist3_interval )   ! same as minutes
   CALL nl_get_auxhist3_interval_d( grid%id, auxhist3_interval_d )
   CALL nl_get_auxhist3_interval_h( grid%id, auxhist3_interval_h )
   CALL nl_get_auxhist3_interval_m( grid%id, auxhist3_interval_m )
   CALL nl_get_auxhist3_interval_s( grid%id, auxhist3_interval_s )
   IF ( auxhist3_interval_m .EQ. 0 ) auxhist3_interval_m = auxhist3_interval
   IF ( MAX( auxhist3_interval_d,   &
             auxhist3_interval_h, auxhist3_interval_m , auxhist3_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist3_interval_d, &
                                        H=auxhist3_interval_h, M=auxhist3_interval_m, S=auxhist3_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist3_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist3_begin  ( grid%id, auxhist3_begin   )
   CALL nl_get_auxhist3_begin_y( grid%id, auxhist3_begin_y )
   CALL nl_get_auxhist3_begin_d( grid%id, auxhist3_begin_d )
   CALL nl_get_auxhist3_begin_h( grid%id, auxhist3_begin_h )
   CALL nl_get_auxhist3_begin_m( grid%id, auxhist3_begin_m )
   CALL nl_get_auxhist3_begin_s( grid%id, auxhist3_begin_s )
   IF ( auxhist3_begin_m .EQ. 0 ) auxhist3_begin_m = auxhist3_begin
   IF ( MAX( auxhist3_begin_y, auxhist3_begin_d,   &
             auxhist3_begin_h, auxhist3_begin_m , auxhist3_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist3_begin_d, &
                                      H=auxhist3_begin_h, M=auxhist3_begin_m, S=auxhist3_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist3_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist3_end( grid%id, auxhist3_end )
   CALL nl_get_auxhist3_end_y( grid%id, auxhist3_end_y )
   CALL nl_get_auxhist3_end_d( grid%id, auxhist3_end_d )
   CALL nl_get_auxhist3_end_h( grid%id, auxhist3_end_h )
   CALL nl_get_auxhist3_end_m( grid%id, auxhist3_end_m )
   CALL nl_get_auxhist3_end_s( grid%id, auxhist3_end_s )
   IF ( auxhist3_end_m .EQ. 0 ) auxhist3_end_m = auxhist3_end
   IF ( MAX( auxhist3_end_y, auxhist3_end_d,   &
             auxhist3_end_h, auxhist3_end_m , auxhist3_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist3_end_d, &
                                     H=auxhist3_end_h, M=auxhist3_end_m, S=auxhist3_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist3_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist3_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist3_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist3_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist4 INTERVAL
   CALL nl_get_auxhist4_interval( grid%id, auxhist4_interval )   ! same as minutes
   CALL nl_get_auxhist4_interval_d( grid%id, auxhist4_interval_d )
   CALL nl_get_auxhist4_interval_h( grid%id, auxhist4_interval_h )
   CALL nl_get_auxhist4_interval_m( grid%id, auxhist4_interval_m )
   CALL nl_get_auxhist4_interval_s( grid%id, auxhist4_interval_s )
   IF ( auxhist4_interval_m .EQ. 0 ) auxhist4_interval_m = auxhist4_interval
   IF ( MAX( auxhist4_interval_d,   &
             auxhist4_interval_h, auxhist4_interval_m , auxhist4_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist4_interval_d, &
                                        H=auxhist4_interval_h, M=auxhist4_interval_m, S=auxhist4_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist4_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist4_begin  ( grid%id, auxhist4_begin   )
   CALL nl_get_auxhist4_begin_y( grid%id, auxhist4_begin_y )
   CALL nl_get_auxhist4_begin_d( grid%id, auxhist4_begin_d )
   CALL nl_get_auxhist4_begin_h( grid%id, auxhist4_begin_h )
   CALL nl_get_auxhist4_begin_m( grid%id, auxhist4_begin_m )
   CALL nl_get_auxhist4_begin_s( grid%id, auxhist4_begin_s )
   IF ( auxhist4_begin_m .EQ. 0 ) auxhist4_begin_m = auxhist4_begin
   IF ( MAX( auxhist4_begin_y, auxhist4_begin_d,   &
             auxhist4_begin_h, auxhist4_begin_m , auxhist4_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist4_begin_d, &
                                      H=auxhist4_begin_h, M=auxhist4_begin_m, S=auxhist4_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist4_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist4_end( grid%id, auxhist4_end )
   CALL nl_get_auxhist4_end_y( grid%id, auxhist4_end_y )
   CALL nl_get_auxhist4_end_d( grid%id, auxhist4_end_d )
   CALL nl_get_auxhist4_end_h( grid%id, auxhist4_end_h )
   CALL nl_get_auxhist4_end_m( grid%id, auxhist4_end_m )
   CALL nl_get_auxhist4_end_s( grid%id, auxhist4_end_s )
   IF ( auxhist4_end_m .EQ. 0 ) auxhist4_end_m = auxhist4_end
   IF ( MAX( auxhist4_end_y, auxhist4_end_d,   &
             auxhist4_end_h, auxhist4_end_m , auxhist4_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist4_end_d, &
                                     H=auxhist4_end_h, M=auxhist4_end_m, S=auxhist4_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist4_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist4_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist4_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist4_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist5 INTERVAL
   CALL nl_get_auxhist5_interval( grid%id, auxhist5_interval )   ! same as minutes
   CALL nl_get_auxhist5_interval_d( grid%id, auxhist5_interval_d )
   CALL nl_get_auxhist5_interval_h( grid%id, auxhist5_interval_h )
   CALL nl_get_auxhist5_interval_m( grid%id, auxhist5_interval_m )
   CALL nl_get_auxhist5_interval_s( grid%id, auxhist5_interval_s )
   IF ( auxhist5_interval_m .EQ. 0 ) auxhist5_interval_m = auxhist5_interval
   IF ( MAX( auxhist5_interval_d,   &
             auxhist5_interval_h, auxhist5_interval_m , auxhist5_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist5_interval_d, &
                                        H=auxhist5_interval_h, M=auxhist5_interval_m, S=auxhist5_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist5_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist5_begin  ( grid%id, auxhist5_begin   )
   CALL nl_get_auxhist5_begin_y( grid%id, auxhist5_begin_y )
   CALL nl_get_auxhist5_begin_d( grid%id, auxhist5_begin_d )
   CALL nl_get_auxhist5_begin_h( grid%id, auxhist5_begin_h )
   CALL nl_get_auxhist5_begin_m( grid%id, auxhist5_begin_m )
   CALL nl_get_auxhist5_begin_s( grid%id, auxhist5_begin_s )
   IF ( auxhist5_begin_m .EQ. 0 ) auxhist5_begin_m = auxhist5_begin
   IF ( MAX( auxhist5_begin_y, auxhist5_begin_d,   &
             auxhist5_begin_h, auxhist5_begin_m , auxhist5_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist5_begin_d, &
                                      H=auxhist5_begin_h, M=auxhist5_begin_m, S=auxhist5_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist5_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist5_end( grid%id, auxhist5_end )
   CALL nl_get_auxhist5_end_y( grid%id, auxhist5_end_y )
   CALL nl_get_auxhist5_end_d( grid%id, auxhist5_end_d )
   CALL nl_get_auxhist5_end_h( grid%id, auxhist5_end_h )
   CALL nl_get_auxhist5_end_m( grid%id, auxhist5_end_m )
   CALL nl_get_auxhist5_end_s( grid%id, auxhist5_end_s )
   IF ( auxhist5_end_m .EQ. 0 ) auxhist5_end_m = auxhist5_end
   IF ( MAX( auxhist5_end_y, auxhist5_end_d,   &
             auxhist5_end_h, auxhist5_end_m , auxhist5_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist5_end_d, &
                                     H=auxhist5_end_h, M=auxhist5_end_m, S=auxhist5_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist5_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist5_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist5_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist5_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist6 INTERVAL
   CALL nl_get_auxhist6_interval( grid%id, auxhist6_interval )   ! same as minutes
   CALL nl_get_auxhist6_interval_d( grid%id, auxhist6_interval_d )
   CALL nl_get_auxhist6_interval_h( grid%id, auxhist6_interval_h )
   CALL nl_get_auxhist6_interval_m( grid%id, auxhist6_interval_m )
   CALL nl_get_auxhist6_interval_s( grid%id, auxhist6_interval_s )
   IF ( auxhist6_interval_m .EQ. 0 ) auxhist6_interval_m = auxhist6_interval
   IF ( MAX( auxhist6_interval_d,   &
             auxhist6_interval_h, auxhist6_interval_m , auxhist6_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist6_interval_d, &
                                        H=auxhist6_interval_h, M=auxhist6_interval_m, S=auxhist6_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist6_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist6_begin  ( grid%id, auxhist6_begin   )
   CALL nl_get_auxhist6_begin_y( grid%id, auxhist6_begin_y )
   CALL nl_get_auxhist6_begin_d( grid%id, auxhist6_begin_d )
   CALL nl_get_auxhist6_begin_h( grid%id, auxhist6_begin_h )
   CALL nl_get_auxhist6_begin_m( grid%id, auxhist6_begin_m )
   CALL nl_get_auxhist6_begin_s( grid%id, auxhist6_begin_s )
   IF ( auxhist6_begin_m .EQ. 0 ) auxhist6_begin_m = auxhist6_begin
   IF ( MAX( auxhist6_begin_y, auxhist6_begin_d,   &
             auxhist6_begin_h, auxhist6_begin_m , auxhist6_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist6_begin_d, &
                                      H=auxhist6_begin_h, M=auxhist6_begin_m, S=auxhist6_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist6_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist6_end( grid%id, auxhist6_end )
   CALL nl_get_auxhist6_end_y( grid%id, auxhist6_end_y )
   CALL nl_get_auxhist6_end_d( grid%id, auxhist6_end_d )
   CALL nl_get_auxhist6_end_h( grid%id, auxhist6_end_h )
   CALL nl_get_auxhist6_end_m( grid%id, auxhist6_end_m )
   CALL nl_get_auxhist6_end_s( grid%id, auxhist6_end_s )
   IF ( auxhist6_end_m .EQ. 0 ) auxhist6_end_m = auxhist6_end
   IF ( MAX( auxhist6_end_y, auxhist6_end_d,   &
             auxhist6_end_h, auxhist6_end_m , auxhist6_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist6_end_d, &
                                     H=auxhist6_end_h, M=auxhist6_end_m, S=auxhist6_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist6_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist6_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist6_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist6_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist7 INTERVAL
   CALL nl_get_auxhist7_interval( grid%id, auxhist7_interval )   ! same as minutes
   CALL nl_get_auxhist7_interval_d( grid%id, auxhist7_interval_d )
   CALL nl_get_auxhist7_interval_h( grid%id, auxhist7_interval_h )
   CALL nl_get_auxhist7_interval_m( grid%id, auxhist7_interval_m )
   CALL nl_get_auxhist7_interval_s( grid%id, auxhist7_interval_s )
   IF ( auxhist7_interval_m .EQ. 0 ) auxhist7_interval_m = auxhist7_interval
   IF ( MAX( auxhist7_interval_d,   &
             auxhist7_interval_h, auxhist7_interval_m , auxhist7_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist7_interval_d, &
                                        H=auxhist7_interval_h, M=auxhist7_interval_m, S=auxhist7_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist7_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist7_begin  ( grid%id, auxhist7_begin   )
   CALL nl_get_auxhist7_begin_y( grid%id, auxhist7_begin_y )
   CALL nl_get_auxhist7_begin_d( grid%id, auxhist7_begin_d )
   CALL nl_get_auxhist7_begin_h( grid%id, auxhist7_begin_h )
   CALL nl_get_auxhist7_begin_m( grid%id, auxhist7_begin_m )
   CALL nl_get_auxhist7_begin_s( grid%id, auxhist7_begin_s )
   IF ( auxhist7_begin_m .EQ. 0 ) auxhist7_begin_m = auxhist7_begin
   IF ( MAX( auxhist7_begin_y, auxhist7_begin_d,   &
             auxhist7_begin_h, auxhist7_begin_m , auxhist7_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist7_begin_d, &
                                      H=auxhist7_begin_h, M=auxhist7_begin_m, S=auxhist7_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist7_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist7_end( grid%id, auxhist7_end )
   CALL nl_get_auxhist7_end_y( grid%id, auxhist7_end_y )
   CALL nl_get_auxhist7_end_d( grid%id, auxhist7_end_d )
   CALL nl_get_auxhist7_end_h( grid%id, auxhist7_end_h )
   CALL nl_get_auxhist7_end_m( grid%id, auxhist7_end_m )
   CALL nl_get_auxhist7_end_s( grid%id, auxhist7_end_s )
   IF ( auxhist7_end_m .EQ. 0 ) auxhist7_end_m = auxhist7_end
   IF ( MAX( auxhist7_end_y, auxhist7_end_d,   &
             auxhist7_end_h, auxhist7_end_m , auxhist7_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist7_end_d, &
                                     H=auxhist7_end_h, M=auxhist7_end_m, S=auxhist7_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist7_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist7_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist7_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist7_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist8 INTERVAL
   CALL nl_get_auxhist8_interval( grid%id, auxhist8_interval )   ! same as minutes
   CALL nl_get_auxhist8_interval_d( grid%id, auxhist8_interval_d )
   CALL nl_get_auxhist8_interval_h( grid%id, auxhist8_interval_h )
   CALL nl_get_auxhist8_interval_m( grid%id, auxhist8_interval_m )
   CALL nl_get_auxhist8_interval_s( grid%id, auxhist8_interval_s )
   IF ( auxhist8_interval_m .EQ. 0 ) auxhist8_interval_m = auxhist8_interval
   IF ( MAX( auxhist8_interval_d,   &
             auxhist8_interval_h, auxhist8_interval_m , auxhist8_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist8_interval_d, &
                                        H=auxhist8_interval_h, M=auxhist8_interval_m, S=auxhist8_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist8_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist8_begin  ( grid%id, auxhist8_begin   )
   CALL nl_get_auxhist8_begin_y( grid%id, auxhist8_begin_y )
   CALL nl_get_auxhist8_begin_d( grid%id, auxhist8_begin_d )
   CALL nl_get_auxhist8_begin_h( grid%id, auxhist8_begin_h )
   CALL nl_get_auxhist8_begin_m( grid%id, auxhist8_begin_m )
   CALL nl_get_auxhist8_begin_s( grid%id, auxhist8_begin_s )
   IF ( auxhist8_begin_m .EQ. 0 ) auxhist8_begin_m = auxhist8_begin
   IF ( MAX( auxhist8_begin_y, auxhist8_begin_d,   &
             auxhist8_begin_h, auxhist8_begin_m , auxhist8_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist8_begin_d, &
                                      H=auxhist8_begin_h, M=auxhist8_begin_m, S=auxhist8_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist8_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist8_end( grid%id, auxhist8_end )
   CALL nl_get_auxhist8_end_y( grid%id, auxhist8_end_y )
   CALL nl_get_auxhist8_end_d( grid%id, auxhist8_end_d )
   CALL nl_get_auxhist8_end_h( grid%id, auxhist8_end_h )
   CALL nl_get_auxhist8_end_m( grid%id, auxhist8_end_m )
   CALL nl_get_auxhist8_end_s( grid%id, auxhist8_end_s )
   IF ( auxhist8_end_m .EQ. 0 ) auxhist8_end_m = auxhist8_end
   IF ( MAX( auxhist8_end_y, auxhist8_end_d,   &
             auxhist8_end_h, auxhist8_end_m , auxhist8_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist8_end_d, &
                                     H=auxhist8_end_h, M=auxhist8_end_m, S=auxhist8_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist8_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist8_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist8_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist8_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist9 INTERVAL
   CALL nl_get_auxhist9_interval( grid%id, auxhist9_interval )   ! same as minutes
   CALL nl_get_auxhist9_interval_d( grid%id, auxhist9_interval_d )
   CALL nl_get_auxhist9_interval_h( grid%id, auxhist9_interval_h )
   CALL nl_get_auxhist9_interval_m( grid%id, auxhist9_interval_m )
   CALL nl_get_auxhist9_interval_s( grid%id, auxhist9_interval_s )
   IF ( auxhist9_interval_m .EQ. 0 ) auxhist9_interval_m = auxhist9_interval
   IF ( MAX( auxhist9_interval_d,   &
             auxhist9_interval_h, auxhist9_interval_m , auxhist9_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist9_interval_d, &
                                        H=auxhist9_interval_h, M=auxhist9_interval_m, S=auxhist9_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist9_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist9_begin  ( grid%id, auxhist9_begin   )
   CALL nl_get_auxhist9_begin_y( grid%id, auxhist9_begin_y )
   CALL nl_get_auxhist9_begin_d( grid%id, auxhist9_begin_d )
   CALL nl_get_auxhist9_begin_h( grid%id, auxhist9_begin_h )
   CALL nl_get_auxhist9_begin_m( grid%id, auxhist9_begin_m )
   CALL nl_get_auxhist9_begin_s( grid%id, auxhist9_begin_s )
   IF ( auxhist9_begin_m .EQ. 0 ) auxhist9_begin_m = auxhist9_begin
   IF ( MAX( auxhist9_begin_y, auxhist9_begin_d,   &
             auxhist9_begin_h, auxhist9_begin_m , auxhist9_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist9_begin_d, &
                                      H=auxhist9_begin_h, M=auxhist9_begin_m, S=auxhist9_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist9_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist9_end( grid%id, auxhist9_end )
   CALL nl_get_auxhist9_end_y( grid%id, auxhist9_end_y )
   CALL nl_get_auxhist9_end_d( grid%id, auxhist9_end_d )
   CALL nl_get_auxhist9_end_h( grid%id, auxhist9_end_h )
   CALL nl_get_auxhist9_end_m( grid%id, auxhist9_end_m )
   CALL nl_get_auxhist9_end_s( grid%id, auxhist9_end_s )
   IF ( auxhist9_end_m .EQ. 0 ) auxhist9_end_m = auxhist9_end
   IF ( MAX( auxhist9_end_y, auxhist9_end_d,   &
             auxhist9_end_h, auxhist9_end_m , auxhist9_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist9_end_d, &
                                     H=auxhist9_end_h, M=auxhist9_end_m, S=auxhist9_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist9_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist9_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist9_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist9_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist10 INTERVAL
   CALL nl_get_auxhist10_interval( grid%id, auxhist10_interval )   ! same as minutes
   CALL nl_get_auxhist10_interval_d( grid%id, auxhist10_interval_d )
   CALL nl_get_auxhist10_interval_h( grid%id, auxhist10_interval_h )
   CALL nl_get_auxhist10_interval_m( grid%id, auxhist10_interval_m )
   CALL nl_get_auxhist10_interval_s( grid%id, auxhist10_interval_s )
   IF ( auxhist10_interval_m .EQ. 0 ) auxhist10_interval_m = auxhist10_interval
   IF ( MAX( auxhist10_interval_d,   &
             auxhist10_interval_h, auxhist10_interval_m , auxhist10_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist10_interval_d, &
                                        H=auxhist10_interval_h, M=auxhist10_interval_m, S=auxhist10_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist10_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist10_begin  ( grid%id, auxhist10_begin   )
   CALL nl_get_auxhist10_begin_y( grid%id, auxhist10_begin_y )
   CALL nl_get_auxhist10_begin_d( grid%id, auxhist10_begin_d )
   CALL nl_get_auxhist10_begin_h( grid%id, auxhist10_begin_h )
   CALL nl_get_auxhist10_begin_m( grid%id, auxhist10_begin_m )
   CALL nl_get_auxhist10_begin_s( grid%id, auxhist10_begin_s )
   IF ( auxhist10_begin_m .EQ. 0 ) auxhist10_begin_m = auxhist10_begin
   IF ( MAX( auxhist10_begin_y, auxhist10_begin_d,   &
             auxhist10_begin_h, auxhist10_begin_m , auxhist10_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist10_begin_d, &
                                      H=auxhist10_begin_h, M=auxhist10_begin_m, S=auxhist10_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist10_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist10_end( grid%id, auxhist10_end )
   CALL nl_get_auxhist10_end_y( grid%id, auxhist10_end_y )
   CALL nl_get_auxhist10_end_d( grid%id, auxhist10_end_d )
   CALL nl_get_auxhist10_end_h( grid%id, auxhist10_end_h )
   CALL nl_get_auxhist10_end_m( grid%id, auxhist10_end_m )
   CALL nl_get_auxhist10_end_s( grid%id, auxhist10_end_s )
   IF ( auxhist10_end_m .EQ. 0 ) auxhist10_end_m = auxhist10_end
   IF ( MAX( auxhist10_end_y, auxhist10_end_d,   &
             auxhist10_end_h, auxhist10_end_m , auxhist10_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist10_end_d, &
                                     H=auxhist10_end_h, M=auxhist10_end_m, S=auxhist10_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist10_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist10_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist10_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist10_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist11 INTERVAL
   CALL nl_get_auxhist11_interval( grid%id, auxhist11_interval )   ! same as minutes
   CALL nl_get_auxhist11_interval_d( grid%id, auxhist11_interval_d )
   CALL nl_get_auxhist11_interval_h( grid%id, auxhist11_interval_h )
   CALL nl_get_auxhist11_interval_m( grid%id, auxhist11_interval_m )
   CALL nl_get_auxhist11_interval_s( grid%id, auxhist11_interval_s )
   IF ( auxhist11_interval_m .EQ. 0 ) auxhist11_interval_m = auxhist11_interval
   IF ( MAX( auxhist11_interval_d,   &
             auxhist11_interval_h, auxhist11_interval_m , auxhist11_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist11_interval_d, &
                                        H=auxhist11_interval_h, M=auxhist11_interval_m, S=auxhist11_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist11_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist11_begin  ( grid%id, auxhist11_begin   )
   CALL nl_get_auxhist11_begin_y( grid%id, auxhist11_begin_y )
   CALL nl_get_auxhist11_begin_d( grid%id, auxhist11_begin_d )
   CALL nl_get_auxhist11_begin_h( grid%id, auxhist11_begin_h )
   CALL nl_get_auxhist11_begin_m( grid%id, auxhist11_begin_m )
   CALL nl_get_auxhist11_begin_s( grid%id, auxhist11_begin_s )
   IF ( auxhist11_begin_m .EQ. 0 ) auxhist11_begin_m = auxhist11_begin
   IF ( MAX( auxhist11_begin_y, auxhist11_begin_d,   &
             auxhist11_begin_h, auxhist11_begin_m , auxhist11_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist11_begin_d, &
                                      H=auxhist11_begin_h, M=auxhist11_begin_m, S=auxhist11_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist11_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist11_end( grid%id, auxhist11_end )
   CALL nl_get_auxhist11_end_y( grid%id, auxhist11_end_y )
   CALL nl_get_auxhist11_end_d( grid%id, auxhist11_end_d )
   CALL nl_get_auxhist11_end_h( grid%id, auxhist11_end_h )
   CALL nl_get_auxhist11_end_m( grid%id, auxhist11_end_m )
   CALL nl_get_auxhist11_end_s( grid%id, auxhist11_end_s )
   IF ( auxhist11_end_m .EQ. 0 ) auxhist11_end_m = auxhist11_end
   IF ( MAX( auxhist11_end_y, auxhist11_end_d,   &
             auxhist11_end_h, auxhist11_end_m , auxhist11_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist11_end_d, &
                                     H=auxhist11_end_h, M=auxhist11_end_m, S=auxhist11_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist11_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist11_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist11_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist11_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist12 INTERVAL
   CALL nl_get_auxhist12_interval( grid%id, auxhist12_interval )   ! same as minutes
   CALL nl_get_auxhist12_interval_d( grid%id, auxhist12_interval_d )
   CALL nl_get_auxhist12_interval_h( grid%id, auxhist12_interval_h )
   CALL nl_get_auxhist12_interval_m( grid%id, auxhist12_interval_m )
   CALL nl_get_auxhist12_interval_s( grid%id, auxhist12_interval_s )
   IF ( auxhist12_interval_m .EQ. 0 ) auxhist12_interval_m = auxhist12_interval
   IF ( MAX( auxhist12_interval_d,   &
             auxhist12_interval_h, auxhist12_interval_m , auxhist12_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist12_interval_d, &
                                        H=auxhist12_interval_h, M=auxhist12_interval_m, S=auxhist12_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist12_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist12_begin  ( grid%id, auxhist12_begin   )
   CALL nl_get_auxhist12_begin_y( grid%id, auxhist12_begin_y )
   CALL nl_get_auxhist12_begin_d( grid%id, auxhist12_begin_d )
   CALL nl_get_auxhist12_begin_h( grid%id, auxhist12_begin_h )
   CALL nl_get_auxhist12_begin_m( grid%id, auxhist12_begin_m )
   CALL nl_get_auxhist12_begin_s( grid%id, auxhist12_begin_s )
   IF ( auxhist12_begin_m .EQ. 0 ) auxhist12_begin_m = auxhist12_begin
   IF ( MAX( auxhist12_begin_y, auxhist12_begin_d,   &
             auxhist12_begin_h, auxhist12_begin_m , auxhist12_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist12_begin_d, &
                                      H=auxhist12_begin_h, M=auxhist12_begin_m, S=auxhist12_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist12_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist12_end( grid%id, auxhist12_end )
   CALL nl_get_auxhist12_end_y( grid%id, auxhist12_end_y )
   CALL nl_get_auxhist12_end_d( grid%id, auxhist12_end_d )
   CALL nl_get_auxhist12_end_h( grid%id, auxhist12_end_h )
   CALL nl_get_auxhist12_end_m( grid%id, auxhist12_end_m )
   CALL nl_get_auxhist12_end_s( grid%id, auxhist12_end_s )
   IF ( auxhist12_end_m .EQ. 0 ) auxhist12_end_m = auxhist12_end
   IF ( MAX( auxhist12_end_y, auxhist12_end_d,   &
             auxhist12_end_h, auxhist12_end_m , auxhist12_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist12_end_d, &
                                     H=auxhist12_end_h, M=auxhist12_end_m, S=auxhist12_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist12_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist12_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist12_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist12_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist13 INTERVAL
   CALL nl_get_auxhist13_interval( grid%id, auxhist13_interval )   ! same as minutes
   CALL nl_get_auxhist13_interval_d( grid%id, auxhist13_interval_d )
   CALL nl_get_auxhist13_interval_h( grid%id, auxhist13_interval_h )
   CALL nl_get_auxhist13_interval_m( grid%id, auxhist13_interval_m )
   CALL nl_get_auxhist13_interval_s( grid%id, auxhist13_interval_s )
   IF ( auxhist13_interval_m .EQ. 0 ) auxhist13_interval_m = auxhist13_interval
   IF ( MAX( auxhist13_interval_d,   &
             auxhist13_interval_h, auxhist13_interval_m , auxhist13_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist13_interval_d, &
                                        H=auxhist13_interval_h, M=auxhist13_interval_m, S=auxhist13_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist13_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist13_begin  ( grid%id, auxhist13_begin   )
   CALL nl_get_auxhist13_begin_y( grid%id, auxhist13_begin_y )
   CALL nl_get_auxhist13_begin_d( grid%id, auxhist13_begin_d )
   CALL nl_get_auxhist13_begin_h( grid%id, auxhist13_begin_h )
   CALL nl_get_auxhist13_begin_m( grid%id, auxhist13_begin_m )
   CALL nl_get_auxhist13_begin_s( grid%id, auxhist13_begin_s )
   IF ( auxhist13_begin_m .EQ. 0 ) auxhist13_begin_m = auxhist13_begin
   IF ( MAX( auxhist13_begin_y, auxhist13_begin_d,   &
             auxhist13_begin_h, auxhist13_begin_m , auxhist13_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist13_begin_d, &
                                      H=auxhist13_begin_h, M=auxhist13_begin_m, S=auxhist13_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist13_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist13_end( grid%id, auxhist13_end )
   CALL nl_get_auxhist13_end_y( grid%id, auxhist13_end_y )
   CALL nl_get_auxhist13_end_d( grid%id, auxhist13_end_d )
   CALL nl_get_auxhist13_end_h( grid%id, auxhist13_end_h )
   CALL nl_get_auxhist13_end_m( grid%id, auxhist13_end_m )
   CALL nl_get_auxhist13_end_s( grid%id, auxhist13_end_s )
   IF ( auxhist13_end_m .EQ. 0 ) auxhist13_end_m = auxhist13_end
   IF ( MAX( auxhist13_end_y, auxhist13_end_d,   &
             auxhist13_end_h, auxhist13_end_m , auxhist13_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist13_end_d, &
                                     H=auxhist13_end_h, M=auxhist13_end_m, S=auxhist13_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist13_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist13_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist13_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist13_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist14 INTERVAL
   CALL nl_get_auxhist14_interval( grid%id, auxhist14_interval )   ! same as minutes
   CALL nl_get_auxhist14_interval_d( grid%id, auxhist14_interval_d )
   CALL nl_get_auxhist14_interval_h( grid%id, auxhist14_interval_h )
   CALL nl_get_auxhist14_interval_m( grid%id, auxhist14_interval_m )
   CALL nl_get_auxhist14_interval_s( grid%id, auxhist14_interval_s )
   IF ( auxhist14_interval_m .EQ. 0 ) auxhist14_interval_m = auxhist14_interval
   IF ( MAX( auxhist14_interval_d,   &
             auxhist14_interval_h, auxhist14_interval_m , auxhist14_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist14_interval_d, &
                                        H=auxhist14_interval_h, M=auxhist14_interval_m, S=auxhist14_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist14_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist14_begin  ( grid%id, auxhist14_begin   )
   CALL nl_get_auxhist14_begin_y( grid%id, auxhist14_begin_y )
   CALL nl_get_auxhist14_begin_d( grid%id, auxhist14_begin_d )
   CALL nl_get_auxhist14_begin_h( grid%id, auxhist14_begin_h )
   CALL nl_get_auxhist14_begin_m( grid%id, auxhist14_begin_m )
   CALL nl_get_auxhist14_begin_s( grid%id, auxhist14_begin_s )
   IF ( auxhist14_begin_m .EQ. 0 ) auxhist14_begin_m = auxhist14_begin
   IF ( MAX( auxhist14_begin_y, auxhist14_begin_d,   &
             auxhist14_begin_h, auxhist14_begin_m , auxhist14_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist14_begin_d, &
                                      H=auxhist14_begin_h, M=auxhist14_begin_m, S=auxhist14_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist14_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist14_end( grid%id, auxhist14_end )
   CALL nl_get_auxhist14_end_y( grid%id, auxhist14_end_y )
   CALL nl_get_auxhist14_end_d( grid%id, auxhist14_end_d )
   CALL nl_get_auxhist14_end_h( grid%id, auxhist14_end_h )
   CALL nl_get_auxhist14_end_m( grid%id, auxhist14_end_m )
   CALL nl_get_auxhist14_end_s( grid%id, auxhist14_end_s )
   IF ( auxhist14_end_m .EQ. 0 ) auxhist14_end_m = auxhist14_end
   IF ( MAX( auxhist14_end_y, auxhist14_end_d,   &
             auxhist14_end_h, auxhist14_end_m , auxhist14_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist14_end_d, &
                                     H=auxhist14_end_h, M=auxhist14_end_m, S=auxhist14_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist14_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist14_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist14_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist14_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist15 INTERVAL
   CALL nl_get_auxhist15_interval( grid%id, auxhist15_interval )   ! same as minutes
   CALL nl_get_auxhist15_interval_d( grid%id, auxhist15_interval_d )
   CALL nl_get_auxhist15_interval_h( grid%id, auxhist15_interval_h )
   CALL nl_get_auxhist15_interval_m( grid%id, auxhist15_interval_m )
   CALL nl_get_auxhist15_interval_s( grid%id, auxhist15_interval_s )
   IF ( auxhist15_interval_m .EQ. 0 ) auxhist15_interval_m = auxhist15_interval
   IF ( MAX( auxhist15_interval_d,   &
             auxhist15_interval_h, auxhist15_interval_m , auxhist15_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist15_interval_d, &
                                        H=auxhist15_interval_h, M=auxhist15_interval_m, S=auxhist15_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist15_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist15_begin  ( grid%id, auxhist15_begin   )
   CALL nl_get_auxhist15_begin_y( grid%id, auxhist15_begin_y )
   CALL nl_get_auxhist15_begin_d( grid%id, auxhist15_begin_d )
   CALL nl_get_auxhist15_begin_h( grid%id, auxhist15_begin_h )
   CALL nl_get_auxhist15_begin_m( grid%id, auxhist15_begin_m )
   CALL nl_get_auxhist15_begin_s( grid%id, auxhist15_begin_s )
   IF ( auxhist15_begin_m .EQ. 0 ) auxhist15_begin_m = auxhist15_begin
   IF ( MAX( auxhist15_begin_y, auxhist15_begin_d,   &
             auxhist15_begin_h, auxhist15_begin_m , auxhist15_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist15_begin_d, &
                                      H=auxhist15_begin_h, M=auxhist15_begin_m, S=auxhist15_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist15_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist15_end( grid%id, auxhist15_end )
   CALL nl_get_auxhist15_end_y( grid%id, auxhist15_end_y )
   CALL nl_get_auxhist15_end_d( grid%id, auxhist15_end_d )
   CALL nl_get_auxhist15_end_h( grid%id, auxhist15_end_h )
   CALL nl_get_auxhist15_end_m( grid%id, auxhist15_end_m )
   CALL nl_get_auxhist15_end_s( grid%id, auxhist15_end_s )
   IF ( auxhist15_end_m .EQ. 0 ) auxhist15_end_m = auxhist15_end
   IF ( MAX( auxhist15_end_y, auxhist15_end_d,   &
             auxhist15_end_h, auxhist15_end_m , auxhist15_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist15_end_d, &
                                     H=auxhist15_end_h, M=auxhist15_end_m, S=auxhist15_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist15_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist15_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist15_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist15_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist16 INTERVAL
   CALL nl_get_auxhist16_interval( grid%id, auxhist16_interval )   ! same as minutes
   CALL nl_get_auxhist16_interval_d( grid%id, auxhist16_interval_d )
   CALL nl_get_auxhist16_interval_h( grid%id, auxhist16_interval_h )
   CALL nl_get_auxhist16_interval_m( grid%id, auxhist16_interval_m )
   CALL nl_get_auxhist16_interval_s( grid%id, auxhist16_interval_s )
   IF ( auxhist16_interval_m .EQ. 0 ) auxhist16_interval_m = auxhist16_interval
   IF ( MAX( auxhist16_interval_d,   &
             auxhist16_interval_h, auxhist16_interval_m , auxhist16_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist16_interval_d, &
                                        H=auxhist16_interval_h, M=auxhist16_interval_m, S=auxhist16_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist16_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist16_begin  ( grid%id, auxhist16_begin   )
   CALL nl_get_auxhist16_begin_y( grid%id, auxhist16_begin_y )
   CALL nl_get_auxhist16_begin_d( grid%id, auxhist16_begin_d )
   CALL nl_get_auxhist16_begin_h( grid%id, auxhist16_begin_h )
   CALL nl_get_auxhist16_begin_m( grid%id, auxhist16_begin_m )
   CALL nl_get_auxhist16_begin_s( grid%id, auxhist16_begin_s )
   IF ( auxhist16_begin_m .EQ. 0 ) auxhist16_begin_m = auxhist16_begin
   IF ( MAX( auxhist16_begin_y, auxhist16_begin_d,   &
             auxhist16_begin_h, auxhist16_begin_m , auxhist16_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist16_begin_d, &
                                      H=auxhist16_begin_h, M=auxhist16_begin_m, S=auxhist16_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist16_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist16_end( grid%id, auxhist16_end )
   CALL nl_get_auxhist16_end_y( grid%id, auxhist16_end_y )
   CALL nl_get_auxhist16_end_d( grid%id, auxhist16_end_d )
   CALL nl_get_auxhist16_end_h( grid%id, auxhist16_end_h )
   CALL nl_get_auxhist16_end_m( grid%id, auxhist16_end_m )
   CALL nl_get_auxhist16_end_s( grid%id, auxhist16_end_s )
   IF ( auxhist16_end_m .EQ. 0 ) auxhist16_end_m = auxhist16_end
   IF ( MAX( auxhist16_end_y, auxhist16_end_d,   &
             auxhist16_end_h, auxhist16_end_m , auxhist16_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist16_end_d, &
                                     H=auxhist16_end_h, M=auxhist16_end_m, S=auxhist16_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist16_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist16_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist16_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist16_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist17 INTERVAL
   CALL nl_get_auxhist17_interval( grid%id, auxhist17_interval )   ! same as minutes
   CALL nl_get_auxhist17_interval_d( grid%id, auxhist17_interval_d )
   CALL nl_get_auxhist17_interval_h( grid%id, auxhist17_interval_h )
   CALL nl_get_auxhist17_interval_m( grid%id, auxhist17_interval_m )
   CALL nl_get_auxhist17_interval_s( grid%id, auxhist17_interval_s )
   IF ( auxhist17_interval_m .EQ. 0 ) auxhist17_interval_m = auxhist17_interval
   IF ( MAX( auxhist17_interval_d,   &
             auxhist17_interval_h, auxhist17_interval_m , auxhist17_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist17_interval_d, &
                                        H=auxhist17_interval_h, M=auxhist17_interval_m, S=auxhist17_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist17_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist17_begin  ( grid%id, auxhist17_begin   )
   CALL nl_get_auxhist17_begin_y( grid%id, auxhist17_begin_y )
   CALL nl_get_auxhist17_begin_d( grid%id, auxhist17_begin_d )
   CALL nl_get_auxhist17_begin_h( grid%id, auxhist17_begin_h )
   CALL nl_get_auxhist17_begin_m( grid%id, auxhist17_begin_m )
   CALL nl_get_auxhist17_begin_s( grid%id, auxhist17_begin_s )
   IF ( auxhist17_begin_m .EQ. 0 ) auxhist17_begin_m = auxhist17_begin
   IF ( MAX( auxhist17_begin_y, auxhist17_begin_d,   &
             auxhist17_begin_h, auxhist17_begin_m , auxhist17_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist17_begin_d, &
                                      H=auxhist17_begin_h, M=auxhist17_begin_m, S=auxhist17_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist17_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist17_end( grid%id, auxhist17_end )
   CALL nl_get_auxhist17_end_y( grid%id, auxhist17_end_y )
   CALL nl_get_auxhist17_end_d( grid%id, auxhist17_end_d )
   CALL nl_get_auxhist17_end_h( grid%id, auxhist17_end_h )
   CALL nl_get_auxhist17_end_m( grid%id, auxhist17_end_m )
   CALL nl_get_auxhist17_end_s( grid%id, auxhist17_end_s )
   IF ( auxhist17_end_m .EQ. 0 ) auxhist17_end_m = auxhist17_end
   IF ( MAX( auxhist17_end_y, auxhist17_end_d,   &
             auxhist17_end_h, auxhist17_end_m , auxhist17_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist17_end_d, &
                                     H=auxhist17_end_h, M=auxhist17_end_m, S=auxhist17_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist17_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist17_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist17_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist17_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist18 INTERVAL
   CALL nl_get_auxhist18_interval( grid%id, auxhist18_interval )   ! same as minutes
   CALL nl_get_auxhist18_interval_d( grid%id, auxhist18_interval_d )
   CALL nl_get_auxhist18_interval_h( grid%id, auxhist18_interval_h )
   CALL nl_get_auxhist18_interval_m( grid%id, auxhist18_interval_m )
   CALL nl_get_auxhist18_interval_s( grid%id, auxhist18_interval_s )
   IF ( auxhist18_interval_m .EQ. 0 ) auxhist18_interval_m = auxhist18_interval
   IF ( MAX( auxhist18_interval_d,   &
             auxhist18_interval_h, auxhist18_interval_m , auxhist18_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist18_interval_d, &
                                        H=auxhist18_interval_h, M=auxhist18_interval_m, S=auxhist18_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist18_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist18_begin  ( grid%id, auxhist18_begin   )
   CALL nl_get_auxhist18_begin_y( grid%id, auxhist18_begin_y )
   CALL nl_get_auxhist18_begin_d( grid%id, auxhist18_begin_d )
   CALL nl_get_auxhist18_begin_h( grid%id, auxhist18_begin_h )
   CALL nl_get_auxhist18_begin_m( grid%id, auxhist18_begin_m )
   CALL nl_get_auxhist18_begin_s( grid%id, auxhist18_begin_s )
   IF ( auxhist18_begin_m .EQ. 0 ) auxhist18_begin_m = auxhist18_begin
   IF ( MAX( auxhist18_begin_y, auxhist18_begin_d,   &
             auxhist18_begin_h, auxhist18_begin_m , auxhist18_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist18_begin_d, &
                                      H=auxhist18_begin_h, M=auxhist18_begin_m, S=auxhist18_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist18_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist18_end( grid%id, auxhist18_end )
   CALL nl_get_auxhist18_end_y( grid%id, auxhist18_end_y )
   CALL nl_get_auxhist18_end_d( grid%id, auxhist18_end_d )
   CALL nl_get_auxhist18_end_h( grid%id, auxhist18_end_h )
   CALL nl_get_auxhist18_end_m( grid%id, auxhist18_end_m )
   CALL nl_get_auxhist18_end_s( grid%id, auxhist18_end_s )
   IF ( auxhist18_end_m .EQ. 0 ) auxhist18_end_m = auxhist18_end
   IF ( MAX( auxhist18_end_y, auxhist18_end_d,   &
             auxhist18_end_h, auxhist18_end_m , auxhist18_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist18_end_d, &
                                     H=auxhist18_end_h, M=auxhist18_end_m, S=auxhist18_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist18_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist18_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist18_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist18_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist19 INTERVAL
   CALL nl_get_auxhist19_interval( grid%id, auxhist19_interval )   ! same as minutes
   CALL nl_get_auxhist19_interval_d( grid%id, auxhist19_interval_d )
   CALL nl_get_auxhist19_interval_h( grid%id, auxhist19_interval_h )
   CALL nl_get_auxhist19_interval_m( grid%id, auxhist19_interval_m )
   CALL nl_get_auxhist19_interval_s( grid%id, auxhist19_interval_s )
   IF ( auxhist19_interval_m .EQ. 0 ) auxhist19_interval_m = auxhist19_interval
   IF ( MAX( auxhist19_interval_d,   &
             auxhist19_interval_h, auxhist19_interval_m , auxhist19_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist19_interval_d, &
                                        H=auxhist19_interval_h, M=auxhist19_interval_m, S=auxhist19_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist19_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist19_begin  ( grid%id, auxhist19_begin   )
   CALL nl_get_auxhist19_begin_y( grid%id, auxhist19_begin_y )
   CALL nl_get_auxhist19_begin_d( grid%id, auxhist19_begin_d )
   CALL nl_get_auxhist19_begin_h( grid%id, auxhist19_begin_h )
   CALL nl_get_auxhist19_begin_m( grid%id, auxhist19_begin_m )
   CALL nl_get_auxhist19_begin_s( grid%id, auxhist19_begin_s )
   IF ( auxhist19_begin_m .EQ. 0 ) auxhist19_begin_m = auxhist19_begin
   IF ( MAX( auxhist19_begin_y, auxhist19_begin_d,   &
             auxhist19_begin_h, auxhist19_begin_m , auxhist19_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist19_begin_d, &
                                      H=auxhist19_begin_h, M=auxhist19_begin_m, S=auxhist19_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist19_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist19_end( grid%id, auxhist19_end )
   CALL nl_get_auxhist19_end_y( grid%id, auxhist19_end_y )
   CALL nl_get_auxhist19_end_d( grid%id, auxhist19_end_d )
   CALL nl_get_auxhist19_end_h( grid%id, auxhist19_end_h )
   CALL nl_get_auxhist19_end_m( grid%id, auxhist19_end_m )
   CALL nl_get_auxhist19_end_s( grid%id, auxhist19_end_s )
   IF ( auxhist19_end_m .EQ. 0 ) auxhist19_end_m = auxhist19_end
   IF ( MAX( auxhist19_end_y, auxhist19_end_d,   &
             auxhist19_end_h, auxhist19_end_m , auxhist19_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist19_end_d, &
                                     H=auxhist19_end_h, M=auxhist19_end_m, S=auxhist19_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist19_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist19_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist19_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist19_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist20 INTERVAL
   CALL nl_get_auxhist20_interval( grid%id, auxhist20_interval )   ! same as minutes
   CALL nl_get_auxhist20_interval_d( grid%id, auxhist20_interval_d )
   CALL nl_get_auxhist20_interval_h( grid%id, auxhist20_interval_h )
   CALL nl_get_auxhist20_interval_m( grid%id, auxhist20_interval_m )
   CALL nl_get_auxhist20_interval_s( grid%id, auxhist20_interval_s )
   IF ( auxhist20_interval_m .EQ. 0 ) auxhist20_interval_m = auxhist20_interval
   IF ( MAX( auxhist20_interval_d,   &
             auxhist20_interval_h, auxhist20_interval_m , auxhist20_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist20_interval_d, &
                                        H=auxhist20_interval_h, M=auxhist20_interval_m, S=auxhist20_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist20_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist20_begin  ( grid%id, auxhist20_begin   )
   CALL nl_get_auxhist20_begin_y( grid%id, auxhist20_begin_y )
   CALL nl_get_auxhist20_begin_d( grid%id, auxhist20_begin_d )
   CALL nl_get_auxhist20_begin_h( grid%id, auxhist20_begin_h )
   CALL nl_get_auxhist20_begin_m( grid%id, auxhist20_begin_m )
   CALL nl_get_auxhist20_begin_s( grid%id, auxhist20_begin_s )
   IF ( auxhist20_begin_m .EQ. 0 ) auxhist20_begin_m = auxhist20_begin
   IF ( MAX( auxhist20_begin_y, auxhist20_begin_d,   &
             auxhist20_begin_h, auxhist20_begin_m , auxhist20_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist20_begin_d, &
                                      H=auxhist20_begin_h, M=auxhist20_begin_m, S=auxhist20_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist20_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist20_end( grid%id, auxhist20_end )
   CALL nl_get_auxhist20_end_y( grid%id, auxhist20_end_y )
   CALL nl_get_auxhist20_end_d( grid%id, auxhist20_end_d )
   CALL nl_get_auxhist20_end_h( grid%id, auxhist20_end_h )
   CALL nl_get_auxhist20_end_m( grid%id, auxhist20_end_m )
   CALL nl_get_auxhist20_end_s( grid%id, auxhist20_end_s )
   IF ( auxhist20_end_m .EQ. 0 ) auxhist20_end_m = auxhist20_end
   IF ( MAX( auxhist20_end_y, auxhist20_end_d,   &
             auxhist20_end_h, auxhist20_end_m , auxhist20_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist20_end_d, &
                                     H=auxhist20_end_h, M=auxhist20_end_m, S=auxhist20_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist20_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist20_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist20_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist20_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist21 INTERVAL
   CALL nl_get_auxhist21_interval( grid%id, auxhist21_interval )   ! same as minutes
   CALL nl_get_auxhist21_interval_d( grid%id, auxhist21_interval_d )
   CALL nl_get_auxhist21_interval_h( grid%id, auxhist21_interval_h )
   CALL nl_get_auxhist21_interval_m( grid%id, auxhist21_interval_m )
   CALL nl_get_auxhist21_interval_s( grid%id, auxhist21_interval_s )
   IF ( auxhist21_interval_m .EQ. 0 ) auxhist21_interval_m = auxhist21_interval
   IF ( MAX( auxhist21_interval_d,   &
             auxhist21_interval_h, auxhist21_interval_m , auxhist21_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist21_interval_d, &
                                        H=auxhist21_interval_h, M=auxhist21_interval_m, S=auxhist21_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist21_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist21_begin  ( grid%id, auxhist21_begin   )
   CALL nl_get_auxhist21_begin_y( grid%id, auxhist21_begin_y )
   CALL nl_get_auxhist21_begin_d( grid%id, auxhist21_begin_d )
   CALL nl_get_auxhist21_begin_h( grid%id, auxhist21_begin_h )
   CALL nl_get_auxhist21_begin_m( grid%id, auxhist21_begin_m )
   CALL nl_get_auxhist21_begin_s( grid%id, auxhist21_begin_s )
   IF ( auxhist21_begin_m .EQ. 0 ) auxhist21_begin_m = auxhist21_begin
   IF ( MAX( auxhist21_begin_y, auxhist21_begin_d,   &
             auxhist21_begin_h, auxhist21_begin_m , auxhist21_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist21_begin_d, &
                                      H=auxhist21_begin_h, M=auxhist21_begin_m, S=auxhist21_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist21_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist21_end( grid%id, auxhist21_end )
   CALL nl_get_auxhist21_end_y( grid%id, auxhist21_end_y )
   CALL nl_get_auxhist21_end_d( grid%id, auxhist21_end_d )
   CALL nl_get_auxhist21_end_h( grid%id, auxhist21_end_h )
   CALL nl_get_auxhist21_end_m( grid%id, auxhist21_end_m )
   CALL nl_get_auxhist21_end_s( grid%id, auxhist21_end_s )
   IF ( auxhist21_end_m .EQ. 0 ) auxhist21_end_m = auxhist21_end
   IF ( MAX( auxhist21_end_y, auxhist21_end_d,   &
             auxhist21_end_h, auxhist21_end_m , auxhist21_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist21_end_d, &
                                     H=auxhist21_end_h, M=auxhist21_end_m, S=auxhist21_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist21_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist21_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist21_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist21_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist22 INTERVAL
   CALL nl_get_auxhist22_interval( grid%id, auxhist22_interval )   ! same as minutes
   CALL nl_get_auxhist22_interval_d( grid%id, auxhist22_interval_d )
   CALL nl_get_auxhist22_interval_h( grid%id, auxhist22_interval_h )
   CALL nl_get_auxhist22_interval_m( grid%id, auxhist22_interval_m )
   CALL nl_get_auxhist22_interval_s( grid%id, auxhist22_interval_s )
   IF ( auxhist22_interval_m .EQ. 0 ) auxhist22_interval_m = auxhist22_interval
   IF ( MAX( auxhist22_interval_d,   &
             auxhist22_interval_h, auxhist22_interval_m , auxhist22_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist22_interval_d, &
                                        H=auxhist22_interval_h, M=auxhist22_interval_m, S=auxhist22_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist22_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist22_begin  ( grid%id, auxhist22_begin   )
   CALL nl_get_auxhist22_begin_y( grid%id, auxhist22_begin_y )
   CALL nl_get_auxhist22_begin_d( grid%id, auxhist22_begin_d )
   CALL nl_get_auxhist22_begin_h( grid%id, auxhist22_begin_h )
   CALL nl_get_auxhist22_begin_m( grid%id, auxhist22_begin_m )
   CALL nl_get_auxhist22_begin_s( grid%id, auxhist22_begin_s )
   IF ( auxhist22_begin_m .EQ. 0 ) auxhist22_begin_m = auxhist22_begin
   IF ( MAX( auxhist22_begin_y, auxhist22_begin_d,   &
             auxhist22_begin_h, auxhist22_begin_m , auxhist22_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist22_begin_d, &
                                      H=auxhist22_begin_h, M=auxhist22_begin_m, S=auxhist22_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist22_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist22_end( grid%id, auxhist22_end )
   CALL nl_get_auxhist22_end_y( grid%id, auxhist22_end_y )
   CALL nl_get_auxhist22_end_d( grid%id, auxhist22_end_d )
   CALL nl_get_auxhist22_end_h( grid%id, auxhist22_end_h )
   CALL nl_get_auxhist22_end_m( grid%id, auxhist22_end_m )
   CALL nl_get_auxhist22_end_s( grid%id, auxhist22_end_s )
   IF ( auxhist22_end_m .EQ. 0 ) auxhist22_end_m = auxhist22_end
   IF ( MAX( auxhist22_end_y, auxhist22_end_d,   &
             auxhist22_end_h, auxhist22_end_m , auxhist22_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist22_end_d, &
                                     H=auxhist22_end_h, M=auxhist22_end_m, S=auxhist22_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist22_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist22_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist22_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist22_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist23 INTERVAL
   CALL nl_get_auxhist23_interval( grid%id, auxhist23_interval )   ! same as minutes
   CALL nl_get_auxhist23_interval_d( grid%id, auxhist23_interval_d )
   CALL nl_get_auxhist23_interval_h( grid%id, auxhist23_interval_h )
   CALL nl_get_auxhist23_interval_m( grid%id, auxhist23_interval_m )
   CALL nl_get_auxhist23_interval_s( grid%id, auxhist23_interval_s )
   IF ( auxhist23_interval_m .EQ. 0 ) auxhist23_interval_m = auxhist23_interval
   IF ( MAX( auxhist23_interval_d,   &
             auxhist23_interval_h, auxhist23_interval_m , auxhist23_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist23_interval_d, &
                                        H=auxhist23_interval_h, M=auxhist23_interval_m, S=auxhist23_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist23_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist23_begin  ( grid%id, auxhist23_begin   )
   CALL nl_get_auxhist23_begin_y( grid%id, auxhist23_begin_y )
   CALL nl_get_auxhist23_begin_d( grid%id, auxhist23_begin_d )
   CALL nl_get_auxhist23_begin_h( grid%id, auxhist23_begin_h )
   CALL nl_get_auxhist23_begin_m( grid%id, auxhist23_begin_m )
   CALL nl_get_auxhist23_begin_s( grid%id, auxhist23_begin_s )
   IF ( auxhist23_begin_m .EQ. 0 ) auxhist23_begin_m = auxhist23_begin
   IF ( MAX( auxhist23_begin_y, auxhist23_begin_d,   &
             auxhist23_begin_h, auxhist23_begin_m , auxhist23_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist23_begin_d, &
                                      H=auxhist23_begin_h, M=auxhist23_begin_m, S=auxhist23_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist23_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist23_end( grid%id, auxhist23_end )
   CALL nl_get_auxhist23_end_y( grid%id, auxhist23_end_y )
   CALL nl_get_auxhist23_end_d( grid%id, auxhist23_end_d )
   CALL nl_get_auxhist23_end_h( grid%id, auxhist23_end_h )
   CALL nl_get_auxhist23_end_m( grid%id, auxhist23_end_m )
   CALL nl_get_auxhist23_end_s( grid%id, auxhist23_end_s )
   IF ( auxhist23_end_m .EQ. 0 ) auxhist23_end_m = auxhist23_end
   IF ( MAX( auxhist23_end_y, auxhist23_end_d,   &
             auxhist23_end_h, auxhist23_end_m , auxhist23_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist23_end_d, &
                                     H=auxhist23_end_h, M=auxhist23_end_m, S=auxhist23_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist23_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist23_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist23_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist23_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
! auxhist24 INTERVAL
   CALL nl_get_auxhist24_interval( grid%id, auxhist24_interval )   ! same as minutes
   CALL nl_get_auxhist24_interval_d( grid%id, auxhist24_interval_d )
   CALL nl_get_auxhist24_interval_h( grid%id, auxhist24_interval_h )
   CALL nl_get_auxhist24_interval_m( grid%id, auxhist24_interval_m )
   CALL nl_get_auxhist24_interval_s( grid%id, auxhist24_interval_s )
   IF ( auxhist24_interval_m .EQ. 0 ) auxhist24_interval_m = auxhist24_interval
   IF ( MAX( auxhist24_interval_d,   &
             auxhist24_interval_h, auxhist24_interval_m , auxhist24_interval_s   ) .GT. 0 ) THEN
     CALL WRFU_TimeIntervalSet( interval, D=auxhist24_interval_d, &
                                        H=auxhist24_interval_h, M=auxhist24_interval_m, S=auxhist24_interval_s, rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_TimeIntervalSet(auxhist24_interval) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ELSE
     interval =  padding_interval
   ENDIF
   CALL nl_get_auxhist24_begin  ( grid%id, auxhist24_begin   )
   CALL nl_get_auxhist24_begin_y( grid%id, auxhist24_begin_y )
   CALL nl_get_auxhist24_begin_d( grid%id, auxhist24_begin_d )
   CALL nl_get_auxhist24_begin_h( grid%id, auxhist24_begin_h )
   CALL nl_get_auxhist24_begin_m( grid%id, auxhist24_begin_m )
   CALL nl_get_auxhist24_begin_s( grid%id, auxhist24_begin_s )
   IF ( auxhist24_begin_m .EQ. 0 ) auxhist24_begin_m = auxhist24_begin
   IF ( MAX( auxhist24_begin_y, auxhist24_begin_d,   &
             auxhist24_begin_h, auxhist24_begin_m , auxhist24_begin_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( begin_time , D=auxhist24_begin_d, &
                                      H=auxhist24_begin_h, M=auxhist24_begin_m, S=auxhist24_begin_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist24_begin) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      begin_time = zero_time
   ENDIF
   CALL nl_get_auxhist24_end( grid%id, auxhist24_end )
   CALL nl_get_auxhist24_end_y( grid%id, auxhist24_end_y )
   CALL nl_get_auxhist24_end_d( grid%id, auxhist24_end_d )
   CALL nl_get_auxhist24_end_h( grid%id, auxhist24_end_h )
   CALL nl_get_auxhist24_end_m( grid%id, auxhist24_end_m )
   CALL nl_get_auxhist24_end_s( grid%id, auxhist24_end_s )
   IF ( auxhist24_end_m .EQ. 0 ) auxhist24_end_m = auxhist24_end
   IF ( MAX( auxhist24_end_y, auxhist24_end_d,   &
             auxhist24_end_h, auxhist24_end_m , auxhist24_end_s   ) .GT. 0 ) THEN
      CALL WRFU_TimeIntervalSet( end_time , D=auxhist24_end_d, &
                                     H=auxhist24_end_h, M=auxhist24_end_m, S=auxhist24_end_s, rc=rc )
      CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeIntervalSet(auxhist24_end) FAILED', &
                            __FILE__ , &
                            __LINE__  )
   ELSE
      end_time = run_length + padding_interval
   ENDIF
   CALL domain_alarm_create( grid, auxhist24_ALARM, interval, begin_time, end_time )
   IF ( interval .NE. padding_interval .AND. begin_time .EQ. zero_time ) THEN
     CALL WRFU_AlarmRingerOn( grid%alarms( auxhist24_ALARM ),  rc=rc )
     CALL wrf_check_error( WRFU_SUCCESS, rc, &
                           'WRFU_AlarmRingerOn(auxhist24_ALARM) FAILED', &
                           __FILE__ , &
                           __LINE__  )
   ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
