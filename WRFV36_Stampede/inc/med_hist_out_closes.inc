!STARTOFREGISTRYGENERATEDINCLUDE 'inc/med_hist_out_closes.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
 CASE ( AUXHIST1_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist1 ) THEN
       CALL close_dataset ( grid%auxhist1_oid , config_flags , n2 )
       grid%auxhist1_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST2_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist2 ) THEN
       CALL close_dataset ( grid%auxhist2_oid , config_flags , n2 )
       grid%auxhist2_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST3_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist3 ) THEN
       CALL close_dataset ( grid%auxhist3_oid , config_flags , n2 )
       grid%auxhist3_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST4_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist4 ) THEN
       CALL close_dataset ( grid%auxhist4_oid , config_flags , n2 )
       grid%auxhist4_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST5_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist5 ) THEN
       CALL close_dataset ( grid%auxhist5_oid , config_flags , n2 )
       grid%auxhist5_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST6_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist6 ) THEN
       CALL close_dataset ( grid%auxhist6_oid , config_flags , n2 )
       grid%auxhist6_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST7_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist7 ) THEN
       CALL close_dataset ( grid%auxhist7_oid , config_flags , n2 )
       grid%auxhist7_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST8_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist8 ) THEN
       CALL close_dataset ( grid%auxhist8_oid , config_flags , n2 )
       grid%auxhist8_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST9_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist9 ) THEN
       CALL close_dataset ( grid%auxhist9_oid , config_flags , n2 )
       grid%auxhist9_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST10_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist10 ) THEN
       CALL close_dataset ( grid%auxhist10_oid , config_flags , n2 )
       grid%auxhist10_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST11_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist11 ) THEN
       CALL close_dataset ( grid%auxhist11_oid , config_flags , n2 )
       grid%auxhist11_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST12_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist12 ) THEN
       CALL close_dataset ( grid%auxhist12_oid , config_flags , n2 )
       grid%auxhist12_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST13_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist13 ) THEN
       CALL close_dataset ( grid%auxhist13_oid , config_flags , n2 )
       grid%auxhist13_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST14_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist14 ) THEN
       CALL close_dataset ( grid%auxhist14_oid , config_flags , n2 )
       grid%auxhist14_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST15_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist15 ) THEN
       CALL close_dataset ( grid%auxhist15_oid , config_flags , n2 )
       grid%auxhist15_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST16_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist16 ) THEN
       CALL close_dataset ( grid%auxhist16_oid , config_flags , n2 )
       grid%auxhist16_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST17_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist17 ) THEN
       CALL close_dataset ( grid%auxhist17_oid , config_flags , n2 )
       grid%auxhist17_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST18_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist18 ) THEN
       CALL close_dataset ( grid%auxhist18_oid , config_flags , n2 )
       grid%auxhist18_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST19_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist19 ) THEN
       CALL close_dataset ( grid%auxhist19_oid , config_flags , n2 )
       grid%auxhist19_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST20_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist20 ) THEN
       CALL close_dataset ( grid%auxhist20_oid , config_flags , n2 )
       grid%auxhist20_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST21_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist21 ) THEN
       CALL close_dataset ( grid%auxhist21_oid , config_flags , n2 )
       grid%auxhist21_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST22_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist22 ) THEN
       CALL close_dataset ( grid%auxhist22_oid , config_flags , n2 )
       grid%auxhist22_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST23_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist23 ) THEN
       CALL close_dataset ( grid%auxhist23_oid , config_flags , n2 )
       grid%auxhist23_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST24_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist24 ) THEN
       CALL close_dataset ( grid%auxhist24_oid , config_flags , n2 )
       grid%auxhist24_oid = 0
       grid%nframes(stream) = 0
     ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
