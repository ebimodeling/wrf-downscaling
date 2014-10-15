!STARTOFREGISTRYGENERATEDINCLUDE 'inc/med_auxinput_in_closes.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
 CASE ( AUXINPUT1_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput1 ) THEN
       CALL close_dataset ( grid%auxinput1_oid , config_flags , "DATASET=AUXINPUT1" )
       grid%auxinput1_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT2_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput2 ) THEN
       CALL close_dataset ( grid%auxinput2_oid , config_flags , "DATASET=AUXINPUT2" )
       grid%auxinput2_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT3_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput3 ) THEN
       CALL close_dataset ( grid%auxinput3_oid , config_flags , "DATASET=AUXINPUT3" )
       grid%auxinput3_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT4_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput4 ) THEN
       CALL close_dataset ( grid%auxinput4_oid , config_flags , "DATASET=AUXINPUT4" )
       grid%auxinput4_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT5_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput5 ) THEN
       CALL close_dataset ( grid%auxinput5_oid , config_flags , "DATASET=AUXINPUT5" )
       grid%auxinput5_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT6_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput6 ) THEN
       CALL close_dataset ( grid%auxinput6_oid , config_flags , "DATASET=AUXINPUT6" )
       grid%auxinput6_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT7_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput7 ) THEN
       CALL close_dataset ( grid%auxinput7_oid , config_flags , "DATASET=AUXINPUT7" )
       grid%auxinput7_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT8_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput8 ) THEN
       CALL close_dataset ( grid%auxinput8_oid , config_flags , "DATASET=AUXINPUT8" )
       grid%auxinput8_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT9_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput9 ) THEN
       CALL close_dataset ( grid%auxinput9_oid , config_flags , "DATASET=AUXINPUT9" )
       grid%auxinput9_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT10_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput10 ) THEN
       CALL close_dataset ( grid%auxinput10_oid , config_flags , "DATASET=AUXINPUT10" )
       grid%auxinput10_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT11_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput11 ) THEN
       CALL close_dataset ( grid%auxinput11_oid , config_flags , "DATASET=AUXINPUT11" )
       grid%auxinput11_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT12_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput12 ) THEN
       CALL close_dataset ( grid%auxinput12_oid , config_flags , "DATASET=AUXINPUT12" )
       grid%auxinput12_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT13_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput13 ) THEN
       CALL close_dataset ( grid%auxinput13_oid , config_flags , "DATASET=AUXINPUT13" )
       grid%auxinput13_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT14_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput14 ) THEN
       CALL close_dataset ( grid%auxinput14_oid , config_flags , "DATASET=AUXINPUT14" )
       grid%auxinput14_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT15_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput15 ) THEN
       CALL close_dataset ( grid%auxinput15_oid , config_flags , "DATASET=AUXINPUT15" )
       grid%auxinput15_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT16_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput16 ) THEN
       CALL close_dataset ( grid%auxinput16_oid , config_flags , "DATASET=AUXINPUT16" )
       grid%auxinput16_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT17_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput17 ) THEN
       CALL close_dataset ( grid%auxinput17_oid , config_flags , "DATASET=AUXINPUT17" )
       grid%auxinput17_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT18_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput18 ) THEN
       CALL close_dataset ( grid%auxinput18_oid , config_flags , "DATASET=AUXINPUT18" )
       grid%auxinput18_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT19_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput19 ) THEN
       CALL close_dataset ( grid%auxinput19_oid , config_flags , "DATASET=AUXINPUT19" )
       grid%auxinput19_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT20_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput20 ) THEN
       CALL close_dataset ( grid%auxinput20_oid , config_flags , "DATASET=AUXINPUT20" )
       grid%auxinput20_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT21_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput21 ) THEN
       CALL close_dataset ( grid%auxinput21_oid , config_flags , "DATASET=AUXINPUT21" )
       grid%auxinput21_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT22_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput22 ) THEN
       CALL close_dataset ( grid%auxinput22_oid , config_flags , "DATASET=AUXINPUT22" )
       grid%auxinput22_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT23_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput23 ) THEN
       CALL close_dataset ( grid%auxinput23_oid , config_flags , "DATASET=AUXINPUT23" )
       grid%auxinput23_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT24_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput24 ) THEN
       CALL close_dataset ( grid%auxinput24_oid , config_flags , "DATASET=AUXINPUT24" )
       grid%auxinput24_oid = 0
       grid%nframes(stream) = 0
     ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
