!STARTOFREGISTRYGENERATEDINCLUDE 'inc/fine_stream_input.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF      ( ( grid%id .EQ. 1 ) .OR. ( config_flags%fine_input_stream .EQ. 0 ) ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_input' )
   CALL input_input      ( fid ,  grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_input' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 1 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput1' )
   CALL input_auxinput1 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput1' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 2 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput2' )
   CALL input_auxinput2 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput2' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 3 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput3' )
   CALL input_auxinput3 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput3' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 4 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput4' )
   CALL input_auxinput4 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput4' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 5 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput5' )
   CALL input_auxinput5 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput5' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 6 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput6' )
   CALL input_auxinput6 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput6' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 7 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput7' )
   CALL input_auxinput7 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput7' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 8 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput8' )
   CALL input_auxinput8 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput8' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 9 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput9' )
   CALL input_auxinput9 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput9' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 10 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput10' )
   CALL input_auxinput10 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput10' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 11 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput11' )
   CALL input_auxinput11 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput11' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 12 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput12' )
   CALL input_auxinput12 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput12' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 13 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput13' )
   CALL input_auxinput13 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput13' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 14 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput14' )
   CALL input_auxinput14 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput14' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 15 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput15' )
   CALL input_auxinput15 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput15' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 16 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput16' )
   CALL input_auxinput16 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput16' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 17 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput17' )
   CALL input_auxinput17 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput17' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 18 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput18' )
   CALL input_auxinput18 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput18' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 19 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput19' )
   CALL input_auxinput19 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput19' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 20 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput20' )
   CALL input_auxinput20 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput20' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 21 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput21' )
   CALL input_auxinput21 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput21' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 22 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput22' )
   CALL input_auxinput22 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput22' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 23 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput23' )
   CALL input_auxinput23 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput23' )
ELSE IF   ( config_flags%fine_input_stream .EQ. 24 ) THEN
   CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_auxinput24' )
   CALL input_auxinput24 ( fid ,   grid , config_flags , ierr )
   CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_auxinput24' )
ELSE
  WRITE( message , '("med_initialdata_input: bad fine_input_stream = ",I4)') config_flags%fine_input_stream
  CALL WRF_ERROR_FATAL ( message )
END IF
!ENDOFREGISTRYGENERATEDINCLUDE
