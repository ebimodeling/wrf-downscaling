!STARTOFREGISTRYGENERATEDINCLUDE 'inc/io_form_for_stream.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
    IF      ( stream .eq. restart_only ) THEN
      CALL nl_get_io_form_restart( 1, io_form )
    ELSE IF ( stream .eq. input_only ) THEN
      CALL nl_get_io_form_input( 1, io_form )
    ELSE IF ( stream .eq. history_only ) THEN
      CALL nl_get_io_form_history( 1, io_form )
    ELSE IF ( stream .eq. boundary_only ) THEN
      CALL nl_get_io_form_boundary( 1, io_form )
    ELSE IF ( stream .eq. auxinput1_only ) THEN
      CALL nl_get_io_form_auxinput1( 1, io_form )
    ELSE IF ( stream .eq. auxhist1_only ) THEN
      CALL nl_get_io_form_auxhist1( 1, io_form )
    ELSE IF ( stream .eq. auxinput2_only ) THEN
      CALL nl_get_io_form_auxinput2( 1, io_form )
    ELSE IF ( stream .eq. auxhist2_only ) THEN
      CALL nl_get_io_form_auxhist2( 1, io_form )
    ELSE IF ( stream .eq. auxinput3_only ) THEN
      CALL nl_get_io_form_auxinput3( 1, io_form )
    ELSE IF ( stream .eq. auxhist3_only ) THEN
      CALL nl_get_io_form_auxhist3( 1, io_form )
    ELSE IF ( stream .eq. auxinput4_only ) THEN
      CALL nl_get_io_form_auxinput4( 1, io_form )
    ELSE IF ( stream .eq. auxhist4_only ) THEN
      CALL nl_get_io_form_auxhist4( 1, io_form )
    ELSE IF ( stream .eq. auxinput5_only ) THEN
      CALL nl_get_io_form_auxinput5( 1, io_form )
    ELSE IF ( stream .eq. auxhist5_only ) THEN
      CALL nl_get_io_form_auxhist5( 1, io_form )
    ELSE IF ( stream .eq. auxinput6_only ) THEN
      CALL nl_get_io_form_auxinput6( 1, io_form )
    ELSE IF ( stream .eq. auxhist6_only ) THEN
      CALL nl_get_io_form_auxhist6( 1, io_form )
    ELSE IF ( stream .eq. auxinput7_only ) THEN
      CALL nl_get_io_form_auxinput7( 1, io_form )
    ELSE IF ( stream .eq. auxhist7_only ) THEN
      CALL nl_get_io_form_auxhist7( 1, io_form )
    ELSE IF ( stream .eq. auxinput8_only ) THEN
      CALL nl_get_io_form_auxinput8( 1, io_form )
    ELSE IF ( stream .eq. auxhist8_only ) THEN
      CALL nl_get_io_form_auxhist8( 1, io_form )
    ELSE IF ( stream .eq. auxinput9_only ) THEN
      CALL nl_get_io_form_auxinput9( 1, io_form )
    ELSE IF ( stream .eq. auxhist9_only ) THEN
      CALL nl_get_io_form_auxhist9( 1, io_form )
    ELSE IF ( stream .eq. auxinput10_only ) THEN
      CALL nl_get_io_form_auxinput10( 1, io_form )
    ELSE IF ( stream .eq. auxhist10_only ) THEN
      CALL nl_get_io_form_auxhist10( 1, io_form )
    ELSE IF ( stream .eq. auxinput11_only ) THEN
      CALL nl_get_io_form_auxinput11( 1, io_form )
    ELSE IF ( stream .eq. auxhist11_only ) THEN
      CALL nl_get_io_form_auxhist11( 1, io_form )
    ELSE IF ( stream .eq. auxinput12_only ) THEN
      CALL nl_get_io_form_auxinput12( 1, io_form )
    ELSE IF ( stream .eq. auxhist12_only ) THEN
      CALL nl_get_io_form_auxhist12( 1, io_form )
    ELSE IF ( stream .eq. auxinput13_only ) THEN
      CALL nl_get_io_form_auxinput13( 1, io_form )
    ELSE IF ( stream .eq. auxhist13_only ) THEN
      CALL nl_get_io_form_auxhist13( 1, io_form )
    ELSE IF ( stream .eq. auxinput14_only ) THEN
      CALL nl_get_io_form_auxinput14( 1, io_form )
    ELSE IF ( stream .eq. auxhist14_only ) THEN
      CALL nl_get_io_form_auxhist14( 1, io_form )
    ELSE IF ( stream .eq. auxinput15_only ) THEN
      CALL nl_get_io_form_auxinput15( 1, io_form )
    ELSE IF ( stream .eq. auxhist15_only ) THEN
      CALL nl_get_io_form_auxhist15( 1, io_form )
    ELSE IF ( stream .eq. auxinput16_only ) THEN
      CALL nl_get_io_form_auxinput16( 1, io_form )
    ELSE IF ( stream .eq. auxhist16_only ) THEN
      CALL nl_get_io_form_auxhist16( 1, io_form )
    ELSE IF ( stream .eq. auxinput17_only ) THEN
      CALL nl_get_io_form_auxinput17( 1, io_form )
    ELSE IF ( stream .eq. auxhist17_only ) THEN
      CALL nl_get_io_form_auxhist17( 1, io_form )
    ELSE IF ( stream .eq. auxinput18_only ) THEN
      CALL nl_get_io_form_auxinput18( 1, io_form )
    ELSE IF ( stream .eq. auxhist18_only ) THEN
      CALL nl_get_io_form_auxhist18( 1, io_form )
    ELSE IF ( stream .eq. auxinput19_only ) THEN
      CALL nl_get_io_form_auxinput19( 1, io_form )
    ELSE IF ( stream .eq. auxhist19_only ) THEN
      CALL nl_get_io_form_auxhist19( 1, io_form )
    ELSE IF ( stream .eq. auxinput20_only ) THEN
      CALL nl_get_io_form_auxinput20( 1, io_form )
    ELSE IF ( stream .eq. auxhist20_only ) THEN
      CALL nl_get_io_form_auxhist20( 1, io_form )
    ELSE IF ( stream .eq. auxinput21_only ) THEN
      CALL nl_get_io_form_auxinput21( 1, io_form )
    ELSE IF ( stream .eq. auxhist21_only ) THEN
      CALL nl_get_io_form_auxhist21( 1, io_form )
    ELSE IF ( stream .eq. auxinput22_only ) THEN
      CALL nl_get_io_form_auxinput22( 1, io_form )
    ELSE IF ( stream .eq. auxhist22_only ) THEN
      CALL nl_get_io_form_auxhist22( 1, io_form )
    ELSE IF ( stream .eq. auxinput23_only ) THEN
      CALL nl_get_io_form_auxinput23( 1, io_form )
    ELSE IF ( stream .eq. auxhist23_only ) THEN
      CALL nl_get_io_form_auxhist23( 1, io_form )
    ELSE IF ( stream .eq. auxinput24_only ) THEN
      CALL nl_get_io_form_auxinput24( 1, io_form )
    ELSE IF ( stream .eq. auxhist24_only ) THEN
      CALL nl_get_io_form_auxhist24( 1, io_form )
    ELSE  ! if no match then do the old service representative schtick
      CALL wrf_error_fatal('internal error: please contact wrfhelp@ucar.edu: io_form_for_stream.inc -- invalid stream number')
    ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
