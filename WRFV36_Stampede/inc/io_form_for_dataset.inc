!STARTOFREGISTRYGENERATEDINCLUDE 'inc/io_form_for_dataset.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
    IF      ( DataSet .eq. 'RESTART' ) THEN
      CALL nl_get_io_form_restart( 1, io_form )
    ELSE IF ( DataSet .eq. 'INPUT' ) THEN
      CALL nl_get_io_form_input( 1, io_form )
    ELSE IF ( DataSet .eq. 'HISTORY' ) THEN
      CALL nl_get_io_form_history( 1, io_form )
    ELSE IF ( DataSet .eq. 'BOUNDARY' ) THEN
      CALL nl_get_io_form_boundary( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT1' ) THEN
      CALL nl_get_io_form_auxinput1( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST1' ) THEN
      CALL nl_get_io_form_auxhist1( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT2' ) THEN
      CALL nl_get_io_form_auxinput2( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST2' ) THEN
      CALL nl_get_io_form_auxhist2( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT3' ) THEN
      CALL nl_get_io_form_auxinput3( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST3' ) THEN
      CALL nl_get_io_form_auxhist3( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT4' ) THEN
      CALL nl_get_io_form_auxinput4( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST4' ) THEN
      CALL nl_get_io_form_auxhist4( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT5' ) THEN
      CALL nl_get_io_form_auxinput5( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST5' ) THEN
      CALL nl_get_io_form_auxhist5( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT6' ) THEN
      CALL nl_get_io_form_auxinput6( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST6' ) THEN
      CALL nl_get_io_form_auxhist6( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT7' ) THEN
      CALL nl_get_io_form_auxinput7( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST7' ) THEN
      CALL nl_get_io_form_auxhist7( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT8' ) THEN
      CALL nl_get_io_form_auxinput8( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST8' ) THEN
      CALL nl_get_io_form_auxhist8( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT9' ) THEN
      CALL nl_get_io_form_auxinput9( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST9' ) THEN
      CALL nl_get_io_form_auxhist9( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT10' ) THEN
      CALL nl_get_io_form_auxinput10( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST10' ) THEN
      CALL nl_get_io_form_auxhist10( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT11' ) THEN
      CALL nl_get_io_form_auxinput11( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST11' ) THEN
      CALL nl_get_io_form_auxhist11( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT12' ) THEN
      CALL nl_get_io_form_auxinput12( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST12' ) THEN
      CALL nl_get_io_form_auxhist12( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT13' ) THEN
      CALL nl_get_io_form_auxinput13( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST13' ) THEN
      CALL nl_get_io_form_auxhist13( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT14' ) THEN
      CALL nl_get_io_form_auxinput14( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST14' ) THEN
      CALL nl_get_io_form_auxhist14( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT15' ) THEN
      CALL nl_get_io_form_auxinput15( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST15' ) THEN
      CALL nl_get_io_form_auxhist15( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT16' ) THEN
      CALL nl_get_io_form_auxinput16( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST16' ) THEN
      CALL nl_get_io_form_auxhist16( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT17' ) THEN
      CALL nl_get_io_form_auxinput17( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST17' ) THEN
      CALL nl_get_io_form_auxhist17( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT18' ) THEN
      CALL nl_get_io_form_auxinput18( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST18' ) THEN
      CALL nl_get_io_form_auxhist18( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT19' ) THEN
      CALL nl_get_io_form_auxinput19( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST19' ) THEN
      CALL nl_get_io_form_auxhist19( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT20' ) THEN
      CALL nl_get_io_form_auxinput20( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST20' ) THEN
      CALL nl_get_io_form_auxhist20( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT21' ) THEN
      CALL nl_get_io_form_auxinput21( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST21' ) THEN
      CALL nl_get_io_form_auxhist21( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT22' ) THEN
      CALL nl_get_io_form_auxinput22( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST22' ) THEN
      CALL nl_get_io_form_auxhist22( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT23' ) THEN
      CALL nl_get_io_form_auxinput23( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST23' ) THEN
      CALL nl_get_io_form_auxhist23( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXINPUT24' ) THEN
      CALL nl_get_io_form_auxinput24( 1, io_form )
    ELSE IF ( DataSet .eq. 'AUXHIST24' ) THEN
      CALL nl_get_io_form_auxhist24( 1, io_form )
    ELSE  ! default if nothing is set in SysDepInfo; use history
      CALL nl_get_io_form_history( 1, io_form )
    ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
