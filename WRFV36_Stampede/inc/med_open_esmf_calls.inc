!STARTOFREGISTRYGENERATEDINCLUDE 'inc/med_open_esmf_calls.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL nl_get_io_form_auxinput1( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 0
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT1_ALARM,       &
                   config_flags%auxinput1_inname, grid%auxinput1_oid, &
                   input_auxinput1, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput2( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 1
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT2_ALARM,       &
                   config_flags%auxinput2_inname, grid%auxinput2_oid, &
                   input_auxinput2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput3( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 2
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT3_ALARM,       &
                   config_flags%auxinput3_inname, grid%auxinput3_oid, &
                   input_auxinput3, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput4( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 3
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT4_ALARM,       &
                   config_flags%auxinput4_inname, grid%auxinput4_oid, &
                   input_auxinput4, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput5( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 4
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT5_ALARM,       &
                   config_flags%auxinput5_inname, grid%auxinput5_oid, &
                   input_auxinput5, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput6( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 5
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT6_ALARM,       &
                   config_flags%auxinput6_inname, grid%auxinput6_oid, &
                   input_auxinput6, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput7( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 6
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT7_ALARM,       &
                   config_flags%auxinput7_inname, grid%auxinput7_oid, &
                   input_auxinput7, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput8( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 7
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT8_ALARM,       &
                   config_flags%auxinput8_inname, grid%auxinput8_oid, &
                   input_auxinput8, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput9( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 8
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT9_ALARM,       &
                   config_flags%auxinput9_inname, grid%auxinput9_oid, &
                   input_auxinput9, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput10( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 9
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT10_ALARM,       &
                   config_flags%auxinput10_inname, grid%auxinput10_oid, &
                   input_auxinput10, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput11( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 10
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT11_ALARM,       &
                   config_flags%auxinput11_inname, grid%auxinput11_oid, &
                   input_auxinput11, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput12( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 11
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT12_ALARM,       &
                   config_flags%auxinput12_inname, grid%auxinput12_oid, &
                   input_auxinput12, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput13( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 12
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT13_ALARM,       &
                   config_flags%auxinput13_inname, grid%auxinput13_oid, &
                   input_auxinput13, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput14( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 13
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT14_ALARM,       &
                   config_flags%auxinput14_inname, grid%auxinput14_oid, &
                   input_auxinput14, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput15( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 14
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT15_ALARM,       &
                   config_flags%auxinput15_inname, grid%auxinput15_oid, &
                   input_auxinput15, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput16( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 15
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT16_ALARM,       &
                   config_flags%auxinput16_inname, grid%auxinput16_oid, &
                   input_auxinput16, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput17( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 16
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT17_ALARM,       &
                   config_flags%auxinput17_inname, grid%auxinput17_oid, &
                   input_auxinput17, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput18( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 17
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT18_ALARM,       &
                   config_flags%auxinput18_inname, grid%auxinput18_oid, &
                   input_auxinput18, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput19( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 18
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT19_ALARM,       &
                   config_flags%auxinput19_inname, grid%auxinput19_oid, &
                   input_auxinput19, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput20( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 19
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT20_ALARM,       &
                   config_flags%auxinput20_inname, grid%auxinput20_oid, &
                   input_auxinput20, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput21( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 20
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT21_ALARM,       &
                   config_flags%auxinput21_inname, grid%auxinput21_oid, &
                   input_auxinput21, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput22( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 21
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT22_ALARM,       &
                   config_flags%auxinput22_inname, grid%auxinput22_oid, &
                   input_auxinput22, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput23( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 22
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT23_ALARM,       &
                   config_flags%auxinput23_inname, grid%auxinput23_oid, &
                   input_auxinput23, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxinput24( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxinput + 23
  CALL open_aux_u( grid, config_flags, stream, AUXINPUT24_ALARM,       &
                   config_flags%auxinput24_inname, grid%auxinput24_oid, &
                   input_auxinput24, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist1( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 0
  CALL open_hist_w( grid, config_flags, stream, AUXHIST1_ALARM,       &
                    config_flags%auxhist1_outname, grid%auxhist1_oid, &
                    output_auxhist1, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist2( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 1
  CALL open_hist_w( grid, config_flags, stream, AUXHIST2_ALARM,       &
                    config_flags%auxhist2_outname, grid%auxhist2_oid, &
                    output_auxhist2, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist3( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 2
  CALL open_hist_w( grid, config_flags, stream, AUXHIST3_ALARM,       &
                    config_flags%auxhist3_outname, grid%auxhist3_oid, &
                    output_auxhist3, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist4( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 3
  CALL open_hist_w( grid, config_flags, stream, AUXHIST4_ALARM,       &
                    config_flags%auxhist4_outname, grid%auxhist4_oid, &
                    output_auxhist4, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist5( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 4
  CALL open_hist_w( grid, config_flags, stream, AUXHIST5_ALARM,       &
                    config_flags%auxhist5_outname, grid%auxhist5_oid, &
                    output_auxhist5, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist6( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 5
  CALL open_hist_w( grid, config_flags, stream, AUXHIST6_ALARM,       &
                    config_flags%auxhist6_outname, grid%auxhist6_oid, &
                    output_auxhist6, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist7( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 6
  CALL open_hist_w( grid, config_flags, stream, AUXHIST7_ALARM,       &
                    config_flags%auxhist7_outname, grid%auxhist7_oid, &
                    output_auxhist7, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist8( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 7
  CALL open_hist_w( grid, config_flags, stream, AUXHIST8_ALARM,       &
                    config_flags%auxhist8_outname, grid%auxhist8_oid, &
                    output_auxhist8, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist9( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 8
  CALL open_hist_w( grid, config_flags, stream, AUXHIST9_ALARM,       &
                    config_flags%auxhist9_outname, grid%auxhist9_oid, &
                    output_auxhist9, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist10( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 9
  CALL open_hist_w( grid, config_flags, stream, AUXHIST10_ALARM,       &
                    config_flags%auxhist10_outname, grid%auxhist10_oid, &
                    output_auxhist10, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist11( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 10
  CALL open_hist_w( grid, config_flags, stream, AUXHIST11_ALARM,       &
                    config_flags%auxhist11_outname, grid%auxhist11_oid, &
                    output_auxhist11, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist12( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 11
  CALL open_hist_w( grid, config_flags, stream, AUXHIST12_ALARM,       &
                    config_flags%auxhist12_outname, grid%auxhist12_oid, &
                    output_auxhist12, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist13( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 12
  CALL open_hist_w( grid, config_flags, stream, AUXHIST13_ALARM,       &
                    config_flags%auxhist13_outname, grid%auxhist13_oid, &
                    output_auxhist13, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist14( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 13
  CALL open_hist_w( grid, config_flags, stream, AUXHIST14_ALARM,       &
                    config_flags%auxhist14_outname, grid%auxhist14_oid, &
                    output_auxhist14, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist15( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 14
  CALL open_hist_w( grid, config_flags, stream, AUXHIST15_ALARM,       &
                    config_flags%auxhist15_outname, grid%auxhist15_oid, &
                    output_auxhist15, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist16( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 15
  CALL open_hist_w( grid, config_flags, stream, AUXHIST16_ALARM,       &
                    config_flags%auxhist16_outname, grid%auxhist16_oid, &
                    output_auxhist16, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist17( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 16
  CALL open_hist_w( grid, config_flags, stream, AUXHIST17_ALARM,       &
                    config_flags%auxhist17_outname, grid%auxhist17_oid, &
                    output_auxhist17, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist18( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 17
  CALL open_hist_w( grid, config_flags, stream, AUXHIST18_ALARM,       &
                    config_flags%auxhist18_outname, grid%auxhist18_oid, &
                    output_auxhist18, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist19( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 18
  CALL open_hist_w( grid, config_flags, stream, AUXHIST19_ALARM,       &
                    config_flags%auxhist19_outname, grid%auxhist19_oid, &
                    output_auxhist19, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist20( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 19
  CALL open_hist_w( grid, config_flags, stream, AUXHIST20_ALARM,       &
                    config_flags%auxhist20_outname, grid%auxhist20_oid, &
                    output_auxhist20, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist21( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 20
  CALL open_hist_w( grid, config_flags, stream, AUXHIST21_ALARM,       &
                    config_flags%auxhist21_outname, grid%auxhist21_oid, &
                    output_auxhist21, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist22( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 21
  CALL open_hist_w( grid, config_flags, stream, AUXHIST22_ALARM,       &
                    config_flags%auxhist22_outname, grid%auxhist22_oid, &
                    output_auxhist22, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist23( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 22
  CALL open_hist_w( grid, config_flags, stream, AUXHIST23_ALARM,       &
                    config_flags%auxhist23_outname, grid%auxhist23_oid, &
                    output_auxhist23, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
CALL nl_get_io_form_auxhist24( 1, io_form )
IF ( use_package( io_form ) == IO_ESMF ) THEN
  stream = first_auxhist + 23
  CALL open_hist_w( grid, config_flags, stream, AUXHIST24_ALARM,       &
                    config_flags%auxhist24_outname, grid%auxhist24_oid, &
                    output_auxhist24, fname, n2, ierr )
  IF ( ierr /= 0 ) RETURN
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
