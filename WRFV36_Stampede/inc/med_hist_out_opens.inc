!STARTOFREGISTRYGENERATEDINCLUDE 'inc/med_hist_out_opens.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
 CASE ( AUXHIST1_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST1_ALARM,       &
                     config_flags%auxhist1_outname, grid%auxhist1_oid, &
                     output_auxhist1, fname, n2, ierr )
   CALL output_auxhist1 ( grid%auxhist1_oid, grid , config_flags , ierr )
 CASE ( AUXHIST2_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST2_ALARM,       &
                     config_flags%auxhist2_outname, grid%auxhist2_oid, &
                     output_auxhist2, fname, n2, ierr )
   CALL output_auxhist2 ( grid%auxhist2_oid, grid , config_flags , ierr )
 CASE ( AUXHIST3_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST3_ALARM,       &
                     config_flags%auxhist3_outname, grid%auxhist3_oid, &
                     output_auxhist3, fname, n2, ierr )
   CALL output_auxhist3 ( grid%auxhist3_oid, grid , config_flags , ierr )
 CASE ( AUXHIST4_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST4_ALARM,       &
                     config_flags%auxhist4_outname, grid%auxhist4_oid, &
                     output_auxhist4, fname, n2, ierr )
   CALL output_auxhist4 ( grid%auxhist4_oid, grid , config_flags , ierr )
 CASE ( AUXHIST5_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST5_ALARM,       &
                     config_flags%auxhist5_outname, grid%auxhist5_oid, &
                     output_auxhist5, fname, n2, ierr )
   CALL output_auxhist5 ( grid%auxhist5_oid, grid , config_flags , ierr )
 CASE ( AUXHIST6_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST6_ALARM,       &
                     config_flags%auxhist6_outname, grid%auxhist6_oid, &
                     output_auxhist6, fname, n2, ierr )
   CALL output_auxhist6 ( grid%auxhist6_oid, grid , config_flags , ierr )
 CASE ( AUXHIST7_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST7_ALARM,       &
                     config_flags%auxhist7_outname, grid%auxhist7_oid, &
                     output_auxhist7, fname, n2, ierr )
   CALL output_auxhist7 ( grid%auxhist7_oid, grid , config_flags , ierr )
 CASE ( AUXHIST8_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST8_ALARM,       &
                     config_flags%auxhist8_outname, grid%auxhist8_oid, &
                     output_auxhist8, fname, n2, ierr )
   CALL output_auxhist8 ( grid%auxhist8_oid, grid , config_flags , ierr )
 CASE ( AUXHIST9_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST9_ALARM,       &
                     config_flags%auxhist9_outname, grid%auxhist9_oid, &
                     output_auxhist9, fname, n2, ierr )
   CALL output_auxhist9 ( grid%auxhist9_oid, grid , config_flags , ierr )
 CASE ( AUXHIST10_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST10_ALARM,       &
                     config_flags%auxhist10_outname, grid%auxhist10_oid, &
                     output_auxhist10, fname, n2, ierr )
   CALL output_auxhist10 ( grid%auxhist10_oid, grid , config_flags , ierr )
 CASE ( AUXHIST11_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST11_ALARM,       &
                     config_flags%auxhist11_outname, grid%auxhist11_oid, &
                     output_auxhist11, fname, n2, ierr )
   CALL output_auxhist11 ( grid%auxhist11_oid, grid , config_flags , ierr )
 CASE ( AUXHIST12_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST12_ALARM,       &
                     config_flags%auxhist12_outname, grid%auxhist12_oid, &
                     output_auxhist12, fname, n2, ierr )
   CALL output_auxhist12 ( grid%auxhist12_oid, grid , config_flags , ierr )
 CASE ( AUXHIST13_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST13_ALARM,       &
                     config_flags%auxhist13_outname, grid%auxhist13_oid, &
                     output_auxhist13, fname, n2, ierr )
   CALL output_auxhist13 ( grid%auxhist13_oid, grid , config_flags , ierr )
 CASE ( AUXHIST14_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST14_ALARM,       &
                     config_flags%auxhist14_outname, grid%auxhist14_oid, &
                     output_auxhist14, fname, n2, ierr )
   CALL output_auxhist14 ( grid%auxhist14_oid, grid , config_flags , ierr )
 CASE ( AUXHIST15_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST15_ALARM,       &
                     config_flags%auxhist15_outname, grid%auxhist15_oid, &
                     output_auxhist15, fname, n2, ierr )
   CALL output_auxhist15 ( grid%auxhist15_oid, grid , config_flags , ierr )
 CASE ( AUXHIST16_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST16_ALARM,       &
                     config_flags%auxhist16_outname, grid%auxhist16_oid, &
                     output_auxhist16, fname, n2, ierr )
   CALL output_auxhist16 ( grid%auxhist16_oid, grid , config_flags , ierr )
 CASE ( AUXHIST17_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST17_ALARM,       &
                     config_flags%auxhist17_outname, grid%auxhist17_oid, &
                     output_auxhist17, fname, n2, ierr )
   CALL output_auxhist17 ( grid%auxhist17_oid, grid , config_flags , ierr )
 CASE ( AUXHIST18_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST18_ALARM,       &
                     config_flags%auxhist18_outname, grid%auxhist18_oid, &
                     output_auxhist18, fname, n2, ierr )
   CALL output_auxhist18 ( grid%auxhist18_oid, grid , config_flags , ierr )
 CASE ( AUXHIST19_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST19_ALARM,       &
                     config_flags%auxhist19_outname, grid%auxhist19_oid, &
                     output_auxhist19, fname, n2, ierr )
   CALL output_auxhist19 ( grid%auxhist19_oid, grid , config_flags , ierr )
 CASE ( AUXHIST20_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST20_ALARM,       &
                     config_flags%auxhist20_outname, grid%auxhist20_oid, &
                     output_auxhist20, fname, n2, ierr )
   CALL output_auxhist20 ( grid%auxhist20_oid, grid , config_flags , ierr )
 CASE ( AUXHIST21_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST21_ALARM,       &
                     config_flags%auxhist21_outname, grid%auxhist21_oid, &
                     output_auxhist21, fname, n2, ierr )
   CALL output_auxhist21 ( grid%auxhist21_oid, grid , config_flags , ierr )
 CASE ( AUXHIST22_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST22_ALARM,       &
                     config_flags%auxhist22_outname, grid%auxhist22_oid, &
                     output_auxhist22, fname, n2, ierr )
   CALL output_auxhist22 ( grid%auxhist22_oid, grid , config_flags , ierr )
 CASE ( AUXHIST23_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST23_ALARM,       &
                     config_flags%auxhist23_outname, grid%auxhist23_oid, &
                     output_auxhist23, fname, n2, ierr )
   CALL output_auxhist23 ( grid%auxhist23_oid, grid , config_flags , ierr )
 CASE ( AUXHIST24_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST24_ALARM,       &
                     config_flags%auxhist24_outname, grid%auxhist24_oid, &
                     output_auxhist24, fname, n2, ierr )
   CALL output_auxhist24 ( grid%auxhist24_oid, grid , config_flags , ierr )
!ENDOFREGISTRYGENERATEDINCLUDE
