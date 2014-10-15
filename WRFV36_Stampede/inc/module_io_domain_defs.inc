!STARTOFREGISTRYGENERATEDINCLUDE 'inc/module_io_domain_defs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
SUBROUTINE output_input ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_input .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, input_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_input
SUBROUTINE output_auxinput1 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput1 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput1_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput1
SUBROUTINE output_auxinput2 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput2 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput2_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput2
SUBROUTINE output_auxinput3 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput3 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput3_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput3
SUBROUTINE output_auxinput4 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput4 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput4_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput4
SUBROUTINE output_auxinput5 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput5 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput5_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput5
SUBROUTINE output_auxinput6 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput6 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput6_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput6
SUBROUTINE output_auxinput7 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput7 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput7_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput7
SUBROUTINE output_auxinput8 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput8 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput8_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput8
SUBROUTINE output_auxinput9 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput9 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput9_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput9
SUBROUTINE output_auxinput10 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput10 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput10_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput10
SUBROUTINE output_auxinput11 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput11 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput11_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput11
SUBROUTINE output_auxinput12 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput12 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput12_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput12
SUBROUTINE output_auxinput13 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput13 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput13_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput13
SUBROUTINE output_auxinput14 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput14 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput14_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput14
SUBROUTINE output_auxinput15 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput15 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput15_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput15
SUBROUTINE output_auxinput16 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput16 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput16_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput16
SUBROUTINE output_auxinput17 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput17 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput17_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput17
SUBROUTINE output_auxinput18 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput18 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput18_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput18
SUBROUTINE output_auxinput19 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput19 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput19_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput19
SUBROUTINE output_auxinput20 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput20 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput20_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput20
SUBROUTINE output_auxinput21 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput21 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput21_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput21
SUBROUTINE output_auxinput22 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput22 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput22_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput22
SUBROUTINE output_auxinput23 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput23 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput23_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput23
SUBROUTINE output_auxinput24 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput24 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxinput24_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxinput24
SUBROUTINE output_history ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_history .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, history_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_history
SUBROUTINE output_auxhist1 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist1 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist1_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist1
SUBROUTINE output_auxhist2 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist2 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist2_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist2
SUBROUTINE output_auxhist3 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist3 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist3_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist3
SUBROUTINE output_auxhist4 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist4 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist4_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist4
SUBROUTINE output_auxhist5 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist5 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist5_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist5
SUBROUTINE output_auxhist6 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist6 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist6_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist6
SUBROUTINE output_auxhist7 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist7 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist7_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist7
SUBROUTINE output_auxhist8 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist8 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist8_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist8
SUBROUTINE output_auxhist9 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist9 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist9_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist9
SUBROUTINE output_auxhist10 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist10 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist10_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist10
SUBROUTINE output_auxhist11 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist11 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist11_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist11
SUBROUTINE output_auxhist12 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist12 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist12_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist12
SUBROUTINE output_auxhist13 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist13 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist13_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist13
SUBROUTINE output_auxhist14 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist14 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist14_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist14
SUBROUTINE output_auxhist15 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist15 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist15_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist15
SUBROUTINE output_auxhist16 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist16 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist16_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist16
SUBROUTINE output_auxhist17 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist17 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist17_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist17
SUBROUTINE output_auxhist18 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist18 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist18_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist18
SUBROUTINE output_auxhist19 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist19 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist19_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist19
SUBROUTINE output_auxhist20 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist20 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist20_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist20
SUBROUTINE output_auxhist21 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist21 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist21_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist21
SUBROUTINE output_auxhist22 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist22 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist22_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist22
SUBROUTINE output_auxhist23 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist23 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist23_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist23
SUBROUTINE output_auxhist24 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist24 .GT. 0 ) THEN
   CALL output_wrf( fid, grid, config_flags, auxhist24_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE output_auxhist24
SUBROUTINE input_input ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_input .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, input_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_input
SUBROUTINE input_auxinput1 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput1 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput1_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput1
SUBROUTINE input_auxinput2 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput2 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput2_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput2
SUBROUTINE input_auxinput3 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput3 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput3_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput3
SUBROUTINE input_auxinput4 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput4 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput4_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput4
SUBROUTINE input_auxinput5 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput5 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput5_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput5
SUBROUTINE input_auxinput6 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput6 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput6_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput6
SUBROUTINE input_auxinput7 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput7 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput7_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput7
SUBROUTINE input_auxinput8 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput8 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput8_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput8
SUBROUTINE input_auxinput9 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput9 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput9_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput9
SUBROUTINE input_auxinput10 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput10 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput10_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput10
SUBROUTINE input_auxinput11 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput11 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput11_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput11
SUBROUTINE input_auxinput12 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput12 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput12_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput12
SUBROUTINE input_auxinput13 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput13 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput13_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput13
SUBROUTINE input_auxinput14 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput14 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput14_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput14
SUBROUTINE input_auxinput15 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput15 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput15_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput15
SUBROUTINE input_auxinput16 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput16 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput16_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput16
SUBROUTINE input_auxinput17 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput17 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput17_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput17
SUBROUTINE input_auxinput18 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput18 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput18_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput18
SUBROUTINE input_auxinput19 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput19 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput19_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput19
SUBROUTINE input_auxinput20 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput20 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput20_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput20
SUBROUTINE input_auxinput21 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput21 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput21_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput21
SUBROUTINE input_auxinput22 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput22 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput22_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput22
SUBROUTINE input_auxinput23 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput23 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput23_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput23
SUBROUTINE input_auxinput24 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxinput24 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxinput24_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxinput24
SUBROUTINE input_history ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_history .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, history_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_history
SUBROUTINE input_auxhist1 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist1 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist1_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist1
SUBROUTINE input_auxhist2 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist2 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist2_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist2
SUBROUTINE input_auxhist3 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist3 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist3_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist3
SUBROUTINE input_auxhist4 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist4 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist4_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist4
SUBROUTINE input_auxhist5 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist5 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist5_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist5
SUBROUTINE input_auxhist6 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist6 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist6_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist6
SUBROUTINE input_auxhist7 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist7 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist7_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist7
SUBROUTINE input_auxhist8 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist8 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist8_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist8
SUBROUTINE input_auxhist9 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist9 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist9_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist9
SUBROUTINE input_auxhist10 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist10 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist10_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist10
SUBROUTINE input_auxhist11 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist11 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist11_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist11
SUBROUTINE input_auxhist12 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist12 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist12_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist12
SUBROUTINE input_auxhist13 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist13 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist13_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist13
SUBROUTINE input_auxhist14 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist14 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist14_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist14
SUBROUTINE input_auxhist15 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist15 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist15_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist15
SUBROUTINE input_auxhist16 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist16 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist16_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist16
SUBROUTINE input_auxhist17 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist17 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist17_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist17
SUBROUTINE input_auxhist18 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist18 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist18_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist18
SUBROUTINE input_auxhist19 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist19 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist19_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist19
SUBROUTINE input_auxhist20 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist20 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist20_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist20
SUBROUTINE input_auxhist21 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist21 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist21_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist21
SUBROUTINE input_auxhist22 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist22 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist22_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist22
SUBROUTINE input_auxhist23 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist23 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist23_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist23
SUBROUTINE input_auxhist24 ( fid , grid , config_flags , ierr )
 IMPLICIT NONE
 TYPE(domain) :: grid
 TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
 INTEGER, INTENT(IN) :: fid
 INTEGER, INTENT(INOUT) :: ierr
 IF ( config_flags%io_form_auxhist24 .GT. 0 ) THEN
   CALL input_wrf( fid, grid, config_flags, auxhist24_only, ierr ) ;
 ENDIF
 RETURN
END SUBROUTINE input_auxhist24
!ENDOFREGISTRYGENERATEDINCLUDE
