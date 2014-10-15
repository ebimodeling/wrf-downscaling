


MODULE module_io_domain
USE module_io
USE module_io_wrf
USE module_configure, ONLY : grid_config_rec_type
USE module_domain, ONLY : domain

CONTAINS

  SUBROUTINE open_r_dataset ( id , fname , grid , config_flags , sysdepinfo, ierr )
   TYPE (domain)             :: grid
   CHARACTER*(*) :: fname
   CHARACTER*(*) :: sysdepinfo
   INTEGER      , INTENT(INOUT) :: id , ierr
   LOGICAL , EXTERNAL :: wrf_dm_on_monitor
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   CHARACTER*128             :: DataSet, tmp
   LOGICAL                   :: anyway
   CALL wrf_open_for_read ( fname ,                     &
                            grid%communicator ,         &
                            grid%iocommunicator ,       &
                            sysdepinfo ,                &
                            id ,                        &
                            ierr )
   RETURN
  END SUBROUTINE open_r_dataset

  SUBROUTINE open_w_dataset ( id , fname , grid , config_flags , outsub , sysdepinfo, ierr )
   TYPE (domain)             :: grid
   CHARACTER*(*) :: fname
   CHARACTER*(*) :: sysdepinfo
   INTEGER      , INTENT(INOUT) :: id , ierr
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   LOGICAL , EXTERNAL :: wrf_dm_on_monitor
   EXTERNAL outsub
   CHARACTER*128             :: DataSet, sysdepinfo_tmp
   LOGICAL                   :: anyway
   CALL wrf_debug ( 100 , 'calling wrf_open_for_write_begin in open_w_dataset' )
   sysdepinfo_tmp = ' '
   IF ( grid%id < 10 ) THEN
     write(sysdepinfo_tmp,'(a,i1)')TRIM(sysdepinfo)//',GRIDID=',grid%id
   ELSE
     write(sysdepinfo_tmp,'(a,i2)')TRIM(sysdepinfo)//',GRIDID=',grid%id
   ENDIF
   CALL wrf_open_for_write_begin ( fname ,     &
                                   grid%communicator ,         &
                                   grid%iocommunicator ,       &
                                   sysdepinfo_tmp ,            &
                                   id ,                        &
                                   ierr )
   IF ( ierr .LE. 0 ) THEN
     CALL wrf_debug ( 100 , 'calling outsub in open_w_dataset' )
     CALL outsub( id , grid , config_flags , ierr )
     CALL wrf_debug ( 100 , 'back from outsub in open_w_dataset' )
   ENDIF
   IF ( ierr .LE. 0 ) THEN
     CALL wrf_debug ( 100 , 'calling wrf_open_for_write_commit in open_w_dataset' )
     CALL wrf_open_for_write_commit ( id ,                        &
                                      ierr )
     CALL wrf_debug ( 100 , 'back from wrf_open_for_write_commit in open_w_dataset' )
   ENDIF
  END SUBROUTINE open_w_dataset

  SUBROUTINE open_u_dataset ( id , fname , grid , config_flags , insub , sysdepinfo, ierr )
   TYPE (domain)             :: grid
   CHARACTER*(*) :: fname
   CHARACTER*(*) :: sysdepinfo
   INTEGER      , INTENT(INOUT) :: id , ierr
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   LOGICAL , EXTERNAL :: wrf_dm_on_monitor
   EXTERNAL insub
   CHARACTER*128             :: DataSet
   LOGICAL                   :: anyway
   CALL wrf_debug ( 100 , 'calling wrf_open_for_read_begin in open_u_dataset' )
   CALL wrf_open_for_read_begin ( fname ,     &
                                   grid%communicator ,         &
                                   grid%iocommunicator ,       &
                                   sysdepinfo ,                &
                                   id ,                        &
                                   ierr )
   IF ( ierr .LE. 0 ) THEN
     CALL wrf_debug ( 100 , 'calling insub in open_u_dataset' )
     CALL insub( id , grid , config_flags , ierr )
   ENDIF
   IF ( ierr .LE. 0 ) THEN
     CALL wrf_debug ( 100 , 'calling wrf_open_for_read_commit in open_u_dataset' )
     CALL wrf_open_for_read_commit ( id ,                        &
                                       ierr )
     CALL wrf_debug ( 100 , 'back from wrf_open_for_read_commit in open_u_dataset' )
   ENDIF
  END SUBROUTINE open_u_dataset

  SUBROUTINE close_dataset( id , config_flags, sysdepinfo ) 
   IMPLICIT NONE
   INTEGER id , ierr
   LOGICAL , EXTERNAL :: wrf_dm_on_monitor
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   CHARACTER*(*) :: sysdepinfo
   CHARACTER*128             :: DataSet
   LOGICAL                   :: anyway
   CALL wrf_ioclose( id , ierr )
  END SUBROUTINE close_dataset










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




  SUBROUTINE input_restart ( fid , grid , config_flags , ierr )
    IMPLICIT NONE
    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
    INTEGER, INTENT(IN) :: fid
    INTEGER, INTENT(INOUT) :: ierr
    IF ( config_flags%io_form_restart .GT. 0 ) THEN
      CALL input_wrf ( fid , grid , config_flags , restart_only , ierr )
    ENDIF
    RETURN
  END SUBROUTINE input_restart



  SUBROUTINE input_boundary ( fid , grid , config_flags , ierr )
    IMPLICIT NONE
    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
    INTEGER, INTENT(IN) :: fid
    INTEGER, INTENT(INOUT) :: ierr
    IF ( config_flags%io_form_boundary .GT. 0 ) THEN
      CALL input_wrf ( fid , grid , config_flags , boundary_only , ierr )
    ENDIF
    RETURN
  END SUBROUTINE input_boundary



  SUBROUTINE output_restart ( fid , grid , config_flags , ierr )
    IMPLICIT NONE
    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
    INTEGER, INTENT(IN) :: fid
    INTEGER, INTENT(INOUT) :: ierr 
    IF ( config_flags%io_form_restart .GT. 0 ) THEN
      CALL output_wrf ( fid , grid , config_flags , restart_only , ierr )
    ENDIF
    RETURN
  END SUBROUTINE output_restart



  SUBROUTINE output_boundary ( fid , grid , config_flags , ierr )
    IMPLICIT NONE
    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
    INTEGER, INTENT(IN) :: fid 
    INTEGER, INTENT(INOUT) :: ierr
    IF ( config_flags%io_form_boundary .GT. 0 ) THEN
      CALL output_wrf ( fid , grid , config_flags , boundary_only , ierr )
    ENDIF
    RETURN
  END SUBROUTINE output_boundary

END MODULE module_io_domain


SUBROUTINE construct_filename1( result , basename , fld1 , len1 )
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  INTEGER , INTENT(IN) :: fld1 , len1
  CHARACTER*64         :: t1, zeros
  
  CALL zero_pad ( t1 , fld1 , len1 )
  result = TRIM(basename) // "_d" // TRIM(t1)
  CALL maybe_remove_colons(result)
  RETURN
END SUBROUTINE construct_filename1

SUBROUTINE construct_filename2( result , basename , fld1 , len1 , date_char )
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  CHARACTER*(*) :: date_char

  INTEGER , INTENT(IN) :: fld1 , len1
  CHARACTER*64         :: t1, zeros
  CALL zero_pad ( t1 , fld1 , len1 )
  result = TRIM(basename) // ".d" // TRIM(t1) // "." // TRIM(date_char)
  CALL maybe_remove_colons(result)
  RETURN
END SUBROUTINE construct_filename2



SUBROUTINE construct_filename2a( result , basename , fld1 , len1 , date_char )
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  CHARACTER*(*) :: date_char
  INTEGER , INTENT(IN) :: fld1 , len1
  CHARACTER*64         :: t1, zeros
  INTEGER   i, j, l

  result=basename
  CALL zero_pad ( t1 , fld1 , len1 )
  i = index( basename , '<domain>' )
  l = len(trim(basename))
  IF ( i .GT. 0 ) THEN
    result = basename(1:i-1) // TRIM(t1) // basename(i+8:l)
  ENDIF
  i = index( result , '<date>' )
  l = len(trim(result))
  IF ( i .GT. 0 ) THEN
    result = result(1:i-1) // TRIM(date_char) // result(i+6:l)
  ENDIF
  CALL maybe_remove_colons(result)
  RETURN
END SUBROUTINE construct_filename2a

SUBROUTINE construct_filename ( result , basename , fld1 , len1 , fld2 , len2 )
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  INTEGER , INTENT(IN) :: fld1 , len1 , fld2 , len2
  CHARACTER*64         :: t1, t2, zeros
  
  CALL zero_pad ( t1 , fld1 , len1 )
  CALL zero_pad ( t2 , fld2 , len2 )
  result = TRIM(basename) // "_d" // TRIM(t1) // "_" // TRIM(t2)
  CALL maybe_remove_colons(result)
  RETURN
END SUBROUTINE construct_filename

SUBROUTINE construct_filename3 ( result , basename , fld1 , len1 , fld2 , len2, fld3, len3 )
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  INTEGER , INTENT(IN) :: fld1 , len1 , fld2 , len2, fld3, len3
  CHARACTER*64         :: t1, t2, t3, zeros

  CALL zero_pad ( t1 , fld1 , len1 )
  CALL zero_pad ( t2 , fld2 , len2 )
  CALL zero_pad ( t3 , fld3 , len3 )
  result = TRIM(basename) // "_d" // TRIM(t1) // "_" // TRIM(t2) // "_" // TRIM(t3)
  CALL maybe_remove_colons(result)
  RETURN
END SUBROUTINE construct_filename3

SUBROUTINE construct_filename4( result , basename , fld1 , len1 , date_char , io_form )
  USE module_state_description
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  CHARACTER*(*) :: date_char

  INTEGER, EXTERNAL :: use_package
  INTEGER , INTENT(IN) :: fld1 , len1 , io_form
  CHARACTER*64         :: t1, zeros
  CHARACTER*4          :: ext
  CALL zero_pad ( t1 , fld1 , len1 )
  IF      ( use_package(io_form) .EQ. IO_INTIO ) THEN
     ext = '.int'
  ELSE IF ( use_package(io_form) .EQ. IO_NETCDF ) THEN
     ext = '.nc '
  ELSE IF ( use_package(io_form) .EQ. IO_PNETCDF ) THEN
    ext = '.nc '
  ELSE IF ( use_package(io_form) .EQ. IO_GRIB1 ) THEN
     ext = '.gb '
  ELSE
     CALL wrf_error_fatal3("<stdin>",1380,&
'improper io_form')
  END IF
  result = TRIM(basename) // ".d" // TRIM(t1) // "." // TRIM(date_char) // TRIM(ext)
  CALL maybe_remove_colons(result)
  RETURN
END SUBROUTINE construct_filename4



SUBROUTINE construct_filename4a( result , basename , fld1 , len1 , date_char , io_form )
  USE module_state_description
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  CHARACTER*(*) :: date_char

  INTEGER, EXTERNAL :: use_package
  INTEGER , INTENT(IN) :: fld1 , len1 , io_form
  CHARACTER*64         :: t1, zeros
  CHARACTER*4          :: ext
  INTEGER   i, j, l
  result=basename
  CALL zero_pad ( t1 , fld1 , len1 )
  IF      ( use_package(io_form) .EQ. IO_INTIO ) THEN
     ext = '.int'
  ELSE IF ( use_package(io_form) .EQ. IO_NETCDF ) THEN
     ext = '.nc '
  ELSE IF ( use_package(io_form) .EQ. IO_PNETCDF ) THEN
    ext = '.nc '
  ELSE IF ( use_package(io_form) .EQ. IO_GRIB1 ) THEN
     ext = '.gb '
  ELSE
     CALL wrf_error_fatal3("<stdin>",1413,&
'improper io_form')
  END IF
  l = len(trim(basename))
  result = basename(1:l) // TRIM(ext)
  i = index( result , '<domain>' )
  l = len(trim(result))
  IF ( i .GT. 0 ) THEN
    result = result(1:i-1) // TRIM(t1) // result(i+8:l)
  ENDIF
  i = index( result , '<date>' )
  l = len(trim(result))
  IF ( i .GT. 0 ) THEN
    result = result(1:i-1) // TRIM(date_char) // result(i+6:l)
  ENDIF
  CALL maybe_remove_colons(result)
  RETURN
END SUBROUTINE construct_filename4a

SUBROUTINE append_to_filename ( result , basename , fld1 , len1 )
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  INTEGER , INTENT(IN) :: fld1 , len1
  CHARACTER*64         :: t1, zeros
  
  CALL zero_pad ( t1 , fld1 , len1 )
  result = TRIM(basename) // "_" // TRIM(t1)
  CALL maybe_remove_colons(result)
  RETURN
END SUBROUTINE append_to_filename

SUBROUTINE zero_pad ( result , fld1 , len1 )
  IMPLICIT NONE
  CHARACTER*(*) :: result
  INTEGER , INTENT (IN)      :: fld1 , len1
  INTEGER                    :: d , x
  CHARACTER*64         :: t2, zeros
  x = fld1 ; d = 0
  DO WHILE ( x > 0 )
    x = x / 10
    d = d + 1
  END DO
  write(t2,'(I9)')fld1
  zeros = '0000000000000000000000000000000'
  result = zeros(1:len1-d) // t2(9-d+1:9)
  RETURN
END SUBROUTINE zero_pad

SUBROUTINE init_wrfio
   USE module_io, ONLY : wrf_ioinit
   IMPLICIT NONE
   INTEGER ierr
   CALL wrf_ioinit(ierr)
END SUBROUTINE init_wrfio










SUBROUTINE adjust_io_timestr ( TI, CT, ST, timestr )
   USE module_utility
   IMPLICIT NONE

   TYPE(WRFU_Time), INTENT(IN)            :: ST,CT    
   TYPE(WRFU_TimeInterval), INTENT(IN)    :: TI       
   CHARACTER*(*), INTENT(INOUT)           :: timestr  

   TYPE(WRFU_Time)                        :: OT
   TYPE(WRFU_TimeInterval)                :: IOI
   INTEGER                                :: n

   IOI = CT-ST                               
   n = WRFU_TimeIntervalDIVQuot( IOI , TI )  
   IOI = TI * n                              
   OT = ST + IOI                             
   CALL wrf_timetoa( OT, timestr )           
   RETURN
END SUBROUTINE adjust_io_timestr




SUBROUTINE maybe_remove_colons( FileName )
  CHARACTER*(*) FileName
  CHARACTER c, d
  INTEGER i, l
  LOGICAL nocolons
  l = LEN(TRIM(FileName))


  CALL nl_get_nocolons(1,nocolons)
  IF ( nocolons ) THEN
    DO i = 3, l
      IF ( FileName(i:i) .EQ. ':' ) THEN
        FileName(i:i) = '_'
      ENDIF
    ENDDO
  ENDIF
  RETURN
END



