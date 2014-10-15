













MODULE module_wrf_quilt



















































  USE module_internal_header_util
  USE module_timing

  USE module_cpl, ONLY : coupler_on, cpl_set_dm_communicator, cpl_finalize


  INTEGER, PARAMETER :: int_num_handles = 99
  INTEGER, PARAMETER :: max_servers = int_num_handles+1  
  LOGICAL, DIMENSION(0:int_num_handles) :: okay_to_write, int_handle_in_use, okay_to_commit
  INTEGER, DIMENSION(0:int_num_handles) :: int_num_bytes_to_write, io_form
  REAL, POINTER,SAVE :: int_local_output_buffer(:)
  INTEGER,      SAVE :: int_local_output_cursor
  LOGICAL          :: quilting_enabled
  LOGICAL          :: disable_quilt = .FALSE.
  INTEGER          :: prev_server_for_handle = -1
  INTEGER          :: server_for_handle(int_num_handles)
  INTEGER          :: reduced(2), reduced_dummy(2)
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor

  INTEGER :: mpi_comm_avail,availrank
  LOGICAL :: in_avail=.false., poll_servers=.false.

  INTEGER nio_groups

  INTEGER :: mpi_comm_local
  LOGICAL :: compute_node
  LOGICAL :: compute_group_master(max_servers)
  INTEGER :: mpi_comm_io_groups(max_servers)
  INTEGER :: nio_tasks_in_group
  INTEGER :: nio_tasks_per_group
  INTEGER :: ncompute_tasks
  INTEGER :: ntasks
  INTEGER :: mytask

  INTEGER, PARAMETER           :: onebyte = 1
  INTEGER comm_io_servers, iserver, hdrbufsize, obufsize
  INTEGER, DIMENSION(4096)     :: hdrbuf
  INTEGER, DIMENSION(int_num_handles)     :: handle



  CONTAINS

    INTEGER FUNCTION get_server_id ( dhandle )









      IMPLICIT NONE
      INTEGER, INTENT(IN) :: dhandle
      IF ( dhandle .GE. 1 .AND. dhandle .LE. int_num_handles ) THEN
        IF ( server_for_handle ( dhandle ) .GE. 1 ) THEN
          get_server_id = server_for_handle ( dhandle )
        ELSE
           IF(poll_servers) THEN
              
              call wrf_quilt_find_server(server_for_handle(dhandle))
           ELSE
              
              prev_server_for_handle = mod ( prev_server_for_handle + 1 , nio_groups )
              server_for_handle( dhandle ) = prev_server_for_handle+1
           ENDIF
           get_server_id=server_for_handle(dhandle)
        ENDIF
      ELSE
         CALL wrf_message('module_io_quilt: get_server_id bad dhandle' )
      ENDIF
    END FUNCTION get_server_id

    SUBROUTINE set_server_id ( dhandle, value )
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: dhandle, value
       IF ( dhandle .GE. 1 .AND. dhandle .LE. int_num_handles ) THEN
         server_for_handle(dhandle) = value
       ELSE
         CALL wrf_message('module_io_quilt: set_server_id bad dhandle' )
       ENDIF
    END SUBROUTINE set_server_id

    LOGICAL FUNCTION get_poll_servers() 
      implicit none
      get_poll_servers=poll_servers
    end FUNCTION get_poll_servers

    SUBROUTINE int_get_fresh_handle( retval )










      INTEGER i, retval
      retval = -1
      DO i = 1, int_num_handles
        IF ( .NOT. int_handle_in_use(i) )  THEN
          retval = i
          GOTO 33
        ENDIF
      ENDDO
33    CONTINUE
      IF ( retval < 0 )  THEN
        CALL wrf_error_fatal3("<stdin>",177,&
"frame/module_io_quilt.F: int_get_fresh_handle() can not")
      ENDIF
      int_handle_in_use(i) = .TRUE.
      NULLIFY ( int_local_output_buffer )
    END SUBROUTINE int_get_fresh_handle

    SUBROUTINE setup_quilt_servers ( nio_tasks_per_group,     &
                                     mytask,                  &
                                     ntasks,                  &
                                     n_groups_arg,            &
                                     nio,                     &
                                     mpi_comm_wrld,           &
                                     mpi_comm_local,          &
                                     mpi_comm_io_groups)




























































      USE module_configure
      USE module_dm, ONLY : compute_mesh
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INTEGER,                      INTENT(IN)  :: nio_tasks_per_group, mytask, ntasks, &
                                                   n_groups_arg, mpi_comm_wrld
      INTEGER,  INTENT(OUT)                     :: mpi_comm_local, nio
      INTEGER, DIMENSION(100),      INTENT(OUT) :: mpi_comm_io_groups

      INTEGER                     :: i, j, ii, comdup, ierr, niotasks, n_groups, iisize
      INTEGER, DIMENSION(ntasks)  :: icolor
      CHARACTER*128 mess
      INTEGER :: io_form_setting
      INTEGER :: me
      INTEGER :: k, m, nprocx, nprocy
      LOGICAL :: reorder_mesh



      CALL nl_get_io_form_history(1,   io_form_setting) ; call sokay( 'history', io_form_setting )
      CALL nl_get_io_form_restart(1,   io_form_setting) ; call sokay( 'restart', io_form_setting )
      CALL nl_get_io_form_auxhist1(1,  io_form_setting) ; call sokay( 'auxhist1', io_form_setting )
      CALL nl_get_io_form_auxhist2(1,  io_form_setting) ; call sokay( 'auxhist2', io_form_setting )
      CALL nl_get_io_form_auxhist3(1,  io_form_setting) ; call sokay( 'auxhist3', io_form_setting )
      CALL nl_get_io_form_auxhist4(1,  io_form_setting) ; call sokay( 'auxhist4', io_form_setting )
      CALL nl_get_io_form_auxhist5(1,  io_form_setting) ; call sokay( 'auxhist5', io_form_setting )
      CALL nl_get_io_form_auxhist6(1,  io_form_setting) ; call sokay( 'auxhist6', io_form_setting )
      CALL nl_get_io_form_auxhist7(1,  io_form_setting) ; call sokay( 'auxhist7', io_form_setting )
      CALL nl_get_io_form_auxhist8(1,  io_form_setting) ; call sokay( 'auxhist8', io_form_setting )
      CALL nl_get_io_form_auxhist9(1,  io_form_setting) ; call sokay( 'auxhist9', io_form_setting )
      CALL nl_get_io_form_auxhist10(1, io_form_setting) ; call sokay( 'auxhist10', io_form_setting )
      CALL nl_get_io_form_auxhist11(1, io_form_setting) ; call sokay( 'auxhist11', io_form_setting )

      n_groups = n_groups_arg
      IF ( n_groups .LT. 1 ) n_groups = 1

      compute_node = .TRUE.







      nio = nio_tasks_per_group
      ncompute_tasks = ntasks - (nio * n_groups)
      IF ( ncompute_tasks .LT. nio ) THEN 
        WRITE(mess,'("Not enough tasks to have ",I3," groups of ",I3," I/O tasks. No quilting.")')n_groups,nio
        nio            = 0
        ncompute_tasks = ntasks
      ELSE                                   
        WRITE(mess,'("Quilting with ",I3," groups of ",I3," I/O tasks.")')n_groups,nio
      ENDIF                                   
      CALL wrf_message(mess)

      IF ( nio .LT. 0 ) THEN
        nio = 0
      ENDIF
      IF ( nio .EQ. 0 ) THEN
        quilting_enabled = .FALSE.
        mpi_comm_local = mpi_comm_wrld
        mpi_comm_io_groups = mpi_comm_wrld
        RETURN
      ENDIF
      quilting_enabled = .TRUE.



      DO i = 1, ncompute_tasks
        icolor(i) = 0
      ENDDO
      ii = 1

      DO i = ncompute_tasks+1, ntasks, nio
        DO j = i, i+nio-1
          icolor(j) = ii
        ENDDO
        ii = ii+1
      ENDDO
      CALL MPI_Comm_dup(mpi_comm_wrld,comdup,ierr)
      CALL MPI_Comm_split(comdup,icolor(mytask+1),mytask,mpi_comm_local,ierr)


      CALL nl_get_reorder_mesh(1,reorder_mesh)
      IF ( reorder_mesh ) THEN
        reorder_mesh = .FALSE.
        CALL nl_set_reorder_mesh(1,reorder_mesh)
        CALL wrf_message('Warning: reorder_mesh does not work with quilting. Disabled reorder_mesh.')
      ENDIF
      
      CALL compute_mesh( ncompute_tasks, nprocx, nprocy )

      nio = min(nio,nprocy)
      m = mod(nprocy,nio)  
      ii = 1
      DO j = 1, nio, 1
         DO k = 1,nprocy/nio+min(m,1)
           DO i = 1, nprocx
             icolor(ii) = j - 1
             ii = ii + 1
           ENDDO
         ENDDO
         m = max(m-1,0)
      ENDDO


      DO j = 1, n_groups
        
        DO i = ncompute_tasks+1,ntasks
          icolor(i) = MPI_UNDEFINED
        ENDDO
        ii = 0
        DO i = ncompute_tasks+(j-1)*nio+1,ncompute_tasks+j*nio
          icolor(i) = ii
          ii = ii+1
        ENDDO
        CALL MPI_Comm_dup(mpi_comm_wrld,comdup,ierr)
        CALL MPI_Comm_split(comdup,icolor(mytask+1),mytask, &
                            mpi_comm_io_groups(j),ierr)
      ENDDO

         if(nio_groups==1) then
            poll_servers=.false.
            call wrf_message('Server polling is useless with one io group.  Disabled poll_servers.')
         endif

      if(poll_servers) then
         
         
         
         
         

         

         call mpi_comm_rank(mpi_comm_wrld,me,ierr)

         icolor=MPI_UNDEFINED
         in_avail=.false.

         if(wrf_dm_on_monitor()) then
            in_avail=.true. 
         endif
         icolor(1)=1

         do j=1,n_groups
            i=ncompute_tasks+j*nio-1
            if(me+1==i) then
               in_avail=.true. 
            endif
            icolor(i)=1
         enddo

         CALL MPI_Comm_dup(mpi_comm_wrld,comdup,ierr)
         CALL MPI_Comm_split(comdup,icolor(me+1),me, &
                             mpi_comm_avail,ierr)

         availrank=MPI_UNDEFINED
         if(in_avail) then
            call mpi_comm_rank(mpi_comm_avail,availrank,ierr)
         endif

      endif

      compute_group_master = .FALSE.
      compute_node         = .FALSE.

      DO j = 1, n_groups

         IF ( mytask .LT. ncompute_tasks .OR.                                                  &    
              (ncompute_tasks+(j-1)*nio .LE. mytask .AND. mytask .LT. ncompute_tasks+j*nio) &    
            ) THEN

         CALL MPI_Comm_Size( mpi_comm_io_groups(j) , iisize, ierr )
         
         
         CALL MPI_Comm_Rank( mpi_comm_io_groups(j) , me , ierr )

         
         
         
         IF (ncompute_tasks+(j-1)*nio .LE. mytask .AND. mytask .LT. ncompute_tasks+j*nio) THEN
            mpi_comm_io_groups(1) = mpi_comm_io_groups(j)
         ELSE
            compute_node = .TRUE.
            
            
            
            compute_group_master(j) = (me .EQ. 0)


         ENDIF
         ENDIF
      ENDDO

    END SUBROUTINE setup_quilt_servers

    SUBROUTINE sokay ( stream, io_form )
    USE module_state_description
    CHARACTER*(*) stream
    CHARACTER*256 mess
    INTEGER io_form

    SELECT CASE (io_form)
      CASE ( IO_NETCDF   )
         RETURN
      CASE ( IO_INTIO   )
         RETURN
      CASE ( IO_GRIB1 )
         RETURN
      CASE ( IO_GRIB2 )
         RETURN
      CASE (0)
         RETURN
      CASE DEFAULT
         WRITE(mess,*)' An output format has been specified that is incompatible with quilting: io_form: ',io_form,' ',TRIM(stream)
         CALL wrf_error_fatal3("<stdin>",468,&
mess)
    END SELECT
    END SUBROUTINE sokay

    SUBROUTINE quilt













      USE module_state_description
      USE module_quilt_outbuf_ops
      USE module_configure, only : grid_config_rec_type, model_config_rec, model_to_grid_config_rec
      IMPLICIT NONE
      INCLUDE 'mpif.h'
  INTEGER, PARAMETER ::  int_ioexit			=  	     10
  INTEGER, PARAMETER ::  int_open_for_write_begin	=  	     20
  INTEGER, PARAMETER ::  int_open_for_write_commit	=  	     30
  INTEGER, PARAMETER ::  int_open_for_read 		=  	     40
  INTEGER, PARAMETER ::  int_inquire_opened 		=  	     60
  INTEGER, PARAMETER ::  int_inquire_filename 		=  	     70
  INTEGER, PARAMETER ::  int_iosync 			=  	     80
  INTEGER, PARAMETER ::  int_ioclose 			=  	     90
  INTEGER, PARAMETER ::  int_next_time 			=  	    100
  INTEGER, PARAMETER ::  int_set_time 			=  	    110
  INTEGER, PARAMETER ::  int_next_var 			=  	    120
  INTEGER, PARAMETER ::  int_dom_ti_real 		=  	    140
  INTEGER, PARAMETER ::  int_dom_ti_double 		=  	    160
  INTEGER, PARAMETER ::  int_dom_ti_integer 		=  	    180
  INTEGER, PARAMETER ::  int_dom_ti_logical 		=  	    200
  INTEGER, PARAMETER ::  int_dom_ti_char 		=  	    220
  INTEGER, PARAMETER ::  int_dom_td_real 		=  	    240
  INTEGER, PARAMETER ::  int_dom_td_double 		=  	    260
  INTEGER, PARAMETER ::  int_dom_td_integer 		=  	    280
  INTEGER, PARAMETER ::  int_dom_td_logical 		=  	    300
  INTEGER, PARAMETER ::  int_dom_td_char 		=  	    320
  INTEGER, PARAMETER ::  int_var_ti_real 		=  	    340
  INTEGER, PARAMETER ::  int_var_ti_double 		=  	    360
  INTEGER, PARAMETER ::  int_var_ti_integer 		=  	    380
  INTEGER, PARAMETER ::  int_var_ti_logical 		=  	    400
  INTEGER, PARAMETER ::  int_var_ti_char 		=  	    420
  INTEGER, PARAMETER ::  int_var_td_real 		=  	    440
  INTEGER, PARAMETER ::  int_var_td_double 		=  	    460
  INTEGER, PARAMETER ::  int_var_td_integer 		=  	    480
  INTEGER, PARAMETER ::  int_var_td_logical 		=  	    500
  INTEGER, PARAMETER ::  int_var_td_char 		=  	    520
  INTEGER, PARAMETER ::  int_field 			=  	    530
  INTEGER, PARAMETER ::  int_var_info 			=  	    540
  INTEGER, PARAMETER ::  int_noop 			=  	    550
      integer, parameter  :: WRF_FILE_NOT_OPENED                  = 100
      integer, parameter  :: WRF_FILE_OPENED_NOT_COMMITTED        = 101
      integer, parameter  :: WRF_FILE_OPENED_FOR_WRITE            = 102
      integer, parameter  :: WRF_FILE_OPENED_FOR_READ             = 103
      integer, parameter  :: WRF_REAL                             = 104
      integer, parameter  :: WRF_DOUBLE                           = 105
      integer, parameter  :: WRF_FLOAT=WRF_REAL
      integer, parameter  :: WRF_INTEGER                          = 106
      integer, parameter  :: WRF_LOGICAL                          = 107
      integer, parameter  :: WRF_COMPLEX                          = 108
      integer, parameter  :: WRF_DOUBLE_COMPLEX                   = 109
      integer, parameter  :: WRF_FILE_OPENED_FOR_UPDATE           = 110


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
      TYPE (grid_config_rec_type)  :: config_flags
      INTEGER itag, ninbuf, ntasks_io_group, ntasks_local_group, mytask_local, ierr
      INTEGER istat
      INTEGER mytask_io_group
      INTEGER   :: nout_set = 0
      INTEGER   :: obufsize, bigbufsize, chunksize, sz
      REAL, DIMENSION(1)      :: dummy
      INTEGER, ALLOCATABLE, DIMENSION(:) :: obuf, bigbuf
      REAL,    ALLOCATABLE, DIMENSION(:) :: RDATA
      INTEGER, ALLOCATABLE, DIMENSION(:) :: IDATA
      CHARACTER (LEN=512) :: CDATA
      CHARACTER (LEN=80) :: fname
      INTEGER icurs, hdrbufsize, itypesize, ftypesize, rtypesize, Status, fstat, io_form_arg
      INTEGER :: DataHandle, FieldType, Comm, IOComm, DomainDesc, code, Count
      INTEGER, DIMENSION(3) :: DomainStart , DomainEnd , MemoryStart , MemoryEnd , PatchStart , PatchEnd
      INTEGER :: dummybuf(1)
      INTEGER :: num_noops, num_commit_messages, num_field_training_msgs, hdr_tag
      CHARACTER (len=256) :: DateStr , Element, VarName, MemoryOrder , Stagger , DimNames(3), FileName, SysDepInfo, mess
      INTEGER, EXTERNAL :: use_package
      LOGICAL           :: stored_write_record, retval
      INTEGER iii, jjj, vid, CC, DD, dom_id
      LOGICAL           :: call_server_ready

logical okay_to_w
character*120 sysline

      dom_id = 1 
      CALL model_to_grid_config_rec ( dom_id , model_config_rec , config_flags )









      SysDepInfo = " "
      if ( config_flags%use_netcdf_classic ) SysDepInfo="use_netcdf_classic"
      CALL ext_ncd_ioinit( SysDepInfo, ierr )
      SysDepInfo = " "
      CALL ext_int_ioinit( SysDepInfo, ierr )





      CALL ext_gr1_ioinit( SysDepInfo, ierr)


      CALL ext_gr2_ioinit( SysDepInfo, ierr)


      call_server_ready = .true. 

      okay_to_commit = .false.
      stored_write_record = .false.
      ninbuf = 0
      
      
      
      
      
      CALL Mpi_Comm_Size (  mpi_comm_io_groups(1), ntasks_io_group,    ierr  )
      CALL MPI_COMM_RANK( mpi_comm_io_groups(1), mytask_io_group,    ierr )
      CALL Mpi_Comm_Size (  mpi_comm_local,        ntasks_local_group, ierr  )
      CALL MPI_COMM_RANK( mpi_comm_local,        mytask_local,       ierr )

      CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )
      IF ( itypesize <= 0 ) THEN
        CALL wrf_error_fatal3("<stdin>",611,&
"external/RSL/module_dm.F: quilt: type size <= 0 invalid")
      ENDIF







       CC = ntasks_io_group - 1

       DD = ncompute_tasks / ntasks_local_group








okay_to_w = .false.
      DO WHILE (.TRUE.)  














         if(poll_servers .and. call_server_ready) then
            call_server_ready=.false.
            
            
            call wrf_quilt_server_ready()
         endif

        
        

        
        ! if needed (currently needed only for ioclose).
        reduced_dummy = 0
        CALL MPI_Reduce( reduced_dummy, reduced, 2, MPI_INTEGER, MPI_SUM, mytask_io_group, mpi_comm_io_groups(1), ierr )
        obufsize = reduced(1)



        IF ( obufsize .LT. 0 ) THEN
          IF ( obufsize .EQ. -100 ) THEN         

            CALL ext_ncd_ioexit( Status )


            CALL ext_int_ioexit( Status )
            CALL ext_gr1_ioexit( Status )
            CALL ext_gr2_ioexit( Status )
            CALL wrf_message ( 'I/O QUILT SERVERS DONE' )
            IF (coupler_on) THEN 
               CALL cpl_finalize() 
            ELSE
               CALL mpi_finalize(ierr)
            END IF
            STOP
          ELSE
            WRITE(mess,*)'Possible 32-bit overflow on output server. Try larger nio_tasks_per_group in namelist.'
            CALL wrf_error_fatal3("<stdin>",684,&
mess)
          ENDIF
        ENDIF







        IF ( obufsize .GT. 0 ) THEN
          ALLOCATE( obuf( (obufsize+1)/itypesize ) )


          CALL collect_on_comm_debug("module_io_quilt.F",713, mpi_comm_io_groups(1),        &
                                onebyte,                      &
                                dummy, 0,                     &
                                obuf, obufsize )

        ELSE
          ! Necessarily, the compute processes send the ioclose signal,
          
          ! will stall on the ioclose message waiting for the quilt 
          
          
          
          
          
          ! Then a header representing the ioclose message is constructed
          
          
          
          
          
          
          ALLOCATE( obuf( 4096 ) )
          
          CALL int_gen_handle_header( obuf, obufsize, itypesize, &
                                      reduced(2) , int_ioclose )

          if(poll_servers) then 
             
             
             call_server_ready=.true.
          endif
        ENDIF











        CALL init_store_piece_of_field
        CALL mpi_type_size ( MPI_INTEGER , itypesize , ierr )



        vid = 0
        icurs = itypesize
        num_noops = 0 
        num_commit_messages = 0 
        num_field_training_msgs = 0 
        DO WHILE ( icurs .lt. obufsize ) 
          hdr_tag = get_hdr_tag( obuf ( icurs / itypesize ) )
          SELECT CASE ( hdr_tag )
            CASE ( int_field )
              CALL int_get_write_field_header ( obuf(icurs/itypesize), hdrbufsize, itypesize, ftypesize,  &
                                                DataHandle , DateStr , VarName , Dummy , FieldType , Comm , IOComm, &
                                                DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                                DomainStart , DomainEnd ,                                    &
                                                MemoryStart , MemoryEnd ,                                    &
                                                PatchStart , PatchEnd )
              chunksize = (PatchEnd(1)-PatchStart(1)+1)*(PatchEnd(2)-PatchStart(2)+1)* &
                          (PatchEnd(3)-PatchStart(3)+1)*ftypesize

              IF ( DomainDesc .EQ. 333933 ) THEN  
                 IF ( num_field_training_msgs .EQ. 0 ) THEN
                   call add_to_bufsize_for_field( VarName, hdrbufsize )

                 ENDIF
                 num_field_training_msgs = num_field_training_msgs + 1
              ELSE
                 call add_to_bufsize_for_field( VarName, hdrbufsize )

              ENDIF
              icurs = icurs + hdrbufsize



              
              
              IF ( DomainDesc .NE. 333933 ) THEN   

                call add_to_bufsize_for_field( VarName, chunksize )
                icurs = icurs + chunksize
              ENDIF
            CASE ( int_open_for_write_commit )  
              hdrbufsize = obuf(icurs/itypesize)
              IF (num_commit_messages.EQ.0) THEN
                call add_to_bufsize_for_field( 'COMMIT', hdrbufsize )
              ENDIF
              num_commit_messages = num_commit_messages + 1
              icurs = icurs + hdrbufsize
            CASE DEFAULT
              hdrbufsize = obuf(icurs/itypesize)




































              IF ((hdr_tag.EQ.int_noop.AND.mytask_local.NE.0.AND.num_noops.LE.0)  &
                  .OR.hdr_tag.NE.int_noop) THEN
                write(VarName,'(I5.5)')vid 

                call add_to_bufsize_for_field( VarName, hdrbufsize )
                vid = vid+1
              ENDIF
              IF ( hdr_tag .EQ. int_noop ) num_noops = num_noops + 1
              icurs = icurs + hdrbufsize
          END SELECT
        ENDDO 



        vid = 0
        icurs = itypesize
        num_noops = 0 
        num_commit_messages = 0 
        num_field_training_msgs = 0 
        DO WHILE ( icurs .lt. obufsize ) 

          hdr_tag = get_hdr_tag( obuf ( icurs / itypesize ) )
          SELECT CASE ( hdr_tag )
            CASE ( int_field )
              CALL int_get_write_field_header ( obuf(icurs/itypesize), hdrbufsize, itypesize, ftypesize,  &
                                                DataHandle , DateStr , VarName , Dummy , FieldType , Comm , IOComm, &
                                                DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                                DomainStart , DomainEnd ,                                    &
                                                MemoryStart , MemoryEnd ,                                    &
                                                PatchStart , PatchEnd )
              chunksize = (PatchEnd(1)-PatchStart(1)+1)*(PatchEnd(2)-PatchStart(2)+1)* &
                          (PatchEnd(3)-PatchStart(3)+1)*ftypesize

              IF ( DomainDesc .EQ. 333933 ) THEN  
                 IF ( num_field_training_msgs .EQ. 0 ) THEN
                   call store_piece_of_field( obuf(icurs/itypesize), VarName, hdrbufsize )

                 ENDIF
                 num_field_training_msgs = num_field_training_msgs + 1
              ELSE
                 call store_piece_of_field( obuf(icurs/itypesize), VarName, hdrbufsize )

              ENDIF
              icurs = icurs + hdrbufsize
              
              
              IF ( DomainDesc .NE. 333933 ) THEN   

                call store_piece_of_field( obuf(icurs/itypesize), VarName, chunksize )
                icurs = icurs + chunksize
              ENDIF
            CASE ( int_open_for_write_commit )  
              hdrbufsize = obuf(icurs/itypesize)
              IF (num_commit_messages.EQ.0) THEN
                call store_piece_of_field( obuf(icurs/itypesize), 'COMMIT', hdrbufsize )
              ENDIF
              num_commit_messages = num_commit_messages + 1
              icurs = icurs + hdrbufsize
            CASE DEFAULT
              hdrbufsize = obuf(icurs/itypesize)
              IF ((hdr_tag.EQ.int_noop.AND.mytask_local.NE.0.AND.num_noops.LE.0)  &
                  .OR.hdr_tag.NE.int_noop) THEN
                write(VarName,'(I5.5)')vid 

                call store_piece_of_field( obuf(icurs/itypesize), VarName, hdrbufsize )
                vid = vid+1
              ENDIF
              IF ( hdr_tag .EQ. int_noop ) num_noops = num_noops + 1
              icurs = icurs + hdrbufsize
          END SELECT
        ENDDO 



        CALL init_retrieve_pieces_of_field


        CALL retrieve_pieces_of_field ( obuf , VarName, obufsize, sz, retval )


        CALL MPI_Reduce( sz, bigbufsize, 1, MPI_INTEGER, MPI_SUM, ntasks_local_group-1, mpi_comm_local, ierr )



        DO WHILE ( retval ) 



          IF ( mytask_local .EQ. ntasks_local_group-1 ) THEN
            ALLOCATE( bigbuf( (bigbufsize+1)/itypesize ) )
         else
            ALLOCATE( bigbuf(1) )
          ENDIF



          CALL collect_on_comm_debug2("module_io_quilt.F",943,Trim(VarName),        &
                                get_hdr_tag(obuf),sz,get_hdr_rec_size(obuf),  &
                                mpi_comm_local,                               &
                                onebyte,                                      &
                                obuf, sz,                                     &
                                bigbuf, bigbufsize )


          IF ( mytask_local .EQ. ntasks_local_group-1 ) THEN






            icurs = itypesize  

            stored_write_record = .false.


            DO WHILE ( icurs .lt. bigbufsize ) 
              CALL mpi_type_size ( MPI_INTEGER , itypesize , ierr )





              SELECT CASE ( get_hdr_tag( bigbuf(icurs/itypesize) ) )




                CASE ( int_noop )
                  CALL int_get_noop_header( bigbuf(icurs/itypesize), hdrbufsize, itypesize )
                  icurs = icurs + hdrbufsize


                CASE ( int_dom_td_real )
                  CALL mpi_type_size( MPI_REAL, ftypesize, ierr )
                  ALLOCATE( RData( bigbuf(icurs/itypesize + 4 ) ) )      
                  CALL int_get_td_header( bigbuf(icurs/itypesize:), hdrbufsize, itypesize, ftypesize, &
                                          DataHandle, DateStr, Element, RData, Count, code )
                  icurs = icurs + hdrbufsize

                  SELECT CASE (use_package(io_form(DataHandle)))
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_put_dom_td_real( handle(DataHandle),TRIM(Element),TRIM(DateStr),RData, Count, Status )
                    CASE ( IO_INTIO   )
                      CALL ext_int_put_dom_td_real( handle(DataHandle),TRIM(Element),TRIM(DateStr),RData, Count, Status )
                 CASE ( IO_GRIB1 )
                    CALL ext_gr1_put_dom_td_real( handle(DataHandle),TRIM(Element),TRIM(DateStr),RData, Count, Status )
                 CASE ( IO_GRIB2 )
                    CALL ext_gr2_put_dom_td_real( handle(DataHandle),TRIM(Element),TRIM(DateStr),RData, Count, Status )
                     CASE DEFAULT
                      Status = 0
                  END SELECT

                  DEALLOCATE( RData )

                CASE ( int_dom_ti_real )

                  CALL mpi_type_size( MPI_REAL, ftypesize, ierr )
                  ALLOCATE( RData( bigbuf(icurs/itypesize + 4 ) ) )      
                  CALL int_get_ti_header( bigbuf(icurs/itypesize:), hdrbufsize, itypesize, ftypesize, &
                                          DataHandle, Element, RData, Count, code )
                  icurs = icurs + hdrbufsize

                  SELECT CASE (use_package(io_form(DataHandle)))
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_put_dom_ti_real( handle(DataHandle),TRIM(Element), RData, Count, Status )

                    CASE ( IO_INTIO   )
                      CALL ext_int_put_dom_ti_real( handle(DataHandle),TRIM(Element), RData, Count, Status )
                 CASE ( IO_GRIB1 )
                    CALL ext_gr1_put_dom_ti_real( handle(DataHandle),TRIM(Element), RData, Count, Status )
                 CASE ( IO_GRIB2 )
                    CALL ext_gr2_put_dom_ti_real( handle(DataHandle),TRIM(Element), RData, Count, Status )
                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  DEALLOCATE( RData )


                CASE ( int_dom_td_integer )

                  CALL mpi_type_size( MPI_INTEGER, ftypesize, ierr )
                  ALLOCATE( IData( bigbuf(icurs/itypesize + 4 ) ) )      
                  CALL int_get_td_header( bigbuf(icurs/itypesize:), hdrbufsize, itypesize, ftypesize, &
                                          DataHandle, DateStr, Element, IData, Count, code )
                  icurs = icurs + hdrbufsize

                  SELECT CASE (use_package(io_form(DataHandle)))
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_put_dom_td_integer( handle(DataHandle),TRIM(Element), Trim(DateStr), IData, Count, Status )
                    CASE ( IO_INTIO   )
                      CALL ext_int_put_dom_td_integer( handle(DataHandle),TRIM(Element), Trim(DateStr), IData, Count, Status )
                 CASE ( IO_GRIB1 )
                    CALL ext_gr1_put_dom_td_integer( handle(DataHandle),TRIM(Element), Trim(DateStr), IData, Count, Status )
                 CASE ( IO_GRIB2 )
                    CALL ext_gr2_put_dom_td_integer( handle(DataHandle),TRIM(Element), Trim(DateStr), IData, Count, Status )
                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  DEALLOCATE( IData )


                CASE ( int_dom_ti_integer )


                  CALL mpi_type_size( MPI_INTEGER, ftypesize, ierr )
                  ALLOCATE( IData( bigbuf(icurs/itypesize + 4 ) ) )      
                  CALL int_get_ti_header( bigbuf(icurs/itypesize:), hdrbufsize, itypesize, ftypesize, &
                                          DataHandle, Element, IData, Count, code )
                  icurs = icurs + hdrbufsize
                  SELECT CASE (use_package(io_form(DataHandle)))
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_put_dom_ti_integer( handle(DataHandle),TRIM(Element), IData, Count, Status )

                    CASE ( IO_INTIO   )
                      CALL ext_int_put_dom_ti_integer( handle(DataHandle),TRIM(Element), IData, Count, Status )
                 CASE ( IO_GRIB1 )
                    CALL ext_gr1_put_dom_ti_integer( handle(DataHandle),TRIM(Element), IData, Count, Status )
                 CASE ( IO_GRIB2 )
                    CALL ext_gr2_put_dom_ti_integer( handle(DataHandle),TRIM(Element), IData, Count, Status )

                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  DEALLOCATE( IData)
 

                CASE ( int_set_time )

                  CALL int_get_ti_header_char( bigbuf(icurs/itypesize), hdrbufsize, itypesize, &
                                               DataHandle, Element, VarName, CData, code )
                  SELECT CASE (use_package(io_form(DataHandle)))
                    CASE ( IO_INTIO   )
                      CALL ext_int_set_time ( handle(DataHandle), TRIM(CData), Status)
                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  icurs = icurs + hdrbufsize


                CASE ( int_dom_ti_char )

                  CALL int_get_ti_header_char( bigbuf(icurs/itypesize), hdrbufsize, itypesize, &
                                               DataHandle, Element, VarName, CData, code )


                  SELECT CASE (use_package(io_form(DataHandle)))
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_put_dom_ti_char ( handle(DataHandle), TRIM(Element), TRIM(CData), Status)
                    CASE ( IO_INTIO   )
                      CALL ext_int_put_dom_ti_char ( handle(DataHandle), TRIM(Element), TRIM(CData), Status)
                 CASE ( IO_GRIB1 )
                    CALL ext_gr1_put_dom_ti_char ( handle(DataHandle), TRIM(Element), TRIM(CData), Status)
                 CASE ( IO_GRIB2 )
                    CALL ext_gr2_put_dom_ti_char ( handle(DataHandle), TRIM(Element), TRIM(CData), Status)
                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  icurs = icurs + hdrbufsize


                CASE ( int_var_ti_char )

                  CALL int_get_ti_header_char( bigbuf(icurs/itypesize), hdrbufsize, itypesize, &
                                               DataHandle, Element, VarName, CData, code )

                  SELECT CASE (use_package(io_form(DataHandle)))
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_put_var_ti_char ( handle(DataHandle), TRIM(Element), TRIM(VarName), TRIM(CData), Status)
                    CASE ( IO_INTIO   )
                      CALL ext_int_put_var_ti_char ( handle(DataHandle), TRIM(Element), TRIM(VarName), TRIM(CData), Status)
                 CASE ( IO_GRIB1 )
                    CALL ext_gr1_put_var_ti_char ( handle(DataHandle), TRIM(Element), TRIM(VarName), TRIM(CData), Status)
                 CASE ( IO_GRIB2 )
                    CALL ext_gr2_put_var_ti_char ( handle(DataHandle), TRIM(Element), TRIM(VarName), TRIM(CData), Status)
                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  icurs = icurs + hdrbufsize

                CASE ( int_ioexit )

                  CALL wrf_error_fatal3("<stdin>",1119,&
                         "quilt: should have handled int_ioexit already")
! The I/O server "root" handles the "ioclose" request.
                CASE ( int_ioclose )
                  CALL int_get_handle_header( bigbuf(icurs/itypesize), hdrbufsize, itypesize, &
                                              DataHandle , code )
                  icurs = icurs + hdrbufsize

                  IF ( DataHandle .GE. 1 ) THEN


                  SELECT CASE (use_package(io_form(DataHandle)))
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_inquire_filename( handle(DataHandle), fname, fstat, Status )
                      IF ( fstat .EQ. WRF_FILE_OPENED_FOR_WRITE .OR. fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                        CALL ext_ncd_ioclose(handle(DataHandle),Status)
                      ENDIF
                    CASE ( IO_INTIO   )
                      CALL ext_int_inquire_filename( handle(DataHandle), fname, fstat, Status )
                      IF ( fstat .EQ. WRF_FILE_OPENED_FOR_WRITE .OR. fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                        CALL ext_int_ioclose(handle(DataHandle),Status)
                      ENDIF
                 CASE ( IO_GRIB1 )
                    CALL ext_gr1_inquire_filename( handle(DataHandle), fname, fstat, Status )
                    IF ( fstat .EQ. WRF_FILE_OPENED_FOR_WRITE .OR. fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                      CALL ext_gr1_ioclose(handle(DataHandle),Status)
                    ENDIF
                 CASE ( IO_GRIB2 )
                    CALL ext_gr2_inquire_filename( handle(DataHandle), fname, fstat, Status )
                    IF ( fstat .EQ. WRF_FILE_OPENED_FOR_WRITE .OR. fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                      CALL ext_gr2_ioclose(handle(DataHandle),Status)
                    ENDIF
                    CASE DEFAULT
                      Status = 0
                  END SELECT
                  ENDIF


                CASE ( int_open_for_write_begin )

                  CALL int_get_ofwb_header( bigbuf(icurs/itypesize), hdrbufsize, itypesize, &
                                            FileName,SysDepInfo,io_form_arg,DataHandle )





                  icurs = icurs + hdrbufsize

                
                  io_form(DataHandle) = io_form_arg

                  SELECT CASE (use_package(io_form(DataHandle)))
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_open_for_write_begin(FileName,Comm,IOComm,SysDepInfo,handle(DataHandle),Status)

                    CASE ( IO_INTIO   )
                      CALL ext_int_open_for_write_begin(FileName,Comm,IOComm,SysDepInfo,handle(DataHandle),Status)
                    CASE ( IO_GRIB1 )
                       CALL ext_gr1_open_for_write_begin(FileName,Comm,IOComm,SysDepInfo,handle(DataHandle),Status)
                    CASE ( IO_GRIB2 )
                       CALL ext_gr2_open_for_write_begin(FileName,Comm,IOComm,SysDepInfo,handle(DataHandle),Status)
                    CASE DEFAULT
                      Status = 0
                  END SELECT
                
                  okay_to_write(DataHandle) = .false.





                CASE ( int_open_for_write_commit )

                  CALL int_get_handle_header( bigbuf(icurs/itypesize), hdrbufsize, itypesize, &
                                              DataHandle , code )
                  icurs = icurs + hdrbufsize
                  okay_to_commit(DataHandle) = .true.











                CASE ( int_field )
                  CALL mpi_type_size( MPI_INTEGER, ftypesize, ierr )
                  CALL int_get_write_field_header ( bigbuf(icurs/itypesize), hdrbufsize, itypesize, ftypesize,  &
                                                    DataHandle , DateStr , VarName , Dummy , FieldType , Comm , IOComm, &
                                                    DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                                    DomainStart , DomainEnd ,                                    &
                                                    MemoryStart , MemoryEnd ,                                    &
                                                    PatchStart , PatchEnd )

                  icurs = icurs + hdrbufsize

                  IF ( okay_to_write(DataHandle) ) THEN




                    IF ( FieldType .EQ. WRF_FLOAT .OR. FieldType .EQ. WRF_DOUBLE)  THEN
                      
                      
                      IF ( FieldType .EQ. WRF_DOUBLE)  THEN

                        CALL mpi_type_size( MPI_DOUBLE_PRECISION, ftypesize, ierr )
                      ELSE
                        CALL mpi_type_size( MPI_REAL, ftypesize, ierr )
                      ENDIF
                      stored_write_record = .true.
                      CALL store_patch_in_outbuf ( bigbuf(icurs/itypesize), dummybuf, TRIM(DateStr), TRIM(VarName) , &
                                                   FieldType, TRIM(MemoryOrder), TRIM(Stagger), DimNames, &
                                                   DomainStart , DomainEnd , &
                                                   MemoryStart , MemoryEnd , &
                                                   PatchStart , PatchEnd )

                    ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
                      CALL mpi_type_size( MPI_INTEGER, ftypesize, ierr )
                      stored_write_record = .true.
                      CALL store_patch_in_outbuf ( dummybuf, bigbuf(icurs/itypesize), TRIM(DateStr), TRIM(VarName) , &
                                                   FieldType, TRIM(MemoryOrder), TRIM(Stagger), DimNames, &
                                                   DomainStart , DomainEnd , &
                                                   MemoryStart , MemoryEnd , &
                                                   PatchStart , PatchEnd )
                    ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN
                      ftypesize = 4
                    ENDIF
                    icurs = icurs + (PatchEnd(1)-PatchStart(1)+1)*(PatchEnd(2)-PatchStart(2)+1)* &
                                    (PatchEnd(3)-PatchStart(3)+1)*ftypesize
                  ELSE
                    SELECT CASE (use_package(io_form(DataHandle)))
                      CASE ( IO_NETCDF   )
                        CALL ext_ncd_write_field ( handle(DataHandle) , TRIM(DateStr) ,         &
                                   TRIM(VarName) , dummy , FieldType , Comm , IOComm,           &
                                   DomainDesc , TRIM(MemoryOrder) , TRIM(Stagger) , DimNames ,  &
                                   DomainStart , DomainEnd ,                                    &
                                   DomainStart , DomainEnd ,                                    &
                                   DomainStart , DomainEnd ,                                    &
                                   Status )
                      CASE DEFAULT
                        Status = 0
                    END SELECT
                  ENDIF
                CASE ( int_iosync )
                  CALL int_get_handle_header( bigbuf(icurs/itypesize), hdrbufsize, itypesize, &
                                            DataHandle , code )
                  icurs = icurs + hdrbufsize
                CASE DEFAULT
                  WRITE(mess,*)'quilt: bad tag: ',get_hdr_tag( bigbuf(icurs/itypesize) ),' icurs ',icurs/itypesize
                  CALL wrf_error_fatal3("<stdin>",1273,&
mess )
              END SELECT

            ENDDO 



            IF (stored_write_record) THEN







              CALL write_outbuf ( handle(DataHandle), use_package(io_form(DataHandle))) 

            ENDIF




            IF (okay_to_commit(DataHandle)) THEN

              SELECT CASE (use_package(io_form(DataHandle)))
                CASE ( IO_NETCDF   )
                  CALL ext_ncd_inquire_filename( handle(DataHandle), fname, fstat, Status )
                  IF ( fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                    CALL ext_ncd_open_for_write_commit(handle(DataHandle),Status)
                    okay_to_write(DataHandle) = .true.
                  ENDIF
                CASE ( IO_INTIO   )
                  CALL ext_int_inquire_filename( handle(DataHandle), fname, fstat, Status )
                  IF ( fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                    CALL ext_int_open_for_write_commit(handle(DataHandle),Status)
                    okay_to_write(DataHandle) = .true.
                  ENDIF
                 CASE ( IO_GRIB1 )
                    CALL ext_gr1_inquire_filename( handle(DataHandle), fname, fstat, Status )
                    IF ( fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                       CALL ext_gr1_open_for_write_commit(handle(DataHandle),Status)
                       okay_to_write(DataHandle) = .true.
                    ENDIF
                 CASE ( IO_GRIB2 )
                    CALL ext_gr2_inquire_filename( handle(DataHandle), fname, fstat, Status )
                    IF ( fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                       CALL ext_gr2_open_for_write_commit(handle(DataHandle),Status)
                       okay_to_write(DataHandle) = .true.
                    ENDIF

                CASE DEFAULT
                  Status = 0
              END SELECT

            okay_to_commit(DataHandle) = .false.
          ENDIF
          DEALLOCATE( bigbuf )
        ENDIF
        if(allocated(bigbuf)) deallocate(bigbuf)


        CALL retrieve_pieces_of_field ( obuf , VarName, obufsize, sz, retval )


        CALL MPI_Reduce( sz, bigbufsize, 1, MPI_INTEGER,MPI_SUM, ntasks_local_group-1,mpi_comm_local, ierr )



      END DO 

      DEALLOCATE( obuf )

      
      IF (stored_write_record) THEN

        SELECT CASE ( use_package(io_form) )
          CASE ( IO_NETCDF   )
            CALL ext_ncd_iosync( handle(DataHandle), Status )
          CASE ( IO_GRIB1   )
            CALL ext_gr1_iosync( handle(DataHandle), Status )
          CASE ( IO_GRIB2   )
            CALL ext_gr2_iosync( handle(DataHandle), Status )
          CASE ( IO_INTIO   )
            CALL ext_int_iosync( handle(DataHandle), Status )
          CASE DEFAULT
            Status = 0
        END SELECT

      ENDIF

      END DO 

    END SUBROUTINE quilt

    SUBROUTINE quilt_pnc





      USE module_state_description
      USE module_quilt_outbuf_ops
      IMPLICIT NONE
      INCLUDE 'mpif.h'
  INTEGER, PARAMETER ::  int_ioexit			=  	     10
  INTEGER, PARAMETER ::  int_open_for_write_begin	=  	     20
  INTEGER, PARAMETER ::  int_open_for_write_commit	=  	     30
  INTEGER, PARAMETER ::  int_open_for_read 		=  	     40
  INTEGER, PARAMETER ::  int_inquire_opened 		=  	     60
  INTEGER, PARAMETER ::  int_inquire_filename 		=  	     70
  INTEGER, PARAMETER ::  int_iosync 			=  	     80
  INTEGER, PARAMETER ::  int_ioclose 			=  	     90
  INTEGER, PARAMETER ::  int_next_time 			=  	    100
  INTEGER, PARAMETER ::  int_set_time 			=  	    110
  INTEGER, PARAMETER ::  int_next_var 			=  	    120
  INTEGER, PARAMETER ::  int_dom_ti_real 		=  	    140
  INTEGER, PARAMETER ::  int_dom_ti_double 		=  	    160
  INTEGER, PARAMETER ::  int_dom_ti_integer 		=  	    180
  INTEGER, PARAMETER ::  int_dom_ti_logical 		=  	    200
  INTEGER, PARAMETER ::  int_dom_ti_char 		=  	    220
  INTEGER, PARAMETER ::  int_dom_td_real 		=  	    240
  INTEGER, PARAMETER ::  int_dom_td_double 		=  	    260
  INTEGER, PARAMETER ::  int_dom_td_integer 		=  	    280
  INTEGER, PARAMETER ::  int_dom_td_logical 		=  	    300
  INTEGER, PARAMETER ::  int_dom_td_char 		=  	    320
  INTEGER, PARAMETER ::  int_var_ti_real 		=  	    340
  INTEGER, PARAMETER ::  int_var_ti_double 		=  	    360
  INTEGER, PARAMETER ::  int_var_ti_integer 		=  	    380
  INTEGER, PARAMETER ::  int_var_ti_logical 		=  	    400
  INTEGER, PARAMETER ::  int_var_ti_char 		=  	    420
  INTEGER, PARAMETER ::  int_var_td_real 		=  	    440
  INTEGER, PARAMETER ::  int_var_td_double 		=  	    460
  INTEGER, PARAMETER ::  int_var_td_integer 		=  	    480
  INTEGER, PARAMETER ::  int_var_td_logical 		=  	    500
  INTEGER, PARAMETER ::  int_var_td_char 		=  	    520
  INTEGER, PARAMETER ::  int_field 			=  	    530
  INTEGER, PARAMETER ::  int_var_info 			=  	    540
  INTEGER, PARAMETER ::  int_noop 			=  	    550
      integer, parameter  :: WRF_FILE_NOT_OPENED                  = 100
      integer, parameter  :: WRF_FILE_OPENED_NOT_COMMITTED        = 101
      integer, parameter  :: WRF_FILE_OPENED_FOR_WRITE            = 102
      integer, parameter  :: WRF_FILE_OPENED_FOR_READ             = 103
      integer, parameter  :: WRF_REAL                             = 104
      integer, parameter  :: WRF_DOUBLE                           = 105
      integer, parameter  :: WRF_FLOAT=WRF_REAL
      integer, parameter  :: WRF_INTEGER                          = 106
      integer, parameter  :: WRF_LOGICAL                          = 107
      integer, parameter  :: WRF_COMPLEX                          = 108
      integer, parameter  :: WRF_DOUBLE_COMPLEX                   = 109
      integer, parameter  :: WRF_FILE_OPENED_FOR_UPDATE           = 110


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
      INTEGER itag, ninbuf, ntasks_io_group, ntasks_local_group, mytask_local, ierr
      INTEGER istat
      INTEGER mytask_io_group
      INTEGER   :: nout_set = 0
      INTEGER   :: obufsize, bigbufsize, chunksize, sz
      REAL,                 DIMENSION(1) :: dummy
      INTEGER, ALLOCATABLE, DIMENSION(:) :: obuf, bigbuf
      REAL,    ALLOCATABLE, DIMENSION(:) :: RDATA
      INTEGER, ALLOCATABLE, DIMENSION(:) :: IDATA
      CHARACTER (LEN=512) :: CDATA
      CHARACTER (LEN=80) :: fname
      INTEGER icurs, hdrbufsize, itypesize, ftypesize, rtypesize, Status, fstat, io_form_arg
      INTEGER :: DataHandle, FieldType, Comm, IOComm, DomainDesc, code, Count
      INTEGER, DIMENSION(3) :: DomainStart , DomainEnd , MemoryStart , MemoryEnd , PatchStart , PatchEnd
      INTEGER :: dummybuf(1)
      INTEGER :: num_noops, num_commit_messages, num_field_training_msgs, hdr_tag
      CHARACTER (len=256) :: DateStr , Element, VarName, MemoryOrder , Stagger , DimNames(3), FileName, SysDepInfo, mess
      INTEGER, EXTERNAL :: use_package
      LOGICAL           :: stored_write_record, retval, written_record
      INTEGER iii, jjj, vid, CC, DD





      SysDepInfo = " "
      CALL ext_ncd_ioinit( SysDepInfo, ierr)
      CALL ext_int_ioinit( SysDepInfo, ierr )
      CALL ext_gr1_ioinit( SysDepInfo, ierr)
      CALL ext_gr2_ioinit( SysDepInfo, ierr)

      okay_to_commit = .false.
      stored_write_record = .false.
      ninbuf = 0
      
      
      CALL Mpi_Comm_Size (  mpi_comm_io_groups(1), ntasks_io_group,    ierr  )
      CALL MPI_COMM_RANK( mpi_comm_io_groups(1), mytask_io_group,    ierr )
      CALL Mpi_Comm_Size (  mpi_comm_local,        ntasks_local_group, ierr  )
      CALL MPI_COMM_RANK( mpi_comm_local,        mytask_local,       ierr )

      CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )
      IF ( itypesize <= 0 ) THEN
        CALL wrf_error_fatal3("<stdin>",1470,&
"external/RSL/module_dm.F: quilt: type size <= 0 invalid")
      ENDIF







       CC = ntasks_io_group - 1

       DD = ncompute_tasks / ntasks_local_group









      DO WHILE (.TRUE.)  













        
        

        
        ! if needed (currently needed only for ioclose).
        reduced_dummy = 0
        CALL MPI_Reduce( reduced_dummy, reduced, 2, MPI_INTEGER, MPI_SUM, mytask_io_group, mpi_comm_io_groups(1), ierr )
        obufsize = reduced(1)



        IF ( obufsize .LT. 0 ) THEN
          IF ( obufsize .EQ. -100 ) THEN         
            CALL ext_ncd_ioexit( Status )
            CALL ext_int_ioexit( Status )
            CALL ext_gr1_ioexit( Status )
            CALL ext_gr2_ioexit( Status )
            CALL wrf_message ( 'I/O QUILT SERVERS DONE' )
            CALL mpi_finalize(ierr)
            STOP
          ELSE
            WRITE(mess,*)'Possible 32-bit overflow on output server. Try larger nio_tasks_per_group in namelist.'
            CALL wrf_error_fatal3("<stdin>",1528,&
mess)
          ENDIF
        ENDIF







        IF ( obufsize .GT. 0 ) THEN
          ALLOCATE( obuf( (obufsize+1)/itypesize ) )


          CALL collect_on_comm_debug("module_io_quilt.F",1709, mpi_comm_io_groups(1),        &
                                onebyte,                      &
                                dummy, 0,                     &
                                obuf, obufsize )

        ELSE
          ! Necessarily, the compute processes send the ioclose signal,
          
          ! will stall on the ioclose message waiting for the quilt 
          
          
          
          
          
          ! Then a header representing the ioclose message is constructed
          
          
          
          
          
          
          ALLOCATE( obuf( 4096 ) )
          
          CALL int_gen_handle_header( obuf, obufsize, itypesize, &
                                      reduced(2) , int_ioclose )
        ENDIF












        CALL init_store_piece_of_field
        CALL mpi_type_size ( MPI_INTEGER , itypesize , ierr )



        vid = 0
        icurs = itypesize
        num_noops = 0 
        num_commit_messages = 0 
        num_field_training_msgs = 0 
        DO WHILE ( icurs .lt. obufsize ) 
          hdr_tag = get_hdr_tag( obuf ( icurs / itypesize ) )
          SELECT CASE ( hdr_tag )
            CASE ( int_field )
              CALL int_get_write_field_header ( obuf(icurs/itypesize), hdrbufsize, itypesize, ftypesize,  &
                                                DataHandle , DateStr , VarName , Dummy , FieldType , Comm , IOComm, &
                                                DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                                DomainStart , DomainEnd ,                                    &
                                                MemoryStart , MemoryEnd ,                                    &
                                                PatchStart , PatchEnd )
              chunksize = (PatchEnd(1)-PatchStart(1)+1)*(PatchEnd(2)-PatchStart(2)+1)* &
                          (PatchEnd(3)-PatchStart(3)+1)*ftypesize

              IF ( DomainDesc .EQ. 333933 ) THEN  
                 IF ( num_field_training_msgs .EQ. 0 ) THEN
                   call add_to_bufsize_for_field( VarName, hdrbufsize )

                 ENDIF
                 num_field_training_msgs = num_field_training_msgs + 1
              ELSE
                 call add_to_bufsize_for_field( VarName, hdrbufsize )

              ENDIF
              icurs = icurs + hdrbufsize



              
              
              IF ( DomainDesc .NE. 333933 ) THEN   

                call add_to_bufsize_for_field( VarName, chunksize )
                icurs = icurs + chunksize
              ENDIF
            CASE ( int_open_for_write_commit )  
              hdrbufsize = obuf(icurs/itypesize)
              IF (num_commit_messages.EQ.0) THEN
                call add_to_bufsize_for_field( 'COMMIT', hdrbufsize )
              ENDIF
              num_commit_messages = num_commit_messages + 1
              icurs = icurs + hdrbufsize
            CASE DEFAULT
              hdrbufsize = obuf(icurs/itypesize)





























              IF (hdr_tag.NE.int_noop) THEN
                write(VarName,'(I5.5)')vid 

                call add_to_bufsize_for_field( VarName, hdrbufsize )
                vid = vid+1
              ENDIF
              IF ( hdr_tag .EQ. int_noop ) num_noops = num_noops + 1
              icurs = icurs + hdrbufsize

          END SELECT
        ENDDO 



        vid = 0
        icurs = itypesize
        num_noops = 0 
        num_commit_messages = 0 
        num_field_training_msgs = 0 
        DO WHILE ( icurs .lt. obufsize ) 

          hdr_tag = get_hdr_tag( obuf ( icurs / itypesize ) )
          SELECT CASE ( hdr_tag )
            CASE ( int_field )
              CALL int_get_write_field_header ( obuf(icurs/itypesize), hdrbufsize, itypesize, ftypesize,  &
                                                DataHandle , DateStr , VarName , Dummy , FieldType , Comm , IOComm, &
                                                DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                                DomainStart , DomainEnd ,                                    &
                                                MemoryStart , MemoryEnd ,                                    &
                                                PatchStart , PatchEnd )
              chunksize = (PatchEnd(1)-PatchStart(1)+1)*(PatchEnd(2)-PatchStart(2)+1)* &
                          (PatchEnd(3)-PatchStart(3)+1)*ftypesize

              IF ( DomainDesc .EQ. 333933 ) THEN  
                 IF ( num_field_training_msgs .EQ. 0 ) THEN
                   call store_piece_of_field( obuf(icurs/itypesize), VarName, hdrbufsize )

                 ENDIF
                 num_field_training_msgs = num_field_training_msgs + 1
              ELSE
                 call store_piece_of_field( obuf(icurs/itypesize), VarName, hdrbufsize )

              ENDIF
              icurs = icurs + hdrbufsize
              
              
              IF ( DomainDesc .NE. 333933 ) THEN   
                call store_piece_of_field( obuf(icurs/itypesize), VarName, chunksize )
                icurs = icurs + chunksize

              ENDIF
            CASE ( int_open_for_write_commit )  
              hdrbufsize = obuf(icurs/itypesize)
              IF (num_commit_messages.EQ.0) THEN
                call store_piece_of_field( obuf(icurs/itypesize), 'COMMIT', hdrbufsize )
              ENDIF
              num_commit_messages = num_commit_messages + 1
              icurs = icurs + hdrbufsize
            CASE DEFAULT
              hdrbufsize = obuf(icurs/itypesize)
              IF (hdr_tag.NE.int_noop) THEN

                write(VarName,'(I5.5)')vid 

                call store_piece_of_field( obuf(icurs/itypesize), VarName, hdrbufsize )
                vid = vid+1
              ENDIF
              IF ( hdr_tag .EQ. int_noop ) num_noops = num_noops + 1
              icurs = icurs + hdrbufsize
          END SELECT
       ENDDO 



       CALL init_retrieve_pieces_of_field


       CALL retrieve_pieces_of_field ( obuf , VarName, obufsize, sz, retval )
       written_record = .false.


       DO WHILE ( retval ) 




            icurs = itypesize  

            stored_write_record = .false.



            DO WHILE ( icurs .lt. sz)





              SELECT CASE ( get_hdr_tag( obuf(icurs/itypesize) ) )


                CASE ( int_noop )
                  CALL int_get_noop_header( obuf(icurs/itypesize), &
                                            hdrbufsize, itypesize )
                  icurs = icurs + hdrbufsize


                CASE ( int_dom_td_real )
                  CALL mpi_type_size( MPI_REAL, ftypesize, ierr )
                  ALLOCATE( RData( obuf(icurs/itypesize + 4 ) ) )      
                  CALL int_get_td_header( obuf(icurs/itypesize:), hdrbufsize, itypesize, ftypesize, &
                                          DataHandle, DateStr, Element, RData, Count, code )
                  icurs = icurs + hdrbufsize

                  SELECT CASE (use_package(io_form(DataHandle)))
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_put_dom_td_real( handle(DataHandle),TRIM(Element),TRIM(DateStr),RData, Count, Status )
                    CASE ( IO_INTIO   )
                      CALL ext_int_put_dom_td_real( handle(DataHandle),TRIM(Element),TRIM(DateStr),RData, Count, Status )
                 CASE ( IO_GRIB1 )
                    CALL ext_gr1_put_dom_td_real( handle(DataHandle),TRIM(Element),TRIM(DateStr),RData, Count, Status )
                 CASE ( IO_GRIB2 )
                    CALL ext_gr2_put_dom_td_real( handle(DataHandle),TRIM(Element),TRIM(DateStr),RData, Count, Status )
                     CASE DEFAULT
                      Status = 0
                  END SELECT

                  DEALLOCATE( RData )

                CASE ( int_dom_ti_real )

                  CALL mpi_type_size( MPI_REAL, ftypesize, ierr )
                  ALLOCATE( RData( obuf(icurs/itypesize + 4 ) ) )      
                  CALL int_get_ti_header( obuf(icurs/itypesize:), hdrbufsize, itypesize, ftypesize, &
                                          DataHandle, Element, RData, Count, code )
                  icurs = icurs + hdrbufsize

                  SELECT CASE (use_package(io_form(DataHandle)))
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_put_dom_ti_real( handle(DataHandle),TRIM(Element), RData, Count, Status )
                    CASE ( IO_INTIO   )
                      CALL ext_int_put_dom_ti_real( handle(DataHandle),TRIM(Element), RData, Count, Status )
                 CASE ( IO_GRIB1 )
                    CALL ext_gr1_put_dom_ti_real( handle(DataHandle),TRIM(Element), RData, Count, Status )
                 CASE ( IO_GRIB2 )
                    CALL ext_gr2_put_dom_ti_real( handle(DataHandle),TRIM(Element), RData, Count, Status )
                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  DEALLOCATE( RData )


                CASE ( int_dom_td_integer )

                  CALL mpi_type_size( MPI_INTEGER, ftypesize, ierr )
                  ALLOCATE( IData( obuf(icurs/itypesize + 4 ) ) )      
                  CALL int_get_td_header( obuf(icurs/itypesize:), hdrbufsize, itypesize, ftypesize, &
                                          DataHandle, DateStr, Element, IData, Count, code )
                  icurs = icurs + hdrbufsize

                  SELECT CASE (use_package(io_form(DataHandle)))
                   CASE ( IO_NETCDF   )
                      CALL ext_ncd_put_dom_td_integer( handle(DataHandle),TRIM(Element), Trim(DateStr), IData, Count, Status )
                   CASE ( IO_INTIO   )
                      CALL ext_int_put_dom_td_integer( handle(DataHandle),TRIM(Element), Trim(DateStr), IData, Count, Status )
                   CASE ( IO_GRIB1 )
                      CALL ext_gr1_put_dom_td_integer( handle(DataHandle),TRIM(Element), Trim(DateStr), IData, Count, Status )
                   CASE ( IO_GRIB2 )
                      CALL ext_gr2_put_dom_td_integer( handle(DataHandle),TRIM(Element), Trim(DateStr), IData, Count, Status )
                   CASE DEFAULT
                      Status = 0
                   END SELECT

                   DEALLOCATE( IData )


                CASE ( int_dom_ti_integer )

                  CALL mpi_type_size( MPI_INTEGER, ftypesize, ierr )
                  ALLOCATE( IData( obuf(icurs/itypesize + 4 ) ) )      
                  CALL int_get_ti_header( obuf(icurs/itypesize:), hdrbufsize, itypesize, ftypesize, &
                                          DataHandle, Element, IData, Count, code )
                  icurs = icurs + hdrbufsize
                  SELECT CASE (use_package(io_form(DataHandle)))
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_put_dom_ti_integer( handle(DataHandle),TRIM(Element), IData, Count, Status )
                    CASE ( IO_INTIO   )
                      CALL ext_int_put_dom_ti_integer( handle(DataHandle),TRIM(Element), IData, Count, Status )
                 CASE ( IO_GRIB1 )
                    CALL ext_gr1_put_dom_ti_integer( handle(DataHandle),TRIM(Element), IData, Count, Status )
                 CASE ( IO_GRIB2 )
                    CALL ext_gr2_put_dom_ti_integer( handle(DataHandle),TRIM(Element), IData, Count, Status )

                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  DEALLOCATE( IData)
 

                CASE ( int_set_time )

                  CALL int_get_ti_header_char( obuf(icurs/itypesize), hdrbufsize, itypesize, &
                                               DataHandle, Element, VarName, CData, code )
                  SELECT CASE (use_package(io_form(DataHandle)))
                    CASE ( IO_INTIO   )
                      CALL ext_int_set_time ( handle(DataHandle), TRIM(CData), Status)
                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  icurs = icurs + hdrbufsize


                CASE ( int_dom_ti_char )

                  CALL int_get_ti_header_char( obuf(icurs/itypesize), hdrbufsize, itypesize, &
                                               DataHandle, Element, VarName, CData, code )

                  SELECT CASE (use_package(io_form(DataHandle)))
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_put_dom_ti_char ( handle(DataHandle), TRIM(Element), TRIM(CData), Status)
                    CASE ( IO_INTIO   )
                      CALL ext_int_put_dom_ti_char ( handle(DataHandle), TRIM(Element), TRIM(CData), Status)
                   CASE ( IO_GRIB1 )
                      CALL ext_gr1_put_dom_ti_char ( handle(DataHandle), TRIM(Element), TRIM(CData), Status)
                   CASE ( IO_GRIB2 )
                      CALL ext_gr2_put_dom_ti_char ( handle(DataHandle), TRIM(Element), TRIM(CData), Status)
                   CASE DEFAULT
                      Status = 0
                   END SELECT

                  icurs = icurs + hdrbufsize


                CASE ( int_var_ti_char )

                  CALL int_get_ti_header_char( obuf(icurs/itypesize), hdrbufsize, itypesize, &
                                               DataHandle, Element, VarName, CData, code )

                  SELECT CASE (use_package(io_form(DataHandle)))
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_put_var_ti_char ( handle(DataHandle), TRIM(Element), TRIM(VarName), TRIM(CData), Status)
                    CASE ( IO_INTIO   )
                      CALL ext_int_put_var_ti_char ( handle(DataHandle), TRIM(Element), TRIM(VarName), TRIM(CData), Status)
                   CASE ( IO_GRIB1 )
                      CALL ext_gr1_put_var_ti_char ( handle(DataHandle), TRIM(Element), TRIM(VarName), TRIM(CData), Status)
                   CASE ( IO_GRIB2 )
                      CALL ext_gr2_put_var_ti_char ( handle(DataHandle), TRIM(Element), TRIM(VarName), TRIM(CData), Status)
                   CASE DEFAULT
                      Status = 0
                   END SELECT

                  icurs = icurs + hdrbufsize

                CASE ( int_ioexit )

                  CALL wrf_error_fatal3("<stdin>",1921,&
                         "quilt: should have handled int_ioexit already")
! Every I/O server handles the "ioclose" request.
                CASE ( int_ioclose )
                  CALL int_get_handle_header( obuf(icurs/itypesize), hdrbufsize, itypesize, &
                                              DataHandle , code )
                  icurs = icurs + hdrbufsize

                  IF ( DataHandle .GE. 1 ) THEN

                     SELECT CASE (use_package(io_form(DataHandle)))
                     CASE ( IO_NETCDF   )
                        CALL ext_ncd_inquire_filename( handle(DataHandle), fname, fstat, Status )
                        IF ( fstat .EQ. WRF_FILE_OPENED_FOR_WRITE .OR. fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                           CALL ext_ncd_ioclose(handle(DataHandle),Status)
                        ENDIF
                     CASE ( IO_INTIO   )
                        CALL ext_int_inquire_filename( handle(DataHandle), fname, fstat, Status )
                        IF ( fstat .EQ. WRF_FILE_OPENED_FOR_WRITE .OR. fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                           CALL ext_int_ioclose(handle(DataHandle),Status)
                        ENDIF
                     CASE ( IO_GRIB1 )
                        CALL ext_gr1_inquire_filename( handle(DataHandle), fname, fstat, Status )
                        IF ( fstat .EQ. WRF_FILE_OPENED_FOR_WRITE .OR. fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                           CALL ext_gr1_ioclose(handle(DataHandle),Status)
                        ENDIF
                     CASE ( IO_GRIB2 )
                        CALL ext_gr2_inquire_filename( handle(DataHandle), fname, fstat, Status )
                        IF ( fstat .EQ. WRF_FILE_OPENED_FOR_WRITE .OR. fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                           CALL ext_gr2_ioclose(handle(DataHandle),Status)
                        ENDIF
                     CASE DEFAULT
                        Status = 0
                     END SELECT
                  ENDIF


                CASE ( int_open_for_write_begin )

                  CALL int_get_ofwb_header( obuf(icurs/itypesize), hdrbufsize, itypesize, &
                                            FileName,SysDepInfo,io_form_arg,DataHandle )





                  icurs = icurs + hdrbufsize

                
                  io_form(DataHandle) = io_form_arg

                  SELECT CASE (use_package(io_form(DataHandle)))
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_open_for_write_begin(FileName,Comm,IOComm,SysDepInfo,handle(DataHandle),Status)

                    CASE ( IO_INTIO   )
                      CALL ext_int_open_for_write_begin(FileName,Comm,IOComm,SysDepInfo,handle(DataHandle),Status)
                    CASE ( IO_GRIB1 )
                       CALL ext_gr1_open_for_write_begin(FileName,Comm,IOComm,SysDepInfo,handle(DataHandle),Status)
                    CASE ( IO_GRIB2 )
                       CALL ext_gr2_open_for_write_begin(FileName,Comm,IOComm,SysDepInfo,handle(DataHandle),Status)
                    CASE DEFAULT
                      Status = 0
                  END SELECT
                
                  okay_to_write(DataHandle) = .false.





                CASE ( int_open_for_write_commit )

                  CALL int_get_handle_header( obuf(icurs/itypesize), hdrbufsize, itypesize, &
                                              DataHandle , code )
                  icurs = icurs + hdrbufsize
                  okay_to_commit(DataHandle) = .true.










                CASE ( int_field )
                  CALL mpi_type_size( MPI_INTEGER, ftypesize, ierr )
                  CALL int_get_write_field_header ( obuf(icurs/itypesize), hdrbufsize, itypesize, ftypesize,  &
                                                    DataHandle , DateStr , VarName , Dummy , FieldType , Comm , IOComm, &
                                                    DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                                    DomainStart , DomainEnd ,                                    &
                                                    MemoryStart , MemoryEnd ,                                    &
                                                    PatchStart , PatchEnd )

                  icurs = icurs + hdrbufsize

                  IF ( okay_to_write(DataHandle) ) THEN









                    IF ( FieldType .EQ. WRF_FLOAT .OR. FieldType .EQ. WRF_DOUBLE)  THEN
                      
                      
                      IF ( FieldType .EQ. WRF_DOUBLE)  THEN

                        CALL mpi_type_size( MPI_DOUBLE_PRECISION, ftypesize, ierr )
                      ELSE
                        CALL mpi_type_size( MPI_REAL, ftypesize, ierr )
                      ENDIF

                    ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
                      CALL mpi_type_size( MPI_INTEGER, ftypesize, ierr )
                    ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN
                      ftypesize = 4
                    ENDIF

                    icurs = icurs + (PatchEnd(1)-PatchStart(1)+1)* &
                                    (PatchEnd(2)-PatchStart(2)+1)* &
                                    (PatchEnd(3)-PatchStart(3)+1)*ftypesize

                  ELSE 

                    SELECT CASE (use_package(io_form(DataHandle)))

                      CASE ( IO_NETCDF   )
                        CALL ext_ncd_write_field ( handle(DataHandle) , TRIM(DateStr) ,         &
                                   TRIM(VarName) , dummy , FieldType , Comm , IOComm,           &
                                   DomainDesc , TRIM(MemoryOrder) , TRIM(Stagger) , DimNames ,  &
                                   DomainStart , DomainEnd ,                                    &
                                   DomainStart , DomainEnd ,                                    &
                                   DomainStart , DomainEnd ,                                    &
                                   Status )
                      CASE DEFAULT
                        Status = 0
                    END SELECT
                  ENDIF
                CASE ( int_iosync )
                  CALL int_get_handle_header( obuf(icurs/itypesize), hdrbufsize, itypesize, &
                                            DataHandle , code )
                  icurs = icurs + hdrbufsize
                CASE DEFAULT
                  WRITE(mess,*)'quilt: bad tag: ',                            &
                               get_hdr_tag( obuf(icurs/itypesize) ),' icurs ',&
                               icurs/itypesize
                  CALL wrf_error_fatal3("<stdin>",2073,&
mess )
              END SELECT

            ENDDO 



            IF (stored_write_record) THEN








              stored_write_record = .false.
              written_record = .true.
            ENDIF




            IF (okay_to_commit(DataHandle)) THEN

              SELECT CASE (use_package(io_form(DataHandle)))
                CASE ( IO_NETCDF   )
                  CALL ext_ncd_inquire_filename( handle(DataHandle), fname, fstat, Status )
                  IF ( fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                    CALL ext_ncd_open_for_write_commit(handle(DataHandle),Status)
                    okay_to_write(DataHandle) = .true.
                  ENDIF
                CASE ( IO_INTIO   )
                  CALL ext_int_inquire_filename( handle(DataHandle), fname, fstat, Status )
                  IF ( fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                    CALL ext_int_open_for_write_commit(handle(DataHandle),Status)
                    okay_to_write(DataHandle) = .true.
                  ENDIF
                 CASE ( IO_GRIB1 )
                    CALL ext_gr1_inquire_filename( handle(DataHandle), fname, fstat, Status )
                    IF ( fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                       CALL ext_gr1_open_for_write_commit(handle(DataHandle),Status)
                       okay_to_write(DataHandle) = .true.
                    ENDIF
                 CASE ( IO_GRIB2 )
                    CALL ext_gr2_inquire_filename( handle(DataHandle), fname, fstat, Status )
                    IF ( fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                       CALL ext_gr2_open_for_write_commit(handle(DataHandle),Status)
                       okay_to_write(DataHandle) = .true.
                    ENDIF

                CASE DEFAULT
                  Status = 0
              END SELECT

            okay_to_commit(DataHandle) = .false.
          ENDIF




        CALL retrieve_pieces_of_field ( obuf , VarName, obufsize, sz, retval )
      END DO 

      DEALLOCATE( obuf )

      
      IF (written_record) THEN

        SELECT CASE ( use_package(io_form) )
          CASE DEFAULT
            Status = 0
        END SELECT
        written_record = .false.

      ENDIF

      END DO 

    END SUBROUTINE quilt_pnc



    SUBROUTINE init_module_wrf_quilt
      USE module_wrf_error, only: init_module_wrf_error








      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INTEGER i
      NAMELIST /namelist_quilt/ nio_tasks_per_group, nio_groups, poll_servers
      INTEGER ntasks, mytask, ierr, io_status
      INTEGER mpi_comm_here, temp_poll
      LOGICAL mpi_inited
      LOGICAL esmf_coupling


      esmf_coupling = .FALSE.

      quilting_enabled = .FALSE.
      IF ( disable_quilt ) RETURN

      DO i = 1,int_num_handles
        okay_to_write(i) = .FALSE.
        int_handle_in_use(i) = .FALSE.
        server_for_handle(i) = 0 
        int_num_bytes_to_write(i) = 0
      ENDDO

      CALL MPI_INITIALIZED( mpi_inited, ierr )
      IF ( .NOT. mpi_inited ) THEN
        CALL mpi_init ( ierr )
        CALL wrf_set_dm_communicator( MPI_COMM_WORLD )
        CALL wrf_termio_dup
      ENDIF
      CALL wrf_get_dm_communicator( mpi_comm_here )

      CALL MPI_Comm_rank ( mpi_comm_here, mytask, ierr ) ;
      CALL Mpi_Comm_Size (  mpi_comm_here, ntasks, ierr  ) ;

      IF ( mytask .EQ. 0 ) THEN
        OPEN ( unit=27, file="namelist.input", form="formatted", status="old" )
        nio_groups = 1
        nio_tasks_per_group  = 0
        poll_servers = .false.
        READ ( 27 , NML = namelist_quilt, IOSTAT=io_status )
        IF (io_status .NE. 0) THEN
          CALL wrf_error_fatal3("<stdin>",2207,&
"ERROR reading namelist namelist_quilt" )
        ENDIF
        CLOSE ( 27 )
        IF ( esmf_coupling ) THEN
          IF ( nio_tasks_per_group > 0 ) THEN
            CALL wrf_error_fatal3("<stdin>",2213,&
"frame/module_io_quilt.F: cannot use "// &
                                 "ESMF coupling with quilt tasks") ;
          ENDIF
        ENDIF
        if(poll_servers) then
           temp_poll=1
        else
           temp_poll=0
        endif
      ENDIF

      CALL mpi_bcast( nio_tasks_per_group  , 1 , MPI_INTEGER , 0 , mpi_comm_here, ierr )
      CALL mpi_bcast( nio_groups , 1 , MPI_INTEGER , 0 , mpi_comm_here, ierr )
      CALL mpi_bcast( temp_poll , 1 , MPI_INTEGER , 0 , mpi_comm_here, ierr )

      poll_servers = (temp_poll == 1)

      CALL setup_quilt_servers( nio_tasks_per_group,            &
                                mytask,               &
                                ntasks,               &
                                nio_groups,           &
                                nio_tasks_in_group,   &
                                mpi_comm_here,        &
                                mpi_comm_local,       &
                                mpi_comm_io_groups)

      call init_module_wrf_error(on_io_server=.true.)

       
       IF ( compute_node ) THEN
          CALL wrf_set_dm_communicator( mpi_comm_local )
          IF (coupler_on) CALL cpl_set_dm_communicator( mpi_comm_local )
       ELSE
          IF (coupler_on) CALL cpl_set_dm_communicator( MPI_COMM_NULL )
          CALL quilt    
       ENDIF
      RETURN
    END SUBROUTINE init_module_wrf_quilt




END MODULE module_wrf_quilt







SUBROUTINE disable_quilting




  USE module_wrf_quilt
  disable_quilt = .TRUE.
  RETURN
END SUBROUTINE disable_quilting

LOGICAL FUNCTION  use_output_servers()




  USE module_wrf_quilt
  use_output_servers = quilting_enabled
  RETURN
END FUNCTION use_output_servers

LOGICAL FUNCTION  use_input_servers()




  USE module_wrf_quilt
  use_input_servers = .FALSE.
  RETURN
END FUNCTION use_input_servers

SUBROUTINE wrf_quilt_open_for_write_begin( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                     DataHandle , io_form_arg, Status )





  USE module_wrf_quilt
  USE module_state_description, ONLY: IO_PNETCDF
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER, PARAMETER ::  int_ioexit			=  	     10
  INTEGER, PARAMETER ::  int_open_for_write_begin	=  	     20
  INTEGER, PARAMETER ::  int_open_for_write_commit	=  	     30
  INTEGER, PARAMETER ::  int_open_for_read 		=  	     40
  INTEGER, PARAMETER ::  int_inquire_opened 		=  	     60
  INTEGER, PARAMETER ::  int_inquire_filename 		=  	     70
  INTEGER, PARAMETER ::  int_iosync 			=  	     80
  INTEGER, PARAMETER ::  int_ioclose 			=  	     90
  INTEGER, PARAMETER ::  int_next_time 			=  	    100
  INTEGER, PARAMETER ::  int_set_time 			=  	    110
  INTEGER, PARAMETER ::  int_next_var 			=  	    120
  INTEGER, PARAMETER ::  int_dom_ti_real 		=  	    140
  INTEGER, PARAMETER ::  int_dom_ti_double 		=  	    160
  INTEGER, PARAMETER ::  int_dom_ti_integer 		=  	    180
  INTEGER, PARAMETER ::  int_dom_ti_logical 		=  	    200
  INTEGER, PARAMETER ::  int_dom_ti_char 		=  	    220
  INTEGER, PARAMETER ::  int_dom_td_real 		=  	    240
  INTEGER, PARAMETER ::  int_dom_td_double 		=  	    260
  INTEGER, PARAMETER ::  int_dom_td_integer 		=  	    280
  INTEGER, PARAMETER ::  int_dom_td_logical 		=  	    300
  INTEGER, PARAMETER ::  int_dom_td_char 		=  	    320
  INTEGER, PARAMETER ::  int_var_ti_real 		=  	    340
  INTEGER, PARAMETER ::  int_var_ti_double 		=  	    360
  INTEGER, PARAMETER ::  int_var_ti_integer 		=  	    380
  INTEGER, PARAMETER ::  int_var_ti_logical 		=  	    400
  INTEGER, PARAMETER ::  int_var_ti_char 		=  	    420
  INTEGER, PARAMETER ::  int_var_td_real 		=  	    440
  INTEGER, PARAMETER ::  int_var_td_double 		=  	    460
  INTEGER, PARAMETER ::  int_var_td_integer 		=  	    480
  INTEGER, PARAMETER ::  int_var_td_logical 		=  	    500
  INTEGER, PARAMETER ::  int_var_td_char 		=  	    520
  INTEGER, PARAMETER ::  int_field 			=  	    530
  INTEGER, PARAMETER ::  int_var_info 			=  	    540
  INTEGER, PARAMETER ::  int_noop 			=  	    550
  CHARACTER *(*), INTENT(IN)  :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER *(*), INTENT(IN)  :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(IN)  :: io_form_arg
  INTEGER ,       INTENT(OUT) :: Status

  CHARACTER*132   :: locFileName, locSysDepInfo
  INTEGER i, itypesize, tasks_in_group, ierr, comm_io_group
  REAL dummy
  INTEGER, EXTERNAL :: use_package

  CALL wrf_debug ( 50, 'in wrf_quilt_open_for_write_begin' ) 
  CALL int_get_fresh_handle(i)
  okay_to_write(i) = .false.
  DataHandle = i

  locFileName = FileName
  locSysDepInfo = SysDepInfo

  CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )

  SELECT CASE(use_package(io_form_arg))

  CASE DEFAULT

     IF ( wrf_dm_on_monitor() ) THEN
        CALL int_gen_ofwb_header( hdrbuf, hdrbufsize, itypesize, &
                                  locFileName,locSysDepInfo,io_form_arg,DataHandle )
     ELSE
        CALL int_gen_noop_header( hdrbuf, hdrbufsize, itypesize )
     ENDIF

  END SELECT

  iserver = get_server_id ( DataHandle )

  CALL get_mpi_comm_io_groups( comm_io_group , iserver )


  CALL Mpi_Comm_Size (  comm_io_group , tasks_in_group , ierr  )



  
  reduced = 0
  reduced(1) = hdrbufsize 
  IF ( wrf_dm_on_monitor() )  reduced(2) = i 
  CALL MPI_Reduce( reduced, reduced_dummy, 2, MPI_INTEGER, MPI_SUM, tasks_in_group-1, comm_io_group, ierr )


  
  CALL collect_on_comm_debug("module_io_quilt.F",2999, comm_io_group,            &
                        onebyte,                       &
                        hdrbuf, hdrbufsize , &
                        dummy, 0 )

  Status = 0


  RETURN  
END SUBROUTINE wrf_quilt_open_for_write_begin

SUBROUTINE wrf_quilt_open_for_write_commit( DataHandle , Status )







  USE module_wrf_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER, PARAMETER ::  int_ioexit			=  	     10
  INTEGER, PARAMETER ::  int_open_for_write_begin	=  	     20
  INTEGER, PARAMETER ::  int_open_for_write_commit	=  	     30
  INTEGER, PARAMETER ::  int_open_for_read 		=  	     40
  INTEGER, PARAMETER ::  int_inquire_opened 		=  	     60
  INTEGER, PARAMETER ::  int_inquire_filename 		=  	     70
  INTEGER, PARAMETER ::  int_iosync 			=  	     80
  INTEGER, PARAMETER ::  int_ioclose 			=  	     90
  INTEGER, PARAMETER ::  int_next_time 			=  	    100
  INTEGER, PARAMETER ::  int_set_time 			=  	    110
  INTEGER, PARAMETER ::  int_next_var 			=  	    120
  INTEGER, PARAMETER ::  int_dom_ti_real 		=  	    140
  INTEGER, PARAMETER ::  int_dom_ti_double 		=  	    160
  INTEGER, PARAMETER ::  int_dom_ti_integer 		=  	    180
  INTEGER, PARAMETER ::  int_dom_ti_logical 		=  	    200
  INTEGER, PARAMETER ::  int_dom_ti_char 		=  	    220
  INTEGER, PARAMETER ::  int_dom_td_real 		=  	    240
  INTEGER, PARAMETER ::  int_dom_td_double 		=  	    260
  INTEGER, PARAMETER ::  int_dom_td_integer 		=  	    280
  INTEGER, PARAMETER ::  int_dom_td_logical 		=  	    300
  INTEGER, PARAMETER ::  int_dom_td_char 		=  	    320
  INTEGER, PARAMETER ::  int_var_ti_real 		=  	    340
  INTEGER, PARAMETER ::  int_var_ti_double 		=  	    360
  INTEGER, PARAMETER ::  int_var_ti_integer 		=  	    380
  INTEGER, PARAMETER ::  int_var_ti_logical 		=  	    400
  INTEGER, PARAMETER ::  int_var_ti_char 		=  	    420
  INTEGER, PARAMETER ::  int_var_td_real 		=  	    440
  INTEGER, PARAMETER ::  int_var_td_double 		=  	    460
  INTEGER, PARAMETER ::  int_var_td_integer 		=  	    480
  INTEGER, PARAMETER ::  int_var_td_logical 		=  	    500
  INTEGER, PARAMETER ::  int_var_td_char 		=  	    520
  INTEGER, PARAMETER ::  int_field 			=  	    530
  INTEGER, PARAMETER ::  int_var_info 			=  	    540
  INTEGER, PARAMETER ::  int_noop 			=  	    550
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER i, itypesize, tasks_in_group, ierr, comm_io_group
  REAL dummy

  CALL wrf_debug ( 50, 'in wrf_quilt_open_for_write_commit' ) 
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      okay_to_write( DataHandle ) = .true.
    ENDIF
  ENDIF

  CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )


  IF ( wrf_dm_on_monitor() ) THEN
     CALL int_gen_handle_header( hdrbuf, hdrbufsize, itypesize, &
                                 DataHandle, int_open_for_write_commit )
  ELSE
     CALL int_gen_noop_header( hdrbuf, hdrbufsize, itypesize )
  ENDIF

  iserver = get_server_id ( DataHandle )
  CALL get_mpi_comm_io_groups( comm_io_group , iserver )

  CALL Mpi_Comm_Size (  comm_io_group , tasks_in_group , ierr  )


  
  reduced = 0
  reduced(1) = hdrbufsize 
  IF ( wrf_dm_on_monitor() )  reduced(2) = DataHandle
  CALL MPI_Reduce( reduced, reduced_dummy, 2, MPI_INTEGER, MPI_SUM, tasks_in_group-1, comm_io_group, ierr )


  
  CALL collect_on_comm_debug("module_io_quilt.F",3075, comm_io_group,            &
                        onebyte,                       &
                        hdrbuf, hdrbufsize , &
                        dummy, 0 )

  Status = 0

  RETURN  
END SUBROUTINE wrf_quilt_open_for_write_commit

SUBROUTINE wrf_quilt_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               DataHandle , Status )





  IMPLICIT NONE
  CHARACTER *(*), INTENT(IN)  :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER *(*), INTENT(IN)  :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_debug ( 50, 'in wrf_quilt_open_for_read' ) 
  DataHandle = -1
  Status = -1
  CALL wrf_error_fatal3("<stdin>",2510,&
"frame/module_io_quilt.F: wrf_quilt_open_for_read not yet supported" )
  RETURN  
END SUBROUTINE wrf_quilt_open_for_read

SUBROUTINE wrf_quilt_inquire_opened ( DataHandle, FileName , FileStatus, Status )





  USE module_wrf_quilt
  IMPLICIT NONE
      integer, parameter  :: WRF_FILE_NOT_OPENED                  = 100
      integer, parameter  :: WRF_FILE_OPENED_NOT_COMMITTED        = 101
      integer, parameter  :: WRF_FILE_OPENED_FOR_WRITE            = 102
      integer, parameter  :: WRF_FILE_OPENED_FOR_READ             = 103
      integer, parameter  :: WRF_REAL                             = 104
      integer, parameter  :: WRF_DOUBLE                           = 105
      integer, parameter  :: WRF_FLOAT=WRF_REAL
      integer, parameter  :: WRF_INTEGER                          = 106
      integer, parameter  :: WRF_LOGICAL                          = 107
      integer, parameter  :: WRF_COMPLEX                          = 108
      integer, parameter  :: WRF_DOUBLE_COMPLEX                   = 109
      integer, parameter  :: WRF_FILE_OPENED_FOR_UPDATE           = 110


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER *(*), INTENT(IN)  :: FileName
  INTEGER ,       INTENT(OUT) :: FileStatus
  INTEGER ,       INTENT(OUT) :: Status

  Status = 0

  CALL wrf_debug ( 50, 'in wrf_quilt_inquire_opened' ) 
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      IF ( okay_to_write( DataHandle ) ) THEN
        FileStatus = WRF_FILE_OPENED_FOR_WRITE
      ENDIF
    ENDIF
  ENDIF
  Status = 0
  
  RETURN
END SUBROUTINE wrf_quilt_inquire_opened

SUBROUTINE wrf_quilt_inquire_filename ( DataHandle, FileName , FileStatus, Status )










  USE module_wrf_quilt
  IMPLICIT NONE
      integer, parameter  :: WRF_FILE_NOT_OPENED                  = 100
      integer, parameter  :: WRF_FILE_OPENED_NOT_COMMITTED        = 101
      integer, parameter  :: WRF_FILE_OPENED_FOR_WRITE            = 102
      integer, parameter  :: WRF_FILE_OPENED_FOR_READ             = 103
      integer, parameter  :: WRF_REAL                             = 104
      integer, parameter  :: WRF_DOUBLE                           = 105
      integer, parameter  :: WRF_FLOAT=WRF_REAL
      integer, parameter  :: WRF_INTEGER                          = 106
      integer, parameter  :: WRF_LOGICAL                          = 107
      integer, parameter  :: WRF_COMPLEX                          = 108
      integer, parameter  :: WRF_DOUBLE_COMPLEX                   = 109
      integer, parameter  :: WRF_FILE_OPENED_FOR_UPDATE           = 110


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER *(*), INTENT(OUT) :: FileName
  INTEGER ,       INTENT(OUT) :: FileStatus
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug ( 50, 'in wrf_quilt_inquire_filename' ) 
  Status = 0
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      IF ( okay_to_write( DataHandle ) ) THEN
        FileStatus = WRF_FILE_OPENED_FOR_WRITE
      ELSE
        FileStatus = WRF_FILE_OPENED_NOT_COMMITTED
      ENDIF
    ELSE
        FileStatus = WRF_FILE_NOT_OPENED
    ENDIF
    Status = 0
    FileName = "bogusfornow"
  ELSE
    Status = -1
  ENDIF
  RETURN
END SUBROUTINE wrf_quilt_inquire_filename

SUBROUTINE wrf_quilt_iosync ( DataHandle, Status )



















  USE module_wrf_quilt
  IMPLICIT NONE
  include "mpif.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  INTEGER locsize , itypesize
  INTEGER ierr, tasks_in_group, comm_io_group, dummy, i

  CALL wrf_debug ( 50, 'in wrf_quilt_iosync' ) 


  IF ( associated ( int_local_output_buffer ) ) THEN

    iserver = get_server_id ( DataHandle )
    CALL get_mpi_comm_io_groups( comm_io_group , iserver )

    CALL Mpi_Comm_Size (  comm_io_group , tasks_in_group , ierr  )

    locsize = int_num_bytes_to_write(DataHandle)


    
    reduced = 0
    reduced(1) = locsize 
    IF ( wrf_dm_on_monitor() )  reduced(2) = DataHandle
    CALL MPI_Reduce( reduced, reduced_dummy, 2, MPI_INTEGER, MPI_SUM, tasks_in_group-1, comm_io_group, ierr )


    
    CALL collect_on_comm_debug("module_io_quilt.F",3242, comm_io_group,            &
                          onebyte,                       &
                          int_local_output_buffer, locsize , &
                          dummy, 0 )


    int_local_output_cursor = 1

    DEALLOCATE ( int_local_output_buffer )
    NULLIFY ( int_local_output_buffer )
  ELSE
    CALL wrf_message ("frame/module_io_quilt.F: wrf_quilt_iosync: no buffer allocated")
  ENDIF

  Status = 0
  RETURN
END SUBROUTINE wrf_quilt_iosync

SUBROUTINE wrf_quilt_ioclose ( DataHandle, Status )







  USE module_wrf_quilt
  USE module_timing
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER, PARAMETER ::  int_ioexit			=  	     10
  INTEGER, PARAMETER ::  int_open_for_write_begin	=  	     20
  INTEGER, PARAMETER ::  int_open_for_write_commit	=  	     30
  INTEGER, PARAMETER ::  int_open_for_read 		=  	     40
  INTEGER, PARAMETER ::  int_inquire_opened 		=  	     60
  INTEGER, PARAMETER ::  int_inquire_filename 		=  	     70
  INTEGER, PARAMETER ::  int_iosync 			=  	     80
  INTEGER, PARAMETER ::  int_ioclose 			=  	     90
  INTEGER, PARAMETER ::  int_next_time 			=  	    100
  INTEGER, PARAMETER ::  int_set_time 			=  	    110
  INTEGER, PARAMETER ::  int_next_var 			=  	    120
  INTEGER, PARAMETER ::  int_dom_ti_real 		=  	    140
  INTEGER, PARAMETER ::  int_dom_ti_double 		=  	    160
  INTEGER, PARAMETER ::  int_dom_ti_integer 		=  	    180
  INTEGER, PARAMETER ::  int_dom_ti_logical 		=  	    200
  INTEGER, PARAMETER ::  int_dom_ti_char 		=  	    220
  INTEGER, PARAMETER ::  int_dom_td_real 		=  	    240
  INTEGER, PARAMETER ::  int_dom_td_double 		=  	    260
  INTEGER, PARAMETER ::  int_dom_td_integer 		=  	    280
  INTEGER, PARAMETER ::  int_dom_td_logical 		=  	    300
  INTEGER, PARAMETER ::  int_dom_td_char 		=  	    320
  INTEGER, PARAMETER ::  int_var_ti_real 		=  	    340
  INTEGER, PARAMETER ::  int_var_ti_double 		=  	    360
  INTEGER, PARAMETER ::  int_var_ti_integer 		=  	    380
  INTEGER, PARAMETER ::  int_var_ti_logical 		=  	    400
  INTEGER, PARAMETER ::  int_var_ti_char 		=  	    420
  INTEGER, PARAMETER ::  int_var_td_real 		=  	    440
  INTEGER, PARAMETER ::  int_var_td_double 		=  	    460
  INTEGER, PARAMETER ::  int_var_td_integer 		=  	    480
  INTEGER, PARAMETER ::  int_var_td_logical 		=  	    500
  INTEGER, PARAMETER ::  int_var_td_char 		=  	    520
  INTEGER, PARAMETER ::  int_field 			=  	    530
  INTEGER, PARAMETER ::  int_var_info 			=  	    540
  INTEGER, PARAMETER ::  int_noop 			=  	    550
  INTEGER ,       INTENT(IN)  :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER i, itypesize, tasks_in_group, comm_io_group, ierr
  REAL dummy


  CALL wrf_debug ( 50, 'in wrf_quilt_ioclose' ) 
  CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )




  IF ( wrf_dm_on_monitor() ) THEN
     CALL int_gen_handle_header( hdrbuf, hdrbufsize, itypesize, &
                                 DataHandle , int_ioclose )
  ELSE
     CALL int_gen_noop_header( hdrbuf, hdrbufsize, itypesize )
  ENDIF

  iserver = get_server_id ( DataHandle )
  CALL get_mpi_comm_io_groups( comm_io_group , iserver )

  CALL Mpi_Comm_Size (  comm_io_group , tasks_in_group , ierr  )


  
  reduced = 0
  IF ( wrf_dm_on_monitor() )  reduced(2) = DataHandle
  CALL MPI_Reduce( reduced, reduced_dummy, 2, MPI_INTEGER, MPI_SUM, tasks_in_group-1, comm_io_group, ierr )
!!JMTIMING   CALL end_timing("MPI_Reduce in ioclose")


  int_handle_in_use(DataHandle) = .false.
  CALL set_server_id( DataHandle, 0 ) 
  okay_to_write(DataHandle) = .false.
  okay_to_commit(DataHandle) = .false.
  int_local_output_cursor = 1
  int_num_bytes_to_write(DataHandle) = 0
  IF ( associated ( int_local_output_buffer ) ) THEN
    DEALLOCATE ( int_local_output_buffer )
    NULLIFY ( int_local_output_buffer )
  ENDIF

  Status = 0
!!JMTIMING   CALL end_timing( "wrf_quilt_ioclose" )

  RETURN
END SUBROUTINE wrf_quilt_ioclose

SUBROUTINE wrf_quilt_ioexit( Status )





  USE module_wrf_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER, PARAMETER ::  int_ioexit			=  	     10
  INTEGER, PARAMETER ::  int_open_for_write_begin	=  	     20
  INTEGER, PARAMETER ::  int_open_for_write_commit	=  	     30
  INTEGER, PARAMETER ::  int_open_for_read 		=  	     40
  INTEGER, PARAMETER ::  int_inquire_opened 		=  	     60
  INTEGER, PARAMETER ::  int_inquire_filename 		=  	     70
  INTEGER, PARAMETER ::  int_iosync 			=  	     80
  INTEGER, PARAMETER ::  int_ioclose 			=  	     90
  INTEGER, PARAMETER ::  int_next_time 			=  	    100
  INTEGER, PARAMETER ::  int_set_time 			=  	    110
  INTEGER, PARAMETER ::  int_next_var 			=  	    120
  INTEGER, PARAMETER ::  int_dom_ti_real 		=  	    140
  INTEGER, PARAMETER ::  int_dom_ti_double 		=  	    160
  INTEGER, PARAMETER ::  int_dom_ti_integer 		=  	    180
  INTEGER, PARAMETER ::  int_dom_ti_logical 		=  	    200
  INTEGER, PARAMETER ::  int_dom_ti_char 		=  	    220
  INTEGER, PARAMETER ::  int_dom_td_real 		=  	    240
  INTEGER, PARAMETER ::  int_dom_td_double 		=  	    260
  INTEGER, PARAMETER ::  int_dom_td_integer 		=  	    280
  INTEGER, PARAMETER ::  int_dom_td_logical 		=  	    300
  INTEGER, PARAMETER ::  int_dom_td_char 		=  	    320
  INTEGER, PARAMETER ::  int_var_ti_real 		=  	    340
  INTEGER, PARAMETER ::  int_var_ti_double 		=  	    360
  INTEGER, PARAMETER ::  int_var_ti_integer 		=  	    380
  INTEGER, PARAMETER ::  int_var_ti_logical 		=  	    400
  INTEGER, PARAMETER ::  int_var_ti_char 		=  	    420
  INTEGER, PARAMETER ::  int_var_td_real 		=  	    440
  INTEGER, PARAMETER ::  int_var_td_double 		=  	    460
  INTEGER, PARAMETER ::  int_var_td_integer 		=  	    480
  INTEGER, PARAMETER ::  int_var_td_logical 		=  	    500
  INTEGER, PARAMETER ::  int_var_td_char 		=  	    520
  INTEGER, PARAMETER ::  int_field 			=  	    530
  INTEGER, PARAMETER ::  int_var_info 			=  	    540
  INTEGER, PARAMETER ::  int_noop 			=  	    550
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER                     :: DataHandle, actual_iserver
  INTEGER i, itypesize, tasks_in_group, comm_io_group, me, ierr 
  REAL dummy

  CALL wrf_debug ( 50, 'in wrf_quilt_ioexit' ) 
  CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )




  IF ( wrf_dm_on_monitor() ) THEN
     CALL int_gen_handle_header( hdrbuf, hdrbufsize, itypesize, &
                                 DataHandle , int_ioexit )  
  ELSE
     CALL int_gen_noop_header( hdrbuf, hdrbufsize, itypesize )
  ENDIF

  DO iserver = 1, nio_groups
    if(poll_servers) then
       
       
       

       call wrf_quilt_find_server(actual_iserver)

       
       
       
    else
       
       actual_iserver=iserver
    endif

    CALL get_mpi_comm_io_groups( comm_io_group , actual_iserver )

    CALL Mpi_Comm_Size (  comm_io_group , tasks_in_group , ierr  )
    CALL mpi_comm_rank( comm_io_group , me , ierr )


    hdrbufsize = -100 
    reduced = 0
    IF ( me .eq. 0 ) reduced(1) = hdrbufsize 
    CALL MPI_Reduce( reduced, reduced_dummy, 2, MPI_INTEGER, MPI_SUM, tasks_in_group-1, comm_io_group, ierr )

  ENDDO
  Status = 0

  RETURN  
END SUBROUTINE wrf_quilt_ioexit

SUBROUTINE wrf_quilt_get_next_time ( DataHandle, DateStr, Status )





  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*)               :: DateStr
  INTEGER                     :: Status
  RETURN
END SUBROUTINE wrf_quilt_get_next_time

SUBROUTINE wrf_quilt_get_previous_time ( DataHandle, DateStr, Status )





  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*)               :: DateStr
  INTEGER                     :: Status
  RETURN
END SUBROUTINE wrf_quilt_get_previous_time

SUBROUTINE wrf_quilt_set_time ( DataHandle, Data,  Status )





  USE module_wrf_quilt
  USE module_state_description, ONLY: IO_PNETCDF
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER, PARAMETER ::  int_ioexit			=  	     10
  INTEGER, PARAMETER ::  int_open_for_write_begin	=  	     20
  INTEGER, PARAMETER ::  int_open_for_write_commit	=  	     30
  INTEGER, PARAMETER ::  int_open_for_read 		=  	     40
  INTEGER, PARAMETER ::  int_inquire_opened 		=  	     60
  INTEGER, PARAMETER ::  int_inquire_filename 		=  	     70
  INTEGER, PARAMETER ::  int_iosync 			=  	     80
  INTEGER, PARAMETER ::  int_ioclose 			=  	     90
  INTEGER, PARAMETER ::  int_next_time 			=  	    100
  INTEGER, PARAMETER ::  int_set_time 			=  	    110
  INTEGER, PARAMETER ::  int_next_var 			=  	    120
  INTEGER, PARAMETER ::  int_dom_ti_real 		=  	    140
  INTEGER, PARAMETER ::  int_dom_ti_double 		=  	    160
  INTEGER, PARAMETER ::  int_dom_ti_integer 		=  	    180
  INTEGER, PARAMETER ::  int_dom_ti_logical 		=  	    200
  INTEGER, PARAMETER ::  int_dom_ti_char 		=  	    220
  INTEGER, PARAMETER ::  int_dom_td_real 		=  	    240
  INTEGER, PARAMETER ::  int_dom_td_double 		=  	    260
  INTEGER, PARAMETER ::  int_dom_td_integer 		=  	    280
  INTEGER, PARAMETER ::  int_dom_td_logical 		=  	    300
  INTEGER, PARAMETER ::  int_dom_td_char 		=  	    320
  INTEGER, PARAMETER ::  int_var_ti_real 		=  	    340
  INTEGER, PARAMETER ::  int_var_ti_double 		=  	    360
  INTEGER, PARAMETER ::  int_var_ti_integer 		=  	    380
  INTEGER, PARAMETER ::  int_var_ti_logical 		=  	    400
  INTEGER, PARAMETER ::  int_var_ti_char 		=  	    420
  INTEGER, PARAMETER ::  int_var_td_real 		=  	    440
  INTEGER, PARAMETER ::  int_var_td_double 		=  	    460
  INTEGER, PARAMETER ::  int_var_td_integer 		=  	    480
  INTEGER, PARAMETER ::  int_var_td_logical 		=  	    500
  INTEGER, PARAMETER ::  int_var_td_char 		=  	    520
  INTEGER, PARAMETER ::  int_field 			=  	    530
  INTEGER, PARAMETER ::  int_var_info 			=  	    540
  INTEGER, PARAMETER ::  int_noop 			=  	    550
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Data
  INTEGER                     :: Status
  INTEGER i, itypesize, tasks_in_group, ierr, comm_io_group
  REAL dummy
  INTEGER                 :: Count
  INTEGER, EXTERNAL       :: use_package

  CALL wrf_debug ( 50, 'in wrf_quilt_set_time' )

  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )
      Count = 0   



      IF ( wrf_dm_on_monitor() ) THEN
         CALL int_gen_ti_header_char( hdrbuf, hdrbufsize, itypesize, &
                                      DataHandle, "TIMESTAMP", "", Data, int_set_time )
      ELSE
         CALL int_gen_noop_header( hdrbuf, hdrbufsize, itypesize )
      ENDIF

      iserver = get_server_id ( DataHandle )
      CALL get_mpi_comm_io_groups( comm_io_group , iserver )
      CALL Mpi_Comm_Size (  comm_io_group , tasks_in_group , ierr  )

      
      reduced = 0
      reduced(1) = hdrbufsize 
      IF ( wrf_dm_on_monitor() )  reduced(2) = DataHandle
      CALL MPI_Reduce( reduced, reduced_dummy, 2, MPI_INTEGER, MPI_SUM, tasks_in_group-1, comm_io_group, ierr )
      
      CALL collect_on_comm_debug("module_io_quilt.F",3515, comm_io_group,            &
                            onebyte,                       &
                            hdrbuf, hdrbufsize , &
                            dummy, 0 )
    ENDIF
  ENDIF

RETURN
END SUBROUTINE wrf_quilt_set_time

SUBROUTINE wrf_quilt_get_next_var ( DataHandle, VarName, Status )






  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*)               :: VarName
  INTEGER                     :: Status
  RETURN
END SUBROUTINE wrf_quilt_get_next_var

SUBROUTINE wrf_quilt_get_dom_ti_real ( DataHandle,Element,   Data, Count, Outcount, Status )











  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  REAL,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Outcount
  INTEGER                     :: Status
  CALL wrf_message('wrf_quilt_get_dom_ti_real not supported yet')
RETURN
END SUBROUTINE wrf_quilt_get_dom_ti_real 

SUBROUTINE wrf_quilt_put_dom_ti_real ( DataHandle,Element,   Data, Count,  Status )








  USE module_wrf_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER, PARAMETER ::  int_ioexit			=  	     10
  INTEGER, PARAMETER ::  int_open_for_write_begin	=  	     20
  INTEGER, PARAMETER ::  int_open_for_write_commit	=  	     30
  INTEGER, PARAMETER ::  int_open_for_read 		=  	     40
  INTEGER, PARAMETER ::  int_inquire_opened 		=  	     60
  INTEGER, PARAMETER ::  int_inquire_filename 		=  	     70
  INTEGER, PARAMETER ::  int_iosync 			=  	     80
  INTEGER, PARAMETER ::  int_ioclose 			=  	     90
  INTEGER, PARAMETER ::  int_next_time 			=  	    100
  INTEGER, PARAMETER ::  int_set_time 			=  	    110
  INTEGER, PARAMETER ::  int_next_var 			=  	    120
  INTEGER, PARAMETER ::  int_dom_ti_real 		=  	    140
  INTEGER, PARAMETER ::  int_dom_ti_double 		=  	    160
  INTEGER, PARAMETER ::  int_dom_ti_integer 		=  	    180
  INTEGER, PARAMETER ::  int_dom_ti_logical 		=  	    200
  INTEGER, PARAMETER ::  int_dom_ti_char 		=  	    220
  INTEGER, PARAMETER ::  int_dom_td_real 		=  	    240
  INTEGER, PARAMETER ::  int_dom_td_double 		=  	    260
  INTEGER, PARAMETER ::  int_dom_td_integer 		=  	    280
  INTEGER, PARAMETER ::  int_dom_td_logical 		=  	    300
  INTEGER, PARAMETER ::  int_dom_td_char 		=  	    320
  INTEGER, PARAMETER ::  int_var_ti_real 		=  	    340
  INTEGER, PARAMETER ::  int_var_ti_double 		=  	    360
  INTEGER, PARAMETER ::  int_var_ti_integer 		=  	    380
  INTEGER, PARAMETER ::  int_var_ti_logical 		=  	    400
  INTEGER, PARAMETER ::  int_var_ti_char 		=  	    420
  INTEGER, PARAMETER ::  int_var_td_real 		=  	    440
  INTEGER, PARAMETER ::  int_var_td_double 		=  	    460
  INTEGER, PARAMETER ::  int_var_td_integer 		=  	    480
  INTEGER, PARAMETER ::  int_var_td_logical 		=  	    500
  INTEGER, PARAMETER ::  int_var_td_char 		=  	    520
  INTEGER, PARAMETER ::  int_field 			=  	    530
  INTEGER, PARAMETER ::  int_var_info 			=  	    540
  INTEGER, PARAMETER ::  int_noop 			=  	    550
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  REAL ,          INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status

  CHARACTER*132   :: locElement
  INTEGER i, typesize, itypesize, tasks_in_group, ierr, comm_io_group
  REAL dummy


  CALL wrf_debug ( 50, 'in wrf_quilt_put_dom_ti_real' ) 
  CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )
  locElement = Element

  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )
      CALL MPI_TYPE_SIZE( MPI_REAL, typesize, ierr )

      IF ( wrf_dm_on_monitor() ) THEN
         CALL int_gen_ti_header( hdrbuf, hdrbufsize, itypesize, typesize, &
                                 DataHandle, locElement, Data, Count, int_dom_ti_real )
      ELSE
         CALL int_gen_noop_header( hdrbuf, hdrbufsize, itypesize )
      ENDIF

      iserver = get_server_id ( DataHandle )
      CALL get_mpi_comm_io_groups( comm_io_group , iserver )
      CALL Mpi_Comm_Size (  comm_io_group , tasks_in_group , ierr  )


      
      reduced = 0
      reduced(1) = hdrbufsize 
      IF ( wrf_dm_on_monitor() )  reduced(2) = DataHandle
      CALL MPI_Reduce( reduced, reduced_dummy, 2, MPI_INTEGER, MPI_SUM, tasks_in_group-1, comm_io_group, ierr )

      
      CALL collect_on_comm_debug("module_io_quilt.F",3633, comm_io_group,            &
                            onebyte,                       &
                            hdrbuf, hdrbufsize , &
                            dummy, 0 )
    ENDIF
  ENDIF

  Status = 0

RETURN
END SUBROUTINE wrf_quilt_put_dom_ti_real 

SUBROUTINE wrf_quilt_get_dom_ti_double ( DataHandle,Element,   Data, Count, Outcount, Status )











  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  real*8                      :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: OutCount
  INTEGER                     :: Status
  CALL wrf_error_fatal3("<stdin>",3134,&
'wrf_quilt_get_dom_ti_double not supported yet')
RETURN
END SUBROUTINE wrf_quilt_get_dom_ti_double 

SUBROUTINE wrf_quilt_put_dom_ti_double ( DataHandle,Element,   Data, Count,  Status )










  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  REAL*8 ,        INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
  CALL wrf_error_fatal3("<stdin>",3156,&
'wrf_quilt_put_dom_ti_double not supported yet')
RETURN
END SUBROUTINE wrf_quilt_put_dom_ti_double 

SUBROUTINE wrf_quilt_get_dom_ti_integer ( DataHandle,Element,   Data, Count, Outcount, Status )











  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  integer                     :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                      :: OutCount
  INTEGER                     :: Status
  CALL wrf_message('wrf_quilt_get_dom_ti_integer not supported yet')
RETURN
END SUBROUTINE wrf_quilt_get_dom_ti_integer 

SUBROUTINE wrf_quilt_put_dom_ti_integer ( DataHandle,Element,   Data, Count,  Status )








  USE module_wrf_quilt
  USE module_state_description, ONLY: IO_PNETCDF
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER, PARAMETER ::  int_ioexit			=  	     10
  INTEGER, PARAMETER ::  int_open_for_write_begin	=  	     20
  INTEGER, PARAMETER ::  int_open_for_write_commit	=  	     30
  INTEGER, PARAMETER ::  int_open_for_read 		=  	     40
  INTEGER, PARAMETER ::  int_inquire_opened 		=  	     60
  INTEGER, PARAMETER ::  int_inquire_filename 		=  	     70
  INTEGER, PARAMETER ::  int_iosync 			=  	     80
  INTEGER, PARAMETER ::  int_ioclose 			=  	     90
  INTEGER, PARAMETER ::  int_next_time 			=  	    100
  INTEGER, PARAMETER ::  int_set_time 			=  	    110
  INTEGER, PARAMETER ::  int_next_var 			=  	    120
  INTEGER, PARAMETER ::  int_dom_ti_real 		=  	    140
  INTEGER, PARAMETER ::  int_dom_ti_double 		=  	    160
  INTEGER, PARAMETER ::  int_dom_ti_integer 		=  	    180
  INTEGER, PARAMETER ::  int_dom_ti_logical 		=  	    200
  INTEGER, PARAMETER ::  int_dom_ti_char 		=  	    220
  INTEGER, PARAMETER ::  int_dom_td_real 		=  	    240
  INTEGER, PARAMETER ::  int_dom_td_double 		=  	    260
  INTEGER, PARAMETER ::  int_dom_td_integer 		=  	    280
  INTEGER, PARAMETER ::  int_dom_td_logical 		=  	    300
  INTEGER, PARAMETER ::  int_dom_td_char 		=  	    320
  INTEGER, PARAMETER ::  int_var_ti_real 		=  	    340
  INTEGER, PARAMETER ::  int_var_ti_double 		=  	    360
  INTEGER, PARAMETER ::  int_var_ti_integer 		=  	    380
  INTEGER, PARAMETER ::  int_var_ti_logical 		=  	    400
  INTEGER, PARAMETER ::  int_var_ti_char 		=  	    420
  INTEGER, PARAMETER ::  int_var_td_real 		=  	    440
  INTEGER, PARAMETER ::  int_var_td_double 		=  	    460
  INTEGER, PARAMETER ::  int_var_td_integer 		=  	    480
  INTEGER, PARAMETER ::  int_var_td_logical 		=  	    500
  INTEGER, PARAMETER ::  int_var_td_char 		=  	    520
  INTEGER, PARAMETER ::  int_field 			=  	    530
  INTEGER, PARAMETER ::  int_var_info 			=  	    540
  INTEGER, PARAMETER ::  int_noop 			=  	    550
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  INTEGER ,       INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status

  CHARACTER*132   :: locElement
  INTEGER i, typesize, itypesize, tasks_in_group, ierr, comm_io_group
  REAL dummy
  INTEGER, EXTERNAL :: use_package



  locElement = Element

  CALL wrf_debug ( 50, 'in wrf_quilt_put_dom_ti_integer' ) 

  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )
      CALL MPI_TYPE_SIZE( MPI_INTEGER, typesize, ierr )



      IF ( wrf_dm_on_monitor() ) THEN
         CALL int_gen_ti_header( hdrbuf, hdrbufsize, itypesize, typesize, &
                                 DataHandle, locElement, Data, Count,     &
                                 int_dom_ti_integer )
      ELSE
         CALL int_gen_noop_header( hdrbuf, hdrbufsize, itypesize )
      ENDIF

      iserver = get_server_id ( DataHandle )
      CALL get_mpi_comm_io_groups( comm_io_group , iserver )
      CALL Mpi_Comm_Size (  comm_io_group , tasks_in_group , ierr  )


      
      reduced = 0
      reduced(1) = hdrbufsize 
      IF ( wrf_dm_on_monitor() )  reduced(2) = DataHandle
      CALL MPI_Reduce( reduced, reduced_dummy, 2, MPI_INTEGER, MPI_SUM, tasks_in_group-1, comm_io_group, ierr )


      
      CALL collect_on_comm_debug("module_io_quilt.F",3793, comm_io_group,            &
                            onebyte,                       &
                            hdrbuf, hdrbufsize , &
                            dummy, 0 )
    ENDIF
  ENDIF
  CALL wrf_debug ( 50, 'returning from wrf_quilt_put_dom_ti_integer' ) 


RETURN
END SUBROUTINE wrf_quilt_put_dom_ti_integer 

SUBROUTINE wrf_quilt_get_dom_ti_logical ( DataHandle,Element,   Data, Count, Outcount, Status )











  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  logical                     :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                      :: OutCount
  INTEGER                     :: Status

RETURN
END SUBROUTINE wrf_quilt_get_dom_ti_logical 

SUBROUTINE wrf_quilt_put_dom_ti_logical ( DataHandle,Element,   Data, Count,  Status )










  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status

  INTEGER i
  INTEGER one_or_zero(Count)

  DO i = 1, Count
    IF ( Data(i) ) THEN
      one_or_zero(i) = 1
    ELSE
      one_or_zero(i) = 0
    ENDIF
  ENDDO

  CALL wrf_quilt_put_dom_ti_integer ( DataHandle,Element,   one_or_zero, Count,  Status )
RETURN
END SUBROUTINE wrf_quilt_put_dom_ti_logical 

SUBROUTINE wrf_quilt_get_dom_ti_char ( DataHandle,Element,   Data,  Status )










  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*)               :: Data
  INTEGER                     :: Status
  CALL wrf_message('wrf_quilt_get_dom_ti_char not supported yet')
RETURN
END SUBROUTINE wrf_quilt_get_dom_ti_char 

SUBROUTINE wrf_quilt_put_dom_ti_char ( DataHandle, Element,  Data,  Status )








  USE module_wrf_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER, PARAMETER ::  int_ioexit			=  	     10
  INTEGER, PARAMETER ::  int_open_for_write_begin	=  	     20
  INTEGER, PARAMETER ::  int_open_for_write_commit	=  	     30
  INTEGER, PARAMETER ::  int_open_for_read 		=  	     40
  INTEGER, PARAMETER ::  int_inquire_opened 		=  	     60
  INTEGER, PARAMETER ::  int_inquire_filename 		=  	     70
  INTEGER, PARAMETER ::  int_iosync 			=  	     80
  INTEGER, PARAMETER ::  int_ioclose 			=  	     90
  INTEGER, PARAMETER ::  int_next_time 			=  	    100
  INTEGER, PARAMETER ::  int_set_time 			=  	    110
  INTEGER, PARAMETER ::  int_next_var 			=  	    120
  INTEGER, PARAMETER ::  int_dom_ti_real 		=  	    140
  INTEGER, PARAMETER ::  int_dom_ti_double 		=  	    160
  INTEGER, PARAMETER ::  int_dom_ti_integer 		=  	    180
  INTEGER, PARAMETER ::  int_dom_ti_logical 		=  	    200
  INTEGER, PARAMETER ::  int_dom_ti_char 		=  	    220
  INTEGER, PARAMETER ::  int_dom_td_real 		=  	    240
  INTEGER, PARAMETER ::  int_dom_td_double 		=  	    260
  INTEGER, PARAMETER ::  int_dom_td_integer 		=  	    280
  INTEGER, PARAMETER ::  int_dom_td_logical 		=  	    300
  INTEGER, PARAMETER ::  int_dom_td_char 		=  	    320
  INTEGER, PARAMETER ::  int_var_ti_real 		=  	    340
  INTEGER, PARAMETER ::  int_var_ti_double 		=  	    360
  INTEGER, PARAMETER ::  int_var_ti_integer 		=  	    380
  INTEGER, PARAMETER ::  int_var_ti_logical 		=  	    400
  INTEGER, PARAMETER ::  int_var_ti_char 		=  	    420
  INTEGER, PARAMETER ::  int_var_td_real 		=  	    440
  INTEGER, PARAMETER ::  int_var_td_double 		=  	    460
  INTEGER, PARAMETER ::  int_var_td_integer 		=  	    480
  INTEGER, PARAMETER ::  int_var_td_logical 		=  	    500
  INTEGER, PARAMETER ::  int_var_td_char 		=  	    520
  INTEGER, PARAMETER ::  int_field 			=  	    530
  INTEGER, PARAMETER ::  int_var_info 			=  	    540
  INTEGER, PARAMETER ::  int_noop 			=  	    550
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: Data
  INTEGER                     :: Status
  INTEGER i, itypesize, tasks_in_group, ierr, comm_io_group, me
  REAL dummy


  CALL wrf_debug ( 50, 'in wrf_quilt_put_dom_ti_char' ) 

  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )



      IF ( wrf_dm_on_monitor() ) THEN
         CALL int_gen_ti_header_char( hdrbuf, hdrbufsize, itypesize, &
                                      DataHandle, Element, "", Data, int_dom_ti_char )
      ELSE
         CALL int_gen_noop_header( hdrbuf, hdrbufsize, itypesize )
      ENDIF

      iserver = get_server_id ( DataHandle )

      CALL get_mpi_comm_io_groups( comm_io_group , iserver )
      CALL Mpi_Comm_Size (  comm_io_group , tasks_in_group , ierr  )
      







      
      reduced_dummy = 0 
      reduced = 0
      reduced(1) = hdrbufsize 
      IF ( wrf_dm_on_monitor() )  reduced(2) = DataHandle


      CALL MPI_Reduce( reduced, reduced_dummy, 2, MPI_INTEGER, MPI_SUM, tasks_in_group-1, comm_io_group, ierr )


      


      CALL collect_on_comm_debug("module_io_quilt.F",3964, comm_io_group,            &
                            onebyte,                       &
                            hdrbuf, hdrbufsize , &
                            dummy, 0 )

    ENDIF
  ENDIF


RETURN
END SUBROUTINE wrf_quilt_put_dom_ti_char 

SUBROUTINE wrf_quilt_get_dom_td_real ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )











  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  real                        :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: OutCount
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_get_dom_td_real 

SUBROUTINE wrf_quilt_put_dom_td_real ( DataHandle,Element, DateStr,  Data, Count,  Status )










  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_put_dom_td_real 

SUBROUTINE wrf_quilt_get_dom_td_double ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )











  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  real*8                          :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                      :: OutCount
  INTEGER                     :: Status
  CALL wrf_error_fatal3("<stdin>",3534,&
'wrf_quilt_get_dom_td_double not supported yet')
RETURN
END SUBROUTINE wrf_quilt_get_dom_td_double 

SUBROUTINE wrf_quilt_put_dom_td_double ( DataHandle,Element, DateStr,  Data, Count,  Status )










  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
  CALL wrf_error_fatal3("<stdin>",3557,&
'wrf_quilt_put_dom_td_double not supported yet')
RETURN
END SUBROUTINE wrf_quilt_put_dom_td_double 

SUBROUTINE wrf_quilt_get_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )











  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  integer                          :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                      :: OutCount
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_get_dom_td_integer 

SUBROUTINE wrf_quilt_put_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count,  Status )










  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  integer ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_put_dom_td_integer 

SUBROUTINE wrf_quilt_get_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )











  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  logical                          :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                      :: OutCount
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_get_dom_td_logical 

SUBROUTINE wrf_quilt_put_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count,  Status )










  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_put_dom_td_logical 

SUBROUTINE wrf_quilt_get_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )










  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*)               :: Data
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_get_dom_td_char 

SUBROUTINE wrf_quilt_put_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )










  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN) :: Data
  INTEGER                          :: Status
RETURN
END SUBROUTINE wrf_quilt_put_dom_td_char 

SUBROUTINE wrf_quilt_get_var_ti_real ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )











  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real                          :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: OutCount
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_get_var_ti_real 

SUBROUTINE wrf_quilt_put_var_ti_real ( DataHandle,Element,  Varname, Data, Count,  Status )










  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_put_var_ti_real 

SUBROUTINE wrf_quilt_get_var_ti_double ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )











  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real*8                      :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: OutCount
  INTEGER                     :: Status
  CALL wrf_error_fatal3("<stdin>",3754,&
'wrf_quilt_get_var_ti_double not supported yet')
RETURN
END SUBROUTINE wrf_quilt_get_var_ti_double 

SUBROUTINE wrf_quilt_put_var_ti_double ( DataHandle,Element,  Varname, Data, Count,  Status )










  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real*8 ,        INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
  CALL wrf_error_fatal3("<stdin>",3777,&
'wrf_quilt_put_var_ti_double not supported yet')
RETURN
END SUBROUTINE wrf_quilt_put_var_ti_double 

SUBROUTINE wrf_quilt_get_var_ti_integer ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )











  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  integer                     :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: OutCount
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_get_var_ti_integer 

SUBROUTINE wrf_quilt_put_var_ti_integer ( DataHandle,Element,  Varname, Data, Count,  Status )










  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  integer ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_put_var_ti_integer 

SUBROUTINE wrf_quilt_get_var_ti_logical ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )











  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  logical                     :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: OutCount
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_get_var_ti_logical 

SUBROUTINE wrf_quilt_put_var_ti_logical ( DataHandle,Element,  Varname, Data, Count,  Status )










  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_put_var_ti_logical 

SUBROUTINE wrf_quilt_get_var_ti_char ( DataHandle,Element,  Varname, Data,  Status )










  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  CHARACTER*(*)               :: Data
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_get_var_ti_char 

SUBROUTINE wrf_quilt_put_var_ti_char ( DataHandle,Element,  Varname, Data,  Status )









  USE module_wrf_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER, PARAMETER ::  int_ioexit			=  	     10
  INTEGER, PARAMETER ::  int_open_for_write_begin	=  	     20
  INTEGER, PARAMETER ::  int_open_for_write_commit	=  	     30
  INTEGER, PARAMETER ::  int_open_for_read 		=  	     40
  INTEGER, PARAMETER ::  int_inquire_opened 		=  	     60
  INTEGER, PARAMETER ::  int_inquire_filename 		=  	     70
  INTEGER, PARAMETER ::  int_iosync 			=  	     80
  INTEGER, PARAMETER ::  int_ioclose 			=  	     90
  INTEGER, PARAMETER ::  int_next_time 			=  	    100
  INTEGER, PARAMETER ::  int_set_time 			=  	    110
  INTEGER, PARAMETER ::  int_next_var 			=  	    120
  INTEGER, PARAMETER ::  int_dom_ti_real 		=  	    140
  INTEGER, PARAMETER ::  int_dom_ti_double 		=  	    160
  INTEGER, PARAMETER ::  int_dom_ti_integer 		=  	    180
  INTEGER, PARAMETER ::  int_dom_ti_logical 		=  	    200
  INTEGER, PARAMETER ::  int_dom_ti_char 		=  	    220
  INTEGER, PARAMETER ::  int_dom_td_real 		=  	    240
  INTEGER, PARAMETER ::  int_dom_td_double 		=  	    260
  INTEGER, PARAMETER ::  int_dom_td_integer 		=  	    280
  INTEGER, PARAMETER ::  int_dom_td_logical 		=  	    300
  INTEGER, PARAMETER ::  int_dom_td_char 		=  	    320
  INTEGER, PARAMETER ::  int_var_ti_real 		=  	    340
  INTEGER, PARAMETER ::  int_var_ti_double 		=  	    360
  INTEGER, PARAMETER ::  int_var_ti_integer 		=  	    380
  INTEGER, PARAMETER ::  int_var_ti_logical 		=  	    400
  INTEGER, PARAMETER ::  int_var_ti_char 		=  	    420
  INTEGER, PARAMETER ::  int_var_td_real 		=  	    440
  INTEGER, PARAMETER ::  int_var_td_double 		=  	    460
  INTEGER, PARAMETER ::  int_var_td_integer 		=  	    480
  INTEGER, PARAMETER ::  int_var_td_logical 		=  	    500
  INTEGER, PARAMETER ::  int_var_td_char 		=  	    520
  INTEGER, PARAMETER ::  int_field 			=  	    530
  INTEGER, PARAMETER ::  int_var_info 			=  	    540
  INTEGER, PARAMETER ::  int_noop 			=  	    550
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  CHARACTER*(*) , INTENT(IN)  :: Data
  INTEGER                     :: Status
  INTEGER i, itypesize, tasks_in_group, ierr, comm_io_group
  REAL dummy



  CALL wrf_debug ( 50, 'in wrf_quilt_put_var_ti_char' ) 

  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )

      IF ( wrf_dm_on_monitor() ) THEN
         CALL int_gen_ti_header_char( hdrbuf, hdrbufsize, itypesize, &
                                      DataHandle, TRIM(Element),     &
                                      TRIM(VarName), TRIM(Data), int_var_ti_char )
      ELSE
         CALL int_gen_noop_header( hdrbuf, hdrbufsize, itypesize )
      ENDIF

      iserver = get_server_id ( DataHandle )
      CALL get_mpi_comm_io_groups( comm_io_group , iserver )
      CALL Mpi_Comm_Size (  comm_io_group , tasks_in_group , ierr  )


      
      reduced = 0
      reduced(1) = hdrbufsize 
      IF ( wrf_dm_on_monitor() )  reduced(2) = DataHandle
      CALL MPI_Reduce( reduced, reduced_dummy, 2, MPI_INTEGER, MPI_SUM, tasks_in_group-1, comm_io_group, ierr )

      
      CALL collect_on_comm_debug("module_io_quilt.F",4496, comm_io_group,            &
                            onebyte,                       &
                            hdrbuf, hdrbufsize , &
                            dummy, 0 )
    ENDIF
  ENDIF


RETURN
END SUBROUTINE wrf_quilt_put_var_ti_char 

SUBROUTINE wrf_quilt_get_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )











  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real                        :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: OutCount
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_get_var_td_real 

SUBROUTINE wrf_quilt_put_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )










  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_put_var_td_real 

SUBROUTINE wrf_quilt_get_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )











  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real*8                      :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: OutCount
  INTEGER                     :: Status
  CALL wrf_error_fatal3("<stdin>",4051,&
'wrf_quilt_get_var_td_double not supported yet')
RETURN
END SUBROUTINE wrf_quilt_get_var_td_double 

SUBROUTINE wrf_quilt_put_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )










  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
  CALL wrf_error_fatal3("<stdin>",4075,&
'wrf_quilt_put_var_td_double not supported yet')
RETURN
END SUBROUTINE wrf_quilt_put_var_td_double 

SUBROUTINE wrf_quilt_get_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount,Status)











  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  integer                     :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: OutCount
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_get_var_td_integer 

SUBROUTINE wrf_quilt_put_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )










  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  integer ,       INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_put_var_td_integer 

SUBROUTINE wrf_quilt_get_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )











  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  logical                          :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                      :: OutCount
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_get_var_td_logical 

SUBROUTINE wrf_quilt_put_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )










  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_put_var_td_logical 

SUBROUTINE wrf_quilt_get_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )










  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  CHARACTER*(*)               :: Data
  INTEGER                     :: Status
RETURN
END SUBROUTINE wrf_quilt_get_var_td_char 

SUBROUTINE wrf_quilt_put_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )










  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  CHARACTER*(*) , INTENT(IN) :: Data
  INTEGER                    :: Status
RETURN
END SUBROUTINE wrf_quilt_put_var_td_char 

SUBROUTINE wrf_quilt_read_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm, &
                            DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )







  IMPLICIT NONE
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) , INTENT(INOUT) :: DateStr
  CHARACTER*(*) , INTENT(INOUT) :: VarName
  INTEGER ,       INTENT(INOUT) :: Field(*)
  integer                       ,intent(in)    :: FieldType
  integer                       ,intent(inout) :: Comm
  integer                       ,intent(inout) :: IOComm
  integer                       ,intent(in)    :: DomainDesc
  character*(*)                 ,intent(in)    :: MemoryOrder
  character*(*)                 ,intent(in)    :: Stagger
  character*(*) , dimension (*) ,intent(in)    :: DimNames
  integer ,dimension(*)         ,intent(in)    :: DomainStart, DomainEnd
  integer ,dimension(*)         ,intent(in)    :: MemoryStart, MemoryEnd
  integer ,dimension(*)         ,intent(in)    :: PatchStart,  PatchEnd
  integer                       ,intent(out)   :: Status
  Status = 0
RETURN
END SUBROUTINE wrf_quilt_read_field

SUBROUTINE wrf_quilt_write_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm,  &
                             DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                             DomainStart , DomainEnd ,                                    &
                             MemoryStart , MemoryEnd ,                                    &
                             PatchStart , PatchEnd ,                                      &
                             Status )


















  USE module_state_description
  USE module_wrf_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
      integer, parameter  :: WRF_FILE_NOT_OPENED                  = 100
      integer, parameter  :: WRF_FILE_OPENED_NOT_COMMITTED        = 101
      integer, parameter  :: WRF_FILE_OPENED_FOR_WRITE            = 102
      integer, parameter  :: WRF_FILE_OPENED_FOR_READ             = 103
      integer, parameter  :: WRF_REAL                             = 104
      integer, parameter  :: WRF_DOUBLE                           = 105
      integer, parameter  :: WRF_FLOAT=WRF_REAL
      integer, parameter  :: WRF_INTEGER                          = 106
      integer, parameter  :: WRF_LOGICAL                          = 107
      integer, parameter  :: WRF_COMPLEX                          = 108
      integer, parameter  :: WRF_DOUBLE_COMPLEX                   = 109
      integer, parameter  :: WRF_FILE_OPENED_FOR_UPDATE           = 110


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) , INTENT(IN)    :: DateStr
  CHARACTER*(*) , INTENT(IN)    :: VarName

  integer                       ,intent(in)    :: FieldType
  integer                       ,intent(inout) :: Comm
  integer                       ,intent(inout) :: IOComm
  integer                       ,intent(in)    :: DomainDesc
  character*(*)                 ,intent(in)    :: MemoryOrder
  character*(*)                 ,intent(in)    :: Stagger
  character*(*) , dimension (*) ,intent(in)    :: DimNames
  integer ,dimension(*)         ,intent(in)    :: DomainStart, DomainEnd
  integer ,dimension(*)         ,intent(in)    :: MemoryStart, MemoryEnd
  integer ,dimension(*)         ,intent(in)    :: PatchStart,  PatchEnd
  integer                       ,intent(out)   :: Status

  integer ii,jj,kk,myrank

  REAL, DIMENSION( MemoryStart(1):MemoryEnd(1), &
                   MemoryStart(2):MemoryEnd(2), &
                   MemoryStart(3):MemoryEnd(3) ) :: Field
  INTEGER locsize , typesize, itypesize
  INTEGER ierr, tasks_in_group, comm_io_group, dummy, i
  INTEGER, EXTERNAL :: use_package


  CALL wrf_debug ( 50, 'in wrf_quilt_write_field' ) 

  IF ( .NOT. (DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles) ) THEN
    CALL wrf_error_fatal3("<stdin>",4319,&
"frame/module_io_quilt.F: wrf_quilt_write_field: invalid data handle" )
  ENDIF
  IF ( .NOT. int_handle_in_use( DataHandle ) ) THEN
    CALL wrf_error_fatal3("<stdin>",4323,&
"frame/module_io_quilt.F: wrf_quilt_write_field: DataHandle not opened" )
  ENDIF

  locsize = (PatchEnd(1)-PatchStart(1)+1)* &
            (PatchEnd(2)-PatchStart(2)+1)* &
            (PatchEnd(3)-PatchStart(3)+1)

  CALL mpi_type_size( MPI_INTEGER, itypesize, ierr )
  
  
  IF ( FieldType .EQ. WRF_DOUBLE ) THEN
    CALL mpi_type_size( MPI_DOUBLE_PRECISION, typesize, ierr )
  ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN
    CALL mpi_type_size( MPI_REAL, typesize, ierr )
  ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
    CALL mpi_type_size( MPI_INTEGER, typesize, ierr )
  ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN
    CALL mpi_type_size( MPI_LOGICAL, typesize, ierr )
  ENDIF

  IF ( .NOT. okay_to_write( DataHandle ) ) THEN

      
      
      
      

      CALL int_gen_write_field_header ( hdrbuf, hdrbufsize, itypesize, typesize,           &
                               DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm,  &
                               333933         , MemoryOrder , Stagger , DimNames ,              &   
                               DomainStart , DomainEnd ,                                    &
                               MemoryStart , MemoryEnd ,                                    &
                               PatchStart , PatchEnd )

      int_num_bytes_to_write(DataHandle) = int_num_bytes_to_write(DataHandle) + locsize * typesize + hdrbufsize

      

      iserver = get_server_id ( DataHandle )

      CALL get_mpi_comm_io_groups( comm_io_group , iserver )
      

      CALL Mpi_Comm_Size (  comm_io_group , tasks_in_group , ierr  )




      
      reduced = 0
      reduced(1) = hdrbufsize 
      IF ( wrf_dm_on_monitor() )  reduced(2) = DataHandle
      CALL MPI_Reduce( reduced, reduced_dummy, 2, MPI_INTEGER, MPI_SUM, tasks_in_group-1, comm_io_group, ierr )

      

      CALL collect_on_comm_debug("module_io_quilt.F",4918, comm_io_group,                   &
                            onebyte,                          &
                            hdrbuf, hdrbufsize ,                 &
                            dummy, 0 )

  ELSE

    IF ( .NOT. associated( int_local_output_buffer ) ) THEN
      ALLOCATE ( int_local_output_buffer( (int_num_bytes_to_write( DataHandle )+1)/itypesize ), Stat=ierr )
      IF(ierr /= 0)THEN
         CALL wrf_error_fatal3("<stdin>",4390,&
"frame/module_io_quilt.F: wrf_quilt_write_field: allocate of int_local_output_buffer failed" )
      END IF
      int_local_output_cursor = 1
    ENDIF
      iserver = get_server_id ( DataHandle )


    
    CALL int_gen_write_field_header ( hdrbuf, hdrbufsize, itypesize, typesize,           &
                             DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm,  &
                             0          , MemoryOrder , Stagger , DimNames ,              &   
                             DomainStart , DomainEnd ,                                    &
                             MemoryStart , MemoryEnd ,                                    &
                             PatchStart , PatchEnd )

    
    
    CALL int_pack_data ( hdrbuf , hdrbufsize , int_local_output_buffer, int_local_output_cursor )

    
    
    CALL int_pack_data ( Field(PatchStart(1):PatchEnd(1),PatchStart(2):PatchEnd(2),PatchStart(3):PatchEnd(3) ), &
                                  locsize * typesize , int_local_output_buffer, int_local_output_cursor )

  ENDIF
  Status = 0


  RETURN
END SUBROUTINE wrf_quilt_write_field

SUBROUTINE wrf_quilt_get_var_info ( DataHandle , VarName , NDim , MemoryOrder , Stagger , &
                              DomainStart , DomainEnd , Status )







  IMPLICIT NONE
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: VarName
  integer                               :: NDim
  character*(*)                         :: MemoryOrder
  character*(*)                         :: Stagger
  integer ,dimension(*)                 :: DomainStart, DomainEnd
  integer                               :: Status
RETURN
END SUBROUTINE wrf_quilt_get_var_info

subroutine wrf_quilt_find_server(iserver)

  
  
  

  
  
  

  use module_wrf_quilt, only : in_avail, mpi_comm_avail, mpi_comm_local

  implicit none
  INCLUDE 'mpif.h'
  integer, intent(inout) :: iserver
  integer :: ierr
  character(255) :: message

  call wrf_message('Polling I/O servers...')

  if(in_avail) then
     call mpi_recv(iserver,1,MPI_INTEGER,MPI_ANY_SOURCE,0,mpi_comm_avail,MPI_STATUS_IGNORE,ierr)
     if(ierr/=0) then
        call wrf_error_fatal3("<stdin>",4465,&
'mpi_recv failed in wrf_quilt_find_server')
     endif
  endif

  call mpi_bcast(iserver,1,MPI_INTEGER,0,mpi_comm_local,ierr)
  if(ierr/=0) then
     call wrf_error_fatal3("<stdin>",4472,&
'mpi_bcast failed in wrf_quilt_find_server')
  endif

  write(message,'("I/O server ",I0," is ready for operations.")') iserver
  call wrf_message(message)


end subroutine wrf_quilt_find_server
subroutine wrf_quilt_server_ready()

  
  
  
  

  
  
  
  

  use module_wrf_quilt, only : mpi_comm_local, in_avail, availrank, mpi_comm_avail

  implicit none
  INCLUDE 'mpif.h'
  integer :: ierr
  character*255 :: message

  write(message,*) 'Entering wrf_quilt_server_ready.'
  call wrf_debug(1,message)

  call mpi_barrier(mpi_comm_local,ierr)
  if(ierr/=0) then
     call wrf_error_fatal3("<stdin>",4505,&
'mpi_barrier failed in wrf_quilt_server_ready')
  endif

  if(in_avail) then
     write(message,'("mpi_ssend ioserver=",I0," in wrf_quilt_server_ready")') availrank
     call wrf_debug(1,message)
     call mpi_ssend(availrank,1,MPI_INTEGER,0,0,mpi_comm_avail,ierr)
     if(ierr/=0) then
        call wrf_error_fatal3("<stdin>",4514,&
'mpi_ssend failed in wrf_quilt_server_ready')
     endif
  endif

  call mpi_barrier(mpi_comm_local,ierr)
  if(ierr/=0) then
     call wrf_error_fatal3("<stdin>",4521,&
'mpi_barrier failed in wrf_quilt_server_ready')
  endif

  write(message,*) 'Leaving wrf_quilt_server_ready.'
  call wrf_debug(1,message)

end subroutine wrf_quilt_server_ready

SUBROUTINE get_mpi_comm_io_groups( retval, isrvr )





      USE module_wrf_quilt
      IMPLICIT NONE
      INTEGER, INTENT(IN ) :: isrvr
      INTEGER, INTENT(OUT) :: retval
      retval = mpi_comm_io_groups(isrvr)
      RETURN
END SUBROUTINE get_mpi_comm_io_groups

SUBROUTINE get_nio_tasks_in_group( retval )





      USE module_wrf_quilt
      IMPLICIT NONE
      INTEGER, INTENT(OUT) :: retval
      retval = nio_tasks_in_group
      RETURN
END SUBROUTINE get_nio_tasks_in_group

SUBROUTINE collect_on_comm_debug(file,line, comm_io_group,   &
                        sze,                                 &
                        hdrbuf, hdrbufsize ,                 &
                        outbuf, outbufsize                   )
  IMPLICIT NONE
  CHARACTER*(*) file
  INTEGER line
  INTEGER comm_io_group
  INTEGER sze
  INTEGER hdrbuf(*), outbuf(*)
  INTEGER hdrbufsize, outbufsize 

  
  CALL collect_on_comm( comm_io_group,                       &
                        sze,                                 &
                        hdrbuf, hdrbufsize ,                 &
                        outbuf, outbufsize                   )
  
  RETURN
END


SUBROUTINE collect_on_comm_debug2(file,line,var,tag,sz,hdr_rec_size, &
                        comm_io_group,                       &
                        sze,                                 &
                        hdrbuf, hdrbufsize ,                 &
                        outbuf, outbufsize                   )
  IMPLICIT NONE
  CHARACTER*(*) file,var
  INTEGER line,tag,sz,hdr_rec_size
  INTEGER comm_io_group
  INTEGER sze
  INTEGER hdrbuf(*), outbuf(*)
  INTEGER hdrbufsize, outbufsize


  CALL collect_on_comm( comm_io_group,                       &
                        sze,                                 &
                        hdrbuf, hdrbufsize ,                 &
                        outbuf, outbufsize                   )

  RETURN
END
