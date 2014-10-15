MODULE module_quilt_outbuf_ops






  INTEGER, PARAMETER :: tabsize = 5
  
  INTEGER, SAVE      :: num_entries


  TYPE varpatch
    LOGICAL                            :: forDeletion 
                                                      
    INTEGER, DIMENSION(3)              :: PatchStart, PatchEnd, PatchExtent
    REAL,    POINTER, DIMENSION(:,:,:) :: rptr 
    INTEGER, POINTER, DIMENSION(:,:,:) :: iptr
  END TYPE varpatch

  
  
  
  
  TYPE outpatchlist
    CHARACTER*80                       :: VarName, DateStr, MemoryOrder, &
                                          Stagger, DimNames(3)
    INTEGER, DIMENSION(3)              :: DomainStart, DomainEnd
    INTEGER                            :: FieldType
    
    INTEGER                            :: nPatch
    
    INTEGER                            :: nActivePatch
    TYPE(varpatch), ALLOCATABLE, DIMENSION(:) :: PatchList

  END TYPE outpatchlist

  TYPE(outpatchlist), DIMENSION(tabsize), SAVE :: outpatch_table

  
  
  
  
  
  
  
  INTEGER, ALLOCATABLE, DIMENSION(:,:), SAVE :: JoinedPatches 
 
  
  
  
  
  INTEGER, ALLOCATABLE, DIMENSION(:), SAVE   :: PatchCount



  TYPE outrec
    CHARACTER*80                       :: VarName, DateStr, MemoryOrder, &
                                          Stagger, DimNames(3)
    INTEGER                            :: ndim
    INTEGER, DIMENSION(3)              :: DomainStart, DomainEnd
    INTEGER                            :: FieldType
    REAL,    POINTER, DIMENSION(:,:,:) :: rptr 
    INTEGER, POINTER, DIMENSION(:,:,:) :: iptr
  END TYPE outrec

  TYPE(outrec), DIMENSION(tabsize) :: outbuf_table

CONTAINS

  SUBROUTINE init_outbuf





    IMPLICIT NONE
    INTEGER :: i, j
    DO i = 1, tabsize

      outbuf_table(i)%VarName = ""
      outbuf_table(i)%DateStr = ""
      outbuf_table(i)%MemoryOrder = ""
      outbuf_table(i)%Stagger = ""
      outbuf_table(i)%DimNames(1) = ""
      outbuf_table(i)%DimNames(2) = ""
      outbuf_table(i)%DimNames(3) = ""
      outbuf_table(i)%ndim = 0
      NULLIFY( outbuf_table(i)%rptr )
      NULLIFY( outbuf_table(i)%iptr )

    ENDDO

    num_entries = 0
  END SUBROUTINE init_outbuf


  SUBROUTINE write_outbuf ( DataHandle , io_form_arg )









    USE module_state_description
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
    INTEGER , INTENT(IN)  :: DataHandle, io_form_arg
    INTEGER               :: ii,ds1,de1,ds2,de2,ds3,de3
    INTEGER               :: Comm, IOComm, DomainDesc 
    INTEGER               :: Status
    CHARACTER*256         :: mess
    Comm = 0 ; IOComm = 0 ; DomainDesc = 0 

    DO ii = 1, num_entries
      WRITE(mess,*)'writing ', &
                    TRIM(outbuf_table(ii)%DateStr)," ",                                   &
                    TRIM(outbuf_table(ii)%VarName)," ",                                   &
                    TRIM(outbuf_table(ii)%MemoryOrder)
      ds1 = outbuf_table(ii)%DomainStart(1) ; de1 = outbuf_table(ii)%DomainEnd(1)
      ds2 = outbuf_table(ii)%DomainStart(2) ; de2 = outbuf_table(ii)%DomainEnd(2)
      ds3 = outbuf_table(ii)%DomainStart(3) ; de3 = outbuf_table(ii)%DomainEnd(3)

      SELECT CASE ( io_form_arg )

        CASE ( IO_NETCDF   )

          IF ( outbuf_table(ii)%FieldType .EQ. WRF_FLOAT ) THEN

          CALL ext_ncd_write_field ( DataHandle ,                                     &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%rptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  
                                 outbuf_table(ii)%DimNames ,                          &  
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )

          ELSE IF ( outbuf_table(ii)%FieldType .EQ. WRF_INTEGER ) THEN
          CALL ext_ncd_write_field ( DataHandle ,                                     &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%iptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  
                                 outbuf_table(ii)%DimNames ,                          &  
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )
          ENDIF
      CASE ( IO_GRIB1   )

          IF ( outbuf_table(ii)%FieldType .EQ. WRF_FLOAT ) THEN

          CALL ext_gr1_write_field ( DataHandle ,                                   &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%rptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  
                                 outbuf_table(ii)%DimNames ,                          &  
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )

          ELSE IF ( outbuf_table(ii)%FieldType .EQ. WRF_INTEGER ) THEN
          CALL ext_gr1_write_field ( DataHandle ,                                   &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%iptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  
                                 outbuf_table(ii)%DimNames ,                          &  
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )
          ENDIF
      CASE ( IO_GRIB2   )

          IF ( outbuf_table(ii)%FieldType .EQ. WRF_FLOAT ) THEN

          CALL ext_gr2_write_field ( DataHandle ,                                   &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%rptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  
                                 outbuf_table(ii)%DimNames ,                          &  
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )

          ELSE IF ( outbuf_table(ii)%FieldType .EQ. WRF_INTEGER ) THEN
          CALL ext_gr2_write_field ( DataHandle ,                                   &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%iptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  
                                 outbuf_table(ii)%DimNames ,                          &  
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )
          ENDIF
        CASE ( IO_INTIO  )
          IF ( outbuf_table(ii)%FieldType .EQ. WRF_FLOAT ) THEN

          CALL ext_int_write_field ( DataHandle ,                                     &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%rptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  
                                 outbuf_table(ii)%DimNames ,                          &  
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )

          ELSE IF ( outbuf_table(ii)%FieldType .EQ. WRF_INTEGER ) THEN

          CALL ext_int_write_field ( DataHandle ,                                     &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%iptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  
                                 outbuf_table(ii)%DimNames ,                          &  
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )

          ENDIF
        CASE DEFAULT
      END SELECT


      IF ( ASSOCIATED( outbuf_table(ii)%rptr) ) DEALLOCATE(outbuf_table(ii)%rptr)
      IF ( ASSOCIATED( outbuf_table(ii)%iptr) ) DEALLOCATE(outbuf_table(ii)%iptr)
      NULLIFY( outbuf_table(ii)%rptr )
      NULLIFY( outbuf_table(ii)%iptr )
    ENDDO
    CALL init_outbuf
  END SUBROUTINE write_outbuf


  SUBROUTINE stitch_outbuf_patches(ibuf)
    USE module_timing
    IMPLICIT none
    INTEGER, INTENT(in) :: ibuf








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
    INTEGER                              :: ipatch, jpatch, ii
    INTEGER                              :: ierr
    INTEGER                              :: npatches
    INTEGER,              DIMENSION(3)   :: newExtent, pos
    INTEGER, ALLOCATABLE, DIMENSION(:,:) :: OldPatchStart
    INTEGER, POINTER,   DIMENSION(:,:,:) :: ibuffer
    REAL,    POINTER,   DIMENSION(:,:,:) :: rbuffer
    CHARACTER*256                        :: mess
integer i,j



    IF(LEN_TRIM(outpatch_table(ibuf)%MemoryOrder) < 2)THEN
       
       
       
      IF ( outpatch_table(ibuf)%FieldType .EQ. WRF_FLOAT ) THEN

          DO jpatch=2,outpatch_table(ibuf)%npatch,1
             outpatch_table(ibuf)%PatchList(jpatch)%forDeletion = .TRUE.
             outpatch_table(ibuf)%nActivePatch = &
                                 outpatch_table(ibuf)%nActivePatch - 1
             DEALLOCATE(outpatch_table(ibuf)%PatchList(jpatch)%rptr)
          END DO

      ELSE IF ( outpatch_table(ibuf)%FieldType .EQ. WRF_INTEGER ) THEN

          DO jpatch=2,outpatch_table(ibuf)%npatch,1
             outpatch_table(ibuf)%PatchList(jpatch)%forDeletion = .TRUE.
             outpatch_table(ibuf)%nActivePatch = &
                                 outpatch_table(ibuf)%nActivePatch - 1
             DEALLOCATE(outpatch_table(ibuf)%PatchList(jpatch)%iptr)
          END DO

      ELSE
         CALL wrf_error_fatal3("<stdin>",375,&
"stitch_outbuf_patches: unrecognised Field Type")
      END IF



      RETURN

    END IF 

    
    
    ALLOCATE(OldPatchStart(3,outpatch_table(ibuf)%npatch), &
             JoinedPatches(outpatch_table(ibuf)%npatch,    &
                           outpatch_table(ibuf)%npatch),   &
             PatchCount(outpatch_table(ibuf)%npatch),      &
             Stat=ierr)
    IF(ierr /= 0)THEN
       CALL wrf_message('stitch_outbuf_patches: unable to stitch patches as allocate failed.')
       RETURN
    END IF

    JoinedPatches(:,:) = -1
    
    
    
    PatchCount(:) = 0
    OldPatchStart(:,:) = 0

    NULLIFY(ibuffer)
    NULLIFY(rbuffer)

    DO jpatch=1,outpatch_table(ibuf)%npatch,1

       
       JoinedPatches(1,jpatch) = jpatch
       PatchCount(jpatch) = 1

       
       
       OldPatchStart(:,jpatch) = outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(:)
    END DO

    
    ipatch = 1
    OUTER: DO WHILE(ipatch < outpatch_table(ibuf)%npatch)

       IF( outpatch_table(ibuf)%PatchList(ipatch)%forDeletion )THEN
          ipatch = ipatch + 1
          CYCLE OUTER
       END IF

       INNER: DO jpatch=ipatch+1,outpatch_table(ibuf)%npatch,1

          IF(outpatch_table(ibuf)%PatchList(jpatch)%forDeletion )THEN
             CYCLE INNER
          END IF

          
          
          
          
          
          
          IF(outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(1) == &
              (outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(1) - 1) )THEN

             
             
             IF( (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(2)==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(2) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(2)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(2)   ) .AND.&
                 (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(3)==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(3) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(3)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(3)) )THEN
		
                



                
                outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(1) = &
                         outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(1)
                CALL merge_patches(ibuf, ipatch, jpatch)

                
                ipatch = 1
                CYCLE OUTER
             END IF
          END IF
          
          
          
          
          IF(outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(1) == &
             (outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(1) + 1))THEN

             
             
             IF( (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(2)==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(2) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(2)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(2)   ) .AND.&
                 (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(3)==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(3) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(3)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(3)) )THEN

                 



		
                outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(1) = &
                        outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(1)
                CALL merge_patches(ibuf, ipatch, jpatch)

                
		ipatch = 1
		CYCLE OUTER
              END IF
           END IF

           
           IF(outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(2) == &
                (outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(2) - 1))THEN

              
              
              IF( (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(1)==     &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(1) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(1)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(1)   ) .AND.&
                 (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(3)==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(3) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(3)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(3)) )THEN

                 



                 
                 outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(2) = &
                         outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(2)
                 CALL merge_patches(ibuf, ipatch, jpatch)

                 
                 ipatch = 1
                 CYCLE OUTER
              END IF
           END IF

           IF(outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(2) == &
                (outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(2) + 1) )THEN

              
              
              IF( (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(1)==     &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(1) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(1)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(1)   ) .AND.&
                 (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(3)==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(3) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(3)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(3)) )THEN

                 



                 
                 outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(2) = &
                         outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(2)
                 CALL merge_patches(ibuf, ipatch, jpatch)

                 
                 ipatch = 1
                 CYCLE OUTER                 
              END IF
           END IF

           
           IF(outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(3) == &
               (outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(3) - 1) )THEN

              
              
              IF( (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(1)==     &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(1) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(1)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(1)   ) .AND.&
                 (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(2)==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(2) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(2)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(2)) )THEN

                 



                 
                 outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(3) = &
                         outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(3)
                 CALL merge_patches(ibuf, ipatch, jpatch)

                 
                 ipatch = 1
                 CYCLE OUTER                 
              END IF
           END IF

           IF(outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(3) == &
                (outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(3) + 1))THEN

              
              
              IF( (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(1)==     &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(1) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(1)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(1)   ) .AND.&
                 (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(2)==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(2) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(2)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(2)) )THEN

                 



                 
                 outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(3) = &
                         outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(3)
                 CALL merge_patches(ibuf, ipatch, jpatch)

                 
                 ipatch = 1
                 CYCLE OUTER                 
              END IF
           END IF

       END DO INNER

       ipatch = ipatch + 1

    END DO OUTER

    npatches = 0

    DO jpatch=1,outpatch_table(ibuf)%npatch,1

       IF ( outpatch_table(ibuf)%PatchList(jpatch)%forDeletion ) CYCLE









       
       npatches = npatches + 1

       
       
       IF(PatchCount(jpatch) == 1) CYCLE

       
       newExtent(:) = outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(:) - &
                      outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(:) + 1
       
       IF ( outpatch_table(ibuf)%FieldType .EQ. WRF_FLOAT ) THEN
          ALLOCATE(rbuffer(newExtent(1), newExtent(2), newExtent(3)), &
                   Stat=ierr)
       ELSE IF ( outpatch_table(ibuf)%FieldType .EQ. WRF_INTEGER ) THEN
          ALLOCATE(ibuffer(newExtent(1), newExtent(2), newExtent(3)), &
                   Stat=ierr)
       END IF
       IF(ierr /= 0)THEN
          CALL wrf_error_fatal3("<stdin>",657,&
'stitch_outbuf_patches: unable to stitch patches as allocate for merge buffer failed.')
          RETURN
       END IF

       
       
       IF( ASSOCIATED(rbuffer) )THEN



          DO ipatch=1,PatchCount(jpatch),1

             ii = JoinedPatches(ipatch, jpatch)

             
             
             
             
             pos(:) = OldPatchStart(:,ii) - &
                      outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(:) + 1
             
             

             rbuffer(pos(1): pos(1)+outpatch_table(ibuf)%PatchList(ii)%PatchExtent(1)-1, &
                     pos(2): pos(2)+outpatch_table(ibuf)%PatchList(ii)%PatchExtent(2)-1, &
                     pos(3): pos(3)+outpatch_table(ibuf)%PatchList(ii)%PatchExtent(3)-1 ) &
                             = &
                      outpatch_table(ibuf)%PatchList(ii)%rptr(:, :, :)

             
             
             DEALLOCATE(outpatch_table(ibuf)%PatchList(ii)%rptr)
          END DO



          
          
          outpatch_table(ibuf)%PatchList(jpatch)%rptr => rbuffer

          
          NULLIFY(rbuffer)

       ELSE IF( ASSOCIATED(ibuffer) )THEN



          DO ipatch=1,PatchCount(jpatch),1

             ii = JoinedPatches(ipatch, jpatch)

             
             pos(:) = OldPatchStart(:,ii) - &
                      outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(:) + 1
             
             
             ibuffer(pos(1): &
                 pos(1)+outpatch_table(ibuf)%PatchList(ii)%PatchExtent(1)-1, &
                 pos(2): &
                 pos(2)+outpatch_table(ibuf)%PatchList(ii)%PatchExtent(2)-1, &
                 pos(3): &
                 pos(3)+outpatch_table(ibuf)%PatchList(ii)%PatchExtent(3)-1 ) = &
                      outpatch_table(ibuf)%PatchList(ii)%iptr(:, :, :)

             DEALLOCATE(outpatch_table(ibuf)%PatchList(ii)%iptr)
          END DO



          
          
          outpatch_table(ibuf)%PatchList(jpatch)%iptr => ibuffer
          NULLIFY(ibuffer)

       END IF

    END DO

    WRITE(mess,*) "--------------------------"
    CALL wrf_message(mess)

    
    outpatch_table(ibuf)%nPatch = npatches

    DEALLOCATE(OldPatchStart, JoinedPatches, PatchCount)



  END SUBROUTINE stitch_outbuf_patches

  
  SUBROUTINE merge_patches(itab, ipatch, jpatch)
    INTEGER, INTENT(in) :: itab, ipatch, jpatch
    
    INTEGER :: ii

    
    
    
    DO ii=1,PatchCount(jpatch),1
       PatchCount(ipatch) = PatchCount(ipatch) + 1
       JoinedPatches(PatchCount(ipatch),ipatch) = JoinedPatches(ii,jpatch)
    END DO
    
    outpatch_table(itab)%PatchList(jpatch)%forDeletion = .TRUE.
    
    outpatch_table(itab)%nActivePatch = outpatch_table(itab)%nActivePatch - 1

  END SUBROUTINE merge_patches

END MODULE module_quilt_outbuf_ops



  SUBROUTINE store_patch_in_outbuf( inbuf_r, inbuf_i, DateStr, VarName , FieldType, MemoryOrder, Stagger, DimNames, &
                                    DomainStart , DomainEnd , &
                                    MemoryStart , MemoryEnd , &
                                    PatchStart , PatchEnd )














    USE module_quilt_outbuf_ops
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
    INTEGER ,                INTENT(IN) :: FieldType
    REAL    , DIMENSION(*) , INTENT(IN) :: inbuf_r
    INTEGER , DIMENSION(*) , INTENT(IN) :: inbuf_i
    INTEGER , DIMENSION(3) , INTENT(IN) :: DomainStart , DomainEnd , MemoryStart , MemoryEnd , PatchStart , PatchEnd
    CHARACTER*(*)          , INTENT(IN) :: DateStr , VarName, MemoryOrder , Stagger, DimNames(3)

    CHARACTER*256         ::  mess
    INTEGER               :: l,m,n,ii,jj
    LOGICAL               :: found

    
    ii = 1
    found = .false.
    DO WHILE ( .NOT. found .AND. ii .LE. num_entries )
      
      IF ( TRIM(VarName) .EQ. TRIM(outbuf_table(ii)%VarName) ) THEN
        IF ( TRIM(DateStr) .EQ. TRIM(outbuf_table(ii)%DateStr) ) THEN
          IF ( TRIM(MemoryOrder) .EQ. TRIM(outbuf_table(ii)%MemoryOrder) ) THEN
            found = .true.
          ELSE
            CALL wrf_error_fatal3("<stdin>",827,&
"store_patch_in_outbuf: memory order disagreement")
          ENDIF
        ELSE
          CALL wrf_error_fatal3("<stdin>",831,&
"store_patch_in_outbuf: multiple dates in buffer")
        ENDIF
      ELSE
        ii = ii + 1
      ENDIF
    ENDDO
    IF ( .NOT. found ) THEN
      num_entries = num_entries + 1
      IF      ( FieldType .EQ. WRF_FLOAT ) THEN
        ALLOCATE( outbuf_table(num_entries)%rptr(DomainStart(1):DomainEnd(1), &
                                                 DomainStart(2):DomainEnd(2),DomainStart(3):DomainEnd(3)) )
      ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
        ALLOCATE( outbuf_table(num_entries)%iptr(DomainStart(1):DomainEnd(1), &
                                                 DomainStart(2):DomainEnd(2),DomainStart(3):DomainEnd(3)) )
      ELSE
        write(mess,*)"store_patch_in_outbuf: unsupported type ", FieldType
        CALL wrf_error_fatal3("<stdin>",848,&
mess)
      ENDIF
      outbuf_table(num_entries)%VarName = TRIM(VarName)
      outbuf_table(num_entries)%DateStr = TRIM(DateStr)
      outbuf_table(num_entries)%MemoryOrder = TRIM(MemoryOrder)
      outbuf_table(num_entries)%Stagger = TRIM(Stagger)
      outbuf_table(num_entries)%DimNames(1) = TRIM(DimNames(1))
      outbuf_table(num_entries)%DimNames(2) = TRIM(DimNames(2))
      outbuf_table(num_entries)%DimNames(3) = TRIM(DimNames(3))
      outbuf_table(num_entries)%DomainStart = DomainStart
      outbuf_table(num_entries)%DomainEnd = DomainEnd
      outbuf_table(num_entries)%FieldType = FieldType
      ii = num_entries
    ENDIF
    jj = 1
    IF (  FieldType .EQ. WRF_FLOAT ) THEN
      DO n = PatchStart(3),PatchEnd(3)
        DO m = PatchStart(2),PatchEnd(2)
          DO l = PatchStart(1),PatchEnd(1)
            outbuf_table(ii)%rptr(l,m,n) = inbuf_r(jj)
            jj = jj + 1
          ENDDO
        ENDDO
      ENDDO
    ENDIF
    IF (  FieldType .EQ. WRF_INTEGER ) THEN
      DO n = PatchStart(3),PatchEnd(3)
        DO m = PatchStart(2),PatchEnd(2)
          DO l = PatchStart(1),PatchEnd(1)
            outbuf_table(ii)%iptr(l,m,n) = inbuf_i(jj)
            jj = jj + 1
          ENDDO
        ENDDO
      ENDDO
    ENDIF

    RETURN

  END SUBROUTINE store_patch_in_outbuf



  SUBROUTINE store_patch_in_outbuf_pnc( inbuf_r, inbuf_i, DateStr, VarName , &
                                        FieldType, MemoryOrder, Stagger,     &
                                        DimNames ,                &
                                        DomainStart , DomainEnd , &
                                        MemoryStart , MemoryEnd , &
                                        PatchStart  , PatchEnd  , &
                                        ntasks )












    USE module_quilt_outbuf_ops, Only: outpatch_table, tabsize, num_entries
    USE module_timing
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
    INTEGER ,               INTENT(IN) :: FieldType
    REAL    , DIMENSION(*), INTENT(IN) :: inbuf_r
    INTEGER , DIMENSION(*), INTENT(IN) :: inbuf_i
    INTEGER , DIMENSION(3), INTENT(IN) :: DomainStart, DomainEnd, MemoryStart,&
                                          MemoryEnd , PatchStart , PatchEnd
    CHARACTER*(*)         , INTENT(IN) :: DateStr , VarName, MemoryOrder , &
                                          Stagger, DimNames(3)
    INTEGER, INTENT(IN) :: ntasks 
                                  

    CHARACTER*256         :: mess
    INTEGER               :: l,m,n,ii,jj,ipatch,ierr
    LOGICAL               :: found



    
    ii = 1
    found = .false.
    DO WHILE ( .NOT. found .AND. ii .LE. num_entries )
      
      IF ( TRIM(VarName) .EQ. TRIM(outpatch_table(ii)%VarName) ) THEN
        IF ( TRIM(DateStr) .EQ. TRIM(outpatch_table(ii)%DateStr) ) THEN
          IF ( TRIM(MemoryOrder) .EQ. TRIM(outpatch_table(ii)%MemoryOrder) ) THEN
            found = .true.
          ELSE
            CALL wrf_error_fatal3("<stdin>",954,&
"store_patch_in_outbuf_pnc: memory order disagreement")
          ENDIF
        ELSE
          CALL wrf_error_fatal3("<stdin>",958,&
"store_patch_in_outbuf_pnc: multiple dates in buffer")
        ENDIF
      ELSE
        ii = ii + 1
      ENDIF
    ENDDO
    IF ( .NOT. found ) THEN
      num_entries = num_entries + 1
      IF(num_entries > tabsize)THEN
         WRITE(mess,*) 'Number of entries in outpatch_table has exceeded tabsize (',&
         tabsize,') in module_quilt_outbuf_ops::store_patch_in_outbuf_pnc'
         CALL wrf_error_fatal3("<stdin>",970,&
mess)
      END IF
      outpatch_table(num_entries)%npatch = 0

      outpatch_table(num_entries)%VarName     = TRIM(VarName)
      outpatch_table(num_entries)%DateStr     = TRIM(DateStr)
      outpatch_table(num_entries)%MemoryOrder = TRIM(MemoryOrder)
      outpatch_table(num_entries)%Stagger     = TRIM(Stagger)
      outpatch_table(num_entries)%DimNames(1) = TRIM(DimNames(1))
      outpatch_table(num_entries)%DimNames(2) = TRIM(DimNames(2))
      outpatch_table(num_entries)%DimNames(3) = TRIM(DimNames(3))
      outpatch_table(num_entries)%DomainStart = DomainStart
      outpatch_table(num_entries)%DomainEnd   = DomainEnd
      outpatch_table(num_entries)%FieldType   = FieldType
      
      
      
      IF ( ALLOCATED(outpatch_table(num_entries)%PatchList) ) &
         DEALLOCATE(outpatch_table(num_entries)%PatchList)
      ALLOCATE(outpatch_table(num_entries)%PatchList(ntasks), Stat=ierr)
      IF(ierr /= 0)THEN
         WRITE(mess,*)'num_entries ',num_entries,' ntasks ',ntasks,' ierr ',ierr
         CALL wrf_message(mess)
         WRITE(mess,*)'Allocation for ',ntasks, &
                      ' patches in store_patch_in_outbuf_pnc() failed.'
         CALL wrf_error_fatal3("<stdin>",996,&
mess )
      ENDIF
      
      DO ii=1, ntasks, 1
         outpatch_table(num_entries)%PatchList(ii)%forDeletion = .FALSE.
         NULLIFY(outpatch_table(num_entries)%PatchList(ii)%rptr)
         NULLIFY(outpatch_table(num_entries)%PatchList(ii)%iptr)
         outpatch_table(num_entries)%PatchList(ii)%PatchStart(:) = 0
         outpatch_table(num_entries)%PatchList(ii)%PatchEnd(:) = 0
         outpatch_table(num_entries)%PatchList(ii)%PatchExtent(:) = 0
      END DO 

      ii = num_entries

      WRITE(mess,*)'Adding field entry no. ',num_entries
      CALL wrf_message(mess)
      WRITE(mess,*)'Variable = ',TRIM(VarName)
      CALL wrf_message(mess)
      WRITE(mess,*)'Domain start = ',DomainStart(:)
      CALL wrf_message(mess)
      WRITE(mess,*)'Domain end   = ',DomainEnd(:)
      CALL wrf_message(mess)
    ENDIF

    
    
    
    IF(LEN_TRIM(outpatch_table(ii)%MemoryOrder) >= 2 .OR. &
       outpatch_table(ii)%npatch < 1)THEN

       
       outpatch_table(ii)%npatch = outpatch_table(ii)%npatch + 1
       outpatch_table(ii)%nActivePatch = outpatch_table(ii)%npatch

       ipatch = outpatch_table(ii)%npatch

       outpatch_table(ii)%PatchList(ipatch)%PatchStart(:) = PatchStart(:)
       outpatch_table(ii)%PatchList(ipatch)%PatchEnd(:)   = PatchEnd(:)
       outpatch_table(ii)%PatchList(ipatch)%PatchExtent(:)= PatchEnd(:) - PatchStart(:) + 1

       ierr = 0

       IF      ( FieldType .EQ. WRF_FLOAT ) THEN
          ALLOCATE( outpatch_table(ii)%PatchList(ipatch)%rptr( &
                                                 outpatch_table(ii)%PatchList(ipatch)%PatchExtent(1), &
                                                 outpatch_table(ii)%PatchList(ipatch)%PatchExtent(2), &
                                                 outpatch_table(ii)%PatchList(ipatch)%PatchExtent(3)),&
                                                 Stat=ierr)
       ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
          ALLOCATE( outpatch_table(ii)%PatchList(ipatch)%iptr( &
                                                 outpatch_table(ii)%PatchList(ipatch)%PatchExtent(1), &
                                                 outpatch_table(ii)%PatchList(ipatch)%PatchExtent(2), &
                                                 outpatch_table(ii)%PatchList(ipatch)%PatchExtent(3)),&
                                                 Stat=ierr)
       ELSE
          WRITE(mess,*)"store_patch_in_outbuf_pnc: unsupported type ", FieldType
          CALL wrf_error_fatal3("<stdin>",1053,&
mess)
       ENDIF

       IF(ierr /= 0)THEN
          WRITE(mess,*)"store_patch_in_outbuf_pnc: failed to allocate memory to hold patch for var. ", TRIM(VarName)
          CALL wrf_error_fatal3("<stdin>",1059,&
mess)
       END IF

       jj = 1

       WRITE(mess,"('Variable ',(A),', patch ',I3,': (',I3,':',I3,',',I3,':',I3,',',I3,':',I3,')')")&
                TRIM(outpatch_table(ii)%VarName),  &
                ipatch, &
                PatchStart(1),PatchEnd(1), &
                PatchStart(2),PatchEnd(2), &
                PatchStart(3),PatchEnd(3)
       CALL wrf_message(mess)

       IF (  FieldType .EQ. WRF_FLOAT ) THEN
          DO n = 1,outpatch_table(ii)%PatchList(ipatch)%PatchExtent(3),1
             DO m = 1,outpatch_table(ii)%PatchList(ipatch)%PatchExtent(2),1
                DO l = 1,outpatch_table(ii)%PatchList(ipatch)%PatchExtent(1),1
                   outpatch_table(ii)%PatchList(ipatch)%rptr(l,m,n) = inbuf_r(jj)
                   jj = jj + 1
                ENDDO
             ENDDO
          ENDDO
       ENDIF
       IF (  FieldType .EQ. WRF_INTEGER ) THEN
          DO n = 1,outpatch_table(ii)%PatchList(ipatch)%PatchExtent(3),1
             DO m = 1,outpatch_table(ii)%PatchList(ipatch)%PatchExtent(2),1
                DO l = 1,outpatch_table(ii)%PatchList(ipatch)%PatchExtent(1),1
                   outpatch_table(ii)%PatchList(ipatch)%iptr(l,m,n) = inbuf_i(jj)
                   jj = jj + 1
                ENDDO
             ENDDO
          ENDDO
       ENDIF

    END IF 



    RETURN

  END SUBROUTINE store_patch_in_outbuf_pnc



  SUBROUTINE add_to_bufsize_for_field( VarName, Nbytes )









    USE module_quilt_outbuf_ops
    IMPLICIT NONE
    CHARACTER*(*)    , INTENT(IN) :: VarName
    INTEGER          , INTENT(IN) :: Nbytes

    CHARACTER*256         :: mess
    INTEGER               :: i, ierr
    INTEGER               :: VarNameAsInts( 256 )
    VarNameAsInts( 1 ) = len(trim(VarName))
    DO i = 2, len(trim(VarName)) + 1
      VarNameAsInts( i ) = ICHAR( VarName(i-1:i-1) )
    ENDDO
    CALL add_to_bufsize_for_field_c ( VarNameAsInts, Nbytes )
    RETURN
  END SUBROUTINE add_to_bufsize_for_field
  
  SUBROUTINE store_piece_of_field( inbuf, VarName, Nbytes )









    USE module_quilt_outbuf_ops
    IMPLICIT NONE
    INTEGER ,                INTENT(IN) :: Nbytes
    INTEGER , DIMENSION(*) , INTENT(IN) :: inbuf
    CHARACTER*(*)          , INTENT(IN) :: VarName

    CHARACTER*256         :: mess
    INTEGER               :: i, ierr
    INTEGER               :: VarNameAsInts( 256 )

    VarNameAsInts( 1 ) = len(trim(VarName))
    DO i = 2, len(trim(VarName)) + 1
      VarNameAsInts( i ) = ICHAR( VarName(i-1:i-1) )
    ENDDO
    CALL store_piece_of_field_c ( inbuf, VarNameAsInts, Nbytes, ierr )
    IF ( ierr .NE. 0 ) CALL wrf_error_fatal3("<stdin>",1155,&
"store_piece_of_field" )
    RETURN
  END SUBROUTINE store_piece_of_field

  SUBROUTINE retrieve_pieces_of_field( outbuf, VarName, obufsz, Nbytes_tot, lret )













    USE module_quilt_outbuf_ops
    IMPLICIT NONE
    INTEGER ,                INTENT(IN) :: obufsz
    INTEGER ,                INTENT(OUT) :: Nbytes_tot
    INTEGER , DIMENSION(*) , INTENT(OUT) :: outbuf
    CHARACTER*(*)    , INTENT(OUT) :: VarName
    LOGICAL                       :: lret   

    CHARACTER*256         :: mess
    INTEGER               :: i, iret
    INTEGER               :: VarNameAsInts( 256 )

    CALL retrieve_pieces_of_field_c ( outbuf, VarNameAsInts, obufsz, Nbytes_tot, iret )
    IF ( iret .NE.  0 ) THEN
       lret = .FALSE.
    ELSE
       lret = .TRUE.
       VarName = ' '
       DO i = 2, VarNameAsInts(1) + 1
         VarName(i-1:i-1) = CHAR(VarNameAsInts( i ))
       ENDDO
    ENDIF
    RETURN
  END SUBROUTINE retrieve_pieces_of_field

