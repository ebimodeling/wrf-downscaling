MODULE module_get_file_names







   INTEGER :: number_of_eligible_files
   CHARACTER(LEN=132) , DIMENSION(:) , ALLOCATABLE :: eligible_file_name

CONTAINS



   SUBROUTINE unix_ls ( root , id )


      IMPLICIT NONE
     
      CHARACTER (LEN=*) , INTENT(IN) :: root
      INTEGER , INTENT(IN) :: id

      CHARACTER (LEN=132) :: command
      INTEGER :: ierr , loop , loslen , strlen
      INTEGER , EXTERNAL :: SYSTEM
      LOGICAL :: unix_access_ok
      LOGICAL, EXTERNAL :: wrf_dm_on_monitor
      CHARACTER*256 message

      
      

      unix_access_ok = .FALSE.

      

      monitor_only_code : IF ( wrf_dm_on_monitor() ) THEN

         loslen = LEN ( command )
         CALL all_spaces ( command , loslen ) 
         WRITE ( command , FMT='("ls -1 ",A,"*d",I2.2,"* > .foo")' ) TRIM ( root ) , id
         
         
         
         

         ierr = SYSTEM ( TRIM ( command ) ) 
         ierr =  SYSTEM ( '( cat .foo | wc -l > .foo1 )' )
         unix_access_ok = .TRUE.

         

         IF ( .NOT. unix_access_ok ) THEN
            PRINT *,'Oops, how can I access UNIX commands from Fortran?'
            CALL wrf_error_fatal3("<stdin>",56,&
'system_or_exec_only' )
         END IF

         

         OPEN (FILE   = '.foo1'       , &
               UNIT   = 112           , &
               STATUS = 'OLD'         , &
               ACCESS = 'SEQUENTIAL'  , &
               FORM   = 'FORMATTED'     )

         READ ( 112 , * ) number_of_eligible_files
         CLOSE ( 112 )

         

         IF ( number_of_eligible_files .LE. 0 ) THEN
            PRINT *,'Oops, we need at least ONE input file (wrfout*) for the ndown program to read.'
            CALL wrf_error_fatal3("<stdin>",75,&
'need_wrfout_input_data' )
         END IF

      ENDIF monitor_only_code

      
      

      CALL wrf_dm_bcast_integer ( number_of_eligible_files, 1 )

      

      ALLOCATE ( eligible_file_name(number_of_eligible_files) , STAT=ierr )

      

      IF ( ierr .NE. 0 ) THEN
print *,'tried to allocate ',number_of_eligible_files,' eligible files, (look at ./foo)'
         WRITE(message,*)'module_get_file_names: unix_ls: unable to allocate filename array Status = ',ierr
         CALL wrf_error_fatal3("<stdin>",95,&
message )
      END IF

      

      CALL init_module_get_file_names

      

      monitor_only_code2: IF ( wrf_dm_on_monitor() ) THEN

         

         OPEN (FILE   = '.foo'        , &
               UNIT   = 111           , &
               STATUS = 'OLD'         , &
               ACCESS = 'SEQUENTIAL'  , &
               FORM   = 'FORMATTED'     )

         

         DO loop = 1 , number_of_eligible_files
            READ ( 111 , FMT='(A)' ) eligible_file_name(loop)
print *,TRIM(eligible_file_name(loop))
         END DO
         CLOSE ( 111 )

         

         ierr = SYSTEM ( '/bin/rm -f .foo'  )
         ierr = SYSTEM ( '/bin/rm -f .foo1' )

      ENDIF monitor_only_code2

      

      DO loop = 1 , number_of_eligible_files
         strlen = LEN( TRIM( eligible_file_name(loop) ) )
         CALL wrf_dm_bcast_string ( eligible_file_name(loop) , strlen  )
      ENDDO

   END SUBROUTINE unix_ls



   SUBROUTINE all_spaces ( command , length_of_char ) 

      IMPLICIT NONE

      INTEGER :: length_of_char
      CHARACTER (LEN=length_of_char) :: command
      INTEGER :: loop

      DO loop = 1 , length_of_char
         command(loop:loop) = ' '
      END DO

   END SUBROUTINE all_spaces



   SUBROUTINE init_module_get_file_names
   
      IMPLICIT NONE
      eligible_file_name = '                                                  ' // &
                           '                                                  ' // &
                           '                                '

   END SUBROUTINE init_module_get_file_names



END MODULE module_get_file_names






