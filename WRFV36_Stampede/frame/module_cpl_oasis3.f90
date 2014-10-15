 MODULE module_cpl_oasis3
   
   
   
   USE module_domain, ONLY : domain
   IMPLICIT NONE
   PRIVATE
   PUBLIC cpl_oasis_init
   PUBLIC cpl_oasis_def_dmcomm
   PUBLIC cpl_oasis_define
   PUBLIC cpl_oasis_toreceive
   PUBLIC cpl_oasis_tosend
   PUBLIC cpl_oasis_snd
   PUBLIC cpl_oasis_rcv
   PUBLIC cpl_oasis_finalize
   PUBLIC cpl_oasis_abort

CONTAINS

   SUBROUTINE cpl_oasis_init( kl_comm ) 
      INTEGER, INTENT(OUT) :: kl_comm       
      IF (.FALSE.) kl_comm = -1 
   END SUBROUTINE cpl_oasis_init

   SUBROUTINE cpl_oasis_def_dmcomm( kdm_comm ) 
      INTEGER, INTENT(IN) :: kdm_comm       
      IF (.FALSE.) WRITE(*,*) kdm_comm 
   END SUBROUTINE cpl_oasis_def_dmcomm

   SUBROUTINE cpl_oasis_define( cdsndname, cdrcvname, pgrid )
      CHARACTER(*), INTENT(IN), DIMENSION(:,:,:)  :: cdsndname, cdrcvname   
      TYPE(domain), INTENT(IN), OPTIONAL, POINTER :: pgrid                  
      IF (.FALSE.) WRITE(*,*) cdsndname, cdrcvname, pgrid%id  
   END SUBROUTINE cpl_oasis_define

   FUNCTION cpl_oasis_toreceive( kdomwrf, kdomext, kfldid )
      INTEGER, INTENT(IN) :: kdomwrf   
      INTEGER, INTENT(IN) :: kdomext   
      INTEGER, INTENT(IN) :: kfldid    
      LOGICAL :: cpl_oasis_toreceive
      IF (.FALSE.) WRITE(*,*) kdomwrf, kdomext, kfldid  
      IF (.FALSE.) cpl_oasis_toreceive = .false.  
   END FUNCTION cpl_oasis_toreceive

   FUNCTION cpl_oasis_tosend( kdomwrf, kdomext, kfldid )
      INTEGER, INTENT(IN) :: kdomwrf   
      INTEGER, INTENT(IN) :: kdomext   
      INTEGER, INTENT(IN) :: kfldid    
      LOGICAL :: cpl_oasis_tosend
      IF (.FALSE.) WRITE(*,*) kdomwrf, kdomext, kfldid  
      IF (.FALSE.) cpl_oasis_tosend = .false.  
   END FUNCTION cpl_oasis_tosend

   SUBROUTINE cpl_oasis_snd( kdomwrf, kdomext, kfldid, ksec, pdata )
      
      INTEGER,              INTENT(IN) :: kdomwrf   
      INTEGER,              INTENT(IN) :: kdomext   
      INTEGER,              INTENT(IN) :: kfldid    
      INTEGER,              INTENT(IN) :: ksec      
      REAL, DIMENSION(:,:), INTENT(IN) :: pdata     
      IF (.FALSE.) WRITE(*,*) kdomwrf, kdomext, kfldid, ksec, pdata 
   END SUBROUTINE cpl_oasis_snd

   SUBROUTINE cpl_oasis_rcv( kdomwrf, kdomext, kfldid, ksec, pcplrcv )
      INTEGER,              INTENT(IN   ) :: kdomwrf   
      INTEGER,              INTENT(IN   ) :: kdomext   
      INTEGER,              INTENT(IN   ) :: kfldid    
      INTEGER,              INTENT(IN   ) :: ksec      
      REAL, DIMENSION(:,:), INTENT(  OUT) :: pcplrcv   
      IF (.FALSE.) WRITE(*,*) kdomwrf, kdomext, kfldid, ksec 
      IF (.FALSE.) pcplrcv(:,:) = -1. 
   END SUBROUTINE cpl_oasis_rcv

   SUBROUTINE cpl_oasis_finalize()
      IF (.FALSE.) WRITE(*,*) 'You should not be there...'
   END SUBROUTINE cpl_oasis_finalize

   SUBROUTINE cpl_oasis_abort( cdroutine, cdtxt )
      CHARACTER(*), INTENT(IN) :: cdroutine   
      CHARACTER(*), INTENT(IN) :: cdtxt       
      IF (.FALSE.) WRITE(*,*) cdroutine, cdtxt   
   END SUBROUTINE cpl_oasis_abort

END MODULE module_cpl_oasis3
