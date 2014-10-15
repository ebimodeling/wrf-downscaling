














 MODULE module_ltng_cpmpr92z
 CONTAINS

 SUBROUTINE ltng_cpmpr92z ( &
                          
                            dx, dy, xland, ht, z, t,              &
                          
                            kLNB,                                 &
                          
                            cldtop_adjustment,                    &
                          
                            ids, ide, jds, jde, kds, kde,         &
                            ims, ime, jms, jme, kms, kme,         &
                            ips, ipe, jps, jpe, kps, kpe,         &
                          
                            total_flashrate                       &
                          )


 USE module_state_description


 USE module_model_constants
 USE module_wrf_error

 IMPLICIT NONE



 REAL,    INTENT(IN   )    ::       dx, dy

 REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: xland, ht
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: z, t


 INTEGER, DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: kLNB     


 REAL,    INTENT(IN   )    ::       cldtop_adjustment


 INTEGER, INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
 INTEGER, INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme
 INTEGER, INTENT(IN   )    ::       ips,ipe, jps,jpe, kps,kpe


 REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(  OUT) :: total_flashrate


 REAL :: dA              
 REAL :: zkm             

 REAL, PARAMETER:: baseArea=1296. 

 INTEGER :: i,k,j

 CHARACTER (LEN=250) :: message



 dA = dx*dy/1E6

 total_flashrate( ips:ipe,jps:jpe ) = 0.


 jloop: DO j=jps,jpe
    iloop: DO i=ips,ipe
        IF ( t(i,kLNB(i,j),j) .lt. 273.15 .and. &
            kLNB(i,j) .ge. kps .and. kLNB(i,j) .le. kpe ) THEN              
            zkm = ( z(i,kLNB(i,j),j) - ht(i,j) )/1E3 + cldtop_adjustment    
            IF ( zkm .gt. 0. ) THEN                                         
              IF ( xland(i,j) .lt. 1.5 ) THEN
                total_flashrate(i,j) = 3.44E-5 * (zkm**4.9) /60.            
              ELSE
                total_flashrate(i,j) = 6.57E-6 * (zkm**4.9) /60.            
              ENDIF
            ENDIF
        ENDIF
    ENDDO iloop
 ENDDO jloop


 total_flashrate(ips:ipe,jps:jpe) = total_flashrate(ips:ipe,jps:jpe) * dA/baseArea

 END SUBROUTINE ltng_cpmpr92z

 END MODULE module_ltng_cpmpr92z
