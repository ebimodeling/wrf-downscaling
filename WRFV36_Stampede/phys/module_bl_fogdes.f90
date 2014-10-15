MODULE module_bl_fogdes

  USE module_model_constants
  USE module_bl_mynn, only: qcgmin, gno, gpw


  IMPLICIT NONE


CONTAINS

  SUBROUTINE bl_fogdes(&
               vdfg,qc_curr,dtbl,rho,dz8w,grav_settling,dqc,       &
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               its,ite, jts,jte, kts,kte                           &
                                                                   )























































   INTEGER, INTENT(IN)                       :: ims,ime,jms,jme,kms,kme &
                                               ,its,ite,jts,jte,kts,kte &
                                               ,ids,ide,jds,jde,kds,kde

   INTEGER, INTENT(IN)                       :: grav_settling

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                        &
                                       INTENT(IN),OPTIONAL    :: qc_curr
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                        &
                                       INTENT(IN)             :: rho
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                        &
                                       INTENT(IN   )          :: dz8w

   REAL, DIMENSION( ims:ime, jms:jme ),INTENT(IN),OPTIONAL    :: vdfg

   REAL, INTENT(INOUT),OPTIONAL                               :: dtbl



   REAL,parameter :: gpw2=0.66666666666667
   REAL :: gfluxp,gfluxm
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                        &
                                       INTENT(INOUT),OPTIONAL :: dqc



   INTEGER :: i,j,k,grav_settling2


  grav_settling2 = MIN(REAL(grav_settling), 1.)

   DO j=jts,jte
     DO i=its,ite

       
       
       
       
       
       

       k=kts

       IF (qc_curr(i,k,j) > qcgmin) THEN
          gfluxm=grav_settling2*qc_curr(i,k,j)*vdfg(i,j)
       ELSE
          gfluxm=0.
       ENDIF

       IF (.5*(qc_curr(i,k+1,j)+qc_curr(i,k,j)) > qcgmin) THEN
          gfluxp=grav_settling2*gno* &
                & (.5*(qc_curr(i,k+1,j)+qc_curr(i,k,j)))**gpw
       ELSE
          gfluxp=0.
       ENDIF

       dqc(i,k,j)=dqc(i,k,j) + (gfluxp - gfluxm)/dz8w(i,kts,j)    

       
       
       

       DO k=kts+1,kte-1

          IF (.5*(qc_curr(i,k+1,j)+qc_curr(i,k,j)) > qcgmin) THEN
             gfluxp=grav_settling2*gno* &
                   & (.5*(qc_curr(i,k+1,j)+qc_curr(i,k,j)))**gpw
          ELSE
             gfluxp=0.
          ENDIF

          IF (.5*(qc_curr(i,k-1,j)+qc_curr(i,k,j)) > qcgmin) THEN
             gfluxm=grav_settling2*gno* &
                   & (.5*(qc_curr(i,k-1,j)+qc_curr(i,k,j)))**gpw
          ELSE
             gfluxm=0.
          ENDIF

          dqc(i,k,j)= dqc(i,k,j) + (gfluxp - gfluxm)/dz8w(i,k,j)  

       ENDDO

      

     ENDDO
   ENDDO

  END SUBROUTINE bl_fogdes



END MODULE module_bl_fogdes
