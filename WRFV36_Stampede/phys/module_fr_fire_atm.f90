


module module_fr_fire_atm

use module_model_constants, only: cp,xlv
use module_fr_fire_util

contains

SUBROUTINE fire_tendency( &
    ids,ide, kds,kde, jds,jde,   & 
    ims,ime, kms,kme, jms,jme,   &
    its,ite, kts,kte, jts,jte,   &
    grnhfx,grnqfx,canhfx,canqfx, & 
    alfg,alfc,z1can,             & 
    zs,z_at_w,dz8w,mu,rho,       &
    rthfrten,rqvfrten)             








   IMPLICIT NONE



   INTEGER , INTENT(in) :: ids,ide, kds,kde, jds,jde, &
                           ims,ime, kms,kme, jms,jme, &
                           its,ite, kts,kte, jts,jte

   REAL, INTENT(in), DIMENSION( ims:ime,jms:jme ) :: grnhfx,grnqfx  
   REAL, INTENT(in), DIMENSION( ims:ime,jms:jme ) :: canhfx,canqfx  
   REAL, INTENT(in), DIMENSION( ims:ime,jms:jme ) :: zs  
   REAL, INTENT(in), DIMENSION( ims:ime,jms:jme ) :: mu  

   REAL, INTENT(in), DIMENSION( ims:ime,kms:kme,jms:jme ) :: z_at_w 
   REAL, INTENT(in), DIMENSION( ims:ime,kms:kme,jms:jme ) :: dz8w   
   REAL, INTENT(in), DIMENSION( ims:ime,kms:kme,jms:jme ) :: rho    

   REAL, INTENT(in) :: alfg 
   REAL, INTENT(in) :: alfc 
   REAL, INTENT(in) :: z1can    



   REAL, INTENT(out), DIMENSION( ims:ime,kms:kme,jms:jme ) ::   &
       rthfrten, & 
       rqvfrten    


   INTEGER :: i,j,k
   INTEGER :: i_st,i_en, j_st,j_en, k_st,k_en

   REAL :: cp_i
   REAL :: rho_i
   REAL :: xlv_i
   REAL :: z_w
   REAL :: fact_g, fact_c
   REAL :: alfg_i, alfc_i

   REAL, DIMENSION( its:ite,kts:kte,jts:jte ) :: hfx,qfx
   


        do j=jts,jte
            do k=kts,min(kte+1,kde)
               do i=its,ite
                   rthfrten(i,k,j)=0.
                   rqvfrten(i,k,j)=0.
               enddo
            enddo
        enddo



   

   cp_i = 1./cp     
   xlv_i = 1./xlv   
   alfg_i = 1./alfg
   alfc_i = 1./alfc




   call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,grnhfx,'fire_tendency:grnhfx')
   call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,grnqfx,'fire_tendency:grnqfx')



   i_st = MAX(its,ids+1)
   i_en = MIN(ite,ide-1)
   k_st = kts
   k_en = MIN(kte,kde-1)
   j_st = MAX(jts,jds+1)
   j_en = MIN(jte,jde-1)



   DO j = j_st,j_en
      DO k = k_st,k_en
         DO i = i_st,i_en

            

            z_w = z_at_w(i,k,j) - zs(i,j) 

            

            fact_g = cp_i * EXP( - alfg_i * z_w )
            IF ( z_w < z1can ) THEN
               fact_c = cp_i
            ELSE
               fact_c = cp_i * EXP( - alfc_i * (z_w - z1can) )
            END IF
            hfx(i,k,j) = fact_g * grnhfx(i,j) + fact_c * canhfx(i,j) 





            

            fact_g = xlv_i * EXP( - alfg_i * z_w )
            IF (z_w < z1can) THEN
               fact_c = xlv_i
            ELSE
               fact_c = xlv_i * EXP( - alfc_i * (z_w - z1can) )
            END IF
            qfx(i,k,j) = fact_g * grnqfx(i,j) + fact_c * canqfx(i,j) 
            






         END DO
      END DO
   END DO






   DO j = j_st,j_en
      DO k = k_st,k_en-1
         DO i = i_st,i_en

            rho_i = 1./rho(i,k,j)

            rthfrten(i,k,j) = - mu(i,j) * rho_i * (hfx(i,k+1,j)-hfx(i,k,j)) / dz8w(i,k,j)
            rqvfrten(i,k,j) = - mu(i,j) * rho_i * (qfx(i,k+1,j)-qfx(i,k,j)) / dz8w(i,k,j)

         END DO
      END DO
   END DO

   call print_3d_stats(its,ite,kts,kte,jts,jte,ims,ime,kms,kme,jms,jme,rthfrten,'fire_tendency:rthfrten')
   call print_3d_stats(its,ite,kts,kte,jts,jte,ims,ime,kms,kme,jms,jme,rqvfrten,'fire_tendency:rqvfrten')

   RETURN

END SUBROUTINE fire_tendency





end module module_fr_fire_atm
