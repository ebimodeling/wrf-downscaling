




module module_fr_fire_driver_wrf


use module_fr_fire_driver
use module_fr_fire_atm
implicit none

contains

subroutine fire_driver_em_init (grid , config_flags               & 
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe)

    

    USE module_domain , only: domain , get_ijk_from_subgrid
    USE module_configure , only : grid_config_rec_type
    implicit none

    TYPE(domain) , TARGET          :: grid   
    TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags
    integer, intent(in):: &
             ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe

    
    integer :: &  
             ifds,ifde, kfds,kfde, jfds,jfde,                              &
             ifms,ifme, kfms,kfme, jfms,jfme,                              &
             ifps,ifpe, kfps,kfpe, jfps,jfpe                              
    

    real,dimension(1,1,1)::rho,z_at_w,dz8w

    call message('fire_driver_em_init: FIRE initialization start')

    
    CALL get_ijk_from_subgrid (  grid ,                   &
                            ifds,ifde, jfds,jfde,kfds,kfde,                        &
                            ifms,ifme, jfms,jfme,kfms,kfme,                        &
                            ifps,ifpe, jfps,jfpe,kfps,kfpe) 

    call fire_driver_em ( grid , config_flags               & 
            ,1,3,0                        & 
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifps,ifpe, jfps,jfpe                                   &
            ) 

    call message('fire_driver_em_init: FIRE initialization complete')

end subroutine fire_driver_em_init





subroutine fire_driver_em_step (grid , config_flags               & 
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,rho,z_at_w,dz8w ) 

    

    USE module_domain, only: domain , get_ijk_from_subgrid
    USE module_configure , only : grid_config_rec_type
    USE module_fr_fire_util, only : fire_test_steps
    implicit none

    TYPE(domain) , TARGET          :: grid   
    TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags
    integer, intent(in):: &
             ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe
    real,dimension(ims:ime, kms:kme, jms:jme),intent(in)::rho,z_at_w,dz8w

    
    integer :: &  
             ifds,ifde, kfds,kfde, jfds,jfde,                              &
             ifms,ifme, kfms,kfme, jfms,jfme,                              &
             ifps,ifpe, kfps,kfpe, jfps,jfpe                              
    integer :: its,ite,jts,jte,kts,kte            
    integer:: ij 

    call message('fire_driver_em_step: FIRE step start')

    
    CALL get_ijk_from_subgrid (  grid ,                   &
                            ifds,ifde, jfds,jfde,kfds,kfde,                        &
                            ifms,ifme, jfms,jfme,kfms,kfme,                        &
                            ifps,ifpe, jfps,jfpe,kfps,kfpe) 

    call fire_driver_em ( grid , config_flags               & 
            ,3,6,fire_test_steps                                &
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifps,ifpe, jfps,jfpe                                   &
            ) 

    
    do ij=1,grid%num_tiles
       
       its = grid%i_start(ij)             
       ite = min(grid%i_end(ij),ide-1)    
       jts = grid%j_start(ij)             
       jte = min(grid%j_end(ij),jde-1)    
       kts=kds
       kte=kde

       call fire_tendency(                 &
            ids,ide-1, kds,kde, jds,jde-1, & 
            ims,ime, kms,kme, jms,jme,      &
            its,ite, kts,kte, jts,jte,      & 
            grid%grnhfx,grid%grnqfx,grid%canhfx,grid%canqfx,        & 
            config_flags%fire_ext_grnd,config_flags%fire_ext_crwn,config_flags%fire_crwn_hgt,                &
            grid%ht,z_at_w,dz8w,grid%mut,rho,          &
            grid%rthfrten,grid%rqvfrten)                

     enddo

       

       call print_3d_stats(its,ite,kts,kte,jts,jte,ims,ime,kms,kme,jms,jme,grid%rthfrten,'fire_driver_phys:rthfrten')
       call print_3d_stats(its,ite,kts,kte,jts,jte,ims,ime,kms,kme,jms,jme,grid%rqvfrten,'fire_driver_phys:rqvfrten')

    call message('fire_driver_em_step: FIRE step complete')
            
end subroutine fire_driver_em_step

end module module_fr_fire_driver_wrf

