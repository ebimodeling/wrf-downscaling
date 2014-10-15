



module cldwat

















   use shr_kind_mod,  only: r8 => shr_kind_r8




   use wv_saturation, only: estblf, hlatv, tmin, hlatf, rgasv, pcf, &
                            cp, epsqs, ttrice




   use module_cam_support, only: masterproc, &
        pcols, pver, pverp, &
        endrun, &
        iulog


   implicit none




   private
   save



   public cldwat_fice          
   public cldwat_readnl
   integer, public::  ktop      




   real(r8),public :: icritw = 2.0e-4
   real(r8),public :: icritc = 45.0e-6
   real(r8),public :: conke  = 5.0e-6





   real(r8), private:: tmax_fice
   real(r8), private:: tmin_fice
   real(r8), private:: tmax_fsnow 
   real(r8), private:: tmin_fsnow 
   real(r8), private:: rhonot   
   real(r8), private:: t0       
   real(r8), private:: cldmin   
   real(r8), private:: small    
   real(r8), private:: c        
   real(r8), private:: d        
   real(r8), private:: esi      
   real(r8), private:: esw      
   real(r8), private:: nos      
   real(r8), private:: pi       
   real(r8), private:: gravit   
   real(r8), private:: rh2o
   real(r8), private:: prhonos
   real(r8), private:: thrpd    
   real(r8), private:: gam3pd   
   real(r8), private:: gam4pd   
   real(r8), private:: rhoi     
   real(r8), private:: rhos     
   real(r8), private:: rhow     
   real(r8), private:: mcon01   
   real(r8), private:: mcon02   
   real(r8), private:: mcon03   
   real(r8), private:: mcon04   
   real(r8), private:: mcon05   
   real(r8), private:: mcon06   
   real(r8), private:: mcon07   
   real(r8), private:: mcon08   

   integer, private ::  k1mb    


   real(r8) :: capnsi               
   real(r8) :: capnc                
   real(r8) :: capnw                
   real(r8) :: kconst               
   real(r8) :: effc                 
   real(r8) :: alpha                
   real(r8) :: capc                 
   real(r8) :: convfw               
   real(r8) :: cracw                
   real(r8) :: critpr               
   real(r8) :: ciautb               

  
  real(r8), parameter :: unset_r8 = huge(1.0_r8)

contains

  subroutine cldwat_readnl(nlfile)

   character(len=*), intent(in) :: nlfile  
end subroutine cldwat_readnl


  subroutine cldwat_fice(ncol, t, fice, fsnow)








    use physconst,          only: tmelt
    implicit none


    integer,  intent(in)  :: ncol                 
    real(r8), intent(in)  :: t(pcols,pver)        

    real(r8), intent(out) :: fice(pcols,pver)     
    real(r8), intent(out) :: fsnow(pcols,pver)    


    integer :: i,k                                   



    
    
    tmax_fice = tmelt    - 10._r8
    tmin_fice = tmax_fice - 30._r8
    tmax_fsnow = tmelt
    tmin_fsnow = tmelt   - 5._r8

    

    do k=1,pver
       do i=1,ncol


          if (t(i,k) > tmax_fice) then
             fice(i,k) = 0.0_r8


          else if (t(i,k) < tmin_fice) then
             fice(i,k) = 1.0_r8


          else 
             fice(i,k) =(tmax_fice - t(i,k)) / (tmax_fice - tmin_fice)
          end if




          if (t(i,k) > tmax_fsnow) then
             fsnow(i,k) = 0.0_r8


          else if (t(i,k) < tmin_fsnow) then
             fsnow(i,k) = 1.0_r8


          else 
             fsnow(i,k) =(tmax_fsnow - t(i,k)) / (tmax_fsnow - tmin_fsnow)
          end if

       end do
    end do

    return
  end subroutine cldwat_fice
end module cldwat
