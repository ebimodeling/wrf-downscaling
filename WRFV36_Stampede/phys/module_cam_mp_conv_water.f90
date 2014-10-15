

  module conv_water

   
   
   
   
   
   
   
   
   
   
   
   
   
   

  use shr_kind_mod,  only: r8=>shr_kind_r8



  use module_cam_support, only: pcols, pver, pverp

  use physconst,     only: gravit, latvap, latice






  use module_cam_support, only: endrun, iulog


  implicit none
  private
  save

  public :: conv_water_register, conv_water_4rad, conv_water_init



  integer :: icwmrsh_idx, icwmrdp_idx, fice_idx, sh_frac_idx, dp_frac_idx, concldql_idx, &
             ast_idx, alst_idx, aist_idx, qlst_idx, qist_idx, sh_cldliq1_idx, sh_cldice1_idx

  contains

  

  subroutine conv_water_register

  
  
  
  
  

  end subroutine conv_water_register


  
  
  

   subroutine conv_water_init()
   
   
   
   
   implicit none
   end subroutine conv_water_init
     
   subroutine conv_water_4rad( lchnk, ncol, ast, sh_icwmr, dp_icwmr, &
        fice, sh_frac, dp_frac, conv_water_mode, rei, pdel, ls_liq,  &
        ls_ice, totg_liq, totg_ice )

   
   
   
   
   
   
   
   
   
   
   
   
   
   
   use module_cam_support, only: outfld
   
   implicit none

   
   
   
   integer,  intent(in) :: lchnk
   integer,  intent(in) :: ncol
   integer,  intent(in) :: conv_water_mode
   real(r8), intent(in) :: rei(pcols,pver)        
   real(r8), intent(in) :: pdel(pcols,pver)       
   real(r8), intent(in) :: ls_liq(pcols,pver)     
   real(r8), intent(in) :: ls_ice(pcols,pver)     
   real(r8), intent(in) :: ast(pcols,pver)
   real(r8), intent(in) :: sh_icwmr(pcols,pver)
   real(r8), intent(in) :: dp_icwmr(pcols,pver)
   real(r8), intent(in) :: fice(pcols,pver)
   real(r8), intent(in) :: sh_frac(pcols,pver)
   real(r8), intent(in) :: dp_frac(pcols,pver)
   real(r8), intent(out):: totg_ice(pcols,pver)   
   real(r8), intent(out):: totg_liq(pcols,pver)   

   
   
   

   
   real(r8), dimension(pcols,pver) ::  sh_cldliq 
   real(r8), dimension(pcols,pver) ::  sh_cldice 

   

   real(r8) :: conv_ice(pcols,pver)               
   real(r8) :: conv_liq(pcols,pver)               
   real(r8) :: tot_ice(pcols,pver)                
   real(r8) :: tot_liq(pcols,pver)                

   integer  :: i,k,itim                           
   real(r8) :: cu_icwmr                           
   real(r8) :: ls_icwmr                           
   real(r8) :: tot_icwmr                          
   real(r8) :: ls_frac                            
   real(r8) :: tot0_frac, cu0_frac, dp0_frac, sh0_frac 
   real(r8) :: kabs, kabsi, kabsl, alpha, dp0, sh0, ic_limit, frac_limit  
   real(r8) :: wrk1         

   
   
   

   parameter( kabsl = 0.090361_r8, frac_limit = 0.01_r8, ic_limit = 1.e-12_r8 )

 

   character(len=16) :: microp_scheme 
   microp_scheme = 'MG'

 

   
   
   
   
   

   do k = 1, pver
   do i = 1, ncol

      if( sh_frac(i,k) <= frac_limit .or. sh_icwmr(i,k) <= ic_limit ) then
          sh0_frac = 0._r8
      else
          sh0_frac = sh_frac(i,k)
      endif
      if( dp_frac(i,k) <= frac_limit .or. dp_icwmr(i,k) <= ic_limit ) then
          dp0_frac = 0._r8
      else
          dp0_frac = dp_frac(i,k)
      endif
      cu0_frac = sh0_frac + dp0_frac

    

      wrk1 = min(1._r8,max(0._r8, ls_ice(i,k)/(ls_ice(i,k)+ls_liq(i,k)+1.e-36_r8)))

      if( ( cu0_frac < frac_limit ) .or. ( ( sh_icwmr(i,k) + dp_icwmr(i,k) ) < ic_limit ) ) then

            cu0_frac = 0._r8
            cu_icwmr = 0._r8
         
            ls_frac = ast(i,k)
            if( ls_frac < frac_limit ) then
                ls_frac  = 0._r8
                ls_icwmr = 0._r8
            else
                ls_icwmr = ( ls_liq(i,k) + ls_ice(i,k) )/max(frac_limit,ls_frac) 
            end if

            tot0_frac = ls_frac
            tot_icwmr = ls_icwmr
           
      else

          
            
            if( microp_scheme .eq. 'MG' ) then
                kabsi = 0.005_r8 + 1._r8/min(max(13._r8,rei(i,k)),130._r8)
            elseif( microp_scheme .eq. 'RK' ) then
                kabsi = 0.005_r8 + 1._r8/rei(i,k)
            endif
            kabs  = kabsl * ( 1._r8 - wrk1 ) + kabsi * wrk1
            alpha = -1.66_r8*kabs*pdel(i,k)/gravit*1000.0_r8

          

            select case (conv_water_mode) 
            case (1) 
               cu_icwmr = ( sh0_frac * sh_icwmr(i,k) + dp0_frac*dp_icwmr(i,k))/max(frac_limit,cu0_frac)
            case (2)
               sh0 = exp(alpha*sh_icwmr(i,k))
               dp0 = exp(alpha*dp_icwmr(i,k))               
               cu_icwmr = log((sh0_frac*sh0+dp0_frac*dp0)/max(frac_limit,cu0_frac))
               cu_icwmr = cu_icwmr/alpha
            case default 

            end select

          
          

            ls_frac   = ast(i,k) 
            ls_icwmr  = (ls_liq(i,k) + ls_ice(i,k))/max(frac_limit,ls_frac) 
            tot0_frac = (ls_frac + cu0_frac) 

            select case (conv_water_mode) 
            case (1) 
               tot_icwmr = (ls_frac*ls_icwmr + cu0_frac*cu_icwmr)/max(frac_limit,tot0_frac)
            case (2)
               tot_icwmr = log((ls_frac*exp(alpha*ls_icwmr)+cu0_frac*exp(alpha*cu_icwmr))/max(frac_limit,tot0_frac))
               tot_icwmr = tot_icwmr/alpha
            case default 

            end select

      end if

    
    
    

      conv_ice(i,k) = cu_icwmr * wrk1
      conv_liq(i,k) = cu_icwmr * (1._r8-wrk1)

      tot_ice(i,k)  = tot_icwmr * wrk1
      tot_liq(i,k)  = tot_icwmr * (1._r8-wrk1)

      totg_ice(i,k) = tot0_frac * tot_icwmr * wrk1
      totg_liq(i,k) = tot0_frac * tot_icwmr * (1._r8-wrk1)

   end do
   end do


   sh_cldliq(:ncol,:pver)= sh_icwmr(:ncol,:pver)*(1-fice(:ncol,:pver))*sh_frac(:ncol,:pver)
   sh_cldice(:ncol,:pver)=sh_icwmr(:ncol,:pver)*fice(:ncol,:pver)*sh_frac(:ncol,:pver)

  
   
   call outfld( 'ICLMRCU ', conv_liq  , pcols, lchnk )
   call outfld( 'ICIMRCU ', conv_ice  , pcols, lchnk )
   call outfld( 'ICWMRSH ', sh_icwmr  , pcols, lchnk )
   call outfld( 'ICWMRDP ', dp_icwmr  , pcols, lchnk ) 
   call outfld( 'ICLMRTOT', tot_liq   , pcols, lchnk )
   call outfld( 'ICIMRTOT', tot_ice   , pcols, lchnk )
   call outfld( 'SH_CLD  ', sh_frac   , pcols, lchnk )
   call outfld( 'DP_CLD  ', dp_frac   , pcols, lchnk )

  end subroutine conv_water_4rad

end module conv_water
