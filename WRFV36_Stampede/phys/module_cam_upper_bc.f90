


module upper_bc









  use shr_kind_mod, only: r8 => shr_kind_r8




  use module_cam_support,       only: pcols, pverp, pcnst =>pcnst_runtime


  implicit none
  private
  save



  public :: ubc_defaultopts    
  public :: ubc_setopts        
  public :: ubc_init           



  public :: ubc_get_vals       


contains


subroutine ubc_defaultopts(tgcm_ubc_file_out, snoe_ubc_file_out)




   character(len=*), intent(out), optional :: tgcm_ubc_file_out
   character(len=*), intent(out), optional :: snoe_ubc_file_out


end subroutine ubc_defaultopts



subroutine ubc_setopts(tgcm_ubc_file_in, snoe_ubc_file_in)




   character(len=*), intent(in), optional :: tgcm_ubc_file_in
   character(len=*), intent(in), optional :: snoe_ubc_file_in


end subroutine ubc_setopts



  subroutine ubc_init





  end subroutine ubc_init




  subroutine ubc_get_vals (lchnk, ncol, ntop_molec, pint, zi, msis_temp, ubc_mmr)





    integer,  intent(in)  :: lchnk                 
    integer,  intent(in)  :: ncol                  
    integer,  intent(in)  :: ntop_molec            
    real(r8), intent(in)  :: pint(pcols,pverp)     
    real(r8), intent(in)  :: zi(pcols,pverp)       

    real(r8), intent(out) :: ubc_mmr(pcols,pcnst)  
    real(r8), intent(out) :: msis_temp(pcols)      

  end subroutine ubc_get_vals

end module upper_bc
