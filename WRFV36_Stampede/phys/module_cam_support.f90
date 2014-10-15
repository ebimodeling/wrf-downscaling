MODULE module_cam_support
  
  
  
  
  
  
  
  use module_state_description, only: param_num_moist
  use shr_kind_mod
  
  implicit none
  
  public
  save
  
  integer(SHR_KIND_IN),parameter,private :: R8 = SHR_KIND_R8 
  
  
  logical, parameter :: masterproc = .TRUE.
  logical, parameter :: iam = .FALSE.
  
  
  integer, parameter :: pcols = 1   
  integer :: pver                   
  integer :: pverp                  
  
  
  integer, parameter :: pcnst = param_num_moist  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  

  integer :: pcnst_runtime    = -999888777 

  
  
  
  integer :: pcnst_mp         = -999888777 
  
  integer :: gas_pcnst_modal_aero = -999888777 

  
  
  
  
  integer :: gas_pcnst_modal_aero_pos = -999888777 

  
  
  integer :: pcnst_non_chem_modal_aero = -999888777  
  
  
  character(len=750) :: iulog       
  
  
  
  integer, parameter, public :: phys_decomp=100
  
  
  integer, parameter :: fieldname_len = 16   

  
  integer, parameter :: nfs = -999888777 

  
  real(r8), parameter, public :: fillvalue = 1.e36_r8     

  
  
CONTAINS
  
  subroutine lower_case( txt_in, txt_lc )
    
    
    
    implicit none
    
    character(len=*), intent(in)  :: txt_in
    character(len=*), intent(out) :: txt_lc
    
    integer :: i, j
    integer, parameter :: iachar_lowera = iachar('a')
    integer, parameter :: iachar_uppera = iachar('A')
    integer, parameter :: iachar_upperz = iachar('Z')
    
    txt_lc = txt_in
    do i = 1, len( trim(txt_lc) )
       j = iachar( txt_lc(i:i) )
       if (j < iachar_uppera) cycle
       if (j > iachar_upperz) cycle
       txt_lc(i:i) = achar( j + iachar_lowera - iachar_uppera )
    end do
    
    return
  end subroutine lower_case
  
  
  
  SUBROUTINE endrun(msg)
    
    
    
    
    
    
    
    
    USE module_wrf_error
    
    
    character(len=*), intent(in), optional :: msg
    
    if(present(msg)) then
       call wrf_error_fatal3("<stdin>",137,&
msg)
    else
       
       call wrf_error_fatal3("<stdin>",141,&
iulog)
    endif
    
  END SUBROUTINE endrun
  
  
  
  
  SUBROUTINE t_stopf(event)
    
    
    
    
    
    
    character(len=*), intent(in) :: event 
    
  END SUBROUTINE t_stopf
  
  
  
  
  SUBROUTINE t_startf(event)
    
    
    
    
    
    
    
    character(len=*), intent(in) :: event
    
  END SUBROUTINE t_startf
  
  
  
  
  SUBROUTINE outfld( fname, field, idim, c)
    
    
    
    
    
    
    character(len=*), intent(in) :: fname
    integer,          intent(in) :: idim          
    integer,          intent(in) :: c             
    real(r8),         intent(in) :: field(idim,*)
    
  END SUBROUTINE outfld
  
  
  
  
  SUBROUTINE addfld(fname, units, numlev, avgflag, long_name, &
       decomp_type, flag_xyfill, flag_isccplev, sampling_seq)
    
    
    
    
    
    
    character(len=*), intent(in) :: fname     
    character(len=*), intent(in) :: units     
    character(len=1), intent(in) :: avgflag   
    character(len=*), intent(in) :: long_name 
    
    integer, intent(in) :: numlev             
    integer, intent(in) :: decomp_type        
    
    logical, intent(in), optional :: flag_xyfill
    logical, intent(in), optional :: flag_isccplev
    character(len=*), intent(in), optional :: sampling_seq
    
  END SUBROUTINE ADDFLD
  
  
  
  
  SUBROUTINE ADD_DEFAULT (name, tindex, flag)
    
    
    
    
    
    
    character(len=*), intent(in) :: name  
    character(len=1), intent(in) :: flag  
    
    integer, intent(in) :: tindex         
    
  END SUBROUTINE ADD_DEFAULT
  
END MODULE module_cam_support
