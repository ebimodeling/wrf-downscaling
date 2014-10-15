


module constituents














  use shr_kind_mod, only: r8 => shr_kind_r8
  use physconst,    only: r_universal






  use module_cam_support,   only: masterproc,endrun,iulog,pcnst =>pcnst_runtime

  implicit none
  private
  save



  public cnst_add             
  public cnst_num_avail       
  public cnst_get_ind         
  public cnst_get_type_byind  
  public cnst_get_type_byname 
  public cnst_read_iv         
  public cnst_chk_dim         
  public cnst_cam_outfld      








  character(len=16),allocatable, public :: cnst_name(:)     
  character(len=128),allocatable,public :: cnst_longname(:) 



  logical, public :: readtrace = .true.             



real(r8),      allocatable, public :: cnst_cp  (:)          
  real(r8),    allocatable, public :: cnst_cv  (:)          
  real(r8),    allocatable, public :: cnst_mw  (:)          
  character*3, allocatable, public :: cnst_type(:)          
  real(r8),    allocatable, public :: cnst_rgas(:)          
  real(r8),    allocatable, public :: qmin     (:)          
  real(r8),    allocatable, public :: qmincg   (:)          
  logical,     allocatable, public :: cnst_fixed_ubc(:)     



   character(len=16), allocatable, public :: apcnst    (:)   
   character(len=16), allocatable, public :: bpcnst    (:)   
   character(len=16), allocatable, public :: hadvnam   (:)   
   character(len=16), allocatable, public :: vadvnam   (:)   
   character(len=16), allocatable, public :: dcconnam  (:)   
   character(len=16), allocatable, public :: fixcnam   (:)   
   character(len=16), allocatable, public :: tendnam   (:)   
   character(len=16), allocatable, public :: ptendnam  (:)   
   character(len=16), allocatable, public :: dmetendnam(:)   
   character(len=16), allocatable, public :: sflxnam   (:)   
   character(len=16), allocatable, public :: tottnam   (:)   



  integer :: padv = 0                      
  logical, allocatable :: read_init_vals(:)         
  logical, allocatable :: cam_outfld_(:)            
                                           
                                           


CONTAINS


  subroutine cnst_add (name, mwc, cpc, qminc, &
                       ind, longname, readiv, mixtype, cam_outfld, fixed_ubc)







    character(len=*), intent(in) :: &
       name      
    real(r8),intent(in)    :: mwc    
    real(r8),intent(in)    :: cpc    
    real(r8),intent(in)    :: qminc  
                                     
    integer, intent(out)   :: ind    

    character(len=*), intent(in), optional :: &
       longname    
    logical,          intent(in), optional :: &
       readiv      
    character(len=*), intent(in), optional :: &
       mixtype     
    logical,          intent(in), optional :: &
       cam_outfld  
    logical,          intent(in), optional :: &
       fixed_ubc 


    
    if(.NOT. allocated(read_init_vals)) allocate(read_init_vals(pcnst))
    if(.NOT. allocated(cam_outfld_)) allocate(cam_outfld_(pcnst))

    padv = padv+1
    ind  = padv
    if (padv > pcnst) then
       write(iulog,*) 'CNST_ADD: advected tracer index greater than pcnst = ', pcnst
       call wrf_message(iulog)
       call endrun
    end if


    cnst_name(ind) = name
    if ( present(longname) )then
       cnst_longname(ind) = longname
    else
       cnst_longname(ind) = name
    end if


    if ( present(readiv) ) then
       read_init_vals(ind) = readiv
    else
       read_init_vals(ind) = readtrace
    end if


    if ( present(mixtype) )then
       cnst_type(ind) = mixtype
    else
       cnst_type(ind) = 'wet'
    end if



    if ( present(cam_outfld) ) then
       cam_outfld_(ind) = cam_outfld
    else
       cam_outfld_(ind) = .true.
    end if


    if ( present(fixed_ubc) ) then
       cnst_fixed_ubc(ind) = fixed_ubc
    else
       cnst_fixed_ubc(ind) = .false.
    end if

    cnst_cp  (ind) = cpc
    cnst_mw  (ind) = mwc
    qmin     (ind) = qminc
    qmincg   (ind) = qminc
    if (ind == 1) qmincg = 0._r8  

    cnst_rgas(ind) = r_universal * mwc
    cnst_cv  (ind) = cpc - cnst_rgas(ind)

    return
  end subroutine cnst_add



  function cnst_num_avail()

     

     integer cnst_num_avail

     cnst_num_avail = pcnst - padv

  end function cnst_num_avail



  subroutine cnst_get_ind (name, ind, abort)






    use module_cam_support, only: lower_case, pcnst_runtime


    character(len=*),  intent(in)  :: name  
    integer,           intent(out) :: ind   
    logical, optional, intent(in)  :: abort 


    integer :: m                                   
    logical :: abort_on_error
    character(len=32) :: name_in, name_in_lc, name_cnst_lc
    integer           :: idone



    name_in = name
    call lower_case( name_in, name_in_lc )
    idone = 0
    do while (idone < 2)
       do m = 1, pcnst_runtime
          call lower_case( cnst_name(m), name_cnst_lc )
          if (name_in_lc == name_cnst_lc) then
             ind = m
             return
          end if
       end do
       idone = idone + 1
       
       if (name_in_lc == 'h2so4') then
          name_in_lc = 'sulf'
       else
          idone = 2
       end if
    end do 

    abort_on_error = .true.
    if ( present(abort) ) abort_on_error = abort

    if ( abort_on_error ) then
       write(iulog,*) 'CNST_GET_IND, name:', name,  ' not found in list:', cnst_name(:)
       call wrf_message(iulog)
       call endrun('CNST_GET_IND: name not found')
    end if


    ind = -1

  end subroutine cnst_get_ind



  character*3 function cnst_get_type_byind (ind)












    integer, intent(in)   :: ind    


    integer :: m                                   



    if (ind.le.pcnst) then
       cnst_get_type_byind = cnst_type(ind)
    else
       
       write(iulog,*) 'CNST_GET_TYPE_BYIND, ind:', ind
       call wrf_message(iulog)
       call endrun
    endif


  end function cnst_get_type_byind



  character*3 function cnst_get_type_byname (name)












    character(len=*), intent(in) :: name 


    integer :: m                                   



    do m = 1, pcnst
       if (name == cnst_name(m)) then
          cnst_get_type_byname = cnst_type(m)
          return
       end if
    end do


    write(iulog,*) 'CNST_GET_TYPE_BYNAME, name:', name,  ' not found in list:', cnst_name(:)
       call wrf_message(iulog)
    call endrun

  end function cnst_get_type_byname


  function cnst_read_iv(m)








    integer, intent(in) :: m    

    logical :: cnst_read_iv     


    cnst_read_iv = read_init_vals(m)
 end function cnst_read_iv


  subroutine cnst_chk_dim











    integer i,m


    if (padv /= pcnst) then
       write(iulog,*)'CNST_CHK_DIM: number of advected tracer ',padv, ' not equal to pcnst = ',pcnst
       call wrf_message(iulog)
       call endrun ()
    endif

    if (masterproc) then
       write(iulog,*) 'Advected constituent list:'
       call wrf_message(iulog)
       do i = 1, pcnst
          write(iulog,'(i4,2x,a8,2x,a128,2x,a3)') i, cnst_name(i), cnst_longname(i), cnst_type(i)
       call wrf_message(iulog)
       end do
    end if

    
    do m=1,pcnst
       apcnst    (m)  = trim(cnst_name(m))//'AP'
       bpcnst    (m)  = trim(cnst_name(m))//'BP'
       hadvnam   (m)  = 'HA'//cnst_name(m)
       vadvnam   (m)  = 'VA'//cnst_name(m)
       fixcnam   (m)  = 'DF'//cnst_name(m)
       tendnam   (m)  = 'TE'//cnst_name(m)
       ptendnam  (m)  = 'PTE'//cnst_name(m)
       dmetendnam(m)  = 'DME'//cnst_name(m)
       tottnam   (m)  = 'TA'//cnst_name(m)
       sflxnam(m)     = 'SF'//cnst_name(m)
    end do


  end subroutine cnst_chk_dim



function cnst_cam_outfld(m)






   integer, intent(in) :: m                
   logical             :: cnst_cam_outfld  


   cnst_cam_outfld = cam_outfld_(m)

end function cnst_cam_outfld



end module constituents
