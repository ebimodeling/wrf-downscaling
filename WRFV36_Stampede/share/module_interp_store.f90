module module_interp_store
  
  
  
  
  
  
  
  
  
  
  
  
  implicit none

  integer, pointer, dimension(:,:) :: IIH,JJH,IIV,JJV
  real, pointer, dimension(:,:) :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
  real, pointer, dimension(:,:) :: VBWGT1,VBWGT2,VBWGT3,VBWGT4

  integer :: grid_id, parent_grid_id
  integer, pointer, dimension(:,:,:) :: iinfo,parent_iinfo, &
                                        iinfo_bxs, iinfo_bxe, &
                                        iinfo_bys, iinfo_bye
  real, pointer, dimension(:,:,:) :: winfo,parent_winfo, &
                                     winfo_bxs, winfo_bxe, &
                                     winfo_bys, winfo_bye
  integer, pointer, dimension(:,:) :: hnear_i, hnear_j

  real, pointer, dimension(:,:) :: parent_fis, nest_fis

end module module_interp_store

subroutine store_interp_info(grid, parent_grid)
  use module_domain_type, only : domain
  use module_interp_store
  implicit none
  type(domain), intent(in) :: grid, parent_grid
  
end subroutine store_interp_info

