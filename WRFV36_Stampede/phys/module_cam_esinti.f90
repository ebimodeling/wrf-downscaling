










module module_cam_esinti

  implicit none

  private
  public esinti

contains


subroutine esinti(epslon  ,latvap  ,latice  ,rh2o    ,cpair   ,tmelt   )












   use shr_kind_mod, only: r8 => shr_kind_r8
   use wv_saturation, only: gestbl
   implicit none




   real(r8), intent(in) :: epslon          
   real(r8), intent(in) :: latvap          
   real(r8), intent(in) :: latice          
   real(r8), intent(in) :: rh2o            
   real(r8), intent(in) :: cpair           
   real(r8), intent(in) :: tmelt           



   real(r8) tmn             
   real(r8) tmx             
   real(r8) trice           
   logical ip           








   tmn   = 173.16_r8

   tmx   = 375.16_r8
   trice =  20.00_r8
   ip    = .true.



   call gestbl(tmn     ,tmx     ,trice   ,ip      ,epslon  , &
               latvap  ,latice  ,rh2o    ,cpair   ,tmelt )

   return
end subroutine esinti

end module module_cam_esinti



