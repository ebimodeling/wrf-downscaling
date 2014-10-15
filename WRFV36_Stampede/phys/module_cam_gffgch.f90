








module module_cam_gffgch

  implicit none

  private
  public gffgch

contains


subroutine gffgch(t       ,es      ,itype   )

























   use shr_kind_mod, only: r8 => shr_kind_r8
   use physconst,    only: tmelt

   use module_cam_support, only: endrun, &
                                 iulog




    
   implicit none




   real(r8), intent(in) :: t          



   integer, intent(inout) :: itype   

   real(r8), intent(out) :: es         



   real(r8) e1         
   real(r8) e2         
   real(r8) eswtr      
   real(r8) f          
   real(r8) f1         
   real(r8) f2         
   real(r8) f3         
   real(r8) f4         
   real(r8) f5         
   real(r8) ps         
   real(r8) t0         
   real(r8) term1      
   real(r8) term2      
   real(r8) term3      
   real(r8) tr         
   real(r8) ts         
   real(r8) weight     
   integer itypo   





   if (itype < 0) then
      tr    = abs(real(itype,r8))
      itypo = itype
      itype = 1
   else
      tr    = 0.0_r8
      itypo = itype
   end if
   if (tr > 40.0_r8) then
      write(iulog,900) tr

      call wrf_message(iulog)

      call endrun ('GFFGCH')                
   end if

   if(t < (tmelt - tr) .and. itype == 1) go to 10



   ps = 1013.246_r8
   ts = 373.16_r8
   e1 = 11.344_r8*(1.0_r8 - t/ts)
   e2 = -3.49149_r8*(ts/t - 1.0_r8)
   f1 = -7.90298_r8*(ts/t - 1.0_r8)
   f2 = 5.02808_r8*log10(ts/t)
   f3 = -1.3816_r8*(10.0_r8**e1 - 1.0_r8)/10000000.0_r8
   f4 = 8.1328_r8*(10.0_r8**e2 - 1.0_r8)/1000.0_r8
   f5 = log10(ps)
   f  = f1 + f2 + f3 + f4 + f5
   es = (10.0_r8**f)*100.0_r8
   eswtr = es

   if(t >= tmelt .or. itype == 0) go to 20



10 continue
   t0    = tmelt
   term1 = 2.01889049_r8/(t0/t)
   term2 = 3.56654_r8*log(t0/t)
   term3 = 20.947031_r8*(t0/t)
   es    = 575.185606e10_r8*exp(-(term1 + term2 + term3))

   if (t < (tmelt - tr)) go to 20



   weight = min((tmelt - t)/tr,1.0_r8)
   es = weight*es + (1.0_r8 - weight)*eswtr

20 continue
   itype = itypo
   return

900 format('GFFGCH: FATAL ERROR ******************************',/, &
           'TRANSITION RANGE FOR WATER TO ICE SATURATION VAPOR', &
           ' PRESSURE, TR, EXCEEDS MAXIMUM ALLOWABLE VALUE OF', &
           ' 40.0 DEGREES C',/, ' TR = ',f7.2)

end subroutine gffgch

end module module_cam_gffgch
