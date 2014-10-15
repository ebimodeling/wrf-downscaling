












module wv_saturation
  use shr_kind_mod, only: r8 => shr_kind_r8




  use module_cam_support, only: endrun, iulog
  use module_wrf_error


  implicit none
  private
  save



  public gestbl   
  public estblf   
  public aqsat    



  public vqsatd   
  public fqsatd   
  public qsat_water  





  public vqsatd_water





  public vqsatd2
  public vqsatd2_single
  public polysvp



  public hlatv, tmin, hlatf, rgasv, pcf, cp, epsqs, ttrice



  integer plenest  
  parameter (plenest=250)





  real(r8) estbl(plenest)      
  real(r8) tmin       
  real(r8) tmax       
  real(r8) ttrice     
  real(r8) pcf(6)     
  real(r8) epsqs      
  real(r8) rgasv      
  real(r8) hlatf      
  real(r8) hlatv      
  real(r8) cp         
  real(r8) tmelt      
  logical icephs  

contains

   real(r8) function estblf( td )



   real(r8), intent(in) :: td         

   real(r8) :: e       
   real(r8) :: ai
   integer  :: i

   e = max(min(td,tmax),tmin)   
   i = int(e-tmin)+1
   ai = aint(e-tmin)
   estblf = (tmin+ai-e+1._r8)* &
            estbl(i)-(tmin+ai-e)* &
            estbl(i+1)
   end function estblf

subroutine gestbl(tmn     ,tmx     ,trice   ,ip      ,epsil   , &
                  latvap  ,latice  ,rh2o    ,cpair   ,tmeltx   )

















   use module_cam_support, only: masterproc
   use module_cam_gffgch





   real(r8), intent(in) :: tmn           
   real(r8), intent(in) :: tmx           
   real(r8), intent(in) :: epsil         
   real(r8), intent(in) :: trice         
   real(r8), intent(in) :: latvap        
   real(r8), intent(in) :: latice        
   real(r8), intent(in) :: rh2o          
   real(r8), intent(in) :: cpair         
   real(r8), intent(in) :: tmeltx        



   real(r8) t             
   integer n          
   integer lentbl     
   integer itype      


   logical ip         





   tmin   = tmn       
   tmax   = tmx       
   ttrice = trice     
   icephs = ip        



   epsqs  = epsil
   hlatv  = latvap
   hlatf  = latice
   rgasv  = rh2o
   cp     = cpair
   tmelt  = tmeltx

   lentbl = INT(tmax-tmin+2.000001_r8)
   if (lentbl .gt. plenest) then
      write(iulog,9000) tmax, tmin, plenest

      call wrf_message(iulog)

      call endrun ('GESTBL')    
   end if





   if (icephs) then
      if (ttrice /= 0.0_r8) then
         itype = -ttrice
      else
         itype = 1
      end if
   else
      itype = 0
   end if

   t = tmin - 1.0_r8
   do n=1,lentbl
      t = t + 1.0_r8
      call gffgch(t,estbl(n),itype)
   end do

   do n=lentbl+1,plenest
      estbl(n) = -99999.0_r8
   end do








   pcf(1) =  5.04469588506e-01_r8
   pcf(2) = -5.47288442819e+00_r8
   pcf(3) = -3.67471858735e-01_r8
   pcf(4) = -8.95963532403e-03_r8
   pcf(5) = -7.78053686625e-05_r8










   if (masterproc) then
      write(iulog,*)' *** SATURATION VAPOR PRESSURE TABLE COMPLETED ***'

      call wrf_message(iulog)

   end if

   return

9000 format('GESTBL: FATAL ERROR *********************************',/, &
            ' TMAX AND TMIN REQUIRE A LARGER DIMENSION ON THE LENGTH', &
            ' OF THE SATURATION VAPOR PRESSURE TABLE ESTBL(PLENEST)',/, &
            ' TMAX, TMIN, AND PLENEST => ', 2f7.2, i3)

end subroutine gestbl

subroutine aqsat(t       ,p       ,es      ,qs        ,ii      , &
                 ilen    ,kk      ,kstart  ,kend      )



















   integer, intent(in) :: ii             
   integer, intent(in) :: kk             
   integer, intent(in) :: ilen           
   integer, intent(in) :: kstart         
   integer, intent(in) :: kend           
   real(r8), intent(in) :: t(ii,kk)          
   real(r8), intent(in) :: p(ii,kk)          



   real(r8), intent(out) :: es(ii,kk)         
   real(r8), intent(out) :: qs(ii,kk)         



   real(r8) omeps             
   integer i, k           



   omeps = 1.0_r8 - epsqs
   do k=kstart,kend
      do i=1,ilen
         es(i,k) = estblf(t(i,k))



         qs(i,k) = epsqs*es(i,k)/(p(i,k) - omeps*es(i,k))




         qs(i,k) = min(1.0_r8,qs(i,k))

         if (qs(i,k) < 0.0_r8) then
            qs(i,k) = 1.0_r8
            es(i,k) = p(i,k)
         end if
      end do
   end do

   return
end subroutine aqsat


subroutine vqsatd(t       ,p       ,es      ,qs      ,gam      , &
                  len     )
















   integer, intent(in) :: len       
   real(r8), intent(in) :: t(len)       
   real(r8), intent(in) :: p(len)       



   real(r8), intent(out) :: es(len)   
   real(r8), intent(out) :: qs(len)   
   real(r8), intent(out) :: gam(len)  



   logical lflg   

   integer i      

   real(r8) omeps     
   real(r8) trinv     
   real(r8) tc        
   real(r8) weight    
   real(r8) hltalt    

   real(r8) hlatsb    
   real(r8) hlatvp    
   real(r8) tterm     
   real(r8) desdt     



   omeps = 1.0_r8 - epsqs
   do i=1,len
      es(i) = estblf(t(i))



      qs(i) = epsqs*es(i)/(p(i) - omeps*es(i))




      qs(i) = min(1.0_r8,qs(i))

      if (qs(i) < 0.0_r8) then
         qs(i) = 1.0_r8
         es(i) = p(i)
      end if
   end do




   trinv = 0.0_r8
   if ((.not. icephs) .or. (ttrice.eq.0.0_r8)) go to 10
   trinv = 1.0_r8/ttrice
   do i=1,len








      tc     = t(i) - tmelt
      lflg   = (tc >= -ttrice .and. tc < 0.0_r8)
      weight = min(-tc*trinv,1.0_r8)
      hlatsb = hlatv + weight*hlatf
      hlatvp = hlatv - 2369.0_r8*tc
      if (t(i) < tmelt) then
         hltalt = hlatsb
      else
         hltalt = hlatvp
      end if
      if (lflg) then
         tterm = pcf(1) + tc*(pcf(2) + tc*(pcf(3) + tc*(pcf(4) + tc*pcf(5))))
      else
         tterm = 0.0_r8
      end if
      desdt  = hltalt*es(i)/(rgasv*t(i)*t(i)) + tterm*trinv
      gam(i) = hltalt*qs(i)*p(i)*desdt/(cp*es(i)*(p(i) - omeps*es(i)))
      if (qs(i) == 1.0_r8) gam(i) = 0.0_r8
   end do
   return



10 do i=1,len




      hlatvp = hlatv - 2369.0_r8*(t(i)-tmelt)
      if (icephs) then
         hlatsb = hlatv + hlatf
      else
         hlatsb = hlatv
      end if
      if (t(i) < tmelt) then
         hltalt = hlatsb
      else
         hltalt = hlatvp
      end if
      desdt  = hltalt*es(i)/(rgasv*t(i)*t(i))
      gam(i) = hltalt*qs(i)*p(i)*desdt/(cp*es(i)*(p(i) - omeps*es(i)))
      if (qs(i) == 1.0_r8) gam(i) = 0.0_r8
   end do

   return

end subroutine vqsatd


subroutine vqsatd_water(t       ,p       ,es      ,qs      ,gam      , &
                        len     )





   integer, intent(in) :: len       
   real(r8), intent(in) :: t(len)       
   real(r8), intent(in) :: p(len)       




   real(r8), intent(out) :: es(len)   
   real(r8), intent(out) :: qs(len)   
   real(r8), intent(out) :: gam(len)  





   integer i      

   real(r8) omeps     
   real(r8) hltalt    

   real(r8) hlatsb    
   real(r8) hlatvp    
   real(r8) desdt     



   omeps = 1.0_r8 - epsqs
   do i=1,len
      es(i) = polysvp(t(i),0)



      qs(i) = epsqs*es(i)/(p(i) - omeps*es(i))




      qs(i) = min(1.0_r8,qs(i))

      if (qs(i) < 0.0_r8) then
         qs(i) = 1.0_r8
         es(i) = p(i)
      end if
   end do



   do i=1,len




      hlatvp = hlatv - 2369.0_r8*(t(i)-tmelt)
      hlatsb = hlatv
      if (t(i) < tmelt) then
         hltalt = hlatsb
      else
         hltalt = hlatvp
      end if
      desdt  = hltalt*es(i)/(rgasv*t(i)*t(i))
      gam(i) = hltalt*qs(i)*p(i)*desdt/(cp*es(i)*(p(i) - omeps*es(i)))
      if (qs(i) == 1.0_r8) gam(i) = 0.0_r8
   end do

   return

end subroutine vqsatd_water

      function polysvp (T,type)







      real(r8) dum

      real(r8) T,polysvp

      integer type



      if (type.eq.1) then



         polysvp = 10._r8**(-9.09718_r8*(273.16_r8/t-1._r8)-3.56654_r8* &
          log10(273.16_r8/t)+0.876793_r8*(1._r8-t/273.16_r8)+ &
          log10(6.1071_r8))*100._r8

      end if



      if (type.eq.0) then
         polysvp = 10._r8**(-7.90298_r8*(373.16_r8/t-1._r8)+ &
             5.02808_r8*log10(373.16_r8/t)- &
             1.3816e-7_r8*(10._r8**(11.344_r8*(1._r8-t/373.16_r8))-1._r8)+ &
             8.1328e-3_r8*(10._r8**(-3.49149_r8*(373.16_r8/t-1._r8))-1._r8)+ &
             log10(1013.246_r8))*100._r8
         end if


      end function polysvp


integer function fqsatd(t    ,p    ,es    ,qs   ,gam   , len     )
  
  
  
  
  
  integer, intent(in) :: len       
  real(r8), intent(in) :: t(len)       
  real(r8), intent(in) :: p(len)       
  
  real(r8), intent(out) :: es(len)   
  real(r8), intent(out) :: qs(len)   
  real(r8), intent(out) :: gam(len)  
  
  call vqsatd(t       ,p       ,es      ,qs      ,gam  , len     )
  fqsatd = 1
  return
end function fqsatd

real(r8) function qsat_water(t,p)
  
  real(r8) t 
  real(r8) p 
  real(r8) es 
  real(r8) ps, ts, e1, e2, f1, f2, f3, f4, f5, f
  
  
  
  

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

  qsat_water = epsqs*es/(p-(1.-epsqs)*es) 
  if(qsat_water < 0.) qsat_water = 1.

end function qsat_water
subroutine vqsatd2(t       ,p       ,es      ,qs      ,dqsdt      , &
                   len     )



















   integer, intent(in) :: len       
   real(r8), intent(in) :: t(len)       
   real(r8), intent(in) :: p(len)       



   real(r8), intent(out) :: es(len)   
   real(r8), intent(out) :: qs(len)   
 
 
   real(r8), intent(out) :: dqsdt(len)  
 




   logical lflg   

   integer i      

   real(r8) omeps     
   real(r8) trinv     
   real(r8) tc        
   real(r8) weight    
   real(r8) hltalt    

   real(r8) hlatsb    
   real(r8) hlatvp    
   real(r8) tterm     
   real(r8) desdt     

 
   real(r8) gam(len)  
 



   omeps = 1.0_r8 - epsqs
   do i=1,len
      es(i) = estblf(t(i))



      qs(i) = epsqs*es(i)/(p(i) - omeps*es(i))




      qs(i) = min(1.0_r8,qs(i))

      if (qs(i) < 0.0_r8) then
         qs(i) = 1.0_r8
         es(i) = p(i)
      end if
   end do




   trinv = 0.0_r8
   if ((.not. icephs) .or. (ttrice.eq.0.0_r8)) go to 10
   trinv = 1.0_r8/ttrice
   do i=1,len








      tc     = t(i) - tmelt
      lflg   = (tc >= -ttrice .and. tc < 0.0_r8)
      weight = min(-tc*trinv,1.0_r8)
      hlatsb = hlatv + weight*hlatf
      hlatvp = hlatv - 2369.0_r8*tc
      if (t(i) < tmelt) then
         hltalt = hlatsb
      else
         hltalt = hlatvp
      end if
      if (lflg) then
         tterm = pcf(1) + tc*(pcf(2) + tc*(pcf(3) + tc*(pcf(4) + tc*pcf(5))))
      else
         tterm = 0.0_r8
      end if
      desdt  = hltalt*es(i)/(rgasv*t(i)*t(i)) + tterm*trinv
      gam(i) = hltalt*qs(i)*p(i)*desdt/(cp*es(i)*(p(i) - omeps*es(i)))
      if (qs(i) == 1.0_r8) gam(i) = 0.0_r8
    
      dqsdt(i) = (cp/hltalt)*gam(i)
    
   end do
   return



10 do i=1,len




      hlatvp = hlatv - 2369.0_r8*(t(i)-tmelt)
      if (icephs) then
         hlatsb = hlatv + hlatf
      else
         hlatsb = hlatv
      end if
      if (t(i) < tmelt) then
         hltalt = hlatsb
      else
         hltalt = hlatvp
      end if
      desdt  = hltalt*es(i)/(rgasv*t(i)*t(i))
      gam(i) = hltalt*qs(i)*p(i)*desdt/(cp*es(i)*(p(i) - omeps*es(i)))
      if (qs(i) == 1.0_r8) gam(i) = 0.0_r8
    
      dqsdt(i) = (cp/hltalt)*gam(i)
    
   end do

   return

end subroutine vqsatd2




subroutine vqsatd2_single(t       ,p       ,es      ,qs      ,dqsdt)



















   real(r8), intent(in) :: t       
   real(r8), intent(in) :: p       



   real(r8), intent(out) :: es     
   real(r8), intent(out) :: qs     
 
 
   real(r8), intent(out) :: dqsdt  
 




   logical lflg   



   real(r8) omeps     
   real(r8) trinv     
   real(r8) tc        
   real(r8) weight    
   real(r8) hltalt    

   real(r8) hlatsb    
   real(r8) hlatvp    
   real(r8) tterm     
   real(r8) desdt     

 
   real(r8) gam       
 



   omeps = 1.0_r8 - epsqs



      es = estblf(t)



      qs = epsqs*es/(p - omeps*es)




      qs = min(1.0_r8,qs)

      if (qs < 0.0_r8) then
         qs = 1.0_r8
         es = p
      end if






   trinv = 0.0_r8
   if ((.not. icephs) .or. (ttrice.eq.0.0_r8)) go to 10
   trinv = 1.0_r8/ttrice










      tc     = t - tmelt
      lflg   = (tc >= -ttrice .and. tc < 0.0_r8)
      weight = min(-tc*trinv,1.0_r8)
      hlatsb = hlatv + weight*hlatf
      hlatvp = hlatv - 2369.0_r8*tc
      if (t < tmelt) then
         hltalt = hlatsb
      else
         hltalt = hlatvp
      end if
      if (lflg) then
         tterm = pcf(1) + tc*(pcf(2) + tc*(pcf(3) + tc*(pcf(4) + tc*pcf(5))))
      else
         tterm = 0.0_r8
      end if
      desdt  = hltalt*es/(rgasv*t*t) + tterm*trinv
      gam = hltalt*qs*p*desdt/(cp*es*(p - omeps*es))
      if (qs == 1.0_r8) gam = 0.0_r8
    
      dqsdt = (cp/hltalt)*gam
    

   return




10 continue






      hlatvp = hlatv - 2369.0_r8*(t-tmelt)
      if (icephs) then
         hlatsb = hlatv + hlatf
      else
         hlatsb = hlatv
      end if
      if (t < tmelt) then
         hltalt = hlatsb
      else
         hltalt = hlatvp
      end if
      desdt  = hltalt*es/(rgasv*t*t)
      gam = hltalt*qs*p*desdt/(cp*es*(p - omeps*es))
      if (qs == 1.0_r8) gam = 0.0_r8
    
      dqsdt = (cp/hltalt)*gam
    



   return

end subroutine vqsatd2_single


end module wv_saturation 
