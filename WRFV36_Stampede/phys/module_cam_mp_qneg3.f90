

subroutine qneg3 (subnam  ,idx     ,ncol    ,ncold   ,lver    ,lconst_beg  , &
                  lconst_end       ,qmin    ,q       )














   use shr_kind_mod, only: r8 => shr_kind_r8



   use module_cam_support, only: iulog

   implicit none





   character*(*), intent(in) :: subnam 

   integer, intent(in) :: idx          
   integer, intent(in) :: ncol         
   integer, intent(in) :: ncold        
   integer, intent(in) :: lver         
   integer, intent(in) :: lconst_beg   
   integer, intent(in) :: lconst_end   

   real(r8), intent(in) :: qmin(lconst_beg:lconst_end)      




   real(r8), intent(inout) :: q(ncold,lver,lconst_beg:lconst_end) 



   integer indx(ncol,lver)  
   integer nval(lver)       
   integer nvals            
   integer nn
   integer iwtmp
   integer i,ii,k           
   integer m                
   integer iw,kw            

   logical found            

   real(r8) worst           




   do m=lconst_beg,lconst_end
      nvals = 0
      found = .false.
      worst = 1.e35_r8
      iw = -1




!DIR$ preferstream
      do k=1,lver
         nval(k) = 0
!DIR$ prefervector
         nn = 0
         do i=1,ncol
            if (q(i,k,m) < qmin(m)) then
               nn = nn + 1
               indx(nn,k) = i
            end if
         end do
         nval(k) = nn
      end do

      do k=1,lver
         if (nval(k) > 0) then
            found = .true.
            nvals = nvals + nval(k)
            iwtmp = -1

            do ii=1,nval(k)
               i = indx(ii,k)
               if (q(i,k,m) < worst) then
                  worst = q(i,k,m)
                  iwtmp = ii
               end if
            end do
            if (iwtmp /= -1 ) kw = k
            if (iwtmp /= -1 ) iw = indx(iwtmp,k)

            do ii=1,nval(k)
               i = indx(ii,k)
               q(i,k,m) = qmin(m)
            end do
         end if
      end do
      if (found .and. abs(worst)>1.e-12_r8) then
         write(iulog,9000)subnam,m,idx,nvals,qmin(m),worst,iw,kw

         call wrf_debug(400,iulog)

      end if
   end do

   return
9000 format(' QNEG3 from ',a,':m=',i3,' lat/lchnk=',i5, &
            ' Min. mixing ratio violated at ',i4,' points.  Reset to ', &
            1p,e8.1,' Worst =',e8.1,' at i,k=',i4,i3)
end subroutine qneg3




subroutine qneg3_modalx1 (subnam  ,idx     ,ncol    ,ncold   ,lver    ,lconst_beg  , &
                          lconst_end       ,qmin    ,q       ,qneg3_worst_thresh )














   use shr_kind_mod, only: r8 => shr_kind_r8



   use module_cam_support, only: iulog

   implicit none





   character*(*), intent(in) :: subnam 

   integer, intent(in) :: idx          
   integer, intent(in) :: ncol         
   integer, intent(in) :: ncold        
   integer, intent(in) :: lver         
   integer, intent(in) :: lconst_beg   
   integer, intent(in) :: lconst_end   

   real(r8), intent(in) :: qmin(lconst_beg:lconst_end)      
   real(r8), intent(in) :: qneg3_worst_thresh(lconst_beg:lconst_end)
                           




   real(r8), intent(inout) :: q(ncold,lver,lconst_beg:lconst_end) 



   integer indx(ncol,lver)  
   integer nval(lver)       
   integer nvals            
   integer nn
   integer iwtmp
   integer i,ii,k           
   integer m                
   integer iw,kw            

   logical found            

   real(r8) worst           
   real(r8) tmp_worst_thresh




   do m=lconst_beg,lconst_end
      nvals = 0
      found = .false.
      worst = 1.e35_r8
      iw = -1




!DIR$ preferstream
      do k=1,lver
         nval(k) = 0
!DIR$ prefervector
         nn = 0
         do i=1,ncol
            if (q(i,k,m) < qmin(m)) then
               nn = nn + 1
               indx(nn,k) = i
            end if
         end do
         nval(k) = nn
      end do

      do k=1,lver
         if (nval(k) > 0) then
            found = .true.
            nvals = nvals + nval(k)
            iwtmp = -1

            do ii=1,nval(k)
               i = indx(ii,k)
               if (q(i,k,m) < worst) then
                  worst = q(i,k,m)
                  iwtmp = ii
               end if
            end do
            if (iwtmp /= -1 ) kw = k
            if (iwtmp /= -1 ) iw = indx(iwtmp,k)

            do ii=1,nval(k)
               i = indx(ii,k)
               q(i,k,m) = qmin(m)
            end do
         end if
      end do

      tmp_worst_thresh = 1.0e-12_r8
      if (qneg3_worst_thresh(m) > 0.0_r8) &
         tmp_worst_thresh = qneg3_worst_thresh(m)

      if (found .and. abs(worst)>tmp_worst_thresh) then
         write(iulog,9000)subnam,m,idx,nvals,qmin(m),worst,iw,kw

         call wrf_debug(400,iulog)

      end if
   end do

   return
9000 format(' QNEG3 from ',a,':m=',i3,' lat/lchnk=',i5, &
            ' Min. mixing ratio violated at ',i4,' points.  Reset to ', &
            1p,e8.1,' Worst =',e8.1,' at i,k=',i4,i3)
end subroutine qneg3_modalx1




