





module module_fr_fire_core

use module_fr_fire_phys, only: fire_params , fire_ros
use module_fr_fire_util








type ignition_line_type
  REAL  ros, &          
        stop_time, &    
        wind_red,  &    
        wrdist,   &     
        wrupwind, &     
        start_x, &      
        start_y, &      
        end_x, &        
        end_y, &        
        start_time, &   
        end_time, &     
        radius          
end type ignition_line_type

contains




    
subroutine init_no_fire(&
    ifds,ifde,jfds,jfde, &
    ifms,ifme,jfms,jfme, &
    ifts,ifte,jfts,jfte, &
    fdx,fdy,time_now,    & 
    fuel_frac,fire_area,lfn,tign)    
implicit none
             



integer, intent(in):: ifds,ifde,jfds,jfde   
integer, intent(in):: ifts,ifte,jfts,jfte   
integer, intent(in):: ifms,ifme,jfms,jfme   
real, intent(in) :: fdx,fdy,time_now        
real, intent(out), dimension (ifms:ifme,jfms:jfme) :: & 
                   fuel_frac,fire_area,lfn,tign       


intrinsic epsilon
                                                

integer:: i,j
real lfn_init,time_init

lfn_init = 2*max((ifde-ifds+1)*fdx,(jfde-jfds+1)*fdy)      
time_init=time_now + max(time_now,1.0)*epsilon(time_now) 
 
do j=jfts,jfte
    do i=ifts,ifte
        fuel_frac(i,j)=1.          
        fire_area(i,j)=0.          
        tign(i,j) = time_init      
        lfn(i,j) = lfn_init        
    enddo
enddo
call message('init_no_fire: state set to no fire')

end subroutine init_no_fire




 

subroutine ignite_fire( ifds,ifde,jfds,jfde,                    & 
                        ifms,ifme,jfms,jfme,                      &
                        ifts,ifte,jfts,jfte,                      &
                        ignition_line,                            &
                        start_ts,end_ts,                    &
                        coord_xf,coord_yf,                &     
                        unit_xf,unit_yf,                  &
                        lfn,tign,ignited)
implicit none














integer, intent(in):: ifds,ifde,jfds,jfde   
integer, intent(in):: ifts,ifte,jfts,jfte   
integer, intent(in):: ifms,ifme,jfms,jfme   
type(ignition_line_type), intent(in):: ignition_line    
real, intent(in):: start_ts,end_ts          
real, dimension(ifms:ifme, jfms:jfme), intent(in):: & 
    coord_xf,coord_yf                       
real, intent(in):: unit_xf,unit_yf          
real, intent(inout), dimension (ifms:ifme,jfms:jfme) :: & 
                   lfn, tign                
integer, intent(out):: ignited              
                        

integer:: i,j
real::lfn_new,time_ign,ax,ay,rels,rele,d
real:: sx,sy                    
real:: ex,ey                    
real:: st,et                    
character(len=128):: msg
real::cx2,cy2,dmax,axmin,axmax,aymin,aymax,dmin
real:: start_x,start_y          
real:: end_x,end_y              
real:: radius                   
real:: start_time,end_time      
real:: ros,tos                  




start_x    = ignition_line%start_x 
start_y    = ignition_line%start_y 
end_x      = ignition_line%end_x   
end_y      = ignition_line%end_y   
start_time = ignition_line%start_time 
end_time   = ignition_line%end_time
radius     = ignition_line%radius  
ros        = ignition_line%ros     


tos        = radius/ros            
st         = start_time            
et         = min(end_ts,end_time)  


if(start_ts>et+tos .or. end_ts<st)return   

if(start_time < end_time)then  
        
        
	
	
        rels = 0.
        sx = start_x
        sy = start_y
        rele =  (et - start_time) / (end_time - start_time)    
	ex = start_x + rele * (end_x - start_x)
	ey = start_y + rele * (end_y - start_y)
else
        
        rels = 0.
        rele = 1.
	sx = start_x
	sy = start_y
	ex = end_x
	ey = end_y
endif


cx2=unit_xf*unit_xf
cy2=unit_yf*unit_yf

axmin=coord_xf(ifts,jfts)
aymin=coord_yf(ifts,jfts)
axmax=coord_xf(ifte,jfte)
aymax=coord_yf(ifte,jfte)
!$OMP CRITICAL(FIRE_CORE_CRIT)
write(msg,'(a,2f11.6,a,2f11.6)')'IGN from ',sx,sy,' to ',ex,ey
call message(msg)
write(msg,'(a,2f10.2,a,2f10.2,a)')'IGN timestep [',start_ts,end_ts,'] in [',start_time,end_time,']'
call message(msg)
write(msg,'(a,2g13.6,a,2g13.6)')'IGN tile coord from  ',axmin,aymin,' to ',axmax,aymax
call message(msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)
ignited=0
dmax=0
dmin=huge(dmax)
11      format('IGN ',6(a,g17.7,1x)) 
12      format('IGN ',4(a,2g13.7,1x))
do j=jfts,jfte   
    do i=ifts,ifte
        ax=coord_xf(i,j)
        ay=coord_yf(i,j)

        
        
        call nearest(d,time_ign,ax,ay,sx,sy,st,ex,ey,et,cx2,cy2)
        dmax=max(d,dmax)
        dmin=min(d,dmin)

        lfn_new=d - min( radius, ros*(end_ts - time_ign) )  
        if(fire_print_msg.ge.3)then
!$OMP CRITICAL(FIRE_CORE_CRIT)
            write(msg,*)'IGN1 i,j=',i,j,' lfn(i,j)=',lfn(i,j),' tign(i,j)=',tign(i,j)
            call message(msg)
            write(msg,*)'IGN2 i,j=',i,j,' lfn_new= ',lfn_new, ' time_ign= ',time_ign,' d=',d
            call message(msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)
        endif
        if(.not.lfn_new>0.) then
            ignited=ignited+1   
        endif
        if(lfn(i,j)>0. .and. .not. lfn_new > 0.) then
            tign(i,j)=time_ign + d/ros  
            if(fire_print_msg.ge.3)then
!$OMP CRITICAL(FIRE_CORE_CRIT)
                write(msg,'(a,2i6,a,2g13.6,a,f10.2,a,2f10.2,a)')'IGN ignited cell ',i,j,' at',ax,ay, &
                    ' time',tign(i,j),' in [',start_ts,end_ts,']'
                call message(msg)
                write(msg,'(a,g10.3,a,f10.2,a,2f10.2,a)')'IGN distance',d,' from ignition line at',time_ign,' in [',st,et,']'
                call message(msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)
            endif
            if(tign(i,j) < start_ts .or. tign(i,j) > end_ts )then
!$OMP CRITICAL(FIRE_CORE_CRIT)
                write(msg,'(a,2i6,a,f11.6,a,2f11.6,a)')'WARNING ',i,j, &
                ' fixing ignition time ',tign(i,j),' outside of the time step [',start_ts,end_ts,']'
                call message (msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)
                tign(i,j) = min(max(tign(i,j),start_ts),end_ts)
            endif
        endif
        lfn(i,j)=min(lfn(i,j),lfn_new)  
        if(fire_print_msg.ge.3)then
!$OMP CRITICAL(FIRE_CORE_CRIT)
            write(msg,*)'IGN3 i,j=',i,j,' lfn(i,j)=',lfn(i,j),' tign(i,j)=',tign(i,j)
            call message(msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)
        endif
    enddo
enddo
!$OMP CRITICAL(FIRE_CORE_CRIT)
write(msg,'(a,2g13.2,a,g10.2,a,g10.2)')'IGN units ',unit_xf,unit_yf,' m max dist ',dmax,' min',dmin
call message(msg)
write(msg,'(a,f6.1,a,f8.1,a,i10)')'IGN radius ',radius,' time of spread',tos,' ignited nodes',ignited
call message(msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)

return
99 continue

end subroutine ignite_fire


!DEC$ ATTRIBUTES FORCEINLINE
SUBROUTINE nearest(d,t,ax,ay,sx,sy,st,ex,ey,et,cx2,cy2)
        implicit none

        real, intent(out):: d,t
        real, intent(in):: ax,ay,sx,sy,st,ex,ey,et,cx2,cy2
        
        
        
        
        

        
        
        
        
        
        
        
        
        
        
        
        

        
        
        

        real:: mx,my,dam2,dames,am_es,cos2,dmc2,mcrel,mid_t,dif_t,des2,cx,cy
        character(len=128):: msg


11      format('IGN ',6(a,g17.7,1x))
12      format('IGN ',4(a,2g13.7,1x))

        
        mx = (sx + ex)*0.5
        my = (sy + ey)*0.5
        dam2=(ax-mx)*(ax-mx)*cx2+(ay-my)*(ay-my)*cy2      
        des2 = (ex-sx)*(ex-sx)*cx2+(ey-sy)*(ey-sy)*cy2          
        dames = dam2*des2
        am_es=(ax-mx)*(ex-sx)*cx2+(ay-my)*(ey-sy)*cy2       
        if(dames>0)then
            cos2 = (am_es*am_es)/dames                  
        else 
            cos2 = 0.
        endif
        dmc2 = dam2*cos2                                
        if(4.*dmc2 < des2)then                          
            
            mcrel = sign(sqrt(4.*dmc2/des2),am_es)      
        elseif(am_es>0)then                             
            mcrel = 1.0 
        else                                            
            mcrel = -1.0
        endif
	cx = (ex + sx)*0.5 + mcrel*(ex - sx)*0.5     
	cy = (ey + sy)*0.5 + mcrel*(ey - sy)*0.5     
        d=sqrt((ax-cx)*(ax-cx)*cx2+(ay-cy)*(ay-cy)*cy2) 
	t = (et + st)*0.5 + mcrel*(et - st)*0.5     
        if(fire_print_msg.ge.3)then
!$OMP CRITICAL(FIRE_CORE_CRIT)
            write(msg,12)'find nearest to [',ax,ay,'] from [',sx,sy,'] [',ex,ey,']' 
            call message(msg)
            write(msg,12)'end times',st,et,' scale squared',cx2,cy2 
            call message(msg)
            write(msg,11)'nearest at mcrel=',mcrel,'from the midpoint, t=',t 
            call message(msg)
            write(msg,12)'nearest is [',cx,cy,'] d=',d 
            call message(msg)
            write(msg,11)'dam2=',dam2,'des2=',des2,'dames=',dames
            call message(msg)
            write(msg,11)'am_es=',am_es,'cos2=',cos2,'dmc2=',dmc2 
            call message(msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)
        endif
END SUBROUTINE nearest






subroutine fuel_left(&
    ims,ime,jms,jme, &
    its,ite,jts,jte, &
    ifs,ife,jfs,jfe, &
    lfn, tign, fuel_time, time_now, fuel_frac, fire_area)
implicit none






integer, intent(in) :: its,ite,jts,jte,ims,ime,jms,jme,ifs,ife,jfs,jfe
real, intent(in), dimension(ims:ime,jms:jme)::lfn,tign,fuel_time
real, intent(in):: time_now
real, intent(out), dimension(ifs:ife,jfs:jfe)::fuel_frac
real, intent(out), dimension(ims:ime,jms:jme):: fire_area













integer::i,j,ir,jr,icl,jcl,isubcl,jsubcl,i2,j2,ii,jj
real::fmax,frat,helpsum1,helpsum2,fuel_left_ff,fire_area_ff,rx,ry,tignf(2,2)

real::lffij,lffi1j,lffij1,lffi1j1,tifij,tifi1j,tifij1,tifi1j1,tx,ty,txx,tyy


character(len=128)::msg
integer::m,omp_get_thread_num
     

call check_mesh_2dim(its-1,ite+1,jts-1,jte+1,ims,ime,jms,jme)
call check_mesh_2dim(its,ite,jts,jte,ifs,ife,jfs,jfe)


ir=fuel_left_irl
jr=fuel_left_jrl

if ((ir.ne.2).or.(jr.ne.2)) then 
   call crash('fuel_left: ir.ne.2 or jr.ne.2 ')
endif

rx=1./ir 
ry=1./jr




















do icl=its,ite
  do jcl=jts,jte
    helpsum1=0
    helpsum2=0

    do isubcl=1,ir
      do jsubcl=1,jr 
        i=(icl-its)*ir+isubcl
        j=(jcl-jts)*jr+jsubcl


        if ((isubcl.eq.1).and.(jsubcl.eq.1)) then
           i2=icl-1
           j2=jcl-1
           ty=0.5
           tx=0.5
           tyy=1.0
           txx=1.0
        else if ((isubcl.eq.2).and.(jsubcl.eq.1)) then
           i2=icl
           j2=jcl-1
           ty=0.5
           tx=0
           tyy=1.0
           txx=0.5
        else if ((isubcl.eq.1).and.(jsubcl.eq.2)) then
           i2=icl-1
           j2=jcl
           tx=0.5
           ty=0
           txx=1.0
           tyy=0.5
        else if ((isubcl.eq.2).and.(jsubcl.eq.2)) then
           i2=icl
           j2=jcl
           tx=0
           ty=0
           txx=0.5
           tyy=0.5
        else
           call crash('fuel_left: isubcl,jsubcl should be only 1 or 2')
        endif 


        lffij=                             &    
                  (1-tx)*(1-ty)*lfn(i2,j2)      &
             +    (1-tx)*ty  *lfn(i2,j2+1)      &
             +     tx*(1-ty)*lfn(i2+1,j2)       &
             +       tx*ty  *lfn(i2+1,j2+1)
        lffi1j=                            &
                    (1-txx)*(1-ty)*lfn(i2,j2)   &
             +      (1-txx)*ty  *lfn(i2,j2+1)   &
             +      (txx)*(1-ty)*lfn(i2+1,j2)   &
             +      (txx)*ty  *lfn(i2+1,j2+1)
        lffij1=                            &
                    (1-tx)*(1-tyy)*lfn(i2,j2)   &
             +      (1-tx)*(tyy)  *lfn(i2,j2+1) &
             +      tx*(1-tyy)*lfn(i2+1,j2)     &
             +      tx*(tyy)  *lfn(i2+1,j2+1)
        lffi1j1 =                               &
                      (1-txx)*(1-tyy)*lfn(i2,j2)     &
             +      (1-txx)*(tyy)  *lfn(i2,j2+1)   &        
             +      (txx)*(1-tyy)*lfn(i2+1,j2)     &
             +      (txx)*(tyy)  *lfn(i2+1,j2+1)

        
        do ii=1,2
          do jj=1,2
            tignf(ii,jj)=tign(i2+ii-1,j2+jj-1)
          enddo
        enddo
        tifij=                                 &
                   (1-tx)*(1-ty)*tignf(1,1)        &
             +     (1-tx)*ty*tignf(1,1+1)          &
             +     tx*(1-ty)*tignf(1+1,1)          &
             +     tx*ty*tignf(1+1,1+1)
        tifi1j=                               &
                   (1-txx)*(1-ty)*tignf(1,1)      &
             +     (1-txx)*ty*tignf(1,1+1)        &
             +     (txx)*(1-ty)*tignf(1+1,1)      &
             +     (txx)*(ty)*tignf(1+1,1+1)            
        tifij1=                               &
                   (1-tx)*(1-tyy)*tignf(1,1)      &
             +     (1-tx)*(tyy)*tignf(1,1+1)      &
             +      tx*(1-tyy)*tignf(1+1,1)       &
             +      tx*(tyy)*tignf(1+1,1+1)
        tifi1j1=                               &
                   (1-txx)*(1-tyy)*tignf(1,1)     &
             +     (1-txx)*(tyy)*tignf(1,1+1)     &
             +     (txx)*(1-tyy)*tignf(1+1,1)     &
             +     (txx)*(tyy)*tignf(1+1,1+1) 

         
        if(fuel_left_method.eq.1)then
          call fuel_left_cell_1( fuel_left_ff, fire_area_ff, &
             lffij,lffij1,lffi1j,lffi1j1,&
             tifij,tifij1,tifi1j,tifi1j1,&
             time_now, fuel_time(icl,jcl))
        elseif(fuel_left_method.eq.2)then
          fire_area_ff=0  
          fuel_left_ff=fuel_left_cell_2( &
             lffij,lffij1,lffi1j,lffi1j1,&
             tifij,tifij1,tifi1j,tifi1j1,&
             time_now, fuel_time(icl,jcl)) 
        else
          call crash('fuel_left: unknown fuel_left_method')
        endif

        
        if(fire_area_ff.lt.-1e-6 .or.  &
          (fire_area_ff.eq.0. .and. fuel_left_ff.lt.1.-1e-6))then
!$OMP CRITICAL(FIRE_CORE_CRIT)
           write(msg,'(a,2i6,2(a,f11.8))')'fuel_left: at node',i,j, &
              ' of refined mesh fuel burnt',1-fuel_left_ff,' fire area',fire_area_ff
!$OMP END CRITICAL(FIRE_CORE_CRIT)
           call crash(msg)
        endif

        helpsum1=helpsum1+fuel_left_ff
        helpsum2=helpsum2+fire_area_ff
      enddo
    enddo
    fuel_frac(icl,jcl)=helpsum1 
    fire_area(icl,jcl)=helpsum2
  enddo 
enddo
  











do j=jts,jte
    do i=its,ite        
        fuel_frac(i,j) = fuel_frac(i,j) /(ir*jr) 
        fire_area(i,j) = fire_area(i,j) /(ir*jr) 
    enddo
enddo


fmax=0
do j=jts,jte
    do i=its,ite        
       if(fire_area(i,j).eq.0.)then
           if(fuel_frac(i,j).lt.1.-1e-6)then
!$OMP CRITICAL(FIRE_CORE_CRIT)
               write(msg,'(a,2i6,2(a,f11.8))')'fuel_left: at node',i,j, &
                   ' fuel burnt',1-fuel_frac(i,j),' but fire area',fire_area(i,j)
!$OMP END CRITICAL(FIRE_CORE_CRIT)
               call crash(msg)
           endif
       else
           frat=(1-fuel_frac(i,j))/fire_area(i,j)
           fmax=max(fmax,frat)
       endif
    enddo
enddo
!$OMP CRITICAL(FIRE_CORE_CRIT)
write(msg,'(a,4i6,a,f10.7)')'fuel_left: tile',its,ite,jts,jte,' max fuel burnt/area',fmax 
!$OMP END CRITICAL(FIRE_CORE_CRIT)
call message(msg)
return


end subroutine fuel_left





subroutine fuel_left_cell_1( fuel_frac_left, fire_frac_area, &
    lfn00,lfn01,lfn10,lfn11, &
    tign00,tign01,tign10,tign11,&
    time_now, fuel_time_cell)

implicit none

real, intent(out):: fuel_frac_left, fire_frac_area 
real, intent(in)::lfn00,lfn01,lfn10,lfn11    
real, intent(in)::tign00,tign01,tign10,tign11
real, intent(in)::time_now                   
real, intent(in)::fuel_time_cell            










































































intrinsic tiny


real::ps,aps,area,ta,out
real::t00,t01,t10,t11
real,parameter::safe=tiny(aps)
character(len=128)::msg







t00=tign00-time_now
if(lfn00>0. .or. t00>0.)t00=0.
t01=tign01-time_now
if(lfn01>0. .or. t01>0.)t01=0.
t10=tign10-time_now
if(lfn10>0. .or. t10>0.)t10=0.
t11=tign11-time_now
if(lfn11>0. .or. t11>0.)t11=0.


ps = lfn00+lfn01+lfn10+lfn11   
aps = abs(lfn00)+abs(lfn01)+abs(lfn10)+abs(lfn11)
aps=max(aps,safe)
area =(-ps/aps+1.)/2.
area = max(area,0.) 
area = min(area,1.)
    

ta=0.25*(t00+t01+t10+t11)


out=1.

if(area>0)out=area*exp(ta/fuel_time_cell) + (1. - area)

if(out>1.)then
!$OMP CRITICAL(FIRE_CORE_CRIT)
    write(msg,*)'out=',out,'>1 area=',area,' ta=',ta
    call message(msg)
    write(msg,*)'tign=',tign00,tign01,tign10,tign11,' time_now=',time_now
!$OMP END CRITICAL(FIRE_CORE_CRIT)
    call message(msg)
    
    call crash('fuel_left_cell_1: fuel fraction > 1')
endif




fuel_frac_left = out
fire_frac_area = area

end subroutine fuel_left_cell_1





real function fuel_left_cell_2(  &
    lfn00,lfn01,lfn10,lfn11, &
    tign00,tign01,tign10,tign11,&
    time_now, fuel_time_cell)

implicit none

real, intent(in)::lfn00,lfn01,lfn10,lfn11    
real, intent(in)::tign00,tign01,tign10,tign11
real, intent(in)::time_now                   
real, intent(in)::fuel_time_cell            










































































call crash('fuel_left_cell_2: not implemented, please use fire_fuel_left_method=1')
fuel_left_cell_2=0.  
end function fuel_left_cell_2

subroutine prop_ls( id, &                                
                ids,ide,jds,jde, &                       
                ims,ime,jms,jme, &                       
                ips,ipe,jps,jpe, &                
                its,ite,jts,jte, &                       
                ts,dt,dx,dy,     &                       
                tbound,          &                       
                lfn_in,lfn_out,tign,ros,  &              
                fp               &
                   )
implicit none































  


































integer,intent(in)::id,ims,ime,jms,jme,ids,ide,jds,jde,its,ite,jts,jte,ips,ipe,jps,jpe 
real,dimension(ims:ime,jms:jme),intent(inout)::lfn_in,tign
real,dimension(ims:ime,jms:jme),intent(out)::lfn_out,ros
real,intent(in)::dx,dy,ts,dt
real,intent(out)::tbound
type(fire_params),intent(in)::fp



real,dimension(its-1:ite+1,jts-1:jte+1):: tend, lfn1 

real::grad2,rr,tbound2,a,a1 

real::gradx,grady,aspeed,err,aerr,time_now
integer::ihs,ihe,jhs,jhe
integer::ihs2,ihe2,jhs2,jhe2
integer::i,j,its1,ite1,jts1,jte1,k,kk,id1
character(len=128)::msg
integer::nfirenodes,nfireline
real::sum_err,min_err,max_err,sum_aerr,min_aerr,max_aerr   


integer,parameter :: mstep=1000, printl=1
real, parameter:: zero=0.,one=1.,eps=epsilon(zero),tol=100*eps, &
    safe=2.,rmin=safe*tiny(zero),rmax=huge(zero)/safe



intrinsic max,min,sqrt,nint,epsilon,tiny,huge
  


!$OMP CRITICAL(FIRE_CORE_CRIT)
write(msg,'(a8,i5,a6,i5,3(a1,i5))')'prop_ls:',id,' tile ',its,':',ite,',',jts,':',jte
!$OMP END CRITICAL(FIRE_CORE_CRIT)
call message(msg)

    a=fire_back_weight 
    a1=1. - a
    
    

    ihs2=max(its-2,ids)   
    ihe2=min(ite+2,ide)
    jhs2=max(jts-2,jds) 
    jhe2=min(jte+2,jde)

    ihs=max(its-1,ids)   
    ihe=min(ite+1,ide)
    jhs=max(jts-1,jds) 
    jhe=min(jte+1,jde)

    call write_array_m(ihs,ihe,jhs,jhe,ims,ime,jms,jme,lfn_in,'lfn_in',id)

    
    call check_mesh_2dim(ihs2,ihe2,jhs2,jhe2,ims,ime,jms,jme)
    call print_2d_stats(ihs2,ihe2,jhs2,jhe2,ims,ime,jms,jme, &
                   lfn_in,'prop_ls: lfn in')
    
    
    
    
    
    
    id1 = id  
    if(id1.ne.0)id1=id1+1000
    call  tend_ls( id1, &
    ims,ime,jms,jme, &                       
    its-1,ite+1,jts-1,jte+1, &                   
    ids,ide,jds,jde, &                       
    ips,ipe,jps,jpe, &                       
    ihs,ihe,jhs,jhe, &                       
    ims,ime,jms,jme, &                       
    its,ite,jts,jte, &                       
    ts,dt,dx,dy,      &                      
    lfn_in, &                                
    tbound, &                                
    tend, ros, &                              
    fp         &                             
)

    call write_array_m(ihs,ihe,jhs,jhe,its-1,ite+1,jts-1,jte+1,tend,'tend1',id)

    
    do j=jhs,jhe
        do i=ihs,ihe
            lfn1(i,j) = lfn_in(i,j) + dt*tend(i,j)
        enddo
    enddo
    
    call print_2d_stats(ihs,ihe,jhs,jhe,its-1,ite+1,jts-1,jte+1, &
                   lfn1,'prop_ls: lfn1')
    

    if(id1.ne.0)id1=id1+1000
    call  tend_ls( id1,&
    its-1,ite+1,jts-1,jte+1, &                   
    its-1,ite+1,jts-1,jte+1, &                   
    ids,ide,jds,jde,     &                   
    ips,ipe,jps,jpe, &                       
    its,ite,jts,jte, &                       
    ims,ime,jms,jme, &                       
    its,ite,jts,jte, &                       
    ts+dt,dt,dx,dy,      &                   
    lfn1, &                                  
    tbound2, &                               
    tend,ros, &                               
    fp &
)

    call write_array_m(its,ite,jts,jte,its-1,ite+1,jts-1,jte+1,tend,'tend2',id)

    call print_2d_stats(its,ite,jts,jte,its-1,ite+1,jts-1,jte+1,tend,'prop_ls: tend2')
        
    tbound=min(tbound,tbound2)

!$OMP CRITICAL(FIRE_CORE_CRIT)
    write(msg,'(a,f10.2,4(a,f7.2))')'prop_ls: time',ts,' dt=',dt,' bound',min(tbound,999.99), &
        ' dx=',dx,' dy=',dy
!$OMP END CRITICAL(FIRE_CORE_CRIT)
    call message(msg)
    if(dt>tbound)then
!$OMP CRITICAL(FIRE_CORE_CRIT)
        write(msg,'(2(a,f10.2))')'prop_ls: WARNING: time step ',dt, &
        ' > bound =',tbound
!$OMP END CRITICAL(FIRE_CORE_CRIT)
        call message(msg)
    endif
    
    
    
    do j=jts,jte
        do i=its,ite
            lfn_out(i,j) = a1*lfn1(i,j) + a*(lfn_in(i,j) + dt*tend(i,j))
            lfn_out(i,j) = min(lfn_out(i,j),lfn_in(i,j)) 
        enddo
    enddo      

    
    
    
    
    
    
    
    
    

    time_now=ts+dt
    time_now = time_now + abs(time_now)*epsilon(time_now)*2.
    do j=jts,jte
        do i=its,ite
            
            if (.not. lfn_out(i,j)>0 .and. lfn_in(i,j)>0)then
                tign(i,j) = ts + dt * lfn_in(i,j) / (lfn_in(i,j) - lfn_out(i,j))
            endif
            
            if(lfn_out(i,j)>0.)tign(i,j)=time_now
        enddo
    enddo
    
    
    
    
    nfirenodes=0
    nfireline=0
    sum_err=0.
    min_err=rmax
    max_err=rmin     
    sum_aerr=0.
    min_aerr=rmax
    max_aerr=rmin    
    its1=its+1
    jts1=jts+1
    ite1=ite-1
    jte1=jte-1
    
    
    
    
    
    do j=jts1,jte1
        do i=its1,ite1
            if(lfn_out(i,j)>0.0)then   
                if(lfn_out(i+1,j)<=0.or.lfn_out(i,j+1)<=0.or. & 
                   lfn_out(i-1,j)<=0.or.lfn_out(i,j-1)<=0)then 
                   gradx=(lfn_out(i+1,j)-lfn_out(i-1,j))/(2.0*dx) 
                   grady=(lfn_out(i,j+1)-lfn_out(i,j-1))/(2.0*dy)
                   grad2=sqrt(gradx*gradx+grady*grady)
                   aspeed = (lfn_in(i,j)-lfn_out(i,j))/(dt*max(grad2,rmin))                   
                    rr = speed_func(gradx,grady,dx,dy,i,j,fp)
                   err=aspeed-rr
                   sum_err=sum_err+err
                   min_err=min(min_err,err)
                   max_err=max(max_err,err)     
                   aerr=abs(err)
                   sum_aerr=sum_aerr+aerr
                   min_aerr=min(min_aerr,aerr)
                   max_aerr=max(max_aerr,aerr)
                   nfireline=nfireline+1
                endif
            else
                nfirenodes=nfirenodes+1
            endif
        enddo
    enddo
!$OMP CRITICAL(FIRE_CORE_CRIT)
    write(msg,'(2(a,i6,f8.4))')'prop_ls: nodes burning',nfirenodes, &
        (100.*nfirenodes)/((ite1-its1+1)*(jte1-jts1+1)),'% next to fireline',nfireline
!$OMP END CRITICAL(FIRE_CORE_CRIT)
    call message(msg)
    if(nfireline>0)then
        call print_stat_line('speed error',its1,ite1,jts1,jte1,min_err,max_err,sum_err/nfireline)
        call print_stat_line('abs(speed error)',its1,ite1,jts1,jte1,min_aerr,max_aerr,sum_aerr/nfireline)
    endif

    
    do k=-1,1,2
        
        do kk=1,boundary_guard   
            i=ids+k*kk
            if(i.ge.its.and.i.le.ite)then
                do j=jts,jte
                    if(lfn_out(i,j)<=0.)goto 9
                enddo
            endif
    enddo
        
        do kk=1,boundary_guard    
            j=jds+k*kk
            if(j.ge.jts.and.j.le.jte)then
                do i=its,ite
                    if(lfn_out(i,j)<=0.)goto 9
                enddo
            endif
        enddo
    enddo
    goto 10
9   continue
!$OMP CRITICAL(FIRE_CORE_CRIT)
    write(msg,'(a,i2,a,2i8)')'prop_ls: fire',boundary_guard, &
        ' cells from domain boundary at node ',i,j
!$OMP END CRITICAL(FIRE_CORE_CRIT)
    call message(msg)     
    call crash('prop_ls: increase the fire region')
10  continue

    call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme, &
                   lfn_out,'prop_ls: lfn out')

end subroutine prop_ls





subroutine tend_ls( id, &
    lims,lime,ljms,ljme, &                   
    tims,time,tjms,tjme, &                   
    ids,ide,jds,jde, &                       
    ips,ipe,jps,jpe, &                       
    ints,inte,jnts,jnte, &                   
    ims,ime,jms,jme, &                       
    its,ite,jts,jte, &                       
    t,dt,dx,dy,      &                       
    lfn, &                                   
    tbound, &                                
    tend, ros,  &                              
    fp &
)

implicit none




integer,intent(in)::id,lims,lime,ljms,ljme,tims,time,tjms,tjme
integer,intent(in)::ims,ime,jms,jme,its,ite,jts,jte
integer, intent(in)::ids,ide,jds,jde,ints,inte,jnts,jnte,ips,ipe,jps,jpe 
real,intent(in)::t                                     
real,intent(in)::dt,dx,dy                                 
real,dimension(lims:lime,ljms:ljme),intent(inout)::lfn 
real,intent(out)::tbound                               
real,dimension(tims:time,tjms:tjme),intent(out)::tend  
real,dimension(ims:ime,jms:jme),intent(out)::ros  
type(fire_params),intent(in)::fp


real:: te,diffLx,diffLy,diffRx,diffRy, & 
   diffCx,diffCy,diff2x,diff2y,grad,rr, &
   ros_base,ros_wind,ros_slope,ros_back,advx,advy,scale,nvx,nvy, &
   speed,tanphi
integer::i,j,itso,iteo,jtso,jteo
character(len=128)msg


real, parameter:: eps=epsilon(0.0)

real, parameter:: zero=0.,one=1.,tol=100*eps, &
    safe=2.,rmin=safe*tiny(zero),rmax=huge(zero)/safe




intrinsic max,min,sqrt,nint,tiny,huge


real,dimension(tims:time,tjms:tjme)::rra,grada,speeda,tanphia


    
    
    call check_mesh_2dim(ints-1,inte+1,jnts-1,jnte+1,lims,lime,ljms,ljme)
    call check_mesh_2dim(ints,inte,jnts,jnte,tims,time,tjms,tjme)
    
    call continue_at_boundary(1,1,fire_lfn_ext_up, &   
    lims,lime,ljms,ljme, &                
    ids,ide,jds,jde, &                    
    ips,ipe,jps,jpe, &                    
    ints,inte,jnts,jnte, &                
    itso,iteo,jtso,jteo, &                
    lfn)                                  

    call print_2d_stats(itso,iteo,jtso,jteo,lims,lime,ljms,ljme, &
                   lfn,'tend_ls: lfn cont')

    call write_array_m(ints-1,inte+1,jnts-1,jnte+1,lims,lime,ljms,ljme,lfn,'tend_lfn_in',id)
    
    tbound=0    
    do j=jnts,jnte
        do i=ints,inte
            
            diffRx = (lfn(i+1,j)-lfn(i,j))/dx
            diffLx = (lfn(i,j)-lfn(i-1,j))/dx
            diffRy = (lfn(i,j+1)-lfn(i,j))/dy
            diffLy = (lfn(i,j)-lfn(i,j-1))/dy
            diffCx = diffLx+diffRx   
            diffCy = diffLy+diffRy
    
            
            select case(fire_upwinding)
            case(0)  
                grad=sqrt(diffCx**2 + diffCy**2)
            case(1) 
                diff2x=select_upwind(diffLx,diffRx)
                diff2y=select_upwind(diffLy,diffRy)
                grad=sqrt(diff2x*diff2x + diff2y*diff2y)
            case(2) 
                diff2x=select_godunov(diffLx,diffRx)
                diff2y=select_godunov(diffLy,diffRy)
                grad=sqrt(diff2x*diff2x + diff2y*diff2y)
            case(3) 
                diff2x=select_eno(diffLx,diffRx)
                diff2y=select_eno(diffLy,diffRy)
                grad=sqrt(diff2x*diff2x + diff2y*diff2y)
            case(4) 
                grad=sqrt(max(diffLx,0.)**2+min(diffRx,0.)**2   &
                        + max(diffLy,0.)**2+min(diffRy,0.)**2)
            case default
                grad=0.
            end select
  
            
            scale=sqrt(diffCx*diffCx+diffCy*diffCy+eps) 
            nvx=diffCx/scale
            nvy=diffCy/scale
                      
            
            speed =  fp%vx(i,j)*nvx + fp%vy(i,j)*nvy
        
    
            

            call fire_ros(ros_base,ros_wind,ros_slope, &
            nvx,nvy,i,j,fp)

            rr=ros_base + ros_wind + ros_slope
            if(fire_grows_only.gt.0)rr=max(rr,0.)

            
            if(i.ge.its.and.i.le.ite.and.j.ge.jts.and.j.le.jte)ros(i,j)=rr

            if(fire_upwind_split.eq.0)then

                
                te = -rr*grad   

            else

                
                te = - ros_base*grad

		
                if (abs(speed)> eps) then
                    advx=fp%vx(i,j)*ros_wind/speed
                    advy=fp%vy(i,j)*ros_wind/speed
                else 
                    advx=0
                    advy=0
                endif

                tanphi =  fp%dzdxf(i,j)*nvx + fp%dzdyf(i,j)*nvy
		
                if(abs(tanphi)>eps) then
                    advx=advx+fp%dzdxf(i,j)*ros_slope/tanphi
                    advy=advy+fp%dzdyf(i,j)*ros_slope/tanphi
                endif

                if(fire_upwind_split.eq.1)then   

                    
                    te = te - max(advx,0.)*diffLx - min(advx,0.)*diffRy &
                            - max(advy,0.)*diffLy - min(advy,0.)*diffRy


                elseif(fire_upwind_split.eq.2)then   
 
                    
                    call crash('prop_ls: bad fire_upwind_split, Lax-Friedrichs not done yet')

                else

                    call crash('prop_ls: bad fire_upwind_split')

                endif
            endif

            
            if (grad > 0.) then
                 tbound = max(tbound,rr*(abs(diff2x)/dx+abs(diff2y)/dy)/grad)
            endif

            
            te=te + fire_viscosity*abs(rr)*((diffRx-diffLx)+(diffRy-diffLy))

            tend(i,j)=te
            rra(i,j)=rr
            grada(i,j)=grad    
            speeda(i,j)=speed
            tanphia(i,j)=tanphi
            
            

            
            
            
            
        enddo
    enddo        

    call write_array_m(ints,inte,jnts,jnte,tims,time,tjms,tjme,rra,'rr',id)
    call write_array_m(ints,inte,jnts,jnte,tims,time,tjms,tjme,grada,'grad',id)
    call write_array_m(ints,inte,jnts,jnte,tims,time,tjms,tjme,speeda,'speed',id)
    call write_array_m(ints,inte,jnts,jnte,tims,time,tjms,tjme,tanphia,'tanphi',id)
    call write_array_m(ints,inte,jnts,jnte,tims,time,tjms,tjme,tend,'tend',id)

    call print_2d_stats(ints,inte,jnts,jnte,tims,time,tjms,tjme, &
                   tend,'tend_ls: tend out')

    
    tbound = 1/(tbound+tol)

end subroutine tend_ls





real function select_upwind(diffLx,diffRx)
implicit none
real, intent(in):: diffLx, diffRx
real diff2x



diff2x=0
if (diffLx>0.and.diffRx>0.)diff2x=diffLx
if (diffLx<0.and.diffRx<0.)diff2x=diffRx

select_upwind=diff2x
end function select_upwind







real function select_godunov(diffLx,diffRx)
implicit none
real, intent(in):: diffLx, diffRx
real diff2x,diffCx






diff2x=0
diffCx=diffRx+diffLx
if (diffLx>0.and..not.diffCx<0)diff2x=diffLx
if (diffRx<0.and.     diffCx<0)diff2x=diffRx

select_godunov=diff2x
end function select_godunov





real function select_eno(diffLx,diffRx)
implicit none
real, intent(in):: diffLx, diffRx
real diff2x



if    (.not.diffLx>0 .and. .not.diffRx>0)then
    diff2x=diffRx
elseif(.not.diffLx<0 .and. .not.diffRx<0)then
    diff2x=diffLx
elseif(.not.diffLx<0 .and. .not.diffRx>0)then
    if(.not. abs(diffRx) < abs(diffLx))then
        diff2x=diffRx
    else
        diff2x=diffLx
    endif
else
    diff2x=0.
endif

select_eno=diff2x
end function select_eno
      




real function speed_func(diffCx,diffCy,dx,dy,i,j,fp)


implicit none

real, intent(in)::diffCx,diffCy  
real, intent(in)::dx,dy  
integer, intent(in)::i,j         
type(fire_params),intent(in)::fp

real::scale,nvx,nvy,r
real::ros_base , ros_wind , ros_slope
real, parameter:: eps=epsilon(0.0)

            
            scale=sqrt(diffCx*diffCx+diffCy*diffCy+eps) 
            nvx=diffCx/scale
            nvy=diffCy/scale
                      
            

            call fire_ros(ros_base,ros_wind,ros_slope, &
            nvx,nvy,i,j,fp)

            r=ros_base + ros_wind + ros_slope
            if(fire_grows_only.gt.0)r=max(r,0.)
            speed_func=r

end function speed_func

end module module_fr_fire_core
