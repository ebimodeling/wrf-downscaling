


module module_fr_fire_model

use module_fr_fire_core
use module_fr_fire_util
use module_fr_fire_phys

contains

subroutine fire_model (                    &
    id,                                     & 
    ifun,                                   & 
    restart,                                &
    need_lfn_update,                          & 
    num_ignitions,                          & 
    ifuelread,nfuel_cat0,                   & 
    ifds,ifde,jfds,jfde,                    & 
    ifms,ifme,jfms,jfme,                    & 
    ifps,ifpe,jfps,jfpe,                    & 
    ifts,ifte,jfts,jfte,                    & 
    time_start,dt,                          & 
    fdx,fdy,                                & 
    ignition_line,                          & 
    ignitions_done,ignited_tile,            &
    coord_xf,coord_yf,unit_xf,unit_yf,      & 
    lfn,lfn_out,tign,fuel_frac,fire_area,   & 
    grnhfx,grnqfx,                          & 
    ros,                                    & 
    nfuel_cat,                              & 
    fuel_time,                              & 
    fp &
) 





































implicit none




integer, intent(in) :: id
integer, intent(in) :: ifun                 
                                            
                                            
                                            
                                            
                                            
logical, intent(in):: restart               
logical, intent(out)::need_lfn_update       

integer, intent(in) :: num_ignitions        
integer, intent(in) :: ifuelread,nfuel_cat0 
integer, intent(in) :: ifds,ifde,jfds,jfde,&  
        ifps,ifpe,jfps,jfpe                
integer, intent(in) :: ifts,ifte,jfts,jfte  
integer, intent(in) :: ifms,ifme,jfms,jfme  
REAL,INTENT(in) :: time_start,dt            
REAL,INTENT(in) :: fdx,fdy                  

type(ignition_line_type), dimension (num_ignitions), intent(in):: ignition_line 
integer, intent(out):: ignited_tile(num_ignitions),ignitions_done
real, dimension(ifms:ifme, jfms:jfme), intent(in):: & 
    coord_xf,coord_yf                       
real, intent(in):: unit_xf,unit_yf          
    

REAL, INTENT(inout), dimension(ifms:ifme,jfms:jfme):: &
    lfn   , &                               
    tign  , &                               
    fuel_frac                               

REAL, INTENT(out), dimension(ifms:ifme,jfms:jfme):: &
    fire_area                               
    

REAL, INTENT(out), dimension(ifms:ifme,jfms:jfme):: &
    lfn_out, &                              
    grnhfx,grnqfx, &                        
    ros                                     
 

real, intent(inout), dimension(ifms:ifme, jfms:jfme)::nfuel_cat 
real,intent(inout),dimension(ifms:ifme,jfms:jfme):: fuel_time
type(fire_params),intent(inout)::fp



integer :: xifms,xifme,xjfms,xjfme  
real, dimension(ifts:ifte,jfts:jfte)::fuel_frac_burnt,fuel_frac_end
integer::ignited,ig,i,j,itso,iteo,jtso,jteo
real::tbound,err,erri,errj,maxgrad,grad,tfa,thf,mhf,tqf,mqf,aw,mw
character(len=128)::msg
logical:: freeze_fire
integer:: stat_lev=1



call check_mesh_2dim(ifts-1,ifte+1,jfts-1,jfte+1,ifms,ifme,jfms,jfme)

xifms=ifms  
xifme=ifme
xjfms=jfms
xjfme=jfme



need_lfn_update=.false.
ignitions_done=0
freeze_fire = fire_const_time > 0. .and. time_start < fire_const_time

if(ifun.eq.1)then       
elseif(ifun.eq.2)then   
        

        
        
        

        call continue_at_boundary(1,1,0., & 
            ifms,ifme,jfms,jfme,           &                
            ifds,ifde,jfds,jfde, &                     
            ifps,ifpe,jfps,jfpe, &            
            ifts,ifte,jfts,jfte, &                
            itso,iteo,jtso,jteo, &              
            fp%zsf)                               


        err=0.
        maxgrad=0.
        do j=jfts,jfte
            do i=ifts,ifte
                erri = fp%dzdxf(i,j) - (fp%zsf(i+1,j)-fp%zsf(i-1,j))/(2.*fdx)
                errj = fp%dzdyf(i,j) - (fp%zsf(i,j+1)-fp%zsf(i,j-1))/(2.*fdy)
                err=max(err,abs(erri),abs(errj))
                grad=sqrt(fp%dzdxf(i,j)**2+fp%dzdyf(i,j)**2)
                maxgrad=max(maxgrad,grad)
            enddo
        enddo
!$OMP CRITICAL(FIRE_MODEL_CRIT)
        write(msg,*)'max gradient ',maxgrad,' max error against zsf',err
!$OMP END CRITICAL(FIRE_MODEL_CRIT)
        call message(msg)

        if(.not.restart)call set_nfuel_cat( &
            ifms,ifme,jfms,jfme, &
            ifts,ifte,jfts,jfte, &
            ifuelread,nfuel_cat0,&
            fp%zsf,nfuel_cat)            

        
        
        if(.not.restart)call set_fire_params(   & 
            ifds,ifde,jfds,jfde, &
            ifms,ifme,jfms,jfme, &
            ifts,ifte,jfts,jfte, &
            fdx,fdy,nfuel_cat0,  &
            nfuel_cat,fuel_time, &
            fp  &
)

        
        if(.not.restart)then
            call init_no_fire  ( &
            ifds,ifde,jfds,jfde, &
            ifms,ifme,jfms,jfme, &
            ifts,ifte,jfts,jfte, &
            fdx,fdy,time_start,  &
            fuel_frac,fire_area,lfn,tign)
            
            need_lfn_update=.true. 

        endif

elseif(ifun.eq.3)then   

    
elseif (ifun.eq.4) then  

    if(fire_print_msg.ge.stat_lev)then
      aw=fun_real(RNRM_SUM,  &
        ifms,ifme,1,1,jfms,jfme, &                
        ifds,ifde,1,1,jfds,jfde, &                
        ifts,ifte,1,1,jfts,jfte, &                
        0,0,0,       &                            
        fp%vx,fp%vy)/((ifde-ifds+1)*(jfde-jfds+1))
      mw=fun_real(RNRM_MAX,  &
        ifms,ifme,1,1,jfms,jfme, &                
        ifds,ifde,1,1,jfds,jfde, &                
        ifts,ifte,1,1,jfts,jfte, &                
        0,0,0,       &                            
        fp%vx,fp%vy)
!$OMP MASTER 
      write(msg,91)time_start,'Average wind        ',aw,'m/s'
      call message(msg,stat_lev)
      write(msg,91)time_start,'Maximum wind        ',mw,'m/s'
      call message(msg,stat_lev)
!$OMP END MASTER 
    endif








    call print_2d_stats(ifts,ifte,jfts,jfte, &
                   ifms,ifme,jfms,jfme, &
                   fuel_frac,'model: fuel_frac start')

    
    
    
    
    
    
    





    if(.not. freeze_fire)then

    call prop_ls(id,     &
        ifds,ifde,jfds,jfde,                      & 
        ifms,ifme,jfms,jfme,                      &
        ifps,ifpe,jfps,jfpe, &                
        ifts,ifte,jfts,jfte,                      &
        time_start,dt,fdx,fdy,tbound,  &
        lfn,lfn_out,tign,ros, fp &
    ) 
    else
        call message('fire_model: EXPERIMENTAL: skipping fireline propagation')

    endif
    
elseif (ifun.eq.5) then 
    
    

    if(.not. freeze_fire)then
    
    do j=jfts,jfte
        do i=ifts,ifte
            lfn(i,j)=lfn_out(i,j)
            
            
        enddo
    enddo

    endif

    
    do ig = 1,num_ignitions
    


            call ignite_fire(                             &
                ifds,ifde,jfds,jfde,                      & 
                ifms,ifme,jfms,jfme,                      &
                ifts,ifte,jfts,jfte,                      &
                ignition_line(ig),                        &
                time_start,time_start+dt,                 &
                coord_xf,coord_yf,unit_xf,unit_yf,        & 
                lfn,tign,ignited)

            ignitions_done=ignitions_done+1
            ignited_tile(ignitions_done)=ignited
                


            call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,lfn,'lfn_ig',id)
            call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,coord_xf,'coord_xf_ig',id)
            call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,coord_yf,'coord_yf_ig',id)


        
    enddo
            
    call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme, &
                   lfn,'fire_model: lfn out')

    
    need_lfn_update=.true. 

elseif (ifun.eq.6) then 

  if(.not. freeze_fire)then

    
    
    call fuel_left(&
        ifms,ifme,jfms,jfme, &
        ifts,ifte,jfts,jfte, &
        ifts,ifte,jfts,jfte, &
        lfn,tign,fuel_time,time_start+dt,fuel_frac_end,fire_area) 

    call print_2d_stats(ifts,ifte,jfts,jfte, &
                   ifts,ifte,jfts,jfte, &
                   fuel_frac_end,'model: fuel_frac end')
    
    do j=jfts,jfte
        do i=ifts,ifte
            fuel_frac_burnt(i,j)=fuel_frac(i,j)-fuel_frac_end(i,j) 
            fuel_frac(i,j)=fuel_frac_end(i,j) 
        enddo
    enddo

    call print_2d_stats(ifts,ifte,jfts,jfte, &
                   ifts,ifte,jfts,jfte, &
                   fuel_frac_burnt,'model: fuel_frac burned')
        
    call heat_fluxes(dt,                          &
        ifms,ifme,jfms,jfme,                      &
        ifts,ifte,jfts,jfte,                      &
        ifts,ifte,jfts,jfte,                      &  
        fp%fgip,                                     &
        fuel_frac_burnt,                          & 
        grnhfx,grnqfx)                              

    if(fire_print_msg.ge.stat_lev)then
      tfa=fun_real(REAL_SUM,  &
        ifms,ifme,1,1,jfms,jfme, &                
        ifds,ifde,1,1,jfds,jfde, &                
        ifts,ifte,1,1,jfts,jfte, &                
        0,0,0,       &                            
        fire_area,fire_area) * fdx * fdy
      thf=fun_real(REAL_SUM,  &
        ifms,ifme,1,1,jfms,jfme, &                
        ifds,ifde,1,1,jfds,jfde, &                
        ifts,ifte,1,1,jfts,jfte, &                
        0,0,0,       &                            
        grnhfx,grnhfx) * fdx * fdy
      mhf=fun_real(REAL_MAX,  &
        ifms,ifme,1,1,jfms,jfme, &                
        ifds,ifde,1,1,jfds,jfde, &                
        ifts,ifte,1,1,jfts,jfte, &                
        0,0,0,       &                            
        grnhfx,grnhfx) 
      tqf=fun_real(REAL_SUM,  &
        ifms,ifme,1,1,jfms,jfme, &                
        ifds,ifde,1,1,jfds,jfde, &                
        ifts,ifte,1,1,jfts,jfte, &                
        0,0,0,       &                            
        grnqfx,grnqfx) * fdx * fdy
      mqf=fun_real(REAL_MAX,  &
        ifms,ifme,1,1,jfms,jfme, &                
        ifds,ifde,1,1,jfds,jfde, &                
        ifts,ifte,1,1,jfts,jfte, &                
        0,0,0,       &                            
        grnqfx,grnqfx) 
!$OMP MASTER 
      write(msg,91)time_start,'Fire area           ',tfa,'m^2'
      call message(msg,stat_lev)
      write(msg,91)time_start,'Heat output         ',thf,'W'
      call message(msg,stat_lev)
      write(msg,91)time_start,'Max heat flux       ',mhf,'W/m^2'
      call message(msg,stat_lev)
      write(msg,91)time_start,'Latent heat output  ',tqf,'W'
      call message(msg,stat_lev)
      write(msg,91)time_start,'Max latent heat flux',mqf,'W/m^2'
      call message(msg,stat_lev)
!$OMP END MASTER
91  format('Time ',f11.3,' s ',a,e12.3,1x,a)
    endif
        

  else
     call message('fire_model: EXPERIMENTAL: skipping fuel burnt computation')

     if (fire_const_grnhfx >= 0. .and. fire_const_grnqfx >= 0.) then

!$OMP CRITICAL(FIRE_MODEL_CRIT)
        write(msg,'(a,2e12.3,a)')'fire_model: EXPERIMENTAL output constant heat flux', &
           fire_const_grnhfx, fire_const_grnqfx, ' W/s'
!$OMP END CRITICAL(FIRE_MODEL_CRIT)
        call message(msg)
        
        do j=jfts,jfte
            do i=ifts,ifte
                grnhfx(i,j)=fire_const_grnhfx
                grnqfx(i,j)=fire_const_grnqfx
            enddo
        enddo

      endif

   endif

    call print_2d_stats(ifts,ifte,jfts,jfte, &
                   ifms,ifme,jfms,jfme, &
                   grnhfx,'model: heat flux(J/m^2/s)')

else
!$OMP CRITICAL(FIRE_MODEL_CRIT)
    write(msg,*)'fire_model: bad ifun=',ifun
!$OMP END CRITICAL(FIRE_MODEL_CRIT)
    call crash(msg)
endif

end subroutine fire_model




            
end module module_fr_fire_model
