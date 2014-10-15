


  module trb_mtn_stress

  use shr_kind_mod,  only : r8 => shr_kind_r8



  use module_cam_support,   only: iulog


  implicit none
  private      
  save

  public init_tms                             
  public compute_tms                          

  
  
  

  real(r8), parameter :: horomin= 1._r8       
  real(r8), parameter :: z0max  = 100._r8     
  real(r8), parameter :: dv2min = 0.01_r8     
  real(r8)            :: orocnst              
  real(r8)            :: z0fac                
  real(r8)            :: karman               
  real(r8)            :: gravit               
  real(r8)            :: rair                 

  contains

  
  
  

  subroutine init_tms( kind, oro_in, z0fac_in, karman_in, gravit_in, rair_in )
    
    integer,  intent(in) :: kind   
    real(r8), intent(in) :: oro_in, z0fac_in, karman_in, gravit_in, rair_in
    
    if( kind .ne. r8 ) then
        write(iulog,*) 'KIND of reals passed to init_tms -- exiting.'
        stop 'compute_tms'
    endif

    orocnst  = oro_in
    z0fac    = z0fac_in
    karman   = karman_in
    gravit   = gravit_in
    rair     = rair_in
    
    return
  end subroutine init_tms

  
  
  

  subroutine compute_tms( pcols    , pver    , ncol    ,                     &
                          u        , v       , t       , pmid    , exner   , &
                          zm       , sgh     , ksrf    , taux    , tauy    , & 
                          landfrac )

    
    
    
    
    
    
    
    
    

    
    
    

    integer,  intent(in)  :: pcols                 
    integer,  intent(in)  :: pver                  
    integer,  intent(in)  :: ncol                  

    real(r8), intent(in)  :: u(pcols,pver)         
    real(r8), intent(in)  :: v(pcols,pver)         
    real(r8), intent(in)  :: t(pcols,pver)         
    real(r8), intent(in)  :: pmid(pcols,pver)      
    real(r8), intent(in)  :: exner(pcols,pver)     
    real(r8), intent(in)  :: zm(pcols,pver)        
    real(r8), intent(in)  :: sgh(pcols)            
    real(r8), intent(in)  :: landfrac(pcols)       
    
    real(r8), intent(out) :: ksrf(pcols)           
    real(r8), intent(out) :: taux(pcols)           
    real(r8), intent(out) :: tauy(pcols)           

    
    
    

    integer  :: i                                  
    integer  :: kb, kt                             
    
    real(r8) :: horo                               
    real(r8) :: z0oro                              
    real(r8) :: dv2                                
    real(r8) :: ri                                 
    real(r8) :: stabfri                            
    real(r8) :: rho                                
    real(r8) :: cd                                 
    real(r8) :: vmag                               

    
    
    
       
    do i = 1, ncol

     

       horo = orocnst * sgh(i)

     

       if( horo < horomin ) then

           ksrf(i) = 0._r8
           taux(i) = 0._r8
           tauy(i) = 0._r8

       else

         

           z0oro = min( z0fac * horo, z0max )

         

           cd = ( karman / log( ( zm(i,pver) + z0oro ) / z0oro) )**2

         

           kt  = pver - 1
           kb  = pver
           dv2 = max( ( u(i,kt) - u(i,kb) )**2 + ( v(i,kt) - v(i,kb) )**2, dv2min )

         
         
         
         
         
         

           ri  = 2._r8 * gravit * ( t(i,kt) * exner(i,kt) - t(i,kb) * exner(i,kb) ) * ( zm(i,kt) - zm(i,kb) ) &
                                / ( ( t(i,kt) * exner(i,kt) + t(i,kb) * exner(i,kb) ) * dv2 )

         
         

         
         
         

           stabfri = max( 0._r8, min( 1._r8, 1._r8 - ri ) )
           cd      = cd * stabfri

         

           rho     = pmid(i,pver) / ( rair * t(i,pver) ) 
           vmag    = sqrt( u(i,pver)**2 + v(i,pver)**2 )
           ksrf(i) = rho * cd * vmag * landfrac(i)
           taux(i) = -ksrf(i) * u(i,pver)
           tauy(i) = -ksrf(i) * v(i,pver)

       end if

    end do
    
    return
  end subroutine compute_tms

  end module trb_mtn_stress
