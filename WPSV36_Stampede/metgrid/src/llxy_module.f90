!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODULE LLXY_MODULE
!
! This module handles transformations between model grid coordinates and 
!   latitude-longitude coordinates. The actual transformations are done through
!   the map_utils module. 
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module llxy_module

   use gridinfo_module
   use list_module
   use map_utils
   use module_debug
   use misc_definitions_module
 
   ! Parameters
   integer, parameter :: MAX_SOURCE_LEVELS = 20
 
   ! Variables
   integer :: current_nest_number
   integer :: SOURCE_PROJ = 0
   ! The following arrays hold values for all available domains 
   ! NOTE: The entries in the arrays for "domain 0" are used for projection
   !       information of user-specified source data
   type (proj_info), dimension(-MAX_SOURCE_LEVELS:MAX_DOMAINS) :: proj_stack
 
   ! The projection and domain that we have computed constants for
   integer :: computed_proj = INVALID
   integer :: computed_domain = INVALID
 
   contains
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: push_source_projection
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine push_source_projection(iprojection, user_stand_lon, user_truelat1, user_truelat2, &
                                  user_dxkm, user_dykm, user_dlat, user_dlon, user_known_x, &
                                  user_known_y, user_known_lat, user_known_lon, earth_radius)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: iprojection
      real, intent(in) :: user_stand_lon, user_truelat1, user_truelat2, user_dxkm, user_dykm, &
                          user_dlat, user_dlon, &
                          user_known_x, user_known_y, user_known_lat, user_known_lon
      real, intent(in), optional :: earth_radius

      SOURCE_PROJ = SOURCE_PROJ-1
      if (SOURCE_PROJ < -MAX_SOURCE_LEVELS) then
         call mprintf(.true.,ERROR,'In push_user_projection(), too many levels of user projections.')
      end if
  
      call map_init(proj_stack(SOURCE_PROJ))

      if (iprojection == PROJ_LATLON) then
         call map_set(iprojection, proj_stack(SOURCE_PROJ), &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      nxmax=nint(360.0 / user_dlon), &
                      latinc=user_dlat, &
                      loninc=user_dlon, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_MERC) then
         call map_set(iprojection, proj_stack(SOURCE_PROJ), &
                      truelat1=user_truelat1, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      dx=user_dxkm, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_CYL) then
         call mprintf(.true.,ERROR,'Should not have PROJ_CYL as projection for ' &
                          //'source data in push_source_projection()')
  
      else if (iprojection == PROJ_CASSINI) then
         call mprintf(.true.,ERROR,'Should not have PROJ_CASSINI as projection for ' &
                          //'source data in push_source_projection()')
  
      else if (iprojection == PROJ_LC) then
         call map_set(iprojection, proj_stack(SOURCE_PROJ), &
                      truelat1=user_truelat1, &
                      truelat2=user_truelat2, &
                      stdlon=user_stand_lon, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      dx=user_dxkm, &
                      r_earth=earth_radius)

      else if (iprojection == PROJ_ALBERS_NAD83) then
         call map_set(iprojection, proj_stack(SOURCE_PROJ), &
                      truelat1=user_truelat1, &
                      truelat2=user_truelat2, &
                      stdlon=user_stand_lon, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      dx=user_dxkm, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_PS) then
         call map_set(iprojection, proj_stack(SOURCE_PROJ), &
                      truelat1=user_truelat1, &
                      stdlon=user_stand_lon, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      dx=user_dxkm, &
                      r_earth=earth_radius)

      else if (iprojection == PROJ_PS_WGS84) then
         call map_set(iprojection, proj_stack(SOURCE_PROJ), &
                      truelat1=user_truelat1, &
                      stdlon=user_stand_lon, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      dx=user_dxkm, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_GAUSS) then
         call map_set(iprojection, proj_stack(SOURCE_PROJ), &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      nxmax=nint(360.0 / user_dlon), &
                      nlat=nint(user_dlat), &
                      loninc=user_dlon, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_ROTLL) then
  ! BUG: Implement this projection.
  
      end if
     
   end subroutine push_source_projection
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: pop_source_projection
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine pop_source_projection()
 
      implicit none
  
      SOURCE_PROJ = SOURCE_PROJ+1
      
      call mprintf((SOURCE_PROJ > 0), ERROR, &
                   'In pop_user_projection(), projection stack has overflowed.')
 
   end subroutine pop_source_projection
 
 

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: set_domain_projection
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine set_domain_projection(iprojection, user_stand_lon, user_truelat1, user_truelat2, &
                                  user_dxkm, user_dykm, user_dlat, user_dlon, &
                                  user_xdim, user_ydim, user_known_x, &
                                  user_known_y, user_known_lat, user_known_lon, &
                                  user_pole_lat, user_pole_lon, earth_radius)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: iprojection
      integer, intent(in) :: user_xdim, user_ydim
      real, intent(in) :: user_stand_lon, user_truelat1, user_truelat2, &
                          user_dxkm, user_dykm, user_dlat, user_dlon, &
                          user_known_x, user_known_y, user_known_lat, user_known_lon, &
                          user_pole_lat, user_pole_lon
      real, intent(in), optional :: earth_radius
  
      current_nest_number = 1

      call map_init(proj_stack(current_nest_number))
  
      if (iprojection == PROJ_LATLON) then
         call map_set(iprojection, proj_stack(current_nest_number), &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      latinc=user_dlat, &
                      loninc=user_dlon, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_MERC) then
         call map_set(iprojection, proj_stack(current_nest_number), &
                      truelat1=user_truelat1, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      dx=user_dxkm, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_CYL) then
         call map_set(iprojection, proj_stack(current_nest_number), &
                      latinc=user_dlat, &
                      loninc=user_dlon, &
                      stdlon=user_stand_lon, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_CASSINI) then
         call map_set(iprojection, proj_stack(current_nest_number), &
                      latinc=user_dlat, &
                      loninc=user_dlon, &
                      dx=user_dxkm,        &
                      dy=user_dykm,        &
                      stdlon=user_stand_lon, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      lat0=user_pole_lat, &
                      lon0=user_pole_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_LC) then
         call map_set(iprojection, proj_stack(current_nest_number), &
                      truelat1=user_truelat1, &
                      truelat2=user_truelat2, &
                      stdlon=user_stand_lon, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      dx=user_dxkm, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_ALBERS_NAD83) then
         call map_set(iprojection, proj_stack(current_nest_number), &
                      truelat1=user_truelat1, &
                      truelat2=user_truelat2, &
                      stdlon=user_stand_lon, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      dx=user_dxkm, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_PS) then
         call map_set(iprojection, proj_stack(current_nest_number), &
                      truelat1=user_truelat1, &
                      stdlon=user_stand_lon, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      dx=user_dxkm, &
                      r_earth=earth_radius)

      else if (iprojection == PROJ_PS_WGS84) then
         call map_set(iprojection, proj_stack(current_nest_number), &
                      truelat1=user_truelat1, &
                      stdlon=user_stand_lon, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      dx=user_dxkm)
  
      else if (iprojection == PROJ_GAUSS) then
         call map_set(iprojection, proj_stack(current_nest_number), &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      nlat=nint(user_dlat), &
                      loninc=user_dlon, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_ROTLL) then
         call map_set(iprojection, proj_stack(current_nest_number), &
                      ixdim=user_xdim, &
                      jydim=user_ydim, &
                      phi=user_dlat, &
                      lambda=user_dlon, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      stagger=HH, &
                      latinc=user_dykm, &
                      loninc=user_dxkm, &
                      r_earth=earth_radius)
  
      end if
     
   end subroutine set_domain_projection





   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: select_domain
   !
   ! Purpose: This routine is used to select which nest x/y <-> lat/lon 
   !   conversions will be with respect to. For example, selecting domain 2 will
   !   cause the llxy routine to compute x/y locations with respect to domain 2
   !   given a lat/lon.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine select_domain(domain_num)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: domain_num
  
      if (domain_num > 1) then
         call mprintf(.true.,ERROR,'In select_domain(), selected domain is greater than 1.')
      end if
  
      current_nest_number = domain_num
 
   end subroutine select_domain
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: iget_selected_domain
   !
   ! Purpose: This function returns the number of the currently selected nest. 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   function iget_selected_domain()
 
      implicit none
  
      ! Return value
      integer :: iget_selected_domain
      
      iget_selected_domain = current_nest_number
 
   end function iget_selected_domain 
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: lltoxy
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine lltoxy(xlat, xlon, x, y, stagger, comp_ll)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: stagger
      real, intent(in) :: xlat, xlon
      real, intent(out) :: x, y
      logical, optional, intent(in) :: comp_ll

      ! Local variables
      logical :: save_comp_ll
  
      ! Account for grid staggering
      if (stagger == HH) then
         proj_stack(current_nest_number)%stagger = HH
      else if (stagger == VV) then
         proj_stack(current_nest_number)%stagger = VV
      end if
  
      if (present(comp_ll)) then
         save_comp_ll = proj_stack(current_nest_number)%comp_ll
         proj_stack(current_nest_number)%comp_ll = comp_ll
      end if

      call latlon_to_ij(proj_stack(current_nest_number), xlat, xlon, x, y)

      if (present(comp_ll)) then
         proj_stack(current_nest_number)%comp_ll = save_comp_ll
      end if
  
      ! Account for grid staggering
      if (stagger == U) then
         x = x + 0.5
      else if (stagger == V) then
         y = y + 0.5
      end if
 
   end subroutine lltoxy
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: lltoxy
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine xytoll(x, y, xlat, xlon, stagger, comp_ll)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: stagger
      real, intent(in) :: x, y
      real, intent(out) :: xlat, xlon
      logical, optional, intent(in) :: comp_ll

      ! Local variables
      real :: rx, ry
      logical :: save_comp_ll
  
      ! Account for grid staggering; we cannot modify x and y, so modify local
      !   copies of them
      if (stagger == U) then
         rx = x - 0.5
         ry = y
      else if (stagger == V) then
         rx = x
         ry = y - 0.5
      else if (stagger == HH) then
         proj_stack(current_nest_number)%stagger = HH
         rx = x
         ry = y
      else if (stagger == VV) then
         proj_stack(current_nest_number)%stagger = VV
         rx = x
         ry = y
      else
         rx = x
         ry = y
      end if

      if (present(comp_ll)) then
         save_comp_ll = proj_stack(current_nest_number)%comp_ll
         proj_stack(current_nest_number)%comp_ll = comp_ll
      end if
  
      call ij_to_latlon(proj_stack(current_nest_number), rx, ry, xlat, xlon)

      if (present(comp_ll)) then
         proj_stack(current_nest_number)%comp_ll = save_comp_ll
      end if
 
   end subroutine xytoll

end module llxy_module
