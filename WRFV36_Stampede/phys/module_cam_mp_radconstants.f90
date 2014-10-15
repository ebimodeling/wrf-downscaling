



module radconstants




use shr_kind_mod,   only: r8 => shr_kind_r8



  use module_cam_support,    only: endrun


implicit none
private



   public :: get_number_sw_bands
   public :: get_sw_spectral_boundaries
   public :: get_ref_solar_band_irrad
   public :: get_true_ref_solar_band_irrad
   public :: get_ref_total_solar_irrad
   public :: get_solar_band_fraction_irrad
   public :: radconstants_init
   public :: rad_gas_index


integer, parameter, public :: ot_length = 32




integer, parameter, public :: nswbands = 19 

integer, parameter, public :: idx_sw_diag = 8 
integer, parameter, public :: idx_lw_diag = 2 













integer, parameter, public :: nrh = 1000  




integer, public, parameter  :: nlwbands = 7

integer, public, parameter :: idx_LW_H2O_NONWND=1

integer, public, parameter :: idx_LW_H2O_WINDOW=2

integer, public, parameter :: idx_LW_0500_0650=3

integer, public, parameter :: idx_LW_0650_0800=4

integer, public, parameter :: idx_LW_0800_1000=5

integer, public, parameter :: idx_LW_1000_1200=6

integer, public, parameter :: idx_LW_1200_2000=7




integer, public, parameter :: gasnamelength = 5
integer, public, parameter :: nradgas = 8
character(len=gasnamelength), public, parameter :: gaslist(nradgas) &
   = (/'H2O  ','O3   ', 'O2   ', 'CO2  ', 'N2O  ', 'CH4  ', 'CFC11', 'CFC12'/)


real(r8), public, parameter :: minmmr(nradgas) &
   = epsilon(1._r8)



   
   real(r8), public, parameter :: wavmin(nswbands) = &
        (/   .200_r8,    .245_r8,    .265_r8,    .275_r8,    .285_r8, &
             .295_r8,  .305_r8,    .350_r8,    .640_r8,    .700_r8,    .701_r8, &
             .701_r8,  .701_r8,    .701_r8,    .702_r8,    .702_r8, &
            2.630_r8, 4.160_r8,   4.160_r8/)

   real(r8), public, parameter :: wavmin_true(nswbands) = &
        (/   .200_r8,    .245_r8,    .265_r8,    .275_r8,    .285_r8, &
             .295_r8,  .305_r8,    .350_r8,    .640_r8,    .700_r8,    .700_r8, &
             .700_r8,  .700_r8,    .700_r8,    .700_r8,    .700_r8, &
            2.630_r8, 4.160_r8,   4.160_r8/)

   
   real(r8), public, parameter :: wavmax(nswbands) = &
        (/   .245_r8,  .265_r8,    .275_r8,    .285_r8,    .295_r8, &
             .305_r8,  .350_r8,    .640_r8,    .700_r8,   5.000_r8,   5.000_r8, &
            5.000_r8, 5.000_r8,   5.000_r8,   5.000_r8,   5.000_r8, &
            2.860_r8, 4.550_r8,   4.550_r8/)

   
   real(r8), public, parameter :: frcsol(nswbands) = &
     (/ .001488_r8, .001389_r8, .001290_r8, .001686_r8, .002877_r8, &
        .003869_r8, .026336_r8, .360739_r8, .065392_r8, .526861_r8, &
        .526861_r8, .526861_r8, .526861_r8, .526861_r8, .526861_r8, &
        .526861_r8, .006239_r8, .001834_r8, .001834_r8/)

   
   real(r8), public, parameter :: ph2o(nswbands) = &
             (/    .000_r8,    .000_r8,    .000_r8,    .000_r8,    .000_r8, &
        .000_r8,   .000_r8,    .000_r8,    .000_r8,    .505_r8,     &
        .210_r8,   .120_r8,    .070_r8,    .048_r8,    .029_r8,     &
        .018_r8,   .000_r8,    .000_r8,    .000_r8/)

   
   real(r8), public, parameter :: pco2(nswbands) = &
             (/    .000_r8,    .000_r8,    .000_r8,    .000_r8,    .000_r8, &
        .000_r8,   .000_r8,    .000_r8,    .000_r8,    .000_r8,     &
        .000_r8,   .000_r8,    .000_r8,    .000_r8,    .000_r8,     &
        .000_r8,  1.000_r8,    .640_r8,    .360_r8/)

   
   real(r8), public, parameter :: po2(nswbands) = &
             (/    .000_r8,    .000_r8,    .000_r8,    .000_r8,    .000_r8, &
        .000_r8,   .000_r8,    .000_r8,   1.000_r8,   1.000_r8,     &
        .000_r8,   .000_r8,    .000_r8,    .000_r8,    .000_r8,     &
        .000_r8,   .000_r8,    .000_r8,    .000_r8/)

   real(r8) :: solfrac_true(nswbands)

contains



subroutine get_number_sw_bands(number_of_bands)
   
   integer, intent(out) :: number_of_bands

   number_of_bands = nswbands

end subroutine get_number_sw_bands


subroutine get_sw_spectral_boundaries(low_boundaries, high_boundaries, units)
   

   real(r8), intent(out) :: low_boundaries(nswbands), high_boundaries(nswbands)
   character(*), intent(in) :: units 

   select case (units)
   case ('inv_cm','cm^-1','cm-1')
      low_boundaries = 1.e4_r8/wavmax
      high_boundaries = 1.e4_r8/wavmin_true
   case('m')
      low_boundaries = 1.e-6_r8*wavmin_true
      high_boundaries = 1.e-6_r8*wavmax
   case('nm')
      low_boundaries = 1.e3_r8*wavmin_true
      high_boundaries = 1.e3_r8*wavmax
   case('micrometer','micron','um')
      low_boundaries = wavmin_true
      high_boundaries = wavmax
   case default
      call endrun('rad_constants.F90: spectral units not acceptable'//units)
   end select

end subroutine get_sw_spectral_boundaries


subroutine get_ref_solar_band_irrad( band_irrad )

   
   real(r8), intent(out) :: band_irrad(nswbands)

   band_irrad = frcsol

end subroutine get_ref_solar_band_irrad


subroutine radconstants_init()





   integer :: ns
   real(r8):: psf(nswbands)      

   do ns = 1, nswbands
      psf(ns) = 1.0_r8
      if(ph2o(ns)/=0._r8) psf(ns) = psf(ns)*ph2o(ns)
      if(pco2(ns)/=0._r8) psf(ns) = psf(ns)*pco2(ns)
      if(po2 (ns)/=0._r8) psf(ns) = psf(ns)*po2 (ns)
      solfrac_true(ns)   = frcsol(ns)*psf(ns) 
    enddo

end subroutine radconstants_init



subroutine get_true_ref_solar_band_irrad( solfrac_true_out )

   

   real(r8), intent(out) :: solfrac_true_out(nswbands)

   solfrac_true_out(:) = solfrac_true(:)

end subroutine get_true_ref_solar_band_irrad


subroutine get_ref_total_solar_irrad(tsi)
   

   real(r8), intent(out) :: tsi
   real(r8) :: solfrac_true(nswbands)

   call get_true_ref_solar_band_irrad( solfrac_true )
   tsi = sum(solfrac_true)

end subroutine get_ref_total_solar_irrad


subroutine get_solar_band_fraction_irrad(fractional_irradiance)
   

   
   real(r8), intent(out) :: fractional_irradiance(1:nswbands)
   real(r8) :: tsi 

   fractional_irradiance = frcsol

end subroutine get_solar_band_fraction_irrad


integer function rad_gas_index(gasname)

   

   character(len=*),intent(in) :: gasname
   integer :: igas

   rad_gas_index = -1
   do igas = 1, nradgas
      if (trim(gaslist(igas)).eq.trim(gasname)) then
         rad_gas_index = igas
         return
      endif
   enddo
   call endrun ("rad_gas_index: can not find gas with name "//gasname)
end function rad_gas_index

end module radconstants
