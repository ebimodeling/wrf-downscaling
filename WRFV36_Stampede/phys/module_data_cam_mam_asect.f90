	module module_data_cam_mam_asect






















	use shr_kind_mod,    only:  r8 => shr_kind_r8

	use modal_aero_data, only:  ntot_amode, maxd_aspectype


	implicit none





















































































































































































	integer, parameter :: maxd_atype = ntot_amode
	integer, parameter :: maxd_asize = 1
	integer, parameter :: maxd_acomp = maxd_aspectype
	integer, parameter :: maxd_aphase = 2

	integer, save :: ai_phase = 1
	integer, save :: cw_phase = 2
	integer, save :: ci_phase = -999888777
	integer, save :: rn_phase = -999888777
	integer, save :: sn_phase = -999888777
	integer, save :: gr_phase = -999888777

	integer, save :: ntype_aer = 0 
	integer, save :: ntot_mastercomp_aer = 0 
	integer, save :: nphase_aer = 0 

	integer, save ::   &
      	  nsize_aer( maxd_atype ),   & 
      	  ncomp_aer( maxd_atype ),   & 
      	  ncomp_plustracer_aer( maxd_atype ),   &
          mastercompptr_aer(maxd_acomp, maxd_atype), &   
      	  massptr_aer( maxd_acomp, maxd_asize, maxd_atype, maxd_aphase ), & 
		
      	  waterptr_aer( maxd_asize, maxd_atype ), & 
      	  hyswptr_aer( maxd_asize, maxd_atype ), &
      	  numptr_aer( maxd_asize, maxd_atype, maxd_aphase ), & 
		
          mprognum_aer(maxd_asize,maxd_atype,maxd_aphase)




	integer, save :: mastercompindx_so4_aer = -999888777
	integer, save :: mastercompindx_nh4_aer = -999888777
	integer, save :: mastercompindx_no3_aer = -999888777
	integer, save :: mastercompindx_pom_aer = -999888777
	integer, save :: mastercompindx_soa_aer = -999888777
	integer, save :: mastercompindx_bc_aer  = -999888777
	integer, save :: mastercompindx_dust_aer = -999888777
	integer, save :: mastercompindx_seas_aer = -999888777


	real, save ::   &
          dens_aer( maxd_acomp, maxd_atype ),  &
          dens_mastercomp_aer( maxd_acomp ),   &
      	  mw_mastercomp_aer( maxd_acomp ),     &
      	  mw_aer( maxd_acomp, maxd_atype ),    &
      	  hygro_mastercomp_aer( maxd_acomp ),  &
      	  hygro_aer( maxd_acomp, maxd_atype )

	real, save ::   &
          volumcen_sect( maxd_asize, maxd_atype ),  &
          volumlo_sect( maxd_asize, maxd_atype ),   &
          volumhi_sect( maxd_asize, maxd_atype ),   &
          dcen_sect( maxd_asize, maxd_atype ),      &
          dlo_sect( maxd_asize, maxd_atype ),       &
          dhi_sect( maxd_asize, maxd_atype ),       &
          sigmag_aer(maxd_asize, maxd_atype)

	character*10, save ::   &
      	  name_mastercomp_aer( maxd_acomp ),  &
      	  namebb_mastercomp_aer( maxd_acomp ),  &
      	  name_aer( maxd_acomp, maxd_atype )

	integer, save ::                     &
      	  lptr_so4_aer(maxd_asize, maxd_atype, maxd_aphase),      &
      	  lptr_nh4_aer(maxd_asize, maxd_atype, maxd_aphase),      &
      	  lptr_no3_aer(maxd_asize, maxd_atype, maxd_aphase),      &
      	  lptr_pom_aer(maxd_asize, maxd_atype, maxd_aphase),      &
      	  lptr_soa_aer(maxd_asize, maxd_atype, maxd_aphase),      &
      	  lptr_bc_aer(maxd_asize, maxd_atype, maxd_aphase),       &
      	  lptr_dust_aer(maxd_asize, maxd_atype, maxd_aphase),     &
          lptr_seas_aer(maxd_asize, maxd_atype, maxd_aphase)









	real, save ::   &
          mw_so4_aer, mw_nh4_aer,   &
          mw_no3_aer, mw_pom_aer,   &
          mw_soa_aer, mw_bc_aer,   &
          mw_dust_aer, mw_seas_aer


	real, save ::   &
          dens_so4_aer, dens_nh4_aer,   &
          dens_no3_aer, dens_pom_aer,   &
          dens_soa_aer, dens_bc_aer,   &
          dens_dust_aer, dens_seas_aer



	real, parameter :: dens_water_aer  = 1.0


	integer, save ::   &
      	  msectional, maerosolincw,   &
      	  maerocoag, maerchem, maeroptical, maerchem_boxtest_output


      integer, allocatable ::  &
         lptr_chem_to_q(:), lptr_chem_to_qqcw(:)



      real, allocatable ::  &
         factconv_chem_to_q(:), factconv_chem_to_qqcw(:)




      real, allocatable ::  &
         mw_chem_array(:), mw_q_array(:), mw_q_mo_array(:)








	end module module_data_cam_mam_asect

