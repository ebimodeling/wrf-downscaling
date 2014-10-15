










      module modal_aero_data




      use shr_kind_mod,  only: r8 => shr_kind_r8



      use module_cam_support, only : pcnst => pcnst_runtime

      use radconstants,  only: nswbands, nlwbands

      implicit none
      save

     integer, parameter ::  maxd_aspectype = 14
    
    



    integer, parameter :: ntot_amode = 3


    
    
    
  integer, parameter ::  ntot_aspectype = 8
  character(len=*),parameter ::  specname_amode(ntot_aspectype) = (/ 'sulfate   ', 'ammonium  ', 'nitrate   ', &
       'p-organic ', 's-organic ', 'black-c   ', &
       'seasalt   ', 'dust      ' /)
    
    

    
    
    real(r8) :: specmw_amode(ntot_aspectype)   = (/ 115.0, 115.0,  62.0,   12.0,   12.0,   12.0,  58.5, 135.0 /)


    
    character(len=*), parameter :: modename_amode(ntot_amode) = (/'accum           ', &
         'aitken          ', &
         'coarse          '/)

    integer :: nspec_amode(ntot_amode) = (/ 6, 3, 3 /)
    integer, parameter :: nspec_amode_max = 6
    
    integer, parameter ::     mprognum_amode(ntot_amode)   = (/ 1, 1, 1/)
    integer, parameter ::     mdiagnum_amode(ntot_amode)   = (/ 0, 0, 0/)
    integer, parameter ::     mprogsfc_amode(ntot_amode)   = (/ 0, 0, 0/)
    integer, parameter ::     mcalcwater_amode(ntot_amode) = (/ 0, 0, 0/)

    
    real(r8), parameter ::     dgnum_amode(ntot_amode)   = (/ 0.1100e-6, 0.0260e-6, 2.000e-6 /)
    real(r8), parameter ::     dgnumlo_amode(ntot_amode) = (/ 0.0535e-6, 0.0087e-6, 1.000e-6 /)
    real(r8), parameter ::     dgnumhi_amode(ntot_amode) = (/ 0.4400e-6, 0.0520e-6, 4.000e-6 /)

    
    real(r8), parameter ::     sigmag_amode(ntot_amode)   = (/ 1.800, 1.600, 1.800 /)

    
    real(r8), parameter ::     rhcrystal_amode(ntot_amode)  = (/ 0.350, 0.350, 0.350 /)
    real(r8), parameter ::     rhdeliques_amode(ntot_amode) = (/ 0.800, 0.800, 0.800 /)


    integer :: msectional = -1


      integer                                               &   
          lspectype_amode( maxd_aspectype, ntot_amode ),    &   
          lmassptr_amode( maxd_aspectype, ntot_amode ),     &   
          lmassptrcw_amode( maxd_aspectype, ntot_amode ),   &   
          numptr_amode( ntot_amode ),                       &   
          numptrcw_amode( ntot_amode )


      real(r8) ::                                 &   
          alnsg_amode( ntot_amode ),              &   
          voltonumb_amode( ntot_amode ),          &   
          voltonumblo_amode( ntot_amode ),        &   
          voltonumbhi_amode( ntot_amode ),        &   
          alnv2n_amode( ntot_amode ),             &   
          alnv2nlo_amode( ntot_amode ),           &   
          alnv2nhi_amode( ntot_amode ),           &   
          specdens_amode( maxd_aspectype ),       &   
          spechygro( maxd_aspectype )


      complex                                     &   
          specrefndxsw( nswbands, maxd_aspectype ),           &   
          specrefndxlw( nlwbands, maxd_aspectype )

      character(len=16), allocatable :: cnst_name_cw( : )

      character(len=8) :: aodvisname(ntot_amode ),       &
                          ssavisname(ntot_amode )
      character(len=48) :: aodvislongname(ntot_amode ),  &
                           ssavislongname(ntot_amode )

      character(len=8) :: fnactname(ntot_amode ),   &
                          fmactname(ntot_amode ),   &
                          nactname(ntot_amode )
      character(len=48) :: fnactlongname(ntot_amode ),   &
                           fmactlongname(ntot_amode ),   &
                           nactlongname(ntot_amode )

      integer                                       &   
          lptr_so4_a_amode(ntot_amode),  lptr_so4_cw_amode(ntot_amode), &   
          lptr_msa_a_amode(ntot_amode),  lptr_msa_cw_amode(ntot_amode), &   
          lptr_nh4_a_amode(ntot_amode),  lptr_nh4_cw_amode(ntot_amode), &   
          lptr_no3_a_amode(ntot_amode),  lptr_no3_cw_amode(ntot_amode), &   
          lptr_pom_a_amode(ntot_amode),  lptr_pom_cw_amode(ntot_amode), &   
          lptr_soa_a_amode(ntot_amode),  lptr_soa_cw_amode(ntot_amode), &   
          lptr_bc_a_amode(ntot_amode),   lptr_bc_cw_amode(ntot_amode),  &   
          lptr_nacl_a_amode(ntot_amode), lptr_nacl_cw_amode(ntot_amode),&   
          lptr_dust_a_amode(ntot_amode), lptr_dust_cw_amode(ntot_amode),&   
          modeptr_accum,  modeptr_aitken,                               &   
          modeptr_ufine,  modeptr_coarse,                               &   
          modeptr_pcarbon,                                              &   
          modeptr_finedust,  modeptr_fineseas,                          &   
          modeptr_coardust,  modeptr_coarseas

      real(r8) ::             &
          specmw_so4_amode,     specdens_so4_amode,       &
          specmw_nh4_amode,     specdens_nh4_amode,       &
          specmw_no3_amode,     specdens_no3_amode,       &
          specmw_pom_amode,     specdens_pom_amode,       &
          specmw_soa_amode,     specdens_soa_amode,       &
          specmw_bc_amode,      specdens_bc_amode,        &
          specmw_dust_amode,    specdens_dust_amode,      &
          specmw_seasalt_amode, specdens_seasalt_amode
      integer, allocatable:: species_class(:)	
				

	integer     spec_class_undefined
	parameter ( spec_class_undefined = 0 )
	integer     spec_class_cldphysics
	parameter ( spec_class_cldphysics = 1 )
	integer     spec_class_aerosol
	parameter ( spec_class_aerosol = 2 )
	integer     spec_class_gas
	parameter ( spec_class_gas = 3 )
	integer     spec_class_other
	parameter ( spec_class_other = 4 )



      real(r8), allocatable :: qneg3_worst_thresh_amode(:)

      
      
      character(len=16), allocatable :: cnst_name_cw_mp(:)

      integer  :: msectional_mp = -1
      integer  :: modeptr_accum_mp    
      integer  :: modeptr_coarse_mp   
      integer  :: modeptr_coardust_mp 
      integer  :: modeptr_aitken_mp   
      integer  :: ntot_amode_mp = ntot_amode

      integer  :: numptrcw_amode_mp(ntot_amode) 
      integer  :: lptr_dust_a_amode_mp(ntot_amode)
      integer  :: lptr_nacl_a_amode_mp(ntot_amode)
      integer  :: numptr_amode_mp(ntot_amode) 	 
      integer  :: nspec_amode_mp(ntot_amode)  = (/ 6, 3, 3 /)
      integer  :: lmassptr_amode_mp(maxd_aspectype, ntot_amode) 
      integer  :: lspectype_amode_mp(maxd_aspectype, ntot_amode)       
      integer  :: lmassptrcw_amode_mp(maxd_aspectype, ntot_amode)
      
      real(r8) :: voltonumb_amode_mp( ntot_amode )
      real(r8) :: alnsg_amode_mp( ntot_amode )
      real(r8) :: voltonumbhi_amode_mp(ntot_amode)
      real(r8) :: voltonumblo_amode_mp(ntot_amode)
      real(r8) :: sigmag_amode_mp(ntot_amode)  = sigmag_amode(1:ntot_amode)
      real(r8) :: dgnum_amode_mp(ntot_amode)   = dgnum_amode(1:ntot_amode)
      real(r8) :: dgnumlo_amode_mp(ntot_amode) = dgnumlo_amode(1:ntot_amode)
      real(r8) :: dgnumhi_amode_mp(ntot_amode) = dgnumhi_amode(ntot_amode) 
      real(r8) :: specdens_amode_mp( maxd_aspectype )
      real(r8) :: specmw_amode_mp(ntot_aspectype)  
      real(r8) :: spechygro_mp( maxd_aspectype )



      end module modal_aero_data






































































































































































