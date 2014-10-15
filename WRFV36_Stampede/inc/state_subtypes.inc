!STARTOFREGISTRYGENERATEDINCLUDE 'inc/state_subtypes.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
TYPE fdob_type
integer                                  :: domain_tot
integer                                  :: ieodi
integer                                  :: iwtsig
integer                                  :: nstat
integer                                  :: nstaw
integer                                  :: ktaur
integer                                  :: levidn(max_domains)
integer                                  :: refprt(max_domains)
real                                     :: window
real                                     :: rtlast
real                                     :: datend
logical                                  :: nudge_uv_pbl
logical                                  :: nudge_t_pbl
logical                                  :: nudge_q_pbl
integer                                  :: sfc_scheme_horiz
integer                                  :: sfc_scheme_vert
real                                     :: max_sndng_gap
real                                     :: sfcfact
real                                     :: sfcfacr
real                                     :: rinfmn
real                                     :: rinfmx
real                                     :: pfree
real                                     :: dcon
real                                     :: dpsmx
real                                     :: tfaci
real                                     :: known_lat
real                                     :: known_lon
character*256                               :: sdate
real                                     :: xtime_at_rest
real                                     :: vif_uv(6)
real                                     :: vif_t(6)
real                                     :: vif_q(6)
real                                     :: vif_fullmin
real                                     :: vif_rampmin
real                                     :: vif_max
real      ,DIMENSION(:,:)     ,POINTER   :: varobs
real      ,DIMENSION(:,:)     ,POINTER   :: errf
real      ,DIMENSION(:)       ,POINTER   :: timeob
real      ,DIMENSION(:)       ,POINTER   :: nlevs_ob
real      ,DIMENSION(:)       ,POINTER   :: lev_in_ob
real      ,DIMENSION(:)       ,POINTER   :: plfo
real      ,DIMENSION(:)       ,POINTER   :: elevob
real      ,DIMENSION(:)       ,POINTER   :: rio
real      ,DIMENSION(:)       ,POINTER   :: rjo
real      ,DIMENSION(:)       ,POINTER   :: rko
integer   ,DIMENSION(:)       ,POINTER   :: obsprt
real      ,DIMENSION(:)       ,POINTER   :: latprt
real      ,DIMENSION(:)       ,POINTER   :: lonprt
real      ,DIMENSION(:)       ,POINTER   :: mlatprt
real      ,DIMENSION(:)       ,POINTER   :: mlonprt
integer   ,DIMENSION(:,:)     ,POINTER   :: stnidprt
real      ,DIMENSION(:)       ,POINTER   :: base_state
END TYPE fdob_type
!ENDOFREGISTRYGENERATEDINCLUDE
