#
# Copyright 2014 Vertum Partners
# Scott B. Capps
#
from optparse import OptionParser
import sys,os
# PURPOSE:
#   Create a WRF run directory and copy all necessary WRF files. 
#   Follow the naming scheme:
#   wrfv[version]_[model]_[pbl]_[microphys]_[lsm]_[sfcphys]_[lwrad]_[swrad]_[cuphys]_[ludataset]_[nudging]_[laps]_[res]
#   where:
#   version=wrfv35, wrfv351, etc.
#   model= NAM, RAP, GFS, NARR, MERRA, eraint, cfsr2, etc..
#   pbl[bl_pbl_physics]=0-12,94
#   microphys[mp_physics]=0-11,13,14,16,17,18,19
#   lsm[sf_surface_physics]=0,1,2,3,4,5,7,8
#   sfcphys[sf_sfclay_physics]=0,1,2,3,4,5,7,10,11
#   lwrad[ra_lw_physics]=0,1,3,4,5,7,31,99
#   swrad[ra_sw_physics]=0,1,2,3,4,5,7,99
#   cuphys[cu_physics]=0,1,2,3,4,5,6,7,14,84,93,99
#   ludataset=modis,nlcd,usgs
#   nudging=nonudge/analysis/spectral
#   laps=laps/nolaps
#   res=Resolution of domains: 9-3-1km,3-1km,2km-667m
#
# USAGE: 
# python make_wrf_run_dir.py --model="eraint" --wrfdir=${HOME}/WRFV351/ --wpsdir=${HOME}/WPSV351/ --rundir=${SCRATCH}/wrfv351_eraint_1_5_2_1_1_1_1_spectral_nolaps_18-6-2km/
#
# Process Command Line Arguments ######################################
parser = OptionParser()
parser.add_option("--model" ,type="string" ,help="Atm model for BCs, NAM, GFS, RAP, etc." ,dest="model")
parser.add_option("--wrfdir",type="string" ,help="Directory containing WRF build"         ,dest="wrfdir")
parser.add_option("--wpsdir",type="string" ,help="Directory containing WPS build"         ,dest="wpsdir")
parser.add_option("--rundir",type="string" ,help="Directory to create WRF run files in"   ,dest="rundir")
(options, args) = parser.parse_args()
model  = options.model
wrfdir = options.wrfdir
wpsdir = options.wpsdir
rundir = options.rundir
# END: Process Command Line Arguments ##################################

os.system('mkdir -p '+rundir)

if model=="nam":
    print "Building directory for your nam run..."
    os.system('/bin/cp '+wpsdir+'ungrib/Variable_Tables/Vtable.NAM '+rundir+'Vtable')
elif model=="gfs":
    print "Building directory for your gfs run..."
    os.system('/bin/cp '+wpsdir+'ungrib/Variable_Tables/Vtable.GFS '+rundir+'Vtable')
elif model=="rap":
    print "Building directory for your RAP run, Remember: use num_metgrid_levels=51 and num_metgrid_soil_levels=6"
    os.system('/bin/cp '+wpsdir+'ungrib/Variable_Tables/Vtable.RAP.hybrid.ncep '+rundir+'Vtable')
elif model=="cfsr2":
    print "Building directory for your CFSR2 run..."
    os.system('/bin/cp '+wpsdir+'ungrib/Variable_Tables/Vtable.CFSR2_web '+rundir+'Vtable')
elif model=="eraint":
    print "Building directory for your ERA-int run..."
    os.system('/bin/cp '+wpsdir+'ungrib/Variable_Tables/Vtable.ERA-interim.ml '+rundir+'Vtable')
elif model=="narr":
    print "Building directory for your NARR run..."
    os.system('/bin/cp '+wpsdir+'ungrib/Variable_Tables/Vtable.NARR '+rundir+'Vtable')
else:
    print "model name not recognized, exiting..."
    sys.exit()
#
# Copy over files from WRF run dir:
os.system('/bin/cp '+wpsdir+'/namelist.wps '+rundir+'namelist.wps.example')
os.system('/bin/cp '+wrfdir+'run/namelist.input '+rundir+'namelist.input.example')
os.system('/bin/cp '+wrfdir+'run/aerosol.formatted '+rundir)
os.system('/bin/cp '+wrfdir+'run/aerosol_lat.formatted '+rundir)
os.system('/bin/cp '+wrfdir+'run/aerosol_lon.formatted '+rundir)
os.system('/bin/cp '+wrfdir+'run/aerosol_plev.formatted '+rundir)
os.system('/bin/cp '+wrfdir+'run/CAM_ABS_DATA '+rundir)
os.system('/bin/cp '+wrfdir+'run/CAM_AEROPT_DATA '+rundir)
os.system('/bin/cp '+wrfdir+'run/CAMtr_volume_mixing_ratio.A1B '+rundir)
os.system('/bin/cp '+wrfdir+'run/CAMtr_volume_mixing_ratio.A2 '+rundir)
os.system('/bin/cp '+wrfdir+'run/CAMtr_volume_mixing_ratio.RCP4.5 '+rundir)
os.system('/bin/cp '+wrfdir+'run/CAMtr_volume_mixing_ratio.RCP6 '+rundir)
os.system('/bin/cp '+wrfdir+'run/CAMtr_volume_mixing_ratio.RCP8.5 '+rundir)
os.system('/bin/cp '+wrfdir+'run/CLM_ALB_ICE_DFS_DATA '+rundir)
os.system('/bin/cp '+wrfdir+'run/CLM_ALB_ICE_DRC_DATA '+rundir)
os.system('/bin/cp '+wrfdir+'run/CLM_ASM_ICE_DFS_DATA '+rundir)
os.system('/bin/cp '+wrfdir+'run/CLM_ASM_ICE_DRC_DATA '+rundir)
os.system('/bin/cp '+wrfdir+'run/CLM_DRDSDT0_DATA '+rundir)
os.system('/bin/cp '+wrfdir+'run/CLM_EXT_ICE_DFS_DATA '+rundir)
os.system('/bin/cp '+wrfdir+'run/CLM_EXT_ICE_DRC_DATA '+rundir)
os.system('/bin/cp '+wrfdir+'run/CLM_KAPPA_DATA '+rundir)
os.system('/bin/cp '+wrfdir+'run/CLM_TAU_DATA '+rundir)
os.system('/bin/cp '+wrfdir+'run/co2_trans '+rundir)
os.system('/bin/cp '+wrfdir+'run/ETAMPNEW_DATA '+rundir)
os.system('/bin/cp '+wrfdir+'run/ETAMPNEW_DATA_DBL '+rundir)
os.system('/bin/cp '+wrfdir+'run/ETAMPNEW_DATA.expanded_rain '+rundir)
os.system('/bin/cp '+wrfdir+'run/ETAMPNEW_DATA.expanded_rain_DBL '+rundir)
os.system('/bin/cp '+wrfdir+'run/GENPARM.TBL '+rundir)
os.system('/bin/cp '+wrfdir+'run/URBPARM.TBL '+rundir)
os.system('/bin/cp '+wrfdir+'run/URBPARM_UZE.TBL '+rundir)
os.system('/bin/cp '+wrfdir+'run/VEGPARM.TBL '+rundir)
os.system('/bin/cp '+wrfdir+'run/LANDUSE.TBL '+rundir)
os.system('/bin/cp '+wrfdir+'run/MPTABLE.TBL '+rundir)
os.system('/bin/cp '+wrfdir+'run/grib2map.tbl '+rundir)
os.system('/bin/cp '+wrfdir+'run/gribmap.txt '+rundir)
os.system('/bin/cp '+wrfdir+'run/ndown.exe '+rundir)
os.system('/bin/cp '+wrfdir+'run/nup.exe '+rundir)
os.system('/bin/cp '+wrfdir+'run/ozone.formatted '+rundir)
os.system('/bin/cp '+wrfdir+'run/ozone_lat.formatted '+rundir)
os.system('/bin/cp '+wrfdir+'run/ozone_plev.formatted '+rundir)
os.system('/bin/cp '+wrfdir+'run/real.exe '+rundir)
os.system('/bin/cp '+wrfdir+'run/wrf.exe '+rundir)
os.system('/bin/cp '+wrfdir+'run/RRTM_DATA '+rundir)
os.system('/bin/cp '+wrfdir+'run/RRTM_DATA_DBL '+rundir)
os.system('/bin/cp '+wrfdir+'run/RRTMG_LW_DATA '+rundir)
os.system('/bin/cp '+wrfdir+'run/RRTMG_LW_DATA_DBL '+rundir)
os.system('/bin/cp '+wrfdir+'run/RRTMG_SW_DATA '+rundir)
os.system('/bin/cp '+wrfdir+'run/RRTMG_SW_DATA '+rundir)
os.system('/bin/cp '+wrfdir+'run/RRTMG_SW_DATA_DBL '+rundir)
os.system('/bin/cp '+wrfdir+'run/SOILPARM.TBL '+rundir)
os.system('/bin/cp '+wrfdir+'run/tc.exe '+rundir)
os.system('/bin/cp '+wrfdir+'run/tr49t67 '+rundir)
os.system('/bin/cp '+wrfdir+'run/tr49t85 '+rundir)
os.system('/bin/cp '+wrfdir+'run/tr67t85 '+rundir)
#
# WPS
os.system('/bin/mkdir -p '+rundir+'metgrid')
os.system('/bin/mkdir -p '+rundir+'geogrid')
os.system('/bin/cp '+wpsdir+'metgrid/METGRID.TBL '+rundir+'metgrid/METGRID.TBL')
os.system('/bin/cp '+wpsdir+'geogrid/GEOGRID.TBL '+rundir+'geogrid/GEOGRID.TBL')
os.system('/bin/cp '+wpsdir+'geogrid.exe '+rundir)
os.system('/bin/cp '+wpsdir+'link_grib.csh '+rundir)
os.system('/bin/cp '+wpsdir+'ungrib/ungrib.exe '+rundir)
os.system('/bin/cp '+wpsdir+'metgrid/metgrid.exe '+rundir)
print 'FINISHED'