# Setting up WRF for meteorological downscaling in Brazil

## Download and extract WRF v 3.5.1

```
wget http://www2.mmm.ucar.edu/wrf/src/WRFV3.5.1.TAR.gz
mkdir WRFV351
tar -xzvf WRFV3.5.1.TAR.gz --strip-components 1 -C ./WRFV351/
```

Then replace the following files:


```
cd WRFV351/phys/
wget http://www2.mmm.ucar.edu/wrf/users/wrfv3.5/module_cu_gf.F
wget http://www2.mmm.ucar.edu/wrf/users/wrfv3.5/module_bl_mynn.F
wget http://www2.mmm.ucar.edu/wrf/users/wrfv3.5/module_radiation_driver.F

cd ../Registry/
wget http://www2.mmm.ucar.edu/wrf/users/wrfv3.5/Registry.EM_COMMON

```

## Download and extract WPS

```
wget http://www2.mmm.ucar.edu/wrf/src/WPSV3.5.1.TAR.gz
mkdir WRSV351
tar -xzvf WPSV3.5.1.TAR.gz --strip-components 1 -C ./WPSV351/


wget http://www2.mmm.ucar.edu/wrf/src/wps_files/geog_complete.tar.bz2 
tar -xvjf geog_complete.tar.bz2 
```

## Compiling WRF

On blacklight.psc.edu, the netcdf4 lib with parallel computing capability is version 4.1.3 compiled with phdf5 1.8.7. 
So we have to change configure.{wps|wrf} to add phdf5 lib path, otherwise netcdf lib linking fails. 

# 

`install/namelist.wps` bounding box for domain

## To change to US

1. design domain

 * namelist.wps
  * change ref lat, lon; intersection is center point of new domain
  * choose map projection: lambert conformal for mid-latitudes (28-50 degrees N/S) will minimize distortion.
  * size: 100 x 100 grid cells (outer and inner)
  * design domain to capture weather features (i.e. on coast, need to capture enough region)
   * Midwest thunderstorm size, need outer domain to cover gulf of mexico, or have boundary conditions that resolve these features. 
  * met: NARR 32km resolution
   * nest down to 18km, 6km, 2km
   * compare to obs - anything should be justified and validated
   * make outer domain as large as possible to prevent numerical artifacts
   * nudging - used in Brazil; (may not be) not necessary in US
  * land use: MODIS or USGS
  * Topography: USGS
  * settings for invariant data: inb_* geopotential, land sea mask
 * `Vtable`
  * WPSV351/ungrib/Variable_Tables/
   * different lookup tables for different boundary condition data
   * we are using era-interim, land sea mask: European
  * wrfv351_eraint_1_5_2_1_1_1_1_spectral_18-6-2km/Vtable from 

Select boundary conditions that best fit observations. We are using era-interim, but cfsr2 is better for other portions of South America. For this particular run, did not want to have to compare CFSR2 to obs; therefore choice of era interim based on prev. publication (will get citation from Scott). Need to be converted to wrf intermediate product.

2. build geo_em files 

 * geogrid.exe reads namelist.wps and builds binary netcdf files
  * generally ensure settings in namelist.wps translated into 'clean' domain
  * `geo_em/d*` make sure that map-factor close to 1 to minimize distortion
  * look in ncview to make sure topography, land use looks okay
 
3. prepare grib files

ERA-Int_pl_1987-09-29.grb
ERA-Int_pl_1987-09-30.grb
ERA-Int_sfc_1987-09-29.grb
ERA-Int_sfc_1987-09-30.grb

 * ungrib
 * convert to wrf intermediate
 *

4. namelist.input

    need to benchmark to optimize performance (iteratively changing namelist parms)

namelist.input control start/finish of run
 * start_* must be the same as restart files


4. TODO

 
 1. python era-int/retrieve_eraint_data_for_wps.py --strt_dt='1987-09-30' --end_dt='1987-10-02'
 2. download restart files
 2. change namelist to match start/end dates
 3. link files link_grib
 4. ungrib
 5. metgrid.exe
 6. real.exe