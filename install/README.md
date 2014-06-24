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
