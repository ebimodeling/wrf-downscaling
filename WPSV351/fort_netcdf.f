        program foo
        include 'netcdf.inc'
        integer ncid , status 
        status = nf_open ( 'foo.nc' , 0 , ncid ) 
        print *,'status = ',status
        end program
