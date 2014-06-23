#module purge
#module load torque mpt icc/11.1 ifort/11.1 imkl psc_path phdf5/1.8.7 netcdf/4.1.3 globus xdusage szip zlib
module load phdf5/1.8.7 netcdf/4.1.3

export EDITOR=emacs
export CC=icc
export FC=ifort
export F77=ifort
export F90=ifort
export CXX=icpc
ulimit -s unlimited
export OMP_NUM_THREADS=1
export NETCDF=${NETCDF_DIR}
export NETCDF_BIN=${NETCDF_DIR}/bin
export NETCDF_LIB=${NETCDF_DIR}/lib
export NETCDF_INC=${NETCDF_DIR}/include
export PS1='\u@\h:\w\$ ' # Prompt user@host:cwd$
alias rm='rm -i'
alias cp='cp -i -p'
alias mv='mv -i'
eval `dircolors -b`
alias ls='ls -F --color=auto'
alias mkdir='mkdir -p'
export WRFIO_NCD_LARGE_FILE_SUPPORT=1
export J="-j 4"

# add phdf5 path to make wrf compile
export PHDF5=/usr/local/packages/phdf5-1.8.7

# python lib for getting ECMWDataServer
export PYTHONPATH=$HOME/pythonlibs:$PYTHONPATH
