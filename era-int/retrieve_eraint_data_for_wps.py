#							 Script to download ERA-interim data for WPS
#									 Neil Berg, October 11 2013
#                                                                        Scott Capps, Dec. 3, 2013
#										Vertum Partners 
#####################################################################################################
# load modules
from ecmwfapi import ECMWFDataServer
import calendar
import os,sys,getopt
import datetime
import logging

# python retrieve_eraint_data_for_wps.py --strt_dt='1987-01-01' --end_dt='1987-01-03'

def server_test(logger,attempts=0,timeout=100,sleep_int=1):
	import time
	if attempts <= timeout:
		logger.info('trying to connect to server')
		try:
			server = ECMWFDataServer()
			del server
		except:
			logger.error('Failed attempt at connecting to server, sleeping...')
			time.sleep(sleep_int)
			return server_test(attempts+1)            
	else:
		logger.error('*** Cannot connect to server, stopping...')
		return False
	return True

def server_retrieve(logger,server,attempts=0,timeout=100,sleep_int=1):
	import time
	if attempts<=timeout:
		logger.info('trying to retrieve data for date: %s' %(str(curr_dt_fmt)))
		try:
			# Download N128 Gaussian Grid Surface Variables 
			server.retrieve({
					'dataset'  : "interim",
					'date'	   : str(curr_dt_fmt),
					'time'	   : "00/06/12/18",
					'step'	   : "0",
					'levtype'  : "sfc",
					'stream'   : "oper",
					'type'	   : "an",
					'class'    : "ei",
					'grid'	   : "128",
					'param'    : "165/166/167/168/134/151/235/31/34/33/141/139/170/183/236/39/40/41/42",
					'target'   : "ERA-Int_sfc_"+str(curr_dt_fmt)+".grb",
					})	
			#
			#	3D VARIABLES	
			#
			server.retrieve({
					'dataset'  : "interim",
					'date'	   : str(curr_dt_fmt),
					'time'	   : "00/06/12/18",
					'step'	   : "0",
					'stream'   : "oper",
					'levtype'  : "pl",
					'levelist' : "all",
					'type'	   : "an",
					'class'    : "ei",
					'grid'	   : "128",
					'param'    : "129/130/131/132/133/157",
					'target'   : "ERA-Int_pl_"+str(curr_dt_fmt)+".grb",
					})
		except:
			logger.error('Failed attempt at connecting to server, sleeping...')
			time.sleep(sleep_int)
			return server_retrieve(logger,server,attempts+1)
	else:
		logger.error('Failed to retrieve data')
		return False
	return True
#
# Process Command Line Argument
options, args = getopt.getopt(sys.argv[1:],'',['strt_dt=','end_dt='])
#
for option,value in options:
	if option in ('--strt_dt'):
		strt_dt = value
	elif option in ('--end_dt'):
		end_dt = value
# END: Process Command Line Argument
#
# BEGIN:  LOGGING ******************************************************
log_file        = './retrieve_eraint_data_for_wps.log'
os.system('/bin/rm -f '+log_file)
logger          = logging.getLogger('retrieve_eraint_data_for_wps.py')
hdlr            = logging.FileHandler(log_file)
formatter = logging.Formatter('%(asctime)s : Line: %(lineno)d - %(levelname)s  ::  %(message)s', datefmt='%m/%d/%Y %I:%M:%S %p')
hdlr.setFormatter(formatter)
logger.addHandler(hdlr)
logger.setLevel(logging.INFO) # DEBUG,INFO,WARNING,ERROR,CRITICAL
# END: LOGGING *********************************************************
#
# Get dates in datetime object 
dt_fmt = '%Y-%m-%d'
strt_dt_fmt = datetime.datetime.strptime(strt_dt,dt_fmt)
end_dt_fmt	= datetime.datetime.strptime(end_dt,dt_fmt)
ndays = (end_dt_fmt - strt_dt_fmt).days
#
#
# Begin: Retrieve data -------------------------------------------------
#
#				 INVARIANT - GEOPOTENTIAL AND LAND SURFACE MASK
#
#server.retrieve({
#	'dataset'  : "interim",
#	'date'	   : "1989-01-01",
#	'time'	   : "12",
#	'step'	   : "0",
#	'levtype'  : "sfc",
#	'stream'   : "oper",
#	'type'	   : "an",
#	'class'    : "ei",
#	'grid'	   : "0.75/0.75",
#	'param'    : "129",
#	'target'   : "inv_geopotential2.grb",
#	})	

#server.retrieve({
#	'dataset'  : "interim",
#	'date'	   : "1989-01-01",
#	'time'	   : "12",
#	'step'	   : "0",
#	'levtype'  : "sfc",
#	'stream'   : "oper",
#	'type'	   : "an",
#	'class'    : "ei",
#	'grid'	   : "0.75/0.75",
#	'param'    : "172",
#	'target'   : "inv_landseamask.grb",
#	})	
#
#
#				SURFACE VARIABLES
#			
for idx in range(ndays):
	curr_dt = strt_dt_fmt + datetime.timedelta(days=int(idx))
	curr_yr	= curr_dt.year
	curr_mo	= curr_dt.month
	curr_dy	= curr_dt.day
	curr_dt_fmt = datetime.date(curr_yr,curr_mo,curr_dy)
	# Initialize the ECMWF data server
	if server_test(logger):
		logger.info('Server is up...')
		server = ECMWFDataServer()
	else:
		logger.info('Unable to connect to the server')
		sys.exit('Could not connect to the server')

	if server_retrieve(logger,server):
		logger.info('Retrieved data')
		del server
	else:
		logger.error('Retrieving data')
		sys.exit()
#
# End: Retrieve ERA-interim data
