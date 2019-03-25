###########################################################################
## This file contains utilities and tests for budget closures
###########################################################################

import os, glob, json, numpy
import bench
from bench.tests.gswp2_constants import *

# Constant
LF = 0.334e6


def write_json(data_dir, filename, data):
    # Before we write the JSON file, we map to the full GSWP2 grid
    
    # Get glon and glat from the mapping file
    mapping = bench.data.load_ncdf(
        os.path.join(data_dir, 'gswp2', 'mapping.nc'), only = ['glat', 'glon']
    )
    # Data is 1-indexed, Python uses 0-indexing
    glat = (mapping['glat'].ndarray() - 1).astype(int)
    glon = (mapping['glon'].ndarray() - 1).astype(int)
    
    nx = int((LON_MAX - LON_MIN) / DLON) + 1
    ny = int((LAT_MAX - LAT_MIN) / DLAT) + 1
    # Map the data
    gridded = numpy.empty((ny, nx))
    # Use a fill value that can never occur with absolute errors, i.e. a -ve number
    gridded[:] = -1e20
    gridded[glat, glon] = data[:]
    
    # Write the file
    with open(filename, 'w') as f:
        json.dump(gridded.tolist(), f, indent = 4, allow_nan = False)


def closures(data_dir, output_dir, json_dir):
    section = {
        'title' : 'Closure of budgets', 'skipped' : False, 'results' : []
    }
    
    # See if any output exists for the tests
    if not glob.glob(os.path.join(output_dir, 'gswp2', 'clos.*.nc')):
        bench.log.warn('Skipping budget closure tests (no model output available)')
        section['skipped'] = True
        return section
    
    section['results'].append(energy(data_dir, output_dir, json_dir))
    section['results'].append(water(data_dir, output_dir, json_dir))
    section['results'].append(carbon(data_dir, output_dir, json_dir))
    
    return section


def energy(data_dir, output_dir, json_dir):
    bench.log.info('Running energy balance closure test')
    
    # Read the variables from the output file
    data = bench.data.load_ncdf(
        os.path.join(output_dir, 'gswp2', 'clos.energy.nc'),
        only = ["surf_ht_store", "rad_net_tile", "ftl", "le", 
                "surf_ht_flux", "anthrop_heat", "snow_melt"]
    )
    
    # Calculate the maximum error in the energy balance for each land point
    errors = (data['rad_net_tile'] + data['anthrop_heat'] - data['surf_ht_store'] -
              data['ftl'] - data['le'] - data['surf_ht_flux'] - (data['snow_melt'] * LF))
    # Since we can only do a max along one axis at once with biggus arrays (currently),
    # we apply it once each over the time and tile axes
    errors = errors.abs().max(axis = 0).max(axis = 0).ndarray().squeeze()
    
    # Write the JSON file
    write_json(data_dir, os.path.join(json_dir, 'closure-energy.json'), errors)
    
    return {
        'template' : 'closure-result.html',
        'title' : 'Energy balance',
        'max' : errors.max(),
        'file' : 'closure-energy.json'
    }
    
    
def water(data_dir, output_dir, json_dir):
    bench.log.info('Running water balance closure test')
    
    # Read the variables from the output file
    data = bench.data.load_ncdf(
        os.path.join(output_dir, 'gswp2', 'clos.water.nc'),
        only = ["precip", "fqw_gb", "elake", "runoff", 
                "smc_tot", "canopy_gb", "snow_mass_gb"]
    )
    
    # Calculate the maximum error in the water balance for each land point
    
    # Each output variable has dimensions land_pts * timesteps
    # First calculate the net flux and total stored water for each timestep
    net_flux = data['precip'] - (data['fqw_gb'] - data['elake']) - data['runoff']
    total_store = data['smc_tot'] + data['canopy_gb'] + data['snow_mass_gb']
    
    # For each point, we want to check that change_in_stores = net_flux * tstep_length
    # for each timestep
    #
    # That is for a vector of values for total water stores S, a vector of values for net
    # water flux F and timestep length t, we want the following to hold for each timestep i:
    #     S{i} - S{i-1} = F{i} * t
    #
    # So first calculate the timestep by timestep change in stores for each land point
    # There will be one less timestep in the delta than in the original stores vector
    delta_store = total_store[1:,:] - total_store[:-1,:]

    # Get the maximum error for each point
    # We exclude the first timestep from the net fluxes vector as per the equation above
    errors = (delta_store - (net_flux[1:,:] * TIMESTEP_LEN)).abs().max(axis = 0).ndarray().squeeze()
    
    # Write the JSON file
    write_json(data_dir, os.path.join(json_dir, 'closure-water.json'), errors)
    
    return {
        'template' : 'closure-result.html',
        'title' : 'Water balance',
        'max' : errors.max(),
        'file' : 'closure-water.json'
    }


def carbon(data_dir, output_dir, json_dir):
    bench.log.info('Running carbon balance closure test')
    
    # Read the variables from the output file
    data = bench.data.load_ncdf(
        os.path.join(output_dir, 'gswp2', 'clos.carb.nc'),
        only = ["npp_gb", "resp_s_gb", "cs_gb", "cv"]
    )
    
    # Calculate the maximum error in the carbon balance for each land point
    
    # Each output variable has dimensions land_pts * timesteps
    # First calculate the net flux and total stored carbon for each timestep
    net_flux = data['npp_gb'] - data['resp_s_gb']
    total_store = data['cs_gb'] + data['cv']
    
    # For each point, we want to check that change_in_stores = net_flux * tstep_length
    # for each timestep
    #
    # That is for a vector of values for total water stores S, a vector of values for net
    # water flux F and timestep length t, we want the following to hold for each timestep i:
    #     S{i} - S{i-1} = F{i} * t
    #
    # So first calculate the timestep by timestep change in stores for each land point
    # There will be one less timestep in the delta than in the original stores vector
    delta_store = total_store[1:,:] - total_store[:-1,:]

    # Get the maximum error for each point
    # We exclude the first timestep from the net fluxes vector as per the equation above
    errors = (delta_store - (net_flux[1:,:] * TIMESTEP_LEN)).abs().max(axis = 0).ndarray().squeeze()
    
    # Write the JSON file
    write_json(data_dir, os.path.join(json_dir, 'closure-carbon.json'), errors)
    
    return {
        'template' : 'closure-result.html',
        'title' : 'Carbon balance',
        'max' : errors.max(),
        'file' : 'closure-carbon.json'
    }
