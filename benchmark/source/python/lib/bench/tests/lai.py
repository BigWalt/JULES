###########################################################################
## This file contains the basin lai vs. observer ndvi test
###########################################################################

import os.path, numpy, json
import bench
from bench.tests.gswp2_constants import *

# Constants
BASINS = ["Amazon", "Congo", "Danube", "Lena", "Mackenzie", "Mississipi", "Niger", "Parana"]

RADIUS_EARTH = 6371.0


def write_json(filename, observed, old_model, new_model):
    """
    Writes a JSON file in the format expected by NVD3
    
    It assumes that observations will use y-axis 1 and model results will use y-axis 2
    """
    
    def get_values(data):
        return [{'x' : i, 'y' : v} for (i, v) in zip(range(0, len(data)), data)]
    
    # NVD3 expects an array of time-series dictionaries
    json_data = []
    json_data.append({
        'key' : 'Observed', 'type' : 'line', 'yAxis' : 1,
        'values' : get_values(observed.tolist())
    })
    json_data.append({
        'key' : 'Old model', 'type' : 'line', 'yAxis' : 2,
        'values' : get_values(old_model.tolist())
    })
    json_data.append({
        'key' : 'New model', 'type' : 'line', 'yAxis' : 2,
        'values' : get_values(new_model.tolist())
    })
    with open(filename, 'w') as f:
        json.dump(json_data, f, indent = 4)


def lai(data_dir, output_dir, json_dir):
    section = {
        'title' : 'Modelled LAI vs. observed NDVI', 'skipped' : False, 'results' : []
    }
            
    ####################################################################################
    # We are going to use the same model file to compute results for each basin
    # So to save overhead of reading the same thing for each basin, we read it here
    ####################################################################################
    model_out_file = os.path.join(output_dir, 'gswp2', 'carb.lai.nc')
    # If there is no data, return a skipped test result
    if not os.path.isfile(model_out_file):
        bench.log.warn('Skipping basin LAI test (no model output available)')
        section['skipped'] = True
        return section
    
    bench.log.info("Running basin LAI test")
    
    # Read the required variables from the output file
    model_data = bench.data.load_ncdf(model_out_file, only = ['frac', 'lai'])
    # We only need the frac for pft tiles
    # While we are indexing, we might as well remove the size 1 dimension
    frac = model_data['frac'][:, 0:5, 0, :]
    lai = model_data['lai'][:, :, 0, :]

    ###############################################################################
    # Compute the results
    ###############################################################################
    for basin in BASINS:
        bench.log.info("    " + basin)
        
        # Old model data is preprocessed into monthly values stored as ascii
        old_model = numpy.fromfile(
            os.path.join(data_dir, "lai", "model", basin + ".dat"), sep = " "
        )
        
        # Same for obs data
        observed = numpy.fromfile(
            os.path.join(data_dir, "lai", "obs", basin + ".dat"), sep = " "
        )
        
        # Read the basin specific data required to calculate the basin average
        # The basin points are 1-indexed - we want 0-indexed
        basin_pts = numpy.fromfile(
            os.path.join(data_dir, "lai", "basin_masks", basin + ".dat"), dtype = numpy.intp, sep = " "
        ) - 1
            
        # Calculate the weight for each basin grid box based on its latitude-adjusted area
        # We want the basin lats in radians
        basin_lats = numpy.fromfile(
            os.path.join(data_dir, "lai", "basin_masks", basin + "_lat.dat"), sep = " "
        ) * numpy.pi / 180.0
        areas = numpy.fabs(
            numpy.sin(basin_lats + (DLAT_RAD / 2)) - numpy.sin(basin_lats - (DLAT_RAD / 2))
        ) * (RADIUS_EARTH ** 2) * DLON_RAD
        weights = areas / areas.sum()
        
        # Calculate the effective lai for the points in the basin
        # Convert from a biggus array to a numpy array at this point so we can use
        # some masked array stuff that is not implemented for biggus arrays (yet)
        weighted_lai = (frac * lai)[:, :, basin_pts].ndarray()
        # Mask out any non-finite values and sum over the pfts
        effective_lai = numpy.ma.masked_array(
            weighted_lai, numpy.logical_not(numpy.isfinite(weighted_lai))
        ).sum(axis = 1)
        
        # Work out the mean lai for each point for each month over the whole range
        new_model = numpy.empty((12))
        for month in range(0, 12):
            new_model[month] = numpy.sum(effective_lai[month:119:12, :].mean(axis = 0) * weights)
            
        # Write the JSON file
        write_json(
            os.path.join(json_dir, 'lai-%s.json' % basin.lower()),
            observed, old_model, new_model
        )
        
        # Append a result to the section
        section['results'].append({
            'template' : 'lai-result.html',
            'basin' : basin,
            'file' : 'lai-%s.json' % basin.lower()
        })
        
    return section
            