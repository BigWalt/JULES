###########################################################################
## This file contains the basin lai vs. observer ndvi test
###########################################################################

import os.path, numpy, json
import bench
from bench.tests.gswp2_constants import *

# Constants
FT_NAMES = ["Broadleaf tree", "Needleleaf tree", "C3 grass", "C4 grass", "Shrub", "Bare soil"]
FT_ABBR = ["bl", "nl", "c3", "c4", "sh", "bs"]
# We are only interested in the pft (1-5) and bare soil (8) tiles, but 0-indexed
FT_TILES = numpy.array([0, 1, 2, 3, 4, 7])


def write_json(filename, observed, old_model, new_model):
    """
    Writes a JSON file in the format expected by NVD3
    
    It assumes that observations will use y-axis 1 and model results will use y-axis 2
    """
    
    def get_values(data):
        return [{'x' : i, 'y' : v} for (i, v) in zip(range(0, len(data)), data)]
    
    # NVD3 expects an array of time-series dictionaries
    json_data = []
    json_data.append({ 'key' : 'Potential vegetation', 'values' : get_values(observed.tolist()) })
    json_data.append({ 'key' : 'Old model', 'values' : get_values(old_model.tolist()) })
    json_data.append({ 'key' : 'New model', 'values' : get_values(new_model.tolist()) })
    with open(filename, 'w') as f:
        json.dump(json_data, f, indent = 4)


def frac(data_dir, output_dir, json_dir):
    section = {
        'title' : 'Modelled LAI vs. observed NDVI', 'skipped' : False, 'results' : []
    }
            
    # Check if data from a model run is available for analysis
    model_out_file = os.path.join(output_dir, 'gswp2', 'triff.frac.nc')
    # If there is no data, return a skipped test result
    if not os.path.isfile(model_out_file):
        bench.log.warn('Skipping dominant pft test (no model output available)')
        section['skipped'] = True
        return section
    
    bench.log.info("Running dominant pft test")
    
    # Get the model data we are interested in
    model_data = bench.data.load_ncdf(model_out_file, only = ['frac'])
    # We only need the last timestep of frac for the tiles we are interested in
    # While we are indexing, we might as well remove the size 1 grid dimension as well
    # Just go straight to a numpy array from a biggus array as we don't need to worry
    # too much about the volume of data
    frac = model_data['frac'][-1, FT_TILES, 0, :].ndarray()
    
    # To account for the fact that frac does not sum to 1 for every land point (because
    # we are not considering the fraction of lakes, urban and ice, we normalise frac
    # against the total (pft + bare soil) frac for that grid cell
    # We have to account for points where tot_frac is 0...
    # Since frac is >= 0, tot_frac is only 0 when all 6 fts have 0 frac
    # In this case, we can just set tot_frac to 1 at these points, since
    # we want the corrected frac to also have 0 for all fts at these points (and 0/1 = 0)
    tot_frac = frac.sum(axis = 0)
    tot_frac[tot_frac < 1e-6] = 1.0
    corrected_frac = numpy.empty(frac.shape)
    for ft in range(0, frac.shape[0]):
        corrected_frac[ft, :] = frac[ft, :] / tot_frac
    
    # Get the mask that indicates where each ft is dominant
    weights = bench.data.load_ncdf(
        os.path.join(data_dir, 'frac', 'dom_pft_weights.nc'), only = ['weights']
    )['weights'].ndarray()
    
    # Read the potential veg and old model data
    pot_veg = bench.data.load_data_frame(os.path.join(data_dir, 'frac', 'pot_veg_dist.dat'))
    old_model = bench.data.load_data_frame(os.path.join(data_dir, 'frac', 'old_model_dist.dat'))
    
    # Build the results for each functional type
    n_fts = len(FT_NAMES)
    for ft in range(0, n_fts):
        bench.log.info("    " + FT_NAMES[ft] + " dominant")
        
        # Find the points where the current ft is dominant
        ft_pts = weights[ft, :] > 0
        
        # Find the mean weighted frac for each ft across those points
        new_model = numpy.empty((n_fts,))
        for i in range(0, n_fts):
            new_model[i] = (corrected_frac[i, ft_pts] * weights[ft, ft_pts]).mean()
            
        # Write the JSON file
        write_json(
            os.path.join(json_dir, 'frac-%s.json' % FT_ABBR[ft]),
            pot_veg[FT_ABBR[ft]].ndarray(), old_model[FT_ABBR[ft]].ndarray(), new_model
        )
        
        # Append a result to the section
        section['results'].append({
            'template' : 'frac-result.html',
            'ft' : FT_NAMES[ft],
            'file' : 'frac-%s.json' % FT_ABBR[ft]
        })
        
    return section
            