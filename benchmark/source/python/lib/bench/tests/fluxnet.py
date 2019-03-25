###########################################################################
## This file contains utilities and tests for the fluxnet sites
###########################################################################

import os, glob, json, math, numpy, scipy.stats
import bench


SITES = [
    { "id" : "bondville", "name" : "Bondville" },
    { "id" : "el-saler", "name" : "El Saler" },
    { "id" : "fort-peck", "name" : "Fort Peck" },
    { "id" : "harvard", "name" : "Harvard Forest" },
    { "id" : "hyytiala", "name" : "Hyytiala" },
    { "id" : "kaamanen", "name" : "Kaamanen" },
    { "id" : "morgan-mon", "name" : "Morgan Monroe" },
    { "id" : "santa67", "name" : "Santarem km67" },
    { "id" : "santa77", "name" : "Santarem km77" },
    { "id" : "tharandt", "name" : "Tharandt" }
]

KG_TO_MICROMOLES = 1.0e3 * 0.0832591 * 1.0e6


def corr_sig_test(r1, r2, n):
    """Takes two correlation coefficients and a sample size and indicates the
       probability of the difference being due to chance
       The method is taken from RBG Williams, Introduction to Statistics
       for Geographers and Earth Scientists"""
    
    z1 = 0.5 * math.log((1 + r1) / (1 - r1))
    z2 = 0.5 * math.log((1 + r2) / (1 - r2))

    std_err = math.sqrt((2.0 / (n - 3.0)))

    z = abs(z1 - z2) / std_err

    # We now look for the probability of the value of Z being in the
    # tail of a normal distribution (upper tail if Z >= 0, lower tail if Z < 0)
    if z < 0:
        p_value = scipy.stats.norm.cdf(z)
    else:
        p_value = 1.0 - scipy.stats.norm.cdf(z)
    # Because we want a two-tailed probability, double it
    return 2.0 * p_value


def rmse(x1, x2):
    """Returns the root-mean-square-error between two arrays"""
    
    return numpy.sqrt(((x1 - x2) ** 2).mean())


def statistics(observed, old_model, new_model):
    stats = {}
    
    # Calculate the mean information
    obs_mean = numpy.mean(observed)
    diff_new = abs(numpy.mean(new_model) - obs_mean)
    diff_old = abs(numpy.mean(old_model) - obs_mean)
    (_, p_value) = scipy.stats.ttest_ind(new_model, old_model)
    # If there is < 50% chance of the difference being due to chance, we
    # report a statistically significant difference
    if p_value > 0.5:
        status = 'no-change'
    elif diff_new < diff_old:
        status = 'better'
    else:
        status = 'worse'
    
    stats['mean'] = { 'new' : diff_new, 'old' : diff_old, 'status' : status }
        
    # Calculate the correlation information
    (corr_new, _) = scipy.stats.pearsonr(observed, new_model)
    (corr_old, _) = scipy.stats.pearsonr(observed, old_model)
    p_value = corr_sig_test(corr_new, corr_old, new_model.size)
    # If there is < 50% chance of the difference being due to chance, we
    # report a statistically significant difference
    if p_value > 0.5:
        status = 'no-change'
    elif corr_new > corr_old:
        status = 'better'
    else:
        status = 'worse'
        
    stats['corr'] = { 'new' : corr_new, 'old' : corr_old, 'status' : status }
    
    # Calculate the RMSE information
    rmse_old = rmse(observed, old_model)
    rmse_new = rmse(observed, new_model)
    
    # We test whether the rmse is within 5% of the old rmse for a result of no-change
    if (abs(rmse_old - rmse_new) / rmse_old) < 0.05:
        status = 'no-change'
    elif rmse_new < rmse_old:
        status = 'better'
    else:
        status = 'worse'
        
    stats['rmse'] = { 'new' : rmse_new, 'old' : rmse_old, 'status' : status }
        
    return stats


def write_json(filename, observed, old_model, new_model):
    """
    Writes a JSON file in the format expected by NVD3
    """
    
    def get_values(data):
        return [{'x' : i, 'y' : v} for (i, v) in zip(range(0, len(data)), data)]
    
    # NVD3 expects an array of time-series dictionaries
    json_data = []
    json_data.append({ 'key' : 'Observed', 'values' : get_values(observed.tolist()) })
    json_data.append({ 'key' : 'Old model', 'values' : get_values(old_model.tolist()) })
    json_data.append({ 'key' : 'New model', 'values' : get_values(new_model.tolist()) })
    with open(filename, 'w') as f:
        json.dump(json_data, f, indent = 4)


def evap(data_dir, output_dir, json_dir):
    section = {
        'title' : 'FLUXNET evaporation', 'skipped' : False, 'results' : []
    }
    
    # See if any output exists for the test
    if not glob.glob(os.path.join(output_dir, 'fluxnet', '*.evap.*')):
        bench.log.warn('Skipping FLUXNET evaporation test (no model output available)')
        section['skipped'] = True
        return section
    
    bench.log.info("Running FLUXNET evaporation test")
        
    for site in SITES:
        # Load the "new model" data
        # Prefer NetCDF files if available
        nc_file = os.path.join(output_dir, 'fluxnet', site['id'] + '.evap.nc')
        asc_file = os.path.join(output_dir, 'fluxnet', site['id'] + '.evap.asc')
        if os.path.isfile(nc_file):
            new_model = bench.data.load_ncdf(nc_file, only = ['latent_heat'])
        elif os.path.isfile(asc_file):
            new_model = bench.data.load_jules_ascii(asc_file, only = ['latent_heat'])
        else:
            bench.log.warn('    Skipping %s (no model output available)' % site['name'])
            continue
        
        # Extract the latent heat from the dataset
        new_model = new_model['latent_heat'].ndarray().squeeze()
        
        bench.log.info("    " + site['name'])
        
        # Load the old model data and observations
        old_model = bench.data.load_data_frame(
            os.path.join(data_dir, 'fluxnet', 'model', site['id'] + '.dat')
        )['lambdae'].ndarray().squeeze()
        observed = bench.data.load_data_frame(
            os.path.join(data_dir, 'fluxnet', 'obs', site['id'] + '.dat')
        )['lambdae'].ndarray().squeeze()
        
        # Write the JSON data file
        write_json(
            os.path.join(json_dir, 'fluxnet-evap-%s.json' % site['id']),
            observed, old_model, new_model
        )
        
        # Append a result to the section
        section['results'].append({
            'template' : 'fluxnet-result.html',
            'site' : site,
            'stats' : statistics(observed, old_model, new_model),
            'file' : 'fluxnet-evap-%s.json' % site['id'],
            'ylab' : 'Latent heat (W/m2)'
        })
        
    return section


def gpp(data_dir, output_dir, json_dir):
    section = {
        'title' : 'FLUXNET gross primary productivity', 'skipped' : False, 'results' : []
    }
    
    # See if any output exists for the test
    if not glob.glob(os.path.join(output_dir, 'fluxnet', '*.carb.*')):
        bench.log.warn('Skipping FLUXNET GPP test (no model output available)')
        section['skipped'] = True
        return section
    
    bench.log.info("Running FLUXNET GPP test")
        
    for site in SITES:
        # Skip santa77 as we have no observations
        if site['id'] == "santa77":
            continue
        
        # Load the "new model" data
        # Prefer NetCDF files if available
        nc_file = os.path.join(output_dir, 'fluxnet', site['id'] + '.carb.nc')
        asc_file = os.path.join(output_dir, 'fluxnet', site['id'] + '.carb.asc')
        if os.path.isfile(nc_file):
            new_model = bench.data.load_ncdf(nc_file, only = ['gpp_gb'])
        elif os.path.isfile(asc_file):
            new_model = bench.data.load_jules_ascii(asc_file, only = ['gpp_gb'])
        else:
            bench.log.warn('    Skipping %s (no model output available)' % site['name'])
            continue
        
        # Extract just the gpp from the dataset and convert the units
        new_model = new_model['gpp_gb'].ndarray().squeeze() * KG_TO_MICROMOLES
        
        bench.log.info("    " + site['name'])
        
        # Load the old model data and observations
        old_model = bench.data.load_data_frame(
            os.path.join(data_dir, 'fluxnet', 'model', site['id'] + '.dat')
        )['gpp'].ndarray().squeeze()
        observed = bench.data.load_data_frame(
            os.path.join(data_dir, 'fluxnet', 'obs', site['id'] + '.dat')
        )['gpp'].ndarray().squeeze()
        
        # Write the JSON data file
        write_json(
            os.path.join(json_dir, 'fluxnet-gpp-%s.json' % site['id']),
            observed, old_model, new_model
        )
        
        # Append a result to the section
        section['results'].append({
            'template' : 'fluxnet-result.html',
            'site' : site,
            'stats' : statistics(observed, old_model, new_model),
            'file' : 'fluxnet-gpp-%s.json' % site['id'],
            'ylab' : 'GPP (micromoles C/m2/s)'
        })
        
    return section


def rsp(data_dir, output_dir, json_dir):
    section = {
        'title' : 'FLUXNET total respiration', 'skipped' : False, 'results' : []
    }
    
    # See if any output exists for the test
    if not glob.glob(os.path.join(output_dir, 'fluxnet', '*.carb.*')):
        bench.log.warn('Skipping FLUXNET respiration test (no model output available)')
        section['skipped'] = True
        return section
    
    bench.log.info("Running FLUXNET respiration test")
        
    for site in SITES:
        # Skip santa77 as we have no observations
        if site['id'] == "santa77":
            continue
        
        # Load the "new model" data
        # Prefer NetCDF files if available
        nc_file = os.path.join(output_dir, 'fluxnet', site['id'] + '.carb.nc')
        asc_file = os.path.join(output_dir, 'fluxnet', site['id'] + '.carb.asc')
        if os.path.isfile(nc_file):
            new_model = bench.data.load_ncdf(nc_file, only = ['resp_s_gb', 'resp_p_gb'])
        elif os.path.isfile(asc_file):
            new_model = bench.data.load_jules_ascii(asc_file, only = ['resp_s_gb', 'resp_p_gb'])
        else:
            bench.log.warn('    Skipping %s (no model output available)' % site['name'])
            continue
        
        # Add the respirations from the dataset and convert the units
        new_model = (new_model['resp_s_gb'].ndarray().squeeze() +
                     new_model['resp_p_gb'].ndarray().squeeze()) * KG_TO_MICROMOLES
        
        bench.log.info("    " + site['name'])
        
        # Load the old model data and observations
        old_model = bench.data.load_data_frame(
            os.path.join(data_dir, 'fluxnet', 'model', site['id'] + '.dat')
        )['rsp'].ndarray().squeeze()
        observed = bench.data.load_data_frame(
            os.path.join(data_dir, 'fluxnet', 'obs', site['id'] + '.dat')
        )['rsp'].ndarray().squeeze()
        
        # Write the JSON data file
        write_json(
            os.path.join(json_dir, 'fluxnet-rsp-%s.json' % site['id']),
            observed, old_model, new_model
        )
        
        # Append a result to the section
        section['results'].append({
            'template' : 'fluxnet-result.html',
            'site' : site,
            'stats' : statistics(observed, old_model, new_model),
            'file' : 'fluxnet-rsp-%s.json' % site['id'],
            'ylab' : 'Total respiration (micromoles C/m2/s)'
        })
        
    return section
        