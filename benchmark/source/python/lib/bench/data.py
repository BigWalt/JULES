#####################################################################
## This file contains various data-related routines used by the
## benchmarking
#####################################################################

import re, collections, operator, numpy, netCDF4, biggus
import bench.biggus_extras as biggex    
    

def load_data_frame(filename):
    """
    Reads a file produced from an R dataframe into a python dict
    of (extended) biggus arrays
    """
    
    data = {}    
    with open(filename, 'r') as file:
        # Get the lines in the file with no newline characters
        lines = map(lambda s: s.strip(), file.readlines())
        # The first line contains the column names
        cols = map(lambda s: s.strip("'\"").lower(), lines[0].split())
        for name in cols:
            data[name] = []
        # Iterate through the rest of the lines, appending to the
        # correct column
        for line in lines[1:]:
            # The first item in each line is a row name, which we ignore
            values = map(float, line.split()[1:])
            for idx, name in enumerate(cols):
                data[name].append(values[idx])
    # Convert to a numpy array
    for name in data:
        data[name] = biggex.WithOperators(
            biggus.NumpyArrayAdapter(numpy.array(data[name]))
        )
    return data


def load_ncdf(filename, only = None):
    """
    Loads the specified variables from a NetCDF file into a python
    dict of (extended) biggus arrays

    If only is given, only those variables are returned
    """
    
    data = {}
    dataset = netCDF4.Dataset(filename)
    var_names = only if only is not None else dataset.variables.keys()
    for name in var_names:
        # Remove all dimensions of size 1
        data[name] = biggex.WithOperators(
            biggus.OrthoArrayAdapter(dataset.variables[name])
        )
    return data


def load_jules_ascii(filename, only = None):
    """
    Loads the specified variables from a JULES ASCII file into a python
    dict of (extended) biggus arrays

    If only is given, only those variables are returned
    """
    
    with open(filename, 'r') as file:
        try:
            # Find the dimensions in the file
            while True:
                line = file.next()
                if line.startswith('# Dimensions'):
                    break
                
            dims = {}
            has_record_dim = False
            while True:
                line = file.next()
                if re.match(r"^# *$", line):
                    break
                if "UNLIMITED" in line:
                    has_record_dim = True
                    continue
                match = re.search(r"(\w+) = (\d+)", line)
                if match:
                    dims[match.group(1)] = int(match.group(2))

            # Find the variables in the file
            while True:
                line = file.next()
                if line.startswith('# Variables'):
                    break
            
            # We want to maintain the ordering of variables
            variables = collections.OrderedDict()
            while True:
                line = file.next()
                # The first non-comment line is the first line of data
                if not line.startswith('#'):
                    break
                # Ignore any lines that don't match (attributes or blank)
                match = re.search(r"(\w+)\(([\w,]+)\)", line)
                if match:
                    var_dims = match.group(2).split(',')
                    # If there is a record dimension, exclude it
                    if has_record_dim:
                        var_dims = var_dims[:-1]
                    # Map dimension names to sizes
                    # Because Python uses row major order, we want the dimensions in reverse
                    variables[match.group(1)] = {
                        'shape' : map(lambda d: dims[d], var_dims[::-1]),
                        'size' : reduce(operator.mul, shape, 1)
                    }
        except StopIteration:
            print "[FATAL ERROR] File has unexpected format"
            raise
        
        # Now we read data until the end of the file - we use a new try because
        # we now don't want to rethrow
        data = {}
        if only is not None:
            for name in only:
                data[name] = []
        else:
            for name in variables:
                data[name] = []
        
        try:
            while True:
                # The first line of values has already been consumed by the
                # variable "hunting", so we advance the line at the end of the loop
                values = map(float, line.strip().split())
                # We have to loop over all the discovered variables, as we at least need
                # to consume their data, even if we aren't returning them
                for name in variables:
                    if name in data:
                        data[name].append(
                            numpy.reshape(
                                values[:variables[name]['size']], variables[name]['shape']
                            )
                        )
                    values = values[variables[name]['size']:]
                line = file.next()
        except StopIteration:
            # Swallow the end of iteration
            pass
        
        # Convert to biggus arrays via numpy arrays
        for name in data:
            data[name] = biggex.WithOperators(
                biggus.NumpyArrayAdapter(numpy.array(data[name]))
            )
        return data
    
