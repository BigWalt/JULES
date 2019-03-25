#########################################################################
# Contains constants required for the gswp2 dataset
#########################################################################

import math


# Constants
TIMESTEP_LEN = 3600.0

# In degrees
DLAT = 1.0
DLON = 1.0
# In radians
DLAT_RAD = DLAT * math.pi / 180.0
DLON_RAD = DLON * math.pi / 180.0

# Max/min in degrees
LAT_MIN = -59.5
LAT_MAX = 89.5
LON_MIN = -179.5
LON_MAX = 179.5
