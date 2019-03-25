import rose.upgrade
import re

from .version34_40 import *
from .version40_41 import *
from .version41_42 import *
from .version42_43 import *
from .version43_44 import *
from .version44_45 import *
from .version45_46 import *
from .version46_47 import *
from .version47_48 import *
from .version48_49 import *
from .version49_50 import *
from .version50_51 import *
from .version51_52 import *


class vn52_txxx(rose.upgrade.MacroUpgrade):

    """Upgrade macro from JULES by Author"""

    BEFORE_TAG = "vn5.2"
    AFTER_TAG = "vn5.2_txxx"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        return config, self.reports


'''
Do not edit any lines below this point - this is the template.
'''


class vn52_txxx(rose.upgrade.MacroUpgrade):

    """Upgrade macro from JULES by Author"""

    BEFORE_TAG = "vn5.2"
    AFTER_TAG = "vn5.2_txxx"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        return config, self.reports
