import rose.upgrade


class vn40_vn41(rose.upgrade.MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.0"
    AFTER_TAG = "vn4.1"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn41_vn42(rose.upgrade.MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.1"
    AFTER_TAG = "vn4.2"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn42_t58(rose.upgrade.MacroUpgrade):
    """FCM make upgrade macro for JULES #58 by Matt Pryor"""

    BEFORE_TAG = "vn4.2"
    AFTER_TAG = "vn4.2_t58"

    def upgrade(self, config, meta_config=None):
        # Add the new environment variables
        self.add_setting(config, ["env", "JULES_PLATFORM"], "custom")
        self.add_setting(config, ["env", "JULES_REMOTE"], "local")
        self.add_setting(config, ["env", "JULES_REMOTE_HOST"], "localhost")
        self.add_setting(config, ["env", "JULES_OMP"], "noomp")

        # Replacement for JULES_PARALLEL
        parallel = self.get_setting_value(config, ["env", "JULES_PARALLEL"])
        self.remove_setting(config, ["env", "JULES_PARALLEL"])
        if parallel and parallel == "serial":
            self.add_setting(config, ["env", "JULES_MPI"], "nompi")
        elif parallel and parallel == "mpi":
            self.add_setting(config, ["env", "JULES_MPI"], "mpi")
        elif parallel is not None:
            # This captures the possibility that JULES_PARALLEL
            # might be set to take the value of another environment variable
            self.add_setting(config, ["env", "JULES_MPI"], parallel)
        else:
            self.add_setting(config, ["env", "JULES_MPI"], "nompi")

        # Replacement for JULES_NETCDF
        netcdf = self.get_setting_value(config, ["env", "JULES_NETCDF"])
        self.remove_setting(config, ["env", "JULES_NETCDF"])
        if netcdf and netcdf == "actual":
            self.add_setting(config, ["env", "JULES_NETCDF"], "netcdf")
        elif netcdf and netcdf == "dummy":
            self.add_setting(config, ["env", "JULES_NETCDF"], "nonetcdf")
        elif netcdf is not None:
            # This captures the possibility that JULES_NETCDF might
            # be set to take the value of another environment variable
            self.add_setting(config, ["env", "JULES_NETCDF"], netcdf)
        else:
            self.add_setting(config, ["env", "JULES_NETCDF"], "nonetcdf")

        return config, self.reports


class vn42_vn43(rose.upgrade.MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.2_t58"
    AFTER_TAG = "vn4.3"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn43_vn44(rose.upgrade.MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.3"
    AFTER_TAG = "vn4.4"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn44_vn45(rose.upgrade.MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.4"
    AFTER_TAG = "vn4.5"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn45_vn46(rose.upgrade.MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.5"
    AFTER_TAG = "vn4.6"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn46_vn47(rose.upgrade.MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.6"
    AFTER_TAG = "vn4.7"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn47_vn48(rose.upgrade.MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.7"
    AFTER_TAG = "vn4.8"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn48_vn49(rose.upgrade.MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.8"
    AFTER_TAG = "vn4.9"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn49_vn50(rose.upgrade.MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.9"
    AFTER_TAG = "vn5.0"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn50_t632(rose.upgrade.MacroUpgrade):
    """Upgrade macro for #632 by Paul Cresswell"""

    BEFORE_TAG = "vn5.0"
    AFTER_TAG = "vn5.0_t632"

    def upgrade(self, config, meta_config=None):
        # Add the new compulsory variables
        self.add_setting(config, ["env", "JULES_FFLAGS_EXTRA"], "")
        self.add_setting(config, ["env", "JULES_LDFLAGS_EXTRA"], "")

        return config, self.reports


class vn50_vn51(rose.upgrade.MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn5.0_t632"
    AFTER_TAG = "vn5.1"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports

class vn51_t735(rose.upgrade.MacroUpgrade):
    """Upgrade macro for #735 by Carolina Duran Rojas"""

    BEFORE_TAG = "vn5.1"
    AFTER_TAG = "vn5.1_t735"

    def upgrade(self, config, meta_config=None):
        platform = self.get_setting_value(config, ["env", "JULES_PLATFORM"])
        if platform == 'uoe':
            self.change_setting_value(config, ["env", "JULES_PLATFORM"],
                                      'uoe-linux-gfortran')
        return config, self.reports


class vn51_t755(rose.upgrade.MacroUpgrade):
    """Upgrade macro for #755 by Karina Williams"""

    BEFORE_TAG = "vn5.1_t735"
    AFTER_TAG = "vn5.1_t755"

    def upgrade(self, config, meta_config=None):
                              
        platform = self.get_setting_value(config, ["env", "JULES_PLATFORM"])
        
        if platform == 'meto-xc40-cce':
            self.change_setting_value(config, ["env", "JULES_REMOTE"], "remote")
            self.change_setting_value(config, ["env", "JULES_REMOTE_HOST"], "'xcfl00'")
                        
        return config, self.reports


class vn51_vn52(rose.upgrade.MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn5.1_t755"
    AFTER_TAG = "vn5.2"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
