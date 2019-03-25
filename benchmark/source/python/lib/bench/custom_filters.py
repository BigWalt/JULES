import unicodedata, re


#################################################################################
## This file contains custom Jinja2 filters
#################################################################################

def slugify(value):
    """
Converts to lowercase, removes non-word characters (alphanumerics and
underscores) and converts spaces to hyphens. Also strips leading and
trailing whitespace.
"""
    value = re.sub('[^\w\s-]', '', value).strip().lower()
    return re.sub('[-\s]+', '-', value)