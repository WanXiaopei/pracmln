"""
Version information for pracmln.
"""

__all__ = [
    "VERSION_MAJOR",
    "VERSION_MINOR",
    "VERSION_PATCH",
    "APPNAME",
    "__version__",
]

APPNAME = "pracmln"

VERSION_MAJOR = 1
VERSION_MINOR = 2
VERSION_PATCH = 3

VERSION_STRING_FULL = "%s.%s.%s" % (VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH)
VERSION_STRING_SHORT = "%s.%s" % (VERSION_MAJOR, VERSION_MINOR)

__version__ = VERSION_STRING_FULL
