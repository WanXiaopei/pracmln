import os
import pip
from distutils.core import setup
import pracmln.version as _version

pip.main(["install", "appdirs"])

__version__ = _version.__version__


with open("requirements.txt", "r") as f:
    requirements = [line.strip() for line in f.readlines() if line.strip()]


def datafiles(d):
    data_files = []
    for root, dirs, files in os.walk(os.path.join(os.path.dirname(__file__), d)):
        if not files:
            continue
        root_ = root.replace(os.getcwd() + os.path.sep, "")
        data_files.append((root_, [os.path.join(root_, file) for file in files]))
    return data_files


def description():
    try:
        with open("README.md") as f:
            return f.read()
    except FileNotFoundError:
        return "Markov logic networks in Python. Please visit http://www.pracmln.org"


setup(
    name="pracmln",
    packages=[
        "pracmln",
        "pracmln._version",
        "pracmln.logic",
        "pracmln.mln",
        "pracmln.utils",
        "pracmln.wcsp",
        "pracmln.mln.grounding",
        "pracmln.mln.inference",
        "pracmln.mln.learning",
    ],
    package_dir={
        "pracmln": "./pracmln",
    },
    data_files=datafiles("examples") + datafiles("3rdparty") + datafiles("libpracmln") + datafiles("etc"),
    version=__version__,
    description="Markov logic networks in Python",
    long_description=description(),
    author="Daniel Nyga",
    author_email="nyga@cs.uni-bremen.de",
    url="http://www.pracmln.org",
    download_url="https://github.com/danielnyga/pracmln/archive/%s.tar.gz"
    % __version__,
    keywords=[
        "statistical relational learning",
        "mln",
        "Markov logic networks",
        "reasoning",
        "probcog",
    ],
    classifiers=[
        # How mature is this project? Common values are
        #   3 - Alpha
        #   4 - Beta
        #   5 - Production/Stable
        "Development Status :: 4 - Beta",
        # Indicate who your project is intended for
        "Intended Audience :: Developers",
        "Topic :: Software Development :: Libraries :: Python Modules",
        "Topic :: Scientific/Engineering :: Artificial Intelligence ",
        # Pick your license as you wish (should match "license" above)
        "License :: OSI Approved :: MIT License",
        # Specify the Python versions you support here. In particular, ensure
        # that you indicate whether you support Python 2, Python 3 or both.
        "Programming Language :: Python :: 2",
        "Programming Language :: Python :: 2.7",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.5",
    ],
    install_requires=requirements,
    entry_points={
        "console_scripts": [
            "mlnlearn=pracmln.mlnlearn:main",
            "mlnquery=pracmln.mlnquery:main",
            "libpracmln-build=pracmln.libpracmln:createcpplibs",
            "pracmlntest=pracmln.test:main",
        ],
    },
)
