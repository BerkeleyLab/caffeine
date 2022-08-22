#!/bin/sh

set -e # exit on error

print_usage_info()
{
    echo "Caffeine Installation Script"
    echo ""
    echo "USAGE:"
    echo "./install.sh [--help | [--prefix=PREFIX]"
    echo ""
    echo " --help             Display this help text"
    echo " --prefix=PREFIX    Install binary in 'PREFIX/bin'"
    echo " --prereqs          Display a list of prerequisite software."
    echo "                    Default prefix='\$HOME/.local/bin'"
    echo ""
    echo "For a non-interactive build with the 'yes' utility installed, execute"
    echo "yes | ./install.sh"
}

GCC_VERSION=12
GASNET_VERSION="2022.3.0"

list_prerequisites()
{
    echo "Caffeine and this installer were developed with the following prerequisite package" 
    echo "versions, which also indicate the versions that the installer will install if"
    echo "missing and if granted permission:"
    echo ""
    echo "  GCC $GCC_VERSION"
    echo "  GASNet-EX $GASNET_VERSION"
    echo "  fpm 0.5.0"
    echo "  pkg-config 0.29.2"
    echo "  realpath 9.0 (Homebrew coreutils 9.0)"
    echo "  GNU Make 3.1 (Homebrew coreutils 9.0)"
    echo ""
    echo "In some cases, earlier versions might also work."
}

while [ "$1" != "" ]; do
    PARAM=$(echo "$1" | awk -F= '{print $1}')
    VALUE=$(echo "$1" | awk -F= '{print $2}')
    case $PARAM in
        -h | --help)
            print_usage_info
            exit
            ;;
        --prereqs)
            list_prerequisites
            exit
            ;;
        --prefix)
            PREFIX=$VALUE
            ;;
        *)
            echo "ERROR: unknown parameter \"$PARAM\""
            usage
            exit 1
            ;;
    esac
    shift
done

set -u # error on use of undefined variable

if [ -z ${FC+x} ] || [ -z ${CC+x} ] || [ -z ${CXX+x} ]; then
  if command -v gfortran-$GCC_VERSION > /dev/null 2>&1; then
    FC=`which gfortran-$GCC_VERSION`
    echo "Setting FC=$FC"
  fi
  if command -v gcc-$GCC_VERSION > /dev/null 2>&1; then
    CC=`which gcc-$GCC_VERSION`
    echo "Setting CC=$CC"
  fi
  if command -v g++-$GCC_VERSION > /dev/null 2>&1; then
    CXX=`which g++-$GCC_VERSION`
    echo "Setting CXX=$CXX"
  fi
fi

if command -v pkg-config > /dev/null 2>&1; then
  PKG_CONFIG=`which pkg-config`
fi
  
if command -v realpath > /dev/null 2>&1; then
  REALPATH=`which realpath`
fi

if command -v make > /dev/null 2>&1; then
  MAKE=`which make`
fi

if command -v fpm > /dev/null 2>&1; then
  FPM=`which fpm`
fi

ask_permission_to_use_homebrew()
{
  echo ""
  echo "Either one or more of the environment variables FC, CC, and CXX are unset or"
  echo "one or more of the following packages are not in the PATH: pkg-config, realpath, make, fpm."
  echo "If you grant permission to install prerequisites, you will be prompted before each installation." 
  echo ""
  echo "Press 'Enter' to choose the square-bracketed default answer:"
  printf "Is it ok to use Homebrew to install prerequisite packages? [yes] "
}

ask_permission_to_install_homebrew()
{
  echo ""
  echo "Homebrew not found. Installing Homebrew requires sudo privileges."
  echo "If you grant permission to install Homebrew, you may be prompted to enter your password." 
  echo ""
  echo "Press 'Enter' to choose the square-bracketed default answer:"
  printf "Is it ok to download and install Homebrew? [yes] "
}

ask_permission_to_install_homebrew_package()
{
  echo ""
  if [ ! -z ${2+x} ]; then
    echo "Homebrew installs $1 collectively in one package named '$2'."
    echo ""
  fi
  printf "Is it ok to use Homebrew to install $1? [yes] "
}

CI=${CI:-"false"} # GitHub Actions workflows set CI=true

exit_if_user_declines()
{
  if [ $CI = true ]; then 
    echo " 'yes' assumed (GitHub Actions workflow detected)"
    return
  fi
  read answer
  if [ -n "$answer" -a "$answer" != "y" -a "$answer" != "Y" -a "$answer" != "Yes" -a "$answer" != "YES" -a "$answer" != "yes" ]; then
    echo "Installation declined."
    case ${1:-} in  
      *GASNet*) 
        echo "Please ensure the $pkg.pc file is in $PKG_CONFIG_PATH and then rerun './install.sh'." ;;
      *GCC*) 
        echo "To use compilers other than Homebrew-installed gcc-$GCC_VERSION, g++-$GCC_VERSION, and gfortran-$GCC_VERSION,"
        echo "please set the FC, CC, and CXX environment variables and rerun './install.sh'." ;;
      *) 
        echo "Please ensure that $1 is installed and in your PATH and then rerun './install.sh'." ;;
    esac
    echo "Caffeine was not installed." 
    exit 1
  fi
}

DEPENDENCIES_DIR="build/dependencies"
if [ ! -d $DEPENDENCIES_DIR ]; then
  mkdir -p $DEPENDENCIES_DIR
fi

if [ -z ${FC+x} ] || [ -z ${CC+x} ] || [ -z ${CXX+x} ] || [ -z ${PKG_CONFIG+x} ] || [ -z ${REALPATH+x} ] || [ -z ${MAKE+x} ] || [ -z ${FPM+x} ] ; then

  ask_permission_to_use_homebrew 
  exit_if_user_declines "brew"

  BREW="brew"

  if ! command -v $BREW > /dev/null 2>&1; then

    ask_permission_to_install_homebrew
    exit_if_user_declines "brew"

    if ! command -v curl > /dev/null 2>&1; then
      echo "No download mechanism found. Please install curl and rerun ./install.sh"
      exit 1
    fi

    curl -L https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh -o $DEPENDENCIES_DIR/install-homebrew.sh --create-dirs
    chmod u+x $DEPENDENCIES_DIR/install-homebrew.sh

    if [ -p /dev/stdin ]; then 
       echo ""
       echo "Pipe detected.  Installing Homebrew requires sudo privileges, which most likely will"
       echo "not work if you are installing non-interactively, e.g., via 'yes | ./install.sh'."
       echo "To install Caffeine non-interactiely, please rerun the Caffeine installer after" 
       echo "executing the following command to install Homebrew:"
       echo "\"./$DEPENDENCIES_DIR/install-homebrew.sh\""
       exit 1
    else
      ./$DEPENDENCIES_DIR/install-homebrew.sh
      rm $DEPENDENCIES_DIR/install-homebrew.sh
    fi

    if [ $(uname) = "Linux" ]; then
      BREW=/home/linuxbrew/.linuxbrew/bin/brew
      eval "$($BREW shellenv)"
    fi
  fi

  if [ -z ${FC+x} ] || [ -z ${CC+x} ] || [ -z ${CXX+x} ]; then
    ask_permission_to_install_homebrew_package "gfortran, gcc, and g++" "gcc@$GCC_VERSION" 
    exit_if_user_declines "GCC"
    "$BREW" install gcc@$GCC_VERSION
  fi
  CC=`which gcc-$GCC_VERSION`
  CXX=`which g++-$GCC_VERSION`
  FC=`which gfortran-$GCC_VERSION`


  if [ -z ${PKG_CONFIG+x} ]; then
    ask_permission_to_install_homebrew_package "'pkg-config'"
    exit_if_user_declines "pkg-config"
    "$BREW" install pkg-config
  fi
  PKG_CONFIG=`which pkg-config`

  if [ -z ${REALPATH+x} ] || [ -z ${MAKE+x} ] ; then
    ask_permission_to_install_homebrew_package "'realpath' and 'make'" "coreutils"
    exit_if_user_declines "realpath"
    "$BREW" install coreutils
  fi
  REALPATH=`which realpath`

  if [ -z ${FPM+x} ] ; then
    ask_permission_to_install_homebrew_package "'fpm'"
    exit_if_user_declines "fpm"
    "$BREW" tap awvwgk/fpm
    "$BREW" install fpm
  fi
  FPM=`which fpm`
fi

PREFIX=`$REALPATH ${PREFIX:-"${HOME}/.local"}`
echo "PREFIX=$PREFIX"

if [ -z ${PKG_CONFIG_PATH+x} ]; then
  PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig"
else
  PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig:$PKG_CONFIG_PATH"
fi
echo "PKG_CONFIG_PATH=$PKG_CONFIG_PATH"

FPM_FC="$($REALPATH $(command -v $FC))"
FPM_CC="$($REALPATH $(command -v $CC))"

ask_package_permission()
{
  echo ""
  echo "$1 not found in $2"
  echo ""
  echo "Press 'Enter' for the square-bracketed default answer:"
  printf "Is it ok to download and install $1? [yes] "
}

pkg="gasnet-smp-seq"
export PKG_CONFIG_PATH

if ! $PKG_CONFIG $pkg ; then
  ask_package_permission "GASNet-EX" "PKG_CONFIG_PATH"
  exit_if_user_declines "GASNet-EX"

  GASNET_TAR_FILE="GASNet-$GASNET_VERSION.tar.gz"
  GASNET_SOURCE_URL="https://gasnet.lbl.gov/EX/GASNet-$GASNET_VERSION.tar.gz"
  if [ ! -d $DEPENDENCIES_DIR ]; then
    mkdir -pv $DEPENDENCIES_DIR
  fi
  
  curl -L $GASNET_SOURCE_URL | tar xvzf - -C $DEPENDENCIES_DIR
  
  if [ -d $DEPENDENCIES_DIR/GASNet-$GASNET_VERSION ]; then
    cd $DEPENDENCIES_DIR/GASNet-$GASNET_VERSION
      FC="$FC" CC="$CC" CXX="$CXX" ./configure --prefix "$PREFIX"
      $MAKE -j 8 all
      $MAKE -j 8 install
    cd -
  fi
fi # if ! $PKG_CONFIG $pkg ; then

exit_if_pkg_config_pc_file_missing()
{
  if ! $PKG_CONFIG $1 ; then
    echo "$1.pc pkg-config file not found"
    exit 1
  fi
}

exit_if_pkg_config_pc_file_missing "$pkg"

GASNET_LDFLAGS="`$PKG_CONFIG $pkg --variable=GASNET_LDFLAGS`"
GASNET_LIBS="`$PKG_CONFIG $pkg --variable=GASNET_LIBS`"
GASNET_CC="`$PKG_CONFIG $pkg --variable=GASNET_CC`"
GASNET_CFLAGS="`$PKG_CONFIG $pkg --variable=GASNET_CFLAGS`"
GASNET_CPPFLAGS="`$PKG_CONFIG $pkg --variable=GASNET_CPPFLAGS`"

# Check whether GASNet was installed using Spack. If yes, bail out.
# Note: relies on the fact that most Spack installations have "opt/spack"
#       in the directory path, and assumes that the first directory returned
#       by pkg-config contains the GASNet lib directory
GASNET_LIBDIR="$(echo $GASNET_LIBS | awk '{print $1};')"
case "$GASNET_LIBDIR" in
  *spack* )
    echo "***NOTICE***: The GASNet library built by Spack is ONLY intended for"
    echo "unit-testing purposes, and is generally UNSUITABLE FOR PRODUCTION USE."
    echo "The RECOMMENDED way to build GASNet is as an embedded library as configured"
    echo "by the higher-level client runtime package (i.e. Caffeine), including"
    echo "system-specific configuration. Exiting install.sh"
    exit 1
    ;;
  * )
    ;; # Do nothing otherwise 
esac

# Strip compiler flags
# Warning: This assumes the full path doesn't contain any spaces!
GASNET_CC_STRIPPED="$(echo $GASNET_CC | awk '{print $1};')"
GASNET_CC_REAL="$($REALPATH $GASNET_CC_STRIPPED)"

if [ "$GASNET_CC_REAL" != "$FPM_CC" ]; then 
  echo "GASNET_CC=$GASNET_CC_REAL" and  "FPM_CC=$FPM_CC don't match"
  exit 1;
fi

echo "# DO NOT EDIT OR COMMIT -- Created by caffeine/install.sh" > build/fpm.toml
cp manifest/fpm.toml.template build/fpm.toml
GASNET_LIB_LOCATIONS=`echo $GASNET_LIBS | awk '{locs=""; for(i = 1; i <= NF; i++) if ($i ~ /^-L/) {locs=(locs " " $i);}; print locs; }'`
GASNET_LIB_NAMES=`echo $GASNET_LIBS | awk '{names=""; for(i = 1; i <= NF; i++) if ($i ~ /^-l/) {names=(names " " $i);}; print names; }' | sed 's/-l//g'`
FPM_TOML_LINK_ENTRY="link = [\"$(echo ${GASNET_LIB_NAMES} | sed 's/ /", "/g')\"]"
echo "${FPM_TOML_LINK_ENTRY}" >> build/fpm.toml
ln -f -s build/fpm.toml

CAFFEINE_PC="$PREFIX/lib/pkgconfig/caffeine.pc"
echo "CAFFEINE_FPM_LDFLAGS=$GASNET_LDFLAGS $GASNET_LIB_LOCATIONS" >  $CAFFEINE_PC
echo "CAFFEINE_FPM_FC=$FPM_FC"                                    >> $CAFFEINE_PC
echo "CAFFEINE_FPM_CC=$GASNET_CC"                                 >> $CAFFEINE_PC
echo "CAFFEINE_FPM_CFLAGS=$GASNET_CFLAGS $GASNET_CPPFLAGS"        >> $CAFFEINE_PC
echo "Name: caffeine"                                             >> $CAFFEINE_PC
echo "Description: Coarray Fortran parallel runtime library"      >> $CAFFEINE_PC
echo "URL: https://gitlab.lbl.gov/berkeleylab/caffeine"           >> $CAFFEINE_PC
echo "Version: 0.1.0"                                             >> $CAFFEINE_PC

exit_if_pkg_config_pc_file_missing "caffeine"

RUN_FPM_SH="build/run-fpm.sh"
echo "#!/bin/sh"                                                              >  $RUN_FPM_SH
echo "#-- DO NOT EDIT -- created by caffeine/install.sh"                      >> $RUN_FPM_SH
echo "\"${FPM}\" \"\$@\" \\"                                                  >> $RUN_FPM_SH
echo "--compiler \"`$PKG_CONFIG caffeine --variable=CAFFEINE_FPM_FC`\"   \\"  >> $RUN_FPM_SH
echo "--c-compiler \"`$PKG_CONFIG caffeine --variable=CAFFEINE_FPM_CC`\" \\"  >> $RUN_FPM_SH
echo "--c-flag \"`$PKG_CONFIG caffeine --variable=CAFFEINE_FPM_CFLAGS`\" \\"  >> $RUN_FPM_SH
echo "--link-flag \"`$PKG_CONFIG caffeine --variable=CAFFEINE_FPM_LDFLAGS`\"" >> $RUN_FPM_SH
chmod u+x $RUN_FPM_SH

./$RUN_FPM_SH build

echo ""
echo "________________ Caffeine has been dispensed! ________________"
echo ""
echo "To rebuild or to run tests or examples via the Fortran Package"
echo "Manager (fpm) with the required compiler/linker flags, pass a"
echo "fpm command to the build/run-fpm.sh script. For example, run"
echo "the program example/hello.f90 as follows:"
echo ""
echo "./$RUN_FPM_SH run --example hello"
