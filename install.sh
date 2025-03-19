#!/bin/bash

set -e # exit on error

print_usage_info()
{
    cat <<'EOF'
Caffeine Installation Script

USAGE:
./install.sh [--help | [--prefix=PREFIX]

 --help             Display this help text
 --prefix=PREFIX    Install library into 'PREFIX' directory
 --prereqs          Display a list of prerequisite software.
                    Default prefix='\$HOME/.local/bin'

All unrecognized arguments will be passed to GASNet's configure.

Some influential environment variables:
  FC          Fortran compiler command
  FFLAGS      Fortran compiler flags
  CC          C compiler command
  CFLAGS      C compiler flags
  CPP         C preprocessor
  CPPFLAGS    C preprocessor flags, e.g. -I<include dir> if you have
              headers in a nonstandard directory <include dir>
  LDFLAGS     linker flags, e.g. -L<lib dir> if you have libraries in a
              nonstandard directory <lib dir>
  LIBS        libraries to pass to the linker, e.g. -l<library>
Use these variables to override the choices made by the installer or to help
it to find libraries and programs with nonstandard names/locations.

For a non-interactive build with the 'yes' utility installed, execute
yes | ./install.sh

Report bugs to fortran@lbl.gov or at https://go.lbl.gov/caffeine

EOF
}

GCC_VERSION=${GCC_VERSION:=14}
GASNET_VERSION="stable"

list_prerequisites()
{
    cat << EOF
Caffeine and this installer were developed with the following prerequisites.
If any are missing and if permission is granted, the installer will install
the latest versions using Homebrew:

  GCC $GCC_VERSION
  GASNet-EX $GASNET_VERSION
  fpm
  git (used by fpm to clone dependencies)
  curl
  pkg-config
  realpath (Homebrew coreutils)
  GNU Make (Homebrew coreutils)

EOF
}

# GASNET_CONFIGURE_ARGS is deliberately inherited from the caller environment
GASNET_CONFIGURE_ARGS=${GASNET_CONFIGURE_ARGS:=}

while [ "$1" != "" ]; do
    orig_arg="$1"
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
            # We pass the unmodified argument to GASNet configure
            # Quoting is believed sufficient for embedded whitespace but not quotes
            GASNET_CONFIGURE_ARGS+="${GASNET_CONFIGURE_ARGS+ }\"${orig_arg//\"/\\\"}\""
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

if ! command -v git > /dev/null 2>&1; then
  echo "git not found. Building Caffeine requires fpm, which uses git to download dependencies."
  echo "Please install git, ensure it is in your PATH, and rerun ./install.sh"
  exit 1
fi

if ! command -v curl > /dev/null 2>&1; then
  echo "curl not found. Please install curl, ensure it is in your PATH, and rerun ./install.sh"
  exit 1
fi

ask_permission_to_use_homebrew()
{
  cat << EOF

Either one or more of the environment variables FC, CC, and CXX are unset or
one or more of the following packages are not in the PATH: pkg-config, realpath, make, fpm.
If you grant permission to install prerequisites, you will be prompted before each installation.

Press 'Enter' to choose the square-bracketed default answer:
EOF
  printf "Is it ok to use Homebrew to install prerequisite packages? [yes] "
}

ask_permission_to_install_homebrew()
{
  cat << EOF

Homebrew not found. Installing Homebrew requires sudo privileges.
If you grant permission to install Homebrew, you may be prompted to enter your password.

Press 'Enter' to choose the square-bracketed default answer:
EOF
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

    curl -L https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh -o $DEPENDENCIES_DIR/install-homebrew.sh --create-dirs
    chmod u+x $DEPENDENCIES_DIR/install-homebrew.sh

    if [ -p /dev/stdin ] && [ $CI = false ]; then
	   cat << EOF

Pipe detected.  Installing Homebrew requires sudo privileges, which most likely will
not work if you are installing non-interactively, e.g., via 'yes | ./install.sh'.
To install Caffeine non-interactiely, please rerun the Caffeine installer after
executing the following command to install Homebrew:
"./$DEPENDENCIES_DIR/install-homebrew.sh"
EOF
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
    if [ uname == "Linux" ]; then
      brew link --force glibc
    fi
  fi
  CC=`which gcc-$GCC_VERSION`
  CXX=`which g++-$GCC_VERSION`
  FC=`which gfortran-$GCC_VERSION`

  if [ -z ${REALPATH+x} ] || [ -z ${MAKE+x} ] ; then
    ask_permission_to_install_homebrew_package "'realpath' and 'make'" "coreutils"
    exit_if_user_declines "realpath"
    "$BREW" install coreutils
  fi
  REALPATH=`which realpath`

  if [ -z ${PKG_CONFIG+x} ]; then
    ask_permission_to_install_homebrew_package "'pkg-config'"
    exit_if_user_declines "pkg-config"
    "$BREW" install pkg-config
  fi
  PKG_CONFIG=`which pkg-config`

  if [ -z ${FPM+x} ] ; then
    ask_permission_to_install_homebrew_package "'fpm'"
    exit_if_user_declines "fpm"
    "$BREW" tap fortran-lang/hombrew-fortran
    "$BREW" install fpm
  fi
  FPM=`which fpm`
fi

PREFIX=${PREFIX:-"${HOME}/.local"}
mkdir -p "$PREFIX"
PREFIX=`$REALPATH "$PREFIX"`
echo "PREFIX=$PREFIX"

if [ -z ${PKG_CONFIG_PATH+x} ]; then
  PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig"
else
  PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig:$PKG_CONFIG_PATH"
fi
echo "PKG_CONFIG_PATH=$PKG_CONFIG_PATH"

FPM_FC="$($REALPATH $(command -v $FC))"
if [[ $FPM_FC == *flang* ]]; then
  FPM_FC=${FPM_FC/flang-[1-9][0-9]*/flang-new}
fi
FPM_CC="$($REALPATH $(command -v $CC))"

ask_package_permission()
{
  cat << EOF

$1 not found in $2

Press 'Enter' for the square-bracketed default answer:
EOF
  printf "Is it ok to download and install $1? [yes] "
}

# TODO: Expand this to other GASNet conduits (issue #66)
GASNET_CONDUIT=smp
pkg="gasnet-$GASNET_CONDUIT-seq"
export PKG_CONFIG_PATH

if ! $PKG_CONFIG $pkg ; then
  ask_package_permission "GASNet-EX" "PKG_CONFIG_PATH"
  exit_if_user_declines "GASNet-EX"

  GASNET_TAR_FILE="GASNet-$GASNET_VERSION.tar.gz"
  GASNET_SOURCE_URL="https://bitbucket.org/berkeleylab/gasnet/downloads/GASNet-$GASNET_VERSION.tar.gz"
  if [ ! -d $DEPENDENCIES_DIR ]; then
    mkdir -pv $DEPENDENCIES_DIR
  fi
  GASNET_DIR=$DEPENDENCIES_DIR/GASNet-$GASNET_VERSION
  if [ -d $GASNET_DIR ]; then
    # clean any existing GASNet build dir we are overwriting
    rm -Rf $GASNET_DIR
  fi
  
  curl -L $GASNET_SOURCE_URL | tar xvzf - -C $DEPENDENCIES_DIR
  
  ( 
      cd $GASNET_DIR
      cmd="set -x ; ./configure --prefix=\"$PREFIX\""
      # user-provided overrides:
      cmd="$cmd $GASNET_CONFIGURE_ARGS"
      # pass-thru compiler settings:
      cmd="$cmd --with-cc=\"$CC\" --with-cxx=\"$CXX\""
      # select the GASNet config settings Caffeine requires, and disable unused features:
      cmd="$cmd --enable-$GASNET_CONDUIT"
      cmd="$cmd --enable-seq --disable-par --disable-parsync"
      cmd="$cmd --disable-segment-everything"
      # TEMPORARY: disable MPI compatibility until Caffeine supports distributed conduits
      cmd="$cmd --without-mpicc"
      eval $cmd
      $MAKE -j 8 all
      $MAKE -j 8 install
  )
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
	cat << EOF
***NOTICE***: The GASNet library built by Spack is ONLY intended for
unit-testing purposes, and is generally UNSUITABLE FOR PRODUCTION USE.
The RECOMMENDED way to build GASNet is as an embedded library as configured
by the higher-level client runtime package (i.e. Caffeine), including
system-specific configuration. Exiting install.sh
EOF
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
cat << EOF > $CAFFEINE_PC
CAFFEINE_FPM_LDFLAGS=$GASNET_LDFLAGS $GASNET_LIB_LOCATIONS
CAFFEINE_FPM_FC=$FPM_FC
CAFFEINE_FPM_CC=$GASNET_CC
CAFFEINE_FPM_CFLAGS=$GASNET_CFLAGS $GASNET_CPPFLAGS
Name: caffeine
Description: Coarray Fortran parallel runtime library
URL: https://gitlab.lbl.gov/berkeleylab/caffeine
Version: 0.4.1
EOF

exit_if_pkg_config_pc_file_missing "caffeine"

user_compiler_flags="${CPPFLAGS:-} ${FFLAGS:-}"

compiler_version=$($FPM_FC --version)
if [[ $compiler_version == *llvm* ]]; then
  compiler_flag="-mmlir -allow-assumed-rank -g -Ofast"
else
  compiler_flag="-g -O3 -ffree-line-length-0"
fi
compiler_flag+=" -DASSERT_MULTI_IMAGE -DASSERT_PARALLEL_CALLBACKS"

if ! [[ "$user_compiler_flags " =~ -[DU]ASSERTIONS[=\ ] ]] ; then 
  # default to enabling assertions, unless the command line sets a relevant flag
  compiler_flag+=" -DASSERTIONS"
fi

# Should come last to allow command-line overrides
compiler_flag+=" $user_compiler_flags"

RUN_FPM_SH="build/run-fpm.sh"
cat << EOF > $RUN_FPM_SH
#!/bin/sh
#-- DO NOT EDIT -- created by caffeine/install.sh
fpm="${FPM}"
fpm_sub_cmd=\$1; shift
if echo "--help -help --version -version --list -list new update list clean publish" | grep -w -q -e "\$fpm_sub_cmd" ; then
  set -x
  exec \$fpm "\$fpm_sub_cmd" "\$@"
elif echo "build test run install" | grep -w -q -e "\$fpm_sub_cmd" ; then
  set -x
  exec \$fpm "\$fpm_sub_cmd" \\
  --profile debug \\
  --flag "$compiler_flag" \\
  --compiler "`$PKG_CONFIG caffeine --variable=CAFFEINE_FPM_FC`"   \\
  --c-compiler "`$PKG_CONFIG caffeine --variable=CAFFEINE_FPM_CC`" \\
  --c-flag "`$PKG_CONFIG caffeine --variable=CAFFEINE_FPM_CFLAGS`" \\
  --link-flag "`$PKG_CONFIG caffeine --variable=CAFFEINE_FPM_LDFLAGS`" \\
  "\$@"
else
  echo "ERROR: Unrecognized fpm subcommand \$fpm_sub_cmd"
  \$fpm list
  exit 1
fi
EOF
chmod u+x $RUN_FPM_SH

./$RUN_FPM_SH build

cat << EOF

________________ Caffeine has been dispensed! ________________

To rebuild or to run tests or examples via the Fortran Package
Manager (fpm) with the required compiler/linker flags, pass a
fpm command to the build/run-fpm.sh script. For example, run
the program example/hello.f90 as follows:

./$RUN_FPM_SH run --example hello
EOF
