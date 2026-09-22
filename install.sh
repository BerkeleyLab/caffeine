#!/bin/bash

set -e # exit on error

print_usage_info()
{
    cat <<'EOF'
Caffeine Installation Script

Usage: ./install.sh [OPTION]...

Options:
 --help             Display this help text
 --prefix=<PREFIX>  Install libraries into <PREFIX> directory
                    Default prefix='\$HOME/.local/bin'
 --network=<NET>    Build Caffeine to target given GASNet network conduit. 
                    <NET> should be one of:
                      smp: single-node shared-memory conduit (default)
                      udp: portable UDP/IP (for Ethernet networks)
                      ibv: InfiniBand IB Verbs
                      ofi: OpenFabrics Interfaces
                      ucx: Unified Communication X
 --prereqs          Display a list of prerequisite software.
 --verbose          Show verbose build commands
 --yes              Assume "yes" to all prompts for non-interactive install
 --enable-debug     Build Caffeine and GASNet in LOW-PERFORMANCE debug mode,
                    disabling optimization and enabling assertions to help find defects.
 --enable-threads   Build a thread-safe Caffeine library and link to
                    thread-safe GASNet, for use in threaded do-concurrent.
 --enable-cmake or --disable-fpm 
                    Build Caffeine library using CMake instead of FPM (the default).

All unrecognized arguments will be passed to GASNet's configure.

Some influential environment variables:
  FC          Fortran compiler command
  FFLAGS      Fortran compiler flags
  CC          C compiler command
  CFLAGS      C compiler flags
  CPPFLAGS    C preprocessor flags, e.g. -I<include dir> if you have
              headers in a nonstandard directory <include dir>
  LDFLAGS     linker flags, e.g. -L<lib dir> if you have libraries in a
              nonstandard directory <lib dir>
  LIBS        libraries to pass to the linker, e.g. -l<library>
Use these variables to override the choices made by the installer or to help
it to find programs with nonstandard names/locations.

Report bugs to fortran@lbl.gov or at https://go.lbl.gov/caffeine

EOF
}

# ---------------------------------------------------------------
# Global variables

GASNET_VERSION="stable"
GASNET_SOURCE_URL="https://github.com/BerkeleyLab/gasnet/releases/download/gex-$GASNET_VERSION/GASNet-$GASNET_VERSION.tar.gz"
ASSERT_GIT=$(awk -F'"' '/^assert =/ {print $2}' manifest/fpm.toml.template)
ASSERT_VERSION=$(awk -F'"' '/^assert =/ {print $4}' manifest/fpm.toml.template)
JULIENNE_GIT=$(awk -F'"' '/^julienne =/ {print $2}' manifest/fpm.toml.template)
JULIENNE_VERSION=$(awk -F'"' '/^julienne =/ {print $4}' manifest/fpm.toml.template)
VERBOSE=
YES=false
USE_FPM=true
APPEND_CFLAGS="${CPPFLAGS:-} ${CFLAGS:-}"
APPEND_CFLAGS_lib=
APPEND_LDFLAGS=
# these variables deliberately inherited from the caller environment
GASNET_CONDUIT="${GASNET_CONDUIT:-smp}"
GASNET_THREADMODE="${GASNET_THREADMODE:-seq}"
GASNET_CODEMODE="${GASNET_CODEMODE:-opt}"
GASNET_CONFIGURE_ARGS=${GASNET_CONFIGURE_ARGS:-}
CI=${CI:-"false"} # GitHub Actions workflows set CI=true

# ---------------------------------------------------------------
# Global helper functions

list_prerequisites()
{
    cat << EOF
Caffeine's build system has the following system software prerequisites.
If any are missing and if permission is granted, the installer will install
the latest versions using Homebrew:

  LLVM flang (or another supported Fortran compiler)
  fpm
  pkg-config
  GNU Make
  git + curl (used to download library dependencies)

The installer will also download and build the following library dependencies,
which are installed along with the Caffeine library to the install prefix:

  GASNet-EX $GASNET_VERSION
    - $GASNET_SOURCE_URL
  Assert $ASSERT_VERSION 
    - $ASSERT_GIT
  Julienne $JULIENNE_VERSION (optional, only used for unit tests)
    - $JULIENNE_GIT

EOF
}


# expand to an absolute path for $1, possibly including symlinks
abspath() {
    if [ -z "$1" ]; then
        echo "ERROR: expected a non-empty pathname" >&2
        return 1
    fi

    if [[ "$1" == /* ]] ; then
      echo "$1"
    else
      echo "$PWD/$1"
    fi
}

# like `which` but always returns an absolute path or empty
# If $2 is set then failure is suppressed in the exit code
abswhich() {
    local cmd_path
    if [ -z "$1" ]; then
        echo "ERROR: expected a non-empty pathname" >&2
        return 1
    fi
    cmd_path=$(type -P -- "$1") || return $( [[ -n "${2:-}" ]] )

    echo "$(abspath $cmd_path)"
}

# expand to the absolute path of $1 with all symlinks and non-canonical elements removed
realpath() {
    set +x
    if [ -z "$1" ]; then
        echo "ERROR: expected a non-empty pathname" >&2
        return 1
    fi

    perl -e '
        use Cwd "abs_path";
        my $abs = abs_path($ARGV[0]);
        if (defined $abs) {
            print "$abs\n";
        } else {
            print "ERROR: $ARGV[0] does not exist";
            exit 1;
        }
    ' "$1"
}

append_gasnet_configure_arg() {
  if [[ -z "$GASNET_CONFIGURE_ARGS" ]] ; then
    GASNET_CONFIGURE_ARGS="\"$1\""
  else
    # Quoting is believed sufficient for embedded whitespace but not quotes
    GASNET_CONFIGURE_ARGS+=" \"${1//\"/\\\"}\""
  fi
}

# ---------------------------------------------------------------
# Command line parsing

while [ "$1" != "" ]; do
    orig_arg="$1"
    PARAM=$(awk -F= '{print $1}' <<< $1)
    VALUE=$(awk -F= '{print $2}' <<< $1)
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
        --network)
            GASNET_CONDUIT=$(tr '[:upper:]' '[:lower:]' <<< $VALUE)
            case $GASNET_CONDUIT in
              smp|udp|mpi|ibv|ofi|ucx) ;;
              *) 
                 echo "ERROR: Unrecognized --network=$GASNET_CONDUIT"
                 print_usage_info
                 exit 1
            esac
            ;;
        --verbose)
            VERBOSE="--verbose"
            set -x
            ;;
        -y | --yes)
            YES="true"
            ;;
        --enable-threads)  GASNET_THREADMODE=par ;;
        --disable-threads) GASNET_THREADMODE=seq ;;

        --enable-cmake | --disable-fpm) USE_FPM= ;;
        --disable-cmake | --enable-fpm) USE_FPM=true ;;

        --enable-debug)  GASNET_CODEMODE=debug ; append_gasnet_configure_arg "$orig_arg" ;;
        --disable-debug) GASNET_CODEMODE=opt ;   append_gasnet_configure_arg "$orig_arg" ;;

        *) # Pass unrecognized args unmodified to GASNet configure
            append_gasnet_configure_arg "$orig_arg"
            ;;
    esac
    shift
done

if [[ -n "$VERBOSE" ]] ; then
( set +x
  echo Command-line arguments:
  echo PREFIX=$PREFIX
  echo GASNET_CONDUIT=$GASNET_CONDUIT
  echo GASNET_CONFIGURE_ARGS=$GASNET_CONFIGURE_ARGS
  echo GASNET_THREADMODE=$GASNET_THREADMODE
  echo GASNET_CODEMODE=$GASNET_CODEMODE
)
fi

# ---------------------------------------------------------------
# Early check for pre-installed Homebrew

BREW="${BREW:-brew}"
if type -P "$BREW" > /dev/null 2>&1; then
  BREW_PREFIX=$($BREW --prefix || exit 0)
  if [ -z ${BREW_PREFIX:+x} ] || [ ! -d "$BREW_PREFIX" ] ; then
    echo Warning: Failed to detect Homebrew prefix
    BREW_PREFIX=
  fi
fi

# ---------------------------------------------------------------
# Initial compiler identification

if [ -n "${FC:+x}" ] && ! type -P "$FC" > /dev/null 2>&1; then
  echo "FC=$FC not found. If you don't yet have a Fortran compiler, please leave environment variable FC unset."
  exit 1
fi
if [ -n "${CC:+x}" ] && ! type -P "$CC" > /dev/null 2>&1; then
  echo "CC=$CC not found. If you don't yet have a C compiler, please leave environment variable CC unset."
  exit 1
fi
if [ -z ${FC:+x} ] ; then # FC unset: default to LLVM if it's in PATH
  if type -P flang > /dev/null 2>&1; then
    FC=$(abswhich flang)
    echo "Setting FC=$FC"
    if [ -n "$BREW_PREFIX" ] && [[ $FC =~ $BREW_PREFIX ]] ; then
      # We are using Homebrew flang, so prefer Homebrew clang/clang++
      export PATH="$BREW_PREFIX/opt/llvm/bin:$PATH"
    fi
  fi
  if type -P clang > /dev/null 2>&1; then
    CC=$(abswhich clang)
    echo "Setting CC=$CC"
  fi
fi
if [[ -n ${FC:+x} && -z ${CC:+x} ]] ; then # Have FC but missing CC
  # try to auto-detect CC from FC
  if [[ $(basename $FC) =~ flang ]] || [[ $(basename $FC) =~ lfortran ]] ; then 
    CC_guess=clang
  else
    CC_guess=gcc
  fi
  if ! [[ $(basename $FC) =~ lfortran ]] && [[ $FC =~ (-[0-9a-z-]+)$ ]] ; then 
    CC_guess_suff=$CC_guess${BASH_REMATCH[0]} 
    if type -P $CC_guess_suff > /dev/null 2>&1; then
      CC=$(abswhich $CC_guess_suff)
      echo "Setting CC=$CC"
    fi
  fi
  if [ -z ${CC:+x} ] && type -P $CC_guess > /dev/null 2>&1; then
    CC=$(abswhich $CC_guess)
    echo "Setting CC=$CC"
  fi
fi
if [[ -z ${CXX:+x} && -n ${CC:+x} ]] ; then 
  # C++ is an optional dependency
  # try to auto-detect from CC
  if [[ $(basename $CC) =~ clang ]] ; then 
    CXX_guess=clang++
  else
    CXX_guess=g++
  fi
  if [[ $CC =~ (-[0-9a-z-]+)$ ]] ; then 
    CXX_guess_suff=$CXX_guess${BASH_REMATCH[0]} 
    if type -P $CXX_guess_suff > /dev/null 2>&1; then
      CXX=$(abswhich $CXX_guess_suff)
      echo "Setting CXX=$CXX"
    fi
  fi
  if [ -z ${CXX:+x} ] && type -P $CXX_guess > /dev/null 2>&1; then
    CXX=$(abswhich $CXX_guess)
    echo "Setting CXX=$CXX"
  fi
fi

set -u # error on use of undefined variable

# ---------------------------------------------------------------
# Dependency identification

# allow overrides via envvar
PKG_CONFIG=$(abswhich ${PKG_CONFIG:-pkg-config} silent)
  
MAKE=$(abswhich ${MAKE:-gmake} silent) # prefer 'gmake' over 'make'
MAKE=$(abswhich ${MAKE:-make} silent)

CMAKE=$(abswhich ${CMAKE:-cmake} silent)

FPM=$(abswhich ${FPM:-fpm} silent)
if [[ -z $FPM && -z $USE_FPM ]] ; then
  FPM="fpm" # deliberately NOT path-expanded
fi

# FPM disallows override of the git command, so don't allow it here either
# Homebrew requires git and curl to operate, so cannot be used to provide them when they are missing
GIT=$(abswhich git silent)
if [[ -z "$GIT" ]] ; then
  echo "git not found. Building Caffeine requires git to download dependencies."
  echo "Please install git, ensure it is in your PATH, and rerun ./install.sh"
  exit 1
fi

# FPM disallows override of the curl command, so don't allow it here either
CURL=$(abswhich curl silent)
if [[ -z "$CURL" ]] ; then
  echo "curl not found. Please install curl, ensure it is in your PATH, and rerun ./install.sh"
  exit 1
fi

# ---------------------------------------------------------------
# Homebrew support

ask_permission_to_use_homebrew()
{
  BUILDER=$( [[ $USE_FPM ]] && echo "fpm" || echo "cmake" )
  cat << EOF

Either one or more of the environment variables FC and CC are unset or
one or more of the following packages are not in the PATH: pkg-config, make, $BUILDER.
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
  printf "Is it ok to use Homebrew to install $1? [yes] "
}

exit_if_user_declines()
{
  if [ $YES = true ]; then 
    echo " 'yes' assumed (--yes option)"
    return
  fi
  if [ $CI = true ]; then 
    echo " 'yes' assumed (GitHub Actions workflow detected)"
    return
  fi
  read answer
  if [ -n "$answer" -a "$answer" != "y" -a "$answer" != "Y" -a "$answer" != "Yes" -a "$answer" != "YES" -a "$answer" != "yes" ]; then
    echo "Installation declined."
    case ${1:-} in  
      *FC*) 
        echo "To use compilers other than Homebrew-installed LLVM flang and clang,"
        echo "please set the FC and CC environment variables and rerun './install.sh'." ;;
      *) 
        echo "Please ensure that $1 is installed and in your PATH and then rerun './install.sh'." ;;
    esac
    echo "Caffeine was not installed." 
    exit 1
  fi
}

DEPENDENCIES_DIR="build/dependencies"
mkdir -p $DEPENDENCIES_DIR

if [ -z ${FC:+x} ] || [ -z ${CC:+x} ] || [ -z ${PKG_CONFIG:+x} ] || [ -z ${MAKE:+x} ] || \
   [ -z ${FPM:+x} ] || [[ -z ${CMAKE:+x} && -z ${USE_FPM:+x} ]] ; then

  ask_permission_to_use_homebrew 
  exit_if_user_declines "brew"

  if ! type -P $BREW > /dev/null 2>&1; then

    ask_permission_to_install_homebrew
    exit_if_user_declines "brew"

    $CURL -L https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh -o $DEPENDENCIES_DIR/install-homebrew.sh --create-dirs
    chmod u+x $DEPENDENCIES_DIR/install-homebrew.sh

    if [ -p /dev/stdin ] && [ $CI = false ]; then
	   cat << EOF

ERROR: Pipe detected.  Installing Homebrew requires sudo privileges,
which is unlikely to work if you are installing non-interactively.
To install Caffeine non-interactively, please rerun the Caffeine installer after
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

  BREW_PREFIX=$($BREW --prefix || exit 0)
  if [ -z ${BREW_PREFIX:+x} ] || [ ! -d "$BREW_PREFIX" ] ; then
    echo Failed to detect Homebrew prefix
    echo 1
  fi

  # fetch the latest package definitions:
  $BREW update

  if [ -z ${FC:+x} ] || [ -z ${CC:+x} ] ; then
    ask_permission_to_install_homebrew_package "'llvm' and 'flang'"
    exit_if_user_declines "FC"
    $BREW install llvm flang

    # Homebrew does not inject clang/clang++ into PATH on macOS
    export PATH="$BREW_PREFIX/opt/llvm/bin:$PATH"
    CC="clang"
    CXX="clang++"
    FC="flang-new"
    for tool in CC CXX FC ; do
      if ! type -P ${!tool} > /dev/null 2>&1 ; then
        eval echo ERROR: Failed to detect Homebrew compiler install at ${!tool}
        exit 1
      else
        eval $tool=$(abswhich ${!tool})
      fi
    done
  fi

  if [ -z ${MAKE:+x} ] ; then
    ask_permission_to_install_homebrew_package "'make'"
    exit_if_user_declines "make"
    $BREW install make
    MAKE=$(abswhich gmake)
  fi

  if [ -z ${PKG_CONFIG:+x} ]; then
    ask_permission_to_install_homebrew_package "'pkg-config'"
    exit_if_user_declines "pkg-config"
    $BREW install pkg-config
    PKG_CONFIG=$(abswhich pkg-config)
  fi

  if [ -z ${FPM:+x} ] ; then
    ask_permission_to_install_homebrew_package "'fpm'"
    exit_if_user_declines "fpm"
    $BREW install fpm
    FPM=$(abswhich fpm)
  fi

  if [ -z ${CMAKE:+x} -a -z ${USE_FPM:+x} ] ; then
    ask_permission_to_install_homebrew_package "'cmake'"
    exit_if_user_declines "cmake"
    $BREW install cmake
    CMAKE=$(abswhich cmake)
  fi
fi

# ---------------------------------------------------------------
# Install location and compiler finalization

PREFIX=${PREFIX:-"${HOME}/.local"}
mkdir -p "$PREFIX"
PREFIX=$(abspath "$PREFIX")
echo "PREFIX=$PREFIX"

PKG_CONFIG_DIR="$PREFIX/lib/pkgconfig"
mkdir -p "$PKG_CONFIG_DIR"
if [ -z ${PKG_CONFIG_PATH:+x} ]; then
  PKG_CONFIG_PATH="$PKG_CONFIG_DIR"
else
  PKG_CONFIG_PATH="$PKG_CONFIG_DIR:$PKG_CONFIG_PATH"
fi
echo "PKG_CONFIG_PATH=$PKG_CONFIG_PATH"
export PKG_CONFIG_PATH

FC="$(abswhich $FC)"
if [[ $(basename $FC) == *flang* ]]; then
  # old versions of fpm rely on basename 'flang-new' to recognize LLVM flang,
  # so look for a corresponding symlink to the same compiler
  TRY_FC=${FC/%flang-[1-9][0-9]/flang-new}
  TRY_FC=${TRY_FC/%flang/flang-new}
  if [[ -x $TRY_FC ]] && [[ $(realpath $TRY_FC) == $(realpath $FC) ]] ; then
    FC="$(abswhich $TRY_FC)"
  fi
fi
CC="$(abswhich $CC)"
CXX="$(abswhich $CXX)"

if [ "${BREW_PREFIX:-unset}" != unset ] ; then
  # fixups necessitated by using Brew flang:
  if [[ $FC =~ flang ]] && [[ $FC =~ $BREW_PREFIX ]] ; then
    # workaround issue #228: clang cannot find Homebrew flang's C header
    APPEND_CFLAGS+=" -I$(dirname $(find "$BREW_PREFIX/Cellar/flang" -name ISO_Fortran_binding.h | head -1))"

    if [ $(uname) = "Linux" ]; then
      # workaround brew's libflang_rt.runtime.so missing from default linker path on Linux
      APPEND_LDFLAGS="-Wl,-rpath=$(dirname $(find "$BREW_PREFIX/Cellar/flang" -name libflang_rt.runtime.so | head -1))"
    fi
  fi
fi

# ---------------------------------------------------------------
# Fortran Flag computation

# save Fortran flag user inputs
user_compiler_flags="${CPPFLAGS:-} ${FFLAGS:-}"

# compiler-specific flag defaults
# FFLAGS is exported via pkg-config
# FFLAGS_lib adds flags for library build that should not be exported
FFLAGS=
FFLAGS_lib="-g"
FFLAGS_debug="-O0"
FFLAGS_opt="-O3"
compiler_version=$($FC --version)
supported_version=
if [[ $compiler_version =~ 'flang' ]]; then
  # use defaults
  supported_version=$(awk 'NR==1 && match($0, /version [0-9]+\.[0-9]+/){ v=substr($0, RSTART+8, RLENGTH-8); if (v+0 >= 19) print v; }' <<< "$compiler_version")
  # flang-19 and older need extra args:
  awk "BEGIN { exit ($supported_version < 20) }" || FFLAGS+=" -mmlir -allow-assumed-rank"
elif [[ $compiler_version =~ 'GNU Fortran' ]]; then
  FFLAGS="-ffree-line-length-0 -Wno-unused-dummy-argument"
  supported_version=$(awk 'NR==1 && match($0, /) [0-9]+\.[0-9]+/){ v=substr($0, RSTART+2, RLENGTH-2); if (v+0 >= 13) print v; }' <<< "$compiler_version")
elif [[ $compiler_version =~ 'LFortran' ]]; then
  # LFortran -g deliberately omitted: not always available, and leads to bizarre errors when it's not
  FFLAGS_lib="--cpp --realloc-lhs-arrays --no-style-suggestions --implicit-argument-casting"
  FFLAGS="--separate-compilation"
  supported_version=$(awk 'NR==1 && match($0, /version: [0-9]+\.[0-9]+/){ v=substr($0, RSTART+9, RLENGTH-9); if (v+0 >= 0.63) print v; }' <<< "$compiler_version")
else # unknown compiler
  FFLAGS_opt=-O2
fi
if [[ -z "$supported_version" ]] ; then
  echo "WARNING: Failed to detect a recognized Fortran compiler."
  echo 
  echo "$FC --version reported the following:"
  echo "$compiler_version"
  echo 
  echo "This does not appear to be one of the compiler+version combinations"
  echo "officially supported by Caffeine (see README.md) and might not work."
  printf "Are you certain you wish to continue installation with $FC? [yes] "
  exit_if_user_declines "FC"
fi

if [[ "$GASNET_CODEMODE" == "debug" ]] ; then 
  FFLAGS_lib="$FFLAGS_debug $FFLAGS_lib"
else
  FFLAGS_lib="$FFLAGS_opt $FFLAGS_lib"
fi

# Configure dependencies:
# We utilize Assert's parallel callbacks feature, with PRIF callbacks provided by libcaffeine.
# This feature previously required ASSERT_PARALLEL_CALLBACKS, but is now always enabled.
# We leave Assert's multi-image support disabled (default), because this is
# subsumed by the parallel callbacks, and we don't want native calls to
# this_image() on compilers that might not support it through PRIF.
# We do rename the assert module to reduce the chance of name conflicts:
FFLAGS_lib+=" -Dassert_m=caf_caffiene_assert_m"
# enable Julienne's multi-image support with PRIF callbacks provided by julienne-driver
FFLAGS_lib+=" -DHAVE_MULTI_IMAGE_SUPPORT -DJULIENNE_PARALLEL_CALLBACKS"

if [[ $GASNET_THREADMODE == "par" ]] ; then
  FFLAGS+=" -DCAF_THREAD_SAFE"
fi

GASNET_CONDUIT_UPPER=$(tr '[:lower:]' '[:upper:]' <<<$GASNET_CONDUIT)
FFLAGS+=" -DCAF_NETWORK_$GASNET_CONDUIT_UPPER"

# Append user flags last to allow command-line overrides
FFLAGS+=" $user_compiler_flags"

if ! [[ "$FFLAGS_lib $FFLAGS " =~ -[DU]ASSERTIONS[=\ ] ]] ; then 
  # assertions not explicitly enabled or disabled on the command-line
  # default assertions based on codemode (--enable-debug)
  if [[ "$GASNET_CODEMODE" == "debug" ]] ; then 
    FFLAGS_lib+=" -DASSERTIONS"
  fi
fi

# Ensure that certain preprocessor settings in FFLAGS are always appended to CFLAGS
for opt in $FFLAGS_lib $FFLAGS; do
  case "$opt" in
    -DASSERTIONS* | -UASSERTIONS* | -DFORCE_PRIF_* | -UFORCE_PRIF_*)
       APPEND_CFLAGS_lib+=" $opt"
       ;;
  esac
done

# ---------------------------------------------------------------
# GASNet identification/build

pkg="gasnet-$GASNET_CONDUIT-$GASNET_THREADMODE"

if ! $PKG_CONFIG $pkg ; then
  GASNET_TAR_FILE="$DEPENDENCIES_DIR/GASNet-$GASNET_VERSION.tar.gz"
  GASNET_DIR=$DEPENDENCIES_DIR/GASNet-$GASNET_VERSION
  if [ -d $GASNET_DIR ]; then
    # clean any existing GASNet build dir we are overwriting
    rm -Rf $GASNET_DIR
  fi
  
  $CURL -L $VERBOSE --retry 10 --retry-all-errors --fail $GASNET_SOURCE_URL -o $GASNET_TAR_FILE
  tar xvzf $GASNET_TAR_FILE -C $DEPENDENCIES_DIR
  
  ( 
      cd $GASNET_DIR
      cmd="set -x ; ./configure --prefix=\"$PREFIX\""
      # user-provided overrides:
      cmd="$cmd $GASNET_CONFIGURE_ARGS"
      # pass-thru compiler settings:
      cmd="$cmd --with-cc=\"$CC\" --with-cxx=\"$CXX\""
      # select the GASNet config settings Caffeine requires, and disable unused features:
      cmd="$cmd --enable-$GASNET_CONDUIT"
      cmd="$cmd --enable-seq --enable-par --disable-parsync"
      cmd="$cmd --disable-segment-everything"
      # TEMPORARY: disable MPI compatibility until we figure out how to support in fpm
      cmd="$cmd --disable-mpi-compat"
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

GASNET_LDFLAGS=$($PKG_CONFIG $pkg --variable=GASNET_LDFLAGS)
GASNET_LIBS=$($PKG_CONFIG $pkg --variable=GASNET_LIBS)
GASNET_CC=$($PKG_CONFIG $pkg --variable=GASNET_CC)
GASNET_CFLAGS=$($PKG_CONFIG $pkg --variable=GASNET_CFLAGS)
GASNET_CPPFLAGS=$($PKG_CONFIG $pkg --variable=GASNET_CPPFLAGS)

# Relies on the first directory in GASNET_LIBS is the GASNet lib directory
GASNET_LIBDIR=$(awk '{print $1};' <<< $GASNET_LIBS)
GASNET_LIBDIR=${GASNET_LIBDIR#-L}

# Check whether GASNet appears to be a Spack install. If yes, bail out.
# Note: most Spack installations have "opt/spack" in the directory path.
if [[ $GASNET_LIBDIR == *spack* ]] && \
   [[ $(realpath $GASNET_LIBDIR) != $(realpath "$PREFIX/lib") ]]; then
  cat << EOF
***NOTICE***: The GASNet library built by Spack is ONLY intended for
unit-testing purposes, and is generally UNSUITABLE FOR PRODUCTION USE.
The RECOMMENDED way to build GASNet is as an embedded library as configured
by the higher-level client runtime package (i.e. Caffeine), including
system-specific configuration. Exiting install.sh
EOF
  exit 1
fi
GASNET_PREFIX=$(dirname $GASNET_LIBDIR)
if [ ! -r "$GASNET_PREFIX/include/gasnetex.h" ] ; then
  echo "ERROR: Failed to detect GASNet install prefix from $GASNET_LIBS"
  exit 1
fi

# Strip compiler flags
# Warning: This assumes the full path doesn't contain any spaces!
GASNET_CC_STRIPPED=$(awk '{print $1};' <<< $GASNET_CC)
if [ "$(realpath $GASNET_CC_STRIPPED)" != "$(realpath $CC)" ]; then 
  echo "ERROR: C Compiler mismatch: GASNET_CC=$(realpath $GASNET_CC_STRIPPED) and CC=$(realpath $CC) don't match"
  exit 1;
fi

if [[ $compiler_version =~ 'LFortran' ]]; then
  # Ensure we use LFortran's copy of ISO_Fortran_binding.h
  APPEND_CFLAGS+=-I$(lfortran --print-c-include-dir)
  # Some LFortan builds issue a fatal error if -g appears on the Fortran compile or link line
  # GASNet sometimes injects this linker option, so ensure we strip it out
  for var in GASNET_LDFLAGS GASNET_LIBS ; do
    space=' ' 
    eval $var="\$space\${$var}\$space"    # surround start/end with space to avoid anchors
    eval $var="\${$var// -g / }" # space is our option boundary
    eval $var="\${$var%% }" # strip the space we added
    eval $var="\${$var## }" # strip the space we added
  done
fi

# ---------------------------------------------------------------
# Output file generation

FPM_TOML="fpm.toml"
rm -f $FPM_TOML
echo "# DO NOT EDIT OR COMMIT -- Created by caffeine/install.sh" > $FPM_TOML
cat manifest/fpm.toml.template >> $FPM_TOML
GASNET_LIB_LOCATIONS=$(awk '{locs=""; for(i = 1; i <= NF; i++) if ($i ~ /^-L/) {locs=(locs " " $i);}; print locs; }' <<< $GASNET_LIBS)
GASNET_LIB_NAMES=$(awk '{names=""; for(i=1; i<=NF; i++) if(sub(/^-l/, "", $i)) names=(names ? names " " : "") $i; print names}' <<< $GASNET_LIBS)
if [[ $GASNET_CONDUIT == "udp" ]] ; then
  GASNET_LIB_NAMES+=" stdc++" # udp-conduit requires C++ libraries
  APPEND_LDFLAGS+=" -lstdc++"
fi
FPM_TOML_LINK_ENTRY="link = [\"$(sed 's/ /", "/g' <<< $GASNET_LIB_NAMES)\"]"
echo "${FPM_TOML_LINK_ENTRY}" >> $FPM_TOML

# flag outputs
CAFFEINE_CFLAGS="$GASNET_CFLAGS $GASNET_CPPFLAGS $APPEND_CFLAGS_lib $APPEND_CFLAGS"
CAFFEINE_LDFLAGS="$GASNET_LDFLAGS $GASNET_LIB_LOCATIONS $APPEND_LDFLAGS"

case $GASNET_CONDUIT in
  ibv|ofi|ucx) 
    GASNET_RUNNER_ARG="${GASNET_RUNNER_ARG:-$GASNET_PREFIX/bin/gasnetrun_$GASNET_CONDUIT -n \${CAF_IMAGES:-2}}"
  ;;
  udp)
    GASNET_RUNNER_ARG="${GASNET_RUNNER_ARG:-$GASNET_PREFIX/bin/amudprun -n \${CAF_IMAGES:-2}}"
  ;;
  mpi)
    GASNET_RUNNER_ARG="${GASNET_RUNNER_ARG:-mpirun -n \${CAF_IMAGES:-2}}"
  ;;
  smp)
    GASNET_RUNNER_ARG="${GASNET_RUNNER_ARG:-env GASNET_PSHM_NODES=\${CAF_IMAGES:-\${GASNET_PSHM_NODES:-2}}}"
  ;;
  *)
    GASNET_RUNNER_ARG="${GASNET_RUNNER_ARG:-}"
  ;;
esac

CAFFEINE_PC="caffeine-$GASNET_CONDUIT-$GASNET_THREADMODE.pc"
cat << EOF > "$PKG_CONFIG_DIR/$CAFFEINE_PC"
# WARNING: This file is automatically generated - do NOT edit directly
# Copyright 2026, The Regents of the University of California
# Terms of use are as specified in license.txt

CAFFEINE_FC=$FC
CAFFEINE_CC=$CC
CAFFEINE_FFLAGS="$FFLAGS"
CAFFEINE_CFLAGS="$APPEND_CFLAGS"
CAFFEINE_LDFLAGS="-L$PREFIX/lib"
CAFFEINE_NETWORK=$GASNET_CONDUIT
CAFFEINE_THREADMODE=$GASNET_THREADMODE
CAFFEINE_CODEMODE=$GASNET_CODEMODE
CAFFEINE_RUNCMD="${GASNET_RUNNER_ARG//'${CAF_IMAGES'*'}'/\$CAF_IMAGES}"

Name: caffeine
Description: The CoArray Fortran Framework of Efficient Interfaces to Network Environments (Caffeine) implements the Parallel Runtime Interface for Fortran (PRIF), providing runtime support for multi-image features in modern Fortran compilers.
URL: https://go.lbl.gov/caffeine
Version: 0.8.1
Requires: gasnet-$GASNET_CONDUIT-$GASNET_THREADMODE
Cflags: \${CAFFEINE_CFLAGS}
Libs: \${CAFFEINE_LDFLAGS} -lcaffeine-$GASNET_CONDUIT-$GASNET_THREADMODE $APPEND_LDFLAGS
EOF
ln -sf "$CAFFEINE_PC" "$PKG_CONFIG_DIR/caffeine-$GASNET_CONDUIT.pc"
ln -sf "$CAFFEINE_PC" "$PKG_CONFIG_DIR/caffeine.pc"

exit_if_pkg_config_pc_file_missing "caffeine"

RUN_FPM_SH="run-fpm.sh"
cat << EOF > $RUN_FPM_SH
#!/bin/bash
#-- DO NOT EDIT -- created by caffeine/install.sh
FPM="$FPM"
FC="$FC"
CC="$CC"
NATIVEFLAGS=""
RAWFLAGS="$FFLAGS_lib $FFLAGS"
FFLAGS="\$NATIVEFLAGS \$RAWFLAGS"
CFLAGS="$CAFFEINE_CFLAGS"
LDFLAGS="$CAFFEINE_LDFLAGS"
FPM_DRIVER=\${FPM_DRIVER:-\$([[ "\$0" == /* ]] && echo "\$0" || echo "\$PWD/\$0")}
export FPM_DRIVER
fpm_sub_cmd=\$1; shift
if [[ "\$fpm_sub_cmd" == "install" && "\$1" != "--list" ]] ; then
  echo "ERROR: Please use install.sh to install Caffeine."
  exit 1
fi
case "\$fpm_sub_cmd" in
--help|-help|help|--version|-version|--list|-list|new|update|list|clean|publish)
  set -x
  exec "\$FPM" "\$fpm_sub_cmd" "\$@"
  ;;
build|test|run|install)
  sed -i.bak 's/^link = .*\$/$FPM_TOML_LINK_ENTRY/' $FPM_TOML
  rm -f $FPM_TOML.bak # issue 282: this is the only portable way to use sed -i
  if [[ -n "$GASNET_RUNNER_ARG" && " test run " == *" \$fpm_sub_cmd "* ]]; then
    set -- "--runner=$GASNET_RUNNER_ARG" "\$@"
  fi
  set -x
  exec "\$FPM" "\$fpm_sub_cmd" \\
  --profile debug \\
  --compiler "\$FC" \\
  --flag "\$FFLAGS" \\
  --c-compiler "\$CC" \\
  --c-flag "\$CFLAGS" \\
  --link-flag "\$LDFLAGS" \\
  "\$@"
  ;;
set-native)
  set -e
  mkdir -p build
  cmd="\$FC \$RAWFLAGS app/print-native-flags.F90 -o build/print-native-flags $APPEND_LDFLAGS"
  eval \$cmd || (set -x ; eval \$cmd)
  NATIVEFLAGS=\$(build/print-native-flags)
  rm -f build/print-native-flags
  sed -i.bak 's/^NATIVEFLAGS=.*\$/NATIVEFLAGS="'"\$NATIVEFLAGS"'"/' \$FPM_DRIVER
  rm -f \$FPM_DRIVER.bak
  echo NATIVEFLAGS=\"\$NATIVEFLAGS\"
  ;;
info)
  LINE=--------------------------------------------------
  SRCDIR=\$(dirname \$FPM_DRIVER)
  GASNETDIR="$GASNET_PREFIX"
  GASNETCONFIG="\$GASNETDIR/include/gasnet_config.h"
  MAKE=$MAKE
  CMAKE=${CMAKE:-}
  echo \$LINE
  echo Version info:
  echo Caffeine \$(grep version \$SRCDIR/fpm.toml)
  if [[ -d \$SRCDIR/.git ]]; then
    GITVER=\$( ( cd \$SRCDIR && git describe --long --dirty --always ) 2> /dev/null)
    [[ -n "\$GITVER" ]] && echo "  git describe: \$GITVER"
  fi
  if [[ -r "\$GASNETCONFIG" ]]; then
    echo GASNet version \$(grep GASNETI_RELEASE_VERSION \$GASNETCONFIG | cut -d' ' -f3-)
  fi
  grep -e assert -e julienne \$SRCDIR/fpm.toml
  echo \$LINE
  echo Platform info:
  uname -a
  [[ -r /etc/os-release ]] && grep -e NAME -e VERSION /etc/os-release
  [[ -x /usr/bin/sw_vers ]] && /usr/bin/sw_vers
  echo \$LINE
  echo Install settings:
  echo ID="\$(date) \$(whoami)"
  echo PREFIX=$PREFIX
  echo FPM=\$FPM
  echo CMAKE=\$CMAKE
  echo MAKE=\$MAKE
  echo FC=\$FC
  echo CC=\$CC
  echo FFLAGS=\$FFLAGS
  echo CFLAGS=\$CFLAGS
  echo LDFLAGS=\$LDFLAGS
  grep -e link \$SRCDIR/fpm.toml
  echo GASNET=\$GASNETDIR
  echo GASNET_CONDUIT=$GASNET_CONDUIT
  echo GASNET_CODEMODE=$GASNET_CODEMODE
  echo GASNET_THREADMODE=$GASNET_THREADMODE
  if [[ -r "\$GASNETCONFIG" ]]; then
    grep -e GASNETI_BUILD_ID -e GASNETI_CONFIGURE_ARGS \$GASNETCONFIG | cut -d' ' -f2-
  fi
  for tool in FC CC $( [[ $USE_FPM ]] && echo "FPM" || echo "CMAKE" ) MAKE ; do
    echo \$LINE
    eval toolval="\\$\$tool"
    echo \$tool : \$toolval
    # strip off any arguments that might be embedded:
    toolval=\$(echo \$toolval | cut -d' ' -f1)
    eval /bin/ls -al \$toolval
    eval /bin/ls -alhL \$toolval
    \$toolval --version
  done
  echo \$LINE
  ;;
*)
  echo "ERROR: Unrecognized fpm subcommand \$fpm_sub_cmd"
  \$FPM list
  exit 1
esac
EOF
chmod u+x $RUN_FPM_SH
# for backwards-compatibility of instructions/scripting:
( cd build && ln -f -s ../$RUN_FPM_SH run-fpm.sh )

# ---------------------------------------------------------------
# Install an ERR handler for build failures
error_handler() {
  set +ex
  echo "Error: Command '$BASH_COMMAND' failed on line $1 with exit code $?."
  echo "Defect reporting information:"
  ./$RUN_FPM_SH info
  echo
  echo Oh no, the Caffeine build appears to have failed!
  echo Please paste the ENTIRE output above into a new issue here:
  echo "   https://github.com/berkeleylab/caffeine/issues"
  exit 1
}

trap 'error_handler $LINENO' ERR

# ---------------------------------------------------------------
# Caffeine build

LIBCAFFEINE_DST=libcaffeine-$GASNET_CONDUIT-$GASNET_THREADMODE.a

./$RUN_FPM_SH set-native

if [[ -n $USE_FPM ]] ; then
  ./$RUN_FPM_SH build $VERBOSE

  LIBCAFFEINE_SRC=$(./$RUN_FPM_SH install --list 2>/dev/null | grep libcaffeine | cut -d' ' -f2)
else # Using CMake instead of FPM to build
  ASSERT_DIR=$DEPENDENCIES_DIR/assert
  mkdir -p $ASSERT_DIR
  if ! [[ -r $ASSERT_DIR/fpm.toml ]] ; then
    # Download Assert: Assumes git version 1.7.7 (2011-09) or later
    $GIT clone -c advice.detachedHead=false --depth 1 --branch $ASSERT_VERSION $ASSERT_GIT $ASSERT_DIR
    ( cd $ASSERT_DIR && $GIT log -n 1 --oneline )
  fi

  # CMake botches module name analysis unless we match the name in the source file:
  ASSERT_SRC="$ASSERT_DIR/src/caf_caffiene_assert_m.F90"
  $FC $FFLAGS_lib $FFLAGS -I$ASSERT_DIR/include -E $ASSERT_DIR/src/assert_m.F90 > $ASSERT_SRC

  # Generate CMakeLists.txt
  cat << EOF > CMakeLists.txt
cmake_minimum_required(VERSION 3.0...4.4 FATAL_ERROR)

# Provide the compilers BEFORE the project() command
set(CMAKE_C_COMPILER "$CC")
set(CMAKE_Fortran_COMPILER "$FC")

project(Caffeine LANGUAGES C Fortran)

# Set the command-line options for the compilers
set(CMAKE_C_FLAGS "$CAFFEINE_CFLAGS -I$(abspath include)")
set(CMAKE_Fortran_FLAGS "$FFLAGS_lib $FFLAGS -I$(abspath include) -I$(abspath $ASSERT_DIR)/include")

add_library(caffeine-$GASNET_CONDUIT-$GASNET_THREADMODE STATIC
EOF
  echo $ASSERT_SRC >> CMakeLists.txt
  # Ownership check to avoid "fatal: detected dubious ownership in repository" in containers
  if [[ -d .git ]] && [[ $(ls -ld .git | awk '{print $3}') == $(id -un) ]] ; then
    $GIT ls-files src | grep -e '.F90$' -e '.c$' >> CMakeLists.txt
  else
    find src -name '*.F90' -or -name '*.c' >> CMakeLists.txt
  fi
  echo ")" >> CMakeLists.txt

  rm -Rf build/cmake
  mkdir -p build/cmake
  (
    cd build/cmake
    $CMAKE ../..
    $MAKE -j 8 ${VERBOSE:+VERBOSE=1}
  )

  LIBCAFFEINE_SRC=build/cmake/$LIBCAFFEINE_DST
fi

# ---------------------------------------------------------------
# Caffeine installation


if ! [ -r "$LIBCAFFEINE_SRC" ]; then
  echo "ERROR: Failed to build/detect libcaffeine"
  exit 1
else
  mkdir -p "$PREFIX/lib"
  cp -af "$LIBCAFFEINE_SRC" "$PREFIX/lib/$LIBCAFFEINE_DST"
  ln -sf "$LIBCAFFEINE_DST" "$PREFIX/lib/libcaffeine-$GASNET_CONDUIT.a"
  ln -sf "$LIBCAFFEINE_DST" "$PREFIX/lib/libcaffeine.a"
fi

mkdir -p "$PREFIX/share/caffeine"
./$RUN_FPM_SH info > "$PREFIX/share/caffeine/caffeine-info-$GASNET_CONDUIT-$GASNET_THREADMODE.txt"

cat << EOF

________________ Caffeine has been dispensed! ________________

Caffeine is now installed in $PREFIX

To rebuild or to run tests or examples via the Fortran Package
Manager (FPM) with the required compiler/linker flags, pass a
fpm command to the run-fpm.sh script. For example, run
the program example/hello.f90 as follows:

  ./$RUN_FPM_SH run --example hello

EOF
if grep '^NATIVEFLAGS=' $RUN_FPM_SH | grep -q DHAVE_MULTI_IMAGE ; then
cat << EOF
To run the more comprehensive test program app/native-multi-image.F90, try:

  ./$RUN_FPM_SH run

or alternatively (without FPM):

  make -C app prif

EOF
fi

