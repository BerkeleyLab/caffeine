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
}

GCC_VER=11
FPM_VERSION="0.5.0"
GASNET_VERSION="2021.9.0"

list_prerequisites()
{
    echo "Caffeine and this installer were developed with the following prerequisite package" 
    echo "versions, which also indicate the versions that the installer will install if"
    echo "missing and if granted permission:"
    echo ""
    echo "  GCC $GCC_VER"
    echo "  GASNet-EX $GASNET_VERSION"
    echo "  fpm $FPM_VERSION"
    echo "  pkg-config 0.29.2"
    echo "  realpath 9.0 (Homebrew coreutils 9.0)"
    echo "  GNU Make 3.1"
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

if [ -z ${PREFIX+x} ] ; then
  PREFIX="$HOME/.local"
  echo ""
  echo "Using default installation prefix: $PREFIX"
fi

if [ -z ${FC+x} ] || [ -z ${CC+x} ] || [ -z ${CXX+x} ]; then
  if command -v gfortran-$GCC_VER > /dev/null 2>&1; then
    FC=gfortran-$GCC_VER
  fi
  if command -v gcc-$GCC_VER > /dev/null 2>&1; then
    CC=gcc-$GCC_VER
  fi
  if command -v g++-$GCC_VER > /dev/null 2>&1; then
    CXX=g++-$GCC_VER
  fi
fi

if command -v pkg-config > /dev/null 2>&1; then
  PKG_CONFIG=pkg-config
fi
  
if command -v realpath > /dev/null 2>&1; then
  REALPATH=realpath
fi

if command -v make > /dev/null 2>&1; then
  MAKE=make
fi

ask_homebrew_permission()
{
  echo ""
  echo "One or more of the following packages are missing: " 
  echo "gfortran $GCC_VER, gcc $GCC_VER, g++ $GCC_VER, pkg-config, realpath, make"
  echo "If you grant permission to install prerequisites, you will be prompted before each installation." 
  echo "Press 'Enter' to choose the square-bracketed default answers: "
  echo ""
  printf "Is it ok to install prerequisites using Homebrew? [yes] "
}

ask_homebrew_package_permission()
{
  echo ""
  case $1 in  
    *gcc*) echo "To use pre-installed compilers, set the FC, CC, and CXX environment variables and rerun './install.sh'." ;;
  esac
  if [ ! -z ${2+x} ]; then
    echo "Homebrew installs $1 collectively in one package ($2):"
  fi
  echo ""
  printf "Is it ok to install $1 using Homebrew? [yes] "
}

exit_if_user_declines()
{
  read answer
  if [ -n "$answer" -a "$answer" != "y" -a "$answer" != "Y" -a "$answer" != "Yes" -a "$answer" != "YES" -a "$answer" != "yes" ]; then
    echo "Installation declined."
    echo "Please ensure that the listed prerequisites are installed and in your PATH and then rerun './install.sh'."
    if [ $1 = "*gcc*" ]; then
      echo "To use compilers other than gcc-$GCC_VER, g++-$GCC_VER, and gfortran-$GCC_VER," 
      echo "please also set the FC, CC, and CXX environment variables."
    fi
    echo "Caffeine was not installed." 
    exit 1
  fi
}

if [ -z ${FC+x} ] || [ -z ${CC+x} ] || [ -z ${CXX+x} ] || [ -z ${PKG_CONFIG+x} ] || [ -z ${REALPATH+x} ] || [ -z ${MAKE+x} ] ; then

  ask_homebrew_permission 
  exit_if_user_declines

  if command -v brew > /dev/null; then
    BREW_COMMAND="brew"
  else
    if ! command -v curl > /dev/null 2>&1; then
      echo "No download mechanism found. Please install curl and rerun ./install.sh"
      exit 1
    fi
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    if [ $(uname) == "Linux" ]; then
      BREW_COMMAND=/home/linuxbrew/.linuxbrew/bin/brew
    fi
  fi

  if [ -z ${FC+x} ] || [ -z ${CC+x} ] || [ -z ${CXX+x} ]; then
    ask_homebrew_package_permission "gfortran, gcc, and g++" "gcc@$GCC_VER" 
    exit_if_user_declines
    "$BREW_COMMAND" install gcc@$GCC_VER
  fi
  if [ -z ${PKG_CONFIG+x} ]; then
    ask_homebrew_package_permission "pkg-config"
    exit_if_user_declines
    "$BREW_COMMAND" install pkg-config
  fi
  if [ -z ${REALPATH+x} ] || [ -z ${MAKE+x} ] ; then
    ask_homebrew_package_permission "realpath and make" "coreutils"
    exit_if_user_declines
    "$BREW_COMMAND" install coreutils
  fi

  CC=`which gcc-$GCC_VER`
  CXX=`which g++-$GCC_VER`
  FC=`which gfortran-$GCC_VER`
  PREFIX=`realpath $PREFIX`
fi

export FPM_FC="$FC"
export FPM_CC="$CC"
FPM_SOURCE_URL="https://github.com/fortran-lang/fpm/archive/refs/tags/v$FPM_VERSION.tar.gz"

if [ ! -d build/dependencies ]; then
  mkdir -p build/dependencies
fi

ask_package_permission()
{
  echo ""
  echo "$1 not found."
  echo ""
  printf "Is it ok to download and install $1? [yes] (Press 'Enter' for the square-bracketed default answer.) "
}

if command -v fpm > /dev/null 2>&1; then
  FPM="fpm"
else
  ask_package_permission "fpm"
  exit_if_user_declines
  curl -L $FPM_SOURCE_URL | tar xvz -C build/dependencies/
  (cd build/dependencies/fpm-$FPM_VERSION && ./install.sh --prefix="$PREFIX")
fi

export PKG_CONFIG_PATH="$PREFIX"/lib/pkgconfig
pkg="gasnet-smp-seq"
if [ ! -f "$PKG_CONFIG_PATH/$pkg.pc" ]; then
  echo ""
  echo "GASNet-EX $pkg.pc file not found in $PKG_CONFIG_PATH."
  echo ""
  printf "Is it ok to download and install GASNet-EX? [yes] (Press 'Enter' for the square-bracketed default answer.) "
  read answer
  if [ -n "$answer" -a "$answer" != "y" -a "$answer" != "Y" -a "$answer" != "Yes" -a "$answer" != "YES" -a "$answer" != "yes" ]; then
    echo "Installation declined."
    echo "Please ensure the $pkg.pc file is in $PKG_CONFIG_PATH and then rerun './install.sh'."
    echo "Caffeine was not installed."
  fi

  GASNET_TAR_FILE="GASNet-$GASNET_VERSION.tar.gz"
  GASNET_SOURCE_URL="https://gasnet.lbl.gov/EX/GASNet-$GASNET_VERSION.tar.gz"
  DEPENDENCIES_DIR="build/dependencies"
  if [ ! -d $DEPENDENCIES_DIR ]; then
    mkdir -pv $DEPENDENCIES_DIR
  fi
  
  curl -L $GASNET_SOURCE_URL | tar xvz - -C $DEPENDENCIES_DIR
  
  if [ -d $DEPENDENCIES_DIR/GASNet-$GASNET_VERSION ]; then
    cd $DEPENDENCIES_DIR/GASNet-$GASNET_VERSION
      FC="$FC" CC="$CC" CXX="$CXX" ../GASNet-$GASNET_VERSION/configure --prefix "$PREFIX"
      make -j 8 all
      make check
      make install
    cd -
  fi
fi

GASNET_LDFLAGS="`pkg-config $pkg --variable=GASNET_LDFLAGS`"
GASNET_LIBS="`pkg-config $pkg --variable=GASNET_LIBS`"
GASNET_CC="`pkg-config $pkg --variable=GASNET_CC`"
GASNET_CFLAGS="`pkg-config $pkg --variable=GASNET_CFLAGS`"
GASNET_CPPFLAGS="`pkg-config $pkg --variable=GASNET_CPPFLAGS`"

echo "# DO NOT EDIT OR COMMIT -- Created by caffeine/install.sh" > build/fpm.toml
cp manifest/fpm.toml.template build/fpm.toml
GASNET_LIB_LOCATIONS=`echo $GASNET_LIBS | awk '{locs=""; for(i = 1; i <= NF; i++) if ($i ~ /^-L/) {locs=(locs " " $i);}; print locs; }'`
GASNET_LIB_NAMES=`echo $GASNET_LIBS | awk '{names=""; for(i = 1; i <= NF; i++) if ($i ~ /^-l/) {names=(names " " $i);}; print names; }' | sed 's/-l//g'`
FPM_TOML_LINK_ENTRY="link = [\"$(echo ${GASNET_LIB_NAMES} | sed 's/ /", "/g')\"]"
echo "${FPM_TOML_LINK_ENTRY}" >> build/fpm.toml
ln -f -s build/fpm.toml

cd "$PKG_CONFIG_PATH"
  echo "CAFFEINE_FPM_LDFLAGS=$GASNET_LDFLAGS $GASNET_LIB_LOCATIONS" >  caffeine.pc
  echo "CAFFEINE_FPM_CC=$GASNET_CC"                                 >> caffeine.pc
  echo "CAFFEINE_FPM_CFLAGS=$GASNET_CFLAGS $GASNET_CPPFLAGS"        >> caffeine.pc
  echo "Name: caffeine"                                             >> caffeine.pc
  echo "Description: Coarray Fortran parallel runtime library"      >> caffeine.pc
  echo "URL: https://gitlab.lbl.gov/berkeleylab/caffeine"           >> caffeine.pc
  echo "Version: 0.1.0"                                             >> caffeine.pc
cd -

cd build
  echo "#!/bin/sh"                                                             >  run-fpm.sh
  echo "#-- DO NOT EDIT -- created by caffeine/install.sh"                     >> run-fpm.sh
  echo "\"${PREFIX}\"/bin/fpm \$@ \\"                                          >> run-fpm.sh
  echo "--c-compiler \"`pkg-config caffeine --variable=CAFFEINE_FPM_CC`\" \\"  >> run-fpm.sh
  echo "--c-flag \"`pkg-config caffeine --variable=CAFFEINE_FPM_CFLAGS`\" \\"  >> run-fpm.sh
  echo "--link-flag \"`pkg-config caffeine --variable=CAFFEINE_FPM_LDFLAGS`\"" >> run-fpm.sh
  chmod u+x run-fpm.sh
cd -

./build/run-fpm.sh build

echo ""
echo "________________ Caffeine has been dispensed! ________________"
echo ""
echo "To rebuild or to run tests or examples via the Fortran Package"
echo "Manager (fpm) with the required compiler/linker flags, pass a"
echo "fpm command to the build/run-fpm.sh script. For example, run"
echo "the program example/hello.f90 as follows:"
echo ""
echo "./build/run-fpm.sh run --example hello"
