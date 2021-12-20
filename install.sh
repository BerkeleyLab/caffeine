#!/bin/dash

set -e # exit on error

usage()
{
    echo "Caffeine Installation Script"
    echo ""
    echo "USAGE:"
    echo "./install.sh [--help | [--prefix=PREFIX]"
    echo ""
    echo " --help             Display this help text"
    echo " --prefix=PREFIX    Install binary in 'PREFIX/bin'"
    echo "                    Default prefix='\$HOME/.local/bin'"
    echo ""
}

PREFIX="$HOME/.local"

while [ "$1" != "" ]; do
    PARAM=$(echo "$1" | awk -F= '{print $1}')
    VALUE=$(echo "$1" | awk -F= '{print $2}')
    case $PARAM in
        -h | --help)
            usage
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
GCC_VER=11

if ! command -v gcc-$GCC_VER > /dev/null 2>&1 \
    || ! command -v g++-$GCC_VER > /dev/null 2>&1 \
    || ! command -v gfortran-$GCC_VER > /dev/null 2>&1 \
    || ! command -v pkg-config > /dev/null 2>&1 \
    || ! command -v realpath > /dev/null 2>&1 \
    || ! command -v make > /dev/null 2>&1; then
  if ! command -v curl > /dev/null 2>&1; then
    echo "No download mechanism found. Please install curl and rerun ./install.sh"
    exit 1
  fi

  if ! command -v brew > /dev/null ; then
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    BREW_COMMAND=/home/linuxbrew/.linuxbrew/bin/brew
  else
    BREW_COMMAND="brew"
  fi
  "$BREW_COMMAND" install pkg-config coreutils gcc@$GCC_VER
fi
export CC=`which gcc-$GCC_VER`
export CXX=`which g++-$GCC_VER`
export FC=`which gfortran-$GCC_VER`

PREFIX=`realpath $PREFIX`

GASNET_TAR_FILE="GASNet-stable.tar.gz"
GASNET_SOURCE_URL="https://bitbucket.org/berkeleylab/gasnet/downloads/$GASNET_TAR_FILE"
DEPENDENCIES_DIR="build/dependencies"
if [ ! -d $DEPENDENCIES_DIR ]; then
  mkdir -pv $DEPENDENCIES_DIR
fi

cd $DEPENDENCIES_DIR

  if [ ! -f $GASNET_TAR_FILE ]; then
    curl -L $GASNET_SOURCE_URL > $GASNET_TAR_FILE
  fi

  if [ ! -d GASNet-stable ]; then
    tar xf $GASNET_TAR_FILE
  fi

  if [ ! -d gasnet ]; then
    mkdir -v gasnet
    cd gasnet
      ../GASNet-stable/configure --prefix "$PREFIX"
      make -j 8 all
      make check
      make install
    cd ..
  fi
cd ../..

export PKG_CONFIG_PATH="$PREFIX"/lib/pkgconfig
pkg="gasnet-smp-seq"

GASNET_LDFLAGS="`pkg-config $pkg --variable=GASNET_LDFLAGS`"
GASNET_LIBS="`pkg-config $pkg --variable=GASNET_LIBS`"
GASNET_CC="`pkg-config $pkg --variable=GASNET_CC`"
GASNET_CFLAGS="`pkg-config $pkg --variable=GASNET_CFLAGS`"
GASNET_CPPFLAGS="`pkg-config $pkg --variable=GASNET_CPPFLAGS`"

echo "# DO NOT EDIT OR COMMIT -- Created by caffeine/install.sh" > build/fpm.toml
cat manifest/fpm.toml.template >> build/fpm.toml
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
  echo "Description: coarray fortran parallel runtime library"      >> caffeine.pc
  echo "URL: https://gitlab.lbl.gov/rouson/caffeine"                >> caffeine.pc
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


export FPM_FC="$FC"
export FPM_CC="$CC"
if [ ! -d build/dependencies/fpm ]; then
  git clone https://github.com/fortran-lang/fpm build/dependencies/fpm
  cd build/dependencies/fpm
    ./install.sh --prefix="$PREFIX"
  cd -
fi

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
