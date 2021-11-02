#!/bin/sh

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
    echo "Set the environment variables FPM_{FC,CC,CXX} and FPM_{F,C,CC}FLAGS to specify"
    echo "the Fortran/C/C++ compilers and build flags this script will use."
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

if command -v curl > /dev/null 2>&1; then
    FETCH="curl -L"
elif command -v wget > /dev/null 2>&1; then
    FETCH="wget -O -"
else
    echo "No download mechanism found. Please install curl or wget first."
    exit 1
fi

GASNET_TAR_FILE="GASNet-stable.tar.gz"
GASNET_SOURCE_URL="https://bitbucket.org/berkeleylab/gasnet/downloads/$GASNET_TAR_FILE"
DEPENDENCIES_DIR="build/dependencies"
if [ ! -d $DEPENDENCIES_DIR ]; then
  mkdir -pv $DEPENDENCIES_DIR
fi

cd $DEPENDENCIES_DIR
  $FETCH $GASNET_SOURCE_URL > $GASNET_TAR_FILE
  
  if [ ! -f $GASNET_TAR_FILE ]; then
    echo "$GASNET_TAR_FILE not found"
    exit 1
  fi
  
  if [ -d GASNet-stable ]; then
    rm -rf GASNet-stable
  fi
  tar xf $GASNET_TAR_FILE
  
  if [ -d gasnet ]; then
    rm -rf gasnet
  fi
cd -

mkdir -v "$DEPENDENCIES_DIR"/gasnet
cd "$DEPENDENCIES_DIR"/gasnet
  ../GASNET-stable/configure --prefix "$PREFIX"
  make -j 8 all
  make check
  make install
cd -

export gasnet_prefix="$HOME"/.local
export PKG_CONFIG_PATH="$gasnet_prefix"/lib/pkgconfig
export pkg="gasnet-smp-seq"

export GASNET_LDFLAGS=`pkg-config $pkg --variable=GASNET_LDFLAGS`
export GASNET_LIBS=`pkg-config $pkg --variable=GASNET_LIBS`
export GASNET_CC=`pkg-config $pkg --variable=GASNET_CC`
export GASNET_CFLAGS=`pkg-config $pkg --variable=GASNET_CFLAGS`
export GASNET_CPPFLAGS=`pkg-config $pkg --variable=GASNET_CPPFLAGS`

export FPM_LDFLAGS="$GASNET_LDFLAGS $GASNET_LIBS"
export FPM_CC="$GASNET_CC"
export FPM_CFLAGS="$GASNET_CFLAGS $GASNET_CPPFLAGS"

fpm build

mkdir -p build/pkgconfig
cd build/pkgconfig
  echo "CAFFEINE_FPM_LDFLAGS=$FPM_LDFLAGS"                     >  caffeine.pc
  echo "CAFFEINE_FPM_CC=$FPM_CC"                               >> caffeine.pc
  echo "CAFFEINE_FPM_CFLAGS=$FPM_CFLAGS"                       >> caffeine.pc
  echo "Name: caffeine"                                        >> caffeine.pc
  echo "Description: coarray fortran parallel runtime library" >> caffeine.pc
  echo "URL: https://gitlab.lbl.gov/rouson/caffeine"           >> caffeine.pc
  echo "Version: 0.1.0"                                        >> caffeine.pc
cd -

cd build
  export PKG_CONFIG_PATH="pkgconfig"
  echo "#!/bin/sh"                                                             >  run-fpm.sh
  echo "#-- DO NOT EDIT -- created by caffeine/install.sh"                     >> run-fpm.sh
  echo "fpm \$@ \\"                                                            >> run-fpm.sh
  echo "--c-compiler \"`pkg-config caffeine --variable=CAFFEINE_FPM_CC`\" \\"  >> run-fpm.sh
  echo "--c-flag \"`pkg-config caffeine --variable=CAFFEINE_FPM_CFLAGS`\" \\"  >> run-fpm.sh
  echo "--link-flag \"`pkg-config caffeine --variable=CAFFEINE_FPM_LDFLAGS`\"" >> run-fpm.sh
  chmod u+x run-fpm.sh
cd -

echo ""
echo "________________ Caffeine has been dispensed! ________________" 
echo ""
echo "To rebuild or to run tests or examples via the Fortran Package"
echo "Manager (fpm) with the required compiler/linker flags, pass a"
echo "fpm command to the build/run-fpm.sh script. For example, run"
echo "the program example/hello.f90 as follows:"
echo ""
echo "./build/run-fpm.sh run --example hello"
