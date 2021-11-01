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

if [ ${skip+x} ]; then

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

mkdir -v gasnet
cd gasnet
../GASNET-stable/configure --prefix "$PREFIX"
make -j 8 all
make check
make install

fi

export gasnet_prefix="$HOME"/.local
export PKG_CONFIG_PATH="$gasnet_prefix"/lib/pkgconfig
export pkg="gasnet-smp-seq"

export GASNET_LDFLAGS=`pkg-config $pkg --variable=GASNET_LDFLAGS`
export GASNET_LIBS=`pkg-config $pkg --variable=GASNET_LIBS`
export GASNET_CC=`pkg-config $pkg --variable=GASNET_CC`
export GASNET_CFLAGS=`pkg-config $pkg --variable=GASNET_CFLAGS`
export GASNET_CPPFLAGS=`pkg-config $pkg --variable=GASNET_CPPFLAGS`

export FPM_LDFLAGS="$GASNET_CFLAGS $GASNET_LDFLAGS $GASNET_LIBS"
export FPM_CC="$GASNET_CC"
export FPM_CFLAGS="$GASNET_CFLAGS $GASNET_CPPFLAGS"

fpm build  --c-compiler "$GASNET_CC" \
  --c-flag "$GASNET_CFLAGS $GASNET_CPPFLAGS" \
  --link-flag "$GASNET_CFLAGS $GASNET_LDFLAGS $GASNET_LIBS"
