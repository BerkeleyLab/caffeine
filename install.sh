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
    echo "Set the environment variables {FC,CC,CXX} and {F,C,CC}FLAGS to specify"
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

if [ -z ${FC+x} ]; then
    FC="gfortran"
fi
if [ -z ${FFLAGS+x} ]; then
    FFLAGS="-g -fbacktrace -O3"
fi

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
if [ -d GASNet-stable ]; then
  rm -rf GASNet-stable
fi
tar xf GASNet-stable.tar.gz
if [ -d build ]; then
  rm -rf build
fi
mkdir -v gasnet
cd gasnet
../GASNET-stable/configure --prefix "$PREFIX"
make all
make check
make install
fpm build
