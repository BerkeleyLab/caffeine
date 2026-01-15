cmake_minimum_required(VERSION 3.20.0)

project(caffeine LANGUAGES C CXX Fortran)
include(ExternalProject)

option(USE_FPM "Specifies whether you want to use FPM to build external dependencies." OFF)
if(NOT DEFINED BUILD_SHARED_LIBS)
  set(BUILD_SHARED_LIBS ON CACHE BOOL "Build shared libraries" FORCE)
endif()

add_subdirectory(src)

# ---------------------------------------------------------
# Dependencies
# ---------------------------------------------------------
if (NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/install)
endif()

target_include_directories(caffeine PUBLIC ${CMAKE_BINARY_DIR}/include)
target_include_directories(caffeine PUBLIC ${CMAKE_SOURCE_DIR}/include)

message(STATUS "CMAKE_Fortran_COMPILER_ID: ${CMAKE_Fortran_COMPILER_ID}")
if(CMAKE_Fortran_COMPILER_ID STREQUAL "LLVMFlang")
  # We do that because fpm require to use "flang-new" as a fortran compiler because 
  # "flang" is used for classic-flang.
  get_filename_component(_DIR "${CMAKE_Fortran_COMPILER}" DIRECTORY)
  set(CMAKE_Fortran_COMPILER "${_DIR}/flang-new" CACHE FILEPATH "" FORCE)
endif()

# --------
# Assert
# --------
SET(ASSERT_GIT "https://github.com/berkeleylab/assert.git")
SET(ASSERT_TAG "3.0.2")
if (USE_FPM)
  set(ASSERT_BUILD_COMMAND fpm build --compiler ${CMAKE_Fortran_COMPILER})
  set(ASSERT_INSTALL_COMMAND 
        fpm install --prefix=<INSTALL_DIR> --compiler ${CMAKE_Fortran_COMPILER} &&
        ${CMAKE_COMMAND} -E make_directory <INSTALL_DIR> &&
        ${CMAKE_COMMAND} -E copy_directory <SOURCE_DIR>/include <INSTALL_DIR>/include
  ) 
else()
  message(STATUS "Since USE_FPM is not enabled, libassert will be installed manually from source."
                 "If you want to use Assert's build system, please enable USE_FPM.")
  set(ASSERT_BUILD_COMMAND ${CMAKE_Fortran_COMPILER} -c <SOURCE_DIR>/src/assert_m.F90 
        -I<SOURCE_DIR>/include
        -DASSERT_MULTI_IMAGE=0
        -fPIC
  )
  set(ASSERT_INSTALL_COMMAND 
        ${CMAKE_COMMAND} -E make_directory <INSTALL_DIR>/lib &&
        ${CMAKE_COMMAND} -E copy_directory <SOURCE_DIR>/include <INSTALL_DIR>/include &&
        ${CMAKE_COMMAND} -E copy <SOURCE_DIR>/assert_m.mod <INSTALL_DIR>/include &&
        ar rcs <INSTALL_DIR>/lib/libassert.a <SOURCE_DIR>/assert_m.o
  )
endif()
ExternalProject_Add(assert_lib 
  GIT_REPOSITORY ${ASSERT_GIT} 
  GIT_TAG ${ASSERT_TAG}
  CONFIGURE_COMMAND ""
  INSTALL_DIR ${CMAKE_BINARY_DIR}
  BUILD_COMMAND ${ASSERT_BUILD_COMMAND} 
  INSTALL_COMMAND ${ASSERT_INSTALL_COMMAND}
  BUILD_IN_SOURCE 1
)

add_library(assert_lib_external STATIC IMPORTED)
set_target_properties(assert_lib_external PROPERTIES
    IMPORTED_LOCATION ${CMAKE_BINARY_DIR}/lib/libassert.a
    INTERFACE_INCLUDE_DIRECTORIES $<INSTALL_INTERFACE:${CMAKE_BINARY_DIR}/include>
)
add_dependencies(assert_lib_external assert_lib)
target_link_libraries(caffeine PRIVATE assert_lib_external)

# -----------
# GASNet-EX
# -----------
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
if (NOT DEFINED GASNet_THREADING)
  set(GASNet_THREADING seq)
endif()
if (NOT DEFINED GASNet_PREFERRED_CONDUITS)
  set(GASNet_PREFERRED_CONDUITS smp)
endif()

find_package(GASNet QUIET)
if (NOT GASNet_FOUND)
  set(GASNet_TAG "2025.8.0")
  set(GASNet_MD5 "7f61c16c6d06e9789157b3b6804e1927")
  message(STATUS "Cannot find GASNet-Ex, maybe GASNet_ROOT variable is not set.")
  message(STATUS "Configuration and building of GASNet-Ex from release ${GASNet_TAG}.")
  if(BUILD_SHARED_LIBS)
    if(NOT CMAKE_C_FLAGS MATCHES "-fPIC")
      set(CMAKE_C_FLAGS "-fPIC ${CMAKE_C_FLAGS}")
    endif()
    if(NOT CMAKE_CXX_FLAGS MATCHES "-fPIC")
      set(CMAKE_CXX_FLAGS "-fPIC ${CMAKE_CXX_FLAGS}")
    endif()
  endif()
  
  ExternalProject_Add(gasnet_ex
      URL https://gasnet.lbl.gov/EX/GASNet-${GASNet_TAG}.tar.gz
      URL_HASH MD5=${GASNet_MD5}
      INSTALL_DIR ${CMAKE_BINARY_DIR}/gasnet
      DOWNLOAD_EXTRACT_TIMESTAMP TRUE
      DOWNLOAD_NO_EXTRACT FALSE
      CONFIGURE_COMMAND 
        <SOURCE_DIR>/configure ac_cv_cxx_exceptions=yes --prefix=<INSTALL_DIR> 
                               --enable-static --enable-seq
                               --with-cflags=${CMAKE_C_FLAGS}
                               --with-cppflags=${CMAKE_CXX_FLAGS}
                               --with-cc=${CMAKE_C_COMPILER}
                               --with-cxx=${CMAKE_CXX_COMPILER}
      BUILD_COMMAND make -j
      INSTALL_COMMAND make install
      BUILD_IN_SOURCE 1
  )
  set(GASNET_INSTALL ${CMAKE_BINARY_DIR}/gasnet)
  add_library(gasnet_lib STATIC IMPORTED)
  set_target_properties(gasnet_lib PROPERTIES
    INTERFACE_COMPILE_DEFINITIONS "GASNET_CONDUIT_SMP;GASNET_SEQ"
      IMPORTED_LOCATION ${GASNET_INSTALL}/lib/libgasnet-smp-seq.a
      INTERFACE_INCLUDE_DIRECTORIES $<INSTALL_INTERFACE:${GASNET_INSTALL}/include>
  )
  add_dependencies(gasnet_lib gasnet_ex)
  target_link_libraries(caffeine PUBLIC gasnet_lib)
  target_include_directories(caffeine PUBLIC ${GASNET_INSTALL}/include)
  target_include_directories(caffeine PUBLIC ${GASNET_INSTALL}/include/smp-conduit)
else()
  target_link_libraries(caffeine PUBLIC GASNet::GASNet)
endif()

# ---------------------------------------------------------
# Installation 
# ---------------------------------------------------------
install(TARGETS caffeine
  DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
)

# ---------------------------------------------------------
# Test using App 
# ---------------------------------------------------------
if(CMAKE_Fortran_COMPILER_ID STREQUAL "LLVMFlang")
  # If you want to view the test logs, please run : make test ARGS="--verbose"
  enable_testing()
  add_executable(test_native_multi_image app/native-multi-image.F90)
  
  target_compile_options(test_native_multi_image PRIVATE "-fcoarray" "-cpp" "-DHAVE_MULTI_IMAGE")
  target_link_libraries(test_native_multi_image PRIVATE caffeine)
  
  add_test(NAME TestNativeMultiImage COMMAND test_native_multi_image)
else()
  message(STATUS "${CMAKE_Fortran_COMPILER_ID} doesn't support lowering to PRIF, test can't be built.")
endif()
