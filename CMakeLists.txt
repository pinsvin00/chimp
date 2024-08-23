cmake_minimum_required(VERSION 3.10)
project(MLang)

# Set the source directory
set(SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src)


find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS})

file(GLOB SOURCES "${SOURCE_DIR}/*.cpp")
set_source_files_properties(src/lex.yy.c PROPERTIES LANGUAGE CXX )


add_executable(mlang src/main.cpp src/Token.cpp src/ASTCreator.cpp src/lex.yy.c)

llvm_map_components_to_libnames(llvm_libs support core irreader)
target_link_libraries(mlang ${llvm_libs})

# Optionally, you can set C++ standard
set_target_properties(mlang PROPERTIES
    CXX_STANDARD 23
    CXX_STANDARD_REQUIRED YES
    CXX_EXTENSIONS NO
)