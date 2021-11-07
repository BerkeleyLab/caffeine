---
project: Caffeine library 
summary: A parallel runtime library supporting standard Fortran features.
src_dir: src/
exclude_dir: doc
output_dir: doc/html
preprocess: true
macro: FORD
preprocessor: gfortran -E
display: public
         protected
         private
source: true
graph: true
md_extensions: markdown.extensions.toc
