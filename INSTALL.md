Instructions:

Step 1)
1. Download submission from CMSx
2. Unzip the file and open on VSCode
3. Navigate to root directory:
   $ cd OCaml_3110_GP

Step 2)
Install New Packages:
  $ opam update
  $ opam upgrade
  $ opam install csv ansiterminal batteries
  $ opam install lablgtk3 
  $ opam install plplot
-> For these installations, it will ask you to choose y/n. Choose y.
-> Also, it will ask you to download using options 1/2/3/4. Choose 1 (install with homebrew) 
if you have mac homebrew.

Step 3)
1. If you encounter path errors, such as the following:
    Error: Error during linking (exit code 1)
  ...then Update Path:
  $ export PKG_CONFIG_PATH=/opt/homebrew/Cellar/pango/1.55.0/lib/pkgconfig:/opt/homebrew/lib/pkgconfig
  export LIBRARY_PATH=/opt/homebrew/Cellar/pango/1.55.0/lib:/opt/homebrew/lib
  export DYLD_LIBRARY_PATH=/opt/homebrew/Cellar/pango/1.55.0/lib:/opt/homebrew/lib
2. Build the Project:
  $ dune build

--------------------------------------------------------------------------
NOTE:
If you see warnings that look like the following, 
ignore them as they do not affect the program.

ld: warning: ignoring duplicate libraries: '-lcairo'
ld: warning: search path '/opt/homebrew/Cellar/pango/1.54.0/lib' not found

They are due to the ocaml gui kits we are using, which rely on the 
same dependencies.
--------------------------------------------------------------------------

Step 4)
Run the program:
  $ dune exec bin/main.exe


Hooray! Welcome to CamelClass!

--------------------------------------------------------------------------
OTHER:
For using a specific file in I/O, some paths you can supply are:
data/test_data.csv
data/test_data_2d.csv
data/test_data_3d.csv

...And more! They are all provided in the data folder.
--------------------------------------------------------------------------