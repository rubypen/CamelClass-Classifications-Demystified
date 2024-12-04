Instructions:

Step 1) 
Install New Packages:
$ opam update
$ opam upgrade
$ opam install csv ansiterminal lablgtk3 plplot

Step 2) 
Access the Project:
1. Download submission from CMSx
2. Unzip the file and open on VSCode

Step 3)
Build the Project:
$ dune build

Step 4)
Fix Installation Errors:
- If you get the following error:
  "File "test/dune", line 2, characters 7-24:
  2 |  (name test_GroupProject) ..."    
- Run the following:
  export PKG_CONFIG_PATH=/opt/homebrew/Cellar/pango/1.55.0/lib/pkgconfig:/opt/homebrew/lib/pkgconfig
  export LIBRARY_PATH=/opt/homebrew/Cellar/pango/1.55.0/lib:/opt/homebrew/lib
  export DYLD_LIBRARY_PATH=/opt/homebrew/Cellar/pango/1.55.0/lib:/opt/homebrew/lib
- Build the project again:
$ dune build
