Assuming we're in the root directory, OCAML_3110_GP:

- To process a default csv (The default csv works with 2D points): 
dune exec bin/main.exe

- To process a csv of 1D points:
dune exec bin/main.exe data/test_data.csv 1D

- To process a csv of 2D points:
dune exec bin/main.exe ./data/test_data_2d.csv 2D

- To process a csv of 3D points:
dune exec bin/main.exe ./data/test_data_3d.csv 3D