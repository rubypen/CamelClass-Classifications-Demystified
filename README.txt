NOTE:
If you see warnings that look like the following, ignore them as they do not 
affect the program and are due to the ocaml gui kit we are using:
ld: warning: ignoring duplicate libraries: '-lcairo', '-lfreetype'

USAGE:
Assuming we're in the root directory, OCAML_3110_GP:

- Run the Program
dune exec bin/main.exe 

- A path for a csv of 4D points if you'd like to input a csv when prompted:
data/test_data_4d.csv