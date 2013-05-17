bachelor-thesis
===============

Installation steps:

- Get Ocaml and Git, Noweb and LaTeX. Also install Ocamlgraph, Graphviz and Inkscape. 

- Get the UPPAAL DBM libraries from www.cs.aau.dk/~adavid/UDBM and install them, either as root or in your home directory.

- Get the Springer LNCS style files and install them, preferably as root.

- Clone the code: git clone https://github.com/sguha100/bachelor-thesis.git (use a proxy if required. The 'env' command is helpful here.)

- Change all references to /home/mihir/something to the location of your installation of the UPPAAL libraries.

- cd to ocaml and run 'make'.

- Now, try running an example. Here, we select breaking2.txt. Run './calc.native < grammar-noweb/examples/breaking2.txt'.

- This generates 4 files in /tmp.
-- /tmp/lts.txt
-- /tmp/lts.dot
-- /tmp/lts_quotient.dot
-- /tmp/quotient_lts.dot
Of these, the last 3 can be processed with graphviz to get png images.