reltool
===============

Pre-installation steps:

- Get G++, Make, Automake, Ocaml (including Findlib), Git, Noweb and Ocamlgraph.

- Get the UPPAAL DBM library, from
  https://github.com/airbornemihir/uppaal-dbm. v2.0.8-reltool2 is
  known to work; you can download the tarball
  (https://github.com/airbornemihir/uppaal-dbm/archive/v2.0.8-reltool2.tar.gz)
  or the zip archive
  (https://github.com/airbornemihir/uppaal-dbm/archive/v2.0.8-reltool2.zip). The
  directory named "modules" contains installation instructions in its
  "README" file.

Installation steps:

- You can download the latest development sources by cloning the git
  repository.
$ git clone https://github.com/sguha100/reltool.git
  Alternatively, you can download a zip file
  (https://github.com/sguha100/reltool/archive/master.zip)
  or a tarball
  (https://github.com/sguha100/reltool/archive/master.tar.gz)
  of the latest development sources if you do not wish to use git or
  clone the entire repository.
  Alternatively, you can download a tarball/zip archive from the
  releases page
  (https://github.com/sguha100/reltool/releases) if you
  do not wish to use development sources.
- run
$ LIBS="-lstdc++" ./configure
  You might have to use the LDFLAGS and CPPFLAGS variables to specify
  the locations of your UPPAAL library and headers, for instance:
$ LDFLAGS="-L$HOME/uppaal/lib" CPPFLAGS="-I$HOME/uppaal/include" LIBS="-lstdc++" ./configure
  or:
$ LDFLAGS="-L/usr/local/uppaal/lib" CPPFLAGS="-I/usr/local/uppaal/include" LIBS="-lstdc++" ./configure
  After this, you can run
$ make
$ make install (make install needs to be a the root user and hence issue $ su if not already running as root.)
  Note that VPATH builds are not supported for now.

- Now, try running an example. Here, we select static_guard_analysis_example1.txt. Run 'calc < grammar-noweb/examples/static_guard_analysis_example1.txt'.

- This generates 4 files in /tmp.
-- /tmp/lts.txt
-- /tmp/lts.dot (DOT representation of the zone graph.)
-- /tmp/lts_quotient.dot (DOT representation of the zone graph with subgraphs showing equivalence classes under LTS bisimilarity.)
-- /tmp/quotient_lts.dot (DOT representation of the bisimilarity quotient of the zone graph.)
  Of these, the last 3 can be processed with the dot tool (from graphviz) to get images in PNG/SVG/other file formats supported by graphviz.
