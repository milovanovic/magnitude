Design Generator of Complex Number Magnitude and its Logarithm
=======================================================

This repository contains design generator of complex number magnitude estimators based on [JPL approximation](https://ipnpr.jpl.nasa.gov/progress_report/42-40/40L.PDF). Design generator can provide high-performance binary logarithm calculation as well.

## Documentation

* doc/mag_generator.md - detailed documentation about design generator
* doc/images - contains design block diagrams and  python plots

Much more useful information about this work can be found inside ["On Efficient Hardware Realizations of Fixed-Point  Complex Number Magnitude and its Logarithms"](https://www.etran.rs/2023/E_PROCEEDINGS_ICETRAN_2023/IcETRAN23_RADOVI/ELI1.2.pdf) paper published on X International Conference IcETRAN,  2023.
If you are using this work for research, please cite it by the following publication:

    @inproceedings{magnitude,
      author={Marija L. Petrovic and Vladimir M. Milovanovic},
      booktitle={X International Conference IcETRAN},
      title={On Efficient Hardware Realizations of Fixed-Point Complex Number Magnitude and its Logarithms},
      year={2023},
      volume={ELI 1.2},
      pages={1-5}
    }

## Prerequisites

The following software packages should be installed prior to running this project:
* [sbt](http://www.scala-sbt.org)
* [Verilator](http://www.veripool.org/wiki/verilator)

## Setup

Proposed design generator is intended to be used inside [chipyard](https://github.com/ucb-bar/chipyard) environment as one of the generators located inside `generators/dsp-blocks`. Anyhow, if you want to use this repository standalone then follow instructions below:

*  Clone this repository.
*  Switch directory.
*  Initialize all tools and submodules.
*  Compile code, generate verilog or run tests.
```
git clone https://github.com/milovanovic/magnitude.git
cd magnitude
./scripts/init_submodules_and_build_sbt.sh
sbt test
```
#### Note
The shell script `init_submodules_and_build_sbt.sh`, initializes all tools and generators required to run this project. Besides that, it initializes `bulid.sbt` with all correctly defined dependencies. Versions of tools and generators correspond to chipyard 1.9.1 release, only `rocket-dsp-utils` differs and it is set to the newest commit. The user can replace versions by changing the corresponding checkout commits inside the same script.
The shell script `remove_submodules.sh` executes commands that reverse the commands listed in `init_submodules_and_build_sbt.sh`.

## Guide For New Contributors

If you are trying to make a contribution to this project, please guide following:
1. You can contribute by submitting pull requests from your fork to the upstream repository.
2. If you need help on making a pull request, follow this [guide](https://docs.github.com/en/github/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests).
3. To understand how to compile and test from the source code, follow the instructions inside the setup section.
