#Antigen
Alpha Build 0.1

Antigen is a Configuration-less RTEMS source builder and cross-compilation tool. Antigen does not require
build set files in order to compile the latest development versions of RTEMS. The goal of Antigen is
to make it easier to contribute to and use the latest HEAD of RTEMS without having to wait for a build
set to be made for one's target architecture.

Antigen is currently used in conjunction with the MLton Standard-ML compiler to experiment with
real-time functional programming in an embedded context.

#Use
Issue `antigen --help` to see example usage. As of Alpha 0.1 only the `--rversion` option is supported.
The target architecture is set to ARM and the prefix is set to `~/rtems`

NOTE: Antigen compiles the entire GCC toolchain for your target architecture. This process typically takes
multiple hours, even on fast hardware.

#Dependencies
Antigen requires a complete GNU build toolchain, including `libc`, `autotools`, `gcc`, etc. Your
system's dependency mileage may vary.

Antigen also requires a recent version of GHC (4.7 is recommended), and the Haskell Cabal.

#Installation
Clone this repository with `git clone https://github.com/nateburgers/antigen.git`, then cd into the
project directory and perform a `cabal install`. Your version of Antigen should be built and placed
in your cabal binary directory, (usually `~/.cabal/bin`). Add this directory to your `PATH` to make
Antigen executable.

Antigen is currently unavailable online as a cabal package. This will change in the very near future.

#Roadmap
## 0.2
* Offer control over target architecture and prefix
* Be capable of building RTEMS from a checked-out Git repository instead of requiring a clone
## 0.3
* Build RTEMS-embedded projects
* Implicitly determine architecture-specific patches (available online)

#Issues
* Because Antigen retrieves all of its configuration information implicitly, its build process is
  far less stable than that of the RTEMS Source Builder.
* Cross-compiling GCC and RTEMS on Mac OS X is flaky and cannot support Clang.
* RTEMS versions prior to 4.9 are unsupported.
