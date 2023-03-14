#!/usr/bin/env bash

# exit script if any command fails
set -e
set -o pipefail

# chipyard 1.8.1 version of tools
DSPTOOLS_COMMIT=a1809fb
ROCKET_COMMIT=44b0b82
FIRESIM_COMMIT=8176b65
API_CONFIG_COMMIT=fd8df11
# the newest one
ROCKET_DSP_COMMIT=16e26e7

git submodule add https://github.com/ucb-bar/dsptools.git tools/dsptools
cd tools/dsptools
git checkout $DSPTOOLS_COMMIT
cd ../..
git submodule add https://github.com/ucb-bar/rocket-dsp-utils.git tools/rocket-dsp-utils
cd tools/rocket-dsp-utils
git checkout $ROCKET_DSP_COMMIT
cd ../..
git submodule add https://github.com/chipsalliance/cde.git tools/api-config-chipsalliance
cd tools/api-config-chipsalliance
git checkout $API_CONFIG_COMMIT
cd ../..
git submodule add https://github.com/chipsalliance/rocket-chip.git generators/rocket-chip
cd generators/rocket-chip
git checkout $ROCKET_COMMIT

git submodule add https://github.com/firesim/firesim.git sims/firesim
cd sims/firesim
git checkout $FIRESIM_COMMIT
cd ../..
git config --local submodule.sims/firesim.update none
git submodule update --init --recursive
git config --local --unset-all submodule.sims/firesim.update
git submodule update --init sims/firesim
cd ../..
mv build.txt build.sbt

if [ -d project ]; then
   echo "Directory project already exists"
else
   mkdir project
fi

# add plugins
echo -e 'addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.15.0")\naddSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.21")\naddSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.5.3")' > ./project/plugins.sbt
