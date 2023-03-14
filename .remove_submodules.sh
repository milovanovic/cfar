git submodule deinit -f ./generators/rocket-chip/
rm -rf .git/modules/generators/rocket-chip/
git rm -f generators/rocket-chip/

git submodule deinit -f tools/rocket-dsp-utils/
rm -rf .git/modules/tools/rocket-dsp-utils/
git rm -f tools/rocket-dsp-utils/

git submodule deinit -f tools/dsptools/
rm -rf .git/modules/tools/dsptools/
git rm -f tools/dsptools/

git submodule deinit -f tools/api-config-chipsalliance/
rm -rf .git/modules/tools/api-config-chipsalliance/
git rm -f tools/api-config-chipsalliance/

git submodule deinit -f sims/firesim
rm -rf .git/modules/sims/firesim
git rm -f sims/firesim

rm .gitmodules
mv build.sbt build.txt
rm project/plugins.sbt
