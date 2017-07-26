# LabelTime
Simulations of the label-time experiment by Katherine E. Twomey and Gert Westermann.

# Dependencies
All python files are written for python3.
This repository depends on the BackPropNetworks repository to work.

After having pulled/cloned both repositories, you may need to add them to your `PYTHONPATH`. Instructions to do so under any OS can be found [here](https://stackoverflow.com/a/12311321/8232125) (don't forget to replace `python` by `python3`).

# Using this repository
## Running simulations
All simulations are started through the [`main.py`](./main.py) file. Simply run `python3 main.py` from this repository in a command-line to start simulations with default parameters.

You can change the number of subjects and most parameters in `main.py`, but you should read the documentation in [`Experiments.py`](./Experiments.py) before doing so.

## Analysing results
All R codes can be found in the [Analyses](./Analyses/) directory. Each R file can be run independently. Filenames are usually meaningful to know what an R file does, but the files are also well documented if you want to check what exactly we are doing with our data.
