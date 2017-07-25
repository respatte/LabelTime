#!/usr/bin/env python
# -*- coding: utf-8 -*-
import time
import warnings
from multiprocessing import Pool

from Experiments import SingleObjectExperiment

def run_subjects(n_subjects, experiment, bash_i=0, explo_ratio=None,
				 verbose=False):
	if verbose:
		t = time.time()
		print("=" * 40)
		if explo_ratio is not None:
			print("Starting runs for explo_ratio =", explo_ratio)
		else:
			print("Starting runs for CategoryExperiment")
	if experiment == "SingleObject":
		condition = SingleObjectExperiment((1,10,8), (.1,explo_ratio),
										   n_subjects, n_subjects*bash_i)
	elif experiment == "Category":
		condition = CategoryExperiment((1,10,0), (.1, 0),
									   n_subjects, n_subjects*bash_i)
	results = condition.run_experiment()
	if verbose:
		t = time.gmtime(time.time() - t)
		print("Runs finished in", time.strftime("%H:%M:%S",t))
	return results

def main():
	total = time.time()
	warnings.filterwarnings("ignore")
	# Run SingleObject experiment with various exploration ratios
	explo_ratios = [.25,.375,.5,.625,.75]
	t_results = {}
	f_results = {}
	for i, explo_ratio in enumerate(explo_ratios):
		results_SO = run_subjects(32, "SingleObject", i, explo_ratio,
								  verbose=True)
		t_results.update(results_SO[1])
		f_results.update(results_SO[0])
	Experiment.output_data(f_results, "Results/SingleObject")
	# Run Category experiment
	results_C = run_subjects(32*5, "Category", verbose=True)
	Experiment.output_data(results_C[0], "Results/Category")
	total = time.gmtime(time.time() - total)
	print("="*27,
		  "Total run time:",
		  time.strftime("%H:%M:%S",total),
		  "="*27)
	return (results_SO, results_C)

if __name__ == "__main__":
	results_SO, results_C = main()
