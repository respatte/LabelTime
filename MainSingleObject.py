#!/usr/bin/env python
# -*- coding: utf-8 -*-
import time
import warnings
from multiprocessing import Pool

from Experiments import SingleObjectExperiment

def run_subjects(n_subjects, explo_ratio, bash_i, memories=1, verbose=False):
	if verbose:
		t = time.time()
		print("=" * 40)
		print("Starting runs for explo_ratio =", explo_ratio)
	condition = SingleObjectExperiment((1,10,8), (.1,explo_ratio),
									   n_subjects, n_subjects*bash_i)
	results = condition.run_experiment()
	if verbose:
		t = time.gmtime(time.time() - t)
		print("Condition finished in", time.strftime("%H:%M:%S",t))
	return results

def main():
	total = time.time()
	warnings.filterwarnings("ignore")
	explo_ratios = [.25,.375,.5,.625,.75]
	t_results = []
	f_results = {}
	for i, explo_ratio in enumerate(explo_ratios):
		results = run_subjects(4, explo_ratio, i, verbose=True)
		t_results.append(results[1])
		f_results.update(results[0])
	SingleObjectExperiment.output_data(f_results,"Results/SingleObject")
	total = time.gmtime(time.time() - total)
	print("="*17,
		  "Total run time for serial version :",
		  time.strftime("%H:%M:%S",total),
		  "="*17)

def main_multiproc():
	total = time.time()
	print("Multiprocessing starting...")
	warnings.filterwarnings("ignore")
	explo_ratios = [.25,.375,.5,.625,.75]
	# Start the subprocesses
	async_results_bpn = {}
	async_results_dmn = {}
	with Pool(processes=10) as pool:
		for i, explo_ratio in enumerate(explo_ratios):
			async_results_bpn[i] = pool.apply_async(run_subjects,
													(8, explo_ratio, i))
			async_results_dmn[i] = pool.apply_async(run_subjects,
													(8, explo_ratio, i, 2))
		pool.close()
		pool.join()
	# Get the results from subprocesses and combine them
	t_results_bpn = {}
	f_results_bpn = {}
	t_results_dmn = {}
	f_results_dmn = {}
	for i in range(len(explo_ratios)):
		results_bpn = async_results_bpn[i].get()
		results_dmn = async_results_dmn[i].get()
		t_results_bpn.update(results_bpn[1])
		f_results_bpn.update(results_bpn[0])
		t_results_dmn.update(results_dmn[1])
		f_results_dmn.update(results_dmn[0])
	SingleObjectExperiment.output_data(f_results_bpn,
									   "Results/SingleObject_BPN")
	SingleObjectExperiment.output_data(f_results_dmn,
									   "Results/SingleObject_DMN")
	total = time.gmtime(time.time() - total)
	print("="*15,
		  "Total run time for multi-proc version :",
		  time.strftime("%H:%M:%S",total),
		  "="*15)

if __name__ == "__main__":
	##main()
	main_multiproc()
