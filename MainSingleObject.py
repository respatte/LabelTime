#!/usr/bin/env python
# -*- coding: utf-8 -*-
import time
import warnings
from multiprocessing import Process, Queue

from Experiments import SingleObjectExperiment

def run_subjects(n_subjects, explo_ratio, bash_i, q=None):
	t = time.time()
	print("=" * 40)
	print("Starting runs for explo_ratio =", explo_ratio)
	condition = SingleObjectExperiment((1,10,8), (.1,explo_ratio),
									   n_subjects, n_subjects*bash_i)
	results = condition.run_experiment()
	t = time.gmtime(time.time() - t)
	print("Condition finished in", time.strftime("%H:%M:%S",t))
	if q:
		q.put(results)
	return results

def main():
	warnings.filterwarnings("ignore")
	explo_ratios = [.25,.375,.5,.625,.75]
	t_results = []
	f_results = {}
	for i, explo_ratio in enumerate(explo_ratios):
		results = run_subjects(24, explo_ratio, i)
		t_results.append(results[1])
		f_results.update(results[0])
	SingleObjectExperiment.output_data(f_results,"Results/SingleObject")

def main_multiproc():
	warnings.filterwarnings("ignore")
	explo_ratios = [.25,.375,.5,.625,.75]
	results = Queue()


if __name__ == "__main__":
	main()
