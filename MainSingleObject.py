#!/usr/bin/env python
# -*- coding: utf-8 -*-
import time
import warnings
from Experiments import SingleObjectExperiment

def main():
	warnings.filterwarnings("ignore")
	explo_ratios = [.25,.375,.5,.625,.75]
	for i, explo_ratio in enumerate(explo_ratios):
		t = time.time()
		print("=" * 40)
		print("Starting runs for explo_ratio =", explo_ratio)
		condition = SingleObjectExperiment((1,10,8),(.1,explo_ratio),24,
										   24*i)
		results = condition.run_experiment()
		condition.output_data(results[0],
							  "Results/SingleObjectExperiment_explo" +\
							  str(explo_ratio))
		t = time.gmtime(time.time() - t)
		print("Condition finished in", time.strftime("%H:%M:%S",t))
	return results


if __name__ == "__main__":
	main()
