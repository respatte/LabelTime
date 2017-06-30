#!/usr/bin/env python
# -*- coding: utf-8 -*-
import time
from Experiments import SingleObjectExperiment

def main():
	explo_ratios = [.25,.375,.5,.625,.75]
	for explo_ratio in explo_ratios:
		t = time.time()
		print("=" * 40)
		print("Starting runs for explo_ratio =", explo_ratio)
		condition = SingleObjectExperiment((1,10,8),(.1,explo_ratio),1024)
		results = condition.run_experiment()
		conidtion.output_data(results,
							  "SingleObjectExperiment_explo" + str(explo_ratio))
		t = time.gmtime(time.time() - t)
		print("Condition finished in", time.strftime("%H:%M:%S",t))


if __name__ == "__main__":
	main()
