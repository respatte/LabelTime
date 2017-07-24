#!/usr/bin/env python
# -*- coding: utf-8 -*-
import numpy as np
from multiprocessing import Pool

from Subjects import *

class SingleObjectExperiment(object):
	"""Class computing a full labeltime experiment with single objects.
	
	Input parameters:
		modality_sizes_stim -- tuple of 3 values for stimulus modality sizes
			Values are given in this order: label_size, physical_size,
			exploration_size. They encode the number of units on which
			to encode each dimension of the stimuli.
		overlap_ratios -- tuple of two overlap ratio values in [0, 1]
			The first value is the overlap ratio for physical values of
			the stimuli. The second value is the overlap ratio in the
			exploration of the stimuli (passed on to each subject).
		n_subjects -- number of subjects to run in total per model (LaF, CR)
			Is expected to be a multiple of 16, for counterbalancing purposes.
		start_subject -- the subject number for the first subject
			Used if the experiment is ran in multiple bashes.
		model -- model used for neural network (BPN or DMN)
	
	SingleObjectExperiment properties:
		mu_t, sigma_t -- total background training time distribution
		mu_p, sigma_p -- background play session time distribution
		pres_time -- max number of presentations at familiarisation
		threshold -- "looking away" threshold at familiarisation
		n_trials -- number of familiarisation trials
		h_ratio -- n_hidden_neurons / n_output_neurons ratio
		lrn_rate -- learning rate for the network
		momentum -- momentum parameter for the network
		l_size, p_size, e_size -- modlity sizes for label, physical, exploration
		p_ratio, e_ratio -- overlap ratios for physical and exploration values
		p_stims -- physical values for stimuli
		l_stims -- label values for stimuli
		test_stims -- full stimuli (label+physical+exploration) for test trials
	
	SingleObjectExperiment methods:
		run_experiment -- run a ful experiment, using only class properties
		generate_stims -- generate physical stimuli with overlap
		output_data -- convert results data to a csv file
	
	"""
	
	def __init__(self, modality_sizes_stim, overlap_ratios,
				 n_subjects=4096, start_subject=0,
				 theta_t=(10500, 100), theta_p=(1500, 50),
				 pres_time=100, threshold=1e-3, n_trials=8,
				 h_ratio=19/24):
		"""Initialise a single-object labeltime experiment.
		
		See class documentation for more details about parameters.
		
		"""
		self.mu_t, self.sigma_t = theta_t
		self.mu_p, self.sigma_p = theta_p
		self.pres_time = pres_time
		self.threshold = threshold
		self.n_trials = n_trials
		self.h_ratio = h_ratio
		# Model list, learning rates and momenta list for each model
		self.models = ["BPN","DMN"]
		self.lrn_rates = [.1, (.001, .1)]
		self.momenta = [.05, (.0005, .05)]
		# Get meaningful short variables from input
		# l_ -> label_
		# p_ -> physical
		# e_ -> exploration
		(self.l_size, self.p_size, self.e_size) = modality_sizes_stim
		(self.p_ratio, self.e_ratio) = overlap_ratios
		self.n_subjects = n_subjects
		self.start_subject = start_subject
		# Generate physical values for stimuli
		self.p_stims = self.generate_stims(self.p_size, self.p_ratio)
		# Generate (no_label, label) part to add to one or the other stimulus
		label = np.ones((1, self.l_size))
		no_label = np.zeros((1, self.l_size))
		self.l_stims = (no_label, label) # index on l_stims is presence of label
		# Generate test stimuli with zeros for label and exploration
		self.test_stims = (np.hstack((np.zeros((1, self.l_size)),
									  self.p_stims[0],
									  np.zeros((1, self.e_size))
									  )),
						   np.hstack((np.zeros((1, self.l_size)),
									  self.p_stims[1],
									  np.zeros((1, self.e_size))
									  ))
						   )
	
	def run_subject(self, subject_i):
		"""Run a single subject with parameters depending on subject number."""
		# Code s_type (subject type) on 4 bits:
		# 	- labbeled item (0=first item labelled, 1=second item labelled)
		# 	- first familiarisation item (1=labelled, 0=unlabelled)
		#	- theory (0=LaF, 1=CR)
		#	- model (0=BPN, 1=DMN)
		s_type = format(subject_i%16,'04b') # type: str
		# Add label to one stimulus, keep labelled first in couple
		labelled_i = int(s_type[0])
		label_stim = np.hstack((self.l_stims[1],
								self.p_stims[labelled_i]))
		no_label_stim = np.hstack((self.l_stims[0],
								   self.p_stims[1-labelled_i]))
		bg_stims = (label_stim, no_label_stim)
		# Create subjects
		theory = int(s_type[2])
		model = int(s_type[3])
		s = SingleObjectSubject(bg_stims, (self.e_size, self.e_ratio),
								theory * self.l_size, self.h_ratio,
								self.lrn_rates[model], self.momenta[model],
								self.models[model])
		# Perform background training on subject
		s.bg_training(self.mu_t, self.sigma_t, self.mu_p, self.sigma_p)
		t_result = s
		# Impair subject recovery memory (hidden to output)
		method = "np.random.uniform(.1, .5, (m,n)) * "
		method += "(2 * np.random.binomial(1, .5, (m,n)) - 1)"
		s.impair_memory([1], method)
		# Prepare stimuli order for familiarisation trials
		first_fam = int(s_type[1])
		first_stim = first_fam * labelled_i + \
					 (1 - first_fam) * (1 - labelled_i)
		test_stims = (self.test_stims[first_stim],
					  self.test_stims[1 - first_stim])
		test_goals = test_stims
		# Stimuli for CR: delete label units
		if theory:
			test_stims = (np.delete(test_stims[0], range(self.l_size), axis=1),
						  np.delete(test_stims[1], range(self.l_size), axis=1))
		# Run and record familiarisation training
		f_result = s.fam_training(test_stims, test_goals,
								  self.pres_time,
								  self.threshold,
								  self.n_trials)
		# Adding a tuple with one value for exploration overlap ratio
		f_result += (self.e_ratio,)
		return (f_result, t_result)
		
	def run_experiment(self):
		"""Run a full experiment.
		
		Return a tuple of results for familiarisation and training. Results
		are recorded in dictionaries, with subject number as key.
		Each subject's familiarisation results is a tuple of results as
		described in SingleObjectSubject class, with an extra value for
		exploration overlap appended to it.
		Each subject's training results is a the subject itself after
		background training. This allows us to find any information we want.
		
		"""
		# Initialise result gatherer as a dictionary (subject number as key)
		results_async = {}
		# Start running subjects
		with Pool() as pool:
			for s in range(self.start_subject,
						   self.start_subject + self.n_subjects):
				results_async[s] = pool.apply_async(self.run_subject, (s,))
			pool.close()
			pool.join()
		f_results = {}
		t_results = {}
		for s in range(self.start_subject,
					   self.start_subject + self.n_subjects):
			tmp_results = results_async[s].get()
			f_results[s] = tmp_results[0]
			t_results[s] = tmp_results[1]
		return (f_results, t_results)
		
	def generate_stims(self, size, ratio):
		"""Generate two stims of given size with given overlap ratio."""
		# Initialise stims as ones
		stim1 = np.ones((1, size))
		stim2 = np.ones((1, size))
		# Computes number of overlapping units
		n_overlap = int(ratio * size)
		if (size - n_overlap) % 2:
			n_overlap += 1
		n_diff = size - n_overlap
		# Select which indices to change to zero for each explo
		# First create a list of indices
		i_total = np.arange(size)
		# Only keep indices with no overlap
		i_diff = np.random.choice(i_total, size=n_diff, replace=False)
		# Select half of the remaining indices for stim1
		i_stim1 = np.random.choice(i_diff, size=n_diff//2, replace=False)
		# Builds indices for explo2 as indices from i_diff not in i_stim1
		i_stim2 = np.setdiff1d(i_diff, i_stim1, assume_unique=True)
		# Set selected values to zero in stim1 and stim2
		stim1[0, i_stim1] = 0
		stim2[0, i_stim2] = 0
		return (stim1,stim2)
	
	def output_data(data, filename):
		"""Write data from the experiment into a filename.csv file.

		Class-wide (not instance-specific) method.
		
		data is a dictionary structured as follows:
		- keys = subject numbers
			- looking_times (number of backpropagations before threshold)
				- trial number
					- looking time to first stimulus
					- looking time to second stimulus
			- errors (error of the model)
				- trial number
					- errors for first stimulus
						(list of length looking time to first stimulus)
					- errors for second stimulus
						(list of length looking time to second stimulus)
			- explo_ratio (exploration overlap ratio for subject)
		Number of trials is assumed to be fixed.
		Subject are ordered so that subject%4 in binary codes for
			- first value: labbeled item (0=first item, 1=second item)
			- second value: first familiarisation item (1=labelled,0=unlabelled)
		
		"""
		# Define column labels
		c_labels_LT = ','.join(["subject",
								"theory",
								"model",
								"explo_overlap",
								"trial",
								"labelled",
								"looking_time"])
		c_labels_errors = ','.join(["subject",
									"theory",
									"model",
									"explo_overlap",
									"trial",
									"labelled",
									"i_presentation",
									"error"])
		rows_LT = [c_labels_LT]
		rows_errors = [c_labels_errors]
		# Extract number of trials
		k = list(data.keys())[0]
		n_trials = len(data[k][0])
		# Prepare meaningful coding for parameters
		theories = ("LaF", "CR")
		models = ("BPN", "DMN")
		labelled = ("no_label", "label")
		pres_time = 100
		for subject in data:
			# Extract information from subject number
			s_type = format(subject%16,'04b')
			theory = int(s_type[2])
			model = int(s_type[3])
			first_fam = int(s_type[1])
			for trial in range(n_trials):
				for stim in range(2):
					# Encode state for current stimulus
					labelled_stim = (stim + first_fam) % 2
					# Create row for looking time results
					row = [str(subject),
						   theories[theory],
						   models[model],
						   str(data[subject][2]),
						   str(trial),
						   labelled[labelled_stim],
						   str(data[subject][0][trial][stim])
						   ]
					rows_LT.append(','.join(row))
					for pres in range(pres_time):
						# Get error or NA if subject "looked away"
						try:
							res = data[subject][1][trial][stim][pres]
						except IndexError:
							res = "NA"
						# Create row for error results
						row = [str(subject),
							   theories[theory],
							   models[model],
							   str(data[subject][2]),
							   str(trial),
							   labelled[labelled_stim],
							   str(pres),
							   str(res)
							   ]
						rows_errors.append(','.join(row))
		# Join all rows with line breaks
		data_LT = '\n'.join(rows_LT)
		data_errors = '\n'.join(rows_errors)
		# Write str results into two files with meaningful extensions
		with open(filename+"_LT.csv", 'w') as f:
			f.write(data_LT + "\n")
		with open(filename+"_errors.csv", 'w') as f:
			f.write(data_errors + "\n")
