#!/usr/bin/env python
# -*- coding: utf-8 -*-
import numpy as np
from multiprocessing import Pool

from Subjects import *

class Experiment(object):
	"""Global class for labeltime experiments.
	
	Input parameters:
		modality_sizes_stim -- tuple of 3 values for stimulus modality sizes
			Values are given in this order: label_size, physical_size,
			exploration_size. They encode the number of units on which
			to encode each dimension of the stimuli.
		overlap_ratios -- tuple of two overlap ratio values in [0, 1]
			The first value is the overlap ratio for physical values of
			the stimuli. The second value is the overlap ratio in the
			exploration of the stimuli (passed on to each subject).
		n_subjects -- number of subjects to run in total per theory (LaF, CR)
			Is expected to be a multiple of 8, for counterbalancing purposes.
		start_subject -- the subject number for the first subject
			Used if the experiment is ran in multiple bashes.
	
	SingleObjectExperiment properties:
		pres_time -- max number of presentations at familiarisation
		threshold -- "looking away" threshold at familiarisation
		n_trials -- number of familiarisation trials
		h_ratio -- n_hidden_neurons / n_output_neurons ratio
		lrn_rate -- learning rate for the network
		momentum -- momentum parameter for the network
		l_size, p_size, e_size -- modlity sizes for label, physical, exploration
		p_ratio, e_ratio -- overlap ratios for physical and exploration values
		p_proto -- physical values for prototypes
		l_stims -- label values for stimuli
		test_stims -- full stimuli (label+physical+exploration) for test trials
	
	SingleObjectExperiment methods:
		run_experiment -- run a ful experiment, using only class properties
		generate_stims -- generate physical stimuli with overlap
		output_data -- convert results data to a csv file
	
	"""
	
	def __init__(self, modality_sizes_stim, overlap_ratios, n_subjects,
				 start_subject, pres_time, threshold, n_trials, h_ratio):
		"""Initialise a labeltime experiment.
		
		See class documentation for more details about parameters.
		
		"""
		self.pres_time = pres_time
		self.threshold = threshold
		self.n_trials = n_trials
		self.h_ratio = h_ratio
		# Theory list
		self.theories = ["LaF", "CR"]
		# Learning rates and momenta list
		self.lrn_rates = (.001, .1)
		self.momenta = (.0005, .05)
		# Get meaningful short variables from input
		# l_ -> label_
		# p_ -> physical
		# e_ -> exploration
		(self.l_size, self.p_size, self.e_size) = modality_sizes_stim
		(self.p_ratio, self.e_ratio) = overlap_ratios
		self.n_subjects = n_subjects
		self.start_subject = start_subject
		# Generate physical values for stimuli
		self.p_proto = self.generate_stims(self.p_size, self.p_ratio)
		# Generate (no_label, label) part to add to one or the other stimulus
		label = np.ones((1, self.l_size))
		no_label = np.zeros((1, self.l_size))
		self.l_stims = (no_label, label) # index on l_stims is presence of label
		# Generate test stimuli with zeros for label and exploration
		self.test_stims = (np.hstack((np.zeros((1, self.l_size)),
									  self.p_proto[0],
									  np.zeros((1, self.e_size))
									  )),
						   np.hstack((np.zeros((1, self.l_size)),
									  self.p_proto[1],
									  np.zeros((1, self.e_size))
									  ))
						   )
	
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
	
	def run_subject(self, subject_i, method):
		"""Run familiarisation for a single subject."""
		# Code s_type (subject type) on 4 bits:
		# 	- labbeled item (0=first item labelled, 1=second item labelled)
		# 	- first familiarisation item (1=labelled, 0=unlabelled)
		#	- theory (0=LaF, 1=CR)
		s_type = format(subject_i%8,'03b') # type: str
		# Get background stimuli
		bg_stims = self.create_subject_stims(s_type)
		# Create subjects
		s = self.create_subject(bg_stims, s_type)
		# Perform background training on subject
		t_results = s.bg_training(self.bg_parameters)
		t_result = s
		# Impair subject recovery memory (hidden to output)
		s.impair_memory([1], method)
		# Prepare stimuli order for familiarisation trials
		labelled_i = int(s_type[0])
		first_fam = int(s_type[1])
		first_stim = first_fam * labelled_i + \
					 (1 - first_fam) * (1 - labelled_i)
		test_stims = (self.test_stims[first_stim],
					  self.test_stims[1 - first_stim])
		test_goals = test_stims
		# Stimuli for CR: delete label units
		theory = int(s_type[2])
		if theory:
			test_stims = (np.delete(test_stims[0], range(self.l_size), axis=1),
						  np.delete(test_stims[1], range(self.l_size), axis=1))
		# Run and record familiarisation training
		f_result = s.fam_training(test_stims, test_goals,
								  self.pres_time,
								  self.threshold,
								  self.n_trials)
		# Adding a tuple with one value for exploration overlap ratio
		return (f_result, self.e_ratio), t_results
		
	def run_experiment(self, method=None):
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
				results_async[s] = pool.apply_async(self.run_subject,
													(s, method))
			pool.close()
			pool.join()
		f_results = {}
		t_results = {}
		for s in range(self.start_subject,
					   self.start_subject + self.n_subjects):
			f_results[s] = results_async[s].get()[0]
			t_results[s] = results_async[s].get()[1]
		return f_results, t_results
	
	def output_fam_data(data, filename):
		"""Write data from familiarisation into a filename.csv file.

		Class-wide (not instance-specific) method.
		
		data is a dictionary structured as follows:
		- keys = subject numbers
			- looking_times (number of backpropagations before threshold)
				- trial number
					- looking time to first stimulus
					- looking time to second stimulus
			- explo_ratio (exploration overlap ratio for subject)
		Number of trials is assumed to be fixed.
		Subject are ordered so that subject%4 in binary codes for
			- first value: labbeled item (0=first item, 1=second item)
			- second value: first familiarisation item (1=labelled,0=unlabelled)
		
		"""
		# Define column labels
		c_labels_LT = ','.join(["subject",
								"theory",
								"explo_overlap",
								"trial",
								"labelled",
								"looking_time"])
		rows_LT = [c_labels_LT]
		# Extract number of trials
		k = list(data.keys())[0]
		n_trials = len(data[k][0])
		# Prepare meaningful coding for parameters
		theories = ("LaF", "CR")
		labelled = ("no_label", "label")
		pres_time = 100
		for subject in data:
			# Extract information from subject number
			s_type = format(subject%8,'03b')
			theory = int(s_type[2])
			first_fam = int(s_type[1])
			for trial in range(n_trials):
				for stim in range(2):
					# Encode state for current stimulus
					labelled_stim = (stim + first_fam) % 2
					# Create row for looking time results
					row = [str(subject),
						   theories[theory],
						   str(data[subject][1]),
						   str(trial),
						   labelled[labelled_stim],
						   str(data[subject][0][trial][stim])
						   ]
					rows_LT.append(','.join(row))
		# Join all rows with line breaks
		data_LT = '\n'.join(rows_LT)
		# Write str results into two files with meaningful extensions
		with open(filename+"_LT.csv", 'w') as f:
			f.write(data_LT + "\n")

	def output_train_data(data, filename):
		"""Write data from training into a filename.csv file.

		Class-wide (not instance-specific) method.
		
		data is a dictionary structured as follows:
		- keys = subject numbers
			- hidden representations as returned per CategorySubject.bg_training
		Number of trials is assumed to be fixed.
		Subject are ordered so that subject%4 in binary codes for
			- first value: labbeled item (0=first item, 1=second item)
			- second value: first familiarisation item (1=labelled,0=unlabelled)
		
		"""
		# Create general column labels
		c_labels = ','.join(["subject",
							 "theory",
							 "step",
							 "labelled",
							 "exemplar"])
		# Get list of recorded steps for subject 0
		k = list(data.keys())[0]
		steps = list(data[k].keys())
		# Get dimension of hidden representations for LTM/STM
		dims_LTM = data[0][steps[0]]["LTM"][0][0].size
		dims_STM = data[0][steps[0]]["STM"][0][0].size
		# Create dimension column names
		dims_labels_LTM = ["dim" + str(i) for i in range(dims_LTM)]
		dims_labels_STM = ["dim" + str(i) for i in range(dims_STM)]
		# Create final column labels
		rows_LTM = [','.join([c_labels] + dims_labels_LTM)]
		rows_STM = [','.join([c_labels] + dims_labels_STM)]
		# Prepare meaningful coding for parameters
		theories = ("LaF", "CR")
		labelled = ("label", "no_label")
		for subject in data:
			# Extract information from subject number
			s_type = format(subject%8,'03b')
			theory = int(s_type[2])
			max_step = max(data[subject])
			for step in data[subject]:
				for category in range(2):
					for exemplar in range(4):
						# Create row for hidden representation results
						glob = [str(subject),
								theories[theory],
								str(step * (step<max_step)),
								labelled[category],
								str(exemplar + 4*category),
								]
						LTM = [str(data[subject][step]["LTM"]\
									   [category][exemplar][0,j])
							   for j in range(dims_LTM)]
						STM = [str(data[subject][step]["STM"]\
									   [category][exemplar][0,j])
							   for j in range(dims_STM)]
						rows_LTM.append(','.join(glob + LTM))
						rows_STM.append(','.join(glob + STM))
		# Join all rows with line breaks
		data_LTM = '\n'.join(rows_LTM)
		data_STM = '\n'.join(rows_STM)
		# Write str results into two files with meaningful extensions
		with open(filename+"_hidden_LTM.csv", 'w') as f:
			f.write(data_LTM + "\n")
		with open(filename+"_hidden_STM.csv", 'w') as f:
			f.write(data_STM + "\n")
	
class SingleObjectExperiment(Experiment):
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
		create_subject_stims -- create stimuli for background training
		output_data -- convert results data to a csv file
	
	"""
	
	def __init__(self, modality_sizes_stim, overlap_ratios, n_subjects,
				 start_subject, theta_t=(500, 50), pres_time=10, pps=4,
				 threshold=1e-3, n_trials=8, h_ratio=19/24):
		"""Initialise a single-object labeltime experiment.
		
		See class documentation for more details about parameters.
		
		"""
		Experiment.__init__(self, modality_sizes_stim, overlap_ratios,
							n_subjects, start_subject, pres_time*pps, threshold,
							n_trials, h_ratio)
		mu_t, sigma_t = theta_t[0]*pps, theta_t[1]*pps
		self.bg_parameters = mu_t, sigma_t
		self.p_stims = self.p_proto
		del self.p_proto
	
	def create_subject_stims(self, s_type):
		"""Create stimuli for the subject depending on subject number."""
		# Add label to one stimulus, keep labelled first in couple
		labelled_i = int(s_type[0])
		label_stim = np.hstack((self.l_stims[1],
								self.p_stims[labelled_i]))
		no_label_stim = np.hstack((self.l_stims[0],
								   self.p_stims[1-labelled_i]))
		return (label_stim, no_label_stim)
	
	def create_subject(self, bg_stims, s_type):
		"""Create subject for experiment depending on subject number."""
		theory = int(s_type[2])
		s = SingleObjectSubject(bg_stims, (self.e_size, self.e_ratio),
								self.theories[theory], self.l_size,
								self.h_ratio,
								self.lrn_rates, self.momenta)
		return s

class CategoryExperiment(Experiment):
	"""Class computing a full labeltime experiment with categories.
	
	Input parameters:
		modality_sizes_stim -- tuple of 3 values for stimulus modality sizes
			Values are given in this order: label_size, physical_size,
			exploration_size. They encode the number of units on which
			to encode each dimension of the stimuli.
		overlap_ratios -- tuple of two overlap ratio values in [0, 1]
			The first value is the overlap ratio for physical values of
			the stimuli. The second value is the overlap ratio in the
			exploration of the stimuli (passed on to each subject).
		n_exemplars -- number of exemplars to generate per category
		n_subjects -- number of subjects to run in total per model (LaF, CR)
			Is expected to be a multiple of 16, for counterbalancing purposes.
		start_subject -- the subject number for the first subject
			Used if the experiment is ran in multiple bashes.
		cat_method -- method used to create a category
			Values can be either 'continuous' to add continuous noise to
			prototypes to create a category, or 'discrete' to add discrete
			noise (i.e. a bits are changed from 0 to 1 and vice versa).
	
	CategoryExperiment properties:
		mu_p, sigma_p -- background play session time distribution
		n_days -- number of days of background training
		pres_time -- max number of presentations at familiarisation
		threshold -- "looking away" threshold at familiarisation
		n_trials -- number of familiarisation trials
		h_ratio -- n_hidden_neurons / n_output_neurons ratio
		lrn_rate -- learning rate for the network
		momentum -- momentum parameter for the network
		l_size, p_size, e_size -- modlity sizes for label, physical, exploration
		p_ratio, e_ratio -- overlap ratios for physical and exploration values
		p_proto -- physical values for prototypes
		l_stims -- label values for stimuli
		test_stims -- full stimuli (label+physical+exploration) for test trials
	
	CategoryExperiment methods:
		run_experiment -- run a ful experiment, using only class properties
		generate_stims -- generate physical stimuli with overlap
		create_subject_stims -- create stimuli for background training
		output_data -- convert results data to a csv file
	
	"""
	
	def __init__(self, modality_sizes_stim, overlap_ratios, n_subjects,
				 start_subject, theta_t=(500, 50) , pres_time=10, pps=4,
				 rec_epoch=100, threshold=1e-3, n_trials=8, h_ratio=19/24,
				 n_exemplars=4, cat_method="continuous"):
		"""Initialise a single-object labeltime experiment.
		
		See class documentation for more details about parameters.
		
		"""
		Experiment.__init__(self, modality_sizes_stim, overlap_ratios,
							n_subjects, start_subject, pres_time*pps, threshold,
							n_trials, h_ratio)
		mu_t, sigma_t = theta_t[0]*pps, theta_t[1]*pps
		self.bg_parameters = mu_t, sigma_t, rec_epoch
		self.cat_method = cat_method
		self.n_exemplars = n_exemplars
		self.categories = [self.generate_category(self.p_proto[0],
												  self.n_exemplars,
												  self.cat_method),
						   self.generate_category(self.p_proto[1],
												  self.n_exemplars,
												  self.cat_method)]

	def create_subject_stims(self, s_type):
		"""Create stimuli for the subject depending on subject number."""
		# Add label to one stimulus, keep labelled first in couple
		labelled_i = int(s_type[0])
		# Add label
		l_stims = []
		no_l_stims = []
		for stim in range(self.n_exemplars):
			l_stims.append(np.hstack((self.l_stims[1],
									  self.categories[labelled_i][stim])))
			no_l_stims.append(np.hstack((self.l_stims[0],
										 self.categories[1-labelled_i][stim])))
		return (l_stims, no_l_stims)
	
	def create_subject(self, bg_stims, s_type):
		"""Create subject for experiment depending on subject number."""
		theory = int(s_type[2])
		# Use test_stims instead of p_proto as label is already included
		# (important for size only, not used in learning)
		s = CategorySubject(self.test_stims, bg_stims, self.theories[theory],
							self.l_size, self.h_ratio, self.lrn_rates,
							self.momenta)
		return s
	
	def generate_category(self, prototype, n_exemplars, cat_method):
		"""Generate a category around a prototype."""
		n = 0
		steps = 0
		exemplars = [prototype] # Initialise exemplars list with prototype
		while n < n_exemplars and steps < 100000:
			steps += 1
			if cat_method == "continuous":
				new_exemplar = prototype + np.random.uniform(-.5, .5,
															 prototype.shape)
			for exemplar in exemplars:
				if np.linalg.norm(exemplar - new_exemplar) < 1:
					new_exemplar = None
					break
			if new_exemplar is not None:
				exemplars.append(new_exemplar)
				n += 1
		if steps == 100000:
			raise(NotImplementedError)
		return exemplars[1:] # Return all exemplars but first (prototype)
