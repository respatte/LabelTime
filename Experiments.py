#!/usr/bin/env python
# -*- coding: utf-8 -*-
import numpy as np

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
	
	SignelObjectExperiment properties:
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
	
	"""
	
	def __init__(self, modality_sizes_stim, overlap_ratios, n_subjects=4096):
		"""Initialise a single-object labeltime experiment.
		
		See class documentation for more details about parameters.
		
		"""
		# Define some fixed parameters not treated as inputs
		self.mu_t, self.sigma_t = 1000, 100
		self.mu_p, self.sigma_p = 30, 5
		self.pres_time = 200
		self.threshold = .05
		self.n_trials = 8
		self.h_ratio = 19/24
		self.lrn_rate = .5
		self.momentum = .1
		# Get meaningful short variables from input
		# l_ -> label_
		# p_ -> physical
		# e_ -> exploration
		(self.l_size, self.p_size, self.e_size) = modality_sizes_stim
		(self.p_ratio, self.e_ratio) = overlap_ratios
		self.n_subjects = n_subjects
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
	
	def run_experiment(self):
		"""Run a full experiment.
		
		Return a dictionary of results, with subject number as key.
		Each subject's results is a table of results as described in
		SingleObjectSubject class.
		
		"""
		# Initialise result gatherer as a dictionary (subject number as key)
		results = {}
		# Start running subjects
		for s in range(self.n_subjects):
			# Code s_type (subject type) on 2 bits:
			# 	- labbeled item (0=first item labelled, 1=second item labelled)
			# 	- first familiarisation item (1=labelled, 0=unlabelled)
			s_type = format(s%4,'03b') # type: str
			# Add label to one stimulus, keep labelled first in couple
			labelled_i = int(s_type[0])
			label_stim = np.hstack((self.l_stims[1],
									self.p_stims[labelled_i]))
			no_label_stim = np.hstack((self.l_stims[0],
									   self.p_stims[labelled_i-1]))
			bg_stims = (label_stim, no_label_stim)
			# Create subjects for LaF and CR
			s_LaF = SingleObjectSubject(bg_stims, (self.e_size, self.e_ratio),
										0, self.h_ratio,
										self.lrn_rate, self.momentum)
			s_CR = SingleObjectSubject(bg_stims, (self.e_size, self.e_ratio),
									   self.l_size, self.h_ratio,
									   self.lrn_rate, self.momentum)
			# Perform background training on subjects
			s_LaF.bg_training(self.mu_t, self.sigma_t, self.mu_p, self.sigma_p)
			s_CR.bg_training(self.mu_t, self.sigma_t, self.mu_p, self.sigma_p)
			# Impair subject recovery memory (hidden to output weights)
			s_LaF.net.weights[-1] = np.random.normal(0, .5,
													s_LaF.net.weights[-1].shape)
			s_CR.net.weights[-1] = np.random.normal(0, .5,
													s_CR.net.weights[-1].shape)
			# Prepare stimuli order for familiarisation trials
			first_fam = int(s_type[1])
			first_stim = first_fam * labelled_i + \
						 (1 - first_fam) * (1 - labelled_i)
			LaF_stims = (self.test_stims[first_stim],
						 self.test_stims[1 - first_stim])
			LaF_goals = LaF_stims
			# Stimuli for CR: take LaF (already ordered), delete label units
			CR_stims = (np.delete(LaF_stims[0], range(self.l_size)),
						np.delete(LaF_stims[1], range(self.l_size)))
			CR_goals = LaF_goals
			# Run and record familiarisation training
			results[s] = s_LaF.fam_training(LaF_stims, LaF_goals,
											self.pres_time,
											self.threshold,
											self.n_trials)
			results[n_subjects+s] = s_CR.fam_training(CR_stims, CR_goals,
													  self.pres_time,
													  self.threshold,
													  self.n_trials)
		return results
		
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
			
