#!/usr/bin/env python
# -*- coding: utf-8 -*-
import numpy as np

from Subjects import *

def labeltime_experiment(modality_sizes_stim, overlap_ratios, n_subjects=4096):
	"""Run a full labeltime experiment, return raw results for all subjects.
	
	Creates stims according to given modality_sizes_stim; this input is
	a tuple of values (label_size, physical_size, exploration_size).
	Overlap between the two stimuli for different modalities is given by
	overlap_ratios = (physical_overlap_ratio, exploration_overlap_ratio).
	Runs n_subjects per theory implemented (LaF vs. CR).
	Labelled item and first item for familiarisation is counterbalanced.
	
	Return a dictionary of results, with subject number as key.
	Each subject's results is a table of results as described in
	SingleObjectSubject class.
	
	"""
	# Define all non-input variables
	mu_t, sigma_t = 1000,100	# Total background training time distribution
	mu_p, sigma_p = 30, 5		# Background play session time distribution
	pres_time = 200				# Max number of presentations at familiarisation
	threshold = .05				# "Looking away" threshold at familiarisation
	n_trials = 8				# Number of familiarisation trials
	h_ratio = 19/24				# (n_hidden_neurons)/(n_output_neurons)
	lrn_rate = .5				# Learning rate for the network
	momentum = .1				# Momentum parameter for the network
	# Get meaningful short variables from input
	# l_ -> label_
	# p_ -> physical
	# e_ -> exploration
	(l_size, p_size, e_size) = modality_sizes_stim
	(p_ratio, e_ratio) = overlap_ratios
	# Generate physical values for stimuli
	p_stims = generate_stims(p_size, p_ratio)
	# Initialise result gatherer as a dictionary (with subject number as key)
	results = {}
	# Start running subjects
	for s in range(n_subjects):
		# Code s_type (subject type) on 2 bits:
		# 	- labbeled item (0=first item labelled, 1=second item labelled)
		# 	- first familiarisation item (0=labelled, 1=unlabelled)
		s_type = format(s%4,'03b') # type: str
		# Add label to one of the stimuli
		label = np.ones((1,l_size))
		labelled_i = int(s_type[0])
		stim1 = np.hstack((label, p_stims[labelled_i]))
		stim2 = np.hstack((0*label, p_stims[labbeled_i-1]))
		# Keep labelled stimulus as first in the couple
		bg_stims = (stim1, stim2)
		# Create subjects for LaF and CR
		s_LaF = SingleObjectSubject(bg_stims, (e_size, e_ratio), 0,
									h_ratio, lrn_rate, momentum)
		s_CR = SingleObjectSubject(bg_stims, (e_size, e_ratio), l_size,
								   h_ratio, lrn_rate, momentum)
		# Perform background training on subjects
		s_LaF.bg_training(mu_t, sigma_t, mu_p, sigma_p)
		s_CR.bg_training(mu_t, sigma_t, mu_p, sigma_p)
		# Impair subject recovery memory (hidden to output weights)
		s_LaF.net.weights[-1] = np.random.normal(0, .5,
												 s_LaF.net.weights[-1].shape)
		s_CR.net.weights[-1] = np.random.normal(0, .5,
												s_CR.net.weights[-1].shape)
		# Create test stimuli as good size with zeros for label and exploration
		# Stimuli for LaF first: keep label units, set to zero
		test_stims = bg_stims
		test_stims[0][0, range(l_size)] = 0
		first_fam = int(s_type[1]) # Already constructing with good stim first
		LaF_stims = (np.hstack((test_stim[first_fam], np.zeros((1, e_size))))
					 np.hstack((test_stim[first_fam-1], np.zeros((1, e_size)))))
		LaF_goals = LaF_stims
		# Stimuli for CR: take LaF (already order), delete label units
		CR_stims = (np.delete(LaF_stims[0],range(l_size)),
					np.delete(LaF_stims[1],range(l_size)))
		CR_goals = LaF_goals
		# Run and record familiarisation training
		results[s] = s_LaF.fam_training(LaF_stims, LaF_goals, pres_time,
										threshold, n_trials)
		results[n_subjects+s] = s_CR.fam_training(CR_stims, CR_goals,
												  pres_time, threshold,
												  n_trials)
	return results
