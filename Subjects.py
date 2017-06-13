#!/usr/bin/env python
# -*- coding: utf-8 -*-
import copy as cp

import numpy as np

import BackPropNetworks as bpn

class SingleObjectSubject(object):
	"""Class computing a participant for the first labeltime study.
	
	In this class, the subject is trained with two single objects to
	reproduce the setup in Twomey & Westermann (2017).
	
	Input parameters:
		stims -- tuple of two stimuli of same size
			All object-specific information is encoded into the stimuli.
			This includes any overlap in visual/physical properties of
			the two stimuli.
			This also includes a label for one of the two stimuli (the
			absence of a label being coded as zeros). The label and its
			size in encoding are set beforehands, as it is not subject-
			specific. 
		exploration -- tuple of values for exploration importance and overlap
			The first value of the tuple defines the number of units on
			which exploration will be encoded. A bigger number of units
			means a richer exploration of the stimulus by the subject.
			The second value is a ratio (in [0, 1]), and defines the
			overlap in the encoding of exploration between the two
			stimuli, i.e. the between-stimulus similarity amongst haptic
			and interaction dimensions. This is subject-specific.
		 m_type -- size of label if implementing CR, 0 if implementing LaF
		 h_ratio -- ratio of hidden neurons compared to input neurons
		 lrn_rate -- learning rate of the backpropagation network
	
	Subject properties:
		full_stims -- tuple of two stimuli of same size
			Those stimuli include both the input stimuli (object-specific)
			and the exploration of the stimuli (subject-specific).
			When implementing a CR model, the label part is cut off.
		goals -- training goals for the network
			Same as full_stims, keeping the label for both Laf and CR.
		net -- backpropagation network used for learning
	
	Subject methods:
		encode_explo -- encode stimuli exploration given importance and overlap
		bg_training -- trains network on stimuli before familiarisation trials
		fam_training -- performs familiarisation trials as in T&W2017
	
	"""

	def __init__(self, stims, exploration, m_type, h_ratio, lrn_rate):
		"""Initialise a simple labeltime subject for K&W2017.
		
		See class documentation for more details about parameters.
		
		"""
		# Create full stimuli
		explo_stims = self.encode_explo(exploration[0], exploration[1])
		self.full_stims = (np.hstack((stims[0], explo_stims[0])),
						   np.hstack((stims[1], explo_stims[1])))
		# Create goals as copies of stimuli
		self.goals = cp.deepcopy(full_stims)
		# Delete input label if CR model
		# m_type gives number of label units if CR, 0 if LaF
		if m_type > 0:
			self.full_stims = (np.delete(full_stims[0], range(m_type),axis=1),
							   np.delete(full_stims[1], range(m_type),axis=1))
		# Create backpropagation network
		n_input = len(self.full_stims[0])
		n_hidden = int(n_input * h_ratio)
		n_output = len(self.goals[0])
		self.net = bpn.BackPropNetwork([n_input,n_hidden,n_output], lrn_rate)

	def encode_explo(self, n_explo, ratio):
		"""Encodes exploration of two stimuli with overlapping.

		The number of overlapping units is rounded so that the number of
		remaining units is even, which is a mathematical requirement.
		"""
		# Initialise exploration as ones
		explo1 = np.ones((1,n_explo))
		explo2 = np.ones((1,n_explo))
		# Computes number of overlapping units
		n_overlap = int(n_explo * ratio)
		if (n_explo - n_overlap) % 2:
			n_explo += 1
		n_diff = n_explo - n_overlap
		# Select which indices to change to zero for each explo
		# First create a list of indices
		i_total = np.arange(n_explo)
		# Only keep indices with no overlap
		i_diff = np.random.choice(i_total, size=n_diff, replace=False)
		# Select half of the remaining indices for explo1
		i_explo1 = np.random.choice(i_diff, size=n_diff//2, replace=False)
		# Builds indices for explo2 as indices from i_diff not in i_explo1
		i_explo2 = np.setdiff1d(i_diff, i_explo1, assume_unique=True)
		# Set selected values to zero in explo1 and explo2
		explo1[0, i_explo1] = 0
		explo2[0, i_explo2] = 0
		return (explo1,explo2)
