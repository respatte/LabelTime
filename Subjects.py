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
		theory -- theory implemented (CR or LaF)
		l_size -- label size
		h_ratio -- ratio of hidden neurons compared to input neurons
		lrn_rate -- learning rate(s) of the backpropagation network
		momentum -- influence of inertial term in [0, 1], or function
		model -- model used for neural network (BPN or DMN)
			If using DMN (DualMemoryNetwork), then learning rate and
			momentum must be given for both memories and lateral
			connections.
			See BackPropNetworks.DualMemoryNetwork documentation for more
			precision.
	
	Subject properties:
		stims -- tuple of two stimuli of same size
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
		impair_memory -- impairs the memory, typically between training and test
	
	"""

	def __init__(self, stims, exploration, theory, l_size, h_ratio, lrn_rate,
				 momentum=None, model="BPN"):
		"""Initialise a simple labeltime subject for K&W2017.
		
		See class documentation for more details about parameters.
		
		"""
		# Create full stimuli
		explo_stims = self.encode_explo(exploration[0], exploration[1])
		full_stims = (np.hstack((stims[0], explo_stims[0])),
					  np.hstack((stims[1], explo_stims[1])))
		# Create goals as copies of stimuli
		self.goals = cp.deepcopy(full_stims)
		# Delete input label if CR model
		# theory gives number of label units if CR, 0 if LaF
		if theory == "CR":
			full_stims = (np.delete(full_stims[0], range(l_size), axis=1),
						  np.delete(full_stims[1], range(l_size), axis=1))
		self.stims = full_stims
		# Create backpropagation network
		n_input = self.stims[0].size
		n_output = self.goals[0].size
		n_hidden = int(n_output * h_ratio)
		self.model = model
		if model == "BPN":
			if momentum:
				self.net = bpn.BackPropNetwork([n_input, n_hidden, n_output],
											   lrn_rate, momentum)
			else:
				self.net = bpn.BackPropNetwork([n_input, n_hidden, n_output],
											   lrn_rate)
		elif model == "DMN":
			# Compute layer sizes for STM (without label output)
			# Label deleted within DualMemoryNetwork model to fit goal to size
			n_input_STM = n_input
			n_output_STM = n_output - l_size
			n_hidden_STM = int(n_output_STM * h_ratio)
			# Create the network
			if momentum:
				self.net = bpn.DualMemoryNetwork([[n_input,
												   n_hidden,
												   n_output],
												  [n_input_STM,
												   n_hidden_STM,
												   n_output_STM]
												 ],
												 lrn_rate, momentum=momentum)
			else:
				self.net = bpn.DualMemoryNetwork([[n_input,
												   n_hidden,
												   n_output]] * 2,
												 lrn_rate)
	
	# Create impaired as an access-only property
	@property
	def impaired(self):
		"""Access the network to impair when needed"""
		if self.model == "BPN":
			return self.net
		elif self.model == "DMN":
			return self.net.STM

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
			n_overlap += 1
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
	
	def bg_training(self, mu_t, sigma_t, mu_p, sigma_p):
		"""Background training of the network on both simuli.
		
		To mimic the experimental conditions, total play time and play
		time per object are not strictly equal but follow a Gaussian
		distribution.
		To further mimic the experimental conditions, the model is
		presented alternatively with one object then the other, for
		differing times.
		The number of play sessions for each object is the same to avoid
		an overtraining of the last presented object. The total play
		time for each object is kept within a standard deviation from
		the mean total looking time to each object.
		
		"""
		# Computing alternating playing times
		play_times1 = []
		sum1 = 0
		play_times2 = []
		sum2 = 0
		while not (abs(sum1-mu_t) <= sigma_t and abs(sum2-mu_t) <= sigma_t):
			play_time1 = round(np.random.normal(mu_p, sigma_p))
			play_time2 = round(np.random.normal(mu_p, sigma_p))
			sum1 += play_time1
			sum2 += play_time2
			if sum1 > (mu_t + sigma_t) or sum2 > (mu_t + sigma_t):
				# One of the total times is too high
				play_times1 = [play_time1]
				sum1 = play_time1
				play_times2 = [play_time2]
				sum2 = play_time2
			else:
				play_times1.append(play_time1)
				play_times2.append(play_time2)
		# Computing "play" sessions
		for session in range(len(play_times1)):
			for t1 in range(play_times1[session]):
				self.net.run(self.stims[0],self.goals[0])
			for t2 in range(play_times2[session]):
				self.net.run(self.stims[1],self.goals[1])
	
	def fam_training(self, test_stims, test_goals,
					 pres_time, threshold, n_trials):
		"""Computes familiarisation training on test_stims.
		
		The model is presented with each stimulus, alternating, for
		n_trials number of trials. For each trial, the model is presented
		with each stiumulus until the network error reaches threshold
		or for pres_time backpropagations.
		stims is a set of two stimuli with values at zero for label and
		exploration units.
		
		Return a couple (looking_times, errors).
		looking_times is a list of the number of backpropagations per
		trial before reaching stopping criteria. For each trial, a tuple
		of number of backpropagations for each stimulus is recorded.
		errors is a list of the errors per trial and per stimulus. Each
		element of this list is a couple of two lists, one per stimulus.
		
		"""
		looking_times = []
		errors = []
		for trial in range(n_trials):
			errors_trial = []
			looking_times_trial = []
			for stim in range(len(test_stims)):
				errors_stim = []
				time_left = pres_time
				error = 1
				while time_left > 0 and error > threshold:
					# Goal specification necessarry for CR models
					self.net.run(test_stims[stim], test_goals[stim])
					error = np.linalg.norm(self.net.error)
					errors_stim.append(error)
					time_left -= 1
				errors_trial.append(errors_stim)
				looking_times_trial.append(pres_time - time_left)
			errors.append(errors_trial)
			looking_times.append(looking_times_trial)
		return (looking_times, errors)
	
	def impair_memory(self, connections, method=None, inertia=True):
		"""Impair subject's memory at given level of connections.
		
		Connections is a list of at least one connection index as
		described BackPropNetworks.
		Method is a string that will be evaluated. Result is added to
		selected weight matrix. A typical example is to use a uniform
		distribution to add noise to the weight matrix. In the method,
		parameters for size must be called m (rows) and n (columns),
		and are extracted from the selected weight matrix before the
		function is evaluated.
		If no method is given, reset connections using initialisation
		function from the network.
		For a dual memory network subject, only STM is affected.
		
		If inertia is set to False, then the inertia of the model is not
		erased. Default behaviour is to reinitialise inertia to zeros.
		
		Actions are performed in place, no return of the function.
		
		"""
		for c in connections:
			# Get shape from selected weight matrix
			# Since this information takes the bias weights into account,
			# we don't need to deal with presence/absence of bias here.
			m, n = self.impaired.weights[c].shape
			if method:
				# Add the evaluated method to the matrix
				self.impaired.weights[c] += eval(method)
			else:
				# Reinitialise the wmatrix
				self.impaired.weights[c] = self.net.init_weights_matrix(m, n,
																	 bias=False)
		if inertia:
			# Reset last inertia to zeros for all layers
			for layer in range(self.impaired.n_layers-1):
				self.impaired.inertia[0][layer] *= 0
			# Delete older inertia
			for _ in range(len(self.impaired.inertia)-1):
				self.impaired.inertia.pop()
				
