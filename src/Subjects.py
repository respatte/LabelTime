#!/usr/bin/env python
# -*- coding: utf-8 -*-
import copy as cp
import numpy as np

import BackPropNetworks as bpn

class Subject(object):
	"""Global subject class with methods common to all subject types.
	
	Input parameters:
		proto -- tuple of two prototypes of same size
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
		proto_stims -- tuple of two prototype stimuli of same size
			Those stimuli include both the input stimuli (object-specific)
			and the exploration of the stimuli (subject-specific).
			When implementing a CR model, the label part is cut off.
		proto_goals -- training goals for the network
			Same as full_stims, keeping the label for both Laf and CR.
		net -- backpropagation network used for learning
		impair -- target network for memory impairment
	
	Subject methods:
		encode_explo -- encode stimuli exploration given importance and overlap
		impair_memory -- impairs the memory, typically between training and test
		fam_training -- performs familiarisation trials as in T&W2017
	
	"""
	
	def __init__(self, proto, exploration, theory, l_size, h_ratio, lrn_rate,
				 momentum=None, model="DMN"):
		"""Initialise a simple labeltime subject for K&W2017.
		
		See class documentation for more details about parameters.
		
		"""
		# Create full stimuli
		# Create exploration if necessary
		if exploration[0]:
			explo_proto = self.encode_explo(exploration[0], exploration[1])
		else:
			# Create array of with no columns
			explo_proto = (np.zeros((1, 0)), np.zeros((1, 0)))
		full_proto = (np.hstack((proto[0], explo_proto[0])),
					  np.hstack((proto[1], explo_proto[1])))
		# Create proto_goals as copies of stimuli
		self.proto_goals = cp.deepcopy(full_proto)
		# Delete input label if CR model
		# theory gives number of label units if CR, 0 if LaF
		if theory == "CR":
			full_proto = (np.delete(full_proto[0], range(l_size), axis=1),
						  np.delete(full_proto[1], range(l_size), axis=1))
		self.proto_stims = full_proto
		# Create backpropagation network
		n_input = self.proto_stims[0].size
		n_output = self.proto_goals[0].size
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
	
	@property # Create self.impaired as an access-only property
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
		for trial in range(n_trials):
			looking_times_trial = []
			for stim in range(len(test_stims)):
				time_left = pres_time
				error = 1
				while time_left > 0 and error > threshold:
					# Goal specification necessarry for CR models
					self.net.run(test_stims[stim], test_goals[stim])
					error = np.linalg.norm(self.net.error)
					time_left -= 1
				looking_times_trial.append(pres_time - time_left)
			looking_times.append(looking_times_trial)
		return looking_times

class SingleObjectSubject(Subject):
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
		impair -- target network for memory impairment
	
	Subject methods:
		encode_explo -- encode stimuli exploration given importance and overlap
		bg_training -- trains network on stimuli before familiarisation trials
		fam_training -- performs familiarisation trials as in T&W2017
		impair_memory -- impairs the memory, typically between training and test
	
	"""

	def __init__(self, stims, exploration, theory, l_size, h_ratio, lrn_rate,
				 momentum=None, model="DMN"):
		"""Initialise a simple labeltime subject for K&W2017.
		
		See class documentation for more details about parameters.
		
		"""
		Subject.__init__(self, stims, exploration, theory, l_size, h_ratio,
						 lrn_rate, momentum, model)
		self.stims = self.proto_stims
		del self.proto_stims
		self.goals = self.proto_goals
		del self.proto_goals
	
	def bg_training(self, bg_parameters):
		"""Background training of the network on both simuli.
		
		bg_parameters is a tuple with values mu_t, sigma_t, mu_p, sigma_p.
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
		mu_t, sigma_t = bg_parameters
		n_steps = round(np.random.normal(mu_t, sigma_t))
		for step in range(n_steps):
			self.net.run(self.stims[0],self.goals[0])
			self.net.run(self.stims[1],self.goals[1])

class CategorySubject(Subject):
	"""Class computing a participant for the second labeltime study.
	
	In this class, the subject is trained with two sets of objects to
	reproduce the setup in Twomey & Westermann (in progress).
	
	Input parameters:
		proto -- tuple of two prototype of same size
			All object-specific information is encoded into the stimuli.
			This includes any overlap in visual/physical properties of
			the two stimuli.
			This also includes a label for one of the two stimuli (the
			absence of a label being coded as zeros). The label and its
			size in encoding are set beforehands, as it is not subject-
			specific.
		stims -- tuple of two stimulus lists (derived from the prototypes)
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
		n_stims -- number of stimuli in each category
		proto_stims -- tuple of two prototype stimuli of same size
			When implementing a CR model, the label part is cut off.
		stims -- tuple of two stimuli lists of same size
			When implementing a CR model, the label part is cut off.
		proto_goals, goals -- training goals for the network
			Same as proto_stims and stims, keeping the label for both theories.
		net -- backpropagation network used for learning
		impair -- target network for memory impairment
	
	Subject methods:
		encode_explo -- encode stimuli exploration given importance and overlap
		bg_training -- trains network on stimuli before familiarisation trials
		fam_training -- performs familiarisation trials as in T&W2017
		impair_memory -- impairs the memory, typically between training and test
	
	"""
	
	def __init__(self, proto, stims, theory, l_size, h_ratio, lrn_rate,
				 momentum, model="DMN"):
		"""Initialise a simple labeltime subject for K&W_inprogress.
		
		See class documentation for more details about parameters.
		
		"""
		Subject.__init__(self, proto, (0,0), theory, l_size, h_ratio,
						 lrn_rate, momentum, model)
		self.n_stims = len(stims[0])
		self.goals = stims
		if theory == "CR":
			self.stims = ([],[])
			for stim in range(self.n_stims):
				self.stims[0].append(np.delete(stims[0][stim],
											   range(l_size),
											   axis=1))
				self.stims[1].append(np.delete(stims[1][stim],
											   range(l_size),
											   axis=1))
		else:
			self.stims = stims
	
	def bg_training(self, bg_parameters):
		"""Background training of the network on both simuli.
		
		bg_parameters is a tuple with values n_days, mu_p, sigma_p.
		To mimic the experimental conditions, exposition time per object
		per session is not fixed but follows a Gaussian distribution.
		To further mimic the experimental conditions, the model is
		presented alternatively with one object then the other, for
		differing times.
		The whole set of stimuli from both categories is presented for
		n_days times, alternating between stimuli fron each category.

		Return a dictionary of hidden representations at specified
		time-steps, with structure as follows:
		- keys = step (last is at the end of training)
			- dict, keys = "LTM", "STM"
				- category sublists (0:labelled category, 1:unlabelled category)
					- list of hidden representations (ndarrays)
		
		"""
		mu_t, sigma_t, rec_epoch = bg_parameters
		n_steps = round(np.random.normal(mu_t, sigma_t))
		h_reps = {}
		for step in range(n_steps):
			if not (1+step) % rec_epoch or step==n_steps-1:
				h_reps[1+step] = {"LTM":[[],[]], "STM":[[],[]]}
			for stim in range(self.n_stims):
				self.net.run(self.stims[0][stim],self.goals[0][stim])
				if not (1+step) % rec_epoch or step==n_steps-1:
					h_reps[1+step]["LTM"][0].append(self.net.LTM.neurons[1])
					h_reps[1+step]["STM"][0].append(self.net.STM.neurons[1])
				self.net.run(self.stims[1][stim],self.goals[1][stim])
				if not (1+step) % rec_epoch or step==n_steps-1:
					h_reps[1+step]["LTM"][1].append(self.net.LTM.neurons[1])
					h_reps[1+step]["STM"][1].append(self.net.STM.neurons[1])
		return h_reps
