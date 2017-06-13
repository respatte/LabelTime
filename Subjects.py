#!/usr/bin/env python
# -*- coding: utf-8 -*-

import numpy as np

import BackPropNetworks as bpn

class SingleObjectSubject(object):
	"""Class computing a participant for the first label-time study.
	
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
		 type -- size of label if implementing CR theory, 0 if implementing LaF
	
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
