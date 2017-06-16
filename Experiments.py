#!/usr/bin/env python
# -*- coding: utf-8 -*-
import numpy as np

def labeltime_experiment(modality_sizes_stim, overlap_ratios, n_subjects=1e4):
	"""Run a full labeltime experiment, returns raw results for all subjects.
	
	Creates stims according to given modality_sizes_stim; this input is
	a tuple of values (label_size, physical_size, exploration_size).
	Overlap between the two stimuli for different modalities is given by
	overlap_ratios = (physical_overlap_ratio, exploration_overlap_ratio).
	Runs n_subjects per condition (label vs. silent) and per theory
	implemented (LaF vs. CR).
	Labelled item and first item for familiarisation is counterbalanced.
	
	"""
