##############################################################################
# (c) Crown copyright 2025 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################

DSL := transmute

# Find the specific files we wish to preprcoess and psyclone from physics source
# Set our target dependency to the version of the file we are to generate after
# the psycloning step.
#
SOURCE_F_FILES := $(foreach THE_FILE, $(PSYCLONE_PHYSICS_FILES), $(patsubst $(SOURCE_DIR)/%.xu90, $(SOURCE_DIR)/%.F90, $(shell find $(SOURCE_DIR) -name '$(THE_FILE).xu90' -print)))

# Default make target for file
#
.PHONY: psyclone

# Call this target, expect these files to be done first
#
psyclone: $(SOURCE_F_FILES)

# Psyclone files back into F90 files.
# Technically as they are preprocessed and psycloned, they should be f90 files,
# however the analysis step broke and so needs some work. For now they will be F90.

# Where an optimisation script exists for a specific file, use it.
#
$(SOURCE_DIR)/%.F90: $(SOURCE_DIR)/%.xu90 $(OPTIMISATION_PATH)/$(DSL)/%.py
	echo Psyclone with file override script $(OPTIMISATION_PATH_PSY)/$(DSL)/$*.py
	PYTHONPATH=$(LFRIC_BUILD)/psyclone:$$PYTHONPATH psyclone \
			-l all \
			-s $(OPTIMISATION_PATH_PSY)/$(DSL)/$*.py \
			-o $(SOURCE_DIR)/$*.F90 \
			$<

# Where a global optimisation script exists, use it.
#
$(SOURCE_DIR)/%.F90: $(SOURCE_DIR)/%.xu90 $(OPTIMISATION_PATH)/$(DSL)/global.py
	echo Psyclone with global script $(OPTIMISATION_PATH_PSY)/$(DSL)/global.py
	PYTHONPATH=$(LFRIC_BUILD)/psyclone:$$PYTHONPATH psyclone \
			-l all \
			-s $(OPTIMISATION_PATH_PSY)/$(DSL)/global.py \
			-o $(SOURCE_DIR)/$*.F90 \
			$<

# Where no optimisation script exists, don't use it.
#
$(SOURCE_DIR)/%.F90: $(SOURCE_DIR)/%.xu90
	echo "Psyclone pass with no optimisation applied, OMP removed"
	PYTHONPATH=$(LFRIC_BUILD)/psyclone:$$PYTHONPATH psyclone \
			-l all \
			-o $(SOURCE_DIR)/$*.F90 \
			$<