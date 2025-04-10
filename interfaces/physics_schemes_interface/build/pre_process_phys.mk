##############################################################################
# (c) Crown copyright 2025 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################

# Build a set of "-D" argument for any pre-processor macros from core
#
MACRO_ARGS := $(addprefix -D,$(PRE_PROCESS_MACROS))

# Find the specific files we wish to pre prcoess and psyclone from physics source
# Set our target dependency to the version of the file we are to generate after
# The preprocessing step. #
# .xu90 files are to represent preprocessed source, bound for psyclone,
# but are not psykal files, denoted by .x90
#
SOURCE_xu_FILES := $(foreach THE_FILE, $(PSYCLONE_PHYSICS_FILES), $(patsubst $(SOURCE_DIR)/%.F90, $(SOURCE_DIR)/%.xu90, $(shell find $(SOURCE_DIR) -name '$(THE_FILE).F90' -print)))

# Default make target for file
# 
.PHONY: pre_process

include $(LFRIC_BUILD)/fortran.mk

# Call this target, expect these files to be done first
#
pre_process: $(SOURCE_xu_FILES)

# Make a copy of target file,
# Preprocess target file,
# Remove origonal F90, see psyclone step
#
# For the nvidia compiler, they only output into f90,
# we need to move any f90 files to xu90 files for psyclone.
# It also seems to place them at the root of the working dir.
# See ticket Apps#624 for further context
#
ifeq ("$(FORTRAN_COMPILER)", "nvfortran")
$(SOURCE_DIR)/%.xu90: $(SOURCE_DIR)/%.F90
	echo Pre processing $<
	cp $< $(SOURCE_DIR)/$*.FO90
	$(FPP) $(FPPFLAGS) $(MACRO_ARGS) $<
	-mv $(SOURCE_DIR)/$*.f90 $@
	-mv $(shell basename $*.f90) $@
	rm $<
else
$(SOURCE_DIR)/%.xu90: $(SOURCE_DIR)/%.F90
	echo Pre processing $<
	cp $< $(SOURCE_DIR)/$*.FO90
	$(FPP) $(FPPFLAGS) $(MACRO_ARGS) $< -o $@
	rm $<
endif
