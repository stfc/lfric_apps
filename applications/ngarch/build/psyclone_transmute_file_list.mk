##############################################################################
# (c) Crown copyright 2025 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################

# Choose which files to Pre-proccess and PSyclone from physics / UM source. 
# These file lists are used in the transmute PSyclone method. 
# Note we may use existing OMP directives in the source and so have no need to 
# Psyclone. GPU offload may do the same, or it may need to PSyclone generally

# Below are two sources of files, this is due to two import methods of UM source
# The forked physics now in LFRic Apps, and FCM repos for UKCA, CASIM, etc.
# Once all of FCM source is forked into LFRic Apps, we can condense these 
# two variables into one under PSYCLONE_PHYSICS_FILES

export PSYCLONE_PHYSICS_FILES_IMPORT =

export PSYCLONE_PHYSICS_FILES_FCM =
