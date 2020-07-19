# Emacs invocation
# Don't load an init file: -q
# Avoid processing X resources: -Q
# Send messages to stderr: --batch
EMACS = emacs -Q -q --batch

# Remove command
RM = rm

# Additional emacs load-path
LOAD_PATH := -L ${PWD}

# Define Compile Command
# Call batch-byte-compile function: -f
COMPILE := -f batch-byte-compile

# Expand the source code files
EL != ls *.el

# Compiled files
ELC = ${EL:.el=.elc}

# Default goal
all: compile

# Compile needed files
compile: $(ELC)

# Translate lisp text (.el) files in byte compiled (.elc) files
$(ELC): $(EL)
	${EMACS} ${LOAD_PATH} ${COMPILE} ${.ALLSRC}

clean:
	${RM} ${ELC}
