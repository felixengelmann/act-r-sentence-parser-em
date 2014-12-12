
An ACT-R sentence parsing model as described in Lewis & Vasishth (2005), extended to generate eye movements.



This model includes:
- an ACT-R 6.0 distribution
- a modified version of the EMMA eye movement module
- a newly developed parsing module

The model and module code is under heavy development and at the moment in a kind of transition stage containing some redundant or deprecated code. Some things might not work.



=================================================
I. INSTALLING LISP (on Mac OS X)
=================================================
Clozure Common Lisp (formerly OpenMCL)
1) download .dmg from http://trac.clozure.com/ccl/
2) mount dmg and copy contents to /Applications/ccl
3) copy "scripts/ccl" and/or "scripts/ccl64" to "/usr/local/bin/" (with sudo)
4) edit both and replace value in "CCL_DEFAULT_DIRECTORY=" with "/Applications/ccl/"
5) make the script(s) executable: "sudo chmod +x ccl(64)"

NEW ALTERNATIVE:
1) Just install Clozure CL from the App Store :)


=================================================
II. GETTING ACT-R
=================================================
An ACT-R distribution is included, but if you wish to retrieve a newer version, you can use the following sources:
SVN: svn://jordan.psy.cmu.edu/usr/local/svnroot/actr6
GITHUB: https://github.com/RyanHope/ACT-R


=================================================
III. MODULES
=================================================
Make sure the following modules (provided in MODULES) are located in actr6/other-files/
- emma-p.lisp
- parsing-module.lisp
(- chunk-tree.lisp)


=================================================
IV. RUNNING ACT-R AND ENVIRONMENT
=================================================
1) In terminal, navigate to the directory of the project you want to load, e.g., ACTR-Sentence-Parser/LewisVasishth2005/.
2) Start LISP:  
     > ccl  or  > ccl64
2a) When running ACT-R for the first time or something has changed in the modules, make sure ACT-R recompiles all files when loading:
   > (push :actr-recompile *features*)
3) Load ACT-R:
   > (load "../actr6/load-act-r-6.lisp")
4) Run environment:
   > (run-environment)
4a) Depending on the lisp distribution or the operating system, it may be necessary to start the environment manually (in actr6/environment/) and then connect ACT-R to it by:
   > (start-environment)
5) Now you can load the desired model, e.g., for loading the LewisVasishth2005 model, type:
   > (load "sp-lv05.lisp")



=================================================
V. FILES
=================================================
1) PROJECT-RELATED
------------------
sp-#PROJECTNAME.lisp
   Main file which loads all other files.
   Sets global variables:
      (setf *output-dir* "output")
      (defparameter *read-corpus* NIL)

model.lisp
   Sets model parameters

chunks.lisp
   Chunks.

productions-parser.lisp
   Parsing rules.

sentences.lisp
   Test sentences, experiment definitions, and parameter spaces for estimation.


2) CORE-MODEL (in sp directory)
-------------------------------
interface.lisp
   Contains basic presentation functions and global variables.

interface-emma.lisp
   EMMA-related functions.

experiment-control.lisp
   Functions for running experiments and parameter estimation.

constants.lisp
   Chunk types.

productions-control.lisp
   Control productions for attention shift and eye-parser interaction.



=================================================
VI. USEFUL FUNCTIONS
=================================================
1) INTERFACE
------------
(ps (SENTENCE &key (time *max-time*) (params nil)))
   -> abbr. for (present-whole-sentence …)
   Present sentence SENTENCE.

(pl (&optional (params nil)))
   -> abbr. for (present-sentence-list …)

(pn (n &optional (params nil)))
   -> abbr. for (present-sentence-number …)

(rl)
   -> abbr. for (reload-sp)
   Reload model.

(clear-sp)
   Reload all files.

(demo)
   Run demo sentence.

(demo1)
   ...or demo2 or demo3


2) EXPERIMENT CONTROL
---------------------
(re (NAME &optional (ITERATIONS 1) params))
   -> abbr. for (run-experiment-em …)
   Run experiment NAME with ITERATIONS
   E.g.: (re 'gg-exp1 50 :params '(:lf 0.8 :mp 2))

(res (name &optional (iterations 1) (subjects 1) params (script "spinresults.R") notes))
   -> abbr. for (run-subjects-em …)
   E.g.: (res 'MV13 20 50 :params '(:lf 0.3 :mp 6 :att-util 0.5 :att-util2 -0.25 :sp-time 0.05))

(run-paramset-em (name &optional (iterations 1) (params nil)))

(search-param-space-em (experiment iterations &optional (pspace '*pspace1*)))

(search-param-space-subjects-em (experiment subjects iterations &optional (pspace '*pspace1*)))
   - E.g.: (search-param-space-subjects-em MV13 20 50 *pspace1*)


3) HELPERS
----------
(print-params)
(print-interface-params)
(print-runtime-vars)

(delete-output)

(setprint s)
- possible values for s:
full        (:v t :CMDT t :trace-detail high :trace-filter nil :model-warnings t)
condensed   (:v t :CMDT t :trace-detail low :trace-filter nil :model-warnings nil :buffer-trace nil)
on          (:v t :CMDT t :trace-detail low :trace-filter nil :model-warnings nil :buffer-trace nil)
default     (:v t :CMDT t :trace-detail low :trace-filter nil :model-warnings nil :buffer-trace nil)
notrace     (:v nil :CMDT t :model-warnings t :buffer-trace nil)
trace       (:v t :CMDT nil :trace-detail medium :trace-filter nil :model-warnings nil :buffer-trace nil)
firing      (:v t :CMDT nil :trace-detail high :trace-filter production-firing-only :model-warnings nil)
off         (:v nil :CMDT nil :model-warnings nil :buffer-trace nil)
buffertrace (:V t :CMDT nil :trace-detail high :model-warnings NIL :trace-filter nil :buffer-trace t)



=================================================
VII. PARAMETERS AND VARIABLES
=================================================
1) MODEL PARAMETERS (interface.lisp)
------------------------------------
(defvar *read-corpus* nil)
(defvar *raw-freq* nil)
(defparameter *surprisal-on* nil)
(defparameter *surprisal-hl-on* nil)
(defparameter *fake-retrieval-on* nil)

2) EMMA PARAMETERS (model.lisp)
-------------------------------
:VISUAL-ENCODING-FACTOR    0.002
:VISUAL-ENCODING-EXPONENT  0.4
:SACCADE-PREPARATION-TIME  0.110
:SURPRISAL-FACTOR          0.005
:SURPRISAL-HL-FACTOR       2

3) PARSING PARAMETRS (model.lisp)
---------------------------------
:gram-lf                   1
:gram-rt                   -1.5
:gram-force-merge          t
:att-util                  0.5
:att-util2                 -0.5
:regr-util                 0.75
:sp-time                   0.03 



=================================================
VIII. OUTPUT
=================================================
1) OUTPUT FUNCTIONS
-------------------
(trialmessage var val)
(attach-message head relation dependent)
(word-message word)
(parsing-skip-message word)


2) OUTPUT FILES
---------------
fixations.txt:
   #EXPERIMENT	#ITERATION	#COND/SENT/ITEM	#WORDPOS	#WORD	#FIXTIME

trialmessages.txt:
   #EXPERIMENT	#ITERATION	#COND/SENT/ITEM	#WORDPOS	#WORD	#VARIABLE	#VALUE

attachments.txt:
   #EXPERIMENT  #SIMULATION #ITEM   #WN   #WORD   #ATTACHTIME

enctimes.text:
   #EXPERIMENT	#ITERATION	#COND/SENT/ITEM	#WORDPOS	#WORD	#ENCTIME	#ECCENTRICITY	#FREQUENCY

timeouts.txt:
   #EXPERIMENT	#ITERATION	#COND/SENT/ITEM	#WORDPOS	#WORD	#EYELOC

subjects.txt:
   #EXPERIMENT-#SUBJNUM #ITERATION  #SOURCEACT



=================================================
IX. INTERACTION WITH PARSING MODULE
=================================================
(parsing-set-begin-time word index location)
   Set begin of attachment.
(parsing-set-end-time)
   Set end of attachment.
(parsing-set-end-time-abort)
   Set end of attachment, indicating attachment was canceled.

(parsing-get-index)
(parsing-get-word)
(parsing-get-loc)
(parsing-get-durations)
(parsing-get-attached-items)
(parsing-get-attached-positions)
(parsing-get-unattached-positions)
(parsing-check-attached index)

(parsing-print-info)
   Displays info about parsing state, current word and location, and attached items.



=================================================
X. INTERACTION WITH EMMA MODULE (interface-emma.lisp)
=================================================
(reset-emma)
(current-eye-loc)
(get-em-trace)
(em-trace->fixations em-trace sentence)
(em-trace->fixations em-trace sentence)






