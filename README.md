## An integrated model of eye movements in sentence comprehension ##

> **The model and module code is under heavy development and at the moment in a kind of transition stage containing some redundant or deprecated code. Some things might not work.**

An ACT-R sentence parsing model as described in Lewis & Vasishth (2005), extended to generate eye movements.

This model includes:
- an ACT-R 6.0 distribution
- a modified version of the EMMA eye movement module
- a newly developed parsing module

This file documents the prerequisites and the model structure. See the wiki at https://github.com/felixengelmann/act-r-sentence-parser-em/wiki for a quick tutorial.

---

### I. Prerequisites
#### Lisp

I recommend Clozure Common Lisp (formerly OpenMCL):
 
 1. On Mac, just install Clozure CL from the App Store.

Alternatively, download the source:

 1. Download CCL from http://trac.clozure.com/ccl/
 2. Copy the contents, e.g., to `/Applications/ccl`
 3. Copy `scripts/ccl` and/or `scripts/ccl64` to `/usr/local/bin/` (with sudo):  
   `> sudo cp scripts/ccl* /usr/local/bin/`  
 4. Edit both and replace value after `=` of `CCL_DEFAULT_DIRECTORY=...` with `/Applications/ccl/`
 5. Make the script(s) executable:  
   `> sudo chmod +x ccl*`  


#### ACT-R

An ACT-R distribution is included, but if you wish to retrieve a newer version, you can refer to the following sources:
 - http://act-r.psy.cmu.edu/
 - https://github.com/RyanHope/ACT-R
 - `svn://jordan.psy.cmu.edu/usr/local/svnroot/actr6`


#### Modules

Make sure the following modules (provided in `MODULES/`) are located in `actr6/other-files/`
- `emma-p.lisp`
- `parsing-module.lisp`
- if desired: `chunk-tree.lisp`


#### Running ACT-R and environment
 1. In terminal, navigate to the directory of the project you want to load, e.g., `act-r-sentence-parser-em/LewisVasishth2005/`
 2. Start Lisp:  
   E.g., `> ccl`  or  `> ccl64`
  - When running ACT-R for the first time or something has changed in the modules, make sure ACT-R recompiles all files when loading:  
  `> (push :actr-recompile *features*)`
 3. Load ACT-R:  
   `(load "../actr6/load-act-r-6.lisp")`
 4. Run environment:  
   `> (run-environment)`
  - Depending on the lisp distribution or the operating system, it may be necessary to start the environment manually (in actr6/environment/) and then connect ACT-R to it by:  
   `> (start-environment)`
 5. Now you can load the desired model, e.g., for loading the LewisVasishth2005 model, type:  
   `> (load "sp-lv05.lisp")`

---

### II. Model structure

#### Files
###### Project-related
 - `sp-#PROJECTNAME.lisp` - Main file which loads all other files. Sets global variables:  
      `(setf *output-dir* "output")`  
      `(defparameter *read-corpus* NIL)`
 - `model.lisp` - Sets model parameters
 - `chunks.lisp` - Chunks
 - `productions-parser.lisp` - Parsing rules
 - `sentences.lisp` - Test sentences, experiment definitions, and parameter spaces for estimation


###### Core-model (in sp directory)
 - `interface.lisp` - Contains basic presentation functions and global variables
 - `productions-control.lisp` - Control productions for attention shift and eye-parser interaction
 - `constants.lisp` - Chunk types
 - `interface-emma.lisp` - EMMA-related functions
 - `support-parser.lisp` - Parser-related functions
 - `support-lexicon.lisp` - Lexicon-creation functions
 - `experiment-control.lisp` - Functions for running experiments and parameter estimation
 - `experiment-control-original.lisp` - Original experiment control functions from Lewis & Vasishth (2005)
 - `helper-functions.lisp` - Useful helpers


#### Useful functions

###### Interface
 - `(ps (SENTENCE &key (time *max-time*) (params nil)))` => abbr. for `(present-whole-sentence …)` - Present sentence `SENTENCE`.  
 - `(pl (&optional (params nil)))` => `(present-sentence-list …)`  
 - `(pn (n &optional (params nil)))` => `(present-sentence-number …)`  
 - `(rl)` => `(reload-sp)` - Reload model.  
 - `(clear-sp)` - Reload all files.  
 - `(demo)` - Run demo sentence.   
 - `(demo1)` - ...or demo2 or demo3

###### Experiment control
 - `(re (NAME &optional (ITERATIONS 1) params))` - Run experiment `NAME` with `ITERATIONS`
    - E.g.: `(re 'gg-exp1 50 :params '(:lf 0.8 :mp 2))`
    - abbr. for `(run-experiment-em …)`
 - `(res (name &optional (iterations 1) (subjects 1) params (script "spinresults.R") notes))` - Run subjects
    - E.g.: `(res 'MV13 20 50 :params '(:lf 0.3 :mp 6 :att-util 0.5 :att-util2 -0.25 :sp-time 0.05))`
    - abbr. for `(run-subjects-em …)`
 - `(run-paramset-em (name &optional (iterations 1) (params nil)))`
 - `(search-param-space-em (experiment iterations &optional (pspace '*pspace1*)))`
 - `(search-param-space-subjects-em (experiment subjects iterations &optional (pspace '*pspace1*)))`
    - E.g.: `(search-param-space-subjects-em MV13 20 50 *pspace1*)`

###### Helpers
`(print-params)` - Prints important parameters  
`(print-interface-params)` - Prints interface parameters  
`(print-runtime-vars)` -  Prints parsing state (soon obsolete)  
`(parsing-print-info)` - Displays info about parsing state, current word and location, and attached items  
`(delete-output)` - Delete output files for fixations etc. in output directory   

`(setprint s)`

>Possible values for s:  
 - **full**         - `(:v t :CMDT t :trace-detail high :trace-filter nil :model-warnings t)`
 - **condensed**    - `(:v t :CMDT t :trace-detail low :trace-filter nil :model-warnings nil :buffer-trace nil)`
 - **on**           - `(:v t :CMDT t :trace-detail low :trace-filter nil :model-warnings nil :buffer-trace nil)`
 - **default**      - `(:v t :CMDT t :trace-detail low :trace-filter nil :model-warnings nil :buffer-trace nil)`
 - **notrace**      - `(:v nil :CMDT t :model-warnings t :buffer-trace nil)`
 - **trace**        - `(:v t :CMDT nil :trace-detail medium :trace-filter nil :model-warnings nil :buffer-trace nil)`
 - **firing**       - `(:v t :CMDT nil :trace-detail high :trace-filter production-firing-only :model-warnings nil)`
 - **off**          - `(:v nil :CMDT nil :model-warnings nil :buffer-trace nil)`
 - **buffertrace**  - `(:V t :CMDT nil :trace-detail high :model-warnings NIL :trace-filter nil :buffer-trace t)`



#### Parameters and variables

###### Model parameters (interface.lisp)
`(defvar *read-corpus* nil)`  
`(defvar *raw-freq* nil)`  
`(defparameter *surprisal-on* nil)`  
`(defparameter *surprisal-hl-on* nil)`  
`(defparameter *fake-retrieval-on* nil)`  

###### EMMA parameters (model.lisp)
`:VISUAL-ENCODING-FACTOR    0.002`  
`:VISUAL-ENCODING-EXPONENT  0.4`  
`:SACCADE-PREPARATION-TIME  0.110`  
`:SURPRISAL-FACTOR          0.005`  
`:SURPRISAL-HL-FACTOR       2`  

###### Parsing parameters (model.lisp)
`:gram-lf                   1`  
`:gram-rt                   -1.5`  
`:gram-force-merge          t`  
`:att-util                  0.5`  
`:att-util2                 -0.5`  
`:regr-util                 0.75`  
`:sp-time                   0.03 `  



#### Output

 - fixations.txt  
   `#EXPERIMENT   #ITERATION  #COND/SENT/ITEM   #WORDPOS #WORD #FIXTIME`
 - trialmessages.txt  
   `#EXPERIMENT   #ITERATION  #COND/SENT/ITEM   #WORDPOS #WORD #VARIABLE   #VALUE`
 - attachments.txt  
   `#EXPERIMENT  #SIMULATION #ITEM   #WN   #WORD   #ATTACHTIME`
 - enctimes.txt  
   `#EXPERIMENT   #ITERATION  #COND/SENT/ITEM   #WORDPOS #WORD #ENCTIME #ECCENTRICITY  #FREQUENCY`
 - timeouts.txt  
   `#EXPERIMENT   #ITERATION  #COND/SENT/ITEM   #WORDPOS #WORD #EYELOC`
 - subjects.txt  
   `#EXPERIMENT-#SUBJNUM #ITERATION  #SOURCEACT`


---


### III. Modeling functions

#### Parser (support-parser.lisp)

`(set-begin-time word)`  
`(set-end-time word)`  
`(set-end-time-abort word)`  
`(word-message word)`  
`(trialmessage var val)`  
`(attach-message head relation dependent)`  
`(parsing-skip-message word)`  
`(report-regression visual-location target-pos target-loc)`
`(start-time-out location)`  
`(exit-time-out)`  
`(current-clause)`  
`(push-clause)`  
`(pop-clause)`  
`(current-ip)`  
`(set-current-ip)`  
`(check-parsed)` 

#### Interaction with parsing module (parsing-module.lisp)

`(parsing-set-begin-time word index location)`  - Set begin of attachment.  
`(parsing-set-end-time)`  - Set end of attachment.  
`(parsing-set-end-time-abort)`  - Set end of attachment, indicating attachment was canceled.  

`(parsing-get-index)`  
`(parsing-get-word)`  
`(parsing-get-loc)`  
`(parsing-get-durations)`  
`(parsing-get-attached-items)`  
`(parsing-get-attached-positions)`  
`(parsing-get-unattached-positions)`  
`(parsing-check-attached index)`  

`(parsing-print-info)` - Displays info about parsing state, current word and location, and attached items.

#### Interaction with EMMA module (interface-emma.lisp)

`(reset-emma)`  
`(current-eye-loc)`  
`(get-em-trace)`  
`(em-trace->fixations em-trace sentence)`  


#### Helpers (helper-functions.lisp)

`(event-message message)`  
`(info-message message)`  
`(priority-event-message message)`  
`(priority-info-message message)`  





