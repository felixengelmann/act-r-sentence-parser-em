## An integrated model of eye movement behaviour in sentence comprehension ##
An ACT-R sentence parsing model based on Lewis & Vasishth (2005), interacting with eye movement control.  
This is part of the PhD Thesis of Felix Engelmann, submitted to the University of Potsdam, 2016.  

This model includes:
- an ACT-R 6.0 distribution
- a modified version of the EMMA eye movement module
- a newly developed parsing module

This file documents the prerequisites and the model structure. See the wiki at https://github.com/felixengelmann/act-r-sentence-parser-em/wiki for a quick tutorial.

---

### I. Prerequisites
#### Lisp
Lisp is required in order to run ACT-R. I recommend Clozure Common Lisp (formerly OpenMCL): http://trac.clozure.com/ccl/.
 
 1. On Mac, just install Clozure CL from the App Store.

Alternatively (on Mac), download the source:

 1. Download CCL from http://trac.clozure.com/ccl/
 2. Copy the contents, e.g., to `/Applications/ccl`
 3. Copy `scripts/ccl` and/or `scripts/ccl64` to `/usr/local/bin/` (with sudo):  
   `> sudo cp scripts/ccl* /usr/local/bin/`  
 4. Edit both and replace value after `=` of `CCL_DEFAULT_DIRECTORY=...` with `/Applications/ccl/`
 5. Make the script(s) executable:  
   `> sudo chmod +x ccl*`  


#### R packages
In order to run the scripts provided for analysis, you need to ensure that R and the following packages are installed:  

If you don not already have R installed, you can retrieve it from https://www.r-project.org/.

 - ggplot2 (plotting)
 - tidyr (data wrangling)
 - dplyr (data wrangling)
 - em2 (eye movement measures, download at http://cran.r-project.org/src/contrib/Archive/em2/)

Packages other than em2 can be installed by typing `install.packages("PACKAGENAME")` in R.  

**em2** has to be downloaded from http://cran.r-project.org/src/contrib/Archive/em2/. Then type `R CMD INSTALL em2_0.9.tar.gz` on the command line.




### II. Getting started
#### ACT-R
An ACT-R distribution is included. The model runs with ACT-R 6.0 and has not been tested on newer versions. If you want to use a different version, refer to:
 - http://act-r.psy.cmu.edu/
 - https://github.com/RyanHope/ACT-R

#### Modules
The parsing module `parsing-module.lisp` and an adjusted version of the EMMA eye movement control model `emma-p.lisp` are located in `actr6/other-files/`.
If desired, you can also put `chunk-tree.lisp` in there to visualise trees.


#### Running ACT-R and environment
 1. In terminal, navigate to the directory of the project you want to load, e.g., `act-r-sentence-parser-em/LewisVasishth2005/`
 2. Start Lisp:  
   E.g., `> ccl`  or  `> ccl64`
  - When running ACT-R for the first time or something has changed in the modules, make sure ACT-R recompiles all files when loading:  
  `(push :actr-recompile *features*)`
 3. Load ACT-R:  
   `(load "../actr6/load-act-r-6.lisp")`
 4. Run environment:  
   `(run-environment)`
  - Depending on the lisp distribution or the operating system, it may be necessary to start the environment manually (in actr6/environment/) and then connect ACT-R to it by:  
   `(start-environment)`
 5. Now you can load the desired model, e.g., for loading the LewisVasishth2005 model, type:  
   `(load "sp-lv05.lisp")`
 
#### Functions to get started
 - Run a demo sentence: `(demo)`
 - Reload model: `(rl)`
 - Delete output: `(delete-output)`
 - Run a specific sentence, e.g.: `(ps *gg-sr*)` or `(ps "the dog bit the boy")`
 - Run an experiment, e.g.: `(re 'gg-exp1 60)`



---

### III. Model structure

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
 - Present sentences:
```cl
(ps (SENTENCE &key (time *max-time*) (params nil)))` ;; (present-whole-sentence …)
Example: (ps *gg-or* :params '(:lf 0.8)) 
(pl (&optional (params nil))) ;; (present-sentence-list …)  
(pn (n &optional (params nil))) ;; (present-sentence-number …)
(demo) (demo1) (demo2) (demo3) ;; Run demo sentence
```

 - Reload model:
```cl
(rl) ;; (reload-sp) ;; Reload model
(reset-sp) ;; Reset model
(clear-sp) ;; Reload all files  
```

###### Experiment control
 - Run experiment `NAME` with `ITERATIONS`:  
```cl 
(re (name &optional (iterations 1) params)) ;; (run-experiment-em …)
Example: (re 'gg-exp1 50 :params '(:lf 0.8 :mp 2))
```

 - Run subjects:
```cl
(res (name &optional (iterations 1) (subjects 1) params (script "spinresults.R") notes)) ;; (run-subjects-em …)
Example: (res 'MV13 20 50 :params '(:lf 0.3 :mp 6))
```

 - Search parameter space:
```cl
(run-paramset-em (name &optional (iterations 1) (params nil)))
(search-param-space-em (experiment iterations &optional (pspace '*pspace1*)))
(search-param-space-subjects-em (experiment subjects iterations &optional (pspace '*pspace1*)))
Example: `(search-param-space-subjects-em MV13 20 50 *pspace1*)`
```

###### Helpers
`(print-params)` - Prints important parameters  
`(print-interface-params)` - Prints interface parameters  
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

###### Interface parameters (interface.lisp)
```cl
(defvar *real-time* T)
(defvar *output-dir* "output")
(defparameter *record-times* T)
```

###### Model parameters (interface.lisp)
```cl
(defvar *read-corpus* nil)  
(defvar *raw-freq* nil)  
(defparameter *surprisal-on* nil)  
(defparameter *surprisal-hl-on* nil)  
(defparameter *fake-retrieval-on* nil)  
(defparameter *time-penalty-factor* 0.1 "Factor for penalizing use of time-out productions (p = -FACTOR*FIRING-COUNT)")
```

###### EMMA parameters (model.lisp)
```cl
:VISUAL-ENCODING-FACTOR    0.002  
:VISUAL-ENCODING-EXPONENT  0.4  
:SACCADE-PREPARATION-TIME  0.110  
:FIXED-PREP-TIME           T
```

###### Parsing parameters (model.lisp)
```cl
:gram-lf                   1  
:gram-rt                   -1.5  
:gram-force-merge          T  
:att-util                  0.5  
:att-util2                 -0.5  
:regr-util                 0.75  
:sp-time                   0.03   
:SURPRISAL-FACTOR          0.005  
:SURPRISAL-HL-FACTOR       2  
```



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


### IV. Modeling functions

#### Parser (support-parser.lisp)

```cl
(set-begin-time word)  
(set-end-time word)  
(set-end-time-abort word)  
(word-message word)  
(trialmessage var val)  
(attach-message head relation dependent)  
(parsing-skip-message word)  
(report-regression visual-location target-pos target-loc)  
(start-time-out location)  
(exit-time-out)  
(current-clause)  
(push-clause)  
(pop-clause)  
(current-ip)  
(set-current-ip)  
(check-parsed visloc) ;; Returns true if word at location visloc has been parsed already.
```

#### Interaction with parsing module (parsing-module.lisp)

```cl
(parsing-set-begin-time word index location) ;; Set begin of attachment.  
(parsing-set-end-time) ;; Set end of attachment.  
(parsing-set-end-time-abort) ;; Set end of attachment, indicating attachment was canceled.  

(parsing-get-index)  
(parsing-get-word)  
(parsing-get-loc)  
(parsing-get-durations)  
(parsing-get-attached-items)  
(parsing-get-attached-positions)  
(parsing-get-unattached-positions)  
(parsing-check-attached index)  

(parsing-print-info) ;; Displays info about parsing state, current word and location, and attached items.
```

#### Interaction with EMMA module (interface-emma.lisp)

```cl
(reset-emma)  
(current-eye-loc)  
(get-em-trace)  
(em-trace->fixations em-trace sentence)  
```

#### Helpers (helper-functions.lisp)

```cl
(event-message message)  
(info-message message)  
(priority-event-message message)  
(priority-info-message message)  
```




