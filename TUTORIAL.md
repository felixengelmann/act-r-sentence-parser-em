## Quick start tutorial

Quick start for the demo model of the Lewis & Vasishth (2005) parser extended with eye movement control.

#### I. Prerequisites
Consult the README for steps required before running the model.


#### II. Run the model

 1. Download the act-r-sentence-parser-em and extract
 2. Start Lisp
 3. If not already there, navigate to act-r-sentence-parser-em/LewisVasishth2005/ with the `cwd` command:  
   `> (cwd „XXX/act-r-sentence-parser-em/LewisVasishth2005“)`
    - replace `XXX` by the rest of the path if necessary
 5. Load ACT-R:  
   `> (load "../actr6/load-act-r-6.lisp")`  
 6. Run environment:  
   `> (run-environment)`  
    - Depending on the lisp distribution or the operating system, it may be necessary to start the environment manually (in actr6/environment/) and then connect ACT-R to it by:  
   `(start-environment)`
 7. Load LewisVasishth model:  
   `> (load "sp-lv05.lisp")`  
 8. Run demo sentence "the dog bit the boy":  
   `> (demo)`  

You should see a trace with the final notice `SENTENCE PARSED SUCCESSFULLY` (mostly; it might fail from time to time due to noise).

#### III. Run a sentence
Now reload and switch on real-time mode to better observe the eye movements (blue) and attention shifts (red) in the experiment window:  
`> (rl)`  
`> (setf *real-time* T)`  

Run a more complicated sentence, an object relative from Grodner & Gibson (2004): "the reporter who the photographer sent to the editor hoped for the story“. Also increase the retrieval latency factor a bit to see more stuff happening:  
`> (ps *gg-or* :params '(:lf 0.8))` 

You can try out other sentences you find in `LewisVasishth2005/sentences.lisp` using the `(ps)` function. In the current state, there are still some that always fail. 

Or run your own sentence (make sure you only use words that are defined in the lexicon in `sp/chunks.lisp`), e.g.:  
`> (ps "the writer surprised the editors *")`  

Information about the parsing state, important interface variables, and model parameters can be displayed with the following functions:  
`> (print-info)`  
`> (print-interface-params)`  
`> (print-params)`  

If you wish, you can change the amount of output you want to see in the trace. For example, show only fired productions:  
`> (setprint firing)`  
> Instead of **firing**, you can also set it to **condensed**, **full**, **default**, or **off**.


##### Output
You will now find three files in the `LewisVasishth2005/output/` directory:
`attachments.txt`, `enctimes.txt`, and `fixations.txt`. 
They contain the attachment times, visual encoding times, and fixation times in tab-separated tables that can be read into R for further analysis.  

##### View the sentence structure
You can now open the "DM Tree viewer" from the according button in your environment. Find the chunk `syn-obj0-2`. That is the youngest copy of the source node chunk of the parse tree (the highest IP). When you click on it, you see the content of the chunk’s slots recursively in tree format. That is the resulting sentence structure the parser has created.

##### Quick analysis
Start R and run the script `1_quick_results.R`, which you find in the `output/` directory.
The script reads the files `fixations.txt` and `attachments.txt` that ACT-R has created in the output directory. It creates a `quick_results.pdf` with plots showing the attachment durations, total fixation times for each word, and the eye movement scanpath.

> Remember to always delete the output files with `(delete-output)` before presenting a new sentence and running the R script, because otherwise the plotted means will be a mixture of unrelated sentences.


#### IV. Run an experiment
Now, let’s delete the output files (in `LewisVasishth2005/output/`), reload the model, and run the Grodner & Gibson (2004) Experiment 1 comparing subject- and object-relative clauses with 60 iterations:  
`> (delete-output)`  
`> (rl)`  
`> (re 'gg-exp1 60)`  

Again, run the R script `1_quick_results.R`. The resulting PDF now shows the processing differences between subject- and object-relative clauses.


##### Experiment Analysis
Now run the R script `2_analyse_experiment.R`, which generates `GG-EXP1-results.pdf`.
The script reads `experiment-data.txt` that was generated by ACT-R and runs the analysis on the regions specified there and compares it to the given data. Both is specified in the experiment definition in `sentences.lisp`.

> For running the experiment analysis R code, you need to install the em2 package for computing eye-tracking measures, which can be downloaded here: http://cran.r-project.org/src/contrib/Archive/em2/.

##### Staub (2010)
Test the model's eye movement predictions by simulating an eye-tracking experiment by Staub (2010): Additionally to the inflated reading times on the embedded verb in an object-relative clause, as also seen in Grodner & Gibson (2005), he found more first-pass regressions from the subject NP of an object-relative clause. This was interpreted as an effect of a general expectation for a subject-relative that has to be revised at this position. Run the experiment:

`> (rl)`  
`> (re 'staub10 100)`  

Then, run the R script `staub10-analysis.R`, which creates `Staub2010-analysis.pdf`.
It shows predicted fixation durations and first-pass regressions on the embedded NP, embedded verb, and main verb together with the empirical data.


#### V. Define new experiments
Coming soon.


---

See the general README for more information on functions, parameters, and the file structure of the model.







 


