Quick start for the demo model of the Lewis & Vasishth (2005) parser extended with eye movement control.


#### I. Installing Lisp

I recommend Clozure Common Lisp (formerly OpenMCL):
 
 1. On Mac, just install Clozure CL from the App Store.

Alternatively download the source:

 1. Download CCL from http://trac.clozure.com/ccl/
 2. Copy the contents, e.g., to `/Applications/ccl`
 3. Copy `scripts/ccl` and/or `scripts/ccl64` to `/usr/local/bin/` (with sudo):  
   `> sudo cp scripts/ccl* /usr/local/bin/`  
 4. Edit both and replace value after `=` of `CCL_DEFAULT_DIRECTORY=...` with `/Applications/ccl/`
 5. Make the script(s) executable:  
   `> sudo chmod +x ccl*`  


#### II. Running demo model

 1. Download the act-r-sentence-parser-em and extract
 2. Start Lisp (If CCL is properly installed, it should start with `ccl` or `ccl64`)
 3. If not already there, navigate to SentenceParser_Demo/LewisVasishth2005/ with the `cwd` command:  
   `> (cwd „XXX/SentenceParser_Demo/LewisVasishth2005“)`
    - replace `XXX` by the rest of the path if necessary
 4. Make sure ACT-R recompiles all files when loading the next time:  
   `> (push :actr-recompile *features*)`  
 5. Load ACT-R:  
   `> (load "../actr6/load-act-r-6.lisp")`  
 6. Run environment:  
   `> (run-environment)`  
 7. Load LewisVasishth model:  
   `> (load "sp-lv05.lisp")`  
 8. Run demo sentence "the dog bit the boy":  
   `> (demo)`  


You should see a trace with the final notice "SENTENCE PARSED SUCCESSFULLY“ (mostly; it might fail from time to time due to noise).

You can now open the "DM Tree viewer" from the according button in your environment. Find the chunk `syn-obj0-2`. That is the youngest copy of the source node chunk of the parse tree (the highest IP). When you click on it, you see the content of the chunk’s slots recursively in tree format. That is the resulting sentence structure the parser has created.

Now reload and switch on real time to better observe the eye movements (blue) and attention shifts (red) in the experiment window:  
`> (rl)`  
`> (setf *real-time* T)`  

Run a more complicated sentence, an object relative from Grodner & Gibson (2004): "the reporter who the photographer sent to the editor hoped for the story“. Also increase the retrieval latency factor a bit to see more stuff happening:  
`> (ps *gg-or* :params '(:lf 0.8))` 

You can try out any other sentence you find in `LewisVasishth2005/sentences`.lisp using the `(ps)` function. In the current state, there are still some that always fail.

Information about the parsing state, important interface variables, and model parameters can be displayed with the following functions:  
`> (print-runtime-vars)`  
`> (print-interface-params)`  
`> (print-params)`  

If you wish, you can change the amount of output you want to see in the trace. For example, show only fired productions:  
`> (setprint firing)`  
> Instead of **firing**, you can also set it to **condensed**, **full**, **default**, or **off**.

Now, let’s delete the output files (in LewisVasishth2005/output/), reload the model, and run the Grodner & Gibson (2004) Experiment 1 comparing subject- and object-relative clauses with 60 iterations:  
`> (delete-output)`  
`> (rl)`  
`> (re 'gg-exp1 60)`  

Start R and run the script demo-analysis.R, which you find in the output directory.
The script reads the files `fixations.txt` and `attachments.txt` that ACT-R has created in the output directory. I will create plots showing the attachment durations, total fixation times for each word, and the eye movement scanpaths of some of the trials. The latter conveniently show the difference in regressions, refixations, and durations between subject- and object-relative clauses.

You can also run this script after a single sentence presentation. Just remember to always delete the output files with (delete-output) before presenting a new sentence, because otherwise the plotted means will be a mixture of unrelated sentences.

---

See the general README for more information on functions, parameters, and the file structure of the model.







 


