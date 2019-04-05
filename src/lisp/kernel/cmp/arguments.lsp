(in-package :cmp)

;; A function of two arguments, an LLVM Value and a variable.
;; The "variable" is just whatever is provided to this code
;; (so that it can work with either b or c clasp).
;; The function should put the Value into the variable, possibly generating code to do so.
;; In order to work with cclasp's SSA stuff, it must be called exactly once for each variable.
(defvar *argument-out*)

;; Generate code to signal an error iff there weren't enough arguments provided.
(defun compile-error-if-not-enough-arguments (minimum nargs)
  (let* ((cmin (irc-size_t minimum))
         (error-block (irc-basic-block-create "not-enough-arguments"))
         (cont-block (irc-basic-block-create "enough-arguments"))
         (cmp (irc-icmp-ult nargs cmin)))
    (irc-cond-br cmp error-block cont-block)
    (irc-begin-block error-block)
    (irc-intrinsic-call-or-invoke
     "va_notEnoughArgumentsException"
     (list (irc-constant-string-ptr *gv-current-function-name*) ; FIXME: use function desc instead?
           nargs cmin))
    (irc-unreachable)
    (irc-begin-block cont-block)))

;; Ditto but with too many.
(defun compile-error-if-too-many-arguments (maximum nargs)
  (let* ((cmax (irc-size_t maximum))
         (error-block (irc-basic-block-create "not-enough-arguments"))
         (cont-block (irc-basic-block-create "enough-arguments"))
         (cmp (irc-icmp-ugt nargs cmax)))
    (irc-cond-br cmp error-block cont-block)
    (irc-begin-block error-block)
    (irc-intrinsic-call-or-invoke
     "va_tooManyArgumentsException"
     (list (irc-constant-string-ptr *gv-current-function-name*) ; FIXME: use function desc instead?
           nargs cmax))
    (irc-unreachable)
    (irc-begin-block cont-block)))

;; Generate code to bind the required arguments.
(defun compile-required-arguments (reqargs cc)
  ;; reqargs is as returned from process-lambda-list- (# ...) where # is the count.
  ;; cc is the calling-convention object.
  (dolist (req (cdr reqargs))
    (funcall *argument-out* (calling-convention-args.va-arg cc) req)))

;;; Unlike the other compile-*-arguments, this one returns a value-
;;; an LLVM Value for the number of arguments remaining.
(defun compile-optional-arguments (optargs nreq calling-conv false true)
  ;; optargs is (# var suppliedp default ...)
  ;; We basically generate a switch, but also return the number of arguments remaining
  ;; (or zero if that's negative).
  ;; For (&optional a b) for example,
  #|
size_t nargs_remaining;
switch (nargs) {
  case 0: nargs_remaining = 0; a = [nil]; a_p = [nil]; b = [nil]; b_p = [nil]; break;
  case 1: nargs_remaining = 0; a = va_arg(); a_p = [t]; b = [nil]; b_p = [nil]; break;
  default: nargs_remaining = nargs - 2; a = va_arg(); a_p = [t]; b = va_arg(); b_p = [t]; break;
}
  |#
  ;; All these assignments are done with phi so it's a bit more confusing to follow, unfortunately.
  (let* ((nargs (calling-convention-nargs calling-conv))
         (nopt (first optargs))
         (nfixed (+ nopt nreq))
         (opts (rest optargs))
         (enough (irc-basic-block-create "enough-for-optional"))
         (undef (irc-undef-value-get %t*%))
         (sw (irc-switch nargs enough nopt))
         (assn (irc-basic-block-create "optional-assignments"))
         (final (irc-basic-block-create "done-parsing-optionals"))
         (zero (irc-size_t 0)))
    ;; We generate the assignments first, although they occur last.
    ;; It's just a bit more convenient to do that way.
    (irc-begin-block assn)
    (let* ((npreds (1+ nopt))
           (nremaining (irc-phi %size_t% npreds "nargs-remaining"))
           (var-phis nil) (suppliedp-phis nil))
      ;; We have to do this in two loops to ensure the PHIs come before any code
      ;; generated by *argument-out*.
      (dotimes (i nopt)
        (push (irc-phi %t*% npreds) suppliedp-phis)
        (push (irc-phi %t*% npreds) var-phis))
      ;; OK _now_ argument-out
      (do* ((cur-opt opts (cdddr cur-opt))
            (var (car cur-opt) (car cur-opt))
            (suppliedp (cadr cur-opt) (cadr cur-opt))
            (var-phis var-phis (cdr var-phis))
            (var-phi (car var-phis) (car var-phis))
            (suppliedp-phis suppliedp-phis (cdr suppliedp-phis))
            (suppliedp-phi (car suppliedp-phis) (car suppliedp-phis)))
           ((endp cur-opt))
        (funcall *argument-out* suppliedp-phi suppliedp)
        (funcall *argument-out* var-phi var))
      (irc-br final)
      ;; Generate a block for each case.
      (do ((i nreq (1+ i)))
          ((= i nfixed))
        (let ((new (irc-basic-block-create (core:bformat nil "supplied-%d-arguments" i))))
          (llvm-sys:add-case sw (irc-size_t i) new)
          (irc-phi-add-incoming nremaining zero new)
          (irc-begin-block new)
          ;; Assign each optional parameter accordingly.
          (do* ((var-phis var-phis (cdr var-phis))
                (var-phi (car var-phis) (car var-phis))
                (suppliedp-phis suppliedp-phis (cdr suppliedp-phis))
                (suppliedp-phi (car suppliedp-phis) (car suppliedp-phis))
                (j nreq (1+ j))
                (enough (< j i) (< j i)))
               ((endp var-phis))
            (irc-phi-add-incoming suppliedp-phi (if enough true false) new)
            (irc-phi-add-incoming var-phi (if enough (calling-convention-args.va-arg calling-conv) undef) new))
          (irc-br assn)))
      ;; Default case: everything gets a value and a suppliedp=T.
      (irc-begin-block enough)
      (irc-phi-add-incoming nremaining (irc-sub nargs (irc-size_t nfixed)) enough)
      (dolist (suppliedp-phi suppliedp-phis)
        (irc-phi-add-incoming suppliedp-phi true enough))
      (dolist (var-phi var-phis)
        (irc-phi-add-incoming var-phi (calling-convention-args.va-arg calling-conv) enough))
      (irc-br assn)
      ;; ready to generate more code
      (irc-begin-block final)
      nremaining)))

(defun compile-rest-argument (rest-var varest-p nremaining calling-conv)
  (cmp:irc-branch-to-and-begin-block (cmp:irc-basic-block-create "process-rest-argument"))
  (when rest-var
    (let* ((rest-alloc (calling-convention-rest-alloc calling-conv))
	   (rest (cond
                   ((eq rest-alloc 'ignore)
                    ;; &rest variable is ignored- allocate nothing
                    (irc-undef-value-get %t*%))
                   ((eq rest-alloc 'dynamic-extent)
                    ;; Do the dynamic extent thing- alloca, then an intrinsic to initialize it.
                    (let ((rrest (alloca-dx-list :length nremaining :label "rrest")))
                      (irc-intrinsic-call "cc_gatherDynamicExtentRestArguments"
                                          (list (cmp:calling-convention-va-list* calling-conv)
                                                nremaining
                                                (irc-bit-cast rrest %t**%)))))
                   (varest-p
                    (let ((temp-valist (alloca-vaslist :label "rest")))
                      (irc-intrinsic-call "cc_gatherVaRestArguments" 
                                          (list (cmp:calling-convention-va-list* calling-conv)
                                                nremaining
                                                temp-valist))))
                   (t
                    ;; general case- heap allocation
                    (irc-intrinsic-call "cc_gatherRestArguments" 
                                        (list (cmp:calling-convention-va-list* calling-conv)
                                              nremaining))))))
      (funcall *argument-out* rest rest-var))))

;;; Keyword processing is the most complicated part, unsurprisingly.
#|
Here is pseudo-C for the parser for (&key a). [foo] indicates an inserted constant.
Having to write with phi nodes unfortunately makes things rather more confusing.

if ((remaining_nargs % 2) == 1)
  cc_oddKeywordException([*current-function-description*]);
tstar bad_keyword = undef;
bool seen_bad_keyword = false;
t_star a_temp = undef, a_p_temp = [nil], allow_other_keys_temp = [nil], allow_other_keys_p_temp = [nil];
for (; remaining_nargs != 0; remaining_nargs -= 2) {
  tstar key = va_arg(valist), value = va_arg(valist);
  if (key == [:a]) {
    if (a_p_temp == [nil]) {
      a_p_temp = [t]; a_temp = value; continue;
    } else continue;
  }
  if (key == [:allow-other-keys]) {
    if (allow_other_keys_p_temp == [nil]) {
      allow_other_keys_p_temp = [t]; allow_other_keys_temp = value; continue;
    } else continue;
  }
  seen_bad_keyword = true; bad_keyword = key;
}
if (seen_bad_keyword)
  cc_ifBadKeywordArgumentException(allow_other_keys_temp, bad_keyword, [*current-function-description*]);
a_p = a_p_temp; a = a_temp;
|#

(defun compile-one-key-test (keyword key-arg suppliedp-phi cont-block false)
  (let* ((keystring (string keyword))
         ;; NOTE: We might save a bit of time by moving this out of the loop.
         ;; Or maybe LLVM can handle it. I don't know.
         (key-const (irc-literal keyword keystring))
         (match (irc-basic-block-create (core:bformat nil "matched-%s" keystring)))
         (mismatch (irc-basic-block-create (core:bformat nil "not-%s" keystring))))
    (let ((test (irc-icmp-eq key-arg key-const)))
      (irc-cond-br test match mismatch))
    (irc-begin-block match)
    (let* ((new (irc-basic-block-create (core:bformat nil "new-%s" keystring)))
           (old (irc-basic-block-create (core:bformat nil "old-%s" keystring))))
      (let ((test (irc-icmp-eq suppliedp-phi false)))
        (irc-cond-br test new old))
      (irc-begin-block new) (irc-br cont-block)
      (irc-begin-block old) (irc-br cont-block)
      (irc-begin-block mismatch)
      (values new old))))
  
(defun compile-key-arguments (keyargs lambda-list-aokp nremaining cc false true)
  (macrolet ((do-keys ((keyword) &body body)
               `(do* ((cur-key (cdr keyargs) (cddddr cur-key))
                      (,keyword (car cur-key) (car cur-key)))
                     ((endp cur-key))
                  ,@body)))
    (let ((aok-parameter-p nil)
          allow-other-keys
          (nkeys (car keyargs))
          (undef (irc-undef-value-get %t*%))
          (start (irc-basic-block-create "parse-key-arguments"))
          (matching (irc-basic-block-create "match-keywords"))
          (after (irc-basic-block-create "after-kw-loop"))
          (unknown-kw (irc-basic-block-create "unknown-kw"))
          (kw-loop (irc-basic-block-create "kw-loop"))
          (kw-loop-continue (irc-basic-block-create "kw-loop-continue")))
      ;; Prepare for :allow-other-keys.
      (unless lambda-list-aokp
        ;; Is there an allow-other-keys argument?
        (do-keys (key)
          (when (eq key :allow-other-keys) (setf aok-parameter-p t) (return)))
        ;; If there's no allow-other-keys argument, add one.
        (unless aok-parameter-p
          (setf keyargs (list* (1+ (car keyargs))
                               ;; default, var, and suppliedp are of course dummies.
                               ;; At the end we can check aok-parameter-p to avoid
                               ;; actually assigning to them.
                               :allow-other-keys nil nil nil
                               (cdr keyargs)))))
      (irc-branch-to-and-begin-block start)
      ;; If the number of arguments remaining is odd, the call is invalid- error.
      (let* ((odd-kw (irc-basic-block-create "odd-kw"))
             (rem (irc-srem nremaining (irc-size_t 2))) ; parity
             (evenp (irc-icmp-eq rem (irc-size_t 0)))) ; is parity zero (is SUB even)?
        (irc-cond-br evenp kw-loop odd-kw)
        ;; There have been an odd number of arguments, so signal an error.
        (irc-begin-block odd-kw)
        (irc-intrinsic-invoke-if-landing-pad-or-call "cc_oddKeywordException"
                                                     (list *current-function-description*))
        (irc-unreachable))
      ;; Loop starts; welcome hell
      (irc-begin-block kw-loop)
      (let ((top-param-phis nil) (top-suppliedp-phis nil)
            (new-blocks nil) (old-blocks nil)
            (nargs-remaining (irc-phi %size_t% 2 "nargs-remaining"))
            (sbkw (irc-phi %i1% 2 "seen-bad-keyword"))
            (bad-keyword (irc-phi %t*% 2 "bad-keyword")))
        (irc-phi-add-incoming nargs-remaining nremaining start)
        (irc-phi-add-incoming sbkw (jit-constant-false) start)
        (irc-phi-add-incoming bad-keyword undef start)
        (do-keys (key)
          (let ((var-phi (irc-phi %t*% 2 (core:bformat nil "%s-top" (string key)))))
            (push var-phi top-param-phis)
            ;; If we're paying attention to :allow-other-keys, track it specially
            ;; and initialize it to NIL.
            (cond ((and (not lambda-list-aokp) (eq key :allow-other-keys))
                   (irc-phi-add-incoming var-phi false start)
                   (setf allow-other-keys var-phi))
                  (t (irc-phi-add-incoming var-phi undef start))))
          (let ((suppliedp-phi (irc-phi %t*% 2 (core:bformat nil "%s-suppliedp-top" (string key)))))
            (push suppliedp-phi top-suppliedp-phis)
            (irc-phi-add-incoming suppliedp-phi false start)))
        (setf top-param-phis (nreverse top-param-phis)
              top-suppliedp-phis (nreverse top-suppliedp-phis))
        ;; Are we done?
        (let ((zerop (irc-icmp-eq nargs-remaining (irc-size_t 0))))
          (irc-cond-br zerop after matching))
        (irc-begin-block matching)
        ;; Start matching keywords
        (let ((key-arg (calling-convention-args.va-arg cc))
              (value-arg (calling-convention-args.va-arg cc)))
          (do* ((cur-key (cdr keyargs) (cddddr cur-key))
                (key (car cur-key) (car cur-key))
                (suppliedp-phis top-suppliedp-phis (cdr suppliedp-phis))
                (suppliedp-phi (car suppliedp-phis) (car suppliedp-phis)))
               ((endp cur-key))
            (multiple-value-bind (new-block old-block)
                (compile-one-key-test key key-arg suppliedp-phi kw-loop-continue false)
              (push new-block new-blocks) (push old-block old-blocks)))
          (setf new-blocks (nreverse new-blocks) old-blocks (nreverse old-blocks))
          ;; match failure - as usual, works through phi
          (irc-branch-to-and-begin-block unknown-kw)
          (irc-br kw-loop-continue)
          ;; Go around again. And do most of the actual work in phis.
          (irc-begin-block kw-loop-continue)
          (let ((npreds (1+ (* 2 nkeys)))) ; two for each key, plus one for unknown-kw.
            (let ((bot-sbkw (irc-phi %i1% npreds "seen-bad-keyword-bottom"))
                  (bot-bad-keyword (irc-phi %t*% npreds "bad-keyword-bottom")))
              ;; Set up the top to use these.
              (irc-phi-add-incoming sbkw bot-sbkw kw-loop-continue)
              (irc-phi-add-incoming bad-keyword bot-bad-keyword kw-loop-continue)
              ;; If we're coming from unknown-kw, store that.
              (irc-phi-add-incoming bot-sbkw (jit-constant-true) unknown-kw)
              (irc-phi-add-incoming bot-bad-keyword key-arg unknown-kw)
              ;; If we're coming from a match block, don't change anything.
              (dolist (new-block new-blocks)
                (irc-phi-add-incoming bot-sbkw sbkw new-block)
                (irc-phi-add-incoming bot-bad-keyword bad-keyword new-block))
              (dolist (old-block old-blocks)
                (irc-phi-add-incoming bot-sbkw sbkw old-block)
                (irc-phi-add-incoming bot-bad-keyword bad-keyword old-block)))
            ;; OK now the actual keyword values.
            (do* ((var-new-blocks new-blocks (cdr var-new-blocks))
                  (var-new-block (car var-new-blocks) (car var-new-blocks))
                  (top-param-phis top-param-phis (cdr top-param-phis))
                  (top-param-phi (car top-param-phis) (car top-param-phis))
                  (top-suppliedp-phis top-suppliedp-phis (cdr top-suppliedp-phis))
                  (top-suppliedp-phi (car top-suppliedp-phis) (car top-suppliedp-phis)))
                 ((endp var-new-blocks))
              (let ((var-phi (irc-phi %t*% npreds))
                    (suppliedp-phi (irc-phi %t*% npreds)))
                ;; fix up the top part to take values from here
                (irc-phi-add-incoming top-param-phi var-phi kw-loop-continue)
                (irc-phi-add-incoming top-suppliedp-phi suppliedp-phi kw-loop-continue)
                ;; If coming from unknown-kw we keep our values the same.
                (irc-phi-add-incoming var-phi top-param-phi unknown-kw)
                (irc-phi-add-incoming suppliedp-phi top-suppliedp-phi unknown-kw)
                ;; All new-blocks other than this key's stick with what they have.
                (dolist (new-block new-blocks)
                  (cond ((eq var-new-block new-block)
                         ;; Here, however, we get the new values
                         (irc-phi-add-incoming var-phi value-arg new-block)
                         (irc-phi-add-incoming suppliedp-phi true new-block))
                        (t
                         (irc-phi-add-incoming var-phi top-param-phi new-block)
                         (irc-phi-add-incoming suppliedp-phi top-suppliedp-phi new-block))))
                ;; All old-blocks stick with what they have.
                (dolist (old-block old-blocks)
                  (irc-phi-add-incoming var-phi top-param-phi old-block)
                  (irc-phi-add-incoming suppliedp-phi top-suppliedp-phi old-block))))))
        (let ((dec (irc-sub nargs-remaining (irc-size_t 2))))
          (irc-phi-add-incoming nargs-remaining dec kw-loop-continue))
        (irc-br kw-loop)
        ;; Loop over.
        (irc-begin-block after)
        ;; If we hit a bad keyword, and care, signal an error.
        (unless lambda-list-aokp
          (let ((aok-check (irc-basic-block-create "aok-check"))
                (kw-assigns (irc-basic-block-create "kw-assigns")))
            (irc-cond-br sbkw aok-check kw-assigns)
            (irc-begin-block aok-check)
            (irc-intrinsic-invoke-if-landing-pad-or-call
             "cc_ifBadKeywordArgumentException"
             ;; aok was initialized to NIL, regardless of the suppliedp, so this is ok.
             (list allow-other-keys bad-keyword *current-function-description*))
            (irc-br kw-assigns)
            (irc-begin-block kw-assigns)))
        (do* ((top-param-phis top-param-phis (cdr top-param-phis))
              (top-param-phi (car top-param-phis) (car top-param-phis))
              (top-suppliedp-phis top-suppliedp-phis (cdr top-suppliedp-phis))
              (top-suppliedp-phi (car top-suppliedp-phis) (car top-suppliedp-phis))
              (cur-key (cdr keyargs) (cddddr cur-key))
              (key (car cur-key) (car cur-key))
              (var (caddr cur-key) (caddr cur-key))
              (suppliedp (cadddr cur-key) (cadddr cur-key)))
             ((endp cur-key))
          (when (or (not (eq key :allow-other-keys)) lambda-list-aokp aok-parameter-p)
            (funcall *argument-out* top-param-phi var)
            (funcall *argument-out* top-suppliedp-phi suppliedp)))))))

(defun compile-general-lambda-list-code (reqargs 
					 optargs 
					 rest-var
                                         varest-p
					 key-flag 
					 keyargs 
					 allow-other-keys
					 calling-conv
                                         &key argument-out (safep t))
  (cmp-log "Entered compile-general-lambda-list-code%N")
  (let* ((*argument-out* argument-out)
         (nargs (calling-convention-nargs calling-conv))
         (nreq (car reqargs))
         (nopt (car optargs))
         (nfixed (+ nreq nopt)))
    (unless (zerop nreq)
      (when safep
        (compile-error-if-not-enough-arguments nreq nargs))
      (compile-required-arguments reqargs calling-conv))
    (let (;; NOTE: Sometimes we don't actually need these.
          We could save miniscule time by not generating.
          (iNIL (irc-nil)) (iT (irc-t)))
      (if (or rest-var key-flag)
          ;; We have &key and/or &rest, so parse with that expectation.
          ;; Specifically, we have to get a variable for how many arguments are left after &optional.
          (let ((nremaining
                  (if (zerop nopt)
                      ;; With no optional arguments it's trivial.
                      (irc-sub nargs (irc-size_t nreq) "nremaining")
                      ;; Otherwise
                      (compile-optional-arguments optargs nreq calling-conv iNIL iT))))
            ;; Note that we don't need to check for too many arguments here.
            (when rest-var
              (compile-rest-argument rest-var varest-p nremaining calling-conv))
            (when key-flag
              (compile-key-arguments keyargs (or allow-other-keys (not safep))
                                     nremaining calling-conv iNIL iT)))
          ;; We don't have &key or &rest, but we might still have &optional.
          (progn
            (unless (zerop nopt)
              ;; Return value of compile-optional-arguments is unneeded-
              ;; we could use it in the error check to save a subtraction, though.
              (compile-optional-arguments optargs nreq calling-conv iNIL iT))
            (when safep
              (compile-error-if-too-many-arguments nfixed nargs)))))))

(defun compile-only-reg-and-opt-arguments (reqargs optargs cc &key argument-out (safep t))
  (let ((register-args (calling-convention-register-args cc))
        (nargs (calling-convention-nargs cc))
        (nreq (car reqargs))
        (nopt (car optargs)))
    ;; FIXME: It would probably be nicer to generate one switch such that not-enough-arguments
    ;; goes to an error block and too-many goes to another. Then we'll only have one test on
    ;; the argument count. LLVM might reduce it to that anyway, though.
    
    ;; Required arguments
    (when (> nreq 0)
      (when safep
        (compile-error-if-not-enough-arguments nreq nargs))
      (dolist (req (cdr reqargs))
        ;; we POP the register-args so that the optionals below won't use em.
        (funcall argument-out (pop register-args) req)))
    ;; Optional arguments. Code is mostly the same as compile-optional-arguments (FIXME).
    (if (> nopt 0)
        (let* ((npreds (1+ nopt))
               (undef (irc-undef-value-get %t*%))
               (true (irc-t))
               (false (irc-nil))
               (default (irc-basic-block-create "enough-for-optional"))
               (assn (irc-basic-block-create "optional-assignments"))
               (after (irc-basic-block-create "argument-parsing-done"))
               (sw (irc-switch nargs default nopt))
               (var-phis nil) (suppliedp-phis nil))
          (irc-begin-block assn)
          (dotimes (i nopt)
            (push (irc-phi %t*% npreds) var-phis)
            (push (irc-phi %t*% npreds) suppliedp-phis))
          (do ((cur-opt (cdr optargs) (cdddr cur-opt))
               (var-phis var-phis (cdr var-phis))
               (suppliedp-phis suppliedp-phis (cdr suppliedp-phis)))
               ((endp cur-opt))
            (funcall argument-out (car suppliedp-phis) (second cur-opt))
            (funcall argument-out (car var-phis) (first cur-opt)))
          (irc-br after)
          ;; Each case
          (dotimes (i nopt)
            (let* ((opti (+ i nreq))
                   (blck (irc-basic-block-create (core:bformat nil "supplied-%d-arguments" opti))))
              (llvm-sys:add-case sw (irc-size_t opti) blck)
              (do ((var-phis var-phis (cdr var-phis))
                   (suppliedp-phis suppliedp-phis (cdr suppliedp-phis))
                   (registers register-args (cdr registers))
                   (optj nreq (1+ optj)))
                  ((endp var-phis))
                (cond ((< optj opti) ; enough arguments
                       (irc-phi-add-incoming (car suppliedp-phis) true blck)
                       (irc-phi-add-incoming (car var-phis) (car registers) blck))
                      (t ; nope
                       (irc-phi-add-incoming (car suppliedp-phis) false blck)
                       (irc-phi-add-incoming (car var-phis) undef blck))))
              (irc-begin-block blck) (irc-br assn)))
          ;; Default
          ;; Just use a register for each argument
          ;; We have to use another block because compile-error-etc does an invoke
          ;; and generates more blocks.
          (let ((default-cont (irc-basic-block-create "enough-for-optional-continued")))
            (do ((var-phis var-phis (cdr var-phis))
                 (suppliedp-phis suppliedp-phis (cdr suppliedp-phis))
                 (registers register-args (cdr registers)))
                ((endp var-phis))
              (irc-phi-add-incoming (car suppliedp-phis) true default-cont)
              (irc-phi-add-incoming (car var-phis) (car registers) default-cont))
            (irc-begin-block default)
            ;; Test for too many arguments
            (compile-error-if-too-many-arguments (+ nreq nopt) nargs)
            (irc-branch-to-and-begin-block default-cont)
            (irc-br assn)
            ;; and, done.
            (irc-begin-block after)))
        ;; No optional arguments, so not much to do
        (compile-error-if-too-many-arguments nreq nargs))))

(defun process-cleavir-lambda-list (lambda-list)
  ;; We assume that the lambda list is in its correct format:
  ;; 1) required arguments are lexical locations.
  ;; 2) optional arguments are (<lexical location> <lexical location>)
  ;; 3) keyword arguments are (<symbol> <lexical location> <lexical location>)
  ;; This lets us cheap out on parsing, except &rest and &allow-other-keys.
  (cmp-log "process-cleavir-lambda-list lambda-list -> %s%N" lambda-list)
  (let (required optional rest-type rest key aok-p key-flag
        (required-count 0) (optional-count 0) (key-count 0))
    (dolist (item lambda-list)
      (case item
        ((&optional) #|ignore|#)
        ((&key) (setf key-flag t))
        ((&rest core:&va-rest) (setf rest-type item))
        ((&allow-other-keys) (setf aok-p t))
        (t (if (listp item)
               (cond ((= (length item) 2)
                      ;; optional
                      (incf optional-count)
                      ;; above, we expect (location -p whatever)
                      ;; though it's specified as (var init -p)
                      ;; FIX ME
                      (push (first item) optional)
                      (push (second item) optional)
                      (push nil optional))
                     (t ;; key, assumedly
                      (incf key-count)
                      (push (first item) key)
                      (push (first item) key)
                      ;; above, we treat this as being the location,
                      ;; even though from process-lambda-list it's
                      ;; the initform.
                      ;; This file needs work FIXME.
                      (push (second item) key)
                      (push (third item) key)))
               ;; nonlist; we picked off lambda list keywords, so it's an argument.
               (cond (rest-type
                      ;; we've seen a &rest lambda list keyword, so this must be that
                      (setf rest item))
                     ;; haven't seen anything, it's required
                     (t (incf required-count)
                        (push item required)))))))
    (values (cons required-count (nreverse required))
            (cons optional-count (nreverse optional))
            rest
            key-flag
            (cons key-count (nreverse key))
            aok-p
            nil ; aux-p; unused here
            (if (eq rest-type 'core:&va-rest) t nil))))

;;; compile-lambda-list-code
;;; You must provide the following lambdas
;;;   alloca-size_t (label) that allocas a size_t slot in the current function
;;;   alloca-vaslist (label) that allocas a vaslist slot in the current function
;;;   translate-datum (datum) that translates a datum into an alloca in the current function
(defun compile-lambda-list-code (lambda-list calling-conv
                                 &key argument-out (safep t))
  (cmp-log "About to process-cleavir-lambda-list lambda-list: %s%N" lambda-list)
  (multiple-value-bind (reqargs optargs rest-var key-flag keyargs allow-other-keys unused-auxs varest-p)
      (process-cleavir-lambda-list lambda-list)
    (cmp-log "About to calling-convention-use-only-registers%N")
    (cmp-log "    reqargs -> %s%N" reqargs)
    (cmp-log "    optargs -> %s%N" optargs)
    (cmp-log "    keyargs -> %s%N" keyargs)
    (cmp-log "    outputs -> %s%N" outputs)
    (if (calling-convention-use-only-registers calling-conv)
        ;; Special cases (foo) (foo x) (foo x y) (foo x y z)  - passed in registers
        (progn
          (compile-only-reg-and-opt-arguments reqargs optargs calling-conv
                                              :argument-out argument-out
                                              :safep safep))
        ;; Test for
        ;; (x &optional y)
        ;; (x y &optional z)
        (progn
          (compile-general-lambda-list-code reqargs 
                                            optargs 
                                            rest-var
                                            varest-p
                                            key-flag 
                                            keyargs 
                                            allow-other-keys
                                            calling-conv
                                            :argument-out argument-out
                                            :safep safep)))))

(defun maybe-alloc-cc-setup (cleavir-lambda-list debug-on)
  "Maybe allocate slots in the stack frame to handle the calls
   depending on what is in the lambda-list (&rest, &key etc) and debug-on.
   Return a calling-convention-configuration object that describes what was allocated.
   See the bclasp version in lambdalistva.lsp."
  ;; Parse a cleavir lambda list a little bit.
  ;; Form is (req+ [&optional (o -p)+] [&rest r] [&key (:k k -p)+] [&allow-other-keys])
  (let ((nreq 0) (nopt 0) (req-opt-only t)
        (state nil))
    (dolist (item cleavir-lambda-list)
      (cond ((eq item '&optional)
             (if (eq state '&optional)
                 (progn (setf req-opt-only nil) ; dupe &optional; just mark as general
                        (return))
                 (setf state '&optional)))
            ((member item lambda-list-keywords)
             (setf req-opt-only nil)
             (return))
            (t (if (eq state '&optional)
                   (incf nopt)
                   (incf nreq)))))
    ;; Currently if nargs <= +args-in-registers+ required arguments and (null debug-on)
    ;;      then can optimize and use the arguments in registers directly
    ;;  If anything else then allocate space to spill the registers
    ;;
    ;; Currently only cases:
    ;; (w)
    ;; (w x)
    ;; (w x y)
    ;; (w x y z)  up to the +args-in-registers+
    ;;    can use only registers
    ;; In the future add support for required + optional 
    ;; (x &optional y)
    ;; (x y &optional z) etc
    (let (;; If only required or optional arguments are used
          ;; and the sum of required and optional arguments is less
          ;; than the number +args-in-register+ then use only registers.
          (may-use-only-registers (and req-opt-only (<= (+ nreq nopt) +args-in-registers+))))
      (if (and may-use-only-registers (null debug-on))
           (make-calling-convention-configuration
            :use-only-registers t)
           (make-calling-convention-configuration
            :use-only-registers may-use-only-registers ; if may-use-only-registers then debug-on is T and we could use only registers
            :register-save-area* (irc-register-save-area :label "register-save-area"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup the calling convention
;;
(defun setup-calling-convention (arguments
                                 &key debug-on rest-alloc cleavir-lambda-list
                                   ignore-arguments)
  (let ((setup (maybe-alloc-cc-setup cleavir-lambda-list debug-on)))
    (let ((cc (initialize-calling-convention arguments
                                             setup
                                             :rewind t
                                             :rest-alloc rest-alloc
                                             :cleavir-lambda-list cleavir-lambda-list)))
      (calling-convention-args.va-start cc)
      cc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; bclasp 
;;;


(defun bclasp-map-lambda-list-symbols-to-indices (cleavir-lambda-list)
  (multiple-value-bind (reqs opts rest key-flag keys aok-p auxargs-dummy va-rest-p)
      (process-cleavir-lambda-list cleavir-lambda-list)
    ;; Create the register lexicals using allocas
    (let (bindings
          (index -1))
      (cmp-log "Processing reqs -> %s%N" reqs)
      (dolist (req (cdr reqs))
        (cmp-log "Add req %s%N" req)
        (push (cons req (incf index)) bindings))
      (cmp-log "Processing opts -> %s%N" opts)
      (do* ((cur (cdr opts) (cdddr cur))
            (opt (car cur) (car cur))
            (optp (cadr cur) (cadr cur)))
           ((null cur))
        (cmp-log "Add opt %s %s%N" opt optp)
        (push (cons opt (incf index)) bindings)
        (push (cons optp (incf index)) bindings))
      (cmp-log "Processing rest -> %s%N" rest)
      (when rest
        (push (cons rest (incf index)) bindings))
      (cmp-log "Processing keys -> %s%N" keys)
      (do* ((cur (cdr keys) (cddddr cur))
            (key (third cur) (third cur))
            (keyp (fourth cur) (fourth cur)))
           ((null cur))
        (push (cons key (incf index)) bindings)
        (push (cons keyp (incf index)) bindings))
      (nreverse bindings))))

(defun bclasp-compile-lambda-list-code (fn-env callconv &key (safep t))
  (let ((cleavir-lambda-list (calling-convention-cleavir-lambda-list callconv)))
    (cmp-log "Entered bclasp-compile-lambda-list-code%N")
    (let* ((output-bindings (bclasp-map-lambda-list-symbols-to-indices cleavir-lambda-list))
           (new-env (irc-new-unbound-value-environment-of-size
                     fn-env
                     :number-of-arguments (length output-bindings)
                     :label "arguments-env")))
      (irc-make-value-frame-set-parent new-env (length output-bindings) fn-env)
      (cmp-log "output-bindings: %s%N" output-bindings)
      (mapc (lambda (ob)
              (cmp-log "Adding to environment: %s%N" ob)
              (core:value-environment-define-lexical-binding new-env (car ob) (cdr ob)))
            output-bindings)
      (cmp-log "register-environment contents -> %s%N" new-env)
      (compile-lambda-list-code
       cleavir-lambda-list
       callconv
       :safep safep
       :argument-out (lambda (value datum)
                       (let* ((info (assoc datum output-bindings))
                              (symbol (car info))
                              (index (cdr info))
                              (ref (codegen-lexical-var-reference symbol 0 index new-env new-env)))
                         (irc-store value ref))))
      new-env)))
