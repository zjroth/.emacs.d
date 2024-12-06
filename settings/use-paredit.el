;; My keybindings for paredit

;; (defhydra hydra-paredit (:hint nil)
;;       "
;;  Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
;; ------------------------------------------------------------------------------------------------------------------------
;;  [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
;;  [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
;;  [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
;;  [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit

;;                                   [<left>]  slurp
;;                                   [<right>] barf"
;;       ;; Moving
;;       ("a" sp-beginning-of-sexp)
;;       ("e" sp-end-of-sexp)
;;       ("f" sp-forward-sexp)
;;       ("b" sp-backward-sexp)
;;       ("n" sp-down-sexp)
;;       ("N" sp-backward-down-sexp)
;;       ("p" sp-up-sexp)
;;       ("P" sp-backward-up-sexp)

;;       ;; Slurping & barfing
;;       ("h" sp-backward-slurp-sexp)
;;       ("H" sp-backward-barf-sexp)
;;       ("l" sp-forward-slurp-sexp)
;;       ("L" sp-forward-barf-sexp)

;;       ;; ("<up>" paredit-raise-sexp)
;;       ;; ("<right>" paredit-forward-slurp-sexp)
;;       ;; ("<left>" paredit-forward-barf-sexp)
;;       ;; ("<left>" paredit-backward-slurp-sexp)
;;       ;; ("<right>" paredit-backward-barf-sexp)
;;       ;; ("<backspace>" 'paredit-splice-sexp-killing-backward)
;;       ;; ("t" 'transpose-sexps)
;;       ("<right>" paredit-forward-slurp-sexp)
;;       ("<left>" paredit-forward-barf-sexp)

;;       ;; Wrapping
;;       ("R" sp-rewrap-sexp)
;;       ("u" sp-unwrap-sexp)
;;       ("U" sp-backward-unwrap-sexp)
;;       ("(" sp-wrap-round)
;;       ("{" sp-wrap-curly)
;;       ("[" sp-wrap-square)

;;       ;; Sexp juggling
;;       ("S" sp-split-sexp)
;;       ("s" sp-splice-sexp)
;;       ("r" sp-raise-sexp)
;;       ("j" sp-join-sexp)
;;       ("t" sp-transpose-sexp)
;;       ("A" sp-absorb-sexp)
;;       ("E" sp-emit-sexp)
;;       ("o" sp-convolute-sexp)

;;       ;; Destructive editing
;;       ("c" sp-change-inner :exit t)
;;       ("C" sp-change-enclosing :exit t)
;;       ("k" sp-kill-sexp)
;;       ("K" sp-backward-kill-sexp)
;;       ("w" sp-copy-sexp)

;;       ("q" nil)
;;       ("g" nil))

;; (add-to-list 'hydra-select-by-mode-list
;;              '(clojure-mode . hydra-paredit/body))

;; (use-package paredit)

;; (defun paredit-wrap-round-from-behind ()
;;   (interactive)
;;   (forward-sexp -1)
;;   (paredit-wrap-round)
;;   (insert " ")
;;   (forward-char -1))

;; (defun setup-paredit-for-mode-map (mode-map)
;;   (define-key mode-map (kbd "s-<up>") 'paredit-raise-sexp)
;;   (define-key mode-map (kbd "s-<right>") 'paredit-forward-slurp-sexp)
;;   (define-key mode-map (kbd "s-<left>") 'paredit-forward-barf-sexp)
;;   (define-key mode-map (kbd "s-S-<left>") 'paredit-backward-slurp-sexp)
;;   (define-key mode-map (kbd "s-S-<right>") 'paredit-backward-barf-sexp)
;;   (define-key mode-map (kbd "s-8") 'paredit-wrap-round)
;;   (define-key mode-map (kbd "s-9") 'paredit-wrap-round-from-behind)
;;   (define-key mode-map (kbd "s-<backspace>") 'paredit-splice-sexp-killing-backward)
;;   (define-key mode-map (kbd "s-t") 'transpose-sexps))

;; (eval-after-load "lisp-mode" '(setup-paredit-for-mode-map emacs-lisp-mode-map))
;; (eval-after-load "clojure-mode" '(setup-paredit-for-mode-map clojure-mode-map))







;; (use-package lispy
;;   ;; :config
;;   ;; lispy-set-key-theme
;; )




(use-package smartparens
  :ensure t
  :after hydra
  :init
  (progn
    ;; https://github.com/abo-abo/hydra/wiki/smartparens
    (defhydra hydra-smartparens (:hint nil)
      "
 Moving^^^^                       Slurp & Barf^^^^                 Wrapping^^^^^^           Sexp juggling^^^^              Destructive^^       Miscellaneous
-------^^^^-----------------------------------^^^^-------------------------^^^^^^------------------------^^^^-------------------------^^--------------------------
 [_a_] beginning  [_n_] down      [_h_] bw slurp  [_→_] into next  [_R_]   rewrap    ^^^^   [_S_] split   [_t_] transpose  [_c_] change inner  [_w_]   copy
 [_e_] end        [_N_] bw down   [_H_] bw barf   [_←_] into prev  [_u_]   unwrap    ^^^^   [_s_] splice  [_A_] absorb     [_C_] change outer  [_/_/_?_] undo/redo
 [_f_] forward    [_p_] up        [_l_] slurp     ^^               [_U_]   bw unwrap ^^^^   [_r_] raise   [_E_] emit       [_k_] kill          [_g_]   quit
 [_b_] backward   [_P_] bw up     [_L_] barf      ^^               [_(__{__[_] wrap (){}[]  [_j_] join    [_o_] convolute  [_K_] bw kill       [_q_]   quit"
      ;; Moving
      ("a" sp-beginning-of-sexp)
      ("e" sp-end-of-sexp)
      ("f" sp-forward-sexp)
      ("b" sp-backward-sexp)
      ("n" sp-down-sexp)
      ("N" sp-backward-down-sexp)
      ("p" sp-up-sexp)
      ("P" sp-backward-up-sexp)

      ;; Slurping & barfing
      ("h" sp-backward-slurp-sexp)
      ("H" sp-backward-barf-sexp)
      ("l" sp-forward-slurp-sexp)
      ("L" sp-forward-barf-sexp)
      ("<right>" sp-add-to-next-sexp)
      ("<left>" sp-add-to-previous-sexp)

      ;; Wrapping
      ("R" sp-rewrap-sexp)
      ("u" sp-unwrap-sexp)
      ("U" sp-backward-unwrap-sexp)
      ("(" sp-wrap-round)
      ("{" sp-wrap-curly)
      ("[" sp-wrap-square)

      ;; Sexp juggling
      ("S" sp-split-sexp)
      ("s" sp-splice-sexp)
      ("r" sp-raise-sexp)
      ("j" sp-join-sexp)
      ("t" sp-transpose-sexp)
      ("A" sp-absorb-sexp)
      ("E" sp-emit-sexp)
      ("o" sp-convolute-sexp)

      ;; Destructive editing
      ("c" sp-change-inner :exit t)
      ("C" sp-change-enclosing :exit t)
      ("k" sp-kill-sexp)
      ("K" sp-backward-kill-sexp)
      ("w" sp-copy-sexp)

      ;; Miscellaneous
      ("/" undo-tree-undo)
      ("?" undo-tree-redo)
      ("q" nil)
      ("g" nil)))

  :config
  (add-to-list 'hydra-select-by-mode-list
               '(clojure-mode . hydra-smartparens/body))

  :bind (("M-[" . hydra-smartparens/body)))






(provide 'use-paredit)
