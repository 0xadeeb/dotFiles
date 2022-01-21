;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Adeeb HS"
      user-mail-address "adeeb.hs1@gmail.com"
      display-line-numbers-type 'relative
      confirm-kill-emacs nil
)

(setq doom-theme 'doom-dracula)

(setq doom-font (font-spec :family "monospace" :size 25 :weight 'semi-light)
       doom-variable-pitch-font (font-spec :family "sans" :size 23))

(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
  :hook prog-mode org-mode) ;; Enables fira-code-mode automatically for programming  and org major modes

(use-package all-the-icons
  :if (display-graphic-p))
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  )             ;; Icons for dired
(setq doom-themes-treemacs-theme "doom-colors")
(setq +ligatures-extras-in-modes '(haskell-mode org-mode))

(use-package highlight-indent-guides
  :ensure t
  :commands highlight-indent-guides-mode
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        ;;highlight-indent-guides-character ?\❯
   )
 )

(setq org-directory "~/org/")
(after! org
  (setq org-startup-folded t
        org-pretty-entities t
        ))
(map! :leader
      :desc "Org babel tangle" "m B" #'org-babel-tangle)
(after! org
  (setq
        org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        ;;org-superstar-item-bullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
        org-log-done 'time
        org-hide-emphasis-markers t
        )
  )

;; Shorten some text
;; (defun my/org-mode/load-prettify-symbols ()
;;   (interactive)
;;   (setq prettify-symbols-alist
;;     '(("#+begin_src" . ?)
;;       ("#+BEGIN_SRC" . ?)
;;       ("#+end_src" . ?)
;;       ("#+END_SRC" . ?)
;;       ("#+begin_example" . ?)
;;       ("#+BEGIN_EXAMPLE" . ?)
;;       ("#+end_example" . ?)
;;       ("#+END_EXAMPLE" . ?)
;;       ("#+header:" . ?)
;;       ("#+HEADER:" . ?)
;;       ("#+results:" . ?)
;;       ("#+RESULTS:" . ?)
;;       ("#+call:" . ?)
;;       ("#+CALL:" . ?)
;;       (":PROPERTIES:" . ?)
;;       (":properties:" . ?)
;;       ))
;;   (prettify-symbols-mode 1))
;; (add-hook 'org-mode-hook 'my/org-mode/load-prettify-symbols)

(setq key-chord-two-keys-delay 0.15)
(key-chord-define evil-insert-state-map "fj" 'evil-normal-state)
(key-chord-mode 1)

(defhydra hydra-window-size (:timeout 5)
"Resize window"
("=" evil-window-increase-width "increaseW")
("-" evil-window-decrease-width "decreaseW")
("+" evil-window-increase-height "increaseH")
("_" evil-window-decrease-height "decreaseH")
("f" nil "finished" :exit t))

(map! :leader
    (:prefix ("w". "window")
    :desc "Resize current window" "c" #'hydra-window-size/body))

(map! :leader
      :desc "M-x" ";" #'execute-extended-command)

(map! :leader
      :desc "Eval Expression" ":" #'eval-expression)

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :after lsp
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :after lsp
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  )

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-pyright
  :after lsp
  :ensure t
  :init
  (setq lsp-pyright-multi-root nil)
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(after! haskell-mode
  (set-ligatures! 'haskell-mode
    :lambda        "\\"
    :composition   "."
    :null          "()"
    :int           "Int"
    :float         "Double"
    :str           "String"
    :bool          "Bool"
    :in            "`elem`"
    :not-in        "`notElem`"
    :union         "`union`"
    :intersect     "`intersect`"
    :or            "||"
    :and           "&&"
    :for           "forall"
    :sum           "sum"
    :product       "product"
    )
   )
(plist-put! +ligatures-extra-symbols
            :sum        "Σ"
            :product    "Ⲡ"
            )

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(setq
 ;; browse-url-browser-function 'eww-browse-url                    ; Use eww as the default browser
 shr-use-fonts  nil                                             ; No special fonts
 shr-use-colors nil                                             ; No colours
 shr-indentation 2                                              ; Left-side margin
 shr-width 70                                                   ; Fold text to 70 columns
 shr-image-animate nil                                          ; Amination switched off
 shr-inhibit-images t                                           ; Images are switched off
 )
(defun my/eww-toggle-images ()
  "Toggle whether images are loaded and reload the current page from cache."
  (interactive)
  (setq-local shr-inhibit-images (not shr-inhibit-images))
  (eww-reload t)
  (message "Images are now %s"
           (if shr-inhibit-images "off" "on")))

(use-package dashboard
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  ;;(setq dashboard-startup-banner "~/.doom.d/doom-emacs-dash.png")  ;; use custom image as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (projects . 5)))

  :custom
        (dashboard-set-init-info t)
        (dashboard-set-navigator t)
        (dashboard-projects-backend 'projectile)

:config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((bookmarks . "book"))))

(add-to-list 'recentf-exclude "/.emacs.d/.local/etc/workspaces/autosave")
(add-to-list 'projectile-ignored-projects "~/.emacs.d")
