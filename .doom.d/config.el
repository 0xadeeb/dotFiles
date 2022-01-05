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
;; Shorten some text
(dolist (cell '(("#+name:" . "✎")
                ("#+NAME:" . "✎")
                ("#+begin_src" . "➤")
                ("#+begin_example" . "➤")
                ("#+end_src" . "⏹")
                ("#+end_example" . "⏹")
                ("#+RESULTS:" . "🠋")))
  (add-to-list 'prettify-symbols-alist cell))

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
        org-superstar-item-bullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
        org-log-done 'time
        org-hide-emphasis-markers t
        )
  )

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
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-pyright
  :ensure t
  :init
  (setq lsp-pyright-multi-root nil)
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
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

(defun my/eww-display+ (buf _alist)
  (let ((w (or (window-in-direction 'right)
               (window-in-direction 'left)
               (window-in-direction 'below)
               (window-in-direction 'above)
               (split-window-horizontally))))
    (set-window-buffer w buf)
    w))

(push `(,(rx "*eww*")
        (my/eww-display+))
      display-buffer-alist)

(use-package dashboard
  :init      ;; tweak dashboard config before loading it
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
        (dashboard-navigator-buttons
        `(((,(all-the-icons-fileicon "brain" :height 1.1 :v-adjust 0.0)
                "Brain" "Knowledge base"
                (lambda (&rest _) (browse-url "http://localhost:8080"))))
            ;; ((,(all-the-icons-material "public" :height 1.1 :v-adjust 0.0)
            ;;     "Homepage" "Personal website"
            ;;     (lambda (&rest _) (browse-url "https://chrishayward.xyz"))))
            ;; ((,(all-the-icons-faicon "university" :height 1.1 :v-adjust 0.0)
            ;;     "Athabasca" "Univeristy login"
            ;;     (lambda (&rest _) (browse-url "https://login.athabascau.ca/cas/login"))))
            ;; ((,(all-the-icons-faicon "book" :height 1.1 :v-adjust 0.0)
            ;;     "Bookshelf" "Vitalsource bookshelf"
            ;;     (lambda (&rest _) (browse-url "https://online.vitalsource.com"))))
            ))
:config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((bookmarks . "book"))))

(add-to-list 'recentf-exclude "/.emacs.d/.local/etc/workspaces/autosave")
