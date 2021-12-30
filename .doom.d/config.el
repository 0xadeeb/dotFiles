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
  :custom (fira-code-mode-disabled-ligatures '("[]" "x")) ;; List of ligatures to turn off
  :hook prog-mode) ;; Enables fira-code-mode automatically for programming major modes

(use-package all-the-icons
  :if (display-graphic-p))
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  )             ;; Icons for dired
(setq doom-themes-treemacs-theme "doom-colors")

(setq org-directory "~/org/")
(setq org-hide-emphasis-markers t)
(after! org
  (setq org-startup-folded t))

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

;; (after! company
;;   (setq +lsp-company-backends '(company-tabnine :separate company-capf company-yasnippet))
;;   (setq company-show-numbers t)
;;   (setq company-idle-delay 0)
;; )
;; (require 'company-tabnine)
;; (add-to-list 'company-backends #'company-tabnine)
;; (setq company-idle-delay 0)  ;; Trigger completion immediately.
;; (setq company-show-numbers t) ;; Number the candidates (use M-1, M-2 etc to select completions).

(setq
 ;; browse-url-browser-function 'eww-browse-url                    ; Use eww as the default browser
 shr-use-fonts  nil                                             ; No special fonts
 shr-use-colors nil                                             ; No colours
 shr-indentation 2                                              ; Left-side margin
 shr-width 70                                                   ; Fold text to 70 columns
 shr-image-animate nil                                          ; Amination switched off
 )
(defun my/eww-toggle-images ()
  "Toggle whether images are loaded and reload the current page from cache."
  (interactive)
  (setq-local shr-inhibit-images (not shr-inhibit-images))
  (eww-reload t)
  (message "Images are now %s"
           (if shr-inhibit-images "off" "on")))

;;(define-key eww-mode-map (kbd "I") #'my/eww-toggle-images)
;;(define-key eww-link-keymap (kbd "I") #'my/eww-toggle-images)

;; minimal rendering by default
(setq-default shr-inhibit-images t)   ; toggle with `I`
(setq-default shr-use-fonts nil)      ; toggle with `F`



(use-package dashboard
  :ensure t
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Let's go evil!")
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  ;;(setq dashboard-startup-banner "~/.doom.d/doom-emacs-dash.png")  ;; use custom image as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (projects . 5)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((bookmarks . "book"))))

(add-to-list 'recentf-exclude "/.emacs.d/.local/etc/workspaces/autosave")
