;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Adeeb HS"
      user-mail-address "adeeb.hs1@gmail.com")

(setq display-line-numbers-type 'relative)

 (setq doom-font (font-spec :family "monospace" :size 25 :weight 'semi-light)
       doom-variable-pitch-font (font-spec :family "sans" :size 23))

(setq doom-theme 'doom-dracula)

(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "x")) ;; List of ligatures to turn off
  :hook prog-mode) ;; Enables fira-code-mode automatically for programming major modes

(setq org-directory "~/org/")
(setq org-hide-emphasis-markers t)
(after! org
  (setq org-startup-folded t))

(setq key-chord-two-keys-delay 0.15)
(key-chord-define evil-insert-state-map "fj" 'evil-normal-state)
(key-chord-mode 1)

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
