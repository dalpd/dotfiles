(setq make-backup-files nil) 
(setq auto-save-default nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(column-number-mode +1)
(global-hl-line-mode +1)
(global-display-line-numbers-mode +1)

(add-to-list 'default-frame-alist
             '(font . "Victor Mono Medium-12"))

;; (load "~/.emacs.d/my-noexternals.el")

(require 'package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#303030" "#ff4b4b" "#d7ff5f" "#fce94f" "#5fafd7" "#d18aff" "#afd7ff" "#c6c6c6"])
 '(custom-safe-themes
   '("13d20048c12826c7ea636fbe513d6f24c0d43709a761052adbca052708798ce3" default))
 '(inhibit-startup-screen t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(kaolin-themes ample-theme markdown-mode racket-mode haskell-mode)))

(package-initialize)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Forces the messages to 0, and kills the *Messages* buffer - thus disabling
;; it on startup.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

(require 'kaolin-themes)
(load-theme 'kaolin-valley-light t)
