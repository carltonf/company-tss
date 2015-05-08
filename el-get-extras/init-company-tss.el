(require 'company)
(customize-set-variable 'tss-completion-engine 'company)
(add-to-list 'company-backends #'company-tss)
