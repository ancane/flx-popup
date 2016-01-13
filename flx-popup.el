;;; flx-popup --- flex mathing support for popup
;; Version: 0.1
;; Package-Requires: ((cl-lib "0.3") (dash "2.12.1") (flx "0.6.1") (popup "0.5.3"))
;; This file is not part of GNU Emacs.
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'flx)
(require 'popup)

(defcustom flx-popup-use-faces t
  "Use `flx-highlight-face' to indicate characters contributing to best score."
  :group 'popup)

;;;###autoload
(defun flx-popup-match (query items)
  "Flx filtering function.
QUERY - search string
ITEMS - popup menu items list"
  (let ((flex-result (flx-popup--re-match query items)))
    (let* ((matches (cl-loop for string in flex-result
                             for score = (flx-score string query flx-file-cache)
                             if score
                             collect (cons string score)
                             into matches
                             finally return matches)))
      (flx-popup--decorate
       (sort matches (lambda (x y) (> (cadr x) (cadr y))))
       ))))

(defun flx-popup--re-match (query items)
  "Apply QUERY to the ITEMS list."
  (if (zerop (length query))
      items
    (let* ((case-fold-search nil) ;; case sensitive
           (re (flx-popup--query-to-regexp query)))
      (-filter
       (lambda (item) (string-match re item))
       items))))

(defun flx-popup--query-to-regexp (query)
  "Convert QUERY to flx style case folding regexp."
  (let* ((breakdown-str (mapcar
                         (lambda (c)
                           (apply 'string c (when (= (downcase c) c)
                                              (list (upcase c)))))
                         query))
         (re (concat (format "[%s]" (nth 0 breakdown-str))
                     (mapconcat
                      (lambda (c) (format "[^%s]*[%s]" c c))
                      (cdr breakdown-str) ""))))
    re))

(defun flx-popup--decorate (things)
  "Highlight imenu items matching search query.
THINGS - (menu-item . score)."
  (if flx-popup-use-faces
      (let ((decorate-count (min ido-max-prospects
                                 (length things))))
        (nconc
         (cl-loop for thing in things
                  for i from 0 below decorate-count
                  collect (flx-popup--propertize thing))
         (mapcar 'car (nthcdr decorate-count things))))
    (mapcar 'car things)))

(defun flx-popup--propertize (thing)
  "Apply text properties according to score.
Preserve existing item properties.
THING - (object . score)."
  (let* ((item (car thing))
         (item-value (popup-item-value item))
         (item-face (popup-item-face item))
         (item-mouse-face (popup-item-mouse-face item))
         (item-selection-face (popup-item-selection-face item))
         (item-sublist (popup-item-sublist item))
         (item-document (popup-item-document item))
         (item-symbol (popup-item-symbol item))
         (item-summary (popup-item-summary item))
         (flx-propertized (flx-propertize item (cdr thing))))
    (popup-make-item flx-propertized
                     :value item-value
                     :face item-face
                     :mouse-face item-mouse-face
                     :selection-face item-selection-face
                     :sublist item-sublist
                     :document item-document
                     :symbol item-symbol
                     :summary item-summary)))

(provide 'flx-popup)

;;; flx-popup.el ends here
