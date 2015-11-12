# defproject

An  elisp macro for managing project dir-local files in the style of use-package. Defproject is designed to provide a simplified interface to the ```dir-locals-set-class-variables``` and ```dir-locals-set-directory-class``` functions provided by Emacs. These functions permit users to set directory local variables in their configurations rather than by adding a ```.dir-locals``` file to a directory.

Defproject provides several advantages over a workflow using only ```dir-locals-set-class-variables``` and ```dir-locals-set-directory-class.``` Specifically,  directories are checked for existence before class variables are assigned,  variables defined in the mode properties of defproject are automatically added to the ```safe-local-variable-values``` list,  and the value of each of the mode specific dir-local variables is evaluated before being assigned. This last feature allows dynamic construction of paths and other values simplifying the readability of the project definition code.

Variables defined in the ```:vars``` property are accessible anywhere within the mode variable definitions and may rely on each other.  In addition to these the value of the ```:path``` property is available as ```project-path```

### Example

```elisp
(defproject girder-NEX
  :path "/home/kotfic/kitware/projects/NEX/src/girder/girder/"
  :vars ((base "/home/kotfic/kitware/projects/NEX/src/")
	 (build-dir (concat base "build/girder/"))
	 (girder-dir (concat base "girder/girder/")))
  :nil
  ((projectile-project-test-cmd . (concat "cd " build-dir " && ctest -j8")))
  :js2-mode
  ((flycheck-jshintrc . (concat build-dir "tests/minerva_jshint.cfg"))
   (flycheck-jscsrc . (concat build-dir "tests/minerva_jsstyle.cfg" )))
  :python-mode
  ((eval . (venv-workon "NEX"))
   (flycheck-python-flake8-executable . "/home/kotfic/.venvs/NEX/bin/flake8")
   (flycheck-flake8rc . (concat girder-dir "tests/flake8.cfg")))
  :init
  ((prodigy-define-service
     :name "NEX Girder"
     :command "python"
     :args '("-m" "girder" "-p" "8081")
     :cwd project-path
     :init (lambda ()
	     (venv-workon "NEX"))
     :tags '(nex girder)
     :on-output (lambda (&rest args)
		  (let ((output (plist-get args :output))
			(service (plist-get args :service)))
		    (when (s-matches? "ENGINE Bus STARTED" output)
		      (prodigy-set-status service 'ready))))))
```
