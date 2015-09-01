# defproject

A list macro for managing project dir-local files in the style of use-package.

### Example

```elisp
(defproject minerva-NEX
  :path "/home/kotfic/kitware/projects/NEX/src/OpenGeoscience/minerva/"
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
   (flycheck-flake8rc . (concat girder-dir "tests/flake8.cfg"))))
```
