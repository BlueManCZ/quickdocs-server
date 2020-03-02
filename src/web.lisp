(in-package :cl-user)
(defpackage quickdocs-server.web
  (:use :cl
        :caveman2
        :quickdocs-server.config
        :quickdocs-server.view
        :quickdocs-server.db
        :datafly
        :sxql
        :quickdocs-database)
  (:import-from :quickdocs-server.search
                :search-projects
                :download-stats)
  (:import-from :lack.component
                :call)
  (:import-from :datafly
                :*connection*
                :connect-cached)
  (:export :*web*))
(in-package :quickdocs-server.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(defmethod lack.component:call :around ((app <web>) env)
  (let ((datafly:*connection*
          (apply #'datafly:connect-cached (cdr (assoc :maindb (config :databases))))))
    (prog1
        (call-next-method)
      ;; Clear caches
      (setf quickdocs-server.search::*ql-download-stats* nil
            quickdocs-database.preference::*preference* nil))))
(clear-routing-rules *web*)


;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"
          (list :ql-dist-version (preference "ql-dist-version")
                :app-env (appenv))))

@route GET "/login/github"
(defun login-github (&key |code|)
  (let* ((resp (dex:post (format nil "https://github.com/login/oauth/access_token?client_id=6b258428f6838326e122&client_secret=533aee14b023f36b560f4aeb7aada966943ca712&code=~A" |code|)))
         (token (cadr (uiop:split-string (car (uiop:split-string resp :separator "&")) :separator "=")))
         (data (dex:get "https://api.github.com/user" :headers `(("Authorization" . ,(format nil "token ~A" token))))))
    (render #P"user.html"
            (jonathan:parse data))))

@route GET "/:project-name/"
(defun project-page (&key project-name |force-raw|)
  (let ((project (retrieve-project project-name)))
    (unless project
      (throw-code 404))

    (let ((dependencies nil
            #|(mapcar (lambda (dep)
                      (list :name (project-name dep)
                            :description (project-description dep)))
                    (project-dependencies project))|#)
          (dependees nil
            #|(mapcar (lambda (dep)
                      (list :name (project-name dep)
                            :description (project-description dep)))
                    (project-dependees project))|#))
      (render #P"project.html"
              `(:project-name ,project-name
                :ql-dist-version ,(project-release-version project)
                :homepage    ,(project-homepage-url* project)
                :repos-url   ,(project-repos-url project)
                :archive-url ,(project-archive-url project)
                :readme ,(let ((readme (project-readme project)))
                           (when readme
                             (list :converted (unless |force-raw| (project-readme-converted readme))
                                   :raw (project-readme-raw readme))))
                :authors ,(project-authors project)
                :maintainers ,(project-maintainers project)
                :licenses ,(project-licenses project)
                :categories ,(project-categories project)
                :dependencies-count ,(length dependencies)
                :dependencies ,dependencies
                :dependees-count ,(length dependees)
                :dependees ,dependees)))))

@route GET "/:project-name/api"
(defun project-api-reference (&key project-name)
  (let ((project (retrieve-project project-name)))
    (unless project
      (throw-code 404))

    (render #P"api.html"
            `(:project-name ,project-name
              :ql-dist-version ,(project-ql-dist-version project)
              :homepage    ,(project-homepage-url* project)
              :repos-url   ,(project-repos-url project)
              :archive-url ,(project-archive-url project)
              :systems ,(mapcar (lambda (system)
                                  (list :name (system-name system)
                                        :description (system-description system)
                                        :packages
                                        (mapcar (lambda (package)
                                                  (setf (getf package :symbols)
                                                        (mapcar (lambda (symbol)
                                                                  (setf (getf symbol :type)
                                                                        (string-downcase (getf symbol :type)))
                                                                  (setf (getf symbol :name)
                                                                        (quickdocs-serializer:symb-name
                                                                         (getf symbol :name)))
                                                                  symbol)
                                                                (getf package :symbols)))
                                                  (setf (getf package :external-symbols)
                                                        (remove-if-not
                                                         (lambda (symbol)
                                                           (getf symbol :externalp))
                                                         (getf package :symbols)))
                                                  package)
                                                (system-extracted-info-packages
                                                 (system-extracted-info system)))))
                                (project-systems project))))))

@route GET "/search"
(defun search-page (&key |q|)
  (let ((projects (search-projects |q| (preference "ql-dist-version"))))
    (render #P"search.html"
            (list
             :projects (mapcar (lambda (project)
                                 (list :name (project-name project)
                                       :release-version (project-release-version project)
                                       :description (project-description project)
                                       :categories (project-categories project)
                                       :systems (length (project-systems project))
                                       :archive-url (project-archive-url project)
                                       :homepage (project-homepage-url* project)
                                       :download-count (gethash (project-name project)
                                                                (quickdocs-server.search:download-stats))))
                               projects)
             :len (length projects)
             :query |q|))))

@route GET "/:project-name"
(defun redirect-to-project (&key project-name |force-raw|)
  (redirect (if |force-raw| (format nil "/~A/?force-raw=~A" (quri:url-encode project-name) |force-raw|)
                (format nil "/~A/" (quri:url-encode project-name)))
            301)
  "redirecting")

@route GET "/badge/:project-name.svg"
(defun quicklisp-badge (&key project-name)
  (let ((project (and project-name
                      (retrieve-project project-name))))

    (setf (response-headers *response*)
          (list :content-type "image/svg+xml"))

    (if project
        (format nil
                "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"137\" height=\"20\"><linearGradient id=\"b\" x2=\"0\" y2=\"100%\"><stop offset=\"0\" stop-color=\"#bbb\" stop-opacity=\".1\"/><stop offset=\"1\" stop-opacity=\".1\"/></linearGradient><mask id=\"a\"><rect width=\"137\" height=\"20\" rx=\"3\" fill=\"#fff\"/></mask><g mask=\"url(#a)\"><path fill=\"#555\" d=\"M0 0h61v20H0z\"/><path fill=\"#007ec6\" d=\"M61 0h76v20H61z\"/><path fill=\"url(#b)\" d=\"M0 0h137v20H0z\"/></g><g fill=\"#fff\" text-anchor=\"middle\" font-family=\"DejaVu Sans,Verdana,Geneva,sans-serif\" font-size=\"11\"><text x=\"30.5\" y=\"15\" fill=\"#010101\" fill-opacity=\".3\">Quicklisp</text><text x=\"30.5\" y=\"14\">Quicklisp</text><text x=\"98\" y=\"15\" fill=\"#010101\" fill-opacity=\".3\">~A</text><text x=\"98\" y=\"14\">~:*~A</text></g></svg>"
                (project-release-version project))
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"144\" height=\"20\"><linearGradient id=\"b\" x2=\"0\" y2=\"100%\"><stop offset=\"0\" stop-color=\"#bbb\" stop-opacity=\".1\"/><stop offset=\"1\" stop-opacity=\".1\"/></linearGradient><mask id=\"a\"><rect width=\"144\" height=\"20\" rx=\"3\" fill=\"#fff\"/></mask><g mask=\"url(#a)\"><path fill=\"#555\" d=\"M0 0h61v20H0z\"/><path fill=\"#9f9f9f\" d=\"M61 0h83v20H61z\"/><path fill=\"url(#b)\" d=\"M0 0h144v20H0z\"/></g><g fill=\"#fff\" text-anchor=\"middle\" font-family=\"DejaVu Sans,Verdana,Geneva,sans-serif\" font-size=\"11\"><text x=\"30.5\" y=\"15\" fill=\"#010101\" fill-opacity=\".3\">Quicklisp</text><text x=\"30.5\" y=\"14\">Quicklisp</text><text x=\"101.5\" y=\"15\" fill=\"#010101\" fill-opacity=\".3\">not available</text><text x=\"101.5\" y=\"14\">not available</text></g></svg>")))


;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
