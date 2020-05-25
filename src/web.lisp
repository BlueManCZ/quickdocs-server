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
  (:import-from :quickdocs-updater.readme
                :pandoc)
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
;; Clearing session

(defun clear-session ()
  (remhash :id *session*)
  (remhash :service-id *session*)
  (remhash :login *session*)
  (remhash :name *session*)
  (remhash :html-url *session*)
  (remhash :avatar-url *session*)
  (remhash :next *session*))

;;
;; Browsing functions

(defun filter-system (name systems)
  (mapcan (lambda (x)
            (and (equal (system-name x) name)
                 (list x)))
    systems))

(defun get-system-details (systems &optional package-name symbol-name)
  (mapcar
    (lambda (system)
      (list :name (system-name system)
            :description (system-description system)
            :packages
            (mapcan (lambda (package)
                      (setf (getf package :symbols)
                            (mapcan (lambda (symbol)
                                      (setf (getf symbol :type)
                                            (string-downcase (getf symbol :type)))
                                      (setf (getf symbol :name)
                                            (quickdocs-serializer:symb-name
                                             (getf symbol :name)))
                                      ;(format t "symbol: ~A|" (getf symbol :name))
                                      (if symbol-name
                                        (and (string-equal (getf symbol :name) symbol-name) (list symbol))
                                        ;(and (> (length (getf symbol :name)) 10) (list symbol))
                                        (list symbol)))
                                    (getf package :symbols)))
                      (setf (getf package :external-symbols)
                            (remove-if-not
                              (lambda (symbol)
                                (getf symbol :externalp))
                              (getf package :symbols)))
                      ;(format t "package: ~A|" (getf package :name))
                      (if package-name
                        (and (equal (getf package :name) package-name) (list package))
                        (list package)))
                    (system-extracted-info-packages
                     (system-extracted-info system)))))
    systems))

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"
          (list :ql-dist-version (preference "ql-dist-version")
                :app-env (appenv))))

@route GET "/login"
(defun login (&key |next|)
  (if (gethash :id *session*)
      (redirect "/account"))
  (if |next|
    (redirect (format nil "/login/github?next=~A" |next|))
    (redirect "/login/github")))

@route GET "/login/github"
(defun login-github (&key |code| |next|)
  (let ((oauth-url "https://github.com/login/oauth/")
        (client-id "6b258428f6838326e122"))
    (when |next| (setf (gethash :next *session*) |next|))
    (if (not |code|)
     (redirect (format nil "~Aauthorize?client_id=~A" oauth-url client-id))
     (let* ((url "https://api.github.com/user")
            (client-secret "533aee14b023f36b560f4aeb7aada966943ca712")
            (resp (dex:post (format nil "~Aaccess_token?client_id=~A&client_secret=~A&code=~A" oauth-url client-id client-secret |code|)))
            (token (second (uiop:split-string (car (uiop:split-string resp :separator "&")) :separator "=")))
            (data (dex:get url :headers `(("Authorization" . ,(format nil "token ~A" token)))))
            (json (jonathan:parse data)))

      (let ((service-id (getf json :|id|))
            (login (getf json :|login|))
            (name (getf json :|name|))
            (company (getf json :|company|))
            (location (getf json :|location|))
            (html-url (getf json :|html_url|))
            (avatar-url (getf json :|avatar_url|)))
         (setf (gethash :service-id *session*) service-id)
         (setf (gethash :login *session*) login)
         (when name (setf (gethash :name *session*) name))
         (setf (gethash :html-url *session*) html-url)
         (setf (gethash :avatar-url *session*) avatar-url)

         (format t "~A" (gethash :name *session*))

         (if (retrieve-one
               (select :*
                 (from :users)
                 (where (:= :service_id service-id))))
            (execute
              (update :users
                (set= :login login
                      :user_name name
                      :html_url html-url
                      :avatar_url avatar-url
                      :company company
                      :location location)
                (where (:= :service_id service-id))))
            (execute
              (insert-into :users
                (set= :service 1
                      :service_id service-id
                      :login login
                      :user_name name
                      :html_url html-url
                      :avatar_url avatar-url
                      :company company
                      :location location))))

         (setf (gethash :id *session*)
               (second (retrieve-one
                         (select :id
                           (from :users)
                           (where (:= :service_id service-id)))))))

      (if (gethash :next *session*)
        (redirect (gethash :next *session*))
        (redirect "/account"))))))

@route GET "/account"
(defun account ()
  (if (not (gethash :id *session*))
      (redirect "/login")
    (let ((examples (retrieve-all
                      (select :*
                        (from :examples)
                        (where (:= :user_id (gethash :id *session*)))))))

      (render #P"user.html" `(:examples ,examples)))))

@route GET "/logout"
(defun logout ()
  (clear-session)
  (redirect "/"))

@route GET "/users"
(defun users ()
  (let ((result (retrieve-all
                  (select :*
                    (from :users)))))
    (render #P"users.html" `(:users ,result))))

@route GET "/docs/"
(defun docs (&key page)
  (render #P"docs/index.html"))

@route GET "/docs/:page"
(defun docs (&key page)
  (format t "Haha ~A" page)
  (cond
    ((string-equal page "about") (render #P"docs/about.html"))
    ((string-equal page "examples") (render #P"docs/examples.html"))
    ((eq NIL page) (render #P"docs/index.html"))
    (t (throw-code 404))))

@route GET "/:project-name/"
(defun project-page (&key project-name |force-raw|)
  (let ((project (retrieve-project project-name)))
    (unless project
      (throw-code 404))

    (let ((dependencies
            (mapcar (lambda (dep)
                      (list :name (project-name dep)
                            :description (project-description dep)))
                    (project-dependencies project)))
          (dependees
            (mapcar (lambda (dep)
                      (list :name (project-name dep)
                            :description (project-description dep)))
                    (project-dependees project))))
      (render #P"project.html"
              `(:project-name ,project-name
                :ql-dist-version ,(project-release-version project)
                :homepage     ,(project-homepage-url* project)
                :repos-url    ,(project-repos-url project)
                :archive-url  ,(project-archive-url project)
                :archive-name ,(car (last (uiop:split-string (project-archive-url project) :separator "/")))
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

    (format t "~A" (mapcar (lambda (x) (system-name x)) (project-systems project)))

    (render #P"api.html"
            `(:project-name ,project-name
              :ql-dist-version ,(project-ql-dist-version project)
              :homepage    ,(project-homepage-url* project)
              :repos-url   ,(project-repos-url project)
              :archive-url ,(project-archive-url project)
              :systems     ,(get-system-details (project-systems project))
              :api         1))))

@route GET "/:project-name/api/:system/:package/:symbol"
(defun symbol-detail (&key project-name system package symbol)
  (let ((project (retrieve-project project-name)))
    (unless project
      (throw-code 404))

    (let ((systems (filter-system system (project-systems project)))
          (examples (retrieve-all
                      (select (:examples.id :user_id :converted :avatar_url :login :user_name)
                        (from :examples)
                        (left-join :users :on (:= :examples.user_id :users.id))
                        (where (:and (:= :project_name project-name)
                                     (:= :project_system system)
                                     (:= :project_package package)
                                     (:= :project_symbol symbol)))))))
      (unless systems
        (throw-code 404))

      (format t "*** ~A" (quri:url-encode symbol))

      (render #P"symbol.html"
              `(:project-name ,project-name
                :ql-dist-version ,(project-ql-dist-version project)
                :homepage    ,(project-homepage-url* project)
                :repos-url   ,(project-repos-url project)
                :archive-url ,(project-archive-url project)
                :examples    ,examples
                :url         ,(format nil "/~A/api/~A/~A/~A" project-name system package symbol)
                :systems     ,(get-system-details systems package symbol)
                :api         1)))))

@route POST "/:project-name/api/:system/:package/:symbol"
(defun symbol-detail-post (&key project-name system package symbol _parsed)
  (if (not (gethash :id *session*))
      (redirect "/login")
    (let ((param (reduce (lambda (ac it) (append ac (list (intern (string-upcase (car it))) (cdr it)))) _parsed :initial-value '())))
      ;(format t "Parametry: ")
      ;(format t "~S" (convert-readme (make-string-input-stream (getf param (intern (string-upcase "text"))))))))
      (execute
        (insert-into :examples
          (set= :user_id (gethash :id *session*)
                :project_name project-name
                :project_system system
                :project_package package
                :project_symbol symbol
                :markdown (getf param (intern (string-upcase "text")))
                :converted (pandoc (make-string-input-stream (getf param (intern (string-upcase "text")))) :from "markdown-raw_html"))))))
  (redirect (format nil "/~A/api/~A/~A/~A" project-name system package symbol)))

@route POST "/remove-example/:id"
(defun remove-comment (&key id)
  (let ((user-id (retrieve-one
                    (select :user_id
                      (from :examples)
                      (where (:= :id id))))))
    (if (and (gethash :id *session*) (getf user-id :user-id) (= (getf user-id :user-id) (gethash :id *session*)))
      (execute
        (delete-from :examples
          (where (:= :id id))))
      "You have no rights to perform this action.")))

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
  (render #P"_errors/404.html"))
