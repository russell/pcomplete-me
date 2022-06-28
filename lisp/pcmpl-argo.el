;;; pcmpl-argo.el --- Argo Workflow Completion       -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Russell Sim

;; Author: Russell Sim <russell.sim@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'pcomplete-me)
(require 'pcmpl-kubectl)

(defconst pcmpl-argo--override-flags '(:kubeconfig :cluster :user :context :namespace :server))

(defun pcmpl-argo--override-args (context)
  "Convert an alist of overrides into CLI flags.

CONTEXT is a context alist."
  (mapcar (lambda (e) (format "--%s=%s" (string-trim (symbol-name (car e)) ":") (cdr e)))
          (seq-filter (lambda (e) (member (car e)
                                          pcmpl-argo--override-flags)) context)))


(defun pcmpl-argo--complete-resource-of (kind &optional context)
  "Return a list of all resources of a type.

KIND is the type of resorce to complete.  CONTEXT is context
alist."
  (let* ((context-args (pcmpl-argo--override-args (or context pcmpl-me--context)))
         (args `(,kind "list" ,@context-args "--output=name")))
    (split-string (funcall #'pcmpl-me--call "argo" args))))


(defun pcmpl-argo--complete-workflow (status &optional context)
  "Return a list of workflows filtered by STATUS.

CONTEXT is context alist."
  (let* ((context-args (pcmpl-argo--override-args (or context pcmpl-me--context)))
         (args `("list" ,@context-args "--output=name" "--status" ,status)))
    (split-string (funcall #'pcmpl-me--call "argo" args))))

(defun pcmpl-argo--complete-workflow-pods (&optional context)
  "Return a list of all workflow pods.

CONTEXT is context alist."
  (let* ((context-args (pcmpl-argo--override-args (or context pcmpl-me--context)))
         (template "{{ range .items  }}{{ .metadata.name }} {{ end }}")
         (args `("pods" ,@context-args
                 "--selector=workflows.argoproj.io/workflow"
                 "--output=template"
                 "--template" ,template)))
    (split-string (funcall #'pcmpl-me--call pcmpl-me-kubectl-command args))))



(pcmpl-me-set-completion-widget
 :argo-workflow (lambda () (pcmpl-argo--complete-workflow "")))
(pcmpl-me-set-completion-widget
 :argo-workflow-to-run (lambda () (pcmpl-argo--complete-workflow "Running,Pending")))
(pcmpl-me-set-completion-widget
 :argo-workflow-running (lambda () (pcmpl-argo--complete-workflow "Running")))
(pcmpl-me-set-completion-widget
 :argo-workflow-failed (lambda () (pcmpl-argo--complete-workflow "Failed")))
(pcmpl-me-set-completion-widget
 :argo-template (lambda () (pcmpl-argo--complete-resource-of "template")))
(pcmpl-me-set-completion-widget
 :argo-cluster-template (lambda () (pcmpl-argo--complete-resource-of "cluster-template")))
(pcmpl-me-set-completion-widget
 :argo-cron (lambda () (pcmpl-argo--complete-resource-of "cron")))

(defconst pcmpl-argo-lint-output
  '("pretty" "simple"))

(pcmpl-me-global-args argo
  :flags
  ;; (rs//replace-sexp (rs//add-null-completers (rs//group-flags (rs//bash-complete-argo-flags "argo"))))
  '(("--argo-base-href" "--argo-base-href=" :null)
    ("--argo-http1")
    ("--argo-server" "--argo-server=" "-s" :null)
    ("--as" "--as=" :null)
    ("--as-group" "--as-group=" :null)
    ("--as-uid" "--as-uid=")
    ("--certificate-authority" "--certificate-authority=" :files)
    ("--client-certificate" "--client-certificate=" :files)
    ("--client-key" "--client-key=" :files)
    ("--cluster" "--cluster=" :kubernetes-cluster)
    ("--context" "--context=" :kubernetes-context)
    ("--gloglevel" "--gloglevel=" :null)
    ("--header" "--header=" "-H" :null)
    ("--insecure-skip-tls-verify")
    ("--insecure-skip-verify" "-k")
    ("--instanceid" "--instanceid=" :null)
    ("--kubeconfig" "--kubeconfig=" :files)
    ("--loglevel" "--loglevel=" :list "debug" "info" "warn" "error")
    ("--namespace" "--namespace=" "-n" :kubernetes-namespace)
    ("--password" "--password=" :null)
    ("--request-timeout" "--request-timeout=" :null)
    ("--secure" "-e")
    ("--server" "--server=" :null)
    ("--tls-server-name" "--tls-server-name=")
    ("--token" "--token=" :null)
    ("--user" "--user=" :kubernetes-user)
    ("--username" "--username=" :null)
    ("--verbose" "-v")))


(pcmpl-me-command (argo)
  :inherit-global-flags t
  :subcommands
  '("archive" "auth" "cluster-template" "completion" "cron" "delete"
    "executor-plugin" "get" "lint" "list" "logs" "node" "resubmit"
    "resume" "retry" "server" "stop" "submit" "suspend"
    "template" "terminate" "version" "wait" "watch"))

(pcmpl-me-command (argo archive)
  :inherit-global-flags t :subcommands
  '("delete" "get" "list" "list-label-keys" "list-label-values"))

(pcmpl-me-command (argo archive delete)
  :inherit-global-flags t)

(pcmpl-me-command (argo archive list-label-keys)
  :inherit-global-flags t)

(pcmpl-me-command (argo archive list-label-values)
  :inherit-global-flags t
  :flags
  '(("--selector=" "-l" :null)))

(pcmpl-me-command (argo archive get)
  :inherit-global-flags t
  :flags
  '(("--output" "--output=" "-o" :null)))

(pcmpl-me-command (argo archive list)
  :inherit-global-flags t
  :flags
  '(("--chunk-size" "--chunk-size=" :null)
    ("--output" "--output=" "-o" :null)
    ("--selector" "--selector=" "-l" :null)))

(pcmpl-me-command (argo auth)
  :inherit-global-flags t
  :subcommands
  '("token"))

(pcmpl-me-command (argo auth token)
  :inherit-global-flags t)

(pcmpl-me-command (argo cluster-template)
  :inherit-global-flags t
  :subcommands
  '("create" "delete" "get" "lint" "list"))

(pcmpl-me-command (argo cluster-template create)
  :inherit-global-flags t
  :flags
  '(("--output" "--output=" "-o" :null)
    ("--strict")))

(pcmpl-me-command (argo cluster-template delete)
  :inherit-global-flags t
  :flags
  '(("--all"))
  :subcommands (pcmpl-me-get-completion-widget :argo-cluster-template))

(pcmpl-me-command (argo cluster-template get)
  :inherit-global-flags t
  :flags
  '(("--output" "--output=" "-o" :null))
  :subcommands (pcmpl-me-get-completion-widget :argo-cluster-template))


;; TODO weird duplicate command here
(pcmpl-me-command (argo cluster-template lint)
  :inherit-global-flags t
  :flags
  '(("--strict")
    ("--output" "--output=" "-o" :list pcmpl-argo-lint-output)))

(pcmpl-me-command (argo cluster-template list)
  :inherit-global-flags t
  :flags
  '(("--output" "--output=" "-o" :null)))

(pcmpl-me-command (argo completion)
  :inherit-global-flags t)

(pcmpl-me-command (argo cron)
  :inherit-global-flags t
  :subcommands
  '("create" "delete" "get" "lint" "list" "resume" "suspend"))

(pcmpl-me-command (argo cron create)
  :inherit-global-flags t :flags
  '(("--entrypoint" "--entrypoint=" :null)
    ("--generate-name" "--generate-name=" :null)
    ("--labels" "--labels=" "-l" :null)
    ("--name" "--name=" :null)
    ("--output" "--output=" "-o" :null)
    ("--parameter-file" "--parameter-file=" "-f" :null)
    ("--parameter" "--parameter=" "-p" :null)
    ("--schedule" "--schedule=" :null)
    ("--serviceaccount" "--serviceaccount=" :null)
    ("--strict")))

(pcmpl-me-command (argo cron delete)
  :inherit-global-flags t
  :flags
  '(("--all"))
  :subcommands (pcmpl-me-get-completion-widget :argo-cron))

(pcmpl-me-command (argo cron get)
  :inherit-global-flags t
  :flags
  '(("--output" "--output=" "-o" :null))
  :subcommands (pcmpl-me-get-completion-widget :argo-cron))

(pcmpl-me-command (argo cron lint)
  :inherit-global-flags t
  :flags
  '(("--strict")
    ("--output" "--output=" "-o" :list pcmpl-argo-lint-output)))

(pcmpl-me-command (argo cron list)
  :inherit-global-flags t
  :flags
  '(("--all-namespaces" "-A")
    ("--output" "--output=" "-o" :null)))

(pcmpl-me-command (argo cron resume)
  :inherit-global-flags t
  :subcommands (pcmpl-me-get-completion-widget :argo-cron))

(pcmpl-me-command (argo cron suspend)
  :inherit-global-flags t
  :subcommands (pcmpl-me-get-completion-widget :argo-cron))

(pcmpl-me-command (argo delete)
  :inherit-global-flags t :flags
  '(("--all")
    ("--all-namespaces" "-A")
    ("--completed")
    ("--dry-run")
    ("--field-selector" "--field-selector=")
    ("--older" "--older=" :null)
    ("--prefix" "--prefix=" :null)
    ("--resubmitted")
    ("--selector" "--selector=" "-l" :null))
  :subcommands (pcmpl-me-get-completion-widget :argo-workflow))

(pcmpl-me-command (argo executor-plugin)
  :inherit-global-flags t
  :subcommands '("build"))

(pcmpl-me-command (argo executor-plugin build)
  :inherit-global-flags t)

(pcmpl-me-command (argo get)
  :inherit-global-flags t :flags
  '(("--no-color")
    ("--no-utf8")
    ("--node-field-selector" "--node-field-selector=" :null)
    ("--output" "--output=" "-o" :null)
    ("--status" "--status=" :null))
  :subcommands (pcmpl-me-get-completion-widget :argo-workflow))

(pcmpl-me-command (argo lint)
  :inherit-global-flags t
  :flags
  '(("--kinds" "--kinds=" :list "workflows" "workflowtemplates"
     "cronworkflows" "clusterworkflowtemplates")
    ("--offline" )
    ("--output" "--output=" "-o" :list pcmpl-argo-lint-output)
    ("--strict"))
  :subcommands (pcmpl-me-get-completion-widget :files))

(pcmpl-me-command (argo list)
  :inherit-global-flags t :flags
  '(("--all-namespaces" "-A")
    ("--chunk-size" "--chunk-size=" :null)
    ("--completed")
    ("--field-selector" "--field-selector=" :null)
    ("--no-headers")
    ("--older" "--older=" :null)
    ("--output" "--output=" "-o" :null)
    ("--prefix" "--prefix=" :null)
    ("--resubmitted")
    ("--running")
    ("--selector" "--selector=" "-l" :null)
    ("--since" "--since=" :null)
    ("--status" "--status=" :null)))

(pcmpl-me-command (argo logs)
  :inherit-global-flags t :flags
  '(("--container" "--container=" "-c" :null)
    ("--follow" "-f")
    ("--grep" "--grep=" :null)
    ("--no-color")
    ("--previous" "-p")
    ("--selector" "--selector=" "-l" :null)
    ("--since" "--since=" :null)
    ("--since-time" "--since-time=" :null)
    ("--tail" "--tail=" :null)
    ("--timestamps")))

(pcmpl-me-command (argo node)
  :inherit-global-flags t :flags
  '(("--message" "--message=" "-m" :null)
    ("--node-field-selector" "--node-field-selector=" :null)
    ("--phase" "--phase=" :null)
    ("--output-parameter" "--output-parameter=" "-p" :null)))

(pcmpl-me-command (argo resubmit)
  :inherit-global-flags t :flags
  '(("--log")
    ("--field-selector" "--field-selector=")
    ("--memoized")
    ("--output" "--output=" "-o" :null)
    ("--priority" "--priority=" :null)
    ("--selector" "--selector=" "-l")
    ("--wait" "-w")
    ("--watch"))
  :subcommands (pcmpl-me-get-completion-widget :argo-workflow))

(pcmpl-me-command (argo resume)
  :inherit-global-flags t
  :flags
  '(("--node-field-selector" "--node-field-selector=" :null))
  :subcommands (pcmpl-me-get-completion-widget :argo-workflow-running))

(pcmpl-me-command (argo retry)
  :inherit-global-flags t
  :flags
  '(("--log")
    ("--field-selector" "--field-selector=")
    ("--node-field-selector" "--node-field-selector=" :null)
    ("--output" "--output=" "-o" :null)
    ("--restart-successful")
    ("--selector" "--selector=" "-l")
    ("--wait" "-w")
    ("--watch"))
  :subcommands (pcmpl-me-get-completion-widget :argo-workflow-failed))

(pcmpl-me-command (argo server)
  :inherit-global-flags t
  :flags
  '(("--access-control-allow-origin" "--access-control-allow-origin=" :null)
    ("--auth-mode" "--auth-mode=" :null)
    ("--basehref" "--basehref=" :null)
    ("--browser" "-b")
    ("--configmap" "--configmap=" :null)
    ("--event-async-dispatch")
    ("--event-operation-queue-size" "--event-operation-queue-size=" :null)
    ("--event-worker-count" "--event-worker-count=" :null)
    ("--hsts")
    ("--log-format" "--log-format=" :null)
    ("--managed-namespace" "--managed-namespace=" :null)
    ("--namespaced")
    ("--port" "--port=" "-p" :null)
    ("--sso-namespace" "--sso-namespace=" :null)
    ("--x-frame-options" "--x-frame-options=" :null)))

(pcmpl-me-command (argo stop)
  :inherit-global-flags t
  :flags
  '(("--dry-run")
    ("--field-selector" "--field-selector=" )
    ("--message" "--message=" :null)
    ("--node-field-selector" "--node-field-selector=" :null)
    ("--selector" "--selector=" "-l")))

(pcmpl-me-command (argo submit)
  :inherit-global-flags t
  :flags
  '(("--dry-run")
    ("--entrypoint" "--entrypoint=" :null)
    ("--from" "--from=" :null)
    ("--generate-name" "--generate-name=" :null)
    ("--labels" "--labels=" "-l" :null)
    ("--log")
    ("--name" "--name=" :null)
    ("--node-field-selector" "--node-field-selector=" :null)
    ("--output" "--output=" "-o" :null)
    ("--parameter-file" "--parameter-file=" "-f" :null)
    ("--parameter" "--parameter=" "-p" :null)
    ("--priority" "--priority=" :null)
    ("--scheduled-time" "--scheduled-time=" :null)
    ("--server-dry-run")
    ("--serviceaccount" "--serviceaccount=" :null)
    ("--status" "--status=" :null)
    ("--strict")
    ("--wait" "-w")
    ("--watch")))

(pcmpl-me-command (argo suspend)
  :inherit-global-flags t
  :subcommands (pcmpl-me-get-completion-widget :argo-workflow-to-run))

(pcmpl-me-command (argo template)
  :inherit-global-flags t
  :subcommands
  '("create" "delete" "get" "lint" "list"))

(pcmpl-me-command (argo template create)
  :inherit-global-flags t
  :flags
  '(("--output" "--output=" "-o" :null)
    ("--strict")))

(pcmpl-me-command (argo template delete)
  :inherit-global-flags t
  :flags
  '(("--all"))
  :subcommands (pcmpl-me-get-completion-widget :argo-template))

(pcmpl-me-command (argo template get)
  :inherit-global-flags t
  :flags
  '(("--output" "--output=" "-o" :null))
  :subcommands (pcmpl-me-get-completion-widget :argo-template))

(pcmpl-me-command (argo template lint)
  :inherit-global-flags t
  :flags
  '(("--strict")
    ("--output" "--output=" "-o" :list pcmpl-argo-lint-output))
  :subcommands (pcmpl-me-get-completion-widget :files))

(pcmpl-me-command (argo template list)
  :inherit-global-flags t
  :flags
  '(("--all-namespaces" "-A")
    ("--output" "--output=" "-o" :list "wide" "name")))

(pcmpl-me-command (argo terminate)
  :inherit-global-flags t
  :flags
  '(("--dry-run")
    ("--field-selector" "--field-selector=" )
    ("--selector" "--selector=" "-l"))
  :subcommands (pcmpl-me-get-completion-widget :argo-workflow-to-run))

(pcmpl-me-command (argo version)
  :inherit-global-flags t
  :flags
  '(("--short")))

(pcmpl-me-command (argo wait)
  :inherit-global-flags t
  :flags
  '(("--ignore-not-found"))
  :subcommands (pcmpl-me-get-completion-widget :argo-workflow-to-run))

(pcmpl-me-command (argo watch)
  :inherit-global-flags t
  :flags
  '(("--node-field-selector" "--node-field-selector=" :null)
    ("--status" "--status=" :null))
  :subcommands (pcmpl-me-get-completion-widget :argo-workflow-to-run))


;;
;; PComplete argo
;;

;;;###autoload
(defun pcomplete/argo ()
  "Completion for kubectl."
  (let ((pcmpl-me--context nil))
    (unwind-protect
        (pcmpl-argo)
      (when pcmpl-me-debug (message "pcomplete/argo: pcmpl-me--context %S" pcmpl-me--context)))))


(provide 'pcmpl-argo)
;;; pcmpl-argo.el ends here
