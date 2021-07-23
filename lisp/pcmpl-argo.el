;;; pcmpl-argo.el --- Argo Workflow Completion       -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Russell Sim

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
         (args `("argo" kind "list" ,@context-args
                 "--output=name")))
    (split-string (apply #'pcmpl-me--call args))))


(defun pcmpl-argo--complete-workflow (status &optional context)
  "Return a list of workflows filtered by STATUS.

CONTEXT is context alist."
  (let* ((context-args (pcmpl-argo--override-args (or context pcmpl-me--context)))
         (args `("argo" "list" ,@context-args
                 "--output=name"
                 "--status" ,status)))
    (split-string (apply #'pcmpl-me--call args))))

(defun pcmpl-argo--complete-workflow-pods (&optional context)
  "Return a list of all workflow pods.

CONTEXT is context alist."
  (let* ((context-args (pcmpl-argo--override-args (or context pcmpl-me--context)))
         (template "{{ range .items  }}{{ .metadata.name }} {{ end }}")
         (args `("kubectl" "pods" ,@context-args
                 "--selector=workflows.argoproj.io/workflow"
                 "--output=template"
                 "--template" ,template)))
    (split-string (apply #'pcmpl-me--call args))))

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


(pcmpl-me-global-args argo
  :flags
  ;; (rs//replace-sexp (rs//add-null-completers (rs//group-flags (rs//bash-complete-argo-flags "argo"))))
  '(("--argo-server" "--argo-server=" "-s" :null)
    ("--as-group" "--as-group=" :null)
    ("--as" "--as=" :null)
    ("--certificate-authority" "--certificate-authority=" :null)
    ("--client-certificate" "--client-certificate=" :null)
    ("--client-key" "--client-key=" :null)
    ("--cluster" "--cluster=" :null)
    ("--context" "--context=" :null)
    ("--gloglevel" "--gloglevel=" :null)
    ("--insecure-skip-tls-verify")
    ("--insecure-skip-verify" "-k")
    ("--instanceid" "--instanceid=" :null)
    ("--kubeconfig" "--kubeconfig=" :null)
    ("--loglevel" "--loglevel=" :null)
    ("--namespace" "--namespace=" "-n" :null)
    ("--password" "--password=" :null)
    ("--request-timeout" "--request-timeout=" :null)
    ("--secure" "-e")
    ("--server" "--server=" :null)
    ("--token" "--token=" :null)
    ("--user" "--user=" :null)
    ("--username" "--username=" :null)
    ("--verbose" "-v")))


(pcmpl-me-command (argo)
  :inherit-global-flags t
  :subcommands
  '("archive" "auth" "cluster-template" "completion" "cron" "delete"
  "get" "lint" "list" "logs" "node" "resubmit" "resume" "retry"
  "server" "stop" "submit" "suspend" "template" "terminate" "version"
  "wait" "watch"))

(pcmpl-me-command (argo archive)
  :inherit-global-flags t :subcommands
  '("delete" "get" "list"))

(pcmpl-me-command (argo archive delete)
  :inherit-global-flags t)

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
  '(("--strict")))

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
  '(("--strict")))

(pcmpl-me-command (argo cron list)
  :inherit-global-flags t
  :flags
  '(("--all-namespaces")
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
    ("--all-namespaces")
    ("--completed")
    ("--dry-run")
    ("--older" "--older=" :null)
    ("--prefix" "--prefix=" :null)
    ("--resubmitted")
    ("--selector" "--selector=" "-l" :null))
  :subcommands (pcmpl-me-get-completion-widget :argo-workflow))

(pcmpl-me-command (argo get)
  :inherit-global-flags t :flags
  '(("--no-color")
    ("--node-field-selector" "--node-field-selector=" :null)
    ("--output" "--output=" "-o" :null)
    ("--status" "--status=" :null))
  :subcommands (pcmpl-me-get-completion-widget :argo-workflow))

(pcmpl-me-command (argo lint)
  :inherit-global-flags t
  :flags
  '(("--strict"))
  :subcommands-fn (pcmpl-me-get-completion-widget :files))

(pcmpl-me-command (argo list)
  :inherit-global-flags t :flags
  '(("--all-namespaces")
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
    ("--no-color")
    ("--previous" "-p")
    ("--since-time" "--since-time=" :null)
    ("--since" "--since=" :null)
    ("--tail" "--tail=" :null)
    ("--timestamps")))

(pcmpl-me-command (argo node)
  :inherit-global-flags t :flags
  '(("--message" "--message=" "-m" :null)
    ("--node-field-selector" "--node-field-selector=" :null)
    ("--output-parameter" "--output-parameter=" "-p" :null)))

(pcmpl-me-command (argo resubmit)
  :inherit-global-flags t :flags
  '(("--log")
    ("--memoized")
    ("--output" "--output=" "-o" :null)
    ("--priority" "--priority=" :null)
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
    ("--node-field-selector" "--node-field-selector=" :null)
    ("--output" "--output=" "-o" :null)
    ("--restart-successful")
    ("--wait" "-w")
    ("--watch"))
  :subcommands (pcmpl-me-get-completion-widget :argo-workflow-failed))

(pcmpl-me-command (argo server)
  :inherit-global-flags t
  :flags
  '(("--auth-mode" "--auth-mode=" :null)
    ("--basehref" "--basehref=" :null)
    ("--browser" "-b")
    ("--configmap" "--configmap=" :null)
    ("--event-operation-queue-size" "--event-operation-queue-size=" :null)
    ("--event-worker-count" "--event-worker-count=" :null)
    ("--hsts")
    ("--managed-namespace" "--managed-namespace=" :null)
    ("--namespaced")
    ("--port" "--port=" "-p" :null)))

(pcmpl-me-command (argo stop)
  :inherit-global-flags t
  :flags
  '(("--message" "--message=" :null)
    ("--node-field-selector" "--node-field-selector=" :null)))

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
  '(("--strict"))
  :subcommands-fn (pcmpl-me-get-completion-widget :files))

(pcmpl-me-command (argo template list)
  :inherit-global-flags t
  :flags
  '(("--all-namespaces")
    ("--output" "--output=" "-o" :null)))

(pcmpl-me-command (argo terminate)
  :inherit-global-flags t
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
    (pcmpl-argo)))


(provide 'pcmpl-argo)
;;; pcmpl-argo.el ends here
