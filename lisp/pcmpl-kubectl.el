;;; pcmpl-kubectl.el --- Kubernetes completion helpers  -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'pcomplete-me)


(defcustom pcmpl-me-kubectl-command "kubectl"
  "Debug messages for pcomplete-me completions."
  :group 'pcomplete-me
  :type 'string)

(defconst kubectl-output-all
  '("json" "yaml" "name" "go-template" "go-template-file" "template" "templatefile" "jsonpath" "jsonpath-as-json" "jsonpath-file"))

(defconst kubectl-output-name
  '("name"))

(defconst kubectl-output-name-or-wide
  '("name" "wide"))

(defconst pcmpl-kubectl--override-flags '(:kubeconfig :cluster :user :context :namespace :server))

(defun pcmpl-kubectl--override-args (context)
  "Convert an alist of overrides into CLI flags.

CONTEXT is a context alist."
  (mapcar (lambda (e) (format "--%s=%s" (string-trim (symbol-name (car e)) ":") (cdr e)))
          (seq-filter (lambda (e) (member (car e)
                                          pcmpl-kubectl--override-flags)) context)))

(defvar pcmpl-kubectl--cache
  (make-hash-table :test #'equal
                   :size pcmpl-me--cache-size)
  "The cache, pair of list and hashtable.")

(defun pcmpl-kubectl--cache-resource (kind &optional context)
  "Return a list of all resources of a type.

KIND is the type of resorce to complete.  CONTEXT is context
alist."
  (let* ((context-args (pcmpl-kubectl--override-args (or context pcmpl-me--context)))
         (args `("get" ,@context-args "--output=json" ,kind)))
    (funcall #'pcmpl-me--call-process-async-cached pcmpl-me-kubectl-command args :on-success-transform #'json-parse-string )))

(defun pcmpl-kubectl--dig (hash &rest path)
  "Take HASH and use the traverse it with PATH."
  (reduce (lambda (h element) (gethash element h)) path :initial-value hash))

(defun pcmpl-kubectl--complete-resource-of (kind &optional context)
  "Return a list of all resources of a type.

KIND is the type of resorce to complete.  CONTEXT is context
alist."
  (let* ((cached-resources (pcmpl-kubectl--cache-resource kind (or context pcmpl-me--context)))
         (resources (when cached-resources (gethash "items" cached-resources))))
    (mapcar (lambda (resource) (pcmpl-kubectl--dig resource "metadata" "name"))
            resources)))

(defun pcmpl-kubectl--complete-containers (&optional context)
  "Return containers from a pod.

CONTEXT is a context alist."
  (let* ((cached-resources (pcmpl-kubectl--cache-resource
                            (pcmpl-me--context-get :resource-kind context)
                            context))
         (resources (when cached-resources (gethash "items" cached-resources)))
         (resource (seq-find
                    (lambda (resource)
                      (equal (pcmpl-kubectl--dig resource "metadata" "name")
                             (pcmpl-me--context-get :resource-name context)))
                    resources)))
    (when resource
     (cl-case (intern (gethash "kind" resource))
       (Pod
        (mapcar
         (lambda (e) (gethash "name" e))
         (vconcat
          (pcmpl-kubectl--dig resource "spec" "initContainers")
          (pcmpl-kubectl--dig resource "spec" "containers"))))
       (Deployment
        (mapcar
         (lambda (e) (gethash "name" e))
         (vconcat
          (pcmpl-kubectl--dig resource "spec" "template" "spec" "initContainers")
          (pcmpl-kubectl--dig resource "spec" "template" "spec" "containers"))))))))

(defun pcmpl-kubectl--complete-resource-types (&optional context)
  "Return all the resource short names from the cluster.

CONTEXT is a context alist."
  (let (;; filter context args to only the --context flag, because api-resources are global
        (context-args (pcmpl-kubectl--override-args (list (assoc :context (or context pcmpl-me--context))))))
   (mapcar
    (lambda(e) (string-match "\\`\\([a-z0-9]+\\)" e)
      (match-string 1 e))
    (seq-filter
     (lambda (e) (not (equal e "")))
     (split-string
      (or
       (apply #'pcmpl-me--call
              `(,pcmpl-me-kubectl-command "api-resources" ,@context-args "--verbs" "get" "--output=wide" "--cached" "--request-timeout=5s" "--no-headers"))
       "")
      "\n")))))

(defun pcmpl-kubectl--complete (type)
  "Return names of available entries in a kubeconfig file.

TYPE is used to specify the scope of the returned names."
  (let ((template (format "{{ range .%s}}{{ .name }} {{end}}" type)))
    (split-string
     (apply #'pcmpl-me--call-process-cached
            `(,pcmpl-me-kubectl-command "config" "view" "--output=template" "--template" ,template)))))

(defun pcmpl-kubectl--complete-resource ()
  "Complete a single resources by name of a single kind.

Support completion in the form \"kind/name\"."
  (cond
   ;; Detect resources like pod/my-pod
   ((pcomplete-match "\\`\\(.*\\)/\\(.*\\)\\'" 0)
    (setq pcomplete-termination-string " ")
    (pcomplete-here* (pcmpl-kubectl--complete-resource-of (pcomplete-match-string 1 0))
                     (pcomplete-match-string 2 0))
    (pcomplete-match "\\`\\(.*\\)/\\(.*\\)\\'")
    (pcmpl-me--context-set :resource-kind (pcomplete-match-string 1))
    (pcmpl-me--context-set :resource-name (pcomplete-match-string 2)))

   ;; Complete kinds
   ((not (pcmpl-me--context-get :resource-kind))
    (setq pcomplete-termination-string "")
    (pcomplete-here* (mapcar (lambda (e) (format "%s/" e))
                             (pcmpl-kubectl--complete-resource-types)))
    (pcomplete-match "\\`\\(.*\\)/\\([a-z9-0]*\\)/\\'")
    (pcmpl-me--context-set :resource-kind (pcomplete-match-string 1)))

   ;; Handle the case where we have already found a kind/name
   ((and (pcmpl-me--context-get :resource-kind)
         (pcmpl-me--context-get :resource-name))
    (pcomplete-here*)))
  t)



(defun pcmpl-kubectl--complete-resources ()
  "Complete resources by name of multiple kinds.

Supports comma separated resource types like \"pod,deployment\"
or slash based resources like \"pod/my-pod\"
\"deployment/my-deployment\" on the same line."
  (cond
   ;; Kinds like pod,deployment
   ((pcomplete-match "\\`.*,\\([a-z9-0]*\\)" 0)
    (pcomplete-here* (pcmpl-kubectl--complete-resource-types) (pcomplete-match-string 1 0))
    (pcmpl-me--context-set :resource-kind (pcomplete-arg 1)))

   ;; Resources based where the kind was already specified
   ((pcmpl-me--context-get :resource-kind)
    (pcomplete-here* (pcmpl-kubectl--complete-resource-of (pcmpl-me--context-get :resource-kind))))

   ;; Resources like kind/name
   ((pcomplete-match "\\`\\(.*\\)/\\(.*\\)\\'" 0)
    (pcomplete-here* (pcmpl-kubectl--complete-resource-of (pcomplete-match-string 1 0))
                     (pcomplete-match-string 2 0)))

   ;; Complete resource kinds
   (t
    (pcomplete-here* (pcmpl-kubectl--complete-resource-types))
    (pcmpl-me--context-set :resource-kind (pcomplete-arg 1))))
  t)


(pcmpl-me-set-completion-widget
 :kubernetes-dry-run '("none" "server" "client"))
(pcmpl-me-set-completion-widget
 :kubernetes-context (lambda () (pcmpl-kubectl--complete "contexts")))
(pcmpl-me-set-completion-widget
 :kubernetes-user (lambda () (pcmpl-kubectl--complete "users")))
(pcmpl-me-set-completion-widget
 :kubernetes-cluster (lambda () (pcmpl-kubectl--complete "clusters")))
(pcmpl-me-set-completion-widget
 :kubernetes-namespace (lambda () (pcmpl-kubectl--complete-resource-of "namespaces")))
(pcmpl-me-set-completion-widget
 :kubernetes-resource #'pcmpl-kubectl--complete-resource)
(pcmpl-me-set-completion-widget
 :kubernetes-resources #'pcmpl-kubectl--complete-resources)
(pcmpl-me-set-completion-widget
 :kubernetes-resource-container #'pcmpl-kubectl--complete-containers)
(pcmpl-me-set-completion-widget
 :kubernetes-pod (lambda () (pcmpl-kubectl--complete-resource-of "pods")))
(pcmpl-me-set-completion-widget
 :kubernetes-daemonset (lambda () (pcmpl-kubectl--complete-resource-of "daemonset")))
(pcmpl-me-set-completion-widget
 :kubernetes-deployment (lambda () (pcmpl-kubectl--complete-resource-of "deployments")))
(pcmpl-me-set-completion-widget
 :kubernetes-replicaset (lambda () (pcmpl-kubectl--complete-resource-of "replicaset")))
(pcmpl-me-set-completion-widget
 :kubernetes-replicationcontroller (lambda () (pcmpl-kubectl--complete-resource-of "replicationcontroller")))
(pcmpl-me-set-completion-widget
 :kubernetes-statefulset (lambda () (pcmpl-kubectl--complete-resource-of "statefulset")))
(pcmpl-me-set-completion-widget
 :kubernetes-service (lambda () (pcmpl-kubectl--complete-resource-of "service")))
(pcmpl-me-set-completion-widget
 :kubernetes-node (lambda () (pcmpl-kubectl--complete-resource-of "nodes")))

(pcmpl-me-global-args kubectl
  :flags
  '(("--add-dir-header")
    ("--alsologtostderr")
    ("--as" "--as=" :null)
    ("--as-group" "--as-group=" :null)
    ("--as-uid" "--as-uid=" :null)
    ("--cache-dir" "--cache-dir=" :dirs)
    ("--certificate-authority" "--certificate-authority=" :files)
    ("--client-certificate" "--client-certificate=" :files)
    ("--client-key" "--client-key=" :files)
    ("--cluster" "--cluster=" :kubernetes-cluster)
    ("--context" "--context=" :kubernetes-context)
    ("--insecure-skip-tls-verify")
    ("--kubeconfig" "--kubeconfig=" :files)
    ("--log-backtrace-at" "--log-backtrace-at=" :null)
    ("--log-dir" "--log-dir=" :dirs)
    ("--log-file" "--log-file=" :files)
    ("--log-file-max-size" "--log-file-max-size=" :null)
    ("--log-flush-frequency" "--log-flush-frequency=" :null)
    ("--logtostderr")
    ("--match-server-version")
    ("--namespace" "--namespace=" "-n" :kubernetes-namespace)
    ("--one-output")
    ("--password" "--password=" :null)
    ("--profile" "--profile=" :list "none" "cpu" "heap" "goroutine" "threadcreate" "block" "mutex")
    ("--profile-output" "--profile-output=" :null)
    ("--request-timeout" "--request-timeout=" :null)
    ("--server" "--server=" "-s" :null)
    ("--skip-headers")
    ("--skip-log-headers")
    ("--stderrthreshold" "--stderrthreshold=" :null)
    ("--tls-server-name" "--tls-server-name=" :null)
    ("--token" "--token=" :null)
    ("--user" "--user=" :kubernetes-user)
    ("--username" "--username=" :null)
    ("--v" "--v=" "-v" :null)
    ("--vmodule" "--vmodule=" :null)
    ("--warnings-as-errors")))

(pcmpl-me-command kubectl
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands
  '("annotate" "api-resources" "api-versions" "apply" "attach" "auth"
"autoscale" "certificate" "cluster-info" "completion" "config"
"cordon" "cp" "create" "debug" "delete" "describe" "diff" "drain"
"edit" "exec" "explain" "expose" "get" "help" "kustomize" "label"
"logs" "options" "patch" "plugin" "port-forward" "proxy" "replace"
"rollout" "run" "scale" "set" "taint" "top" "uncordon" "version"
"wait"))


(pcmpl-me-command (kubectl annotate)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--all")
    ("--all-namespaces" "-A")
    ("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--field-selector" "--field-selector=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--list")
    ("--local")
    ("--output" "--output=" "-o" :list kubectl-output-all)
    ("--overwrite")
    ("--recursive" "-R")
    ("--resource-version" "--resource-version=" :null)
    ("--selector" "--selector=" "-l" :null)
    ("--show-managed-fields")
    ("--template" "--template=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-resource))


(pcmpl-me-command (kubectl api-resources)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--api-group" "--api-group=" :null)
    ("--cached")
    ("--namespaced")
    ("--no-headers")
    ("--output" "--output=" "-o" :list kubectl-output-name-or-wide)
    ("--sort-by" "--sort-by=" :list "name" "kind")
    ("--verbs" "--verbs=" :null)))


(pcmpl-me-command (kubectl api-versions)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl apply)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--all")
    ("--allow-missing-template-keys")
    ("--cascade")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--force")
    ("--force-conflicts")
    ("--grace-period" "--grace-period=" :null)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--openapi-patch")
    ("--output" "--output=" "-o" :list kubectl-output-all)
    ("--overwrite")
    ("--prune")
    ("--prune-whitelist" "--prune-whitelist=" :null)
    ("--recursive" "-R")
    ("--selector" "--selector=" "-l" :null)
    ("--server-side")
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--timeout" "--timeout=" :null)
    ("--validate")
    ("--wait"))
  :subcommands
  '("edit-last-applied" "set-last-applied" "view-last-applied"))


(pcmpl-me-command (kubectl apply edit-last-applied)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :list kubectl-output-all)
    ("--recursive" "-R")
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--windows-line-endings")))


(pcmpl-me-command (kubectl apply set-last-applied)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--create-annotation")
    ("--dry-run" :kubernetes-dry-run)
    ("--filename" "--filename=" "-f" :files)
    ("--output" "--output=" "-o" :list kubectl-output-all)
    ("--show-managed-fields")
    ("--template" "--template=" :null)))


(pcmpl-me-command (kubectl apply view-last-applied)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--all")
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :list kubectl-output-all)
    ("--recursive" "-R")
    ("--selector" "--selector=" "-l" :null)))


(pcmpl-me-command (kubectl attach)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--container" "--container=" "-c" :kubernetes-resource-container)
    ("--pod-running-timeout" "--pod-running-timeout=" :null)
    ("--quiet" "-q")
    ("--stdin" "-i")
    ("--tty" "-t"))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-pod))


(pcmpl-me-command (kubectl auth)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands '("can-i" "reconcile"))


(pcmpl-me-command (kubectl auth can-i)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--all-namespaces" "-A")
    ("--list")
    ("--no-headers")
    ("--quiet" "-q")
    ("--subresource" "--subresource=" :null)))


(pcmpl-me-command (kubectl auth reconcile)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :list kubectl-output-all)
    ("--recursive" "-R")
    ("--remove-extra-permissions")
    ("--remove-extra-subjects")
    ("--show-managed-fields")
    ("--template" "--template=" :null)))


(pcmpl-me-command (kubectl autoscale)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags '(("--max=" :null))
  :subcommands
  '("deployment" "replicaset" "replicationcontroller" "statefulset"))


(pcmpl-me-command (kubectl autoscale deployment)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags '(("--max=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-deployment))


(pcmpl-me-command (kubectl autoscale replicaset)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags '(("--max=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-replicaset))


(pcmpl-me-command (kubectl autoscale replicationcontroller)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags '(("--max=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-replicationcontroller))


(pcmpl-me-command (kubectl autoscale statefulset)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags '(("--max=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-statefulset))


(pcmpl-me-command (kubectl certificate)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands '("approve" "deny"))


(pcmpl-me-command (kubectl certificate approve)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--filename" "--filename=" "-f" :files)
    ("--force")
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :list kubectl-output-all)
    ("--recursive" "-R")
    ("--show-managed-fields")
    ("--template" "--template=" :null)))


(pcmpl-me-command (kubectl certificate deny)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--filename" "--filename=" "-f" :files)
    ("--force")
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :list kubectl-output-all)
    ("--recursive" "-R")
    ("--show-managed-fields")
    ("--template" "--template=" :null)))


(pcmpl-me-command (kubectl cluster-info)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands '("dump"))


(pcmpl-me-command (kubectl cluster-info dump)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--all-namespaces" "-A")
    ("--allow-missing-template-keys")
    ("--namespaces" "--namespaces=" :null)
    ("--output-directory" "--output-directory=" :null)
    ("--output" "--output=" "-o" :list kubectl-output-all)
    ("--pod-running-timeout" "--pod-running-timeout=" :null)
    ("--show-managed-fields")
    ("--template" "--template=" :null)))


(pcmpl-me-command (kubectl completion)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags '(("--help" "-h"))
  :subcommands '("bash" "fish" "powershell" "zsh"))


(pcmpl-me-command (kubectl completion bash)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags '(("--help" "-h")))


(pcmpl-me-command (kubectl completion zsh)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags '(("--help" "-h")))


(pcmpl-me-command (kubectl completion fish)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags '(("--help" "-h")))


(pcmpl-me-command (kubectl completion powershell)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags '(("--help" "-h")))


(pcmpl-me-command (kubectl config)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands
  '("current-context" "delete-cluster" "delete-context" "delete-user"
"get-clusters" "get-contexts" "get-users" "rename-context" "set"
"set-cluster" "set-context" "set-credentials" "unset" "use-context"
"view"))


(pcmpl-me-command (kubectl config current-context)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl config delete-cluster)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-cluster))


(pcmpl-me-command (kubectl config delete-context)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-context))


(pcmpl-me-command (kubectl config delete-user)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-user))


(pcmpl-me-command (kubectl config get-clusters)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl config get-contexts)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--no-headers")
    ("--output" "--output=" "-o" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-context))


(pcmpl-me-command (kubectl config get-users)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-user))


(pcmpl-me-command (kubectl config rename-context)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-context))


(pcmpl-me-command (kubectl config set)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags '(("--set-raw-bytes")))


(pcmpl-me-command (kubectl config set-cluster)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags '(("--embed-certs")))


(pcmpl-me-command (kubectl config set-context)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags '(("--current")))


(pcmpl-me-command (kubectl config set-credentials)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--auth-provider-arg" "--auth-provider-arg=" :null)
    ("--auth-provider" "--auth-provider=" :null)
    ("--embed-certs")
    ("--exec-api-version" "--exec-api-version=" :null)
    ("--exec-arg" "--exec-arg=" :null)
    ("--exec-command" "--exec-command=" :null)
    ("--exec-env" "--exec-env=" :null)))


(pcmpl-me-command (kubectl config unset)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl config use-context)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-context))


(pcmpl-me-command (kubectl config view)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--flatten")
    ("--merge")
    ("--minify")
    ("--output" "--output=" "-o" :null)
    ("--raw")
    ("--show-managed-fields")
    ("--template" "--template=" :null)))


(pcmpl-me-command (kubectl cordon)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--dry-run" :kubernetes-dry-run)
    ("--selector" "--selector=" "-l" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-node))


(pcmpl-me-command (kubectl cp)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--container" "--container=" "-c" :kubernetes-resource-container)
    ("--retries" "--retries=")
    ("--no-preserve"))
  :subcommands (pcmpl-me-get-completion-widget :files))


(pcmpl-me-command (kubectl create)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--edit")
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :null)
    ("--raw" "--raw=" :null)
    ("--recursive" "-R")
    ("--save-config")
    ("--selector" "--selector=" "-l" :null)
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--validate")
    ("--windows-line-endings"))
  :subcommands
  '("clusterrole"
    "clusterrolebinding"
    "configmap"
    "cronjob"
    "deployment"
    "ingress"
    "job"
    "namespace"
    "poddisruptionbudget"
    "priorityclass"
    "quota"
    "role"
    "rolebinding"
    "secret"
    "service"
    "serviceaccount"))


(pcmpl-me-command (kubectl create clusterrole)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--aggregation-rule" "--aggregation-rule=" :null)
    ("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--non-resource-url" "--non-resource-url=" :null)
    ("--output" "--output=" "-o" :null)
    ("--resource-name" "--resource-name=" :null)
    ("--resource" "--resource=" :null)
    ("--save-config")
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--validate")
    ("--verb" "--verb=" :null)))


(pcmpl-me-command (kubectl create clusterrolebinding)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--clusterrole=" :null)))


(pcmpl-me-command (kubectl create configmap)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--append-hash")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--from-env-file" "--from-env-file=" :null)
    ("--from-file" "--from-file=" :null)
    ("--from-literal" "--from-literal=" :null)
    ("--output" "--output=" "-o" :null)
    ("--save-config")
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--validate")))


(pcmpl-me-command (kubectl create cronjob)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--image=" :null)
    ("--schedule=" :null)))


(pcmpl-me-command (kubectl create deployment)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--image=" :null)))


(pcmpl-me-command (kubectl create ingress)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--annotation" "--annotation=" :null)
    ("--class" "--class=" :null)
    ("--default-backend" "--default-backend=" :null)
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--output" "--output=" "-o" :null)
    ("--rule" "--rule=" :null)
    ("--save-config")
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--validate")))


(pcmpl-me-command (kubectl create job)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--from" "--from=" :kubernetes-resource)
    ("--image" "--image=" :null)
    ("--output" "--output=" "-o" :null)
    ("--save-config")
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--validate")))


(pcmpl-me-command (kubectl create namespace)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--output" "--output=" "-o" :null)
    ("--save-config")
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--validate")))


(pcmpl-me-command (kubectl create poddisruptionbudget)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--max-unavailable" "--max-unavailable=" :null)
    ("--min-available" "--min-available=" :null)
    ("--output" "--output=" "-o" :null)
    ("--save-config")
    ("--selector" "--selector=" :null)  ;;  "-l" is missing upstream?
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--validate")))


(pcmpl-me-command (kubectl create priorityclass)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--description" "--description=" :null)
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--global-default")
    ("--output" "--output=" "-o" :null)
    ("--preemption-policy" "--preemption-policy=" :null)
    ("--save-config")
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--validate")
    ("--value" "--value=" :null)))


(pcmpl-me-command (kubectl create quota)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--hard" "--hard=" :null)
    ("--output" "--output=" "-o" :null)
    ("--save-config")
    ("--scopes" "--scopes=" :null)
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--validate")))


(pcmpl-me-command (kubectl create role)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--output" "--output=" "-o" :null)
    ("--resource-name" "--resource-name=" :null)
    ("--resource" "--resource=" :null)
    ("--save-config")
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--validate")
    ("--verb" "--verb=" :null)))


(pcmpl-me-command (kubectl create rolebinding)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--clusterrole" "--clusterrole=" :null)
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--group" "--group=" :null)
    ("--output" "--output=" "-o" :null)
    ("--role" "--role=" :null)
    ("--save-config")
    ("--serviceaccount" "--serviceaccount=" :null)
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--validate")))


(pcmpl-me-command (kubectl create secret)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands
  '("docker-registry" "generic" "tls"))


(pcmpl-me-command (kubectl create secret docker-registry)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--append-hash")
    ("--docker-email" "--docker-email=" :null)
    ("--docker-password" "--docker-password=" :null)
    ("--docker-server" "--docker-server=" :null)
    ("--docker-username" "--docker-username=" :null)
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--from-file" "--from-file=" :null)
    ("--output" "--output=" "-o" :null)
    ("--save-config")
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--validate")))


(pcmpl-me-command (kubectl create secret generic)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--append-hash")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--from-env-file" "--from-env-file=" :null)
    ("--from-file" "--from-file=" :null)
    ("--from-literal" "--from-literal=" :null)
    ("--output" "--output=" "-o" :null)
    ("--save-config")
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--type" "--type=" :null)
    ("--validate")))


(pcmpl-me-command (kubectl create secret tls)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--append-hash")
    ("--cert" "--cert=" :null)
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--key" "--key=" :null)
    ("--output" "--output=" "-o" :null)
    ("--save-config")
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--validate")))


(pcmpl-me-command (kubectl create service)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands
  '("clusterip" "externalname" "loadbalancer" "nodeport"))


(pcmpl-me-command (kubectl create service clusterip)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--clusterip" "--clusterip=" :null)
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--output" "--output=" "-o" :null)
    ("--save-config")
    ("--show-managed-fields")
    ("--tcp" "--tcp=" :null)
    ("--template" "--template=" :null)
    ("--validate")))


(pcmpl-me-command (kubectl create service externalname)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--external-name=" :null)))


(pcmpl-me-command (kubectl create service loadbalancer)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--output" "--output=" "-o" :null)
    ("--save-config")
    ("--show-managed-fields")
    ("--tcp" "--tcp=" :null)
    ("--template" "--template=" :null)
    ("--validate")))


(pcmpl-me-command (kubectl create service nodeport)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--node-port" "--node-port=" :null)
    ("--output" "--output=" "-o" :null)
    ("--save-config")
    ("--show-managed-fields")
    ("--tcp" "--tcp=" :null)
    ("--template" "--template=" :null)
    ("--validate")))


(pcmpl-me-command (kubectl create serviceaccount)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--output" "--output=" "-o" :null)
    ("--save-config")
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--validate")))


(pcmpl-me-command (kubectl debug)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--arguments-only")
    ("--attach")
    ("--container" "--container=" "-c" :kubernetes-resource-container)
    ("--copy-to" "--copy-to=" :null)
    ("--env" "--env=" :null)
    ("--image-pull-policy" "--image-pull-policy=" :null)
    ("--image" "--image=" :null)
    ("--quiet" "-q")
    ("--replace")
    ("--same-node")
    ("--set-image" "--set-image=" :null)
    ("--share-processes")
    ("--stdin" "-i")
    ("--target" "--target=" :null)
    ("--tty" "-t")))


(pcmpl-me-command (kubectl delete)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--all")
    ("--all-namespaces" "-A")
    ("--cascade")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-selector" "--field-selector=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--force")
    ("--grace-period" "--grace-period=" :null)
    ("--ignore-not-found")
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--now")
    ("--output" "--output=" "-o" :list kubectl-output-name)
    ("--raw" "--raw=" :null)
    ("--recursive" "-R")
    ("--selector" "--selector=" "-l" :null)
    ("--timeout" "--timeout=" :null)
    ("--wait"))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-resources))


(pcmpl-me-command (kubectl describe)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--all-namespaces" "-A")
    ("--chunk-size" "--chunk-size=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--recursive" "-R")
    ("--selector" "--selector=" "-l" :null)
    ("--show-events"))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-resource))


(pcmpl-me-command (kubectl diff)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--force-conflicts")
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--recursive" "-R")
    ("--selector" "--selector=" "-l" :null)
    ("--server-side")))


(pcmpl-me-command (kubectl drain)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--chunk-size" "--chunk-size=" :null)
    ("--delete-emptydir-data")
    ("--disable-eviction")
    ("--dry-run" :kubernetes-dry-run)
    ("--force")
    ("--grace-period" "--grace-period=" :null)
    ("--ignore-daemonsets")
    ("--pod-selector" "--pod-selector=" :null)
    ("--selector" "--selector=" "-l" :null)
    ("--skip-wait-for-delete-timeout" "--skip-wait-for-delete-timeout=" :null)
    ("--timeout" "--timeout=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-node))


(pcmpl-me-command (kubectl edit)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output-patch")
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--save-config")
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--validate")
    ("--windows-line-endings"))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-resource))


(pcmpl-me-command (kubectl exec)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--container" "--container=" "-c" :kubernetes-resource-container)
    ("--filename" "--filename=" "-f" :files)
    ("--pod-running-timeout" "--pod-running-timeout=" :null)
    ("--quiet" "-q")
    ("--stdin" "-i")
    ("--tty" "-t"))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-resource))


(pcmpl-me-command (kubectl explain)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--api-version" "--api-version=" :null)
    ("--recursive")))


(pcmpl-me-command (kubectl expose)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--cluster-ip" "--cluster-ip=" :null)
    ("--dry-run" :kubernetes-dry-run)
    ("--external-ip" "--external-ip=" :null)
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--generator" "--generator=" :null)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--labels" "--labels=" :null)
    ("--load-balancer-ip" "--load-balancer-ip=" :null)
    ("--name" "--name=" :null)
    ("--output" "--output=" "-o" :null)
    ("--override-type" "--override-type=" :null)
    ("--overrides" "--overrides=" :null)
    ("--port" "--port=" :null)
    ("--protocol" "--protocol=" :null)
    ("--recursive" "-R")
    ("--save-config")
    ("--selector" "--selector=" "-l" :null)
    ("--session-affinity" "--session-affinity=" :null)
    ("--show-managed-fields")
    ("--target-port" "--target-port=" :null)
    ("--template" "--template=" :null)
    ("--type" "--type=" :null))
  :subcommands
  '("deployment" "pod" "replicaset" "replicationcontroller" "service"))


(pcmpl-me-command (kubectl expose deployment)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--cluster-ip" "--cluster-ip=" :null)
    ("--dry-run" :kubernetes-dry-run)
    ("--external-ip" "--external-ip=" :null)
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--generator" "--generator=" :null)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--labels" "--labels=" :null)
    ("--load-balancer-ip" "--load-balancer-ip=" :null)
    ("--name" "--name=" :null)
    ("--output" "--output=" "-o" :null)
    ("--override-type" "--override-type=" :null)
    ("--overrides" "--overrides=" :null)
    ("--port" "--port=" :null)
    ("--protocol" "--protocol=" :null)
    ("--recursive" "-R")
    ("--save-config")
    ("--selector" "--selector=" "-l" :null)
    ("--session-affinity" "--session-affinity=" :null)
    ("--show-managed-fields")
    ("--target-port" "--target-port=" :null)
    ("--template" "--template=" :null)
    ("--type" "--type=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-deployment))


(pcmpl-me-command (kubectl expose pod)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--cluster-ip" "--cluster-ip=" :null)
    ("--dry-run" :kubernetes-dry-run)
    ("--external-ip" "--external-ip=" :null)
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--generator" "--generator=" :null)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--labels" "--labels=" :null)
    ("--load-balancer-ip" "--load-balancer-ip=" :null)
    ("--name" "--name=" :null)
    ("--output" "--output=" "-o" :null)
    ("--override-type" "--override-type=" :null)
    ("--overrides" "--overrides=" :null)
    ("--port" "--port=" :null)
    ("--protocol" "--protocol=" :null)
    ("--recursive" "-R")
    ("--save-config")
    ("--selector" "--selector=" "-l" :null)
    ("--session-affinity" "--session-affinity=" :null)
    ("--show-managed-fields")
    ("--target-port" "--target-port=" :null)
    ("--template" "--template=" :null)
    ("--type" "--type=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-pod))


(pcmpl-me-command (kubectl expose replicaset)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--cluster-ip" "--cluster-ip=" :null)
    ("--dry-run" :kubernetes-dry-run)
    ("--external-ip" "--external-ip=" :null)
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--generator" "--generator=" :null)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--labels" "--labels=" :null)
    ("--load-balancer-ip" "--load-balancer-ip=" :null)
    ("--name" "--name=" :null)
    ("--output" "--output=" "-o" :null)
    ("--override-type" "--override-type=" :null)
    ("--overrides" "--overrides=" :null)
    ("--port" "--port=" :null)
    ("--protocol" "--protocol=" :null)
    ("--recursive" "-R")
    ("--save-config")
    ("--selector" "--selector=" "-l" :null)
    ("--session-affinity" "--session-affinity=" :null)
    ("--show-managed-fields")
    ("--target-port" "--target-port=" :null)
    ("--template" "--template=" :null)
    ("--type" "--type=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-replicaset))


(pcmpl-me-command (kubectl expose replicationcontroller)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--cluster-ip" "--cluster-ip=" :null)
    ("--dry-run" :kubernetes-dry-run)
    ("--external-ip" "--external-ip=" :null)
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--generator" "--generator=" :null)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--labels" "--labels=" :null)
    ("--load-balancer-ip" "--load-balancer-ip=" :null)
    ("--name" "--name=" :null)
    ("--output" "--output=" "-o" :null)
    ("--override-type" "--override-type=" :null)
    ("--overrides" "--overrides=" :null)
    ("--port" "--port=" :null)
    ("--protocol" "--protocol=" :null)
    ("--recursive" "-R")
    ("--save-config")
    ("--selector" "--selector=" "-l" :null)
    ("--session-affinity" "--session-affinity=" :null)
    ("--show-managed-fields")
    ("--target-port" "--target-port=" :null)
    ("--template" "--template=" :null)
    ("--type" "--type=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-replicationcontroller))


(pcmpl-me-command (kubectl expose service)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--cluster-ip" "--cluster-ip=" :null)
    ("--dry-run" :kubernetes-dry-run)
    ("--external-ip" "--external-ip=" :null)
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--generator" "--generator=" :null)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--labels" "--labels=" :null)
    ("--load-balancer-ip" "--load-balancer-ip=" :null)
    ("--name" "--name=" :null)
    ("--output" "--output=" "-o" :null)
    ("--override-type" "--override-type=" :null)
    ("--overrides" "--overrides=" :null)
    ("--port" "--port=" :null)
    ("--protocol" "--protocol=" :null)
    ("--recursive" "-R")
    ("--save-config")
    ("--selector" "--selector=" "-l" :null)
    ("--session-affinity" "--session-affinity=" :null)
    ("--show-managed-fields")
    ("--target-port" "--target-port=" :null)
    ("--template" "--template=" :null)
    ("--type" "--type=" :null)))


(pcmpl-me-command (kubectl get)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--all-namespaces" "-A")
    ("--allow-missing-template-keys")
    ("--chunk-size" "--chunk-size=" :null)
    ("--field-selector" "--field-selector=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--ignore-not-found")
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--label-columns" "--label-columns=" "-L" :null)
    ("--no-headers")
    ("--output-watch-events")
    ("--output" "--output=" "-o" :null)
    ("--raw" "--raw=" :null)
    ("--recursive" "-R")
    ("--selector" "--selector=" "-l" :null)
    ("--server-print")
    ("--show-kind")
    ("--show-labels")
    ("--show-managed-fields")
    ("--sort-by" "--sort-by=" :null)
    ("--template" "--template=" :null)
    ("--watch" "-w")
    ("--watch-only"))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-resources))


(pcmpl-me-command (kubectl help)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands
  '("annotate" "api-resources" "api-versions" "alpha" "apply"
    "attach" "auth" "autoscale" "certificate" "cluster-info"
    "completion" "config" "cordon" "cp" "create" "debug"
    "delete" "describe" "diff" "drain" "edit" "exec" "explain"
    "expose" "get" "help" "kustomize" "label" "logs" "options"
    "patch" "plugin" "port-forward" "proxy" "replace"
    "rollout" "run" "scale" "set" "taint" "top" "uncordon"
    "version" "wait"))

(pcmpl-me-command (kubectl help alpha)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help annotate)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help api-resources)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help api-versions)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help apply)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands
  '("edit-last-applied" "set-last-applied" "view-last-applied"))


(pcmpl-me-command (kubectl help apply edit-last-applied)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help apply set-last-applied)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help apply view-last-applied)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help attach)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help auth)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands
  '("can-i" "reconcile"))


(pcmpl-me-command (kubectl help auth can-i)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help auth reconcile)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help autoscale)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help certificate)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands
  '("approve" "deny"))


(pcmpl-me-command (kubectl help certificate approve)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help certificate deny)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help cluster-info)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands
  '("dump"))


(pcmpl-me-command (kubectl help cluster-info dump)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help completion)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help config)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands
  '("current-context" "delete-cluster" "delete-context" "delete-user" "get-clusters" "get-contexts" "get-users" "rename-context" "set" "set-cluster" "set-context" "set-credentials" "unset" "use-context" "view"))


(pcmpl-me-command (kubectl help config current-context)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help config delete-cluster)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help config delete-context)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help config delete-user)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help config get-clusters)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help config get-contexts)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help config get-users)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help config rename-context)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help config set)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help config set-cluster)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help config set-context)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help config set-credentials)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help config unset)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help config use-context)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help config view)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help cordon)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help cp)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands
  '("clusterrole" "clusterrolebinding" "configmap" "cronjob"
"deployment" "ingress" "job" "namespace" "poddisruptionbudget"
"priorityclass" "quota" "role" "rolebinding" "secret" "service"
"serviceaccount"))


(pcmpl-me-command (kubectl help create clusterrole)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create clusterrolebinding)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create configmap)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create cronjob)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create deployment)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create ingress)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create job)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create namespace)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create poddisruptionbudget)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create priorityclass)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create quota)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create role)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create rolebinding)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create secret)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands
  '("docker-registry" "generic" "tls"))


(pcmpl-me-command (kubectl help create secret docker-registry)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create secret generic)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create secret tls)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create service)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands
  '("clusterip" "externalname" "loadbalancer" "nodeport"))


(pcmpl-me-command (kubectl help create service clusterip)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create service externalname)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create service loadbalancer)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create service nodeport)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help create serviceaccount)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help debug)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help delete)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help describe)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help diff)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help drain)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help edit)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help exec)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help explain)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help expose)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help get)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help help)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help kustomize)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help label)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help logs)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help options)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help patch)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help plugin)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands '("list"))


(pcmpl-me-command (kubectl help plugin list)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help port-forward)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help proxy)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help replace)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help rollout)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands '("history" "pause" "restart" "resume" "status" "undo"))


(pcmpl-me-command (kubectl help rollout history)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help rollout pause)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help rollout restart)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help rollout resume)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help rollout status)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help rollout undo)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help run)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help scale)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help set)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands
  '("env" "image" "resources" "selector" "serviceaccount" "subject"))


(pcmpl-me-command (kubectl help set env)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help set image)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help set resources)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help set selector)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help set serviceaccount)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help set subject)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help taint)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help top)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands
  '("node" "pod"))


(pcmpl-me-command (kubectl help top node)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help top pod)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help uncordon)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help version)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl help wait)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl kustomize)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--as-current-user")
    ("--enable-alpha-plugins")
    ("--enable-helm")
    ("--enable-managedby-label")
    ("--env" "--env=" :null)
    ("--helm-command" "--helm-command=" :null)
    ("--load-restrictor" "--load-restrictor=" :null)
    ("--mount" "--mount=" :null)
    ("--network")
    ("--network-name" "--network-name=" :null)
    ("--output" "--output=" "-o" :null)
    ("--reorder" "--reorder=" :null)
    ("-e")))


(pcmpl-me-command (kubectl label)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--all")
    ("--all-namespaces" "-A")
    ("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--field-selector" "--field-selector=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--list")
    ("--local")
    ("--output" "--output=" "-o" :null)
    ("--overwrite")
    ("--recursive" "-R")
    ("--resource-version" "--resource-version=" :null)
    ("--selector" "--selector=" "-l" :null)
    ("--show-managed-fields")
    ("--template" "--template=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-resource))


(pcmpl-me-command (kubectl logs)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--all-containers")
    ("--container" "--container=" "-c" :kubernetes-resource-container)
    ("--follow" "-f")
    ("--ignore-errors")
    ("--insecure-skip-tls-verify-backend")
    ("--limit-bytes" "--limit-bytes=" :null)
    ("--max-log-requests" "--max-log-requests=" :null)
    ("--pod-running-timeout" "--pod-running-timeout=" :null)
    ("--prefix")
    ("--previous")
    ("--selector" "--selector=" "-l" :null)
    ("--since-time" "--since-time=" :null)
    ("--since" "--since=" :null)
    ("--tail" "--tail=" :null)
    ("--timestamps")
    ("-p"))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-resource))


(pcmpl-me-command (kubectl options)
  :inherit-global-flags t)


(pcmpl-me-command (kubectl patch)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--local")
    ("--output" "--output=" "-o" :null)
    ("--patch-file" "--patch-file=" :null)
    ("--patch" "--patch=" :null)
    ("--recursive" "-R")
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--type" "--type=" :null)
    ("-p"))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-resource))


(pcmpl-me-command (kubectl plugin)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands
  '("list"))


(pcmpl-me-command (kubectl plugin list)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--name-only")))


(pcmpl-me-command (kubectl port-forward)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--address" "--address=" :null)
    ("--pod-running-timeout" "--pod-running-timeout=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-resource))


(pcmpl-me-command (kubectl proxy)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--accept-hosts" "--accept-hosts=" :null)
    ("--accept-paths" "--accept-paths=" :null)
    ("--address" "--address=" :null)
    ("--api-prefix" "--api-prefix=" :null)
    ("--append-server-path")
    ("--disable-filter")
    ("--keepalive" "--keepalive=" :null)
    ("--port" "--port=" :null)
    ("--reject-methods" "--reject-methods=" :null)
    ("--reject-paths" "--reject-paths=" :null)
    ("--unix-socket" "--unix-socket=" :null)
    ("--www-prefix" "--www-prefix=" :null)
    ("--www" "--www=" :null)
    ("-P")
    ("-p")
    ("-u")
    ("-w")))


(pcmpl-me-command (kubectl replace)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--cascade")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--force")
    ("--grace-period" "--grace-period=" :null)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :null)
    ("--raw" "--raw=" :null)
    ("--recursive" "-R")
    ("--save-config")
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--timeout" "--timeout=" :null)
    ("--validate")
    ("--wait")))


(pcmpl-me-command (kubectl rollout)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands
  '("history" "pause" "restart" "resume" "status" "undo"))


(pcmpl-me-command (kubectl rollout history)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--revision" "--revision=" :null)
    ("--show-managed-fields")
    ("--template" "--template=" :null))
  :subcommands
  '("daemonset" "deployment" "statefulset"))


(pcmpl-me-command (kubectl rollout history daemonset)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--revision" "--revision=" :null)
    ("--show-managed-fields")
    ("--template" "--template=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-daemonset))


(pcmpl-me-command (kubectl rollout history deployment)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--revision" "--revision=" :null)
    ("--show-managed-fields")
    ("--template" "--template=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-deployment))


(pcmpl-me-command (kubectl rollout history statefulset)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--revision" "--revision=" :null)
    ("--show-managed-fields")
    ("--template" "--template=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-statefulset))


(pcmpl-me-command (kubectl rollout pause)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--show-managed-fields")
    ("--template" "--template=" :null))
  :subcommands
  '("deployment"))


(pcmpl-me-command (kubectl rollout pause deployment)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--show-managed-fields")
    ("--template" "--template=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-deployment))


(pcmpl-me-command (kubectl rollout restart)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--show-managed-fields")
    ("--template" "--template=" :null))
  :subcommands
  '("daemonset" "deployment" "statefulset"))


(pcmpl-me-command (kubectl rollout restart daemonset)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--show-managed-fields")
    ("--template" "--template=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-daemonset))


(pcmpl-me-command (kubectl rollout restart deployment)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--show-managed-fields")
    ("--template" "--template=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-deployment))


(pcmpl-me-command (kubectl rollout restart statefulset)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--show-managed-fields")
    ("--template" "--template=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-statefulset))


(pcmpl-me-command (kubectl rollout resume)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--show-managed-fields")
    ("--template" "--template=" :null))
  :subcommands
  '("deployment"))


(pcmpl-me-command (kubectl rollout resume deployment)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--show-managed-fields")
    ("--template" "--template=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-deployment))


(pcmpl-me-command (kubectl rollout status)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--recursive" "-R")
    ("--revision" "--revision=" :null)
    ("--timeout" "--timeout=" :null)
    ("--watch" "-w"))
  :subcommands
  '("daemonset" "deployment" "statefulset"))


(pcmpl-me-command (kubectl rollout status daemonset)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--recursive" "-R")
    ("--revision" "--revision=" :null)
    ("--timeout" "--timeout=" :null)
    ("--watch" "-w"))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-daemonset))


(pcmpl-me-command (kubectl rollout status deployment)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--recursive" "-R")
    ("--revision" "--revision=" :null)
    ("--timeout" "--timeout=" :null)
    ("--watch" "-w"))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-deployment))


(pcmpl-me-command (kubectl rollout status statefulset)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--recursive" "-R")
    ("--revision" "--revision=" :null)
    ("--timeout" "--timeout=" :null)
    ("--watch" "-w"))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-statefulset))


(pcmpl-me-command (kubectl rollout undo)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--to-revision" "--to-revision=" :null))
  :subcommands
  '("daemonset" "deployment" "statefulset"))


(pcmpl-me-command (kubectl rollout undo daemonset)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--to-revision" "--to-revision=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-daemonset))


(pcmpl-me-command (kubectl rollout undo deployment)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--to-revision" "--to-revision=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-deployment))


(pcmpl-me-command (kubectl rollout undo statefulset)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--to-revision" "--to-revision=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-statefulset))


(pcmpl-me-command (kubectl run)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags '(("--image=" :null)))


(pcmpl-me-command (kubectl scale)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags '(("--replicas=" :null))
  :subcommands
  '("deployment" "replicaset" "replicationcontroller" "statefulset"))


(pcmpl-me-command (kubectl scale deployment)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags '(("--replicas=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-deployment))


(pcmpl-me-command (kubectl scale replicaset)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags '(("--replicas=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-replicaset))


(pcmpl-me-command (kubectl scale replicationcontroller)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags '(("--replicas=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-replicationcontroller))


(pcmpl-me-command (kubectl scale statefulset)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags '(("--replicas=" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-statefulset))


(pcmpl-me-command (kubectl set)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands
  '("env" "image" "resources" "selector" "serviceaccount" "subject"))


(pcmpl-me-command (kubectl set env)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--all")
    ("--allow-missing-template-keys")
    ("--containers" "--containers=" :null)
    ("--dry-run" :kubernetes-dry-run)
    ("--env" "--env=" :null)
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--from" "--from=" :null)
    ("--keys" "--keys=" :null)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--list")
    ("--local")
    ("--output" "--output=" "-o" :null)
    ("--overwrite")
    ("--prefix" "--prefix=" :null)
    ("--recursive" "-R")
    ("--resolve")
    ("--selector" "--selector=" "-l" :null)
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("-c")
    ("-e")))


(pcmpl-me-command (kubectl set image)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--all")
    ("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--local")
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--selector" "--selector=" "-l" :null)
    ("--show-managed-fields")
    ("--template" "--template=" :null)))


(pcmpl-me-command (kubectl set resources)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--all")
    ("--allow-missing-template-keys")
    ("--containers" "--containers=" :null)
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--limits" "--limits=" :null)
    ("--local")
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--requests" "--requests=" :null)
    ("--selector" "--selector=" "-l" :null)
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("-c")))


(pcmpl-me-command (kubectl set selector)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--all")
    ("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--local")
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--resource-version" "--resource-version=" :null)
    ("--show-managed-fields")
    ("--template" "--template=" :null)))


(pcmpl-me-command (kubectl set serviceaccount)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--all")
    ("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--local")
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--show-managed-fields")
    ("--template" "--template=" :null)))


(pcmpl-me-command (kubectl set subject)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--all")
    ("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--group" "--group=" :null)
    ("--kustomize" "--kustomize=" "-k" :dirs)
    ("--local")
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--selector" "--selector=" "-l" :null)
    ("--serviceaccount" "--serviceaccount=" :null)
    ("--show-managed-fields")
    ("--template" "--template=" :null)))


(pcmpl-me-command (kubectl taint)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--all")
    ("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--output" "--output=" "-o" :null)
    ("--overwrite")
    ("--selector" "--selector=" "-l" :null)
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--validate"))
  :subcommands
  '("node"))


(pcmpl-me-command (kubectl taint node)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--all")
    ("--allow-missing-template-keys")
    ("--dry-run" :kubernetes-dry-run)
    ("--field-manager" "--field-manager=" :null)
    ("--output" "--output=" "-o" :null)
    ("--overwrite")
    ("--selector" "--selector=" "-l" :null)
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--validate"))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-node))


(pcmpl-me-command (kubectl top)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :subcommands '("node" "pod"))


(pcmpl-me-command (kubectl top node)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--no-headers")
    ("--selector" "--selector=" "-l" :null)
    ("--show-capacity")
    ("--sort-by" "--sort-by=" :null)
    ("--use-protocol-buffers"))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-node))


(pcmpl-me-command (kubectl top pod)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--all-namespaces" "-A")
    ("--containers")
    ("--field-selector" "--field-selector=" :null)
    ("--no-headers")
    ("--selector" "--selector=" "-l" :null)
    ("--sort-by" "--sort-by=" :null)
    ("--use-protocol-buffers"))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-pod))


(pcmpl-me-command (kubectl uncordon)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--dry-run" :kubernetes-dry-run)
    ("--selector" "--selector=" "-l" :null))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-node))


(pcmpl-me-command (kubectl version)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--client")
    ("--output" "--output=" "-o" :null)
    ("--short")))


(pcmpl-me-command (kubectl wait)
  :inherit-global-flags t
  :filter-flags #'pcmpl-me--normalise-flags
  :flags
  '(("--all")
    ("--all-namespaces" "-A")
    ("--allow-missing-template-keys")
    ("--field-selector" "--field-selector=" :null)
    ("--filename" "--filename=" "-f" :files)
    ("--for" "--for=" :null)
    ("--local")
    ("--output" "--output=" "-o" :null)
    ("--recursive" "-R")
    ("--selector" "--selector=" "-l" :null)
    ("--show-managed-fields")
    ("--template" "--template=" :null)
    ("--timeout" "--timeout=" :null)))


;;
;; PComplete kubectl
;;

;;;###autoload
(defun pcomplete/kubectl ()
  "Completion for kubectl."
  (let ((pcmpl-me--context nil))
    (unwind-protect
        (pcmpl-kubectl)
      (when pcmpl-me-debug (message "pcomplete/kubectl: pcmpl-me--context %S" pcmpl-me--context)))))

(provide 'pcmpl-kubectl)
;;; pcmpl-kubectl.el ends here
