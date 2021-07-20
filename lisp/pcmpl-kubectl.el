;;; pcmpl-kubectl.el --- Kubernetes completion helpers  -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'pcomplete-me)

(defconst pcmpl-kubectl-commands
  ;; (rs//replace-sexp (rs//bash-complete-kubectl-subcommands "kubectl"))
  '(("annotate")
    ("api-resources")
    ("api-versions")
    ("apply"
     (("edit-last-applied")
      ("set-last-applied")
      ("view-last-applied")))
    ("attach")
    ("auth"
     (("can-i")
      ("reconcile")))
    ("autoscale"
     (("deployment")
      ("replicaset")
      ("replicationcontroller")
      ("statefulset")))
    ("certificate"
     (("approve")
      ("deny")))
    ("cluster-info"
     (("dump")))
    ("completion"
     (("bash")
      ("zsh")))
    ("config"
     (("current-context")
      ("delete-cluster")
      ("delete-context")
      ("delete-user")
      ("get-clusters")
      ("get-contexts")
      ("get-users")
      ("rename-context")
      ("set")
      ("set-cluster")
      ("set-context")
      ("set-credentials")
      ("unset")
      ("use-context")
      ("view")))
    ("cordon")
    ("cp")
    ("create"
     (("clusterrole")
      ("clusterrolebinding")
      ("configmap")
      ("cronjob")
      ("deployment")
      ("ingress")
      ("job")
      ("namespace")
      ("poddisruptionbudget")
      ("priorityclass")
      ("quota")
      ("role")
      ("rolebinding")
      ("secret"
       (("docker-registry")
        ("generic")
        ("tls")))
      ("service"
       (("clusterip")
        ("externalname")
        ("loadbalancer")
        ("nodeport")))
      ("serviceaccount")))
    ("debug")
    ("delete")
    ("describe")
    ("diff")
    ("drain")
    ("edit")
    ("exec")
    ("explain")
    ("expose"
     (("deployment")
      ("pod")
      ("replicaset")
      ("replicationcontroller")
      ("service")))
    ("get")
    ("help"
     (("annotate")
      ("api-resources")
      ("api-versions")
      ("apply"
       (("edit-last-applied")
        ("set-last-applied")
        ("view-last-applied")))
      ("attach")
      ("auth"
       (("can-i")
        ("reconcile")))
      ("autoscale")
      ("certificate"
       (("approve")
        ("deny")))
      ("cluster-info"
       (("dump")))
      ("completion")
      ("config"
       (("current-context")
        ("delete-cluster")
        ("delete-context")
        ("delete-user")
        ("get-clusters")
        ("get-contexts")
        ("get-users")
        ("rename-context")
        ("set")
        ("set-cluster")
        ("set-context")
        ("set-credentials")
        ("unset")
        ("use-context")
        ("view")))
      ("cordon")
      ("cp")
      ("create"
       (("clusterrole")
        ("clusterrolebinding")
        ("configmap")
        ("cronjob")
        ("deployment")
        ("ingress")
        ("job")
        ("namespace")
        ("poddisruptionbudget")
        ("priorityclass")
        ("quota")
        ("role")
        ("rolebinding")
        ("secret"
         (("docker-registry")
          ("generic")
          ("tls")))
        ("service"
         (("clusterip")
          ("externalname")
          ("loadbalancer")
          ("nodeport")))
        ("serviceaccount")))
      ("debug")
      ("delete")
      ("describe")
      ("diff")
      ("drain")
      ("edit")
      ("exec")
      ("explain")
      ("expose")
      ("get")
      ("help")
      ("kustomize")
      ("label")
      ("logs")
      ("options")
      ("patch")
      ("plugin"
       (("list")))
      ("port-forward")
      ("proxy")
      ("replace")
      ("rollout"
       (("history")
        ("pause")
        ("restart")
        ("resume")
        ("status")
        ("undo")))
      ("run")
      ("scale")
      ("set"
       (("env")
        ("image")
        ("resources")
        ("selector")
        ("serviceaccount")
        ("subject")))
      ("taint")
      ("top"
       (("node")
        ("pod")))
      ("uncordon")
      ("version")
      ("wait")))
    ("kustomize")
    ("label")
    ("logs")
    ("options")
    ("patch")
    ("plugin"
     (("list")))
    ("port-forward")
    ("proxy")
    ("replace")
    ("rollout"
     (("history"
       (("daemonset")
        ("deployment")
        ("statefulset")))
      ("pause"
       (("deployment")))
      ("restart"
       (("daemonset")
        ("deployment")
        ("statefulset")))
      ("resume"
       (("deployment")))
      ("status"
       (("daemonset")
        ("deployment")
        ("statefulset")))
      ("undo"
       (("daemonset")
        ("deployment")
        ("statefulset")))))
    ("run")
    ("scale"
     (("deployment")
      ("replicaset")
      ("replicationcontroller")
      ("statefulset")))
    ("set"
     (("env")
      ("image")
      ("resources")
      ("selector")
      ("serviceaccount")
      ("subject")))
    ("taint"
     (("node")))
    ("top"
     (("node")
      ("pod")))
    ("uncordon")
    ("version")
    ("wait"))
  "List of `kubectl' commands")

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


(defun pcmpl-kubectl--call (&rest args)
  ""
  (cl-destructuring-bind (code result)
      (apply #'pcmpl-kubectl--call1 args)
    (if (= code 0)
        result
      (message (string-trim result))
      "")))

(defun pcmpl-kubectl--call1 (program &rest args)
  ""
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(defun pcmpl-kubectl--complete-resource-of (kind &optional context)
  "Return a list of all resources of a type.

KIND is the type of resorce to complete.  CONTEXT is context
alist."
  (let* ((context-args (pcmpl-kubectl--override-args (or context pcmpl-me--context)))
         (template "{{ range .items  }}{{ .metadata.name }} {{ end }}")
         (args `("kubectl" "get" ,@context-args "--output=template" "--template" ,template ,kind)))
    (split-string (apply #'pcmpl-kubectl--call args))))


(defun pcmpl-kubectl--complete-containers (&optional context)
  "Return containers from a pod.

Take a KIND like `pod/my-pod' or `pod my-pod'.  CONTEXT is a
context alist."
  (let ((context-args (pcmpl-kubectl--override-args (or context pcmpl-me--context)))
        (template (mapconcat
                   #'identity
                   '( ;; Pod like
                     "{{ range .spec.initContainers }}{{ .name }} {{end}}"
                     "{{ range .spec.containers  }}{{ .name }}{{ end }}"
                     ;; Deployments
                     "{{ range .spec.template.spec.initContainers }}{{ .name }} {{end}}"
                     "{{ range .spec.template.spec.containers}}{{ .name }} {{end}}")
                   " "))
        (resource (format "%s/%s"
                          (pcmpl-me--context-get :resource-kind context)
                          (pcmpl-me--context-get :resource-name context))))
   (split-string
    (apply #'pcmpl-kubectl--call
           `("kubectl" "get" ,@context-args "--output=template" "--template" ,template ,resource)))))


(defun pcmpl-kubectl--complete-resource-types (&optional context)
  "Return all the resource short names from the cluster.

CONTEXT is a context alist."
  (let ((context-args (pcmpl-kubectl--override-args (or context pcmpl-me--context))))
   (mapcar
    (lambda(e) (string-match "\\`\\([a-z]+\\)" e)
      (match-string 1 e))
    (seq-filter
     (lambda (e) (not (equal e "")))
     (split-string
      (apply #'pcmpl-kubectl--call
             `("kubectl" "api-resources" ,@context-args "--verbs" "get" "--output=wide" "--no-headers"))
      "\n")))))

(defun pcmpl-kubectl--complete-resource-types-full (&optional context)
  "Return all the resource types fully qualified names from the cluster.

CONTEXT is a context alist."
  (let ((context-args (pcmpl-kubectl--override-args (or context pcmpl-me--context))))
   (split-string
    (apply #'pcmpl-kubectl--call
           `("kubectl" "api-resources" ,@context-args "--output=name" "--cached" "--request-timeout=5s" "--verbs=get")))))

(defun pcmpl-kubectl--complete (type)
  "Return names of available entries in a kubeconfig file.

TYPE is used to specify the scope of the returned names."
  (let ((template "{{ range .%s}}{{ .name }} {{end}}"))
    (split-string
     (apply #'pcmpl-kubectl--call
            `("kubectl" "config" "view" "--output=template" "--template" ,template ,type)))))

(defun pcmpl-kubectl--complete-resource ()
  "Complete a single resources by name of a single kind.

Support completion in the for \"kind name\" and  \"kind/name\"."
  (cond
   ;; Detect resources like pod/my-pod
   ((pcomplete-match "\\`\\(.*\\)/\\(.*\\)\\'" 0)
    (pcomplete-here* (pcmpl-kubectl--complete-resource-of (pcomplete-match-string 1 0))
                     (pcomplete-match-string 2 0))
    (pcomplete-match "\\`\\(.*\\)/\\(.*\\)\\'" 1)
    (pcmpl-me--context-set :resource-kind (pcomplete-match-string 1 1))
    (pcmpl-me--context-set :resource-name (pcomplete-match-string 2 1)))

   ;; Complete kinds
   ((not (pcmpl-me--context-get :resource-kind))
    (pcomplete-here* (mapcar (lambda (e) (format "%s/" e))
                             (pcmpl-kubectl--complete-resource-types)))
    (pcomplete-match "\\`\\(.*\\)/\\([a-z9-0]*\\)/\\'" 1)
    (pcmpl-me--context-set :resource-kind (pcomplete-match-string 1 1)))

   ;; Handle the case where we have already found a kind/name
   ((and (pcmpl-me--context-get :resource-kind)
         (pcmpl-me--context-get :resource-name))
    (pcomplete-here*))))

;;   kubectl logs [-f] [-p] (POD | TYPE/NAME) [-c CONTAINER] [options]
;;   kubectl port-forward TYPE/NAME [options] [LOCAL_PORT:]REMOTE_PORT [...[LOCAL_PORT_N:]REMOTE_PORT_N]
;;   kubectl exec (POD | TYPE/NAME) [-c CONTAINER] [flags] -- COMMAND [args...] [options]
;;   kubectl attach (POD | TYPE/NAME) -c CONTAINER [options]

;;   kubectl describe (-f FILENAME | TYPE [NAME_PREFIX | -l label] | TYPE/NAME) [options]
;;   kubectl delete ([-f FILENAME] | [-k DIRECTORY] | TYPE [(NAME | -l label | --all)]) [options]
;;   kubectl label [--overwrite] (-f FILENAME | TYPE NAME) KEY_1=VAL_1 ... KEY_N=VAL_N [--resource-version=version] [options]


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
    (pcmpl-me--context-set :resource-kind (pcomplete-arg 1)))))

(pcmpl-me-set-completion-widget :kubernetes-context (lambda () (pcmpl-kubectl--complete "contexts")))
(pcmpl-me-set-completion-widget :kubernetes-user (lambda () (pcmpl-kubectl--complete "users")))
(pcmpl-me-set-completion-widget :kubernetes-cluster (lambda () (pcmpl-kubectl--complete "clusters")))
(pcmpl-me-set-completion-widget :kubernetes-namespace (lambda () (pcmpl-kubectl--complete-resource-of "namespaces")))
(pcmpl-me-set-completion-widget :kubernetes-resource (lambda () (pcmpl-kubectl--complete-resource)))
(pcmpl-me-set-completion-widget :kubernetes-resources (lambda () (pcmpl-kubectl--complete-resources)))
(pcmpl-me-set-completion-widget :kubernetes-resource-container (lambda () (pcmpl-kubectl--complete-containers)))
(pcmpl-me-set-completion-widget :kubernetes-pod (lambda () (pcmpl-kubectl--complete-resource-of "pods")))
(pcmpl-me-set-completion-widget :kubernetes-node (lambda () (pcmpl-kubectl--complete-resource-of "nodes")))

(pcmpl-me-global-args kubectl
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl"))
  '((("--add-dir-header"
      "--alsologtostderr"
      "--one-output"
      "--logtostderr"
      "--match-server-version"
      "--skip-headers"
      "--skip-log-headers"
      "--warnings-as-errors"))
    (("--as" "--as="
      "--as-group" "--as-group="
      "--help" "-h"
      "--insecure-skip-tls-verify"
      "--log-backtrace-at" "--log-backtrace-at="
      "--log-file-max-size" "--log-file-max-size="
      "--log-flush-frequency" "--log-flush-frequency="
      "--password" "--password="
      "--profile-output" "--profile-output="
      "--request-timeout" "--request-timeout="
      "--server" "--server=" "-s"
      "--stderrthreshold" "--stderrthreshold="
      "--tls-server-name" "--tls-server-name="
      "--token" "--token="
      "--username" "--username="
      "--vmodule" "--vmodule="
      "--v" "--v=" "-v"))
    (("--profile" "--profile=") . (:list "none" "cpu" "heap" "goroutine" "threadcreate" "block" "mutex"))
    (("--cluster" "--cluster=") . (:kubernetes-cluster))
    (("--user" "--user=") . (:kubernetes-user))
    (("--context" "--context=") . (:kubernetes-context))
    (("--namespace" "--namespace=" "-n") . (:kubernetes-namespaces))
    (("--log-file" "--log-file=") . (:files))
    (("--kubeconfig" "--kubeconfig=") . (:files))
    (("--certificate-authority" "--certificate-authority=") . (:files))
    (("--client-certificate" "--client-certificate=") . (:files))
    (("--client-key" "--client-key=") . (:files))
    (("--log-dir" "--log-dir=") . (:dirs))
    (("--cache-dir" "--cache-dir=") . (:dirs))))

(pcmpl-me-command kubectl
  :inherit-global-flags t
  :subcommands
  '("annotate"
    "api-resources"
    "api-versions"
    "apply"
    "attach"
    "auth"
    "autoscale"
    "certificate"
    "cluster-info"
    "completion"
    "config"
    "cordon"
    "cp"
    "create"
    "debug"
    "delete"
    "describe"
    "diff"
    "drain"
    "edit"
    "exec"
    "explain"
    "expose"
    "get"
    "help"
    "kustomize"
    "label"
    "logs"
    "options"
    "patch"
    "plugin"
    "port-forward"
    "proxy"
    "replace"
    "rollout"
    "run"
    "scale"
    "set"
    "taint"
    "top"
    "uncordon"
    "version"
    "wait"))

(pcmpl-me-command (kubectl annotate)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl annotate" pcmpl-kubectl--global-flags))
  '((("--all"
      "--allow-missing-template-keys"
      "--dry-run"
      "--field-manager" "--field-manager="
      "--field-selector" "--field-selector="
      "--list"
      "--local"
      "--overwrite"
      "--record"
      "--recursive" "-R"
      "--show-managed-fields"
      "--template" "--template="))
    (("--output" "--output=" "-o") . (:list kubectl-output-all))
    (("--resource-version" "--resource-version="))
    (("--selector" "--selector=" "-l"))
    (("--kustomize" "--kustomize=" "-k") . (:dirs))
    (("--filename" "--filename=" "-f") . (:files))))

(pcmpl-me-command (kubectl api-versions)
  :inherit-global-flags t)

(pcmpl-me-command (kubectl api-resources)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl api-resources" pcmpl-kubectl--global-flags))
  '((("--api-group" "--api-group="
      "--cached"
      "--namespaced"
      "--no-headers"
      "--verbs" "--verbs="))
    (("--sort-by" "--sort-by=") . (:list "name" "kind"))
    (("--output" "--output=" "-o") . (:list kubectl-output-name-or-wide))))

(pcmpl-me-command (kubectl apply)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl apply" pcmpl-kubectl--global-flags))
  '((("--all"
      "--allow-missing-template-keys"
      "--field-manager" "--field-manager="
      "--force"
      "--force-conflicts"
      "--grace-period" "--grace-period="
      "--openapi-patch"
      "--overwrite"
      "--prune"
      "--prune-whitelist" "--prune-whitelist="
      "--record"
      "--recursive" "-R"
      "--selector" "--selector=" "-l"
      "--server-side"
      "--show-managed-fields"
      "--template" "--template="
      "--timeout" "--timeout="
      "--validate"
      "--wait"))
    (("--kustomize" "--kustomize=" "-k") . (:dirs))
    (("--filename" "--filename=" "-f") . (:files))
    (("--output" "--output=" "-o") . (:list "json" "yaml" "name" "go-template" "go-template-file" "template" "templatefile" "jsonpath" "jsonpath-as-json" "jsonpath-file"))
    (("--cascade") . (:list "background" "orphan" "foreground"))
    (("--dry-run") . (:list "none" "server" "client")))
  :subcommands
  '("edit-last-applied" "set-last-applied" "view-last-applied"))


(pcmpl-me-command (kubectl apply edit-last-applied)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl apply edit-last-applied" pcmpl-kubectl--global-flags))
  '((("--allow-missing-template-keys"
      "--field-manager" "--field-manager="
      "--record"
      "--recursive" "-R"
      "--show-managed-fields"
      "--template" "--template="
      "--windows-line-endings"))
    (("--kustomize" "--kustomize=" "-k") . (:dirs))
    (("--filename" "--filename=" "-f") . (:files))
    (("--output" "--output=" "-o") . (:list kubectl-output-all))))

(pcmpl-me-command (kubectl apply set-last-applied)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl apply set-last-applied" pcmpl-kubectl--global-flags))
  '((("--allow-missing-template-keys"
      "--create-annotation"
      "--dry-run"
      "--show-managed-fields"
      "--template" "--template="))
    (("--filename" "--filename=" "-f") . (:files))
    (("--output" "--output=" "-o") . (:list kubectl-output-all))))

(pcmpl-me-command (kubectl apply view-last-applied)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl apply view-last-applied" pcmpl-kubectl--global-flags))
  '((("--all"))
    (("--recursive" "-R"))
    (("--selector" "--selector=" "-l"))
    (("--filename" "--filename=" "-f") . (:files))
    (("--output" "--output=" "-o") . (:list kubectl-output-all))))

(pcmpl-me-command (kubectl attach)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl attach" pcmpl-kubectl--global-flags))
  '((("--pod-running-timeout" "--pod-running-timeout="
      "--quiet" "-q"
      "--stdin" "-i"
      "--tty" "-t"))
    ;; TODO support finding containers
    (("--container" "--container=" "-c")))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-pod))

(pcmpl-me-command (kubectl auth)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl auth" pcmpl-kubectl--global-flags))
  'nil)

(pcmpl-me-command (kubectl auth can-i)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl auth can-i" pcmpl-kubectl--global-flags))
  '((("--all-namespaces" "-A"
      "--list"
      "--no-headers"
      "--quiet" "-q"
      "--subresource" "--subresource="))))

(pcmpl-me-command (kubectl auth reconcile)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl auth reconcile" pcmpl-kubectl--global-flags))
  '((("--allow-missing-template-keys"
      "--dry-run"
      "--recursive" "-R"
      "--remove-extra-permissions"
      "--remove-extra-subjects"
      "--show-managed-fields"
      "--template" "--template="))
    (("--output" "--output=" "-o") . (:list kubectl-output-all))
    (("--kustomize" "--kustomize=" "-k") . (:dirs))
    (("--filename" "--filename=" "-f") . (:files))))

(pcmpl-me-command (kubectl autoscale)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl autoscale" pcmpl-kubectl--global-flags))
  '((("--max=")))
  :subcommands '("deployment" "replicaset" "replicationcontroller" "statefulset"))

(pcmpl-me-command (kubectl autoscale deployment)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl autoscale deployment" pcmpl-kubectl--global-flags))
  '((("--max="))))

(pcmpl-me-command (kubectl autoscale replicaset)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl autoscale replicaset" pcmpl-kubectl--global-flags))
  '((("--max="))))

(pcmpl-me-command (kubectl autoscale replicationcontroller)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl autoscale replicationcontroller" pcmpl-kubectl--global-flags))
  '((("--max="))))

(pcmpl-me-command (kubectl autoscale statefulset)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl autoscale statefulset" pcmpl-kubectl--global-flags))
  '((("--max="))))

(pcmpl-me-command (kubectl certificate)
  :inherit-global-flags t)

(pcmpl-me-command (kubectl certificate approve)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl certificate approve" pcmpl-kubectl--global-flags))
  '((("--allow-missing-template-keys"
      "--force"
      "--show-managed-fields"
      "--template" "--template="))
    (("--output" "--output=" "-o") . (:list kubectl-output-all))
    (("--recursive" "-R"))
    (("--kustomize" "--kustomize=" "-k") . (:dirs))
    (("--filename" "--filename=" "-f") . (:files))))

(pcmpl-me-command (kubectl certificate deny)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl certificate deny" pcmpl-kubectl--global-flags))
  '((("--allow-missing-template-keys"
      "--force"
      "--recursive" "-R"
      "--show-managed-fields"
      "--template" "--template="))
    (("--output" "--output=" "-o") . (:list kubectl-output-all))
    (("--kustomize" "--kustomize=" "-k") . (:dirs))
    (("--filename" "--filename=" "-f") . (:files))))

(pcmpl-me-command (kubectl cluster-info)
  :inherit-global-flags t)

(pcmpl-me-command (kubectl cluster-info dump)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl cluster-info dump" pcmpl-kubectl--global-flags))
  '((("--all-namespaces" "-A"
      "--allow-missing-template-keys"
      "--namespaces" "--namespaces="
      "--output-directory" "--output-directory="
      "--pod-running-timeout" "--pod-running-timeout="
      "--show-managed-fields"
      "--template" "--template="))
    (("--output" "--output=" "-o") . (:list kubectl-output-all))))

(pcmpl-me-command (kubectl completion)
  :inherit-global-flags t
  :subcommands '("bash" "zsh"))

(pcmpl-me-command (kubectl completion bash)
  (:inherit-global-flags t))

(pcmpl-me-command (kubectl completion zsh)
  (:inherit-global-flags t))

(pcmpl-me-command (kubectl config)
  :inherit-global-flags t
  :subcommands '("current-context"
                 "delete-cluster"
                 "delete-context"
                 "delete-user"
                 "get-clusters"
                 "get-contexts"
                 "get-users"
                 "rename-context"
                 "set"
                 "set-cluster"
                 "set-context"
                 "set-credentials"
                 "unset"
                 "use-context"
                 "view"))

(pcmpl-me-command (kubectl config current-context)
  :inherit-global-flags t)

(pcmpl-me-command (kubectl config delete-cluster)
  :inherit-global-flags t
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-cluster))

(pcmpl-me-command (kubectl config delete-context)
  :inherit-global-flags t
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-context))

(pcmpl-me-command (kubectl config delete-user)
  :inherit-global-flags t
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-user))

(pcmpl-me-command (kubectl config get-clusters)
  :inherit-global-flags t)

(pcmpl-me-command (kubectl config get-contexts)
  :inherit-global-flags t
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-context))

(pcmpl-me-command (kubectl config get-users)
  :inherit-global-flags t
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-user))

(pcmpl-me-command (kubectl config rename-context)
  :inherit-global-flags t
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-context))

(pcmpl-me-command (kubectl config use-context)
  :inherit-global-flags t
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-context))

(pcmpl-me-command (kubectl cordon)
  :inherit-global-flags t
  :flags '((("--dry-run"))
           (("--selector" "--selector=" "-l")))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-node))

(pcmpl-me-command (kubectl drain)
  :inherit-global-flags t
  :flags '((("--delete-emptydir-data" "--disable-eviction" "--dry-run" "--force"
             "--grace-period" "--grace-period="
             "--ignore-daemonsets" "--ignore-errors"
             "--pod-selector" "--pod-selector="
             "--skip-wait-for-delete-timeout" "--skip-wait-for-delete-timeout="
             "--timeout" "--timeout="))
           (("--selector" "--selector=" "-l")))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-node))

(pcmpl-me-command (kubectl describe)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl describe" pcmpl-kubectl--global-flags))
  '((("--show-events"))
    (("--all-namespaces" "-A"))
    (("--selector" "--selector=" "-l"))
    (("--recursive" "-R"))
    (("--kustomize" "--kustomize=" "-k") . (:dirs))
    (("--filename" "--filename=" "-f") . (:files)))
  :subcommands-fn (pcmpl-me-get-completion-widget :kubernetes-resource))

(pcmpl-me-command (kubectl delete)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl delete" pcmpl-kubectl--global-flags))
  '((("--all"
      "--all-namespaces" "-A"
      "--cascade"
      "--dry-run"
      "--force"
      "--ignore-not-found"
      "--now"
      "--wait"))
    (("--recursive" "-R"))
    (("--field-selector" "--field-selector="))
    (("--grace-period" "--grace-period="))
    (("--raw" "--raw="))
    (("--selector" "--selector=" "-l"))
    (("--timeout" "--timeout="))
    (("--output" "--output=" "-o") . (:list kubectl-output-name))
    (("--kustomize" "--kustomize=" "-k") . (:dirs))
    (("--filename" "--filename=" "-f") . (:files)))
  :subcommands-fn (pcmpl-me-get-completion-widget :kubernetes-resource))

(pcmpl-me-command (kubectl exec)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl exec" pcmpl-kubectl--global-flags))
  '((("--container" "--container=" "-c"))
    (("--pod-running-timeout" "--pod-running-timeout=" "-t"))
    (("--tty" "-t"))
    (("--stdin" "-i"))
    (("--quiet" "-q"))
    (("--filename" "--filename=" "-f") . (:files)))
  :subcommands-fn (pcmpl-me-get-completion-widget :kubernetes-pod))

(pcmpl-me-command (kubectl get)
  :inherit-global-flags t
  :flags
  '((("--all-namespaces" "-A"
      "--allow-missing-template-keys"
      "--chunk-size" "--chunk-size="
      "--field-selector" "--field-selector="
      "--ignore-not-found"
      "--label-columns" "--label-columns=" "-L"
      "--no-headers"
      "--output-watch-events"
      "--output" "--output=" "-o"
      "--raw" "--raw="
      "--selector" "--selector=" "-l"
      "--server-print"
      "--show-kind"
      "--show-labels"
      "--show-managed-fields"
      "--sort-by" "--sort-by="
      "--template" "--template="
      "--watch" "-w"
      "--watch-only"))
    (("--recursive" "-R"))
    (("--kustomize" "--kustomize=" "-k") . (:dirs))
    (("--filename" "--filename=" "-f") . (:files)))
  :subcommands-fn (pcmpl-me-get-completion-widget :kubernetes-resources))

(pcmpl-me-command (kubectl logs)
  :inherit-global-flags t
  :flags
  ;; (rs//replace-sexp (rs//bash-complete-flags "kubectl logs" pcmpl-kubectl--global-flags))
  '((("--all-containers"
      "--ignore-errors"
      "--insecure-skip-tls-verify-backend"
      "--prefix"
      "--timestamps"))
    (("--limit-bytes" "--limit-bytes="))
    (("--max-log-requests" "--max-log-requests="))
    (("--pod-running-timeout" "--pod-running-timeout="))
    (("--previous" "-p"))
    (("--selector" "--selector=" "-l"))
    (("--since" "--since="))
    (("--since-time" "--since-time="))
    (("--tail" "--tail="))
    (("--follow" "-f"))
    (("--container" "--container=" "-c") . (:kubernetes-resource-container)))
  :subcommands-fn (pcmpl-me-get-completion-widget :kubernetes-resource))

(pcmpl-me-command (kubectl top)
  :inherit-global-flags t
  :subcommands '("pod" "node"))

(pcmpl-me-command (kubectl top pod)
  :inherit-global-flags t
  :flags '((("--all-namespaces" "-A"
             "--containers" "--no-headers"
             "--selector" "--selector=" "-l"
             "--sort-by" "--sort-by="
             "--use-protocol-buffers")))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-pod))

(pcmpl-me-command (kubectl top node)
  :inherit-global-flags t
  :flags '((("--no-headers"
             "--selector" "--selector=" "-l"
             "--sort-by" "--sort-by="
             "--use-protocol-buffers")))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-node))

(pcmpl-me-command (kubectl uncordon)
  :inherit-global-flags t
  :flags '((("--dry-run"))
           (("--selector" "--selector=" "-l")))
  :subcommands (pcmpl-me-get-completion-widget :kubernetes-node))

;;
;; PComplete kubectl
;;

;;;###autoload
(defun pcomplete/kubectl ()
  "Completion for kubectl."
  (let ((pcmpl-me--context nil))
    (unwind-protect
        (pcmpl-kubectl)
      (message "context: %S" pcmpl-me--context))))

(provide 'pcmpl-kubectl)
;;; pcmpl-kubectl.el ends here
