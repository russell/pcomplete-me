;;; pcmpl-kubectl-test.el --- pcmpl-kubectl Tests    -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Russell Sim

;; Author: Russell Sim <russell.sim@gmail.com>
;; Keywords:

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

(require 'pcmpl-kubectl)
(require 'bash-completion-export-utils)

(defconst pcmpl-kubectl-test-bashinit (format "%s/pcmpl-kubectl-test.sh"
                                              (cond
                                               (buffer-file-name
                                                (file-name-directory buffer-file-name))
                                               (load-file-name
                                                (file-name-directory load-file-name)))))

(defun rs//bash-complete-kubectl-subcommand (command &optional global-flags)
  ""
  (rs//bash-complete-subcommand command '("KUBECONFIG=/dev/null")))


(cl-defmacro pcmpl-me-test (command (&key inherit-global-flags))
  (let* ((command-list (if (listp command) command (cons command nil)))
         (global-command (car command-list))
         (global-flags (intern (mapconcat 'symbol-name `(pcmpl ,global-command -global-flags) "-")))
         (subcommand-flags (intern (mapconcat 'symbol-name `(pcmpl ,@command-list -flags) "-")))
         (subcommand-subcommands (intern (mapconcat 'symbol-name `(pcmpl ,@command-list -subcommands) "-"))))
    `(progn
       (ert-deftest ,(intern (mapconcat 'symbol-name `(pcmpl ,@command-list -flags) "-")) ()
         (should
          (equal
           (cl-set-difference
            (cl-sort
             ,subcommand-flags
             'string-lessp)
            (let ((bash-completion-start-files `(,pcmpl-kubectl-test-bashinit)))
              (rs//bash-complete-flags ,(mapconcat 'symbol-name command-list " ") ,global-flags))
            :test #'string-equal)
           nil)))
       (ert-deftest ,(intern (mapconcat 'symbol-name `(pcmpl ,@command-list -subcommands) "-")) ()
         (should
          (equal
           (cl-set-difference
            ,subcommand-subcommands
            (let ((bash-completion-start-files `(,pcmpl-kubectl-test-bashinit)))
             (rs//bash-complete-kubectl-subcommand ,(mapconcat 'symbol-name command-list " ")))
            :test #'string-equal)
           nil))))))

(pcmpl-me-test (kubectl annotate) (:inherit-global-flags t))
(pcmpl-me-test (kubectl api-resources) (:inherit-global-flags t))
(pcmpl-me-test (kubectl api-versions) (:inherit-global-flags t))
(pcmpl-me-test (kubectl apply edit-last-applied) (:inherit-global-flags t))
(pcmpl-me-test (kubectl apply set-last-applied) (:inherit-global-flags t))
(pcmpl-me-test (kubectl apply view-last-applied) (:inherit-global-flags t))
(pcmpl-me-test (kubectl apply) (:inherit-global-flags t))
(pcmpl-me-test (kubectl attach) (:inherit-global-flags t))
(pcmpl-me-test (kubectl auth can-i) (:inherit-global-flags t))
(pcmpl-me-test (kubectl auth reconcile) (:inherit-global-flags t))
(pcmpl-me-test (kubectl auth) (:inherit-global-flags t))
(pcmpl-me-test (kubectl autoscale deployment) (:inherit-global-flags t))
(pcmpl-me-test (kubectl autoscale replicaset) (:inherit-global-flags t))
(pcmpl-me-test (kubectl autoscale replicationcontroller) (:inherit-global-flags t))
(pcmpl-me-test (kubectl autoscale statefulset) (:inherit-global-flags t))
(pcmpl-me-test (kubectl autoscale) (:inherit-global-flags t))
(pcmpl-me-test (kubectl certificate approve) (:inherit-global-flags t))
(pcmpl-me-test (kubectl certificate deny) (:inherit-global-flags t))
(pcmpl-me-test (kubectl certificate) (:inherit-global-flags t))
(pcmpl-me-test (kubectl cluster-info dump) (:inherit-global-flags t))
(pcmpl-me-test (kubectl cluster-info) (:inherit-global-flags t))
(pcmpl-me-test (kubectl completion bash) (:inherit-global-flags t))
(pcmpl-me-test (kubectl completion zsh) (:inherit-global-flags t))
(pcmpl-me-test (kubectl completion) (:inherit-global-flags t))
(pcmpl-me-test (kubectl config current-context) (:inherit-global-flags t))
(pcmpl-me-test (kubectl config delete-cluster) (:inherit-global-flags t))
(pcmpl-me-test (kubectl config delete-context) (:inherit-global-flags t))
(pcmpl-me-test (kubectl config delete-user) (:inherit-global-flags t))
(pcmpl-me-test (kubectl config get-clusters) (:inherit-global-flags t))
(pcmpl-me-test (kubectl config get-contexts) (:inherit-global-flags t))
(pcmpl-me-test (kubectl config get-users) (:inherit-global-flags t))
(pcmpl-me-test (kubectl config rename-context) (:inherit-global-flags t))
(pcmpl-me-test (kubectl config set) (:inherit-global-flags t))
(pcmpl-me-test (kubectl config set-cluster) (:inherit-global-flags t))
(pcmpl-me-test (kubectl config set-context) (:inherit-global-flags t))
(pcmpl-me-test (kubectl config set-credentials) (:inherit-global-flags t))
(pcmpl-me-test (kubectl config unset) (:inherit-global-flags t))
(pcmpl-me-test (kubectl config use-context) (:inherit-global-flags t))
(pcmpl-me-test (kubectl config view) (:inherit-global-flags t))
(pcmpl-me-test (kubectl config) (:inherit-global-flags t))
(pcmpl-me-test (kubectl cordon) (:inherit-global-flags t))
(pcmpl-me-test (kubectl cp) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create clusterrole) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create clusterrolebinding) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create configmap) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create cronjob) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create deployment) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create ingress) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create job) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create namespace) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create poddisruptionbudget) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create priorityclass) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create quota) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create role) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create rolebinding) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create secret docker-registry) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create secret generic) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create secret tls) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create secret) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create service clusterip) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create service externalname) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create service loadbalancer) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create service nodeport) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create service) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create serviceaccount) (:inherit-global-flags t))
(pcmpl-me-test (kubectl create) (:inherit-global-flags t))
(pcmpl-me-test (kubectl debug) (:inherit-global-flags t))
(pcmpl-me-test (kubectl delete) (:inherit-global-flags t))
(pcmpl-me-test (kubectl describe) (:inherit-global-flags t))
(pcmpl-me-test (kubectl diff) (:inherit-global-flags t))
(pcmpl-me-test (kubectl drain) (:inherit-global-flags t))
(pcmpl-me-test (kubectl edit) (:inherit-global-flags t))
(pcmpl-me-test (kubectl exec) (:inherit-global-flags t))
(pcmpl-me-test (kubectl explain) (:inherit-global-flags t))
(pcmpl-me-test (kubectl expose deployment) (:inherit-global-flags t))
(pcmpl-me-test (kubectl expose pod) (:inherit-global-flags t))
(pcmpl-me-test (kubectl expose replicaset) (:inherit-global-flags t))
(pcmpl-me-test (kubectl expose replicationcontroller) (:inherit-global-flags t))
(pcmpl-me-test (kubectl expose service) (:inherit-global-flags t))
(pcmpl-me-test (kubectl expose) (:inherit-global-flags t))
(pcmpl-me-test (kubectl get) (:inherit-global-flags t))
(pcmpl-me-test (kubectl help) (:inherit-global-flags t))
(pcmpl-me-test (kubectl kustomize) (:inherit-global-flags t))
(pcmpl-me-test (kubectl label) (:inherit-global-flags t))
(pcmpl-me-test (kubectl logs) (:inherit-global-flags t))
(pcmpl-me-test (kubectl options) (:inherit-global-flags t))
(pcmpl-me-test (kubectl patch) (:inherit-global-flags t))
(pcmpl-me-test (kubectl plugin list) (:inherit-global-flags t))
(pcmpl-me-test (kubectl plugin) (:inherit-global-flags t))
(pcmpl-me-test (kubectl port-forward) (:inherit-global-flags t))
(pcmpl-me-test (kubectl proxy) (:inherit-global-flags t))
(pcmpl-me-test (kubectl replace) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout history daemonset) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout history deployment) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout history statefulset) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout history) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout pause deployment) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout pause) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout restart daemonset) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout restart deployment) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout restart statefulset) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout restart) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout resume deployment) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout resume) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout status daemonset) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout status deployment) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout status statefulset) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout status) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout undo daemonset) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout undo deployment) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout undo statefulset) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout undo) (:inherit-global-flags t))
(pcmpl-me-test (kubectl rollout) (:inherit-global-flags t))
(pcmpl-me-test (kubectl run) (:inherit-global-flags t))
(pcmpl-me-test (kubectl scale deployment) (:inherit-global-flags t))
(pcmpl-me-test (kubectl scale replicaset) (:inherit-global-flags t))
(pcmpl-me-test (kubectl scale replicationcontroller) (:inherit-global-flags t))
(pcmpl-me-test (kubectl scale statefulset) (:inherit-global-flags t))
(pcmpl-me-test (kubectl scale) (:inherit-global-flags t))
(pcmpl-me-test (kubectl set env) (:inherit-global-flags t))
(pcmpl-me-test (kubectl set image) (:inherit-global-flags t))
(pcmpl-me-test (kubectl set resources) (:inherit-global-flags t))
(pcmpl-me-test (kubectl set selector) (:inherit-global-flags t))
(pcmpl-me-test (kubectl set serviceaccount) (:inherit-global-flags t))
(pcmpl-me-test (kubectl set) (:inherit-global-flags t))
(pcmpl-me-test (kubectl taint node) (:inherit-global-flags t))
(pcmpl-me-test (kubectl taint) (:inherit-global-flags t))
(pcmpl-me-test (kubectl top node) (:inherit-global-flags t))
(pcmpl-me-test (kubectl top pod) (:inherit-global-flags t))
(pcmpl-me-test (kubectl top) (:inherit-global-flags t))
(pcmpl-me-test (kubectl uncordon) (:inherit-global-flags t))
(pcmpl-me-test (kubectl version) (:inherit-global-flags t))
(pcmpl-me-test (kubectl wait) (:inherit-global-flags t))


(provide 'pcmpl-kubectl-test)
;;; pcmpl-kubectl-test.el ends here
