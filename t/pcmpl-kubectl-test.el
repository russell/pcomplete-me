;;; pcmpl-kubectl-test.el --- pcmpl-kubectl Tests    -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Russell Sim

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

(require 'thunk)

(require 'pcomplete-me-test)
(require 'pcmpl-kubectl)
(require 'bash-completion-export-utils)

(defconst pcmpl-kubectl-test-bashinit (list
                                       (format "%s/pcmpl-kubectl-test.sh"
                                               (cond
                                                (buffer-file-name
                                                 (file-name-directory buffer-file-name))
                                                (load-file-name
                                                 (file-name-directory load-file-name))))))

(defun pcmpl-me-kubectl-test-completion (command-list type &optional global-flags)
  "COMMAND-LIST as a representation of the command"
  (let ((command (mapconcat 'symbol-name command-list " "))
        (bash-completion-start-files pcmpl-kubectl-test-bashinit))
    (rs//bash-complete-isolated
     (cl-case type
       (:flags (rs//bash-complete-flags command global-flags))
       (:subcommand (rs//bash-complete-subcommand command))))))


(pcmpl-me-test (kubectl annotate) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl api-resources) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl api-versions) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl apply edit-last-applied) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl apply set-last-applied) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl apply view-last-applied) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl apply) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl attach) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl auth can-i) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl auth reconcile) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl auth) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl autoscale deployment) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl autoscale replicaset) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl autoscale replicationcontroller) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl autoscale statefulset) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl autoscale) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl certificate approve) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl certificate deny) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl certificate) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl cluster-info dump) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl cluster-info) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl completion bash) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl completion zsh) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl completion fish) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl completion powershell) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl completion) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl config current-context) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl config delete-cluster) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl config delete-context) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl config delete-user) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl config get-clusters) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl config get-contexts) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl config get-users) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl config rename-context) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl config set) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl config set-cluster) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl config set-context) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl config set-credentials) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl config unset) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl config use-context) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl config view) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl config) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl cordon) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl cp) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create clusterrole) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create clusterrolebinding) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create configmap) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create cronjob) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create deployment) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create ingress) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create job) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create namespace) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create poddisruptionbudget) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create priorityclass) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create quota) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create role) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create rolebinding) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create secret docker-registry) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create secret generic) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create secret tls) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create secret) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create service clusterip) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create service externalname) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create service loadbalancer) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create service nodeport) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create service) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create serviceaccount) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl create) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl debug) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl delete) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl describe) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl diff) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl drain) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl edit) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl exec) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl explain) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl expose deployment) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl expose pod) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl expose replicaset) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl expose replicationcontroller) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl expose service) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl expose) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl get) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl help) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl kustomize) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl label) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl logs) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl options) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl patch) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl plugin list) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl plugin) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl port-forward) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl proxy) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl replace) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout history daemonset) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout history deployment) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout history statefulset) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout history) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout pause deployment) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout pause) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout restart daemonset) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout restart deployment) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout restart statefulset) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout restart) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout resume deployment) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout resume) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout status daemonset) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout status deployment) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout status statefulset) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout status) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout undo daemonset) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout undo deployment) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout undo statefulset) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout undo) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl rollout) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl run) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl scale deployment) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl scale replicaset) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl scale replicationcontroller) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl scale statefulset) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl scale) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl set env) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl set image) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl set resources) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl set selector) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl set serviceaccount) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl set) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl taint node) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl taint) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl top node) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl top pod) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl top) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl uncordon) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl version) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))
(pcmpl-me-test (kubectl wait) (pcmpl-me-kubectl-test-completion :inherit-global-flags t))


(provide 'pcmpl-kubectl-test)
;;; pcmpl-kubectl-test.el ends here
