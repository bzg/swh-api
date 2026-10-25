(ns build
  "clojure -T:build jar | install | deploy"
  (:require [clojure.edn :as edn]
            [clojure.tools.build.api :as b]))

(def lib 'io.github.bzg/swh-api)
(def version "0.1.0")
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (clean nil)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis @basis
                :src-dirs ["src"]
                :scm {:url "https://github.com/bzg/swh-api"
                      :connection "scm:git:git://github.com/bzg/swh-api.git"
                      :developerConnection "scm:git:ssh://git@github.com/bzg/swh-api.git"
                      :tag (str "v" version)}
                :pom-data [[:description "Client for the Software Heritage Web API"]
                           [:url "https://github.com/bzg/swh-api"]
                           [:licenses
                            [:license
                             [:name "Eclipse Public License 2.0"]
                             [:url "https://www.eclipse.org/legal/epl-2.0/"]]]]})
  (b/copy-dir {:src-dirs ["src"] :target-dir class-dir})
  (b/jar {:class-dir class-dir :jar-file jar-file}))

(defn install [_]
  (jar nil)
  (b/install {:basis @basis
              :lib lib
              :version version
              :jar-file jar-file
              :class-dir class-dir}))

(defn- assert-no-local-deps
  "tools.build silently drops :local/root deps from the pom; switch them
  to :mvn/version before deploying."
  []
  (when-let [locals (->> (:deps (edn/read-string (slurp "deps.edn")))
                         (filter (comp :local/root val))
                         (map key)
                         seq)]
    (throw (ex-info "Refusing to deploy with :local/root deps" {:deps locals}))))

(defn deploy
  "Needs CLOJARS_USERNAME and CLOJARS_PASSWORD (deploy token)."
  [_]
  (assert-no-local-deps)
  (jar nil)
  ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
   {:installer :remote
    :artifact (b/resolve-path jar-file)
    :pom-file (b/pom-path {:lib lib :class-dir class-dir})}))
