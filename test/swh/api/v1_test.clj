(ns swh.api.v1-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [swh.api.v1 :as api]
            [swhid.core :as swhid]))

(def cnt "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2")
(def dir "swh:1:dir:d198bc9d7a6bcf6db04f476d29314f157507d505")

(defn- fake
  "Fake :request fn: records requests in `calls`, replies with `responses`
  (one per call, last one repeated)."
  [calls & responses]
  (let [n (atom -1)]
    (fn [req]
      (swap! calls conj req)
      (nth responses (min (swap! n inc) (dec (count responses)))))))

(defn- never
  "A :request fn for calls that must not happen."
  [_]
  (throw (AssertionError. "unexpected request")))

(defmacro ^:private caught
  "The ex-info thrown by `body`, nil when none."
  [& body]
  `(try ~@body nil (catch clojure.lang.ExceptionInfo e# e#)))

(defn- json-response [m]
  {:status 200 :headers {} :body (json/generate-string m)})

(defn- cnt-ids
  "`n` distinct content SWHIDs."
  [n]
  (mapv #(str "swh:1:cnt:" (format "%040d" %)) (range n)))

(deftest known-test
  (testing "returns {swhid boolean}, accepts strings and maps"
    (let [calls (atom [])
          resp (json-response {cnt {:known true} dir {:known false}})
          result (api/known [cnt (swhid/parse dir)]
                            {:request (fake calls resp)})]
      (is (= {cnt true dir false} result))
      (let [req (first @calls)]
        (is (= :post (:method req)))
        (is (= (str api/default-base-url "/known/") (:uri req)))
        (is (= [cnt dir] (json/parse-string (:body req)))))))
  (testing "batches by 1000"
    (let [calls (atom [])
          ids (cnt-ids 1500)
          reply (fn [req]
                  (swap! calls conj req)
                  (json-response (into {} (map (fn [id] [id {:known true}]))
                                       (json/parse-string (:body req)))))
          result (api/known ids {:request reply})]
      (is (= 2 (count @calls)))
      (is (= 1500 (count result)))
      (is (every? true? (vals result)))))
  (testing "a failing batch keeps the previous results under :partial"
    (let [ids (cnt-ids 1500)
          ok (json-response (into {} (map (fn [id] [id {:known true}])) (take 1000 ids)))
          ko {:status 429 :headers {} :body ""}
          partial-of (fn [& responses]
                       (caught (api/known ids {:request (apply fake (atom []) responses)})))
          e (partial-of ok ko)]
      (is (= (zipmap (take 1000 ids) (repeat true)) (:partial (meta (ex-data e)))))
      (is (not (contains? (ex-data e) :partial)) "kept out of the printed ex-data")
      (is (= (ex-data e) (ex-data (ex-cause e))) "the cause is the original error")
      (is (= {} (:partial (meta (ex-data (partial-of ko))))) "first batch failing")))
  (testing "a 2xx body that is not a JSON object throws instead of answering false"
    (let [ids (cnt-ids 1500)
          ok (json-response (into {} (map (fn [id] [id {:known true}])) (take 1000 ids)))]
      (doseq [bad [{:status 200 :headers {} :body ""}
                   (json-response [])]]
        (let [e (caught (api/known ids {:request (fake (atom []) ok bad)}))]
          (is (instance? clojure.lang.ExceptionInfo e) (:body bad))
          (is (= (zipmap (take 1000 ids) (repeat true)) (:partial (meta (ex-data e))))
              (:body bad))))))
  (testing "other exceptions go through unwrapped"
    (is (thrown? java.io.IOException
                 (api/known [cnt] {:request (fn [_] (throw (java.io.IOException. "down")))}))))
  (testing "no SWHID, no call"
    (is (= {} (api/known [] {:request never}))))
  (testing "invalid SWHID throws before any call"
    (is (thrown? clojure.lang.ExceptionInfo
                 (api/known ["not-a-swhid"] {:request never}))))
  (testing "a qualified SWHID string is reduced to its core form"
    (let [calls (atom [])]
      (api/known [(str cnt ";origin=https://example.org")]
                 {:request (fake calls (json-response {cnt {:known true}}))})
      (is (= [cnt] (json/parse-string (:body (first @calls))))))))

(deftest auth-and-errors-test
  (testing "token becomes a bearer header"
    (let [calls (atom [])]
      (api/known [cnt] {:token "s3cret"
                        :request (fake calls (json-response {cnt {:known true}}))})
      (is (= "Bearer s3cret" (get-in (first @calls) [:headers "Authorization"])))))
  (testing "requests carry a timeout, 60 s by default"
    (let [calls (atom [])]
      (api/known [cnt] {:request (fake calls (json-response {cnt {:known true}}))})
      (is (= api/default-timeout-ms (:timeout (first @calls))))
      (api/known [cnt] {:timeout-ms 5000
                        :request (fake calls (json-response {cnt {:known true}}))})
      (is (= 5000 (:timeout (last @calls))))))
  (testing "429 throws with retry-after"
    (let [e (caught (api/known [cnt] {:request (fn [_] {:status 429
                                                        :headers {"retry-after" "42"}
                                                        :body ""})}))]
      (is (:rate-limited (ex-data e)))
      (is (= 42 (:retry-after (ex-data e))))))
  (testing "429 with a repeated retry-after header (vector value)"
    (let [e (caught (api/known [cnt] {:request (fn [_] {:status 429
                                                        :headers {"retry-after" ["42" "42"]}
                                                        :body ""})}))]
      (is (= 42 (:retry-after (ex-data e))))))
  (testing "other errors throw with status"
    (let [e (caught (api/known [cnt] {:request (fn [_] {:status 500 :headers {} :body "{}"})}))]
      (is (= 500 (:status (ex-data e))))))
  (testing "a non-JSON 2xx body (captive portal, proxy) is an error with its status"
    (let [e (caught (api/known [cnt] {:request (fn [_] {:status 200 :headers {}
                                                        :body "<html>Log in</html>"})}))]
      (is (= 200 (:status (ex-data e))))
      (is (= "<html>Log in</html>" (:body (ex-data e))))))
  (testing "non-JSON error bodies are kept raw"
    (let [e (caught (api/known [cnt] {:request (fn [_] {:status 502 :headers {}
                                                        :body "<html>Bad Gateway</html>"})}))]
      (is (= 502 (:status (ex-data e))))
      (is (= "<html>Bad Gateway</html>" (:body (ex-data e)))))))

(deftest rate-limit-test
  (let [headers {"x-ratelimit-limit" "120"
                 "x-ratelimit-remaining" ["117"] ; repeated header
                 "x-ratelimit-reset" "1790000000"}
        rl {:limit 120 :remaining 117 :reset 1790000000}
        with-headers (fn [m] (assoc (json-response m) :headers headers))
        remaining (fn [n] (update (with-headers {}) :headers
                                  assoc "x-ratelimit-remaining" (str n)))]
    (testing "X-RateLimit-* headers end up in the metadata of the result"
      (is (= rl (:rate-limit
                 (meta (api/resolve-swhid
                        cnt {:request (fn [_] (with-headers {:browse_url "…"}))})))))
      (is (= rl (:rate-limit
                 (meta (api/save-requests
                        "https://example.org/repo"
                        {:request (fn [_] (with-headers []))}))))))
    (testing "known carries the latest rate limit received"
      (is (= (assoc rl :remaining 116)
             (:rate-limit
              (meta (api/known (cnt-ids 1500)
                               {:request (fake (atom []) (remaining 117) (remaining 116))})))))
      (is (= rl
             (:rate-limit
              (meta (api/known (cnt-ids 1500)
                               {:request (fake (atom []) (remaining 117) (json-response {}))}))))
          "a last batch without headers keeps the previous state"))
    (testing "no headers, no metadata"
      (is (nil? (meta (api/resolve-swhid
                       cnt {:request (fn [_] (json-response {:browse_url "…"}))})))))
    (testing "an empty body gives nil, which carries no metadata"
      (is (nil? (api/resolve-swhid
                 cnt {:request (fn [_] {:status 200 :headers headers :body ""})}))))
    (testing "missing or non-numeric headers are left out"
      (is (= {:limit 120}
             (:rate-limit
              (meta (api/resolve-swhid
                     cnt {:request (fn [_] (assoc (json-response {})
                                                  :headers {"x-ratelimit-limit" "120"
                                                            "x-ratelimit-reset" "soon"}))}))))))
    (testing "HTTP errors carry the rate limit in their ex-data"
      (doseq [status [429 500]]
        (let [e (caught (api/known [cnt] {:request (fn [_] {:status status
                                                            :headers headers
                                                            :body ""})}))]
          (is (= rl (:rate-limit (ex-data e))) status)
          (is (= "/known/" (:path (ex-data e))) status))))))

(deftest save-code-now-test
  (let [calls (atom [])
        resp (json-response {:save_request_status "accepted"})]
    (testing "POST on the origin save endpoint, git by default"
      (is (= "accepted"
             (:save_request_status
              (api/save-code-now! "https://github.com/bzg/swhid-clj"
                                  {:request (fake calls resp)}))))
      (is (= :post (:method (first @calls))))
      (is (= (str api/default-base-url
                  "/origin/save/git/url/https://github.com/bzg/swhid-clj/")
             (:uri (first @calls)))))
    (testing "save-requests is the GET counterpart, returning a vector"
      (is (vector? (api/save-requests "https://github.com/bzg/swhid-clj"
                                      {:request (fake calls (json-response []))})))
      (is (= :get (:method (last @calls))))
      (is (= (:uri (first @calls)) (:uri (last @calls)))))
    (testing ":visit-type"
      (api/save-code-now! "https://hg.example.org/repo"
                          {:visit-type "hg" :request (fake calls resp)})
      (is (str/includes? (:uri (last @calls)) "/origin/save/hg/url/")))
    (testing "what the path cannot hold raw is percent-encoded"
      (api/save-code-now! "https://example.org/a repo?b=1"
                          {:request (fake calls resp)})
      (is (str/ends-with? (:uri (last @calls))
                          "/url/https://example.org/a%20repo%3Fb=1/")))))

(deftest latest-visit-test
  (testing "GET on /origin/…/visit/latest/, requiring a snapshot"
    (let [calls (atom [])
          visit {:date "2026-07-01T03:14:15+00:00" :status "full"
                 :snapshot "0000000000000000000000000000000000000042"}
          result (api/latest-visit "https://github.com/bzg/swhid-clj"
                                   {:request (fake calls (json-response visit))})]
      (is (= "2026-07-01T03:14:15+00:00" (:date result)))
      (is (= :get (:method (first @calls))))
      (is (= (str api/default-base-url
                  "/origin/https://github.com/bzg/swhid-clj/visit/latest/"
                  "?require_snapshot=true")
             (:uri (first @calls))))))
  (testing ":require-snapshot? false drops the query string"
    (let [calls (atom [])]
      (api/latest-visit "https://example.org/repo"
                        {:require-snapshot? false
                         :request (fake calls (json-response {:status "full"}))})
      (is (not (str/includes? (:uri (first @calls)) "?")))))
  (testing "404 means not archived: nil, no throw"
    (is (nil? (api/latest-visit "https://example.org/unknown"
                                {:request (fn [_] {:status 404 :headers {}
                                                   :body "{}"})}))))
  (testing "other errors still throw"
    (is (thrown? clojure.lang.ExceptionInfo
                 (api/latest-visit "https://example.org/repo"
                                   {:request (fn [_] {:status 500 :headers {}
                                                      :body "{}"})}))))
  (testing "what the path cannot hold raw is percent-encoded, not cut"
    (let [calls (atom [])]
      (api/latest-visit "https://example.org/repo?x=1#y"
                        {:request (fake calls (json-response {:status "full"}))})
      (is (= (str api/default-base-url
                  "/origin/https://example.org/repo%3Fx=1%23y/visit/latest/"
                  "?require_snapshot=true")
             (:uri (first @calls)))))))

(deftest visits-test
  (testing "GET on /origin/…/visits/, 10 per page by default"
    (let [calls (atom [])
          visits [{:visit 2 :date "2026-10-19T09:15:55.367000+00:00" :status "full"}
                  {:visit 1 :date "2026-07-01T03:14:15+00:00" :status "partial"}]
          result (api/visits "https://github.com/bzg/swhid-clj"
                             {:request (fake calls (json-response visits))})]
      (is (= [2 1] (mapv :visit result)))
      (is (= (str api/default-base-url
                  "/origin/https://github.com/bzg/swhid-clj/visits/?per_page=10")
             (:uri (first @calls))))
      (api/visits "https://example.org/repo" {:per-page 3 :request (fake calls (json-response []))})
      (is (str/ends-with? (:uri (last @calls)) "?per_page=3"))))
  (testing "404 means not archived: nil, no throw"
    (is (nil? (api/visits "https://example.org/unknown"
                          {:request (fn [_] {:status 404 :headers {} :body "{}"})})))))

(deftest resolve-swhid-test
  (let [calls (atom [])]
    (api/resolve-swhid (swhid/parse cnt)
                       {:request (fake calls (json-response {:browse_url "…"}))})
    (is (= (str api/default-base-url "/resolve/" cnt "/")
           (:uri (first @calls))))
    (testing "qualifiers of a SWHID map are kept"
      (api/resolve-swhid (swhid/parse (str cnt ";origin=https://example.org/repo"))
                         {:request (fake calls (json-response {:browse_url "…"}))})
      (is (= (str api/default-base-url "/resolve/" cnt ";origin=https://example.org/repo/")
             (:uri (last @calls)))))
    (testing "a hand-built SWHID map is validated before any call"
      (is (thrown? clojure.lang.ExceptionInfo
                   (api/resolve-swhid {:type :content :hash "zz/../../x"} {:request never}))))
    (testing "qualifier values are percent-encoded, their escapes included"
      ;; the API decodes the path once: %3B must reach it as %253B
      (api/resolve-swhid (str cnt ";origin=https://example.org/a%3Bb?x=1;path=/a[b]")
                         {:request (fake calls (json-response {:browse_url "…"}))})
      (is (= (str api/default-base-url "/resolve/" cnt
                  ";origin=https://example.org/a%253Bb%3Fx=1;path=/a%255Bb%255D/")
             (:uri (last @calls)))))))

(deftest whereis-test
  (let [calls (atom [])
        found (str cnt ";origin=https://example.org/lib;anchor=swh:1:rel:"
                   (apply str (repeat 40 "1")))]
    (is (= found (api/whereis (str cnt ";lines=1-2")
                              {:request (fake calls (json-response found))})))
    (is (= (str api/default-base-url "/provenance/whereis/" cnt "/")
           (:uri (first @calls)))
        "the qualifiers are dropped")
    (testing "nil when the archive knows no provenance"
      (is (nil? (api/whereis cnt {:request (fake calls {:status 200 :headers {} :body "null"})})))
      (is (nil? (api/whereis cnt {:request (fake calls {:status 404 :headers {} :body ""})}))))
    (testing "the permission is required"
      (is (= 403 (:status (ex-data (caught (api/whereis cnt {:request (fake calls {:status 403 :headers {} :body ""})})))))))))

;; The one pure part the tests through :request do not cover.
(deftest path-encode-test
  (is (= "https://h.example:80/a-b_c.d~e/!$&'()*+,;=@x"
         (#'api/path-encode "https://h.example:80/a-b_c.d~e/!$&'()*+,;=@x"))
      "pchar and / stay raw")
  (is (= "%25%3F%23%20%5B%5D%7C%22%3C%3E%5C%5E%60%7B%7D"
         (#'api/path-encode "%?# []|\"<>\\^`{}")))
  (is (= "caf%C3%A9-%F0%9F%A6%8A" (#'api/path-encode "café-🦊"))
      "non-ASCII as UTF-8, astral code points whole"))

;; Integration tests against the real archive; run with SWH_API_TESTS=1.
(deftest ^:integration integration-test
  (when (System/getenv "SWH_API_TESTS")
    (testing "known"
      (is (= {cnt true
              "swh:1:cnt:0000000000000000000000000000000000000042" false}
             (api/known [cnt "swh:1:cnt:0000000000000000000000000000000000000042"]))))
    (testing "resolve"
      (is (string? (:browse_url (api/resolve-swhid cnt)))))))
