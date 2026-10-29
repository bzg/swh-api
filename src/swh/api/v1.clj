(ns swh.api.v1
  "Minimal client for the Software Heritage Web API
  (https://archive.softwareheritage.org/api/1/).

  Every function takes an optional `opts` map as last argument:
    {:base-url    \"https://archive.softwareheritage.org/api/1\" ; default
     :token       \"…\"   ; SWH bearer token (raises rate limits)
     :timeout-ms  60000  ; per request; default 60 s
     :keywordize? true  ; keyword keys in parsed responses
     :request     f}     ; testing hook: replaces the HTTP call

  Responses are parsed JSON. HTTP errors throw ex-info with :status and
  :path in their ex-data; a 429 also carries {:rate-limited true
  :retry-after <seconds>}.

  The X-RateLimit-* headers of a response are exposed as
  {:rate-limit {:limit <n> :remaining <n> :reset <epoch seconds>}}
  in the metadata of the result when it is a map or a vector (a nil
  result carries none), and in the ex-data of HTTP errors. A key is
  absent when its header is missing or not a number.

  Runs on the JVM and Babashka (babashka.http-client and cheshire are
  built into bb)."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.string :as str]
            [swhid.core :as swhid]))

(def default-base-url "https://archive.softwareheritage.org/api/1")

(defn- parse-body [body keywordize?]
  (when-not (str/blank? body)
    ;; strict: a top-level JSON array is a vector, not a lazy seq
    (json/parse-string-strict body keywordize?)))

(defn- header-long
  "Header `k` of `headers` as a long, nil when absent or not a number."
  [headers k]
  ;; the header value is a vector when repeated; str because a :request
  ;; hook may return numbers
  (let [v (get headers k)]
    (some-> (if (coll? v) (first v) v) str parse-long)))

(defn- rate-limit
  "Rate-limit state from the X-RateLimit-* `headers`, nil when absent."
  [headers]
  (not-empty
   (into {}
         (keep (fn [k]
                 (when-let [v (header-long headers (str "x-ratelimit-" (name k)))]
                   [k v])))
         [:limit :remaining :reset])))

(def default-timeout-ms
  "How long a request may take, in milliseconds."
  60000)

(defn- request-map
  "The HTTP request for `method` on `path` with JSON `body` (nil for none)."
  [{:keys [base-url token timeout-ms]
    :or {base-url default-base-url timeout-ms default-timeout-ms}}
   method path body]
  {:method method
   :uri (str base-url path)
   :headers (cond-> {"Accept" "application/json"}
              body (assoc "Content-Type" "application/json")
              token (assoc "Authorization" (str "Bearer " token)))
   :body (some-> body json/generate-string)
   :timeout timeout-ms
   :throw false}) ; parse-response handles the status

(defn- parse-response
  "The parsed body of the response `resp` to a request on `path`, with
  its rate limit as metadata; throws ex-info on HTTP errors."
  [{:keys [status headers body]} path keywordize?]
  (let [rl (rate-limit headers)
        error (fn [msg data]
                (ex-info msg (cond-> (assoc data :status status :path path)
                               rl (assoc :rate-limit rl))))]
    (cond
      (<= 200 status 299)
      ;; a captive portal or a proxy may answer HTML with a 200
      (let [result (try (parse-body body keywordize?)
                        (catch Exception e
                          (throw (error "SWH API answered with a non-JSON body"
                                        {:body body :cause (ex-message e)}))))]
        (cond-> result
          ;; nil (empty body) and scalars cannot carry metadata
          (and rl (instance? clojure.lang.IObj result))
          (vary-meta assoc :rate-limit rl)))
      (= 429 status)
      (throw (error "SWH API rate limit exceeded"
                    {:rate-limited true
                     :retry-after (header-long headers "retry-after")}))
      :else
      (throw (error (str "SWH API error " status)
                    ;; error bodies are not always JSON (proxy HTML pages)
                    {:body (try (parse-body body false)
                                (catch Exception _ body))})))))

(defn- request
  [{:keys [keywordize?] req-fn :request :or {keywordize? true} :as opts}
   method path body]
  (parse-response ((or req-fn http/request) (request-map opts method path body))
                  path keywordize?))

(defn- unless-404
  "Calls `f`; nil when it throws a 404 (unknown origin)."
  [f]
  (try
    (f)
    (catch clojure.lang.ExceptionInfo e
      (when-not (= 404 (:status (ex-data e)))
        (throw e)))))

;; Origin URLs and SWHIDs go in the path as is, `/` included (this is
;; what the SWH API and its official clients do); the API decodes the path
;; once, so what a path cannot hold raw is percent-encoded, % included.
(defn- path-encode
  "`s` with the characters that are not allowed raw in a URL path
  (RFC 3986 pchar, plus /) percent-encoded as UTF-8."
  [s]
  (str/replace s #"[^A-Za-z0-9\-._~!$&'()*+,;=:@/]"
               (fn [^String c]
                 (apply str (map #(format "%%%02X" (bit-and % 0xff))
                                 (.getBytes c "UTF-8"))))))

(defn- swhid-map
  "Validated SWHID map of `x` (map or string)."
  [x]
  (if (map? x)
    ;; make validates a hand-built map (swhid->string trusts it)
    (swhid/make (:type x) (:hash x) (:qualifiers x))
    (swhid/parse x)))

(defn- swhid-str
  "Core SWHID string of `x` (map or string); qualifiers are dropped."
  [x]
  (swhid/swhid->string (swhid/core-swhid (swhid-map x))))

;; API limit on /known/.
(def ^:private known-batch-size 1000)

(defn- known-batches
  "Core SWHID strings of `swhids`, by batches of the API size;
  throws on an invalid SWHID."
  [swhids]
  (partition-all known-batch-size (mapv swhid-str swhids)))

(defn- add-known
  "`acc` with the answer of /known/ `resp` for the SWHIDs of `batch`, and
  the metadata (rate limit) of `resp`."
  [acc batch resp]
  ;; get-in on anything else would answer false for all
  (when-not (map? resp)
    (throw (ex-info "SWH API answered /known/ with no JSON object"
                    {:path "/known/" :body resp})))
  (-> (into acc
            (map (fn [id] [id (boolean (get-in resp [id "known"]))]))
            batch)
      (vary-meta merge (meta resp))))

(defn- with-partial
  "ex-info `e` with the results so far, `acc`, in the metadata of its
  ex-data (not in the ex-data itself, which gets printed)."
  [e acc]
  (ex-info (ex-message e) (vary-meta (ex-data e) assoc :partial acc) e))

(defn known
  "Which of `swhids` (strings or SWHID maps) are present in the archive?
  Returns {core-swhid-string boolean}. Batches requests by 1000; the
  :rate-limit metadata is the latest one received. When a batch fails
  with an HTTP error, the results of the previous batches are under
  :partial in the metadata of the ex-data (not in the ex-data itself,
  which gets printed with the exception)."
  ([swhids] (known swhids nil))
  ([swhids opts]
   (reduce
    (fn [acc batch]
      (try
        (add-known acc batch (request (assoc opts :keywordize? false)
                                      :post "/known/" batch))
        (catch clojure.lang.ExceptionInfo e
          (throw (with-partial e acc)))))
    {}
    (known-batches swhids))))

(defn- resolve-path
  "Path resolving `swhid` (string or map, qualifiers allowed)."
  [swhid]
  (str "/resolve/" (path-encode (swhid/swhid->string (swhid-map swhid))) "/"))

(defn resolve-swhid
  "Resolve a SWHID (string or map, qualifiers allowed) into archive
  metadata, including a browse URL."
  ([swhid] (resolve-swhid swhid nil))
  ([swhid opts]
   (request opts :get (resolve-path swhid) nil)))

(defn whereis
  "Where the archive found the artifact `swhid` (string or map; its
  qualifiers are dropped): a qualified SWHID string whose :origin and
  :anchor qualifiers name an origin and the release or revision holding
  it there; nil when the archive knows none (it answers null, or a
  404). The provenance API needs an account with a special permission
  (a 401 or 403 otherwise)."
  ([swhid] (whereis swhid nil))
  ([swhid opts]
   (unless-404 #(request opts :get (str "/provenance/whereis/" (swhid-str swhid) "/") nil))))

(defn- latest-visit-path [origin-url require-snapshot?]
  (str "/origin/" (path-encode origin-url) "/visit/latest/"
       (when require-snapshot? "?require_snapshot=true")))

(defn- visits-path [origin-url per-page]
  (str "/origin/" (path-encode origin-url) "/visits/?per_page=" per-page))

(defn- save-path [visit-type origin-url]
  (str "/origin/save/" (path-encode visit-type) "/url/" (path-encode origin-url) "/"))

(defn latest-visit
  "Latest archival visit of `origin-url`, nil when the origin is not in
  the archive. Only visits that produced a snapshot count, unless
  :require-snapshot? is false in opts. The :date of the result is the
  date of the last archival."
  ([origin-url] (latest-visit origin-url nil))
  ([origin-url {:keys [require-snapshot?] :or {require-snapshot? true} :as opts}]
   (unless-404 #(request opts :get (latest-visit-path origin-url require-snapshot?) nil))))

(defn visits
  "The archival visits of `origin-url`, most recent first, as the archive
  lists them (:date, :status, :snapshot, …); nil when the origin is not
  in the archive. opts: :per-page (10 by default)."
  ([origin-url] (visits origin-url nil))
  ([origin-url {:keys [per-page] :or {per-page 10} :as opts}]
   (unless-404 #(request opts :get (visits-path origin-url per-page) nil))))

(defn save-code-now!
  "Ask the archive to archive `origin-url` now. Returns the created save
  request. opts: :visit-type (\"git\" by default, or \"hg\", \"svn\", …)."
  ([origin-url] (save-code-now! origin-url nil))
  ([origin-url {:keys [visit-type] :or {visit-type "git"} :as opts}]
   (request opts :post (save-path visit-type origin-url) nil)))

(defn save-requests
  "List the save requests already made for `origin-url`, most recent
  first. Same :visit-type option as save-code-now!."
  ([origin-url] (save-requests origin-url nil))
  ([origin-url {:keys [visit-type] :or {visit-type "git"} :as opts}]
   (request opts :get (save-path visit-type origin-url) nil)))
