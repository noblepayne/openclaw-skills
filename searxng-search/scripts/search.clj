#!/usr/bin/env bb
;; SearXNG web search client
;; Usage: bb search.clj "query" [options-json]

(require '[babashka.http-client :as http]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[clojure.java.io :as io])

;; Configuration
(def searxng-url (System/getenv "SEARXNG_URL"))
(def rate-limit-file ".searxng-last-request")
(def min-delay-ms 1000) ; Minimum 1 second between requests

(defn get-searxng-url []
  (or (System/getenv "SEARXNG_URL")
      "http://localhost:8888"))

(defn check-rate-limit []
  (when (.exists (io/file rate-limit-file))
    (let [last-request (Long/parseLong (slurp rate-limit-file))
          now (System/currentTimeMillis)
          elapsed (- now last-request)]
      (when (< elapsed min-delay-ms)
        (Thread/sleep (- min-delay-ms elapsed))))))

(defn update-rate-limit []
  (spit rate-limit-file (str (System/currentTimeMillis))))

;; Search function
(defn search [query options]
  (let [params (merge {:q query :format "json"} options)
        url (str searxng-url "/search")]
    (try
      (check-rate-limit)
      (let [response (http/get url {:query-params params
                                    :timeout 30000
                                    :connect-timeout 5000
                                    :throw true})]
        (update-rate-limit)
        (json/parse-string (:body response) true))
      (catch clojure.lang.ExceptionInfo e
        (let [data (ex-data e)
              status (:status data)]
          (cond
            (= status 429)
            (do (println "Error: Rate limited by SearXNG. Please wait a moment.")
                (System/exit 1))
            
            (= status 404)
            (do (println "Error: SearXNG endpoint not found. Check SEARXNG_URL.")
                (System/exit 1))
            
            (>= status 500)
            (do (println (str "Error: SearXNG server error (" status ")"))
                (System/exit 1))
            
            :else
            (do (println (str "Error: HTTP " status " - " (.getMessage e)))
                (System/exit 1)))))
      (catch java.net.SocketTimeoutException e
        (println "Error: Connection timeout. Is SearXNG running?")
        (println (str "Tried to connect to: " searxng-url))
        (System/exit 1))
      (catch java.net.ConnectException e
        (println "Error: Cannot connect to SearXNG.")
        (println (str "Tried to connect to: " searxng-url))
        (println "Check that SearXNG is running and SEARXNG_URL is correct.")
        (System/exit 1))
      (catch Exception e
        (println (str "Error: " (.getMessage e)))
        (System/exit 1)))))

;; Format results for output
(defn format-result [idx result]
  (let [{:keys [title url content score engines]} result
        score-str (if score (format "%.2f" score) "N/A")
        engines-str (str/join ", " (or engines []))
        content-clean (-> (or content "")
                          (str/replace #"\s+" " ")
                          (str/trim))]
    (str "\n" (inc idx) ". " title " [Score: " score-str "]\n"
         "   URL: " url "\n"
         "   " (subs content-clean 0 (min 200 (count content-clean)))
         (when (> (count content-clean) 200) "...")
         "\n"
         (when (seq engines-str)
           (str "   Engines: " engines-str "\n")))))

(defn format-results [data num-results]
  (let [{:keys [query number_of_results results]} data
        sorted-results (take num-results
                            (sort-by #(or (:score %) 0) > results))]
    (if (empty? sorted-results)
      (str "No results found for: \"" query "\"")
      (str "Search Results for \"" query "\"\n"
           "Found " number_of_results " total results\n"
           (str/join (map-indexed format-result sorted-results))))))

;; Main execution
(defn -main [& args]
  (when (empty? args)
    (println "Usage: bb search.clj \"query\" [options-json]")
    (println "Example: bb search.clj \"NixOS\" '{\"category\": \"it\"}'")
    (System/exit 1))
  
  (let [query (first args)
        options-json (second args)
        options (if options-json
                 (try
                   (json/parse-string options-json true)
                   (catch Exception e
                     (println "Error: Invalid JSON options")
                     (System/exit 1)))
                 {})
        num-results (or (:num_results options) 5)
        search-params (-> options
                         (dissoc :num_results)
                         (update :language #(or % "en")))]
    
    ;; Perform search
    (let [results (search query search-params)]
      (println (format-results results num-results)))))

;; Run if called as script
(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
