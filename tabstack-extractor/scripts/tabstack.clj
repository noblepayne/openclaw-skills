#!/usr/bin/env bb
;; Tabstack API wrapper in Babashka (using built-in http-client)
;; Usage: bb tabstack.clj <command> [args]
;; Configuration: Set TABSTACK_API_KEY environment variable

(ns tabstack
  (:require [babashka.http-client :as http]
            [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(def base-url "https://api.tabstack.ai/v1")

(defn get-api-key []
  (let [env-key (System/getenv "TABSTACK_API_KEY")
        config-file (str (System/getProperty "user.home") "/.config/tabstack/config.edn")
        config-key (when (fs/exists? config-file)
                     (try
                       (-> (slurp config-file)
                           (read-string)
                           (:api-key))
                       (catch Exception _ nil)))]
    (or env-key config-key)))

(defn check-api-key []
  (let [api-key (get-api-key)]
    (when (str/blank? api-key)
      (println "⚠️  Warning: TABSTACK_API_KEY not configured")
      (println "Set it with: export TABSTACK_API_KEY=\"your_api_key_here\"")
      (println "Or create ~/.config/tabstack/config.edn with {:api-key \"...\"}"))
    api-key))

(defn headers []
  (let [api-key (check-api-key)]
    (when api-key
      {"Authorization" (str "Bearer " api-key)
       "Content-Type" "application/json"})))

(defn test-connection []
  (println "Testing Tabstack API connection...")
  (let [headers (headers)]
    (if (nil? headers)
      (do (println "❌ No API key configured")
          false)
      (try
        (let [response (http/get (str base-url "/")
                                 {:headers headers
                                  :throw false})]
          (if (= 200 (:status response))
            (do (println "✅ Tabstack API connection successful")
                true)
            (do (println "❌ Tabstack API connection failed")
                false)))
        (catch Exception e
          (println "❌ Connection error:" (.getMessage e))
          false)))))

(defn extract-markdown [url]
  (println "Extracting markdown from:" url)
  (let [headers (headers)]
    (if (nil? headers)
      (do (println "❌ No API key configured")
          nil)
      (try
        (let [response (http/post (str base-url "/extract/markdown")
                                  {:headers headers
                                   :body (json/generate-string {:url url})
                                   :throw false})]
          (if (= 200 (:status response))
            (:body response)
            (do (println "Error:" (:status response) (:body response))
                nil)))
        (catch Exception e
          (println "Error:" (.getMessage e))
          nil)))))

(defn extract-json [url schema-file & {:keys [timeout-ms]
                                       :or {timeout-ms 45000}}]
  (println "Extracting JSON from:" url)
  (println "Using schema:" schema-file)
  (let [headers (headers)]
    (if (nil? headers)
      (do (println "❌ No API key configured")
          nil)
      (try
        (let [schema (json/parse-string (slurp schema-file))
              response (http/post (str base-url "/extract/json")
                                  {:headers headers
                                   :body (json/generate-string {:url url
                                                          :json_schema schema})
                                   :throw false})]
          (cond
            (= 200 (:status response))
            (json/parse-string (:body response))
            
            (= 408 (:status response)) ; Timeout
            (do (println "⏱️  Timeout after" timeout-ms "ms - try simpler schema or increase timeout")
                nil)
            
            :else
            (do (println "Error:" (:status response) (:body response))
                nil)))
        (catch java.net.http.HttpTimeoutException e
          (println "⏱️  Request timeout:" (.getMessage e))
          nil)
        (catch Exception e
          (println "Error:" (.getMessage e))
          nil)))))

(defn extract-json-with-retry 
  "Extract JSON with retry logic"
  [url schema-file & {:keys [max-retries delay-ms timeout-ms]
                      :or {max-retries 3 delay-ms 1000 timeout-ms 45000}}]
  (loop [retries 0]
    (let [result (extract-json url schema-file :timeout-ms timeout-ms)]
      (cond
        result result
        (>= retries max-retries) (do (println "Max retries reached") nil)
        :else (do (println (str "Retry " (inc retries) "/" max-retries "..."))
                  (Thread/sleep delay-ms)
                  (recur (inc retries)))))))

(defn cache-result [key data]
  "Simple file-based caching"
  (let [cache-dir (str (System/getProperty "user.home") "/.cache/tabstack")
        cache-file (str cache-dir "/" (hash key) ".edn")]
    (fs/create-dirs cache-dir)
    (spit cache-file (pr-str {:timestamp (System/currentTimeMillis)
                              :data data}))))

(defn get-cached [key max-age-ms]
  "Get cached result if not expired"
  (let [cache-dir (str (System/getProperty "user.home") "/.cache/tabstack")
        cache-file (str cache-dir "/" (hash key) ".edn")]
    (when (fs/exists? cache-file)
      (let [cached (read-string (slurp cache-file))
            age (- (System/currentTimeMillis) (:timestamp cached))]
        (when (< age max-age-ms)
          (:data cached))))))

(defn extract-with-cache [url schema-file & {:keys [cache-ms]
                                             :or {cache-ms (* 1000 60 60 24)}}]
  "Extract with caching (default 24 hours)"
  (let [cache-key (str url "::" schema-file)
        cached (get-cached cache-key cache-ms)]
    (if cached
      (do (println "Using cached result")
          cached)
      (let [result (extract-json url schema-file)]
        (when result
          (cache-result cache-key result))
        result))))

(defn batch-extract [urls schema-file & {:keys [concurrent batch-size]
                                         :or {concurrent 3 batch-size 10}}]
  "Extract from multiple URLs with concurrency control"
  (let [batches (partition-all batch-size urls)]
    (mapcat (fn [batch]
              (->> batch
                   (pmap (fn [url] [url (extract-json url schema-file)]))
                   (filter (fn [[_ result]] result))
                   (map (fn [[url result]] (assoc result :source_url url)))))
            batches)))

;; Command line interface
(defn -main [& args]
  (let [command (first args)]
    (case command
      "test" (test-connection)
      "markdown" (when-let [url (second args)]
                   (println (extract-markdown url)))
      "json" (when-let [[_ url schema-file] args]
               (pprint (extract-json url schema-file)))
      "json-retry" (when-let [[_ url schema-file] args]
                     (pprint (extract-json-with-retry url schema-file)))
      "json-cache" (when-let [[_ url schema-file] args]
                     (pprint (extract-with-cache url schema-file)))
      "batch" (when-let [[_ urls-file schema-file] args]
                (let [urls (str/split-lines (slurp urls-file))]
                  (pprint (batch-extract urls schema-file))))
      (do (println "Usage: bb tabstack.clj <command> [args]")
          (println "Commands:")
          (println "  test                 - Test API connection")
          (println "  markdown <url>       - Extract markdown from URL")
          (println "  json <url> <schema>  - Extract JSON using schema file")
          (println "  json-retry <url> <schema> - Extract with retry logic")
          (println "  json-cache <url> <schema> - Extract with caching")
          (println "  batch <urls-file> <schema> - Batch extract from URLs")))))

;; Run if called directly
(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))