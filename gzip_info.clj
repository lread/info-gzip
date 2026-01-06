(ns gzip-info
  "Lil quick and dirty to look at gzip files.

  The spec https://www.ietf.org/rfc/rfc1952.txt describes seldom used fields,
  like comment and extra fields. These fields aren't even used by the gzip
  tools.

  The JDK doesn't populate any optional fields, including the (file) name.

  Typically, a gzip file has 1 'member', but can have many.
  Each member, syntactically, can be thought of as an entire gzip file.
  On uncompress the gzip tool will concatenate these members together.
  This tool shows each member."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]))

(defn byte->unsigned [b]
  (bit-and b 0xff))

(defn bytes->number [bytes]
  (reduce (fn [acc [b shift-bits]]
            (bit-or acc
                    (bit-shift-left (byte->unsigned b) shift-bits)))
          0
          (map vector bytes (iterate #(+ 8 %) 0))))

(defn read-zero-terminated-str [is fieldname]
  (loop [bs []]
    (let [b (.read is)]
      (case b
        0 (String. (byte-array bs) "ISO-8859-1")
        -1 (throw (ex-info (str "Unexpected eof reading" fieldname) {}))
        (recur (conj bs b))))))

(defn read-bytes [is num-bytes desc]
  (loop [ndx 0
         buffer []]
    (if (= ndx num-bytes)
      (byte-array buffer)
      (let [b (.read is)]
        (when (= -1 b)
          (throw (ex-info (format "Unexpected eof reading %d bytes for %s"
                                  num-bytes desc) {})))
        (recur (inc ndx)
               (conj buffer b))))))

(defn gzip-member [is]
  (loop [ctx {:next :fixed-header}
         parsed {}]
    (case (:next ctx)
      :fixed-header
      (let [fixed-header-bytes (mapv byte->unsigned (read-bytes is 10 "fixed header"))
            id1 (get fixed-header-bytes 0)
            id2 (get fixed-header-bytes 1)
            cm (get fixed-header-bytes 2)
            flg (get fixed-header-bytes 3)
            mtime (bytes->number (subvec fixed-header-bytes 4 8))
            xfl (get fixed-header-bytes 8)
            os (get fixed-header-bytes 9)]
        (when-not (and (= 0x1f id1) (= 0x8b id2))
          (throw (ex-info (str "Expected [id1 id2] to be [0x1f 0x8b], but was: " (map #(format "%02x" %) [id1 id2])) {})))
        ;; reserved flg bits should be 0 and cm should be 8, but we won't throw if this is not the case
        (recur {:next :extra}
               (cond-> {:raw-fixed-header {:id1 id1
                                           :id2 id2
                                           :cm cm
                                           :flg flg
                                           :mtime mtime
                                           :xfl xfl
                                           :os os}
                        :flags {:ftext (bit-test flg 0)
                                :fhcrc (bit-test flg 1)
                                :fextra (bit-test flg 2)
                                :fname (bit-test flg 3)
                                :fcomment (bit-test flg 4)}
                        :os (get {0 :fat
                                  1 :amiga
                                  2 :vms
                                  3 :unix
                                  4 :vmcms
                                  5 :atari
                                  6 :hpfs
                                  7 :mac
                                  8 :zsystem
                                  9 :cpm
                                  10 :tops20
                                  11 :ntfs
                                  12 :qdos
                                  13 :acorn
                                  255 :unknown}
                                 os :invalid)
                        :xfl (get {0 :default 2 :best 4 :fast} xfl :invalid)}
                 (not (zero? mtime)) (assoc :mtime (str (java.time.Instant/ofEpochSecond mtime))))))
      :extra
      (recur {:next :original-file-name}
             (if-not (-> parsed :flags :fextra)
               parsed
               (let [extra-fields-length-bytes (read-bytes is 2 "extra fields length")
                     extra-fields-length (bytes->number extra-fields-length-bytes)
                     extra-fields-bytes (read-bytes is extra-fields-length "extra fields")
                     extra-fields (loop [extra-fields []
                                         ndx 0]
                                    (if (>= ndx extra-fields-length)
                                      extra-fields
                                      ;; subvec will throw if out of bounds, we'll use this as a crude validation
                                      (let [len (bytes->number (subvec (vec extra-fields-bytes) (+ ndx 2) (+ ndx 4)))]
                                        (recur (conj extra-fields
                                                     {:s1 (get extra-fields-bytes ndx)
                                                      :s2 (get extra-fields-bytes (inc ndx))
                                                                   ;; spec does specify field format, but I'll load it into a string
                                                      :subfield (String. (byte-array (subvec (vec extra-fields-bytes) (+ ndx 4) (+ ndx 4 len))))})
                                               (+ ndx 4 len 1)))))]
                 (assoc parsed :extra-field extra-fields))))
      :original-file-name
      (recur {:next :file-comment}
             (if-not (-> parsed :flags :fname)
               parsed
               (assoc parsed :original-file-name (read-zero-terminated-str is "name"))))
      :file-comment
      (recur {:next :hcrc}
             (if-not (-> parsed :flags :fcomment)
               parsed
               (assoc parsed :file-comment (read-zero-terminated-str is "comment"))))
      :hcrc
      (recur {:next :compressed-blocks}
             (if-not (-> parsed :flags :fhcrc)
               parsed
               (let [crc16-bytes (read-bytes is 2 "crc16")]
                 (assoc parsed :crc16 (bytes->number crc16-bytes)))))
      ;; I don't see a way to skip compressed-blocks other than going through deflating
      ;; (assuming multi-member gz files, for single member should just find trailer at eof)
      ;; TODO: Consider not loading these into mem, a big file will blow the heap.
      :compressed-blocks
      (recur {:next :trailer}
             (let [_ (.mark is Integer/MAX_VALUE)
                   inflater  (java.util.zip.Inflater. true) ;; mimic what gzip does
                   inf-is (java.util.zip.InflaterInputStream. is inflater)
                   uncompressed-bytes (loop [bs []
                                             b (.read inf-is)]
                                        (if (= -1 b)
                                          bs
                                          (recur
                                           (conj bs b)
                                           (.read inf-is))))]
               ;; inflater input stream reads more than it has to, reposition:
               (.reset is)
               (.skip is (.getBytesRead inflater))
               (.end inflater)
               (assoc parsed
                      #_#_:uncompressed-bytes uncompressed-bytes
                      :uncompressed-string (String. (byte-array uncompressed-bytes)))))
      :trailer
      (assoc parsed
             :crc32 (bytes->number (read-bytes is 4 "crc32"))
             :isize (bytes->number (read-bytes is 4 "isize"))))))

(defn gzip-info [f]
  (with-open [is (io/input-stream f)]
    (let [member (gzip-member is)]
      (loop [members [member]]
        (if (zero? (.available is))
          members
          (recur (conj members (gzip-member is))))))))

(defn -main [& args]
  (->> (first args)
       gzip-info
       (mapv #(into (sorted-map) %))
       pprint/pprint))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

(comment
  (gzip-info "out/a/b/c/boop.txt.gz")
  ;; => [{:raw-fixed-header
  ;;      {:id1 31, :id2 139, :cm 8, :flg 0, :mtime 0, :xfl 0, :os 255},
  ;;      :flags
  ;;      {:ftext false, :fhcrc false, :fextra false, :fname false, :fcomment false},
  ;;      :os :unknown,
  ;;      :xfl :default,
  ;;      :uncompressed-string "boop.txt",
  ;;      :crc32 704202681,
  ;;      :isize 8}]
  
  (gzip-info "foo.txt.gz")
  ;; => [{:name "foo.txt",
  ;;      :uncompressed-string "foo\n",
  ;;      :isize 4,
  ;;      :crc32 2117232040,
  ;;      :xfl :default,
  ;;      :raw-fixed-header
  ;;      {:id1 31, :id2 139, :cm 8, :flg 8, :mtime 1767731832, :xfl 0, :os 3},
  ;;      :os :unix,
  ;;      :flags
  ;;      {:ftext false, :fhcrc false, :fextra false, :fname true, :fcomment false},
  ;;      :mtime "2026-01-06T20:37:12Z"}]

  (gzip-info "silly.gz")
  ;; => [{:crc16 53822,
  ;;      :name "foo.bar",
  ;;      :uncompressed-string
  ;;      "This is a test of the emergency broadcast system.\nRemember, this is only a test.\n",
  ;;      :isize 81,
  ;;      :crc32 392113465,
  ;;      :comment "no comment",
  ;;      :xfl :default,
  ;;      :raw-fixed-header
  ;;      {:id1 31, :id2 139, :cm 8, :flg 31, :mtime 1767634725, :xfl 0, :os 3},
  ;;      :os :unix,
  ;;      :flags {:ftext true, :fhcrc true, :fextra true, :fname true, :fcomment true},
  ;;      :extra-fields [{:s1 120, :s2 49, :field "abcd"}],
  ;;      :mtime "2026-01-05T17:38:45Z"}]

  (gzip-info "foo.txt.gz")
  ;; => [{:name "foo.txt",
  ;;      :uncompressed-string "foo",
  ;;      :isize 3,
  ;;      :crc32 2356372769,
  ;;      :xfl :fast,
  ;;      :raw-fixed-header
  ;;      {:id1 31, :id2 139, :cm 8, :flg 8, :mtime 1767546088, :xfl 4, :os 3},
  ;;      :os :unix,
  ;;      :flags
  ;;      {:ftext false, :fhcrc false, :fextra false, :fname true, :fcomment false},
  ;;      :mtime "2026-01-04T17:01:28Z"}]

  (gzip-info "fiddle.clj.gz")
  ;; => [{:name "fiddle.clj",
  ;;      :uncompressed-string
  ;;      "(require '[babashka.fs :as fs])\n;; => nil\n\n(fs/relativize \"\" \"/assets/script.js\")\n;; => Execution error (IllegalArgumentException) at sun.nio.fs.UnixPath/relativize (UnixPath.java:485).\n;;    'other' is different type of Path\n\n\n(fs/relativize \".\" \"/assets/script.js\")\n;; => Execution error (IllegalArgumentException) at sun.nio.fs.UnixPath/relativize (UnixPath.java:485).\n;;    'other' is different type of Path\n\n(fs/same-file? \".\" \"\")\n;; => true\n(fs/same-file? \"./\" \"\")\n;; => true\n\n\n(fs/relativize \".\" \"/assets/script.js\")\n;; => Execution error (IllegalArgumentException) at sun.nio.fs.UnixPath/relativize (UnixPath.java:485).\n;;    'other' is different type of Path\n\n\n(fs/relativize \"/assets\" \"/assets/script.js\")\n;; => #object[sun.nio.fs.UnixPath 0x2fd1780a \"script.js\"]\n\n\n(fs/relativize \"assets\" \"assets/script.js\")\n;; => #object[sun.nio.fs.UnixPath 0x71a941a2 \"script.js\"]\n\n(fs/relativize \".\" \".\")\n;; => #object[sun.nio.fs.UnixPath 0x269179e5 \"\"]\n",
  ;;      :isize 952,
  ;;      :crc32 840078440,
  ;;      :xfl :default,
  ;;      :raw-fixed-header
  ;;      {:id1 31, :id2 139, :cm 8, :flg 8, :mtime 1767543912, :xfl 0, :os 3},
  ;;      :os :unix,
  ;;      :flags
  ;;      {:ftext false, :fhcrc false, :fextra false, :fname true, :fcomment false},
  ;;      :mtime "2026-01-04T16:25:12Z"}]

  :eoc)
