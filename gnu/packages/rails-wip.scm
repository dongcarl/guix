;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu packages rails-wip)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ragel)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web)
  #:use-module (guix build-system ruby)

  #:use-module (gnu packages ruby)
  #:use-module (gnu packages rails))

(define-public ruby-hoe-travis
(package
  (name "ruby-hoe-travis")
  (version "1.2")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "hoe-travis" version))
      (sha256
        (base32
          "1qg5sf9kpd0akj94glq1sj55ys5cxphgajxa0mllm40sb4dnzny2"))))
  (build-system ruby-build-system)
  (propagated-inputs
    `(("ruby-hoe" ,ruby-hoe)
      ("ruby-travis" ,ruby-travis)))
  (synopsis
    "hoe-travis is a Hoe plugin that allows your gem to gain maximum benefit from
http://travis-ci.org.  The plugin contains a <code>.travis.yml</code>
generator and a pre-defined rake task which runs the tests and ensures your
manifest file is correct.

With hoe-travis it is easy to add additional checks.  Custom checks can be
easily verified locally by simply running a rake task instead of committing
and pushing a change, waiting for travis to run your tests, then trying a new
commit if you didn't fix the problem.")
  (description
    "hoe-travis is a Hoe plugin that allows your gem to gain maximum benefit from
http://travis-ci.org.  The plugin contains a <code>.travis.yml</code>
generator and a pre-defined rake task which runs the tests and ensures your
manifest file is correct.

With hoe-travis it is easy to add additional checks.  Custom checks can be
easily verified locally by simply running a rake task instead of committing
and pushing a change, waiting for travis to run your tests, then trying a new
commit if you didn't fix the problem.")
  (home-page
    "https://github.com/drbrain/hoe-travis")
  (license #f)))







(define-public ruby-addressable
  (package
    (name "ruby-addressable")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "addressable" version))
       (sha256
        (base32
         "0mpn7sbjl477h56gmxsjqb89r5s3w7vx5af994ssgc3iamvgzgvs"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-rakefile
          (lambda _
            (substitute* "Gemfile"
              (("git: 'https://github.com/sporkmonger/rack-mount.git',") "")
              ((".*launchy.*") "")
              ((".*redcarpet.*") ""))
            #t))
         (add-before 'check 'delete-network-test
                     (lambda _
                       (delete-file "spec/addressable/net_http_compat_spec.rb")
                       #t)))))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("bundler" ,bundler)
       ("ruby-sporkmonger-rack-mount" ,ruby-sporkmonger-rack-mount)
       ("ruby-rspec-its", ruby-rspec-its)
       ("ruby-yard" ,ruby-yard)
       ("ruby-simplecov" ,ruby-simplecov)))
    (synopsis "Uniform resource identifier (URI) reimplementation")
    (description
     "Addressable is a replacement for the URI implementation that is part of
Ruby's standard library.  It more closely conforms to the relevant RFCs and
adds support for IRIs and URI templates.")
    (home-page
     "https://github.com/sporkmonger/addressable")
    (license license:asl2.0)))

(define-public ruby-sporkmonger-rack-mount
  ;; Testing the addressable gem requires a newer commit than that released, so
  ;; use an up to date version.
  (let ((revision "1")
        (commit "076aa2c47d9a4c081f1e9bcb56a826a9e72bd5c3"))
    (package
      (name "ruby-sporkmonger-rack-mount")
      (version (string-append "0.8.3." revision "." commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sporkmonger/rack-mount.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1scx273g3xd93424x9lxc4zyvcp2niknbw5mkz6wkivpf7xsyxdq"))))
      (build-system ruby-build-system)
      (arguments
       ;; Tests currently fail so disable them.
       ;; https://github.com/sporkmonger/rack-mount/pull/1
       `(#:tests? #f))
      (propagated-inputs `(("ruby-rack" ,ruby-rack)))
      (synopsis "Stackable dynamic tree based Rack router")
      (description
       "@code{Rack::Mount} supports Rack's @code{X-Cascade} convention to
continue trying routes if the response returns pass.  This allows multiple
routes to be nested or stacked on top of each other.")
      (home-page "https://github.com/sporkmonger/rack-mount")
      (license license:expat))))

(define-public ruby-sass-spec
  ;; There is no gem published so we package directly from the git repository.
  (let ((commit "ac3d4160c19543273a8a515b0ec00014e311f329"))
    (package
      (name "ruby-sass-spec")
      (version (string-append "0.8.3-1." (string-take commit 8)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sass/sass-spec.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0hmi1blvysf6388hmivn9idsn1s7hgi3yma3vzwdcaqjyhjbhsca"))))
      (build-system ruby-build-system)
      (arguments
       `(#:tests? #t)) ; There are no tests, but this is to stop people using
                       ; this gem as it does not install correctly.
      (synopsis "")
      (description
       "")
      (home-page "")
      (license license:expat)))) ;?

(define-public ruby-rspec-its
  (package
    (name "ruby-rspec-its")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       ;; Use GitHub as a source because otherwise we cannot patch directly
       (uri (string-append "https://github.com/rspec/rspec-its/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "088f3y7vd3c8g2gpi0z76qabaabfg0fb70ly3d0xqzbani0lrf09"))
       (patches (search-patches "ruby-rspec-its-remove-rspec-gemspec.patch"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-gemfile
                     (lambda _
                       (substitute* "rspec-its.gemspec"
                         (("rake.*") "rake'\n")
                         (("cucumber.*") "cucumber'\n"))
                       #t)))))
    (propagated-inputs
     `(("ruby-rspec-core" ,ruby-rspec-core)
       ("ruby-rspec-expectations" ,ruby-rspec-expectations)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-cucumber" ,ruby-cucumber)
       ("ruby-aruba" ,ruby-aruba)))
    (synopsis "RSpec extension that provides the @code{its} method")
    (description
     "RSpec::Its provides the its method as a short-hand to specify the expected
value of an attribute.  For example, one can use @code{its(:size)\\{should
eq(1)\\}}.")
    (home-page "https://github.com/rspec/rspec-its")
    (license license:expat)))

(define-public ruby-cucumber
  (package
    (name "ruby-cucumber")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "cucumber" version))
       (sha256
        (base32
         "1k4j31a93r0zhvyq2mm2k8irppbvkzbsg44r3mf023959v18fzih"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"
                     #:tests? #f
                     )) ; Test failure as documented at https://github.com/cucumber/cucumber/issues/58
    (propagated-inputs
     `(("ruby-builder" ,ruby-builder)
      ("ruby-cucumber-core" ,ruby-cucumber-core)
      ("ruby-cucumber-wire" ,ruby-cucumber-wire)
      ("ruby-diff-lcs" ,ruby-diff-lcs)
      ("ruby-gherkin" ,ruby-gherkin)
      ("ruby-multi-json" ,ruby-multi-json)
      ("ruby-multi-test" ,ruby-multi-test)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-aruba", ruby-aruba*) ; use untested aruba version to avoid
                                   ; dependency cycle
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-pry" ,ruby-pry)
       ("ruby-nokogiri" ,ruby-nokogiri)))
    (synopsis
     "Behaviour Driven Development with elegance and joy")
    (description
     "Behaviour Driven Development with elegance and joy")
    (home-page "http://cukes.info")
    (license license:expat)))

(define ruby-cucumber*
  (package (inherit ruby-cucumber)
    (arguments
     `(#:tests? #f))
    (native-inputs
     `())))

(define-public ruby-cucumber-wire
  (package
    (name "ruby-cucumber-wire")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "cucumber-wire" version))
       (sha256
        (base32
         "09ymvqb0sbw2if1nxg8rcj33sf0va88ancq5nmp8g01dfwzwma2f"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; Do not test to avoid dependency cycle with
                     ; ruby-cucumber.
    (synopsis "Wire protocol for Cucumber")
    (description "Wire protocol for Cucumber")
    (home-page "http://cucumber.io")
    (license license:expat)))

(define-public ruby-multi-test
  (package
    (name "ruby-multi-test")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "multi_test" version))
       (sha256
        (base32
         "1sx356q81plr67hg16jfwz9hcqvnk03bd9n75pmdw8pfxjfy1yxd"))))
    (build-system ruby-build-system)
    (arguments
     ;; Tests require different sets of specific gem versions to be available,
     ;; and there is no gemfile that specifies the newest versions of
     ;; dependencies to be tested.
     `(#:tests? #f))
    (synopsis
     "Wafter-thin gem to help control rogue test/unit/autorun requires")
    (description
     "Wafter-thin gem to help control rogue test/unit/autorun requires")
    (home-page "http://cukes.info")
    (license license:expat)))

(define-public ruby-multi-json
  (package
    (name "ruby-multi-json")
    (version "1.11.2")
    (source
     (origin
       (method url-fetch)
       ;; Tests are not distributed at rubygems.org so download from GitHub
       ;; instead.
       (uri (string-append "https://github.com/intridea/multi_json/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "04vkfz88vj6ab3097n0xgm0335j753ik03zkq44grq6m36m94vk5"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-signing-key-reference
          (lambda _
            (substitute* "multi_json.gemspec"
              ((".*spec.signing_key.*") ""))
            #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-yard" ,ruby-yard)
       ("ruby-json-pure" ,ruby-json-pure)
       ("ruby-oj" ,ruby-oj)
       ("ruby-yajl-ruby" ,ruby-yajl-ruby)))
    (synopsis
     "A common interface to multiple JSON libraries, including Oj, Yajl, the JSON gem (with C-extensions), the pure-Ruby JSON gem, NSJSONSerialization, gson.rb, JrJackson, and OkJson.")
    (description
     "A common interface to multiple JSON libraries, including Oj, Yajl, the JSON gem (with C-extensions), the pure-Ruby JSON gem, NSJSONSerialization, gson.rb, JrJackson, and OkJson.")
    (home-page
     "http://github.com/intridea/multi_json")
    (license license:expat)))

(define-public ruby-oj
  (package
    (name "ruby-oj")
    (version "2.14.3")
    (source
     (origin
       (method url-fetch)
       ;; Version on rubygems.org does not contain Rakefile, so download from
       ;; GitHub instead.
       (uri (string-append "https://github.com/ohler55/oj/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0hyyrf21b9dhsq01gdv6wq7zhq9lzb3z9l52jv914qzmrhnbwx4f"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'compile
           (lambda _
             (zero? (system* "rake" "compile")))))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rake-compiler" ,ruby-rake-compiler)))
    (synopsis
     "The fastest JSON parser and object serializer. ")
    (description
     "The fastest JSON parser and object serializer. ")
    (home-page "http://www.ohler.com/oj")
    (license license:expat)))

(define-public ruby-yajl-ruby
  (package
    (name "ruby-yajl-ruby")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "yajl-ruby" version))
       (sha256
        (base32
         "0zvvb7i1bl98k3zkdrnx9vasq0rp2cyy5n7p9804dqs4fz9xh9vf"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         ;; Remove broken test reported at
         ;; https://github.com/brianmario/yajl-ruby/issues/157
         (add-before 'check 'remove-failing-tests
           (lambda _
             (substitute* "spec/parsing/large_number_spec.rb"
               ((".*should eq\\(\\['', 0\\]\\).*") ""))
             #t)))))
     (native-inputs
      `(("ruby-rake-compiler" ,ruby-rake-compiler)
        ("ruby-rspec" ,ruby-rspec-2)))
     (synopsis
      "Ruby C bindings to the excellent Yajl JSON stream-based parser library.")
     (description
      "Ruby C bindings to the excellent Yajl JSON stream-based parser library.")
     (home-page
      "http://github.com/brianmario/yajl-ruby")
     (license license:expat)))

(define-public ruby-bzip2-ruby
  ;; Use git reference because gem is out of date.
  (let ((revision "1")
        (commit "e58f154624ff2d770e92a70e0d8fb1a9e6564720"))
    (package
      (name "ruby-bzip2-ruby")
      (version (string-append "0.2.7." revision "." commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/brianmario/bzip2-ruby.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "072jzd1fm6z0fnc166q7n4h8h4vrb9jhpcl7swh7d0qm4fx4cxvz"))))
      (build-system ruby-build-system)
      (arguments
       '(#:test-target "spec"
        ; #:gem-flags
        ; (list "--"
        ;       (string-append "--with-bz2-dir="
        ;                      (assoc-ref %build-inputs "bzip2")
        ;                      "/include/" ))
        #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-gemspec-and-source
             (lambda _
               (substitute* "bzip2-ruby.gemspec"
                 (("s.files = `git ls-files`") "s.files = `find *`")
                 (("s.test_files = `git ls-files spec`")
                  "s.files = `find spec`"))
               ;; Use part of the patch proposed at
               ;; https://github.com/brianmario/bzip2-ruby/pull/26
               (substitute* "ext/bzip2/writer.c"
                 (("RBASIC\\(res\\)->klass = rb_cString;")
                  "RBASIC_SET_CLASS(res, rb_cString);"))
               #t)))))
      (native-inputs
       `(("bundler" ,bundler)
         ("ruby-rspec" ,ruby-rspec-2)
         ("ruby-rake-compiler" ,ruby-rake-compiler)))
      (synopsis "Ruby C bindings to libbzip2.")
      (description "Ruby C bindings to libbzip2.")
      (home-page
       "http://github.com/brianmario/bzip2-ruby")
      (license #f))))

(define-public ruby-aruba
  (package
    (name "ruby-aruba")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "aruba" version))
       (sha256
        (base32
         "0cvxvw0v7wnhz15piylxrwpjdgjccwyrddda052z97cpnj5qjg5w"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f ; There are 3 test failures to do with running commands, not
                   ; sure what the issue is.
       #:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-gemfile
           (lambda _
             (substitute* "Gemfile"
               ((".*byebug.*") "\n")
               ((".*pry.*") "\n")
               ((".*yaml.*") "\n")
               ((".*bcat.*") "\n")
               ((".*kramdown.*") "\n")
               ((".*fuubar.*") "\n")
               ((".*rubocop.*") "\n")
               ((".*cucumber-pro.*") "\n")
               ((".*license_finder.*") "\n")
               ((".*relish.*") "\n")
               )
             (substitute* "spec/spec_helper.rb"
               ((".*simplecov.*") "")
               (("^SimpleCov.*") ""))
             #t))
         (add-before 'check 'set-home
                     (lambda _ (setenv "HOME" "/tmp") #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-fuubar" ,ruby-fuubar)))
    (propagated-inputs
     `(("ruby-childprocess" ,ruby-childprocess)
       ("ruby-contracts" ,ruby-contracts)
       ("ruby-cucumber" ,ruby-cucumber)
       ("ruby-ffi" ,ruby-ffi)
       ("ruby-rspec-expectations" ,ruby-rspec-expectations)
       ("ruby-thor" ,ruby-thor)))
    (synopsis
     "Extension for popular TDD and BDD frameworks like \"Cucumber\", \"RSpec\" and \"Minitest\" to make testing commandline applications meaningful, easy and fun.")
    (description
     "Extension for popular TDD and BDD frameworks like \"Cucumber\", \"RSpec\" and \"Minitest\" to make testing commandline applications meaningful, easy and fun.")
    (home-page "http://github.com/cucumber/aruba")
    (license license:expat)))

;; A version of ruby-aruba without tests run so that circular dependencies can
;; be avoided.
(define ruby-aruba*
  (package (inherit ruby-aruba)
    (arguments
     `(#:tests? #f))
    (propagated-inputs ; TODO: use alist-replace rather than repeating these.
     `(("ruby-childprocess" ,ruby-childprocess)
       ("ruby-contracts" ,ruby-contracts)
       ("ruby-cucumber" ,ruby-cucumber*) ; use untested cucumber to avoid
                                        ; dependency cycle
       ("ruby-event-bus" ,ruby-event-bus)
       ("ruby-ffi" ,ruby-ffi)
       ("ruby-rspec-expectations" ,ruby-rspec-expectations)
       ("ruby-thor" ,ruby-thor)))
    (native-inputs
     `())))

(define-public ruby-fuubar
  (package
    (name "ruby-fuubar")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       ;; The gem does not include files required for testing.
       (uri (string-append "https://github.com/thekompanee/fuubar/archive/releases/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1363fy7x12srr51cy5x1xkf9z7jlzzlwpsifwgx6bivnp6lar3ny"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f ; some tests fail when running (zero? (system* "rspec"
                   ; "-Ilib" "spec")) inside the container, but seem to work outside.
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'delete-certificate
           (lambda _
             ;; Remove 's.cert_chain' as we do not build with a private key
             (substitute* "fuubar.gemspec"
               ((".*cert_chain.*") "")
               ((".*signing_key.*") ""))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)))
    (propagated-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("ruby-ruby-progressbar" ,ruby-ruby-progressbar)))
    (synopsis
     "the instafailing RSpec progress bar formatter")
    (description
     "the instafailing RSpec progress bar formatter")
    (home-page
     "https://github.com/thekompanee/fuubar")
    (license license:expat)))

;; Seems to work except for 2 rspec errors already fixed upstream
;; https://github.com/egonSchiele/contracts.ruby/commit/c1f22bfc6b28125b55d42a33ca3e05f15e82d6f2.diff
;; might need to add that as a patch when adding to guix
(define-public ruby-contracts
  (package
    (name "ruby-contracts")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "contracts" version))
       (sha256
        (base32
         "0xszv56p58q7da8agc4dsnw8x46gnh6ahbag5gdmvbxjgml03mdl"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f ; enable these when adding TO GUIX!!!!
       #:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-rakefile
          (lambda _
            (substitute* "Rakefile"
              ((".*rubocop.*") "")
              ((".*RuboCop.*") ""))
            #t)))))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)))
    (synopsis
     "This library provides contracts for Ruby. Contracts let you clearly express how your code behaves, and free you from writing tons of boilerplate, defensive code.")
    (description
     "This library provides contracts for Ruby. Contracts let you clearly express how your code behaves, and free you from writing tons of boilerplate, defensive code.")
    (home-page
     "http://github.com/egonSchiele/contracts.ruby")
    (license #f)))

(define-public ruby-event-bus
  (package
    (name "ruby-event-bus")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "event-bus" version))
       (sha256
        (base32
         "0bqcznr15q1346avpddnyd9144hqdww86yfpb4jayaj6lm0fqwyq"))))
    (build-system ruby-build-system)
    (arguments
     ;; disable testing to break the cycle with aruba.  Instead simply test that
     ;; the library can be require'd.
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "ruby" "-Ilib" "-r" "event/bus")))))))
    (synopsis
     "This gem notifies subscribers about event")
    (description
     "This gem notifies subscribers about event")
    (home-page
     "https://github.com/cucumber/event-bus")
    (license license:expat)))

(define-public ruby-childprocess
  (package
    (name "ruby-childprocess")
    (version "0.5.8")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "childprocess" version))
       (sha256
        (base32
         "1lv7axi1fhascm9njxh3lx1rbrnsm8wgvib0g7j26v4h1fcphqg0"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f
;       #:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'remove-dependency-and-patch-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "spec/spec_helper.rb"
               ;; cannot require coveralls otherwise there is a dependency cycle
               ;; childprocess, coveralls, rest-client, webmock, addressable,
               ;; rspec-its, aruba, childprocess.
               ((".*coveralls.*") "")
               ((".*Coveralls.*") "")
               ;; patch path to /bin/sh
               (("/bin/sh") (which "sh"))
               ;; testing
               (("; sleep") "; puts \"before_sleep\"; sleep; puts \"after_sleep\"")
               )
             (substitute* "lib/childprocess/unix/process.rb"
               (("send_signal 'TERM'") "puts 'terming'; send_signal 'TERM'")
               (("send_signal 'KILL'") "puts 'killing'; send_signal 'KILL'")
               (("return true if @exit_code")
                "puts \"exit_code: #{@exit_code}\"; p self; return true if @exit_code"))
             ;; sleep POLL_INTERVAL
             (substitute* "lib/childprocess/abstract_process.rb"
               (("sleep POLL_INTERVAL")
                "puts 'sleeping'; sleep POLL_INTERVAL; puts 'awake'")
               (("unless ok") "puts \"ok was #{ok} and exited? was #{exited?}\"; unless ok"))
             #t))
         ;(replace 'check
         ;  ;; for testing for ben only
         ;  (lambda _
         ;    (zero? (system* "rspec" "spec/childprocess_spec.rb" "-e" "kills
         ;    the full process tree"))))
         )))
    (propagated-inputs
     `(("ruby-ffi" ,ruby-ffi)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis
     "This gem aims at being a simple and reliable solution for controlling external programs running in the background on any Ruby / OS combination.")
    (description
     "This gem aims at being a simple and reliable solution for controlling external programs running in the background on any Ruby / OS combination.")
    (home-page
     "http://github.com/jarib/childprocess")
    (license license:expat)))

(define-public ruby-sinatra
  (package
  (name "ruby-sinatra")
  (version "1.4.7")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "sinatra" version))
      (sha256
        (base32
          "1b81kbr65mmcl9cdq2r6yc16wklyp798rxkgmm5pr9fvsj7jwmxp"))))
  (build-system ruby-build-system)
  (propagated-inputs
   ;; The tests do not pass with rack >= 2, so we require all dependencies to
   ;; use rack 1 too.
    `(("ruby-rack" ,ruby-rack-1)
      ("ruby-rack-protection" ,ruby-rack-protection/rack-1)
      ("ruby-tilt" ,ruby-tilt)))
  (native-inputs
   `(("ruby-rack-test" ,ruby-rack-test/rack-1)))
  (synopsis
    "Sinatra is a DSL for quickly creating web applications in Ruby with minimal effort.")
  (description
    "Sinatra is a DSL for quickly creating web applications in Ruby with minimal effort.")
  (home-page "http://www.sinatrarb.com/")
  (license license:expat)))

(define-public ruby-rack-1
  (package
    (inherit ruby-rack)
    (version "1.6.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rack" version))
              (sha256
               (base32
                "09bs295yq6csjnkzj7ncj50i6chfxrhmzg1pk6p0vd2lb9ac8pj5"))))
    (native-inputs
     `(("ruby-bacon" ,ruby-bacon)))))

;; A version that propagates rack 1.x rather than the newest (currently 2.x).
(define-public ruby-rack-protection/rack-1
  (package
    (inherit ruby-rack-protection)
    (propagated-inputs
     `(("ruby-rack" ,ruby-rack-1)))))

;; A version that propagates rack 1.x rather than the newest (currently 2.x).
(define-public ruby-rack-test/rack-1
  (package
    (inherit ruby-rack-test)
    (propagated-inputs
     `(("ruby-rack" ,ruby-rack-1)))))

(define-public ruby-tilt
  (package
    (name "ruby-tilt")
    (version "2.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "tilt" version))
       (sha256
        (base32
         "0lgk8bfx24959yq1cn55php3321wddw947mgj07bxfnwyipy9hqf"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; There are a number of unpackaged dependencies e.g. kramdown.
    ;; (native-inputs
    ;;  `(("bundler" ,bundler)
    ;;    ("ruby-yard" ,ruby-yard)
    ;;    ("ruby-asciidoctor" ,ruby-asciidoctor)
    ;;    ("ruby-builder" ,ruby-builder)
    ;;    ("ruby-coffee-script" ,ruby-coffee-script)
    ;;    ("ruby-contest" ,ruby-contest)
    ;;    ("ruby-creole" ,ruby-creole)
    ;;    ("ruby-erubis" ,ruby-erubis)
    ;;    ("ruby-haml" ,ruby-haml); do we really need haml <4 ?
    ;;    ("ruby-kramdown" ,ruby-kramdown)))
    (synopsis
     "Generic interface to multiple Ruby template engines")
    (description
     "Generic interface to multiple Ruby template engines")
    (home-page "http://github.com/rtomayko/tilt/")
    (license license:expat)))

(define-public ruby-radius
  (package
    (name "ruby-radius")
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "radius" version))
       (sha256
        (base32
         "0n0clzgvxpjm2gjlpz98x6gkw5hb84bmd435a1yaqs3m0k896v5s"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-simplecov" ,ruby-simplecov)
       ("ruby-coveralls" ,ruby-coveralls)))
    (synopsis
     "Radius is a powerful tag-based template language for Ruby inspired by the template languages used in MovableType and TextPattern. It uses tags similar to XML, but can be used to generate any form of plain text (HTML, e-mail, etc...).")
    (description
     "Radius is a powerful tag-based template language for Ruby inspired by the template languages used in MovableType and TextPattern. It uses tags similar to XML, but can be used to generate any form of plain text (HTML, e-mail, etc...).")
    (home-page "http://github.com/jlong/radius")
    (license #f)))

(define-public ruby-coveralls
  (package
    (name "ruby-coveralls")
    (version "0.8.10")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "coveralls" version))
       (sha256
        (base32
         "1y6gzahhaymgcxcgm7y16sgbiafsb7i2flhy1sq4x1jizk8bih5s"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
                     #:phases
       (modify-phases %standard-phases
         (add-after 'extract-gemspec 'update-dependency
                    (lambda _
                      ;; Relax dependency to avoid conflicting versions in nio4r
                      ;; for instance.
                      (substitute* "coveralls-ruby.gemspec"
                        (("<simplecov.*") "<simplecov>)\n")
                        (("<tins.*") "<tins>)\n"))
                        
             #t))
         (add-before 'check 'fix-dependencies
                     (lambda _
                       (substitute* "spec/spec_helper.rb"
                         ((".*pry.*") "\n"))
                       #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-truthy" ,ruby-truthy)
       ("ruby-webmock" ,ruby-webmock)
       ("ruby-vcr" ,ruby-vcr)
       ("git" ,git))) ; git is required for testing
    (propagated-inputs
     `(("ruby-json" ,ruby-json)
       ("ruby-rest-client" ,ruby-rest-client)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-term-ansicolor" ,ruby-term-ansicolor)
       ("ruby-thor" ,ruby-thor)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-tins" ,ruby-tins)))
    (synopsis
     "A Ruby implementation of the Coveralls API.")
    (description
     "A Ruby implementation of the Coveralls API.")
    (home-page "https://coveralls.io")
    (license license:expat)))

(define-public ruby-truthy
(package
  (name "ruby-truthy")
  (version "1.0.0")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "truthy" version))
      (sha256
        (base32
          "19silgd65j3qwfk5w891p9wcmzdmi9ddm2kg5zbvvqn2h9lkfzmd"))))
  (build-system ruby-build-system)
  (arguments
     `(#:tests? #f)) ; Files required for testing are not in gem, and homepage
                     ; has disappeared.
  (propagated-inputs `(("ruby-hoe" ,ruby-hoe)))
  (synopsis
    "Easily get truthiness values of Ruby objects")
  (description
    "Easily get truthiness values of Ruby objects")
  (home-page "https://rubygems.org/gems/truthy")
  (license #f)))

(define-public ruby-rest-client
  (package
    (name "ruby-rest-client")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rest-client" version))
       (sha256
        (base32
         "1m8z0c4yf6w47iqz6j2p7x1ip4qnnzvhdph9d5fgx081cvjly3p7"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-dependencies
           (lambda _
             (substitute* "rest-client.gemspec"
               ((".*pry.*") "\n"))
             #t))
         (add-before 'check 'delete-network-tests
                     (lambda _
                       (delete-file "spec/integration/request_spec.rb")
                       #t)))))
    (propagated-inputs
     `(("ruby-http-cookie" ,ruby-http-cookie)
       ("ruby-mime-types" ,ruby-mime-types-2)
       ("ruby-netrc" ,ruby-netrc)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-webmock", ruby-webmock)
       ("ruby-rspec", ruby-rspec-2)))
    (synopsis
     "A simple HTTP and REST client for Ruby, inspired by the Sinatra microframework style of specifying actions: get, put, post, delete.")
    (description
     "A simple HTTP and REST client for Ruby, inspired by the Sinatra microframework style of specifying actions: get, put, post, delete.")
    (home-page
     "https://github.com/rest-client/rest-client")
    (license license:expat)))

(define-public ruby-webmock
  (package
    (name "ruby-webmock")
    (version "1.22.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "webmock" version))
       (sha256
        (base32
         "0la47vzbikhvnx8qcj8jli87agzzffwh11ggm7rpq43iz2rwp0sl"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("bundler" ,bundler)
       ("ruby-addressable" ,ruby-addressable)
       ("ruby-crack" ,ruby-crack)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-hashdiff" ,ruby-hashdiff)))
    (synopsis
     "WebMock allows stubbing HTTP requests and setting expectations on HTTP requests.")
    (description
     "WebMock allows stubbing HTTP requests and setting expectations on HTTP requests.")
    (home-page "http://github.com/bblimke/webmock")
    (license license:expat)))

(define-public ruby-crack
  (package
    (name "ruby-crack")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "crack" version))
       (sha256
        (base32
         "0abb0fvgw00akyik1zxnq7yv391va148151qxdghnzngv66bl62k"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
                  (lambda _
                    (zero? (length
                            (filter (lambda (file)
                                      (display file)(display "\n")
                                      (not (zero? (system* "ruby" file))))
                                    (find-files "spec" ".*rb$")))))))))
    (propagated-inputs
     `(("ruby-safe-yaml" ,ruby-safe-yaml)))
    (synopsis
     "Really simple JSON and XML parsing, ripped from Merb and Rails.")
    (description
     "Really simple JSON and XML parsing, ripped from Merb and Rails.")
    (home-page "http://github.com/jnunemaker/crack")
    (license license:expat)))

(define-public ruby-safe-yaml
  (package
    (name "ruby-safe-yaml")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "safe_yaml" version))
       (sha256
        (base32
         "1hly915584hyi9q9vgd968x2nsi5yag9jyf5kq60lwzi5scr7094"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("ruby-hashie" ,ruby-hashie)
       ("ruby-heredoc-unindent" ,ruby-heredoc-unindent)))
    (synopsis "Parse YAML safely")
    (description "Parse YAML safely")
    (home-page "https://github.com/dtao/safe_yaml")
    (license license:expat)))

(define-public ruby-hashie
  (package
    (name "ruby-hashie")
    (version "3.4.4")
    (source
     (origin
       (method url-fetch)
       ;; The gem does not include Gemfile.
       (uri (string-append "https://github.com/intridea/hashie/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "19lyb9vwjpag7w67jhg4yfrkfn2p1glyv444z04j8d4wawxhrk9x"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-dependencies
           (lambda _
             (substitute* "Gemfile"
               ((".*pry.*") "")
               ((".*guard.*") "")
               ((".*codeclimate.*") "")
               (("rubocop.*") "rubocop'\n")
               (("rspec-core.*") "rspec-core'\n"))
             (substitute* "spec/spec_helper.rb"
               ((".*pry.*") ""))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rspec-pending-for" ,ruby-rspec-pending-for)
       ;("ruby-guard" ,ruby-guard)
       ;("ruby-guard-rspec" ,ruby-guard-rspec)
       ("ruby-activesupport" ,ruby-activesupport)
       ("ruby-rubocop" ,ruby-rubocop)))
    (synopsis
     "Hashie is a collection of classes and mixins that make hashes more powerful.")
    (description
     "Hashie is a collection of classes and mixins that make hashes more powerful.")
    (home-page "https://github.com/intridea/hashie")
    (license license:expat)))

(define-public ruby-rspec-pending-for
  (package
    (name "ruby-rspec-pending-for")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rspec-pending_for" version))
       (sha256
        (base32
         "0f9sj7v3j14fvd631smxr04l53pk8dqwn9ybqkjdqmzvcv73b5n6"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"))
    (native-inputs
     `(("bundler" ,bundler)))
    (propagated-inputs
     `(("ruby-rspec-core" ,ruby-rspec-core)
       ("ruby-ruby-engine" ,ruby-ruby-engine)
       ("ruby_version" ,ruby_version)))
    (synopsis
     "Mark specs pending or skipped for specific Ruby engine (e.g. MRI or JRuby) / version combinations")
    (description
     "Mark specs pending or skipped for specific Ruby engine (e.g. MRI or JRuby) / version combinations")
    (home-page
     "https://github.com/pboling/rspec-pending_for")
    (license #f)))

(define-public ruby_version ; There is another gem called 'ruby-version' so we
                                        ; use an underscore in this name
  (package
    (name "ruby_version")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ruby_version" version))
       (sha256
        (base32
         "0854i1bjy56176anr05l5m0vc81nl53c7fyfg7sljj62m1d64dgj"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-dependencies
                     (lambda _
                       (delete-file "Gemfile.lock")
                       (delete-file "pkg/ruby_version-1.0.0.gem")
                       (substitute* "ruby_version.gemspec"
                         ((".*rdoc.*") "\n")
                         (("rake.*") "rake>)\n")
                         ((".*rubygems-tasks.*") "\n"))
                       (substitute* "Rakefile"
                         (("^require 'rubygems/tasks'") "")
                         (("Gem::Tasks.new") ""))
                       #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec-2)
       ("ruby-rake" ,ruby-rake)))
    (synopsis
     "Provides a RubyVersion class to simplify checking for the right Ruby version in your programs.")
    (description
     "Provides a RubyVersion class to simplify checking for the right Ruby version in your programs.")
    (home-page
     "https://github.com/janlelis/ruby_version")
    (license license:expat)))

(define-public ruby-rake
  (package
    (name "ruby-rake")
    (version "11.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rake" version))
       (sha256
        (base32
         "0m7fk7n0q459b1866cpq0gyz6904srhajrag0ybpdyl5sw4c2xff"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)))
    (synopsis
     "Rake is a Make-like program implemented in Ruby. Tasks and dependencies are
specified in standard Ruby syntax.")
    (description
     "Rake is a Make-like program implemented in Ruby. Tasks and dependencies are
specified in standard Ruby syntax.")
    (home-page "https://github.com/ruby/rake")
    (license license:expat)))









(define-public ruby-appraisal
  (package
    (name "ruby-appraisal")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "appraisal" version))
       (sha256
        (base32
         "10ng010lhswdykjhwic7bgv28qpjj42qwxvprpj1f4cavdimh4vp"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f ; tests require network access
       #:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         ;; remove bundler from Rakefile to avoid dependency issues
         (add-before 'check 'remove-dependency-checking
           (lambda _
;             (substitute* "Rakefile"
 ;              (("^require 'bundler.*") ""))
             (substitute* "Gemfile"
               (("thor.*") "thor'\n"))
             #t)))))
    (propagated-inputs
     `(("bundler" ,bundler)
       ("ruby-thor" ,ruby-thor)))
    (native-inputs
     `(("ruby-activesupport" ,ruby-activesupport)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis
     "Appraisal integrates with bundler and rake to test your library against different versions of dependencies in repeatable scenarios called \"appraisals.\"")
    (description
     "Appraisal integrates with bundler and rake to test your library against different versions of dependencies in repeatable scenarios called \"appraisals.\"")
    (home-page
     "http://github.com/thoughtbot/appraisal")
    (license license:expat)))

(define-public ruby-kramdown
  (package
    (name "ruby-kramdown")
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "kramdown" version))
       (sha256
        (base32
         "12sral2xli39mnr4b9m2sxdlgam4ni0a1mkxawc5311z107zj3p0"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-prawn" ,ruby-prawn)))
    (synopsis
     "kramdown is yet-another-markdown-parser but fast, pure Ruby,
using a strict syntax definition and supporting several common extensions.
")
    (description
     "kramdown is yet-another-markdown-parser but fast, pure Ruby,
using a strict syntax definition and supporting several common extensions.
")
    (home-page "http://kramdown.gettalong.org")
    (license license:expat)))

(define-public ruby-prawn
  (package
    (name "ruby-prawn")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "prawn" version))
       (sha256
        (base32
         "0z9q3l8l73pvx6rrqz40xz9xd5izziprjnimb572hcah6dh30cnw"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; Further dependencies such as pdf-inspector need to be
                     ; packaged.
    (propagated-inputs
     `(("ruby-pdf-core" ,ruby-pdf-core)
       ("ruby-ttfunk" ,ruby-ttfunk)))
    (synopsis
     "  Prawn is a fast, tiny, and nimble PDF generator for Ruby
")
    (description
     "  Prawn is a fast, tiny, and nimble PDF generator for Ruby
")
    (home-page
     "http://prawn.majesticseacreature.com")
    ;; Prawn is released under a slightly modified form of the License of Ruby,
    ;; allowing you to choose between Matz's terms, the GPLv2, or GPLv3.
    (license (list license:gpl2 license:gpl3
                   (license:non-copyleft "file://LICENSE"
                                         "See LICENSE in the distribution")))))

(define-public ruby-pdf-core
  (package
    (name "ruby-pdf-core")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "pdf-core" version))
       (sha256
        (base32
         "1ks95byqs08vxgf2a7q3spryi467rwimm2awc84fqa2yxf97ikjy"))))
    (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; Further dependencies need to be packaged.
    ;; (native-inputs
    ;;  `(("bundler" ,bundler)
    ;;    ("pdf-reader" ,pdf-reader)
    ;;    ("pdf-inspector" ,pdf-inspector)))
    (synopsis
     "PDF::Core is used by Prawn to render PDF documents")
    (description
     "PDF::Core is used by Prawn to render PDF documents")
    (home-page
     "http://prawn.majesticseacreature.com")
    (license #f)))

(define-public ruby-pdf-reader
  (package
    (name "ruby-pdf-reader")
    (version "1.3.3")
    (source
     (origin
       (method url-fetch)
       ;; fetch from github as rubygem does not contain tests
       (uri (string-append "https://github.com/yob/pdf-reader/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1bvrjr93nhfhm652s6fc0i5kvs44y1bqa6dq5dyms38nfqnsym5b"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; remove gems that are not actually required for running tests
         (add-before 'check 'remove-dependency-checking
           (lambda _
             (substitute* "pdf-reader.gemspec"
               ((".*spec.add_development_dependency.*ZenTest.*") "")
;;               ((".*spec.add_development_dependency.*cane.*") "")
               ((".*spec.add_development_dependency.*morecane.*") "")
               ((".*spec.add_development_dependency.*ir_b.*") ""))
             #t)))))
    (propagated-inputs
     `(("ruby-afm" ,ruby-afm)
       ("ruby-ascii85" ,ruby-ascii85)
       ("ruby-hashery" ,ruby-hashery)
       ("ruby-rc4" ,ruby-rc4)
       ("ruby-ttfunk" ,ruby-ttfunk)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec-2)))
    (synopsis
     "The PDF::Reader library implements a PDF parser conforming as much as possible to the PDF specification from Adobe")
    (description
     "The PDF::Reader library implements a PDF parser conforming as much as possible to the PDF specification from Adobe")
    (home-page "http://github.com/yob/pdf-reader")
    (license #f)))

(define-public ruby-cane
  (package
    (name "ruby-cane")
    (version "2.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "cane" version))
       (sha256
        (base32
         "176x9g8ax9xky4303sf0xzf6ya62mdrm4bfrdj0hhy45x43v03x1"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-parallel" ,ruby-parallel)
       ("bundler" ,bundler)))
    (synopsis
     "Fails your build if code quality thresholds are not met")
    (description
     "Fails your build if code quality thresholds are not met")
    (home-page "http://github.com/square/cane")
    (license license:asl2.0)))

(define-public ruby-parallel
  (package
    (name "ruby-parallel")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       ;; fetch from github as rubygem does not contain tests
       (uri (string-append "https://github.com/grosser/parallel/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0py2zv58ar26v2inxbwq83p9ay62q74gh28iw6mvcfbyiiqby8im"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "rspec-rerun:spec"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-gemspec
           (lambda _
             (substitute* "parallel.gemspec"
               (("git ls-files") "find"))
             #t)))))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("bundler" ,bundler)
       ("ruby-i18n" ,ruby-i18n)))
    (synopsis "Run Ruby code in parallel processes")
    (description
     "Run any kind of code in parallel processes")
    (home-page "https://github.com/grosser/parallel")
    (license license:expat)))










(define-public ruby-coffee-script
  (package
    (name "ruby-coffee-script")
    (version "2.4.1")
    (source
     (origin
       (method url-fetch)
       ;; fetch from github as the gem does not contain testing code
       (uri (string-append
             "https://github.com/rails/ruby-coffee-script/archive/v"
             version ".tar.gz"))
       (sha256
        (base32
         "0gbcg40ks4ifm332ljmgq2l44ssld0z6xhjzk48v6mpaxyz8mc92"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-test
           (lambda _
             ;; patch submitted upstream at
             ;; https://github.com/rails/ruby-coffee-script/pull/6
             (substitute* "test/test_coffee_script.rb"
               ((" unexpected unless\\\"")
                " unexpected unless\", '[stdin]:3:11: unexpected unless'"))
             #t)))))
    (propagated-inputs
     `(("ruby-coffee-script-source" ,ruby-coffee-script-source)
       ("ruby-execjs" ,ruby-execjs)
       ("ruby-duktape", ruby-duktape))) ; use as the JS interpreter
    (synopsis "bridge to the javascript CoffeeScript compiler.")
    (description
     "Ruby CoffeeScript is a bridge to the javascript CoffeeScript compiler.
CoffeeScript is an attempt to expose the good parts of JavaScript in a simple
way.")
    (home-page
     "http://github.com/rails/ruby-coffee-script")
    (license license:expat)))

(define-public ruby-coffee-script-source
  (package
  (name "ruby-coffee-script-source")
  (version "1.10.0")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "coffee-script-source" version))
      (sha256
        (base32
          "1k4fg39rrkl3bpgchfj94fbl9s4ysaz16w8dkqncf2vyf79l3qz0"))))
  (build-system ruby-build-system)
  (arguments
   `(#:tests? #f)) ; no tests
  (synopsis "Source code for the ruby-coffee-script gem.")
  (description
    "This gem contains the JavaScript source code used for converting between
CoffeeScript to JavaScript.  It is updated each time a new version of
CoffeeScript is released.")
  ;; homepage listed on rubygems does not exist, so use the rubygems URL instead
  (home-page
    "https://rubygems.org/gems/coffee-script-source")
  (license license:expat)))

(define-public ruby-execjs
  (package
    (name "ruby-execjs")
    (version "2.7.0")
    (source
     (origin
       (method url-fetch)
       ;; fetch from github as the gem does not contain testing code
       (uri (string-append "https://github.com/rails/execjs/archive/v"
                           version ".tar.gz"))
       (sha256
        (base32
         "0h1bi96gks205pz2mn2zwldz8h0ajwggyi85bwfbksmrsn2lwick"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; tests require ruby-therubyracer and thus v8.
    (native-inputs
     `(("bundler" ,bundler)))
    (synopsis
     "ExecJS lets you run JavaScript code from Ruby.")
    (description
     "ExecJS lets you run JavaScript code from Ruby.")
    (home-page "https://github.com/rails/execjs")
    (license license:expat)))

(define-public duktape
  (package
    (name "duktape")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       ;; Use the GitHub source for ease of updating.
       (uri (string-append "https://github.com/svaarala/duktape/releases/download/v"
                           version "/duktape-" version ".tar.xz"))
       (sha256
        (base32
         "0rd9wz5716qhzqwwj26i2x5m8dd020rvaf2i08sa4jxrl6nk3cil"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; Tests require many dependencies including v8.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         ;; Installing is simply copying source code files so they can be
         ;; embedded elsewhere.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (src (string-append out "/src")))
               (install-file "src/duktape.c" src)
               (install-file "src/duktape.h" src)))))))
    (synopsis "Embeddable JavaScript engine")
    (description
     "Duktape is an embeddable Javascript engine, with a focus on portability
and compact footprint.  Duktape is easy to integrate into a C/C++ project: add
@code{duktape.c}, @code{duktape.h}, and @code{duk_config.h} to your build, and
use the Duktape API to call Ecmascript functions from C code and vice versa.")
    (home-page "http://duktape.org")
    (license license:expat)))

(define-public ruby-duktape
  (package
    (name "ruby-duktape")
    (version "1.3.0.4")
    (source
     (origin
       (method url-fetch)
       ;; fetch from github as the gem does not contain testing code
       (uri (string-append "https://github.com/judofyr/duktape.rb/archive/v"
                           version ".tar.gz"))
       (sha256
        (base32
         "1xgs7ll9xwm5p451mh70cm5646wijc2jdvjdb81a17wwvccf7djw"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Duktape comes with the duktape .c and .h files.  Replace these with
         ;; those from the duktape Guix package.
         (add-after 'unpack 'replace-bundled-duktape
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (for-each (lambda (file)
                         (delete-file (string-append "ext/duktape/" file))
                         (copy-file
                          (string-append
                           (assoc-ref inputs "duktape") "/src/" file)
                          (string-append "ext/duktape/" file)))
                       (list "duktape.c" "duktape.h"))))
         (add-before 'check 'remove-dependency
           (lambda _
             ;; Gem is not needed for testing.
             (substitute* "Gemfile"
               (("^  gem 'aws-sdk', '~> 2.2.0'") ""))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-sdoc" ,ruby-sdoc)
       ("duktape" ,duktape)))
    (synopsis "Bindings to the Duktape JavaScript interpreter")
    (description
     "Bindings to the Duktape JavaScript interpreter")
    (home-page
     "https://github.com/judofyr/duktape.rb")
    (license license:expat)))

(define-public ruby-therubyracer
  (package
    (name "ruby-therubyracer")
    (version "0.12.2")
    (source
     (origin
       (method url-fetch)
       ;; Build from GitHub source so that patches can be applied.
       (uri (string-append "https://github.com/cowboyd/therubyracer/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0vyiacgg0hngl3gxh7n5lvskac5ms2rcbjrixj4mc8c2adhmqj1a"))
       (patches (search-patches "ruby-therubyracer-fix-gemspec.patch"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (delete 'extract-gemspec)
         (add-before 'build 'fix-gemfile
           (lambda _
             (substitute* "Gemfile"
               (("redjs.*") "redjs'\n")
               ((".*gem-compiler.*") "\n"))
             #t))
         (add-before 'install 'prepare-libv8
           ;; Since therubyracer requires 'libv8' when compiling native
           ;; extensions during installation, it must be installed into the same
           ;; gem directory.  We make 'libv8' a native input and copy the entire
           ;; libv8 gem directory into the the current output and install
           ;; therubyracer into that.
           (lambda* (#:key inputs outputs #:allow-other-keys)
                       (copy-recursively
                        (assoc-ref inputs "ruby-libv8")
                        (assoc-ref outputs "out"))
                       #t))
         (add-before 'check 'compile
                     (lambda _
                       (zero? (system* "rake" "compile")))))))
    (propagated-inputs
     `(("ruby-ref" ,ruby-ref)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-redjs" ,ruby-redjs)
       ("ruby-rspec" ,ruby-rspec-2)
       ("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-libv8" ,ruby-libv8-3.16.14)))
    (synopsis
     "Call JavaScript code and manipulate JavaScript objects from Ruby. Call Ruby code and manipulate Ruby objects from JavaScript.")
    (description
     "Call JavaScript code and manipulate JavaScript objects from Ruby. Call Ruby code and manipulate Ruby objects from JavaScript.")
    (home-page
     "http://github.com/cowboyd/therubyracer")
    (license license:expat)))

(define-public ruby-libv8-3.16.14
  (package
    (name "ruby-libv8")
    (version "3.16.14.14") ; Package an even-numbered release so only source
                           ; code it included and not binaries.
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "libv8" version))
       (sha256
        (base32
         "1sipv60i1fxia2y08q2n2q179pxizhnx9065x8630wdnvbpqrl0x"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       ;#:gem-flags (list "--" "--with-system-v8")
       #:phases
       (modify-phases %standard-phases
         ;; Non-printing characters trip up this build phase and it isn't used
         ;; anyway.
                                        ;(delete 'extract-gemspec)
         ;; (replace 'replace-git-ls-files
         ;;          (lambda _
         ;;            (substitute* "libv8.gemspec"
         ;;              (("git ls-files") "find . -type f |sort"))
         ;;            #t))
         (add-after 'extract-gemspec 'fix-dependencies
                    (lambda _
                      ;; Remove non-printing character that trips up substitute*
                      ;; (system* "sed" "s/ stub.*//" "-i" "libv8.gemspec")
                      ;; (delete-file-recursively "vendor")
                      ;; (substitute* "libv8.gemspec"
                      ;;   (("<rake>.*") "<rake>)\n")
                      ;;   ((", \\\"vendor.*") "]\n")) ; Do not distribute
                      ;;                   ; depot_tools as this includes pre-built
                      ;;                   ; binaries.
                      
             #t))
         )))
         ;; (Add-after 'install 'remove-extraneous-files
         ;;   (lambda* (#:key outputs #:allow-other-keys)
         ;;     (delete-file-recursively
         ;;      (string-append 
         ;;       (assoc-ref outputs "out")
         ;;       ;; TODO: Generalise this path.
         ;;       "/lib/ruby/gems/2.3.0/gems/libv8-5.2.361.43.1/vendor")))))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-rspec" ,ruby-rspec-2)
       ("ruby-rspec-spies" ,ruby-rspec-spies)
       ("which" ,which)
       ("python" ,python-2)))
    (synopsis
     "Distributes the V8 JavaScript engine in binary and source forms in order
to support fast builds of The Ruby Racer")
    (description
     "Distributes the V8 JavaScript engine in binary and source forms in order
to support fast builds of The Ruby Racer")
    (home-page "http://github.com/cowboyd/libv8")
    (license license:expat)))


;; haml 4 cannot currently be packaged because tilt is not yet packaged.
(define-public ruby-haml
  (package
    (name "ruby-haml")
    (version "4.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "haml" version))
       (sha256
        (base32
         "0mrzjgkygvfii66bbylj2j93na8i89998yi01fin3whwqbvx0m1p"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; circular dependency with tilt
;    (propagated-inputs `(("ruby-tilt" ,ruby-tilt)))
    (synopsis "Haml (HTML Abstraction Markup Language) library.")
    (description
     "Haml (HTML Abstraction Markup Language) is a layer on top of HTML or XML
that is designed to express the structure of documents using indentation rather
than closing tags.  It was originally envisioned as a plugin for Ruby on Rails,
but it can function as a stand-alone templating engine.")
    (home-page "http://haml.info/")
    (license license:expat)))

;; haml 3 cannot currently be packaged because action_pack, action_controller
;; and action_view are required at test time.
(define-public ruby-haml-3
  (package
    (name "ruby-haml")
    (version "3.1.8")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "haml" version))
              (sha256
               (base32
                "05qnmrcjp85bgjwgmb0yhi7jyb7vd9jdqgxhzc7mmz0ch58rvxj4"))))
    (build-system ruby-build-system)
                                        ;    (arguments
                                        ;     `(#:tests? #f))
    (synopsis "Haml (HTML Abstraction Markup Language) library.")
    (description
     "Haml (HTML Abstraction Markup Language) is a layer on top of HTML or XML
that is designed to express the structure of documents using indentation rather
than closing tags.  It was originally envisioned as a plugin for Ruby on Rails,
but it can function as a stand-alone templating engine.")
    (home-page "http://haml.info/")
    (license license:expat)))




(define-public ruby-backports
  (package
  (name "ruby-backports")
  (version "3.6.7")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "backports" version))
      (sha256
        (base32
          "0m8jkjh4kymgfipd6yb0zcxlas4x5r60k2c94zkklb1ryma33grc"))))
  (build-system ruby-build-system)
  (synopsis
    "Essential backports that enable many of the nice features of Ruby 1.8.7 up to 2.1.0 for earlier versions.")
  (description
    "Essential backports that enable many of the nice features of Ruby 1.8.7 up to 2.1.0 for earlier versions.")
  (home-page
    "http://github.com/marcandre/backports")
  (license license:expat)))

(define-public ruby-faraday
  (package
    (name "ruby-faraday")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "faraday" version))
       (sha256
        (base32
         "1kplqkpn2s2yl3lxdf6h7sfldqvkbkpxwwxhyk7mdhjplb5faqh6"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-multipart-post" ,ruby-multipart-post)))
    (synopsis "HTTP/REST API client library.")
    (description "HTTP/REST API client library.")
    (home-page
     "https://github.com/lostisland/faraday")
    (license license:expat)))

(define-public ruby-faraday-middleware
  (package
    (name "ruby-faraday-middleware")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "faraday_middleware" version))
       (sha256
        (base32
         "0nxia26xzy8i56qfyz1bg8dg9yb26swpgci8n5jry8mh4bnx5r5h"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-faraday" ,ruby-faraday)))
    (synopsis "Various middleware for Faraday")
    (description "Various middleware for Faraday")
    (home-page
     "https://github.com/lostisland/faraday_middleware")
    (license license:expat)))

(define-public ruby-gh
  (package
    (name "ruby-gh")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "gh" version))
       (sha256
        (base32
         "0j7m6jmxzkxvnqgnhmci33a89qpaxxcrm55kk5vz4bcpply04hx2"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-addressable" ,ruby-addressable)
       ("ruby-backports" ,ruby-backports)
       ("ruby-faraday" ,ruby-faraday)
       ("ruby-multi-json" ,ruby-multi-json)
       ("ruby-net-http-persistent"
        ,ruby-net-http-persistent)
       ;("ruby-net-http-pipeline"
       ; ,ruby-net-http-pipeline)
       ))
    (synopsis
     "multi-layer client for the github api v3")
    (description
     "multi-layer client for the github api v3")
    (home-page "http://gh.rkh.im/")
    (license license:expat)))

(define-public ruby-highline
  (package
    (name "ruby-highline")
    (version "1.7.8")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "highline" version))
       (sha256
        (base32
         "1nf5lgdn6ni2lpfdn4gk3gi47fmnca2bdirabbjbz1fk9w4p8lkr"))))
    (build-system ruby-build-system)
    (synopsis
     "A high-level IO library that provides validation, type conversion, and more for
command-line interfaces. HighLine also includes a complete menu system that can
crank out anything from simple list selection to complete shells with just
minutes of work.
")
    (description
     "A high-level IO library that provides validation, type conversion, and more for
command-line interfaces. HighLine also includes a complete menu system that can
crank out anything from simple list selection to complete shells with just
minutes of work.
")
    (home-page "https://github.com/JEG2/highline")
    (license #f)))

(define-public ruby-launchy
  (package
    (name "ruby-launchy")
    (version "2.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "launchy" version))
       (sha256
        (base32
         "190lfbiy1vwxhbgn4nl4dcbzxvm049jwc158r2x7kq3g5khjrxa2"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-addressable" ,ruby-addressable)))
    (synopsis
     "Launchy is helper class for launching cross-platform applications in a fire and forget manner. There are application concepts (browser, email client, etc) that are common across all platforms, and they may be launched differently on each platform. Launchy is here to make a common approach to launching external application from within ruby programs.")
    (description
     "Launchy is helper class for launching cross-platform applications in a fire and forget manner. There are application concepts (browser, email client, etc) that are common across all platforms, and they may be launched differently on each platform. Launchy is here to make a common approach to launching external application from within ruby programs.")
    (home-page
     "http://github.com/copiousfreetime/launchy")
    (license #f)))



;; TODO: inherit from mime-types
;; (define-public ruby-mime-types-2
;;   (package
;;     (name "ruby-mime-types")
;;     (version "2.6.2")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri (rubygems-uri "mime-types" version))
;;        (sha256
;;         (base32
;;          "136ybsrwn1k7zcbxbrczf0n4z3liy5ygih3q9798s8pi80smi5dm"))))
;;     (build-system ruby-build-system)
;;     (native-inputs
;;      `(("ruby-hoe" ,ruby-hoe)
;;        ("ruby-hoe-travis" ,ruby-hoe-travis)))
;;     (synopsis "library and registry for MIME content type definitions")
;;     (description "The mime-types library provides a library and registry for
;; information about MIME content type definitions.  It can be used to determine
;; defined filename extensions for MIME types, or to use filename extensions to
;; look up the likely MIME type definitions.")
;;     (home-page "https://github.com/mime-types/ruby-mime-types/")
;;     (license (list license:gpl2+ license:expat license:artistic2.0))))








(define-public ruby-travis
  (package
    (name "ruby-travis")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "travis" version))
       (sha256
        (base32
         "0s88790wlhlsaxs9561w3h6vhj00sc36bw6k7rajj6vi0416s73z"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-addressable" ,ruby-addressable)
       ("ruby-backports" ,ruby-backports)
       ("ruby-faraday" ,ruby-faraday)
       ("ruby-faraday-middleware"
        ,ruby-faraday-middleware)
       ("ruby-gh" ,ruby-gh)
       ("ruby-highline" ,ruby-highline)
       ("ruby-launchy" ,ruby-launchy)
       ("ruby-pry" ,ruby-pry)
       ;("ruby-pusher-client" ,ruby-pusher-client)
       ;("ruby-typhoeus" ,ruby-typhoeus)
       ))
    (synopsis
     "CLI and Ruby client library for Travis CI")
    (description
     "CLI and Ruby client library for Travis CI")
    (home-page
     "https://github.com/travis-ci/travis.rb")
    (license license:expat)))

(define-public ruby-rails
  (package
   (name "ruby-rails")
   (version "5.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "rails" version))
     (sha256
      (base32
       "1vymw9wpygfwx3cz29ak8nrzfhj01bbca8fcc5swsv1x8pfy4yrf"))))
   (build-system ruby-build-system)
   (arguments
    `(#:tests? #f)) ; No tests included in gem. TODO: download from GitHub so
                    ; tests can be run.
   (propagated-inputs
    `(("ruby-actioncable" ,ruby-actioncable)
      ("ruby-actionmailer" ,ruby-actionmailer)
      ("ruby-actionpack" ,ruby-actionpack)
      ("ruby-actionview" ,ruby-actionview)
      ("ruby-activejob" ,ruby-activejob)
      ("ruby-activemodel" ,ruby-activemodel)
      ("ruby-activerecord" ,ruby-activerecord)
      ("ruby-activesupport" ,ruby-activesupport)
      ("bundler" ,bundler)
      ("ruby-railties" ,ruby-railties)
      ("ruby-sprockets-rails" ,ruby-sprockets-rails)
      ("ruby-spring" ,ruby-spring)
      ("ruby-sqlite3" ,ruby-sqlite3)
      ("ruby-puma" ,ruby-puma)
      ("ruby-sass-rails" ,ruby-sass-rails)
      ("ruby-uglifier" ,ruby-uglifier)
      ("ruby-coffee-rails" ,ruby-coffee-rails)
      ("ruby-jquery-rails" ,ruby-jquery-rails)
      ("ruby-turbolinks" ,ruby-turbolinks)
      ("ruby-jbuilder" ,ruby-jbuilder)
      ("ruby-byebug" ,ruby-byebug)
      ("ruby-web-console" ,ruby-web-console)
      ("ruby-listen" ,ruby-listen-3.0)
      ("ruby-spring-watcher-listen" ,ruby-spring-watcher-listen)))
   (synopsis
    "Ruby on Rails is a full-stack web framework optimized for programmer happiness and sustainable productivity. It encourages beautiful code by favoring convention over configuration.")
   (description
    "Ruby on Rails is a full-stack web framework optimized for programmer happiness and sustainable productivity.  It encourages beautiful code by favoring convention over configuration.")
   (home-page "http://www.rubyonrails.org")
   (license license:expat)))

(define-public ruby-actioncable
  (package
   (name "ruby-actioncable")
   (version "5.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "actioncable" version))
     (sha256
      (base32
       "1x78kvrwkc6i8bwli8kkf21yikz70b5s6jl2ncpb8fz7dj1xkkj3"))))
   (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; tests not included
   (propagated-inputs
    `(("ruby-actionpack" ,ruby-actionpack)
      ("ruby-nio4r" ,ruby-nio4r)
      ("ruby-websocket-driver" ,ruby-websocket-driver)))
   (synopsis
    "Structure many real-time application concerns into channels over a single WebSocket connection.")
   (description
    "Structure many real-time application concerns into channels over a single WebSocket connection.")
   (home-page "http://rubyonrails.org")
   (license license:expat)))

(define-public ruby-actionmailer
  (package
   (name "ruby-actionmailer")
   (version "5.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "actionmailer" version))
     (sha256
      (base32
       "1lyrw3jgpmjbsjp9lsd4qhyr9slsm1h3pcb75kmszs9lg8bkb586"))))
   (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; tests not included
   (propagated-inputs
    `(("ruby-actionpack" ,ruby-actionpack)
      ("ruby-actionview" ,ruby-actionview)
      ("ruby-activejob" ,ruby-activejob)
      ("ruby-mail" ,ruby-mail)
      ("ruby-rails-dom-testing"
       ,ruby-rails-dom-testing)))
   (synopsis
    "Email on Rails. Compose, deliver, receive, and test emails using the familiar controller/view pattern. First-class support for multipart email and attachments.")
   (description
    "Email on Rails.  Compose, deliver, receive, and test emails using the familiar controller/view pattern.  First-class support for multipart email and attachments.")
   (home-page "http://www.rubyonrails.org")
   (license license:expat)))

(define-public ruby-actionpack
  (package
   (name "ruby-actionpack")
   (version "5.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "actionpack" version))
     (sha256
      (base32
       "0yfq3l561808bh9ahp245a14ikh2li8k67nvk3rjdpxciwkvmqvw"))))
   (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; tests not included
   (propagated-inputs
    `(("ruby-actionview" ,ruby-actionview)
      ("ruby-activesupport" ,ruby-activesupport)
      ("ruby-rack" ,ruby-rack)
      ("ruby-rack-test" ,ruby-rack-test)
      ("ruby-rails-dom-testing"
       ,ruby-rails-dom-testing)
      ("ruby-rails-html-sanitizer"
       ,ruby-rails-html-sanitizer)))
   (synopsis
    "Web apps on Rails. Simple, battle-tested conventions for building and testing MVC web applications. Works with any Rack-compatible server.")
   (description
    "Web apps on Rails.  Simple, battle-tested conventions for building and testing MVC web applications.  Works with any Rack-compatible server.")
   (home-page "http://www.rubyonrails.org")
   (license license:expat)))

(define-public ruby-actionview
  (package
   (name "ruby-actionview")
   (version "5.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "actionview" version))
     (sha256
      (base32
       "10f9d1jl945vr0l4sfr8v7rf3lkdbq33f5yvnx36aa2vskz529m1"))))
   (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; tests not included
   (propagated-inputs
    `(("ruby-activesupport" ,ruby-activesupport)
      ("ruby-builder" ,ruby-builder)
      ("ruby-erubis" ,ruby-erubis)
      ("ruby-rails-dom-testing"
       ,ruby-rails-dom-testing)
      ("ruby-rails-html-sanitizer"
       ,ruby-rails-html-sanitizer)))
   (synopsis
    "Simple, battle-tested conventions and helpers for building web pages.")
   (description
    "Simple, battle-tested conventions and helpers for building web pages.")
   (home-page "http://www.rubyonrails.org")
   (license license:expat)))

(define-public ruby-activejob
  (package
   (name "ruby-activejob")
   (version "5.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "activejob" version))
     (sha256
      (base32
       "1xrchgz6xm5j2wqfqhh6qixvssv37hsdpyi4m2zb1m53qjn0q3pv"))))
   (build-system ruby-build-system)
   (arguments
     `(#:tests? #f))
   (propagated-inputs
    `(("ruby-activesupport" ,ruby-activesupport)
      ("ruby-globalid" ,ruby-globalid)))
   (synopsis
    "Declare job classes that can be run by a variety of queueing backends.")
   (description
    "Declare job classes that can be run by a variety of queueing backends.")
   (home-page "http://www.rubyonrails.org")
   (license license:expat)))

(define-public ruby-activemodel
  (package
   (name "ruby-activemodel")
   (version "5.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "activemodel" version))
     (sha256
      (base32
       "11j5rixlcp3i8a5mxfl4b0yhac3kzzhgmvcibbnphy5x974nr8aa"))))
   (build-system ruby-build-system)
   (arguments
     `(#:tests? #f))
   (propagated-inputs
    `(("ruby-activesupport" ,ruby-activesupport)))
   (synopsis
    "A toolkit for building modeling frameworks like Active Record. Rich support for attributes, callbacks, validations, serialization, internationalization, and testing.")
   (description
    "This package provides a toolkit for building modeling frameworks like Active Record.  Rich support for attributes, callbacks, validations, serialization, internationalization, and testing.")
   (home-page "http://www.rubyonrails.org")
   (license license:expat)))

(define-public ruby-activerecord
  (package
   (name "ruby-activerecord")
   (version "5.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "activerecord" version))
     (sha256
      (base32
       "1sl1f0capcdfvvabsc3cxfrdq78nwafp1340zq6gxqrwkvab1nxq"))))
   (build-system ruby-build-system)
   (arguments
     `(#:tests? #f))
   (propagated-inputs
    `(("ruby-activemodel" ,ruby-activemodel)
      ("ruby-activesupport" ,ruby-activesupport)
      ("ruby-arel" ,ruby-arel)))
   (synopsis
    "Databases on Rails. Build a persistent domain model by mapping database tables to Ruby classes. Strong conventions for associations, validations, aggregations, migrations, and testing come baked-in.")
   (description
    "Databases on Rails.  Build a persistent domain model by mapping database tables to Ruby classes.  Strong conventions for associations, validations, aggregations, migrations, and testing come baked-in.")
   (home-page "http://www.rubyonrails.org")
   (license license:expat)))

(define-public ruby-railties
  (package
   (name "ruby-railties")
   (version "5.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "railties" version))
     (sha256
      (base32
       "0jrp9752vsfm8jz7mz8spns2cgzhg374hn1nrrnjl433rkjr6b3r"))))
   (build-system ruby-build-system)
   (arguments
    `(#:tests? #f)) ; tests not included
   (propagated-inputs
    `(("ruby-actionpack" ,ruby-actionpack)
      ("ruby-activesupport" ,ruby-activesupport)
      ("ruby-method-source" ,ruby-method-source)
      ("ruby-thor" ,ruby-thor)))
   (synopsis
    "Rails internals: application bootup, plugins, generators, and rake tasks.")
   (description
    "Rails internals: application bootup, plugins, generators, and rake tasks.")
   (home-page "http://www.rubyonrails.org")
   (license license:expat)))

(define-public ruby-sprockets-rails
  (package
   (name "ruby-sprockets-rails")
   (version "3.1.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "sprockets-rails" version))
     (sha256
      (base32
       "1sak0as7ka964f6zjb1w8hkvfkkbf55kpcyvh7k6nyrb6pqnwmnf"))))
   (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; tests not included
   (propagated-inputs
    `(("ruby-actionpack" ,ruby-actionpack)
      ("ruby-activesupport" ,ruby-activesupport)
      ("ruby-sprockets" ,ruby-sprockets)))
   (synopsis "Sprockets Rails integration")
   (description "Sprockets Rails integration")
   (home-page
    "https://github.com/rails/sprockets-rails")
   (license license:expat)))

(define-public ruby-nio4r
  (package
   (name "ruby-nio4r")
   (version "1.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "nio4r" version))
     (sha256
      (base32
       "1adnm77xfxck0mrvid5d7lwng783gh580rh3y18nq4bwdikr6nha"))))
   (build-system ruby-build-system)
   (arguments
    `(#:test-target "spec"
      #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-dependencies
                     (lambda _
                       (substitute* "Gemfile"
                         (("rubocop.*") "rubocop\"\n"))
                       #t))
         (add-before 'check 'compile
                     (lambda _
                       (zero? (system* "rake" "compile")))))))
   (native-inputs
    `(("bundler" ,bundler)
      ("ruby-rake-compiler" ,ruby-rake-compiler)
      ("ruby-rspec" ,ruby-rspec)
      ("ruby-rubocop" ,ruby-rubocop)
      ("ruby-coveralls" ,ruby-coveralls)))
   (synopsis "New IO for Ruby")
   (description "New IO for Ruby")
   (home-page "https://github.com/celluloid/nio4r")
   (license license:expat)))

(define-public ruby-websocket-driver
  (package
   (name "ruby-websocket-driver")
   (version "0.6.4")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "websocket-driver" version))
     (sha256
      (base32
       "1m37q24mxykvixcj8sv0jz7y2a88spysxg5rp4zf4p1q7mbblshy"))))
   (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; tests not included
   (propagated-inputs
    `(("ruby-websocket-extensions"
       ,ruby-websocket-extensions)))
   (synopsis
    "WebSocket protocol handler with pluggable I/O")
   (description
    "WebSocket protocol handler with pluggable I/O")
   (home-page
    "http://github.com/faye/websocket-driver-ruby")
   (license license:expat)))

(define-public ruby-mail
  (package
   (name "ruby-mail")
   (version "2.6.4")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "mail" version))
     (sha256
      (base32
       "0c9vqfy0na9b5096i5i4qvrvhwamjnmajhgqi3kdsdfl8l6agmkp"))))
   (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; needs more pkgs
   (native-inputs
    `(("bundler" ,bundler)
      ("ruby-rspec" ,ruby-rspec)))
   (propagated-inputs
    `(("ruby-mime-types" ,ruby-mime-types)))
   (synopsis "A really Ruby Mail handler.")
   (description
    "This package provides a really Ruby Mail handler.")
   (home-page "https://github.com/mikel/mail")
   (license license:expat)))

(define-public ruby-mime-types-2
  (package
    (inherit ruby-mime-types)
    (version "2.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mime-types" version))
       (sha256
        (base32
         "136ybsrwn1k7zcbxbrczf0n4z3liy5ygih3q9798s8pi80smi5dm"))))
    (arguments
     `(#:tests? #f ; Deprecated library and by default tests fail.
                ))))

(define-public ruby-rails-dom-testing
  (package
   (name "ruby-rails-dom-testing")
   (version "2.0.1")
   (source
    (origin
      (method url-fetch)
      ;; The gem does not include a Rakefile, so we fetch the tarball from
      ;; Github.
      (uri (string-append "https://github.com/rails/rails-dom-testing/archive/v"
                          version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32
        "17pb4nns4cl6nnh6vxldm086cvhc02vhgy1d5mm0imwpqjb0pb6q"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("bundler" ,bundler)))
   (propagated-inputs
    `(("ruby-activesupport" ,ruby-activesupport)
      ("ruby-nokogiri" ,ruby-nokogiri)))
   (synopsis
    " This gem can compare doms and assert certain elements exists in doms using Nokogiri. ")
   (description
    " This gem can compare doms and assert certain elements exists in doms using Nokogiri. ")
   (home-page
    "https://github.com/rails/rails-dom-testing")
   (license license:expat)))

(define-public ruby-rails-html-sanitizer
  (package
   (name "ruby-rails-html-sanitizer")
   (version "1.0.3")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "rails-html-sanitizer" version))
     (sha256
      (base32
       "138fd86kv073zqfx0xifm646w6bgw2lr8snk16lknrrfrss8xnm7"))))
   (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; tests not included
   (propagated-inputs
    `(("ruby-loofah" ,ruby-loofah)))
   (synopsis
    "HTML sanitization for Rails applications")
   (description
    "HTML sanitization for Rails applications")
   (home-page
    "https://github.com/rails/rails-html-sanitizer")
   (license license:expat)))

(define-public ruby-globalid
  (package
   (name "ruby-globalid")
   (version "0.3.6")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "globalid" version))
     (sha256
      (base32
       "145xrpsfx1qqjy33r6qa588wb16dvdhxzj2aysh755vhg6hgm291"))))
   (build-system ruby-build-system)
   (arguments
     `(#:tests? #f))
   (propagated-inputs
    `(("ruby-activesupport" ,ruby-activesupport)))
   (synopsis
    "URIs for your models makes it easy to pass references around.")
   (description
    "URIs for your models makes it easy to pass references around.")
   (home-page "http://www.rubyonrails.org")
   (license license:expat)))

(define-public ruby-sprockets
(package
  (name "ruby-sprockets")
  (version "3.6.3")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "sprockets" version))
      (sha256
        (base32
          "0flynmaaxa53pv15x7kcxr7z6h1hn7ifrxk13dfhhvh6h38jnzkv"))))
  (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; tests not included
  (propagated-inputs
    `(("ruby-concurrent" ,ruby-concurrent)
      ("ruby-rack" ,ruby-rack)))
  (synopsis
    "Sprockets is a Rack-based asset packaging system that concatenates and serves JavaScript, CoffeeScript, CSS, LESS, Sass, and SCSS.")
  (description
    "Sprockets is a Rack-based asset packaging system that concatenates and serves JavaScript, CoffeeScript, CSS, LESS, Sass, and SCSS.")
  (home-page "https://github.com/rails/sprockets")
  (license license:expat)))

(define-public ruby-websocket-extensions
(package
  (name "ruby-websocket-extensions")
  (version "0.1.2")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "websocket-extensions" version))
      (sha256
        (base32
          "07qnsafl6203a2zclxl20hy4jq11c471cgvd0bj5r9fx1qqw06br"))))
  (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; tests not included
  (synopsis
    "Generic extension manager for WebSocket connections")
  (description
    "Generic extension manager for WebSocket connections")
  (home-page
    "http://github.com/faye/websocket-extensions-ruby")
  (license license:expat)))

(define-public ruby-loofah
(package
  (name "ruby-loofah")
  (version "2.0.3")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "loofah" version))
      (sha256
        (base32
          "109ps521p0sr3kgc460d58b4pr1z4mqggan2jbsf0aajy9s6xis8"))))
  (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; 1 test fails
  (native-inputs
   `(("ruby-hoe" ,ruby-hoe)
     ("ruby-rr" ,ruby-rr)))
  (propagated-inputs
    `(("ruby-nokogiri" ,ruby-nokogiri)))
  (synopsis
    "Loofah is a general library for manipulating and transforming HTML/XML
documents and fragments. It's built on top of Nokogiri and libxml2, so
it's fast and has a nice API.

Loofah excels at HTML sanitization (XSS prevention). It includes some
nice HTML sanitizers, which are based on HTML5lib's whitelist, so it
most likely won't make your codes less secure. (These statements have
not been evaluated by Netexperts.)

ActiveRecord extensions for sanitization are available in the
`loofah-activerecord` gem (see
https://github.com/flavorjones/loofah-activerecord).")
  (description
    "Loofah is a general library for manipulating and transforming HTML/XML
documents and fragments.  It's built on top of Nokogiri and libxml2, so
it's fast and has a nice API.

Loofah excels at HTML sanitization (XSS prevention).  It includes some
nice HTML sanitizers, which are based on HTML5lib's whitelist, so it
most likely won't make your codes less secure. (These statements have
not been evaluated by Netexperts.)

ActiveRecord extensions for sanitization are available in the
`loofah-activerecord` gem (see
https://github.com/flavorjones/loofah-activerecord).")
  (home-page
    "https://github.com/flavorjones/loofah")
  (license license:expat)))

(define-public ruby-rr
(package
  (name "ruby-rr")
  (version "1.2.0")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "rr" version))
      (sha256
        (base32
          "0b05ycaw17wbxzycv1wvzklpqjnmi0dqy01igcl5jfmy1ydky66r"))))
  (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; test files not included
  (native-inputs
   `(("bundler" ,bundler)
     ("ruby-rspec" ,ruby-rspec)))
  (synopsis
    "RR is a test double framework that features a rich selection of double techniques and a terse syntax.")
  (description
    "RR is a test double framework that features a rich selection of double techniques and a terse syntax.")
  (home-page "https://rr.github.io/rr")
  (license license:expat)))

(define-public ruby-rubocop
(package
  (name "ruby-rubocop")
  (version "0.41.2")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "rubocop" version))
      (sha256
        (base32
          "02adr908a9l8nhdfjz137i20w1dv8mbfiamy0m9z9q0fvslfdxly"))))
  (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; test files not included
  (propagated-inputs
    `(("ruby-parser" ,ruby-parser)
      ("ruby-powerpack" ,ruby-powerpack)
      ("ruby-rainbow" ,ruby-rainbow)
      ("ruby-ruby-progressbar" ,ruby-ruby-progressbar)
      ("ruby-unicode-display-width"
       ,ruby-unicode-display-width)))
  (synopsis
    "    Automatic Ruby code style checking tool.
    Aims to enforce the community-driven Ruby Style Guide.
")
  (description
    "    Automatic Ruby code style checking tool.
    Aims to enforce the community-driven Ruby Style Guide.
")
  (home-page "http://github.com/bbatsov/rubocop")
  (license license:expat)))

(define-public ruby-parser
(package
  (name "ruby-parser")
  (version "2.3.1.2")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "parser" version))
      (sha256
        (base32
          "0fxcs83z28wxn6bphbq5q40c1y5ab8zl8ww17jwkbi032wf6iik6"))))
  (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; need more
  (native-inputs
   `(("bundler" ,bundler)
     ("ruby-racc" ,ruby-racc)))
  (propagated-inputs `(("ruby-ast" ,ruby-ast)))
  (synopsis "A Ruby parser written in pure Ruby.")
  (description
    "This package provides a Ruby parser written in pure Ruby.")
  (home-page
    "https://github.com/whitequark/parser")
  (license license:expat)))

(define-public ruby-powerpack
(package
  (name "ruby-powerpack")
  (version "0.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "powerpack" version))
      (sha256
        (base32
          "1fnn3fli5wkzyjl4ryh0k90316shqjfnhydmc7f8lqpi0q21va43"))))
  (build-system ruby-build-system)
  (arguments
   `(#:test-target "spec"))
  (native-inputs
   `(("bundler" ,bundler)
     ("ruby-rspec" ,ruby-rspec)
     ("ruby-yard" ,ruby-yard)))
  (synopsis
    "A few useful extensions to core Ruby classes.")
  (description
    "This package provides a few useful extensions to core Ruby classes.")
  (home-page
    "https://github.com/bbatsov/powerpack")
  (license license:expat)))

(define-public ruby-rainbow
(package
  (name "ruby-rainbow")
  (version "2.1.0")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "rainbow" version))
      (sha256
        (base32
          "11licivacvfqbjx2rwppi8z89qff2cgs67d4wyx42pc5fg7g9f00"))))
  (build-system ruby-build-system)
  (arguments
   `(#:test-target "spec"))
  (native-inputs
   `(("bundler" ,bundler)
     ("ruby-rspec" ,ruby-rspec)))
  (synopsis
    "Colorize printed text on ANSI terminals")
  (description
    "Colorize printed text on ANSI terminals")
  (home-page "https://github.com/sickill/rainbow")
  (license license:expat)))

(define-public ruby-rspec-support
  (package
    (name "ruby-rspec-support")
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       ;; The gem does not include a Rakefile, nor does it contain a
       ;; gemspec file, nor does it come with the tests.  This is why
       ;; we fetch the tarball from Github.
       (uri (string-append "https://github.com/rspec/rspec-support/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "13sikbiligm43c036cs0rsqhnrjy94l0an23qmla7lyngwp0asl5"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; Tests require further depedencies e.g. ruby-rake, and
                     ; further work.
    ;; (arguments
    ;;  `(#:phases
    ;;    (modify-phases %standard-phases
    ;;      (add-before 'check 'fix-dependencies
    ;;        (lambda _
    ;;          ; Do not require rspec depedencies to be checked out from GitHub.
    ;;          (substitute* "Gemfile"
    ;;            ((", :git.*") "\n"))
    ;;          (substitute* "rspec-support.gemspec"
    ;;            ((".*rake.*") ""))
    ;;          #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-thread-order" ,ruby-thread-order)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-simplecov" ,ruby-simplecov)))
    (synopsis "Support utilities for RSpec gems")
    (description "Support utilities for RSpec gems")
    (home-page
     "https://github.com/rspec/rspec-support")
    (license license:expat)))

;; not needed since the rspec-support not using it?
(define-public ruby-thread-order
  (package
    (name "ruby-thread-order")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "thread_order" version))
       (sha256
        (base32
         "1n8zs3m7na5jmpf0pw0z79vg0x0lfzajpynp28zb43l89l00qcfi"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; Do not run tests to avoid depedency cycle with
                     ; ruby-rspec-support.
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)))
    (synopsis
     "Test helper for ordering threaded code (does not depend on gems or stdlib, tested on 1.8.7 - 2.2, rbx, jruby).")
    (description
     "Test helper for ordering threaded code (does not depend on gems or stdlib, tested on 1.8.7 - 2.2, rbx, jruby).")
    (home-page
     "https://github.com/JoshCheek/thread_order")
    (license license:expat)))

;; There is a separate gem called 'progressbar', so we name the package here
;; 'ruby-ruby-progressbar'.
(define-public ruby-ruby-progressbar
  (package
    (name "ruby-ruby-progressbar")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ruby-progressbar" version))
       (sha256
        (base32
         "1qzc7s7r21bd7ah06kskajc2bjzkr9y0v5q48y0xwh2l55axgplm"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; Tests require further dependencies such as 'rspectacular'.
    (synopsis
     "Ruby/ProgressBar is an extremely flexible text progress bar library for Ruby.
The output can be customized with a flexible formatting system including:
percentage, bars of various formats, elapsed time and estimated time remaining.
")
    (description
     "Ruby/ProgressBar is an extremely flexible text progress bar library for Ruby.
The output can be customized with a flexible formatting system including:
percentage, bars of various formats, elapsed time and estimated time remaining.
")
    (home-page
     "https://github.com/jfelchner/ruby-progressbar")
    (license license:expat)))

(define-public ruby-unicode-display-width
(package
  (name "ruby-unicode-display-width")
  (version "1.1.0")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "unicode-display_width" version))
      (sha256
        (base32
          "194d70pfxq4d7rrk0vsk1dvj46ns2f350308khi7q5cvnmg3h1xs"))))
  (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; test data not included in gem.
  (synopsis
    "[Unicode 1.1.0] Determines the monospace display width of a string using EastAsianWidth.txt, Unicode general category, and other data.")
  (description
    "[Unicode 1.1.0] Determines the monospace display width of a string using EastAsianWidth.txt, Unicode general category, and other data.")
  (home-page
    "http://github.com/janlelis/unicode-display_width")
  (license license:expat)))

(define-public ruby-ast
(package
  (name "ruby-ast")
  (version "2.3.0")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "ast" version))
      (sha256
        (base32
          "0pp82blr5fakdk27d1d21xq9zchzb6vmyb1zcsl520s3ygvprn8m"))))
  (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; TODO: run tests
  (native-inputs
   `(("bundler" ,bundler)
     ("ruby-bacon" ,ruby-bacon)
     ("ruby-racc" ,ruby-racc)))
  (synopsis
    "A library for working with Abstract Syntax Trees.")
  (description
    "This package provides a library for working with Abstract Syntax Trees.")
  (home-page "https://whitequark.github.io/ast/")
  (license license:expat)))

(define-public ruby-racc
(package
  (name "ruby-racc")
  (version "1.4.14")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "racc" version))
      (sha256
        (base32
          "00yhs2ag7yy5v83mqvkbnhk9bvsh6mx3808k53n61ddzx446v1zl"))))
  (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; tests fail to compile
  (native-inputs
   `(("ruby-hoe" ,ruby-hoe)
     ("ruby-rake-compiler" ,ruby-rake-compiler)))
  (synopsis
    "Racc is a LALR(1) parser generator.
  It is written in Ruby itself, and generates Ruby program.

  NOTE: Ruby 1.8.x comes with Racc runtime module.  You
  can run your parsers generated by racc 1.4.x out of the
  box.")
  (description
    "Racc is a LALR(1) parser generator.
  It is written in Ruby itself, and generates Ruby program.

  NOTE: Ruby 1.8.x comes with Racc runtime module.  You
  can run your parsers generated by racc 1.4.x out of the
  box.")
  (home-page
    "http://i.loveruby.net/en/projects/racc/")
  (license license:expat)))

(define-public ruby-sass-rails
(package
  (name "ruby-sass-rails")
  (version "5.0.6")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "sass-rails" version))
      (sha256
        (base32
          "0iji20hb8crncz14piss1b29bfb6l89sz3ai5fny3iw39vnxkdcb"))))
  (build-system ruby-build-system)
  (arguments
   `(#:tests? #f)) ; No tests are included in gem.
  (propagated-inputs
    `(("ruby-railties" ,ruby-railties)
      ("ruby-sass" ,ruby-sass)
      ("ruby-sprockets" ,ruby-sprockets)
      ("ruby-sprockets-rails" ,ruby-sprockets-rails)
      ("ruby-tilt" ,ruby-tilt)))
  (synopsis
    "Sass adapter for the Rails asset pipeline.")
  (description
    "Sass adapter for the Rails asset pipeline.")
  (home-page "https://github.com/rails/sass-rails")
  (license license:expat)))

(define-public ruby-uglifier
(package
  (name "ruby-uglifier")
  (version "3.0.0")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "uglifier" version))
      (sha256
        (base32
          "05a7xqzzlliqbd32jfkmis08pb0cljns7jx14ybkqb9zbg7ph35h"))))
  (build-system ruby-build-system)
  (arguments
   `(#:tests? #f)) ; Tests fail, perhaps they require npm? test-target "spec"
     ;; #:phases
     ;;   (modify-phases %standard-phases
     ;;     (add-before 'check 'list
     ;;                 (lambda _
     ;;                   (system* "rake" "js")
     ;;                   (system* "gem" "list"))))))
  (propagated-inputs
   `(("ruby-execjs" ,ruby-execjs)))
  ;; (native-inputs
  ;;  `(("bundler" ,bundler)
  ;;    ("ruby-rubocop" ,ruby-rubocop)
  ;;    ("ruby-rspec" ,ruby-rspec)
  ;;    ("ruby-sourcemap" ,ruby-sourcemap)
  ;;    ("ruby-duktape" ,ruby-duktape))) ; Use as the JS interpreter
  (synopsis
    "Uglifier minifies JavaScript files by wrapping UglifyJS to be accessible in Ruby")
  (description
    "Uglifier minifies JavaScript files by wrapping UglifyJS to be accessible in Ruby")
  (home-page "http://github.com/lautis/uglifier")
  (license license:expat)))

(define-public ruby-sourcemap
(package
  (name "ruby-sourcemap")
  (version "0.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "sourcemap" version))
      (sha256
        (base32
          "0l111zg9qh2g15rpmi2d006m3h27hl462d7zmc4js67ppmkcd7w8"))))
  (build-system ruby-build-system)
  (native-inputs
   `(("bundler" ,bundler)
     ("ruby-minitest" ,ruby-minitest)))
  (synopsis "Ruby source maps")
  (description "Ruby source maps")
  (home-page "http://github.com/maccman/sourcemap")
  (license license:expat)))

(define-public ruby-coffee-rails
(package
  (name "ruby-coffee-rails")
  (version "4.2.1")
  (source
    (origin
      (method url-fetch)
      ;; Tests are not distributed at rubygems.org so download from GitHub
      ;; instead.
      (uri (string-append "https://github.com/rails/coffee-rails/archive/v"
                          version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0a7cac67n1ndky3n8b1vdrdq67cahq6sn76saiw5xgzd296b3zjy"))
       (patches (search-patches "ruby-coffee-rails-fix-rakefile.patch"))))
  (build-system ruby-build-system)
  (arguments
   `(#:tests? #f)) ; Tests require rails, disable to precent circular dependency.
  (propagated-inputs
    `(("ruby-coffee-script" ,ruby-coffee-script)
      ("ruby-railties" ,ruby-railties)))
  (synopsis
    "CoffeeScript adapter for the Rails asset pipeline.")
  (description
    "CoffeeScript adapter for the Rails asset pipeline.")
  (home-page
    "https://github.com/rails/coffee-rails")
  (license license:expat)))

(define-public ruby-jquery-rails
  (package
    (name "ruby-jquery-rails")
    (version "4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "jquery-rails" version))
       (sha256
        (base32
         "1asbrr9hqf43q9qbjf87f5lm7fp12pndh76z89ks6jwxf1350fj1"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)))
    (propagated-inputs
     `(("ruby-rails-dom-testing"
        ,ruby-rails-dom-testing)
       ("ruby-railties" ,ruby-railties)
       ("ruby-thor" ,ruby-thor)))
    (synopsis
     "This gem provides jQuery and the jQuery-ujs driver for your Rails 4+ application.")
    (description
     "This gem provides jQuery and the jQuery-ujs driver for your Rails 4+ application.")
    (home-page
     "http://rubygems.org/gems/jquery-rails")
    (license license:expat)))

(define-public ruby-turbolinks
  (package
    (name "ruby-turbolinks")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "turbolinks" version))
       (sha256
        (base32
         "1dpsl17mygsd3hjcb2zq05n9zygbi0qc5130h276lw6py8g7nppc"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; There are no tests.
    (propagated-inputs
     `(("ruby-turbolinks-source" ,ruby-turbolinks-source)))
    (synopsis
     "Rails engine for Turbolinks 5 support")
    (description
     "Rails engine for Turbolinks 5 support")
    (home-page
     "https://github.com/turbolinks/turbolinks-rails")
    (license license:expat)))

(define-public ruby-jbuilder
  (package
    (name "ruby-jbuilder")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "jbuilder" version))
       (sha256
        (base32
         "1jbh1296imd0arc9nl1m71yfd7kg505p8srr1ijpsqv4hhbz5qci"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-dependencies'
           (lambda _
             (substitute* "Gemfile"
               ((".*pry.*") ""))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-mocha" ,ruby-mocha)
       ("ruby-appraisal" ,ruby-appraisal)))
    (propagated-inputs
     `(("ruby-activesupport" ,ruby-activesupport)
       ("ruby-multi-json" ,ruby-multi-json)))
    (synopsis
     "Create JSON structures via a Builder-style DSL")
    (description
     "Create JSON structures via a Builder-style DSL")
    (home-page "https://github.com/rails/jbuilder")
    (license license:expat)))

(define-public ruby-web-console
(package
  (name "ruby-web-console")
  (version "3.3.1")
  (source
    (origin
      (method url-fetch)
      ;; Download from GitHub as test files are not provided in the gem.
      (uri (string-append "https://github.com/rails/web-console/archive/v"
                          version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1gmymwvgwqjv4gmg2vb0abm6flwax9sg73wmh6yb7m818pn2lz3n"))))
  (build-system ruby-build-system)
  (arguments
   `(#:tests? #f)) ; Do not test to prevent dependency cycle with rails.
  ;; (arguments
  ;;  `(#:phases
  ;;    (modify-phases %standard-phases
  ;; (add-before 'check 'setenv
  ;;                    (lambda _
  ;;                      (setenv "RUBYLIB" "lib")
  ;;                      ;; (substitute* "Rakefile"
  ;;                      ;;   (("require 'web_console") ; This is not required for
  ;;                      ;;                  ; travis, so hmm.
  ;;                      ;;    "require 'pathname'; require 'uri'; require 'web_console"))
  ;;                      #t)))))
  ;; (native-inputs
  ;;  `(("bundler" ,bundler)))
  (propagated-inputs
    `(("ruby-actionview" ,ruby-actionview)
      ("ruby-activemodel" ,ruby-activemodel)
      ("ruby-debug-inspector" ,ruby-debug-inspector)
      ("ruby-railties" ,ruby-railties)))
  (synopsis
    "A debugging tool for your Ruby on Rails applications.")
  (description
    "This package provides a debugging tool for your Ruby on Rails applications.")
  (home-page
    "https://github.com/rails/web-console")
  (license license:expat)))

(define-public ruby-sass
(package
  (name "ruby-sass")
  (version "3.4.22")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "sass" version))
      (sha256
        (base32
          "0dkj6v26fkg1g0majqswwmhxva7cd6p3psrhdlx93qal72dssywy"))))
  (build-system ruby-build-system)
  (arguments
   ;; Cannot run sass-spec options because the sass-spec package does
   ;; not work. TODO: Fix.
   `(#:test-target "test:ruby"))
  (native-inputs
   `(("ruby-rubycop" ,ruby-rubocop)
     ("ruby-ruby-progressbar" ,ruby-ruby-progressbar)))
  (synopsis
    "      Sass makes CSS fun again. Sass is an extension of CSS, adding
      nested rules, variables, mixins, selector inheritance, and more.
      It's translated to well-formatted, standard CSS using the
      command line tool or a web-framework plugin.
")
  (description
    "      Sass makes CSS fun again.  Sass is an extension of CSS, adding
      nested rules, variables, mixins, selector inheritance, and more.
      It's translated to well-formatted, standard CSS using the
      command line tool or a web-framework plugin.
")
  (home-page "http://sass-lang.com/")
  (license license:expat)))

(define-public ruby-turbolinks-source
  (package
    (name "ruby-turbolinks-source")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "turbolinks-source" version))
       (sha256
        (base32
         "1s197pamkac9kkhslj41gxxihx6jp3dh4g394k9zmbxwkfrf36zb"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; There are no tests.
    (synopsis "Turbolinks JavaScript assets")
    (description "Turbolinks JavaScript assets")
    (home-page
     "https://github.com/turbolinks/turbolinks-source-gem")
    (license license:expat)))

(define-public ruby-ref
(package
  (name "ruby-ref")
  (version "2.0.0")
  (source
    (origin
      (method url-fetch)
      ;; Tests are not distributed at rubygems.org so download from GitHub
      ;; instead.
      (uri (string-append "https://github.com/ruby-concurrency/ref/archive/v"
                          version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1090z8vzf8bf7gx3akvrn4alcklyanbg6hipjmzlzkx0lr1l5zj4"))))
  (build-system ruby-build-system)
  (arguments
   `(#:test-target "spec"))
  (synopsis
    "Library that implements weak, soft, and strong references in Ruby that work
across multiple runtimes (MRI, Jruby and Rubinius). Also includes implementation
of maps/hashes that use references and a reference queue.")
  (description
    "Library that implements weak, soft, and strong references in Ruby that work
across multiple runtimes (MRI, Jruby and Rubinius).  Also includes
implementation of maps/hashes that use references and a reference queue.")
  (home-page
    "http://github.com/ruby-concurrency/ref")
  (license license:expat)))

(define-public ruby-redjs
  ;; There are no releases on rubygems and the last git commit was in 2012, so
  ;; we package that.
  (let ((commit "0d844f066666f967a78b20beb164c52d9ac3f5ca"))
    (package
      (name "ruby-redjs")
      (version (string-append "0.4.6-1." (string-take commit 8)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/cowboyd/redjs.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0cl3543xnzfn5qvlwjl2g1gg0jm6mfa75ag9qh89pfay7b0mxz7i"))))
      (build-system ruby-build-system)
      (arguments
       `(#:tests? #f ; There are no tests.
         #:phases
         (modify-phases %standard-phases
           (replace 'replace-git-ls-files
             (lambda _
               (substitute* "redjs.gemspec"
                 (("git ls-files") "find . -type f |sort"))
               #t)))))
      (synopsis "")
      (description
       "")
      (home-page "")
      (license license:expat)))) ;?

(define-public ruby-rubygems
  (package
    (name "ruby-rubygems")
    (version (string-append "2.6.6"))
    (source (origin
              (method url-fetch)
              (uri (string-append "https://rubygems.org/rubygems/rubygems-"
                                  version ".tgz"))
              (sha256
               (base32
                "0x0ldlwr627d0brw96jdbscib6d2nk19izvnh8lzsasszi1k5rkq"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (zero? (system*
                     "ruby" "setup.rb"
                     (string-append
                      "--prefix=" (assoc-ref outputs "out"))))))
         (delete 'check) ; fix
         (delete 'install) ; done in build
         )))
    (synopsis "")
    (description
     "")
    (home-page "")
    (license license:expat))) ;?


(define-public ruby-heredoc-unindent
  (package
    (name "ruby-heredoc-unindent")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "heredoc_unindent" version))
       (sha256
        (base32
         "14ijr2fsjwhrkjkcaz81d5xnfa4vvgvcflrff83avqw9klm011yw"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)))
    
    (synopsis
     "This gem removes common margin from indented strings")
    (description
     "This gem removes common margin from indented strings, such as the ones
produced by indented heredocs.  In other words, it strips out leading whitespace
chars at the beggining of each line, but only as much as the line with the
smallest margin.

It is acknowledged that many strings defined by heredocs are just code and fact
is that most parsers are insensitive to indentation.  If, however, the strings
are to be used otherwise, be it for printing or testing, the extra indentation
will probably be an issue and hence this gem.")
    (home-page
     "https://github.com/adrianomitre/heredoc_unindent")
    (license license:expat)))

(define-public ruby-hashdiff
  (package
    (name "ruby-hashdiff")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "hashdiff" version))
       (sha256
        (base32
         "1r06gar8zp4hyzyc0ky7n6mybjj542lrfda5y78fm5hyhiplv104"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec-2)))
    (synopsis
     " HashDiff is a diff lib to compute the smallest difference between two hashes. ")
    (description
     " HashDiff is a diff lib to compute the smallest difference between two hashes. ")
    (home-page
     "https://github.com/liufengyun/hashdiff")
    (license license:expat)))

(define-public ruby-vcr
(package
  (name "ruby-vcr")
  (version "3.0.3")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "vcr" version))
      (sha256
        (base32
          "1y19gb8vz1rbhx1qhim6kpp0fzksqhd7grb50hmrbjx5h4hc3y0y"))))
  (build-system ruby-build-system)
  (arguments
     `(#:tests? #f)) ; No Rakefile in gem, and there's likely unpackaged
                     ; dependencies anyway.
  (synopsis
    "Record your test suite's HTTP interactions and replay them during future test runs for fast, deterministic, accurate tests.")
  (description
    "Record your test suite's HTTP interactions and replay them during future test runs for fast, deterministic, accurate tests.")
  (home-page "http://vcr.github.io/vcr")
  (license license:expat)))

(define-public ruby-listen
(package
  (name "ruby-listen")
  (version "3.1.5")
  (source
    (origin
      (method url-fetch)
      ;; The gem does not include a Rakefile, so we fetch the tarball from
      ;; Github.
      (uri (string-append "https://github.com/guard/listen/archive/v"
                          version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
         "0f0mx4nzpd17svvmkcb2q66w3gdvs1zkrpc0p3wq1s2va5b5cnss"))
      (patches (search-patches "ruby-listen-patch-gemspec.patch"))))
  (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-dependencies'
           (lambda _
             (substitute* "Rakefile"
               ((".*rubocop.*") ""))
             #t)))))
  (native-inputs
   `(("bundler" ,bundler)
     ("ruby-rspec" ,ruby-rspec)
     ("ruby-coveralls" ,ruby-coveralls)))
  (propagated-inputs
    `(("ruby-rb-fsevent" ,ruby-rb-fsevent)
      ("ruby-rb-inotify" ,ruby-rb-inotify)
      ("ruby-ruby-dep" ,ruby-ruby-dep)))
  (synopsis
    "The Listen gem listens to file modifications and notifies you about the changes. Works everywhere!")
  (description
    "The Listen gem listens to file modifications and notifies you about the changes.  Works everywhere!")
  (home-page "https://github.com/guard/listen")
  (license license:expat)))

(define-public ruby-listen-3.0
  (package
    (inherit ruby-listen)
    (name "ruby-listen")
    (version "3.0.8")
    (source
     (origin
       (method url-fetch)
       ;; The gem does not include a Rakefile, so we fetch the tarball from
       ;; Github.
       (uri (string-append "https://github.com/guard/listen/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "06cy038xlbi8hcr9nv0c9wvafi7s3d05sdc7ydkv8qndi9bs68l3"))
       (patches (search-patches "ruby-listen-3.0.8-patch-gemspec.patch"))))))

(define-public ruby-ruby-dep
(package
  (name "ruby-ruby-dep")
  (version "1.4.0")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "ruby_dep" version))
      (sha256
        (base32
          "12i57gpy0gmkwnd7l6xdjpfw9bygxmgwx4hjwgg4mca2jr7d3g47"))))
  (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; Do not test to avoid circular dependency with gem_isolator.
  (synopsis
    "Creates a version constraint of supported Rubies,suitable for a gemspec file")
  (description
    "Creates a version constraint of supported Rubies,suitable for a gemspec file")
  (home-page "https://github.com/e2/ruby_dep")
  (license license:expat)))

(define-public ruby-rb-fsevent
(package
  (name "ruby-rb-fsevent")
  (version "0.9.7")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "rb-fsevent" version))
      (sha256
        (base32
          "1xlkflgxngwkd4nyybccgd1japrba4v3kwnp00alikj404clqx4v"))))
  (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; Tests require a special setup of rbenv.
  (native-inputs
   `(("bundler" ,bundler)
     ("ruby-rspec" ,ruby-rspec-2)
     ("ruby-guard-rspec" ,ruby-guard-rspec)))
  (synopsis
    "FSEvents API with Signals catching (without RubyCocoa)")
  (description
    "FSEvents API with Signals catching (without RubyCocoa)")
  (home-page "http://rubygems.org/gems/rb-fsevent")
  (license license:expat)))

(define-public ruby-rb-inotify
(package
  (name "ruby-rb-inotify")
  (version "0.9.7")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "rb-inotify" version))
      (sha256
        (base32
          "1yfcp3065n08balljmxn0qzwhdbwwxn2h9z89wmydyfj2gq1p71d"))))
  (build-system ruby-build-system)
  (arguments
   `(#:tests? #f)) ; Tests are not included in the gem.
  (propagated-inputs `(("ruby-ffi" ,ruby-ffi)))
  (synopsis
    "A Ruby wrapper for Linux's inotify, using FFI")
  (description
    "This package provides a Ruby wrapper for Linux's inotify, using FFI")
  (home-page "http://github.com/nex3/rb-inotify")
  (license #f)))

(define-public ruby-guard-rspec
(package
  (name "ruby-guard-rspec")
  (version "4.7.3")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "guard-rspec" version))
      (sha256
        (base32
          "1jkm5xp90gm4c5s51pmf92i9hc10gslwwic6mvk72g0yplya0yx4"))))
  (build-system ruby-build-system)
   (arguments
     `(#:tests? #f)) ; Tests require further dependencies e.g. gem_isolator.
  (propagated-inputs
    `(("ruby-guard" ,ruby-guard)
      ("ruby-guard-compat" ,ruby-guard-compat)
      ("ruby-rspec" ,ruby-rspec)))
  (synopsis
    "Guard::RSpec automatically run your specs (much like autotest).")
  (description
    "Guard::RSpec automatically run your specs (much like autotest).")
  (home-page
    "https://github.com/guard/guard-rspec")
  (license license:expat)))

(define-public ruby-guard-compat
(package
  (name "ruby-guard-compat")
  (version "1.2.1")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "guard-compat" version))
      (sha256
        (base32
          "1zj6sr1k8w59mmi27rsii0v8xyy2rnsi09nqvwpgj1q10yq1mlis"))))
  (build-system ruby-build-system)
  (arguments
   `(#:test-target "spec"))
  (native-inputs
   `(("bundler" ,bundler)
     ("ruby-rspec" ,ruby-rspec)
     ("ruby-rubocop" ,ruby-rubocop)))
  (synopsis
    "Helps creating valid Guard plugins and testing them")
  (description
    "Helps creating valid Guard plugins and testing them")
  (home-page "")
  (license license:expat)))

(define-public ruby-spring-watcher-listen
  (package
    (name "ruby-spring-watcher-listen")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "spring-watcher-listen" version))
       (sha256
        (base32
         "16gjqbhj3bkambafziza4pbw6g4zvh8z17lj87m8r90pgzcp93sj"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f ; circular dependency with rails?
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-gemfile
           (lambda _
             (substitute* "Gemfile"
               (("^gem \\\"spring.*") "gem 'spring'\ngem 'listen'\n"))
             #t)))))
  (native-inputs
   `(("bundler" ,bundler)
     ("ruby-activesupport" ,ruby-activesupport)))
  (propagated-inputs
    `(("ruby-listen" ,ruby-listen)
      ("ruby-spring" ,ruby-spring)))
  (synopsis
    "Makes spring watch files using the listen gem.")
  (description
    "Makes spring watch files using the listen gem.")
  (home-page
    "https://github.com/jonleighton/spring-watcher-listen")
  (license license:expat)))

(define-public ruby-rspec-spies
(package
  (name "ruby-rspec-spies")
  (version "2.1.4")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "rspec-spies" version))
      (sha256
        (base32
          "0pyjy35k59gsiv7l8585yx4gkg2vhycjycz9jlmgkx8s62dxnjgv"))))
  (build-system ruby-build-system)
  (arguments
   `(#:tests? #f)) ; Tests fail, is this completely deprecated?
  ;; phases
  ;;      (modify-phases %standard-phases
  ;;        (replace 'check
  ;;                 (lambda _
  ;;                   (zero? (system* "rspec"))))
  ;;        (add-before 'check 'fix-dependencies
  ;;                    (lambda _
  ;;                      (delete-file "Gemfile.lock")
  ;;                     (substitute* "Gemfile"
  ;;                       ((".*jeweler.*") "\n"))
  ;;                      #t)))))
  ;; (native-inputs
  ;;  `(("bundler" ,bundler)
  ;;    ("ruby-appraisal" ,ruby-appraisal)))
  (propagated-inputs
   `(("ruby-rspec" ,ruby-rspec-2)))
  (synopsis "test spies, for rspec")
  (description "test spies, for rspec")
  (home-page
    "http://github.com/technicalpickles/rspec-spies")
  (license #f)))

