#lang racket/base
(require racket/runtime-path
         racket/list
         racket/function
         xml
         (prefix-in d: racket/date))

(struct date (year month day))
(struct user (name bday))
(struct activity (start-age end-age freq name))

(define (date->seconds d)
  (d:find-seconds 0 0 0 (date-day d) (date-month d) (date-year d)))

;; xxx Not the same computation humans normally use
(define (user-age u)
  (if (date? (user-bday u))
    (/ (- (current-seconds)
          (date->seconds (user-bday u)))
       (* 60 60 24 365))
    (user-bday u)))

(define (list-max l)
  (apply max l))

(define (activity-live? a u)
  (and (<= (activity-start-age a)
           (user-age u))
       (< (user-age u)
          (activity-end-age a))))

(define (activity-died? u a)
  (and (<= (activity-start-age a)
           (user-age u))
       (not (< (user-age u)
               (activity-end-age a)))))

(define (add-spacing l)
  (if (= (length l) 5)
    l
    (add-between l `(td ([class "gapday"]) nbsp))))

(define-runtime-path source-dir ".")

(define (go output-p users activities)
  (define live-activities
    (filter (λ (a)
              (ormap (curry activity-live? a)
                     users))
            activities))
  (define max-freq
    (list-max (map activity-freq live-activities)))

  (define xe
    `(html
      (head
       (title "Kid Grid")
       (link ([rel "stylesheet"]
              [type "text/css"]
              [href "style.css"])))
      (body
       (div
        ([id "container"])
        (div
         ([id "header"])
         (h3 "Week of "
             (script
              "document.write(new Date().toDateString());")))
        (div
         ([id "body"])
         (table
          ([id "activities"])
          (tbody
           (tr ([class "boxes"])
               ,@(for/list ([u (in-list users)])
                   (define died-as
                     (filter (curry activity-died? u) activities))
                   `(td ([class "died"])
                     (table
                      (tr
                       (td ,(user-name u))
                       (td ,@(map (λ (a) `(span ,(activity-name a))) died-as)))))))
           ,@(append*
              (for/list ([a (in-list live-activities)])
                (list
                 `(tr ([class "activity"])
                      (td ([colspan ,(number->string (length users))])
                          ,(activity-name a)))
                 `(tr ([class "boxes"])
                   ,@(for/list ([u (in-list users)])
                       (if (activity-live? a u)
                         `(td
                           (table ([id "days"])
                                  (tr ,@(add-spacing
                                         (for/list ([i (in-range (activity-freq a))])
                                           `(td
                                             (table ([id "day"])
                                                    (tr
                                                     ,@(for/list ([j (in-range 3)])
                                                         `(td nbsp))))))))))
                         `(td ([class "blank"]) nbsp))))))))))
        (div ([id "footer"])
             ,@(for/list ([i (in-range 21)])
                 `(span ,(number->string i) nbsp)))))))

  (with-output-to-file output-p
    #:exists 'replace
    (λ () 
      (displayln "<!DOCTYPE html>")
      (write-xexpr xe))))

(module+ main
  (require racket/runtime-path)
  (define-runtime-path index.html "static/index.html")

  (define ??? 18.0)
  (go
   index.html
   (list
    (user "Frog"
          5.0)
    (user "Peach"
          3.5))
   (list
    (activity 3.0 3.5 5 "Reading > Letters")
    (activity 3.0 3.5 5 "Math > Numbers")
    ;; (activity 3.0 ??? 1 "Life Skills")
    ;; (activity 3.0 3.5 3 "Reading > Learning Books")
    (activity 3.0 3.5 3 "Writing > Tracing")
    (activity 3.25 ??? 3 "Computer > Keyboard & Mouse")
    (activity 3.5 4.0 5 "Reading > Letters [written]")
    (activity 3.5 4.0 5 "Math > Numbers [written]")
    (activity 3.5 ??? 5 "Reading > Blends")
    (activity 4.0 ??? 3 "Music")
    (activity 4.0 ??? 5 "Reading > Words [written]")
    (activity 4.0 5.0 5 "Math > Addition [10x10] [written]")
    (activity 4.5 ??? 5 "Reading > Beginning reader books (to parent)")
    (activity 5.0 ??? 5 "Math > Subtraction [10x10] [written]")
    (activity 5.0 ??? 3 "Science > Basic reading (w/ parent)")
    (activity 5.0 ??? 3 "History > Basic reading (w/ parent)")
    (activity 5.0 ??? 5 "Literature > Basic reading (w/ parent)")
    (activity 5.0 ??? 5 "Composition > Sentences [written]"))))
