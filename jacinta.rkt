#lang racket
;(require "./lexicons/AFINN-lexicon") ; Sentiment lexicon
;(require net/url)
(require data-science-master)
;(require uk-tweet-data)
(require plot)
(require math)
(require racket/date)

;; Step 1: Read the Twitter dataset
(define twitter-data (read-csv "United Kingdom.csv" #:header? #t))

;; Extract necessary columns
(define created-at-column ($ twitter-data 'created_at))
(define text-column ($ twitter-data 'text))

;;; Step 2: Preprocess tweets
;;; Normalize case, remove URLs, remove punctuation, and remove spaces
;; from each tweet. This function takes a list of words and returns a
;; preprocessed subset of words/tokens as a list
(define (clean-tweet text)
  (string-normalize-spaces
   (remove-punctuation
    (remove-urls                              
     (string-downcase text))))) 

;; Preprocess the tweets, handling missing text data
(define cleaned-tweets
  (map (λ (tweet)
         (if (string? tweet)
             (clean-tweet tweet)
             "Unknown"))
       text-column))

;; Combine all cleaned tweets into a single string
(define all-cleaned-text (string-join cleaned-tweets " "))
(define words (document->tokens all-cleaned-text #:sort? #t))

;; Perform sentiment analysis using NRC lexicon
(define sentiment (list->sentiment words #:lexicon 'nrc))

;;  Parse dates and extract months
(define (extract-month date-str)
  (if (or (string=? date-str "")
          (empty? (string-split date-str " ")))
      "Unknown" ; Return "Unknown" for invalid or empty input
      (let ([tokens (string-split date-str " ")])
        (substring (car tokens) 0 7)))) ; Extract year and month from valid input

;; Generate months from the created_at column
(define months
  (map extract-month created-at-column))

;; Add the month information to the sentiment data
(define sentiment-length (length sentiment))
(define months-length (length months))

(displayln (string-append "Sentiment list length: " (number->string sentiment-length)))
(displayln (string-append "Months list length: " (number->string months-length)))
(define cleaned-sentiment (filter (λ (s) (not (eq? s #f))) sentiment))

;; Ensure both lists are the same length
(define months-truncated (take months (length cleaned-sentiment)))

;; Now both lists have the same length
(define sentiment-with-months
  (map (λ (row month)
         (append row (list month)))
       cleaned-sentiment
       months-truncated))
;(define sentiment-with-months
 ; (map (λ (row month)
 ;        (append row (list month)))
  ;     sentiment
  ;     months))

;; Aggregate sentiment data by month
(define monthly-sentiment
  (aggregate sum
            ; ($ sentiment-with-months 'sentiment)
             ($ sentiment-with-months 'month)))

;; Sort the data for plotting
(define sorted-sentiment
  (sort monthly-sentiment
        (λ (x y) (string<? (first x) (first y)))))

;; Plot the aggregated data
(let ([counts (map (λ (month sentiment) (list month sentiment)) sorted-sentiment)])
  (parameterize ((plot-width 800))
    (plot (list
           (tick-grid)
           (discrete-histogram counts
                               #:color "MediumSlateBlue"
                               #:line-color "MediumSlateBlue"))
          #:x-label "Month"
          #:y-label "Sentiment Frequency")))

