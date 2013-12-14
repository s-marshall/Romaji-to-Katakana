(ns parser-toy.core (:use clojure.algo.monads))

; Romaji-to-Katakana transliteration using monads

(defn romaji-to-kana [get-romaji-function state]
  
  (let [conversion-table {
          "a" "ア" "i" "イ" "u" "ウ" "e" "エ" "o" "オ"
          "ka" "カ" "ki" "キ" "ku" "ク" "ke" "ケ" "ko" "コ"
          "sa" "サ" "shi" "シ" "su" "ス" "se" "セ" "so" "ソ"
          "ta" "タ" "ti" "チ" "tsu" "ツ" "te" "テ" "to" "ト"
          "na" "ナ" "ni" "ニ" "nu" "ヌ" "ne" "ネ" "no" "ノ"
          "ha" "ハ" "hi" "ヒ" "hu" "フ" "he" "ヘ" "ho" "ホ"
          "ma" "マ" "mi" "ミ" "mu" "ム" "me" "メ" "mo" "モ"
          "ya" "ヤ" "yu" "ユ" "yo" "ヨ"
          "ra" "ラ" "ri" "リ" "ru" "ル" "re" "レ" "ro" "ロ"
          "wa" "ワ" "wo" "ヲ" "n" "ン"}
          
         romaji-and-remainder (get-romaji-function (second state))  
         romaji (first romaji-and-remainder)  
         unprocessed-string (second romaji-and-remainder)
         kana (conversion-table romaji)
        ]
      (if (not (nil? kana))
        [(str (first state) kana) unprocessed-string]
        nil)))

(defn get-one-char [string]
  (if (empty? string)
    []
    [(subs string 0 1) (subs string 1)]))

(defn get-two-chars [string]
  (if (< (count string) 2)
    []
    [(subs string 0 2) (subs string 2)]))

(defn two-letter-conversion [state] (romaji-to-kana get-two-chars state))
(defn one-letter-conversion [state] (romaji-to-kana get-one-char state))

(with-monad (state-t maybe-m)
  (defn convert [state]
    ((domonad (state-t maybe-m)
      [katakana (m-plus two-letter-conversion one-letter-conversion)
        :when (not (nil? katakana))]
          katakana) state)))

; Same as the above function, just a different form
(defn convert [state]
  ((domonad (state-t maybe-m)
    [katakana (m-plus two-letter-conversion one-letter-conversion)
      :when (not (nil? katakana))]
        katakana) state))

(defn convert-romaji-to-katakana [input-state]
  (let [input (second input-state)
        output (first input-state)]
        
    (if (empty? input)
      output
      (let [result (convert input-state)]
        (convert-romaji-to-katakana result)))))
        
