(ns parser-toy.core-test
  (:require [clojure.test :refer :all]
            [parser-toy.core :refer :all]))

(deftest convert-test
  (testing "convert"
    (is (= (convert ["" "kantan"]) ["カ" "ntan"]))
    (is (= (convert ["" "kuroi"]) ["ク" "roi"]))
    (is (= (convert ["" "kxroi"]) nil))
    (is (= (convert (convert ["" "kana"])) ["カナ" ""]))))

(deftest convert-romaji-to-katakana-test
  (testing "convert-romaji-to-katakana"
    (is (= (convert-romaji-to-katakana ["" "kimono"]) "キモノ"))
    (is (= (convert-romaji-to-katakana ["" "dog"]) nil))
    (is (= (convert-romaji-to-katakana ["" "kin"]) "キン"))
    (is (= (convert-romaji-to-katakana ["" "unkou"]) "ウンコウ"))
    (is (= (convert-romaji-to-katakana ["" "wanwan"]) "ワンワン"))))

