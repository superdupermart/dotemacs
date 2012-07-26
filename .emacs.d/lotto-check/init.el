;; 아래의 내용을 .emacs 에 복사
(add-to-list 'load-path "~/.emacs.d/lotto-check/")
(require 'lotto-check)

;; 로또 당첨 정보를 제공하는 데이터 소스를 지정합니다.
;; 기본값은 +lotto-data-source-lotto-k+ 이며,
;; 네이버, 다음, 네이트, 나눔로또 공식 홈페이지, 사용자 정의 방법 등을 제공합니다.
;; 네이버: +lotto-data-source-naver+
;; 다음: +lotto-data-source-daum+
;; 네이트: +lotto-data-source-nate+
;; 나눔로또 공식 홈페이지: +lotto-data-source-645lotto+
;; 사용자 정의 소스: lotto-info-data-source-custom
;; * 사용자 정의 소스 방식을 사용할 경우 lotto-info-data-source-custom 변수에
;;   사용자 정의 소스를 지정합니다.
;;   예) (setq lotto-info-data-source-custom 'my-lotto-info-data-src-1)
(setq lotto-info-data-source +lotto-data-source-lotto-k+)
;; 로또 당첨 정보를 저장할 파일을 지정합니다.
(setq lotto-database-file "~/.emacs.d/lotto-database")
;; interactive function 실행 시 결과를 별도의 buffer에 보여주도록 설정합니다.
(setq lotto-use-buffer-for-message t)